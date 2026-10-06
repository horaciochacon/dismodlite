# Bandas de edad y promedios poblacionales sobre intervalos de edad.
#
# Notación: ver R/edo.R.
#
# Toda cantidad del modelo por banda de edad (en el ancla, en los datos y en las tablas de salida) es el promedio
# poblacional de la cantidad anual sobre el intervalo de la banda; nunca se evalúa en el punto medio. Este archivo es
# el único lugar de esa regla: la usan la verosimilitud (ancla y datos), dl_cascada, dl_avd, dl_validar_ancla,
# dl_sensibilidad, dl_etiquetas y dl_resumir. La renormalización de la cascada (.dl_renormalizar, R/cascada.R) la
# usa en su forma de banda fina: un promedio simple de las edades de la banda, porque todas pesan N_b.
#
# Convenciones
#   Banda de edad: intervalo [inicio, fin) = [age_start, age_end) en años, cerrado a la izquierda y abierto a la
#     derecha: la edad a pertenece a la banda si age_start <= a < age_end (.dl_banda_de).
#   Bandas finas: las bandas de la tabla de población que no contienen a otra presente (sin agregados como «todas las
#     edades»), ordenadas por age_start y sin solapes: una partición de las edades. N_b es la población de la banda
#     fina b en una ubicación y un sexo.
#   Malla anual: las edades enteras a = edad_inicio, ..., edad_fin (sufijo _anual). x(a) es una cantidad anual
#     (p, i (1 - p), p f, ...) en la edad exacta a.
#
# Promedio poblacional de x sobre el intervalo [inicio, fin):
#   A   = { a de la malla anual : inicio <= a < fin }
#   n_b = número de edades de la malla anual en la banda fina b (de toda la malla, no solo de A)
#   m_a = N_b(a) / n_b(a),                       b(a) = banda fina que contiene la edad a
#   w_a = m_a / sum_{a' en A} m_a'
#   q   = sum_{a en A} w_a x(a)                  (los w_a suman 1)
# Cada edad pesa con la población de su banda fina repartida por igual entre las edades de la malla en esa banda
# (m_a, personas por año de edad): dentro de una banda las edades pesan igual y, entre bandas, una banda completa
# pesa en proporción a N_b. Si todas las bandas de A tienen el mismo n_b (una sola banda, o varias quinquenales), n_b
# se cancela y w_a = N_b(a) / sum N_b(a'): ese caso se calcula así, sin dividir, y no cambia ni un bit respecto de la
# versión 2.2.0. Solo cuando A cruza bandas con distinto n_b (un dato de todas las edades sobre quinquenios y una
# banda abierta de 80 años y más, que en la malla 0..99 tiene 20 edades) se divide por n_b; hasta la versión 2.2.0 no
# se dividía y esa banda abierta pesaba 4 veces lo que le corresponde frente a un quinquenio (20 edades frente a 5).
# Un intervalo sin edades de la malla, o con una edad sin banda fina, es un error; nunca un cero.
# Es una cuadratura por rectángulos con paso de 1 año y x evaluada en la edad exacta a (el cumpleaños), no a mitad
# del año de edad: el promedio de [inicio, fin) usa x(inicio), ..., x(fin - 1), cuya edad promedio es medio año
# menor que la del intervalo (42 frente a 42,5 en [40, 45)). La banda abierta final se representa con las edades
# enteras hasta edad_fin.

# ---- Constantes ------------------------------------------------------------------------------------------------

# Banda «todas las edades» del catálogo de edades GBD: age_group_id 22 = [0, 125), que contiene a todas las demás. Es
# la banda de una tabla de proxies sin gradiente por edad (sin columna age_group_id).
.DL_BANDA_TODAS_LAS_EDADES <- 22L

# Estadístico puntual de las tablas por banda: la media de las simulaciones (convención GBD). La media es lineal, así
# que los departamentos suman el nacional, las causas hijas suman la causa madre y las agregaciones del informe
# (causas, sexos, edades) son exactas por construcción; con la mediana, las sumas se desviaban hasta 1 % por celda y
# 2 % en el total del grupo. Los manifiestos lo declaran (params.estadistico_puntual): si cambia, cambia con él la
# línea val = mean(val) de .dl_stats_bandas.
.DL_ESTADISTICO_PUNTUAL <- "media"

# ---- Bandas finas y contención ----------------------------------------------------------------------------------

# Bandas finas de una tabla de límites (age_group_id, age_start, age_end): quita toda banda que contiene a otra banda
# de la tabla (age_start_j >= age_start_i y age_end_j <= age_end_i, j != i), como «todas las edades» junto a las
# quinquenales u «80+» junto a 80-84. Dos bandas con los mismos límites se contienen mutuamente y caen las dos.
# Devuelve la tabla ordenada por age_start.
.dl_bandas_finas <- function(limites) {
  n <- nrow(limites)
  contiene_otra <- vapply(seq_len(n), function(i)
    any(seq_len(n) != i & limites$age_start >= limites$age_start[i] & limites$age_end <= limites$age_end[i]),
    logical(1))
  limites <- limites[!contiene_otra]
  data.table::setorder(limites, age_start)[]
}

# Banda de cada edad: índice de la fila de `lim` (bandas ordenadas por age_start y sin solapes) con
# age_start <= a < age_end; NA si ninguna la contiene (antes de la primera banda, desde el fin de la última o en un
# hueco). Único lugar de la regla de contención [inicio, fin): la usan los pesos de intervalo, los pesos por edad de la
# cascada y su renormalización.
.dl_banda_de <- function(edades, lim) {
  banda <- findInterval(edades, lim$age_start)               # última banda con age_start <= a (0 si ninguna)
  dentro <- banda >= 1L & edades < lim$age_end[pmax(banda, 1L)]
  ifelse(dentro, banda, NA_integer_)
}

# TRUE si dos de las bandas [age_start, age_end) se solapan: ordenadas por inicio, alguna empieza antes del fin de la
# anterior (si dos bandas se solapan, también se solapan dos consecutivas). Ni las bandas finas de la población
# (.dl_bandas_poblacion) ni las del ancla de una medida y un sexo (.dl_materializar_medida) pueden solaparse.
.dl_hay_solape <- function(age_start, age_end) {
  o <- order(age_start)
  any(age_start[o][-1L] < age_end[o][-length(o)])
}

# Límites (age_group_id, age_start, age_end) de las bandas `ids` en el catálogo de edades (insumos$bandas_catalogo),
# ordenados por age_start. Error con los ids que el catálogo no trae; `de` dice de dónde vienen («de la población»,
# «del proxy», ...).
.dl_limites_bandas <- function(catalogo, ids, de) {
  lim <- catalogo[catalogo$age_group_id %in% ids]   # sin DT[age_group_id %in% ...]: no deja un índice en `catalogo`
  falta <- setdiff(ids, lim$age_group_id)
  if (length(falta))
    .dl_stop("banda(s) de edad %s sin l\u00edmites en el cat\u00e1logo de bandas: %s", de,
             paste(falta, collapse = ", "))
  lim[order(age_start)]
}

# Bandas finas de la tabla de población, con sus límites del catálogo de edades: la partición sobre la que se ponderan
# todos los promedios por intervalo (insumos$bandas_pobl). Error si un age_group_id de la población no está en el
# catálogo o si dos bandas finas se solapan sin que una contenga a la otra.
.dl_bandas_poblacion <- function(pobl, catalogo) {
  bandas <- .dl_bandas_finas(.dl_limites_bandas(catalogo, sort(unique(pobl$age_group_id)), "de la poblaci\u00f3n"))
  if (.dl_hay_solape(bandas$age_start, bandas$age_end))
    .dl_stop(paste0("la poblaci\u00f3n trae bandas de edad que se solapan sin que una contenga a la otra (p. ej. ",
                    "75-84 y 80-89); declara una sola partici\u00f3n de las edades"))
  bandas
}

# ---- Promedio poblacional sobre intervalos ---------------------------------------------------------------------

# Pesos w_a del promedio poblacional sobre [age_start, age_end) (ver la cabecera del archivo):
#   edades_anual  edades de la malla anual (años enteros, en orden creciente);
#   pobl_sexo     población de una ubicación y un sexo por banda fina (columnas age_group_id y val);
#   bandas        bandas finas (age_group_id, age_start, age_end), p. ej. insumos$bandas_pobl.
# Devuelve list(idx, w): las posiciones en `edades_anual` de las edades de A y sus pesos w_a (suman 1).
# Gemela en C++: ninguna; .dl_ctx_cpp() (R/rcpp.R) pasa estos pesos ya calculados a q_banda().
.dl_pesos_intervalo <- function(edades_anual, age_start, age_end, pobl_sexo, bandas) {
  idx <- which(edades_anual >= age_start & edades_anual < age_end)
  if (!length(idx))
    .dl_stop("el intervalo de edad [%s, %s) no contiene ninguna edad de la malla anual", age_start, age_end)
  banda <- .dl_banda_de(edades_anual[idx], bandas)
  if (anyNA(banda))
    .dl_stop("edad(es) %s del intervalo [%s, %s) sin banda en la poblaci\u00f3n",
             paste(unique(edades_anual[idx][is.na(banda)]), collapse = ", "), age_start, age_end)
  ids_banda <- bandas$age_group_id[banda]
  N <- pobl_sexo$val[match(ids_banda, pobl_sexo$age_group_id)]           # N_b(a) de cada edad a de A
  if (anyNA(N))
    .dl_stop("poblaci\u00f3n sin banda(s) %s, necesaria(s) para el intervalo [%s, %s)",
             paste(unique(ids_banda[is.na(N)]), collapse = ", "), age_start, age_end)
  # n_b(a): edades de la malla en la banda de cada edad a de A. Con el mismo n_b en todo A la división se cancela y
  # no se hace: los operandos son los de la versión 2.2.0 (invariancia numérica).
  n_malla <- tabulate(.dl_banda_de(edades_anual, bandas), nbins = nrow(bandas))[banda]
  if (length(unique(n_malla)) > 1L) N <- N / n_malla
  list(idx = idx, w = N / sum(N))
}

# Promedio poblacional de una cantidad anual sobre cada intervalo j: q_j = sum_{a en A_j} w_a x(a), con x_anual en la
# malla anual y `pesos` la lista de .dl_pesos_intervalo de los intervalos. La verosimilitud la evalúa en cada paso del
# muestreador. Gemela en C++: q_banda() (una banda).
.dl_q_intervalos <- function(x_anual, pesos) {
  vapply(pesos, function(pesos_banda) sum(x_anual[pesos_banda$idx] * pesos_banda$w), numeric(1))
}

# ---- Promedios por banda de las simulaciones de un ajuste --------------------------------------------------------

# Bandas del ancla de prevalencia: las celdas (sexo, banda) de las tablas de salida de la corrida.
.dl_bandas_ancla <- function(b)
  b$prior_gbd[measure_id == .dl_medida_id("prevalence"), list(sex_id, age_group_id, age_start, age_end)]

# Promedio poblacional por simulación de una cantidad anual de f$draws_q sobre las bandas pedidas, en cada ubicación
# con su propia población:
#   val(ubicación, sexo, banda, simulación) = sum_{a en A} w_a x(a),   x = integrando(draws_q) en la malla anual.
# `integrando`: función de la tabla draws_q (columnas p, i, f, ipop) que devuelve x; por defecto, p.
# `locs`: ubicaciones a agregar; por defecto todas las de draws_q (el ancla y los departamentos de una cascada). Las
# validaciones y las etiquetas nacionales pasan solo el ancla, para no agregar departamentos que luego descartarían.
# Devuelve (location_id, sex_id, age_group_id, draw, val), ordenada por esas columnas.
.dl_agregar_bandas <- function(f, b, bandas, integrando = function(dq) dq$p, locs = NULL) {
  dq_todas <- f$draws_q
  if (!"location_id" %in% names(dq_todas)) dq_todas <- data.table::copy(dq_todas)[, location_id := b$loc_ancla]
  locs <- locs %||% unique(dq_todas$location_id)
  dq_todas <- dq_todas[location_id %in% locs]     # el subconjunto es una copia: la key no toca f$draws_q (en caché)
  data.table::setkey(dq_todas, location_id, sex_id)
  out <- data.table::rbindlist(lapply(locs, function(loc) {
    data.table::rbindlist(lapply(unique(bandas$sex_id), function(sexo) {
      dq <- dq_todas[list(loc, sexo)]
      data.table::setorder(dq, draw, edad)
      dq[, x := integrando(.SD)]
      pobl_sexo <- b$poblacion[location_id == loc & sex_id == sexo]
      if (!nrow(pobl_sexo))
        .dl_stop("no hay poblaci\u00f3n para la ubicaci\u00f3n %s, sexo %s", loc, sexo)
      edades_anual <- sort(unique(dq$edad))
      bandas_sexo <- bandas[sex_id == sexo]
      data.table::rbindlist(lapply(seq_len(nrow(bandas_sexo)), function(j) {
        pesos_banda <- .dl_pesos_intervalo(edades_anual, bandas_sexo$age_start[j], bandas_sexo$age_end[j], pobl_sexo,
                                           b$bandas_pobl)
        dq[, list(location_id = loc, sex_id = sexo, age_group_id = bandas_sexo$age_group_id[j],
                  val = sum(x[pesos_banda$idx] * pesos_banda$w)), by = draw]
      }))
    }))
  }))
  data.table::setcolorder(out, c("location_id", "sex_id", "age_group_id", "draw", "val"))
  data.table::setorder(out, location_id, sex_id, age_group_id, draw)
  out
}

# Prevalencia p por banda del ancla y simulación.
.dl_q_bandas <- function(f, b, locs = NULL) .dl_agregar_bandas(f, b, .dl_bandas_ancla(b), locs = locs)

# Incidencia poblacional i (1 - p) (casos nuevos por persona-año de toda la población) por banda y simulación.
.dl_ipop_bandas <- function(f, b, bandas = .dl_bandas_ancla(b), locs = NULL)
  .dl_agregar_bandas(f, b, bandas, function(dq) dq$ipop, locs = locs)

# ---- Población de bandas agregadas ------------------------------------------------------------------------------

# Población por (ubicación, sexo, banda pedida): N_B = suma de N_b sobre las bandas finas b contenidas en la banda
# pedida B (age_start_b >= age_start_B y age_end_b <= age_end_B, con los límites del catálogo; la banda 22 = [0, 125)
# las contiene a todas). La usan la regla promedio_cierra_ancla y las pruebas de la cascada.
.dl_poblacion_en_bandas <- function(pobl, ids_banda, catalogo) {
  if (is.null(catalogo)) .dl_stop("falta el cat\u00e1logo de bandas de edad")
  if (!length(ids_banda)) return(pobl[0L, list(location_id, sex_id, age_group_id, pob = val)])
  finas <- .dl_bandas_finas(catalogo[age_group_id %in% unique(pobl$age_group_id)])
  data.table::rbindlist(lapply(as.integer(ids_banda), function(banda) {
    limites <- .dl_limites_bandas(catalogo, banda, "pedida")
    dentro <- finas[age_start >= limites$age_start & age_end <= limites$age_end, age_group_id]
    if (!length(dentro))
      .dl_stop("ninguna banda de la poblaci\u00f3n cabe en la banda %s", banda)
    pobl[age_group_id %in% dentro, list(age_group_id = banda, pob = sum(val)), by = list(location_id, sex_id)]
  }))
}

# Tabla de proxies con la banda garantizada: una tabla sin gradiente por edad (sin columna age_group_id, o con NA) es
# la banda «todas las edades» (.DL_BANDA_TODAS_LAS_EDADES). La tabla de los insumos no se modifica (su hash es el de
# la tabla leída): se copia solo si hace falta. Único lugar de la convención; la usan .dl_dX (cascada) y la regla
# promedio_cierra_ancla.
.dl_proxy_con_banda <- function(px) {
  if ("age_group_id" %in% names(px) && !anyNA(px$age_group_id)) return(px)
  px <- data.table::copy(px)
  if (!"age_group_id" %in% names(px)) px[, age_group_id := .DL_BANDA_TODAS_LAS_EDADES]
  px[is.na(age_group_id), age_group_id := .DL_BANDA_TODAS_LAS_EDADES]
  px
}

# ---- Resumen de las simulaciones por banda ----------------------------------------------------------------------

# Resumen por (ubicación,) sexo y banda de una tabla de simulaciones (..., draw, val):
#   val          media de las simulaciones (.DL_ESTADISTICO_PUNTUAL);
#   lower, upper cuantiles cola = (1 - ui) / 2 y 1 - cola de las simulaciones (stats::quantile, tipo 7).
.dl_stats_bandas <- function(draws_banda, ui = 0.95) {
  cola <- (1 - ui) / 2
  by <- intersect(c("location_id", "sex_id", "age_group_id"), names(draws_banda))
  draws_banda[, list(val = mean(val),
                     lower = stats::quantile(val, cola, names = FALSE),
                     upper = stats::quantile(val, 1 - cola, names = FALSE)),
              by = by]
}

# El mismo resumen desde una tabla de simulaciones en columnas (location_id, sex_id, age_group_id, draw_1..draw_n),
# como se guardan en disco.
.dl_stats_desde_ancho <- function(draws_ancho, ui = 0.95) {
  largo <- data.table::melt(draws_ancho, id.vars = c("location_id", "sex_id", "age_group_id"),
                            variable.name = "draw", value.name = "val")
  .dl_stats_bandas(largo, ui)
}
