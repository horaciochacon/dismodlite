# Traducción del contrato al formato completo: las tablas del contrato de un proyecto (`tablas`: lista nombrada de
# dl_tabla; las opcionales pueden faltar) y la configuración traducida `cfg` -> las tablas del formato completo que
# .dl_escribir_traduccion (R/proyecto.R) escribe en la carpeta temporal. Los números leídos se escriben con el texto
# más corto que vuelve al mismo double (.dl_num_exacto); las sumas, como fwrite (.dl_num_texto), igual que la
# traducción del formato simple de la 1.0.0. Una función pequeña por tabla (.dl_trad_<tabla>), con las columnas, su
# orden y el orden de las filas de la función del formato simple de la que sale (R/formato_simple.R).
#
# Bandas de edad: el contrato las da como [edad_inicio, edad_fin) (banda abierta: edad_fin = .DL_EDAD_ABIERTA). Por
# dentro, cada banda lleva un age_group_id: el del grupo de edad de GBD con esos límites o, si no lo es, uno sintético
# (.dl_bandas_proyecto: .DL_ID_BANDA_SINTETICA + su rango en orden de edad). El ancla se lleva a las bandas de la
# población: las de menos de un año de GBD a [0, 1) por su ancho (.dl_agrupar_menores_1) y las más finas que la
# población con poblacion_detalle (.dl_agrupar_ancla); con la clave experta anchor.agrupar_bandas_finas = true no se
# agrupa aquí: lo hace dl_insumos() como en la versión 0.2.2.

# Primer id de las bandas de edad sintéticas (las que no son un grupo de edad de GBD). Muy por encima de los ids de
# GBD (< 1000) y de los 9002xx que usaban los proyectos de la versión 0.2.2.
.DL_ID_BANDA_SINTETICA <- 990000L

# medida del ancla del contrato -> su slug en .DL_MEDIDAS (de ahí sale el measure_id de GBD).
.DL_MEDIDA_ANCLA <- c(prevalencia = "prevalence", incidencia = "incidence", avd = "yld", mortalidad = "csmr")

# efecto_sobre de la tabla betas -> el parametro de la extracción del formato completo.
.DL_EFECTO_PARAMETRO <- c(prevalencia = "Prevalence", incidencia = "Incidence",
                          mortalidad_exceso = "Excess mortality rate")

# componente de la tabla fuentes_gbd -> component_id de la evidencia del GHDx (los de .DL_GHDX_COMPONENTE, R/insumos.R:
# 5 fuentes no fatales, 4 causas de muerte).
.DL_COMPONENTE_FUENTE <- c(no_fatal = 5L, causa_de_muerte = 4L)

# Tolerancia con que las bandas de menos de un año de GBD deben sumar un año de ancho (sus límites son fracciones de
# año escritas con 15 cifras).
.DL_TOLERANCIA_PRIMER_ANIO <- 1e-9

# Las tablas del contrato `tablas` para `cfg`, en el formato completo: ancla, poblacion, pesos_80 (solo con
# anchor.agrupar_bandas_finas), cov, extraccion, proxies, datos, severidad, nombres_loc, más fuentes (list.csv), bandas
# (las sintéticas, para el catálogo de edades y las etiquetas) e ids_covariable (covariable -> covariate_id). Cada paso
# va por `paso(nombre, expr)`, como en .dl_traducir_tablas: un paso que falla deja NULL y los que lo necesitan no
# corren.
.dl_traducir_contrato <- function(tablas, cfg, paso = function(nombre, expr) expr) {
  x <- list()
  x$bandas <- .dl_bandas_proyecto(tablas)
  x$poblacion <- paso("poblacion", .dl_trad_poblacion(tablas, cfg, x$bandas))
  if (!is.null(x$poblacion))
    x$ancla <- paso("ancla", .dl_trad_ancla(tablas, cfg, x$bandas))
  if (isTRUE(cfg$anchor$agrupar_bandas_finas) && !is.null(x$ancla))
    x$pesos_80 <- paso("poblacion", .dl_pesos_80_simple(x$poblacion$tabla, cfg, x$ancla))
  if (!is.null(tablas$betas)) {
    x$cov <- paso("covariables", .dl_trad_covariables(tablas, cfg, x$bandas))
    if (!is.null(x$cov)) x$extraccion <- .dl_trad_betas(tablas, cfg, x$cov)
    if (length(cfg$covariables) && !is.null(x$cov))
      x$proxies <- paso("covariables", .dl_trad_proxies(tablas, cfg, x$cov, x$bandas))
  }
  x$nombres_loc <- .dl_nombres_ubicaciones(tablas)
  if (!is.null(tablas$datos)) x$datos <- paso("datos", .dl_trad_datos(tablas, cfg, x$nombres_loc))
  if (!is.null(tablas$severidad)) x$severidad <- paso("severidad", .dl_trad_severidad(tablas, cfg))
  if (!is.null(tablas$fuentes_gbd)) x$fuentes <- .dl_trad_fuentes(tablas, cfg)
  x$ids_covariable <- x$cov$ids
  x
}

# ---- Bandas de edad ----

# Nombre de la banda [a0, a1) como en los mensajes: «30-44 años», «85 años y más».
.dl_nombre_banda <- function(a0, a1)
  ifelse(a1 < .DL_EDAD_ABIERTA, sprintf("%g-%g a\u00f1os", a0, a1 - 1), sprintf("%g a\u00f1os y m\u00e1s", a0))

# Bandas de edad del proyecto que no son grupos de GBD (de la población, del ancla y de poblacion_detalle), con su id
# sintético en orden de (edad_inicio, edad_fin) y su nombre (.dl_nombre_banda).
.dl_bandas_proyecto <- function(tablas) {
  todas <- data.table::rbindlist(lapply(tablas[intersect(c("poblacion", "ancla", "poblacion_detalle"), names(tablas))],
    function(d) if (all(c("edad_inicio", "edad_fin") %in% names(d)))
      unique(data.table::data.table(edad_inicio = d$edad_inicio, edad_fin = d$edad_fin))))
  if (!nrow(todas)) todas <- data.table::data.table(edad_inicio = numeric(), edad_fin = numeric())
  b <- unique(todas)[!.dl_es_banda_gbd(edad_inicio, edad_fin)]
  data.table::setorder(b, edad_inicio, edad_fin)
  b[, `:=`(age_group_id = .DL_ID_BANDA_SINTETICA + seq_len(.N), nombre = .dl_nombre_banda(edad_inicio, edad_fin))][]
}

# age_group_id de cada banda [a0, a1): el grupo de edad de GBD o, si no lo es, su id en `bandas`
# (.dl_bandas_proyecto). Sin `bandas`, se arman de las mismas edades.
.dl_id_banda <- function(a0, a1, bandas = NULL) {
  id <- .dl_grupo_edad(a0, a1)
  if (!anyNA(id)) return(as.integer(id))
  bandas <- bandas %||% .dl_bandas_proyecto(list(poblacion = data.table::data.table(edad_inicio = a0, edad_fin = a1)))
  k <- is.na(id)
  id[k] <- bandas$age_group_id[match(paste(a0[k], a1[k]), paste(bandas$edad_inicio, bandas$edad_fin))]
  as.integer(id)
}

# Nombre de cada age_group_id `id`: el del catálogo de GBD o el de la banda sintética en `bandas`.
.dl_nombre_grupo <- function(id, bandas) {
  g <- .dl_catalogo_referencia("demograficos")[tabla == "age_group"]
  n <- g$name[match(as.character(id), g$id)]
  k <- is.na(n)
  n[k] <- bandas$nombre[match(id[k], bandas$age_group_id)]
  n
}

# ---- Ubicaciones ----

# Código de la ubicación nacional de la tabla ubicaciones: la que no tiene padre.
.dl_ubicacion_nacional <- function(tablas) {
  u <- tablas$ubicaciones
  padre <- if ("padre" %in% names(u)) u$padre else rep(NA_character_, nrow(u))
  u$ubicacion[is.na(padre)][1L]
}

# location_id y location_name de cada ubicación (sin nombre, su código).
.dl_nombres_ubicaciones <- function(tablas) {
  u <- tablas$ubicaciones
  nombre <- if ("nombre" %in% names(u)) u$nombre else rep(NA_character_, nrow(u))
  data.table::data.table(location_id = u$ubicacion, location_name = data.table::fifelse(is.na(nombre), u$ubicacion,
                                                                                        nombre))
}

# Filas de `d` que son de la ubicación `nacional`: sin la columna ubicacion, todas (regla de omisión del eje).
.dl_es_nacional <- function(d, nacional)
  if (!"ubicacion" %in% names(d)) rep(TRUE, nrow(d)) else is.na(d$ubicacion) | d$ubicacion == nacional

# Filas de `d` de la causa `causa`: sin la columna causa, todas; con ella, las de la causa y las sin causa.
.dl_filas_de_causa <- function(d, causa) {
  if (!"causa" %in% names(d)) return(d)
  k <- is.na(d$causa) | d$causa == causa      # fuera de d[]: dentro, `causa` sería la columna
  d[k]
}

# ---- Población ----

# poblacion -> la tabla poblacion del formato completo (.dl_poblacion_simple: mismas columnas y orden) y los nombres.
# Sin filas nacionales, son la suma de las demás; con anchor.agrupar_bandas_finas, se agrega la banda 80+ (la suma de
# 80-84 a 95+) si no viene, como en la versión 0.2.2. Las sumas se escriben como fwrite (exactas con conteos enteros).
.dl_trad_poblacion <- function(tablas, cfg, bandas) {
  p <- tablas$poblacion
  out <- data.table::data.table(location_id = p$ubicacion, year = as.character(p$anio),
                                sex_id = .dl_codigos_sexo(p$sexo),
                                age_group_id = .dl_id_banda(p$edad_inicio, p$edad_fin, bandas),
                                val = .dl_num_exacto(p$poblacion), a0 = p$edad_inicio, a1 = p$edad_fin)
  anio <- as.character(.dl_anio_ajuste(cfg))
  falta <- setdiff(unlist(cfg$sexos), out$sex_id[out$year == anio])
  if (length(falta))
    .dl_stop(paste0("la tabla poblacion no trae la poblaci\u00f3n de %s para %s (el a\u00f1o que se estima; ",
                    "a\u00f1os que trae: %s)"),
             .dl_nombres_sexo(falta), anio, .dl_lista(out$year[out$sex_id %in% falta]))
  sumar <- function(x, por) {
    s <- x[, list(v = sum(as.numeric(val))), by = por]
    s[, val := .dl_num_texto(v)][, v := NULL][]
  }
  nacional <- .dl_loc_ancla(cfg)
  if (!any(out$location_id == nacional))
    out <- rbind(sumar(out, c("year", "sex_id", "age_group_id", "a0", "a1"))[, location_id := nacional], out,
                 use.names = TRUE)
  finas <- .DL_FINAS_80$ids
  if (isTRUE(cfg$anchor$agrupar_bandas_finas) && !any(out$age_group_id == .DL_FINAS_80$id_salida)) {
    f <- out[age_group_id %in% finas]
    f <- f[f[, .N, by = list(location_id, year, sex_id)][N == length(finas)], on = c("location_id", "year", "sex_id")]
    out <- rbind(out, sumar(f, c("location_id", "year", "sex_id"))[, `:=`(
      age_group_id = .DL_FINAS_80$id_salida, a0 = 80, a1 = .DL_EDAD_ABIERTA)], use.names = TRUE)
  }
  out[, location_level := .dl_nivel_simple(location_id, cfg)]
  # el orden del formato completo: la nacional primero; una banda antes que las más finas que contiene
  data.table::setorder(out, location_level, location_id, year, sex_id, a0, -a1)
  list(tabla = out[, list(location_id, location_level, year, sex_id, age_group_id, val, acquisition_id = "poblacion")],
       nombres = .dl_nombres_ubicaciones(tablas))
}

# ---- Ancla ----

# ancla -> el ancla del formato completo (.dl_ancla_simple) de la causa de `cfg`, en la ubicación nacional. Sin
# anchor.agrupar_bandas_finas, en las bandas de la población (.dl_agrupar_menores_1 y .dl_agrupar_ancla); las bandas
# enteramente por debajo de edad_inicio pasan tal cual, sin esas comprobaciones: dl_insumos() las deja fuera y lo dice.
.dl_trad_ancla <- function(tablas, cfg, bandas) {
  a <- data.table::as.data.table(as.data.frame(.dl_filas_de_causa(tablas$ancla, cfg$cause_id)))
  if (!nrow(a))
    .dl_stop("la tabla ancla no trae filas de la causa %d (causas que trae: %s): agr\u00e9galas o revisa la causa",
             cfg$cause_id, .dl_lista(tablas$ancla$causa))
  if (!isTRUE(cfg$anchor$agrupar_bandas_finas)) {
    bajo <- a$edad_fin <= as.numeric(cfg$edad_inicio)
    pob <- unique(data.table::data.table(edad_inicio = tablas$poblacion$edad_inicio,
                                         edad_fin = tablas$poblacion$edad_fin))
    a <- rbind(a[bajo], .dl_agrupar_ancla(.dl_agrupar_menores_1(a[!bajo]), pob, tablas$poblacion_detalle,
                                          .dl_anio_ancla(cfg)))
  }
  .dl_ancla_completa(a, cfg, tablas, bandas)
}

# Columnas del eje y de la clave del ancla `a` que no son la edad ni los valores: las que agrupan al agregar bandas.
.dl_por_ancla <- function(a) setdiff(names(a), c("edad_inicio", "edad_fin", "valor", "inferior", "superior"))

# Bandas de GBD de menos de un año (0-6 días, 7-27 días, 1-5 meses, 6-11 meses) -> [0, 1) (id 28), con peso igual a su
# ancho en años (población uniforme dentro del primer año: la aproximación declarada de .DL_FINAS_5):
#   w_b = a1_b - a0_b,   val_[0,1) = sum_b w_b val_b / sum_b w_b      (igual lower y upper)
# Error si no cubren el primer año entero.
.dl_agrupar_menores_1 <- function(a) {
  k <- a$edad_fin <= 1 & !(a$edad_inicio == 0 & a$edad_fin == 1)
  if (!any(k)) return(a)
  finas <- a[k][, w := edad_fin - edad_inicio]
  por <- .dl_por_ancla(a)
  agr <- finas[, list(valor = sum(valor * w) / sum(w), inferior = sum(inferior * w) / sum(w),
                      superior = sum(superior * w) / sum(w), ancho = sum(w)), by = por]
  if (any(abs(agr$ancho - 1) > .DL_TOLERANCIA_PRIMER_ANIO))
    .dl_stop(paste0("ancla: las bandas de menos de un a\u00f1o no cubren el primer a\u00f1o entero (suman %g ",
                    "a\u00f1os); trae todas (0-6 d\u00edas, 7-27 d\u00edas, 1-5 meses y 6-11 meses) o la banda de ",
                    "menos de un a\u00f1o"),
             agr$ancho[abs(agr$ancho - 1) > .DL_TOLERANCIA_PRIMER_ANIO][1L])
  agr[, `:=`(edad_inicio = 0, edad_fin = 1, ancho = NULL)]
  rbind(a[!k], agr[, names(a), with = FALSE])
}

# Cómo cae la banda [a0, a1) del ancla en las bandas de la población `bandas` (que reparten las edades):
#   k > 0  está dentro de la banda k (igual a ella o más fina: se agrupa en ella);
#   0      es la unión exacta de varias bandas (empieza en el inicio de una, termina en el fin de otra y toda banda que
#          toca queda dentro): se usa tal cual;
#   NA     cruza el límite de una banda.
.dl_banda_contenedora <- function(a0, a1, bandas) {
  k <- which(bandas$edad_inicio <= a0 & bandas$edad_fin >= a1)
  if (length(k) == 1L) return(k)
  toca <- bandas$edad_inicio < a1 & bandas$edad_fin > a0
  dentro <- bandas$edad_inicio >= a0 & bandas$edad_fin <= a1
  if (a0 %in% bandas$edad_inicio && a1 %in% bandas$edad_fin && all(dentro[toca])) return(0L)
  NA_integer_
}

# Agrupa las bandas del ancla `a` (de una causa, un año y un sexo por grupo) que son más finas que la población en la
# banda de la población que las contiene. Para cada banda B de la población que contiene estrictamente bandas b del
# ancla:
#   w_b    = N_b / sum_{b' en B} N_b'         N_b: población de b en poblacion_detalle (año del ancla, mismo sexo)
#   val_B  = sum_b w_b val_b,   lower_B = sum_b w_b lower_b,   upper_B = sum_b w_b upper_b
# que se calcula como sum_b(val_b N_b) / sum_b N_b. Promediar los límites en escala natural es la aproximación
# declarada de .dl_agregar_finas (R/insumos.R). Una banda del ancla que es la unión de varias de la población queda
# tal cual. Error si una banda del ancla sale de las edades de la población o cruza el límite de una de sus bandas, o
# si hace falta poblacion_detalle y no la hay.
.dl_agrupar_ancla <- function(a, bandas_pob, detalle, anio_ancla) {
  fuera <- a$edad_inicio < min(bandas_pob$edad_inicio) | a$edad_fin > max(bandas_pob$edad_fin)
  if (any(fuera))
    .dl_stop(paste0("la poblaci\u00f3n no cubre la banda %s del ancla (la poblaci\u00f3n va de %g a %s a\u00f1os): ",
                    "agrega esas edades a la poblaci\u00f3n o quita la banda del ancla"),
             .dl_nombre_banda(a$edad_inicio[fuera][1L], a$edad_fin[fuera][1L]), min(bandas_pob$edad_inicio),
             if (max(bandas_pob$edad_fin) >= .DL_EDAD_ABIERTA) "m\u00e1s" else format(max(bandas_pob$edad_fin)))
  B <- vapply(seq_len(nrow(a)), function(i) .dl_banda_contenedora(a$edad_inicio[i], a$edad_fin[i], bandas_pob), 1L)
  if (anyNA(B))
    .dl_stop(paste0("ancla: la banda %s cruza el l\u00edmite de una banda de la poblaci\u00f3n (no est\u00e1 dentro ",
                    "de una ni es una uni\u00f3n de ellas); usa en la poblaci\u00f3n bandas que la contengan enteras"),
             .dl_nombre_banda(a$edad_inicio[is.na(B)][1L], a$edad_fin[is.na(B)][1L]))
  B0 <- ifelse(B > 0L, bandas_pob$edad_inicio[pmax(B, 1L)], a$edad_inicio)
  B1 <- ifelse(B > 0L, bandas_pob$edad_fin[pmax(B, 1L)], a$edad_fin)
  fina <- a$edad_inicio != B0 | a$edad_fin != B1
  if (!any(fina)) return(a)
  finas <- a[fina][, `:=`(B0 = B0[fina], B1 = B1[fina])]
  if (is.null(detalle))
    .dl_stop(paste0("la poblaci\u00f3n trae %s y el ancla %s: agrega poblacion_detalle (poblaci\u00f3n nacional por ",
                    "sexo y edad con ese detalle, por ejemplo de GBD o de la ONU), o una poblaci\u00f3n con ese ",
                    "detalle en todas las ubicaciones"),
             paste(unique(.dl_nombre_banda(finas$B0, finas$B1)), collapse = ", "),
             paste(unique(.dl_nombre_banda(finas$edad_inicio, finas$edad_fin)), collapse = ", "))
  finas[, N := .dl_poblacion_detalle(finas, detalle, anio_ancla)]
  por <- c(.dl_por_ancla(a), "B0", "B1")
  agr <- finas[, list(valor = sum(valor * N) / sum(N), inferior = sum(inferior * N) / sum(N),
                      superior = sum(superior * N) / sum(N)), by = por]
  data.table::setnames(agr, c("B0", "B1"), c("edad_inicio", "edad_fin"))
  rbind(a[!fina], agr[, names(a), with = FALSE])
}

# N_b de cada fila de `finas` (sexo, edad_inicio, edad_fin): su población en poblacion_detalle del año del ancla.
.dl_poblacion_detalle <- function(finas, detalle, anio_ancla) {
  det <- detalle[detalle$anio == anio_ancla]
  N <- det$poblacion[match(paste(finas$sexo, finas$edad_inicio, finas$edad_fin),
                           paste(det$sexo, det$edad_inicio, det$edad_fin))]
  if (anyNA(N))
    .dl_stop("poblacion_detalle no trae la poblaci\u00f3n de %d de las bandas del ancla %s", anio_ancla,
             paste(unique(sprintf("%s %s", finas$sexo[is.na(N)],
                                  .dl_nombre_banda(finas$edad_inicio[is.na(N)], finas$edad_fin[is.na(N)]))),
                   collapse = ", "))
  N
}

# El ancla `a` (en el contrato) con las columnas del formato completo de .dl_ancla_simple, todas como texto. La
# métrica es «Contrato» (.DL_METRICA_CONTRATO: los valores ya están en proporción o por persona-año).
.dl_ancla_completa <- function(a, cfg, tablas, bandas) {
  med <- .DL_MEDIDAS[match(.DL_MEDIDA_ANCLA[a$medida], .DL_MEDIDAS$slug)]
  id <- .dl_id_banda(a$edad_inicio, a$edad_fin, bandas)
  sexo <- .dl_codigos_sexo(a$sexo)
  sx <- .dl_sexos_ref()
  nombre <- if ("nombre_causa" %in% names(a)) a$nombre_causa else rep(NA_character_, nrow(a))
  nombre[is.na(nombre)] <- cfg$origen$nombre %||% as.character(cfg$cause_id)
  nacional <- .dl_loc_ancla(cfg)
  nl <- .dl_nombres_ubicaciones(tablas)
  nombre_loc <- nl$location_name[match(nacional, nl$location_id)]
  if (is.na(nombre_loc)) nombre_loc <- nacional
  data.table::data.table(
    acquisition_id = "ancla", source = .DL_STD_SOURCE, round = as.character(max(a$anio)), entity = "cause",
    location_id = nacional, location_name = nombre_loc,
    location_level = "0", year = as.character(a$anio), age_group_id = as.character(id),
    age_group_name = .dl_nombre_grupo(id, bandas), sex_id = as.character(sexo),
    sex_name = sx$name[match(sexo, as.integer(sx$id))], cause_id = as.character(cfg$cause_id), cause_name = nombre,
    measure_id = as.character(med$measure_id_gbd), measure_name = NA_character_, metric_id = NA_character_,
    metric_name = .DL_METRICA_CONTRATO$metric_std, val = .dl_num_exacto(a$valor), lower = .dl_num_exacto(a$inferior),
    upper = .dl_num_exacto(a$superior), ui_level = "0.95")
}

# ---- Covariables y betas ----

# Filas de la tabla betas que valen para `causa`: las suyas y las sin causa; una causa hija sin filas propias toma las
# de su `padre`. Sin la columna causa, todas. NULL sin tabla.
.dl_betas_de_causa <- function(betas, causa, padre = NULL) {
  if (is.null(betas) || !"causa" %in% names(betas)) return(betas)
  propia <- function(cc) !is.na(betas$causa) & betas$causa == cc
  k <- propia(causa)
  if (!any(k) && !is.null(padre)) k <- propia(padre)
  betas[k | is.na(betas$causa)]
}

# covariate_id de cada covariable de `betas` y de cada una que nombra valor_nacional_de (lista nombrada): el
# covariable_id de su fila nacional en la tabla covariables (lo llena el lector del GHDx) o, sin él, su posición en esa
# lista (la de betas primero).
.dl_ids_covariable <- function(tablas, betas = tablas$betas) {
  vn <- betas$valor_nacional_de
  nombres <- unique(c(betas$covariable, vn[!is.na(vn)]))
  if (!length(nombres)) return(list())
  cov <- tablas$covariables
  nac <- if (!is.null(cov) && "covariable_id" %in% names(cov))
    cov[.dl_es_nacional(cov, .dl_ubicacion_nacional(tablas)) & !is.na(cov$covariable_id)]
  stats::setNames(lapply(seq_along(nombres), function(k) {
    v <- unique(nac$covariable_id[nac$covariable == nombres[k]])
    if (length(v) == 1L) as.integer(v) else k
  }), nombres)
}

# Columna `cn` de `d`, o `defecto` en cada fila si no la trae.
.dl_col <- function(d, cn, defecto = NA) if (cn %in% names(d)) d[[cn]] else rep(defecto, nrow(d))

# Códigos de sexo de `d` (sin la columna o vacío: ambos, 3) y age_group_id (sin edades: todas, 22).
.dl_sexo_edad_cov <- function(d, bandas) {
  sexo <- .dl_codigos_sexo(.dl_col(d, "sexo", "ambos"))
  sexo[is.na(sexo)] <- .DL_CASCADA_SEXO_AMBOS
  a0 <- .dl_col(d, "edad_inicio", NA_real_)
  edad <- rep(.DL_BANDA_TODAS_LAS_EDADES, nrow(d))
  con <- !is.na(a0)
  edad[con] <- .dl_id_banda(a0[con], .dl_col(d, "edad_fin", NA_real_)[con], bandas)
  list(sexo = sexo, edad = edad)
}

# covariables y betas -> ids (covariable -> covariate_id), nacional (los valores nacionales en el formato de la descarga
# del GHDx, para la carpeta covariables/ de la traducción) y betas (las de la causa). NULL si la causa no tiene betas.
.dl_trad_covariables <- function(tablas, cfg, bandas) {
  betas <- .dl_betas_de_causa(tablas$betas, cfg$cause_id, cfg$extraction$cause_id)
  if (is.null(betas) || !nrow(betas)) return(NULL)
  if (is.null(tablas$covariables))
    .dl_stop("la tabla betas trae las covariables %s y falta la tabla covariables (sus valores nacionales)",
             .dl_lista(betas$covariable))
  ids <- .dl_ids_covariable(tablas, betas)
  nacional <- .dl_loc_ancla(cfg)
  cov <- tablas$covariables
  d <- cov[.dl_es_nacional(cov, nacional) & cov$covariable %in% names(ids)]
  faltan <- setdiff(names(ids), d$covariable)
  if (length(faltan))
    .dl_stop("la tabla covariables no trae el valor nacional (ubicaci\u00f3n %s) de %s, que usa la tabla betas",
             nacional, paste(faltan, collapse = ", "))
  se <- .dl_sexo_edad_cov(d, bandas)
  anio <- .dl_col(d, "anio", NA_integer_)
  anio[is.na(anio)] <- .dl_anio_ancla(cfg)
  ghdx <- data.table::data.table(
    covariate_id = vapply(d$covariable, function(n) ids[[n]], 1L, USE.NAMES = FALSE),
    covariate_name_short = d$covariable,
    location_id = nacional, year_id = as.integer(anio), age_group_id = as.integer(se$edad),
    sex_id = as.integer(se$sexo),
    mean_value = .dl_num_exacto(d$valor), lower_value = .dl_num_exacto(.dl_col(d, "inferior", NA_real_)),
    upper_value = .dl_num_exacto(.dl_col(d, "superior", NA_real_)))
  list(ids = ids, nacional = ghdx, betas = betas)
}

# La extracción (el YAML de las betas) desde las betas de la causa (`cov$betas`, en el orden de sus filas), con cada
# beta como el texto más corto de su número; el intervalo, solo si trae los dos límites (sin él, la beta es fija).
.dl_trad_betas <- function(tablas, cfg, cov) {
  b <- cov$betas
  inf <- .dl_col(b, "inferior", NA_real_)
  sup <- .dl_col(b, "superior", NA_real_)
  list(meta = list(causa_gbd = list(cause_id = .dl_extraction_cause_id(cfg)), fuente = .DL_PROCEDENCIA_SIMPLE),
       covariables_gbd = lapply(seq_len(nrow(b)), function(k) {
         c(list(covariate_id = as.character(cov$ids[[b$covariable[k]]]), covariate_name_short = b$covariable[k],
                nombre_impreso = b$covariable[k], nivel = "pais", rol = "predictiva",
                parametro = .DL_EFECTO_PARAMETRO[[b$efecto_sobre[k]]], beta_valor = .dl_num_exacto(b$beta[k])),
           if (!is.na(inf[k]) && !is.na(sup[k]))
             list(beta_inferior = .dl_num_exacto(inf[k]), beta_superior = .dl_num_exacto(sup[k])))
       }))
}

# Error estándar de cada fila de covariables `d`: error_estandar o, sin él, (superior - inferior) / 3,92 (el
# intervalo del 95 % como media ± 1,96 sd), escrito como fwrite. Error si una fila no trae ninguno.
.dl_se_covariable <- function(d) {
  se <- .dl_num_exacto(.dl_col(d, "error_estandar", NA_real_))
  k <- is.na(se)
  se[k] <- .dl_num_texto((.dl_col(d, "superior", NA_real_)[k] - .dl_col(d, "inferior", NA_real_)[k]) /
                           .DL_ANCHO_IC95_EN_SD)
  if (anyNA(se))
    .dl_stop("la tabla covariables no trae error_estandar (ni inferior y superior) de %s en %s",
             d$covariable[is.na(se)][1L], d$ubicacion[is.na(se)][1L])
  se
}

# Filas subnacionales de covariables -> tabla cov_proxy (.dl_proxies_simple: mismas columnas y orden), las de las
# covariables con proxy en cfg$covariables y el año que se estima. ancla_ghdx: el valor nacional (año del ancla; mismo
# sexo o ambos) de la covariable o de su valor_nacional_de (covariables[].sustituye), en que debe cerrar el promedio
# ponderado (regla promedio_cierra_ancla); covariate_id_gbd es el id de esa misma covariable.
.dl_trad_proxies <- function(tablas, cfg, cov, bandas) {
  decl <- vapply(cfg$covariables, function(cv) cv$covariate_name_short, "")
  ref <- vapply(cfg$covariables, function(cv) cv$sustituye$covariate_name_short %||% cv$covariate_name_short, "")
  ids_proxy <- vapply(cfg$covariables, function(cv) as.integer(cv$proxy$covariate_id_proxy), 1L)
  sin_id <- setdiff(ref, names(cov$ids))
  if (length(sin_id))
    .dl_stop(paste0("el valor nacional de los proxies sale de %s, que no est\u00e1 en la tabla betas (ni en su ",
                    "valor_nacional_de): agr\u00e9gala como valor_nacional_de de la covariable que ancla"),
             paste(sin_id, collapse = ", "))
  c0 <- tablas$covariables
  d <- c0[!.dl_es_nacional(c0, .dl_loc_ancla(cfg)) & c0$covariable %in% decl]
  ajuste <- .dl_anio_ajuste(cfg)
  anios <- .dl_col(d, "anio", NA_integer_)        # no `anio`: dentro de d[] sería la columna
  anios[is.na(anios)] <- ajuste
  if (!ajuste %in% anios)
    .dl_stop(paste0("la tabla covariables no trae filas subnacionales de %d (el a\u00f1o que se estima) de las ",
                    "covariables %s (a\u00f1os que trae: %s). Sin ellas no hay estimaci\u00f3n subnacional por ",
                    "covariables: agrega los valores de ese a\u00f1o o usa subnacional.modo: plano"), ajuste,
             paste(decl, collapse = ", "), .dl_lista(anios))
  d <- d[anios == ajuste]
  se <- .dl_sexo_edad_cov(d, bandas)
  r <- ref[match(d$covariable, decl)]
  out <- data.table::data.table(
    covariate_id_proxy = ids_proxy[match(d$covariable, decl)], covariate_id_gbd = as.integer(unlist(cov$ids[r])),
    location_id = d$ubicacion, year = as.character(ajuste), sex_id = as.integer(se$sexo),
    age_group_id = as.integer(se$edad), valor_crudo = .dl_num_exacto(d$valor),
    valor_calibrado = .dl_num_exacto(d$valor),
    valor_calibrado_se = .dl_se_covariable(d), metodo_calibracion = "valores_de_proxies_csv",
    ancla_ghdx = .dl_ancla_proxies(cov$nacional, r, se$sexo, .dl_anio_ancla(cfg)), acquisition_id = "proxies")
  data.table::setorder(out, covariate_id_proxy, year, sex_id, age_group_id, location_id)
}

# El valor nacional (texto de mean_value en `nac`, el GHDx de .dl_trad_covariables) de la covariable `r[i]` en `anio`,
# del sexo `sexo[i]` o de ambos; uno solo por covariable y sexo.
.dl_ancla_proxies <- function(nac, r, sexo, anio) {
  nac <- nac[year_id == anio]
  clave <- paste(r, sexo)
  una <- which(!duplicated(clave))
  valor <- vapply(una, function(i) {
    x <- .dl_filas_del_sexo(nac[covariate_name_short == r[i]], sexo[i])
    if (nrow(x) != 1L)
      .dl_stop(paste0("la tabla covariables trae %d valor(es) nacional(es) de %s de %d (el a\u00f1o del ancla) para ",
                      "%s: los proxies necesitan uno"), nrow(x), r[i], anio, .dl_nombres_sexo(sexo[i]))
    x$mean_value
  }, "")
  valor[match(clave, clave[una])]
}

# ---- Datos, severidad y fuentes ----

# datos -> tabla datos (.dl_datos_simple: mismas columnas; dato_id «fila_<n>»): medida -> tipo_dato con el
# vocabulario de datos_en_ajuste (prevalencia_registro -> prev_admin), ubicacion -> location_id; sin causa, la de `cfg`.
.dl_trad_datos <- function(tablas, cfg, nombres_loc) {
  d <- tablas$datos
  col <- function(cn) .dl_col(d, cn)
  num <- function(cn) .dl_num_exacto(as.numeric(col(cn)))
  y0 <- ifelse(is.na(col("anio_inicio")), col("anio"), col("anio_inicio"))
  y1 <- ifelse(is.na(col("anio_fin")), col("anio"), col("anio_fin"))
  if (anyNA(y0) || anyNA(y1))
    .dl_stop("la tabla datos: falta anio (o anio_inicio y anio_fin) en %s",
             paste(utils::head(sprintf("fila_%d", which(is.na(y0) | is.na(y1))), 5L), collapse = ", "))
  tipo <- unname(.dl_tipos_datos_simple()[d$medida])
  med <- .DL_TIPOS_DATO$measure_id[match(tipo, .DL_TIPOS_DATO$tipo)]
  med[tipo == "prev_admin"] <- .dl_medida_id("prevalence")
  sexo <- .dl_codigos_sexo(d$sexo)
  fuente <- col("fuente")
  k <- match(d$ubicacion, nombres_loc$location_id)
  causa <- col("causa")
  data.table::data.table(
    dato_id = sprintf("fila_%d", seq_len(nrow(d))), cause_id = as.character(ifelse(is.na(causa), cfg$cause_id, causa)),
    tipo_dato = tipo, measure_id = med, measure_name = .DL_MEDIDAS$measure_name[match(med, .DL_MEDIDAS$measure_id)],
    location_id = d$ubicacion, location_name = ifelse(is.na(k), d$ubicacion, nombres_loc$location_name[k]),
    location_level = .dl_nivel_simple(d$ubicacion, cfg), sex_id = sexo,
    sex_name = .dl_sexos_ref()$name[match(sexo, as.integer(.dl_sexos_ref()$id))], age_start = num("edad_inicio"),
    age_end = num("edad_fin"), age_group_id = .dl_grupo_edad(d$edad_inicio, d$edad_fin),
    year_start = as.character(y0), year_end = as.character(y1), val = num("valor"), se = num("error_estandar"),
    x = num("casos"), n = num("muestra"), n_efectivo = num("muestra_efectiva"),
    definicion = ifelse(is.na(col("definicion")), d$medida, col("definicion")), es_referencia = TRUE,
    crosswalk_id = NA_character_, ajuste_completitud = FALSE, completitud = NA_character_,
    outlier = as.logical(col("excluir")) %in% TRUE, outlier_motivo = as.character(col("motivo")),
    acquisition_id = ifelse(is.na(fuente), "datos_locales", .dl_slugify(fuente)), nid_ghdx = NA_character_,
    cita = ifelse(is.na(fuente), "datos.csv", fuente))
}

# severidad -> la tabla severidad de la causa (.dl_severidad_simple: mismas columnas), con los pesos de GBD de los
# estados que no los traen (.dl_pesos_gbd); health_state_id: id_estado o el orden de aparición del estado. NULL si la
# tabla no trae la causa. La severidad por edad o sexo la admite el contrato, pero el cálculo de AVD todavía no.
.dl_trad_severidad <- function(tablas, cfg) {
  s <- .dl_filas_de_causa(tablas$severidad, cfg$cause_id)
  if (!nrow(s)) return(NULL)
  varia <- c(length(unique(.dl_col(s, "sexo"))) > 1L,
             nrow(unique(data.table::data.table(.dl_col(s, "edad_inicio"), .dl_col(s, "edad_fin")))) > 1L)
  if (any(varia))
    .dl_stop(paste0("la severidad por edad o sexo todav\u00eda no la usa el c\u00e1lculo de AVD: deja en la tabla ",
                    "severidad una fila por estado de la causa %d, sin sexo ni edades"), cfg$cause_id)
  s <- .dl_pesos_gbd(s)
  ids <- ifelse(is.na(s$id_estado), match(s$estado, unique(s$estado)), s$id_estado)
  data.table::data.table(
    cause_id = as.character(cfg$cause_id), health_state_id = as.character(ids),
    proportion = .dl_num_exacto(s$proporcion), prop_lower = .dl_num_exacto(s$inferior),
    prop_upper = .dl_num_exacto(s$superior), dw_mean = .dl_num_exacto(s$peso_discapacidad),
    dw_lower = .dl_num_exacto(s$peso_inferior), dw_upper = .dl_num_exacto(s$peso_superior),
    beta_covariable = NA_character_, location_id_fuente = .dl_loc_ancla(cfg), fuente = "extraction")
}

# fuentes_gbd -> list.csv de la evidencia del formato completo (cause_id, component_id 5 no fatal / 4 causas de
# muerte, location_id, nid); sin causa, la de `cfg`; sin ubicación, la nacional.
.dl_trad_fuentes <- function(tablas, cfg) {
  f <- tablas$fuentes_gbd
  causa <- .dl_col(f, "causa", NA_integer_)
  ubic <- .dl_col(f, "ubicacion", NA_character_)
  data.table::data.table(cause_id = as.integer(ifelse(is.na(causa), cfg$cause_id, causa)),
                         component_id = unname(.DL_COMPONENTE_FUENTE[f$componente]),
                         location_id = ifelse(is.na(ubic), .dl_loc_ancla(cfg), ubic), nid = f$nid)
}
