# Validación del ajuste contra el ancla (dl_validar_ancla). Notación: ver R/edo.R.
#  - anchor_identity: la prevalencia posterior reproduce la prevalencia GBD del ancla, por sexo y banda. El umbral
#    de la corrida es solo sobre el error relativo mediano; una celda fuera del IC95 se revisa y la decisión se
#    documenta, pero no es por sí sola un error.
#  - implied_incidence: la incidencia poblacional que implica la EDO, i (1 - p), que no entró al ajuste, frente a la
#    incidencia GBD del ancla de incidencia, reservada para validar. Cómo se interpreta depende del tipo de prior de
#    EMR y queda declarado de antemano en la nota.
#  - amplitud_csmr (con una cascada): el gradiente departamental predicho frente al csmr departamental reservado para
#    validar (held-out), que nunca entra a la verosimilitud. Con la EMR fija en cero el modelo predice 0 muertes y la
#    comprobación se omite, con su motivo.

# Comprobación por banda: estadísticos del modelo por sexo y banda frente al valor de referencia `ref`.
.dl_check_bandas <- function(draws_banda, ref, check, nota) {
  st <- .dl_stats_bandas(draws_banda)
  m <- merge(st, ref[, list(sex_id, age_group_id, valor_gbd = val)], by = c("sex_id", "age_group_id"))
  m[, list(check = check, sex_id, age_group_id, valor_modelo = val, valor_gbd,
           err_rel = abs(val - valor_gbd) / valor_gbd,
           cubierto_ic95 = valor_gbd >= lower & valor_gbd <= upper, nota = nota)]
}

# csmr predicho para cada fila de validación: la mediana, sobre las simulaciones, de sum_a w_a p_d(a) f_d(a), con los
# pesos de población w_a de la ubicación d de la fila en su intervalo de edad.
.dl_csmr_predicho <- function(casc, b, filas) {
  # simulaciones de cada ubicación y sexo, ordenadas por simulación y edad, separadas una sola vez
  por_celda <- split(casc$draws_q[order(draw, edad)], by = c("location_id", "sex_id"))
  data.table::rbindlist(lapply(seq_len(nrow(filas)), function(j) {
    r <- filas[j]
    dq <- por_celda[[paste(r$location_id, r$sex_id, sep = ".")]]
    if (is.null(dq))
      .dl_stop("la cascada no tiene simulaciones de la ubicaci\u00f3n %s (sexo %d)",
               r$location_id, as.integer(r$sex_id))
    pobl <- b$poblacion[location_id == r$location_id & sex_id == r$sex_id]
    edades_anual <- sort(unique(dq$edad))
    pesos_banda <- .dl_pesos_intervalo(edades_anual, r$age_start, r$age_end, pobl, b$bandas_pobl)
    v <- dq[, list(v = sum((p * f)[pesos_banda$idx] * pesos_banda$w)), by = draw]
    data.table::data.table(dato_id = r$dato_id, location_id = r$location_id, sex_id = r$sex_id,
                           age_start = r$age_start, age_end = r$age_end, val = stats::median(v$v))
  }))
}

# Correlación de rangos, o NA cuando no hay orden que correlacionar: menos de 3 puntos, o x o y constantes (salvo
# ruido de coma flotante, medido en relación con el tamaño de los valores: tolerancia relativa 1e-9).
.dl_spearman <- function(x, y) {
  if (length(x) < 3L) return(NA_real_)
  tol <- 1e-9
  if (stats::sd(x) <= tol * max(abs(x), 1) || stats::sd(y) <= tol * max(abs(y), 1)) return(NA_real_)
  stats::cor(x, y, method = "spearman")
}

# Validación de amplitud: el gradiente departamental predicho frente al observado en el csmr departamental reservado
# para validar (held-out; nunca entra a la verosimilitud). Por sexo, la pendiente de log(obs_d / obs_nac) sobre
# log(pred_d / pred_nac) (el criterio, declarado de antemano, es que sea cercana a 1) y la correlación de rangos (el
# orden). obs_nac es el agregado poblacional de las observaciones departamentales (la misma adquisición; no depende
# del csmr nacional de la verosimilitud).
.dl_amplitud_csmr <- function(casc, b) {
  held <- b$datos[tipo_dato == "csmr" & location_level == 1L & outlier == FALSE]
  if (!nrow(held)) return(NULL)
  pred_d <- .dl_csmr_predicho(casc, b, held)
  celdas <- unique(held[, list(sex_id, age_start, age_end)])
  nac <- data.table::copy(celdas)[, `:=`(location_id = b$loc_ancla, dato_id = sprintf("nac_%d_%s", sex_id, age_start))]
  pred_n <- .dl_csmr_predicho(casc, b, nac)
  pob <- b$poblacion[location_level == 1L, list(N = sum(val)), by = list(location_id, sex_id)]
  m <- merge(held[, list(dato_id, location_id, sex_id, age_start, age_end, obs = val)],
             pred_d[, list(dato_id, pred = val)], by = "dato_id")
  m <- merge(m, pred_n[, list(sex_id, age_start, age_end, pred_nac = val)], by = c("sex_id", "age_start", "age_end"))
  m <- merge(m, pob, by = c("location_id", "sex_id"))
  m[, obs_nac := sum(obs * N) / sum(N), by = list(sex_id, age_start, age_end)]
  m[, `:=`(y = log(obs / obs_nac), x = log(pred / pred_nac))]
  # orden: Spearman entre departamentos dentro de cada celda (sexo, intervalo), promediado sobre las celdas con al
  # menos 3 departamentos (con 1 no hay correlación y con 2 es +-1 por construcción); un csmr de validación escaso
  # (una causa rara, con celdas de un solo departamento) deja NA, nunca un error.
  rangos <- m[, list(rho = .dl_spearman(x, y)), by = list(sex_id, age_start, age_end)]
  # Versión estandarizada por edad: un punto por departamento y sexo. Tasa estandarizada con la población nacional de
  # las celdas presentes en ese departamento (obs y pred sobre las mismas celdas, así el cociente es comparable
  # aunque el csmr de validación sea ralo); quita el ruido de Poisson de las celdas con pocas muertes.
  pn <- b$poblacion[location_level == 0L, list(w = sum(val)), by = list(sex_id, age_group_id)]
  pn <- merge(pn, b$bandas_pobl[, list(age_group_id, age_start, age_end)], by = "age_group_id")
  ms <- merge(m, pn[, list(sex_id, age_start, age_end, w)], by = c("sex_id", "age_start", "age_end"))
  # Razón sobre el agregado nacional de las mismas celdas del departamento: sin ese cociente, un csmr de validación
  # ralo mide la composición por edad, no el gradiente.
  std <- ms[, list(ys = log(sum(w * obs) / sum(w * obs_nac)), xs = log(sum(w * pred) / sum(w * pred_nac))),
            by = list(sex_id, location_id)]
  # pendiente de y sobre x con su IC95 (NA con menos de 3 puntos)
  pendiente <- function(x, y) {
    if (length(x) < 3L) return(rep(NA_real_, 3))
    fit <- stats::lm(y ~ x); c(unname(stats::coef(fit)[2]), stats::confint(fit)[2, ])
  }
  m[, {
    pe <- pendiente(x, y)
    rh <- rangos[sex_id == .BY$sex_id & !is.na(rho)]$rho
    s <- std[sex_id == .BY$sex_id & is.finite(ys) & is.finite(xs)]
    ps <- pendiente(s$xs, s$ys)
    list(check = "amplitud_csmr", kappa = casc$kappa,
         pendiente = pe[1], pendiente_lower = pe[2], pendiente_upper = pe[3],
         spearman = if (length(rh)) mean(rh) else NA_real_, n_celdas_spearman = length(rh),
         n_celdas = .N, n_departamentos = data.table::uniqueN(location_id),
         pendiente_std = ps[1], pendiente_std_lower = ps[2], pendiente_std_upper = ps[3],
         spearman_std = .dl_spearman(s$xs, s$ys),
         n_departamentos_std = nrow(s))
  }, by = sex_id]
}

#' Validar el ajuste contra el ancla
#'
#' Dos comprobaciones por corrida: `anchor_identity` (la prevalencia posterior reproduce la del ancla, por banda) e
#' `implied_incidence` (la incidencia que implica la ecuación frente a la incidencia de referencia, que no entró al
#' ajuste). Con una cascada y mortalidad subnacional reservada en los datos (las filas de `mortalidad` del año
#' `subnacional.anio_validacion` en `datos.csv`), agrega la validación de la amplitud del gradiente subnacional
#' (atributo `amplitud`).
#'
#' @details
#' - `anchor_identity`: la mediana de la prevalencia de cada banda frente a la del ancla. El error relativo mediano
#'   de las bandas es una compuerta de la corrida: si pasa de 0.05 ([dl_configuracion()], `anchor.gate_err_mediano`;
#'   en un proyecto, `ancla.error_maximo`), [dl_exportar_corrida()] no la escribe. Una banda fuera del intervalo del
#'   95 % del ancla se revisa y la decisión se documenta, pero no es un error por sí sola.
#' - `implied_incidence`: la incidencia poblacional que implica la ecuación, i (1 - p), frente a la incidencia del
#'   ancla (la medida 6 de las descargas; sin ella, un mensaje dice que la comprobación se omite). Es una validación
#'   externa: la incidencia del ancla no entra al ajuste.
#' - `amplitud`: por sexo, la pendiente de log(observado / nacional) sobre log(predicho / nacional) en las
#'   ubicaciones subnacionales con mortalidad reservada (cercana a 1 si la cascada reproduce el tamaño del
#'   gradiente) y la correlación de rangos de Spearman (el orden de las ubicaciones). La mortalidad reservada nunca
#'   entra al ajuste. Con la mortalidad en exceso fija en 0 (`mortalidad_exceso.prior: cero`) el modelo no predice
#'   muertes y esta comprobación se omite; también con el reparto por razón (`subnacional.modo: razon`), que no tiene
#'   un gradiente por covariables que comparar. El atributo `sin_amplitud` dice el motivo, y el manifiesto de la
#'   corrida lo repite solo cuando la mortalidad reservada es de un año distinto del de la corrida.
#'
#' @inheritParams dl_ajustar
#' @param ajuste Ajuste nacional de [dl_ajustar()].
#' @param rutas Rutas de [dl_rutas()]; se usa el ancla de incidencia. Por defecto, las de los insumos.
#' @param cascada Cascada de [dl_cascada()] (opcional): con ella, la validación de la amplitud.
#' @return Tabla (data.table) con una fila por comprobación (`check`), sexo (`sex_id`) y banda (`age_group_id`):
#'   `valor_modelo` (la mediana del modelo), `valor_gbd` (el ancla), `err_rel` (el error relativo), `cubierto_ic95`
#'   (si el valor del modelo cae en el intervalo del 95 % del ancla) y `nota` (cómo se interpreta). Atributos:
#'   - `resumen`: por comprobación, el error relativo mediano (`err_rel_mediano`) y la fracción de bandas cubiertas
#'     (`cobertura`);
#'   - `sin_incidencia_gbd`: `TRUE` si el ancla de incidencia no trae filas de la causa (GBD no publica incidencia
#'     para algunas causas) y la comprobación se omitió;
#'   - `amplitud` (con una cascada por proxies y mortalidad subnacional reservada): por sexo, `kappa`, la
#'     `pendiente` con su intervalo (`pendiente_lower`, `pendiente_upper`), `spearman`, el número de celdas y de
#'     ubicaciones, y las mismas medidas estandarizadas por edad (`*_std`); o `sin_amplitud`: por qué no se
#'     calculó.
#' @seealso [dl_exportar_corrida()] (la compuerta del ancla), [dl_cascada()] y [dl_etiquetas()].
#' @family diagnóstico
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' v <- dl_validar_ancla(f, b, cascada = dl_cascada(f, b, semilla = 1))
#' attr(v, "resumen")
#' head(v[check == "anchor_identity", c("sex_id", "age_group_id", "valor_modelo", "valor_gbd",
#'                                      "err_rel", "cubierto_ic95")])
#' attr(v, "amplitud")[, c("sex_id", "pendiente", "spearman")]
#' }
#' @export
dl_validar_ancla <- function(ajuste, insumos, rutas = insumos$rutas, cascada = NULL) {
  .dl_exigir_clase(ajuste, "dl_fit", "ajuste", "dl_ajustar()")
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  rutas <- .dl_resolver_rutas(rutas, insumos)
  .dl_exigir_mismos_insumos(ajuste, insumos, "ajuste")
  cfg <- insumos$cfg
  ancla <- insumos$prior_gbd[measure_id == .dl_medida_id("prevalence")]
  out <- .dl_check_bandas(.dl_q_bandas(ajuste, insumos, locs = insumos$loc_ancla), ancla, "anchor_identity",
                          paste0("lambda y datos de la corrida tal cual; una celda fuera del IC95 se revisa y la ",
                                 "decisi\u00f3n se documenta"))
  archivo_inc <- .dl_path(rutas, "std_incidence", opcional = TRUE)
  inc <- if (!is.null(archivo_inc))
    .dl_materializar_medida(cfg, "incidence", rutas, archivo = archivo_inc, opcional = TRUE)
  # GBD no publica incidencia para algunas causas (las que se modelan a partir de un deterioro): la incidencia de la
  # corrida queda sin referencia y el manifiesto lo declara (atributo sin_incidencia_gbd).
  sin_inc_gbd <- !is.null(archivo_inc) && is.null(inc)
  if (sin_inc_gbd) {
    .dl_message(paste0("el ancla de incidencia no trae filas de la causa %d (GBD no publica incidencia para ella); ",
                       "se omite la comprobaci\u00f3n implied_incidence"), cfg$cause_id)
  }
  if (!is.null(inc)) {
    nota_inc <- if (identical(cfg$emr_prior$tipo, "informativo_edad"))
      paste0("incidencia reservada para validar: mide la consistencia interna del modelo GBD con r = 0 y la EMR de ",
             "csmr/prevalencia (interpretaci\u00f3n declarada de antemano)")
    else if (.dl_emr_es_cero(cfg))
      paste0("incidencia reservada para validar: con la EMR fija en 0 mide la consistencia de i con la remisi\u00f3n ",
             "declarada (interpretaci\u00f3n declarada de antemano)")
    else paste0("incidencia reservada para validar: con una EMR no informativa mide adem\u00e1s la ",
                "identificabilidad de i (interpretaci\u00f3n declarada de antemano)")
    # Fase aguda descontada: al modelo crónico entran los sobrevivientes a 28 días; con cfr_30d declarada, la
    # referencia es la incidencia GBD x (1 - cfr_30d).
    cfr <- cfg$emr_prior$fraccion_aguda$cfr_30d
    if (!is.null(cfr)) {
      inc <- data.table::copy(inc)[, `:=`(val = val * (1 - cfr), lower = lower * (1 - cfr), upper = upper * (1 - cfr))]
      nota_inc <- sprintf("%s; referencia = sobrevivientes a 28 d\u00edas: incidencia GBD x (1 - cfr_30d = %g)",
                          nota_inc, cfr)
    }
    out <- rbind(out, .dl_check_bandas(.dl_ipop_bandas(ajuste, insumos, inc, locs = insumos$loc_ancla), inc,
                                       "implied_incidence", nota_inc))
  } else if (!sin_inc_gbd) {
    .dl_message(paste0("sin ancla de incidencia (`ancla_incidencia` de dl_rutas()); se omite la comprobaci\u00f3n ",
                       "implied_incidence"))
  }
  resumen <- out[, list(err_rel_mediano = stats::median(err_rel),
                        cobertura = mean(cubierto_ic95)), by = check]
  data.table::setattr(out, "resumen", resumen)
  data.table::setattr(out, "sin_incidencia_gbd", sin_inc_gbd)
  # Amplitud del gradiente subnacional: con una cascada por proxies y mortalidad subnacional de validación.
  # Sin ella, el motivo va en una nota y en el atributo sin_amplitud, que el manifiesto de la corrida declara.
  if (!is.null(cascada)) .dl_chequear_cascada(cascada, insumos)
  plana <- !is.null(cascada) && identical(cascada$modo, "plana")
  # Con el reparto por razón la cascada es la plana, pero la corrida sí tiene diferencias entre ubicaciones (las de
  # la razón): el motivo es propio, no hay un gradiente por covariables que comparar.
  por_razon <- plana && identical(cfg$cascada$modo$valor, "razon")
  # Con la EMR fija en cero el modelo predice p f = 0 muertes en toda ubicación: no hay gradiente de mortalidad que
  # comparar con la mortalidad subnacional reservada, y se declara como motivo.
  emr_cero <- !is.null(cascada) && !plana && .dl_emr_es_cero(cfg) &&
    nrow(insumos$datos[tipo_dato == "csmr" & location_level == 1L & outlier == FALSE]) > 0L
  am <- if (!is.null(cascada) && !plana && !emr_cero) .dl_amplitud_csmr(cascada, insumos)
  if (!is.null(am)) {
    data.table::setattr(out, "amplitud", am)
    return(out)
  }
  # el motivo en palabras genéricas («subnacional»): el largo va al atributo sin_amplitud, que el manifiesto declara, y
  # el corto, al mensaje
  k <- if (emr_cero) 5L else if (por_razon) 6L else if (plana) 1L else if (!is.null(cascada)) 2L
       else if (nrow(insumos$datos[tipo_dato == "csmr" & location_level == 1L])) 3L else 4L
  motivo <- c("la cascada es plana (dX = 0) y la amplitud subnacional no est\u00e1 definida",
              "los datos no traen mortalidad subnacional de validaci\u00f3n",
              "los datos traen mortalidad subnacional de validaci\u00f3n, pero la validaci\u00f3n no tuvo la cascada",
              "sin cascada ni mortalidad subnacional de validaci\u00f3n",
              "la mortalidad en exceso est\u00e1 fija en 0 y el modelo no predice muertes por la causa",
              paste("el reparto subnacional es por raz\u00f3n y no hay un gradiente por covariables que comparar con",
                    "la mortalidad subnacional de validaci\u00f3n"))[k]
  .dl_message("se omite la validaci\u00f3n con la mortalidad subnacional reservada: %s",
              c("la cascada es plana, sin diferencias entre ubicaciones", "los datos no la traen",
                "los datos la traen, pero la validaci\u00f3n no tuvo la cascada", "sin cascada ni datos",
                "la mortalidad en exceso est\u00e1 fija en 0",
                "el reparto subnacional es por raz\u00f3n, sin un gradiente por covariables que comparar")[k])
  data.table::setattr(out, "sin_amplitud", motivo)
  out
}
