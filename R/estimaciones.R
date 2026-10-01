# Tabla ordenada de estimaciones (media e intervalo) a partir de las simulaciones de un ajuste, una cascada o un
# resultado de dl_avd(). El estimador puntual es la media de las simulaciones, como en las celdas de las corridas
# (.DL_ESTADISTICO_PUNTUAL), y el intervalo sale de los cuantiles (1 - nivel) / 2 y 1 - (1 - nivel) / 2.

# Medida -> columna de draws_q: la incidencia es la poblacional, i (1 - p), la misma que exportan las corridas.
.DL_COLUMNA_MEDIDA <- c(prevalencia = "p", incidencia = "ipop", mortalidad_exceso = "f")

#' Estimaciones por edad, sexo y ubicación
#'
#' Resume las simulaciones en una tabla con la media y el intervalo de incertidumbre de cada celda. Con un ajuste o
#' una cascada, por edad simple; con un resultado de [dl_avd()], por banda de edad.
#'
#' @param x Ajuste de [dl_ajustar()], cascada de [dl_cascada()] o resultado de [dl_avd()].
#' @param medida `"prevalencia"`, `"incidencia"` (incidencia poblacional, i (1 - p), por persona-año),
#'   `"mortalidad_exceso"` o `"avd"` (solo con un resultado de [dl_avd()], que por defecto da `"avd"`; también
#'   admite `"prevalencia"` por banda).
#' @param nivel Nivel del intervalo (0.95: cuantiles 2.5 % y 97.5 %).
#' @return Una tabla (data.table) con una fila por celda: `location_id`, `sex_id`, `edad` (la edad entera; con un
#'   resultado de [dl_avd()], `age_group_id`, la banda de edad del ancla), `medida`, `media` (la media de las
#'   simulaciones, el mismo valor central de las tablas de las corridas) e `inferior` y `superior` (los cuantiles
#'   del intervalo). Unidades: la prevalencia es una proporción; la incidencia y la mortalidad en exceso, tasas por
#'   persona-año; los AVD, años por persona y año (x 100 000 da la tasa por 100 000 de las corridas).
#' @seealso [dl_ajustar()], [dl_cascada()], [dl_avd()]; [dl_resumir()] da las celdas por banda de edad con los
#'   nombres de los catálogos.
#' @family ajuste
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' head(dl_estimaciones(f))
#' head(dl_estimaciones(f, "incidencia"))
#' }
#' @export
dl_estimaciones <- function(x, medida = c("prevalencia", "incidencia", "mortalidad_exceso", "avd"), nivel = 0.95) {
  if (missing(x))
    .dl_stop("falta `x` (un ajuste de dl_ajustar(), una cascada de dl_cascada() o un resultado de dl_avd())")
  .dl_exigir_nivel(nivel)
  cola <- (1 - nivel) / 2
  medidas <- c("prevalencia", "incidencia", "mortalidad_exceso", "avd")
  if (!missing(medida)) {   # como match.arg(): se aceptan abreviaturas ("prev")
    k <- if (.dl_es_texto1(medida)) pmatch(medida, medidas) else NA_integer_
    if (is.na(k))
      .dl_stop("`medida` debe ser %s, pero es %s",
               paste0("\"", medidas, "\"", collapse = ", "), .dl_describir_objeto(medida))
    # «mortalidad» sola no se completa: podría ser la mortalidad específica por causa, que no es una medida de aquí
    if (medidas[k] == "mortalidad_exceso" && !startsWith(medida, "mortalidad_"))
      .dl_stop(paste0("`medida` = \"%s\" es ambigua: la mortalidad en exceso (EMR) es \"mortalidad_exceso\"; ",
                      "la mortalidad espec\u00edfica por causa no es una medida de dl_estimaciones()"), medida)
    medida <- medidas[k]
  }
  if (inherits(x, "dl_yld")) {
    medida <- if (missing(medida)) "avd" else medida
    if (!medida %in% c("avd", "prevalencia"))
      .dl_stop("con un resultado de dl_avd() la medida es \"avd\" o \"prevalencia\"")
    tabla <- if (medida == "avd") x$draws_yld else x$draws_prev_banda
    por <- c("location_id", "sex_id", "age_group_id")
    d <- data.table::data.table(location_id = tabla$location_id, sex_id = tabla$sex_id,
                                age_group_id = tabla$age_group_id, v = tabla$val)
  } else if (inherits(x, "dl_fit")) {
    medida <- if (missing(medida)) "prevalencia" else medida
    if (medida == "avd")
      .dl_stop("los AVD se calculan con dl_avd(); pasa su resultado a dl_estimaciones()")
    dq <- x$draws_q
    por <- c("location_id", "sex_id", "edad")
    d <- data.table::data.table(location_id = dq$location_id, sex_id = dq$sex_id, edad = dq$edad,
                                v = dq[[.DL_COLUMNA_MEDIDA[[medida]]]])
  } else {
    .dl_stop("`x` debe ser un ajuste (dl_ajustar()), una cascada (dl_cascada()) o un resultado de dl_avd(); es %s",
             .dl_describir_objeto(x))
  }
  out <- d[, list(media = mean(v), inferior = stats::quantile(v, cola, names = FALSE),
                  superior = stats::quantile(v, 1 - cola, names = FALSE)), by = por]
  data.table::set(out, j = "medida", value = medida)
  data.table::setcolorder(out, c(por, "medida", "media", "inferior", "superior"))
  out[]
}
