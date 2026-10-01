# Factor de comorbilidad (COMO) por banda de edad y sexo, calibrado al AVD del ancla.
#
# Notación: ver R/edo.R y R/avd.R (p_b, pi_e, DW_e, COMO_b). Por sexo y banda b del ancla de prevalencia:
#
#   COMO_b = AVD_ancla_b / ( p_ancla_b x sum_e pi_e DW_e )
#
#   AVD_ancla_b   AVD de referencia (la ruta `ancla_avd`), por persona: el archivo viene por 100 000 y
#                 .dl_materializar_medida() (R/insumos.R) lo divide por 100 000
#   p_ancla_b     prevalencia del ancla de los insumos (proporción)
#   pi_e, DW_e    proporción y peso de discapacidad centrales de la tabla de severidad (sin muestreo)
# El factor no tiene unidades. Malla: las bandas de edad y sexo del ancla de prevalencia; el AVD de referencia
# tiene que cubrirlas todas.
#
# El AVD de referencia ya viene corregido por comorbilidad; el denominador reconstruye el AVD sin esa corrección con
# los mismos insumos. Como la tabla de severidad es la misma en todas las edades, el cociente absorbe además el
# patrón etario de la severidad (inseparable de la comorbilidad) y el error de reconstruir la partición: es una
# calibración etaria, no un factor COMO puro. Por eso no se trunca, puede superar 1 y no se valida como la tabla
# como_factor del esquema (que exige un factor en (0, 1]).
#
# Componente de la causa (anchor.componente): los insumos traen la prevalencia del ancla ya escalada por la fracción
# de prevalencia del componente; aquí el AVD de referencia se escala por su fracción de AVD
# (sum de val x DW de las secuelas del componente / la misma suma en la causa; R/severidad.R).

#' Factor de comorbilidad por edad y sexo
#'
#' Calibra los AVD por banda de edad y sexo al AVD de referencia: factor = AVD del ancla / (prevalencia del ancla
#' x suma de proporción x peso de discapacidad de la tabla de severidad). Absorbe la corrección por comorbilidad
#' (COMO) y el patrón etario de la severidad; se aplica en [dl_avd()].
#'
#' @details
#' El AVD de referencia (la medida 3 de las descargas del ancla) ya viene corregido por comorbilidad; el denominador
#' reconstruye el AVD sin esa corrección con la prevalencia del ancla y la tabla de severidad. Como la tabla de
#' severidad es la misma en todas las edades, el cociente absorbe además el patrón etario de la severidad: es una
#' calibración por edad, no un factor de comorbilidad puro, y puede ser mayor que 1. Se aplica a la nación y a todas
#' las ubicaciones subnacionales por igual. Sin AVD en el ancla no se puede calcular; [dl_correr()] entonces calcula
#' los AVD sin él y el manifiesto lo declara.
#'
#' @inheritParams dl_ajustar
#' @param rutas Rutas de [dl_rutas()]; se usa el ancla de AVD (`ancla_avd`). Por defecto, las de los insumos.
#' @return Tabla (data.table) con una fila por sexo y banda de edad del ancla: `location_id` (la ubicación nacional),
#'   `cause_family` (la causa), `age_group_id`, `sex_id`, `factor` (sin unidades), `dispersion` (`NA`: el factor no
#'   tiene incertidumbre propia) y `fuente` (cómo se calculó, en palabras).
#' @seealso [dl_avd()] (que lo aplica) y [dl_proyecto()] (el ancla y la tabla de severidad).
#' @family carga
#' @examples
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' como <- dl_factor_comorbilidad(b)
#' head(como[, c("sex_id", "age_group_id", "factor")])
#' range(como$factor)
#' @export
dl_factor_comorbilidad <- function(insumos, rutas = insumos$rutas) {
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  rutas <- .dl_resolver_rutas(rutas, insumos)
  cfg <- insumos$cfg
  archivo <- .dl_path(rutas, "std_yld", motivo = sprintf(paste0(
    "el factor se calibra con el AVD de referencia, AVD por 100 000 de GBD Results de la causa %d en el a\u00f1o de ",
    "ajuste, por edad y sexo"), cfg$cause_id))
  ancla_avd <- .dl_materializar_medida(cfg, "yld", rutas, archivo = archivo)        # AVD_ancla_b, por persona
  if (!is.null(insumos$componente)) {
    fraccion_avd <- insumos$componente$fraccion_yld
    ancla_avd <- data.table::copy(ancla_avd)[, `:=`(val = val * fraccion_avd, lower = lower * fraccion_avd,
                                                    upper = upper * fraccion_avd)]
  }
  ancla_prev <- insumos$prior_gbd[measure_id == .dl_medida_id("prevalence")]    # p_ancla_b
  suma_pi_dw <- sum(insumos$severidad$proportion * insumos$severidad$dw_mean)   # sum_e pi_e DW_e
  por_banda <- merge(ancla_prev[, list(sex_id, age_group_id, prev = val)],
                     ancla_avd[,  list(sex_id, age_group_id, yld = val)], by = c("sex_id", "age_group_id"))
  if (nrow(por_banda) != nrow(ancla_prev))
    .dl_stop("el ancla de AVD (`ancla_avd`) no cubre todas las bandas de edad y sexo del ancla de prevalencia")
  por_banda[, factor := yld / (prev * suma_pi_dw)]                # COMO_b, sin tope (cabecera)
  if (any(!is.finite(por_banda$factor) | por_banda$factor <= 0))
    .dl_stop(paste0("el factor no es finito y positivo en alguna banda; revisa el AVD y la prevalencia del ancla ",
                    "(ceros o faltantes) y los pesos de discapacidad de la tabla de severidad"))
  out <- data.table::data.table(location_id = insumos$loc_ancla, cause_family = as.character(cfg$cause_id),
    age_group_id = as.integer(por_banda$age_group_id), sex_id = as.integer(por_banda$sex_id),
    factor = por_banda$factor, dispersion = NA_real_,
    fuente = sprintf(paste("calibraci\u00f3n etaria emp\u00edrica (composici\u00f3n de la severidad y",
                           "comorbilidad): AVD del ancla / (prevalencia del ancla x suma de pi x DW), causa %d,",
                           "sin tope"), cfg$cause_id))
  data.table::setorder(out, sex_id, age_group_id)
  out
}
