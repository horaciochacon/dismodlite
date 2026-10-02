# Sensibilidad de las estimaciones a lambda, rho y kappa (y, si la rejilla lo trae, a fraccion_aguda):
# dl_sensibilidad(). Notación: ver R/edo.R.
#
# Ejes de la rejilla:
#   lambda          peso del ancla (power prior)                                         en (0, 1]
#   rho             correlación por edad del ancla, AR(1)                                en [0, 1)
#   kappa           fracción del gradiente departamental que aplica la cascada           en (0, 1]
#   fraccion_aguda  fracción aguda de la mortalidad por la causa, fuera del prior de f   en [0, 1)
# Por cada combinación (lambda, rho, fraccion_aguda) se hacen un ajuste, un ajuste solo con el ancla y sus etiquetas
# (la cache de la sesión reutiliza los ajustes entre llamadas); por cada kappa, una cascada sobre ese ajuste, que es
# determinista y no vuelve a muestrear. Sin proxies departamentales en los insumos no hay cascada y kappa sale NA.
# Resultado: la tabla nacional por banda con el abanico departamental (mínimo, mediana y máximo del cociente
# departamento / nación); el detalle por departamento va en el atributo "departamental".
#
# Paralelismo: `procesos` reparte las combinaciones y opciones$cores las cadenas de cada ajuste. El resultado no
# depende del reparto: cada cadena deriva su semilla de `semilla` (R/muestreador.R), no del proceso que la corre.

#' Sensibilidad a lambda, rho y kappa
#'
#' Recorre una grilla de pesos del ancla (lambda), correlaciones por edad (rho) y fracciones del gradiente
#' subnacional (kappa): un ajuste por (lambda, rho) y una cascada por kappa, con las etiquetas recalculadas.
#'
#' @details
#' En un proyecto, la grilla de la configuración es `sensibilidad.peso` (lambda), `sensibilidad.correlacion_edad`
#' (rho) y `sensibilidad.kappa` (kappa); por defecto, `[0.25, 0.5, 1]`, `[0, 0.5, 0.9]` y `[0.5, 1]` (ver
#' [dl_configuracion()]). Cada combinación (lambda, rho) es un ajuste completo con `opciones`, así que la grilla
#' entera tarda varias veces lo que un ajuste; `procesos` reparte las combinaciones entre procesos. El eje opcional
#' `fraccion_aguda` cambia la fracción de las muertes por la causa que se descuenta del prior de la mortalidad en
#' exceso. Una combinación con la que la cascada falla (por ejemplo, prevalencias subnacionales mayores que 1 con
#' un lambda pequeño) no detiene las demás: su fila lo explica en `nota`.
#'
#' Lo que se mira: cuánto cambia la prevalencia nacional (y su etiqueta) de una combinación a otra y cuánto se abre
#' el abanico subnacional con kappa. Si un resultado cambia mucho dentro de valores razonables, la decisión que lo
#' fija (el peso del ancla, por ejemplo) merece una justificación en `notas`.
#'
#' @inheritParams dl_ajustar
#' @param grilla Lista con los vectores `lambda`, `rho` y `kappa` (y, opcionalmente, `fraccion_aguda`); por
#'   defecto, la grilla `sensibilidad` de la configuración. `kappa` es obligatorio solo si hay proxies
#'   subnacionales.
#' @param opciones Opciones de [dl_opciones_mcmc()] de cada ajuste de la grilla (por defecto, 200 simulaciones y 2
#'   cadenas de 6000 iteraciones con 3000 de calentamiento).
#' @param procesos Combinaciones (lambda, rho) en paralelo (en Windows se usa 1).
#' @return Tabla (data.table) nacional con una fila por combinación (`lambda`, `rho`, `fraccion_aguda`, `kappa`),
#'   sexo (`sex_id`) y banda (`age_group_id`): la prevalencia nacional de la banda (`val`, con su intervalo del 95 %
#'   en `lower` y `upper`), su `etiqueta`, el abanico subnacional (`dpto_min`, `dpto_med` y `dpto_max`: el mínimo,
#'   la mediana y el máximo, entre las ubicaciones subnacionales, del cociente ubicación / nación) y `nota` (por qué
#'   una fila no tiene abanico; `NA` si lo tiene). El atributo `departamental` trae el detalle por ubicación
#'   subnacional (`location_id`, con `val`, `lower` y `upper`). Los nombres `dpto_*` y `departamental` se conservan
#'   de la versión 0.2.2.
#' @seealso [dl_etiquetas()], [dl_cascada()] (kappa) y [dl_correr()] (que la guarda en
#'   `diagnostics/sensibilidad.csv`).
#' @family diagnóstico
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' # una grilla pequeña: dos pesos del ancla, un rho y un kappa
#' s <- dl_sensibilidad(b, grilla = list(lambda = c(0.5, 1), rho = 0.5, kappa = 1), semilla = 1,
#'                      opciones = op)
#' # prevalencia nacional de una banda de edad con cada peso del ancla
#' s[age_group_id == 18, c("lambda", "sex_id", "val", "lower", "upper", "dpto_min", "dpto_max")]
#' head(attr(s, "departamental"))
#' }
#' @export
dl_sensibilidad <- function(insumos, grilla = insumos$cfg$sensibilidad, semilla,
                            opciones = dl_opciones_mcmc(simulaciones = 200L, cadenas = 2L, iteraciones = 6000L,
                                                        calentamiento = 3000L),
                            procesos = 1L) {
  .dl_exigir_semilla(semilla)
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_exigir_clase(opciones, "dl_mcmc_opts", "opciones", "dl_opciones_mcmc()")
  if (!.dl_es_entero1(procesos) || procesos < 1)
    .dl_stop("`procesos` debe ser un entero >= 1; es %s", .dl_describir_objeto(procesos))
  con_cascada <- nrow(insumos$cov_proxy) > 0L
  .dl_exigir_grilla(grilla, con_cascada, revisar_nombres = !missing(grilla))
  # Motor rcpp: el núcleo C++ se compila antes de repartir las combinaciones entre procesos.
  if (opciones$engine == "rcpp") .dl_rcpp()

  kappas <- if (con_cascada) as.numeric(unlist(grilla$kappa)) else NA_real_
  # fraccion_aguda es un eje opcional: sin él, cada ajuste usa la de la configuración (0 si no la declara).
  fracciones <- if (length(grilla$fraccion_aguda)) as.numeric(unlist(grilla$fraccion_aguda))
                else .dl_fraccion_aguda(insumos$cfg)
  combinaciones <- expand.grid(lambda = as.numeric(unlist(grilla$lambda)), rho = as.numeric(unlist(grilla$rho)),
                               fraccion_aguda = fracciones, KEEP.OUT.ATTRS = FALSE)
  una_combinacion <- function(k) {
    lam <- combinaciones$lambda[k]; rho <- combinaciones$rho[k]; fa <- combinaciones$fraccion_aguda[k]
    # Ajuste, ajuste solo con el ancla y etiquetas (bajo el rho de la combinación) con los insumos modificados.
    insumos_k <- .dl_bundle_con(insumos, lambda = lam, rho = rho,
                                fraccion_aguda = if (length(grilla$fraccion_aguda)) fa else NULL)
    ajuste_k <- dl_ajustar(insumos_k, opciones = opciones, semilla = semilla)
    ajuste_prior_k <- dl_ajustar_solo_prior(insumos_k, opciones = opciones, semilla = semilla, ajuste = ajuste_k)
    etiquetas_k <- dl_etiquetas(ajuste_k, ajuste_prior_k, insumos_k, grilla_rho = rho, semilla = semilla,
                                opciones = opciones)
    # Prevalencia nacional por banda (media e intervalo) con su etiqueta.
    nac <- .dl_stats_bandas(.dl_q_bandas(ajuste_k, insumos_k, locs = insumos_k$loc_ancla))[, location_id := NULL]
    nac <- merge(nac, etiquetas_k[location_id == insumos_k$loc_ancla, list(sex_id, age_group_id, etiqueta)],
                 by = c("sex_id", "age_group_id"))
    # Fila de la combinación sin abanico subnacional, con la razón en `nota`.
    sin_dpto <- function(kp, nota)
      data.table::data.table(lambda = lam, rho = rho, fraccion_aguda = fa, kappa = kp, nac,
                             dpto_min = NA_real_, dpto_med = NA_real_, dpto_max = NA_real_, nota = nota)
    por_kappa <- lapply(kappas, function(kp) {
      if (is.na(kp)) return(list(fila = sin_dpto(NA_real_, "sin proxies subnacionales: la cascada no aplica")))
      # dl_cascada() se detiene si la prevalencia de algún departamento sale de [0, 1]. En la rejilla, una
      # combinación extrema (lambda pequeño: simulaciones con p cercana a 1) se reporta en `nota` y no detiene el
      # resto.
      cascada_k <- tryCatch(suppressWarnings(dl_cascada(ajuste_k, insumos_k, kappa = kp, semilla = semilla,
                                                        motor = opciones$engine)),
                            error = function(e) e)
      if (inherits(cascada_k, "error"))
        return(list(fila = sin_dpto(kp, paste("dl_cascada():", .dl_detalle(cascada_k)))))
      st <- .dl_stats_bandas(.dl_q_bandas(cascada_k, insumos_k))
      dep <- st[location_id != insumos_k$loc_ancla]
      # Abanico departamental: mínimo, mediana y máximo del cociente departamento / nación de cada celda.
      cocientes <- merge(dep, nac[, list(sex_id, age_group_id, nac = val)], by = c("sex_id", "age_group_id"))
      cocientes[, cociente := val / nac]
      res <- merge(nac, cocientes[, list(dpto_min = min(cociente), dpto_med = stats::median(cociente),
                                         dpto_max = max(cociente)),
                                  by = list(sex_id, age_group_id)], by = c("sex_id", "age_group_id"))
      list(fila = data.table::data.table(lambda = lam, rho = rho, fraccion_aguda = fa, kappa = kp, res,
                                         nota = NA_character_),
           departamental = data.table::data.table(lambda = lam, rho = rho, fraccion_aguda = fa, kappa = kp, dep))
    })
    list(filas = data.table::rbindlist(lapply(por_kappa, `[[`, "fila")),
         departamental = data.table::rbindlist(lapply(por_kappa, `[[`, "departamental")))
  }
  piezas <- .dl_paralelo(nrow(combinaciones), una_combinacion, procesos)
  .dl_exigir_sin_fallos(piezas, "una combinaci\u00f3n de la grilla")
  out <- data.table::rbindlist(lapply(piezas, `[[`, "filas"))
  data.table::setorder(out, lambda, rho, fraccion_aguda, kappa, sex_id, age_group_id)
  departamental <- data.table::rbindlist(lapply(piezas, `[[`, "departamental"))
  if (nrow(departamental))
    data.table::setorder(departamental, lambda, rho, fraccion_aguda, kappa, location_id, sex_id, age_group_id)
  data.table::setattr(out, "departamental", departamental)
  out[]
}

# `grilla` de dl_sensibilidad(): una lista con los ejes lambda y rho (obligatorios), kappa (obligatorio si hay
# cascada) y fraccion_aguda (opcional), cada uno con números en su dominio. Con `revisar_nombres` (una rejilla
# escrita por el usuario, no la de la configuración) un eje con otro nombre es un error, para que un nombre mal
# escrito no se ignore en silencio.
.dl_exigir_grilla <- function(grilla, con_cascada, revisar_nombres) {
  if (!is.list(grilla))
    .dl_stop(paste0("`grilla` debe ser una lista con los ejes lambda, rho y kappa, por ejemplo list(lambda = c(0.5, ",
                    "1), rho = 0.5, kappa = 1); es %s"), .dl_describir_objeto(grilla))
  otros <- setdiff(names(grilla), names(.DL_DOMINIO_REJILLA))
  if (revisar_nombres && (length(otros) || (length(grilla) && is.null(names(grilla)))))
    .dl_stop("`grilla` solo admite los ejes %s; tiene %s",
             paste(names(.DL_DOMINIO_REJILLA), collapse = ", "),
             if (length(otros)) paste(otros, collapse = ", ") else "elementos sin nombre")
  obligatorios <- c("lambda", "rho", if (con_cascada) "kappa")
  for (eje in names(.DL_DOMINIO_REJILLA))
    if (eje %in% obligatorios || length(grilla[[eje]]))
      .dl_exigir_rejilla(grilla[[eje]], eje, paste0("grilla$", eje))
  invisible(grilla)
}
