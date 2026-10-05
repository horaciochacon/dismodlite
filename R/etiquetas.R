# Etiquetas de cuánto informan los datos locales a cada celda (sexo x banda de edad): dl_etiquetas().
# Notación: ver R/edo.R.
#
# En cada celda se comparan las simulaciones de log q, con q la prevalencia de la banda (promedio de p ponderado por
# población en [inicio, fin); R/bandas.R), del ajuste con datos («post») y del ajuste solo con el ancla («prior»),
# hechos con la misma semilla:
#   contracción     c = 1 - Var_post(log q) / Var_prior(log q)
#   desplazamiento  d = |mediana_post(log q) - mediana_prior(log q)|
# Etiqueta (.dl_etiqueta): prior-driven si d o c son pequeños (umbrales abajo), data-driven si c es grande y
# prior-informed en medio.
# c depende de la correlación por edad del ancla (rho): se recalcula con cada rho de la rejilla y se publica la
# etiqueta más conservadora, con el rho que la dio (rho_usado). Las celdas departamentales no tienen parámetros
# propios: heredan la etiqueta de su celda nacional.
# Los tres valores de la etiqueta son parte del contrato de las tablas de salida: no se traducen.

# Umbrales de las etiquetas: convenciones del método. Cambiarlos cambia las etiquetas publicadas.
.DL_ETIQUETA_DESPLAZAMIENTO_MIN <- 0.05  # d < 0.05: los datos movieron la mediana de q menos de un 5 % (e^0.05 = 1.051)
.DL_ETIQUETA_CONTRACCION_MIN <- 0.05     # c < 0.05: los datos redujeron la varianza de log q menos de un 5 %
.DL_ETIQUETA_CONTRACCION_DATOS <- 0.3    # c >= 0.3: los datos redujeron la varianza de log q al menos un 30 %

# Etiquetas de la más conservadora (1) a la menos conservadora (3).
.DL_ETIQUETA_NIVEL <- c("prior-driven" = 1L, "prior-informed" = 2L, "data-driven" = 3L)

# Reajustes por rho cuando dl_etiquetas() no recibe `opciones`: cadenas cortas, porque de ellos solo se usan la
# varianza y la mediana de log q por celda.
.DL_REAJUSTE_CADENAS <- 2L
.DL_REAJUSTE_ITERACIONES <- 6000L
.DL_REAJUSTE_CALENTAMIENTO <- 3000L
# Adelgazamiento fijo, no el del ajuste: con 2 cadenas de 3000 iteraciones útiles guardan 600 simulaciones. Con el
# adelgazamiento de un ajuste largo (30, por ejemplo) quedarían 200, pocas para una varianza por celda.
.DL_REAJUSTE_ADELGAZAMIENTO <- 10L

# Etiqueta de una celda a partir de su contracción c y su desplazamiento d (delta_logq).
.dl_etiqueta <- function(contraccion, delta_logq) {
  if (delta_logq < .DL_ETIQUETA_DESPLAZAMIENTO_MIN || contraccion < .DL_ETIQUETA_CONTRACCION_MIN) return("prior-driven")
  if (contraccion >= .DL_ETIQUETA_CONTRACCION_DATOS) "data-driven" else "prior-informed"
}

# Contracción c (contraccion) y desplazamiento d (delta) de cada celda nacional entre `ajuste` y el ajuste solo con
# el ancla `ajuste_prior`, con el rho que se usó en ambos (rho_usado). Las bandas y la población salen de `insumos`.
.dl_contraccion_celdas <- function(ajuste, ajuste_prior, insumos, rho) {
  q_post <- .dl_q_bandas(ajuste, insumos, locs = insumos$loc_ancla)
  # Sin datos locales, el ajuste solo con el ancla es el mismo objeto: no se agrega dos veces.
  q_prior <- if (identical(ajuste_prior$draws_q, ajuste$draws_q)) q_post
             else .dl_q_bandas(ajuste_prior, insumos, locs = insumos$loc_ancla)
  m <- merge(
    q_post[, list(v_post = stats::var(log(val)), med_post = stats::median(log(val))),
           by = list(sex_id, age_group_id)],
    q_prior[, list(v_prior = stats::var(log(val)), med_prior = stats::median(log(val))),
            by = list(sex_id, age_group_id)],
    by = c("sex_id", "age_group_id"))
  m[, list(sex_id, age_group_id, rho_usado = rho,
           contraccion = 1 - v_post / v_prior, delta = abs(med_post - med_prior))]
}

# Opciones de los reajustes por rho cuando dl_etiquetas() no recibe `opciones`: cadenas cortas con su propio
# adelgazamiento (.DL_REAJUSTE_ADELGAZAMIENTO) y el motor de `ajuste`. Se piden a lo sumo las simulaciones que guardan
# esas cadenas (dl_ajustar() no puede dar más), así dl_opciones_mcmc() no avisa: las de `ajuste`, hasta 600.
.dl_opciones_reajuste <- function(ajuste) {
  corto <- list(chains = .DL_REAJUSTE_CADENAS, iter = .DL_REAJUSTE_ITERACIONES, warmup = .DL_REAJUSTE_CALENTAMIENTO,
                thin = .DL_REAJUSTE_ADELGAZAMIENTO)
  dl_opciones_mcmc(simulaciones = min(nrow(ajuste$draws_par[[1]]), .dl_simulaciones_guardadas(corto)),
                   cadenas = corto$chains, iteraciones = corto$iter, calentamiento = corto$warmup,
                   adelgazamiento = corto$thin, nucleos = 1L, motor = ajuste$params$engine)
}

#' Etiquetas de cuánto informan los datos cada celda
#'
#' Compara, por sexo y banda, la varianza posterior de log(prevalencia) con la del ajuste solo con el ancla:
#' contracción c = 1 - Var_post / Var_prior. Cada celda recibe la etiqueta `prior-driven`, `prior-informed` o
#' `data-driven`; con varios valores de rho se queda la más conservadora. Las celdas subnacionales heredan la
#' etiqueta de su celda nacional.
#'
#' @details
#' En cada celda (sexo y banda de edad) se comparan la contracción c y el desplazamiento d = |mediana_post(log q) -
#' mediana_prior(log q)|, con q la prevalencia de la banda:
#' - `prior-driven` (la estimación viene del ancla): d < 0.05 o c < 0.05;
#' - `data-driven` (los datos locales redujeron la varianza al menos un 30 %): c >= 0.3;
#' - `prior-informed`: en medio.
#'
#' Sin datos locales en el ajuste todas las celdas son `prior-driven`. La contracción depende de la correlación por
#' edad del ancla (rho): con cada valor de `grilla_rho` distinto del de la configuración se hacen un ajuste y un
#' ajuste solo con el ancla nuevos (con `opciones`) y se publica la etiqueta más conservadora, con el rho que la dio.
#' Las celdas subnacionales no tienen parámetros propios: heredan la etiqueta de su celda nacional y declaran el
#' `kappa` de la cascada, para que ningún resultado subnacional se lea como evidencia local. Los tres valores de la
#' etiqueta son parte del contrato de las tablas de salida y no se traducen.
#'
#' @inheritParams dl_ajustar
#' @param ajuste Ajuste de [dl_ajustar()].
#' @param ajuste_prior Ajuste de [dl_ajustar_solo_prior()] de los mismos insumos.
#' @param grilla_rho Valores de la correlación por edad del ancla (rho) con que se recalculan las etiquetas; por
#'   defecto, la grilla `sensibilidad.correlacion_edad` de la configuración.
#' @param opciones Opciones de [dl_opciones_mcmc()] de los reajustes por rho (por defecto, cadenas cortas: 2 de
#'   6000 iteraciones con 3000 de calentamiento y adelgazamiento 10, sea cual sea el del ajuste, con el motor del
#'   ajuste; guardan 600 simulaciones, y de ellas se toman a lo sumo las del ajuste).
#' @param cascada Cascada de [dl_cascada()] (opcional): agrega las celdas subnacionales.
#' @return Tabla (data.table) con una fila por ubicación, sexo y banda (`location_id`, `year`, `age_group_id`,
#'   `sex_id`, `cause_id`): `etiqueta`, `contraccion` (c), `lambda_usado` (el peso del ancla), `rho_usado` (el rho
#'   de la etiqueta publicada) y `kappa_usado` (el de la cascada en las celdas subnacionales; `NA` en las
#'   nacionales).
#' @seealso [dl_ajustar_solo_prior()], [dl_sensibilidad()] y [dl_exportar_corrida()] (que las guarda en la corrida).
#' @family diagnóstico
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' f0 <- dl_ajustar_solo_prior(b, op, semilla = 1, ajuste = f)
#' # solo con el rho de la configuración (sin reajustes)
#' et <- dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 1)
#' head(et)
#' # sin datos locales en el ajuste, todas las celdas vienen del ancla
#' table(et$etiqueta)
#' }
#' @export
dl_etiquetas <- function(ajuste, ajuste_prior, insumos, grilla_rho = as.numeric(unlist(insumos$cfg$sensibilidad$rho)),
                         semilla, opciones = NULL, cascada = NULL) {
  .dl_exigir_semilla(semilla)
  .dl_exigir_clase(ajuste, "dl_fit", "ajuste", "dl_ajustar()")
  .dl_exigir_clase(ajuste_prior, "dl_fit", "ajuste_prior", "dl_ajustar_solo_prior()")
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_exigir_mismos_insumos(ajuste, insumos, "ajuste")
  .dl_exigir_mismos_insumos(ajuste_prior, insumos, "ajuste_prior")
  if (!isTRUE(ajuste_prior$prior_only)) .dl_stop("`ajuste_prior` debe venir de dl_ajustar_solo_prior()")
  .dl_exigir_rejilla(grilla_rho, "rho", "grilla_rho")
  if (!is.null(opciones)) .dl_exigir_clase(opciones, "dl_mcmc_opts", "opciones", "dl_opciones_mcmc()")
  if (!is.null(cascada)) .dl_chequear_cascada(cascada, insumos)
  if (is.null(opciones)) opciones <- .dl_opciones_reajuste(ajuste)

  # c y d de cada celda con cada rho: con el rho de la configuración, los dos ajustes recibidos; con otro rho, un
  # ajuste y un ajuste solo con el ancla nuevos (la cache de la sesión los reutiliza entre llamadas).
  por_rho <- data.table::rbindlist(lapply(grilla_rho, function(rho) {
    if (isTRUE(all.equal(rho, insumos$cfg$anchor$rho_edad)))
      return(.dl_contraccion_celdas(ajuste, ajuste_prior, insumos, rho))
    con_rho <- .dl_bundle_con(insumos, rho = rho)
    f <- dl_ajustar(con_rho, opciones = opciones, semilla = semilla)
    f0 <- dl_ajustar_solo_prior(con_rho, opciones = opciones, semilla = semilla, ajuste = f)
    .dl_contraccion_celdas(f, f0, insumos, rho)
  }))
  por_rho[, etiqueta := mapply(.dl_etiqueta, contraccion, delta)]
  por_rho[, nivel := .DL_ETIQUETA_NIVEL[etiqueta]]
  # La más conservadora de cada celda: menor nivel y, a igual nivel, menor contracción.
  out <- por_rho[order(nivel, contraccion), .SD[1], by = list(sex_id, age_group_id)]
  res <- data.table::data.table(
    location_id = insumos$loc_ancla, year = .dl_anio_ajuste(insumos$cfg),
    age_group_id = out$age_group_id, sex_id = out$sex_id, cause_id = insumos$cfg$cause_id,
    etiqueta = out$etiqueta, contraccion = out$contraccion,
    lambda_usado = as.numeric(insumos$cfg$anchor$lambda), rho_usado = out$rho_usado,
    kappa_usado = NA_real_)
  # Celdas departamentales: heredan la etiqueta y la contracción de su celda nacional y declaran el kappa de la
  # cascada, para que ningún resultado departamental se lea como evidencia local.
  if (!is.null(cascada)) {
    dep <- data.table::rbindlist(lapply(cascada$departamentos, function(d)
      data.table::copy(res)[, `:=`(location_id = d, kappa_usado = cascada$kappa)]))
    res <- rbind(res, dep)
  }
  data.table::setorder(res, location_id, sex_id, age_group_id)
  res
}
