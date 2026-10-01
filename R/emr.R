# Prior de la mortalidad en exceso f(a) por edad y su techo.
#
# Notación: ver R/edo.R. Aquí f es la mortalidad en exceso en C (por persona-año), p la prevalencia y
# theta = (log i en los nudos, log f en los nudos).
#
# De dónde sale el prior. Las muertes por la causa ocurren solo en C, a la tasa f; por persona-año de toda la
# población son
#   csmr = f C / (S + C) = f p        =>        f = csmr / p.
# El ancla trae, por sexo y banda de edad [age_start, age_end), la prevalencia p y la mortalidad específica por causa
# csmr (muertes por persona-año), cada una con su desviación estándar en log desde su intervalo del 95 %,
# sigma_log = (log upper - log lower) / 3.92 (columna sigma_log de R/insumos.R; 3.92 = .DL_ANCHO_IC95_EN_SD de
# R/betas.R). Con la fracción aguda del csmr descontada (las muertes de los primeros 28 días pertenecen al modelo
# agudo, no al crónico):
#   mu_log = log(csmr) + log1p(-fraccion_aguda) - log(p)        [= log(csmr (1 - fraccion_aguda) / p)]
#   sd_log = k_sd * sqrt(sigma_log_p^2 + sigma_log_csmr^2)       [errores en log independientes]
# con k_sd = emr_prior.factor_sd.valor (1 por defecto). Unidades: f y csmr por persona-año, p proporción.
#
# Cómo entra en la log-posterior (R/verosimilitud.R). Con el prior informativo, por nudo k:
#   log N(log f_k; mu_log_k, sd_log_k),   f_k dentro de la cota [f_min, f_max]  (fuera: log-posterior -Inf).
# La constante de normalización del truncamiento a la cota no depende de theta y se omite. Con el prior plano
# (emr_prior.tipo plano_cota) el término vale 0 y solo queda la cota.
#
# Techo de EMR: f_max, la cota superior de f. Impreso en la configuración (emr_prior.cota = [f_min, f_max]) o
# derivado del ancla:
#   cota = [0, K_techo * max_b(csmr_b (1 - fraccion_aguda) / p_b)],   máximo sobre sexos y bandas b del ancla,
# con K_techo = emr_prior.factor_techo (.DL_EMR_TECHO_K por defecto). El techo lo aplican el muestreador y la
# cascada.

# ---- Constantes ------------------------------------------------------------------------------------------------

# K_techo, factor del techo derivado: f_max = K_techo * max(csmr / p del ancla). El techo no informa (el prior
# log-normal por edad domina): es un muro contra valores absurdos en el muestreador y en la cascada, donde el
# multiplicador exp(beta * dX) de una covariable puede pedir mortalidades en exceso departamentales muy altas.
# K_techo = 3 es una elección de juicio: log(3) ~ 1,1, entre dos y cinco sd_log por encima del máximo del prior.
# emr_prior.factor_techo lo cambia.
.DL_EMR_TECHO_K <- 3

# Rango del prior plano: log f uniforme en [log(f_max) - log(1000), log(f_max)], es decir, f_min = f_max / 1000 (o la
# cota inferior impresa, si es mayor). El piso hace propio el prior: sin él, log f deriva hacia -Inf y las cadenas no
# convergen (R-hat de 3 a 4 en causas reales). Tres órdenes de magnitud bajo el techo equivalen a f ~ 0 para la EDO.
.DL_EMR_PLANO_RANGO <- 1000

# Punto de partida del prior plano, en log bajo el techo: mu_log = log(f_max) - 2, es decir, f = f_max / e^2
# (~ 0,14 f_max). El prior plano no tiene media: mu_log solo sirve de valor inicial al muestreador, un punto interior
# lejos del techo y del piso f_max / 1000.
.DL_EMR_PLANO_INICIO_BAJO_TECHO <- 2

# Factor sobre sd_log de un nudo fuera de la cobertura de las bandas del ancla (antes de la primera o desde el fin de
# la última). El nudo hereda mu_log de la banda más cercana; como es una extrapolación, el prior se declara menos
# seguro (sd x 2, varianza x 4) en lugar de inventar una tendencia por edad.
.DL_EMR_SD_FUERA_DE_BANDAS <- 2

# ---- Parámetros del prior en la configuración --------------------------------------------------------------------

# fraccion_aguda: fracción de las muertes por la causa que ocurre en los primeros 28 días y pertenece al modelo
# agudo; el prior crónico usa csmr (1 - fraccion_aguda). Sin el campo, 0: todo el csmr es crónico.
.dl_fraccion_aguda <- function(cfg) as.numeric(cfg$emr_prior$fraccion_aguda$valor %||% 0)

# k_sd: factor sobre sd_log del prior. Un ancla no monótona puede exigir en algunas edades una f más alta que
# csmr / p; el prior se relaja (sd_log crece) sin desplazarse (mu_log no cambia) y el factor queda declarado en el
# manifiesto de la corrida. Sin el campo, 1.
.dl_factor_sd_emr <- function(cfg) as.numeric(cfg$emr_prior$factor_sd$valor %||% 1)

# ---- Prior informativo por banda ------------------------------------------------------------------------------

#' Prior de la mortalidad en exceso por edad
#'
#' Construye el prior informativo de la mortalidad en exceso f por sexo y banda de edad a partir del ancla. Como las
#' muertes por la causa son csmr = f p, el prior centra log f en log(csmr / p), con la fracción aguda del csmr
#' descontada, y propaga a log f la incertidumbre de las dos medidas:
#'
#' `mu_log = log(csmr) + log(1 - fraccion_aguda) - log(p)` y
#' `sd_log = factor_sd * sqrt(sigma_p^2 + sigma_csmr^2)`,
#'
#' donde sigma_p y sigma_csmr son las desviaciones estándar en log del ancla (sigma_log, de sus intervalos del 95 %),
#' `fraccion_aguda` es `emr_prior.fraccion_aguda.valor` (0 por defecto) y `factor_sd` es `emr_prior.factor_sd.valor`
#' (1 por defecto).
#'
#' [dl_ajustar()] lo usa con el prior por defecto (`mortalidad_exceso.prior: desde_ancla`): cada nudo toma el prior
#' de la banda del ancla que lo contiene (un nudo fuera de las bandas, el de la más cercana, con el doble de
#' desviación estándar) y log f queda truncado bajo el techo de la mortalidad en exceso (`mortalidad_exceso.techo` o,
#' sin él, 3 veces el máximo de csmr / p del ancla). Con `mortalidad_exceso.prior: plano` el prior de log f es plano
#' bajo el techo y esta tabla no se usa.
#'
#' @param insumos Insumos de [dl_insumos()] (con el ancla de mortalidad).
#' @return Tabla (data.table) con una fila por sexo y banda del ancla: `sex_id`, `age_group_id`, `age_start`,
#'   `age_end` (la banda es `[age_start, age_end)`), `mu_log` y `sd_log` (media y desviación estándar de log f, con f
#'   por persona-año).
#' @seealso [dl_ajustar()], [dl_configuracion()] (las claves `mortalidad_exceso.*`).
#' @family avanzado
#' @examples
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' prior <- dl_prior_emr(b)
#' head(prior)
#' # la mortalidad en exceso central del prior, por persona-año
#' head(exp(prior$mu_log))
#' @export
dl_prior_emr <- function(insumos) {
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_prior_emr_tabla(insumos$prior_gbd, insumos$cfg)
}

# La tabla de dl_prior_emr() desde el ancla (prior_gbd) y la configuración; la usan también el techo derivado
# (.dl_techo_emr, antes de que existan los insumos) y el contexto de la verosimilitud (.dl_ctx_emr).
.dl_prior_emr_tabla <- function(prior_gbd, cfg) {
  ancla <- prior_gbd
  prev <- ancla[measure_id == .dl_medida_id("prevalence")]
  csmr <- ancla[measure_id == .dl_medida_id("csmr")]
  if (!nrow(csmr))
    .dl_stop(paste0("los insumos no traen la mortalidad espec\u00edfica por causa (measure_id %s); ",
                    "dl_insumos() la lee del archivo `ancla_mortalidad` de dl_rutas()"), .dl_medida_id("csmr"))
  fraccion_aguda <- .dl_fraccion_aguda(cfg)
  k_sd <- .dl_factor_sd_emr(cfg)
  # una fila por sexo y banda presentes en las dos medidas; las columnas de cada una llevan el sufijo _prev o _csmr
  por_banda <- merge(prev, csmr, by = c("sex_id", "age_group_id", "age_start", "age_end", "year"),
                     suffixes = c("_prev", "_csmr"))
  por_banda[, list(sex_id, age_group_id, age_start, age_end,
                   mu_log = log(val_csmr) + log1p(-fraccion_aguda) - log(val_prev),
                   sd_log = k_sd * sqrt(sigma_log_prev^2 + sigma_log_csmr^2))]
}

# ---- Techo de EMR ---------------------------------------------------------------------------------------------

# Cota [f_min, f_max] de f (por persona-año). dl_insumos la calcula una vez y la guarda en cfg$emr_prior$cota;
# .dl_bundle_con la recalcula si cambia la fracción aguda.
#   impresa:  emr_prior.cota de la configuración;
#   derivada: [0, K_techo * max(csmr (1 - fraccion_aguda) / p)], con max(exp(mu_log)) de dl_prior_emr sobre sexos
#             y bandas, y K_techo = emr_prior.factor_techo (.DL_EMR_TECHO_K por defecto).
# Devuelve list(cota, origen = "impreso" | "derivado", k, emr_max_ancla), que el manifiesto de la corrida declara
# (k y emr_max_ancla son NA con el techo impreso).
.dl_techo_emr <- function(cfg, prior_gbd) {
  cota_impresa <- cfg$emr_prior$cota
  if (!is.null(cota_impresa))
    return(list(cota = as.numeric(unlist(cota_impresa)), origen = "impreso", k = NA_real_, emr_max_ancla = NA_real_))
  k_techo <- as.numeric(cfg$emr_prior$factor_techo %||% .DL_EMR_TECHO_K)
  prior_emr <- .dl_prior_emr_tabla(prior_gbd, cfg)
  emr_max <- max(exp(prior_emr$mu_log))
  if (!is.finite(emr_max) || emr_max <= 0)
    .dl_stop(paste0("no se puede derivar el techo de la mortalidad en exceso: mortalidad / prevalencia del ancla no ",
                    "es un n\u00famero finito y positivo; declara emr_prior.cota en la configuraci\u00f3n"))
  list(cota = c(0, k_techo * emr_max), origen = "derivado", k = k_techo, emr_max_ancla = emr_max)
}

# ---- Prior de f en los nudos --------------------------------------------------------------------------------------
# Las dos funciones devuelven lo que .dl_ctx guarda en ctx$emr: list(mu_log, sd_log, cota, plano), con mu_log y
# sd_log por nudo (en el orden de `nudos`), cota = [f_min, f_max] y plano = TRUE solo con el prior plano.

# Prior plano (emr_prior.tipo plano_cota): log f uniforme entre el piso y el techo,
#   f_max = cota impresa superior,   f_min = max(cota impresa inferior, f_max / .DL_EMR_PLANO_RANGO),
# sin término en la log-posterior (plano = TRUE), solo el truncamiento a [f_min, f_max]. mu_log es el valor inicial
# del muestreador, log(f_max) - .DL_EMR_PLANO_INICIO_BAJO_TECHO en todos los nudos; sd_log no existe (NA).
.dl_emr_plano <- function(nudos, cota) {
  cota <- as.numeric(unlist(cota))
  if (length(cota) != 2L || !is.finite(cota[2]) || cota[2] <= 0)
    .dl_stop(paste0("el prior plano (emr_prior.tipo plano_cota) exige emr_prior.cota = [m\u00ednimo, ",
                    "m\u00e1ximo] con m\u00e1ximo > 0"))
  cota[1] <- max(cota[1], cota[2] / .DL_EMR_PLANO_RANGO)
  list(mu_log = rep(log(cota[2]) - .DL_EMR_PLANO_INICIO_BAJO_TECHO, length(nudos)),
       sd_log = rep(NA_real_, length(nudos)), cota = cota, plano = TRUE)
}

# Prior informativo en los nudos desde la tabla por banda de un sexo (dl_prior_emr filtrada por sexo). Cada nudo a_k
# toma la banda que lo contiene, age_start <= a_k < age_end (contención [inicio, fin), .dl_banda_de):
#   mu_log_k = mu_log[banda],   sd_log_k = sd_log[banda].
# Un nudo fuera de la cobertura de las bandas (a_k < inicio de la primera, o a_k >= fin de la última) toma la banda
# más cercana, la primera o la última, con sd_log_k = sd_log[banda] * .DL_EMR_SD_FUERA_DE_BANDAS. Las bandas del ancla
# no se solapan (.dl_materializar_medida lo exige) y son contiguas; si hubiera un hueco entre dos, un nudo dentro de
# él tomaría la última banda sin inflar la sd.
.dl_emr_en_nudos <- function(tabla_sexo, nudos, cota) {
  bandas <- data.table::as.data.table(tabla_sexo)
  data.table::setorder(bandas, age_start)
  banda <- .dl_banda_de(nudos, bandas)
  sin_banda <- is.na(banda)
  banda[sin_banda] <- ifelse(nudos[sin_banda] < bandas$age_start[1], 1L, nrow(bandas))
  fuera_de_bandas <- nudos < bandas$age_start[1] | nudos >= bandas$age_end[nrow(bandas)]
  list(mu_log = bandas$mu_log[banda],
       sd_log = bandas$sd_log[banda] * ifelse(fuera_de_bandas, .DL_EMR_SD_FUERA_DE_BANDAS, 1),
       cota = as.numeric(unlist(cota)), plano = FALSE)
}

# ---- Insumos con otros parámetros del ancla o de la EMR ----------------------------------------------------------

# Copia de los insumos con lambda (anchor.lambda), rho (anchor.rho_edad) o la fracción aguda del csmr cambiados, para
# la sensibilidad (dl_sensibilidad) y las etiquetas (dl_etiquetas). El hash no se recalcula (sigue siendo el de los
# insumos originales), así que el resultado es solo de uso interno. Con el techo derivado, el techo se vuelve a
# derivar con la nueva fracción aguda (sigue al csmr descontado); el techo impreso no cambia.
.dl_bundle_con <- function(b, lambda = NULL, rho = NULL, fraccion_aguda = NULL) {
  if (!is.null(lambda)) b$cfg$anchor$lambda <- lambda
  if (!is.null(rho)) b$cfg$anchor$rho_edad <- rho
  if (!is.null(fraccion_aguda)) {
    b$cfg$emr_prior$fraccion_aguda <- list(valor = as.numeric(fraccion_aguda),
                                           procedencia = b$cfg$emr_prior$fraccion_aguda$procedencia %||% "sensibilidad")
    if (identical(b$techo_emr$origen, "derivado")) {
      cfg_sin_cota <- b$cfg
      cfg_sin_cota$emr_prior$cota <- NULL                 # sin cota impresa, .dl_techo_emr deriva el techo
      techo <- .dl_techo_emr(cfg_sin_cota, b$prior_gbd)
      b$techo_emr <- techo
      b$cfg$emr_prior$cota <- techo$cota
    }
  }
  b
}
