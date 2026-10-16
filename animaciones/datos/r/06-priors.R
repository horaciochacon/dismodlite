# Escena 06, los priors: la suavidad de log i (segundas diferencias en los nudos, sigma) y el prior de la mortalidad
# en exceso (csmr / p del ancla por banda, su paso a los nudos y el techo). Hombres (sex_id 1) del proyecto de
# ejemplo (causa 9100, datos sintéticos).
exportar <- function(ctx) {
  b <- ctx$insumos()
  ajuste <- ctx$ajuste()
  sexo <- 1L
  nudos <- as.numeric(unlist(b$cfg$nudos_incidencia))
  sigma <- as.numeric(b$cfg$sigma_suavidad)

  # El prior por banda del ancla, tal como lo da dl_prior_emr()
  prior <- dl_prior_emr(b)
  bandas <- prior[sex_id == sexo][order(age_start)]

  # El prior en los nudos, con el paso de banda a nudo del paquete (.dl_ctx_emr, el que usa la log-posterior)
  en_nudos <- dismodlite:::.dl_ctx_emr(b, sexo, nudos)

  # La media posterior de theta en los nudos (dl_ajustar, 4 cadenas, semilla 1)
  draws <- ajuste$draws_par[[as.character(sexo)]]
  log_i <- unname(colMeans(draws[, paste0("logi_", nudos)]))
  log_f <- unname(colMeans(draws[, paste0("logf_", nudos)]))
  log_f_lo <- unname(apply(draws[, paste0("logf_", nudos)], 2, stats::quantile, 0.025))
  log_f_hi <- unname(apply(draws[, paste0("logf_", nudos)], 2, stats::quantile, 0.975))

  d2 <- diff(diff(log_i))
  techo <- b$techo_emr

  # El costo de doblar log i (+0.6 en el nudo de 60, ilustrativo) con sigma = 0.5 y con sigma = 1 (ilustrativo):
  # cuánto baja L_suavidad, con la función del paquete
  doblar <- 0.6
  li_doblada <- log_i
  li_doblada[nudos == 60] <- li_doblada[nudos == 60] + doblar
  costo <- function(s) dismodlite:::.dl_lp_suavidad(log_i, s) - dismodlite:::.dl_lp_suavidad(li_doblada, s)

  # Las variantes del prior de f, con la misma configuración del ejemplo salvo mortalidad_exceso.prior:
  #   plano: el piso y el techo de la cota (.dl_ctx_emr -> .dl_emr_plano);
  #   tabla: la del ejemplo de ?dl_prior_emr (el prior del ancla con el primer tramo extendido hasta el nudo de 30),
  #          por tramo (dl_prior_emr) y en los nudos (.dl_ctx_emr).
  variante <- function(tipo, ...) {
    config <- yaml::read_yaml(file.path(dl_ejemplo(), "config", "9100.yaml"))
    config$mortalidad_exceso$prior <- tipo
    suppressMessages(dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100, configuracion = config, ...)))
  }
  b_plano <- variante("plano")
  todas <- dl_prior_emr(b)
  tabla <- data.frame(sexo = ifelse(todas$sex_id == 1, "hombres", "mujeres"),
                      edad_inicio = todas$age_start, edad_fin = todas$age_end,
                      mu_log = todas$mu_log, sd_log = todas$sd_log)
  tabla$edad_inicio[tabla$edad_inicio == min(bandas$age_start)] <- min(nudos)
  b_tabla <- variante("tabla", prior_mortalidad_exceso = tabla)
  tramos <- dl_prior_emr(b_tabla)[sex_id == sexo][order(age_start)]
  tabla_nudos <- dismodlite:::.dl_ctx_emr(b_tabla, sexo, nudos)

  # El techo por defecto (sin mortalidad_exceso.techo): 3 veces la mayor csmr / p del ancla, en los dos sexos
  techo_defecto <- dismodlite:::.DL_EMR_TECHO_K * max(exp(prior$mu_log))

  list(
    fuente = paste0("Datos sintéticos (causa 9100), hombres; techo por defecto: dos sexos. dl_prior_emr() y media ",
                    "posterior de dl_ajustar(). Ilustrativos: deformaciones de log i, σ = 1 y la tabla"),
    sexo = sexo, nudos = nudos, sigma = sigma,
    log_i = log_i, d2 = d2,
    l_suavidad = dismodlite:::.dl_lp_suavidad(log_i, sigma),
    bandas = list(age_start = bandas$age_start, age_end = bandas$age_end, mu_log = bandas$mu_log,
                  sd_log = bandas$sd_log, f_por_mil = 1000 * exp(bandas$mu_log)),
    nudo_mu_log = en_nudos$mu_log, nudo_sd_log = en_nudos$sd_log,
    log_f = log_f, log_f_lo = log_f_lo, log_f_hi = log_f_hi,
    l_emr = sum(stats::dnorm(log_f, en_nudos$mu_log, en_nudos$sd_log, log = TRUE)),
    techo = techo$cota[2], techo_origen = techo$origen, techo_defecto = techo_defecto,
    factor_techo = dismodlite:::.DL_EMR_TECHO_K, sd_fuera = dismodlite:::.DL_EMR_SD_FUERA_DE_BANDAS,
    plano_rango = dismodlite:::.DL_EMR_PLANO_RANGO,
    piso_plano = dismodlite:::.dl_ctx_emr(b_plano, sexo, nudos)$cota[1],
    techo_plano = dismodlite:::.dl_ctx_emr(b_plano, sexo, nudos)$cota[2],
    doblar = doblar, costo_05 = costo(0.5), costo_1 = costo(1),
    tabla = list(age_start = tramos$age_start, age_end = tramos$age_end, mu_log = tramos$mu_log,
                 sd_log = tramos$sd_log),
    tabla_nudo_mu_log = tabla_nudos$mu_log, tabla_nudo_sd_log = tabla_nudos$sd_log,
    fraccion_aguda = dismodlite:::.dl_fraccion_aguda(b$cfg), factor_sd = dismodlite:::.dl_factor_sd_emr(b$cfg)
  )
}
