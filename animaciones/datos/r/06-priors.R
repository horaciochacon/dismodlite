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

  # El techo por defecto (sin mortalidad_exceso.techo): 3 veces la mayor csmr / p del ancla, en los dos sexos
  techo_defecto <- dismodlite:::.DL_EMR_TECHO_K * max(exp(prior$mu_log))

  list(
    fuente = paste0("Datos sintéticos (causa 9100), hombres; el techo por defecto, en los dos sexos. dl_prior_emr() y ",
                    "media posterior de dl_ajustar(). Ilustrativos: deformaciones de log i y σ = 1"),
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
    fraccion_aguda = dismodlite:::.dl_fraccion_aguda(b$cfg), factor_sd = dismodlite:::.dl_factor_sd_emr(b$cfg)
  )
}
