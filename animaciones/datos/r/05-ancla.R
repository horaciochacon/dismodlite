# Escena 05, el ancla: las bandas de prevalencia del ancla de los hombres (val, lower, upper y el sigma_log que
# calcula dl_insumos()), la matriz de correlación AR(1) que usa la log-posterior (.dl_chol_ar1, R/verosimilitud.R),
# la prevalencia por banda del ajuste del ejemplo (ctx$ajuste()) y la de seis ajustes cortos con otros lambda y rho,
# hechos como los hace dl_sensibilidad() (.dl_bundle_con y las cadenas de .DL_OPCIONES_SENSIBILIDAD).
exportar <- function(ctx) {
  b <- ctx$insumos()
  sexo <- 1L
  ancla <- dismodlite:::.dl_ancla_sexo(b, sexo)            # las bandas de prevalencia, ordenadas por edad
  n <- nrow(ancla)
  lambda <- b$cfg$anchor$lambda
  rho <- b$cfg$anchor$rho_edad

  # R = U'U con U = chol(R), la que guarda el contexto de la log-posterior.
  U <- dismodlite:::.dl_chol_ar1(n, rho)
  R <- crossprod(U)

  # Prevalencia del modelo por banda q_b (promedio ponderado por la población, R/bandas.R): media e IC 95 %.
  bandas <- function(f, bk) {
    q <- dismodlite:::.dl_q_bandas(f, bk, locs = bk$loc_ancla)
    s <- dismodlite:::.dl_stats_bandas(q)[sex_id == sexo]
    s <- s[match(ancla$age_group_id, s$age_group_id)]
    list(val = s$val, lower = s$lower, upper = s$upper, ancho = mean((s$upper - s$lower) / s$val),
         simulaciones = data.table::uniqueN(q$draw))
  }
  ajuste <- bandas(ctx$ajuste(), b)
  op_sens <- do.call(dl_opciones_mcmc, dismodlite:::.DL_OPCIONES_SENSIBILIDAD)
  sensibilidad <- list()
  for (lam in c(1, 0.5)) for (r in c(0, 0.5, 0.9)) {
    bk <- dismodlite:::.dl_bundle_con(b, lambda = lam, rho = r)
    s <- bandas(dl_ajustar(bk, op_sens, semilla = ctx$semilla), bk)
    sensibilidad[[length(sensibilidad) + 1L]] <- c(list(lambda = lam, rho = r), s)
  }

  # L_ancla en la media posterior de q_b: la función del paquete y la fórmula de la guía con la matriz densa.
  z <- log(ajuste$val) - log(ancla$val)
  Sigma <- diag(ancla$sigma_log) %*% R %*% diag(ancla$sigma_log) / lambda
  l_formula <- -0.5 * (n * log(2 * pi) + as.numeric(determinant(Sigma)$modulus) + sum(z * solve(Sigma, z)))
  l_paquete <- dismodlite:::.dl_lp_mvn_ar1(z, ancla$sigma_log, U, lambda)

  list(
    fuente = paste0("Ejemplo sintético (causa 9100), hombres · ajuste: 4 cadenas × 4000 iteraciones · ",
                    "sensibilidad: 2 × 6000, como dl_sensibilidad()"),
    sexo = sexo, lambda = lambda, rho = rho, ancho_ic95_en_sd = dismodlite:::.DL_ANCHO_IC95_EN_SD,
    ancla = list(age_group_id = ancla$age_group_id, age_start = ancla$age_start, age_end = ancla$age_end,
                 val = ancla$val, lower = ancla$lower, upper = ancla$upper, sigma_log = ancla$sigma_log),
    R = unname(lapply(seq_len(n), function(k) R[k, ])),
    ajuste = ajuste,
    opciones_ajuste = unclass(ctx$opciones())[c("draws", "chains", "iter", "warmup")],
    opciones_sensibilidad = dismodlite:::.DL_OPCIONES_SENSIBILIDAD,
    sensibilidad = sensibilidad,
    z = z, l_ancla = list(formula = l_formula, paquete = l_paquete)
  )
}
