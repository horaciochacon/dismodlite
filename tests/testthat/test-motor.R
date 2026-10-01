fit9100 <- function(b, seed = 20260830L)
  dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 400L, cadenas = 2L, iteraciones = 9000L,
                                            calentamiento = 3000L, nucleos = 1L, adelgazamiento = 10L),
             semilla = seed)

q_por_banda <- function(f, b, sx) {
  ctx <- .dl_ctx(b, sx)
  qmat <- vapply(seq_len(nrow(f$draws_par[[as.character(sx)]])), function(k) {
    p <- .dl_edo_ctx(f$draws_par[[as.character(sx)]][k, ], ctx)$p
    vapply(ctx$pesos_ancla, function(pw) sum(p[pw$idx] * pw$w), numeric(1))
  }, numeric(nrow(ctx$ancla$bandas)))
  list(bandas = ctx$ancla$bandas, q_med = apply(qmat, 1, stats::median))
}

test_that("anchor_identity: lambda = 1 sin datos reproduce el ancla GBD (error relativo mediano < 5 %)", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- fit9100(b)
  err <- unlist(lapply(c(1L, 2L), function(sx) {
    r <- q_por_banda(f, b, sx)
    abs(r$q_med / r$bandas$val - 1)
  }))
  expect_lt(stats::median(err), 0.05)
  # Diagnósticos sanos con la corrida corta (el umbral estricto, R-hat < 1.01, lo aplica dl_exportar_corrida()):
  expect_lt(max(f$mcmc$rhat), 1.05)
  expect_gt(min(f$mcmc$ess), 50)
  # Celdas fuera del intervalo de incertidumbre del ancla: no hacen fallar la prueba, se informan con un mensaje
  fuera <- unlist(lapply(c(1L, 2L), function(sx) {
    r <- q_por_banda(f, b, sx)
    sum(r$q_med < r$bandas$lower | r$q_med > r$bandas$upper)
  }))
  if (sum(fuera) > 0)
    message(sprintf("anchor_identity: %d celda(s) fuera del intervalo de incertidumbre del ancla", sum(fuera)))
})

test_that("recuperación de parámetros sintéticos con semilla fija", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  ctx <- .dl_ctx(b, 1L)
  nk <- length(ctx$nudos)
  theta_v <- c(seq(-8, -4, length.out = nk),                        # logi* creciente
               pmin(ctx$emr$mu_log - 0.1, log(0.05)))               # logf* dentro de cota y prior
  sol_v <- .dl_edo_ctx(theta_v, ctx)
  q_v <- vapply(ctx$pesos_ancla, function(pw) sum(sol_v$p[pw$idx] * pw$w), numeric(1))
  set.seed(99)
  pr <- b$prior_gbd
  filas <- pr$measure_id == 5L & pr$sex_id == 1L
  val_sim <- q_v * exp(stats::rnorm(length(q_v), 0, 0.05))
  pr[filas, `:=`(val = val_sim, lower = val_sim * exp(-1.96 * 0.1),
                 upper = val_sim * exp(1.96 * 0.1), sigma_log = 0.1)]
  b$prior_gbd <- pr
  cfg1 <- b$cfg; cfg1$sexos <- list(1L); b$cfg <- cfg1
  f <- fit9100(b, seed = 31L)
  r <- q_por_banda(f, b, 1L)
  expect_lt(stats::median(abs(r$q_med / q_v - 1)), 0.05)
  # cobertura de los nudos de incidencia: logi* dentro del 95% central en >= 5 de 7 nudos
  dr <- f$draws_par[["1"]][, seq_len(nk), drop = FALSE]
  dentro <- vapply(seq_len(nk), function(j) {
    ci <- stats::quantile(dr[, j], c(0.025, 0.975))
    theta_v[j] >= ci[1] && theta_v[j] <= ci[2]
  }, logical(1))
  expect_gte(sum(dentro), 5L)
})
