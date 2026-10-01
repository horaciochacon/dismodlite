# La log-posterior (R/verosimilitud.R): contexto, términos frente a su cuenta directa, remisión y prior de EMR.

b9100 <- function() dl_insumos(cfg9100(), rutas_nacional())

test_that("los pesos de intervalo usan población por banda y normalizan a 1", {
  b <- b9100(); ctx <- .dl_ctx(b, 1L); pobl <- b$poblacion[location_id == b$loc_ancla & sex_id == 1L]
  pw <- .dl_pesos_intervalo(ctx$anual, 75, 85, pobl, b$bandas_pobl)
  expect_identical(ctx$anual[pw$idx], 75:84)
  expect_equal(sum(pw$w), 1, tolerance = 1e-12)
  # 75-79 y 80-84 pesan según las bandas 20 y 30 (80-84, la fina de la población del ejemplo)
  p20 <- pobl[age_group_id == 20L, val]; p30 <- pobl[age_group_id == 30L, val]
  expect_equal(sum(pw$w[1:5]) / sum(pw$w[6:10]), p20 / p30, tolerance = 1e-12)
})

test_that("MVN-AR(1): con rho = 0 y lambda = 1 se reduce a normales independientes", {
  z <- c(0.1, -0.2, 0.05); s <- c(0.2, 0.3, 0.25)
  R0 <- chol(diag(3))
  esperado <- sum(stats::dnorm(z, 0, s, log = TRUE))
  expect_equal(.dl_lp_mvn_ar1(z, s, R0, 1), esperado, tolerance = 1e-10)
})

test_that("lambda escala la cuadrática y el determinante (power prior)", {
  z <- c(0.1, -0.2, 0.05); s <- c(0.2, 0.3, 0.25)
  R <- chol(0.5^abs(outer(1:3, 1:3, "-")))
  lp1 <- .dl_lp_mvn_ar1(z, s, R, 1)
  lp05 <- .dl_lp_mvn_ar1(z, s, R, 0.5)
  # densidad MVN(0, Sigma/lambda): quad*lambda y +n/2*log(lambda)
  Rfull <- 0.5^abs(outer(1:3, 1:3, "-")); Sig <- diag(s) %*% Rfull %*% diag(s)
  ref <- function(l) -0.5 * (l * drop(t(z) %*% solve(Sig) %*% z) + determinant(Sig)$modulus[1] -
                             3 * log(l) + 3 * log(2 * pi))
  expect_equal(lp1, ref(1), tolerance = 1e-10)
  expect_equal(lp05, ref(0.5), tolerance = 1e-10)
})

test_that("la log-posterior es finita en el punto inicial y -Inf fuera de la cota EMR", {
  b <- b9100(); ctx <- .dl_ctx(b, 1L)
  th0 <- .dl_theta_inicial(ctx)
  expect_true(is.finite(.dl_log_post(th0, ctx)))
  th_mal <- th0; th_mal[length(th0)] <- log(0.26)      # EMR > 0.25
  expect_identical(.dl_log_post(th_mal, ctx), -Inf)
})

test_that("componentes: sin datos, datos = 0; el ancla filtra solo prevalencia", {
  b <- b9100(); ctx <- .dl_ctx(b, 1L)
  expect_identical(nrow(ctx$ancla$bandas), nrow(b$prior_gbd[measure_id == 5L & sex_id == 1L]))
  comp <- .dl_lp_componentes(.dl_theta_inicial(ctx), ctx)
  expect_identical(unname(comp["datos"]), 0)
  expect_equal(unname(comp["total"]), sum(comp[c("suavidad", "emr", "ancla", "datos")]), tolerance = 1e-12)
})

lp_lognormal <- function(q, val, se, eta = 0) stats::dnorm(log(q + eta), log(val + eta), se / (val + eta), log = TRUE)

test_that("despacho por tipo: cada fila de .DL_TIPOS_DATO contribuye según su integrando (val, se)", {
  b <- b9100(); b$cfg$medidas_entrada <- list("prev_estudio", "incidencia", "csmr")
  filas <- rbind(
    fila_datos(dato_id = "p1", tipo_dato = "prev_estudio", sex_id = 1L, age_start = 55, age_end = 60, val = 0.02, se = 0.004),
    fila_datos(dato_id = "i1", tipo_dato = "incidencia", measure_id = 6L, measure_name = "Incidence",
               sex_id = 1L, age_start = 60, age_end = 65, val = 0.003, se = 0.0006),
    fila_datos(dato_id = "c1", tipo_dato = "csmr", measure_id = 900006L, measure_name = "csmr",
               sex_id = 1L, age_start = 70, age_end = 75, val = 0.0004, se = 0.0001,
               ajuste_completitud = TRUE, completitud = 0.9, acquisition_id = "fixture_csmr_datos_v1"))
  b$datos <- filas
  ctx <- .dl_ctx(b, 1L)
  expect_identical(ctx$datos$tipo_cod, c(1L, 2L, 3L))
  expect_true(all(ctx$datos$ruta == 0L))
  th <- .dl_theta_inicial(ctx)
  comp <- .dl_lp_componentes(th, ctx)
  expect_setequal(names(comp), c("suavidad", "emr", "ancla", "datos", "total",
                                 "datos_prev_estudio", "datos_incidencia", "datos_csmr"))
  sol <- .dl_edo_ctx(th, ctx)
  q <- function(g, j) sum(g[ctx$pesos_datos[[j]]$idx] * ctx$pesos_datos[[j]]$w)
  expect_equal(unname(comp["datos_prev_estudio"]), lp_lognormal(q(sol$p, 1), 0.02, 0.004), tolerance = 1e-12)
  expect_equal(unname(comp["datos_incidencia"]), lp_lognormal(q(sol$i * (1 - sol$p), 2), 0.003, 0.0006), tolerance = 1e-12)
  expect_equal(unname(comp["datos_csmr"]), lp_lognormal(q(sol$p * sol$f, 3), 0.0004, 0.0001), tolerance = 1e-12)
  expect_equal(unname(comp["datos"]), sum(comp[c("datos_prev_estudio", "datos_incidencia", "datos_csmr")]), tolerance = 1e-12)
  expect_equal(unname(comp["total"]), sum(comp[c("suavidad", "emr", "ancla", "datos")]), tolerance = 1e-12)
})

test_that("ruta de conteos: binomial para prevalencia (n_efectivo manda), Poisson para incidencia y csmr", {
  b <- b9100(); b$cfg$medidas_entrada <- list("prev_estudio", "incidencia", "csmr")
  b$datos <- rbind(
    fila_datos(dato_id = "p1", sex_id = 1L, age_start = 55, age_end = 60, val = NA_real_, se = NA_real_,
               x = 30L, n = 1500L, n_efectivo = 1000),
    fila_datos(dato_id = "i1", tipo_dato = "incidencia", measure_id = 6L, measure_name = "Incidence",
               sex_id = 1L, age_start = 60, age_end = 65, val = NA_real_, se = NA_real_, x = 12L, n = 4000L))
  ctx <- .dl_ctx(b, 1L)
  expect_identical(ctx$datos$ruta, c(1L, 1L)); expect_identical(ctx$datos$n_ef, c(1000, 4000))
  th <- .dl_theta_inicial(ctx); sol <- .dl_edo_ctx(th, ctx)
  q <- function(g, j) sum(g[ctx$pesos_datos[[j]]$idx] * ctx$pesos_datos[[j]]$w)
  comp <- .dl_lp_componentes(th, ctx)
  qp <- q(sol$p, 1); qi <- q(sol$i * (1 - sol$p), 2)
  expect_equal(unname(comp["datos_prev_estudio"]), 30 * log(qp) + 970 * log1p(-qp), tolerance = 1e-12)
  expect_equal(unname(comp["datos_incidencia"]), 12 * log(4000 * qi) - 4000 * qi, tolerance = 1e-12)
})

test_that("offset_lognormal: val = 0 exige eta y con eta entra; tipos no admitidos fallan; nivel 1 no entra", {
  b <- b9100(); b$cfg$medidas_entrada <- list("prev_estudio", "prev_admin")
  b$datos <- fila_datos(sex_id = 1L, age_start = 55, age_end = 60, val = 0, se = 0.001)
  expect_error(.dl_ctx(b, 1L), "val <= 0.*offset_lognormal")
  b$cfg$offset_lognormal <- 1e-6
  ctx <- .dl_ctx(b, 1L)
  expect_equal(ctx$datos$eta, 1e-6)
  expect_true(is.finite(.dl_lp_componentes(.dl_theta_inicial(ctx), ctx)["datos"]))
  b$datos[1, tipo_dato := "prev_admin"]
  expect_error(.dl_ctx(b, 1L), "no admite datos locales de tipo_dato prev_admin")
  b$datos <- fila_datos(sex_id = 1L, location_id = "01", location_name = "Amazonas", location_level = 1L,
                        age_start = 55, age_end = 60, val = 0.02, se = 0.004)
  ctx <- .dl_ctx(b, 1L)
  expect_identical(nrow(ctx$datos), 0L)
})

# Remisión fija por causa: dp/da = i (1 - p) - r p - f p (1 - p) con la r de la configuración.
test_that("la remisión de la configuración entra a la EDO de la verosimilitud (R y C++)", {
  skip_if_not_installed("Rcpp")
  b <- dl_insumos(cfg9100(), rutas_nacional())
  b12 <- b; b12$cfg$remision$valor <- 12
  ctx0 <- .dl_ctx(b, 1L); ctx12 <- .dl_ctx(b12, 1L)
  expect_true(all(ctx0$r_media == 0)); expect_true(all(ctx12$r_media == 12))   # vector sobre la malla h/2
  th <- .dl_theta_inicial(ctx0)
  s0 <- .dl_edo_ctx(th, ctx0); s12 <- .dl_edo_ctx(th, ctx12)
  expect_true(all(s12$p <= s0$p)); expect_lt(max(s12$p) / max(s0$p), 0.5)   # r = 12 vacía el estado
  # paridad con el solver directo y con C++
  nk <- length(ctx12$nudos)
  i_media <- exp(as.vector(ctx12$W %*% th[seq_len(nk)])); f_media <- exp(as.vector(ctx12$W %*% th[nk + seq_len(nk)]))
  p_dir <- dl_edo_resolver(i_media, f_media, remision = 12, p0 = 0, nsub = ctx12$nsub)
  expect_equal(s12$p, p_dir[ctx12$idx_anual_malla])
  lpr <- .dl_lp_rcpp(ctx12)
  expect_equal(unname(lpr$componentes(th)), unname(.dl_lp_componentes(th, ctx12)), tolerance = 1e-9)
  expect_false(isTRUE(all.equal(lpr$total(th), unname(.dl_lp_componentes(th, ctx0)[["total"]]))))
})

test_that("remision.por_edad llega al contexto como vector sobre la malla h/2, con paridad R/C++", {
  skip_if_not_installed("Rcpp")
  b <- dl_insumos(cfg9100(), rutas_nacional())
  bt <- b; bt$cfg$remision$por_edad <- list(list(edad_inicio = 30, edad_fin = 45, valor = 2, fuente = "test"))
  ctx <- .dl_ctx(bt, 1L)
  edades_media <- seq(ctx$edad_inicio, ctx$edad_fin, by = 1 / (2 * ctx$nsub))
  expect_equal(ctx$edades_media, edades_media)
  expect_length(ctx$r_media, length(edades_media))
  expect_equal(ctx$r_media, ifelse(edades_media >= 30 & edades_media < 45, 2, 0))
  th <- .dl_theta_inicial(ctx)
  lpr <- .dl_lp_rcpp(ctx)
  expect_equal(unname(lpr$componentes(th)), unname(.dl_lp_componentes(th, ctx)), tolerance = 1e-9)
  s_tr <- .dl_edo_ctx(th, ctx); s_0 <- .dl_edo_ctx(th, .dl_ctx(b, 1L))
  expect_true(all(s_tr$p <= s_0$p)); expect_lt(s_tr$p[ctx$anual == 44] / s_0$p[ctx$anual == 44], 0.9)
})

# Prior de EMR plano dentro del techo: el término de EMR vale 0 y solo queda la cota; paridad R/C++.
test_that("emr_prior.tipo plano_cota: sin término de prior de EMR, solo truncamiento a la cota", {
  skip_if_not_installed("Rcpp")
  cfg <- cfg9100(); cfg$emr_prior$tipo <- "plano_cota"; cfg$emr_prior$tipo_procedencia <- "test"
  b <- dl_insumos(cfg, rutas_nacional())
  ctx <- .dl_ctx(b, 1L)
  expect_true(isTRUE(ctx$emr$plano)); expect_equal(ctx$emr$cota, c(0.25 / 1000, 0.25))   # piso techo/1000: prior propio
  th <- .dl_theta_inicial(ctx)
  expect_true(all(is.finite(th)))
  r <- .dl_lp_componentes(th, ctx)
  expect_equal(unname(r[["emr"]]), 0)
  lpr <- .dl_lp_rcpp(ctx)
  expect_equal(unname(lpr$componentes(th)), unname(r), tolerance = 1e-9)
  th2 <- th; th2[length(th)] <- log(0.26)                # f > cota => -Inf igual que antes
  expect_true(all(is.infinite(lpr$componentes(th2))))
  th3 <- th; th3[length(th)] <- log(0.25 / 1e5)           # f bajo el piso => -Inf (prior propio)
  expect_true(all(is.infinite(lpr$componentes(th3)))); expect_true(all(is.infinite(.dl_lp_componentes(th3, ctx))))
  # con el prior informativo el término no es cero
  ctx_i <- .dl_ctx(dl_insumos(cfg9100(), rutas_nacional()), 1L)
  expect_false(unname(.dl_lp_componentes(th, ctx_i)[["emr"]]) == 0)
  # plano_cota con cota null: el bundle materializa csmr y deriva el techo (k x max csmr/prev)
  cfg2 <- cfg; cfg2$emr_prior$cota <- NULL
  b2 <- dl_insumos(cfg2, rutas_nacional())
  expect_identical(b2$techo_emr$origen, "derivado")
  ctx2 <- .dl_ctx(b2, 1L)
  expect_true(isTRUE(ctx2$emr$plano)); expect_equal(ctx2$emr$cota[2], b2$techo_emr$cota[2])
})

test_that(".dl_edo_log_media: desplazar log i y log f en la malla equivale a desplazar los nudos; techo en la malla", {
  b <- dl_insumos(cfg9100(), rutas_nacional()); ctx <- .dl_ctx(b, 1L)
  nk <- length(ctx$nudos); theta <- c(rep(log(0.01), nk), rep(log(0.05), nk))
  m <- .dl_log_tasas_media(theta, ctx)
  expect_length(m$log_i, nrow(ctx$W)); expect_equal(nrow(ctx$W), length(ctx$edades_media))
  a <- .dl_edo_ctx(theta + c(rep(0.3, nk), rep(-0.2, nk)), ctx)
  s <- .dl_edo_log_media(m$log_i + 0.3, m$log_f - 0.2, ctx)
  expect_equal(s$p, a$p, tolerance = 1e-12); expect_equal(s$i, a$i, tolerance = 1e-12); expect_equal(s$f, a$f, tolerance = 1e-12)
  expect_equal(s$truncado, 0L)
  t <- .dl_edo_log_media(m$log_i, m$log_f + 5, ctx, techo_f = 0.1)
  expect_equal(t$truncado, 1L); expect_true(all(t$f <= 0.1 + 1e-12))
  # un desplazamiento por tramos: solo cambia i donde no es 0
  delta <- ifelse(ctx$edades_media >= 60, 0.5, 0)
  u <- .dl_edo_log_media(m$log_i + delta, m$log_f, ctx); base <- .dl_edo_ctx(theta, ctx)
  expect_equal(u$i[ctx$anual < 60], base$i[ctx$anual < 60]); expect_true(all(u$i[ctx$anual >= 60] > base$i[ctx$anual >= 60]))
})

# ---- Cada término de la log-posterior frente a su cuenta directa ----

test_that("el término del ancla coincide con la normal multivariada densa", {
  set.seed(1); n <- 6; z <- rnorm(n, 0, 0.1); s <- runif(n, 0.05, 0.2); rho <- 0.5; lambda <- 0.7
  R <- rho^abs(outer(seq_len(n), seq_len(n), "-")); Sig <- diag(s) %*% R %*% diag(s)
  denso <- -0.5 * (lambda * drop(t(z) %*% solve(Sig, z)) + log(det(Sig)) - n * log(lambda) + n * log(2 * pi))
  expect_equal(dismodlite:::.dl_lp_mvn_ar1(z, s, chol(R), lambda), denso, tolerance = 1e-10)
  expect_identical(.dl_chol_ar1(n, rho), chol(R))
})

test_that("suavidad: suma de log dnorm de las segundas diferencias de log i en los nudos", {
  set.seed(5); logi <- sort(rnorm(7, -6, 1)); sigma <- 0.5
  expect_identical(.dl_lp_suavidad(logi, sigma), sum(stats::dnorm(diff(diff(logi)), 0, sigma, log = TRUE)))
  d2 <- (logi[3:7] - logi[2:6]) - (logi[2:6] - logi[1:5])
  expect_equal(.dl_lp_suavidad(logi, sigma), sum(-0.5 * log(2 * pi) - log(sigma) - d2^2 / (2 * sigma^2)),
               tolerance = 1e-12)
})

test_that("prior de EMR: log-normal por nudo, 0 con el prior plano y truncamiento a la cota", {
  logf <- log(c(0.01, 0.02, 0.05))
  emr <- list(mu_log = log(c(0.012, 0.02, 0.04)), sd_log = c(0.3, 0.4, 0.5), plano = FALSE)
  expect_equal(.dl_lp_prior_emr(logf, emr), sum(stats::dnorm(logf, emr$mu_log, emr$sd_log, log = TRUE)),
               tolerance = 1e-14)
  expect_identical(.dl_lp_prior_emr(logf, utils::modifyList(emr, list(plano = TRUE))), 0)
  expect_true(.dl_f_fuera_de_cota(log(c(0.01, 0.3)), c(0, 0.25)))
  expect_false(.dl_f_fuera_de_cota(log(c(0.01, 0.25)), c(0, 0.25)))
})

test_that("datos: log-normal, binomial y Poisson coinciden con dnorm, dbinom y dpois salvo las constantes omitidas", {
  q <- c(0.01, 0.03, 0.2); x <- c(3, 12, 80); n <- c(400, 500, 350)
  expect_equal(.dl_lp_binomial(q, x, n), stats::dbinom(x, n, q, log = TRUE) - lchoose(n, x), tolerance = 1e-12)
  expect_equal(.dl_lp_poisson(q, x, n), stats::dpois(x, n * q, log = TRUE) + lgamma(x + 1), tolerance = 1e-12)
  val <- c(0.012, 0.025, 0.18); se <- c(0.003, 0.004, 0.02); eta <- 1e-4; s_log <- se / (val + eta)
  expect_identical(.dl_lp_lognormal(q, val, s_log, eta),
                   stats::dnorm(log(q + eta), log(val + eta), s_log, log = TRUE))
  # la constante omitida no depende de q: entre dos q la diferencia es la de la densidad completa
  q2 <- c(0.02, 0.05)
  expect_equal(diff(.dl_lp_binomial(q2, 12, 500)), diff(stats::dbinom(12, 500, q2, log = TRUE)), tolerance = 1e-12)
  expect_equal(diff(.dl_lp_poisson(q2, 12, 500)), diff(stats::dpois(12, 500 * q2, log = TRUE)), tolerance = 1e-12)
})

test_that("la log-posterior es la suma de sus cuatro términos y cada uno sale de su función", {
  b <- dl_insumos(cfg9100_datos(), rutas_completas()); ctx <- .dl_ctx(b, 1L)
  expect_gt(nrow(ctx$datos), 0L)
  set.seed(6); nk <- length(ctx$nudos)
  th <- .dl_theta_inicial(ctx) + stats::rnorm(2 * nk, 0, 0.1)
  comp <- .dl_lp_componentes(th, ctx); sol <- .dl_edo_ctx(th, ctx)
  expect_identical(comp[["suavidad"]], .dl_lp_suavidad(th[seq_len(nk)], ctx$sigma_suavidad))
  expect_identical(comp[["emr"]], .dl_lp_prior_emr(th[nk + seq_len(nk)], ctx$emr))
  expect_identical(comp[["ancla"]], .dl_lp_ancla(.dl_q_intervalos(sol$p, ctx$pesos_ancla), ctx$ancla))
  expect_identical(comp[paste0("datos_", .DL_TIPOS_DATO$tipo)], .dl_lp_datos(sol, ctx))
  expect_true(all(is.finite(comp)) && comp[["datos"]] != 0)
  expect_identical(comp[["total"]], comp[["suavidad"]] + comp[["emr"]] + comp[["ancla"]] + comp[["datos"]])
  expect_identical(names(comp), .DL_LP_NOMBRES)
})

test_that("el punto inicial: log i = .DL_LOG_I_INICIAL y log f al menos .DL_MARGEN_LOG_F_INICIAL bajo el techo", {
  ctx <- .dl_ctx(b9100(), 1L); th <- .dl_theta_inicial(ctx); nk <- length(ctx$nudos)
  expect_true(all(th[seq_len(nk)] == .DL_LOG_I_INICIAL))
  expect_equal(unname(th[nk + seq_len(nk)]), pmin(ctx$emr$mu_log, log(ctx$emr$cota[2]) - .DL_MARGEN_LOG_F_INICIAL))
  expect_true(all(th[nk + seq_len(nk)] <= log(ctx$emr$cota[2]) - .DL_MARGEN_LOG_F_INICIAL))
  expect_identical(names(th), c(paste0("logi_", ctx$nudos), paste0("logf_", ctx$nudos)))
})
