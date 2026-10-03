# Posterior gaussiana exacta de un paseo aleatorio con prior difuso (varianza v0 enorme en el primer tiempo), para
# comparar con el filtro y el suavizador. Devuelve media y varianza en t_obj. v0 = 1e6: el sesgo del prior difuso es
# O(1/v0) (~1e-9) y la matriz sigue bien condicionada; con v0 = 1e10 solve() pierde ~1e-6 de precisión.
posterior_exacta <- function(t, y, se, q, t_obj, v0 = 1e6) {
  tt <- c(t, t_obj); t0 <- min(tt)
  K <- v0 + q * outer(tt - t0, tt - t0, pmin)
  n <- length(t)
  Koo <- K[seq_len(n), seq_len(n)] + diag(se^2, n)
  kxo <- K[n + 1L, seq_len(n)]
  list(m = drop(kxo %*% solve(Koo, y)), v = K[n + 1L, n + 1L] - drop(kxo %*% solve(Koo, kxo)))
}

test_that("el suavizador es la posterior exacta, con huecos y años sin encuesta", {
  t <- c(2018, 2019, 2021, 2024); y <- c(0.10, 0.25, 0.05, 0.18); se <- c(0.1, 0.12, 0.08, 0.15); q <- 0.01
  for (t_obj in c(2018, 2020, 2022, 2024, 2026, 2015)) {
    r <- dismodlite:::.dl_suavizar_serie(t, y, se, q, t_obj)
    e <- posterior_exacta(t, y, se, q, t_obj)
    expect_equal(r$g, e$m, tolerance = 1e-6)
    expect_equal(r$S, e$v, tolerance = 1e-6)
  }
})

test_that("q muy chico da la media ponderada; q muy grande, la edición; una edición, la edición", {
  t <- c(2018, 2020, 2023); y <- c(0.1, 0.3, 0.2); se <- c(0.1, 0.2, 0.1)
  r0 <- dismodlite:::.dl_suavizar_serie(t, y, se, 1e-12, 2023)
  expect_equal(r0$g, sum(y / se^2) / sum(1 / se^2), tolerance = 1e-6)
  rinf <- dismodlite:::.dl_suavizar_serie(t, y, se, 1e6, 2023)
  expect_equal(rinf$g, 0.2, tolerance = 1e-4)
  r1 <- dismodlite:::.dl_suavizar_serie(2020, 0.3, 0.2, 0.05, 2020)
  expect_equal(c(r1$g, r1$S), c(0.3, 0.04))
})

test_that("antes de la primera edición: el gradiente de esa edición con más varianza", {
  r <- dismodlite:::.dl_suavizar_serie(c(2019, 2021), c(0.2, 0.1), c(0.1, 0.1), 0.02, 2016)
  r19 <- dismodlite:::.dl_suavizar_serie(c(2019, 2021), c(0.2, 0.1), c(0.1, 0.1), 0.02, 2019)
  expect_equal(r$g, r19$g)
  expect_equal(r$S, r19$S + 0.02 * 3)
})

test_that("un año sin encuesta tiene más varianza que sus vecinos con encuesta", {
  t <- c(2018, 2019, 2021, 2022); y <- c(0.1, 0.2, 0.15, 0.1); se <- rep(0.1, 4)
  S <- vapply(2018:2022, function(a) dismodlite:::.dl_suavizar_serie(t, y, se, 0.01, a)$S, 0)
  expect_gt(S[3], max(S[2], S[4]))
})

test_that("q se recupera en series simuladas", {
  set.seed(42)
  q_real <- 0.01
  series <- lapply(1:200, function(i) {
    t <- c(2015, 2017, 2019, 2021, 2023)
    g <- cumsum(c(rnorm(1, 0, 0.3), rnorm(4, 0, sqrt(q_real * 2))))
    se <- runif(5, 0.05, 0.1)
    list(t = t, y = g + rnorm(5, 0, se), se = se)
  })
  q_hat <- dismodlite:::.dl_estimar_q(series)
  expect_gt(q_hat, q_real / 2)
  expect_lt(q_hat, q_real * 2)
  expect_true(is.na(dismodlite:::.dl_estimar_q(list(list(t = 2020, y = 0.1, se = 0.1)))))
})

test_that("el gradiente de una edición, en cociente y en diferencia", {
  p <- c(0.2, 0.4); se <- c(0.02, 0.04); N <- c(1, 3)
  pbar <- sum(N * p) / sum(N)
  a <- dismodlite:::.dl_gradiente_edicion(p, se, N, "cociente")
  expect_equal(a$g, log(p / pbar)); expect_equal(a$se_g, se / p)
  b <- dismodlite:::.dl_gradiente_edicion(p, se, N, "diferencia")
  expect_equal(b$g, p - pbar); expect_equal(b$se_g, se)
})

test_that("el cierre es exacto en el valor nacional", {
  g <- c(-0.2, 0.1, 0.3); S <- c(0.01, 0.02, 0.03); w <- c(0.5, 0.3, 0.2); X <- 56.4
  for (tr in c("cociente", "diferencia")) {
    r <- dismodlite:::.dl_cerrar_proxies(g, S, w, X, tr)
    expect_equal(sum(w * r$valor), X, tolerance = 1e-12)
  }
  r <- dismodlite:::.dl_cerrar_proxies(g, S, w, X, "cociente")
  expect_equal(r$se, r$valor * sqrt(S))
})

test_that("los años en desorden dan lo mismo que ordenados", {
  t <- c(2018, 2019, 2021, 2024); y <- c(0.10, 0.25, 0.05, 0.18); se <- c(0.1, 0.12, 0.08, 0.15)
  o <- c(3, 1, 4, 2)
  expect_equal(dismodlite:::.dl_suavizar_serie(t[o], y[o], se[o], 0.01, 2022),
               dismodlite:::.dl_suavizar_serie(t, y, se, 0.01, 2022))
  expect_equal(dismodlite:::.dl_kalman_nivel_local(t[o], y[o], se[o], 0.01, 2022)$loglik,
               dismodlite:::.dl_kalman_nivel_local(t, y, se, 0.01, 2022)$loglik)
})

test_that("una serie con años repetidos es un error, no una observación descartada", {
  expect_error(dismodlite:::.dl_suavizar_serie(c(2018, 2018, 2020), c(0.1, 0.2, 0.3), rep(0.1, 3), 0.01, 2020),
               "años repetidos: 2018")
  expect_error(dismodlite:::.dl_estimar_q(list(list(t = c(2018, 2018), y = c(0.1, 0.2), se = c(0.1, 0.1)))),
               "años repetidos")
})

test_that("una transformación desconocida es un error", {
  expect_error(dismodlite:::.dl_gradiente_edicion(c(0.2, 0.4), c(0.02, 0.04), c(1, 3), "log"), "desconocida")
  expect_error(dismodlite:::.dl_cerrar_proxies(0.1, 0.01, 1, 5, "log"), "desconocida")
  expect_error(dismodlite:::.dl_cerrar_proxies(0.1, 0.01, 1, 5, NULL), "desconocida")
})

test_that("el cociente con un valor menor o igual que 0 es un error; la diferencia lo acepta", {
  expect_error(dismodlite:::.dl_gradiente_edicion(c(0, 0.4), c(0.02, 0.04), c(1, 3), "cociente"), "positivos")
  expect_error(dismodlite:::.dl_gradiente_edicion(c(-0.1, 0.4), c(0.02, 0.04), c(1, 3), "cociente"), "positivos")
  expect_no_error(dismodlite:::.dl_gradiente_edicion(c(0, 0.4), c(0.02, 0.04), c(1, 3), "diferencia"))
})

test_that("el cierre con pesos que suman 0 es un error", {
  expect_error(dismodlite:::.dl_cerrar_proxies(c(0.1, 0.2), c(0.01, 0.02), c(0, 0), 5, "cociente"), "suman 0")
  expect_error(dismodlite:::.dl_cerrar_proxies(c(0.1, 0.2), c(0.01, 0.02), c(0, 0), 5, "diferencia"), "suman 0")
})
