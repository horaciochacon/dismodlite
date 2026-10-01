# Muestreador (R/muestreador.R): recupera una normal conocida, es reproducible, adapta solo durante el calentamiento,
# adelgaza como dice su encabezado, deriva las semillas de cada cadena de (seed, cadena) y calcula R-hat y ESS con
# las fórmulas documentadas. Los mensajes nombran la función y el argumento.

lp_normal2 <- function(theta) {
  # Normal 2D correlacionada: mu = (1, -1), sd = (1, 2), rho = 0.6
  S <- matrix(c(1, 1.2, 1.2, 4), 2)
  z <- theta - c(1, -1)
  -0.5 * drop(t(z) %*% solve(S) %*% z)
}

lp_normal_estandar <- function(theta) -0.5 * sum(theta^2)

test_that("dl_mh recupera media y covarianza de una normal 2D", {
  th0 <- c(a = 0, b = 0)
  r <- dl_mh(lp_normal2, th0, bloques = list(1:2), iter = 22000L, warmup = 2000L,
                  seed = 11L, thin = 4L)
  expect_identical(colnames(r$draws), c("a", "b"))
  expect_lt(max(abs(colMeans(r$draws) - c(1, -1))), 0.15)
  expect_lt(abs(stats::cov(r$draws)[1, 2] - 1.2), 0.4)
  expect_gt(r$aceptacion[1], 0.1); expect_lt(r$aceptacion[1], 0.6)
})

test_that("dl_mh es determinista con la misma seed y exige lp finita en theta0", {
  th0 <- c(a = 0, b = 0)
  r1 <- dl_mh(lp_normal2, th0, list(1:2), 2200L, 200L, seed = 5L)
  r2 <- dl_mh(lp_normal2, th0, list(1:2), 2200L, 200L, seed = 5L)
  expect_identical(r1$draws, r2$draws)
  expect_error(dl_mh(function(t) -Inf, th0, list(1:2), 2200L, 200L, seed = 5L), "^dl_mh\\(\\): .*finita")
})

test_that("bloques separados adaptan escalas independientes", {
  r <- dl_mh(lp_normal2, c(a = 0, b = 0), bloques = list(1L, 2L),
                  iter = 6200L, warmup = 2000L, seed = 7L)
  expect_length(r$aceptacion, 2L)
  expect_length(r$escala, 2L)
})

test_that("sin calentamiento no hay adaptaci\u00f3n: la escala queda en 2.38 / sqrt(d_b)", {
  r <- dl_mh(lp_normal_estandar, c(a = 0, b = 0, c = 0), bloques = list(1:2, 3L), iter = 300L, warmup = 0L,
             seed = 3L)
  expect_identical(r$escala, c(2.38 / sqrt(2), 2.38 / sqrt(1)))
  expect_identical(nrow(r$draws), 300L)
})

test_that("despu\u00e9s del calentamiento escala y forma quedan fijas: una cadena m\u00e1s larga extiende la corta", {
  corta <- dl_mh(lp_normal2, c(a = 0, b = 0), list(1:2), iter = 1400L, warmup = 400L, seed = 9L)
  larga <- dl_mh(lp_normal2, c(a = 0, b = 0), list(1:2), iter = 2400L, warmup = 400L, seed = 9L)
  expect_identical(larga$draws[seq_len(1000L), ], corta$draws)
  expect_identical(larga$escala, corta$escala)
})

test_that("el adelgazamiento guarda theta en las iteraciones T_cal + j thin", {
  todas <- dl_mh(lp_normal2, c(a = 0, b = 0), list(1:2), iter = 1400L, warmup = 200L, seed = 4L)
  cada3 <- dl_mh(lp_normal2, c(a = 0, b = 0), list(1:2), iter = 1400L, warmup = 200L, seed = 4L, thin = 3L)
  cada7 <- dl_mh(lp_normal2, c(a = 0, b = 0), list(1:2), iter = 1400L, warmup = 200L, seed = 4L, thin = 7L)
  expect_identical(cada3$draws, todas$draws[seq(3L, 1200L, by = 3L), ])
  expect_identical(cada7$draws, todas$draws[seq(7L, 1197L, by = 7L), ])   # floor(1200 / 7) = 171 filas
  # la aceptación es la de todas las iteraciones posteriores al calentamiento, guardadas o no
  expect_identical(cada3$aceptacion, todas$aceptacion)
})

test_that("la escala sigue el paso de Robbins-Monro hacia una aceptaci\u00f3n de 0.28", {
  escala_adaptada <- dismodlite:::.dl_mh_escala_adaptada
  expect_identical(escala_adaptada(1.5, 0.28, 4), 1.5)
  expect_equal(escala_adaptada(1, 0.48, 4), exp(0.1))     # (0.48 - 0.28) / sqrt(4)
  expect_lt(escala_adaptada(1, 0.1, 1), 1)
})

test_that("la forma de la propuesta es el factor de Cholesky (triangular inferior) de la covarianza", {
  set.seed(2)
  theta_b <- matrix(rnorm(200), ncol = 2) %*% matrix(c(1, 0.5, 0, 2), 2)
  L <- dismodlite:::.dl_mh_forma_propuesta(theta_b)
  expect_equal(L %*% t(L), stats::cov(theta_b) + diag(1e-8, 2), tolerance = 1e-12)
  expect_identical(L[1, 2], 0)
})

test_that("cada cadena usa las semillas s 1000 + c (inicio) y s 1000 + c + 500 (cadena)", {
  th0 <- c(a = 0, b = 0); s <- 20260929L; cad <- 2L   # s 1000 no cabe en un entero de R
  r <- dismodlite:::.dl_correr_cadena(lp_normal2, th0, list(1:2), cadena = cad, iter = 600L, warmup = 200L, seed = s)
  set.seed((s * 1000 + cad) %% 2147483647)
  inicio <- th0 + rnorm(2, 0, 0.1)
  a_mano <- dl_mh(lp_normal2, inicio, list(1:2), 600L, 200L, seed = (s * 1000 + cad + 500) %% 2147483647)
  expect_identical(r, a_mano)
})

test_that("dl_mh_cadenas: cada cadena depende solo de (seed, cadena), en serie o en paralelo", {
  th0 <- c(a = 0, b = 0)
  cadenas <- dl_mh_cadenas(lp_normal2, th0, list(1:2), chains = 3L, iter = 600L, warmup = 200L, seed = 5L)
  expect_length(cadenas, 3L)
  expect_identical(cadenas[[3]], dismodlite:::.dl_correr_cadena(lp_normal2, th0, list(1:2), cadena = 3L, iter = 600L,
                                                                warmup = 200L, seed = 5L))
  skip_on_os("windows")
  expect_identical(dl_mh_cadenas(lp_normal2, th0, list(1:2), chains = 3L, iter = 600L, warmup = 200L, seed = 5L,
                                 cores = 2L), cadenas)
})

test_that("rhat ~ 1 en cadenas iid y > 1.1 en cadenas desplazadas; ess razonable", {
  set.seed(1)
  iid <- lapply(1:4, function(k) list(draws = matrix(rnorm(2000), ncol = 2,
                                                     dimnames = list(NULL, c("a", "b")))))
  rh <- dl_rhat(lapply(iid, `[[`, "draws"))
  expect_lt(max(rh), 1.02)
  mal <- iid; mal[[1]]$draws <- mal[[1]]$draws + 3
  expect_gt(max(dl_rhat(lapply(mal, `[[`, "draws"))), 1.1)
  ess <- dl_ess(lapply(iid, `[[`, "draws"))
  expect_gt(min(ess), 1500)   # iid: ESS ~ n total (4000), con margen
})

test_that("R-hat partido coincide con la f\u00f3rmula de Gelman et al. calculada a mano", {
  set.seed(3)
  cadenas <- list(matrix(rnorm(21), ncol = 1), matrix(rnorm(21, 0.3), ncol = 1))
  n <- 10   # 21 %/% 2: la fila 21 de cada cadena queda fuera
  mitades <- list(cadenas[[1]][1:10, 1], cadenas[[1]][11:20, 1], cadenas[[2]][1:10, 1], cadenas[[2]][11:20, 1])
  var_entre <- n * var(vapply(mitades, mean, numeric(1)))
  var_dentro <- mean(vapply(mitades, var, numeric(1)))
  expect_equal(unname(dl_rhat(cadenas)), sqrt(((n - 1) / n * var_dentro + var_entre / n) / var_dentro),
               tolerance = 1e-14)
})

test_that("la suma de Geyer para en el primer par negativo y deja fuera un rezago suelto", {
  suma_geyer <- dismodlite:::.dl_suma_geyer
  expect_equal(suma_geyer(c(0.5, 0.3, 0.1, -0.2, 0.4, 0.4)), 0.8)   # pares 0.8 | -0.1 (para)
  expect_equal(suma_geyer(c(0.2, 0.1, 0.3)), 0.3)                    # t_max impar: el rezago 3 queda fuera
})

test_that("ESS coincide con n_cadenas n_sim / (1 + 2 suma de Geyer) y se acerca al de un AR(1)", {
  set.seed(4)
  ar1 <- function(n, phi) as.numeric(stats::filter(rnorm(n), phi, method = "recursive"))
  cadenas <- list(matrix(ar1(2000, 0.6)), matrix(ar1(2000, 0.6)))
  t_max <- 40L
  autocor <- rowMeans(vapply(cadenas, function(x) acf(x[, 1], lag.max = t_max, plot = FALSE)$acf[-1],
                             numeric(t_max)))
  suma <- 0
  for (k in seq(1, t_max - 1, by = 2)) {
    if (autocor[k] + autocor[k + 1] < 0) break
    suma <- suma + (autocor[k] + autocor[k + 1])
  }
  ess <- unname(dl_ess(cadenas, max_lag = t_max))
  expect_equal(ess, 2 * 2000 / (1 + 2 * suma), tolerance = 1e-12)
  expect_gt(ess, 600); expect_lt(ess, 1600)   # AR(1) con phi = 0.6: n (1 - phi) / (1 + phi) = 1000
})

test_that("mensajes en espa\u00f1ol que nombran la funci\u00f3n y el argumento", {
  th0 <- c(a = 0, b = 0)
  expect_error(dl_mh(lp_normal2, th0, list(1:2), 2200L, 200L), "^dl_mh\\(\\): falta `seed` \\(semilla\\)")
  expect_error(dl_mh(lp_normal2, th0, list(1:2), 2200L, 150L, seed = 1L),
               "^dl_mh\\(\\): `warmup` \\(calentamiento\\) debe ser un m\u00faltiplo de 200")
  expect_error(dl_mh(lp_normal2, th0, list(1:2), 200L, 200L, seed = 1L), "^dl_mh\\(\\): `iter`.*mayor que `warmup`")
  expect_error(dl_mh(lp_normal2, th0, list(1:2), 2200L, 200L, seed = 1L, thin = 0L), "^dl_mh\\(\\): `thin`")
  expect_error(dl_mh(lp_normal2, th0, list(1:3), 2200L, 200L, seed = 1L), "^dl_mh\\(\\): `bloques`")
  expect_error(dl_mh(lp_normal2, th0, list(1:2), 2200L, 200L, seed = 1.5),
               "^dl_mh\\(\\): `seed` \\(semilla\\) debe ser un n\u00famero entero")
  expect_error(dl_mh(lp_normal2, th0, list(1:2), 2200L, 200L, seed = 3e9), "^dl_mh\\(\\): `seed` .*entre")
  expect_error(dl_mh_cadenas(lp_normal2, th0, list(1:2), chains = 2L, iter = 2200L, warmup = 200L),
               "^dl_mh_cadenas\\(\\): falta `seed`")
  expect_error(dl_mh_cadenas(lp_normal2, th0, list(1:2), chains = 0L, iter = 2200L, warmup = 200L, seed = 1L),
               "^dl_mh_cadenas\\(\\): `chains` \\(cadenas\\)")
  m <- matrix(rnorm(20), ncol = 2)
  expect_error(dl_rhat(m), "^dl_rhat\\(\\): `draws_list` debe ser una lista de matrices")
  expect_error(dl_rhat(list(m, m[-1, ])), "^dl_rhat\\(\\): .*mismas filas")
  expect_error(dl_ess(list(m[1:3, ])), "^dl_ess\\(\\): .*al menos 4 simulaciones")
  expect_error(dl_ess(list(m), max_lag = 1), "^dl_ess\\(\\): `max_lag`")
})
