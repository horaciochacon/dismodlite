# La ecuación de la prevalencia (R/edo.R): soluciones cerradas, orden de RK4, matriz W y remisión por edad.

# Integral exacta de i(a) = exp(interpolación lineal de log i entre nudos) entre a0 y a1: referencia del invariante.
int_i_analitica <- function(theta_logi, nudos, a0, a1) {
  cortes <- sort(unique(c(a0, a1, nudos[nudos > a0 & nudos < a1])))
  logi_en <- function(x) stats::approx(nudos, theta_logi, xout = min(max(x, nudos[1]), nudos[length(nudos)]), rule = 2)$y
  tot <- 0
  for (k in seq_len(length(cortes) - 1)) {
    u0 <- cortes[k]; u1 <- cortes[k + 1]
    l0 <- logi_en(u0); l1 <- logi_en(u1); m <- (l1 - l0) / (u1 - u0)
    tot <- tot + if (abs(m) < 1e-12) exp(l0) * (u1 - u0) else (exp(l1) - exp(l0)) / m
  }
  tot
}

nudos <- c(25, 40, 50, 60, 70, 80, 95)
logi  <- c(-8.5, -7.2, -6.4, -5.6, -5.0, -4.5, -4.2)

test_that("dp/da sale del sistema de compartimentos y no depende de m", {
  set.seed(3)
  S <- runif(20, 100, 1000); C <- runif(20, 1, 100); i <- runif(20, 0, 0.05); r <- runif(20, 0, 0.5)
  f <- runif(20, 0, 0.3)
  for (m in c(0, 0.01, 0.2)) {
    dS <- -(i + m) * S + r * C
    dC <- i * S - (r + m + f) * C
    dp <- (dC * (S + C) - C * (dS + dC)) / (S + C)^2   # derivada de p = C / (S + C)
    expect_equal(.dl_dp_da(i, r, f, C / (S + C)), dp, tolerance = 1e-12)
  }
})

test_that("las etapas de RK4 escritas en línea son .dl_dp_da (idéntico bit a bit)", {
  set.seed(4)
  nsub <- 5L; n_media <- 2L * nsub * 30L + 1L; h <- 1 / nsub
  i_media <- exp(rnorm(n_media, -5, 0.5)); f_media <- exp(rnorm(n_media, -3, 0.5)); r_media <- runif(n_media, 0, 0.5)
  p <- 0.01
  for (k in seq_len((n_media - 1L) %/% 2L)) {
    j <- 2L * k - 1L
    k1 <- .dl_dp_da(i_media[j], r_media[j], f_media[j], p[k])
    k2 <- .dl_dp_da(i_media[j + 1L], r_media[j + 1L], f_media[j + 1L], p[k] + h / 2 * k1)
    k3 <- .dl_dp_da(i_media[j + 1L], r_media[j + 1L], f_media[j + 1L], p[k] + h / 2 * k2)
    k4 <- .dl_dp_da(i_media[j + 2L], r_media[j + 2L], f_media[j + 2L], p[k] + h * k3)
    p[k + 1L] <- p[k] + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  expect_identical(dl_edo_resolver(i_media, f_media, remision = r_media, p0 = 0.01, nsub = nsub), p)
})

test_that("sin mortalidad en exceso, la EDO reproduce la solución cerrada", {
  i <- 0.01; r <- 0.05; nsub <- 5L; anios <- 40
  p <- dl_edo_resolver(rep(i, 2 * nsub * anios + 1), rep(0, 2 * nsub * anios + 1), remision = r, nsub = nsub)
  t <- seq(0, anios, by = 1 / nsub)
  expect_equal(p, i / (i + r) * (1 - exp(-(i + r) * t)), tolerance = 1e-9)
})

test_that("sin remisión y tasas constantes, la EDO reproduce la solución de Riccati", {
  i <- 0.01; f <- 0.08; nsub <- 5L; anios <- 40
  p <- dl_edo_resolver(rep(i, 2 * nsub * anios + 1), rep(f, 2 * nsub * anios + 1), nsub = nsub)
  t <- seq(0, anios, by = 1 / nsub); K <- i * exp((i - f) * t)
  expect_equal(p, (K - i) / (K - f), tolerance = 1e-9)
})

test_that("con remisión y mortalidad en exceso a la vez, la EDO reproduce la solución de Riccati", {
  # dp/da = i - (i + r + f) p + f p^2 = f (p - p_menos) (p - p_mas), con p_menos < p_mas las raíces de
  # f p^2 - (i + r + f) p + i = 0. Desde p(0) = 0: p(t) = p_menos p_mas (1 - E) / (p_mas - p_menos E),
  # E = exp(-f (p_mas - p_menos) t); p tiende a p_menos.
  i <- 0.01; r <- 0.05; f <- 0.08; nsub <- 5L; anios <- 40
  p <- dl_edo_resolver(rep(i, 2 * nsub * anios + 1), rep(f, 2 * nsub * anios + 1), remision = r, nsub = nsub)
  b <- i + r + f; raiz <- sqrt(b^2 - 4 * f * i)
  p_menos <- (b - raiz) / (2 * f); p_mas <- (b + raiz) / (2 * f)
  t <- seq(0, anios, by = 1 / nsub); E <- exp(-f * (p_mas - p_menos) * t)
  expect_equal(p, p_menos * p_mas * (1 - E) / (p_mas - p_menos * E), tolerance = 1e-9)
})

test_that("RK4 converge con orden 4", {
  i <- 0.02; f <- 0.3; anios <- 20
  exacta <- { K <- i * exp((i - f) * anios); (K - i) / (K - f) }
  err <- vapply(c(1L, 2L), function(ns) { p <- dl_edo_resolver(rep(i, 2 * ns * anios + 1),
    rep(f, 2 * ns * anios + 1), nsub = ns); abs(p[length(p)] - exacta) }, numeric(1))
  expect_gt(err[1] / err[2], 10); expect_lt(err[1] / err[2], 22)
})

test_that("con el nsub por defecto el error relativo es menor que 1e-6 aun con f = 0,3", {
  i <- 0.02; f <- 0.3; anios <- 80; nsub <- .DL_NSUB
  p <- dl_edo_resolver(rep(i, 2 * nsub * anios + 1), rep(f, 2 * nsub * anios + 1), nsub = nsub)
  t <- seq(0, anios, by = 1 / nsub); K <- i * exp((i - f) * t); exacta <- (K - i) / (K - f)
  expect_lt(max(abs(p[-1] - exacta[-1]) / exacta[-1]), 1e-6)
})

test_that("la matriz W interpola exacto en los nudos, suma 1 por fila y extrapola constante", {
  nudos <- c(30, 40, 60, 95); W <- dismodlite:::.dl_base_interp(nudos, c(25, 30, 35, 40, 95, 99))
  expect_equal(rowSums(W), rep(1, 6)); expect_equal(W[2, ], c(1, 0, 0, 0)); expect_equal(W[1, ], W[2, ])
  expect_equal(W[3, 1:2], c(0.5, 0.5)); expect_equal(W[6, ], c(0, 0, 0, 1))
})

test_that("la base de interpolación coincide con approx rule = 2", {
  ev <- c(20, 25, 33.3, 40, 62.5, 95, 99)
  W <- .dl_base_interp(nudos, ev)
  expect_equal(as.vector(W %*% logi),
               stats::approx(nudos, logi, xout = pmin(pmax(ev, 25), 95), rule = 2)$y,
               tolerance = 1e-12)
})

test_that("mallas: la malla h/2 tiene 2 n_anios nsub + 1 puntos y las edades enteras caen donde se espera", {
  nsub <- 5L; e <- .dl_edades_media(30L, 99L, nsub)
  expect_length(e, 2L * 69L * nsub + 1L)
  expect_equal(e[.dl_idx_anual_media(length(e), nsub)], as.numeric(30:99), tolerance = 1e-14)
  expect_identical(.dl_idx_anual_malla(69L, nsub), .dl_idx_anual_media(length(e), nsub) %/% 2L + 1L)
})

test_that("los valores por defecto de las firmas son las constantes .DL_NSUB y .DL_EDAD_FIN", {
  expect_identical(formals(dl_edo_resolver)$nsub, .DL_NSUB)
  expect_identical(formals(dl_edo)$nsub, .DL_NSUB)
  expect_equal(formals(dl_edo)$edad_fin, .DL_EDAD_FIN)
})

test_that("invariante: r = 0, f = 0 => p = 1 - exp(-int i)", {
  s <- dl_edo(logi, rep(-30, 7), nudos, edad_inicio = 25)
  p_ref <- vapply(s$edades, function(a) 1 - exp(-int_i_analitica(logi, nudos, 25, a)), numeric(1))
  expect_lt(max(abs(s$p - p_ref)), 1e-7)
})

test_that("f > 0 baja la prevalencia y p queda en [0, 1]", {
  s0 <- dl_edo(logi, rep(-30, 7), nudos, edad_inicio = 25)
  s1 <- dl_edo(logi, rep(log(0.04), 7), nudos, edad_inicio = 25)
  expect_true(all(s1$p[-1] < s0$p[-1]))
  expect_true(all(s1$p >= 0 & s1$p <= 1))
})

test_that("p0 y edad_fin se respetan", {
  s <- dl_edo(logi, rep(-30, 7), nudos, edad_inicio = 25, edad_fin = 60, p0 = 0.01)
  expect_identical(s$edades, seq(25, 60))
  expect_equal(s$p[1], 0.01)
})

# La remisión puede ser un vector sobre la malla h/2 (un valor por punto, como i_media); un número vale para todas
# las edades. Remisión alta en la infancia, como en una enfermedad aguda que se cura.
test_that("dl_edo_resolver acepta r vectorial por punto de la malla y coincide con el escalar cuando es constante", {
  nsub <- 5L; edades_media <- seq(25, 60, by = 1 / (2 * nsub))
  W <- .dl_base_interp(nudos, edades_media)
  i_media <- exp(as.vector(W %*% logi)); f_media <- rep(0.01, length(i_media))
  p_esc <- dl_edo_resolver(i_media, f_media, remision = 0.3, p0 = 0, nsub = nsub)
  p_vec <- dl_edo_resolver(i_media, f_media, remision = rep(0.3, length(i_media)), p0 = 0, nsub = nsub)
  expect_equal(p_vec, p_esc, tolerance = 1e-14)
  # r = 4 hasta los 40 y 0 después: hasta los 40 coincide con el escalar 4; después la prevalencia sube más
  r_tramo <- ifelse(edades_media < 40, 4, 0)
  p_tr <- dl_edo_resolver(i_media, f_media, remision = r_tramo, p0 = 0, nsub = nsub)
  p_4 <- dl_edo_resolver(i_media, f_media, remision = 4, p0 = 0, nsub = nsub)
  idx <- .dl_idx_anual_malla(35, nsub)
  # hasta los 39 idéntico; el paso de 39 a 40 ya usa r = 0 en el punto 40 de la malla h/2 (RK4), así que difiere
  expect_equal(p_tr[idx][seq(25, 39) - 24], p_4[idx][seq(25, 39) - 24], tolerance = 1e-12)
  expect_true(all(p_tr[idx][seq(45, 60) - 24] > p_4[idx][seq(45, 60) - 24]))
  expect_error(dl_edo_resolver(i_media, f_media, remision = c(1, 2), p0 = 0, nsub = nsub),
               "^dl_edo_resolver\\(\\): `remision` .*longitud")
  expect_error(dl_edo_resolver(i_media, f_media[-1], nsub = nsub), "^dl_edo_resolver\\(\\): `f_media`")
})
