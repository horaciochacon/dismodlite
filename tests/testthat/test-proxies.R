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

# ---- dl_calibrar_proxies() ----

# La población del contrato exige las columnas de edad: una sola banda abierta desde 0 (todas las edades).
pob_toy <- function() data.frame(ubicacion = rep(c("A", "B", "N"), each = 2), anio = rep(c(2019, 2023), 3),
                                 sexo = "ambos", edad_inicio = 0, edad_fin = NA,
                                 poblacion = c(100, 110, 300, 290, 400, 400))
nac_toy <- function(v = 50) data.frame(anio = 2023L, sexo = "ambos", covariable = "haqi", valor = v,
                                       inferior = v - 2, superior = v + 2)
crudos_toy <- function() data.frame(ubicacion = rep(c("A", "B"), 3), anio = rep(c(2019, 2021, 2023), each = 2),
                                    covariable = "haqi", indicador = "índice de prueba",
                                    valor = c(40, 60, 42, 58, 41, 61), error_estandar = 2)

# En los crudos de prueba el gradiente casi no cambia entre ediciones (menos que su error): q queda en el borde
# inferior y la función lo avisa.
test_that("dl_calibrar_proxies cierra en el valor nacional y declara la calibración", {
  expect_warning(cal <- dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), anio = 2023,
                                            transformacion = c(haqi = "diferencia")), "borde inferior")
  expect_s3_class(cal, "dl_tabla")
  w <- c(A = 110, B = 290)[cal$ubicacion]
  expect_equal(sum(w * cal$valor) / sum(w), 50, tolerance = 1e-12)
  k <- attr(cal, "calibracion")
  expect_identical(k$metodo, "paseo_aleatorio")
  expect_true(is.finite(k$q))
  expect_true(all(c("g", "se_g", "g_suavizado", "S") %in% names(attr(cal, "series"))))
})

test_that("edicion usa la edición del año o la más cercana", {
  cal <- dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), anio = 2022, metodo = "edicion",
                             transformacion = c(haqi = "diferencia"), anio_nacional = 2023)
  s <- attr(cal, "series")
  expect_identical(unique(s$anio[s$usada]), 2021L)   # 2021 y 2023 a la misma distancia: la anterior
})

test_that("una sola edición por serie: paseo_aleatorio avisa y usa la edición", {
  cr <- crudos_toy()[crudos_toy()$anio == 2023, ]
  expect_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")),
                 "haqi.*una sola edici")
  expect_true(is.na(attr(cal, "calibracion")$q))
})

test_that("ediciones fuera de los años de la población usan el año más cercano y lo declaran", {
  cr <- crudos_toy(); cr$anio <- cr$anio - 5L      # 2014, 2016, 2018
  expect_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")),
                 "borde inferior")
  expect_match(attr(cal, "calibracion")$anios_poblacion, "2014.*2019")
})

test_that("series desbalanceadas: una ubicación sin una edición se interpola, no se descarta", {
  cr <- crudos_toy()[-3, ]                          # A sin 2021
  expect_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2021, transformacion = c(haqi = "diferencia"),
                                            anio_nacional = 2023), "borde inferior")
  expect_setequal(cal$ubicacion, c("A", "B"))
})

test_that("errores claros de dl_calibrar_proxies", {
  cr <- crudos_toy(); cr$valor[1] <- -1
  expect_error(dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023), "cociente.*mayor que 0")
  expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023,
                                   excluir = data.frame(anio = 2021, motivo = NA)), "motivo")
  expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023, transformacion = c(otra = "cociente")),
               "otra")
  expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy()[pob_toy()$ubicacion != "B", ], 2023),
               "B.*poblaci")
  expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy()[0, ], pob_toy(), 2023), "haqi.*nacional.*2023")
  # banda de los crudos que cruza una banda de la población
  pob <- data.frame(ubicacion = rep(c("A", "B"), each = 2), anio = 2023, sexo = "ambos",
                    edad_inicio = c(0, 15), edad_fin = c(15, NA), poblacion = 100)
  cr <- crudos_toy(); cr$edad_inicio <- 10; cr$edad_fin <- 50
  expect_error(dl_calibrar_proxies(cr, nac_toy(), pob, 2023), "10-49")
})

test_that("más errores: error estándar 0, transformación desconocida, valor nacional ambiguo", {
  cr <- crudos_toy(); cr$error_estandar[2] <- 0
  expect_error(dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023), "error_estandar.*mayor que 0")
  expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "log")),
               "desconocida")
  # dos filas nacionales que sirven igual: una del sexo y de todas las edades, otra de ambos y de la banda
  pob <- data.frame(ubicacion = rep(c("A", "B"), each = 4), anio = 2023, sexo = rep(c("hombres", "mujeres"), 4),
                    edad_inicio = rep(c(0, 0, 15, 15), 2), edad_fin = rep(c(15, 15, NA, NA), 2), poblacion = 100)
  cr <- data.frame(ubicacion = c("A", "B"), anio = 2023, sexo = "hombres", edad_inicio = 15, edad_fin = NA,
                   covariable = "x", valor = c(1, 2), error_estandar = 0.1)
  nac <- data.frame(anio = 2023, covariable = "x", sexo = c("hombres", "ambos"), edad_inicio = c(NA, 15),
                    edad_fin = NA, valor = c(1.5, 1.6))
  expect_error(dl_calibrar_proxies(cr, nac, pob, 2023, metodo = "edicion"), "x.*2 filas.*nacional.*2023.*hombres")
  # la que coincide en sexo y banda gana sobre las que coinciden en menos
  nac$edad_inicio[1] <- 15; nac$valor[1] <- 1.7
  cal <- dl_calibrar_proxies(cr, nac, pob, 2023, metodo = "edicion")
  expect_equal(sum(cal$valor) / 2, 1.7, tolerance = 1e-12)
})

test_that("exclusiones: la edición no entra y queda declarada con su motivo", {
  cal <- suppressWarnings(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023,
                                              transformacion = c(haqi = "diferencia"),
                                              excluir = data.frame(anio = 2021, motivo = "cambio de cuestionario")))
  expect_false(2021L %in% attr(cal, "series")$anio)
  expect_match(attr(cal, "calibracion")$excluidas, "2021.*cambio de cuestionario")
  expect_warning(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023, metodo = "edicion",
                                     excluir = data.frame(anio = 2012, motivo = "error de tipeo")),
                 "no están en proxies_crudos.*2012")
  expect_match(cal$fuente[1], "^índice de prueba: calibrado \\(paseo_aleatorio, q = .*ediciones 2019, 2023\\)")
})

test_that("cociente por sexo y banda: suma los sexos de la población y cierra en el nacional de cada banda", {
  pob <- expand.grid(ubicacion = c("A", "B"), anio = 2023, sexo = c("hombres", "mujeres"),
                     edad_inicio = c(0, 15, 50), stringsAsFactors = FALSE)
  pob$edad_fin <- c(15, 50, NA)[match(pob$edad_inicio, c(0, 15, 50))]
  pob$poblacion <- seq_len(nrow(pob)) * 10
  cr <- data.frame(ubicacion = rep(c("A", "B"), 2), anio = rep(c(2021, 2023), each = 2), covariable = "x",
                   edad_inicio = 15, edad_fin = 125, valor = c(0.2, 0.4, 0.25, 0.35), error_estandar = 0.02)
  nac <- data.frame(anio = 2023, covariable = "x", sexo = "ambos", edad_inicio = c(0, 15), edad_fin = c(15, NA),
                    valor = c(0.1, 0.3))
  cal <- dl_calibrar_proxies(cr, nac, pob, 2023, metodo = "edicion")
  N <- function(u) sum(pob$poblacion[pob$ubicacion == u & pob$edad_inicio >= 15])
  w <- vapply(cal$ubicacion, N, 0)
  expect_equal(sum(w * cal$valor) / sum(w), 0.3, tolerance = 1e-12)
  expect_equal(cal$error_estandar, unname(cal$valor * 0.02 / c(A = 0.25, B = 0.35)[cal$ubicacion]))
  expect_identical(unique(cal$edad_inicio), 15)
})

test_that("q en el borde del intervalo: se declara y se avisa", {
  cr <- crudos_toy(); cr$valor <- rep(c(40, 60), 3)   # el gradiente no cambia: q al borde inferior
  expect_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")),
                 "borde inferior")
  expect_true(attr(cal, "calibracion")$q_en_borde)
  cr$valor <- c(40, 60, 44, 58, 40, 61); cr$error_estandar <- 1     # el gradiente se mueve más que su error
  expect_no_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")))
  expect_false(attr(cal, "calibracion")$q_en_borde)
})
