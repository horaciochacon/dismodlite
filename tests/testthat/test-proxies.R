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

test_that("dl_calibrar_proxies cierra en el valor nacional y declara la calibración", {
  cal <- dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), anio = 2023,
                             transformacion = c(haqi = "diferencia"))
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
  cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia"))
  expect_match(attr(cal, "calibracion")$anios_poblacion, "2014.*2019")
})

# Con dos ubicaciones y A sin 2021 solo B está en todas las ediciones: no hay promedio de referencia común. Con una
# tercera ubicación sí lo hay, y A se interpola.
test_that("series desbalanceadas: una ubicación sin una edición se interpola, no se descarta", {
  cr <- crudos_toy()[-3, ]                          # A sin 2021
  expect_error(dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2021, transformacion = c(haqi = "diferencia"),
                                   anio_nacional = 2023), "solo 1 ubicaci.*excluye")
  cr <- rbind(cr, data.frame(ubicacion = "C", anio = c(2019, 2021, 2023), covariable = "haqi",
                             indicador = "índice de prueba", valor = c(50, 51, 52), error_estandar = 2))
  pob <- rbind(pob_toy(), transform(pob_toy()[pob_toy()$ubicacion == "B", ], ubicacion = "C"))
  expect_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob, 2021, transformacion = c(haqi = "diferencia"),
                                            anio_nacional = 2023), "faltan: A en 2021.*B, C")
  expect_setequal(cal$ubicacion, c("A", "B", "C"))
})

# Gradientes verdaderos constantes, sin ruido: falte o no la ubicación más grande en 2021, los valores calibrados
# son los mismos y q no se infla (con la referencia sobre todas las presentes, el resto subía un 11 %).
test_that("un panel desbalanceado da lo mismo que el balanceado si los gradientes no cambian", {
  u <- paste0("R", 1:6); N <- c(500, 100, 120, 80, 150, 50); c_d <- c(1.4, 0.6, 0.8, 1.1, 0.9, 1.2)
  nivel <- c(`2019` = 0.20, `2021` = 0.25, `2023` = 0.22)
  pob <- data.frame(ubicacion = rep(u, 2), anio = rep(c(2019, 2023), each = 6), sexo = "ambos", edad_inicio = 0,
                    edad_fin = NA, poblacion = rep(N, 2))
  cr <- data.frame(ubicacion = rep(u, 3), anio = rep(c(2019, 2021, 2023), each = 6), covariable = "x",
                   valor = rep(c_d, 3) * rep(nivel, each = 6))
  cr$error_estandar <- cr$valor * 0.05
  nac <- data.frame(anio = 2023, covariable = "x", valor = 0.22)
  bal <- suppressMessages(dl_calibrar_proxies(cr, nac, pob, 2023))
  expect_warning(des <- suppressMessages(dl_calibrar_proxies(cr[-7, ], nac, pob, 2023)), "faltan: R1 en 2021")
  expect_equal(des$valor[order(des$ubicacion)], bal$valor[order(bal$ubicacion)], tolerance = 1e-8)
  expect_lte(attr(des, "calibracion")$q, attr(bal, "calibracion")$q * 10)
  expect_identical(attr(des, "calibracion")$q_en_borde, "inferior")
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
  expect_error(dl_calibrar_proxies(cr, nac, pob, 2023, metodo = "edicion"), "x.*2 filas.*nacional.*2023.*hombres.*ubicacion: vac\u00eda, vac\u00eda")
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
  expect_identical(as.data.frame(attr(cal, "excluidas")),
                   data.frame(covariable = "haqi", anio = 2021L, motivo = "cambio de cuestionario"))
  # en el manifiesto de una corrida, cada excluida con su anio y su motivo
  px <- dismodlite:::.dl_params_proxies(cal)$proxies$haqi
  expect_identical(px$excluidas, list(list(anio = 2021L, motivo = "cambio de cuestionario")))
  expect_identical(px$ediciones, list(2019L, 2023L))
  expect_warning(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023, metodo = "edicion",
                                     excluir = data.frame(anio = 2012, motivo = "error de tipeo")),
                 "no están en proxies_crudos.*2012")
  expect_match(cal$fuente[1], "^índice de prueba: calibrado \\(paseo_aleatorio, q = .*ediciones 2019, 2023\\)")
})

test_that("sin edades, todas las edades: la población de todas sus bandas, aunque empiece después de 0", {
  # población desde 30 años, en dos bandas
  pob <- data.frame(ubicacion = rep(c("A", "B"), each = 4), anio = rep(c(2019, 2019, 2023, 2023), 2),
                    sexo = "ambos", edad_inicio = rep(c(30, 60), 4), edad_fin = rep(c(60, NA), 4),
                    poblacion = c(60, 40, 70, 40, 200, 100, 190, 100))
  cal <- dl_calibrar_proxies(crudos_toy(), nac_toy(), pob, anio = 2023, transformacion = c(haqi = "diferencia"))
  expect_false("edad_inicio" %in% names(cal))
  w <- c(A = 110, B = 290)[cal$ubicacion]
  expect_equal(sum(w * cal$valor) / sum(w), 50, tolerance = 1e-12)
  # con las columnas de edad, una covariable con las edades vacías es de todas las edades y sale sin edades
  cr <- rbind(cbind(crudos_toy(), edad_inicio = NA, edad_fin = NA),
              cbind(transform(crudos_toy(), covariable = "ldi"), edad_inicio = 60, edad_fin = NA))
  nac <- rbind(nac_toy(), transform(nac_toy(), covariable = "ldi"))
  cal2 <- dl_calibrar_proxies(cr, nac, pob, anio = 2023, transformacion = c(haqi = "diferencia"))
  expect_identical(cal2$valor[cal2$covariable == "haqi"], cal$valor)
  expect_true(all(is.na(cal2$edad_inicio[cal2$covariable == "haqi"])))
  expect_identical(unique(cal2$edad_inicio[cal2$covariable == "ldi"]), 60)
  # la banda explícita de 0 y más es la misma: todas las edades
  cal3 <- dl_calibrar_proxies(cbind(crudos_toy(), edad_inicio = 0, edad_fin = NA), nac_toy(), pob, anio = 2023,
                              transformacion = c(haqi = "diferencia"))
  expect_identical(cal3$valor, cal$valor)
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

test_that("q en el borde del intervalo: se declara; el inferior se informa y el superior se avisa", {
  cr <- crudos_toy(); cr$valor <- rep(c(40, 60), 3)   # el gradiente no cambia: q al borde inferior
  expect_no_warning(expect_message(
    cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")),
    "borde inferior"))
  expect_identical(attr(cal, "calibracion")$q_en_borde, "inferior")
  # saltos de unos 6 puntos frente a un error de 0,05: q pasaría de 1e3 σ̄² = 2,5e-3; queda en el borde superior
  cr$valor <- c(40, 60, 47, 52, 41, 61); cr$error_estandar <- 0.05
  expect_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")),
                 "borde superior")
  expect_identical(attr(cal, "calibracion")$q_en_borde, "superior")
  cr$valor <- c(40, 60, 44, 58, 40, 61); cr$error_estandar <- 1     # el gradiente se mueve más que su error
  expect_no_warning(cal <- dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")))
  expect_identical(attr(cal, "calibracion")$q_en_borde, NA_character_)
})

# Con `diferencia`, q está en unidades del indicador al cuadrado: el intervalo de búsqueda es relativo a σ̄² (la
# mediana de se_g²), así que el mismo indicador en otra escala da los mismos valores (por el factor) y el mismo borde.
test_that("q no depende de la escala del indicador: ×100 y ÷100 dan lo mismo, salvo el factor", {
  cr <- crudos_toy(); cr$valor <- c(40, 60, 44, 58, 40, 61); cr$error_estandar <- 1
  calibrar <- function(f) {
    x <- cr; x$valor <- x$valor * f; x$error_estandar <- x$error_estandar * f
    dl_calibrar_proxies(x, nac_toy(50 * f), pob_toy(), 2023, transformacion = c(haqi = "diferencia"))
  }
  base <- calibrar(1)
  for (f in c(100, 1 / 100)) {
    expect_no_warning(cal <- calibrar(f))
    expect_equal(cal$valor / f, base$valor, tolerance = 1e-6)
    expect_equal(cal$error_estandar / f, base$error_estandar, tolerance = 1e-6)
    expect_equal(attr(cal, "calibracion")$q / f^2, attr(base, "calibracion")$q, tolerance = 1e-6)
    expect_identical(attr(cal, "calibracion")$q_en_borde, NA_character_)
  }
})

test_that("calibracion$ediciones son las usadas; con el paseo aleatorio, usada es TRUE en todas", {
  cal <- dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), anio = 2022, metodo = "edicion",
                             transformacion = c(haqi = "diferencia"), anio_nacional = 2023)
  expect_identical(attr(cal, "calibracion")$ediciones, "2021")
  cal <- suppressMessages(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), anio = 2022,
                                              transformacion = c(haqi = "diferencia"), anio_nacional = 2023))
  expect_identical(attr(cal, "calibracion")$ediciones, "2019, 2021, 2023")
  expect_true(all(attr(cal, "series")$usada))
  cr <- crudos_toy()[crudos_toy()$anio == 2023, ]         # sin q: como edicion
  cal <- suppressWarnings(dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023, transformacion = c(haqi = "diferencia")))
  expect_identical(attr(cal, "calibracion")$ediciones, "2023")
})

test_that("la edición 2021 con población de 2019 y 2023 usa 2019 (empate: el anterior)", {
  cal <- suppressMessages(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023,
                                              transformacion = c(haqi = "diferencia")))
  expect_match(attr(cal, "calibracion")$anios_poblacion, "2021 \u2192 2019")
  s <- attr(cal, "series"); s21 <- s[s$anio == 2021]
  pbar <- (42 * 100 + 58 * 300) / 400                    # pesos de 2019
  expect_equal(s21$g[order(s21$ubicacion)], c(42, 58) - pbar)
})

test_that("R1: la fila nacional con código convive con una subnacional de los crudos en covariables", {
  cv <- data.frame(ubicacion = c("PE", "A"), anio = 2023, covariable = "haqi", valor = c(50, 99))
  cal <- dl_calibrar_proxies(crudos_toy(), cv, pob_toy(), 2023, metodo = "edicion",
                             transformacion = c(haqi = "diferencia"))
  w <- c(A = 110, B = 290)[cal$ubicacion]
  expect_equal(sum(w * cal$valor) / sum(w), 50, tolerance = 1e-12)
})

test_that("errores menores: sexo que falta en la población y años de excluir no enteros", {
  cr <- crudos_toy(); cr$sexo <- "mujeres"
  expect_error(dl_calibrar_proxies(cr, nac_toy(), pob_toy(), 2023), "poblaci\u00f3n no trae mujeres")
  expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023,
                                   excluir = data.frame(anio = 2021.5, motivo = "x")), "enteros")
  for (a in c(Inf, -Inf, NaN))
    expect_error(dl_calibrar_proxies(crudos_toy(), nac_toy(), pob_toy(), 2023,
                                     excluir = data.frame(anio = a, motivo = "x")), "enteros")
})

# La carpeta covariables/ del ejemplo trae las descargas del GHDx con varias ubicaciones (el país y otras): sin
# ubicacion_gbd no se sabe cuál es el valor nacional; con ella, da lo mismo que la tabla ya leída del proyecto.
test_that("ubicacion_gbd: la carpeta de descargas del GHDx del ejemplo, con varias ubicaciones", {
  d <- dl_ejemplo()
  args <- list(file.path(d, "proxies_crudos.csv"), file.path(d, "covariables"), file.path(d, "poblacion.csv"),
               anio = 2023, transformacion = c(haqi = "diferencia"))
  expect_error(do.call(dl_calibrar_proxies, args), "varias ubicaciones.*ubicacion_gbd")
  cal <- suppressMessages(do.call(dl_calibrar_proxies, c(args, ubicacion_gbd = 123)))
  p <- suppressMessages(dl_proyecto(d, causa = 9100))
  expect_equal(cal$valor, p$calibracion$valor)
  expect_identical(cal$ubicacion, p$calibracion$ubicacion)
})

# El año del proyecto cambiado después de traducirlo: los proxies (calibrados o no) son del año con que se tradujo,
# y la cascada fallaba después, sin decir por qué. Ahora, un error temprano que dice qué hacer.
test_that("proxies de otro año que el que se estima: error temprano que pide volver a traducir", {
  p <- suppressMessages(dl_proyecto(dl_ejemplo(), causa = 9100))
  p$configuracion$years$ajuste <- 2019L
  expect_error(suppressMessages(dl_insumos(p)), "proxies.*2023.*2019.*dl_proyecto\\(\\).*anio: 2019")
  expect_error(suppressMessages(dl_insumos(p$configuracion, p$rutas)), "proxies.*2023.*2019")
})
