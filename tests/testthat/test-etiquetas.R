# dl_etiquetas(): contracción frente al ajuste solo con el ancla, rejilla de rho y etiqueta más conservadora.

test_that("umbrales de .dl_etiqueta: contracción 0.05 y 0.3, desplazamiento 0.05", {
  et <- .dl_etiqueta
  expect_equal(et(0.5,  0.5), "data-driven")
  expect_equal(et(0.1,  0.5), "prior-informed")
  expect_equal(et(0.04, 0.5), "prior-driven")
  expect_equal(et(0.5,  0.01), "prior-driven")   # un desplazamiento pequeño manda sobre la contracción
  # en los umbrales: c = 0.05 y d = 0.05 ya no son pequeños; c = 0.3 ya es grande
  expect_equal(et(0.05, 0.05), "prior-informed")
  expect_equal(et(0.3,  0.5), "data-driven")
  expect_equal(et(0.299, 0.5), "prior-informed")
})

test_that("9100 sin datos locales: toda celda es prior-driven con contracción 0", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  # las opciones y la semilla del ajuste de test-avd.R: en la suite completa sale de la cache de la sesión
  o <- dl_opciones_mcmc(simulaciones = 60L, cadenas = 2L, iteraciones = 3000L, calentamiento = 1400L)
  f  <- dl_ajustar(b, opciones = o, semilla = 20260831L)
  f0 <- dl_ajustar_solo_prior(b, opciones = o, semilla = 20260831L, ajuste = f)
  lab <- dl_etiquetas(f, f0, b, grilla_rho = c(0.5), semilla = 20260831L)
  expect_true(all(lab$etiqueta == "prior-driven"))
  expect_true(all(abs(lab$contraccion) < 1e-12))   # sin datos locales, los dos ajustes son el mismo
  expect_setequal(names(lab), c("location_id", "year", "age_group_id", "sex_id", "cause_id",
                                "etiqueta", "contraccion", "lambda_usado", "rho_usado", "kappa_usado"))
})

test_that("con un dato fuerte y un ancla débil la celda deja de ser prior-driven", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  b$cfg$anchor$lambda <- 0.05
  b$datos <- fila_datos(age_start = 60, age_end = 65, sex_id = 1L, val = 0.15, se = 0.002)
  o <- dl_opciones_mcmc(simulaciones = 80L, cadenas = 2L, iteraciones = 6000L, calentamiento = 3000L)
  f  <- dl_ajustar(b, opciones = o, semilla = 9L)
  f0 <- dl_ajustar_solo_prior(b, opciones = o, semilla = 9L)   # con un dato local: se vuelve a muestrear sin él
  lab <- dl_etiquetas(f, f0, b, grilla_rho = c(0.5), semilla = 9L)
  celda <- lab[sex_id == 1L & age_group_id == 17L]   # banda 60-64
  expect_false(celda$etiqueta == "prior-driven")
})

test_that("rejilla de rho: se publica la etiqueta más conservadora con su rho_usado", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  o <- dl_opciones_mcmc(simulaciones = 40L, cadenas = 2L, iteraciones = 2000L, calentamiento = 1000L)
  f  <- dl_ajustar(b, opciones = o, semilla = 3L)
  f0 <- dl_ajustar_solo_prior(b, opciones = o, semilla = 3L, ajuste = f)
  lab <- dl_etiquetas(f, f0, b, grilla_rho = c(0.0, 0.5), semilla = 3L, opciones = o)
  expect_true(all(lab$rho_usado %in% c(0.0, 0.5)))
  expect_true(all(lab$lambda_usado == 1.0) && all(is.na(lab$kappa_usado)))
})

test_that("las etiquetas departamentales heredan la nacional y llevan kappa_usado", {
  b <- dl_insumos(cfg9100_datos(), rutas_completas())
  o <- dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L, calentamiento = 400L)
  f <- dl_ajustar(b, opciones = o, semilla = 8L); f0 <- dl_ajustar_solo_prior(b, opciones = o, semilla = 8L)
  casc <- suppressWarnings(dl_cascada(f, b, kappa = 0.5, semilla = 8L))
  lab <- dl_etiquetas(f, f0, b, grilla_rho = c(0.5), semilla = 8L, opciones = o, cascada = casc)
  nac <- lab[location_id == "123"]; d1 <- lab[location_id == "01"]
  expect_equal(nrow(lab), (1L + 25L) * nrow(nac))            # nacional y 25 departamentos
  m <- merge(nac, d1, by = c("sex_id", "age_group_id"))
  expect_identical(m$etiqueta.x, m$etiqueta.y); expect_equal(m$contraccion.x, m$contraccion.y)
  expect_true(all(is.na(nac$kappa_usado)) && all(d1$kappa_usado == 0.5))
  # con datos nacionales de csmr en la verosimilitud, alguna celda deja de ser prior-driven
  expect_true(any(nac$etiqueta != "prior-driven"))
})

test_that("argumentos inválidos: mensajes en español antes de reajustar", {
  x <- corrida_mini(); b <- x$insumos
  for (g in list(1, c(0.5, -0.1), numeric(), "0.5", NA_real_))
    expect_error(dl_etiquetas(x$ajuste, x$ajuste_prior, b, grilla_rho = g, semilla = 7L),
                 "^dl_etiquetas\\(\\): `grilla_rho` debe tener uno o m\u00e1s n\u00fameros en \\[0, 1\\)",
                 info = deparse(g))
  expect_error(dl_etiquetas(x$ajuste, x$ajuste_prior, b, grilla_rho = 0.5, semilla = 7L, opciones = list()),
               "^dl_etiquetas\\(\\): `opciones` debe venir de dl_opciones_mcmc\\(\\)")
  expect_error(dl_etiquetas(x$ajuste, x$ajuste_prior, b, grilla_rho = 0.5, semilla = 7L, cascada = x$ajuste),
               "^dl_etiquetas\\(\\): `cascada` debe venir de dl_cascada\\(\\)")
  # la rejilla puede venir como lista, como en el YAML
  expect_identical(dl_etiquetas(x$ajuste, x$ajuste_prior, b, grilla_rho = list(0.5), semilla = 7L), x$etiquetas)
})

test_that("los reajustes por rho usan adelgazamiento 10, sea cual sea el del ajuste", {
  # lo que .dl_opciones_reajuste() mira de un ajuste: sus simulaciones, su adelgazamiento y su motor
  ajuste_con <- function(adelgazamiento, simulaciones = 1000L, motor = "mh")
    list(params = list(thin = adelgazamiento, engine = motor), draws_par = list(matrix(0, simulaciones, 2L)))
  con_10 <- .dl_opciones_reajuste(ajuste_con(10L))
  con_30 <- .dl_opciones_reajuste(ajuste_con(30L))
  # 2 cadenas x (6000 - 3000) / 10 = 600 simulaciones, también con un ajuste de adelgazamiento 30 (antes, 200)
  expect_identical(con_30, con_10)
  expect_identical(con_30, dl_opciones_mcmc(simulaciones = 600L, cadenas = 2L, iteraciones = 6000L,
                                            calentamiento = 3000L, adelgazamiento = 10L))
  expect_identical(.DL_REAJUSTE_ADELGAZAMIENTO, formals(dl_opciones_mcmc)$adelgazamiento)
  # con un ajuste de adelgazamiento 10 nada cambia respecto de la versión 2.2.0, que heredaba el del ajuste
  expect_identical(con_10, dl_opciones_mcmc(simulaciones = min(1000L, 2L * ((6000L - 3000L) %/% 10L)), cadenas = 2L,
                                            iteraciones = 6000L, calentamiento = 3000L, adelgazamiento = 10L,
                                            nucleos = 1L, motor = "mh"))
  # un ajuste con menos simulaciones que las que guardan las cadenas cortas pide las suyas; el motor es el del ajuste
  corto <- .dl_opciones_reajuste(ajuste_con(30L, simulaciones = 40L, motor = "rcpp"))
  expect_identical(corto$draws, 40L)
  expect_identical(corto$engine, "rcpp")
  expect_identical(corto$thin, 10L)
})
