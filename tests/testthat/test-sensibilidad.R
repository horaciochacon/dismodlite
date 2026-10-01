# dl_sensibilidad(): rejilla lambda x rho x kappa con las etiquetas recalculadas; un ajuste por (lambda, rho) desde
# la cache de la sesión, una cascada por kappa y el abanico departamental.

test_that("dl_sensibilidad cubre lambda x rho x kappa, kappa sin volver a muestrear, y responde a lambda", {
  dl_limpiar_cache()
  b <- dl_insumos(cfg9100_datos(), rutas_completas())
  g <- list(lambda = c(0.1, 1.0), rho = c(0.5), kappa = c(0.5, 1.0))
  o <- dl_opciones_mcmc(simulaciones = 40L, cadenas = 2L, iteraciones = 2500L, calentamiento = 1200L)
  t1 <- system.time(s <- dl_sensibilidad(b, grilla = g, semilla = 13L, opciones = o))[["elapsed"]]
  bandas <- nrow(unique(b$prior_gbd[measure_id == 5L, list(sex_id, age_group_id)]))
  expect_equal(nrow(s), 2L * 1L * 2L * bandas)
  expect_setequal(unique(s$kappa), c(0.5, 1.0))
  expect_true(all(c("dpto_min", "dpto_med", "dpto_max", "nota") %in% names(s)))
  ok <- s[!is.na(dpto_med)]
  expect_true(all(ok$dpto_min <= ok$dpto_med & ok$dpto_med <= ok$dpto_max))
  # lambda = 1 (ancla fuerte) siempre produce cascada; con lambda = 0.1 alguna simulación puede salirse de la cota
  # (p nacional cercana a 1): la combinación se reporta con una nota en vez de detener la rejilla
  expect_true(all(!is.na(s[lambda == 1.0]$dpto_med)))
  expect_true(all(is.na(s$nota) == !is.na(s$dpto_med)))
  dep <- attr(s, "departamental"); expect_setequal(unique(dep$location_id), sprintf("%02d", 1:25))
  expect_equal(nrow(dep[lambda == 1.0]), 1L * 2L * 25L * bandas)   # 25 departamentos
  # kappa mayor: abanico departamental más ancho
  ab <- s[lambda == 1.0, list(w = mean(dpto_max - dpto_min)), by = kappa]
  expect_gt(ab[kappa == 1.0]$w, ab[kappa == 0.5]$w)
  ancho <- s[kappa == 1.0, list(w = mean(upper - lower)), by = lambda]   # lambda pequeño: intervalo más ancho
  expect_gt(ancho[lambda == 0.1]$w, ancho[lambda == 1.0]$w)
  # segunda llamada: los ajustes están en la cache, así que es mucho más rápida e idéntica
  t2 <- system.time(s2 <- dl_sensibilidad(b, grilla = g, semilla = 13L, opciones = o))[["elapsed"]]
  expect_lt(t2, t1 / 2)
  sin_attr <- function(x) { x <- as.data.frame(x); attr(x, "departamental") <- NULL; x }
  expect_equal(sin_attr(s), sin_attr(s2))
  expect_equal(as.data.frame(attr(s, "departamental")), as.data.frame(attr(s2, "departamental")))
})

test_that("sin proxies departamentales la sensibilidad sale con kappa NA y toda celda prior-driven", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  s <- dl_sensibilidad(b, grilla = list(lambda = 1.0, rho = 0.5, kappa = c(0.5, 1.0)), semilla = 2L,
                       opciones = dl_opciones_mcmc(simulaciones = 10L, cadenas = 1L, iteraciones = 600L,
                                                   calentamiento = 200L))
  expect_true(all(is.na(s$kappa))); expect_true(all(is.na(s$dpto_med)))
  expect_true(all(s$etiqueta == "prior-driven"))              # sin datos locales ninguna celda depende de ellos
  expect_identical(nrow(attr(s, "departamental")), 0L)
})

test_that("dl_sensibilidad exige semilla", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  expect_error(dl_sensibilidad(b), "semilla")
})

test_that("dl_sensibilidad es determinista con procesos y núcleos en paralelo", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  g <- list(lambda = c(0.5, 1.0), rho = c(0.5), kappa = c(1.0))
  s1 <- dl_sensibilidad(b, grilla = g, semilla = 21L,
                        opciones = dl_opciones_mcmc(simulaciones = 30L, cadenas = 2L, iteraciones = 1500L,
                                                    calentamiento = 600L))
  dl_limpiar_cache()
  s2 <- dl_sensibilidad(b, grilla = g, semilla = 21L,
                        opciones = dl_opciones_mcmc(simulaciones = 30L, cadenas = 2L, iteraciones = 1500L,
                                                    calentamiento = 600L, nucleos = 2L),
                        procesos = 2L)
  expect_equal(as.data.frame(s1), as.data.frame(s2))
})

test_that("rejilla y procesos inválidos: mensajes en español antes de ajustar", {
  x <- corrida_mini(); b <- x$insumos; o <- x$opciones           # insumos con proxies: la cascada aplica
  expect_error(dl_sensibilidad(b, grilla = c(lambda = 1), semilla = 7L, opciones = o),
               "^dl_sensibilidad\\(\\): `grilla` debe ser una lista con los ejes lambda, rho y kappa")
  expect_error(dl_sensibilidad(b, grilla = list(lambda = 1, rho = 0.5, kapa = 1), semilla = 7L, opciones = o),
               "`grilla` solo admite los ejes lambda, rho, kappa, fraccion_aguda; tiene kapa")
  expect_error(dl_sensibilidad(b, grilla = list(lambda = 1, rho = 0.5), semilla = 7L, opciones = o),
               "`grilla\\$kappa` debe tener uno o m\u00e1s n\u00fameros en \\(0, 1\\]; es NULL")
  expect_error(dl_sensibilidad(b, grilla = list(lambda = c(0, 1), rho = 0.5, kappa = 1), semilla = 7L, opciones = o),
               "`grilla\\$lambda` debe tener uno o m\u00e1s n\u00fameros en \\(0, 1\\]; es 0, 1")
  expect_error(dl_sensibilidad(b, grilla = list(lambda = 1, rho = 1, kappa = 1), semilla = 7L, opciones = o),
               "`grilla\\$rho` debe tener uno o m\u00e1s n\u00fameros en \\[0, 1\\)")
  expect_error(dl_sensibilidad(b, grilla = list(lambda = 1, rho = 0.5, kappa = 1, fraccion_aguda = 1), semilla = 7L,
                               opciones = o),
               "`grilla\\$fraccion_aguda` debe tener uno o m\u00e1s n\u00fameros en \\[0, 1\\)")
  for (p in list(0L, 1.5, NA, Inf, "2"))
    expect_error(dl_sensibilidad(b, grilla = list(lambda = 1, rho = 0.5, kappa = 1), semilla = 7L, opciones = o,
                                 procesos = p),
                 "^dl_sensibilidad\\(\\): `procesos` debe ser un entero >= 1", info = deparse(p))
})
