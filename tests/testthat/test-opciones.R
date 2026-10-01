# dl_opciones_mcmc() y la cache de ajustes de la sesión.

test_that("dl_opciones_mcmc valida y dl_ajustar acepta las opciones", {
  o <- dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L, calentamiento = 400L)
  expect_s3_class(o, "dl_mcmc_opts")
  expect_identical(o$engine, "mh")
  expect_error(dl_opciones_mcmc(iteraciones = 100L, calentamiento = 200L), "calentamiento")
  expect_error(dl_opciones_mcmc(calentamiento = 150L), "200")
  expect_error(dl_opciones_mcmc(motor = "otro"), "`motor` debe ser \"mh\" o \"rcpp\"")
  expect_output(print(o), "dl_mcmc_opts")
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- dl_ajustar(b, opciones = o, semilla = 4L)
  expect_identical(f$params$draws, 20L)
  expect_true("location_id" %in% names(f$draws_q))
  expect_true(all(f$draws_q$location_id == b$loc_ancla))
  expect_equal(f$draws_q$ipop, f$draws_q$i * (1 - f$draws_q$p))
})

test_that("cache: los mismos insumos, opciones y semilla devuelven el ajuste sin volver a muestrear", {
  dl_limpiar_cache()
  b <- dl_insumos(cfg9100(), rutas_nacional())
  o <- dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L, calentamiento = 400L)
  t1 <- system.time(f1 <- dl_ajustar(b, opciones = o, semilla = 4L))[["elapsed"]]
  t2 <- system.time(f2 <- dl_ajustar(b, opciones = o, semilla = 4L))[["elapsed"]]
  expect_identical(f1$draws_par, f2$draws_par)
  expect_lt(t2, t1 / 5)
  f3 <- dl_ajustar(b, opciones = o, semilla = 4L, cache = FALSE)
  expect_identical(f1$draws_par, f3$draws_par)        # idéntico bit a bit al muestreo directo
  # otra lambda (.dl_bundle_con no cambia el hash): otra clave
  b2 <- .dl_bundle_con(b, lambda = 0.5)
  f4 <- dl_ajustar(b2, opciones = o, semilla = 4L)
  expect_false(identical(f1$draws_par, f4$draws_par))
  dl_limpiar_cache()
  expect_length(ls(.dl_cache_env), 0L)
})

test_that("motor admite abreviaturas; más simulaciones que las guardadas avisa en español; print en español", {
  expect_identical(dl_opciones_mcmc(motor = "r")$engine, "rcpp")
  expect_identical(dl_mcmc_opts()$engine, "mh")
  expect_error(dl_opciones_mcmc(motor = "Rcpp"), "`motor` debe ser \"mh\" o \"rcpp\", pero es el texto \"Rcpp\"")
  expect_warning(o <- dl_opciones_mcmc(simulaciones = 500L, cadenas = 2L, iteraciones = 2000L, calentamiento = 1000L),
                 "se piden 500 simulaciones y las cadenas guardan 200")
  expect_identical(o$draws, 500L)
  expect_error(dl_opciones_mcmc(simulaciones = "muchas"), "`simulaciones` debe ser un entero >= 1")
  expect_output(print(dl_opciones_mcmc()), "simulaciones 1000 \\| cadenas 4 \\| iteraciones 50000 \\| calentamiento")
})

test_that("enteros: un número con decimales se rechaza y el texto de un entero se acepta, como antes", {
  expect_error(dl_opciones_mcmc(simulaciones = 2.5),
               "^dl_opciones_mcmc\\(\\): `simulaciones` debe ser un entero >= 1; es el n\u00famero 2.5")
  expect_error(dl_opciones_mcmc(cadenas = NULL), "`cadenas` debe ser un entero >= 1; es NULL")
  expect_error(dl_opciones_mcmc(nucleos = c(1L, 2L)), "`nucleos` debe ser un entero >= 1")
  expect_error(dl_opciones_mcmc(iteraciones = 1e10), "`iteraciones` debe ser un entero >= 1")
  o <- dl_opciones_mcmc(simulaciones = 40, cadenas = "2", iteraciones = 2000, calentamiento = 1000)
  expect_identical(unclass(o), list(draws = 40L, chains = 2L, iter = 2000L, warmup = 1000L, thin = 10L, cores = 1L,
                                    engine = "mh"))
  expect_error(dl_opciones_mcmc(iteraciones = 1000L, calentamiento = 1000L),
               "`calentamiento` \\(1000\\) debe ser menor que `iteraciones` \\(1000\\)")
  expect_error(dl_opciones_mcmc(calentamiento = 1100L), "m\u00faltiplo de 200 .*es 1100")
})
