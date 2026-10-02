# Ajuste corto: suficiente para diagnósticos gruesos y rápido para las pruebas automáticas (~1 min).
fit_corto <- function(b, seed = 20260830L)
  dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 200L, cadenas = 2L, iteraciones = 3000L,
                                            calentamiento = 1000L, nucleos = 1L, adelgazamiento = 10L),
             semilla = seed)

test_that("dl_ajustar devuelve un dl_fit completo con los insumos de la causa 9100", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- fit_corto(b)
  expect_s3_class(f, "dl_fit")
  expect_setequal(names(f$draws_par), c("1", "2"))
  expect_identical(colnames(f$draws_par[["1"]]),
                   c(paste0("logi_", f$grid$nudos), paste0("logf_", f$grid$nudos)))
  expect_lte(nrow(f$draws_par[["1"]]), 200L)
  expect_setequal(unique(f$draws_q$sex_id), c(1L, 2L))
  expect_identical(sort(unique(f$draws_q$edad)), f$grid$edades)
  expect_true(all(f$draws_q$p >= 0 & f$draws_q$p <= 1))
  expect_true(all(is.finite(f$mcmc$rhat) & f$mcmc$rhat > 0.9))
  expect_identical(f$bundle_hash, b$hash)
  expect_false(f$prior_only)
  expect_output(print(f), "dl_fit")
})

test_that("semilla obligatoria y motor fuera de {mh, rcpp} rechazado", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  expect_error(dl_ajustar(b, opciones = dl_opciones_mcmc(cadenas = 1L, iteraciones = 400L, calentamiento = 200L)),
               "semilla")
  expect_error(dl_opciones_mcmc(motor = "otro"), "`motor` debe ser")   # motor fuera de {mh, rcpp}
})

test_that("solo con el ancla: sin datos locales y con la misma semilla reproduce dl_ajustar exactamente", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f  <- fit_corto(b)
  f0 <- dl_ajustar_solo_prior(b, opciones = dl_opciones_mcmc(simulaciones = 200L, cadenas = 2L, iteraciones = 3000L,
                                                             calentamiento = 1000L, adelgazamiento = 10L),
                              semilla = 20260830L)
  expect_true(f0$prior_only)
  expect_identical(f0$bundle_hash, b$hash)
  expect_identical(f0$draws_par, f$draws_par)   # los insumos de 9100 no tienen datos locales
})

test_that("dl_ajustar_solo_prior con `ajuste` lo reutiliza si no hay datos locales (sin volver a muestrear)", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L,
                                                 calentamiento = 400L), semilla = 4L)
  f0 <- dl_ajustar_solo_prior(b, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L,
                                                             calentamiento = 400L), semilla = 4L,
                              ajuste = f)
  expect_true(f0$prior_only)
  expect_identical(f0$draws_par, f$draws_par)
  expect_error(dl_ajustar_solo_prior(b, semilla = 5L, ajuste = f), "semilla 4")   # otra semilla: se rechaza
  # con datos locales no hay atajo: se vuelve a muestrear sin ellos y el resultado difiere del ajuste
  b2 <- b; b2$datos <- fila_datos(age_start = 60, age_end = 65, sex_id = 1L, val = 0.15, se = 0.002)
  f2 <- dl_ajustar(b2, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L,
                                                   calentamiento = 400L), semilla = 4L)
  f02 <- dl_ajustar_solo_prior(b2, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1000L,
                                                               calentamiento = 400L), semilla = 4L,
                               ajuste = f2)
  expect_false(identical(f02$draws_par, f2$draws_par))
})

test_that("semilla inválida y objetos equivocados: mensajes en español con los nombres de las funciones", {
  x <- corrida_mini(); b <- x$insumos; o <- x$opciones
  expect_error(dl_ajustar(b, o), "falta `semilla`.*por ejemplo semilla = 1")
  for (s in list(NA, NULL, 1.5, c(1, 2), "1"))
    expect_error(dl_ajustar(b, o, semilla = s), "^dl_ajustar\\(\\): `semilla` debe ser un número entero",
                 info = deparse(s))
  expect_error(dl_avd(x$ajuste, b, semilla = 1.5), "^dl_avd\\(\\): `semilla` debe ser")
  expect_error(dl_ajustar(dl_configuracion_ejemplo(9100L, formato = "completo"), o, semilla = 1),
               paste0("`insumos` debe venir de dl_insumos\\(\\), pero es una configuraci\u00f3n.*",
                      "dl_insumos\\(configuracion, rutas\\)"))
  expect_error(dl_cascada(b, x$ajuste, semilla = 1), "^dl_cascada\\(\\): `ajuste` debe venir de dl_ajustar.*orden")
  expect_error(dl_avd(b, x$ajuste, semilla = 1), "^dl_avd\\(\\): `ajuste` debe venir de")
  expect_error(dl_etiquetas(x$ajuste, x$ajuste, b, semilla = 1), "`ajuste_prior` debe venir de dl_ajustar_solo_prior")
  expect_output(print(x$ajuste), "<dl_fit> insumos .* semilla 7 \\| motor mh")
  expect_output(print(x$ajuste), "simulaciones por sexo: 10")
  expect_output(print(x$ajuste_prior), "<dl_fit> \\(solo con el ancla\\) insumos")
})

test_that("`cache` que no es TRUE ni FALSE: mensaje en español con la función del usuario", {
  x <- corrida_mini(); b <- x$insumos; o <- x$opciones
  expect_error(dl_ajustar(b, o, semilla = 7L, cache = "si"),
               "^dl_ajustar\\(\\): `cache` debe ser TRUE o FALSE; es el texto \"si\"")
  expect_error(dl_ajustar_solo_prior(b, o, semilla = 7L, cache = NA),
               "^dl_ajustar_solo_prior\\(\\): `cache` debe ser TRUE o FALSE")
  expect_identical(dl_ajustar(b, o, semilla = 7L, cache = TRUE)$draws_par, x$ajuste$draws_par)
})
