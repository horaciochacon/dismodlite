# dl_resumir(): las piezas de una corrida (nombres, sinónimos y piezas que no casan) y las rutas por defecto, que son
# las de los insumos.

test_that("los insumos guardan sus rutas fuera del hash y las etapas siguientes las usan por defecto", {
  x <- corrida_mini(); b <- x$insumos
  expect_identical(b$rutas, dl_rutas_ejemplo(9100L, datos = FALSE))
  sin_rutas <- b; sin_rutas$rutas <- NULL
  expect_identical(dismodlite:::.dl_hash_bundle(sin_rutas, dismodlite:::.DL_TABLAS_BUNDLE)$hash, b$hash)
  r <- dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = b))
  expect_identical(r, dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = b), rutas = b$rutas))
  expect_identical(dl_factor_comorbilidad(b), dl_factor_comorbilidad(b, b$rutas))
  v <- suppressMessages(dl_validar_ancla(x$ajuste, b))
  expect_true("implied_incidence" %in% v$check)   # el ancla de incidencia sale de las rutas de los insumos
})

test_that("piezas acepta los nombres en español e ignora el resumen", {
  x <- corrida_mini()
  r <- dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = x$insumos))
  expect_identical(dl_resumir(list(ajuste = x$ajuste, avd = x$avd, insumos = x$insumos)), r)
  expect_identical(dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = x$insumos, resumen = r)), r)
  expect_output(print(r), "<dl_resumen>.*AVD.*prevalencia.*simulaciones por medida: 10")
  expect_output(print(r), "intervalo del 95 %")
})

test_that("`nivel` fuera de (0, 1) se rechaza en español, nombrando `nivel`", {
  x <- corrida_mini(); piezas <- list(fit = x$ajuste, yld = x$avd, bundle = x$insumos)
  for (nv in list(95, 0, 1, "0.95", NA_real_, c(0.9, 0.95)))
    expect_error(dl_resumir(piezas, nivel = nv), "^dl_resumir\\(\\): `nivel` debe ser un número entre 0 y 1",
                 info = paste(format(nv), collapse = " "))
  expect_identical(unique(dl_resumir(piezas, nivel = 0.9)$celdas$ui_level), 0.9)   # válido, aunque no exportable
})

test_that("piezas mal armadas: errores en español que dicen qué va en cada una", {
  x <- corrida_mini(); f <- x$ajuste; y <- x$avd; b <- x$insumos
  expect_error(dl_resumir(f), "^dl_resumir\\(\\): `piezas` es una lista.*es un ajuste")
  expect_error(dl_resumir(list(fit = f, bundle = b)), "falta la pieza `avd` \\(o `yld`\\).*dl_avd\\(ajuste, insumos")
  expect_error(dl_resumir(list(ajuste = f, avd = y)), "falta la pieza `insumos` \\(o `bundle`\\).*dl_insumos")
  expect_error(dl_resumir(list()), "^dl_resumir\\(\\): `piezas` est\u00e1 vac\u00eda")
  expect_error(dl_resumir(list(fit = f, cascade = x$cascada, yld = y, bundle = b)),
               "`cascade` no es una pieza.*la cascada va en `ajuste`")
  expect_error(dl_resumir(list(fit = f, ajuste = f, yld = y, bundle = b)), "repetida")
  expect_error(dl_resumir(list(fit = b, yld = y, bundle = f)), "`piezas\\$fit` debe venir de dl_ajustar")
  # AVD de la cascada con el ajuste nacional (y al revés): se rechaza, porque mezclaría prevalencia departamental
  # con incidencia nacional en el mismo resumen
  expect_error(dl_resumir(list(fit = f, yld = x$avd_cascada, bundle = b)), "no salen del ajuste de `fit`")
  expect_error(dl_resumir(list(fit = x$cascada, yld = y, bundle = b)), "no salen del ajuste de `fit`.*cascada")
  rc <- dl_resumir(list(fit = x$cascada, yld = x$avd_cascada, bundle = b))
  expect_identical(sort(unique(rc$celdas[measure_id == 6L]$location_id)),
                   sort(unique(rc$celdas[measure_id == 5L]$location_id)))
})
