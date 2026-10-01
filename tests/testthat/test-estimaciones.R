# dl_estimaciones(): media e intervalo por celda desde las simulaciones de un ajuste, una cascada o dl_avd().

.estimaciones_mini <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, datos = FALSE)))
      f <- dl_ajustar(b, dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L),
                      semilla = 7L)
      x <<- list(insumos = b, ajuste = f)
    }
    x
  }
})

test_that("de un ajuste: columnas, media de las simulaciones por celda y cuantiles del nivel", {
  x <- .estimaciones_mini()
  e <- dl_estimaciones(x$ajuste)
  expect_identical(names(e), c("location_id", "sex_id", "edad", "medida", "media", "inferior", "superior"))
  expect_true(all(e$medida == "prevalencia"))
  dq <- x$ajuste$draws_q
  esperado <- dq[, list(media = mean(p), inferior = stats::quantile(p, 0.025, names = FALSE),
                        superior = stats::quantile(p, 0.975, names = FALSE)), by = list(location_id, sex_id, edad)]
  expect_identical(nrow(e), nrow(esperado))
  expect_equal(e$media, esperado$media, tolerance = 1e-12)
  expect_equal(e$inferior, esperado$inferior, tolerance = 1e-12)
  expect_equal(e$superior, esperado$superior, tolerance = 1e-12)
  e90 <- dl_estimaciones(x$ajuste, nivel = 0.9)
  q90 <- dq[, list(i = stats::quantile(p, 0.05, names = FALSE), s = stats::quantile(p, 0.95, names = FALSE)),
            by = list(location_id, sex_id, edad)]
  expect_equal(e90$inferior, q90$i, tolerance = 1e-12)
  expect_equal(e90$superior, q90$s, tolerance = 1e-12)
  expect_true(all(e90$inferior >= e$inferior & e90$superior <= e$superior))
})

test_that("de un ajuste: incidencia poblacional y mortalidad en exceso; los AVD piden dl_avd()", {
  x <- .estimaciones_mini()
  dq <- x$ajuste$draws_q
  inc <- dl_estimaciones(x$ajuste, "incidencia")
  expect_true(all(inc$medida == "incidencia"))
  expect_equal(inc$media, dq[, mean(ipop), by = list(location_id, sex_id, edad)]$V1, tolerance = 1e-12)
  emr <- dl_estimaciones(x$ajuste, medida = "mortalidad_exceso")
  expect_equal(emr$media, dq[, mean(f), by = list(location_id, sex_id, edad)]$V1, tolerance = 1e-12)
  expect_error(dl_estimaciones(x$ajuste, "avd"), "dl_avd")
  expect_error(dl_estimaciones(x$ajuste, nivel = 1.2), "nivel")
  expect_error(dl_estimaciones(x$ajuste, "otra"), "`medida` debe ser")
  expect_identical(dl_estimaciones(x$ajuste, "prev"), dl_estimaciones(x$ajuste))   # abreviatura, como match.arg()
  # «mortalidad» sola no se completa a la mortalidad en exceso: podría ser la específica por causa
  expect_error(dl_estimaciones(x$ajuste, "mortalidad"), "^dl_estimaciones\\(\\): `medida` = \"mortalidad\" es ambigua")
  expect_identical(dl_estimaciones(x$ajuste, "mortalidad_e"), emr)
  expect_error(dl_estimaciones(), "^dl_estimaciones\\(\\): falta `x`")
  expect_error(dl_estimaciones(list(a = 1)), "ajuste")
  expect_error(dl_estimaciones(x$insumos), "^dl_estimaciones\\(\\): `x` debe ser un ajuste.*; es un objeto de insumos")
})

test_that("de un resultado de dl_avd(): filas por banda con age_group_id", {
  x <- .estimaciones_mini()
  y <- dl_avd(x$ajuste, x$insumos, semilla = 7L)
  e <- dl_estimaciones(y)
  expect_identical(names(e), c("location_id", "sex_id", "age_group_id", "medida", "media", "inferior", "superior"))
  expect_true(all(e$medida == "avd"))
  expect_setequal(unique(e$age_group_id), unique(x$insumos$prior_gbd$age_group_id))
  expect_equal(e$media, y$draws_yld[, mean(val), by = list(location_id, sex_id, age_group_id)]$V1, tolerance = 1e-12)
  ep <- dl_estimaciones(y, "prevalencia")
  expect_true(all(ep$medida == "prevalencia"))
  expect_equal(ep$media, y$draws_prev_banda[, mean(val), by = list(location_id, sex_id, age_group_id)]$V1,
               tolerance = 1e-12)
  expect_error(dl_estimaciones(y, "incidencia"), "avd")
})

test_that("de una cascada: la naci\u00f3n y los 25 departamentos", {
  x <- .estimaciones_mini()
  casc <- suppressWarnings(dl_cascada(x$ajuste, x$insumos, semilla = 7L))
  e <- dl_estimaciones(casc)
  expect_setequal(unique(e$location_id), c(x$insumos$loc_ancla, casc$departamentos))
  expect_length(casc$departamentos, 25L)
})
