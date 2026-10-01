fx <- function(f) testthat::test_path("fixtures", f)

test_that("cada fuente se reconoce por sus columnas", {
  rec <- dismodlite:::.dl_reconocer_lector
  expect_identical(rec(names(data.table::fread(fx("gbd_results_mezcla.csv")))), "gbd_results")
  expect_identical(rec(c("covariate_id", "covariate_name_short", "location_id", "mean_value")), "ghdx_covariables")
  expect_identical(rec(c("nid", "cause_id", "location_id", "component_id")), "ghdx_fuentes")
  expect_identical(rec(c("anio", "sexo", "edad_inicio", "edad_fin", "medida", "valor")), "contrato")
  expect_true(is.na(rec(c("a", "b"))))
})

test_that("GBD Results: Rate de la ubicación del país, por persona, edades en años", {
  a <- dl_tabla("ancla", fx("gbd_results_mezcla.csv"), ubicacion_gbd = 123)
  expect_setequal(a$medida, c("prevalencia", "mortalidad", "avd", "incidencia"))
  p <- a[causa == 9100L & medida == "prevalencia"]
  expect_identical(nrow(p), 1L)                                  # sin Number, Percent, All ages ni Global
  expect_identical(p$valor, 12000 / 1e5)                         # la misma operación que .dl_materializar_medida
  expect_identical(c(p$edad_inicio, p$edad_fin), c(80, 85))
  expect_identical(p$sexo, "hombres")
  expect_identical(p$nombre_causa, "Causa A")
})

test_that("GBD Results con varias ubicaciones y sin ubicacion_gbd pide la clave", {
  expect_error(dl_tabla("ancla", fx("gbd_results_mezcla.csv")), "ubicacion_gbd")
})

test_that("GBD Results en Percent para reproducir corridas anteriores", {
  a <- dl_tabla("ancla", fx("gbd_results_mezcla.csv"), ubicacion_gbd = 123, metrica_prevalencia = "Percent")
  expect_identical(a[causa == 9100L & medida == "prevalencia"]$valor, 0.13)
  expect_identical(a[medida == "mortalidad"]$valor, 300 / 1e5)  # las demás medidas siguen en Rate
})

test_that("GHDx covariables: el valor nacional, la banda estandarizada antes que todas las edades", {
  d <- data.table::data.table(covariate_id = 785L, covariate_name_short = "sev", location_id = 123L,
                              year_id = 2023L, age_group_id = c(22L, 27L), sex_id = 3L,
                              mean_value = c(1.1, 1.2), lower_value = 1, upper_value = 1.4)
  t <- dl_tabla("covariables", d, ubicacion_gbd = 123)
  expect_identical(t$valor, 1.2)
  expect_false(any(c("ubicacion", "edad_inicio") %in% names(t)))
  expect_identical(t$sexo, "ambos")
  expect_identical(t$covariable_id, 785L)
})

test_that("una carpeta mezcla descargas y tablas del contrato", {
  d <- withr::local_tempdir()
  file.copy(fx("gbd_results_mezcla.csv"), d)
  data.table::fwrite(data.table::data.table(causa = 9300L, anio = 2023L, sexo = "mujeres", edad_inicio = 40,
                                            edad_fin = 45, medida = "prevalencia", valor = 0.01, inferior = 0.008,
                                            superior = 0.012), file.path(d, "propia.csv"))
  a <- dl_tabla("ancla", d, ubicacion_gbd = 123)
  expect_setequal(unique(a$causa), c(9100L, 9200L, 9300L))
})

test_that("un archivo que no se reconoce dice qué columnas trae y qué se esperaba", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), f)
  expect_error(dl_tabla("ancla", f), "a, b.*GBD Results")
})

test_that("GHDx fuentes: componente en palabras", {
  t <- dl_tabla("fuentes_gbd", fx("ghdx_fuentes.csv"))
  expect_setequal(t$componente, c("no_fatal", "causa_de_muerte"))
})
