test_that("dl_esquema carga y autovalida el contrato", {
  sch <- dl_esquema()
  expect_s3_class(sch, "dl_schema")
  expect_setequal(names(sch$tablas),
    c("datos","prior_gbd","betas","crosswalks","cov_valores","cov_proxy","severidad","poblacion",
      "impairment_envelope","impairment_attribution","impairment_severity_split","sequela_map","como_factor"))
  expect_true(all(vapply(sch$tablas, function(t) length(t$clave) >= 1, TRUE)))
})

test_that("la copia de inst/schema es idéntica byte a byte a la de referencia", {
  # copia del contrato dismod_lite/v1 de la versión 1.0.0, guardada con las referencias de las pruebas: fija el
  # contrato instalado (un cambio debe ser deliberado). Difiere del de la versión 0.2.2 solo en el valor `local`
  # de crosswalks.origen, que reemplaza al que nombraba a un equipo externo.
  a <- readBin(testthat::test_path("_referencia", "esquemas", "dismod_lite.v1.yaml"), "raw", 1e6)
  b <- readBin(system.file("schema", "dismod_lite.v1.yaml", package = "dismodlite"), "raw", 1e6)
  expect_identical(a, b)
})

test_that("un esquema mal formado se rechaza con un mensaje que nombra el problema", {
  tmp <- tempfile(fileext = ".yaml")
  writeLines(c("schema: dismod_lite/v1", "tablas:", "  datos: {columnas: {}, reglas: []}"), tmp)
  expect_error(dl_esquema(tmp), "^dl_esquema\\(\\): el esquema .* no es válido: la tabla datos no tiene columnas")
  expect_error(dl_esquema(file.path(tempdir(), "no_existe.yaml")), "`archivo` debe ser la ruta de un archivo YAML")
  expect_error(dl_esquema_tabla(dl_esquema(), "dato"), "tabla desconocida «dato»; las tablas del esquema son: datos")
})
