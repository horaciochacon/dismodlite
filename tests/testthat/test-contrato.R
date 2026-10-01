test_that("la definición del contrato tiene las nueve tablas y el eje en cada una", {
  ref <- dismodlite:::.dl_tablas_ref()
  expect_setequal(unique(ref$tabla), dismodlite:::.DL_TABLAS)
  expect_true(all(ref$columna[ref$rol == "eje"] %in% dismodlite:::.DL_EJE))
  expect_identical(dismodlite:::.dl_columnas_contrato("poblacion", "exigidas"),
                   c("ubicacion", "anio", "sexo", "edad_inicio", "edad_fin", "poblacion"))
})

test_that("los nombres de columna se normalizan: minúsculas, espacios y alias", {
  d <- data.table::data.table(" Year " = 2023, SEX = "hombres", val = 1)
  n <- dismodlite:::.dl_normalizar_nombres(d, "poblacion")
  expect_identical(names(n), c("anio", "sexo", "poblacion"))
  # un alias no pisa una columna que ya trae el nombre canónico
  d2 <- data.table::data.table(anio = 2023, year = 2020)
  expect_error(dismodlite:::.dl_normalizar_nombres(d2, "poblacion"), "anio.*year")
})

test_that("el sexo se escribe en palabras o con su código", {
  expect_identical(dismodlite:::.dl_sexo_contrato(c("Hombres", "2", "ambos", "Male", " female ", "x")),
                   c("hombres", "mujeres", "ambos", "hombres", "mujeres", NA))
})
