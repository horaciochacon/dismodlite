test_that("acepta una tabla valida", {
  expect_silent(dl_validar_tabla(mini_como(), "como_factor"))
})
test_that("columna faltante", {
  bad <- mini_como(); bad[, fuente := NULL]
  expect_error(dl_validar_tabla(bad, "como_factor"), "como_factor[.]fuente: falta")
})
test_that("columna sobrante", {
  bad <- mini_como(); bad[, extra := 1]
  expect_error(dl_validar_tabla(bad, "como_factor"), "como_factor[.]extra: no est\u00e1 en el esquema")
})
test_that("tipo incorrecto", {
  bad <- mini_como(factor = "alto")
  expect_error(dl_validar_tabla(bad, "como_factor"), "como_factor[.]factor: tipo")
})
test_that("enum invalido", {
  bad <- data.table::data.table(crosswalk_id = "cw_1", cause_id = 502L, definicion_alt = "a",
    definicion_ref = "r", escala = "cubica", direccion = "alt_a_ref", beta = 0.1,
    beta_lower = NA_real_, beta_upper = NA_real_, modo = "fija", var_default = 0.1,
    origen = "gbd", fuente = "t")
  expect_error(dl_validar_tabla(bad, "crosswalks"), "crosswalks[.]escala: valor\\(es\\) no admitido")
})
test_that("el origen de una corrección (crosswalks.origen) es gbd o local", {
  expect_silent(dl_validar_tabla(fila_crosswalk(origen = "gbd"), "crosswalks"))
  expect_silent(dl_validar_tabla(fila_crosswalk(origen = "local"), "crosswalks"))
  expect_error(dl_validar_tabla(fila_crosswalk(origen = "otro"), "crosswalks"),
               "crosswalks[.]origen: valor\\(es\\) no admitido")
})
test_that("clave duplicada", {
  bad <- rbind(mini_como(), mini_como())
  expect_error(dl_validar_tabla(bad, "como_factor"), "como_factor: clave duplicada")
})
test_that("NA en columna obligatoria", {
  bad <- mini_como(fuente = NA_character_)
  expect_error(dl_validar_tabla(bad, "como_factor"), "como_factor[.]fuente: NA en columna obligatoria")
})
test_that("tabla desconocida falla fuerte", {
  expect_error(dl_validar_tabla(mini_como(), "no_existe"), "^dl_validar_tabla\\(\\): tabla desconocida «no_existe»")
})
test_that("los problemas van en un solo error que nombra la tabla, y `datos` debe ser una tabla", {
  bad <- mini_como(factor = "alto"); bad[, extra := 1]
  expect_error(dl_validar_tabla(bad, "como_factor"),
               "^dl_validar_tabla\\(\\): la tabla como_factor tiene 2 problema\\(s\\):\n  - como_factor[.]extra")
  expect_error(dl_validar_tabla(mini_como(factor = "alto"), "como_factor"), "tipo esperado num \\(número\\)")
  expect_error(dl_validar_tabla(list(a = 1), "como_factor"), "`datos` debe ser una tabla")
})
test_that("`causa` se valida igual en todas las funciones: un cause_id entero o su texto", {
  expect_identical(.dl_exigir_causa("9100"), 9100L)
  expect_identical(.dl_exigir_causa(9100), 9100L)
  for (mala in list(9100.7, "abc", NA, c(9100, 9101), 0))
    expect_error(.dl_exigir_causa(mala), "`causa` debe ser un cause_id entero", info = format(mala))
  expect_error(dl_severidad_desde_particion(withr::local_tempdir(), 9100.7),
               "^dl_severidad_desde_particion\\(\\): `causa` debe ser un cause_id entero")
  expect_error(dl_sumar_hijas(list(), causa = "x", nombre = "prueba", carpeta = withr::local_tempdir()),
               "^dl_sumar_hijas\\(\\): `causa` debe ser un cause_id entero")
  expect_error(dl_configuracion_ejemplo(9999L), "`causa` debe ser 9100, 9101, 9102 o 9103")
})
