# Nombres anteriores (versión 0.2.2), desde el árbol fuente (se omite en el paquete instalado, que no trae man/):
# la página ?dl_nombres_anteriores documenta exactamente los nombres del mapa del paquete. Las demás pruebas de los
# nombres anteriores están en test-nombres-anteriores.R.

test_that("?dl_nombres_anteriores documenta todos los nombres anteriores, y solo ellos", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  rd <- tools::parse_Rd(file.path(r, "man", "dl_nombres_anteriores.Rd"))
  alias <- unlist(lapply(Filter(function(x) identical(attr(x, "Rd_tag"), "\\alias"), rd), as.character))
  expect_setequal(setdiff(alias, "dl_nombres_anteriores"), names(.DL_NOMBRES_ANTERIORES))
})
