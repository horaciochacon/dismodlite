# Las guías y las novedades, desde el árbol fuente (se omite en el paquete instalado, que no trae vignettes/articles
# ni _pkgdown.yml): cada tabla del contrato tiene su lugar en la guía de los datos, y el reparto por razón, en las
# guías, las novedades y el índice de la referencia.

test_that("cada tabla del contrato tiene su fila y su sección en la guía «Preparar tus datos»", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  guia <- readLines(file.path(r, "vignettes", "preparar-datos.qmd"), encoding = "UTF-8")
  for (t in .DL_TABLAS) {
    expect_true(any(startsWith(guia, sprintf("| `%s` |", t))), info = t)
    expect_true(any(startsWith(guia, sprintf("### `%s`", t))), info = t)
  }
  expect_true(any(grepl("las **once tablas**", guia, fixed = TRUE)))
})

test_that("el reparto por razón está en las guías, las novedades y el índice de la referencia", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  leer <- function(...) paste(readLines(file.path(r, ...), encoding = "UTF-8"), collapse = "\n")
  subnacional <- leer("vignettes", "articles", "desagregacion-subnacional.qmd")
  for (texto in c("## Los cuatro modos", "| `razon` |", "## Modo razón {#modo-razon}", "dl_repartir_razon(",
                  "razon_semilla"))
    expect_match(subnacional, texto, fixed = TRUE, info = texto)
  expect_match(leer("vignettes", "el-modelo.qmd"), "subnacional.modo: razon", fixed = TRUE)
  expect_match(leer("vignettes", "articles", "corridas-y-consolidado.qmd"), "`razon.csv`", fixed = TRUE)
  expect_match(leer("NEWS.md"), "## Reparto subnacional por razón", fixed = TRUE)
  expect_match(leer("_pkgdown.yml"), "  - dl_repartir_razon", fixed = TRUE)
})
