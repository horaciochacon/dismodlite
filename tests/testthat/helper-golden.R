# Instantáneas de caracterización: lo que devuelve el paquete sobre los datos de ejemplo, fijado para que un cambio
# de salida no intencional falle. Viven en tests/testthat/_referencia/caracterizacion/.
# Regenerar SOLO ante un cambio de salida intencional y declarado:
#   DL_ACTUALIZAR_GOLDEN=1 Rscript -e 'devtools::test(filter = "config")'
# y revisar el diff de la carpeta en git antes del commit.
golden_caract_dir <- function() testthat::test_path("_referencia", "caracterizacion")

# Compara `lineas` con la instantánea `nombre`; con DL_ACTUALIZAR_GOLDEN=1 la (re)escribe.
expect_golden <- function(lineas, nombre) {
  f <- file.path(golden_caract_dir(), nombre)
  if (identical(Sys.getenv("DL_ACTUALIZAR_GOLDEN"), "1")) {
    dir.create(golden_caract_dir(), showWarnings = FALSE, recursive = TRUE)
    writeLines(enc2utf8(lineas), f, useBytes = TRUE)
    return(testthat::succeed(paste("instantánea escrita:", nombre)))
  }
  if (!file.exists(f))
    return(testthat::fail(sprintf(
      "instantánea ausente: %s (generarla con DL_ACTUALIZAR_GOLDEN=1 en el commit de referencia)", nombre)))
  esperado <- readLines(f, encoding = "UTF-8")
  n <- max(length(lineas), length(esperado))
  a <- c(lineas, rep("<fin>", n - length(lineas))); b <- c(esperado, rep("<fin>", n - length(esperado)))
  d <- which(a != b)
  testthat::expect(length(d) == 0L, sprintf(paste0(
    "%s: %d línea(s) distintas de la instantánea; la primera, %d:\n  antes: %s\n  ahora: %s\n",
    "(si el cambio es intencional: DL_ACTUALIZAR_GOLDEN=1 y revisar el diff en git)"),
    nombre, length(d), d[1], b[d[1]], a[d[1]]))
}
