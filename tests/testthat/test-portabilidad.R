test_that(".dl_leer_gz lee gz de R base y conserva location_id como texto (cero a la izquierda)", {
  d <- data.table::data.table(location_id = c("01", "02", "123"), val = c(1.5, 2.5, 3.5))
  p <- withr::local_tempfile(fileext = ".csv.gz")
  con <- gzfile(p, "wt"); utils::write.csv(d, con, row.names = FALSE); close(con)
  r <- .dl_leer_gz(p)
  expect_identical(r$location_id, c("01", "02", "123"))
  expect_equal(r$val, c(1.5, 2.5, 3.5))
})
test_that(".dl_leer_gz sin columna location_id no falla con colClasses = NULL", {
  p <- withr::local_tempfile(fileext = ".csv.gz")
  con <- gzfile(p, "wt"); writeLines(c("a,b", "1,2"), con); close(con)
  expect_identical(names(.dl_leer_gz(p, colClasses = NULL)), c("a", "b"))
})
test_that(".dl_slugify da lo mismo en todos los sistemas para nombres con tilde y eñe", {
  s <- .dl_slugify
  expect_identical(s("Cañete"), "canete")
  expect_identical(s("Huánuco"), "huanuco")
  expect_identical(s("Ancash (Áncash)"), "ancash_ancash")
  expect_identical(s("O’Higgins"), "ohiggins")
})
test_that("dl_configuracion sin carpeta_config pide el argumento y no nombra variables de entorno", {
  msg <- tryCatch(dl_configuracion(9100L), error = conditionMessage)
  expect_match(msg, "falta `carpeta_config`.*dl_configuracion_ejemplo\\(9100\\)")
  expect_false(grepl("[A-Z]{2,}_[A-Z_]+|Sys[.]getenv|variable de entorno", msg))
})
test_that("el paquete carga y expone dl_version()", {
  expect_true(is.function(dl_version))
  expect_identical(dl_version(), "2.2.0")
})
# El paquete instalado en una librería bajo una ruta con espacios y tildes (una carpeta de usuario de Windows como
# «C:/Users/Ana María/...»): en otro proceso de R se carga desde ahí y lee de su carpeta el esquema, la versión y
# los datos de ejemplo. Con R CMD check se copia el paquete instalado; con devtools::test() (árbol fuente) se
# instala con R CMD INSTALL. La ruta llega al otro proceso en un archivo UTF-8, no en la línea de comandos.
# Como ese proceso es nuevo, comprueba también que library(dismodlite) carga data.table antes de cualquier llamada
# a data.table:: (dl_reresumir_corrida() como primera llamada de una sesión).
test_that("el paquete instalado bajo una ruta con espacios y tildes resuelve sus archivos desde ahí", {
  skip_on_cran()
  lib <- file.path(withr::local_tempdir(), "mi carpeta Mar\u00eda", "librer\u00eda de R")
  dir.create(lib, recursive = TRUE)
  origen <- .dl_pkg_dir()
  # el otro proceso busca las dependencias en las librerías de esta sesión (con --vanilla no lee la configuración
  # del usuario) y, bajo R CMD check, no lee el arranque de las pruebas (R_TESTS)
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), R_TESTS = NA)
  if (file.exists(file.path(origen, "Meta", "package.rds"))) {
    expect_true(file.copy(origen, lib, recursive = TRUE))           # paquete instalado: se copia
  } else {
    fuente <- dirname(origen)                                         # load_all(): origen es <fuente>/inst
    skip_if_not(file.exists(file.path(fuente, "DESCRIPTION")), "sin paquete instalado ni árbol fuente")
    out <- suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                    c("CMD", "INSTALL", "--no-docs", "--no-html", "--no-multiarch",
                                      "--no-test-load", "--no-byte-compile", paste0("--library=", shQuote(lib)),
                                      shQuote(fuente)), stdout = TRUE, stderr = TRUE))
    expect_null(attr(out, "status"), label = paste(c("R CMD INSTALL:", utils::tail(out, 5)), collapse = "\n"))
  }
  expect_true(file.exists(file.path(lib, "dismodlite", "DESCRIPTION")))
  aux <- withr::local_tempdir()
  ruta_lib <- file.path(aux, "lib.txt")
  writeLines(enc2utf8(c(lib, .libPaths())), ruta_lib, useBytes = TRUE)
  guion <- file.path(aux, "prueba.R")
  writeLines(c(
    sprintf("libs <- readLines(%s, encoding = 'UTF-8')", deparse(ruta_lib)),
    "lib <- libs[1]",
    ".libPaths(libs)",
    "dt_antes <- 'data.table' %in% loadedNamespaces()",
    "library(dismodlite, lib.loc = lib)",
    # library() carga data.table (NAMESPACE lo importa): `[` sobre una tabla del paquete, antes de cualquier
    # llamada a data.table::, ya filtra filas como data.table (con data.frame elegiría columnas)
    "m <- dismodlite:::.DL_MEDIDAS",
    "cat('data.table', dt_antes, 'data.table' %in% loadedNamespaces(), nrow(m[m$exporta]) == sum(m$exporta), '\\n')",
    # las rutas del paquete van con / también en Windows
    "stopifnot(startsWith(dismodlite:::.dl_pkg_dir(), normalizePath(file.path(lib, 'dismodlite'), winslash = '/')))",
    "stopifnot(startsWith(dismodlite:::.dl_schema_default_path(), normalizePath(lib, winslash = '/')))",
    "stopifnot(startsWith(normalizePath(dl_ejemplo(), winslash = '/'), normalizePath(lib, winslash = '/')))",
    "esq <- dl_esquema()",
    "b <- dl_insumos(dl_configuracion_ejemplo(9100L, formato = 'completo'),",
    "                dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = 'completo'))",
    "cat('version', dl_version(), '\\n')",
    "cat('insumos', class(b)[1], nrow(b$prior_gbd) > 0, '\\n')"), guion)
  res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(guion)),
                                  stdout = TRUE, stderr = TRUE))
  expect_null(attr(res, "status"), label = paste(res, collapse = "\n"))
  expect_true(any(res == paste("version", dl_version(), "")), label = paste(res, collapse = "\n"))
  expect_true(any(res == "insumos dl_bundle TRUE "), label = paste(res, collapse = "\n"))
  expect_true(any(res == "data.table FALSE TRUE TRUE "), label = paste(res, collapse = "\n"))
})
test_that("las rutas de los datos de ejemplo y de las referencias caben en un tarball portable (100 bytes)", {
  # R CMD build avisa y R CMD check da una NOTE por cada ruta de más de 100 bytes desde dismodlite/
  largas <- function(prefijo, raiz) {
    rel <- list.files(raiz, recursive = TRUE, all.files = TRUE)
    ruta <- paste(prefijo, rel, sep = "/")
    ruta[nchar(ruta, type = "bytes") > 100L]
  }
  expect_identical(largas("dismodlite/inst/extdata", system.file("extdata", package = "dismodlite")), character())
  expect_identical(largas("dismodlite/tests/testthat", testthat::test_path()), character())
})
test_that("el schema estimates.v1 del paquete es idéntico al canónico", {
  # copia del contrato estimates/v1 de la versión 1.0.0, guardada con las referencias de las pruebas: fija el
  # contrato instalado (un cambio debe ser deliberado). Difiere del de la versión 0.2.2 en que no declara la fuente
  # de un equipo externo ni la ruta de la huella de los catálogos, y en el texto de las descripciones y de las
  # reglas (en español); las claves que lee el paquete son las mismas.
  a <- readBin(testthat::test_path("_referencia", "esquemas", "estimates.v1.yaml"), "raw", 1e6)
  b <- readBin(system.file("schema", "estimates.v1.yaml", package = "dismodlite"), "raw", 1e6)
  expect_identical(a, b)
})
test_that("el paquete resuelve sus esquemas y perfiles desde su carpeta instalada", {
  expect_true(file.exists(.dl_schema_default_path()))
  expect_true(startsWith(.dl_schema_default_path(), .dl_pkg_dir()))
  expect_true(is.list(.dl_schema_estimates()))
  expect_true(all(file.exists(.dl_inst("perfiles", c("perfil_v1.yaml", "perfil_v2.yaml")))))
  expect_error(.dl_inst_archivo("schema", "no_existe.yaml"), "vuelve a instalar dismodlite")
})
test_that("una corrida se registra por defecto solo si se da el registro (nombres nuevos y anteriores)", {
  nuevas <- c(dl_exportar_corrida = "registro", dl_reresumir_corrida = "registro", dl_sumar_hijas = "registro",
              dl_consolidar = "registro_salida")
  for (fn in names(nuevas)) {
    expect_identical(deparse(formals(get(fn))$registrar), sprintf("!is.null(%s)", nuevas[[fn]]), info = fn)
    expect_null(formals(get(fn))[[nuevas[[fn]]]], info = fn)
  }
  anteriores <- c(dl_export = "registry_path", dl_resumir_run = "registry_path", dl_sum_hijas = "registry_path",
                  dl_export_cdc = "registry_out")
  for (fn in names(anteriores)) {
    expect_identical(deparse(formals(get(fn))$register), sprintf("!is.null(%s)", anteriores[[fn]]), info = fn)
    expect_null(formals(get(fn))[[anteriores[[fn]]]], info = fn)
  }
  # registrar = TRUE sin registro se detiene antes de escribir nada, con un mensaje que nombra el argumento
  d <- withr::local_tempdir()
  expect_error(dl_exportar_corrida(list(), "x", carpeta = d, etiquetas = data.frame(), registrar = TRUE),
               "registrar = TRUE exige `registro`")
  expect_error(dl_reresumir_corrida(d, carpeta = d, registrar = TRUE), "registrar = TRUE exige `registro`")
  expect_error(dl_sumar_hijas(d, 9100L, "x", carpeta = d, registrar = TRUE), "registrar = TRUE exige `registro`")
  expect_error(dl_consolidar("r.yaml", d, "p.yaml", "m.csv", registrar = TRUE),
               "registrar = TRUE exige `registro_salida`")
  expect_error(dl_export(list(), "x", out_root = d, labels = data.frame(), register = TRUE), "registro")
  expect_length(list.files(d, recursive = TRUE), 0L)
})
