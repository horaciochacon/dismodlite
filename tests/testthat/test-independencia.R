# El paquete no depende de nada fuera de sí mismo y de los argumentos: sus archivos (esquemas, perfiles, C++) salen
# siempre de la carpeta instalada, del entorno solo lee los respaldos de las rutas (DATA_ROOT y CATALOGOS_DIR) y una
# corrida se anota en un registro solo si se le da uno.

test_that("el paquete no lee la variable de entorno del repositorio anterior", {
  src <- system.file(package = "dismodlite")
  withr::local_envvar(DISMODLITE_DIR = withr::local_tempdir())
  expect_identical(dismodlite:::.dl_pkg_dir(), normalizePath(src))
  expect_true(startsWith(dismodlite:::.dl_schema_default_path(), normalizePath(src)))
  expect_identical(dl_version(), unname(read.dcf(system.file("DESCRIPTION", package = "dismodlite"),
                                                 fields = "Version")[1, 1]))
})

test_that("el paquete importa data.table: cargarlo carga data.table y `[` de sus tablas es el de data.table", {
  # sin la importación, library(dismodlite) no cargaría data.table y la primera función que indexa una tabla
  # constante del paquete sin llamar antes a data.table:: (dl_reresumir_corrida) fallaría en una sesión nueva; la
  # comprobación en un proceso nuevo está en test-portabilidad.R
  expect_true("data.table" %in% names(getNamespaceImports("dismodlite")))
  m <- dismodlite:::.DL_MEDIDAS
  expect_identical(nrow(m[m$exporta]), sum(m$exporta))
})

test_that("del entorno, el paquete solo lee DATA_ROOT y CATALOGOS_DIR (respaldos de las rutas)", {
  # todas las llamadas a Sys.getenv() en las funciones del paquete (cuerpos y valores por defecto)
  ns <- asNamespace("dismodlite")
  leidas <- unique(unlist(lapply(ls(ns, all.names = TRUE), function(nm) {
    f <- get(nm, envir = ns)
    if (!is.function(f)) return(NULL)
    txt <- paste(deparse(f, control = "keepNA"), collapse = "\n")
    m <- regmatches(txt, gregexpr("Sys[.]getenv\\([^)]*\\)", txt))[[1]]
    sub("^Sys[.]getenv\\((.*)\\)$", "\\1", m)
  })))
  expect_setequal(leidas, c("\"DATA_ROOT\"", "\"CATALOGOS_DIR\""))
})

test_that("sin `registro`, dl_rutas() no apunta a ningún registro aunque haya variables de entorno", {
  withr::local_envvar(DATA_ROOT = withr::local_tempdir(), CATALOGOS_DIR = withr::local_tempdir())
  expect_null(dl_rutas()$registry)
  # el mensaje distingue la carpeta `registro` de dl_rutas() del registro de corridas
  expect_error(dismodlite:::.dl_etiquetas_es(dl_rutas()),
               paste0("falta la ruta de «registro» \\(la carpeta de tablas de referencia .*no es el registro de ",
                      "corridas\\) en `rutas`"))
})

test_that("una corrida no se registra si no se da `registro`", {
  b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L),
                                   dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE)))
  f <- dl_ajustar(b, dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L),
                  semilla = 1L)
  y <- dl_avd(f, b, semilla = 1L)
  f0 <- dl_ajustar_solo_prior(b, dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L,
                                                  calentamiento = 200L), semilla = 1L, ajuste = f)
  et <- dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 1L)
  carpeta <- withr::local_tempdir()
  # un registro de corridas en la carpeta, con el nombre de siempre: no se toca si no se pasa
  reg <- file.path(carpeta, "datasets.yaml"); writeLines("datasets: []", reg)
  corrida <- dl_exportar_corrida(list(resumen = dl_resumir(list(fit = f, yld = y, bundle = b)), fit = f, yld = y,
                                      bundle = b), nombre = "prueba", carpeta = carpeta, etiquetas = et,
                                 forzar = TRUE)
  expect_true(dir.exists(corrida$dir))
  expect_identical(readLines(reg), "datasets: []")
  expect_length(setdiff(list.files(carpeta, pattern = "^datasets[.]yaml$", recursive = TRUE), "datasets.yaml"), 0L)
  # con `registro`, sí
  otra <- dl_exportar_corrida(list(resumen = dl_resumir(list(fit = f, yld = y, bundle = b)), fit = f, yld = y,
                                   bundle = b), nombre = "prueba", carpeta = carpeta, etiquetas = et, forzar = TRUE,
                              registro = reg)
  expect_identical(vapply(yaml::read_yaml(reg)$datasets, function(d) d$run_id, ""), otra$run_id)
})
