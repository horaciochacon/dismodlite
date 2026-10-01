# dl_consolidar(): escribe mod/consolidado/<id>/{canonico/, tablas/, manifest.yaml} con los perfiles del paquete
# (inst/perfiles), sin carpetas, archivos ni textos de versiones anteriores del paquete, y con la prosa del
# manifiesto armada con las causas presentes. Las cuentas del consolidado las compara con la versión 0.2.2 el arnés
# (escenarios E8 y E10 de test-compatibilidad-legado.R).

# Una corrida mínima del subtipo 9101 (no es suma de nadie: se consolida sola), exportada y registrada en una
# carpeta con espacios y tildes. Memorizada en la sesión.
corrida_9101 <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      carpeta <- file.path(withr::local_tempdir(.local_envir = testthat::teardown_env()), "Ana María",
                           "Mis análisis")
      dir.create(carpeta, recursive = TRUE)
      reg <- file.path(carpeta, "registro de corridas.yaml"); writeLines("datasets: []", reg)
      r <- dl_rutas_ejemplo(9101L, datos = FALSE, proxies = FALSE)
      b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9101L), r))
      o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
      f <- dl_ajustar(b, o, semilla = 3L)
      f0 <- dl_ajustar_solo_prior(b, o, semilla = 3L, ajuste = f)
      y <- dl_avd(f, b, semilla = 3L)
      piezas <- list(resumen = dl_resumir(list(fit = f, yld = y, bundle = b)), fit = f, yld = y, bundle = b)
      run <- dl_exportar_corrida(piezas, "acs-9101", carpeta = carpeta,
                                 etiquetas = dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 3L), forzar = TRUE,
                                 registro = reg)
      x <<- list(carpeta = carpeta, registro = reg, rutas = r, corrida = run)
    }
    x
  }
})

perfil_paquete <- function(v) system.file("perfiles", sprintf("perfil_%s.yaml", v), package = "dismodlite")

# «cdc» como palabra (no dentro de una suma de control hexadecimal, que puede contener esas letras) o «entrega».
.PROHIBIDO <- "(^|[^[:alnum:]])cdc([^[:alnum:]]|$)|entrega"
prohibido <- function(x) any(grepl(.PROHIBIDO, x, ignore.case = TRUE))

# Texto de todos los archivos de texto bajo `dir` (CSV y YAML).
.texto_arbol <- function(dir) {
  arch <- list.files(dir, pattern = "[.](csv|yaml)$", recursive = TRUE, full.names = TRUE)
  unlist(lapply(arch, readLines, encoding = "UTF-8", warn = FALSE))
}

test_that("los perfiles del paquete se leen y no llevan nombres de versiones anteriores", {
  for (v in c("v1", "v2")) {
    p <- perfil_paquete(v)
    expect_true(file.exists(p), info = v)
    perf <- dl_perfil_cargar(p)
    expect_identical(perf$id, paste0("perfil_", v))
    expect_identical(perf$medidas, c("prevalence", "incidence", "yld"))
    expect_false(prohibido(readLines(p, encoding = "UTF-8")), info = v)
  }
  expect_identical(sort(list.files(system.file("perfiles", package = "dismodlite"))),
                   c("perfil_v1.yaml", "perfil_v2.yaml"))
})

test_that("el consolidado escribe mod/consolidado/<id>/ con canonico/, tablas/ y manifest.yaml", {
  skip("contrato: se reescribe en la Tarea 8")
  x <- corrida_9101()
  maestro <- ejemplo_completo("registro", "master_gbd.csv")
  cons <- dl_consolidar(x$registro, x$carpeta, perfil_paquete("v1"), maestro, rutas = x$rutas, anios = 2023L)
  # nombre por defecto "consolidado"; no se registra sin registro_salida
  expect_match(cons$run_id, "^[0-9]{4}-[0-9]{2}-[0-9]{2}_consolidado_v1$")
  expect_identical(cons$dir, file.path(x$carpeta, "mod", "consolidado", cons$run_id))
  expect_setequal(list.files(cons$dir), c("canonico", "manifest.yaml", "tablas"))
  expect_setequal(list.files(file.path(cons$dir, "tablas")), c("prevalence.csv", "incidence.csv", "yld.csv"))
  expect_setequal(list.files(file.path(cons$dir, "canonico", "cause")), c("prevalence", "incidence", "yld"))
  expect_length(yaml::read_yaml(x$registro)$datasets, 1L)
  # manifiesto: método, perfil y particiones con la carpeta nueva
  man <- yaml::read_yaml(file.path(cons$dir, "manifest.yaml"))
  expect_identical(man$method, "consolidado")
  expect_identical(man$perfil$id, "perfil_v1")
  expect_identical(man$perfil$archivo, "perfil_v1.yaml")
  expect_true(all(startsWith(vapply(man$files, function(f) f$path, ""),
                             paste0("mod/consolidado/", cons$run_id, "/canonico/cause/"))))
  expect_identical(unlist(man$params$causas), 9101L)
  # tablas del perfil: columnas del perfil y una fila por celda y métrica de la tabla canónica
  prev <- data.table::fread(file.path(cons$dir, "tablas", "prevalence.csv"), colClasses = list(character = "ubigeo"))
  expect_identical(names(prev)[1:4], c("causa_gbd_id", "causa_gbd", "anio", "ubigeo"))
  expect_setequal(unique(prev$metrica), c("Percent", "Number"))
  expect_true(all(prev$run_origen == x$corrida$run_id))
  # con perfil_v2: archivos en español y registro de salida
  reg_salida <- file.path(x$carpeta, "consolidados.yaml"); writeLines("datasets: []", reg_salida)
  cons2 <- dl_consolidar(x$registro, x$carpeta, perfil_paquete("v2"), maestro, rutas = x$rutas, anios = 2023L,
                         registro_salida = reg_salida)
  expect_match(cons2$run_id, "_consolidado_v2$")
  expect_setequal(list.files(file.path(cons2$dir, "tablas")), c("prevalencia.csv", "incidencia.csv", "avd.csv"))
  ds <- yaml::read_yaml(reg_salida)$datasets
  expect_length(ds, 1L)
  expect_identical(ds[[1]]$run_id, cons2$run_id)
  expect_identical(ds[[1]]$method, "consolidado")
})

test_that("ni las carpetas, ni los archivos, ni su contenido llevan «cdc» o «entrega»", {
  skip("contrato: se reescribe en la Tarea 8")
  x <- corrida_9101()
  cons <- dl_consolidar(x$registro, x$carpeta, perfil_paquete("v2"), ejemplo_completo("registro", "master_gbd.csv"),
                        rutas = x$rutas, anios = 2023L, nombre = "revision")
  # la corrida exportada, sus insumos congelados, los consolidados y los registros
  rutas <- list.files(x$carpeta, recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  expect_true(any(startsWith(rutas, "mod/consolidado/")))
  expect_false(prohibido(rutas))
  expect_false(prohibido(.texto_arbol(x$carpeta)))
  expect_false(prohibido(unlist(cons$manifest)))
  # la regla sí detecta cada forma
  expect_true(all(vapply(c("mod/export_cdc/x", "correspondencia_cdc: a", "CDC", "entrega/"), prohibido, TRUE)))
  expect_false(prohibido("sha256: 3cdc9f0e"))
})

test_that("la prosa del manifiesto sale de las causas presentes, sin textos fijos de un país o una causa", {
  skip("contrato: se reescribe en la Tarea 8")
  x <- corrida_9101()
  cons <- dl_consolidar(x$registro, x$carpeta, perfil_paquete("v1"), ejemplo_completo("registro", "master_gbd.csv"),
                        rutas = x$rutas, anios = 2023L, nombre = "prosa")
  texto <- readLines(file.path(cons$dir, "manifest.yaml"), encoding = "UTF-8")
  # cabecera con las causas y años presentes y el perfil
  expect_match(texto[1], paste0("^# manifiesto de un consolidado mod/consolidado: 1 causa\\(s\\) \\(9101\\) y ",
                                "1 a\u00f1o\\(s\\) \\(2023\\)$"))
  expect_match(texto[2], "perfil perfil_v1$")
  expect_false(any(grepl("INEI|COVID|CVD|Perú|Peru|GHE|correspondencia", texto)))
  lim <- unlist(yaml::read_yaml(file.path(cons$dir, "manifest.yaml"))$limitaciones)
  # una sola causa ajustada, sin sumas, proyecciones ni fase aguda: solo los conteos y la remisión a cada corrida
  expect_length(lim, 2L)
  expect_match(lim[1], "^conteos = tasa x población congelada")
  expect_match(lim[2], "run_fuente")
})

test_that("las limitaciones nombran las causas por suma, las hijas omitidas, los años proyectados y la fase aguda", {
  man_fit <- list(params = list(csmr_fraccion_aguda = 0.3))
  man_suma <- list(causa = list(hijas = list(list(csmr_fraccion_aguda = 0), list(csmr_fraccion_aguda = 0))))
  sel <- data.table::data.table(cause_id = c(9100L, 9101L), year = 2024L, agregacion = c("suma_de_hijas", NA),
                                manifest = list(man_suma, man_fit))
  bloques <- list(list(cause_id = 9100L, year = 2024L, anio_ancla = 2023L,
                       hijas_omitidas = list(list(cause_id = 9103L))),
                  list(cause_id = 9101L, year = 2024L, anio_ancla = 2023L, hijas_omitidas = list()))
  lim <- unlist(dismodlite:::.dl_consolidado_limitaciones(sel, bloques, "poblacion_prueba"))
  expect_length(lim, 5L)
  expect_match(lim[1], "(poblacion_prueba)", fixed = TRUE)
  expect_match(lim[2], "^causa\\(s\\) 9100 como suma de sus hijas.*hijas 9103 omitidas")
  expect_match(lim[3], "2024 proyectado")
  expect_match(lim[4], "^causa\\(s\\) 9101 con la fase aguda descontada")
})

test_that("un perfil mal escrito da un error en español que nombra la función y el archivo", {
  d <- withr::local_tempdir(); p <- file.path(d, "perfil_malo.yaml")
  expect_error(dl_perfil_cargar(p),
               "^dl_perfil_cargar\\(\\): no existe el archivo del perfil .*perfil_malo.yaml.*perfil_v1.yaml")
  writeLines(c("perfil: malo", "archivos:", "  por: cause", "  nombre: '{measure_slug}.csv'"), p)
  expect_error(dl_perfil_cargar(p),
               "^dl_perfil_cargar\\(\\): el perfil de columnas perfil_malo.yaml tiene archivos.por distinto")
  # desde dl_consolidar(), el mensaje nombra dl_consolidar()
  expect_error(dl_consolidar("r.yaml", d, p, "m.csv"), "^dl_consolidar\\(\\): el perfil de columnas perfil_malo")
})

test_that("una causa y un año sin corrida vigente se nombran; permitir_huecos los declara", {
  skip("contrato: se reescribe en la Tarea 8")
  x <- corrida_9101()
  expect_error(dl_consolidado_seleccionar(x$registro, x$carpeta, anios = c(2023L, 2024L), rutas = x$rutas),
               "^dl_consolidado_seleccionar\\(\\): no hay corrida vigente para estas causas/años: 9101/2024")
  sel <- dl_consolidado_seleccionar(x$registro, x$carpeta, anios = c(2023L, 2024L), permitir_huecos = TRUE,
                                    rutas = x$rutas)
  expect_identical(nrow(attr(sel, "huecos")), 1L)
})
