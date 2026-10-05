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
      r <- dl_rutas_ejemplo(9101L, datos = FALSE, proxies = FALSE, formato = "completo")
      b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9101L, formato = "completo"), r))
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
  x <- corrida_9101()
  expect_error(dl_consolidado_seleccionar(x$registro, x$carpeta, anios = c(2023L, 2024L), rutas = x$rutas),
               "^dl_consolidado_seleccionar\\(\\): no hay corrida vigente para estas causas/años: 9101/2024")
  sel <- dl_consolidado_seleccionar(x$registro, x$carpeta, anios = c(2023L, 2024L), permitir_huecos = TRUE,
                                    rutas = x$rutas)
  expect_identical(nrow(attr(sel, "huecos")), 1L)
})

# ---- La incidencia que una corrida deja fuera, y una causa con hijas desde su propio ajuste ----

# Corrida mínima de la causa `causa` del ejemplo (formato completo), exportada en `carpeta` y anotada en `reg`.
# `cambios`: los de dl_configuracion() (por ejemplo exportar.incidencia).
.corrida_minima <- function(causa, carpeta, reg, cambios = NULL) {
  r <- dl_rutas_ejemplo(causa, datos = FALSE, proxies = FALSE, formato = "completo")
  b <- suppressMessages(dl_insumos(dl_configuracion(causa, ejemplo_completo("config"), cambios = cambios), r))
  o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
  f <- dl_ajustar(b, o, semilla = 3L)
  f0 <- dl_ajustar_solo_prior(b, o, semilla = 3L, ajuste = f)
  y <- dl_avd(f, b, semilla = 3L)
  piezas <- list(resumen = dl_resumir(list(fit = f, yld = y, bundle = b)), fit = f, yld = y, bundle = b)
  dl_exportar_corrida(piezas, sprintf("acs-%d", causa), carpeta = carpeta,
                      etiquetas = dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 3L), forzar = TRUE,
                      registro = reg)
}

# Una carpeta de corridas con las tres hijas de 9100 (la 9102 con exportar.incidencia = false), el ajuste de la causa
# padre 9100 y la suma de las hijas, y tres registros: `hijas` (9101 y 9102), `ajuste` (las hijas y el ajuste de
# 9100) y `suma` (las hijas y la suma de 9100). Memorizada en la sesión.
corridas_acs <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      carpeta <- withr::local_tempdir(.local_envir = testthat::teardown_env())
      reg <- function(nm) { f <- file.path(carpeta, paste0(nm, ".yaml")); writeLines("datasets: []", f); f }
      todo <- reg("todo")
      sin_incidencia <- list(exportar = list(incidencia = list(valor = FALSE, procedencia = "prueba: sin datos")))
      hijas <- list(.corrida_minima(9101L, carpeta, todo), .corrida_minima(9102L, carpeta, todo, sin_incidencia),
                    .corrida_minima(9103L, carpeta, todo))
      ajuste <- .corrida_minima(9100L, carpeta, todo)
      rutas <- dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo")
      suma <- dl_sumar_hijas(vapply(hijas, function(h) h$dir, ""), 9100L, "acs-suma", carpeta = carpeta,
                             registro = todo, rutas = rutas)
      # un registro con solo las corridas `ids` del registro completo
      entradas <- yaml::read_yaml(todo)$datasets
      solo <- function(nm, ids) {
        f <- file.path(carpeta, paste0(nm, ".yaml"))
        yaml::write_yaml(list(datasets = Filter(function(d) d$run_id %in% ids, entradas)), f)
        f
      }
      id <- function(l) vapply(l, function(h) h$run_id, "")
      x <<- list(carpeta = carpeta, rutas = rutas, hijas = hijas, ajuste = ajuste, suma = suma,
                 reg_hijas = solo("hijas", id(hijas[1:2])), reg_ajuste = solo("ajuste", c(id(hijas), ajuste$run_id)),
                 reg_suma = solo("suma", c(id(hijas), suma$run_id)))
    }
    x
  }
})

test_that("el consolidado omite la incidencia de la causa que la deja fuera y trae sus otras dos medidas", {
  x <- corridas_acs()
  maestro <- ejemplo_completo("registro", "master_gbd.csv")
  sel <- dl_consolidado_seleccionar(x$reg_hijas, x$carpeta, rutas = x$rutas)
  expect_identical(sel$cause_id, c(9101L, 9102L))
  canon <- dl_consolidado_canonico(sel, "2026-01-01_prueba_v1")
  expect_identical(nrow(canon[cause_id == 9102L & measure_id == 6L]), 0L)
  expect_gt(nrow(canon[cause_id == 9101L & measure_id == 6L]), 0L)
  # las otras dos medidas, de las dos causas, con las mismas celdas
  for (m in c(5L, 3L))
    expect_identical(nrow(canon[cause_id == 9102L & measure_id == m]), nrow(canon[cause_id == 9101L & measure_id == m]),
                     info = m)
  expect_setequal(unique(canon[cause_id == 9102L]$measure_id), c(5L, 3L))
  cons <- dl_consolidar(x$reg_hijas, x$carpeta, perfil_paquete("v1"), maestro, rutas = x$rutas, nombre = "sin-inc")
  leer <- function(medida) data.table::fread(file.path(cons$dir, "tablas", paste0(medida, ".csv")),
                                             colClasses = list(character = "ubigeo"))
  expect_identical(sort(unique(leer("incidence")$causa_gbd_id)), 9101L)
  expect_identical(sort(unique(leer("prevalence")$causa_gbd_id)), c(9101L, 9102L))
  expect_identical(sort(unique(leer("yld")$causa_gbd_id)), c(9101L, 9102L))
  # el manifiesto lo declara en el bloque de la causa y en las limitaciones; el de la otra causa no gana la clave
  man <- yaml::read_yaml(file.path(cons$dir, "manifest.yaml"))
  bloque <- function(k) Filter(function(b) b$cause_id == k, man$bloques)[[1]]
  expect_identical(bloque(9102L)$incidencia_exportada, FALSE)
  expect_false("incidencia_exportada" %in% names(bloque(9101L)))
  lim <- unlist(man$limitaciones)
  expect_identical(sum(startsWith(lim, "causa(s) 9102 sin incidencia en el consolidado")), 1L)
  # la corrida conserva su incidencia
  expect_true(file.exists(file.path(x$hijas[[2]]$dir, "cause", "incidence", paste0(x$hijas[[2]]$run_id, ".csv"))))
  # solo la causa sin incidencia: la tabla de incidencia queda con su encabezado y sin filas
  cons2 <- dl_consolidar(x$reg_hijas, x$carpeta, perfil_paquete("v1"), maestro, rutas = x$rutas, causas = 9102L,
                         nombre = "solo-sin-inc")
  expect_identical(nrow(data.table::fread(file.path(cons2$dir, "tablas", "incidence.csv"))), 0L)
  expect_gt(nrow(data.table::fread(file.path(cons2$dir, "tablas", "prevalence.csv"))), 0L)
  # los nombres anteriores hacen lo mismo
  expect_identical(dl_export_canonico(sel, "2026-01-01_prueba_v1"), canon)
})

test_that("la suma de hijas no hereda la marca: suma la incidencia de todas y el consolidado la trae", {
  x <- corridas_acs()
  expect_false("exporta_incidencia" %in% names(x$suma$manifest$causa))
  expect_false(any(grepl("exportar.incidencia", unlist(x$suma$manifest$limitaciones), fixed = TRUE)))
  sel <- dl_consolidado_seleccionar(x$reg_suma, x$carpeta, causas = 9100L, rutas = x$rutas)
  expect_identical(sel$agregacion, "suma_de_hijas")
  canon <- dl_consolidado_canonico(sel, "2026-01-01_prueba_v1")
  expect_gt(nrow(canon[measure_id == 6L]), 0L)
})

test_that("una causa con hijas en el maestro se consolida desde su ajuste solo con ajuste_directo, y lo declara", {
  x <- corridas_acs()
  maestro <- ejemplo_completo("registro", "master_gbd.csv")
  # sin el argumento: el error de siempre
  expect_error(dl_consolidado_seleccionar(x$reg_ajuste, x$carpeta, rutas = x$rutas),
               paste0("^dl_consolidado_seleccionar\\(\\): la causa 9100 se reporta como la suma de sus hijas .*",
                      "la corrida vigente .*_acs-9100_v1 es un ajuste"))
  expect_error(dl_consolidar(x$reg_ajuste, x$carpeta, perfil_paquete("v1"), maestro, rutas = x$rutas),
               "^dl_consolidar\\(\\): la causa 9100 se reporta como la suma de sus hijas")
  # con el argumento: la corrida de su ajuste, y la selección lo anota
  sel <- dl_consolidado_seleccionar(x$reg_ajuste, x$carpeta, rutas = x$rutas, ajuste_directo = 9100)
  expect_identical(sel[cause_id == 9100L]$run_id, x$ajuste$run_id)
  expect_true(is.na(sel[cause_id == 9100L]$agregacion))
  expect_identical(attr(sel, "ajuste_directo"), 9100L)
  cons <- dl_consolidar(x$reg_ajuste, x$carpeta, perfil_paquete("v1"), maestro, rutas = x$rutas, nombre = "directo",
                        ajuste_directo = 9100L)
  man <- yaml::read_yaml(file.path(cons$dir, "manifest.yaml"))
  bloque <- function(k) Filter(function(b) b$cause_id == k, man$bloques)[[1]]
  expect_identical(bloque(9100L)$ajuste_directo, TRUE)
  expect_identical(bloque(9100L)$agregacion, "fit")
  expect_identical(bloque(9100L)$run_fuente, x$ajuste$run_id)
  expect_false("ajuste_directo" %in% names(bloque(9101L)))
  lim <- unlist(man$limitaciones)
  expect_identical(sum(startsWith(lim, "causa(s) 9100 con hijas en master_gbd.csv consolidada(s) desde su propio")),
                   1L)
  expect_false(any(grepl("como suma de sus hijas", lim)))
  prev <- data.table::fread(file.path(cons$dir, "tablas", "prevalence.csv"), colClasses = list(character = "ubigeo"))
  expect_true(all(prev[causa_gbd_id == 9100L]$run_origen == x$ajuste$run_id))
  # sin el argumento nada cambia: la selección de las hijas no gana el atributo ni sus bloques la clave
  sel_h <- dl_consolidado_seleccionar(x$reg_hijas, x$carpeta, rutas = x$rutas)
  expect_null(attr(sel_h, "ajuste_directo"))
  expect_identical(dl_consolidado_seleccionar(x$reg_hijas, x$carpeta, rutas = x$rutas, ajuste_directo = integer()),
                   sel_h)
})

test_that("ajuste_directo se valida: causas enteras, entre las pedidas, con hijas, y con un ajuste vigente", {
  x <- corridas_acs()
  sel <- function(reg, ...) dl_consolidado_seleccionar(reg, x$carpeta, rutas = x$rutas, ...)
  # la corrida vigente de la causa es una suma: error
  expect_error(sel(x$reg_suma, ajuste_directo = 9100L),
               paste0("`ajuste_directo` nombra la causa 9100 y su corrida vigente .*_acs-suma_v1 es una suma de hijas, ",
                      "no un ajuste"))
  expect_error(dl_consolidar(x$reg_suma, x$carpeta, perfil_paquete("v1"), ejemplo_completo("registro", "master_gbd.csv"),
                             rutas = x$rutas, ajuste_directo = 9100L),
               "^dl_consolidar\\(\\): `ajuste_directo` nombra la causa 9100 .* es una suma de hijas")
  # no son causas
  for (malo in list("9100", 9100.5, c(9100, NA), c(9100L, 9100L), -1, list(9100L)))
    expect_error(sel(x$reg_ajuste, ajuste_directo = malo), "`ajuste_directo` debe ser un vector de causas")
  # no está entre las pedidas, o no tiene corrida entre las seleccionadas
  expect_error(sel(x$reg_ajuste, causas = c(9101L, 9102L), ajuste_directo = 9100L),
               "`ajuste_directo` nombra la\\(s\\) causa\\(s\\) 9100, que no está\\(n\\) en `causas` \\(9101, 9102\\)")
  expect_error(sel(x$reg_hijas, ajuste_directo = 9100L),
               "`ajuste_directo` nombra la\\(s\\) causa\\(s\\) 9100, que no está\\(n\\) entre las causas con corrida")
  # una causa sin hijas en el maestro: aviso, y la selección es la de siempre (sin el atributo)
  expect_warning(s <- sel(x$reg_hijas, ajuste_directo = 9101L),
                 "`ajuste_directo` nombra la\\(s\\) causa\\(s\\) 9101, que master_gbd.csv no declara con hijas")
  expect_identical(s, sel(x$reg_hijas))
  # NULL vale por ninguna
  expect_identical(sel(x$reg_hijas, ajuste_directo = NULL), sel(x$reg_hijas))
  # los nombres anteriores conservan los argumentos de la versión 0.2.2: no tienen este
  expect_false("fit_directo" %in% names(formals(dl_export_seleccionar)))
  expect_false("fit_directo" %in% names(formals(dl_export_cdc)))
})
