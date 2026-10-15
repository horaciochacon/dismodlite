# Espacio en disco de las simulaciones de una corrida: guardar_simulaciones = FALSE no escribe draws/ y el manifiesto
# lo declara; cifras_simulaciones guarda cada simulación con menos cifras significativas; quien necesita las
# simulaciones (re-resumen, suma de subtipos) falla con un mensaje que dice por qué faltan y qué hacer.

.piezas_sim <- function(x = corrida_mini())
  list(resumen = dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = x$insumos)), fit = x$ajuste, yld = x$avd,
       bundle = x$insumos)

.exportar_sim <- function(d, nombre, ...) {
  x <- corrida_mini()
  dl_exportar_corrida(.piezas_sim(x), nombre, carpeta = d, etiquetas = x$etiquetas, forzar = TRUE, ...)
}

.celdas_run <- function(run, slug)
  data.table::fread(file.path(run$dir, "cause", slug, paste0(run$run_id, ".csv")))[, run_id := NULL]

test_that("guardar_simulaciones = FALSE no escribe draws/, el manifiesto lo declara y las celdas no cambian", {
  d <- withr::local_tempdir()
  todo <- .exportar_sim(d, "con-sim")
  sin <- .exportar_sim(d, "sin-sim", guardar_simulaciones = FALSE)
  expect_true(dir.exists(file.path(todo$dir, "draws")))
  expect_false(dir.exists(file.path(sin$dir, "draws")))
  man <- yaml::read_yaml(file.path(sin$dir, "manifest.yaml"))
  man0 <- yaml::read_yaml(file.path(todo$dir, "manifest.yaml"))
  expect_identical(man$params$simulaciones_guardadas, FALSE)
  # por defecto, ni la clave ni las cifras: los manifiestos de siempre no cambian
  expect_false(any(c("simulaciones_guardadas", "cifras_simulaciones") %in% names(man0$params)))
  expect_identical(man$params[names(man0$params)], man0$params)
  for (s in c("prevalence", "incidence", "yld"))
    expect_identical(.celdas_run(sin, s), .celdas_run(todo, s), info = s)
})

test_that("cifras_simulaciones guarda N cifras significativas, lo declara y no cambia las celdas", {
  d <- withr::local_tempdir()
  todo <- .exportar_sim(d, "todas")
  seis <- .exportar_sim(d, "seis", cifras_simulaciones = 6)
  expect_identical(yaml::read_yaml(file.path(seis$dir, "manifest.yaml"))$params$cifras_simulaciones, 6L)
  for (s in c("prevalence", "incidence", "yld")) {
    a <- dismodlite:::.dl_leer_draws(todo$dir, s, 2023L)
    b <- dismodlite:::.dl_leer_draws(seis$dir, s, 2023L)
    cols <- grep("^draw_", names(a), value = TRUE)
    ma <- as.matrix(a[, cols, with = FALSE]); mb <- as.matrix(b[, cols, with = FALSE])
    rel <- abs(mb - ma) / pmax(abs(ma), .Machine$double.xmin)
    expect_lte(max(rel[ma != 0]), 0.5 * 10^(1 - 6) * (1 + 1e-9))
    expect_true(all(mb[ma == 0] == 0))
    expect_identical(.celdas_run(seis, s), .celdas_run(todo, s), info = s)   # las celdas, de las simulaciones enteras
  }
  # el resumen de quien llamó no se redondea por referencia
  x <- corrida_mini(); piezas <- .piezas_sim(x); antes <- data.table::copy(piezas$resumen$draws)
  dl_exportar_corrida(piezas, "otra", carpeta = d, etiquetas = x$etiquetas, forzar = TRUE, cifras_simulaciones = 3)
  expect_identical(piezas$resumen$draws, antes)
})

test_that("los argumentos de las simulaciones se validan antes de escribir nada", {
  d <- withr::local_tempdir()
  expect_error(.exportar_sim(d, "a", guardar_simulaciones = NA), "`guardar_simulaciones` debe ser TRUE o FALSE")
  for (malo in list(0, 16, 2.5, "6", c(5, 6)))
    expect_error(.exportar_sim(d, "a", cifras_simulaciones = malo),
                 "`cifras_simulaciones` debe ser NULL o un entero entre 1 y 15", info = format(malo))
  expect_error(.exportar_sim(d, "a", guardar_simulaciones = FALSE, cifras_simulaciones = 6),
               "`cifras_simulaciones` no aplica con guardar_simulaciones = FALSE")
  expect_length(list.files(d, recursive = TRUE, include.dirs = TRUE), 0L)
})

test_that("el re-resumen de una corrida sin simulaciones falla diciendo por qué faltan y qué hacer", {
  d <- withr::local_tempdir()
  r <- dl_rutas_ejemplo(9100L, datos = FALSE)
  sin <- .exportar_sim(d, "sin-sim", guardar_simulaciones = FALSE)
  expect_error(dl_reresumir_corrida(sin$dir, carpeta = d, rutas = r),
               paste0("^dl_reresumir_corrida\\(\\): la corrida .*_sin-sim_v1 se escribió sin simulaciones ",
                      "\\(guardar_simulaciones = FALSE en su manifiesto\\).*guardar_simulaciones = TRUE"))
  # borradas después de escribirla: el manifiesto no lo declara
  borrada <- .exportar_sim(d, "borrada")
  unlink(file.path(borrada$dir, "draws"), recursive = TRUE)
  expect_error(dl_reresumir_corrida(borrada$dir, carpeta = d, rutas = r),
               paste0("^dl_reresumir_corrida\\(\\): la corrida .*_borrada_v1 no tiene las simulaciones de prevalence ",
                      "\\(.*prevalence_2023.csv.gz\\): se borraron después de escribirla.*",
                      "guardar_simulaciones = TRUE"))
  expect_false(any(grepl("_v2$", list.dirs(.dl_dir_corrida(d), recursive = FALSE))))   # nada escrito
  # con cifras_simulaciones, el re-resumen funciona y hereda la declaración
  seis <- .exportar_sim(d, "seis", cifras_simulaciones = 6)
  rr <- dl_reresumir_corrida(seis$dir, carpeta = d, rutas = r)
  expect_identical(rr$manifest$params$cifras_simulaciones, 6L)
})

test_that("la suma de subtipos nombra la hija sin simulaciones y escribe la suma con las opciones pedidas", {
  x <- corridas_acs()
  dirs <- vapply(x$hijas, function(h) h$dir, "")
  d <- withr::local_tempdir()
  # una hija copiada sin draws/
  copia <- file.path(d, "mod", "dismod_lite", basename(dirs[2])); dir.create(dirname(copia), recursive = TRUE)
  file.copy(dirs[2], dirname(copia), recursive = TRUE)
  unlink(file.path(copia, "draws"), recursive = TRUE)
  expect_error(dl_sumar_hijas(c(dirs[1], copia, dirs[3]), 9100L, "suma", carpeta = d, rutas = x$rutas),
               sprintf("^dl_sumar_hijas\\(\\): la corrida %s no tiene las simulaciones de ", basename(dirs[2])))
  # la suma sin simulaciones propias: sus celdas son las de la suma con ellas
  sin <- dl_sumar_hijas(dirs, 9100L, "suma-sin", carpeta = d, rutas = x$rutas, guardar_simulaciones = FALSE)
  expect_false(dir.exists(file.path(sin$dir, "draws")))
  expect_identical(sin$manifest$params$simulaciones_guardadas, FALSE)
  for (s in c("prevalence", "yld"))
    expect_equal(.celdas_run(sin, s), .celdas_run(x$suma, s), info = s)
  seis <- dl_sumar_hijas(dirs, 9100L, "suma-seis", carpeta = d, rutas = x$rutas, cifras_simulaciones = 6)
  expect_identical(seis$manifest$params$cifras_simulaciones, 6L)
  expect_false("cifras_simulaciones" %in% names(x$suma$manifest$params))
  expect_false(any(vapply(seis$manifest$causa$hijas, function(h) "cifras_simulaciones" %in% names(h), NA)))
  # una hija guardada con menos cifras: la suma lo declara en causa.hijas
  m <- yaml::read_yaml(file.path(copia, "manifest.yaml"))
  m$params$cifras_simulaciones <- 6L
  yaml::write_yaml(m, file.path(copia, "manifest.yaml"))
  file.copy(file.path(dirs[2], "draws"), copia, recursive = TRUE)
  mixta <- dl_sumar_hijas(c(dirs[1], copia, dirs[3]), 9100L, "suma-mixta", carpeta = d, rutas = x$rutas)
  expect_identical(lapply(mixta$manifest$causa$hijas, `[[`, "cifras_simulaciones"), list(NULL, 6L, NULL))
})

test_that("dl_correr() pasa guardar_simulaciones y cifras_simulaciones a la corrida y a la suma", {
  visto <- list()
  local_mocked_bindings(
    .dl_correr_proyecto = function(...) { visto$proyecto <<- list(...)$simulaciones; "proyecto" },
    .dl_correr_suma = function(...) { visto$suma <<- list(...)$simulaciones; "suma" })
  expect_identical(dl_correr(dl_ejemplo(), 9100, semilla = 1, carpeta_salida = withr::local_tempdir(),
                             guardar_simulaciones = FALSE), "proyecto")
  expect_identical(visto$proyecto, list(guardar = FALSE, cifras = NULL))
  d <- copia_con_suma()
  expect_identical(dl_correr(d, 9200, cifras_simulaciones = 6), "suma")
  expect_identical(visto$suma, list(guardar = TRUE, cifras = 6L))
  expect_error(dl_correr(dl_ejemplo(), 9100, semilla = 1, carpeta_salida = withr::local_tempdir(),
                         cifras_simulaciones = 0), "`cifras_simulaciones` debe ser NULL o un entero entre 1 y 15")
})
