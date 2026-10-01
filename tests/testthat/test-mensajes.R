# Mensajes de error y avisos: empiezan con la función que llamó el usuario («dl_ajustar(): ...»), citan los nombres
# nuevos de los argumentos (semilla, calentamiento, carpeta) y no nombran variables internas ni variables de
# entorno. Estas pruebas llaman a las funciones; la revisión del código fuente (todo mensaje sale por .dl_stop(),
# .dl_warn() o .dl_message(), que le ponen la función delante) está en test-fuente-mensajes.R.

.mensaje_de <- function(expr) tryCatch({ expr; NA_character_ }, error = conditionMessage)

test_that("dl_ajustar() sin semilla: el mensaje nombra la función y el argumento `semilla`", {
  b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, datos = FALSE)))
  expect_error(dl_ajustar(b), "^dl_ajustar\\(\\): .*semilla")
  expect_error(dl_ajustar(b, semilla = "uno"), "^dl_ajustar\\(\\): `semilla` debe ser un número entero")
  expect_error(dl_ajustar_solo_prior(b), "^dl_ajustar_solo_prior\\(\\): .*semilla")
})

test_that("dl_opciones_mcmc(): el calentamiento se valida con su nombre en español y el múltiplo que exige", {
  m <- .mensaje_de(dl_opciones_mcmc(calentamiento = 150))
  expect_match(m, "^dl_opciones_mcmc\\(\\): ")
  expect_match(m, "calentamiento")
  expect_match(m, "200")
  expect_error(dl_opciones_mcmc(iteraciones = 1000, calentamiento = 1000), "`calentamiento` \\(1000\\) debe ser menor")
  expect_error(dl_opciones_mcmc(cadenas = 0), "^dl_opciones_mcmc\\(\\): `cadenas` debe ser un entero")
})

test_that("las funciones que escriben corridas piden `carpeta` sin nombrar variables de entorno", {
  # sin la carpeta de datos de respaldo en el entorno (setup-entorno.R), `carpeta` vacía o ausente es un error; la
  # comprobación va antes de leer las piezas, así que bastan argumentos mínimos
  llamadas <- list(
    dl_exportar_corrida  = function(...) dl_exportar_corrida(list(), nombre = "prueba", ...),
    dl_reresumir_corrida = function(...) dl_reresumir_corrida(withr::local_tempdir(), ...),
    dl_sumar_hijas       = function(...) dl_sumar_hijas(list(), causa = 9100L, nombre = "prueba", ...),
    dl_consolidar        = function(...) dl_consolidar(tempfile(), perfil = "perfil_v1", maestro = tempfile(), ...))
  for (fn in names(llamadas)) {
    for (m in c(.mensaje_de(llamadas[[fn]](carpeta = "")), .mensaje_de(llamadas[[fn]]()))) {
      expect_match(m, sprintf("^%s\\(\\): falta `carpeta`", fn), info = fn)
      expect_no_match(m, "DATA_ROOT|Sys[.]getenv", info = fn)
    }
  }
})

test_that("anidadas o con |>, el mensaje nombra la función que recibió el argumento malo", {
  # el argumento que es otra llamada se evalúa en el entorno del usuario: el error de la configuración es de
  # dl_configuracion_ejemplo(), el de las rutas es de dl_insumos(), aunque el usuario escribió dl_ajustar() afuera
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(anchor = list(lambda = 5))) |>
                 dl_insumos(dl_rutas_ejemplo(9100L)) |> dl_ajustar(semilla = 1),
               "^dl_configuracion_ejemplo\\(\\): ")
  cfg <- dl_configuracion_ejemplo(9100L)
  rutas_malas <- dl_rutas_ejemplo(9100L, poblacion = file.path(tempdir(), "no_existe", "poblacion.csv"))
  expect_error(dl_ajustar(dl_insumos(cfg, rutas_malas), semilla = 1), "^dl_insumos\\(\\): la ruta")
  # por una variable, con do.call() o dentro de una función del usuario (aunque se llame dl_...): la función exportada
  f <- dl_insumos
  expect_error(f(cfg, rutas_malas), "^dl_insumos\\(\\): la ruta")
  expect_error(do.call(dl_insumos, list(cfg, rutas_malas)), "^dl_insumos\\(\\): la ruta")
  dl_mi_flujo <- function() dl_insumos(cfg, rutas_malas)
  expect_error(dl_mi_flujo(), "^dl_insumos\\(\\): la ruta")
  # con un nombre anterior, el mensaje nombra ese nombre
  expect_error(dl_config("x"), "^dl_config\\(\\): `causa`")
})

test_that("el error es de clase dl_error, con el detalle sin la función y la función que llamó el usuario", {
  b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, datos = FALSE)))
  e <- tryCatch(dl_ajustar(b), error = identity)
  expect_s3_class(e, "dl_error")
  expect_identical(e$funcion, "dl_ajustar")
  expect_match(e$detalle, "^falta `semilla`")
  expect_identical(conditionMessage(e), paste("dl_ajustar():", e$detalle))
  # con un nombre anterior: el mensaje nombra la función que se llamó, cita el argumento con su nombre nuevo y lo dice
  # en una última línea que remite a ?dl_nombres_anteriores (fuera del detalle)
  e_anterior <- tryCatch(dl_fit(b), error = identity)
  expect_s3_class(e_anterior, "dl_error")
  expect_identical(e_anterior$detalle, e$detalle)
  expect_identical(conditionMessage(e_anterior), paste0(
    "dl_fit(): ", e$detalle, "\n(dl_fit() es el nombre anterior de dl_ajustar(): el mensaje usa los argumentos de ",
    "dl_ajustar(); ver ?dl_nombres_anteriores)"))
  # los avisos llevan la función igual
  w <- tryCatch(dl_opciones_mcmc(simulaciones = 5000L, cadenas = 1L, iteraciones = 400L, calentamiento = 200L),
                warning = identity)
  expect_s3_class(w, "dl_warning")
  expect_identical(w$funcion, "dl_opciones_mcmc")
  # sin una función exportada en la pila (una función interna llamada directamente), el mensaje empieza con dismodlite:
  expect_error(dismodlite:::.dl_stop("x %d", 1L), "^dismodlite: x 1$")
})

test_that("un YAML con un error de sintaxis se reporta en español, con la línea y el archivo", {
  carpeta <- withr::local_tempdir()
  lineas <- readLines(dl_ejemplo("config", "9100.yaml"))
  lineas[5] <- paste0(lineas[5], ": : [")
  writeLines(lineas, file.path(carpeta, "9100.yaml"))
  m <- .mensaje_de(dl_configuracion(9100L, carpeta_config = carpeta))
  expect_match(m, "^dl_configuracion\\(\\): el archivo «9100.yaml» no es un YAML válido \\(línea 5, columna")
  expect_no_match(m, "Scanner|mapping values|yaml.load")
})

test_that("los avisos de data.table::fread() llegan con la función del usuario delante", {
  # pesos_80mas.csv con una última línea incompleta: fread la descarta con un aviso en inglés, que el paquete
  # traduce; cualquier otro aviso de fread llega con su texto, pero también con la función delante
  f <- file.path(withr::local_tempdir(), "pesos_80mas.csv")
  writeLines(c(readLines(ejemplo_completo("pesos_80mas.csv")), "30"), f)
  avisos <- character()
  withCallingHandlers(
    suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, datos = FALSE,
                                                                                  pesos_80mas = f))),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_gte(length(avisos), 1L)                              # uno por cada lectura del archivo
  expect_match(avisos, "^dl_insumos\\(\\): se descartó la última línea de «pesos_80mas.csv»", all = TRUE)
})
