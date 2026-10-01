# Herramientas de la carpeta de un proyecto (R/herramientas.R): dl_nuevo_proyecto() escribe la configuración
# comentada desde la tabla de claves y las plantillas; dl_revisar_proyecto() lista cada problema con su corrección;
# dl_correr() hace la corrida entera.

# La revisión sin lo que imprime (copia_ejemplo(), leer_texto() y escribir_texto(): helper-dismodlite.R).
revisar_callado <- function(...) { salida <- utils::capture.output(r <- dl_revisar_proyecto(...)); r }

# ---- dl_nuevo_proyecto() ----

test_that("dl_nuevo_proyecto() escribe la configuración comentada desde la tabla de claves y no sobrescribe", {
  d <- file.path(withr::local_tempdir(), "proyecto nuevo")
  expect_message(dl_nuevo_proyecto(d, causa = 501, nombre = "Enfermedad ficticia: tipo 1", anio = 2020,
                                   edad_inicio = 40),
                 "^dl_nuevo_proyecto\\(\\): proyecto de la causa 501")
  for (f in c("config.yaml", "LEEME.md", "poblacion.csv", "severidad.csv", "proxies.csv", "datos.csv"))
    expect_true(file.exists(file.path(d, f)), info = f)
  for (f in c("ancla", "covariables")) expect_true(dir.exists(file.path(d, f)), info = f)
  expect_identical(names(leer_texto(file.path(d, "poblacion.csv"))),
                   c("location_id", "location_name", "anio", "sexo", "edad_inicio", "edad_fin", "poblacion"))
  # sin comentar, solo lo dado; cada clave de la tabla aparece con su descripción, su símbolo y su valor por defecto
  y <- yaml::read_yaml(file.path(d, "config.yaml"))
  expect_identical(y, list(causa = 501L, nombre = "Enfermedad ficticia: tipo 1", anio = 2020L, edad_inicio = 40L))
  lineas <- readLines(file.path(d, "config.yaml"), encoding = "UTF-8")
  t <- .dl_claves_simple()
  for (i in seq_len(nrow(t))) {
    expect_true(any(startsWith(lineas, sprintf("# `%s`%s: ", t$clave[i],
                                               if (nzchar(t$simbolo[i])) sprintf(" (%s)", t$simbolo[i]) else ""))),
                info = t$clave[i])
  }
  expect_true("# remision: 0" %in% lineas)
  expect_true("#   peso: 1" %in% lineas)
  expect_true("#   peso: [0.25, 0.5, 1]" %in% lineas)
  # sin valor fijo por defecto, la línea comentada lleva un ejemplo (el de la tabla de claves)
  expect_true("#   - nombre: haqi" %in% lineas)
  # con `anio`, el ejemplo de los años que por defecto son `anio` es el anterior (ancla.anio admite anio - 1)
  expect_true("#   anio: 2019" %in% lineas)
  expect_true("#   anio_validacion: 2019" %in% lineas)
  expect_true("# nudos: [30, 40, 50, 60, 70, 80, 95]" %in% lineas)
  expect_true("# subtipos: [1011, 1012, 1013]" %in% lineas)
  expect_lte(max(nchar(lineas)), 120L)
  # el LEEME dice de dónde descargar el ancla y las covariables (las direcciones del contrato estimates/v1)
  leeme <- paste(readLines(file.path(d, "LEEME.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(leeme, .dl_schema_estimates()$sources$gbd$url, fixed = TRUE)
  expect_match(leeme, .dl_schema_estimates()$sources$ghdx$url, fixed = TRUE)
  # otra llamada no toca lo que ya existe
  writeLines(c("causa: 501", "anio: 2021", "edad_inicio: 40"), file.path(d, "config.yaml"))
  expect_message(dl_nuevo_proyecto(d, causa = 501), "ya existían \\(no se tocaron\\): LEEME.md, config.yaml")
  expect_identical(readLines(file.path(d, "config.yaml")), c("causa: 501", "anio: 2021", "edad_inicio: 40"))
  # otra causa en un proyecto de una causa: el mensaje dice cómo pasar a config/
  expect_error(dl_nuevo_proyecto(d, causa = 502), "config/501.yaml")
  dir.create(file.path(d, "config"))
  file.rename(file.path(d, "config.yaml"), file.path(d, "config", "501.yaml"))
  suppressMessages(dl_nuevo_proyecto(d, causa = 502, anio = 2020, edad_inicio = 40))
  expect_true(file.exists(file.path(d, "config", "502.yaml")))
  expect_identical(yaml::read_yaml(file.path(d, "config", "502.yaml"))$causa, 502L)
})

test_that("la revisión de un proyecto nuevo: la configuración espera al ancla y lo omitido no cuenta como aviso", {
  d <- file.path(withr::local_tempdir(), "vacio")
  suppressMessages(dl_nuevo_proyecto(d, causa = 501, anio = 2020, edad_inicio = 40))
  salida <- utils::capture.output(r <- dl_revisar_proyecto(d))
  expect_identical(r$estado[r$paso == "configuración"], "omitido")
  expect_match(r$detalle[r$paso == "configuración"], "espera a ancla/")
  expect_identical(r$estado[r$paso == "insumos"], "omitido")
  expect_identical(r$sugerencia[r$estado == "omitido"], c("", ""))
  expect_match(salida[length(salida)], "^[0-9]+ error\\(es\\) y 0 aviso\\(s\\)")
})

test_that("dl_nuevo_proyecto() valida sus argumentos; los valores de la configuración los revisa la revisión", {
  d <- file.path(withr::local_tempdir(), "p")
  expect_error(dl_nuevo_proyecto(), "^dl_nuevo_proyecto\\(\\): falta `carpeta`")
  expect_error(dl_nuevo_proyecto(d), "falta `causa`")
  expect_error(dl_nuevo_proyecto(3, causa = 1), "`carpeta` debe ser la ruta de una carpeta")
  expect_false(dir.exists(d))
  # nombre, anio y edad_inicio van a config.yaml tal como se dan: la revisión dice cuál no vale
  suppressMessages(dl_nuevo_proyecto(d, causa = 1, nombre = 3, anio = 2020.5))
  r <- revisar_callado(d)
  e <- r$detalle[r$paso == "configuración" & r$estado == "error"]
  expect_true(any(startsWith(e, "nombre: debe ser un texto")))
  expect_true(any(startsWith(e, "anio: debe ser un entero")))
})

# ---- De la plantilla a un proyecto revisado ----

test_that("de la plantilla al proyecto: llenada con los archivos del ejemplo, la revisión da todo en orden", {
  d <- file.path(withr::local_tempdir(), "de la plantilla")
  suppressMessages(dl_nuevo_proyecto(d, causa = 9100, nombre = "Arteriopatía crónica sintética",
                                     anio = 2023, edad_inicio = 30))
  # los datos subnacionales del ejemplo son de 2019: su año de validación, como en la configuración del ejemplo (sin
  # él, esas filas quedarían fuera con un aviso)
  lineas <- readLines(file.path(d, "config.yaml"), encoding = "UTF-8")
  lineas <- sub("^# subnacional:$", "subnacional:", sub("^#   anio_validacion: .*$", "  anio_validacion: 2019", lineas))
  writeLines(enc2utf8(lineas), file.path(d, "config.yaml"), useBytes = TRUE)
  # recién creado: faltan las descargas del ancla y las filas de la población
  r <- revisar_callado(d)
  expect_identical(r$estado[r$paso == "ancla/"], "error")
  expect_identical(r$estado[r$paso == "poblacion.csv"], "error")
  expect_match(r$detalle[r$paso == "poblacion.csv"], "solo tiene el encabezado")
  # con el ancla, la población y los datos del ejemplo; severidad.csv sigue con solo el encabezado: los insumos se
  # arman, pero dl_correr() no puede escribir la corrida (un error), y proxies.csv sin filas no se usa
  file.copy(dl_ejemplo("ancla", "sintetico_acs_v1.csv"), file.path(d, "ancla"))
  file.copy(dl_ejemplo("poblacion.csv"), d, overwrite = TRUE)
  file.copy(dl_ejemplo("datos.csv"), d, overwrite = TRUE)
  r <- revisar_callado(d)
  expect_identical(r$estado[r$paso == "severidad.csv"], "error")
  expect_match(r$detalle[r$paso == "severidad.csv"], "la causa 9100 no tiene estados de salud")
  expect_match(r$sugerencia[r$paso == "severidad.csv"], "^columnas causa, estado, proporcion")
  expect_identical(r$estado[r$paso == "insumos"], "ok")
  expect_false("proxies.csv" %in% r$paso)
  file.copy(dl_ejemplo("severidad.csv"), d, overwrite = TRUE)
  salida <- utils::capture.output(r <- dl_revisar_proyecto(d))
  expect_identical(unique(r$estado), "ok")
  expect_setequal(r$paso, c("configuración", "ancla/", "poblacion.csv", "pesos de 80+", "datos.csv", "severidad.csv",
                            "insumos"))
  # la línea en orden de un archivo dice las filas que trae; los pesos de 80+ no son un archivo
  expect_identical(r$detalle[r$paso == "datos.csv"],
                   sprintf("leído: %d fila(s)", nrow(leer_texto(dl_ejemplo("datos.csv")))))
  expect_identical(r$detalle[r$paso == "pesos de 80+"], "calculados desde poblacion.csv")
  expect_true(all(r$causa == 9100L))
  expect_identical(names(r), c("causa", "paso", "estado", "detalle", "sugerencia"))
  expect_identical(salida[length(salida)], "Todo en orden.")
  # quitar el «# » de las claves comentadas con un valor por defecto fijo (las de los bloques y las sueltas; no las
  # que llevan un ejemplo, ni las listas de registros) da la misma configuración: los valores por defecto de la
  # plantilla son los del paquete
  cfg <- dl_configuracion(9100, d)
  lineas <- readLines(file.path(d, "config.yaml"), encoding = "UTF-8")
  t <- .dl_claves_simple()
  con_ejemplo <- unique(sub("^.*[.]", "", t$clave[vapply(t$valor, is.null, NA) & t$defecto != "obligatoria"]))
  campo <- sub("^# *([a-z_]+):.*$", "\\1", lineas)
  clave <- grepl("^# ([a-z_]+:.*|  [a-z_]+:.*)$", lineas) & !campo %in% con_ejemplo &
    !lineas %in% sprintf("# %s:", unique(sub("\\[\\].*$", "", grep("[]]", t$clave, value = TRUE))))
  expect_gte(sum(clave), 14L)        # 10 claves con valor fijo y 4 bloques (subnacional ya va sin «# »)
  lineas[clave] <- substring(lineas[clave], 3L)
  writeLines(enc2utf8(lineas), file.path(d, "config.yaml"), useBytes = TRUE)
  cfg_todas <- dl_configuracion(9100, d)
  sin_origen <- function(x) x[setdiff(names(x), "origen")]
  expect_identical(sin_origen(cfg_todas), sin_origen(cfg))
})

# ---- dl_revisar_proyecto() ----

test_that("dl_revisar_proyecto() lista cada problema de una copia rota, cada uno en su archivo y con su corrección", {
  d <- copia_ejemplo()
  # 9101: una clave mal escrita
  cfg <- readLines(file.path(d, "config", "9101.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "ancla:", "  pesos: 0.5")), file.path(d, "config", "9101.yaml"), useBytes = TRUE)
  # poblacion.csv sin la columna sexo
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  escribir_texto(pob[, sexo := NULL], file.path(d, "poblacion.csv"))
  # datos.csv con un tipo que no existe
  datos <- leer_texto(file.path(d, "datos.csv"))
  datos$tipo[3] <- "muertes"
  escribir_texto(datos, file.path(d, "datos.csv"))
  # una descarga del ancla guardada desde Excel con «;»
  writeLines(c("measure_id;location_id;val", "5;123;0,01"), file.path(d, "ancla", "de_excel.csv"))

  salida <- utils::capture.output(r <- dl_revisar_proyecto(d))
  expect_setequal(unique(r$causa), 9100:9103)
  error <- function(causa, paso) r[r$causa == causa & r$paso == paso & r$estado == "error", ]
  expect_match(error(9101, "configuración")$detalle, "ancla.pesos: clave desconocida; ¿quisiste decir")
  expect_identical(error(9101, "configuración")$sugerencia, "ver ?dl_configuracion")
  for (causa in c(9100, 9102)) {
    expect_match(error(causa, "poblacion.csv")$detalle, "le falta\\(n\\) la\\(s\\) columna\\(s\\) sexo")
    expect_match(error(causa, "datos.csv")$detalle, "tiene: muertes")
    expect_match(error(causa, "ancla/")$detalle, "de_excel.csv")
    expect_match(error(causa, "ancla/")$detalle, "separador de columnas")
    expect_identical(r$estado[r$causa == causa & r$paso == "insumos"], "omitido")   # no cuenta como aviso
  }
  expect_true(all(nzchar(r$sugerencia[r$estado == "error"])))
  expect_match(salida[length(salida)], "error\\(es\\) y [0-9]+ aviso\\(s\\): corrige los errores")
  expect_true(any(grepl("causa 9101", salida)))
})

test_that("dl_revisar_proyecto() pone el cierre de los proxies en su archivo y avisa de la mortalidad por 100 000", {
  d <- copia_ejemplo()
  # proxies que no cierran en el valor nacional
  px <- leer_texto(file.path(d, "proxies.csv"))
  k <- which(px$covariable == "LDI_pc" & px$anio == "2023")[1]
  px$valor[k] <- as.character(as.numeric(px$valor[k]) * 1.5)
  escribir_texto(px, file.path(d, "proxies.csv"))
  # un valor de mortalidad por 100 000 y datos en el ajuste con ancla.peso 1 (doble conteo)
  datos <- leer_texto(file.path(d, "datos.csv"))
  m <- which(datos$tipo == "mortalidad" & datos$location_id == "123")[2]
  datos$valor[m] <- as.character(as.numeric(datos$valor[m]) * 1e5)
  escribir_texto(datos, file.path(d, "datos.csv"))
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]")),
             file.path(d, "config", "9100.yaml"), useBytes = TRUE)

  r <- revisar_callado(d, causa = 9100)
  cierre <- r[r$paso == "proxies.csv" & r$estado == "error", ]
  expect_identical(nrow(cierre), 1L)
  expect_match(cierre$detalle, "^el promedio ponderado por población .*\\(covariable LDI_pc, ")   # sin su archivo
  expect_match(cierre$sugerencia, "^columnas covariable, location_id, .*\\(ver \\?dl_proyecto\\)")
  unidades <- r[r$paso == "datos.csv" & r$estado == "aviso", ]
  expect_identical(nrow(unidades), 1L)
  expect_match(unidades$detalle, sprintf("1 valor\\(es\\) de mortalidad parecen tasas por 100 000.*\\(fila_%d;", m))
  expect_match(unidades$detalle, "divide valor y error_estandar por 100 000")
  # el aviso del doble conteo sale una sola vez: lo da dl_insumos()
  expect_identical(sum(grepl("ancla.peso: 0.5", r$detalle)), 1L)
  expect_identical(r$sugerencia[grepl("ancla.peso: 0.5", r$detalle)], "ver ?dl_configuracion")   # nombra una clave
  # el proyecto sin cambios, en orden: cada archivo con todas sus filas (también las de otras causas y años)
  r <- revisar_callado(dl_ejemplo(), causa = 9100)
  expect_identical(unique(r$estado), "ok")
  expect_identical(r$detalle[r$paso == "proxies.csv"],
                   sprintf("leído: %d fila(s)", nrow(leer_texto(dl_ejemplo("proxies.csv")))))
  expect_identical(r$detalle[r$paso == "ancla/"],
                   sprintf("leído: %d fila(s)", nrow(leer_texto(dl_ejemplo("ancla", "sintetico_acs_v1.csv")))))
})

test_that("dl_revisar_proyecto() avisa de las filas de datos.csv que quedan fuera y de un datos_en_ajuste sin ellas", {
  d <- copia_ejemplo()
  datos <- leer_texto(file.path(d, "datos.csv"))
  sub <- which(datos$location_id != "123")        # las subnacionales (las nacionales no entran: sin datos_en_ajuste)
  datos$anio[sub[1:2]] <- "2020"                                   # de otro año
  datos$sexo[sub[3]] <- "ambos"                                    # de ambos sexos
  escribir_texto(datos, file.path(d, "datos.csv"))
  r <- revisar_callado(d, causa = 9100)
  aviso <- r[r$estado == "aviso", ]
  expect_identical(nrow(aviso), 1L)
  expect_identical(aviso$paso, "insumos")
  expect_match(aviso$detalle, paste0("^3 fila\\(s\\) de datos.csv de la causa 9100 quedan fuera: 2 de otro año ",
                                     "\\(las nacionales deben incluir 2023 y las subnacionales 2019\\); 1 de ambos sexos"))
  # datos_en_ajuste sin ninguna fila nacional que entre: lo dice, en lugar del aviso del doble conteo (ancla.peso 1)
  escribir_texto(leer_texto(dl_ejemplo("datos.csv"))[location_id != "123"], file.path(d, "datos.csv"))
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "datos_en_ajuste: [incidencia]")), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  r <- revisar_callado(d, causa = 9100)
  aviso <- r[r$estado == "aviso", ]
  expect_identical(nrow(aviso), 1L)
  expect_match(aviso$detalle, paste0("^datos_en_ajuste declara incidencia, pero ninguna fila nacional de datos.csv ",
                                     "del año 2023 entra al ajuste \\(sin excluir\\): el ajuste usa solo el ancla$"))
  expect_identical(aviso$sugerencia, "ver ?dl_configuracion")
})

test_that("dl_revisar_proyecto(): formato completo, carpeta sin configuración o causa que no está", {
  r <- revisar_callado(ejemplo_completo(), causa = 9101)
  expect_identical(r$paso, c("configuración", "insumos"))
  expect_identical(unique(r$estado), "ok")
  expect_match(r$detalle[1], "config/9101.yaml: formato completo")
  d <- withr::local_tempdir()
  r <- revisar_callado(d)
  expect_identical(r$estado, "error")
  expect_match(r$detalle, "no es la carpeta de un proyecto.*dl_nuevo_proyecto\\(\\)")
  r <- revisar_callado(dl_ejemplo(), causa = 1)
  expect_match(r$detalle, "no tiene la configuración de la causa 1 \\(tiene: 9100, 9101, 9102, 9103\\)")
  expect_error(dl_revisar_proyecto(file.path(d, "no existe")), "^dl_revisar_proyecto\\(\\): `carpeta` debe ser")
})

test_that("un config.yaml que no es una lista de claves es un error del paquete, también en la revisión", {
  # una línea sin «:» se lee como un texto suelto; una lista con guiones, como una lista sin claves
  d <- withr::local_tempdir()
  detalle <- "config.yaml no es una lista de claves (clave: valor, una por línea)"
  for (texto in list("causa 4711", c("causa 4711", "anio 2023"), c("- causa: 4711", "- anio: 2023"))) {
    writeLines(texto, file.path(d, "config.yaml"))
    expect_error(dl_proyecto(d), paste0("^dl_proyecto\\(\\): ", gsub("([()])", "\\\\\\1", detalle), "$"),
                 class = "dl_error")
    r <- revisar_callado(d)
    expect_identical(r[c("paso", "estado", "detalle")],
                     data.frame(paso = "configuración", estado = "error", detalle = detalle))
    expect_identical(revisar_callado(d, causa = 4711)$detalle, detalle)
    expect_error(dl_proyecto(d, causa = 4711), detalle, fixed = TRUE, class = "dl_error")
  }
  # una causa que no es un valor simple (una lista de claves) no se toma como la causa
  writeLines(c("causa: {a: [1, 2]}", "anio: 2023"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "config.yaml no declara `causa`", class = "dl_error")
})

# ---- dl_correr() ----

test_that("dl_correr(rapido = TRUE) sobre el ejemplo simple escribe la carpeta de la corrida", {
  d <- copia_ejemplo()
  # rejilla de sensibilidad de una sola combinación, para que la prueba sea corta
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  cfg <- sub("  peso: [0.1, 0.5, 1.0]", "  peso: [1]\n  correlacion_edad: [0.5]\n  kappa: [1]", cfg, fixed = TRUE)
  writeLines(enc2utf8(cfg), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  reg <- file.path(d, "registro de corridas.yaml"); writeLines("datasets: []", reg)
  mensajes <- character()
  run <- withCallingHandlers(dl_correr(d, causa = 9100, semilla = 3, rapido = TRUE, registro = reg),
                             message = function(m) {
                               mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
                             })
  expect_s3_class(run, "dl_run")
  expect_match(mensajes[1], "^dl_correr\\(\\): corrida de prueba \\(rapido = TRUE\\).*no sirven para publicar")
  expect_true(startsWith(normalizePath(run$dir), normalizePath(file.path(d, "resultados", "mod", "dismod_lite"))))
  expect_match(run$run_id, "_causa-9100-prueba_v1$")
  expect_identical(vapply(yaml::read_yaml(reg)$datasets, function(x) x$run_id, ""), run$run_id)   # registrada
  man <- run$manifest
  expect_true(man$validacion$gates$force)
  expect_identical(man$params$chains, 2L)
  expect_identical(man$params$iter, 2000L)
  expect_identical(man$params$seed, 3L)
  expect_false(is.null(man$cascada))                             # el ejemplo trae proxies: hay cascada
  expect_identical(man$configuracion$formato, "simple")
  for (f in c("manifest.yaml", "diagnostics/sensibilidad.csv", "diagnostics/validacion.csv",
              file.path("etiquetas", paste0(run$run_id, ".csv"))))
    expect_true(file.exists(file.path(run$dir, f)), info = f)
  expect_setequal(list.files(file.path(run$dir, "cause")), c("prevalence", "incidence", "yld"))
  # los pasos siguen disponibles por separado: el ajuste de la corrida está en la cache de la sesión
  b <- suppressMessages(dl_insumos(dl_proyecto(d, 9100)))
  f <- dl_ajustar(b, do.call(dl_opciones_mcmc, .DL_OPCIONES_PRUEBA), semilla = 3)
  expect_identical(round(max(f$mcmc$rhat), 6), man$validacion$gates$rhat_max)
  # un máximo del error del ancla que el ajuste no cumple: la corrida no se escribe, tampoco con rapido = TRUE, y el
  # mensaje dice qué clave declarar (en `avanzado`, en el formato simple). La configuración cambió: los insumos son
  # otros y el ajuste se vuelve a muestrear; el error llega justo después de la validación, antes de los AVD
  cfg <- c(cfg, "avanzado:", "  anchor:", "    gate_err_mediano: {valor: 0.0001, procedencia: prueba}")
  writeLines(enc2utf8(cfg), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  e <- expect_error(suppressMessages(dl_correr(d, causa = 9100, semilla = 3, rapido = TRUE, sensibilidad = FALSE)),
                    class = "dl_error")
  expect_match(conditionMessage(e), paste0("se aleja del ancla \\(error relativo mediano .*avanzado: \\{anchor: ",
                                           "\\{gate_err_mediano: \\{valor: \\.\\.\\., procedencia: \\.\\.\\.\\}\\}\\} ",
                                           "\\(ver \\?dl_configuracion\\); rapido = TRUE no la salta"))
  # sin los nombres internos de las comprobaciones ni `forzar`, que dl_correr() no tiene
  expect_no_match(paste(c(conditionMessage(e), mensajes), collapse = "\n"), "anchor_identity|amplitud_csmr")
  expect_no_match(conditionMessage(e), "forzar")
})

test_that("dl_correr() se detiene antes de ajustar sin semilla, sin severidad o sin dónde escribir", {
  expect_error(dl_correr(dl_ejemplo(), 9100), "^dl_correr\\(\\): falta `semilla`")
  expect_error(dl_correr(dl_ejemplo(), 9100, semilla = 1),
               "dentro de la instalación del paquete .*`carpeta_salida`")
  expect_error(dl_correr(dl_ejemplo(), 9100, semilla = 1, rapido = "si"), "`rapido` debe ser TRUE o FALSE")
  expect_error(dl_correr(3, semilla = 1), "`proyecto` debe ser la carpeta de un proyecto")
  expect_error(dl_correr(dl_ejemplo(), 9100, semilla = 1, carpeta_salida = tempdir(), registro = "no_existe.yaml"),
               "no existe el archivo del registro de corridas \\(`registro`\\)")
  expect_error(dl_correr(dl_proyecto(dl_ejemplo(), 9100), causa = 9101, semilla = 1, carpeta_salida = tempdir()),
               "`causa` es 9101 y el proyecto es de la causa 9100")
  d <- copia_ejemplo()
  unlink(file.path(d, "severidad.csv"))
  expect_error(suppressMessages(dl_correr(d, 9100, semilla = 1, rapido = TRUE)),
               "la causa 9100 no tiene estados de salud en la tabla severidad.csv")
  expect_false(dir.exists(file.path(d, "resultados")))
})

test_that("dl_correr() comprueba la convergencia justo después del ajuste nacional, antes de seguir", {
  d <- copia_ejemplo()
  o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
  mensajes <- character()
  expect_error(withCallingHandlers(dl_correr(d, 9101, semilla = 1, opciones = o), message = function(m) {
    mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
  }), "^dl_correr\\(\\): las cadenas no convergieron")
  expect_match(mensajes[length(mensajes)], "ajuste nacional")
  expect_false(dir.exists(file.path(d, "resultados")))
})

test_that("dl_revisar_proyecto() ubica en su archivo lo que antes llegaba con palabras del formato completo", {
  d <- copia_ejemplo()
  # el ancla sin muertes (con el prior de la mortalidad en exceso por defecto) y un poblacion.csv de Excel con «;»
  a <- leer_texto(file.path(d, "ancla", "sintetico_acs_v1.csv"))
  escribir_texto(a[measure_id != "1"], file.path(d, "ancla", "sintetico_acs_v1.csv"))
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  data.table::fwrite(pob, file.path(d, "poblacion.csv"), sep = ";")
  r <- revisar_callado(d, causa = 9101)
  error <- function(paso) r[r$paso == paso & r$estado == "error", ]
  expect_identical(r$estado[r$paso == "configuración"], "ok")
  expect_match(error("ancla/")$detalle, "no trae la mortalidad")
  expect_match(error("ancla/")$detalle, "mortalidad_exceso: \\{prior: plano, techo: \\.\\.\\.\\}")
  expect_match(error("poblacion.csv")$detalle, "separador de columnas")
  expect_match(error("poblacion.csv")$sugerencia, "columnas location_id, location_name, anio, sexo")
  expect_identical(r$estado[r$paso == "severidad.csv"], "ok")          # los demás archivos se revisan igual
  expect_identical(r$estado[r$paso == "insumos"], "omitido")

  # proxies de otro año y datos imposibles: cada problema en su archivo
  d <- copia_ejemplo()
  px <- leer_texto(file.path(d, "proxies.csv"))
  escribir_texto(px[anio != "2023"], file.path(d, "proxies.csv"))
  r <- revisar_callado(d, causa = 9101)
  expect_match(r$detalle[r$paso == "proxies.csv" & r$estado == "error"], "no trae filas de 2023")
  d <- copia_ejemplo()
  datos <- leer_texto(file.path(d, "datos.csv"))
  k <- which(datos$tipo == "prevalencia_estudio" & datos$location_id == "123" & datos$anio == "2023" &
               datos$excluir == "FALSE")[1]
  datos$valor[k] <- "2"
  escribir_texto(datos, file.path(d, "datos.csv"))
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  cfg <- c(cfg, "ancla:", "  peso: 0.5", "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]")
  writeLines(enc2utf8(cfg), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  r <- revisar_callado(d, causa = 9100)
  e <- r[r$paso == "datos.csv" & r$estado == "error", ]
  expect_match(e$detalle, sprintf("prevalencia fuera de \\[0, 1\\].*fila_%d", k))
  expect_match(e$sugerencia, "^columnas causa, tipo, location_id")
})

test_that("dl_revisar_proyecto(): covariables/ sin age_group_id ni el intervalo es un error de su paso", {
  # la estimación subnacional usa el intervalo del valor nacional (lower_value, upper_value): sin esas columnas, en
  # una descarga o en todas, la revisión lo dice en covariables/ y sigue con los demás archivos
  for (todas in c(FALSE, TRUE)) {
    d <- copia_ejemplo()
    archivos <- sort(list.files(file.path(d, "covariables"), full.names = TRUE))
    for (f in if (todas) archivos else archivos[1]) {
      cv <- leer_texto(f)
      escribir_texto(cv[, c("covariate_name_short", "location_id", "year_id", "sex_id", "mean_value")], f)
    }
    r <- revisar_callado(d, causa = 9100)
    e <- r[r$paso == "covariables/" & r$estado == "error", ]
    expect_identical(nrow(e), 1L, info = todas)
    expect_match(e$detalle, "le falta\\(n\\) la\\(s\\) columna\\(s\\) age_group_id, lower_value, upper_value")
    expect_identical(r$estado[r$paso == "insumos"], "omitido")
    expect_identical(r$estado[r$paso == "severidad.csv"], "ok")
  }
})
