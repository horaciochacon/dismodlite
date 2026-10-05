# Herramientas de la carpeta de un proyecto (R/herramientas.R): dl_nuevo_proyecto() escribe la configuración
# comentada desde la tabla de claves y las plantillas de las tablas del contrato; dl_revisar_proyecto() lista cada
# problema con su corrección, tabla por tabla y con las reglas entre tablas (R/contrato.R); dl_correr() hace la corrida
# entera.

# La revisión sin lo que imprime (copia_ejemplo(), leer_texto() y escribir_texto(): helper-dismodlite.R).
revisar_callado <- function(...) { salida <- utils::capture.output(r <- dl_revisar_proyecto(...)); r }

# ---- dl_nuevo_proyecto() ----

test_that("dl_nuevo_proyecto() escribe la configuración comentada y las plantillas, y no sobrescribe", {
  d <- file.path(withr::local_tempdir(), "proyecto nuevo")
  expect_message(dl_nuevo_proyecto(d, causa = 501, nombre = "Enfermedad ficticia: tipo 1", anio = 2020,
                                   edad_inicio = 40),
                 "^dl_nuevo_proyecto\\(\\): proyecto de la causa 501")
  for (f in c("config.yaml", "LEEME.md", "ubicaciones.csv", "poblacion.csv", "betas.csv", "datos.csv",
              "severidad.csv", "poblacion_detalle.csv", "proxies_crudos.csv", "razones.csv"))
    expect_true(file.exists(file.path(d, f)), info = f)
  for (f in c("ancla", "covariables", "fuentes_gbd")) expect_true(dir.exists(file.path(d, f)), info = f)
  # las plantillas: solo el encabezado, las columnas de dl_plantilla()
  for (t in c("ubicaciones", "betas", "severidad", "proxies_crudos", "razones"))
    expect_identical(readLines(file.path(d, paste0(t, ".csv"))), paste(names(dl_plantilla(t)), collapse = ","))
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
  expect_false(any(grepl("^(# )?covariables:", lineas)))        # las covariables van en la tabla betas
  expect_true("# remision: 0" %in% lineas)
  expect_true("#   peso: 1" %in% lineas)
  expect_true("#   peso: [0.25, 0.5, 1]" %in% lineas)
  # sin valor fijo por defecto, la línea comentada lleva un ejemplo (el de la tabla de claves)
  expect_true("#   particion: particion/mi_corrida" %in% lineas)
  expect_true("#   secuelas: [5001, 5002]" %in% lineas)
  # con `anio`, el ejemplo de los años que por defecto son `anio` es el anterior (ancla.anio admite anio - 1)
  expect_true("#   anio: 2019" %in% lineas)
  expect_true("#   anio_validacion: 2019" %in% lineas)
  expect_true("# nudos: [30, 40, 50, 60, 70, 80, 95]" %in% lineas)
  expect_true("# subtipos: [1011, 1012, 1013]" %in% lineas)
  # las claves de una causa que es un subtipo, de la fase aguda y del error del ancla, comentadas con su ejemplo
  expect_true("# subtipo_de: 1010" %in% lineas)
  # las de una causa que es la suma de sus subtipos: la clave y la lista de los omitidos, como registros
  expect_true("# suma_de_subtipos: sí" %in% lineas)
  k <- match("# subtipos_omitidos:", lineas)
  expect_identical(lineas[k + 1:2], c("#   - causa: 1013", "#     motivo: sin datos suficientes"))
  expect_true("#   error_maximo: 0.08" %in% lineas)
  expect_true("#   fraccion_aguda: 0.3" %in% lineas)
  expect_true("#   fraccion_aguda: [0, 0.3, 0.5]" %in% lineas)
  expect_lte(max(nchar(lineas)), 120L)
  # el LEEME: la carpeta del proyecto, ?dl_tablas y de dónde descargar el ancla y las covariables (las direcciones
  # del contrato estimates/v1)
  leeme <- paste(readLines(file.path(d, "LEEME.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(leeme, "?dl_tablas", fixed = TRUE)
  for (f in c("ubicaciones.csv", "poblacion_detalle.csv", "proxies_crudos.csv", "razones.csv", "fuentes_gbd/"))
    expect_match(leeme, f, fixed = TRUE)
  expect_match(leeme, .dl_schema_estimates()$sources$gbd$url, fixed = TRUE)
  expect_match(leeme, .dl_schema_estimates()$sources$ghdx$url, fixed = TRUE)
  # el paso del reparto por razón: cuándo se llena razones.csv
  expect_match(leeme, "Si repartes por razón (`subnacional: {modo: razon}`", fixed = TRUE)
  # otra llamada no toca lo que ya existe
  writeLines(c("causa: 501", "anio: 2021", "edad_inicio: 40"), file.path(d, "config.yaml"))
  expect_message(dl_nuevo_proyecto(d, causa = 501), "archivos nuevos: ninguno\n.*ya existían \\(no se tocaron\\): LEEME.md")
  expect_identical(readLines(file.path(d, "config.yaml")), c("causa: 501", "anio: 2021", "edad_inicio: 40"))
  # otra causa en un proyecto de una causa: el mensaje dice cómo pasar a config/
  expect_error(dl_nuevo_proyecto(d, causa = 502), "config/501.yaml")
  dir.create(file.path(d, "config"))
  file.rename(file.path(d, "config.yaml"), file.path(d, "config", "501.yaml"))
  suppressMessages(dl_nuevo_proyecto(d, causa = 502, anio = 2020, edad_inicio = 40))
  expect_true(file.exists(file.path(d, "config", "502.yaml")))
  expect_identical(yaml::read_yaml(file.path(d, "config", "502.yaml"))$causa, 502L)
})

test_that("la revisión de un proyecto nuevo: faltan las tablas obligatorias y lo demás espera, sin contar como aviso", {
  d <- file.path(withr::local_tempdir(), "vacio")
  suppressMessages(dl_nuevo_proyecto(d, causa = 501, anio = 2020, edad_inicio = 40))
  salida <- utils::capture.output(r <- dl_revisar_proyecto(d))
  expect_identical(r$paso[r$estado == "error"], c("ubicaciones", "poblacion", "ancla"))
  expect_match(r$detalle[r$paso == "poblacion"], "falta la tabla \\(o solo trae el encabezado\\): va en poblacion.csv")
  expect_match(r$sugerencia[r$paso == "ubicaciones"], "^columnas ubicacion, nombre, padre \\(ver \\?dl_tablas\\)")
  # las opcionales con solo el encabezado no aparecen: no existen
  expect_false(any(c("betas", "datos", "severidad", "poblacion_detalle", "proxies_crudos", "proxies") %in% r$paso))
  expect_identical(r$estado[r$paso == "configuración"], "omitido")
  expect_match(r$detalle[r$paso == "configuración"], "espera a la tabla ubicaciones")
  expect_identical(r$estado[r$paso %in% c("proyecto", "insumos")], c("omitido", "omitido"))
  expect_true(all(r$sugerencia[r$estado == "omitido"] == ""))
  expect_identical(salida[length(salida)], "3 error(es) y 0 aviso(s): corrige los errores y vuelve a revisar.")
})

test_that("dl_nuevo_proyecto() valida sus argumentos; los valores de la configuración los revisa la revisión", {
  d <- file.path(withr::local_tempdir(), "p")
  expect_error(dl_nuevo_proyecto(), "^dl_nuevo_proyecto\\(\\): falta `carpeta`")
  expect_error(dl_nuevo_proyecto(d), "falta `causa`")
  expect_error(dl_nuevo_proyecto(3, causa = 1), "`carpeta` debe ser la ruta de una carpeta")
  expect_false(dir.exists(d))
  # nombre, anio y edad_inicio van a config.yaml tal como se dan: la revisión dice cuál no vale, aunque falten tablas
  suppressMessages(dl_nuevo_proyecto(d, causa = 1, nombre = 3, anio = 2020.5))
  r <- revisar_callado(d)
  e <- r$detalle[r$paso == "configuración" & r$estado == "error"]
  expect_true(any(startsWith(e, "nombre: debe ser un texto")))
  expect_true(any(startsWith(e, "anio: debe ser un entero")))
  expect_true(any(startsWith(e, "edad_inicio: falta (obligatoria)")))
})

# ---- De la plantilla a un proyecto revisado ----

test_that("de la plantilla al proyecto: llenada con las tablas del ejemplo, la revisión da todo en orden", {
  d <- file.path(withr::local_tempdir(), "de la plantilla")
  suppressMessages(dl_nuevo_proyecto(d, causa = 9100, nombre = "Arteriopatía crónica sintética",
                                     anio = 2023, edad_inicio = 30))
  # los datos subnacionales del ejemplo son de 2019: su año de validación, como en la configuración del ejemplo (sin
  # él, esas filas quedarían fuera con un aviso)
  lineas <- readLines(file.path(d, "config.yaml"), encoding = "UTF-8")
  lineas <- sub("^# subnacional:$", "subnacional:", sub("^#   anio_validacion: .*$", "  anio_validacion: 2019", lineas))
  writeLines(enc2utf8(lineas), file.path(d, "config.yaml"), useBytes = TRUE)
  # con las tablas obligatorias y los datos del ejemplo; severidad.csv sigue con solo el encabezado: los insumos se
  # arman, pero dl_correr() no puede escribir la corrida (un error), y betas.csv sin filas no se usa
  file.copy(dl_ejemplo("ancla", "sintetico_acs_v1.csv"), file.path(d, "ancla"))
  for (f in c("ubicaciones.csv", "poblacion.csv", "datos.csv")) file.copy(dl_ejemplo(f), d, overwrite = TRUE)
  r <- revisar_callado(d)
  expect_identical(r$estado[r$paso == "severidad"], "error")
  expect_match(r$detalle[r$paso == "severidad"], "la causa 9100 no tiene estados de salud en la tabla severidad:")
  expect_match(r$sugerencia[r$paso == "severidad"], "^columnas causa, estado, proporcion")
  expect_identical(r$estado[r$paso == "insumos"], "ok")
  expect_false("betas" %in% r$paso)
  file.copy(dl_ejemplo("severidad.csv"), d, overwrite = TRUE)
  salida <- utils::capture.output(r <- dl_revisar_proyecto(d))
  expect_identical(unique(r$estado), "ok")
  expect_identical(r$paso, c("ubicaciones", "poblacion", "ancla", "datos", "severidad", "configuración", "proyecto",
                             "insumos"))
  # la línea en orden de una tabla dice las filas que trae y, si pasó por un lector, cuál
  expect_identical(r$detalle[r$paso == "datos"], sprintf("leída: %d fila(s)", nrow(leer_texto(dl_ejemplo("datos.csv")))))
  expect_identical(r$detalle[r$paso == "ancla"],
                   sprintf("leída: %d fila(s) (descarga de GBD Results)", nrow(dl_tabla("ancla", dl_ejemplo("ancla")))))
  expect_identical(r$detalle[r$paso == "proyecto"], "las reglas entre tablas se cumplen")
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
  # declarar el máximo del error del ancla con su valor por defecto solo agrega su procedencia
  lineas <- sub("^#   error_maximo: 0.08$", "  error_maximo: 0.05", lineas)
  writeLines(enc2utf8(lineas), file.path(d, "config.yaml"), useBytes = TRUE)
  cfg_gate <- dl_configuracion(9100, d)
  expect_identical(cfg_gate$anchor$gate_err_mediano,
                   list(valor = 0.05, procedencia = "declarado en la configuración del proyecto"))
  cfg_gate$anchor$gate_err_mediano <- cfg_todas$anchor$gate_err_mediano
  expect_identical(sin_origen(cfg_gate), sin_origen(cfg_todas))
})

# ---- dl_revisar_proyecto() ----

test_that("dl_revisar_proyecto() lista cada problema de una copia rota, cada uno en su tabla y con su corrección", {
  d <- escribir_proxies_calibrados(copia_ejemplo())     # sin proxies_crudos: su calibración esperaría a la población
  # 9101: una clave mal escrita
  cfg <- readLines(file.path(d, "config", "9101.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "ancla:", "  pesos: 0.5")), file.path(d, "config", "9101.yaml"), useBytes = TRUE)
  # poblacion.csv sin la columna sexo
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  escribir_texto(pob[, sexo := NULL], file.path(d, "poblacion.csv"))
  # datos.csv con una medida que no existe
  datos <- leer_texto(file.path(d, "datos.csv"))
  datos$medida[3] <- "muertes"
  escribir_texto(datos, file.path(d, "datos.csv"))
  # una descarga del ancla guardada desde Excel con «;»
  writeLines(c("measure_id;location_id;val", "5;123;0,01"), file.path(d, "ancla", "de_excel.csv"))

  salida <- utils::capture.output(r <- dl_revisar_proyecto(d))
  expect_setequal(unique(r$causa), 9100:9103)
  error <- function(causa, paso) r[r$causa == causa & r$paso == paso & r$estado == "error", ]
  expect_match(error(9101, "configuración")$detalle, "ancla.pesos: clave desconocida; ¿quisiste decir")
  expect_identical(error(9101, "configuración")$sugerencia, "ver ?dl_configuracion")
  for (causa in c(9100, 9102)) {
    expect_match(error(causa, "poblacion")$detalle, "faltan las columnas sexo")
    expect_match(error(causa, "datos")$detalle, "medida: muertes no es uno de .*\\(fila\\(s\\) 4\\)")
    expect_match(error(causa, "ancla")$detalle, "de_excel.csv")
    expect_match(error(causa, "ancla")$detalle, "separador de columnas")
    expect_identical(r$estado[r$causa == causa & r$paso == "configuración"], "ok")   # las demás tablas se leyeron
    expect_identical(r$estado[r$causa == causa & r$paso %in% c("proyecto", "insumos")], c("omitido", "omitido"))
  }
  expect_true(all(nzchar(r$sugerencia[r$estado == "error"])))
  expect_match(salida[length(salida)], "error\\(es\\) y [0-9]+ aviso\\(s\\): corrige los errores")
  expect_true(any(grepl("causa 9101", salida)))
})

test_that("dl_revisar_proyecto() pone el cierre de los proxies en su tabla y avisa de la mortalidad por 100 000", {
  d <- escribir_proxies_calibrados(copia_ejemplo())
  # proxies que no cierran en el valor nacional
  px <- leer_texto(file.path(d, "covariables", "proxies.csv"))
  k <- which(px$covariable == "LDI_pc" & px$anio == "2023")[1]
  px$valor[k] <- as.character(as.numeric(px$valor[k]) * 1.5)
  escribir_texto(px, file.path(d, "covariables", "proxies.csv"))
  # un valor de mortalidad por 100 000 y datos en el ajuste con ancla.peso 1 (doble conteo)
  datos <- leer_texto(file.path(d, "datos.csv"))
  m <- which(datos$medida == "mortalidad" & datos$ubicacion == "123")[2]
  datos$valor[m] <- as.character(as.numeric(datos$valor[m]) * 1e5)
  escribir_texto(datos, file.path(d, "datos.csv"))
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]")),
             file.path(d, "config", "9100.yaml"), useBytes = TRUE)

  r <- revisar_callado(d, causa = 9100)
  cierre <- r[r$paso == "covariables" & r$estado == "error", ]
  expect_identical(nrow(cierre), 1L)
  expect_match(cierre$detalle, "^el promedio ponderado por población .*\\(covariable LDI_pc, ")   # sin su tabla
  expect_match(cierre$sugerencia, "^columnas ubicacion, anio, covariable, valor, .*\\(ver \\?dl_tablas\\)")
  unidades <- r[r$paso == "proyecto" & r$estado == "aviso", ]
  expect_identical(nrow(unidades), 1L)
  expect_match(unidades$detalle, sprintf(paste0("^datos: 1 valor\\(es\\) de mortalidad parecen tasas por 100 000.*",
                                                "\\(fila\\(s\\) %d;"), m + 1L))
  expect_match(unidades$detalle, "divide valor y error_estandar por 100 000")
  expect_match(unidades$sugerencia, "^columnas causa, ubicacion, anio")              # las de la tabla datos
  # el aviso del doble conteo sale una sola vez: lo da dl_insumos()
  expect_identical(sum(grepl("ancla.peso = 1", r$detalle)), 1L)
  expect_identical(r$sugerencia[grepl("ancla.peso = 1", r$detalle)], "ver ?dl_configuracion")   # nombra una clave
  # el proyecto sin cambios, en orden: cada tabla con todas sus filas (también las de otras causas y años)
  r <- revisar_callado(dl_ejemplo(), causa = 9100)
  expect_identical(unique(r$estado), "ok")
  expect_identical(r$detalle[r$paso == "covariables"],
                   sprintf("leída: %d fila(s) (descarga de covariables del GHDx)",
                           nrow(dl_proyecto(dl_ejemplo(), 9100)$tablas$covariables)))
})

test_that("dl_revisar_proyecto() avisa de las filas de datos que quedan fuera y de un datos_en_ajuste sin ellas", {
  d <- copia_ejemplo()
  datos <- leer_texto(file.path(d, "datos.csv"))
  sub <- which(datos$ubicacion != "123")        # las subnacionales (las nacionales no entran: sin datos_en_ajuste)
  datos$anio[sub[1:2]] <- "2020"                                   # de otro año
  datos$sexo[sub[3]] <- "ambos"                                    # de ambos sexos
  escribir_texto(datos, file.path(d, "datos.csv"))
  r <- revisar_callado(d, causa = 9100)
  aviso <- r[r$estado == "aviso", ]
  expect_identical(nrow(aviso), 1L)
  expect_identical(aviso$paso, "insumos")
  expect_match(aviso$detalle, paste0("^3 fila\\(s\\) de la tabla datos de la causa 9100 quedan fuera: 2 de otro año ",
                                     "\\(las nacionales deben incluir 2023 y las subnacionales 2019\\); 1 de ambos sexos"))
  # datos_en_ajuste sin ninguna fila nacional que entre: lo dice, en lugar del aviso del doble conteo (ancla.peso 1)
  escribir_texto(leer_texto(dl_ejemplo("datos.csv"))[ubicacion != "123"], file.path(d, "datos.csv"))
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "datos_en_ajuste: [incidencia]")), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  r <- revisar_callado(d, causa = 9100)
  aviso <- r[r$estado == "aviso", ]
  expect_identical(nrow(aviso), 1L)
  expect_match(aviso$detalle, paste0("^datos_en_ajuste declara incidencia, pero ninguna fila nacional de la tabla ",
                                     "datos del año 2023 entra al ajuste \\(sin excluir\\): el ajuste usa solo el ancla$"))
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
  # mensaje dice qué clave declarar (ancla.error_maximo, en el formato simple). La configuración cambió: los insumos
  # son otros y el ajuste se vuelve a muestrear; el error llega justo después de la validación, antes de los AVD
  cfg <- c(cfg, "ancla:", "  error_maximo: 0.0001")
  writeLines(enc2utf8(cfg), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  e <- expect_error(suppressMessages(dl_correr(d, causa = 9100, semilla = 3, rapido = TRUE, sensibilidad = FALSE)),
                    class = "dl_error")
  expect_match(conditionMessage(e), paste0("se aleja del ancla \\(error relativo mediano .*configuración: ",
                                           "ancla: \\{error_maximo: \\.\\.\\.\\} \\(ver \\?dl_configuracion\\); ",
                                           "ni rapido = TRUE ni forzar = TRUE la saltan"))
  expect_no_match(conditionMessage(e), "avanzado|gate_err_mediano")
  # sin los nombres internos de las comprobaciones
  expect_no_match(paste(c(conditionMessage(e), mensajes), collapse = "\n"), "anchor_identity|amplitud_csmr")
  # forzar = TRUE tampoco la salta (sin rapido: cadenas cortas dadas en `opciones`)
  expect_error(suppressMessages(dl_correr(d, causa = 9100, semilla = 3, sensibilidad = FALSE, forzar = TRUE,
                                          opciones = do.call(dl_opciones_mcmc, .DL_OPCIONES_PRUEBA))),
               "se aleja del ancla \\(error relativo mediano")
  expect_identical(length(list.files(file.path(d, "resultados", "mod", "dismod_lite"))), 1L)
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
  e <- expect_error(suppressMessages(dl_correr(d, 9100, semilla = 1, rapido = TRUE)),
                    "la causa 9100 no tiene estados de salud en la tabla severidad: sin ellos no hay AVD")
  expect_no_match(conditionMessage(e), "la tabla la tabla")
  expect_false(dir.exists(file.path(d, "resultados")))
})

test_that("dl_correr() comprueba la convergencia justo después del ajuste nacional, antes de seguir", {
  d <- copia_ejemplo()
  o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
  mensajes <- character()
  expect_error(withCallingHandlers(dl_correr(d, 9101, semilla = 1, opciones = o), message = function(m) {
    mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
  }), "^dl_correr\\(\\): las cadenas no convergieron.*forzar = TRUE la escribe igual")
  expect_match(mensajes[length(mensajes)], "ajuste nacional")
  expect_false(dir.exists(file.path(d, "resultados")))
})

test_that("dl_correr(forzar = TRUE) escribe la corrida aunque las cadenas no converjan y lo dice una vez", {
  d <- copia_ejemplo()
  o <- dl_opciones_mcmc(simulaciones = 100L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L,
                        adelgazamiento = 1L)
  mensajes <- character()
  run <- withCallingHandlers(dl_correr(d, 9101, semilla = 1, opciones = o, sensibilidad = FALSE, forzar = TRUE),
                             message = function(m) {
                               mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
                             })
  expect_s3_class(run, "dl_run")
  expect_match(run$run_id, "_causa-9101_v1$")                       # el nombre de la corrida no cambia
  expect_true(run$manifest$validacion$gates$force)
  forzada <- grep("forzar", mensajes, value = TRUE)
  expect_length(forzada, 1L)
  expect_match(forzada, "forzar = TRUE: las cadenas no convergieron \\(R-hat máximo [0-9.]+.*ESS mínimo [0-9]+")
  expect_no_match(mensajes[1], "corrida de prueba")
  for (mal in list("TRUE", NA, c(TRUE, FALSE), 1))
    expect_error(dl_correr(d, 9101, semilla = 1, forzar = mal), "`forzar` debe ser TRUE o FALSE")
})

test_that("dl_correr(anios = ) valida `anios` antes de leer ni ajustar nada", {
  d <- copia_ejemplo()
  for (mal in list(integer(), c(2019, NA), 2019.5, "2019", c(2019, 2019), TRUE, c(2019, Inf), 0))
    expect_error(dl_correr(d, 9101, semilla = 1, carpeta_salida = tempdir(), anios = mal),
                 "`anios` debe ser|`anios` no debe repetir")
  expect_error(dl_correr(d, 9101, semilla = 1, carpeta_salida = tempdir(), anios = c(2019, 2023, 2019)),
               "`anios` no debe repetir años: se repite 2019")
})

test_that("dl_correr(anios = ) corre la causa una vez por año y devuelve una corrida por año", {
  d <- copia_ejemplo()
  salida <- file.path(withr::local_tempdir(), "resultados")
  correr <- function(p, causa = 9101, ...)
    suppressMessages(dl_correr(p, causa, semilla = 1, rapido = TRUE, sensibilidad = FALSE, carpeta_salida = salida, ...))
  estimaciones <- function(run)
    utils::read.csv(file.path(run$dir, "cause", "prevalence", paste0(run$run_id, ".csv")),
                    fileEncoding = "UTF-8")[, c("val", "lower", "upper")]
  # la carpeta: un año que no es el de la configuración (2019) y el que sí (2023)
  r <- correr(d, anios = c(2019, 2023))
  expect_s3_class(r, "dl_corridas")
  expect_length(r, 2L)
  expect_identical(names(r), c("2019", "2023"))
  expect_true(all(vapply(r, inherits, NA, "dl_run")))
  expect_identical(vapply(r, function(x) x$manifest$params$year, 0L), c("2019" = 2019L, "2023" = 2023L))
  expect_match(r[["2019"]]$run_id, "_causa-9101-2019-prueba_v1$")
  expect_match(r[["2023"]]$run_id, "_causa-9101-2023-prueba_v1$")
  expect_true(all(dir.exists(vapply(r, function(x) x$dir, ""))))
  expect_false(isTRUE(all.equal(estimaciones(r[["2019"]]), estimaciones(r[["2023"]]))))
  # print(): una línea por año con el identificador de la corrida
  salida_print <- capture.output(print(r))
  expect_match(salida_print[1], "^<dl_corridas> 2 corrida\\(s\\)")
  expect_match(salida_print[2], paste0("^  2019: ", r[["2019"]]$run_id, "$"))
  expect_match(salida_print[3], paste0("^  2023: ", r[["2023"]]$run_id, "$"))
  # sin `anios`, todo como antes: una corrida, el año de la configuración y el nombre sin año
  sola <- correr(d)
  expect_s3_class(sola, "dl_run")
  expect_match(sola$run_id, "_causa-9101-prueba_v1$")
  expect_identical(estimaciones(r[["2023"]]), estimaciones(sola))
  # un proyecto de dl_proyecto() leído para otro año se relee para cada uno
  p <- suppressMessages(dl_proyecto(d, 9101, anio = 2019))
  r1 <- correr(p, anios = 2023)
  expect_s3_class(r1, "dl_corridas")
  expect_identical(names(r1), "2023")
  expect_identical(estimaciones(r1[["2023"]]), estimaciones(sola))
  # y uno con las tablas como argumentos (el de la causa 9100, que trae sus betas: una causa que las toma de otra
  # las busca en la carpeta), también
  cfg <- yaml::read_yaml(file.path(d, "config", "9100.yaml"))
  m <- suppressMessages(do.call(dl_proyecto, c(list(configuracion = cfg, anio = 2019),
                                               Filter(Negate(is.null), dl_proyecto(d, 9100)$tablas))))
  expect_null(m$carpeta)
  r2 <- correr(m, 9100, anios = 2023)
  expect_identical(names(r2), "2023")
  expect_match(r2[["2023"]]$run_id, "_causa-9100-2023-prueba_v1$")
  expect_identical(estimaciones(r2[["2023"]]), estimaciones(correr(d, 9100)))
  # un año que no se puede leer se descubre antes de correr ninguno: el error dice cuál y no se escribe nada
  escritas <- function() list.files(file.path(salida, "mod", "dismod_lite"))
  antes <- escritas()
  for (anios in list(c(2019, 2025), c(2025, 2019), 2025)) {
    e <- expect_error(correr(d, anios = anios), "^dl_correr\\(\\): el proyecto no se puede leer para el año 2025: ",
                      class = "dl_error")
    expect_match(conditionMessage(e), "No se corrió ningún año: corrige ese año o quítalo de `anios`\\.$")
    expect_no_match(conditionMessage(e), "la corrida del año")
    expect_identical(e[c("anio", "escritas")], list(anio = 2025L, escritas = integer()))
  }
  expect_error(correr(p, anios = c(2019, 2025)), "el proyecto no se puede leer para el año 2025", class = "dl_error")
  expect_identical(escritas(), antes)
})

test_that("dl_correr(anios = ) lee el proyecto de cada año una sola vez: sus mensajes no se repiten", {
  d <- copia_ejemplo()
  mensajes <- character()
  r <- withCallingHandlers(dl_correr(d, 9101, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                     carpeta_salida = withr::local_tempdir(), anios = c(2023, 2024)),
                           message = function(m) {
                             mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
                           })
  expect_identical(names(r), c("2023", "2024"))
  # la proyección de 2024 se anuncia una vez, al leer los años, antes de correr el primero
  proyeccion <- grep("el ancla no trae 2024: se proyecta desde 2023", mensajes)
  expect_length(proyeccion, 1L)
  expect_lt(proyeccion, grep("año 2023 \\(1 de 2\\)", mensajes))
})

test_that("el error de un año conserva la clase del error original", {
  propio <- structure(class = c("error_propio", "error", "condition"), list(message = "algo falló", call = NULL))
  e <- expect_error(.dl_en_anio(2020L, list(), stop(propio)), "la corrida del año 2020 falló: algo falló",
                    class = "error_propio")
  expect_s3_class(e, "dl_error")
  expect_identical(class(e), c("error_propio", "dl_error", "error", "condition"))
  expect_identical(e[c("anio", "escritas")], list(anio = 2020L, escritas = integer()))
  # el de un error del paquete sigue siendo un dl_error, sin clases repetidas
  e <- expect_error(.dl_en_anio(2020L, list(), .dl_stop("no se pudo")), "falló: no se pudo", class = "dl_error")
  expect_identical(class(e), c("dl_error", "error", "condition"))
})

test_that("el error de un año conserva los campos del error original", {
  e <- expect_error(.dl_en_anio(2020L, list(), .dl_stop("faltan corridas", campos = list(faltan = 9102L, anio = 1L))),
                    "la corrida del año 2020 falló: faltan corridas", class = "dl_error")
  expect_identical(e$faltan, 9102L)
  expect_identical(e$anio, 2020L)                         # el año que falló, no el campo del original
  expect_identical(e$detalle, "la corrida del año 2020 falló: faltan corridas\n  No quedó escrita ninguna corrida.")
  expect_identical(sum(names(e) %in% c("message", "call", "detalle", "funcion", "anio", "escritas")), 6L)
  propio <- structure(class = c("error_propio", "error", "condition"),
                      list(message = "algo falló", call = NULL, dato = list(a = 1)))
  expect_identical(expect_error(.dl_en_anio(2020L, list(), stop(propio)), class = "error_propio")$dato, list(a = 1))
  # los problemas entre tablas de un año de dl_correr(anios = ) se leen igual que los de dl_proyecto()
  d <- copia_ejemplo()
  f <- file.path(d, "ubicaciones.csv")
  u <- leer_texto(f)
  escribir_texto(u[rep(seq_len(nrow(u)), ifelse(seq_len(nrow(u)) == 2L, 2L, 1L))], f)
  e0 <- expect_error(suppressMessages(dl_proyecto(d, 9101)), class = "dl_error")
  expect_gt(length(e0$problemas), 0L)
  for (anios in list(2023, c(2019, 2023))) {
    e <- expect_error(suppressMessages(dl_correr(d, 9101, semilla = 1, rapido = TRUE, anios = anios)),
                      sprintf("el proyecto no se puede leer para el año %d", anios[1L]), class = "dl_error")
    expect_identical(e$problemas, e0$problemas)
  }
})

test_that("dl_correr(anios = ) con forzar = TRUE y registro: cada año se escribe forzado y queda registrado", {
  d <- copia_ejemplo()
  reg <- file.path(d, "registro.yaml"); writeLines("datasets: []", reg)
  o <- dl_opciones_mcmc(simulaciones = 100L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L,
                        adelgazamiento = 1L)
  mensajes <- character()
  r <- withCallingHandlers(dl_correr(d, 9101, semilla = 1, opciones = o, sensibilidad = FALSE, forzar = TRUE,
                                     registro = reg, anios = c(2019, 2023)),
                           message = function(m) {
                             mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
                           })
  expect_s3_class(r, "dl_corridas")
  expect_identical(names(r), c("2019", "2023"))
  expect_true(all(vapply(r, function(x) isTRUE(x$manifest$validacion$gates$force), NA)))
  expect_match(vapply(r, function(x) x$run_id, ""), "_causa-9101-20(19|23)_v1$")     # corridas de producción
  expect_length(grep("forzar = TRUE: las cadenas no convergieron", mensajes), 2L)    # una vez por año
  registradas <- vapply(yaml::read_yaml(reg)$datasets, function(x) x$run_id, "")
  expect_identical(registradas, unname(vapply(r, function(x) x$run_id, "")))
})

# ---- Una causa que es la suma de sus subtipos ----

test_that("dl_revisar_proyecto() de una suma: sus tablas, su configuración y sus subtipos, sin pasos de modelo", {
  d <- copia_con_suma()
  r <- revisar_callado(d, causa = 9200)
  expect_identical(sum(r$estado == "error"), 0L)
  expect_identical(sum(r$estado == "aviso"), 0L)
  expect_identical(unique(r$paso), c("ubicaciones", "poblacion", "configuración", "proyecto"))
  expect_match(r$detalle[nrow(r)], "^suma de los subtipos 9101 \\+ 9102 \\+ 9103: dl_correr\\(\\) suma las corridas")
  # un proyecto ya leído se revisa igual
  r2 <- revisar_callado(dl_proyecto(d, 9200))
  expect_identical(r2$estado, rep("ok", nrow(r2)))
  expect_false("insumos" %in% r2$paso)
  # las demás causas del proyecto se siguen revisando enteras
  r1 <- revisar_callado(d, causa = 9101)
  expect_true("insumos" %in% r1$paso)
  expect_identical(sum(r1$estado == "error"), 0L)
  # un subtipo sin configuración en el proyecto: un aviso que lo nombra; las claves del modelo, otro
  unlink(file.path(d, "config", "9102.yaml"))
  cat("edad_inicio: 30\nancla:\n  peso: 0.5\n", file = file.path(d, "config", "9200.yaml"), append = TRUE)
  r3 <- revisar_callado(d, causa = 9200)
  expect_identical(sum(r3$estado == "error"), 0L)
  avisos <- r3[r3$estado == "aviso", ]
  expect_identical(avisos$paso, c("configuración", "proyecto"))
  expect_match(avisos$detalle[1], "no se ajusta: no se usa\\(n\\) la\\(s\\) clave\\(s\\) edad_inicio, ancla$")
  expect_match(avisos$detalle[2],
               "^el/los subtipo\\(s\\) 9102 no tiene\\(n\\) configuración en el proyecto.*subtipo_de: 9200")
  # una tabla de la suma que falta es un error en su paso; las que no son suyas no se piden
  unlink(file.path(d, "poblacion.csv"))
  unlink(file.path(d, "ancla"), recursive = TRUE)
  r4 <- revisar_callado(d, causa = 9200)
  expect_identical(r4$paso[r4$estado == "error"], "poblacion")
})

test_that("dl_correr() de una suma busca las corridas de sus subtipos y las suma, por año", {
  d <- copia_con_suma()
  salida <- file.path(withr::local_tempdir(), "resultados")
  corridas <- file.path(salida, "mod", "dismod_lite")
  # las corridas de prueba de los tres subtipos para 2019 y 2023, una sola vez
  for (k in 9101:9103)
    suppressMessages(dl_correr(d, k, semilla = 1, rapido = TRUE, sensibilidad = FALSE, carpeta_salida = salida,
                               anios = c(2019, 2023)))
  simulaciones <- function(dir, anio = 2023)
    as.data.frame(.dl_leer_draws(dir, "prevalence", anio))
  de_subtipo <- function(k, anio = 2023)
    file.path(corridas, grep(sprintf("_causa-%d-%d-prueba_v1$", k, anio), list.files(corridas), value = TRUE))
  mensajes <- character()
  # sin `semilla`: una suma no la usa
  run <- withCallingHandlers(dl_correr(d, 9200, carpeta_salida = salida, rapido = TRUE),
                             message = function(m) {
                               mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
                             })
  expect_s3_class(run, "dl_run")
  expect_match(run$run_id, "_causa-9200-prueba_v1$")
  expect_match(mensajes[1],
               "causa 9200: suma de los subtipos 9101 \\+ 9102 \\+ 9103, con sus corridas de prueba de 2023")
  # y qué corrida tomó de cada subtipo
  expect_identical(sub("\n$", "", mensajes[2:4]),
                   sprintf("dl_correr(): %d: %s", 9101:9103, basename(vapply(9101:9103, de_subtipo, ""))))
  expect_no_match(paste(mensajes, collapse = "\n"), "cadena|forzar")
  man <- run$manifest
  expect_identical(man$causa$agregacion, "suma_de_hijas")
  expect_identical(man$causa$cause_name, "Suma sintética de subtipos")
  expect_identical(vapply(man$causa$hijas, `[[`, 0L, "cause_id"), 9101:9103)
  expect_identical(vapply(man$causa$hijas, `[[`, "", "run_id"), basename(vapply(9101:9103, de_subtipo, "")))
  expect_identical(man$params$year, 2023L)
  # la prevalencia de la suma es la suma, simulación a simulación, de las de los subtipos: con la misma población
  # en cada celda, también en número de casos
  suma <- simulaciones(run$dir)
  partes <- lapply(9101:9103, function(k) simulaciones(de_subtipo(k)))
  celda <- c("location_id", "sex_id", "age_group_id")
  for (x in partes) expect_identical(x[celda], suma[celda])
  cols <- setdiff(names(suma), celda)
  expect_equal(suma[cols], Reduce(`+`, lapply(partes, `[`, cols)), tolerance = 1e-12)
  expect_gt(sum(suma[suma$location_id == "123", cols]), 0)

  # por año: cada suma con las corridas de su año, y el año en el nombre
  r <- suppressMessages(dl_correr(d, 9200, carpeta_salida = salida, rapido = TRUE, anios = c(2019, 2023)))
  expect_s3_class(r, "dl_corridas")
  expect_identical(names(r), c("2019", "2023"))
  expect_match(r[["2019"]]$run_id, "_causa-9200-2019-prueba_v1$")
  for (a in c(2019L, 2023L)) {
    expect_identical(r[[as.character(a)]]$manifest$params$year, a)
    expect_identical(vapply(r[[as.character(a)]]$manifest$causa$hijas, `[[`, "", "run_id"),
                     basename(vapply(9101:9103, de_subtipo, "", anio = a)))
  }
  expect_identical(simulaciones(r[["2023"]]$dir), suma)

  # con un subtipo omitido: suma los otros dos y el manifiesto lo declara, con su motivo entre las limitaciones
  cat("subtipos_omitidos:\n  - {causa: 9103, motivo: \"prueba\"}\n", file = file.path(d, "config", "9200.yaml"),
      append = TRUE)
  ro <- suppressMessages(dl_correr(d, 9200, carpeta_salida = salida, rapido = TRUE))
  expect_identical(vapply(ro$manifest$causa$hijas, `[[`, 0L, "cause_id"), 9101:9102)
  expect_identical(ro$manifest$causa$hijas_omitidas, list(list(cause_id = 9103L)))
  expect_true(any(grepl("^hija 9103 omitida de la suma — prueba$", unlist(ro$manifest$limitaciones))))
  expect_equal(simulaciones(ro$dir)[cols], Reduce(`+`, lapply(partes[1:2], `[`, cols)), tolerance = 1e-12)

  # solo hay corridas de prueba: la suma de producción no las toma, y el error lo dice
  n <- length(list.files(corridas))
  e <- expect_error(suppressMessages(dl_correr(d, 9200, carpeta_salida = salida)), class = "dl_error")
  expect_match(conditionMessage(e), "falta la corrida de producción de 2023 del/de los subtipo\\(s\\) 9101, 9102 en ")
  expect_match(conditionMessage(e), "De 9101, 9102 solo hay corridas de prueba: para sumarlas, rapido = TRUE")
  # falta la corrida de un subtipo: el error dice cuál, de qué año y dónde se buscó; con `anios`, el año que falló
  unlink(de_subtipo(9102), recursive = TRUE)
  e <- expect_error(suppressMessages(dl_correr(d, 9200, carpeta_salida = salida, rapido = TRUE)), class = "dl_error")
  expect_match(conditionMessage(e), "falta la corrida de prueba de 2023 del/de los subtipo\\(s\\) 9102 en ")
  expect_match(conditionMessage(e), .dl_dir_corrida(salida), fixed = TRUE)
  expect_identical(e$faltan, 9102L)
  e <- expect_error(suppressMessages(dl_correr(d, 9200, carpeta_salida = salida, rapido = TRUE, anios = c(2019, 2023))),
                    "la corrida del año 2023 falló: falta la corrida de prueba de 2023", class = "dl_error")
  expect_identical(e$escritas, 2019L)
  expect_identical(e[c("faltan", "anio")], list(faltan = 9102L, anio = 2023L))
  # un año que falla al correr (el proyecto de cada año se leyó bien): el error dice qué años quedaron escritos
  expect_match(conditionMessage(e), "Quedaron escritas las corridas de: 2019 \\(.*_causa-9200-2019-prueba_v[0-9]+\\)\\.$")
  escrita <- sub("^.*\\(([^()]*_causa-9200-2019-prueba_v[0-9]+)\\)\\.$", "\\1",
                 grep("Quedaron", strsplit(conditionMessage(e), "\n")[[1L]], value = TRUE))
  expect_true(escrita %in% list.files(corridas))
  expect_identical(length(list.files(corridas)), n)              # se borró una y quedó escrita la suma de 2019

  # las corridas de un subtipo que declara otra causa padre no se suman: el error dice cómo declararla
  otra <- withr::local_tempdir()
  writeLines(enc2utf8(c("causa: 9300", "nombre: Otra suma", "anio: 2019", "subtipos: [9101, 9103]",
                        "suma_de_subtipos: sí")), file.path(otra, "config.yaml"), useBytes = TRUE)
  for (f in c("ubicaciones.csv", "poblacion.csv")) file.copy(dl_ejemplo(f), file.path(otra, f))
  expect_error(suppressMessages(dl_correr(otra, carpeta_salida = salida, rapido = TRUE)),
               paste0("_causa-9101-2019-prueba_v1, .*_causa-9103-2019-prueba_v1 \\(subtipo\\(s\\) 9101, 9103\\) no ",
                      "declara\\(n\\) a la causa 9300 como su causa padre: .*subtipo_de: 9300"), class = "dl_error")

  # un proyecto que se ajusta sigue exigiendo la semilla
  expect_error(dl_correr(d, 9101, carpeta_salida = salida, rapido = TRUE), "^dl_correr\\(\\): falta `semilla`")
})

test_that("una suma de producción declara en sus limitaciones los subtipos escritos con forzar = TRUE", {
  d <- copia_con_suma()
  salida <- file.path(withr::local_tempdir(), "resultados")
  o <- do.call(dl_opciones_mcmc, .DL_OPCIONES_PRUEBA)      # cadenas cortas, en corridas de producción
  for (k in 9101:9103)
    suppressMessages(dl_correr(d, k, semilla = 1, opciones = o, sensibilidad = FALSE, forzar = TRUE,
                               carpeta_salida = salida))
  suma <- suppressMessages(dl_correr(d, 9200, carpeta_salida = salida))
  expect_match(suma$run_id, "_causa-9200_v1$")
  lim <- unlist(yaml::read_yaml(file.path(suma$dir, "manifest.yaml"))$limitaciones)
  expect_identical(lim, unlist(suma$manifest$limitaciones))
  expect_length(grep("forzar = TRUE", lim), 1L)
  expect_match(lim[length(lim)], "^corrida\\(s\\) de la\\(s\\) hija\\(s\\) 9101, 9102, 9103 escrita\\(s\\) con forzar = TRUE")
  # dl_sumar_hijas(), con las mismas corridas elegidas a mano, escribe el manifiesto de siempre
  a_mano <- dl_sumar_hijas(file.path(dirname(suma$dir), vapply(suma$manifest$causa$hijas, `[[`, "", "run_id")),
                           causa = 9200, nombre = "a-mano", carpeta = salida, nombre_causa = "Suma sintética de subtipos",
                           rutas = suppressMessages(dl_proyecto(d, 9200))$rutas)
  expect_identical(unlist(a_mano$manifest$limitaciones), lim[-length(lim)])
  # y una suma de prueba no lo dice: toda corrida de prueba se escribe así
  for (k in 9101:9103)
    suppressMessages(dl_correr(d, k, semilla = 1, rapido = TRUE, sensibilidad = FALSE, carpeta_salida = salida))
  prueba <- suppressMessages(dl_correr(d, 9200, carpeta_salida = salida, rapido = TRUE))
  expect_no_match(unlist(prueba$manifest$limitaciones), "forzar")
})

test_that("la revisión avisa de una clave corta y su equivalente de `avanzado` con valores distintos", {
  avisos <- function(s) { x <- character(); .dl_avisar_avanzado(s, function(paso, estado, detalle) {
    expect_identical(c(paso, estado), c("configuración", "aviso")); x <<- c(x, detalle) }); x }
  procedencia <- "prueba"
  s <- list(causa = 502L, anio = 2023L, subtipo_de = 501L, remision = 0.1,
            ancla = list(error_maximo = 0.1, peso = 0.5),
            mortalidad_exceso = list(fraccion_aguda = 0.3), sensibilidad = list(fraccion_aguda = list(0, 0.3)),
            avanzado = list(anchor = list(gate_err_mediano = list(valor = 0.2, procedencia = procedencia),
                                          lambda = list(valor = 0.5, procedencia = procedencia)),
                            emr_prior = list(fraccion_aguda = list(valor = 0.4, procedencia = procedencia)),
                            sensibilidad = list(fraccion_aguda = list(0, 0.5)),
                            extraction = list(cause_id = 777L, motivo = "prueba"), nsub = 4L))
  a <- avisos(s)
  expect_length(a, 4L)                     # ancla.peso dice lo mismo que avanzado: anchor.lambda; nsub no tiene clave
  expect_identical(a[1], paste0("ancla.error_maximo (0.1) y avanzado: anchor.gate_err_mediano (0.2) dicen valores ",
                                "distintos: rige el de `avanzado`; deja uno de los dos"))
  expect_match(a[2], "^mortalidad_exceso.fraccion_aguda \\(0.3\\) y avanzado: emr_prior.fraccion_aguda \\(0.4\\) dicen")
  expect_match(a[3], "^sensibilidad.fraccion_aguda \\(0, 0.3\\) y avanzado: sensibilidad.fraccion_aguda \\(0, 0.5\\) dicen")
  expect_match(a[4], "^subtipo_de \\(501\\) y avanzado: extraction.cause_id \\(777\\) dicen")
  # los mismos valores, solo la clave corta, solo `avanzado` o un bloque de `avanzado` sin el valor: sin aviso
  igual <- s
  igual$avanzado <- list(anchor = list(gate_err_mediano = list(valor = 0.1, procedencia = procedencia)),
                         emr_prior = list(fraccion_aguda = list(cfr_30d = 0.1)), extraction = list(cause_id = 501L))
  expect_length(avisos(igual), 0L)
  expect_length(avisos(s[names(s) != "avanzado"]), 0L)
  expect_length(avisos(s[c("causa", "anio", "avanzado")]), 0L)
  # en la revisión de la carpeta y en la del proyecto leído, en el paso de la configuración; dl_proyecto() no cambia
  f <- dl_ejemplo("config", "9101.yaml")
  d <- copia_ejemplo(`config/9101.yaml` = c(
    readLines(f, encoding = "UTF-8"), "ancla:", "  error_maximo: 0.1", "avanzado:", "  anchor:",
    "    gate_err_mediano: {valor: 0.2, procedencia: prueba}"))
  esperado <- "^ancla.error_maximo \\(0.1\\) y avanzado: anchor.gate_err_mediano \\(0.2\\) dicen valores distintos"
  r <- revisar_callado(d, causa = 9101)
  expect_false(any(r$estado == "error"))
  expect_match(r$detalle[r$paso == "configuración" & r$estado == "aviso"], esperado)
  p <- expect_no_warning(suppressMessages(dl_proyecto(d, 9101)))
  expect_identical(p$configuracion$anchor$gate_err_mediano$valor, 0.2)
  r2 <- revisar_callado(p)
  expect_match(r2$detalle[r2$paso == "configuración" & r2$estado == "aviso"], esperado)
  # el ejemplo, sin `avanzado`, no trae el aviso
  r3 <- revisar_callado(copia_ejemplo(), causa = 9101)
  expect_false(any(r3$paso == "configuración" & r3$estado == "aviso"))
})

test_that("un año que el ancla no trae: la revisión no tiene errores y avisa de la proyección", {
  d <- copia_ejemplo()
  r <- revisar_callado(suppressMessages(dl_proyecto(d, 9101, anio = 2024)))
  expect_false(any(r$estado == "error"))
  aviso <- r$detalle[r$paso == "proyecto" & r$estado == "aviso"]
  expect_match(aviso, "^el ancla no trae 2024: se proyecta desde 2023", all = FALSE)
  # lo mismo en la revisión de la carpeta, con el año en la configuración
  f <- file.path(d, "config", "9101.yaml")
  writeLines(sub("^anio: 2023$", "anio: 2024", readLines(f, encoding = "UTF-8")), f, useBytes = TRUE)
  r2 <- revisar_callado(d, causa = 9101)
  expect_false(any(r2$estado == "error"))
  expect_identical(r2$detalle[r2$paso == "proyecto" & r2$estado == "aviso"], aviso)
  # un proyecto que no proyecta no trae el aviso
  r3 <- revisar_callado(copia_ejemplo(), causa = 9101)
  expect_no_match(paste(r3$detalle, collapse = "\n"), "se proyecta desde")
})

test_that("dl_revisar_proyecto() ubica en su tabla lo que antes llegaba con palabras del formato completo", {
  # el ancla sin muertes (con el prior de la mortalidad en exceso por defecto): una regla entre tablas
  d <- copia_ejemplo()
  a <- leer_texto(file.path(d, "ancla", "sintetico_acs_v1.csv"))
  escribir_texto(a[measure_id != "1"], file.path(d, "ancla", "sintetico_acs_v1.csv"))
  r <- revisar_callado(d, causa = 9101)
  e <- r[r$paso == "proyecto" & r$estado == "error", ]
  expect_identical(r$estado[r$paso == "configuración"], "ok")
  expect_match(e$detalle, "^ancla: no trae la mortalidad de la causa 9101")
  expect_match(e$detalle, "mortalidad_exceso: \\{prior: plano, techo: \\.\\.\\.\\}")
  expect_match(e$sugerencia, "^columnas causa, anio, sexo")                     # las de la tabla ancla
  expect_identical(r$estado[r$paso == "insumos"], "omitido")
  # un poblacion.csv de Excel con «;»: el error de su tabla; las demás se revisan igual
  d <- copia_ejemplo()
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  data.table::fwrite(pob, file.path(d, "poblacion.csv"), sep = ";")
  r <- revisar_callado(d, causa = 9101)
  expect_match(r$detalle[r$paso == "poblacion" & r$estado == "error"], "separador de columnas")
  expect_match(r$sugerencia[r$paso == "poblacion" & r$estado == "error"],
               "columnas ubicacion, anio, sexo, edad_inicio, edad_fin, poblacion")
  expect_identical(r$estado[r$paso == "severidad"], "ok")
  expect_identical(r$estado[r$paso %in% c("proyecto", "insumos")], c("omitido", "omitido"))

  # proxies de otro año y datos imposibles: los errores de los insumos
  d <- escribir_proxies_calibrados(copia_ejemplo())
  px <- leer_texto(file.path(d, "covariables", "proxies.csv"))
  escribir_texto(px[anio != "2023"], file.path(d, "covariables", "proxies.csv"))
  r <- revisar_callado(d, causa = 9101)
  expect_match(r$detalle[r$estado == "error"], "no trae filas subnacionales de 2023")
  d <- copia_ejemplo()
  datos <- leer_texto(file.path(d, "datos.csv"))
  k <- which(datos$medida == "prevalencia" & datos$ubicacion == "123" & datos$anio == "2023" &
               datos$excluir == "FALSE")[1]
  datos$valor[k] <- "2"
  escribir_texto(datos, file.path(d, "datos.csv"))
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  cfg <- c(cfg, "ancla:", "  peso: 0.5", "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]")
  writeLines(enc2utf8(cfg), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  r <- revisar_callado(d, causa = 9100)
  e <- r[r$paso == "datos" & r$estado == "error", ]
  # la fila como en los mensajes de las tablas: la línea del CSV (el encabezado es la 1)
  expect_match(e$detalle, sprintf("prevalencia fuera de \\[0, 1\\].*fila\\(s\\) %d$", k + 1L))
  expect_match(e$sugerencia, "^columnas causa, ubicacion, anio")
})

test_that("dl_revisar_proyecto(): el valor nacional de una covariable con proxies sin su intervalo es un error", {
  # la estimación subnacional sortea el valor nacional de su intervalo (lower_value, upper_value en el GHDx): sin
  # esas columnas, en una descarga o en todas, la revisión lo dice entre las reglas del proyecto
  for (todas in c(FALSE, TRUE)) {
    d <- copia_ejemplo()
    archivos <- sort(list.files(file.path(d, "covariables"), full.names = TRUE))
    for (f in if (todas) archivos else archivos[1]) {
      cv <- leer_texto(f)
      escribir_texto(cv[, c("covariate_name_short", "location_id", "year_id", "sex_id", "mean_value")], f)
    }
    r <- revisar_callado(d, causa = 9100)
    e <- r[r$estado == "error", ]
    expect_identical(nrow(e), 1L, info = todas)
    expect_identical(e$paso, "proyecto")
    expect_match(e$detalle, sprintf("^covariables: el valor nacional de %s de 2023 .*no trae su intervalo",
                                    if (todas) "LDI_pc, SEV_scalar_agestd_cvd_pvd, haqi" else "haqi"))
    expect_identical(r$estado[r$paso == "insumos"], "omitido")
    expect_identical(r$estado[r$paso == "covariables"], "ok")
  }
})

# ---- Reglas entre tablas ----

# Las tablas, la configuración y el número de causas del ejemplo para la causa 9100, para probar las reglas una a una.
contrato_ejemplo <- function() {
  p <- dl_proyecto(ejemplo_calibrado(), 9100)
  list(tablas = lapply(p$tablas, data.table::copy), cfg = p$configuracion)
}
problemas <- function(x, n_causas = 4L) .dl_problemas_proyecto(x$tablas, x$cfg, n_causas)

test_that("el proyecto de ejemplo cumple las reglas entre tablas", {
  expect_identical(problemas(contrato_ejemplo()), list(problemas = character(), avisos = character()))
})

test_that("la revisión encuentra una ubicación que no está en ubicaciones.csv", {
  d <- escribir_proxies_calibrados(dl_ejemplo(copiar_en = withr::local_tempdir()))
  pob <- data.table::fread(file.path(d, "poblacion.csv"), colClasses = list(character = "ubicacion"))
  pob[ubicacion == "01", ubicacion := "99"]
  data.table::fwrite(pob, file.path(d, "poblacion.csv"))
  r <- revisar_callado(d, 9100)
  expect_true(any(r$estado == "error" & grepl("99.*ubicaciones", r$detalle)))
  e <- r[r$paso == "proyecto" & r$estado == "error", ]
  expect_match(e$detalle, "^poblacion: la\\(s\\) ubicación\\(es\\) 99 no está\\(n\\) en la tabla ubicaciones")
  expect_match(e$sugerencia, "^columnas ubicacion, anio, sexo")
})

test_that("la revisión avisa de una covariable con proxies y sin beta", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  b <- data.table::fread(file.path(d, "betas.csv"))
  data.table::fwrite(b[covariable != "haqi"], file.path(d, "betas.csv"))
  r <- revisar_callado(d, 9100)
  expect_true(any(r$estado == "aviso" & grepl("haqi.*no tiene beta", r$detalle)))
  expect_identical(r$estado[r$paso == "insumos"], "ok")                       # un aviso no detiene nada
})

test_that("la revisión pide poblacion_detalle cuando la población es más gruesa que el ancla", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  pob <- data.table::fread(file.path(d, "poblacion.csv"), colClasses = list(character = "ubicacion"))
  g <- pob[edad_inicio >= 80, list(edad_inicio = 80, edad_fin = NA_real_, poblacion = sum(poblacion)),
           by = list(ubicacion, anio, sexo)]
  data.table::fwrite(rbind(pob[edad_inicio < 80], g, use.names = TRUE), file.path(d, "poblacion.csv"))
  r <- revisar_callado(d, 9100)
  expect_true(any(r$estado == "error" & grepl("poblacion_detalle", r$detalle)))
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "error"], "^ancla: la población trae 80 años y más")
  # con poblacion_detalle, la misma comprobación pasa (la de la traducción, que agrupa el ancla)
  det <- pob[edad_inicio >= 80, list(poblacion = sum(poblacion)), by = list(anio, sexo, edad_inicio, edad_fin)]
  data.table::fwrite(det, file.path(d, "poblacion_detalle.csv"))
  r <- revisar_callado(d, 9100)
  expect_identical(unique(r$estado), "ok")
})

test_that("un proyecto nuevo trae las plantillas y se revisa sin errores de forma", {
  d <- file.path(withr::local_tempdir(), "nuevo")
  suppressMessages(dl_nuevo_proyecto(d, 1234, anio = 2023, edad_inicio = 30))
  expect_true(all(file.exists(file.path(d, c("config.yaml", "ubicaciones.csv", "poblacion.csv", "betas.csv",
                                             "LEEME.md")))))
  expect_true(dir.exists(file.path(d, "ancla")))
  expect_identical(readLines(file.path(d, "poblacion.csv")), "ubicacion,anio,sexo,edad_inicio,edad_fin,poblacion")
  r <- revisar_callado(d)
  expect_false(any(r$paso == "configuración" & r$estado == "error"))          # la configuración escrita vale
})

test_that("reglas de ubicaciones: una sola nacional, el padre es la nacional y sin códigos repetidos", {
  x <- contrato_ejemplo()
  u <- x$tablas$ubicaciones
  x$tablas$ubicaciones <- u[, padre := NA_character_]
  expect_match(problemas(x)$problemas, "^ubicaciones: 26 filas van sin padre \\(01, 02, 03, 04, 05 y 21 más\\)")
  x <- contrato_ejemplo()
  x$tablas$ubicaciones[ubicacion == "02", padre := "01"]
  expect_match(problemas(x)$problemas, "^ubicaciones: el padre de una ubicación subnacional es la nacional \\(123\\), y trae 01")
  x <- contrato_ejemplo()
  x$tablas$ubicaciones <- rbind(x$tablas$ubicaciones, x$tablas$ubicaciones[2])
  expect_match(problemas(x)$problemas, "^ubicaciones: códigos repetidos: 01$")
})

test_that("reglas de la población: el año y los sexos del modelo, las mismas bandas en todo y desde edad_inicio", {
  x <- contrato_ejemplo()
  p <- x$tablas$poblacion
  x$tablas$poblacion <- p[!(anio == 2023 & sexo == "mujeres")]
  expect_match(problemas(x)$problemas, "^poblacion: no trae la población de mujeres de 2023 \\(el año que se estima")
  x$tablas$poblacion <- p[!(ubicacion == "05" & anio == 2019 & edad_inicio == 30)]
  expect_match(problemas(x)$problemas, "^poblacion: las bandas de edad .* 05 2019 hombres, 05 2019 mujeres no traen")
  x$tablas$poblacion <- p[edad_inicio >= 35]
  expect_match(problemas(x)$problemas, "^poblacion: la primera banda empieza a los 35 años, después de edad_inicio \\(30\\)")
})

test_that("reglas del ancla: la prevalencia del año y los sexos, la mortalidad si la usa el prior y la columna causa", {
  x <- contrato_ejemplo()
  a <- x$tablas$ancla
  x$tablas$ancla <- a[!(medida == "prevalencia" & anio == 2023 & sexo == "hombres")]
  expect_match(problemas(x)$problemas, paste0("^ancla: no trae la prevalencia de la causa 9100 de hombres de 2023 ",
                                               "\\(años que trae: 2019\\)$"))
  x$tablas$ancla <- data.table::copy(a)[anio == 2019][, anio := 2022L]          # el año anterior: la pista
  expect_match(problemas(x)$problemas[1], "; si 2023 aún no tiene estimación de GBD, proyecta desde 2022 con ancla: \\{anio: 2022\\}$")
  x$tablas$ancla <- data.table::copy(a)[anio == 2019][, anio := 2025L]          # ningún año anterior
  expect_match(problemas(x)$problemas[1], paste0("de hombres y mujeres de 2023 \\(años que trae: 2025\\); solo se ",
                                                  "proyecta desde un año anterior, y el ancla no trae ninguno$"))
  x$tablas$ancla <- a[medida != "mortalidad"]
  expect_match(problemas(x)$problemas, "^ancla: no trae la mortalidad de la causa 9100 \\(medidas que trae")
  # con el prior plano y su techo, la mortalidad no hace falta
  x$cfg$emr_prior <- list(tipo = "plano_cota", cota = c(0, 0.25))
  expect_identical(problemas(x)$problemas, character())
  x <- contrato_ejemplo()
  x$tablas$ancla[, causa := NULL]
  expect_match(problemas(x)$problemas, "^ancla: falta la columna causa: el proyecto tiene 4 causas")
  expect_identical(problemas(x, n_causas = 1L)$problemas, character())
})

test_that("reglas de las betas: el valor nacional de cada covariable, valor_nacional_de y la escala", {
  x <- contrato_ejemplo()
  cv <- x$tablas$covariables
  x$tablas$covariables <- cv[!(is.na(ubicacion) & covariable == "haqi" & anio == 2023)]
  expect_match(problemas(x)$problemas, "^betas: haqi no tiene\\(n\\) valor nacional de 2023 \\(el año del ancla\\)",
               all = FALSE)
  x <- contrato_ejemplo()
  x$tablas$betas[covariable == "LDI_pc", `:=`(escala = 2)]
  x$tablas$betas[, valor_nacional_de := c(NA, NA, "haqi_estandarizado")]
  expect_setequal(problemas(x)$problemas, c(
    "betas: valor_nacional_de nombra haqi_estandarizado, sin valor nacional de 2023 (el año del ancla) en la tabla covariables",
    "betas: escala solo se usa con la transformación lineal; con log o logit va vacía o 1 (LDI_pc)"))
})

test_that("reglas de los proxies: cada ubicación con proxies los trae todos; sin ninguno, un aviso", {
  x <- contrato_ejemplo()
  cv <- x$tablas$covariables
  x$tablas$covariables <- cv[!(ubicacion %in% "25" & covariable == "haqi")]
  expect_match(problemas(x)$problemas, paste0("^covariables: 25 no trae\\(n\\) el proxy de haqi de 2023 \\(el año que ",
                                               "se estima\\) y sí el de otras covariables"))
  x$tablas$covariables <- cv[!(ubicacion %in% "25")]
  expect_identical(problemas(x)$problemas, character())
  expect_match(problemas(x)$avisos, paste0("^covariables: la\\(s\\) ubicación\\(es\\) subnacional\\(es\\) 25 no ",
                                           "tiene\\(n\\) proxies de 2023 .*queda\\(n\\) fuera"))
})

test_that("regla de la severidad: las proporciones de la causa suman 1, con la tolerancia del redondeo", {
  x <- contrato_ejemplo()
  s <- data.table::copy(x$tablas$severidad)
  x$tablas$severidad[causa == 9100 & estado == "Asintomático", proporcion := proporcion - 0.1]
  expect_match(problemas(x)$problemas, "^severidad: las proporciones de la causa 9100 suman 0.9; deben sumar 1$")
  x$tablas$severidad <- data.table::copy(s)[causa == 9100 & estado == "Asintomático",
                                            proporcion := proporcion + .DL_TOLERANCIA_SUMA_PARTICION / 2]
  expect_identical(problemas(x)$problemas, character())
  x$tablas$severidad <- data.table::copy(s)[causa == 9101, proporcion := proporcion / 2]   # otra causa: no es esta
  expect_identical(problemas(x)$problemas, character())
})

# ---- dl_revisar_proyecto() de un proyecto ya leído ----

# El proyecto de ejemplo de la causa 9100 por la puerta de los data.frame (sin carpeta), con `cambiar(tablas)` antes.
proyecto_de_tablas <- function(cambiar = identity) {
  d <- ejemplo_calibrado()
  leer <- function(f) as.data.frame(data.table::fread(f, colClasses = "character", encoding = "UTF-8"))
  t <- cambiar(list(ubicaciones = leer(file.path(d, "ubicaciones.csv")), poblacion = leer(file.path(d, "poblacion.csv")),
                    ancla = file.path(d, "ancla"), covariables = file.path(d, "covariables"),
                    betas = leer(file.path(d, "betas.csv")), datos = leer(file.path(d, "datos.csv")),
                    severidad = leer(file.path(d, "severidad.csv"))))
  do.call(dl_proyecto, c(list(causa = 9100, configuracion = file.path(d, "config", "9100.yaml")), t))
}

test_that("dl_revisar_proyecto() de un proyecto armado con data.frame: el ejemplo, sin errores", {
  salida <- utils::capture.output(r <- dl_revisar_proyecto(proyecto_de_tablas()))
  expect_identical(unique(r$estado), "ok")
  expect_identical(r$paso, c("ubicaciones", "poblacion", "ancla", "covariables", "betas", "datos", "severidad",
                             "configuración", "proyecto", "insumos"))
  expect_identical(r$detalle[r$paso == "poblacion"], "leída: 2100 fila(s), de argumento `poblacion` (data.frame)")
  expect_match(r$detalle[r$paso == "ancla"], "^leída: [0-9]+ fila\\(s\\), de ancla \\(descarga de GBD Results\\)$")
  expect_identical(r$detalle[r$paso == "configuración"], "9100.yaml: proyecto")
  expect_true(all(r$causa == 9100L))
  expect_match(salida[1], "^Revisión del proyecto \\(sin carpeta: las tablas vienen como argumentos\\), causa 9100$")
  # con la carpeta, la cabecera la nombra; sin severidad, los insumos se arman y falta lo que dl_correr() necesita
  salida <- utils::capture.output(r <- dl_revisar_proyecto(dl_proyecto(dl_ejemplo(), 9100)))
  expect_match(salida[1], "^Revisión del proyecto «acs_peru», causa 9100$")
  expect_identical(unique(r$estado), "ok")
  p <- dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30),
                   ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = dl_ejemplo("poblacion.csv"),
                   ancla = dl_ejemplo("ancla"))
  salida <- utils::capture.output(r <- dl_revisar_proyecto(p, causa = 9101))
  expect_match(salida[1], "^Revisión del proyecto \\(sin carpeta: las tablas vienen como argumentos\\), causa 9101$")
  expect_identical(r$detalle[r$paso == "configuración"], "la configuración dada como lista: proyecto")
  expect_identical(r$estado[r$paso == "proyecto"], "ok")
  expect_identical(r$paso[r$estado == "error"], "severidad")
  expect_error(dl_revisar_proyecto(p, causa = 9100), "`causa` es 9100 y el proyecto es de la causa 9101")
  # del formato completo: la configuración y los insumos
  r <- revisar_callado(dl_proyecto(ejemplo_completo(), 9101))
  expect_identical(r$paso, c("configuración", "insumos"))
  expect_identical(unique(r$estado), "ok")
})

test_that("un error entre tablas: dl_proyecto() con data.frame se detiene con lo que la revisión de la carpeta dice", {
  e <- expect_error(proyecto_de_tablas(function(t) { t$poblacion$ubicacion[t$poblacion$ubicacion == "01"] <- "99"; t }),
                    class = "dl_error")
  d <- escribir_proxies_calibrados(dl_ejemplo(copiar_en = withr::local_tempdir()))
  pob <- data.table::fread(file.path(d, "poblacion.csv"), colClasses = list(character = "ubicacion"))
  pob[ubicacion == "01", ubicacion := "99"]
  data.table::fwrite(pob, file.path(d, "poblacion.csv"))
  r_carpeta <- revisar_callado(d, 9100)
  expect_identical(e$problemas, r_carpeta$detalle[r_carpeta$estado == "error"])
  expect_identical(r_carpeta$paso[r_carpeta$estado == "error"], "proyecto")
  expect_identical(r_carpeta$estado[r_carpeta$paso == "insumos"], "omitido")
})

test_that("la revisión muestra los avisos de cada tabla (en una carpeta y en un proyecto ya leído)", {
  d <- copia_ejemplo()
  datos <- leer_texto(file.path(d, "datos.csv"))
  k <- which(datos$medida == "prevalencia")[1]
  datos$valor[k] <- "1.5"
  escribir_texto(datos, file.path(d, "datos.csv"))
  for (x in list(d, suppressMessages(dl_proyecto(d, 9100)))) {
    r <- revisar_callado(x, causa = 9100)
    expect_match(r$detalle[r$paso == "datos" & r$estado == "aviso"], "^prevalencia mayor que 1 \\(fila\\(s\\) ")
  }
})

# ---- La configuración y los números exactos ----

test_that("severidad.padre y componente.secuelas exigen severidad.particion; una partición que no existe, error", {
  d <- copia_ejemplo()
  f <- file.path(d, "config", "9101.yaml")
  cfg <- readLines(f, encoding = "UTF-8")
  writeLines(enc2utf8(c(cfg, "severidad:", "  padre: 9100", "componente:", "  secuelas: [1, 2]")), f, useBytes = TRUE)
  e <- expect_error(dl_proyecto(d, 9101), class = "dl_error")
  expect_setequal(e$problemas, c(
    "severidad.padre: exige severidad.particion (la carpeta de la corrida de partición que reparte las secuelas de la causa padre)",
    "componente.secuelas: exige severidad.particion (la carpeta de la corrida de partición con las fracciones de esas secuelas)"))
  r <- revisar_callado(d, causa = 9101)
  expect_identical(sum(r$paso == "configuración" & r$estado == "error"), 2L)
  writeLines(enc2utf8(c(cfg, "severidad:", "  particion: particion/no_existe")), f, useBytes = TRUE)
  expect_error(dl_proyecto(d, 9101), "severidad.particion: no existe la carpeta «particion/no_existe»")
  r <- revisar_callado(d, causa = 9101)
  expect_match(r$detalle[r$paso == "configuración" & r$estado == "error"], "no existe la carpeta")
})

test_that(".dl_num_exacto(): si ni el texto hexadecimal vuelve al número, un error claro", {
  local_mocked_bindings(.dl_leer_numeros = function(s) if (any(grepl("^0x", s))) rep(0.5, length(s)) else NULL)
  expect_error(.dl_num_exacto(0.1), "no hay un texto que vuelva exactamente a los números 0.1", class = "dl_error")
})

test_that("la revisión de un proyecto con proxies_crudos tiene el paso «proxies» (carpeta y proyecto leído)", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  r <- revisar_callado(d, causa = 9100)
  x <- r[r$paso == "proxies", ]
  expect_identical(x$estado, "ok")
  for (cv in c("SEV_scalar_agestd_cvd_pvd", "LDI_pc", "haqi"))
    expect_match(x$detalle, paste0(cv, ": paseo_aleatorio, q = [0-9.e-]+, ediciones 2019, 2021, 2023"))
  pasos <- unique(r$paso)
  expect_lt(match("proxies_crudos", pasos), match("proxies", pasos))
  expect_lt(match("proxies", pasos), match("configuración", pasos))
  expect_false(any(r$estado == "error"))
  r2 <- revisar_callado(suppressMessages(dl_proyecto(d, 9100)))
  expect_identical(r2[r2$paso == "proxies", "detalle"], x$detalle)
  # sin proxies_crudos ni claves proxies.* no hay paso «proxies»
  sin <- escribir_proxies_calibrados(dl_ejemplo(copiar_en = withr::local_tempdir()))
  expect_false("proxies" %in% revisar_callado(sin, causa = 9100)$paso)
})

test_that("claves proxies.* sin proxies_crudos: un aviso en la revisión (carpeta y proyecto leído), nada más", {
  d <- escribir_proxies_calibrados(dl_ejemplo(copiar_en = withr::local_tempdir()))
  cat("proxies:\n  transformacion: {haqi: diferencia}\n", file = file.path(d, "config", "9100.yaml"), append = TRUE)
  aviso <- "las claves proxies.* no se usan: el proyecto no trae proxies_crudos"
  for (x in list(d, suppressMessages(dl_proyecto(d, 9100)))) {
    r <- revisar_callado(x, causa = 9100)
    expect_identical(r$estado[r$paso == "proxies"], "aviso")
    expect_identical(r$detalle[r$paso == "proxies"], aviso)
    expect_false(any(r$estado == "error"))
  }
  # dl_proyecto() no avisa
  expect_no_warning(suppressMessages(dl_proyecto(d, 9100)))
})

test_that("el paso «proxies» muestra el borde inferior de q, los avisos y los errores de la calibración", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  f <- file.path(d, "proxies_crudos.csv")
  cr <- data.table::fread(f, colClasses = list(character = "ubicacion"))
  # gradiente constante: q en el borde inferior, dicho en la línea en orden (no es un aviso)
  base <- cr[anio == 2019L]
  cr2 <- data.table::rbindlist(lapply(c(2019L, 2021L, 2023L), function(a) data.table::copy(base)[, anio := a][]))
  data.table::fwrite(cr2, f)
  x <- revisar_callado(d, causa = 9100)
  x <- x[x$paso == "proxies", ]
  expect_identical(x$estado, "ok")
  expect_match(x$detalle, "q = [0-9.e+-]+ \\(en el borde inferior: el gradiente es prácticamente constante\\)")
  # una ubicación que falta en una edición: el aviso de dl_calibrar_proxies() en el paso
  data.table::fwrite(cr2[!(ubicacion == "05" & anio == 2021L)], f)
  x <- revisar_callado(d, causa = 9100)
  expect_match(x$detalle[x$paso == "proxies" & x$estado == "aviso"], "no traen las mismas ubicaciones", all = FALSE)
  # también en la revisión de un proyecto ya leído
  p <- suppressWarnings(suppressMessages(dl_proyecto(d, 9100)))
  expect_match(attr(p$calibracion, "avisos"), "no traen las mismas ubicaciones", all = FALSE)
  x2 <- revisar_callado(p)
  expect_identical(x2$detalle[x2$paso == "proxies" & x2$estado == "aviso"],
                   x$detalle[x$paso == "proxies" & x$estado == "aviso"])
  # un error de la calibración va en el paso, y las reglas entre tablas esperan
  data.table::fwrite(cr2[ubicacion == "05", error_estandar := 0], f)
  x <- revisar_callado(d, causa = 9100)
  expect_match(x$detalle[x$paso == "proxies" & x$estado == "error"], "error_estandar debe ser mayor que 0")
  expect_identical(x$estado[x$paso %in% c("configuración", "proyecto")], c("omitido", "omitido"))
})

# ---- dl_correr(): la sensibilidad con el motor y los núcleos del ajuste ----

# Corre una corrida de producción corta de 9101 (forzada, sin reajustes de etiquetas) con `opciones` y devuelve los
# argumentos con que dl_correr() llamó a dl_sensibilidad(): una espía con su misma firma (y sus mismos valores por
# defecto) que no ajusta nada y devuelve NULL.
sensibilidad_pedida <- function(opciones, env = parent.frame()) {
  d <- copia_ejemplo(env = env)
  cfg <- readLines(file.path(d, "config", "9101.yaml"), encoding = "UTF-8")
  cfg <- sub("  peso: [0.1, 0.5, 1.0]", "  peso: [1]\n  correlacion_edad: [0.5]\n  kappa: [1]", cfg, fixed = TRUE)
  writeLines(enc2utf8(cfg), file.path(d, "config", "9101.yaml"), useBytes = TRUE)
  visto <- NULL
  espia <- dl_sensibilidad
  body(espia) <- quote({
    visto <<- list(opciones = opciones, procesos = procesos, semilla = semilla)
    NULL
  })
  environment(espia) <- environment()
  local_mocked_bindings(dl_sensibilidad = espia)
  run <- suppressMessages(dl_correr(d, 9101, semilla = 1, opciones = opciones, forzar = TRUE))
  expect_false(file.exists(file.path(run$dir, "diagnostics", "sensibilidad.csv")))    # la espía devolvió NULL
  visto
}

test_that("las cadenas propias de la sensibilidad son las de la firma de dl_sensibilidad()", {
  expect_identical(.dl_opciones_sensibilidad("mh"), eval(formals(dl_sensibilidad)$opciones))
  expect_identical(.dl_opciones_sensibilidad("mh")[c("draws", "chains", "iter", "warmup", "thin", "cores")],
                   list(draws = 200L, chains = 2L, iter = 6000L, warmup = 3000L, thin = 10L, cores = 1L))
})

test_that("dl_correr() con el motor por defecto y 1 núcleo pide la sensibilidad de siempre", {
  o <- dl_opciones_mcmc(simulaciones = 100L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L,
                        adelgazamiento = 1L)
  visto <- sensibilidad_pedida(o)
  # los argumentos de dl_sensibilidad(b, semilla = semilla), la llamada de la versión 2.2.0: las opciones por defecto
  # de su firma (motor "mh", 1 núcleo) y un proceso. Con los mismos argumentos, el resultado es el mismo bit a bit
  expect_identical(visto$opciones, dl_opciones_mcmc(simulaciones = 200L, cadenas = 2L, iteraciones = 6000L,
                                                    calentamiento = 3000L))
  expect_identical(visto$procesos, 1L)
  expect_identical(visto$semilla, 1)
})

test_that("dl_correr() pasa a la sensibilidad el motor del ajuste y sus núcleos como procesos", {
  skip_if_not_installed("Rcpp")
  skip_on_cran()
  skip_on_os("windows")                                  # allí `nucleos` > 1 avisa y usa un proceso
  o <- dl_opciones_mcmc(simulaciones = 100L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L,
                        adelgazamiento = 1L, nucleos = 2L, motor = "rcpp")
  visto <- sensibilidad_pedida(o)
  expect_identical(visto$opciones$engine, "rcpp")
  expect_identical(visto$procesos, 2L)
  # las cadenas siguen siendo las de la sensibilidad, no las del ajuste, con 1 núcleo por ajuste
  expect_identical(visto$opciones, .dl_opciones_sensibilidad("rcpp"))
  expect_identical(visto$opciones$cores, 1L)
})
