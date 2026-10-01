# Formato simple (R/formato_simple.R, R/proyecto.R): el proyecto de ejemplo en el formato simple (acs_peru) da los
# mismos insumos que el mismo proyecto en el formato completo (acs_peru_completo), y los errores de un proyecto simple
# citan sus archivos y sus claves.

# ---- Equivalencia con el formato completo ----

# Columnas de procedencia que el formato simple no tiene y llena con sus propias constantes. Ninguna entra en una
# cuenta; se comparan todas las demás, con identical().
.PROCEDENCIA_SIMPLE <- list(
  betas = c(nombre_impreso = "sin nombre publicado: va el nombre de la covariable",
            transformacion_procedencia = "«declarado en la configuración simple»",
            fuente = "sin tabla impresa de la extracción"),
  cov_proxy = c(valor_crudo = "proxies.csv trae un solo valor, que ya cierra: valor_crudo = valor",
                n_efectivo = "proxies.csv no trae tamaño efectivo (columna opcional del contrato)",
                metodo_calibracion = "constante del formato simple", acquisition_id = "el nombre del archivo"),
  severidad = c(fuente = "tabla extraída (extraction); la del ejemplo completo viene de una partición (mod)"),
  poblacion = c(acquisition_id = "el nombre del archivo"),
  datos = c(dato_id = "la fila de datos.csv", ajuste_completitud = "datos.csv ya viene corregido: FALSE",
            completitud = "datos.csv no trae la completitud aplicada", acquisition_id = "de la columna fuente",
            cita = "la columna fuente"))

insumos_dos_formatos <- local({
  memo <- list()
  function(causa) {
    k <- as.character(causa)
    if (is.null(memo[[k]])) memo[[k]] <<- suppressMessages(list(
      simple = dl_insumos(dl_proyecto(dl_ejemplo(), causa)),
      completo = dl_insumos(dl_configuracion_ejemplo(causa, formato = "completo"),
                            dl_rutas_ejemplo(causa, formato = "completo"))))
    memo[[k]]
  }
})

for (causa in c(9100L, 9101L)) {
  test_that(sprintf("la causa %d del ejemplo simple da las mismas tablas de insumos que el completo", causa), {
    b <- insumos_dos_formatos(causa)
    for (t in c("prior_gbd", "betas", "cov_valores", "cov_proxy", "severidad", "poblacion", "datos")) {
      s <- b$simple[[t]]; c <- b$completo[[t]]
      fuera <- names(.PROCEDENCIA_SIMPLE[[t]])
      expect_identical(setdiff(names(c), fuera), setdiff(names(s), fuera), info = t)
      cols <- setdiff(names(c), fuera)
      expect_identical(s[, cols, with = FALSE], c[, cols, with = FALSE], info = t)
    }
    expect_gt(nrow(b$simple$prior_gbd), 0L)
    expect_gt(nrow(b$simple$cov_proxy), 0L)
    if (causa == 9100L) expect_gt(nrow(b$simple$datos), 0L)
    for (k in c("meta", "loc_ancla", "techo_emr", "bandas_pobl", "bandas_catalogo"))
      expect_identical(b$simple[[k]], b$completo[[k]], info = k)
  })
}

# El campo `campo` (un vector de claves) de una configuración, o NULL si no está.
campo_de <- function(x, campo) Reduce(function(a, k) if (is.list(a)) a[[k]] else NULL, campo, x)

test_that("la configuración simple del ejemplo se traduce a la completa (lo que entra en las cuentas)", {
  for (causa in c(9100L, 9102L)) {
    s <- dl_configuracion_ejemplo(causa); c <- dl_configuracion_ejemplo(causa, formato = "completo")
    expect_s3_class(s, "dl_config")
    for (campo in list("cause_id", "sexos", "edad_inicio", "nudos_incidencia", "sigma_suavidad", "decisiones",
                       c("years", "ajuste"), c("anchor", "lambda"), c("anchor", "rho_edad"), c("anchor", "medidas"),
                       c("remision", "valor"), c("emr_prior", "tipo"), c("cascada", "kappa"),
                       c("cascada", "cota_warning"), c("cascada", "heldout_anio", "valor"),
                       c("sensibilidad", "lambda"), c("sensibilidad", "rho"), c("sensibilidad", "kappa"),
                       c("extraction", "cause_id")))   # un subtipo declara las betas de su causa padre
      expect_identical(campo_de(s, campo), campo_de(c, campo), info = paste(causa, paste(campo, collapse = ".")))
    expect_identical(as.numeric(unlist(s$emr_prior$cota)), as.numeric(unlist(c$emr_prior$cota)))
    expect_identical(unlist(s$medidas_entrada), unlist(c$medidas_entrada))
    expect_identical(.dl_loc_ancla(s), .dl_loc_ancla(c))
    expect_identical(s$covariables, lapply(c$covariables, function(cv) {
      cv$proxy$justificacion <- .DL_PROCEDENCIA_SIMPLE; cv
    }))
    campos_tr <- function(x) lapply(x$transformaciones, function(t) t[c("covariate_name_short", "transformacion")])
    expect_identical(campos_tr(s), campos_tr(c))
  }
  # las claves tomadas por defecto quedan registradas con su valor
  pd <- dl_configuracion_ejemplo(9100L)$origen$por_defecto
  expect_identical(pd[["ancla.peso"]], "1")
  expect_identical(pd[["nudos"]], "[30, 40, 50, 60, 70, 80, 95]")
  expect_false("mortalidad_exceso.techo" %in% names(pd))       # la declara la configuración
})

test_that("dl_proyecto() lee el ejemplo, lo imprime en español y dl_insumos() acepta el proyecto", {
  p <- dl_proyecto(dl_ejemplo(), 9100)
  expect_s3_class(p, "dl_proyecto")
  expect_s3_class(p$configuracion, "dl_config")
  expect_s3_class(p$rutas, "dl_paths")
  expect_identical(p$formato, "simple")
  salida <- paste(capture.output(print(p)), collapse = "\n")
  expect_match(salida, "causa 9100")
  expect_match(salida, "tomado por defecto")
  expect_match(salida, "ancla.peso \\(\u03bb\\) = 1")
  expect_error(dl_proyecto(dl_ejemplo()), "varias causas \\(9100, 9101, 9102, 9103\\)")
  # la traducción se reutiliza mientras el proyecto no cambie
  expect_identical(dl_proyecto(dl_ejemplo(), 9100)$rutas, p$rutas)
  b <- suppressMessages(dl_insumos(p))
  expect_s3_class(b, "dl_bundle")
  expect_identical(b$rutas, p$rutas)
  # el formato completo también es un proyecto
  pc <- dl_proyecto(system.file("extdata", "acs_peru_completo", package = "dismodlite"), 9100)
  expect_identical(pc$formato, "completo")
  expect_identical(pc$rutas, dl_rutas_ejemplo(9100L, formato = "completo"))
})

test_that("la traducción se reutiliza mientras no cambia lo que traduce; la nueva borra la anterior", {
  d <- copia_ejemplo()
  p <- dl_proyecto(d, 9101)
  r <- p$rutas
  # el mismo contenido con otra fecha: la misma traducción
  Sys.setFileTime(file.path(d, "datos.csv"), Sys.time() + 60)
  expect_identical(dl_proyecto(d, 9101)$rutas, r)
  # otro contenido de una tabla: otra traducción, y la anterior se borra
  escribir_texto(leer_texto(file.path(d, "datos.csv"))[-1], d, "datos.csv")
  r2 <- dl_proyecto(d, 9101)$rutas
  expect_false(identical(r2$poblacion, r$poblacion))
  expect_false(file.exists(r$poblacion))
  # un proyecto leído antes de ese cambio: sus insumos piden volver a leerlo
  expect_error(dl_insumos(p), "la traducción de este proyecto ya no está .*vuelve a llamar a dl_proyecto\\(")
  # otro contenido con la fecha de antes (cp -p, rsync -t): otra traducción, con el valor nuevo
  f <- file.path(d, "poblacion.csv")
  t0 <- file.mtime(f)
  pob <- leer_texto(f)
  pob$poblacion[1] <- "999999"
  escribir_texto(pob, f)
  Sys.setFileTime(f, t0)
  expect_true("999999" %in% leer_texto(dl_proyecto(d, 9101)$rutas$poblacion)$val)
  # el nombre de otra causa del proyecto (va al catálogo de causas): otra traducción
  y <- yaml::read_yaml(file.path(d, "config", "9102.yaml"))
  y$nombre <- "otro nombre"
  yaml::write_yaml(y, file.path(d, "config", "9102.yaml"))
  expect_false(identical(dl_proyecto(d, 9101)$rutas$poblacion, r2$poblacion))
})

# ---- Errores de un proyecto simple ----

config_9100 <- function() readLines(dl_ejemplo("config", "9100.yaml"), encoding = "UTF-8")

test_that("una clave desconocida de la configuración simple sugiere la más parecida", {
  cfg <- sub("^mortalidad_exceso:$", "ancla:\n  pesos: 0.5\nmortalidad_exceso:", config_9100())
  d <- copia_ejemplo(`config/9100.yaml` = cfg)
  expect_error(dl_proyecto(d, 9100), "ancla.pesos: clave desconocida; \u00bfquisiste decir `ancla.peso`\\?")
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "remisio: 0"))
  expect_error(dl_proyecto(d, 9100), "remisio: clave desconocida; \u00bfquisiste decir `remision`\\?")
  # los errores del validador del formato completo citan la clave simple y la completa
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla:", "  peso: 5"))
  expect_error(dl_proyecto(d, 9100), "ancla.peso \\(anchor.lambda\\): debe estar en \\(0, 1\\]")
  # una clave con punto escrita como la citan los mensajes: dice que va dentro de su bloque
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla.peso: 0.5", "sensibilidad.kappa: [0.5, 1]"))
  e <- expect_error(dl_proyecto(d, 9100), class = "dl_error")
  expect_match(conditionMessage(e), "ancla.peso: va dentro de su bloque: ancla: \\{peso: 0.5\\}")
  expect_match(conditionMessage(e),
               "sensibilidad.kappa: va dentro de su bloque: sensibilidad: \\{kappa: \\[0.5, 1\\]\\}")
})

test_that("subnacional.modo: plano sin ubicaciones subnacionales en la población es un error de la configuración", {
  d <- file.path(withr::local_tempdir(), "nacional")
  dir.create(file.path(d, "ancla"), recursive = TRUE)
  file.copy(dl_ejemplo("ancla", "sintetico_acs_v1.csv"), file.path(d, "ancla"))
  pob <- leer_texto(dl_ejemplo("poblacion.csv"))
  pob <- pob[, list(location_id = "123", poblacion = sum(as.numeric(poblacion))),
             by = list(anio, sexo, edad_inicio, edad_fin)]
  escribir_texto(pob, d, "poblacion.csv")
  writeLines(c("causa: 9101", "anio: 2023", "edad_inicio: 30"), file.path(d, "config.yaml"))
  expect_identical(dl_proyecto(d)$configuracion$origen$subnacional, "no")
  writeLines(c("causa: 9101", "anio: 2023", "edad_inicio: 30", "subnacional:", "  modo: plano"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "subnacional.modo: es plano y poblacion.csv no trae ubicaciones subnacionales")
})

test_that("una medida desconocida (datos_en_ajuste o la columna tipo de datos.csv) lista las admitidas", {
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "datos_en_ajuste: [prevalencia, mortalidad]"))
  expect_error(dl_proyecto(d, 9100),
               "datos_en_ajuste: valor\\(es\\) no admitido\\(s\\): prevalencia; los admitidos son: prevalencia_estudio")
  # la prevalencia de registro se lee de datos.csv, pero no entra al ajuste
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "datos_en_ajuste: [prevalencia_registro]"))
  expect_error(dl_proyecto(d, 9100), paste0("datos_en_ajuste: valor\\(es\\) no admitido\\(s\\): prevalencia_registro; ",
                                            "los admitidos son: prevalencia_estudio, incidencia, mortalidad"))
  # una fila nacional de prevalencia de registro que no se excluye: el mensaje pide excluirla, no agregar su tipo
  datos <- readLines(dl_ejemplo("datos.csv"), encoding = "UTF-8")
  datos[2] <- sub(",mortalidad,", ",prevalencia_registro,", datos[2], fixed = TRUE)
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla:", "  peso: 0.5",
                                             "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]"),
                     datos.csv = datos)
  e <- expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), class = "dl_error")
  expect_match(conditionMessage(e), paste0("datos.csv: las filas nacionales de prevalencia_registro no pueden entrar ",
                                           "al ajuste: m\u00e1rcalas para excluir, con su motivo"))
  expect_no_match(conditionMessage(e), "agr\u00e9galo")
  datos[2] <- sub(",prevalencia_registro,", ",muertes,", datos[2], fixed = TRUE)
  d <- copia_ejemplo(datos.csv = datos)
  expect_error(dl_proyecto(d, 9100), "la columna tipo de \u00abdatos.csv\u00bb admite .*; tiene: muertes")
})

test_that("una beta mal formada o una columna que falta se reportan con la clave o el archivo", {
  cfg <- sub("beta: [0.62, 0.55, 0.70]", "beta: [0.62, 0.55]", config_9100(), fixed = TRUE)
  d <- copia_ejemplo(`config/9100.yaml` = cfg)
  expect_error(dl_proyecto(d, 9100),
               "covariables\\[1\\].beta: debe ser \\[media, inferior, superior\\] o un solo n\u00famero")
  cfg <- sub("    beta: [-0.21, -0.30, -0.12]", "", config_9100(), fixed = TRUE)
  d <- copia_ejemplo(`config/9100.yaml` = cfg)
  expect_error(dl_proyecto(d, 9100), "covariables\\[2\\].beta: falta \\(obligatoria\\)")
  pob <- data.table::fread(dl_ejemplo("poblacion.csv"), colClasses = "character", encoding = "UTF-8")
  pob[, sexo := NULL]
  d <- copia_ejemplo()
  data.table::fwrite(pob, file.path(d, "poblacion.csv"))
  expect_error(dl_proyecto(d, 9100), "a \u00abpoblacion.csv\u00bb le falta\\(n\\) la\\(s\\) columna\\(s\\) sexo")
})

test_that("proxies que no cierran en el valor nacional detienen los insumos con la explicación", {
  px <- data.table::fread(dl_ejemplo("proxies.csv"), colClasses = "character", encoding = "UTF-8")
  k <- which(px$covariable == "LDI_pc" & px$anio == "2023")[1]
  px$valor[k] <- as.character(as.numeric(px$valor[k]) * 1.5)
  d <- copia_ejemplo()
  data.table::fwrite(px, file.path(d, "proxies.csv"))
  e <- expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))),
                    "proxies.csv: el promedio ponderado.*\\(covariable LDI_pc, ambos sexos, .*el valor nacional")
  expect_no_match(e$problemas, "promedio_cierra_ancla|900101")        # sin la regla ni el id interno del proxy
})

test_that("covariables[].escala_confirmada confirma una beta grande de haqi; escala distinta de 1 solo con lineal", {
  # una beta de haqi por punto de 0-100 que no es creíble sin confirmarla: el mensaje nombra las claves simples
  cfg <- sub("beta: [-0.012, -0.018, -0.006]", "beta: [-1.2, -1.8, -0.6]", config_9100(), fixed = TRUE)
  d <- copia_ejemplo(`config/9100.yaml` = cfg)
  expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))),
               "covariables\\[3\\]\\.escala: 0.01 .*covariables\\[3\\]\\.escala_confirmada: true")
  d <- copia_ejemplo(`config/9100.yaml` = sub("beta: [-1.2, -1.8, -0.6]",
                                              "beta: [-1.2, -1.8, -0.6]\n    escala_confirmada: true", cfg, fixed = TRUE))
  p <- dl_proyecto(d, 9100)
  expect_true(p$configuracion$transformaciones[[3]]$escala_confirmada)
  expect_null(p$configuracion$transformaciones[[1]]$escala_confirmada)        # sin la clave, no se declara
  expect_identical(suppressMessages(dl_insumos(p))$betas[covariate_name_short == "haqi"]$beta, -1.2)
  # un lógico: true o false
  d <- copia_ejemplo(`config/9100.yaml` = sub("beta: [-1.2, -1.8, -0.6]",
                                              "beta: [-1.2, -1.8, -0.6]\n    escala_confirmada: si", cfg, fixed = TRUE))
  expect_error(dl_proyecto(d, 9100), "covariables\\[3\\]\\.escala_confirmada: debe ser un lógico")
  # con log, una escala distinta de 1 no interviene: es un error, no un valor que se ignora
  cfg <- sub("beta: [0.62, 0.55, 0.70]", "beta: [0.62, 0.55, 0.70]\n    escala: 0.01", config_9100(), fixed = TRUE)
  d <- copia_ejemplo(`config/9100.yaml` = cfg)
  expect_error(dl_proyecto(d, 9100),
               "covariables\\[1\\]\\.escala: solo vale con transformacion: lineal \\(con log no interviene\\)")
  d <- copia_ejemplo(`config/9100.yaml` = sub("escala: 0.01", "escala: 1", cfg, fixed = TRUE))
  expect_s3_class(dl_proyecto(d, 9100), "dl_proyecto")
})

test_that("con datos locales en el ajuste y ancla.peso 1, dl_insumos() avisa del doble conteo en palabras simples", {
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(),
                                             "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]"))
  expect_warning(suppressMessages(dl_insumos(dl_proyecto(d, 9100))),
                 "\\(datos_en_ajuste\\) con ancla.peso = 1.*Considera ancla.peso: 0.5 y anota el motivo en `notas`")
})

test_that("con un proyecto simple, los mensajes de dl_insumos() citan sus claves, sus archivos y sus palabras", {
  # los problemas de una regla del contrato: el archivo y la clave simple, no la tabla ni la clave completas
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla:", "  peso: 0.5", "datos_en_ajuste: [incidencia]"))
  e <- expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), class = "dl_error")
  # dos tipos fuera de datos_en_ajuste (prevalencia_estudio y mortalidad), con el plural «(s)» de los mensajes
  expect_match(conditionMessage(e), paste0("datos.csv: tipo\\(s\\) de dato [a-z_]+, [a-z_]+ en filas ",
                                           "nacionales que entran al ajuste sin estar en datos_en_ajuste de"))
  expect_match(conditionMessage(e), "agr\u00e9galo\\(s\\) a datos_en_ajuste o marca esas filas para excluir, con su motivo")
  expect_no_match(conditionMessage(e), "csmr|medidas_entrada|outlier|prev_estudio|tipos_declarados")
  # el mismo texto, término a término: palabras exactas, nunca dentro de una ruta o de un nombre de archivo
  expect_identical(.dl_texto_simple(c("la tabla cov_proxy", "cascada.heldout_anio.valor", "anchor.lambda = 1",
                                      "transformaciones[2].covariate_name_short", "datos.val: NA en columna",
                                      "  - datos: pareja_val_se_o_x_n \u2014 sin valor: fila_1",
                                      "el tipo de dato csmr", "poblacion.csv no trae la poblaci\u00f3n",
                                      "corrida escrita en /Users/ana/decisiones/transformaciones/csmr/resultados")),
                   c("proxies.csv", "subnacional.anio_validacion", "ancla.peso = 1", "covariables[2].nombre",
                     "datos.csv, columna valor: NA en columna", "  - datos.csv: sin valor: fila_1",
                     "el tipo de dato mortalidad", "poblacion.csv no trae la poblaci\u00f3n",
                     "corrida escrita en /Users/ana/decisiones/transformaciones/csmr/resultados"))
  # sin un proyecto (la configuración y las rutas por separado), los mensajes quedan como están
  p <- dl_proyecto(d, 9100)
  expect_error(suppressMessages(dl_insumos(p$configuracion, p$rutas)), "datos: tipos_declarados .* medidas_entrada")
})

# ---- Un proyecto que no es del Perú ----

# País ficticio (escribir_pais_ficticio(), helper-dismodlite.R) con una causa (501) y una configuración mínima: la
# ubicación nacional y el nombre salen del ancla, la estimación subnacional es plana (la población trae regiones y no
# hay proxies).
proyecto_ficticio <- function(env = parent.frame())
  escribir_pais_ficticio(file.path(withr::local_tempdir(.local_envir = env), "pais ficticio"), 501L, 2020L,
                         c("causa: 501", "anio: 2020", "edad_inicio: 40"), nombre = "Enfermedad ficticia")

test_that("un proyecto de otro país, con códigos subnacionales libres, corre el ajuste, la cascada y los AVD", {
  p <- dl_proyecto(proyecto_ficticio())
  expect_identical(p$configuracion$anchor$location_id, 999L)
  expect_identical(p$configuracion$origen$subnacional, "plano")
  expect_identical(p$configuracion$origen$nombre, "Enfermedad ficticia")
  b <- suppressMessages(dl_insumos(p))
  expect_setequal(b$poblacion$location_id, c("999", "A", "B", "C"))
  expect_identical(b$poblacion[location_level == 0L, sum(val)], b$poblacion[location_level == 1L, sum(val)])
  expect_identical(nrow(b$betas), 0L)
  o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
  f <- dl_ajustar(b, o, semilla = 3L)
  casc <- suppressWarnings(dl_cascada(f, b, semilla = 3L))
  expect_setequal(casc$departamentos, c("A", "B", "C"))
  y <- dl_avd(casc, b, semilla = 3L)
  expect_s3_class(y, "dl_yld")
  r <- dl_resumir(list(fit = casc, yld = y, bundle = b))
  expect_setequal(unique(r$celdas$location_id), c("999", "A", "B", "C"))
})

# ---- Casos de un proyecto real que la traducción resuelve en su borde ----

ancla_ejemplo <- function() leer_texto(dl_ejemplo("ancla", "sintetico_acs_v1.csv"))

test_that("una descarga del ancla con otras ubicaciones (global, región) y ambos sexos da los mismos insumos", {
  a <- ancla_ejemplo()
  mas <- rbind(data.table::copy(a)[, `:=`(location_id = "1", location_name = "Global")],
               data.table::copy(a)[, `:=`(location_id = "120", location_name = "Región andina")],
               data.table::copy(a)[, `:=`(sex_id = "3", sex_name = "Both")])
  d <- copia_ejemplo()
  escribir_texto(rbind(a, mas), d, "ancla", "sintetico_acs_v1.csv")
  b <- suppressMessages(dl_insumos(dl_proyecto(d, 9100)))
  expect_identical(b$hash, insumos_dos_formatos(9100L)$simple$hash)
})

test_that("una prevalencia descargada solo en Percent se pide en Rate; las demás causas no cambian", {
  a <- ancla_ejemplo()
  a <- a[!(a$cause_id == "9101" & a$measure_id == "5" & a$metric_name == "Rate")]
  d <- copia_ejemplo()
  escribir_texto(a, d, "ancla", "sintetico_acs_v1.csv")
  expect_error(dl_proyecto(d, 9101), paste0("ancla/ no trae la prevalencia \\(measure_id 5, métrica Rate\\) de la ",
                                            "causa 9101 en la ubicación 123 \\(métricas que trae: Percent; ",
                                            "ubicaciones: 123\\)"))
  expect_identical(suppressMessages(dl_insumos(dl_proyecto(d, 9100)))$hash, insumos_dos_formatos(9100L)$simple$hash)
})

test_that("lo que el ancla no trae se dice en palabras del formato simple, con la corrección", {
  a <- ancla_ejemplo()
  # sin muertes, con el prior de la mortalidad en exceso por defecto
  d <- copia_ejemplo()
  escribir_texto(a[measure_id != "1"], d, "ancla", "sintetico_acs_v1.csv")
  expect_error(dl_proyecto(d, 9100), paste0("ancla/ no trae la mortalidad \\(measure_id 1, métrica Rate\\) de la ",
                                            "causa 9100.*mortalidad_exceso: \\{prior: plano, techo: \\.\\.\\.\\}"))
  # con el prior plano y un techo, las muertes no hacen falta
  cfg <- sub("^mortalidad_exceso:$", "mortalidad_exceso:\n  prior: plano", config_9100())
  writeLines(enc2utf8(cfg), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  expect_s3_class(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), "dl_bundle")
  # el año que se estima sin estimación de GBD: sugiere proyectar con ancla.anio
  d <- copia_ejemplo(`config/9101.yaml` = sub("^anio: 2023$", "anio: 2024",
                                               readLines(dl_ejemplo("config", "9101.yaml"), encoding = "UTF-8")))
  expect_error(dl_proyecto(d, 9101), "para 2024 \\(años que trae: 2019, 2023\\); si 2024 .*ancla: \\{anio: 2023\\}")
  # una causa sin filas en el ancla (por ejemplo, recién agregada al proyecto)
  suppressMessages(dl_nuevo_proyecto(d, causa = 4712, anio = 2023, edad_inicio = 30))
  expect_error(dl_proyecto(d, 4712),
               "ancla/ no trae filas de la causa 4712 \\(causas que trae: 9100, 9101, 9102, 9103\\)")
  expect_error(dl_proyecto(dl_ejemplo(), causa = 1),
               "el proyecto no tiene la configuración de la causa 1 \\(tiene: 9100, 9101, 9102, 9103\\)")
})

test_that("proxies.csv: solo las covariables declaradas, del año que se estima y de ubicaciones de la población", {
  px <- leer_texto(dl_ejemplo("proxies.csv"))
  d <- copia_ejemplo()
  escribir_texto(px[anio != "2023"], d, "proxies.csv")
  expect_error(dl_proyecto(d, 9100), "proxies.csv no trae filas de 2023 .*\\(años que trae: 2019, 2024\\)")
  escribir_texto(px[location_id == "01", location_id := "Norte"], d, "proxies.csv")
  expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), "proxies.csv: .*sin población subnacional: Norte")
  # una causa del proyecto que declara menos covariables: las demás de proxies.csv no se usan
  d <- copia_ejemplo()
  y <- yaml::read_yaml(file.path(d, "config", "9100.yaml"))
  y$covariables <- y$covariables[1]
  yaml::write_yaml(y, file.path(d, "config", "9100.yaml"))
  b <- suppressMessages(dl_insumos(dl_proyecto(d, 9100)))
  expect_identical(unique(b$cov_proxy$covariate_id_proxy), 900101L)
  # sin ninguna covariable declarada en proxies.csv, la estimación subnacional por defecto es plana; pedir la de
  # covariables es un error, no un ajuste nacional
  y$covariables[[1]]$nombre <- "otra"
  yaml::write_yaml(y, file.path(d, "config", "9100.yaml"))
  expect_identical(dl_configuracion(9100, d)$origen$subnacional, "plano")
  y$subnacional$modo <- "covariables"
  yaml::write_yaml(y, file.path(d, "config", "9100.yaml"))
  expect_error(dl_proyecto(d, 9100), "subnacional.modo: es covariables y ninguna covariable declarada \\(otra\\)")
})

test_that("un subtipo sin covariables toma las de su causa padre; con otras, es un error", {
  c9101 <- readLines(dl_ejemplo("config", "9101.yaml"), encoding = "UTF-8")
  sin <- c9101[seq_len(grep("^covariables:$", c9101) - 1L)]
  d <- copia_ejemplo(`config/9101.yaml` = c(sin, "subnacional:", "  anio_validacion: 2019"))
  cfg <- dl_configuracion(9101, d)
  expect_identical(cfg$transformaciones, dl_configuracion_ejemplo(9101L)$transformaciones)
  expect_identical(cfg$origen$por_defecto[["covariables"]], "las de la causa padre (9100)")
  d <- copia_ejemplo(`config/9101.yaml` = sub("beta: [0.62, 0.55, 0.70]", "beta: 0.5", c9101, fixed = TRUE))
  expect_error(dl_proyecto(d, 9101), "covariables: la causa es un subtipo de la 9100 y sus covariables no son")
})

test_that("datos imposibles (una prevalencia de 2, más casos que muestra) detienen los insumos con sus filas", {
  x <- leer_texto(dl_ejemplo("datos.csv"))
  k <- which(x$tipo == "prevalencia_estudio" & x$location_id == "123" & x$anio == "2023" & x$excluir == "FALSE")
  x$valor[k[1]] <- "2"
  x[k[2], `:=`(valor = "", error_estandar = "", casos = "9000", muestra = "5000")]
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla:", "  peso: 0.5",
                                             "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]"))
  escribir_texto(x, d, "datos.csv")
  e <- expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), class = "dl_error")
  expect_match(conditionMessage(e), sprintf("prevalencia fuera de \\[0, 1\\].*fila_%d", k[1]))
  expect_match(conditionMessage(e), sprintf("datos.csv: más casos que muestra en una prevalencia: fila_%d", k[2]))
  expect_identical(e$tabla, "datos")
  # excluir es lógica: otro valor lo detiene el lector
  x$excluir[1] <- "si"
  escribir_texto(x, d, "datos.csv")
  expect_error(dl_proyecto(d, 9100), "la columna excluir de la tabla datos.csv tiene valores que no son l.gicos")
  # un datos.csv con solo el encabezado es como si no estuviera
  d <- copia_ejemplo(datos.csv = readLines(dl_ejemplo("datos.csv"), n = 1L))
  expect_identical(nrow(suppressMessages(dl_insumos(dl_proyecto(d, 9100)))$datos), 0L)
  expect_match(paste(capture.output(print(dl_proyecto(d, 9100))), collapse = "\n"), "datos.csv +sin filas")
})

test_that("la población debe traer el año que se estima en cada sexo, con los sexos de las etiquetas del paquete", {
  pob <- leer_texto(dl_ejemplo("poblacion.csv"))
  d <- copia_ejemplo()
  escribir_texto(pob[anio != "2023"], d, "poblacion.csv")
  expect_error(dl_proyecto(d, 9100),
               paste0("poblacion.csv no trae la población de hombres y mujeres para 2023 \\(anio, el año que se ",
                      "estima; años que trae: 2019, 2024\\)"))
  escribir_texto(pob[sexo != "mujeres"], d, "poblacion.csv")
  expect_error(dl_proyecto(d, 9100), "de mujeres para 2023 \\(anio, el año que se estima; años que trae: ninguno\\)")
  escribir_texto(pob[1L, sexo := "M"], d, "poblacion.csv")
  expect_error(dl_proyecto(d, 9100),
               "la columna sexo de «poblacion.csv» admite hombres \\(1\\), mujeres \\(2\\), ambos \\(3\\); tiene: M")
})

test_that("un dato excluido sin motivo (la celda vacía) detiene los insumos y la revisión lo pone en datos.csv", {
  x <- leer_texto(dl_ejemplo("datos.csv"))
  k <- which(x$excluir == "TRUE")
  x$motivo[k] <- ""
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla:", "  peso: 0.5",
                                             "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]"))
  escribir_texto(x, d, "datos.csv")
  motivo <- sprintf("datos.csv: un dato marcado para excluir exige su motivo: fila_%d", k)
  expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), motivo, fixed = TRUE)
  utils::capture.output(r <- dl_revisar_proyecto(d, 9100))
  expect_identical(r$detalle[r$paso == "datos.csv" & r$estado == "error"], sub("^datos.csv: ", "", motivo))
})

test_that("claves simples: `modo: no` sin comillas, sexos como conjunto, registros incompletos y dominios", {
  c9101 <- function() readLines(dl_ejemplo("config", "9101.yaml"), encoding = "UTF-8")
  d <- copia_ejemplo(`config/9101.yaml` = sub("^subnacional:$", "subnacional:\n  modo: no", c9101()))
  p <- dl_proyecto(d, 9101)
  expect_identical(p$configuracion$origen$subnacional, "no")
  # solo true y false son lógicos: yes, no, on y off son textos
  cfg <- sub("^nombre: .*$", "nombre: off", c9101())
  expect_identical(dl_configuracion(9101, copia_ejemplo(`config/9101.yaml` = cfg))$origen$nombre, "off")
  expect_identical(length(p$configuracion$covariables), 0L)
  d <- copia_ejemplo(`config/9101.yaml` = c(c9101(), "sexos: [mujeres, hombres]"))
  expect_identical(dl_proyecto(d, 9101)$configuracion$sexos, c(1L, 2L))
  # `departamentos`, el nombre anterior del bloque `subnacional`, vale igual (y no los dos a la vez)
  d <- copia_ejemplo(`config/9101.yaml` = sub("^subnacional:$", "departamentos:\n  modo: plano", c9101()))
  expect_identical(dl_proyecto(d, 9101)$configuracion$origen$subnacional, "plano")
  d <- copia_ejemplo(`config/9101.yaml` = c(c9101(), "departamentos:", "  kappa: 0.5"))
  expect_error(dl_proyecto(d, 9101), "departamentos: es el nombre anterior de `subnacional`")
  # los registros incompletos: todos los campos vacíos juntos
  cfg <- sub("^covariables:$", "covariables:\n  - nombre:\n    efecto_sobre:\n    transformacion:\n    beta:",
             c9101())
  d <- copia_ejemplo(`config/9101.yaml` = cfg)
  e <- expect_error(dl_proyecto(d, 9101), class = "dl_error")
  for (campo in c("nombre", "efecto_sobre", "transformacion", "beta"))
    expect_match(conditionMessage(e), sprintf("covariables\\[1\\]\\.%s: falta \\(obligatoria\\)", campo))
  # dominios: los revisa el validador del formato completo, citados con la clave simple
  cfg <- c(sub("^mortalidad_exceso:$", "mortalidad_exceso:\n  prior: plano", c9101()),
           "incidencia:", "  suavidad: -1", "nudos: [30, 20, 95]", "remision: -0.5")
  cfg <- sub("techo: 0.20", "techo: -1", cfg, fixed = TRUE)
  d <- copia_ejemplo(`config/9101.yaml` = cfg)
  e <- expect_error(dl_proyecto(d, 9101), class = "dl_error")
  expect_match(conditionMessage(e), "incidencia.suavidad \\(sigma_suavidad\\): debe ser un número > 0")
  expect_match(conditionMessage(e), "nudos \\(nudos_incidencia\\): deben ser edades crecientes")
  expect_match(conditionMessage(e), "remision \\(remision.valor\\): debe ser un número >= 0")
  expect_match(conditionMessage(e),
               "mortalidad_exceso.techo \\(emr_prior.cota\\): debe ser un número > 0, por persona-año")
})

test_that("cada clave simple llega a su destino de la tabla y los errores del destino vuelven a la clave", {
  t <- .dl_claves_simple()
  base <- list(causa = 501L, anio = 2020L, edad_inicio = 40L)
  ctx <- list(ubicacion = 999L, nombre = "x", subnacional = FALSE)
  claves <- c("causa", "anio", "edad_inicio", "ubicacion_nacional", "ancla.peso", "ancla.correlacion_edad",
              "ancla.anio", "nudos", "incidencia.suavidad", "remision", "subnacional.kappa",
              "subnacional.anio_validacion", "sensibilidad.peso", "sensibilidad.correlacion_edad",
              "sensibilidad.kappa")
  for (k in claves) {
    v <- switch(k, causa = 501L, ubicacion_nacional = 998L, nudos = c(40L, 60L, 95L),
                if (grepl("anio", k)) 2019L else if (t$tipo[t$clave == k] == "lista") c(0.2, 0.7) else 0.37)
    s <- base
    partes <- strsplit(k, ".", fixed = TRUE)[[1]]
    if (length(partes) == 1L) s[[k]] <- v else s[[partes[1]]] <- stats::setNames(list(v), partes[2])
    cfg <- .dl_traducir_config_simple(s, "config.yaml", ctx)
    destino <- t$destino[t$clave == k]
    llega <- campo_de(cfg, strsplit(destino, ".", fixed = TRUE)[[1]])
    if (is.list(llega) && !is.null(llega$valor)) llega <- llega$valor
    expect_equal(unlist(llega), v, info = k)
    expect_match(.dl_problema_en_simple(destino, "x", s),
                 paste0("^", gsub(".", "[.]", k, fixed = TRUE), "[ :]"), info = k)
  }
  # los bloques y las listas de registros salen de la misma tabla
  expect_identical(unique(sub("[.].*$", "", grep("^[a-z_]+[.]", t$clave, value = TRUE))),
                   c("ancla", "incidencia", "mortalidad_exceso", "subnacional", "sensibilidad"))
  expect_identical(unique(sub("\\[\\].*$", "", grep("[]]", t$clave, value = TRUE))), "covariables")
  # el error de una clave se usa solo con su destino exacto y si la configuración da la clave: el de un campo dentro
  # del destino, o el de un valor de `avanzado` (con la forma del formato completo), es el del validador
  techo <- list(mortalidad_exceso = list(techo = 0))
  expect_match(.dl_problema_en_simple("emr_prior.cota", "x", techo), "techo \\(emr_prior.cota\\): debe ser un n")
  expect_identical(.dl_problema_en_simple("emr_prior.cota", "x", list()), "mortalidad_exceso.techo (emr_prior.cota): x")
  # un valor de `avanzado` (el campo o su bloque) se cita como tal, con el mensaje del validador sin traducir
  avanzado <- list(avanzado = list(emr_prior = list(cota = 5)))
  expect_identical(.dl_problema_en_simple("emr_prior.cota", "x", c(techo, avanzado)), "avanzado: emr_prior.cota: x")
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "avanzado:", "  cascada:",
                                             "    modo: {valor: x, procedencia: y}", "  anchor:",
                                             "    gate_err_mediano: {valor: 0.08}"))
  e <- expect_error(dl_proyecto(d, 9100), class = "dl_error")
  expect_match(conditionMessage(e), "- avanzado: cascada.modo: valores admitidos: proxy")
  expect_match(conditionMessage(e), "- avanzado: anchor.gate_err_mediano.procedencia: ")
  expect_no_match(conditionMessage(e), "subnacional.modo")
  expect_identical(.dl_problema_en_simple("cascada.heldout_anio.procedencia", "falta", list()),
                   "cascada.heldout_anio.procedencia: falta")
})

test_that("config.yaml: su ruta vale como carpeta_config y cause_id sugiere causa", {
  d <- proyecto_ficticio()
  expect_identical(dl_configuracion(501, file.path(d, "config.yaml"))$cause_id, 501L)
  # la causa pedida la compara el validador del formato completo, citado con la clave simple
  expect_error(dl_configuracion(4711, file.path(d, "config.yaml")),
               "causa \\(cause_id\\): es 501 y se pidió la causa 4711")
  writeLines(c("cause_id: 501", "anio: 2020", "edad_inicio: 40"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "cause_id: clave desconocida; ¿quisiste decir `causa`\\?")
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "nudos_incidencia: [40, 60, 80, 95]"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "nudos_incidencia: clave desconocida; ¿quisiste decir `nudos`\\?")
  # la forma anterior, los nudos dentro de incidencia, dice dónde está ahora la clave
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "incidencia:", "  nudos: [40, 60, 80, 95]"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "incidencia.nudos: clave desconocida; `incidencia.nudos` ahora es `nudos`, en el primer nivel",
               fixed = TRUE)
  writeLines(c("anio: 2020", "edad_inicio: 40"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "config.yaml no declara `causa`")
  # la estimación por covariables sin proxies.csv es un error, no un ajuste nacional sin aviso
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "subnacional:", "  modo: covariables"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "subnacional.modo: es covariables y ninguna covariable declarada \\(ninguna\\)")
})

test_that("el ubigeo de dos dígitos de los catálogos sigue la regla de la configuración, no la carpeta", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "catalogos"))
  data.table::fwrite(data.table::data.table(location_id = c("999", "A"), location_name = c("País", "Región A"),
                                            location_level = 0:1, parent_id = c(NA, "999")),
                     file.path(d, "catalogos", .dl_schema_estimates()$catalogos$locations))
  r <- dl_rutas(catalogos = file.path(d, "catalogos"))
  expect_error(.dl_catalogo(r, "locations"), "no son de dos dígitos")
  libres <- .dl_rutas_con_codigos(r, list(anchor = list(location_id = 999L)))
  expect_identical(.dl_catalogo(libres, "locations")$location_id, c("999", "A"))
  expect_identical(.dl_rutas_con_codigos(r, list(anchor = list(location = "peru"))), r)
  # la lectura con códigos libres queda en la memoria aparte: una lectura estricta después sigue exigiendo la regla
  expect_error(.dl_catalogo(r, "locations"), "no son de dos dígitos")
})

test_that("las betas y las prevalencias convertidas se escriben con el texto que vuelve al mismo número", {
  expect_identical(.dl_num_exacto(c(0.62, -0.012)), c("0.62", "-0.012"))
  x <- c(1 / 3, 0.1 + 0.2, 0.62000000000000011, 2 / 3 * 1e-5)
  expect_identical(as.numeric(.dl_num_exacto(x)), x)
})
