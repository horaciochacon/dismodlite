# El proyecto (R/proyecto.R, R/formato_simple.R): dl_proyecto() lee la configuración y las tablas del contrato de una
# carpeta, o de argumentos (data.frame o rutas), y las traduce al formato completo. Las dos puertas dan el mismo
# proyecto, y los errores citan las claves de la configuración y las tablas del contrato.

# El campo `campo` (un vector de claves) de una configuración, o NULL si no está.
campo_de <- function(x, campo) Reduce(function(a, k) if (is.list(a)) a[[k]] else NULL, campo, x)

# País ficticio (escribir_pais_ficticio(), helper-dismodlite.R) con una causa (501) y una configuración mínima: la
# ubicación nacional sale de ubicaciones.csv y el nombre del ancla; la estimación subnacional es plana (hay regiones y
# no hay covariables).
proyecto_ficticio <- function(env = parent.frame(), config = c("causa: 501", "anio: 2020", "edad_inicio: 40"))
  escribir_pais_ficticio(file.path(withr::local_tempdir(.local_envir = env), "pais ficticio"), 501L, 2020L, config,
                         nombre = "Enfermedad ficticia")

# Las tablas de un proyecto en `d` como data.frame (con ubicacion como texto), por su nombre.
tablas_de <- function(d, nombres) lapply(stats::setNames(nm = nombres), function(t) {
  f <- file.path(d, paste0(t, ".csv"))
  if (file.exists(f)) as.data.frame(data.table::fread(f, colClasses = "character", encoding = "UTF-8"))
  else file.path(d, t)
})

# ---- Las dos puertas ----

test_that("las dos puertas dan el mismo proyecto", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  p1 <- dl_proyecto(d, 9100)
  pob <- data.table::fread(file.path(d, "poblacion.csv"), colClasses = list(character = "ubicacion"))
  unlink(file.path(d, "poblacion.csv"))
  p2 <- dl_proyecto(d, 9100, poblacion = as.data.frame(pob))
  expect_identical(suppressMessages(dl_insumos(p1))$hash, suppressMessages(dl_insumos(p2))$hash)
})

test_that("sin carpeta: la configuración y todas las tablas como argumentos", {
  d <- dl_ejemplo()
  args <- lapply(stats::setNames(nm = c("ubicaciones", "poblacion", "ancla", "covariables", "betas", "datos",
                                        "severidad")),
                 function(t) { f <- file.path(d, t); if (file.exists(f)) f else paste0(f, ".csv") })
  p <- do.call(dl_proyecto, c(list(causa = 9100, configuracion = file.path(d, "config", "9100.yaml")), args))
  expect_identical(suppressMessages(dl_insumos(p))$hash, suppressMessages(dl_insumos(dl_proyecto(d, 9100)))$hash)
})

test_that("una tabla opcional con solo el encabezado es como si no estuviera", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  writeLines(readLines(file.path(d, "datos.csv"), n = 1L), file.path(d, "datos.csv"))
  p <- dl_proyecto(d, 9100)
  expect_null(p$tablas$datos)
})

test_that("un argumento que no es una tabla del contrato es un error con la lista", {
  expect_error(dl_proyecto(dl_ejemplo(), 9100, proxies = data.frame()), "ubicaciones, poblacion")
})

test_that("faltan las tablas obligatorias: el error dice dónde se buscaron", {
  d <- withr::local_tempdir()
  writeLines(c("causa: 1", "anio: 2023", "edad_inicio: 30"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "ubicaciones.csv")
})

test_that("las dos puertas, en un proyecto que no es el ejemplo: carpeta, data.frame y sin carpeta dan lo mismo", {
  d <- proyecto_ficticio()
  p1 <- dl_proyecto(d)
  expect_identical(p1$formato, "simple")
  expect_s3_class(p1$tablas$poblacion, "dl_tabla")
  expect_setequal(names(p1$tablas), c("ubicaciones", "poblacion", "ancla", "severidad"))
  b1 <- suppressMessages(dl_insumos(p1))
  expect_identical(b1$contrato, p1$tablas)                         # las tablas del contrato, fuera del hash
  # una tabla como data.frame en lugar de su archivo
  t <- tablas_de(d, c("ubicaciones", "poblacion", "ancla", "severidad"))
  unlink(file.path(d, "poblacion.csv"))
  p2 <- dl_proyecto(d, poblacion = t$poblacion)
  expect_identical(suppressMessages(dl_insumos(p2))$hash, b1$hash)
  expect_identical(suppressMessages(dl_insumos(p1))$hash, b1$hash)   # la traducción de p1 sigue ahí
  # sin carpeta: la configuración (ruta o lista) y todas las tablas como argumentos
  p3 <- do.call(dl_proyecto, c(list(configuracion = file.path(d, "config.yaml")), t))
  expect_identical(suppressMessages(dl_insumos(p3))$hash, b1$hash)
  p4 <- do.call(dl_proyecto, c(list(causa = 501, configuracion = list(causa = 501L, anio = 2020L, edad_inicio = 40L)),
                               t))
  expect_identical(suppressMessages(dl_insumos(p4))$hash, b1$hash)
  expect_null(p4$carpeta)
})

test_that("las tablas: argumentos que no son del contrato, sin nombre, rutas que no existen y tablas vacías", {
  d <- proyecto_ficticio()
  expect_error(dl_proyecto(d, proxies = data.frame()), "ubicaciones, poblacion, ancla.*no se reconoce: proxies")
  expect_error(dl_proyecto(d, 501, data.frame()), "por su nombre.*\\(sin nombre\\)")
  expect_error(dl_proyecto(d, datos = file.path(d, "no_existe.csv")), "datos.*no_existe.csv")
  expect_error(dl_proyecto(), "falta `carpeta`")
  # una opcional con solo el encabezado, o una carpeta sin filas, no existe
  writeLines("medida,ubicacion,sexo,edad_inicio,edad_fin,valor", file.path(d, "datos.csv"))
  dir.create(file.path(d, "betas"))
  writeLines("covariable,efecto_sobre,transformacion,beta", file.path(d, "betas", "b.csv"))
  p <- dl_proyecto(d)
  expect_null(p$tablas$datos)
  expect_null(p$tablas$betas)
  # una obligatoria vacía falta, con dónde se buscó
  writeLines(readLines(file.path(d, "poblacion.csv"), n = 1L), file.path(d, "poblacion.csv"))
  expect_error(dl_proyecto(d), "poblacion.*poblacion.csv.*poblacion/")
  # ubicaciones sin la columna padre es una tabla válida sola (todas sus filas serían nacionales), pero no cumple la
  # regla entre tablas que exige una sola nacional: dl_proyecto() se detiene con ella. Una tabla mal formada (la
  # población sin sexo) es el error de la tabla del contrato, con su origen
  t <- tablas_de(d, "ubicaciones")$ubicaciones
  t$padre <- NULL
  expect_error(dl_proyecto(d, poblacion = tablas_de(proyecto_ficticio(), "poblacion")$poblacion, ubicaciones = t),
               "ubicaciones: 4 filas van sin padre")
  pob <- tablas_de(proyecto_ficticio(), "poblacion")$poblacion
  pob$sexo <- NULL
  expect_error(dl_proyecto(d, poblacion = pob), "la tabla poblacion tiene .*faltan las columnas sexo")
})

test_that("dl_proyecto() se detiene con todos los problemas entre tablas y avisa de las sospechas", {
  args <- list(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30),
               ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = dl_ejemplo("poblacion.csv"),
               ancla = dl_ejemplo("ancla"))
  # dos filas sin padre en ubicaciones
  u <- leer_texto(dl_ejemplo("ubicaciones.csv"))
  u$padre[2L] <- ""
  e <- expect_error(do.call(dl_proyecto, utils::modifyList(args, list(ubicaciones = as.data.frame(u)))),
                    "entre tablas", class = "dl_error")
  expect_match(e$problemas, "^ubicaciones: 2 filas van sin padre \\(01, 123\\)", all = FALSE)
  # read.csv() convierte el código «01» en 1: las ubicaciones de la población no están en ubicaciones (antes el
  # proyecto corría en modo plano con subnacionales «1», «10»...)
  pob <- utils::read.csv(dl_ejemplo("poblacion.csv"))
  e <- expect_error(do.call(dl_proyecto, utils::modifyList(args, list(poblacion = pob))), class = "dl_error")
  expect_match(e$problemas, "^poblacion: la\\(s\\) ubicaci\u00f3n\\(es\\) 1, 2, 3, 4, 5 y 4 m\u00e1s no est", all = FALSE)
  # todos los problemas juntos, no solo el primero
  e <- expect_error(do.call(dl_proyecto, utils::modifyList(args, list(ubicaciones = as.data.frame(u),
                                                                      poblacion = pob))), class = "dl_error")
  expect_length(e$problemas, 2L)
  # una sospecha (mortalidad por 100 000 en datos) es un aviso, que no detiene
  datos <- leer_texto(dl_ejemplo("datos.csv"))
  datos[medida == "mortalidad", causa := "9101"]
  datos[medida == "mortalidad", valor := "5"]
  expect_warning(p <- do.call(dl_proyecto, c(args, list(datos = as.data.frame(datos)))), "parecen tasas por 100 000")
  expect_s3_class(p, "dl_proyecto")
})

test_that("ubicacion_gbd por defecto: el código nacional si está en la descarga, si no la única ubicación", {
  # el código nacional de ubicaciones no es el location_id de la descarga (999), sea texto o dígitos
  for (codigo in c("PE", "604")) {
    d <- proyecto_ficticio()
    u <- leer_texto(file.path(d, "ubicaciones.csv"))
    u[ubicacion == "999", ubicacion := codigo][padre == "999", padre := codigo]
    escribir_texto(u, d, "ubicaciones.csv")
    p <- dl_proyecto(d)
    expect_identical(as.character(p$configuracion$anchor$location_id), codigo)
    expect_identical(p$configuracion$origen$por_defecto[["ubicacion_gbd"]], "999")
    expect_gt(nrow(p$tablas$ancla), 0L)
  }
  # con otra ubicación en la descarga, el código nacional (604) no es ninguna de las dos: error que pide la clave
  a <- leer_texto(file.path(d, "ancla", "descarga_gbd.csv"))
  escribir_texto(rbind(a, data.table::copy(a)[, location_id := "1000"]), d, "ancla", "descarga_gbd.csv")
  expect_error(dl_proyecto(d), paste0("trae varias ubicaciones \\(1000, 999\\) y ninguna es el c\u00f3digo nacional ",
                                      "de ubicaciones \\(604\\): declara ubicacion_gbd"))
  # si el código nacional es una de ellas, es esa
  u[ubicacion == "604", ubicacion := "1000"][padre == "604", padre := "1000"]
  escribir_texto(u, d, "ubicaciones.csv")
  expect_identical(dl_proyecto(d)$configuracion$origen$por_defecto[["ubicacion_gbd"]], "1000")
  # declarada, manda y debe estar en la descarga
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "ubicacion_gbd: 999"), file.path(d, "config.yaml"))
  p <- dl_proyecto(d)
  expect_false("ubicacion_gbd" %in% names(p$configuracion$origen$por_defecto))
  expect_identical(as.character(p$configuracion$anchor$location_id), "1000")
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "ubicacion_gbd: 5"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "no trae la ubicaci\u00f3n 5 \\(ubicacion_gbd\\)")
  # sin descargas de GBD, ubicacion_gbd no se usa y no aparece entre lo tomado por defecto
  d <- proyecto_ficticio()
  f <- file.path(withr::local_tempdir(), "ancla.csv")
  data.table::fwrite(dl_tabla("ancla", file.path(d, "ancla")), f)
  p <- dl_proyecto(d, ancla = f)
  expect_false("ubicacion_gbd" %in% names(p$configuracion$origen$por_defecto))
})

test_that("las fuentes del GHDx van con el código nacional del proyecto y el doble conteo sigue deteniendo", {
  # el código nacional (PAIS) no es el location_id de GBD del país (999), que se declara: la lista del GHDx trae
  # también una ubicación subnacional de GBD
  d <- proyecto_ficticio(config = c("causa: 501", "anio: 2020", "edad_inicio: 40", "ubicacion_gbd: 999"))
  u <- leer_texto(file.path(d, "ubicaciones.csv"))
  u[ubicacion == "999", ubicacion := "PAIS"][padre == "999", padre := "PAIS"]
  escribir_texto(u, d, "ubicaciones.csv")
  # una fuente no fatal del país y otra de la ubicación subnacional de GBD
  dir.create(file.path(d, "fuentes_gbd"))
  writeLines(c("nid,title,cause_id,location_id,component_id", "1001,Encuesta A,501,999,5",
               "1002,Encuesta B,501,4567,5"), file.path(d, "fuentes_gbd", "ghdx.csv"))
  expect_warning(p <- dl_proyecto(d), "se descartan las fuentes de .ghdx.csv. de otras ubicaciones de GBD \\(4567\\)")
  expect_identical(p$tablas$fuentes_gbd$ubicacion, "PAIS")
  expect_identical(p$tablas$fuentes_gbd$nid, 1001L)
  # con el ancla a peso completo, una fuente local no fatal del país es doble conteo: dl_insumos() se detiene
  expect_error(suppressMessages(dl_insumos(p)), "exige 0 fuentes locales no fatales para la causa 501.*nid 1001")
})

test_that("la procedencia de lo que la configuración completa exige declarar nombra la configuración del proyecto", {
  cfg <- dl_proyecto(proyecto_ficticio())$configuracion
  expect_identical(cfg$edad_inicio_fuente, "declarado en la configuraci\u00f3n del proyecto")
  expect_identical(cfg$remision$fuente, "declarado en la configuraci\u00f3n del proyecto")
})

test_that("print() muestra las tablas del proyecto y lo tomado por defecto", {
  p <- dl_proyecto(proyecto_ficticio())
  salida <- paste(capture.output(print(p)), collapse = "\n")
  expect_match(salida, "Enfermedad ficticia, causa 501")
  expect_match(salida, "poblacion +.*fila")
  expect_match(salida, "betas +no")
  expect_match(salida, "subnacional: plano")
  expect_match(salida, "tomado por defecto")
})

test_that("un subtipo sin betas propias usa las de su causa padre (subtipos en la configuración del padre)", {
  d <- proyecto_ficticio()
  dir.create(file.path(d, "config"))
  file.rename(file.path(d, "config.yaml"), file.path(d, "config", "501.yaml"))
  cat("subtipos: [502]\n", file = file.path(d, "config", "501.yaml"), append = TRUE)
  writeLines(c("causa: 502", "anio: 2020", "edad_inicio: 40"), file.path(d, "config", "502.yaml"))
  data.table::fwrite(data.table::data.table(causa = 501L, covariable = "indice", efecto_sobre = "prevalencia",
                                            transformacion = "log", beta = 0.5), file.path(d, "betas.csv"))
  data.table::fwrite(data.table::data.table(covariable = "indice", valor = 50, inferior = 45, superior = 55),
                     file.path(d, "covariables.csv"))
  a <- data.table::fread(file.path(d, "ancla", "descarga_gbd.csv"))
  data.table::fwrite(rbind(a, data.table::copy(a)[, cause_id := 502L]), file.path(d, "ancla", "descarga_gbd.csv"))
  hija <- dl_proyecto(d, 502)
  expect_identical(hija$configuracion$origen$betas$covariable, "indice")
  expect_identical(hija$configuracion$extraction$cause_id, 501L)
  expect_identical(suppressMessages(dl_insumos(hija))$betas$covariate_name_short, "indice")
})

# Una corrida de partición de severidad mínima en `d` (particion/mini): la causa 302 de GBD con sus cuatro secuelas y
# sus estados de salud del catálogo del paquete (665 -> 355, 666 -> 356, 667 -> 357, 668 -> 540), con proporciones
# 0,6, 0,25, 0,1 y 0,05.
escribir_particion_mini <- function(d) {
  base <- data.table::data.table(run_id = "mini", source = "gbd", round = 2023L, method = "severity_split",
                                 location_id = "999", location_name = "Pa\u00eds ficticio", location_level = 0L,
                                 year = 2020L, age_group_id = 22L, age_group_name = "All ages", sex_id = 3L,
                                 sex_name = "Both", cause_id = 302L, cause_name = "diarrheal_diseases",
                                 measure_id = 900001L, measure_name = "Proportion", metric_id = 2L, metric_name = "Percent",
                                 val = c(0.6, 0.25, 0.1, 0.05), ui_level = 0.95)[, `:=`(lower = val * 0.9, upper = val * 1.1)]
  for (e in c("cause_sequela", "cause_health_state")) {
    x <- data.table::copy(base)[, entity := e]
    if (e == "cause_sequela") x[, `:=`(sequela_id = 665:668, sequela_name = paste0("s", 665:668))]
    else x[, `:=`(health_state_id = c(355L, 356L, 357L, 540L), health_state_name = paste0("e", 1:4))]
    dir.create(file.path(d, "particion", "mini", e, "proportion"), recursive = TRUE)
    data.table::fwrite(x, file.path(d, "particion", "mini", e, "proportion", "mini.csv"))
  }
  invisible(d)
}

test_that("severidad.particion: la severidad sale de la partición y componente.secuelas escala la prevalencia", {
  config <- c("causa: 302", "anio: 2020", "edad_inicio: 40", "severidad:", "  particion: particion/mini")
  d <- escribir_particion_mini(escribir_pais_ficticio(file.path(withr::local_tempdir(), "pf"), 302L, 2020L, config))
  unlink(file.path(d, "severidad.csv"))
  p <- dl_proyecto(d)
  expect_identical(p$rutas$severity_split, normalizePath(file.path(d, "particion", "mini"), winslash = "/"))
  b <- suppressMessages(dl_insumos(p))
  expect_setequal(b$severidad$health_state_id, c(355L, 356L, 357L, 540L))
  expect_null(b$componente)
  # el componente de las secuelas 665 y 666: su severidad y la prevalencia del ancla por 0,85
  writeLines(c(config, "componente:", "  secuelas: [665, 666]"), file.path(d, "config.yaml"))
  pc <- dl_proyecto(d)
  bc <- suppressMessages(dl_insumos(pc))
  expect_setequal(bc$severidad$health_state_id, c(355L, 356L))
  expect_equal(bc$componente$fraccion_prevalencia, 0.85)
  prev <- function(x) x$prior_gbd[measure_id == 5L][order(sex_id, age_group_id)]$val
  expect_equal(prev(bc), prev(b) * 0.85)
})

test_that("dos proyectos con la configuración como lista no se borran la traducción; el aviso dice qué hacer", {
  d <- proyecto_ficticio()
  t <- tablas_de(d, c("ubicaciones", "poblacion", "ancla", "severidad"))
  p1 <- do.call(dl_proyecto, c(list(configuracion = list(causa = 501L, anio = 2020L, edad_inicio = 40L)), t))
  p2 <- do.call(dl_proyecto, c(list(configuracion = list(causa = 501L, anio = 2020L, edad_inicio = 40L,
                                                         ancla = list(peso = 0.5))), t))
  expect_s3_class(suppressMessages(dl_insumos(p1)), "dl_bundle")
  expect_s3_class(suppressMessages(dl_insumos(p2)), "dl_bundle")
  expect_false(identical(p1$rutas$poblacion, p2$rutas$poblacion))
  unlink(dirname(p1$rutas$poblacion), recursive = TRUE)
  expect_error(dl_insumos(p1), paste0("ya no est\u00e1 .*vuelve a llamar a dl_proyecto\\(\\) con la misma ",
                                      "configuraci\u00f3n y las mismas tablas"))
})

# ---- La traducción y su caché ----

test_that("la traducción se reutiliza mientras no cambia lo que traduce; la nueva borra la anterior", {
  d <- proyecto_ficticio()
  p <- dl_proyecto(d)
  r <- p$rutas
  # el mismo contenido con otra fecha: la misma traducción
  Sys.setFileTime(file.path(d, "severidad.csv"), Sys.time() + 60)
  expect_identical(dl_proyecto(d)$rutas, r)
  # otro contenido de una tabla: otra traducción, y la anterior se borra
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  pob$poblacion[1] <- "999999"
  escribir_texto(pob, d, "poblacion.csv")
  r2 <- dl_proyecto(d)$rutas
  expect_false(identical(r2$poblacion, r$poblacion))
  expect_false(file.exists(r$poblacion))
  expect_true("999999" %in% leer_texto(r2$poblacion)$val)
  # un proyecto leído antes de ese cambio: sus insumos piden volver a leerlo
  expect_error(dl_insumos(p), "la traducción de este proyecto ya no está .*vuelve a llamar a dl_proyecto\\(")
})

test_that("la traducción escribe las covariables nacionales, las fuentes y las bandas que no son de GBD", {
  d <- proyecto_ficticio()
  data.table::fwrite(data.table::data.table(covariable = "indice", efecto_sobre = "prevalencia",
                                            transformacion = "log", beta = 0.5), file.path(d, "betas.csv"))
  data.table::fwrite(data.table::data.table(covariable = "indice", covariable_id = 5001L, valor = 50, inferior = 45,
                                            superior = 55), file.path(d, "covariables.csv"))
  data.table::fwrite(data.table::data.table(componente = "no_fatal", nid = 123456L), file.path(d, "fuentes_gbd.csv"))
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "ancla:", "  peso: 0.5"), file.path(d, "config.yaml"))
  p <- dl_proyecto(d)
  ghdx <- leer_texto(list.files(p$rutas$ghdx_cov, full.names = TRUE))
  expect_identical(ghdx$covariate_id, "5001")
  expect_identical(ghdx$location_id, "999")
  expect_identical(leer_texto(file.path(p$rutas$ghdx_store, "list.csv"))$nid, "123456")
  b <- suppressMessages(dl_insumos(p))
  expect_identical(b$cov_valores$val, 50)
  expect_identical(b$fuentes_locales$nid_no_fatal, 123456L)
  # una población con bandas que no son de GBD (40-49, 50-59, ...): sus ids sintéticos van al catálogo de edades
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  pob[, `:=`(edad_inicio = as.character(10 * (as.numeric(edad_inicio) %/% 10)))]
  pob[, edad_fin := ifelse(edad_inicio == "80", "125", as.character(as.numeric(edad_inicio) + 10))]
  pob <- pob[, list(poblacion = as.character(sum(as.numeric(poblacion)))),
             by = list(location_id, anio, sexo, edad_inicio, edad_fin)]
  escribir_texto(pob, d, "poblacion.csv")
  det <- leer_texto(file.path(proyecto_ficticio(), "poblacion.csv"))
  det <- det[, list(poblacion = sum(as.numeric(poblacion))), by = list(anio, sexo, edad_inicio, edad_fin)]
  escribir_texto(det, d, "poblacion_detalle.csv")
  p <- dl_proyecto(d)
  cat_edades <- leer_texto(file.path(p$rutas$catalogos, .dl_schema_estimates()$catalogos$demograficos))
  expect_true(all(c("40-49 a\u00f1os", "70-79 a\u00f1os") %in% cat_edades$name))
  et <- leer_texto(file.path(p$rutas$registry, "etiquetas_es.csv"))
  expect_true("40-49 a\u00f1os" %in% et$name_es)
  b <- suppressMessages(dl_insumos(p))
  expect_setequal(unique(b$prior_gbd$age_group_id), unique(b$poblacion$age_group_id))
})

# ---- Errores de la configuración ----

config_9100 <- function() readLines(dl_ejemplo("config", "9100.yaml"), encoding = "UTF-8")

test_that("una clave desconocida de la configuración simple sugiere la más parecida", {
  d <- proyecto_ficticio(config = c("causa: 501", "anio: 2020", "edad_inicio: 40", "ancla:", "  pesos: 0.5"))
  expect_error(dl_proyecto(d), "ancla.pesos: clave desconocida; \u00bfquisiste decir `ancla.peso`\\?")
  # el error nombra la configuración del proyecto (no «simple»)
  expect_error(dl_proyecto(d), "la configuraci\u00f3n del proyecto \u00abconfig.yaml\u00bb tiene 1 problema")
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "remisio: 0"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "remisio: clave desconocida; \u00bfquisiste decir `remision`\\?")
  # los errores del validador del formato completo citan la clave simple y la completa
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "ancla:", "  peso: 5"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "ancla.peso \\(anchor.lambda\\): debe estar en \\(0, 1\\]")
  # una clave con punto escrita como la citan los mensajes: dice que va dentro de su bloque
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "ancla.peso: 0.5", "sensibilidad.kappa: [0.5, 1]"),
             file.path(d, "config.yaml"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_match(conditionMessage(e), "ancla.peso: va dentro de su bloque: ancla: \\{peso: 0.5\\}")
  expect_match(conditionMessage(e),
               "sensibilidad.kappa: va dentro de su bloque: sensibilidad: \\{kappa: \\[0.5, 1\\]\\}")
})

test_that("subnacional.modo: plano sin ubicaciones subnacionales es un error de la configuración", {
  d <- proyecto_ficticio()
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  pob <- pob[, list(location_id = "999", poblacion = sum(as.numeric(poblacion))),
             by = list(anio, sexo, edad_inicio, edad_fin)]
  escribir_texto(pob, d, "poblacion.csv")
  escribir_texto(data.table::data.table(ubicacion = "999", nombre = "Pa\u00eds ficticio"), d, "ubicaciones.csv")
  expect_identical(dl_proyecto(d)$configuracion$origen$subnacional, "no")
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "subnacional:", "  modo: plano"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "subnacional.modo: es plano y la tabla poblacion no trae ubicaciones subnacionales")
})

test_that("la población debe traer el año que se estima en cada sexo; un sexo desconocido lo detiene la tabla", {
  d <- proyecto_ficticio()
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  escribir_texto(pob[sexo != "mujeres"], d, "poblacion.csv")
  expect_error(dl_proyecto(d), "poblacion: no trae la poblaci\u00f3n de mujeres de 2020")
  escribir_texto(pob[1L, sexo := "M"], d, "poblacion.csv")
  expect_error(dl_proyecto(d), "sexo: M no es hombres, mujeres ni ambos")
})

test_that("config.yaml: su ruta vale como carpeta_config y cause_id sugiere causa", {
  d <- proyecto_ficticio()
  expect_identical(dl_configuracion(501, file.path(d, "config.yaml"))$cause_id, 501L)
  # la causa pedida la compara el validador del formato completo, citado con la clave simple
  expect_error(dl_configuracion(4711, file.path(d, "config.yaml")),
               "causa \\(cause_id\\): es 501 y se pidi\u00f3 la causa 4711")
  writeLines(c("cause_id: 501", "anio: 2020", "edad_inicio: 40"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "cause_id: clave desconocida; \u00bfquisiste decir `causa`\\?")
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "nudos_incidencia: [40, 60, 80, 95]"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "nudos_incidencia: clave desconocida; \u00bfquisiste decir `nudos`\\?")
  # la forma anterior, los nudos dentro de incidencia, dice dónde está ahora la clave
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "incidencia:", "  nudos: [40, 60, 80, 95]"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "incidencia.nudos: clave desconocida; `incidencia.nudos` ahora es `nudos`, en el primer nivel",
               fixed = TRUE)
  writeLines(c("anio: 2020", "edad_inicio: 40"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "config.yaml no declara `causa`")
  # la estimación por covariables sin betas es un error, no un ajuste nacional sin aviso
  writeLines(c("causa: 501", "anio: 2020", "edad_inicio: 40", "subnacional:", "  modo: covariables"),
             file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "subnacional.modo: es covariables y ninguna covariable de la tabla betas \\(ninguna\\)")
})

test_that("un config.yaml que no es una lista de claves es un error del paquete", {
  # una línea sin «:» se lee como un texto suelto; una lista con guiones, como una lista sin claves
  d <- withr::local_tempdir()
  detalle <- "config.yaml no es una lista de claves (clave: valor, una por l\u00ednea)"
  for (texto in list("causa 4711", c("causa 4711", "anio 2023"), c("- causa: 4711", "- anio: 2023"))) {
    writeLines(texto, file.path(d, "config.yaml"))
    expect_error(dl_proyecto(d), paste0("^dl_proyecto\\(\\): ", gsub("([()])", "\\\\\\1", detalle), "$"),
                 class = "dl_error")
    expect_error(dl_proyecto(d, causa = 4711), detalle, fixed = TRUE, class = "dl_error")
  }
  # una causa que no es un valor simple (una lista de claves) no se toma como la causa
  writeLines(c("causa: {a: [1, 2]}", "anio: 2023"), file.path(d, "config.yaml"))
  expect_error(dl_proyecto(d), "config.yaml no declara `causa`", class = "dl_error")
  # la configuración como argumento: un archivo que existe o una lista
  expect_error(dl_proyecto(configuracion = file.path(d, "no.yaml")), "`configuracion` debe ser la ruta de un archivo")
  expect_error(dl_proyecto(configuracion = list(anio = 2023)), "configuracion no declara `causa`")
})

test_that("cada clave simple llega a su destino de la tabla y los errores del destino vuelven a la clave", {
  t <- .dl_claves_simple()
  base <- list(causa = 501L, anio = 2020L, edad_inicio = 40L)
  ctx <- list(ubicacion = "999", nombre = "x", subnacional = FALSE)
  claves <- c("causa", "anio", "edad_inicio", "ancla.peso", "ancla.correlacion_edad", "ancla.anio", "nudos",
              "incidencia.suavidad", "remision", "subnacional.kappa", "subnacional.anio_validacion", "sensibilidad.peso",
              "sensibilidad.correlacion_edad", "sensibilidad.kappa")
  for (k in claves) {
    v <- switch(k, causa = 501L, nudos = c(40L, 60L, 95L),
                if (grepl("anio", k)) 2019L else if (t$tipo[t$clave == k] == "lista de n\u00fameros") c(0.2, 0.7)
                else 0.37)
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
  # el error de una clave se usa solo con su destino exacto y si la configuración da la clave: el de un campo dentro
  # del destino, o el de un valor de `avanzado` (con la forma del formato completo), es el del validador
  techo <- list(mortalidad_exceso = list(techo = 0))
  expect_match(.dl_problema_en_simple("emr_prior.cota", "x", techo), "techo \\(emr_prior.cota\\): debe ser un n")
  expect_identical(.dl_problema_en_simple("emr_prior.cota", "x", list()), "mortalidad_exceso.techo (emr_prior.cota): x")
  avanzado <- list(avanzado = list(emr_prior = list(cota = 5)))
  expect_identical(.dl_problema_en_simple("emr_prior.cota", "x", c(techo, avanzado)), "avanzado: emr_prior.cota: x")
  d <- proyecto_ficticio(config = c("causa: 501", "anio: 2020", "edad_inicio: 40", "avanzado:", "  cascada:",
                                    "    modo: {valor: x, procedencia: y}", "  anchor:",
                                    "    gate_err_mediano: {valor: 0.08}"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_match(conditionMessage(e), "- avanzado: cascada.modo: valores admitidos: proxy")
  expect_match(conditionMessage(e), "- avanzado: anchor.gate_err_mediano.procedencia: ")
  expect_identical(.dl_problema_en_simple("cascada.heldout_anio.procedencia", "falta", list()),
                   "cascada.heldout_anio.procedencia: falta")
})

test_that("los mensajes del formato completo, en las palabras de la configuración y de las tablas del contrato", {
  expect_identical(.dl_texto_simple(c("la tabla cov_proxy", "cascada.heldout_anio.valor", "anchor.lambda = 1",
                                      "datos.val: NA en columna", "cov_proxy.valor_calibrado_se vac\u00edo",
                                      "  - datos: pareja_val_se_o_x_n \u2014 sin valor: fila_1",
                                      "el tipo de dato csmr", "la tabla poblacion no trae la poblaci\u00f3n",
                                      "corrida escrita en /Users/ana/decisiones/datos.val/csmr/resultados")),
                   c("la tabla covariables", "subnacional.anio_validacion", "ancla.peso = 1",
                     "datos, columna valor: NA en columna", "covariables, columna error_estandar vac\u00edo",
                     "  - datos: sin valor: fila_1", "el tipo de dato mortalidad",
                     "la tabla poblacion no trae la poblaci\u00f3n",
                     "corrida escrita en /Users/ana/decisiones/datos.val/csmr/resultados"))
})

# ---- Pruebas sobre el ejemplo (se migra en la Tarea 9) ----

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
  expect_identical(dl_proyecto(dl_ejemplo(), 9100)$rutas, p$rutas)
  b <- suppressMessages(dl_insumos(p))
  expect_s3_class(b, "dl_bundle")
  expect_identical(b$rutas, p$rutas)
  # el formato completo también es un proyecto
  pc <- dl_proyecto(system.file("extdata", "acs_peru_completo", package = "dismodlite"), 9100)
  expect_identical(pc$formato, "completo")
  expect_null(pc$tablas)
  expect_identical(pc$rutas, dl_rutas_ejemplo(9100L, formato = "completo"))
})

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
    campos_tr <- function(x) lapply(x$transformaciones, function(t) t[c("covariate_name_short", "transformacion")])
    expect_identical(campos_tr(s), campos_tr(c))
  }
  pd <- dl_configuracion_ejemplo(9100L)$origen$por_defecto
  expect_identical(pd[["ancla.peso"]], "1")
  expect_identical(pd[["nudos"]], "[30, 40, 50, 60, 70, 80, 95]")
})

test_that("una medida desconocida en datos_en_ajuste lista las admitidas; prevalencia_registro no entra al ajuste", {
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "datos_en_ajuste: [prevalencia_registro]"))
  expect_error(dl_proyecto(d, 9100), "datos_en_ajuste: valor\\(es\\) no admitido\\(s\\): prevalencia_registro")
})

test_that("con datos locales en el ajuste y ancla.peso 1, dl_insumos() avisa del doble conteo en palabras simples", {
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(),
                                             "datos_en_ajuste: [prevalencia_estudio, incidencia, mortalidad]"))
  expect_warning(suppressMessages(dl_insumos(dl_proyecto(d, 9100))),
                 "\\(datos_en_ajuste\\) con ancla.peso = 1.*Considera ancla.peso: 0.5 y anota el motivo en `notas`")
})

test_that("con un proyecto, los mensajes de dl_insumos() citan sus claves y sus tablas", {
  d <- copia_ejemplo(`config/9100.yaml` = c(config_9100(), "ancla:", "  peso: 0.5", "datos_en_ajuste: [incidencia]"))
  e <- expect_error(suppressMessages(dl_insumos(dl_proyecto(d, 9100))), class = "dl_error")
  expect_match(conditionMessage(e), "datos_en_ajuste")
  expect_no_match(conditionMessage(e), "csmr|medidas_entrada|outlier|prev_estudio|tipos_declarados")
  # sin un proyecto (la configuración y las rutas por separado), los mensajes quedan como están
  p <- dl_proyecto(d, 9100)
  expect_error(suppressMessages(dl_insumos(p$configuracion, p$rutas)), "datos: tipos_declarados .* medidas_entrada")
})

test_that("claves simples: `modo: no` sin comillas, sexos como conjunto y el nombre anterior de subnacional", {
  c9101 <- function() readLines(dl_ejemplo("config", "9101.yaml"), encoding = "UTF-8")
  d <- copia_ejemplo(`config/9101.yaml` = sub("^subnacional:$", "subnacional:\n  modo: no", c9101()))
  p <- dl_proyecto(d, 9101)
  expect_identical(p$configuracion$origen$subnacional, "no")
  expect_identical(length(p$configuracion$covariables), 0L)
  cfg <- sub("^nombre: .*$", "nombre: off", c9101())
  expect_identical(dl_configuracion(9101, copia_ejemplo(`config/9101.yaml` = cfg))$origen$nombre, "off")
  d <- copia_ejemplo(`config/9101.yaml` = c(c9101(), "sexos: [mujeres, hombres]"))
  expect_identical(dl_proyecto(d, 9101)$configuracion$sexos, c(1L, 2L))
  d <- copia_ejemplo(`config/9101.yaml` = sub("^subnacional:$", "departamentos:\n  modo: plano", c9101()))
  expect_identical(dl_proyecto(d, 9101)$configuracion$origen$subnacional, "plano")
  d <- copia_ejemplo(`config/9101.yaml` = c(c9101(), "departamentos:", "  kappa: 0.5"))
  expect_error(dl_proyecto(d, 9101), "departamentos: es el nombre anterior de `subnacional`")
})

test_that("dominios de la configuración: los revisa el validador del formato completo, citados con la clave simple", {
  d <- proyecto_ficticio(config = c("causa: 501", "anio: 2020", "edad_inicio: 40", "mortalidad_exceso:",
                                    "  prior: plano", "  techo: -1", "incidencia:", "  suavidad: -1",
                                    "nudos: [40, 20, 95]", "remision: -0.5"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_match(conditionMessage(e), "incidencia.suavidad \\(sigma_suavidad\\): debe ser un n\u00famero > 0")
  expect_match(conditionMessage(e), "nudos \\(nudos_incidencia\\): deben ser edades crecientes")
  expect_match(conditionMessage(e), "remision \\(remision.valor\\): debe ser un n\u00famero >= 0")
  expect_match(conditionMessage(e),
               "mortalidad_exceso.techo \\(emr_prior.cota\\): debe ser un n\u00famero > 0, por persona-a\u00f1o")
})

# ---- Un proyecto que no es del Perú ----

test_that("un proyecto de otro país, con códigos subnacionales libres, corre el ajuste, la cascada y los AVD", {
  p <- dl_proyecto(proyecto_ficticio())
  expect_identical(.dl_loc_ancla(p$configuracion), "999")
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

# ---- Utilidades ----

test_that("el ubigeo de dos dígitos de los catálogos sigue la regla de la configuración, no la carpeta", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "catalogos"))
  data.table::fwrite(data.table::data.table(location_id = c("999", "A"), location_name = c("Pa\u00eds", "Regi\u00f3n A"),
                                            location_level = 0:1, parent_id = c(NA, "999")),
                     file.path(d, "catalogos", .dl_schema_estimates()$catalogos$locations))
  r <- dl_rutas(catalogos = file.path(d, "catalogos"))
  expect_error(.dl_catalogo(r, "locations"), "no son de dos d\u00edgitos")
  libres <- .dl_rutas_con_codigos(r, list(anchor = list(location_id = 999L)))
  expect_identical(.dl_catalogo(libres, "locations")$location_id, c("999", "A"))
  expect_identical(.dl_rutas_con_codigos(r, list(anchor = list(location = "peru"))), r)
  expect_error(.dl_catalogo(r, "locations"), "no son de dos d\u00edgitos")
})

test_that("las betas y las prevalencias convertidas se escriben con el texto que vuelve al mismo número", {
  expect_identical(.dl_num_exacto(c(0.62, -0.012)), c("0.62", "-0.012"))
  x <- c(1 / 3, 0.1 + 0.2, 0.62000000000000011, 2 / 3 * 1e-5)
  expect_identical(as.numeric(.dl_num_exacto(x)), x)
  # una tasa de GBD / 100 000 sin texto decimal que vuelva a ella cuando R no tiene long double (arm64): el texto
  # hexadecimal; as.numeric() y fread() lo leen igual, también en una columna con decimales
  y <- c(0.62, as.numeric(c("40.9715235289338", "1229.0265041539", "120.315679245624")) / 1e5)
  t <- .dl_num_exacto(y)
  expect_identical(as.numeric(t), y)
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("x", t), f)
  expect_identical(data.table::fread(f)$x, y)
  expect_identical(.dl_num_exacto(0x1.ad9e18947113fp-12) %in% c("0.00040971523528933804", "0x1.ad9e18947113fp-12"),
                   TRUE)
})

# ---- La configuración del proyecto con el contrato de insumos ----

test_that("la configuración del proyecto ya no tiene covariables y avisa del nombre nuevo de ubicacion_nacional", {
  probs <- dismodlite:::.dl_problemas_config_simple(list(causa = 1L, anio = 2023L, edad_inicio = 30,
                                                         ubicacion_nacional = 123, covariables = list()))
  expect_true(any(grepl("ubicacion_nacional.*ubicacion_gbd", probs)))
  expect_true(any(grepl("covariables.*clave desconocida", probs)))
})

test_that("las betas del contexto se traducen a transformaciones y proxies", {
  betas <- data.table::data.table(covariable = c("sev", "haqi"), efecto_sobre = c("prevalencia", "mortalidad_exceso"),
                                  transformacion = c("log", "lineal"), escala = c(NA, 0.01), beta = c(0.6, -0.01))
  cfg <- dismodlite:::.dl_traducir_config_simple(
    list(causa = 9100L, anio = 2023L, edad_inicio = 30), "config.yaml",
    list(ubicacion = "PAIS", betas = betas, covariables_subnacionales = "haqi", subnacional = TRUE, nombre = "x"))
  expect_identical(vapply(cfg$transformaciones, `[[`, "", "covariate_name_short"), c("sev", "haqi"))
  expect_identical(cfg$transformaciones[[2]]$escala, 0.01)
  expect_identical(cfg$covariables[[1]]$proxy$covariate_id_proxy, 900102L)
  expect_identical(cfg$anchor$location_id, "PAIS")
  expect_false(cfg$anchor$agrupar_bandas_finas)
  expect_identical(cfg$origen$unidades, "contrato")
  expect_identical(cfg$origen$betas, betas)
})

test_that("severidad.particion y componente.secuelas llegan al formato completo", {
  cfg <- dismodlite:::.dl_traducir_config_simple(
    list(causa = 9101L, anio = 2023L, edad_inicio = 30,
         severidad = list(particion = "particion/acs_v1", padre = 9100L), componente = list(secuelas = c(1L, 2L))),
    "config.yaml", list(ubicacion = "PAIS", betas = NULL, covariables_subnacionales = character(),
                        subnacional = FALSE, nombre = "x"))
  expect_identical(cfg$severidad$fuente, "mod")
  expect_identical(cfg$severidad$padre, 9100L)
  expect_identical(cfg$severidad$run_id, "acs_v1")
  expect_identical(cfg$anchor$componente$sequela_ids, c(1L, 2L))
  expect_true(dismodlite:::.dl_es_simple(cfg))
  expect_length(dismodlite:::.dl_validar_config(cfg, 9101L)$problemas, 0L)   # el validador completo la acepta
})

test_that("las claves nuevas de la configuración del proyecto se revisan: ubicacion_gbd, severidad, componente y los datos", {
  ok <- list(causa = 1L, anio = 2023L, edad_inicio = 30, ubicacion_gbd = 130, datos_en_ajuste = c("prevalencia", "mortalidad"),
             severidad = list(particion = "particion/x", padre = 1010L), componente = list(secuelas = c(5001L, 5002L)))
  expect_identical(dismodlite:::.dl_problemas_config_simple(ok), character())
  ok$datos_en_ajuste <- "prevalencia_estudio"
  expect_identical(dismodlite:::.dl_problemas_config_simple(ok), character())
  expect_identical(dismodlite:::.dl_vocabulario_simple("datos_en_ajuste")[["prevalencia_estudio"]],
                   dismodlite:::.dl_vocabulario_simple("datos_en_ajuste")[["prevalencia"]])
  probs <- dismodlite:::.dl_problemas_config_simple(list(causa = 1L, anio = 2023L, edad_inicio = 30,
    severidad = list(padre = "uno"), componente = list(secuelas = "a")))
  expect_true(any(grepl("severidad.padre: debe ser un entero", probs)))
  expect_true(any(grepl("componente.secuelas: debe ser una lista de enteros", probs)))
})

test_that("valor_nacional_de se traduce a la sustitución del valor nacional del proxy", {
  betas <- data.table::data.table(covariable = c("sev_edad", "sev"), efecto_sobre = "prevalencia",
                                  transformacion = "log", escala = NA_real_, beta = 0.6,
                                  valor_nacional_de = c("sev", NA))
  cfg <- dismodlite:::.dl_traducir_config_simple(
    list(causa = 9100L, anio = 2023L, edad_inicio = 30), "config.yaml",
    list(ubicacion = "PAIS", betas = betas, covariables_subnacionales = c("sev_edad", "sev"), subnacional = TRUE,
         nombre = "x", ids_covariable = list(sev_edad = 11L, sev = 12L)))
  sus <- cfg$covariables[[1]]$sustituye
  expect_identical(sus$covariate_id, 12L)
  expect_identical(sus$covariate_name_short, "sev")
  expect_null(cfg$covariables[[2]]$sustituye)
})
