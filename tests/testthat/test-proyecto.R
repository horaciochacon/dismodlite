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
                                        "severidad", "proxies_crudos")),
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
  # una fuente no fatal del país (repetida, como en la lista real del GHDx) y otra de la ubicación subnacional de GBD
  dir.create(file.path(d, "fuentes_gbd"))
  writeLines(c("nid,title,cause_id,location_id,component_id", "1001,Encuesta A,501,999,5",
               "1001,Encuesta A (ronda 2),501,999,5", "1002,Encuesta B,501,4567,5"),
             file.path(d, "fuentes_gbd", "ghdx.csv"))
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

test_that("las filas de datos en los mensajes de dl_insumos() se cuentan como en los de las tablas", {
  x <- leer_texto(dl_ejemplo("datos.csv"))
  k <- which(x$medida == "prevalencia" & x$ubicacion == "123")[2L]
  x$error_estandar[k] <- "0"
  f <- file.path(withr::local_tempdir(), "datos.csv")
  escribir_texto(x, f)
  args <- list(configuracion = list(causa = 9100, anio = 2023, edad_inicio = 30, datos_en_ajuste = "prevalencia",
                                    ancla = list(peso = 0.5)),
               ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = dl_ejemplo("poblacion.csv"),
               ancla = dl_ejemplo("ancla"))
  insumos <- function(datos) suppressMessages(dl_insumos(do.call(dl_proyecto, c(args, list(datos = datos)))))
  # de un CSV, la línea del archivo (el encabezado es la 1); de un data.frame, su fila
  e <- expect_error(insumos(f), class = "dl_error")
  expect_match(conditionMessage(e), sprintf("error est\u00e1ndar que no es un n\u00famero positivo: fila\\(s\\) %d$", k + 1L))
  expect_false(grepl("fila_", conditionMessage(e)))
  e <- expect_error(insumos(as.data.frame(x)), class = "dl_error")
  expect_match(conditionMessage(e), sprintf("positivo: fila\\(s\\) %d del data.frame$", k))
  expect_match(e$problemas, sprintf("fila\\(s\\) %d del data.frame$", k))
})

test_that("print() muestra las tablas del proyecto y lo tomado por defecto", {
  p <- dl_proyecto(proyecto_ficticio())
  salida <- paste(capture.output(print(p)), collapse = "\n")
  expect_match(salida, "Enfermedad ficticia, causa 501")
  expect_match(salida, "poblacion +.*fila")
  expect_match(salida, "betas +no")
  expect_match(salida, "subnacional: plano")
  expect_match(salida, "tomado por defecto")
  expect_false(grepl("proxies calibrados", salida))
  # con proxies_crudos, la calibración: por covariable, el método, q y las ediciones
  salida <- capture.output(print(dl_proyecto(dl_ejemplo(), 9100)))
  i <- grep("^  proxies calibrados de proxies_crudos \\(año 2023\\):$", salida)
  expect_length(i, 1L)
  expect_match(salida[i + 1:3], paste0("^    (SEV_scalar_agestd_cvd_pvd|LDI_pc|haqi): paseo_aleatorio, ",
                                       "q = [0-9.e-]+, ediciones 2019, 2021, 2023$"))
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

# Un subtipo (502) en su propia carpeta, sin la configuración de su padre (501): declara la causa de sus betas en
# avanzado.extraction (o en subtipo_de, con `clave_corta`); la tabla betas trae una beta de la causa `causa_betas`.
# `config`: líneas que se agregan.
subtipo_en_su_carpeta <- function(causa_betas, config = character(), env = parent.frame(), clave_corta = FALSE) {
  padre <- if (clave_corta) "subtipo_de: 501"
           else c("avanzado:", "  extraction:", "    cause_id: 501", "    motivo: subtipo de la causa 501")
  d <- escribir_pais_ficticio(file.path(withr::local_tempdir(.local_envir = env), "subtipo"), 502L, 2020L,
                              c("causa: 502", "anio: 2020", "edad_inicio: 40", config, padre))
  data.table::fwrite(data.table::data.table(causa = causa_betas, covariable = "indice", efecto_sobre = "prevalencia",
                                            transformacion = "log", beta = 0.5), file.path(d, "betas.csv"))
  data.table::fwrite(data.table::data.table(ubicacion = c("999", "A", "B", "C"), covariable = "indice",
                                            valor = 50)[, `:=`(inferior = valor * 0.9,
                                                                               superior = valor * 1.1)],
                     file.path(d, "covariables.csv"))
  d
}

test_that("un subtipo en su propia carpeta usa las betas de la causa de avanzado.extraction si no tiene las suyas", {
  hija <- dl_proyecto(subtipo_en_su_carpeta(501L))
  expect_identical(hija$configuracion$origen$betas$covariable, "indice")
  expect_identical(hija$configuracion$extraction$cause_id, 501L)
  expect_identical(hija$configuracion$extraction$motivo, "subtipo de la causa 501")
  b <- suppressMessages(dl_insumos(hija))
  expect_identical(b$betas$covariate_name_short, "indice")
  # lo mismo que con las betas bajo el subtipo
  propia <- suppressMessages(dl_insumos(dl_proyecto(subtipo_en_su_carpeta(502L))))
  expect_identical(b$hash, propia$hash)
})

test_that("un subtipo sin betas propias ni de la causa de avanzado.extraction: el error dice qué hacer", {
  d <- subtipo_en_su_carpeta(777L, c("subnacional:", "  modo: covariables"))
  expect_error(dl_proyecto(d), paste0("la tabla betas no trae filas de la causa 502 ni de la causa 501, de la que ",
                                      "toma las betas \\(trae las de 777\\).*declárala en subtipo_de"))
})

test_that("subtipo_de: un subtipo en su propia carpeta o dado en memoria usa las betas de su causa padre", {
  d <- subtipo_en_su_carpeta(501L, clave_corta = TRUE)
  hija <- dl_proyecto(d)
  expect_identical(hija$configuracion$extraction$cause_id, 501L)
  expect_identical(hija$configuracion$extraction$motivo, "subtipo de la causa 501")
  b <- suppressMessages(dl_insumos(hija))
  expect_identical(b$betas$covariate_name_short, "indice")
  # lo mismo que declararlo bajo avanzado
  expect_identical(b$hash, suppressMessages(dl_insumos(dl_proyecto(subtipo_en_su_carpeta(501L))))$hash)
  # sin carpeta, con las tablas en memoria, también las encuentra
  mem <- dl_proyecto(configuracion = list(causa = 502, anio = 2020, edad_inicio = 40, subtipo_de = 501),
                     ubicaciones = tablas_de(d, "ubicaciones")$ubicaciones, poblacion = tablas_de(d, "poblacion")$poblacion,
                     ancla = file.path(d, "ancla"), betas = tablas_de(d, "betas")$betas,
                     covariables = tablas_de(d, "covariables")$covariables)
  expect_identical(mem$configuracion$origen$betas$covariable, "indice")
  expect_identical(suppressMessages(dl_insumos(mem))$betas$covariate_name_short, "indice")
  # sin las betas de la causa ni las de la padre, el error nombra subtipo_de
  expect_error(dl_proyecto(subtipo_en_su_carpeta(777L, c("subnacional:", "  modo: covariables"), clave_corta = TRUE)),
               "ni de la causa 501, de la que toma las betas.*declárala en subtipo_de")
})

test_that("subtipo_de y avanzado.extraction a la vez: manda avanzado", {
  d <- subtipo_en_su_carpeta(501L, c("subtipo_de: 777"))
  expect_identical(dl_proyecto(d)$configuracion$extraction$cause_id, 501L)
})

test_that("subtipo_de y el subtipos de la configuración del padre: la misma causa, o un error que nombra las dos", {
  copia <- function(subtipo_de, env = parent.frame()) {
    s <- readLines(dl_ejemplo("config", "9101.yaml"), encoding = "UTF-8")
    copia_ejemplo(`config/9101.yaml` = c(s, sprintf("subtipo_de: %d", subtipo_de)), env = env)
  }
  p <- dl_proyecto(copia(9100L), 9101)
  expect_identical(p$configuracion$extraction$cause_id, 9100L)
  e <- expect_error(dl_proyecto(copia(9102L), 9101), class = "dl_error")
  expect_match(conditionMessage(e),
               "subtipo_de: es 9102 y la configuración de la causa 9100 declara a la causa 9101 en su clave `subtipos`")
  # la revisión lo dice en el paso de la configuración
  r <- suppressMessages(utils::capture.output(rev <- dl_revisar_proyecto(copia(9102L), causa = 9101)))
  expect_match(rev$detalle[rev$paso == "configuración"], "subtipo_de: es 9102 y la configuración de la causa 9100")
  # la propia causa no es su padre
  expect_match(dismodlite:::.dl_problemas_config_simple(list(causa = 9101L, anio = 2023L, edad_inicio = 30,
                                                             subtipo_de = 9101L)),
               "^subtipo_de: es la propia causa \\(9101\\)")
})

# ---- Una causa que es la suma de sus subtipos (suma_de_subtipos) ----

test_that("sí o no se reconoce igual en cualquier configuración regional", {
  formas <- list("sí", "Sí", "SÍ", "sÍ", "si", "Si", "SI", " sí ", "no", "No", "NO", TRUE, FALSE,
                 "quizá", "s", "", 1L, NULL)
  esperado <- c(as.list(rep(c(TRUE, FALSE, TRUE, FALSE), c(8L, 3L, 1L, 1L))), list(NULL, NULL, NULL, NULL, NULL))
  expect_identical(lapply(formas, .dl_si_no), esperado)
  # en una sesión en C, tolower() no pasa «Í» a minúscula
  withr::with_locale(c(LC_CTYPE = "C"), expect_identical(lapply(formas, .dl_si_no), esperado))
  expect_identical(.dl_si_no("S\u00cd"), TRUE)
})

test_that("suma_de_subtipos y subtipos_omitidos: la forma, con errores que nombran la clave", {
  base <- list(causa = 9200L, nombre = "Suma", anio = 2023L)
  forma <- function(...) .dl_problemas_config_simple(c(base, list(...)))
  # una suma no necesita edad_inicio ni las claves del modelo; sí o no, también con true y false
  for (v in list("sí", "si", "SÍ", TRUE)) expect_length(forma(subtipos = 9101:9102, suma_de_subtipos = v), 0L)
  for (v in list("no", FALSE))
    expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = v), "^edad_inicio: falta \\(obligatoria\\)")
  # un valor que no es sí ni no: solo ese problema, sin exigir las claves del modelo
  expect_identical(forma(subtipos = 9101:9102, suma_de_subtipos = "quizá"), "suma_de_subtipos: debe ser sí o no")
  expect_setequal(sub(":.*$", "", .dl_problemas_config_simple(list(subtipos = 9101:9102, suma_de_subtipos = "quizá"))),
                  c("suma_de_subtipos", "causa", "anio"))
  expect_match(forma(suma_de_subtipos = "sí"), "^suma_de_subtipos: exige `subtipos`")
  sin_nombre <- .dl_problemas_config_simple(list(causa = 9200L, anio = 2023L, subtipos = 9101:9102,
                                                 suma_de_subtipos = "sí"))
  expect_match(sin_nombre, "^suma_de_subtipos: exige `nombre`")
  omitido <- list(list(causa = 9102L, motivo = "sin modelo"))
  expect_length(forma(subtipos = 9101:9102, suma_de_subtipos = "sí", subtipos_omitidos = omitido), 0L)
  expect_match(forma(edad_inicio = 30, subtipos = 9101:9102, subtipos_omitidos = omitido),
               "^subtipos_omitidos: exige suma_de_subtipos: sí")
  expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = "sí",
                     subtipos_omitidos = list(list(causa = 9999L, motivo = "x"))),
               "^subtipos_omitidos\\[1\\].causa: la causa 9999 no está en `subtipos` \\(9101, 9102\\)")
  expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = "sí", subtipos_omitidos = list(list(causa = 9102L))),
               "^subtipos_omitidos\\[1\\].motivo: falta \\(obligatoria\\)")
  expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = "sí",
                     subtipos_omitidos = list(list(causa = "x", motivo = "m"))),
               "^subtipos_omitidos\\[1\\].causa: debe ser un entero")
  expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = "sí",
                     subtipos_omitidos = list(list(causa = 9101L, motivo = "a"), list(causa = 9101L, motivo = "b"))),
               "^subtipos_omitidos: causa repetido: 9101")
  expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = "sí",
                     subtipos_omitidos = list(list(causa = 9101L, motivo = "a"), list(causa = 9102L, motivo = "b"))),
               "^subtipos_omitidos: omite todos los `subtipos`")
  expect_match(forma(subtipos = 9101:9102, suma_de_subtipos = "sí", subtipos_omitidos = "9102"),
               "^subtipos_omitidos: es una lista de registros, cada uno con las claves causa, motivo")
})

test_that("dl_proyecto() de una suma solo lee ubicaciones y poblacion, y su print() dice qué suma", {
  d <- copia_con_suma()
  p <- dl_proyecto(d, 9200)
  expect_s3_class(p, "dl_proyecto")
  expect_true(.dl_es_suma(p))
  expect_true(.dl_es_suma(p$configuracion))
  expect_false(.dl_es_suma(dl_proyecto(d, 9101)))
  expect_identical(names(p$tablas), c("ubicaciones", "poblacion"))
  cfg <- p$configuracion
  expect_s3_class(cfg, "dl_config")
  expect_identical(cfg$cause_id, 9200L)
  expect_identical(.dl_anio_ajuste(cfg), 2023L)
  expect_null(cfg$suma)
  expect_identical(cfg$origen$nombre, "Suma sintética de subtipos")
  # las rutas: la población, los catálogos y el registro, con la causa y sus subtipos
  expect_setequal(names(Filter(Negate(is.null), unclass(p$rutas))), c("poblacion", "registry", "catalogos"))
  maestro <- leer_texto(file.path(p$rutas$registry, "master_gbd.csv"))
  expect_identical(maestro$hijos[maestro$cause_id == "9200"], "9101|9102|9103")
  salida <- capture.output(print(p))
  expect_match(salida[1], "^<dl_proyecto> Suma sintética de subtipos, causa 9200 \\| año 2023")
  expect_true(any(grepl("^  suma de los subtipos 9101 \\+ 9102 \\+ 9103: no se ajusta", salida)))
  expect_false(any(grepl("ancla|severidad|betas|subnacional|por defecto", salida)))
  expect_match(capture.output(print(cfg)), "causa 9200 \\| año: 2023 \\| suma de los subtipos 9101 \\+ 9102 \\+ 9103")
  # con un subtipo omitido: suma.omitidas del formato completo, y el print() lo dice
  po <- dl_proyecto(copia_con_suma(c("subtipos_omitidos:", "  - {causa: 9103, motivo: \"prueba\"}")), 9200)
  expect_identical(po$configuracion$suma$omitidas, list(list(cause_id = 9103L, motivo = "prueba")))
  expect_true(any(grepl("^  suma de los subtipos 9101 \\+ 9102 \\(omitido: 9103\\)", capture.output(print(po)))))
  # leída para otro año, como cualquier proyecto
  expect_identical(.dl_anio_ajuste(dl_proyecto(d, 9200, anio = 2019)$configuracion), 2019L)
  # no tiene insumos: dl_insumos() dice quién la suma
  expect_error(dl_insumos(p), "la causa 9200 es una suma de subtipos .*no se ajusta.*dl_correr\\(\\) la suma",
               class = "dl_error")
  expect_error(dl_insumos(cfg), "la causa 9200 es una suma de subtipos")
  # y sigue dando los problemas de sus tablas: una ubicación de la población que no está en ubicaciones
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  pob$ubicacion[pob$ubicacion == "01"] <- "99"
  escribir_texto(pob, d, "poblacion.csv")
  e <- expect_error(dl_proyecto(d, 9200), "problema\\(s\\) entre tablas", class = "dl_error")
  expect_match(e$problemas, "^poblacion: la\\(s\\) ubicación\\(es\\) 99 no est")
})

test_that("un proyecto de suma en su propia carpeta: solo config.yaml, ubicaciones.csv y poblacion.csv", {
  d <- withr::local_tempdir()
  writeLines(enc2utf8(c("causa: 9200", "nombre: Suma en su carpeta", "anio: 2023", "subtipos: [9101, 9102]",
                        "suma_de_subtipos: sí")), file.path(d, "config.yaml"), useBytes = TRUE)
  expect_error(dl_proyecto(d), "faltan tablas obligatorias del proyecto.*ubicaciones.*poblacion")
  expect_no_match(tryCatch(dl_proyecto(d), error = conditionMessage), "ancla")
  for (f in c("ubicaciones.csv", "poblacion.csv")) file.copy(dl_ejemplo(f), file.path(d, f))
  p <- dl_proyecto(d)
  expect_true(.dl_es_suma(p))
  expect_identical(p$configuracion$cause_id, 9200L)
  # con las tablas como argumentos, también; las que no son de una suma no se leen
  m <- dl_proyecto(configuracion = list(causa = 9200L, nombre = "Suma", anio = 2023L, subtipos = 9101:9102,
                                        suma_de_subtipos = TRUE),
                   ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = dl_ejemplo("poblacion.csv"),
                   ancla = dl_ejemplo("ancla"))
  expect_identical(names(m$tablas), c("ubicaciones", "poblacion"))
})

test_that("un subtipo en `subtipos` de dos configuraciones del proyecto es un error que nombra las dos causas", {
  d <- copia_ejemplo(`config/9200.yaml` = c("causa: 9200", "nombre: Suma", "anio: 2023",
                                            "subtipos: [9101, 9102, 9103]", "suma_de_subtipos: sí"))
  patron <- "subtipos: la causa 9101 está en `subtipos` de las configuraciones de las causas 9100 y 9200"
  expect_error(dl_proyecto(d, 9200), patron, class = "dl_error")
  expect_error(dl_proyecto(d, 9101), patron, class = "dl_error")
})

test_that("las claves ancla.error_maximo, mortalidad_exceso.fraccion_aguda y sensibilidad.fraccion_aguda llegan a su destino", {
  proc <- .DL_PROCEDENCIA_SIMPLE
  base <- list(causa = 501L, anio = 2020L, edad_inicio = 40L)
  ctx <- list(ubicacion = "999", nombre = "x", subnacional = FALSE)
  cfg <- .dl_config_simple(c(base, list(ancla = list(error_maximo = 0.1),
                                        mortalidad_exceso = list(fraccion_aguda = 0.5),
                                        sensibilidad = list(fraccion_aguda = c(0.4, 0.5)))), "config.yaml", 501L, ctx)
  expect_identical(cfg$anchor$gate_err_mediano, list(valor = 0.1, procedencia = proc))
  expect_identical(cfg$emr_prior$fraccion_aguda, list(valor = 0.5, procedencia = proc))
  expect_identical(cfg$sensibilidad$fraccion_aguda, c(0.4, 0.5))
  # la misma configuración que las claves del formato completo bajo avanzado, con la procedencia de la traducción
  larga <- .dl_config_simple(c(base, list(avanzado = list(
    anchor = list(gate_err_mediano = list(valor = 0.1, procedencia = proc)),
    emr_prior = list(fraccion_aguda = list(valor = 0.5, procedencia = proc)),
    sensibilidad = list(fraccion_aguda = list(0.4, 0.5))))), "config.yaml", 501L, ctx)
  sin_origen <- function(x) x[setdiff(names(x), "origen")]
  expect_identical(sin_origen(cfg), sin_origen(larga))
  # un valor entero, una sola fracción en la sensibilidad
  uno <- .dl_config_simple(c(base, list(mortalidad_exceso = list(fraccion_aguda = 0L),
                                        sensibilidad = list(fraccion_aguda = 0.3))), "config.yaml", 501L, ctx)
  expect_identical(uno$emr_prior$fraccion_aguda$valor, 0)
  expect_identical(uno$sensibilidad$fraccion_aguda, 0.3)
  # `avanzado` manda sobre la clave corta, y completa su bloque (cfr_30d no tiene clave corta)
  avanzado <- .dl_config_simple(c(base, list(ancla = list(error_maximo = 0.1),
                                             mortalidad_exceso = list(fraccion_aguda = 0.5),
                                             avanzado = list(anchor = list(gate_err_mediano = list(valor = 0.2,
                                                                                                   procedencia = "x")),
                                                             emr_prior = list(fraccion_aguda = list(cfr_30d = 0.1))))),
                                "config.yaml", 501L, ctx)
  expect_identical(avanzado$anchor$gate_err_mediano, list(valor = 0.2, procedencia = "x"))
  expect_identical(avanzado$emr_prior$fraccion_aguda,
                   list(valor = 0.5, procedencia = proc, cfr_30d = 0.1))
})

test_that("sin las claves nuevas la configuración y los insumos de un proyecto de hoy no cambian", {
  # sin la clave la configuración completa no declara el umbral, la fracción aguda ni la de la sensibilidad
  cfg <- dl_configuracion_ejemplo(9101L)
  expect_identical(cfg$anchor$gate_err_mediano, list(valor = 0.05))
  expect_null(cfg$emr_prior$fraccion_aguda)
  expect_null(cfg$sensibilidad$fraccion_aguda)
  expect_false(any(c("ancla.error_maximo", "mortalidad_exceso.fraccion_aguda", "sensibilidad.fraccion_aguda",
                     "subtipo_de", "suma_de_subtipos") %in% names(cfg$origen$por_defecto)))
  expect_null(cfg$suma)
  # el hash de los insumos de la versión anterior a las claves
  hashes <- vapply(c(9100L, 9101L), function(ca) suppressMessages(dl_insumos(dl_proyecto(dl_ejemplo(), ca)))$hash, "")
  expect_identical(unname(hashes), c("ff1e50fe6b5f79a924d4b99476c9759de85a2c5ba9c775627de0ca2785069e49",
                                     "0e21efee8666d99ad26034bea8f3951ea636d58a8321e78e474a464c98822535"))
})

test_that("las claves nuevas fuera de rango o de otro tipo son un error que nombra la clave corta", {
  base <- list(causa = 501L, anio = 2020L, edad_inicio = 40L)
  ctx <- list(ubicacion = "999", nombre = "x", subnacional = FALSE)
  forma <- function(...) .dl_problemas_config_simple(c(base, list(...)))
  expect_match(forma(ancla = list(error_maximo = "poco")), "^ancla.error_maximo: debe ser un número en \\(0, 1\\)")
  expect_match(forma(mortalidad_exceso = list(fraccion_aguda = "mucha")),
               "^mortalidad_exceso.fraccion_aguda: debe ser un número en \\[0, 1\\)")
  expect_match(forma(sensibilidad = list(fraccion_aguda = "a")),
               "^sensibilidad.fraccion_aguda: debe ser una lista de números")
  expect_match(forma(subtipo_de = 9.5), "^subtipo_de: debe ser un entero positivo, el cause_id de la causa padre")
  expect_match(forma(subtipo_de = "x"), "^subtipo_de: debe ser un entero")
  expect_match(forma(subtipo_de = 501L), "^subtipo_de: es la propia causa \\(501\\)")
  expect_match(forma(subtipo_de = 0L), "^subtipo_de: debe ser un entero positivo")
  expect_match(forma(subtipo_de = -3L), "^subtipo_de: debe ser un entero positivo")
  for (v in list(1.5, -1, 1, c(0, 1.5)))
    expect_match(forma(sensibilidad = list(fraccion_aguda = v)),
                 "^sensibilidad.fraccion_aguda: cada valor debe estar en \\[0, 1\\)", info = v)
  expect_length(forma(sensibilidad = list(fraccion_aguda = c(0, 0.3, 0.99))), 0L)
  expect_match(forma(ancla = list(error_maximo = 0.1, tope = 1)), "ancla.tope: clave desconocida")
  # los dominios, el validador completo, citado por la clave corta
  fuera <- function(...) tryCatch(.dl_config_simple(c(base, list(...)), "config.yaml", 501L, ctx),
                                  dl_error = function(e) conditionMessage(e))
  for (v in list(0, 1, -0.1, 2))
    expect_match(fuera(ancla = list(error_maximo = v)), "ancla.error_maximo \\(anchor.gate_err_mediano.valor\\): valor debe estar en \\(0, 1\\)",
                 info = v)
  for (v in list(1, -0.1))
    expect_match(fuera(mortalidad_exceso = list(fraccion_aguda = v)),
                 "mortalidad_exceso.fraccion_aguda \\(emr_prior.fraccion_aguda.valor\\): valor debe ser un número en \\[0, 1\\)",
                 info = v)
})

test_that("sensibilidad.fraccion_aguda fuera de [0, 1) y subtipo_de no positivo se atrapan al leer el proyecto", {
  copia <- function(sensibilidad = "peso: [0.1, 0.5, 1.0]", subtipo_de = NULL, env = parent.frame()) {
    s <- readLines(dl_ejemplo("config", "9101.yaml"), encoding = "UTF-8")
    s[s == "  peso: [0.1, 0.5, 1.0]"] <- paste0("  ", sensibilidad)
    copia_ejemplo(`config/9101.yaml` = c(s, subtipo_de), env = env)
  }
  casos <- list(list("fraccion_aguda: [0, 1.5]", NULL, "sensibilidad.fraccion_aguda"),
                list("fraccion_aguda: [-1]", NULL, "sensibilidad.fraccion_aguda"),
                list("peso: [0.5]", "subtipo_de: 0", "subtipo_de"))
  for (k in casos) {
    d <- copia(k[[1L]], k[[2L]])
    e <- expect_error(dl_proyecto(d, 9101), class = "dl_error")
    expect_match(conditionMessage(e), paste0(k[[3L]], ": (cada valor debe estar en \\[0, 1\\)|debe ser un entero positivo)"))
    rev <- suppressMessages(dl_revisar_proyecto(d, 9101))
    expect_match(rev$detalle[rev$paso == "configuración"], k[[3L]], fixed = TRUE)
  }
})

test_that("proxies en bandas que no son de GBD ni de la población (uniones de sus bandas) llevan su id y cierran", {
  d <- escribir_proxies_calibrados(dl_ejemplo(copiar_en = withr::local_tempdir()))
  f <- file.path(d, "covariables", "proxies.csv")
  px <- data.table::fread(f, colClasses = list(character = "ubicacion"), encoding = "UTF-8")
  pob <- data.table::fread(file.path(d, "poblacion.csv"), colClasses = list(character = "ubicacion"))
  # 45-59, 60-74 y 75+: promedios de las bandas de 5 años, ponderados por la población de la ubicación, el sexo y el
  # año; así el promedio subnacional sigue cerrando en el valor nacional
  anchas <- data.table::data.table(a0 = c(45, 60, 75), a1 = c(60, 75, 125))
  px[, banda := findInterval(edad_inicio, anchas$a0)]
  px[!is.na(edad_inicio) & edad_inicio < 45, banda := NA_integer_]
  px[pob, p := i.poblacion, on = c("ubicacion", "anio", "sexo", "edad_inicio", "edad_fin")]
  px[edad_inicio == 80, p := pob[.SD, on = c("ubicacion", "anio", "sexo"), sum(x.poblacion[x.edad_inicio >= 80]),
                                 by = .EACHI]$V1]
  agr <- px[!is.na(banda), list(valor = sum(valor * p) / sum(p), error_estandar = sum(error_estandar * p) / sum(p)),
            by = list(ubicacion, anio, sexo, banda, covariable, fuente)]
  agr[, `:=`(edad_inicio = anchas$a0[banda], edad_fin = anchas$a1[banda])]
  nuevo <- rbind(px[is.na(banda)], agr, fill = TRUE)[, names(data.table::fread(f, nrows = 0L)), with = FALSE]
  data.table::fwrite(nuevo, f)
  b <- suppressMessages(dl_insumos(dl_proyecto(d, 9100)))
  ids <- unique(b$cov_proxy$age_group_id)
  expect_true(all(c(13L, 22L) %in% ids))
  sint <- setdiff(ids, c(13L, 22L))
  expect_length(sint, 3L)
  expect_true(all(sint > dismodlite:::.DL_ID_BANDA_SINTETICA))
  lim <- b$bandas_catalogo[age_group_id %in% sint][order(age_start)]
  expect_identical(as.numeric(lim$age_start), c(45, 60, 75))
  expect_identical(as.numeric(lim$age_end), c(60, 75, 125))
})

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

test_that("la severidad de una partición con un límite mayor que 1 se acepta con un aviso; la tabla del usuario, no", {
  # el componente de la secuela 668 (0,05 de la causa): su estado 540 queda en 1, con límites 0,9 y 1,1 (0,045 y
  # 0,055 divididos por la cuota del componente), como en la versión 0.2.2
  config <- c("causa: 302", "anio: 2020", "edad_inicio: 40", "severidad:", "  particion: particion/mini",
              "componente:", "  secuelas: [668]")
  d <- escribir_particion_mini(escribir_pais_ficticio(file.path(withr::local_tempdir(), "pf"), 302L, 2020L, config))
  unlink(file.path(d, "severidad.csv"))
  expect_warning(p <- dl_proyecto(d), paste0("severidad.particion: el l\u00edmite superior de la proporci\u00f3n pasa ",
                                             "de 1 en el/los estado\\(s\\) 540 \\(hasta 1.1\\) de la partici\u00f3n mini"))
  b <- suppressMessages(dl_insumos(p))
  expect_equal(b$severidad$prop_upper, 1.1)
  expect_equal(b$severidad$prop_lower, 0.9)
  # la misma severidad escrita por el usuario sigue en [0, 1]
  sev <- data.frame(causa = 302L, estado = "540", proporcion = 1, inferior = 0.9, superior = 1.1)
  expect_error(dl_tabla("severidad", sev), "superior: hay valores mayores que 1")
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

test_that("un hueco entre las bandas de la población se nombra (no es una banda del ancla que cruza un límite)", {
  d <- proyecto_ficticio()
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  escribir_texto(pob[edad_inicio != "50"], d, "poblacion.csv")
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_identical(e$problemas, paste0("poblacion: las bandas de edad dejan un hueco: falta(n) 50-54 a\u00f1os; ",
                                       "las bandas van seguidas, de la primera a la \u00faltima"))
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
                     "  - datos: sin valor: fila(s) 2", "el tipo de dato mortalidad",
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

test_that("las claves proxies.* se revisan", {
  probs <- dismodlite:::.dl_problemas_config_simple(list(causa = 1L, anio = 2023L, edad_inicio = 30,
    proxies = list(metodo = "kernel", transformacion = list(haqi = "razon"), excluir = list(list(anio = 2021)))))
  expect_true(any(grepl("proxies.metodo", probs)))
  expect_true(any(grepl("proxies.transformacion.*razon", probs)))
  expect_true(any(grepl("proxies.excluir.*motivo", probs)))
  # la forma de cada una
  probs <- dismodlite:::.dl_problemas_config_simple(list(causa = 1L, anio = 2023L, edad_inicio = 30,
    proxies = list(transformacion = "cociente", excluir = list(list(anio = "x", motivo = "m", otro = 1)))))
  expect_true(any(grepl("proxies.transformacion: .*bloque", probs)))
  expect_true(any(grepl("proxies.excluir\\[1\\].anio: debe ser un entero", probs)))
  expect_true(any(grepl("proxies.excluir\\[1\\].otro: clave desconocida", probs)))
  probs <- dismodlite:::.dl_problemas_config_simple(list(causa = 1L, anio = 2023L, edad_inicio = 30,
    proxies = list(excluir = list(2021))))
  expect_true(any(grepl("proxies.excluir: es una lista de registros", probs)))
  # una covariable sin valor ({haqi: } en el YAML) es un problema, no «cociente» en silencio
  s <- c(list(causa = 1L, anio = 2023L, edad_inicio = 30),
         yaml::yaml.load("proxies:\n  transformacion: {haqi: , ldi: diferencia}"))
  probs <- dismodlite:::.dl_problemas_config_simple(s)
  expect_true(any(grepl("proxies.transformacion.haqi: falta el valor.*cociente, diferencia", probs)))
  expect_false(any(grepl("proxies.transformacion.ldi", probs)))
})

test_that("una configuración con proxies.* válidas pasa y la traducción las ignora", {
  s <- list(causa = 9100L, anio = 2023L, edad_inicio = 30,
            proxies = list(metodo = "edicion", transformacion = list(haqi = "diferencia", sev = "cociente"),
                           excluir = list(list(anio = 2021L, motivo = "cambio de modo de la encuesta"))))
  expect_identical(dismodlite:::.dl_problemas_config_simple(s), character())
  expect_identical(dismodlite:::.dl_problemas_config_simple(list(causa = 1L, anio = 2023L, edad_inicio = 30,
                                                                 proxies = list(excluir = list()))), character())
  ctx <- list(ubicacion = "PAIS", betas = NULL, covariables_subnacionales = character(), subnacional = FALSE,
              nombre = "x")
  cfg <- dismodlite:::.dl_traducir_config_simple(s, "config.yaml", ctx)
  base <- dismodlite:::.dl_traducir_config_simple(s["proxies" != names(s)], "config.yaml", ctx)
  cfg$origen$configuracion <- base$origen$configuracion <- NULL
  expect_identical(cfg, base)                                     # sin huella en la configuración completa
  expect_false(any(grepl("proxies", names(cfg$origen$por_defecto))))
  expect_length(dismodlite:::.dl_validar_config(cfg, 9100L)$problemas, 0L)
})

test_that("la configuración comentada de un proyecto nuevo trae las claves proxies.* como ejemplo", {
  l <- dismodlite:::.dl_plantilla_config(list(causa = 1L, anio = 2023L, edad_inicio = 30))
  i <- which(l == "# proxies:")
  expect_length(i, 1L)
  expect_identical(l[i + 1:3], c("#   metodo: paseo_aleatorio", "#   transformacion: {haqi: diferencia}",
                                 "#   excluir: [{anio: 2021, motivo: cambio de modo de la encuesta}]"))
  y <- yaml::yaml.load(paste(sub("^# ", "", l[i + 0:3]), collapse = "\n"))      # quitar el «# » deja YAML válido
  expect_identical(y$proxies$excluir[[1]]$anio, 2021L)
})

# ---- Proyecto con proxies_crudos: calibra al leer ----

test_that("un proyecto con proxies_crudos calibra al leer y la puerta de R da el mismo hash", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  p <- dl_proyecto(d, 9100)
  cal <- p$calibracion
  covs <- c("SEV_scalar_agestd_cvd_pvd", "LDI_pc", "haqi")
  expect_s3_class(cal, "dl_tabla")
  expect_identical(attr(cal, "calibracion")$covariable, covs)
  expect_identical(attr(cal, "calibracion")$transformacion, c("cociente", "cociente", "diferencia"))
  expect_identical(sort(unique(cal$ubicacion)), sort(p$tablas$ubicaciones$ubicacion[-1L]))
  expect_identical(unique(cal$anio), 2023L)
  # las tablas del proyecto quedan como vinieron: covariables sin filas subnacionales, proxies_crudos aparte
  cv <- p$tablas$covariables
  expect_false(any(cv$covariable %in% covs & !is.na(cv$ubicacion) & cv$ubicacion != "123"))
  expect_identical(nrow(p$tablas$proxies_crudos), 1500L)
  b <- suppressMessages(dl_insumos(p))      # antes de la otra puerta, que traduce en la misma carpeta
  expect_identical(sort(unique(b$cov_proxy$location_id)), sort(unique(cal$ubicacion)))
  # puerta de R: calibrar a mano y pasar covariables completas, sin proxies_crudos
  cal2 <- dl_calibrar_proxies(p$tablas$proxies_crudos, cv, p$tablas$poblacion, anio = 2023,
                              transformacion = c(haqi = "diferencia"))
  unlink(file.path(d, "proxies_crudos.csv"))
  p2 <- dl_proyecto(d, 9100, covariables = data.table::rbindlist(list(cv, cal2), fill = TRUE))
  expect_null(p2$calibracion)
  b2 <- suppressMessages(dl_insumos(p2))
  expect_identical(b$hash, b2$hash)
  expect_identical(b$calibracion_proxies, cal)              # fuera del hash, como el contrato
  expect_null(b2$calibracion_proxies)
  expect_identical(b$contrato, p$tablas)
})

test_that("el proyecto calibra con proxies.metodo, proxies.transformacion, proxies.excluir y el año del ancla", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  f <- file.path(d, "config", "9100.yaml")
  cambiar_proxies_config(f, c("proxies:", "  metodo: edicion", "  transformacion: {haqi: diferencia}",
                              "  excluir: [{anio: 2021, motivo: prueba}]"))
  p <- dl_proyecto(d, 9100)
  k <- attr(p$calibracion, "calibracion")
  expect_identical(k[covariable == "haqi", c("metodo", "transformacion", "ediciones", "excluidas")],
                   data.table::data.table(metodo = "edicion", transformacion = "diferencia", ediciones = "2023",
                                          excluidas = "2021 (prueba)"))
  expect_identical(k$transformacion, c("cociente", "cociente", "diferencia"))
  # los años de `cambios` (dl_configuracion()) mandan sobre los de la configuración: calibra el año que se estima
  s <- dismodlite:::.dl_leer_config(f)
  cal <- dismodlite:::.dl_calibracion_proyecto(s, f, p$tablas, list(years = list(ajuste = 2019L)))
  expect_identical(unique(cal$anio), 2019L)
  expect_identical(dl_configuracion(9100, file.path(d, "config"),
                                    cambios = list(years = list(ajuste = 2019L)))$years$ajuste, 2019L)
  # una covariable de proxies.transformacion que no está en proxies_crudos, en palabras de la clave
  writeLines(sub("haqi: diferencia", "otra: diferencia", readLines(f)), f)
  expect_error(dl_proyecto(d, 9100), "proxies.transformacion.otra: .*no está en proxies_crudos")
})

test_that("subnacionales de una covariable en las dos tablas es un problema", {
  d <- dl_ejemplo(copiar_en = withr::local_tempdir())
  cr <- data.table::fread(file.path(d, "proxies_crudos.csv"), colClasses = list(character = "ubicacion"),
                          encoding = "UTF-8")
  sub <- cr[covariable == "haqi" & anio == 2023, list(ubicacion, anio, sexo, edad_inicio, covariable, valor,
                                                      error_estandar)]
  data.table::fwrite(sub, file.path(d, "covariables", "subnacionales.csv"))
  expect_error(dl_proyecto(d, 9100), "proxies_crudos: la covariable haqi tiene filas subnacionales en las dos tablas")
  # la regla entre tablas, sobre las tablas como vinieron
  p <- dl_proyecto(dl_ejemplo(), 9100)
  tablas <- p$tablas
  tablas$covariables <- dl_tabla("covariables", data.table::rbindlist(list(tablas$covariables, sub), fill = TRUE))
  pr <- dismodlite:::.dl_problemas_proyecto(p$tablas, p$configuracion, originales = tablas)
  expect_match(pr$problemas, "haqi tiene filas subnacionales en las dos tablas", all = FALSE)
  expect_length(dismodlite:::.dl_problemas_proyecto(p$tablas, p$configuracion)$problemas, 0L)
})

test_that("una covariable calibrada sin beta la ve la regla de siempre", {
  p <- dl_proyecto(dl_ejemplo(), 9100)
  tm <- dismodlite:::.dl_tablas_modelo(p$tablas, p$calibracion)
  expect_true(any(tm$covariables$covariable == "haqi" & tm$covariables$ubicacion %in% "01"))
  tm$betas <- tm$betas[covariable != "haqi"]
  pr <- dismodlite:::.dl_problemas_proyecto(tm, p$configuracion, originales = p$tablas)
  expect_match(pr$avisos, "haqi", all = FALSE)
})

test_that("ediciones repetidas en proxies.excluir o en `excluir` son un problema claro", {
  s <- list(causa = 9100L, anio = 2023L, edad_inicio = 30,
            proxies = list(excluir = list(list(anio = 2021L, motivo = "a"), list(anio = 2021L, motivo = "b"))))
  expect_match(dismodlite:::.dl_problemas_config_simple(s), "proxies.excluir: la edición 2021 se repite",
               all = FALSE)
  expect_error(dismodlite:::.dl_validar_excluir(data.frame(anio = c(2021, 2021), motivo = c("a", "b"))),
               "`excluir` repite la\\(s\\) edición\\(es\\) 2021")
})

# ---- El año que se estima (`anio`) y el año del ancla ----

# Cambia el `anio` de la configuración de `causa` en la copia `d` del ejemplo y, con `ancla`, declara ancla.anio.
config_con_anio <- function(d, causa, anio = NULL, ancla = NULL) {
  f <- file.path(d, "config", sprintf("%d.yaml", causa))
  l <- readLines(f, encoding = "UTF-8")
  if (!is.null(anio)) l <- sub("^anio: [0-9]+$", sprintf("anio: %d", anio), l)
  if (!is.null(ancla)) l <- c(l, "", "ancla:", sprintf("  anio: %d", ancla))
  writeLines(enc2utf8(l), f, useBytes = TRUE)
  d
}

# dl_proyecto(...) con sus mensajes aparte: list(p, mensajes).
proyecto_y_mensajes <- function(...) {
  mensajes <- character()
  p <- withCallingHandlers(dl_proyecto(...), message = function(m) {
    mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  list(p = p, mensajes = mensajes)
}

test_that("dl_proyecto(anio = ) lee el proyecto para otro año, como si la configuración lo trajera", {
  p <- dl_proyecto(copia_ejemplo(), 9101, anio = 2019)
  expect_identical(p$configuracion$years$ajuste, 2019L)
  expect_identical(unique(p$calibracion$anio), 2019L)
  q <- dl_proyecto(config_con_anio(copia_ejemplo(), 9101, anio = 2019), 9101)
  sin_rutas <- function(cfg) { cfg$origen[c("archivo", "betas")] <- NULL; cfg }      # las de cada copia
  expect_identical(sin_rutas(p$configuracion), sin_rutas(q$configuracion))
  expect_identical(suppressMessages(dl_insumos(p))$hash, suppressMessages(dl_insumos(q))$hash)
  # con las tablas como argumentos y en el formato completo
  r <- dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30), anio = 2019,
                   ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = dl_ejemplo("poblacion.csv"),
                   ancla = dl_ejemplo("ancla"))
  expect_identical(r$configuracion$years$ajuste, 2019L)
  expect_identical(.dl_anio_ajuste(dl_proyecto(ejemplo_completo(), 9100, anio = 2019)$configuracion), 2019L)
  expect_identical(.dl_anio_ajuste(dl_proyecto(ejemplo_completo(), 9100)$configuracion), 2023L)
})

test_that("`anio` es un solo número entero", {
  for (mal in list("2019", c(2019, 2023), 2019.5, NA, TRUE))
    expect_error(dl_proyecto(dl_ejemplo(), 9101, anio = mal), "`anio` debe ser un año, un solo número entero")
})

test_that(".dl_anio_ancla_proyecto(): el año, el menor con ancla.anio o el último anterior que trae el ancla", {
  expect_identical(.dl_anio_ancla_proyecto(2024, NULL, 2018:2023), list(anio = 2023L, proyectado = TRUE))
  expect_identical(.dl_anio_ancla_proyecto(2021, NULL, 2018:2023), list(anio = 2021L, proyectado = FALSE))
  expect_identical(.dl_anio_ancla_proyecto(2019, 2023, 2018:2023), list(anio = 2019L, proyectado = FALSE))
  expect_identical(.dl_anio_ancla_proyecto(2024, 2023, 2018:2023), list(anio = 2023L, proyectado = FALSE))
  expect_identical(.dl_anio_ancla_proyecto(2017, NULL, 2018:2023), list(anio = 2017L, proyectado = FALSE))
  expect_identical(.dl_anio_ancla_proyecto(2024, NULL, c(2019L, 2023L, 2025L)), list(anio = 2023L, proyectado = TRUE))
  expect_identical(.dl_anio_ancla_proyecto(2024, NULL, integer()), list(anio = 2024L, proyectado = FALSE))
})

test_that("sin ancla.anio, un año que el ancla no trae se proyecta desde el último anterior y se anuncia", {
  # el ejemplo trae el ancla y las covariables nacionales hasta 2023 y la población hasta 2024
  x <- proyecto_y_mensajes(copia_ejemplo(), 9101, anio = 2024)
  cfg <- x$p$configuracion
  expect_identical(cfg$years$ajuste, 2024L)
  expect_identical(cfg$years$ancla$valor, 2023L)
  expect_match(cfg$years$ancla$procedencia, "^proyección: la tabla ancla no trae .* 2024; se proyecta desde 2023")
  expect_length(x$mensajes, 1L)
  expect_match(x$mensajes, "^dl_proyecto\\(\\): el ancla no trae 2024: se proyecta desde 2023")
  expect_identical(cfg$origen$por_defecto[["ancla.anio"]], "2023")
  expect_identical(unique(x$p$calibracion$anio), 2024L)       # los proxies, del año que se estima
  b <- suppressMessages(dl_insumos(x$p))
  expect_identical(unique(b$prior_gbd$year), 2024L)           # el ancla de 2023, con la etiqueta de 2024
  # lo mismo con el año escrito en la configuración
  y <- proyecto_y_mensajes(config_con_anio(copia_ejemplo(), 9101, anio = 2024), 9101)
  expect_identical(y$p$configuracion$years, cfg$years)
  expect_identical(y$mensajes, x$mensajes)
  expect_identical(suppressMessages(dl_insumos(y$p))$hash, b$hash)
  # y es la proyección que se declara con ancla.anio: los mismos números, otra procedencia y sin anuncio
  z <- proyecto_y_mensajes(config_con_anio(copia_ejemplo(), 9101, ancla = 2023), 9101, anio = 2024)
  expect_identical(z$p$configuracion$years$ancla, list(valor = 2023L, procedencia = .DL_PROCEDENCIA_SIMPLE))
  expect_identical(z$mensajes, character())
  bz <- suppressMessages(dl_insumos(z$p))
  expect_identical(bz$prior_gbd, b$prior_gbd)
  expect_identical(bz$cov_proxy, b$cov_proxy)
})

test_that("con ancla.anio, el año del ancla es el menor entre ese y el que se estima", {
  d <- config_con_anio(copia_ejemplo(), 9101, ancla = 2023)
  x <- proyecto_y_mensajes(d, 9101, anio = 2019)
  expect_identical(.dl_anio_ancla(x$p$configuracion), 2019L)
  expect_identical(x$mensajes, character())
  expect_identical(suppressMessages(dl_insumos(x$p))$prior_gbd,
                   suppressMessages(dl_insumos(dl_proyecto(copia_ejemplo(), 9101, anio = 2019)))$prior_gbd)
  expect_identical(.dl_anio_ancla(dl_proyecto(d, 9101)$configuracion), 2023L)
})

test_that("un ancla.anio posterior al `anio` de la configuración es un error; solo el argumento `anio` lo baja", {
  d <- config_con_anio(copia_ejemplo(), 9101, ancla = 2025)
  expect_identical(.dl_anio_ancla(dl_proyecto(d, 9101, anio = 2019)$configuracion), 2019L)
  # con proxies_crudos, el error llega antes, de su calibración (no hay valor nacional de 2025); sin ellos, del validador
  expect_error(dl_proyecto(d, 9101), "no tiene valor nacional en 2025")
  unlink(file.path(d, "proxies_crudos.csv"))
  expect_error(dl_proyecto(d, 9101), "ancla.anio .*: debe ser un entero igual al año de ajuste o un año antes")
  # con la configuración como lista y las tablas como argumentos, lo mismo
  minimo <- function(...) dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30,
                                                           ancla = list(anio = 2025L)), ...,
                                      ubicaciones = dl_ejemplo("ubicaciones.csv"),
                                      poblacion = dl_ejemplo("poblacion.csv"), ancla = dl_ejemplo("ancla"))
  expect_error(minimo(), "ancla.anio .*: debe ser un entero igual al año de ajuste o un año antes")
  expect_identical(minimo(anio = 2019)$configuracion$years$ancla,
                   list(valor = 2019L, procedencia = .DL_PROCEDENCIA_SIMPLE))
})

test_that("las traducciones de dos años del mismo proyecto conviven; la del mismo año se reemplaza", {
  d <- copia_ejemplo()
  p19 <- dl_proyecto(d, 9101, anio = 2019)
  p23 <- dl_proyecto(d, 9101)
  expect_identical(unique(suppressMessages(dl_insumos(p19))$prior_gbd$year), 2019L)
  expect_identical(unique(suppressMessages(dl_insumos(p23))$prior_gbd$year), 2023L)
  # cambia una tabla: la traducción nueva de 2023 borra la anterior de 2023 y deja la de 2019
  pob <- leer_texto(file.path(d, "poblacion.csv"))
  pob$poblacion[pob$anio == "2023"][1] <- "999999"
  escribir_texto(pob, d, "poblacion.csv")
  q23 <- dl_proyecto(d, 9101)
  expect_false(identical(q23$rutas$poblacion, p23$rutas$poblacion))
  expect_false(file.exists(p23$rutas$poblacion))
  expect_true(file.exists(p19$rutas$poblacion))
  expect_error(dl_insumos(p23), "la traducción de este proyecto ya no está")
})

test_that("un año anterior a todos los del ancla: el error de las reglas, que no hay de dónde proyectar", {
  minimo <- function(anio) dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30), anio = anio,
                                       ubicaciones = dl_ejemplo("ubicaciones.csv"),
                                       poblacion = dl_ejemplo("poblacion.csv"), ancla = dl_ejemplo("ancla"))
  e <- expect_error(minimo(2018), class = "dl_error")
  expect_match(e$problemas, paste0("^ancla: no trae la prevalencia de la causa 9101 de hombres y mujeres de 2018 ",
                                   "\\(años que trae: 2019, 2023\\); solo se proyecta desde un año anterior, y el ",
                                   "ancla no trae ninguno$"), all = FALSE)
  # más de un año atrás no es mantener el nivel: no se proyecta
  expect_error(minimo(2021), paste0("anio: la tabla ancla no trae la prevalencia de la causa en 2021 y el último año ",
                                    "anterior que trae es 2019: el ancla se proyecta a lo sumo un año"))
})
