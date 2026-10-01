# Lectura de los CSV de entrada: un CSV guardado desde Excel con configuración regional en español
# (punto y coma, coma decimal, ubigeo sin el cero inicial, texto que no está en UTF-8) da un error en español que
# nombra el archivo, la columna y la causa probable, en vez de un error de tipo más adelante. Un CSV válido se lee
# igual que con data.table::fread().

# Los insumos del ejemplo en el formato completo (sus CSV son los que se leen tal cual) con `...` como cambios de rutas.
.insumos_con <- function(...)
  suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"),
                              dl_rutas_ejemplo(9100L, ..., formato = "completo")))

.mensaje <- function(expr) tryCatch({ expr; NA_character_ }, error = conditionMessage)

test_that("datos.csv guardado con punto y coma y coma decimal: error que nombra el archivo y las columnas", {
  d <- data.table::fread(ejemplo_completo("datos.csv"), colClasses = list(character = "location_id"), encoding = "UTF-8")
  f <- file.path(withr::local_tempdir(), "datos.csv")
  data.table::fwrite(d, f, sep = ";", dec = ",")
  expect_error(.insumos_con(datos = f), "coma decimal|separador")
  m <- .mensaje(.insumos_con(datos = f))
  expect_match(m, "datos.csv", fixed = TRUE)
  expect_match(m, "separador")
  expect_match(m, "coma decimal")
  expect_match(m, "columna\\(s\\) [^)]*\\bval\\b")
  expect_match(m, "Excel")
})

test_that("n\u00fameros con coma decimal en un CSV separado por comas: error con el archivo y la columna", {
  d <- data.table::fread(ejemplo_completo("datos.csv"), colClasses = list(character = "location_id"), encoding = "UTF-8")
  d[, se := ifelse(is.na(se), NA_character_, sub(".", ",", format(se, digits = 15), fixed = TRUE))]
  f <- file.path(withr::local_tempdir(), "datos_coma.csv")
  data.table::fwrite(d, f)
  m <- .mensaje(.insumos_con(datos = f))
  expect_match(m, "coma decimal")
  expect_match(m, "datos_coma.csv", fixed = TRUE)
  expect_match(m, "columna\\(s\\) se \\(")
})

test_that("poblacion.csv con location_id 1 en lugar de 01: error que nombra la columna y el archivo", {
  p <- data.table::fread(ejemplo_completo("poblacion.csv"), colClasses = list(character = "location_id"))
  p[location_level == 1L, location_id := as.character(as.integer(location_id))]
  f <- file.path(withr::local_tempdir(), "poblacion.csv")
  data.table::fwrite(p, f)
  expect_error(.insumos_con(datos = FALSE, proxies = FALSE, poblacion = f), "location_id")
  m <- .mensaje(.insumos_con(datos = FALSE, proxies = FALSE, poblacion = f))
  expect_match(m, "poblacion.csv", fixed = TRUE)
  expect_match(m, "cero inicial")
  expect_match(m, "\u00ab1\u00bb")
})

test_that("proxies y datos departamentales sin el cero inicial tambi\u00e9n se detectan", {
  x <- data.table::fread(ejemplo_completo("proxies_departamentales.csv"), colClasses = list(character = "location_id"))
  x[, location_id := as.character(as.integer(location_id))]
  f <- file.path(withr::local_tempdir(), "proxies.csv")
  data.table::fwrite(x, f)
  m <- .mensaje(.insumos_con(datos = FALSE, proxies = f))
  expect_match(m, "location_id")
  expect_match(m, "proxies.csv", fixed = TRUE)
  d <- data.table::fread(ejemplo_completo("datos.csv"), colClasses = list(character = "location_id"), encoding = "UTF-8")
  d[location_level == 1L, location_id := as.character(as.integer(location_id))]
  f2 <- file.path(withr::local_tempdir(), "datos_dpto.csv")
  data.table::fwrite(d, f2)
  expect_match(.mensaje(.insumos_con(datos = f2)), "location_id.*datos_dpto[.]csv")
})

test_that("un CSV que no est\u00e1 en UTF-8 (Latin-1) da un error que pide guardarlo en UTF-8", {
  txt <- readLines(ejemplo_completo("datos.csv"), encoding = "UTF-8")
  f <- file.path(withr::local_tempdir(), "datos_latin1.csv")
  writeLines(iconv(txt, from = "UTF-8", to = "latin1"), f, useBytes = TRUE)
  m <- .mensaje(.insumos_con(datos = f))
  expect_match(m, "UTF-8")
  expect_match(m, "datos_latin1.csv", fixed = TRUE)
})

test_that("un CSV v\u00e1lido se lee igual que con data.table::fread()", {
  for (a in c("datos.csv", "poblacion.csv", "proxies_departamentales.csv", "pesos_80mas.csv")) {
    cc <- list(character = "location_id")
    if (a == "pesos_80mas.csv") cc <- NULL
    expect_identical(.dl_leer_csv(ejemplo_completo(a), colClasses = cc),
                     data.table::fread(ejemplo_completo(a), colClasses = cc, encoding = "UTF-8"), info = a)
  }
  cat_loc <- .dl_leer_csv(ejemplo_completo("catalogos", "catalogo_locations_peru.csv"),
                          colClasses = list(character = "location_id"))
  expect_true(all(nchar(cat_loc$location_id[cat_loc$location_level == 1L]) == 2L))
})

test_that("el texto de los CSV queda marcado como UTF-8: los nombres se comparan igual con un locale C", {
  skip_on_os("windows")
  a <- .dl_leer_csv(ejemplo_completo("ancla", "prevalencia.csv"), colClasses = list(character = "location_id"))
  nombre <- unique(a$cause_name[a$cause_id == 9100L])
  expect_identical(Encoding(nombre), "UTF-8")
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_identical(.dl_slugify(nombre), "arteriopatia_cronica_sintetica")
})

# Mensaje de error y avisos que emitió `expr` (sin mostrarlos).
.mensaje_y_avisos <- function(expr) {
  avisos <- character()
  m <- withCallingHandlers(tryCatch({ expr; NA_character_ }, error = conditionMessage),
                           warning = function(w) {
                             avisos <<- c(avisos, conditionMessage(w))
                             invokeRestart("muffleWarning")
                           })
  list(mensaje = m, avisos = avisos)
}

test_that("datos.csv de Excel en espa\u00f1ol: un solo mensaje con todos los problemas y una receta que funciona", {
  d <- data.table::fread(ejemplo_completo("datos.csv"), colClasses = list(character = "location_id"), encoding = "UTF-8")
  for (v in c("es_referencia", "ajuste_completitud", "outlier"))
    data.table::set(d, j = v, value = ifelse(d[[v]], "VERDADERO", "FALSO"))
  d[location_level == 1L, location_id := as.character(as.integer(location_id))]
  f <- file.path(withr::local_tempdir(), "datos.csv")
  data.table::fwrite(d, f, sep = ";", dec = ",")
  writeLines(iconv(readLines(f, encoding = "UTF-8"), from = "UTF-8", to = "latin1"), f, useBytes = TRUE)
  m <- .mensaje(.insumos_con(datos = f))
  expect_match(m, "^dl_insumos\\(\\): el archivo \u00abdatos.csv\u00bb parece guardado desde Excel")
  for (p in c("separador de columnas", "coma decimal", "no est\u00e1 en UTF-8",
              "es_referencia, ajuste_completitud, outlier",
              "VERDADERO/FALSO", "cero inicial", "Usar separadores del sistema", "encoding = \"Latin-1\"",
              "sprintf\\(\"%02d\""))
    expect_match(m, p, info = p)
  # la receta del mensaje, tal cual, deja el archivo listo: los insumos son los del datos.csv original
  receta <- sub("^  ", "", strsplit(sub("\n  archivo: .*$", "", sub("^.*Para corregirlo en R:\n", "", m)), "\n")[[1]])
  eval(parse(text = receta), envir = new.env(parent = globalenv()))
  expect_equal(.insumos_con(datos = f)$datos, .insumos_con()$datos)
})

test_that("la receta de un CSV sin location_id no lleva colClasses (pesos_80mas)", {
  f <- file.path(withr::local_tempdir(), "pesos_80mas.csv")
  data.table::fwrite(data.table::fread(ejemplo_completo("pesos_80mas.csv")), f, sep = ";", dec = ",")
  m <- .mensaje(.insumos_con(datos = FALSE, proxies = FALSE, pesos_80mas = f))
  expect_match(m, "pesos_80mas.csv", fixed = TRUE)
  expect_match(m, "dec = \",\"", fixed = TRUE)
  expect_false(grepl("colClasses", m, fixed = TRUE))
})

test_that("poblaci\u00f3n con separador de miles: error con la columna y un ejemplo, sin avisos en ingl\u00e9s", {
  p <- data.table::fread(ejemplo_completo("poblacion.csv"), colClasses = list(character = "location_id"))
  p[, val := formatC(val, format = "d", big.mark = ".", decimal.mark = ",")]
  f <- file.path(withr::local_tempdir(), "poblacion.csv")
  data.table::fwrite(p, f)
  r <- .mensaje_y_avisos(.insumos_con(datos = FALSE, proxies = FALSE, poblacion = f))
  expect_match(r$mensaje, "separador de miles en la\\(s\\) columna\\(s\\) val \\(p. ej. \u00ab1[.]174[.]097\u00bb\\)")
  expect_length(r$avisos, 0L)
})

test_that("columnas vac\u00edas y fila de comas al final (Excel): error que pide borrarlas, sin avisos", {
  f <- file.path(withr::local_tempdir(), "datos.csv")
  writeLines(c(paste0(readLines(ejemplo_completo("datos.csv"), encoding = "UTF-8"), ",,"), ",,,,"), f, useBytes = TRUE)
  r <- .mensaje_y_avisos(.insumos_con(datos = f))
  expect_match(r$mensaje, "columnas vac\u00edas sin nombre al final \\(V31, V32\\)")
  expect_match(r$mensaje, "d[, c(\"V31\", \"V32\") := NULL]", fixed = TRUE)
  expect_length(r$avisos, 0L)
})

test_that("un valor que no se puede convertir se muestra en el error (no \u00abNA en columna obligatoria\u00bb)", {
  d <- data.table::fread(ejemplo_completo("datos.csv"), colClasses = list(character = "location_id"), encoding = "UTF-8")
  d[, es_referencia := as.character(es_referencia)]
  d[which(location_level == 1L)[1L], es_referencia := "s\u00ed"]   # una fila departamental: las nacionales no entran
  f <- file.path(withr::local_tempdir(), "datos.csv")
  data.table::fwrite(d, f)
  expect_match(.mensaje(.insumos_con(datos = f)),
               paste0("columna es_referencia de la tabla datos tiene valores que no son l\u00f3gicos ",
                      "\\(TRUE/FALSE\\): \u00abs\u00ed\u00bb"))
})

test_that("archivos que faltan, carpetas vac\u00edas, archivos vac\u00edos: el error nombra el archivo y su pieza", {
  tmp <- withr::local_tempdir()
  reg <- file.path(tmp, "registro"); dir.create(reg)
  file.copy(list.files(ejemplo_completo("registro"), full.names = TRUE), reg)
  unlink(file.path(reg, "sequela_rei.csv"))
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, registro = reg)),
               "^dl_insumos\\(\\): no existe el archivo \u00absequela_rei.csv\u00bb de la carpeta de `registro`")
  expect_match(.mensaje(.insumos_con(datos = tmp)), "el archivo de `datos` .* es una carpeta")
  vacio <- file.path(tmp, "poblacion.csv"); file.create(vacio)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, poblacion = vacio)),
               "el archivo de `poblacion` \\(\u00abpoblacion.csv\u00bb\\) est\u00e1 vac\u00edo")
  cov <- file.path(tmp, "covariables"); dir.create(cov)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, covariables = cov)),
               "la carpeta de `covariables` no tiene archivos CSV")
  ancla <- file.path(tmp, "ancla_vacia"); dir.create(ancla)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, ancla_prevalencia = ancla)),
               "^dl_insumos\\(\\): la carpeta de `ancla_prevalencia` no tiene archivos CSV; se espera un archivo CSV")
})

test_that("un CSV con solo la fila de encabezado (plantilla vacía) da un solo mensaje claro", {
  f <- file.path(withr::local_tempdir(), "datos.csv")
  writeLines(readLines(ejemplo_completo("datos.csv"), n = 1L, encoding = "UTF-8"), f, useBytes = TRUE)
  m <- .mensaje(.insumos_con(datos = f))
  expect_match(m, "^dl_insumos\\(\\): el archivo de `datos` \\(«datos.csv»\\) solo tiene la fila de encabezado")
  expect_match(m, "dl_rutas_ejemplo(causa, datos = FALSE)", fixed = TRUE)
  expect_false(grepl("tipo esperado", m, fixed = TRUE))
  p <- file.path(withr::local_tempdir(), "proxies.csv")
  writeLines(readLines(ejemplo_completo("proxies_departamentales.csv"), n = 1L, encoding = "UTF-8"), p, useBytes = TRUE)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = p)), "el archivo de `proxies` .* solo tiene la fila")
})

test_that("una celda que no es un número en la severidad, el ancla o los pesos se muestra en el error", {
  tmp <- withr::local_tempdir()
  s <- data.table::fread(ejemplo_completo("severidad", "9100.csv"), encoding = "UTF-8",
                         colClasses = list(character = c("beta_covariable", "location_id_fuente", "fuente")))
  s[, dw_mean := as.character(dw_mean)][1L, dw_mean := "n/d"]
  fs <- file.path(tmp, "severidad.csv"); data.table::fwrite(s, fs)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, severidad = fs)),
               "^dl_insumos\\(\\): la columna dw_mean de la tabla severidad tiene valores que no son números: «n/d»")
  a <- data.table::fread(ejemplo_completo("ancla", "prevalencia.csv"), colClasses = list(character = "location_id"),
                         encoding = "UTF-8")
  a[, upper := as.character(upper)][.N, upper := "s.d."]
  fa <- file.path(tmp, "prevalencia.csv"); data.table::fwrite(a, fa)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, ancla_prevalencia = fa)),
               "la columna upper de la tabla ancla_prevalencia tiene valores que no son números: «s.d.»")
  w <- data.table::fread(ejemplo_completo("pesos_80mas.csv"))
  w[, peso := as.character(peso)][1L, peso := "?"]
  fw <- file.path(tmp, "pesos_80mas.csv"); data.table::fwrite(w, fw)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = FALSE, pesos_80mas = fw)),
               "la columna peso de la tabla pesos_80mas tiene valores que no son números: «\\?»")
})

test_that("una celda que no se puede convertir en los proxies o en sequela_rei.csv se muestra al leer", {
  tmp <- withr::local_tempdir()
  x <- data.table::fread(ejemplo_completo("proxies_departamentales.csv"), colClasses = list(character = "location_id"))
  x[, valor_crudo := as.character(valor_crudo)][1L, valor_crudo := "n/d"]
  fx <- file.path(tmp, "proxies.csv"); data.table::fwrite(x, fx)
  expect_match(.mensaje(.insumos_con(datos = FALSE, proxies = fx)),
               paste0("^dl_insumos\\(\\): la columna valor_crudo de la tabla cov_proxy tiene valores que no son ",
                      "números: «n/d»"))
  reg <- file.path(tmp, "registro"); dir.create(reg)
  file.copy(list.files(ejemplo_completo("registro"), full.names = TRUE), reg)
  s <- data.table::fread(file.path(reg, "sequela_rei.csv"), colClasses = list(character = "rol"))
  s[, sequela_id := as.character(sequela_id)][which(cause_id == 9101L)[1L], sequela_id := "s/n"]
  data.table::fwrite(s, file.path(reg, "sequela_rei.csv"))
  expect_match(.mensaje(suppressMessages(dl_insumos(dl_configuracion_ejemplo(9101L),
                                                    dl_rutas_ejemplo(9101L, datos = FALSE, proxies = FALSE,
                                                                     registro = reg)))),
               "la columna sequela_id de la tabla sequela_map tiene valores que no son enteros: «s/n»")
})

test_that("dl_rutas() pide la ruta de un archivo, no la tabla ya leída", {
  expect_error(dl_rutas(datos = data.frame(x = 1)),
               "^dl_rutas\\(\\): `datos` debe ser la ruta de un archivo o de una carpeta \\(un texto\\).*es una tabla")
  expect_error(dl_rutas(cambios = list(std_prior = 3)), "`ancla_prevalencia` debe ser la ruta de un archivo")
  expect_error(dl_rutas(cambios = list(ancla_prevalenci = "x.csv")), "quisiste decir `ancla_prevalencia`")
  expect_identical(dl_rutas(cambios = c(datos = "a.csv"))$datos, "a.csv")   # un vector con nombres también vale
})

test_that("las rutas se resuelven igual en todas las funciones: NULL, objeto de dl_rutas() o lista con nombres", {
  b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L),
                                   dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE)))
  # NULL: las de los insumos; una lista con nombres (en español o con las claves anteriores) pasa por dl_rutas()
  expect_identical(.dl_resolver_rutas(NULL, b), b$rutas)
  expect_identical(.dl_resolver_rutas(NULL), dl_rutas())
  lista <- list(poblacion = "p.csv", std_prior = "a.csv")
  expect_identical(.dl_resolver_rutas(lista), dl_rutas(cambios = lista))
  # una tabla u otro objeto del paquete no son rutas: el error nombra la función del usuario
  expect_error(dl_factor_comorbilidad(b, rutas = data.frame(x = 1)),
               "^dl_factor_comorbilidad\\(\\): `rutas` debe venir de dl_rutas\\(\\).*es una tabla")
  expect_error(dl_insumos(dl_configuracion_ejemplo(9100L), dl_configuracion_ejemplo(9100L)),
               "^dl_insumos\\(\\): `rutas` debe venir de dl_rutas\\(\\).*configuración")
  # las claves de una lista se citan en `rutas`, el argumento que escribió el usuario (no en `cambios`)
  cfg <- dl_configuracion_ejemplo(9100L)
  expect_error(dl_insumos(cfg, rutas = list(datoss = "x.csv")),
               "^dl_insumos\\(\\): clave\\(s\\) desconocida\\(s\\) en `rutas`: datoss \\(¿quisiste decir `datos`")
  expect_error(dl_insumos(cfg, rutas = list("x.csv")), "^dl_insumos\\(\\): cada elemento de `rutas` lleva nombre")
  expect_error(dl_bundle(cfg, paths = list(foo = "x.csv")),
               "^dl_bundle\\(\\): clave\\(s\\) desconocida\\(s\\) en `rutas`")
})

test_that("una pieza opcional sin dar es NULL; dada y ausente, un error que la nombra", {
  expect_null(.dl_path(dl_rutas(), "std_incidence", opcional = TRUE))
  r <- dl_rutas(ancla_incidencia = file.path(tempdir(), "no_existe", "incidencia.csv"))
  expect_error(.dl_path(r, "std_incidence", opcional = TRUE), "la ruta de «ancla_incidencia» no existe")
  # la ruta por defecto que deriva DATA_ROOT no la dio el usuario: la de una pieza opcional, si no existe, es NULL;
  # la de una pieza obligatoria sigue siendo un error que muestra la ruta
  withr::with_envvar(c(DATA_ROOT = withr::local_tempdir()), {
    expect_null(.dl_path(dl_rutas(), "std_incidence", opcional = TRUE))
    expect_null(.dl_path(dl_rutas(), "cov_std", opcional = TRUE))
    expect_error(.dl_path(dl_rutas(), "std_prior"), "la ruta de «ancla_prevalencia» no existe")
  })
  expect_error(.dl_path(dl_rutas(), "std_yld", motivo = "hace falta para algo"),
               "falta la ruta de «ancla_avd» en `rutas`: hace falta para algo\\. Pásala con rutas = dl_rutas")
  # los archivos de la carpeta `registro` se leen con el lector de CSV del paquete
  e <- .dl_etiquetas_es(dl_rutas_ejemplo(9100L))
  expect_true(all(c("tabla", "id", "name_es", "slug_es") %in% names(e)) && is.character(e$id))
  expect_error(.dl_archivo_registro(dl_rutas_ejemplo(9100L), "no_existe.csv"),
               "no existe el archivo «no_existe.csv» de la carpeta de `registro`")
})

test_that("una lectura memorizada se renueva si cambia el contenido, aunque el archivo conserve su fecha", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("a", f)
  t0 <- file.mtime(f)
  expect_identical(.dl_leer_memo(f, readLines), "a")
  writeLines("b", f)
  Sys.setFileTime(f, t0)                                      # como cp -p o rsync -t
  expect_identical(.dl_leer_memo(f, readLines), "b")
})
