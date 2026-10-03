test_that("la definición del contrato tiene las nueve tablas y el eje en cada una", {
  ref <- dismodlite:::.dl_tablas_ref()
  expect_setequal(unique(ref$tabla), dismodlite:::.DL_TABLAS)
  expect_true(all(ref$columna[ref$rol == "eje"] %in% dismodlite:::.DL_EJE))
  expect_identical(dismodlite:::.dl_columnas_contrato("poblacion", "exigidas"),
                   c("ubicacion", "anio", "sexo", "edad_inicio", "edad_fin", "poblacion"))
})

test_that("los nombres de columna se normalizan: minúsculas, espacios y alias", {
  d <- data.table::data.table(" Year " = 2023, SEX = "hombres", val = 1)
  n <- dismodlite:::.dl_normalizar_nombres(d, "poblacion")
  expect_identical(names(n), c("anio", "sexo", "poblacion"))
  # un alias no pisa una columna que ya trae el nombre canónico
  d2 <- data.table::data.table(anio = 2023, year = 2020)
  expect_error(dismodlite:::.dl_normalizar_nombres(d2, "poblacion"), "anio.*year")
})

test_that("el sexo se escribe en palabras o con su código", {
  expect_identical(dismodlite:::.dl_sexo_contrato(c("Hombres", "2", "ambos", "Male", " female ", "x")),
                   c("hombres", "mujeres", "ambos", "hombres", "mujeres", NA))
})

pob <- function(...) data.frame(ubicacion = "PE", anio = 2023L, sexo = "hombres", edad_inicio = c(30, 35),
                                edad_fin = c(35, NA), poblacion = c(100, 50), ...)

test_that("dl_tabla valida y devuelve la tabla en el contrato", {
  t <- dl_tabla("poblacion", pob())
  expect_s3_class(t, "dl_tabla")
  expect_identical(t$edad_fin, c(35, 125))
  expect_identical(attr(t, "tabla"), "poblacion")
  expect_match(attr(t, "origen"), "data.frame")
})

test_that("dl_tabla junta todos los problemas de una tabla en un solo error", {
  d <- pob()
  d$sexo <- c("hombres", "x")
  d$poblacion <- c("100", "muchos")
  e <- expect_error(dl_tabla("poblacion", d), class = "dl_error")
  expect_match(conditionMessage(e), "sexo")
  expect_match(conditionMessage(e), "poblacion")
  expect_length(e$problemas, 2L)
})

test_that("las bandas de edad: solapes, no enteras y abierta que no es la última", {
  d <- pob(); d$edad_inicio <- c(30, 33)
  expect_error(dl_tabla("poblacion", d), "solapan")
  d <- pob(); d$edad_fin <- c(34.5, NA)
  expect_error(dl_tabla("poblacion", d), "enteros")
  d <- pob(); d$edad_fin <- c(NA, 40)
  expect_error(dl_tabla("poblacion", d), "abierta")
  # un grupo de edad de GBD de menos de un año sí se admite
  d <- pob(); d$edad_inicio <- c(0, 0.0191780821917808); d$edad_fin <- c(0.0191780821917808, 0.0767123287671233)
  expect_s3_class(dl_tabla("poblacion", d), "dl_tabla")
})

test_that("un tibble, factores y números como texto se aceptan", {
  skip_if_not_installed("tibble")
  d <- tibble::as_tibble(pob())
  d$sexo <- factor(d$sexo)
  d$poblacion <- as.character(d$poblacion)
  t <- dl_tabla("poblacion", d)
  expect_type(t$poblacion, "double")
  expect_type(t$sexo, "character")
})

test_that("una tabla de un CSV de Excel en español da el mensaje de lectura con el nombre de la tabla", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("ubicacion;anio;sexo;edad_inicio;edad_fin;poblacion", "PE;2023;hombres;30;35;100"), f)
  expect_error(dl_tabla("poblacion", f), "poblacion")
})

test_that("la prevalencia mayor que 1 es un aviso, no un error", {
  a <- data.frame(anio = 2023L, sexo = "hombres", edad_inicio = 30, edad_fin = 35, medida = "prevalencia",
                  valor = 1500, inferior = 1200, superior = 1800)
  t <- dl_tabla("ancla", a)
  expect_match(attr(t, "avisos"), "100 000")
})

test_that("dl_plantilla escribe el encabezado y una fila de ejemplo", {
  f <- withr::local_tempfile(fileext = ".csv")
  dl_plantilla("poblacion", f)
  expect_identical(readLines(f)[1L], "ubicacion,anio,sexo,edad_inicio,edad_fin,poblacion")
  expect_s3_class(dl_plantilla("betas"), "data.frame")
  expect_error(dl_plantilla("proxies"), "tablas del contrato")
})

test_that("la banda abierta se juzga por fila: con los sexos intercalados la tabla es válida", {
  d <- data.frame(ubicacion = "PE", anio = 2023L, sexo = c("hombres", "mujeres", "hombres", "mujeres"),
                  edad_inicio = c(30, 30, 35, 35), edad_fin = c(35, 35, NA, NA), poblacion = c(100, 90, 50, 45))
  t <- dl_tabla("poblacion", d)
  expect_identical(t$edad_fin, c(35, 35, 125, 125))
  # la abierta que no es la última de su grupo se sigue rechazando, también con los grupos intercalados
  d$edad_fin <- c(NA, 35, NA, NA)
  e <- expect_error(dl_tabla("poblacion", d), "abierta", class = "dl_error")
  expect_match(conditionMessage(e), "fila\\(s\\) 1 del data.frame")
})

test_that("una celda vacía en una columna exigida es un problema (salvo edad_fin, la banda abierta)", {
  d <- pob(); d$sexo <- c("hombres", NA)
  e <- expect_error(dl_tabla("poblacion", d), class = "dl_error")
  expect_match(conditionMessage(e), "sexo: la columna es obligatoria")
  expect_match(conditionMessage(e), "fila\\(s\\) 2 del data.frame")
  d <- pob(); d$sexo <- c("hombres", "")
  expect_error(dl_tabla("poblacion", d), "sexo: la columna es obligatoria")
  d <- pob(); d$poblacion <- c(100, NA)
  e <- expect_error(dl_tabla("poblacion", d), class = "dl_error")
  expect_match(conditionMessage(e), "poblacion: la columna es obligatoria")
  expect_length(e$problemas, 1L)
  # una columna opcional puede quedar vacía y edad_fin vacío sigue siendo la banda abierta
  a <- data.frame(anio = 2023L, sexo = "hombres", edad_inicio = 30, edad_fin = NA, medida = "prevalencia",
                  valor = 0.1, inferior = 0.05, superior = 0.2, nombre_causa = NA_character_)
  expect_s3_class(dl_tabla("ancla", a), "dl_tabla")
})

test_that("la página ?dl_tablas se genera con tabla en HTML y lista en texto, con tildes en los tipos", {
  rd <- dismodlite:::.dl_rd_tablas()
  expect_true(any(grepl("\\ifelse{html}{", rd, fixed = TRUE)))
  expect_true(any(grepl("\\item{\\code{poblacion}}", rd, fixed = TRUE)))
  expect_true(any(grepl("número", rd)))
  expect_false(any(grepl("\\tab numero \\tab", rd, fixed = TRUE)))
})

test_that("un grupo que mezcla filas sin edad con filas por banda es un problema de validación, no un fallo", {
  cov <- data.frame(anio = 2023L, sexo = "ambos", covariable = "sev", valor = c(1, 2), edad_inicio = c(NA, 30),
                    edad_fin = c(NA, 35))
  e <- expect_error(dl_tabla("covariables", cov), class = "dl_error")
  expect_match(paste(e$problemas, collapse = "\n"), "mezcla filas sin edad.*fila\\(s\\) 1, 2 del data.frame")
  # el orden de las filas no cambia el diagnóstico
  e2 <- expect_error(dl_tabla("covariables", cov[2:1, ]), class = "dl_error")
  expect_match(paste(e2$problemas, collapse = "\n"), "mezcla filas sin edad")
  # filas sin edad en grupos distintos (otra covariable) y bandas que no se solapan siguen siendo válidas
  ok <- data.frame(anio = 2023L, sexo = "ambos", covariable = c("sev", "otra", "otra"), valor = 1,
                   edad_inicio = c(NA, 30, 35), edad_fin = c(NA, 35, 40))
  expect_s3_class(dl_tabla("covariables", ok), "dl_tabla")
  # las bandas solapadas se siguen detectando junto a una fila sin edad de otra covariable
  mal <- ok; mal$edad_inicio[3] <- 33
  expect_error(dl_tabla("covariables", mal), "se solapan")
})

test_that("las filas de los mensajes: la línea del CSV en un archivo, el número de fila en un data.frame", {
  d <- pob(); d$sexo <- c("hombres", "masculino")
  e <- expect_error(dl_tabla("poblacion", d), class = "dl_error")
  expect_match(conditionMessage(e), "masculino no es hombres, mujeres ni ambos \\(fila\\(s\\) 2 del data.frame\\)")
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE)
  e <- expect_error(dl_tabla("poblacion", f), class = "dl_error")
  expect_match(conditionMessage(e), "\\(fila\\(s\\) 3\\)")    # la fila 2 es la línea 3 del CSV (tras el encabezado)
  expect_no_match(conditionMessage(e), "data.frame")
})

test_that("el texto NA de write.csv() es vacío en columnas de números o lógicas, nunca en las de texto", {
  f <- withr::local_tempfile(fileext = ".csv")
  b <- data.frame(covariable = "haqi", efecto_sobre = "mortalidad_exceso", transformacion = "lineal", escala = NA,
                  beta = -0.012, inferior = NA, superior = NA, escala_confirmada = NA)
  utils::write.csv(b, f, row.names = FALSE)                       # escribe NA en las celdas vacías
  t <- dl_tabla("betas", f)
  expect_true(is.na(t$escala) && is.na(t$inferior) && is.na(t$escala_confirmada))
  u <- data.frame(ubicacion = c("PAIS", "NA"), nombre = c("País", "Namibia"), padre = c(NA, "PAIS"))
  utils::write.csv(u, f, row.names = FALSE)
  t <- dl_tabla("ubicaciones", f)
  expect_identical(t$ubicacion, c("PAIS", "NA"))                  # NA es un código de ubicación válido
})

test_that("un dato con anio_inicio mayor que anio_fin es un problema de la tabla datos", {
  d <- data.frame(ubicacion = "01", anio_inicio = c(2019, 2023), anio_fin = c(2021, 2020), sexo = "ambos",
                  edad_inicio = 40, edad_fin = 45, medida = "prevalencia", valor = 0.1, error_estandar = 0.01)
  e <- expect_error(dl_tabla("datos", d), class = "dl_error")
  expect_identical(e$problemas, "anio_inicio debe ser menor o igual que anio_fin (fila(s) 2 del data.frame)")
})

test_that("proxies_crudos: tabla del contrato con su plantilla y sus reglas", {
  d <- dl_plantilla("proxies_crudos")
  expect_s3_class(dl_tabla("proxies_crudos", d), "dl_tabla")
  d <- dl_plantilla("proxies_crudos"); d$anio[2] <- d$anio[1]
  expect_error(dl_tabla("proxies_crudos", d), "repetidas")
})
