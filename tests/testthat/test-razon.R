# Reparto subnacional por razón (subnacional.modo: razon; R/razon.R): la configuración y su traducción, las reglas de
# la tabla razones, el cálculo del reparto y la corrida. El proyecto de prueba es el país ficticio con tres regiones
# (escribir_proyecto_razon(), helper-dismodlite.R).

proyecto_razon <- function(..., env = parent.frame()) {
  d <- file.path(withr::local_tempdir(.local_envir = env), "país razón")
  escribir_proyecto_razon(d, ...)
}
razones_de <- function(d) leer_texto(file.path(d, "razones.csv"))
revisar_callado <- function(...) { salida <- utils::capture.output(r <- dl_revisar_proyecto(...)); r }

# ---- La configuración y las reglas de la tabla ----

test_that("subnacional.modo: razon se traduce a cascada.modo: razon y la semilla del sorteo va en avanzado", {
  d <- proyecto_razon()
  p <- dl_proyecto(d)
  expect_identical(p$configuracion$origen$subnacional, "razon")
  expect_identical(p$configuracion$cascada$modo$valor, "razon")
  expect_null(p$configuracion$cascada$razon_semilla)
  expect_identical(names(p$tablas$razones), c("ubicacion", "anio", "razon", "error_log", "fuente"))
  expect_output(print(p), "subnacional: razon")
  b <- suppressMessages(dl_insumos(p))
  expect_identical(nrow(b$cov_proxy), 0L)
  expect_identical(b$contrato$razones$razon, c(1.4, 0.7, 0))
  # la semilla del sorteo: un entero en avanzado; otra cosa, o sin el modo razon, es un problema de la configuración
  d2 <- proyecto_razon(config = c("avanzado:", "  cascada:", "    razon_semilla: 20261002"))
  expect_identical(dl_proyecto(d2)$configuracion$cascada$razon_semilla, 20261002L)
  d3 <- proyecto_razon(config = c("avanzado:", "  cascada:", "    razon_semilla: 1.5"))
  expect_error(dl_proyecto(d3), "cascada.razon_semilla: debe ser un entero no negativo")
  cfg <- readLines(file.path(d2, "config.yaml"))
  writeLines(sub("modo: razon", "modo: plano", cfg), file.path(d2, "config.yaml"))
  expect_error(suppressWarnings(dl_proyecto(d2)), "cascada.razon_semilla: solo se usa con cascada.modo: razon")
})

test_that("modo razon sin la tabla razones, o con covariables subnacionales, es un problema de la configuración", {
  d <- proyecto_razon()
  unlink(file.path(d, "razones.csv"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_match(e$problemas, "^subnacional.modo: es razon y el proyecto no trae la tabla razones")
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "configuración" & r$estado == "error"], "no trae la tabla razones")
  # con valores subnacionales de una covariable (el ejemplo los calibra de proxies_crudos): el reparto no los usa
  d <- copia_ejemplo()
  cfg <- readLines(file.path(d, "config", "9101.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(append(cfg, "  modo: razon", match("subnacional:", cfg))), file.path(d, "config", "9101.yaml"),
             useBytes = TRUE)
  rz <- data.frame(ubicacion = sprintf("%02d", 1:25), anio = 2023L, razon = 1, error_log = 0)
  e <- expect_error(dl_proyecto(d, 9101, razones = rz), class = "dl_error")
  expect_match(e$problemas, "es razon y el proyecto trae valores subnacionales de la\\(s\\) covariable\\(s\\) .*haqi")
  expect_match(e$problemas, "el reparto por razón no usa covariables")
})

test_that("la tabla razones debe traer todas las ubicaciones subnacionales del año, y solo ellas", {
  d <- proyecto_razon()
  rz <- razones_de(d)
  problemas <- function(tabla) {
    escribir_texto(tabla, d, "razones.csv")
    expect_error(dl_proyecto(d), class = "dl_error")$problemas
  }
  # falta una ubicación: el error la nombra
  expect_identical(problemas(rz[ubicacion != "B"]), paste0(
    "razones: falta la razón de 2023 de la(s) ubicación(es) subnacional(es) B: el reparto necesita la de ",
    "todas las de la población de ese año"))
  # sobra la nacional
  sobra <- rbind(rz, data.table::data.table(anio = "2023", ubicacion = "999", razon = "1", error_log = "0",
                                            fuente = "x"))
  expect_match(problemas(sobra), "^razones: trae la razón de 2023 de 999, que no es/son")
  # todas en cero: no hay nada que repartir
  expect_match(problemas(data.table::copy(rz)[, razon := "0"]), "^razones: todas las razones de 2023 son 0")
  # una razón que no es finita
  expect_match(problemas(data.table::copy(rz)[ubicacion == "A", razon := "Inf"]),
               "^razones: razon y error_log deben ser números finitos \\(ubicación\\(es\\) A\\)")
  # el problema nombra la columna que no es finita en cada ubicación
  infinitas <- data.table::copy(rz)[ubicacion == "A", razon := "Inf"][ubicacion == "B", error_log := "Inf"]
  expect_identical(problemas(infinitas),
                   paste0("razones: razon y error_log deben ser números finitos (ubicación(es) A, B): razon en A; ",
                          "error_log en B"))
  # NaN y -Inf no llegan a la regla: la tabla los rechaza al leerla, con la columna y la fila
  expect_match(problemas(data.table::copy(rz)[ubicacion == "A", razon := "NaN"]), "^razon: NaN no es un número")
  expect_match(problemas(data.table::copy(rz)[ubicacion == "B", error_log := "NaN"]),
               "^error_log: NaN no es un número")
  expect_match(problemas(data.table::copy(rz)[ubicacion == "A", razon := "-Inf"]),
               "^razon: hay valores menores que 0")
  # una fila de la causa y otra sin causa para la misma ubicación
  dos <- rbind(data.table::copy(rz)[, causa := "7001"], data.table::copy(rz)[ubicacion == "A"][, causa := ""])
  expect_match(problemas(dos), "^razones: más de una razón de 2023 para la\\(s\\) ubicación\\(es\\) A")
  # las razones de otra causa no cuentan: la causa se queda sin razones del año
  expect_match(problemas(data.table::copy(rz)[, causa := "7002"]),
               "^razones: no trae las razones de la causa 7001 de 2023 .*años que trae: ninguno\\)$")
  # la revisión lo dice en el paso «proyecto», sin detenerse
  escribir_texto(rz[ubicacion != "B"], d, "razones.csv")
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "error"], "^razones: falta la razón de 2023")
})

test_that("la tabla razones sin el modo razon es un aviso: no se usa", {
  d <- proyecto_razon()
  cfg <- readLines(file.path(d, "config.yaml"))
  writeLines(sub("modo: razon", "modo: plano", cfg), file.path(d, "config.yaml"))
  expect_warning(p <- dl_proyecto(d), "razones: la tabla no se usa: subnacional.modo es plano")
  expect_identical(p$configuracion$cascada$modo$valor, "plana")
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "aviso"], "razones: la tabla no se usa")
  # sin el modo razon las reglas de la tabla no corren: una razón infinita y la fila de la nacional siguen en aviso
  rz <- razones_de(d)
  escribir_texto(rbind(data.table::copy(rz)[ubicacion == "A", razon := "Inf"],
                       data.table::data.table(anio = "2023", ubicacion = "999", razon = "1", error_log = "Inf",
                                              fuente = "x")), d, "razones.csv")
  expect_warning(p <- dl_proyecto(d), "razones: la tabla no se usa: subnacional.modo es plano")
  r <- revisar_callado(d)
  expect_false(any(r$estado == "error"))
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "aviso"], "razones: la tabla no se usa")
})

test_that("dl_correr(anios = ) con un año sin razones se detiene antes de correr el primero", {
  # razones de 2023 y de 2024 en la misma tabla, población de 2023, 2024 y 2025 y el ancla de 2023 (2024 se proyecta)
  d <- proyecto_razon(anios = c(2023L, 2024L), anios_poblacion = 2023:2025)
  expect_identical(suppressMessages(dl_proyecto(d, anio = 2024))$configuracion$cascada$modo$valor, "razon")
  rz <- razones_de(d)
  escribir_texto(rz[anio == "2023"], d, "razones.csv")
  salida <- withr::local_tempdir()
  e <- expect_error(suppressMessages(dl_correr(d, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                               carpeta_salida = salida, anios = c(2023, 2024))),
                    "^dl_correr\\(\\): el proyecto no se puede leer para el año 2024: ", class = "dl_error")
  expect_match(conditionMessage(e), paste0("razones: no trae las razones de la causa 7001 de 2024 \\(el año que se ",
                                           "estima; años que trae: 2023\\)"))
  expect_match(conditionMessage(e), "No se corrió ningún año")
  expect_identical(e$anio, 2024L)
  expect_identical(e$escritas, integer())
  expect_false(dir.exists(file.path(salida, "mod")))
})
