# Mortalidad en exceso (EMR) fija en 0: `mortalidad_exceso: {prior: cero}` (emr_prior.tipo cero en el formato
# completo). theta = log i en los nudos, la EDO corre con f = 0 exacto y la log-posterior no tiene término de EMR.
# El proyecto de prueba es el de ejemplo con ese prior (copia_emr_cero(), helper-dismodlite.R): conserva sus proxies y
# sus betas (cascada por covariables), su ancla trae mortalidad y sus datos traen mortalidad subnacional reservada.

# ---- Tarea 1.1: configuración e insumos ----

test_that("formato completo: emr_prior.tipo cero exige procedencia y rechaza lo que ajusta un prior o un techo", {
  cero <- function(...) list(emr_prior = list(tipo = "cero", tipo_procedencia = "causa sin muertes", cota = NULL, ...))
  completa <- function(cambios) dl_configuracion(9100L, file.path(ruta_acs(), "config"), cambios = cambios)
  expect_identical(completa(cero())$emr_prior$tipo, "cero")
  expect_error(completa(list(emr_prior = list(tipo = "cero", cota = NULL))),
               "config[.]emr_prior[.]tipo_procedencia: cero exige procedencia")
  expect_error(completa(list(emr_prior = list(tipo = "cero", tipo_procedencia = "x"))),      # la cota del ejemplo
               "config[.]emr_prior[.]tipo: cero fija la mortalidad en exceso en 0 y no admite emr_prior[.]cota")
  e <- tryCatch(completa(cero(fraccion_aguda = list(valor = 0.3, procedencia = "x"), factor_techo = 4,
                              factor_sd = list(valor = 2, procedencia = "x"))), error = conditionMessage)
  for (campo in c("fraccion_aguda", "factor_sd", "factor_techo"))
    expect_match(e, sprintf("no admite emr_prior[.]%s ", campo), info = campo)
  expect_error(completa(c(cero(), list(medidas_entrada = c("incidencia", "csmr")))),
               "config[.]emr_prior[.]tipo: cero .* no admite csmr en medidas_entrada")
  expect_identical(completa(c(cero(), list(medidas_entrada = "incidencia")))$medidas_entrada, "incidencia")
  expect_error(completa(c(cero(), list(sensibilidad = list(fraccion_aguda = c(0, 0.3))))),
               "config[.]emr_prior[.]tipo: cero .* no admite sensibilidad[.]fraccion_aguda")
  expect_error(completa(list(emr_prior = list(tipo = "nula"))),
               "config[.]emr_prior[.]tipo: valores admitidos: informativo_edad, plano_cota o cero")
})

test_that("formato simple: `prior: cero` se traduce a emr_prior.tipo cero, sin techo que tomar por defecto", {
  cfg <- dl_proyecto(copia_emr_cero(), causa = 9100)$configuracion
  expect_identical(cfg$emr_prior, list(tipo = "cero", tipo_procedencia = .DL_PROCEDENCIA_SIMPLE))
  expect_false("mortalidad_exceso.techo" %in% names(cfg$origen$por_defecto))
  # con los otros priores el techo por defecto se sigue informando
  ctx <- list(ubicacion = "999", nombre = "x", subnacional = FALSE)
  base <- list(causa = 501L, anio = 2020L, edad_inicio = 40L)
  expect_true("mortalidad_exceso.techo" %in%
                names(.dl_config_simple(base, "config.yaml", 501L, ctx)$origen$por_defecto))
  expect_match(.dl_problemas_config_simple(c(base, list(mortalidad_exceso = list(prior = "nulo")))),
               "^mortalidad_exceso.prior: valor\\(es\\) no admitido\\(s\\): nulo; los admitidos son: desde_ancla, plano, cero$")
})

test_that("formato simple: `prior: cero` con techo, fracción aguda o mortalidad en el ajuste es un error claro", {
  problema <- function(extra, dentro = character()) {
    d <- copia_emr_cero(extra)
    f <- file.path(d, "config", "9100.yaml")
    cfg <- readLines(f, encoding = "UTF-8")
    writeLines(enc2utf8(append(cfg, dentro, which(cfg == "  prior: cero"))), f, useBytes = TRUE)
    e <- expect_error(dl_proyecto(d, causa = 9100), class = "dl_error")
    e$problemas
  }
  antes <- "^mortalidad_exceso.prior \\(emr_prior.tipo\\): cero fija la mortalidad en exceso en 0 y no admite "
  expect_match(problema(character(), "  techo: 0.25"), paste0(antes, "mortalidad_exceso.techo \\(no hay prior ni techo"))
  expect_match(problema(character(), "  fraccion_aguda: 0.3"), paste0(antes, "mortalidad_exceso.fraccion_aguda "))
  expect_match(problema("datos_en_ajuste: [incidencia, mortalidad]"),
               paste0(antes, "mortalidad en datos_en_ajuste \\(el modelo predice 0 muertes por la causa\\): quita ",
                      "mortalidad de datos_en_ajuste o usa otro prior$"))
  # la revisión del proyecto lo muestra en la configuración, sin detenerse
  d <- copia_emr_cero("datos_en_ajuste: [mortalidad]")
  salida <- utils::capture.output(r <- suppressMessages(dl_revisar_proyecto(d, causa = 9100)))
  expect_identical(r$estado[r$paso == "configuración"], "error")
  # los demás datos sí entran al ajuste
  expect_identical(dl_proyecto(copia_emr_cero("datos_en_ajuste: [incidencia]"), causa = 9100)$configuracion$medidas_entrada,
                   "incidencia")
})

test_that("insumos con `prior: cero`: techo [0, 0] de origen cero, y la mortalidad del ancla ni se exige ni se usa", {
  d <- copia_emr_cero()
  b <- suppressMessages(dl_insumos(dl_proyecto(d, causa = 9100)))
  expect_identical(b$techo_emr, list(cota = c(0, 0), origen = "cero", k = NA_real_, emr_max_ancla = NA_real_))
  expect_identical(b$cfg$emr_prior$cota, c(0, 0))
  # el ancla del ejemplo trae mortalidad: se ignora sin mensaje (como el prior plano con techo), no entra a los insumos
  expect_identical(unique(b$prior_gbd$measure_id), 5L)
  expect_error(dl_prior_emr(b), "^dl_prior_emr\\(\\): la mortalidad en exceso está fija en 0 \\(emr_prior.tipo cero\\)")
  # sin la mortalidad en la tabla ancla, los mismos insumos; con el prior por defecto, esa tabla no basta
  f <- file.path(d, "ancla", "sintetico_acs_v1.csv")
  a <- leer_texto(f)
  escribir_texto(a[measure_name != "Deaths"], f)
  sin <- dl_proyecto(d, causa = 9100)
  expect_identical(suppressMessages(dl_insumos(sin))$hash, b$hash)
  salida <- utils::capture.output(r <- suppressMessages(dl_revisar_proyecto(d, causa = 9100)))
  expect_false(any(r$estado == "error"))
  writeLines(sub("  prior: cero", "  techo: 0.25", readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8"),
                 fixed = TRUE), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  expect_error(dl_proyecto(d, causa = 9100), "ancla: no trae la mortalidad de la causa 9100", class = "dl_error")
})

test_that("la plantilla de dl_nuevo_proyecto() y la ayuda de la configuración nombran el prior cero", {
  d <- file.path(withr::local_tempdir(), "proyecto nuevo")
  suppressMessages(dl_nuevo_proyecto(d, causa = 501, anio = 2020, edad_inicio = 40))
  lineas <- readLines(file.path(d, "config.yaml"), encoding = "UTF-8")
  i <- which(startsWith(lineas, "# `mortalidad_exceso.prior`: "))
  expect_length(i, 1L)
  descripcion <- gsub(" #\\s+", " ", paste(lineas[i:(i + 4L)], collapse = " "))    # la descripción ocupa varias líneas
  expect_match(descripcion, "cero \\(la EMR queda fija en 0 y no se estima")
  expect_identical(names(.dl_vocabulario_simple("mortalidad_exceso.prior")), c("desde_ancla", "plano", "cero"))
})
