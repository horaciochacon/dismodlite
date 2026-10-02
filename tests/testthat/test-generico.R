# Un proyecto que no se parece al ejemplo: un país ficticio (location_id 999) con tres regiones de códigos libres (A, B
# y C), una causa inventada (8001), una covariable de nombre inventado con su valor nacional (una descarga del GHDx) y
# sus valores por región (la tabla covariables del contrato) y un solo año (2021). Si algo del paquete supusiera el
# Perú (ubicación 123, departamentos de dos dígitos, sus catálogos), el ejemplo o sus causas, esta corrida de punta a
# punta fallaría: ajuste nacional, cascada por covariables, AVD y la carpeta de la corrida, con cadenas cortas. El otro
# caso, la estimación subnacional plana (sin covariables) con la configuración mínima de tres claves, está en
# test-proyecto.R (proyecto_ficticio()).

# Escribe el proyecto en `d`: el país ficticio de escribir_pais_ficticio() (helper-dismodlite.R) con la causa 8001 en
# 2021, la beta de la covariable en betas.csv y, en covariables/, su descarga del GHDx (el valor nacional) y sus valores
# por región (una tabla del contrato).
escribir_proyecto_generico <- function(d) {
  escribir_pais_ficticio(d, 8001L, 2021L, c("causa: 8001", "anio: 2021", "edad_inicio: 40"))
  data.table::fwrite(data.table::data.table(covariable = "indice_inventado", efecto_sobre = "prevalencia",
                                            transformacion = "log", beta = 0.5, inferior = 0.3, superior = 0.7),
                     file.path(d, "betas.csv"))
  # covariable: valor nacional 50 (todas las edades, ambos sexos) y, por región, 40, 50 y el valor de C que hace que
  # el promedio ponderado por la población cierre en 50
  nacional <- 50
  dir.create(file.path(d, "covariables"))
  data.table::fwrite(data.table::data.table(
    covariate_id = 5001L, covariate_name_short = "indice_inventado", location_id = 999L,
    location_name = "Pa\u00eds ficticio", year_id = 2021L, age_group_id = 22L, age_group_name = "All Ages", sex_id = 3L,
    sex = "Both", mean_value = nacional, lower_value = 45, upper_value = 55),
    file.path(d, "covariables", "indice_inventado.csv"))
  pob <- data.table::fread(file.path(d, "poblacion.csv"))
  w <- pob[, list(w = sum(poblacion)), by = location_id]$w
  valor <- c(40, 50, NA)
  valor[3] <- (nacional * sum(w) - sum(w[1:2] * valor[1:2])) / w[3]
  data.table::fwrite(data.table::data.table(
    covariable = "indice_inventado", ubicacion = c("A", "B", "C"), anio = 2021L, sexo = "ambos",
    valor = format(valor, digits = 17), error_estandar = 0.5),
    file.path(d, "covariables", "regiones.csv"))
  invisible(d)
}

test_that("un proyecto de otro pa\u00eds, con regiones y una covariable propias, corre de punta a punta", {
  d <- file.path(withr::local_tempdir(), "pa\u00eds ficticio")
  escribir_proyecto_generico(d)

  p <- dl_proyecto(d)
  expect_identical(p$formato, "simple")
  expect_identical(.dl_loc_ancla(p$configuracion), "999")
  expect_identical(p$configuracion$origen$subnacional, "covariables")
  b <- suppressMessages(dl_insumos(p))
  expect_setequal(b$poblacion$location_id, c("999", "A", "B", "C"))
  expect_identical(b$poblacion[location_level == 0L, sum(val)], b$poblacion[location_level == 1L, sum(val)])
  expect_identical(b$betas$covariate_name_short, "indice_inventado")
  expect_setequal(b$cov_proxy$location_id, c("A", "B", "C"))

  # ajuste nacional, ajuste solo con el ancla y cascada por la covariable
  o <- dl_opciones_mcmc(simulaciones = 40L, cadenas = 2L, iteraciones = 2000L, calentamiento = 1000L)
  f <- dl_ajustar(b, o, semilla = 11L)
  f0 <- dl_ajustar_solo_prior(b, o, semilla = 11L, ajuste = f)
  casc <- suppressWarnings(dl_cascada(f, b, semilla = 11L))
  expect_setequal(casc$departamentos, c("A", "B", "C"))
  v <- suppressMessages(dl_validar_ancla(f, b, cascada = casc))

  # AVD y resumen: la covariable (A < B < C, beta positiva) ordena la prevalencia de las regiones
  y <- dl_avd(casc, b, semilla = 11L)
  expect_s3_class(y, "dl_yld")
  r <- dl_resumir(list(fit = casc, yld = y, bundle = b))
  expect_setequal(unique(r$celdas$location_id), c("999", "A", "B", "C"))
  expect_setequal(unique(r$celdas$measure_id), c(5L, 6L, 3L))       # prevalencia, incidencia y AVD
  expect_true(all(is.finite(r$celdas$val) & r$celdas$val > 0))
  prev <- data.table::dcast(r$celdas[measure_id == 5L], sex_id + age_group_id ~ location_id, value.var = "val")
  expect_identical(nrow(prev), 2L * nrow(.BANDAS_FICTICIO))
  expect_true(all(prev$A < prev$B & prev$B < prev$C))

  # carpeta de la corrida (forzada: cadenas cortas)
  lab <- dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 11L, cascada = casc)
  run <- dl_exportar_corrida(list(resumen = r, fit = casc, yld = y, bundle = b), nombre = "generico",
                             carpeta = withr::local_tempdir(), etiquetas = lab, validacion = v, forzar = TRUE)
  expect_s3_class(run, "dl_run")
  expect_true(file.exists(file.path(run$dir, "manifest.yaml")))
  expect_identical(run$manifest$params$anchor, 999L)
  expect_identical(run$manifest$cascada$modo, "proxy")
  expect_identical(run$manifest$cascada$departamentos, 3L)
  expect_identical(unlist(run$manifest$cascada$proxies), "indice_inventado")
  # sin notas en la configuración, las decisiones del manifiesto son una lista vacía
  expect_identical(run$manifest$decisiones, list())
  expect_true("decisiones: []" %in% readLines(file.path(run$dir, "manifest.yaml"), encoding = "UTF-8"))
  celdas <- data.table::fread(file.path(run$dir, "cause", "prevalence", paste0(run$run_id, ".csv")),
                              colClasses = list(character = "location_id"))
  expect_setequal(unique(celdas$location_id), c("999", "A", "B", "C"))
  # nada de la corrida nombra el Perú
  texto <- unlist(lapply(list.files(run$dir, pattern = "[.](yaml|csv)$", recursive = TRUE, full.names = TRUE),
                         readLines, warn = FALSE, encoding = "UTF-8"))
  expect_false(any(grepl("peru|per\u00fa", texto, ignore.case = TRUE)))
})
