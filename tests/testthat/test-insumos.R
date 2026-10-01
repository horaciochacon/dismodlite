# rutas_nacional(), rutas_completas(), cfg9100() y cfg9100_datos() viven en helper-dismodlite.R (las comparten las
# pruebas del motor).

test_that("los insumos de 9100 se construyen y todas sus tablas validan", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  expect_s3_class(b, "dl_bundle")
  expect_identical(nrow(b$datos), 0L)
  expect_gt(nrow(b$prior_gbd), 0L)
  expect_setequal(unique(b$betas$parametro_objetivo), c("emr", "prevalencia"))   # extraccion.yaml: SEV y LDI, HAQ
  expect_true(21L %in% b$prior_gbd$age_group_id)          # banda 80+ agregada
  expect_false(any(b$prior_gbd$age_group_id %in% c(30L, 31L, 32L, 235L)))
  expect_match(b$hash, "^[0-9a-f]{64}$")
})

test_that("sigma_log se deriva del UI publicado", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  fila <- b$prior_gbd[1]
  expect_equal(fila$sigma_log, (log(fila$upper) - log(fila$lower)) / (2 * 1.96), tolerance = 1e-12)
})

# La evidencia, la extracción y la partición de severidad son del formato completo: esas pruebas usan el ejemplo en
# ese formato.
cfg9100_completo <- function() dl_configuracion_ejemplo(9100L, formato = "completo")
rutas_nacional_completo <- function() dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo")

test_that("anti-doble-conteo: csmr en medidas_entrada + CoD peruana en el store -> error", {
  cfg <- cfg9100_completo(); cfg$medidas_entrada <- list("csmr")
  expect_error(dl_insumos(cfg, rutas_nacional_completo()), "doble conteo.*CoD")
  cfg$anchor$evidencia_ghdx <- NULL                 # sin declararla, la evidencia se lee si las rutas la traen
  expect_error(dl_insumos(cfg, rutas_nacional_completo()), "doble conteo.*CoD")
})

test_that("fuente local no fatal en la evidencia: error con lambda = 1; con lambda < 1 pasa y declara el nid", {
  d <- withr::local_tempdir()
  fx <- rutas_nacional_completo()$ghdx_store
  file.copy(list.files(fx, full.names = TRUE), d)
  lst <- data.table::fread(file.path(d, "list.csv"))
  extra <- data.table::copy(lst[1])
  extra[, `:=`(cause_id = 9100L, component_id = 5L, location_id = 123L, nid = 999002L)]
  data.table::fwrite(rbind(lst, extra), file.path(d, "list.csv"))
  p <- rutas_nacional_completo(); p$ghdx_store <- d
  expect_error(dl_insumos(cfg9100_completo(), p), "0 fuentes locales.*lambda < 1")
  cfg <- cfg9100_completo(); cfg$anchor$lambda <- 0.5        # con fuentes locales, el ancla no va a peso completo
  b <- dl_insumos(cfg, p)
  expect_identical(b$fuentes_locales$nid_no_fatal, 999002L)
  expect_identical(dl_insumos(cfg9100_completo(), rutas_nacional_completo())$fuentes_locales$nid_no_fatal, integer())
})

test_that("token_transformacion se aplica al materializar betas", {
  d <- withr::local_tempdir()
  y <- yaml::read_yaml(file.path(ruta_acs(), "config", "9100.yaml"))
  idx <- which(vapply(y$transformaciones, function(t) t$covariate_name_short == "haqi", TRUE))
  y$transformaciones[[idx]]$transformacion <- "log"   # HAQI no imprime «Log-» -> viola
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  cfg <- dl_configuracion(9100L, d)
  expect_error(dl_insumos(cfg, rutas_nacional_completo()), "token_transformacion")
})

test_that("dl_congelar_insumos escribe los CSV y hash.json, y el hash es reproducible", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  d <- withr::local_tempdir()
  dl_congelar_insumos(b, d)
  expect_true(file.exists(file.path(d, "prior_gbd.csv")))
  h <- jsonlite::read_json(file.path(d, "hash.json"))
  expect_true("prior_gbd.csv" %in% names(h))
  b2 <- dl_insumos(cfg9100(), rutas_nacional())
  expect_identical(b$hash, b2$hash)
  expect_error(dl_congelar_insumos(b), "^dl_congelar_insumos\\(\\): falta `carpeta`")
  expect_error(dl_congelar_insumos(b, 3), "`carpeta` debe ser la ruta de una carpeta")
})

test_that("print de los insumos es legible", {
  expect_output(print(dl_insumos(cfg9100(), rutas_nacional())), "dl_bundle.*causa 9100")
})

test_that("los insumos materializan datos y cov_proxy, y la población trae los niveles 0 y 1", {
  b <- dl_insumos(cfg9100_datos(), rutas_completas())
  expect_gt(nrow(b$datos), 0L); expect_setequal(unique(b$datos$tipo_dato), c("csmr", "incidencia", "prev_estudio"))
  expect_setequal(unique(b$cov_proxy$location_id), sprintf("%02d", 1:25))   # los 25 departamentos
  expect_setequal(unique(b$poblacion$location_level), c(0L, 1L))
  # sin rutas: tablas vacías
  b0 <- dl_insumos(cfg9100(), rutas_nacional())
  expect_identical(nrow(b0$datos), 0L); expect_identical(nrow(b0$cov_proxy), 0L)
  # print(): una línea por tabla, con qué es; crosswalks (siempre vacía) no se muestra
  salida <- utils::capture.output(print(b))
  expect_match(salida[1], "^<dl_bundle> insumos de la causa 9100")
  expect_true(any(grepl(sprintf("^  cov_proxy +%d filas  valores subnacionales de las covariables", nrow(b$cov_proxy)),
                        salida)))
  expect_false(any(grepl("crosswalks", salida)))
  expect_length(salida, 1L + length(.DL_TABLAS_BUNDLE) - 1L)
})

test_that("reglas: tipo no declarado, doble uso de acquisition, proxy sin configuración, población que no cierra", {
  cfg <- cfg9100_datos(); cfg$medidas_entrada <- list("csmr")
  expect_error(dl_insumos(cfg, rutas_completas()), "tipos_declarados")
  d <- file.path(tempdir(), "bundle_v02"); dir.create(d, showWarnings = FALSE)
  datos <- data.table::fread(rutas_completas()$datos, colClasses = list(character = "location_id"))
  acq_prior <- unique(data.table::fread(rutas_completas()$std_csmr)$acquisition_id)
  datos[tipo_dato == "csmr", acquisition_id := acq_prior]                        # la del prior de EMR
  data.table::fwrite(datos, file.path(d, "datos.csv"))
  p <- rutas_completas(); p$datos <- file.path(d, "datos.csv")
  expect_error(dl_insumos(cfg9100_datos(), p), "doble_uso_acquisition .*misma fuente")
  proxy <- data.table::fread(rutas_completas()$cov_proxy, colClasses = list(character = "location_id"))
  proxy[covariate_id_proxy == 900101L, covariate_id_gbd := 57L]          # declarado, pero apunta a otra beta
  data.table::fwrite(proxy, file.path(d, "cov_proxy.csv"))
  p <- rutas_completas(); p$cov_proxy <- file.path(d, "cov_proxy.csv")
  expect_error(dl_insumos(cfg9100_datos(), p), "proxy_mapea_config")
  pob <- data.table::fread(rutas_completas()$poblacion, colClasses = list(character = "location_id"))
  pob[, val := as.numeric(val)][location_id == "01", val := val * 1.1]     # conteos enteros en el ejemplo
  data.table::fwrite(pob, file.path(d, "poblacion.csv"))
  p <- rutas_completas(); p$poblacion <- file.path(d, "poblacion.csv")
  expect_error(dl_insumos(cfg9100_datos(), p), "nivel1_suma_nivel0")
})

# Betas de varios modelos con la misma clave en el YAML de extracción; anchor.modelo_variante elige las del ancla.
.ext_con_variante <- function(extra, envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  y <- yaml::read_yaml(rutas_nacional()$extraction)
  y$covariables_gbd <- c(y$covariables_gbd, extra(y$covariables_gbd))
  f <- file.path(d, "extraccion.yaml"); yaml::write_yaml(y, f)
  p <- rutas_nacional(); p$extraction <- f; p
}
.dup_step2 <- function(cov) {   # copia de la fila LDI_pc/EMR etiquetada «step 2» (misma clave)
  i <- which(vapply(cov, function(x) identical(x$covariate_name_short, "LDI_pc"), TRUE))[1]
  x <- cov[[i]]; x$modelo_variante <- "step 2"; x$beta_valor <- "-9"; list(x)
}

test_that("sin anchor.modelo_variante, la clave duplicada lista las variantes disponibles", {
  p <- .ext_con_variante(.dup_step2)
  expect_error(dl_insumos(cfg9100(), p), "modelo_variante.*sin_variante.*step 2")
})

test_that("anchor.modelo_variante = sin_variante materializa solo el ancla y cuenta las excluidas", {
  p <- .ext_con_variante(.dup_step2)
  cfg <- cfg9100(); cfg$anchor$modelo_variante <- "sin_variante"
  b <- dl_insumos(cfg, p)
  expect_identical(nrow(b$betas), nrow(dl_insumos(cfg9100(), rutas_nacional())$betas))
  expect_false(-9 %in% b$betas$beta)
  expect_identical(b$seleccion_betas$modelo_variante, "sin_variante")
  expect_identical(b$seleccion_betas$filas_excluidas, 1L)
  expect_identical(b$seleccion_betas$variantes_excluidas, "step 2")
})

test_that("una etiqueta que no existe en el yaml falla listando las disponibles", {
  cfg <- cfg9100(); cfg$anchor$modelo_variante <- "step 4"
  expect_error(dl_insumos(cfg, rutas_nacional()), "«step 4».*disponibles.*sin_variante")
})

test_that("la covariable citada en severidad.beta_covariable entra aunque sea de otra variante", {
  p <- .ext_con_variante(function(cov) {   # haqi/proporcion re-etiquetada como modelo ajeno
    i <- which(vapply(cov, function(x) identical(x$covariate_name_short, "haqi"), TRUE))[1]
    x <- cov[[i]]; x$modelo_variante <- "envelope IC"; x$beta_valor <- "-0.05"; list(x)   # |beta| < 0.1: no dispara el chequeo de escala
  })
  d <- withr::local_tempdir()
  sev <- data.table::fread(p$severidad, colClasses = list(character = "beta_covariable"))
  sev$beta_covariable[1] <- "haqi"; data.table::fwrite(sev, file.path(d, "sev.csv")); p$severidad <- file.path(d, "sev.csv")
  cfg <- cfg9100(); cfg$anchor$modelo_variante <- "sin_variante"
  expect_error(dl_insumos(cfg, p), "clave duplicada")   # ambas haqi/proporcion entran: la citada y la del ancla
})

test_that("una predictiva sin covariate_name_short se excluye y se declara", {
  p <- .ext_con_variante(function(cov) {
    x <- cov[[1]]; x$covariate_id <- NULL; x$covariate_name_short <- NULL
    x$nombre_impreso <- "Log-transformed age-standardised prevalence of schistosomiasis"; list(x)
  })
  b <- dl_insumos(cfg9100(), p)
  expect_identical(nrow(b$betas), 3L)
  expect_identical(b$seleccion_betas$sin_covariable, "Log-transformed age-standardised prevalence of schistosomiasis")
})

# Unidad de modelado distinta de la unidad de extracción (una causa hija con el YAML de la causa padre):
# extraction.cause_id.
test_that("extraction.cause_id: los insumos cruzan el meta del YAML de extracción", {
  d <- withr::local_tempdir()
  y <- yaml::read_yaml(rutas_nacional()$extraction)
  y$meta <- list(causa_gbd = list(cause_id = 9100L, cause_name = "ACS"))
  f <- file.path(d, "extraccion.yaml"); yaml::write_yaml(y, f)
  p <- rutas_nacional(); p$extraction <- f
  expect_s3_class(dl_insumos(cfg9100(), p), "dl_bundle")             # sin campo: misma causa, pasa
  cfg <- cfg9100(); cfg$extraction <- list(cause_id = 494L, motivo = "test")
  expect_error(dl_insumos(cfg, p), "causa 9100 y la configuración espera la 494")
  y$meta$causa_gbd$cause_id <- 494L; yaml::write_yaml(y, f)
  b <- dl_insumos(cfg, p)                                        # YAML del padre, configuración de la hija
  expect_identical(unique(b$betas$cause_id), 9100L)                       # las betas llevan la causa modelada
  expect_error(dl_insumos(cfg9100(), p), "causa 494 y la configuración espera la 9100")
})

test_that("dl_configuracion valida extraction.cause_id y exige motivo si difiere", {
  d <- withr::local_tempdir()
  y <- yaml::read_yaml(file.path(ruta_acs(), "config", "9100.yaml"))
  y$extraction <- list(cause_id = 494)
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_error(dl_configuracion(9100L, d), "extraction.motivo")
  y$extraction <- list(cause_id = "x", motivo = "m")
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_error(dl_configuracion(9100L, d), "extraction.cause_id")
  y$extraction <- list(cause_id = 494, motivo = "prueba: el YAML de extracción es de la causa padre")
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_identical(dl_configuracion(9100L, d)$extraction$cause_id, 494)
})

# Escala de HAQI: betas lleva la columna escala (1 si la configuración no la declara para esa covariable); una beta
# de haqi con |beta| >= 0.1 y escala 1 no es creíble (exp(-1 x 20 puntos) ~ 0) salvo con escala_confirmada.
test_that("betas materializa escala y rechaza haqi con beta grande en escala 0-100 sin confirmar", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  expect_true("escala" %in% names(b$betas))
  expect_identical(b$betas[covariate_name_short == "haqi"]$escala, 1)
  expect_identical(b$betas[covariate_name_short != "haqi"]$escala, rep(1, nrow(b$betas) - 1L))
  betas <- rbind(fila_beta(), fila_beta(covariate_name_short = "haqi", parametro_objetivo = "emr",
                                        transformacion = "lineal", beta = -1, beta_lower = -1, beta_upper = -0.99))
  betas[, escala := 1]
  expect_error(.dl_chequear_escala(betas, list()), "escala")
  betas[, escala := 0.01]
  expect_silent(.dl_chequear_escala(betas, list()))
  betas[, escala := 1]
  expect_silent(.dl_chequear_escala(betas, list(haqi = TRUE)))
  betas[covariate_name_short == "haqi", beta := -0.015]
  expect_silent(.dl_chequear_escala(betas, list()))
})

# Bandas finas de la infancia: las estimaciones GBD traen 0-6 días, 7-27 días, 1-5 meses, 6-11 meses, 12-23 meses y
# 2-4 años; la malla anual del modelo y la población (<5) no las alojan. Se agregan a «<5 years» (id 1) como las
# finas de 80+, con peso igual al ancho de la banda en años (población uniforme dentro de 0-5, aproximación
# declarada) salvo que pesos_80mas traiga esos ids.
test_that("las bandas finas <5 del std se agregan a <5 years con peso por ancho de banda", {
  d <- withr::local_tempdir()
  std <- data.table::fread(rutas_nacional()$std_prior)
  # una fila del ancla de la causa y año de ajuste, en la métrica que lee el paquete (Rate, por 100 000)
  base <- std[sex_id == 1L & cause_id == 9100L & year == 2023L & metric_name == "Rate"][1]
  fila <- function(id, nm, v) { r <- data.table::copy(base); r[, `:=`(age_group_id = id, age_group_name = nm,
    val = v * 1e5, lower = v * 0.8e5, upper = v * 1.2e5)]; r }
  finas <- rbind(fila(2L, "0-6 days", 1e-6), fila(3L, "7-27 days", 1e-5), fila(388L, "1-5 months", 5e-5),
                 fila(389L, "6-11 months", 1e-4), fila(238L, "12-23 months", 2e-4), fila(34L, "2-4 years", 3e-4))
  data.table::fwrite(rbind(std, finas), file.path(d, "prior.csv"))
  p <- rutas_nacional(); p$std_prior <- file.path(d, "prior.csv")
  cfg <- cfg9100(); cfg$edad_inicio <- 0; cfg$nudos_incidencia <- list(0, 40, 50, 60, 70, 80, 95)
  cfg$edad_inicio_fuente <- "test"
  b <- suppressMessages(dl_insumos(cfg, p))
  pr <- b$prior_gbd[measure_id == 5L & sex_id == 1L]
  expect_true(1L %in% pr$age_group_id); expect_false(any(c(2L, 3L, 388L, 389L, 238L, 34L) %in% pr$age_group_id))
  anchos <- c(7, 21, 154.5, 182.5, 365, 1095) / 365      # 0-6 d, 7-27 d, 1-5 m, 6-11 m, 12-23 m, 2-4 a
  esperado <- sum(c(1e-6, 1e-5, 5e-5, 1e-4, 2e-4, 3e-4) * anchos) / sum(anchos)
  expect_equal(pr[age_group_id == 1L]$val, esperado, tolerance = 1e-6)
  expect_identical(pr[age_group_id == 1L]$age_group_name, "<5 years")
  expect_equal(pr[age_group_id == 1L]$age_start, 0); expect_equal(pr[age_group_id == 1L]$age_end, 5)
  expect_false(1L %in% b$prior_gbd[measure_id == 5L & sex_id == 2L]$age_group_id)   # solo se agrega donde hay finas
})

# All ages / Age-standardized: con edad_inicio 0, el filtro por age_start dejaría pasar «All ages» (0-125) como si
# fuera una banda más del ancla. Las bandas agregadas del catálogo (ids 22 y 27) nunca son celdas del ancla.
test_that("All ages y Age-standardized quedan fuera del ancla aunque edad_inicio sea 0", {
  d <- withr::local_tempdir()
  std <- data.table::fread(rutas_nacional()$std_prior)
  # una fila del ancla de la causa y año de ajuste, en la métrica que lee el paquete (Rate, por 100 000)
  base <- std[sex_id == 1L & cause_id == 9100L & year == 2023L & metric_name == "Rate"][1]
  agg <- data.table::copy(base)[, `:=`(age_group_id = 22L, age_group_name = "All ages", val = 500)]
  ags <- data.table::copy(base)[, `:=`(age_group_id = 27L, age_group_name = "Age-standardized", val = 400)]
  data.table::fwrite(rbind(std, agg, ags), file.path(d, "prior.csv"))
  p <- rutas_nacional(); p$std_prior <- file.path(d, "prior.csv")
  cfg <- cfg9100(); cfg$edad_inicio <- 0; cfg$nudos_incidencia <- list(0, 40, 50, 60, 70, 80, 95)
  cfg$edad_inicio_fuente <- "test"
  expect_message(b <- dl_insumos(cfg, p), "All ages")
  expect_false(any(c(22L, 27L) %in% b$prior_gbd$age_group_id))
})

# Componente de la causa: con anchor.componente, los insumos escalan la prevalencia del ancla a la fracción del
# componente (partición de severidad en `particion_severidad`), dejan el csmr tal cual, exigen que la tabla de
# severidad sea la del componente y exponen b$componente; dl_factor_comorbilidad() escala el AVD de referencia por
# su fracción.
test_that("anchor.componente escala la prevalencia del ancla y el YLD de referencia a las secuelas del componente", {
  base <- file.path(tempdir(), "split_comp_bundle"); unlink(base, recursive = TRUE)
  # el subtipo 9101 del ejemplo: secuela 91012 (leve, estado 9801) y 91011 (asintomática, estado 799)
  r9101 <- dl_rutas_ejemplo(9101L, datos = FALSE, proxies = FALSE, formato = "completo")
  split_causa_sintetico(base, "r_v1", 9101L, list(list(91012L, 0.4, 0.3, 0.5), list(91011L, 0.6, 0.5, 0.7)))
  sev <- dl_severidad_desde_particion(file.path(base, "r_v1"), 9101L, rutas = r9101, secuelas = 91012L)
  sev_csv <- file.path(base, "severidad_comp.csv"); data.table::fwrite(sev, sev_csv)
  cfg <- dl_configuracion_ejemplo(9101L, formato = "completo")
  cfg$anchor$componente <- list(sequela_ids = 91012L, motivo = "test: solo la leve")
  p <- r9101; p$severity_split <- file.path(base, "r_v1"); p$severidad <- sev_csv
  b0 <- dl_insumos(dl_configuracion_ejemplo(9101L, formato = "completo"), r9101)
  sin_split <- r9101; sin_split$severity_split <- NULL
  expect_error(dl_insumos(cfg, sin_split), "particion_severidad")             # exige la ruta de la partición
  b <- dl_insumos(cfg, p)
  expect_equal(b$componente$fraccion_prevalencia, 0.4); expect_equal(b$componente$fraccion_yld, 1)
  pv0 <- b0$prior_gbd[measure_id == 5L]; pv <- b$prior_gbd[measure_id == 5L]
  expect_equal(pv$val, 0.4 * pv0$val); expect_equal(pv$lower, 0.4 * pv0$lower); expect_equal(pv$upper, 0.4 * pv0$upper)
  expect_equal(b$prior_gbd[measure_id == 900006L]$val, b0$prior_gbd[measure_id == 900006L]$val)   # csmr sin escalar
  expect_identical(b$severidad$health_state_id, 9801L)
  # el AVD de referencia del factor de comorbilidad se escala por la fracción de AVD del componente (1 aquí: el
  # estado 799 no aporta AVD)
  cf <- dl_factor_comorbilidad(b, p)
  # factor f0 con la severidad completa (sum(pi*dw) sobre 799, 9801 y 9802); con el componente prev x 0.4,
  # sum(pi*dw) = dw_9801 y yld x 1 => factor = f0 x sum(pi*dw) / (0.4 x dw_9801), banda a banda
  cf0 <- dl_factor_comorbilidad(b0, r9101)
  sev0 <- data.table::fread(r9101$severidad)
  esperado <- cf0$factor * sum(sev0$proportion * sev0$dw_mean) / (0.4 * sev0[health_state_id == 9801L]$dw_mean)
  expect_identical(cf[, list(sex_id, age_group_id)], cf0[, list(sex_id, age_group_id)])
  expect_true(all(abs(cf$factor - esperado) < 1e-6))
  # una tabla de severidad que no es la del componente se rechaza
  p2 <- p; p2$severidad <- r9101$severidad
  expect_error(dl_insumos(cfg, p2), "componente")
})

# cov_valores: las covariables sin CSV propio en `covariables` toman su valor nacional de `covariables_std` (tabla de
# estimaciones GHDx), con su acquisition_id.
test_that("cov_valores: las covariables sin CSV en `covariables` salen de `covariables_std`", {
  raw <- withr::local_tempdir()
  file.copy(file.path(rutas_nacional()$ghdx_cov, c("HAQI.csv", "LDI_PC.csv")), raw)   # sin el CSV del SEV
  cols <- c("acquisition_id", "source", "round", "entity", "location_id", "location_name", "location_level", "year",
            "age_group_id", "age_group_name", "sex_id", "sex_name", "covariate_id", "covariate_name_short",
            "measure_id", "measure_name", "metric_id", "metric_name", "val", "lower", "upper", "ui_level")
  std <- data.table::rbindlist(lapply(2019:2023, function(y) data.table::data.table(
    acquisition_id = "ghdx_std_test", source = "ghdx", round = "2023", entity = "covariate", location_id = "123",
    location_name = "Peru", location_level = 0L, year = y, age_group_id = 27L, age_group_name = "Age-standardized",
    sex_id = c(1L, 2L), sex_name = c("Male", "Female"), covariate_id = 785L,
    covariate_name_short = "SEV_scalar_agestd_cvd_pvd", measure_id = 900003L, measure_name = "Covariate value",
    metric_id = 1L, metric_name = "Number", val = c(0.978, 0.776), lower = c(0.978, 0.776), upper = c(0.978, 0.776),
    ui_level = 0.95)))
  data.table::setcolorder(std, cols)
  csv <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(std, csv)
  p <- rutas_nacional(); p$ghdx_cov <- raw; p$cov_std <- csv
  b <- dl_insumos(cfg9100(), p)
  cv <- b$cov_valores[covariate_name_short == "SEV_scalar_agestd_cvd_pvd"]
  expect_identical(nrow(cv), 10L)
  expect_true(all(cv$acquisition_id == "ghdx_std_test") && all(cv$location_id == "123"))
  expect_true(all(b$cov_valores[covariate_name_short != "SEV_scalar_agestd_cvd_pvd"]$acquisition_id == "ghdx_cov_crudo_congelado"))
  # el CSV de `covariables` manda: con él presente, `covariables_std` no duplica ni cambia los insumos
  p2 <- rutas_nacional(); p2$cov_std <- csv
  expect_identical(dl_insumos(cfg9100(), p2)$hash, dl_insumos(cfg9100(), rutas_nacional())$hash)
  # sin CSV ni `covariables_std` para una covariable: los insumos siguen (el ajuste nacional no la usa: dX = 0) y es
  # dl_cascada() quien se detiene al no tener valor nacional
  p3 <- p; p3$cov_std <- NULL
  expect_false("SEV_scalar_agestd_cvd_pvd" %in% dl_insumos(cfg9100(), p3)$cov_valores$covariate_name_short)
  # covariables por banda de edad en `covariables_std`: la clave de cov_valores no tiene edad, así que solo entra la
  # banda estandarizada por edad (27) o la de todas las edades (22); sin ninguna de las dos, la covariable queda
  # fuera con un mensaje en vez de duplicar la clave.
  por_edad <- data.table::rbindlist(lapply(c(10L, 11L, 27L), function(a) { s <- data.table::copy(std); s[, age_group_id := a]; s }))
  csv2 <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(por_edad, csv2)
  p4 <- p; p4$cov_std <- csv2
  b4 <- dl_insumos(cfg9100(), p4)
  expect_true(all(b4$cov_valores[covariate_name_short == "SEV_scalar_agestd_cvd_pvd"]$age_group_id == 27L))
  solo_edad <- por_edad[age_group_id != 27L]
  csv3 <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(solo_edad, csv3)
  p5 <- p; p5$cov_std <- csv3
  expect_message(b5 <- dl_insumos(cfg9100(), p5), "banda")
  expect_false("SEV_scalar_agestd_cvd_pvd" %in% b5$cov_valores$covariate_name_short)
})

# El ancla del proxy (ancla_ghdx) y el valor nacional que la cascada resta (cov_valores del año de ajuste, mismo
# sexo o sexo 3) tienen que ser el mismo número; si no, dX arrastra un sesgo constante a todos los departamentos
# (otra adquisición GHDx, otra banda de edad, otra escala).
test_that("cov_proxy: ancla_igual_cov_valores — el ancla del proxy debe ser el valor nacional de cov_valores", {
  b <- dl_insumos(cfg9100_datos(), rutas_completas())
  cv <- b$cov_valores[covariate_id == 785L & year == 2023L & sex_id == 1L]$val
  expect_equal(unique(b$cov_proxy[covariate_id_proxy == 900101L & sex_id == 1L]$ancla_ghdx), cv)
  proxy <- data.table::fread(rutas_completas()$cov_proxy, colClasses = list(character = "location_id"))
  proxy[covariate_id_proxy == 900101L & sex_id == 1L, `:=`(ancla_ghdx = ancla_ghdx * 1.05, valor_calibrado = valor_calibrado * 1.05)]
  csv <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(proxy, csv)
  p <- rutas_completas(); p$cov_proxy <- csv
  expect_error(dl_insumos(cfg9100_datos(), p), "ancla_igual_cov_valores")
})

# La tabla de proxies puede traer los de varias causas: los insumos se quedan con los que declara la configuración;
# los demás no son un error (no se usan) y no entran al hash de los insumos.
test_that("cov_proxy: los proxies no declarados en covariables se descartan; los declarados siguen validando", {
  proxy <- data.table::fread(rutas_completas()$cov_proxy, colClasses = list(character = "location_id"))
  ajeno <- data.table::copy(proxy[covariate_id_proxy == 900101L])[, `:=`(covariate_id_proxy = 900104L, covariate_id_gbd = 1022L)]
  csv <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(rbind(proxy, ajeno), csv)
  p <- rutas_completas(); p$cov_proxy <- csv
  b <- dl_insumos(cfg9100_datos(), p)
  expect_setequal(unique(b$cov_proxy$covariate_id_proxy), unique(proxy$covariate_id_proxy))
  expect_identical(b$hash, dl_insumos(cfg9100_datos(), rutas_completas())$hash)   # el ajeno no cambia el hash
  # un proxy declarado que falta en la tabla sigue deteniendo la cascada
  p2 <- rutas_completas(); csv2 <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(proxy[covariate_id_proxy != 900102L], csv2); p2$cov_proxy <- csv2
  b2 <- dl_insumos(cfg9100_datos(), p2)
  o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
  f2 <- dl_ajustar(b2, opciones = o, semilla = 1L)
  expect_error(dl_cascada(f2, b2, kappa = 1, semilla = 1L), "cov_proxy")
})

# Sin medidas_entrada, la tabla datos entra solo para validar la cascada (nivel 1): las filas nacionales irían a la
# verosimilitud y la regla tipos_declarados las rechazaría.
test_that("datos: sin medidas_entrada, las filas de nivel 0 quedan fuera con aviso y el nivel 1 entra para validar", {
  cfg <- cfg9100_datos(); cfg$medidas_entrada <- list()
  # el mensaje cita medidas_entrada (con un proyecto simple, dl_insumos() lo cambia por datos_en_ajuste)
  expect_message(m <- .dl_materializar_datos(cfg, rutas_completas(), dl_esquema()), "medidas_entrada")
  expect_identical(nrow(m[location_level == 0L]), 0L)
  expect_gt(nrow(m[location_level == 1L]), 0L)
  b <- suppressMessages(dl_insumos(cfg, rutas_completas()))   # tipos_declarados ya no tiene nada que rechazar
  expect_identical(nrow(b$datos[location_level == 0L]), 0L)
})

# severidad.fuente = mod sin `severidad`: los insumos derivan la tabla de la corrida de partición de severidad
# (`particion_severidad`) con dl_severidad_desde_particion() (padre o componente).
test_that("dl_insumos materializa la severidad desde la partición cuando no llega la tabla", {
  base <- file.path(tempdir(), "split_bridge_bundle"); unlink(base, recursive = TRUE)
  sev0 <- data.table::fread(rutas_nacional_completo()$severidad)
  run <- split_sintetico(base, "r_v1", 9100L, Map(list, sev0$health_state_id, sev0$proportion, sev0$prop_lower,
                                                  sev0$prop_upper))
  cfg <- cfg9100_completo(); cfg$severidad <- list(fuente = "mod", run_id = "r_v1")
  p <- rutas_nacional_completo(); p$severidad <- NULL; p$severity_split <- run
  b <- dl_insumos(cfg, p)
  ref <- dl_severidad_desde_particion(file.path(base, "r_v1"), 9100L, rutas = rutas_nacional_completo())
  expect_equal(b$severidad$health_state_id, ref$health_state_id); expect_equal(b$severidad$proportion, ref$proportion)
  expect_equal(b$severidad$dw_mean, ref$dw_mean)
  expect_true(is.character(b$severidad$beta_covariable))
  # sin tabla ni partición: error claro
  p2 <- rutas_nacional_completo(); p2$severidad <- NULL; p2$severity_split <- NULL
  expect_error(dl_insumos(cfg, p2), "severidad")
})

test_that("datos: el nivel 1 de validación entra por cascada.heldout_anio; la verosimilitud, en el año de ajuste", {
  datos <- data.table::fread(rutas_completas()$datos, colClasses = list(character = "location_id"))
  datos[location_level == 1L, `:=`(year_start = 2019L, year_end = 2019L)]
  datos[location_level == 0L & sex_id == 1L & tipo_dato == "csmr", `:=`(year_start = 2019L, year_end = 2019L)]   # un dato nacional viejo
  csv <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(datos, csv)
  p <- rutas_completas(); p$datos <- csv
  cfg <- cfg9100_datos(); cfg$cascada$heldout_anio <- NULL                  # el ejemplo ya declara 2019
  # las filas de la causa que quedan fuera por el año, con un mensaje que dice cuántas y qué año deben incluir
  n_viejas <- nrow(datos[location_level == 0L & year_start == 2019L])
  expect_message(m0 <- .dl_materializar_datos(cfg, p, dl_esquema()),
                 sprintf("%d fila\\(s\\) .* quedan fuera: %d de otro año \\(deben incluir 2023\\)",
                         nrow(datos[year_start == 2019L]), nrow(datos[year_start == 2019L])),
                 class = "dl_mensaje_revision")
  expect_identical(nrow(m0[location_level == 1L]), 0L)                  # sin el campo: 2019 no es el año de ajuste
  cfg$cascada$heldout_anio <- list(valor = 2019L, procedencia = "test")
  expect_message(m1 <- .dl_materializar_datos(cfg, p, dl_esquema()),
                 sprintf("quedan fuera: %d de otro año \\(las nacionales deben incluir 2023 y las subnacionales 2019\\)",
                         n_viejas))
  expect_identical(nrow(m1[location_level == 1L]), nrow(datos[location_level == 1L]))
  expect_true(all(m1[location_level == 1L]$year_start == 2019L))
  # la verosimilitud (nivel 0) sigue siendo la del año de ajuste: el csmr nacional de 2019 no entra
  expect_false(any(m1[location_level == 0L]$year_start == 2019L))
  expect_identical(nrow(m1[location_level == 0L]), nrow(datos[location_level == 0L & year_start == 2023L]))
  # y los insumos enteros validan con la validación subnacional de otro año (el mensaje, ya probado arriba)
  b <- suppressMessages(dl_insumos(cfg, p))
  expect_gt(nrow(b$datos[location_level == 1L]), 0L)
})

test_that("datos: una celda por debajo de edad_inicio queda fuera con aviso (no tiene prediccion en la grilla)", {
  datos <- data.table::fread(rutas_completas()$datos, colClasses = list(character = "location_id"))
  joven <- data.table::copy(datos[location_level == 1L][1])[, `:=`(dato_id = "csmr_joven", age_start = 20, age_end = 25, age_group_id = 9L)]
  csv <- withr::local_tempfile(fileext = ".csv"); data.table::fwrite(rbind(datos, joven), csv)
  p <- rutas_completas(); p$datos <- csv
  cfg <- cfg9100_datos()                                     # edad_inicio 30
  expect_message(m <- .dl_materializar_datos(cfg, p, dl_esquema()), "edad_inicio")
  expect_false("csmr_joven" %in% m$dato_id)
  expect_identical(nrow(m), nrow(datos))
})

test_that("datos: las columnas num opcionales enteramente vacias sobreviven al viaje por CSV", {
  # fread() lee como lógica una columna sin ningún valor. Una tabla `datos` hecha de tasas de GBD deja n_efectivo y
  # completitud vacías en todas sus filas (una tasa no tiene tamaño muestral efectivo ni factor de completitud), y
  # sin la conversión al tipo del esquema la validación la rechazaría con «tipo esperado num».
  d <- rbind(
    fila_datos(dato_id = "csmr_gbd_123_1_15_2023", tipo_dato = "csmr", measure_id = 900006L,
               measure_name = "csmr", sex_id = 1L, sex_name = "Male", age_group_id = 15L,
               age_start = 50, age_end = 55, year_start = 2023L, year_end = 2023L,
               val = 1.03e-4, se = 2.9e-5, acquisition_id = "gbd_deaths_test"),
    fila_datos(dato_id = "csmr_gbd_123_2_15_2023", tipo_dato = "csmr", measure_id = 900006L,
               measure_name = "csmr", sex_id = 2L, sex_name = "Female", age_group_id = 15L,
               age_start = 50, age_end = 55, year_start = 2023L, year_end = 2023L,
               val = 1.13e-4, se = 3.2e-5, acquisition_id = "gbd_deaths_test"))
  expect_true(all(is.na(d$n_efectivo)) && all(is.na(d$completitud)))   # columnas enteramente vacías
  csv <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(d, csv)
  # Releído tal cual, fread() devuelve columnas lógicas: el caso que cubre la conversión.
  crudo <- data.table::fread(csv, colClasses = list(character = "location_id"))
  expect_true(is.logical(crudo$n_efectivo) && is.logical(crudo$completitud))

  p <- rutas_nacional(); p$datos <- csv
  cfg <- list(cause_id = 502L, years = list(ajuste = list(2023L)), sexos = list(1L, 2L), medidas_entrada = list("csmr"))
  m <- .dl_materializar_datos(cfg, p, dl_esquema())
  expect_identical(nrow(m), 2L)
  # Cada columna vuelve con el tipo que declara el esquema, y la tabla valida.
  expect_true(is.numeric(m$n_efectivo)); expect_true(is.numeric(m$completitud))
  expect_true(is.numeric(m$val) && is.numeric(m$se) && is.numeric(m$age_start))
  expect_true(is.integer(m$cause_id) && is.integer(m$sex_id) && is.integer(m$location_level))
  expect_true(is.character(m$crosswalk_id) && is.character(m$dato_id))
  expect_true(is.logical(m$es_referencia) && is.logical(m$outlier))
  expect_silent(dl_validar_tabla(m, "datos"))
})

# Sustitución declarada: el proxy de una beta puede anclarse en otra covariable GBD (la versión estandarizada por
# edad de una covariable por edad). proxy_mapea_config acepta el par declarado, ancla_igual_cov_valores compara con
# la sustituta y .dl_materializar_cov trae la sustituta a cov_valores aunque no tenga beta.
test_that("cov_proxy: sustituye — el proxy puede anclar en la covariable sustituta declarada; la sustituta entra en cov_valores", {
  d <- withr::local_tempdir()
  proxy <- data.table::fread(rutas_completas()$cov_proxy, colClasses = list(character = "location_id"))
  # el proxy 900101 (beta SEV, 785) pasa a anclar en LDI (57): mismo cociente departamental, otra escala nacional
  # (el ancla de LDI de cada año: la tabla trae 2019, 2023 y 2024)
  proxy[proxy[covariate_id_proxy == 900102L, list(ldi = unique(ancla_ghdx)), by = year], on = "year", ldi := i.ldi]
  proxy[covariate_id_proxy == 900101L, `:=`(valor_calibrado = valor_calibrado / ancla_ghdx * ldi,
                                             valor_calibrado_se = valor_calibrado_se / ancla_ghdx * ldi,
                                             ancla_ghdx = ldi, covariate_id_gbd = 57L)]
  proxy[, ldi := NULL]
  data.table::fwrite(proxy, file.path(d, "cov_proxy.csv"))
  p <- rutas_completas(); p$cov_proxy <- file.path(d, "cov_proxy.csv")
  expect_error(dl_insumos(cfg9100_datos(), p), "proxy_mapea_config")           # sin declarar: sigue siendo error
  cfg <- cfg9100_datos()
  cfg$covariables[[1]]$sustituye <- list(covariate_id = 57L, covariate_name_short = "LDI_pc", procedencia = "test")
  b <- dl_insumos(cfg, p)
  expect_s3_class(b, "dl_bundle")
  expect_true(all(b$cov_proxy[covariate_id_proxy == 900101L]$covariate_id_gbd == 57L))
  # una sustituta sin beta entra en cov_valores por nombre (CSV de `covariables`): haqi_bis = copia de haqi, id 999
  dir.create(file.path(d, "ghdx_cov"))
  for (f in list.files(rutas_completas()$ghdx_cov, full.names = TRUE)) file.copy(f, file.path(d, "ghdx_cov"))
  h <- data.table::fread(file.path(d, "ghdx_cov", "HAQI.csv")); h[, covariate_name_short := "haqi_bis"]
  data.table::fwrite(h, file.path(d, "ghdx_cov", "HAQI_BIS.csv"))
  p2 <- p; p2$ghdx_cov <- file.path(d, "ghdx_cov")
  proxy2 <- data.table::copy(proxy)
  cv <- dl_insumos(cfg9100_datos(), rutas_completas())$cov_valores
  haq <- cv[covariate_name_short == "haqi" & year == 2023L]$val[1]
  proxy2[covariate_id_proxy == 900101L, `:=`(valor_calibrado = valor_calibrado / ancla_ghdx * haq,
                                              valor_calibrado_se = valor_calibrado_se / ancla_ghdx * haq,
                                              ancla_ghdx = haq, covariate_id_gbd = 999L)]
  data.table::fwrite(proxy2, file.path(d, "cov_proxy2.csv")); p2$cov_proxy <- file.path(d, "cov_proxy2.csv")
  cfg$covariables[[1]]$sustituye <- list(covariate_id = 999L, covariate_name_short = "haqi_bis", procedencia = "test")
  b2 <- dl_insumos(cfg, p2)
  expect_true(999L %in% b2$cov_valores$covariate_id)
  expect_false("haqi_bis" %in% b2$betas$covariate_name_short)
})

# Estabilidad del RK4: con una remisión alta y el paso h = 1/nsub = 0,2, una simulación con EMR alta puede dar
# (r + f) h > 2,785 (límite de estabilidad de RK4 para y' = -k y) y prevalencias negativas a edades altas. dl_insumos
# detiene esa combinación antes del ajuste: hay que subir nsub en la configuración.
test_that("dl_insumos detiene un paso RK4 inestable ((remision + techo de EMR) / nsub > 2.5) y nsub mayor lo resuelve",
          {
  cfg <- cfg9100(); cfg$remision$valor <- 13          # 13 / 5 = 2.6 > 2.5 ya sin EMR (con el techo del ejemplo)
  expect_error(dl_insumos(cfg, rutas_nacional()), "nsub")
  cfg$nsub <- 20L
  expect_s3_class(dl_insumos(cfg, rutas_nacional()), "dl_bundle")
})

test_that(".dl_leer_std: en una carpeta de particiones, los CSV de adquisiciones no activas en el registro no entran", {
  # La carpeta conserva el CSV de la adquisición sustituida; sin el filtro, una nueva descarga que cubre los mismos
  # años duplicaría la clave del ancla («prior_gbd: clave duplicada»).
  d <- withr::local_tempdir(); reg <- withr::local_tempdir()
  fila <- function(acq, y) data.table::data.table(acquisition_id = acq, location_id = "123", year = y, val = 1)
  data.table::fwrite(fila("acq_nueva", 2019:2023), file.path(d, "acq_nueva.csv"))
  # Con parte (<acq>.<parte>.csv, como las particiones de población): el id es lo anterior al punto.
  data.table::fwrite(fila("acq_vieja", 2023), file.path(d, "acq_vieja.nacional.csv"))
  data.table::fwrite(fila("acq_sin_registrar", 2018), file.path(d, "acq_sin_registrar.csv"))
  writeLines(c("datasets:", "- acquisition_id: acq_nueva", "  status: active", "- acquisition_id: acq_vieja",
               "  status: superseded", "  superseded_by: acq_nueva"), file.path(reg, "datasets.yaml"))
  expect_message(x <- .dl_leer_std(d, reg), "acq_vieja")
  expect_setequal(unique(x$acquisition_id), c("acq_nueva", "acq_sin_registrar"))   # la no registrada entra (NA = activa)
  expect_equal(sum(x$year == 2023), 1L)
  # Sin registro se lee todo; la carpeta del registro también vale (además del archivo datasets.yaml).
  expect_equal(nrow(.dl_leer_std(d)), 7L)
  expect_equal(nrow(suppressMessages(.dl_leer_std(d, file.path(reg, "datasets.yaml")))), 6L)
  # Todo superseded: error claro, no una tabla vacía.
  writeLines(c("datasets:", "- acquisition_id: acq_nueva", "  status: superseded", "- acquisition_id: acq_vieja",
               "  status: superseded", "- acquisition_id: acq_sin_registrar", "  status: retired"), file.path(reg, "datasets.yaml"))
  expect_error(suppressMessages(.dl_leer_std(d, reg)), "no tiene archivos CSV de adquisiciones activas")
})

test_that(".dl_leer_std con subruta: la población se lee desde la carpeta raíz y la ronda la decide el registro", {
  # Con dos rondas de la serie de población, una sustituida por la otra, solo entran las particiones
  # population/population activas (una partición de otra entidad bajo la misma fuente no se mezcla).
  root <- withr::local_tempdir(); reg <- withr::local_tempdir()
  fila <- function(acq, y) data.table::data.table(acquisition_id = acq, location_id = "123", year = y, val = 1)
  for (sub in c("2017/population/population", "2020/population/population", "2020/indicator/value"))
    dir.create(file.path(root, sub), recursive = TRUE)
  data.table::fwrite(fila("inei2017_pob", 2019:2023), file.path(root, "2017/population/population/inei2017_pob.nacional.csv"))
  data.table::fwrite(fila("inei2020_pob", 1995:2030), file.path(root, "2020/population/population/inei2020_pob.nacional.csv"))
  data.table::fwrite(fila("inei2020_ind", 2020), file.path(root, "2020/indicator/value/inei2020_ind.csv"))
  writeLines(c("datasets:", "- acquisition_id: inei2017_pob", "  status: superseded", "  superseded_by: inei2020_pob",
               "- acquisition_id: inei2020_pob", "  status: active"), file.path(reg, "datasets.yaml"))
  expect_message(x <- .dl_leer_std(root, reg, subruta = "population/population"), "inei2017_pob")
  expect_equal(unique(x$acquisition_id), "inei2020_pob"); expect_equal(range(x$year), c(1995L, 2030L))
  expect_error(.dl_leer_std(root, reg, subruta = "no/existe"), "no tiene archivos CSV bajo «no/existe»")
  withr::local_envvar(DATA_ROOT = root)
  expect_equal(dl_rutas()$poblacion, file.path(root, "std", "inei"))
})

# Año del ancla: con years.ancla distinto de years.ajuste, los insumos leen el ancla, el csmr y las covariables GHDx
# del año del ancla y los reetiquetan al año de ajuste; población y cov_proxy son las del año de ajuste. Después
# (cascada, reglas, resumen, corrida) todo sigue filtrando por years.ajuste.
test_that("years.ancla: el ancla se reetiqueta al año de ajuste; población y cov_proxy del año de ajuste", {
  # el ejemplo trae población y proxies de 2019, 2023 y 2024: los insumos de 2024 se quedan con los de 2024
  p <- rutas_completas()
  pob <- data.table::fread(p$poblacion, colClasses = list(character = "location_id"))[year == 2024L]
  px <- data.table::fread(p$cov_proxy, colClasses = list(character = "location_id"))[year == 2024L]
  cfg <- cfg9100_datos(); cfg$years$ajuste <- list(2024L)
  cfg$years$ancla <- list(valor = 2023L, procedencia = "datos de ejemplo: sin ancla 2024")
  # los datos locales del ejemplo son de 2023 (las subnacionales, de 2019): con el año de ajuste 2024 quedan fuera, y
  # dos mensajes lo dicen
  expect_message(expect_message(b <- dl_insumos(cfg, p), "de otro año \\(las nacionales deben incluir 2024"),
                 "ninguna fila nacional de `datos` del año 2024 entra al ajuste")
  expect_true(all(b$prior_gbd$year == 2024L))
  expect_true(all(b$poblacion$year == 2024L) && nrow(b$poblacion) == nrow(pob))
  expect_true(all(b$cov_proxy$year == 2024L) && nrow(b$cov_proxy) == nrow(px))
  expect_true(any(b$cov_valores$year == 2024L) && !any(b$cov_valores$year == 2023L))
  # el mismo ancla que los insumos de 2023, salvo la etiqueta del año
  b23 <- dl_insumos(cfg9100_datos(), rutas_completas())
  expect_equal(b$prior_gbd[, !"year"], b23$prior_gbd[, !"year"], ignore_attr = TRUE)
  # sin years.ancla, un año de ajuste sin estimaciones GBD se detiene
  cfg$years$ancla <- NULL
  expect_error(dl_insumos(cfg, p), "no tiene filas del ancla")
})

test_that("dl_insumos() lee una sola vez el esquema y los pesos de las bandas finas (prevalencia y csmr)", {
  cfg <- cfg9100(); p <- rutas_nacional()
  n <- c(esquema = 0L, pesos = 0L)
  esquema <- dl_esquema; pesos <- .dl_leer_pesos_finas
  local_mocked_bindings(
    dl_esquema = function(...) { n[["esquema"]] <<- n[["esquema"]] + 1L; esquema(...) },
    .dl_leer_pesos_finas = function(paths) { n[["pesos"]] <<- n[["pesos"]] + 1L; pesos(paths) })
  b <- dl_insumos(cfg, p)
  expect_true(.dl_medida_id("csmr") %in% b$prior_gbd$measure_id)
  expect_identical(n, c(esquema = 1L, pesos = 1L))
})

test_that("la carpeta `registro` no necesita modelos_proporcion_deterioro.csv", {
  reg <- withr::local_tempdir()
  file.copy(list.files(rutas_nacional()$registry, full.names = TRUE), reg)
  unlink(file.path(reg, "modelos_proporcion_deterioro.csv"))
  p <- rutas_nacional(); p$registry <- reg
  expect_identical(dl_insumos(cfg9100(), p)$hash, dl_insumos(cfg9100(), rutas_nacional())$hash)
})

test_that("un ancla con bandas de edad que se solapan se detiene al armar los insumos", {
  # 50-69 años (age_group_id 25) junto a las bandas quinquenales: el prior de EMR no sabría qué banda toma un nudo
  f <- file.path(withr::local_tempdir(), "prevalencia.csv")
  a <- data.table::fread(ejemplo_completo("ancla", "prevalencia.csv"), colClasses = list(character = "location_id"))
  extra <- a[age_group_id == 15L][, `:=`(age_group_id = 25L, age_group_name = "50-69 years")]
  data.table::fwrite(rbind(a, extra), f)
  expect_error(dl_insumos(cfg9100(), dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, ancla_prevalencia = f, formato = "completo")),
               "^dl_insumos\\(\\): «prevalencia.csv» \\(`ancla_prevalencia`\\) trae bandas de edad que se solapan")
})

test_that("sin agrupar bandas finas, el ancla conserva 80-84 ... 95+ y no lee pesos_80mas", {
  rutas <- dl_rutas_ejemplo(9100, formato = "completo")
  rutas["pesos_80mas"] <- list(NULL)
  cfg <- dl_configuracion_ejemplo(9100, formato = "completo",
                                  cambios = list(anchor = list(agrupar_bandas_finas = FALSE)))
  b <- suppressMessages(dl_insumos(cfg, rutas))
  edades <- b$prior_gbd[measure_id == 5L & sex_id == 1L]$age_group_id
  expect_true(all(c(30L, 31L, 32L, 235L) %in% edades))
  expect_false(21L %in% edades)
})

test_that("sin la clave anchor.agrupar_bandas_finas (cfg sin validar o bundle anterior) se agrupa como con TRUE", {
  rutas <- dl_rutas_ejemplo(9100, formato = "completo")
  cfg <- dl_configuracion_ejemplo(9100, formato = "completo")
  expect_true(cfg$anchor$agrupar_bandas_finas)
  con <- suppressMessages(dl_insumos(cfg, rutas))$prior_gbd
  cfg$anchor$agrupar_bandas_finas <- NULL
  sin <- suppressMessages(dl_insumos(cfg, rutas))$prior_gbd
  expect_identical(sin, con)
  expect_true(21L %in% sin$age_group_id)
})
