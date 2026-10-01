# Rutas y configuraciones de prueba sobre los datos sintéticos de ejemplo (inst/extdata/acs_peru, el formato
# simple): la causa padre 9100 (arteriopatía crónica sintética) en 2023, que usan casi todas las pruebas del motor.
# Las pruebas de lo que solo tiene el formato completo (evidencia, extracción, partición de severidad, registro,
# lectura de sus archivos) usan el mismo ejemplo en ese formato: ejemplo_completo() y formato = "completo".

# Archivo o carpeta del ejemplo en el formato completo (inst/extdata/acs_peru_completo).
ejemplo_completo <- function(...) file.path(ruta_acs(), ...)

# Las dos variantes de 9100 que usan las pruebas: el ajuste nacional puro (sin datos locales ni proxies
# departamentales; las tablas datos y cov_proxy de los insumos quedan vacías) y el completo (datos.csv y
# proxies_departamentales.csv).
# Por ahora usan el ejemplo en el formato completo (los mismos números): el simple se migra a las tablas del contrato.
rutas_nacional <- function() dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo")
rutas_completas <- function() dl_rutas_ejemplo(9100L, formato = "completo")

cfg9100 <- function() dl_configuracion_ejemplo(9100L, formato = "completo")

# El ejemplo simple (inst/extdata/acs_peru) todavía trae la configuración anterior (covariables y ubicacion_nacional):
# hasta que se migre a las tablas del contrato, lo que lo usa se salta. Se quita con el ejemplo nuevo.
ejemplo_simple_pendiente <- function() testthat::skip("contrato: el ejemplo se migra en la Tarea 9")

# 9100 con los datos locales en la verosimilitud: las tres medidas de datos.csv y lambda 0,5 (con csmr en el ajuste
# el ancla no va a peso completo). El held-out departamental es el del config (2019, el año de los datos de nivel 1).
cfg9100_datos <- function() {
  cfg <- cfg9100(); cfg$medidas_entrada <- list("prev_estudio", "incidencia", "csmr"); cfg$anchor$lambda <- 0.5
  cfg$decisiones <- list("lambda 0.5 porque csmr entra a la verosimilitud (datos de ejemplo)"); cfg
}

# Raíz del árbol fuente (la carpeta con DESCRIPTION y R/), o NULL si las pruebas corren sobre el paquete instalado
# (R CMD check), donde R/ ya no existe. La usan las pruebas que leen el código fuente: los archivos test-fuente-*.R.
raiz_fuente <- function() {
  r <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  if (dir.exists(file.path(r, "R")) && file.exists(file.path(r, "DESCRIPTION"))) r else NULL
}

# ---- Proyectos en el formato simple ----

# Copia del proyecto de ejemplo (formato simple) en una carpeta temporal con un espacio en el nombre; `...`: archivos
# que se reemplazan (ruta relativa = sus líneas).
copia_ejemplo <- function(..., env = parent.frame()) {
  ejemplo_simple_pendiente()
  d <- dl_ejemplo(copiar_en = file.path(withr::local_tempdir(.local_envir = env), "mi proyecto"))
  cambios <- list(...)
  for (f in names(cambios)) writeLines(enc2utf8(cambios[[f]]), file.path(d, f), useBytes = TRUE)
  d
}

# Una tabla de un proyecto como texto (todas las columnas character, vacíos como ""), y su escritura en file.path(...).
leer_texto <- function(f) data.table::fread(f, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
escribir_texto <- function(x, ...) data.table::fwrite(x, file.path(...), eol = "\n")

# Bandas de edad del país ficticio: 40-44 ... 75-79 años y 80 y más (los grupos de edad 13 a 21 de GBD).
.BANDAS_FICTICIO <- data.table::data.table(
  age_id = 13:21, edad_inicio = seq(40, 80, 5), edad_fin = c(seq(45, 80, 5), 125),
  age_name = c(sprintf("%d-%d years", seq(40, 75, 5), seq(44, 79, 5)), "80+ years"))

# Proyecto simple de un país ficticio (location_id 999) con tres regiones de códigos libres (A, B y C), para probar que
# nada del paquete supone el Perú. Escribe en `d` config.yaml (las líneas `config`), ancla/ (una descarga de GBD Results
# de la causa `causa` llamada `nombre`, del año `anio`), poblacion.csv (solo las regiones: la nacional es su suma) y
# severidad.csv. El ancla es coherente con el modelo: la prevalencia de dl_edo() con una incidencia y una mortalidad en
# exceso (EMR) que crecen con la edad (10 % más de incidencia en los hombres), promediada en cada banda; la mortalidad
# es p x EMR, la incidencia i x (1 - p) y los AVD p x 0,037 (el peso de discapacidad medio de severidad.csv), por
# 100 000 (Rate), como la prevalencia (la métrica que lee el paquete). Devuelve `d`.
escribir_pais_ficticio <- function(d, causa, anio, config, nombre = "Enfermedad inventada") {
  dir.create(file.path(d, "ancla"), recursive = TRUE)
  writeLines(config, file.path(d, "config.yaml"))
  nudos <- c(40, 50, 60, 70, 80, 95)
  a <- data.table::rbindlist(lapply(1:2, function(sx) {
    e <- dl_edo(log(c(2, 4, 7, 11, 16, 20) * 1e-3 * (1 + 0.1 * (sx == 1L))), log(c(2, 2.5, 3.5, 5, 8, 12) * 1e-2),
                nudos, edad_inicio = 40)
    banda <- findInterval(e$edades, .BANDAS_FICTICIO$edad_inicio)
    x <- data.table::data.table(b = banda, p = e$p, csmr = e$p * e$f, inc = e$i * (1 - e$p))
    x <- x[, list(p = mean(p), csmr = mean(csmr), inc = mean(inc)), by = b]
    data.table::data.table(sex_id = sx, b = rep(x$b, 4L), measure_id = rep(c(5L, 1L, 6L, 3L), each = nrow(x)),
                           val = c(1e5 * x$p, 1e5 * x$csmr, 1e5 * x$inc, 1e5 * 0.037 * x$p))
  }))
  medidas <- data.table::data.table(
    measure_id = c(5L, 1L, 6L, 3L),
    measure_name = c("Prevalence", "Deaths", "Incidence", "YLDs (Years Lived with Disability)"),
    metric_id = c(3L, 3L, 3L, 3L), metric_name = c("Rate", "Rate", "Rate", "Rate"))
  a <- cbind(a, medidas[match(a$measure_id, medidas$measure_id), -"measure_id"], .BANDAS_FICTICIO[a$b])
  a[, `:=`(location_id = 999L, location_name = "Pa\u00eds ficticio", sex_name = c("Male", "Female")[sex_id],
           cause_id = causa, cause_name = nombre, year = anio, lower = val * 0.8, upper = val * 1.25)]
  data.table::fwrite(a[, list(measure_id, measure_name, location_id, location_name, sex_id, sex_name, age_id,
                              age_name, cause_id, cause_name, metric_id, metric_name, year, val, upper, lower)],
                     file.path(d, "ancla", "descarga_gbd.csv"))
  pob <- data.table::CJ(location_id = c("A", "B", "C"), sexo = c("hombres", "mujeres"),
                        b = seq_len(nrow(.BANDAS_FICTICIO)))
  pob[, `:=`(location_name = paste("Regi\u00f3n", location_id), anio = anio,
             edad_inicio = .BANDAS_FICTICIO$edad_inicio[b], edad_fin = .BANDAS_FICTICIO$edad_fin[b],
             poblacion = round(c(A = 20000, B = 35000, C = 15000)[location_id] * exp(-0.15 * (b - 1)) *
                                 ifelse(sexo == "mujeres", 1.1, 1)))]
  data.table::fwrite(pob[, list(location_id, location_name, anio, sexo, edad_inicio, edad_fin, poblacion)],
                     file.path(d, "poblacion.csv"))
  data.table::fwrite(data.table::data.table(
    causa = causa, estado = c("leve", "grave"), proporcion = c(0.7, 0.3), proporcion_inferior = c(0.6, 0.2),
    proporcion_superior = c(0.8, 0.4), peso_discapacidad = c(0.01, 0.1), peso_inferior = c(0.005, 0.07),
    peso_superior = c(0.02, 0.14)), file.path(d, "severidad.csv"))
  invisible(d)
}

# Constructores de filas mínimas válidas por tabla: una fila completa con valores de prueba; `...` reemplaza
# columnas (por ejemplo fila_datos(val = 0.05)). Los identificadores son arbitrarios: las reglas del contrato no
# dependen de ellos.
.fila <- function(base, ...) {
  o <- list(...)
  for (nm in names(o)) base[[nm]] <- o[[nm]]
  data.table::as.data.table(base)
}
mini_como <- function(...) .fila(list(location_id = "123", cause_family = "cvd",
  age_group_id = 10L, sex_id = 1L, factor = 0.9, dispersion = NA_real_, fuente = "test"), ...)
fila_datos <- function(...) .fila(list(dato_id = "t_1", cause_id = 502L, tipo_dato = "prev_estudio",
  measure_id = 5L, measure_name = "Prevalence", location_id = "123", location_name = "Peru",
  location_level = 0L, sex_id = 3L, sex_name = "Both", age_start = 40, age_end = 45,
  age_group_id = NA_integer_, year_start = 2019L, year_end = 2019L, val = 0.02, se = 0.004,
  x = NA_integer_, n = NA_integer_, n_efectivo = NA_real_, definicion = "ABI<0.9",
  es_referencia = TRUE, crosswalk_id = NA_character_, ajuste_completitud = FALSE, completitud = NA_real_,
  outlier = FALSE, outlier_motivo = NA_character_, acquisition_id = "lit_test_1",
  nid_ghdx = NA_integer_, cita = "test"), ...)
fila_prior <- function(...) .fila(list(cause_id = 502L, measure_id = 5L, location_id = "123",
  sex_id = 1L, age_group_id = 10L, age_group_name = "25-29 years", age_start = 25, age_end = 30,
  year = 2023L, val = 0.01, lower = 0.008, upper = 0.012, sigma_log = 0.1, lambda = 1,
  acquisition_id = "gbd_test"), ...)
fila_beta <- function(...) .fila(list(cause_id = 502L, covariate_id = 785L,
  covariate_name_short = "sev_scalar_pad", nombre_impreso = "Log-transformed age-standardised SEV scalar: PAD",
  parametro_objetivo = "prevalencia", transformacion = "log", transformacion_procedencia = "test", escala = 1,
  beta = 0.86, beta_lower = 0.79, beta_upper = 0.95, modo = "informativa", rol = "predictiva",
  nivel = "pais", fuente = "extraction"), ...)
fila_crosswalk <- function(...) .fila(list(crosswalk_id = "cw_502_claims", cause_id = 502L,
  definicion_alt = "claims", definicion_ref = "ABI<0.9", escala = "logit", direccion = "alt_a_ref",
  beta = -1.87, beta_lower = -1.92, beta_upper = -1.82, modo = "informativa", var_default = NA_real_,
  origen = "gbd", fuente = "tabla 4"), ...)
fila_proxy <- function(...) .fila(list(covariate_id_proxy = 900101L, covariate_id_gbd = 785L,
  location_id = "01", year = 2023L, sex_id = 3L, valor_crudo = 0.3, valor_calibrado = 15,
  valor_calibrado_se = 1.2, n_efectivo = 800, metodo_calibracion = "centrado_escalado_nacional",
  ancla_ghdx = 15, acquisition_id = "endes_test"), ...)
fila_severidad <- function(...) .fila(list(cause_id = 502L, health_state_id = 381L, proportion = 1,
  prop_lower = 0.9, prop_upper = 1, dw_mean = 0.041, dw_lower = 0.026, dw_upper = 0.062,
  beta_covariable = NA_character_, location_id_fuente = "123", fuente = "extraction"), ...)
fila_attr <- function(...) .fila(list(rei_id = 196L, cause_id = 492L, proportion_model = "valve_diseases",
  location_id = "123", year = 2023L, age_group_id = 22L, sex_id = 3L, p_causa = 1,
  p_lower = NA_real_, p_upper = NA_real_, suma_a_uno_dentro_de = "envelope", fuente = "test"), ...)
fila_q <- function(...) .fila(list(rei_id = 196L, rei_id_severidad = NA_integer_,
  health_state_id = NA_integer_, nivel = "asymptomatic", q_h = 1, q_lower = NA_real_,
  q_upper = NA_real_, invariante_en = "location,year,age,sex", cause_id_override = NA_integer_,
  location_id = NA_character_, year = NA_integer_, age_group_id = NA_integer_, sex_id = NA_integer_,
  fuente = "test"), ...)
fila_seq <- function(...) .fila(list(cause_id = 502L, sequela_id = 1L, sequela_name = "test",
  health_state_id = NA_integer_, rei_id = NA_integer_, rei_id_severidad = NA_integer_,
  rol = "directa"), ...)
fila_pob <- function(...) .fila(list(location_id = "123", location_level = 0L, year = 2023L,
  sex_id = 3L, age_group_id = 10L, val = 1000, acquisition_id = "inei_test"), ...)
fila_envelope <- function(...) .fila(list(rei_id = 196L, location_id = "123", year = 2023L,
  age_group_id = 22L, sex_id = 3L, val = 0.01, lower = 0.008, upper = 0.012, ui_level = 0.95,
  origen = "gbd_published", correcciones_pre_reparto = NA_character_, acquisition_id = "gbd_test"), ...)

# Corridas sintéticas de partición de severidad (mod/severity_split): una partición <entidad>/proportion/<run_id>.csv
# con las columnas del contrato estimates/v1. `filas` = lista de list(id, val, lower, upper). Las usa test-insumos.R.
.split_particion <- function(base, run_id, entidad, id_col, cause_id, cause_name, filas) {
  d <- file.path(base, run_id, entidad, "proportion"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  dt <- data.table::rbindlist(lapply(filas, function(f) {
    fila <- list(run_id = run_id, source = "gbd", round = "2023", entity = entidad, method = "severity_split",
                 location_id = "123", location_name = "Peru", location_level = 0L, year = 2023L, age_group_id = 22L,
                 age_group_name = "All ages", sex_id = 3L, sex_name = "Both", cause_id = cause_id, cause_name = cause_name)
    fila[[id_col]] <- f[[1]]; fila[[sub("_id$", "_name", id_col)]] <- "x"
    data.table::as.data.table(c(fila, list(measure_id = 900001L, measure_name = "Proportion", metric_id = 2L,
                                           metric_name = "Percent", val = f[[2]], lower = f[[3]], upper = f[[4]], ui_level = 0.95)))
  }))
  data.table::fwrite(dt, file.path(d, paste0(run_id, ".csv")))
  file.path(base, run_id)
}
# proporciones por estado de salud de la causa (lo que lee dl_severidad_desde_particion() sin padre ni componente)
split_sintetico <- function(base, run_id, cause_id, filas) {
  out <- .split_particion(base, run_id, "cause_health_state", "health_state_id", cause_id, "x", filas)
  yaml::write_yaml(list(run_id = run_id, method = "severity_split", causa = list(cause_id = cause_id)),
                   file.path(base, run_id, "manifest.yaml"))
  out
}
# proporciones por secuela de la causa (componente, anchor.componente)
split_causa_sintetico <- function(base, run_id, cause_id, filas)
  .split_particion(base, run_id, "cause_sequela", "sequela_id", cause_id, "x", filas)

# Corrida mínima de 9100 sin datos locales (cadenas muy cortas), memorizada en la sesión: insumos, opciones, ajuste,
# ajuste solo con el ancla, cascada, AVD del ajuste y de la cascada y etiquetas. La usan test-resumir.R y
# test-corrida.R.
corrida_mini <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"),
                                       dl_rutas_ejemplo(9100L, datos = FALSE, formato = "completo")))
      o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
      f <- dl_ajustar(b, o, semilla = 7L)
      f0 <- dl_ajustar_solo_prior(b, o, semilla = 7L, ajuste = f)
      casc <- suppressWarnings(dl_cascada(f, b, semilla = 7L))
      x <<- list(insumos = b, opciones = o, ajuste = f, ajuste_prior = f0, cascada = casc,
                 avd = dl_avd(f, b, semilla = 7L), avd_cascada = dl_avd(casc, b, semilla = 7L),
                 etiquetas = dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 7L))
    }
    x
  }
})
