# dl_factor_comorbilidad(): COMO_b = AVD del ancla / (prevalencia del ancla x sum_e pi_e DW_e), por sexo y banda
# (R/comorbilidad.R). En los datos de ejemplo el AVD del ancla se construyó con un factor que varía con la edad
# (1 - 0.004 (a - 30)) y el ancla lleva ruido: el factor esperado se calcula a mano desde los CSV del ancla y la tabla
# de severidad. Usan el ejemplo en el formato completo, cuyos archivos del ancla (Rate por 100 000) y pesos_80mas.csv
# lee el cálculo a mano.

# Factor esperado por sexo y banda del ancla: AVD / (prevalencia x suma de pi x DW), con las bandas finas 80+
# agregadas a la 21 con pesos_80mas (como dl_insumos()); la prevalencia y el AVD del archivo, en Rate por 100 000.
factor_como_a_mano <- function(p, causa = 9100L, anio = 2023L) {
  w <- data.table::fread(p$pesos_80mas)
  banda <- function(f, escala) {
    d <- data.table::fread(f, colClasses = list(character = "location_id"))
    d <- merge(d[cause_id == causa & year == anio & location_id == "123" & metric_name == "Rate"], w,
               by = c("age_group_id", "sex_id"), all.x = TRUE)
    d[age_group_id %in% c(30L, 31L, 32L, 235L), `:=`(val = sum(val * peso) / sum(peso), age_group_id = 21L),
      by = sex_id]
    unique(d[, list(sex_id, age_group_id, val = val / escala)])
  }
  sev <- data.table::fread(p$severidad)
  m <- merge(banda(p$std_prior, 1e5), banda(p$std_yld, 1e5), by = c("sex_id", "age_group_id"),
             suffixes = c("_prev", "_avd"))
  m[, list(sex_id, age_group_id, esperado = val_avd / (val_prev * sum(sev$proportion * sev$dw_mean)))]
}

test_that("dl_factor_comorbilidad reproduce el factor avd / (prev x sum(pi*dw)) del ancla de ejemplo", {
  b <- dl_insumos(cfg9100_completo(), rutas_nacional_completo())
  cf <- dl_factor_comorbilidad(b, rutas_nacional_completo())
  expect_setequal(names(cf), c("location_id", "cause_family", "age_group_id", "sex_id",
                               "factor", "dispersion", "fuente"))
  m <- merge(cf, factor_como_a_mano(rutas_nacional_completo()), by = c("sex_id", "age_group_id"))
  expect_identical(nrow(m), nrow(cf))
  expect_true(all(abs(m$factor - m$esperado) < 1e-6))
  expect_gt(diff(range(cf$factor)), 0.1)            # el factor varía con la edad en los datos de ejemplo
  expect_setequal(unique(cf$age_group_id), unique(b$prior_gbd[measure_id == 5L]$age_group_id))
  expect_true(all(cf$cause_family == "9100"))
})

test_that("dl_factor_comorbilidad no trunca: la calibraci\u00f3n etaria puede superar 1", {
  b <- dl_insumos(cfg9100_completo(), rutas_nacional_completo())
  tmp <- file.path(tempdir(), "yld_inflado.csv")
  y <- data.table::fread(rutas_nacional_completo()$std_yld, colClasses = list(character = "location_id"))
  y[, `:=`(val = val * 3, lower = lower * 3, upper = upper * 3)]
  data.table::fwrite(y, tmp)
  p2 <- rutas_nacional_completo(); p2$std_yld <- tmp
  cf0 <- dl_factor_comorbilidad(b, rutas_nacional_completo())
  cf <- dl_factor_comorbilidad(b, p2)
  expect_true(all(abs(cf$factor - 3 * cf0$factor) < 1e-6))   # 3 x el factor, sin tope
  expect_true(all(cf$factor > 1))
})

test_that("dl_factor_comorbilidad sin el ancla de AVD da un error que dice c\u00f3mo obtenerla", {
  b <- dl_insumos(cfg9100_completo(), rutas_nacional_completo())
  p2 <- rutas_nacional_completo(); p2$std_yld <- NULL
  expect_error(dl_factor_comorbilidad(b, p2),
               "^dl_factor_comorbilidad\\(\\): falta la ruta de «ancla_avd» en `rutas`: .*AVD de referencia.*GBD")
})

test_that("dl_factor_comorbilidad exige que el ancla de AVD cubra todas las bandas del ancla de prevalencia", {
  b <- dl_insumos(cfg9100_completo(), rutas_nacional_completo())
  tmp <- withr::local_tempfile(fileext = ".csv")
  y <- data.table::fread(rutas_nacional_completo()$std_yld, colClasses = list(character = "location_id"))
  data.table::fwrite(y[!(sex_id == 1L & age_group_id == 15L)], tmp)
  p2 <- rutas_nacional_completo(); p2$std_yld <- tmp
  expect_error(dl_factor_comorbilidad(b, p2), "^dl_factor_comorbilidad\\(\\): .*no cubre todas las bandas")
})
