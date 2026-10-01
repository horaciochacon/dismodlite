# La prevalencia del ancla se lee en Rate / 100 000 (la proporción de la población); Percent solo para reproducir
# corridas de versiones anteriores (anchor.metrica_prevalencia). Ver .dl_metrica_std en R/esquema.R.

# Prevalencia del ancla de 9100 en los insumos, sin la banda 80+ (la 21, que se arma con las bandas finas y sus pesos).
prevalencia_ancla <- function(formato, cambios = NULL) {
  cfg <- dl_configuracion_ejemplo(9100, formato = formato, cambios = cambios)
  b <- suppressMessages(dl_insumos(cfg, dl_rutas_ejemplo(9100, formato = formato)))
  b$prior_gbd[measure_id == 5L & age_group_id != 21L][order(sex_id, age_group_id)]
}
como_antes <- list(anchor = list(metrica_prevalencia = list(valor = "Percent", procedencia = "corrida anterior")))

test_that("la prevalencia del ancla es Rate / 100 000 por defecto, en los dos formatos", {
  for (formato in c("completo", "simple")) {
    p <- prevalencia_ancla(formato)
    a <- data.table::fread(dl_ejemplo("ancla", "sintetico_acs_v1.csv"))
    rate <- a[measure_id == 5L & metric_name == "Rate" & cause_id == 9100L & year == 2023L]
    esperado <- rate$val[match(paste(p$sex_id, p$age_group_id), paste(rate$sex_id, rate$age_id))] / 1e5
    expect_identical(p$val, esperado)
  }
})

test_that("anchor.metrica_prevalencia: Percent lee lo mismo que las versiones hasta la 1.0.0", {
  p <- prevalencia_ancla("completo", como_antes)
  a <- data.table::fread(dl_ejemplo("ancla", "sintetico_acs_v1.csv"))
  pct <- a[measure_id == 5L & metric_name == "Percent" & cause_id == 9100L & year == 2023L]
  expect_identical(p$val, pct$val[match(paste(p$sex_id, p$age_group_id), paste(pct$sex_id, pct$age_id))])
  # en los datos sintéticos las dos métricas son la misma proporción: difieren solo en el último bit
  expect_equal(p$val, prevalencia_ancla("completo")$val, tolerance = 1e-14)
})

test_that("anchor.metrica_prevalencia exige un valor conocido y procedencia", {
  expect_error(dl_configuracion_ejemplo(9100, formato = "completo",
                                        cambios = list(anchor = list(metrica_prevalencia = list(valor = "Percent")))),
               "metrica_prevalencia.procedencia")
  expect_error(dl_configuracion_ejemplo(9100, formato = "completo", cambios = list(anchor = list(
    metrica_prevalencia = list(valor = "Number", procedencia = "x")))), "debe ser Rate \\(por defecto\\) o Percent")
  expect_error(dl_configuracion_ejemplo(9100, formato = "completo",
                                        cambios = list(anchor = list(metrica_prevalencia = "Percent"))),
               "debe ser un bloque \\{valor, procedencia\\}")
})
