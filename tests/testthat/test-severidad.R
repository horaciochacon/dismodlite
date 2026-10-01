# dl_severidad_desde_particion(): pi_e desde una partición de severidad (directa, hija dentro del padre o componente)
# y renormalización por s = sum_e pi_e (R/severidad.R). La partición del ejemplo completo (particion/acs_v1) reparte
# la prevalencia del padre 9100 entre las secuelas de sus tres hijas; los pesos de discapacidad del catálogo son
# 0 (799, asintomático), 0.02 (9801, leve) y 0.07 (9802, moderado).

# La partición de severidad y sus catálogos (secuelas, estados de salud) son del formato completo.
rutas_completo <- function() dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo")

test_that("hija: pi_e = suma de sus secuelas en la partici\u00f3n del padre / cuota (= prev(hija) / prev(padre))", {
  corrida <- ejemplo_completo("particion", "acs_v1")
  sev <- dl_severidad_desde_particion(corrida, 9101L, rutas = rutas_completo(), padre = 9100L)
  sq <- data.table::fread(list.files(file.path(corrida, "cause_sequela", "proportion"), full.names = TRUE))
  hija <- sq[cause_id == 9100L & sequela_id %in% c(91011L, 91012L, 91013L)]   # secuelas de 9101 en el catálogo
  cuota <- sum(hija$val)
  expect_equal(attr(sev, "cuota_hija"), cuota, tolerance = 1e-15)
  esperado <- c(`799` = hija[sequela_id == 91011L]$val, `9801` = hija[sequela_id == 91012L]$val,
                `9802` = hija[sequela_id == 91013L]$val) / cuota
  expect_equal(sev$proportion, unname(esperado[as.character(sev$health_state_id)]), tolerance = 1e-12)
  expect_equal(sum(sev$proportion), 1, tolerance = 1e-15)
  expect_equal(sev$dw_mean, c(`799` = 0, `9801` = 0.02, `9802` = 0.07)[as.character(sev$health_state_id)],
               ignore_attr = TRUE)
  expect_true(all(sev$location_id_fuente == "123") && all(sev$fuente == "mod"))
})

test_that("directa: las proporciones se dividen por s = su suma (redondeo) y s queda como atributo", {
  base <- withr::local_tempdir()
  s <- 1 + 8e-7
  filas <- list(list(799L, 0.5 * s, 0.45, 0.55), list(9801L, 0.3 * s, 0.25, 0.35), list(9802L, 0.2 * s, 0.15, 0.25))
  corrida <- split_sintetico(base, "r_v1", 9100L, filas)
  sev <- dl_severidad_desde_particion(corrida, 9100L, rutas = rutas_completo())
  leida <- data.table::fread(file.path(corrida, "cause_health_state", "proportion", "r_v1.csv"))
  suma <- sum(leida$val)                                                      # s, tal como se lee del CSV
  expect_equal(suma, s, tolerance = 1e-15)
  expect_identical(attr(sev, "renormalizacion"), suma)
  expect_identical(sev$health_state_id, c(799L, 9801L, 9802L))                 # ordenadas por proporción
  expect_identical(sev$proportion, leida$val / suma)
  expect_identical(sev$prop_lower, leida$lower / suma)                          # el intervalo, por el mismo s
  expect_identical(sev$prop_upper, leida$upper / suma)
  expect_equal(sum(sev$proportion), 1, tolerance = 1e-15)
  # una suma lejos de 1 no es redondeo: error
  mal <- split_sintetico(base, "r_v2", 9100L, list(list(799L, 0.5, 0.45, 0.55), list(9801L, 0.3, 0.25, 0.35)))
  expect_error(dl_severidad_desde_particion(mal, 9100L, rutas = rutas_completo()),
               "^dl_severidad_desde_particion\\(\\): .*suman 0.8")
})

test_that("componente: pi_e dentro de las secuelas elegidas y fracciones de prevalencia y de AVD", {
  base <- withr::local_tempdir()
  corrida <- split_causa_sintetico(base, "r_v1", 9101L, list(list(91011L, 0.5, 0.4, 0.6),
                                                             list(91012L, 0.35, 0.3, 0.4),
                                                             list(91013L, 0.15, 0.1, 0.2)))
  sev <- dl_severidad_desde_particion(corrida, 9101L, rutas = rutas_completo(), secuelas = 91013L)
  expect_identical(sev$health_state_id, 9802L)
  expect_identical(sev$proportion, 1)
  comp <- attr(sev, "componente")
  expect_equal(comp$fraccion_prevalencia, 0.15 / (0.5 + 0.35 + 0.15))
  # fracción de AVD = sum val x DW del componente / la de la causa (DW 0, 0.02 y 0.07)
  expect_equal(comp$fraccion_yld, 0.15 * 0.07 / (0.5 * 0 + 0.35 * 0.02 + 0.15 * 0.07))
  expect_identical(comp$health_state_ids, 9802L)
  # dos secuelas: pi_e dividido por su suma
  sev2 <- dl_severidad_desde_particion(corrida, 9101L, rutas = rutas_completo(), secuelas = c(91012L, 91013L))
  expect_equal(sev2$proportion, c(0.35, 0.15) / 0.5)
})

test_that("errores de dl_severidad_desde_particion() en espa\u00f1ol y con el nombre de la funci\u00f3n", {
  corrida <- ejemplo_completo("particion", "acs_v1")
  r <- rutas_completo()
  expect_error(dl_severidad_desde_particion(corrida, 9101L, rutas = r, padre = 9100L, secuelas = 91011L),
               "^dl_severidad_desde_particion\\(\\): .*excluyentes")
  expect_error(dl_severidad_desde_particion(corrida, 9199L, rutas = r, padre = 9100L),
               "^dl_severidad_desde_particion\\(\\): ninguna secuela .*9199")
  expect_error(dl_severidad_desde_particion(corrida, 9100L, rutas = r, beta_covariable = c(`12345` = "haqi")),
               "^dl_severidad_desde_particion\\(\\): `beta_covariable` .*12345")
  expect_error(dl_severidad_desde_particion(corrida, "noventa", rutas = r),
               "^dl_severidad_desde_particion\\(\\): `causa` debe ser")
  expect_error(dl_severidad_desde_particion(withr::local_tempdir(), 9100L, rutas = r),
               "^dl_severidad_desde_particion\\(\\): se esperaba un \u00fanico CSV")
})
