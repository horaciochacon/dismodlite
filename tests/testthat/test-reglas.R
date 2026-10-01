test_that("toda regla del esquema está implementada", {
  sch <- dl_esquema()
  declaradas <- unique(unlist(lapply(sch$tablas, `[[`, "reglas")))
  faltan <- setdiff(declaradas, ls(.dl_reglas))
  expect_identical(faltan, character(0))
})

test_that("edad_valida: age_start >= age_end viola", {
  expect_error(dl_validar_tabla(fila_datos(age_start = 60, age_end = 40), "datos"), "edad_valida")
})
test_that("pareja_val_se_o_x_n: sin (val,se) ni (x,n) viola; con (x,n) pasa", {
  expect_error(dl_validar_tabla(fila_datos(val = NA_real_, se = NA_real_, x = NA_integer_, n = NA_integer_), "datos"),
               "pareja_val_se_o_x_n")
  expect_silent(dl_validar_tabla(fila_datos(val = NA_real_, se = NA_real_, x = 12L, n = 300L), "datos"))
})
test_that("crosswalk_si_alternativa: es_referencia FALSE sin crosswalk_id viola", {
  expect_error(dl_validar_tabla(fila_datos(es_referencia = FALSE, crosswalk_id = NA_character_), "datos"),
               "crosswalk_si_alternativa")
})
test_that("outlier_con_motivo y nivel_0_o_1", {
  expect_error(dl_validar_tabla(fila_datos(outlier = TRUE, outlier_motivo = NA_character_), "datos"),
               "outlier_con_motivo")
  # una celda vacía (así la escribe y la lee un CSV) también es un motivo que falta
  expect_error(dl_validar_tabla(fila_datos(outlier = TRUE, outlier_motivo = " "), "datos"), "outlier_con_motivo")
  expect_no_error(dl_validar_tabla(fila_datos(outlier = TRUE, outlier_motivo = "atípico"), "datos"))
  expect_error(dl_validar_tabla(fila_datos(location_id = "0101", location_level = 2L), "datos"), "nivel_0_o_1")
})
test_that("prior_gbd: ui_ordenado, lambda_en_0_1, location_es_123", {
  expect_error(dl_validar_tabla(fila_prior(lower = 0.5, val = 0.3, upper = 0.6), "prior_gbd"), "ui_ordenado")
  expect_error(dl_validar_tabla(fila_prior(lambda = 1.5), "prior_gbd"), "lambda_en_0_1")
  expect_error(dl_validar_tabla(fila_prior(location_id = "120"), "prior_gbd"), "location_es_123")
})
test_that("betas: modo_consistente_con_ic y token_transformacion", {
  expect_error(dl_validar_tabla(fila_beta(modo = "informativa", beta_lower = NA_real_), "betas"),
               "modo_consistente_con_ic")
  expect_error(dl_validar_tabla(fila_beta(covariate_name_short = "haqi", transformacion = "log",
                                          nombre_impreso = "Healthcare Access and Quality Index"), "betas"),
               "token_transformacion")
  expect_silent(dl_validar_tabla(fila_beta(), "betas"))
})
test_that("crosswalks: fija_con_var_default", {
  expect_error(dl_validar_tabla(fila_crosswalk(modo = "fija", beta_lower = NA_real_, beta_upper = NA_real_,
                                               var_default = NA_real_), "crosswalks"), "fija_con_var_default")
})
test_that("cov_proxy: proxy_acunado y promedio_cierra_ancla", {
  expect_error(dl_validar_tabla(fila_proxy(covariate_id_proxy = 57L), "cov_proxy"), "proxy_acunado")
  filas <- rbind(fila_proxy(location_id = "01", valor_calibrado = 10),
                 fila_proxy(location_id = "02", valor_calibrado = 30))
  ctx <- list(poblacion = data.table::data.table(location_id = c("01", "02"), val = c(1, 1)), tol = 1e-8)
  # ancla_ghdx = 15 por defecto pero el promedio poblacional es 20 -> viola
  expect_error(dl_validar_tabla(filas, "cov_proxy", contexto = ctx), "promedio_cierra_ancla")
  cierra <- rbind(fila_proxy(location_id = "01", valor_calibrado = 10),
                  fila_proxy(location_id = "02", valor_calibrado = 20))
  expect_silent(dl_validar_tabla(cierra, "cov_proxy", contexto = ctx))
  # Componente temporal de la calibración de los proxies: dos columnas opcionales del contrato; una tabla sin ellas
  # sigue validando y una con ellas no cae en «no está en el esquema».
  con_t <- cierra[, `:=`(metodo_temporal = "paseo_aleatorio", n_ediciones = 6L)]
  expect_silent(dl_validar_tabla(con_t, "cov_proxy", contexto = ctx))
})
test_that("cov_proxy: promedio_cierra_ancla cierra por banda con la población contenida; sin age_group_id, banda 22", {
  sch <- dl_esquema(); cat <- .dl_bandas_catalogo(rutas_nacional())
  pob <- rbind(fila_pob(location_id = "01", location_level = 1L, sex_id = 1L, age_group_id = 13L, val = 100),
               fila_pob(location_id = "01", location_level = 1L, sex_id = 1L, age_group_id = 17L, val = 300),
               fila_pob(location_id = "02", location_level = 1L, sex_id = 1L, age_group_id = 13L, val = 300),
               fila_pob(location_id = "02", location_level = 1L, sex_id = 1L, age_group_id = 17L, val = 100))
  ctx <- list(poblacion = pob, bandas_catalogo = cat, tol = 1e-8)
  # banda 24 (15-49) contiene la 13 (40-44); banda 25 (50-69) contiene la 17 (60-64): pesos distintos por banda
  px <- rbind(fila_proxy(location_id = "01", sex_id = 1L, age_group_id = 24L, valor_calibrado = 12, ancla_ghdx = 15),
              fila_proxy(location_id = "02", sex_id = 1L, age_group_id = 24L, valor_calibrado = 16, ancla_ghdx = 15),
              fila_proxy(location_id = "01", sex_id = 1L, age_group_id = 25L, valor_calibrado = 16, ancla_ghdx = 15),
              fila_proxy(location_id = "02", sex_id = 1L, age_group_id = 25L, valor_calibrado = 12, ancla_ghdx = 15))
  expect_silent(dl_validar_tabla(px, "cov_proxy", sch, ctx))   # (100*12 + 300*16)/400 = 15 y (300*16 + 100*12)/400 = 15
  px2 <- data.table::copy(px)[location_id == "01" & age_group_id == 24L, valor_calibrado := 13]
  expect_error(dl_validar_tabla(px2, "cov_proxy", sch, ctx),
               "promedio_cierra_ancla.*\\(proxy 900101, hombres, 15-49 años\\)")    # sin covariables en ctx: el id
  # sin la columna: banda 22 = toda la población (400 y 400): (14 + 16)/2 = 15 cierra
  px3 <- rbind(fila_proxy(location_id = "01", sex_id = 1L, valor_calibrado = 14, ancla_ghdx = 15),
               fila_proxy(location_id = "02", sex_id = 1L, valor_calibrado = 16, ancla_ghdx = 15))
  expect_silent(dl_validar_tabla(px3, "cov_proxy", sch, ctx))
  # una banda sin población contenida se detiene con la banda por delante
  px4 <- fila_proxy(location_id = "01", sex_id = 1L, age_group_id = 9L, valor_calibrado = 15, ancla_ghdx = 15)
  expect_error(dl_validar_tabla(px4, "cov_proxy", sch, ctx), "banda 9")
  p <- .dl_poblacion_en_bandas(pob, c(24L, 22L), cat)
  expect_equal(p[age_group_id == 24L & location_id == "01"]$pob, 100)
  expect_equal(p[age_group_id == 22L & location_id == "02"]$pob, 400)
})
test_that("severidad: proporciones_suman_uno y dw_en_0_1", {
  dos <- rbind(fila_severidad(health_state_id = 381L, proportion = 0.6),
               fila_severidad(health_state_id = 382L, proportion = 0.6))
  expect_error(dl_validar_tabla(dos, "severidad"), "proporciones_suman_uno")
  expect_error(dl_validar_tabla(fila_severidad(dw_mean = 1.2), "severidad"), "dw_en_0_1")
})
test_that("impairment_envelope reusa ui_ordenado", {
  expect_error(dl_validar_tabla(fila_envelope(lower = 0.5, val = 0.3, upper = 0.6), "impairment_envelope"),
               "ui_ordenado")
  expect_silent(dl_validar_tabla(fila_envelope(), "impairment_envelope"))
})
test_that("impairment_attribution: atribucion_suma_uno y modelo_padre_registrado", {
  dos <- rbind(fila_attr(cause_id = 492L, p_causa = 0.6), fila_attr(cause_id = 503L, p_causa = 0.6))
  expect_error(dl_validar_tabla(dos, "impairment_attribution"), "atribucion_suma_uno")
  reg <- data.table::data.table(rei_id = 196L, proportion_model = "valve_diseases", cause_id = 492L)
  expect_error(dl_validar_tabla(fila_attr(proportion_model = "inventado", p_causa = 1),
                                "impairment_attribution", contexto = list(modelos_proporcion = reg)),
               "modelo_padre_registrado")
  expect_silent(dl_validar_tabla(fila_attr(), "impairment_attribution", contexto = list(modelos_proporcion = reg)))
})
test_that("impairment_severity_split: split_suma_uno_con_asintomatico", {
  sin_asint <- rbind(fila_q(nivel = "mild", q_h = 0.5), fila_q(nivel = "moderate", q_h = 0.5))
  expect_error(dl_validar_tabla(sin_asint, "impairment_severity_split"), "asintomatico")
  no_suma <- rbind(fila_q(nivel = "asymptomatic", q_h = 0.5), fila_q(nivel = "mild", q_h = 0.4))
  expect_error(dl_validar_tabla(no_suma, "impairment_severity_split"), "split_suma_uno")
})
test_that("sequela_map: rol_deterioro_con_rei y residual_unico_por_causa", {
  expect_error(dl_validar_tabla(fila_seq(rol = "impairment_severity", rei_id = NA_integer_), "sequela_map"),
               "rol_deterioro_con_rei")
  dos <- rbind(fila_seq(sequela_id = 1L, rol = "residual_sin_deterioro"),
               fila_seq(sequela_id = 2L, rol = "residual_sin_deterioro"))
  expect_error(dl_validar_tabla(dos, "sequela_map"), "residual_unico_por_causa")
})
test_that("como_factor: factor_en_0_1 y poblacion: poblacion_no_negativa", {
  expect_error(dl_validar_tabla(mini_como(factor = 1.4), "como_factor"), "factor_en_0_1")
  expect_error(dl_validar_tabla(fila_pob(val = -5), "poblacion"), "poblacion_no_negativa")
})
