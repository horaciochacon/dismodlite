# dl_avd(): AVD = prevalencia de la banda x sum_e pi_e DW_e x COMO (R/avd.R). Proporciones y pesos de discapacidad
# Beta por momentos, renormalización por simulación, canal de proporción con una beta por simulación y factor de
# comorbilidad por banda.

# Entrada diminuta hecha a mano: una ubicación (123), un sexo, edades anuales 0 a 3 y tres simulaciones; bandas del
# ancla [0, 2) y [2, 4); población por edad simple (100, 300, 200, 200); dos estados de severidad sin incertidumbre
# (pi = 0.25 y 0.75, DW = 0.1 y 0.4, así que sum_e pi_e DW_e = 0.325).
avd_diminuto <- function() {
  p <- c(0.01, 0.02, 0.03, 0.04,  0.02, 0.02, 0.05, 0.07,  0.00, 0.01, 0.02, 0.10)
  draws_q <- data.table::data.table(sex_id = 1L, draw = rep(1:3, each = 4), edad = rep(0:3, 3), p = p)
  ajuste <- structure(list(draws_q = draws_q, draws_par = list(`1` = matrix(0, nrow = 3, ncol = 2)),
                           bundle_hash = "diminuto"), class = "dl_fit")
  bandas_ancla <- data.table::data.table(age_group_id = c(101L, 102L), age_start = c(0, 2), age_end = c(2, 4))
  bandas_pobl <- data.table::data.table(age_group_id = 201:204, age_start = 0:3, age_end = 1:4)
  sev <- rbind(fila_severidad(cause_id = 1L, health_state_id = 1L, proportion = 0.25, prop_lower = 0.25,
                              prop_upper = 0.25, dw_mean = 0.1, dw_lower = 0.1, dw_upper = 0.1),
               fila_severidad(cause_id = 1L, health_state_id = 2L, proportion = 0.75, prop_lower = 0.75,
                              prop_upper = 0.75, dw_mean = 0.4, dw_lower = 0.4, dw_upper = 0.4))
  insumos <- structure(list(
    hash = "diminuto", loc_ancla = "123", severidad = sev,
    prior_gbd = data.table::data.table(measure_id = 5L, sex_id = 1L, bandas_ancla),
    poblacion = data.table::data.table(location_id = "123", sex_id = 1L, age_group_id = 201:204,
                                       val = c(100, 300, 200, 200)),
    bandas_pobl = bandas_pobl,
    betas = data.table::data.table(covariate_name_short = character(), beta = numeric(), beta_lower = numeric(),
                                   beta_upper = numeric(), modo = character())),
    class = "dl_bundle")
  # prevalencia de cada banda, a mano: promedio de p(a) ponderado por la población de cada edad
  p_banda <- c(0.25 * p[1] + 0.75 * p[2], 0.5 * p[3] + 0.5 * p[4],
               0.25 * p[5] + 0.75 * p[6], 0.5 * p[7] + 0.5 * p[8],
               0.25 * p[9] + 0.75 * p[10], 0.5 * p[11] + 0.5 * p[12])
  list(ajuste = ajuste, insumos = insumos,
       comorbilidad = data.table::data.table(sex_id = 1L, age_group_id = c(101L, 102L), factor = c(0.9, 0.8)),
       esperado = data.table::data.table(age_group_id = rep(c(101L, 102L), 3), draw = rep(1:3, each = 2),
                                         p = p_banda, como = rep(c(0.9, 0.8), 3)))
}

test_that("AVD = prevalencia de la banda x suma de pi x DW x COMO en una entrada diminuta hecha a mano", {
  x <- avd_diminuto()
  e <- x$esperado[order(age_group_id, draw)]
  sin_como <- dl_avd(x$ajuste, x$insumos, semilla = 1L)
  con_como <- dl_avd(x$ajuste, x$insumos, comorbilidad = x$comorbilidad, semilla = 1L)
  expect_identical(nrow(con_como$draws_yld), 6L)
  expect_identical(con_como$draws_yld$age_group_id, e$age_group_id)
  expect_identical(con_como$draws_yld$draw, e$draw)
  expect_equal(con_como$draws_prev_banda$val, e$p, tolerance = 1e-14)
  expect_equal(sin_como$draws_yld$val, e$p * 0.325, tolerance = 1e-14)
  expect_equal(con_como$draws_yld$val, e$p * 0.325 * e$como, tolerance = 1e-14)
  expect_false(sin_como$como_aplicado)
  expect_true(con_como$como_aplicado)
})

test_that("un factor de comorbilidad sin alguna banda del ancla detiene dl_avd() en vez de perder esas filas", {
  x <- avd_diminuto()
  expect_error(dl_avd(x$ajuste, x$insumos, comorbilidad = x$comorbilidad[age_group_id == 101L], semilla = 1L),
               "^dl_avd\\(\\): falta el factor de comorbilidad")
})

test_that("con proporciones y pesos inciertos, AVD = prevalencia x suma de pi x DW de las mismas simulaciones x COMO", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1500L,
                                                 calentamiento = 800L), semilla = 1L)
  cf <- dl_factor_comorbilidad(b, rutas_nacional())
  y <- dl_avd(f, b, comorbilidad = cf, semilla = 5L)
  s <- .dl_simular_pi_dw(b$severidad, n_sim = 20L, semilla = 5L)
  suma_pi_dw <- rowSums(s$pi_ke * s$dw_ke)
  expect_gt(stats::sd(suma_pi_dw), 0)                    # la tabla de ejemplo trae intervalos: pi y DW varían
  m <- merge(merge(y$draws_yld, y$draws_prev_banda, by = c("location_id", "sex_id", "age_group_id", "draw"),
                   suffixes = c("_avd", "_p")),
             cf[, list(sex_id, age_group_id, factor)], by = c("sex_id", "age_group_id"))
  expect_identical(nrow(m), nrow(y$draws_yld))
  expect_true(all(abs(m$val_avd - m$val_p * suma_pi_dw[m$draw] * m$factor) < 1e-15))
})

test_that("momentos Beta: media y amplitud del intervalo con menos de 1 % y 2 % de error", {
  forma <- .dl_beta_momentos(0.4, 0.31, 0.50)
  set.seed(1); x <- rbeta(2e5, forma$forma1, forma$forma2)
  expect_lt(abs(mean(x) - 0.4) / 0.4, 0.01)
  sd_obj <- (0.50 - 0.31) / 3.92
  expect_lt(abs(sd(x) - sd_obj) / sd_obj, 0.02)
  expect_equal(.dl_beta_momentos(0, 0, 0)$constante, 0)   # DW del estado asintomático
})

test_that("un intervalo que ninguna Beta admite entra como constante con aviso que nombra el elemento", {
  expect_warning(forma <- .dl_beta_momentos(0.9225, 0, 1.8924, que = "la proporci\u00f3n del estado 9802"),
                 "proporci\u00f3n del estado 9802.*Beta")
  expect_equal(forma$constante, 0.9225)
  ok <- .dl_beta_momentos(0.3, 0.2, 0.4)
  expect_true(is.null(ok$constante) && ok$forma1 > 0 && ok$forma2 > 0)
})

test_that("pi renormalizada suma 1 en cada simulaci\u00f3n y se reporta el desplazamiento de su media", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  s <- .dl_simular_pi_dw(b$severidad, n_sim = 500L, semilla = 7L)
  expect_true(all(abs(rowSums(s$pi_ke) - 1) < 1e-12))
  expect_identical(dim(s$dw_ke), dim(s$pi_ke))
  expect_setequal(s$desplazamiento$health_state_id, b$severidad$health_state_id)
  expect_true(all(is.finite(s$desplazamiento$desplazamiento)))
})

test_that("canal de proporci\u00f3n: dX = 0 no mueve pi; dX > 0 con beta < 0 baja el estado en logit", {
  pi0 <- matrix(c(0.6, 0.4), nrow = 3, ncol = 2, byrow = TRUE)
  bd <- matrix(-0.0061, nrow = 3, ncol = 1)
  igual <- .dl_aplicar_canal_proporcion(pi0, estados = 2L, beta_ke = bd, dX = 0)
  expect_equal(igual, pi0)
  mov <- .dl_aplicar_canal_proporcion(pi0, estados = 2L, beta_ke = bd, dX = 10)
  expect_true(all(mov[, 2] < 0.4) && all(abs(rowSums(mov) - 1) < 1e-12))
})

test_that("canal de proporci\u00f3n: beta_covariable sin beta en betas es error; con beta, dX = 0 no altera el AVD", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1500L,
                                                 calentamiento = 800L), semilla = 1L)
  y0 <- dl_avd(f, b, semilla = 1L)
  b2 <- b; b2$severidad <- data.table::copy(b$severidad)[2, beta_covariable := "haqi"]
  y1 <- dl_avd(f, b2, semilla = 1L)
  expect_equal(y1$draws_yld$val, y0$draws_yld$val)     # nacional: dX = 0
  b3 <- b; b3$severidad <- data.table::copy(b$severidad)[2, beta_covariable := "inexistente"]
  expect_error(dl_avd(f, b3, semilla = 1L), "^dl_avd\\(\\): .*sin beta.*inexistente")
})

test_that("dl_avd: caso degenerado = p_banda x DW exacto, y COMO multiplica", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 60L, cadenas = 2L, iteraciones = 3000L,
                                                 calentamiento = 1400L), semilla = 20260831L)
  b2 <- b
  b2$severidad <- fila_severidad(health_state_id = 380L, proportion = 1, prop_lower = 1,
                                 prop_upper = 1, dw_mean = 0.2, dw_lower = 0.2, dw_upper = 0.2)
  y <- dl_avd(f, b2, comorbilidad = NULL, semilla = 20260831L)
  # con proporciones y DW constantes, AVD = p_banda x 0.2 simulación a simulación
  m <- merge(y$draws_yld, y$draws_prev_banda, by = c("sex_id", "age_group_id", "draw"))
  expect_true(all(abs(m$val.x - m$val.y * 0.2) < 1e-12))
  cf <- dl_factor_comorbilidad(b, rutas_nacional())
  y2 <- dl_avd(f, b2, comorbilidad = cf, semilla = 20260831L)
  expect_true(y2$como_aplicado)
  # el factor del ejemplo varía por sexo y banda: cada celda del AVD se multiplica por el suyo
  m2 <- merge(merge(y2$draws_yld, y$draws_yld, by = c("sex_id", "age_group_id", "draw"), suffixes = c("_como", "_sin")),
              cf[, list(sex_id, age_group_id, factor)], by = c("sex_id", "age_group_id"))
  expect_identical(nrow(m2), nrow(y$draws_yld))
  expect_true(all(abs(m2$val_como - m2$val_sin * m2$factor) < 1e-9))
})

test_that("dl_avd exige semilla y una partici\u00f3n de severidad de la ubicaci\u00f3n del ancla", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1500L,
                                                 calentamiento = 800L), semilla = 1L)
  expect_error(dl_avd(f, b), "semilla")
  b3 <- b; b3$severidad <- data.table::copy(b$severidad)[1, location_id_fuente := "102"]
  expect_error(dl_avd(f, b3, semilla = 1L), "^dl_avd\\(\\): .*location_id_fuente 102")
})

test_that("print de dl_avd() en espa\u00f1ol", {
  x <- avd_diminuto()
  y <- dl_avd(x$ajuste, x$insumos, comorbilidad = x$comorbilidad, semilla = 1L)
  salida <- utils::capture.output(print(y))
  expect_match(salida[1], "AVD por simulaci\u00f3n.*factor de comorbilidad aplicado")
  expect_match(salida[2], "ubicaciones: 1 .*simulaciones: 3")
})
