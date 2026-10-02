test_that("los insumos materializan csmr (900006) junto a la prevalencia y en proporción", {
  b <- dl_insumos(cfg9100_completo(), rutas_nacional_completo())   # el formato completo agrupa las bandas de 80+
  expect_setequal(unique(b$prior_gbd$measure_id), c(5L, 900006L))
  cs <- b$prior_gbd[measure_id == 900006L]
  expect_true(all(cs$val < 0.01))                       # ya dividido entre 1e5
  expect_true(all(is.finite(cs$sigma_log) & cs$sigma_log > 0))
  expect_true(21L %in% cs$age_group_id)                 # 80+ agregada también para csmr
})

test_that("emr_prior informativo sin ruta std_csmr => error que pide el ancla de mortalidad", {
  po <- rutas_nacional(); po$std_csmr <- NULL
  expect_error(dl_insumos(cfg9100(), po), "ancla_mortalidad.*GBD Results")
})

test_that("dl_prior_emr: mu_log = log(csmr/prev), sd propagada", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  e <- dl_prior_emr(b)
  fila <- e[sex_id == 1L][order(age_start)][1]
  pr <- b$prior_gbd[measure_id == 5L & sex_id == 1L & age_group_id == fila$age_group_id]
  cs <- b$prior_gbd[measure_id == 900006L & sex_id == 1L & age_group_id == fila$age_group_id]
  expect_equal(fila$mu_log, log(cs$val) - log(pr$val), tolerance = 1e-12)
  expect_equal(fila$sd_log, sqrt(pr$sigma_log^2 + cs$sigma_log^2), tolerance = 1e-12)
})

test_that("nudo fuera de cobertura de bandas hereda la banda más cercana con sd x2", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  e <- dl_prior_emr(b)[sex_id == 1L]
  nudos <- c(25, 40, 50, 60, 70, 80, 95)
  en <- .dl_emr_en_nudos(e, nudos, list(0, 0.25))       # la cota del config
  data.table::setorder(e, age_start)
  expect_equal(en$mu_log[1], e$mu_log[1])               # nudo 25 -> banda 40-44
  expect_equal(en$sd_log[1], e$sd_log[1] * 2)           # con sd inflada
  expect_equal(en$mu_log[2], e$mu_log[1])               # nudo 40 cae DENTRO de 40-44
  expect_equal(en$sd_log[2], e$sd_log[1])               # sin inflar
  expect_identical(en$cota, c(0, 0.25))
})

test_that("prior plano: piso f_max / 1000, sin sd y punto de partida log(f_max) - 2 en cada nudo", {
  e <- .dl_emr_plano(c(30, 50, 70), list(0, 0.4))
  expect_equal(e$cota, c(0.4 / 1000, 0.4))
  expect_equal(e$mu_log, rep(log(0.4) - 2, 3))
  expect_true(all(is.na(e$sd_log)) && isTRUE(e$plano))
  expect_equal(.dl_emr_plano(30, c(0.01, 0.4))$cota, c(0.01, 0.4))    # la cota inferior impresa, si supera al piso
  expect_error(.dl_emr_plano(30, list(0.1)), "emr_prior.cota")
})

# Techo de EMR: el impreso en la configuración o, sin cota impresa, el derivado del ancla (k * max csmr/prev).
test_that("techo impreso: los insumos lo conservan y lo declaran como impreso", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  expect_identical(b$techo_emr$origen, "impreso")
  expect_identical(as.numeric(b$cfg$emr_prior$cota), c(0, 0.25))   # la del config del ejemplo
})

test_that("cota null: techo = [0, k * max(csmr/prev)] con k = 3 por defecto, declarado en los insumos", {
  cfg <- cfg9100(); cfg$emr_prior$cota <- NULL
  b <- dl_insumos(cfg, rutas_nacional())
  emr_max <- max(exp(dl_prior_emr(b)$mu_log))
  expect_identical(b$techo_emr$origen, "derivado")
  expect_equal(b$techo_emr$k, 3)
  expect_equal(as.numeric(b$cfg$emr_prior$cota), c(0, 3 * emr_max), tolerance = 1e-12)
  expect_equal(b$techo_emr$emr_max_ancla, emr_max, tolerance = 1e-12)
  # el hash de los insumos no cambia: el techo derivado no entra en las tablas congeladas
  expect_identical(b$hash, dl_insumos(cfg9100(), rutas_nacional())$hash)
})

test_that("emr_prior.factor_techo del config cambia k y dl_configuracion lo valida", {
  cfg <- cfg9100(); cfg$emr_prior$cota <- NULL; cfg$emr_prior$factor_techo <- 5
  b <- dl_insumos(cfg, rutas_nacional())
  expect_equal(b$techo_emr$k, 5)
  expect_equal(b$cfg$emr_prior$cota[2], 5 * b$techo_emr$emr_max_ancla, tolerance = 1e-12)
  d <- withr::local_tempdir()
  y <- yaml::read_yaml(file.path(ruta_acs(), "config", "9100.yaml"))
  y$emr_prior$factor_techo <- 0.5
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_error(dl_configuracion(9100L, d), "emr_prior.factor_techo")
  y$emr_prior$factor_techo <- NULL; y$emr_prior$cota <- list(0.1)
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_error(dl_configuracion(9100L, d), "emr_prior.cota")
})

test_that("el techo derivado llega a la likelihood y a la cascada (ctx y truncamiento)", {
  cfg <- cfg9100(); cfg$emr_prior$cota <- NULL
  b <- dl_insumos(cfg, rutas_nacional())
  ctx <- .dl_ctx(b, 1L)
  expect_equal(ctx$emr$cota, as.numeric(b$cfg$emr_prior$cota))
})

# Fracción aguda del csmr: las muertes de los primeros 28 días pertenecen al modelo agudo. El prior de EMR del
# modelo crónico y el techo derivado descuentan emr_prior.fraccion_aguda.valor del csmr; sin el campo, 0.
test_that("dl_prior_emr y el techo derivado descuentan la fraccion aguda del csmr", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  e0 <- dl_prior_emr(b)
  b2 <- .dl_bundle_con(b, fraccion_aguda = 0.5)
  e2 <- dl_prior_emr(b2)
  expect_equal(e2$mu_log, e0$mu_log + log(0.5), tolerance = 1e-12)
  expect_equal(e2$sd_log, e0$sd_log)
  cfg <- cfg9100(); cfg$emr_prior$cota <- NULL; cfg$emr_prior$fraccion_aguda <- list(valor = 0.5, procedencia = "test")
  t0 <- .dl_techo_emr(cfg9100() |> (\(c) { c$emr_prior$cota <- NULL; c })(), b$prior_gbd)
  t2 <- .dl_techo_emr(cfg, b$prior_gbd)
  expect_equal(t2$cota[2], t0$cota[2] * 0.5, tolerance = 1e-12)
  expect_equal(t2$emr_max_ancla, t0$emr_max_ancla * 0.5, tolerance = 1e-12)
})

test_that("dl_sensibilidad recorre fraccion_aguda cuando la rejilla la trae y la reporta en columna", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  g <- list(lambda = c(1.0), rho = c(0.5), fraccion_aguda = c(0, 0.5))
  o <- dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 1600L, calentamiento = 800L)
  s <- dl_sensibilidad(b, grilla = g, semilla = 5L, opciones = o)
  bandas <- nrow(unique(b$prior_gbd[measure_id == 5L, list(sex_id, age_group_id)]))
  expect_equal(nrow(s), 2L * bandas)
  expect_setequal(unique(s$fraccion_aguda), c(0, 0.5))
  s0 <- dl_sensibilidad(b, grilla = list(lambda = 1.0, rho = 0.5), semilla = 5L, opciones = o)
  expect_true(all(s0$fraccion_aguda == 0))
})

# Prior de EMR más laxo: emr_prior.factor_sd multiplica sd_log; mu_log y el techo no cambian.
test_that("emr_prior.factor_sd infla sd_log del prior y llega a la likelihood", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  e0 <- dl_prior_emr(b)
  cfg <- cfg9100(); cfg$emr_prior$factor_sd <- list(valor = 3, procedencia = "test")
  b3 <- dl_insumos(cfg, rutas_nacional())
  e3 <- dl_prior_emr(b3)
  expect_equal(e3$mu_log, e0$mu_log)
  expect_equal(e3$sd_log, 3 * e0$sd_log)
  expect_equal(b3$cfg$emr_prior$cota, b$cfg$emr_prior$cota)
  expect_equal(.dl_ctx(b3, 1L)$emr$sd_log, 3 * .dl_ctx(b, 1L)$emr$sd_log)
})
