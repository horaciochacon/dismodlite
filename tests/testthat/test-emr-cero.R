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

# ---- Tarea 1.2: log-posterior y ajuste, en R y en C++ ----

# Insumos del proyecto con la EMR fija en 0 y su ajuste de prueba con el motor `motor`, memorizados en la sesión.
emr_cero_mini <- local({
  x <- list()
  function(motor = "mh") {
    if (is.null(x$insumos)) x$insumos <<- suppressMessages(dl_insumos(dl_proyecto(copia_emr_cero(), causa = 9100)))
    if (is.null(x[[motor]])) {
      o <- dl_opciones_mcmc(simulaciones = 20L, cadenas = 2L, iteraciones = 800L, calentamiento = 400L, motor = motor)
      x[[motor]] <<- dl_ajustar(x$insumos, o, semilla = 3L, cache = FALSE)
    }
    list(insumos = x$insumos, ajuste = x[[motor]])
  }
})

test_that("EMR cero: theta solo trae log i, la EDO corre con f = 0 exacto y el término de EMR vale 0", {
  b <- emr_cero_mini()$insumos
  ctx <- .dl_ctx(b, 1L)
  nk <- length(ctx$nudos)
  expect_identical(ctx$emr, list(mu_log = numeric(), sd_log = numeric(), cota = c(0, 0), plano = TRUE, cero = TRUE))
  th <- .dl_theta_inicial(ctx)
  expect_identical(th, stats::setNames(rep(-6, nk), paste0("logi_", ctx$nudos)))
  m <- .dl_log_tasas_media(th, ctx)
  expect_identical(m$log_f, rep(-Inf, nrow(ctx$W)))
  sol <- .dl_edo_ctx(th, ctx)
  expect_identical(sol$f, numeric(length(ctx$anual))); expect_identical(sol$truncado, 0L)
  expect_true(all(sol$p[-1] > 0) && all(sol$p < 1))
  # la misma prevalencia que la EDO con f = 0 escrita a mano, y con r = 0, p = 1 - exp(-integral de i)
  referencia <- dl_edo_resolver(exp(m$log_i), numeric(nrow(ctx$W)), ctx$r_media, 0, ctx$nsub)[ctx$idx_anual_malla]
  expect_identical(sol$p, referencia)
  expect_equal(sol$p, 1 - exp(-exp(-6) * (ctx$anual - ctx$edad_inicio)), tolerance = 1e-10)
  lp <- .dl_lp_componentes(th, ctx)
  expect_identical(names(lp), .DL_LP_NOMBRES)
  expect_identical(unname(lp[["emr"]]), 0)
  expect_true(is.finite(lp[["total"]]))
  expect_equal(unname(lp[["total"]]), unname(lp[["suavidad"]] + lp[["ancla"]]), tolerance = 1e-12)
  # los otros priores no cambian: theta sigue trayendo log i y log f
  ctx_i <- .dl_ctx(dl_insumos(cfg9100(), rutas_nacional()), 1L)
  expect_false(.dl_ctx_emr_cero(ctx_i))
  expect_length(.dl_theta_inicial(ctx_i), 2L * length(ctx_i$nudos))
})

test_that("EMR cero: dl_ajustar() (motor mh) muestrea solo log i, en un bloque, y deja f = 0 en las simulaciones", {
  x <- emr_cero_mini("mh"); f <- x$ajuste; b <- x$insumos
  nudos <- as.numeric(unlist(b$cfg$nudos_incidencia))
  expect_identical(colnames(f$draws_par[["1"]]), paste0("logi_", nudos))
  expect_identical(f$mcmc$parametro, rep(paste0("logi_", nudos), 2L))
  expect_identical(unique(f$aceptacion$bloque), 1L)
  expect_true(all(f$draws_q$f == 0))
  expect_true(all(f$draws_q$p > 0 | f$draws_q$edad == b$cfg$edad_inicio))
  expect_output(print(f), "<dl_fit>")
  # el ajuste solo con el ancla es el mismo (sin datos en el ajuste), y la mortalidad en exceso estimada es 0
  f0 <- dl_ajustar_solo_prior(b, f$params, semilla = 3L, ajuste = f)
  expect_true(f0$prior_only); expect_identical(f0$draws_par, f$draws_par)
  emr <- dl_estimaciones(f, "mortalidad_exceso")
  expect_true(all(emr$media == 0 & emr$inferior == 0 & emr$superior == 0))
  # con la misma semilla, las mismas cadenas
  otra <- dl_ajustar(b, f$params, semilla = 3L, cache = FALSE)
  expect_identical(otra$draws_par, f$draws_par)
})

# Insumos del proyecto con la EMR fija en 0 y la incidencia nacional de la tabla datos en el ajuste (ancla.peso 0.5):
# de las filas nacionales de datos quedan solo las de incidencia (las demás tendrían que entrar al ajuste o excluirse).
insumos_emr_cero_incidencia <- function(env = parent.frame()) {
  d <- copia_emr_cero(c("datos_en_ajuste: [incidencia]", "ancla:", "  peso: 0.5"), env = env)
  datos <- leer_texto(file.path(d, "datos.csv"))
  escribir_texto(datos[ubicacion != "123" | medida == "incidencia"], d, "datos.csv")
  suppressMessages(dl_insumos(dl_proyecto(d, causa = 9100)))
}

test_that("EMR cero con datos de incidencia en el ajuste: el término de datos entra y el de EMR sigue en 0", {
  b <- insumos_emr_cero_incidencia()
  ctx <- .dl_ctx(b, 1L)
  expect_gt(nrow(ctx$datos), 0L); expect_true(all(ctx$datos$tipo_dato == "incidencia"))
  lp <- .dl_lp_componentes(.dl_theta_inicial(ctx), ctx)
  expect_true(is.finite(lp[["datos_incidencia"]]) && lp[["datos_incidencia"]] != 0)
  expect_identical(unname(lp[c("emr", "datos_csmr")]), c(0, 0))
})

# El gemelo en C++ (motor rcpp)

test_that("EMR cero: la log-posterior en C++ coincide con la de R (1e-9) con theta solo de log i", {
  skip_if_not_installed("Rcpp")
  for (b in list(emr_cero_mini()$insumos, insumos_emr_cero_incidencia())) {
    ctx <- .dl_ctx(b, 2L)
    expect_true(.dl_ctx_cpp(ctx)$emr_cero)
    lpr <- .dl_lp_rcpp(ctx)
    th0 <- .dl_theta_inicial(ctx)
    set.seed(5)
    for (rep in 1:10) {
      th <- th0 + rnorm(length(th0), 0, 0.3)
      r <- .dl_lp_componentes(th, ctx)
      cc <- lpr$componentes(th)
      expect_identical(names(cc), names(r))
      expect_equal(unname(cc), unname(r), tolerance = 1e-9)
      expect_equal(lpr$total(th), unname(r[["total"]]), tolerance = 1e-9)
      expect_identical(unname(cc[["emr"]]), 0)
    }
  }
  # con los otros priores la bandera va apagada
  expect_false(.dl_ctx_cpp(.dl_ctx(dl_insumos(cfg9100(), rutas_nacional()), 1L))$emr_cero)
})

test_that("EMR cero: dl_ajustar() con el motor rcpp da las mismas cadenas que con el motor mh (1e-9)", {
  skip_if_not_installed("Rcpp")
  mh <- emr_cero_mini("mh")$ajuste; cpp <- emr_cero_mini("rcpp")$ajuste
  expect_identical(cpp$params$engine, "rcpp")
  expect_identical(colnames(cpp$draws_par[["1"]]), colnames(mh$draws_par[["1"]]))
  for (sexo in c("1", "2")) expect_equal(cpp$draws_par[[sexo]], mh$draws_par[[sexo]], tolerance = 1e-9)
  expect_equal(cpp$draws_q$p, mh$draws_q$p, tolerance = 1e-9)
  expect_true(all(cpp$draws_q$f == 0))
  expect_identical(cpp$mcmc$parametro, mh$mcmc$parametro)
})

# ---- Tarea 1.3: cascada, validación, AVD, etiquetas y sensibilidad ----

test_that("EMR cero: la cascada por covariables mueve la incidencia y deja f = 0 en cada ubicación, en los dos motores", {
  x <- emr_cero_mini("mh"); b <- x$insumos
  expect_gt(nrow(b$cov_proxy), 0L)
  casc <- dl_cascada(x$ajuste, b, semilla = 3L)
  expect_s3_class(casc, "dl_cascade"); expect_identical(casc$modo, "proxy")
  expect_identical(casc$truncados_emr, 0L)
  expect_true(all(casc$draws_q$f == 0))
  expect_length(casc$departamentos, 25L)
  # hay gradiente subnacional (lo mueve la covariable de la incidencia) y la renormalización cierra con el nacional
  p60 <- dl_estimaciones(casc)[location_id != b$loc_ancla & sex_id == 1L & edad == 60L]$media
  expect_gt(max(p60) / min(p60), 1.05)
  expect_true(all(is.finite(casc$renorm$factor) & casc$renorm$factor > 0))
  # la beta del ejemplo que actúa sobre la mortalidad en exceso (haqi) no mueve nada: f = 0 en toda ubicación
  expect_true("f" %in% .dl_proxies(b)$canal)
  skip_if_not_installed("Rcpp")
  cpp <- dl_cascada(emr_cero_mini("rcpp")$ajuste, b, semilla = 3L)
  expect_equal(cpp$draws_q$p, casc$draws_q$p, tolerance = 1e-9)
  expect_true(all(cpp$draws_q$f == 0))
})

test_that("EMR cero: cascada plana, validación, AVD, etiquetas y sensibilidad corren sobre el ajuste sin log f", {
  x <- emr_cero_mini("mh"); b <- x$insumos; f <- x$ajuste
  casc <- dl_cascada(f, b, semilla = 3L)
  # validación: la nota de la incidencia dice cómo se lee con la EMR en 0, y la amplitud (mortalidad subnacional
  # reservada, que el ejemplo trae) se omite con su motivo: el modelo no predice muertes
  expect_message(v <- dl_validar_ancla(f, b, cascada = casc),
                 "se omite la validación con la mortalidad subnacional reservada: la mortalidad en exceso está fija en 0")
  expect_setequal(attr(v, "resumen")$check, c("anchor_identity", "implied_incidence"))
  expect_match(unique(v[check == "implied_incidence"]$nota), "con la EMR fija en 0 mide la consistencia de i con la remisión")
  expect_null(attr(v, "amplitud"))
  expect_identical(attr(v, "sin_amplitud"),
                   "la mortalidad en exceso está fija en 0 y el modelo no predice muertes por la causa")
  y <- suppressWarnings(dl_avd(casc, b, dl_factor_comorbilidad(b), semilla = 3L))
  expect_true(all(is.finite(y$draws_yld$yld) & y$draws_yld$yld >= 0))
  res <- dl_resumir(list(fit = casc, yld = y, bundle = b))
  expect_s3_class(res, "dl_resumen")
  f0 <- dl_ajustar_solo_prior(b, f$params, semilla = 3L, ajuste = f)
  et <- dl_etiquetas(f, f0, b, grilla_rho = b$cfg$anchor$rho_edad, semilla = 3L, cascada = casc)
  expect_true(all(et$etiqueta == "prior-driven"))
  s <- suppressWarnings(dl_sensibilidad(b, grilla = list(lambda = c(0.5, 1), rho = 0.5, kappa = 1), semilla = 3L,
                                        opciones = f$params))
  expect_identical(sort(unique(s$lambda)), c(0.5, 1)); expect_true(all(s$fraccion_aguda == 0))
  # cascada plana: las tasas nacionales en cada ubicación, también con f = 0
  d <- copia_emr_cero()
  cfg <- readLines(file.path(d, "config", "9100.yaml"), encoding = "UTF-8")
  i <- which(cfg == "subnacional:")
  writeLines(enc2utf8(append(cfg, "  modo: plano", i)), file.path(d, "config", "9100.yaml"), useBytes = TRUE)
  bp <- suppressMessages(dl_insumos(dl_proyecto(d, causa = 9100)))
  fp <- dl_ajustar(bp, f$params, semilla = 3L, cache = FALSE)
  expect_identical(fp$draws_par, f$draws_par)             # el ajuste nacional no depende del modo subnacional
  plana <- dl_cascada(fp, bp, semilla = 3L)
  expect_identical(plana$modo, "plana"); expect_true(all(plana$draws_q$f == 0))
})
