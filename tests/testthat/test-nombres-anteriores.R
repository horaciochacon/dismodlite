# Nombres anteriores (versión 0.2.2): cada uno sigue exportado, conserva sus argumentos y da el mismo resultado que
# su función nueva; los objetos se mezclan entre nombres y la cache de ajustes es una sola.

# Nombre anterior -> función nueva: el mapa del paquete, con el que los mensajes de un nombre anterior remiten a la
# función nueva (R/nombres_anteriores.R). Las pruebas de abajo lo comprueban contra la lista de nombres de la versión
# 0.2.2 y contra el cuerpo de cada nombre anterior; test-fuente-nombres-anteriores.R, contra ?dl_nombres_anteriores.
.nombres_anteriores <- .DL_NOMBRES_ANTERIORES

# Argumentos de cada nombre anterior en la versión 0.2.2, en orden.
.argumentos_0_2_2 <- list(
  dl_config = c("cause_id", "config_dir", "overrides"), dl_paths = "overrides", dl_bundle = c("cfg", "paths"),
  dl_bundle_freeze = c("b", "dir"),
  dl_mcmc_opts = c("draws", "chains", "iter", "warmup", "thin", "cores", "engine"),
  dl_fit = c("b", "opts", "seed", "cache"), dl_fit_prior_only = c("b", "opts", "seed", "fit", "cache"),
  dl_cascade = c("f", "b", "kappa", "seed", "engine"), dl_validate_gbd = c("f", "b", "paths", "cascade"),
  dl_validate = c("dt", "tabla", "sch", "ctx"), dl_validate_estimates = c("dt", "paths", "entity"),
  dl_schema = "path", dl_schema_tabla = c("sch", "nombre"), dl_yld = c("f", "b", "como", "seed"),
  dl_como_factor = c("b", "paths"), dl_emr_prior = "b",
  dl_severidad_desde_split = c("dir_run", "cause_id", "paths", "beta_covariable", "padre", "sequela_ids"),
  dl_labels = c("f", "f0", "b", "rho_grid", "seed", "opts", "cascade"),
  dl_sensitivity = c("b", "grid", "seed", "opts", "workers"), dl_summarize = c("piezas", "ui_level", "paths"),
  dl_export = c("piezas", "run_slug", "out_root", "labels", "validacion", "sensibilidad", "keep_draws", "register",
                "force", "ess_min", "registry_path", "paths"),
  dl_resumir_run = c("dir_run", "out_root", "ui_level", "register", "registry_path", "paths"),
  dl_sum_hijas = c("runs_hijas", "cause_id", "run_slug", "out_root", "cause_name", "ui_level", "register",
                   "registry_path", "paths", "omitidas"),
  dl_export_cdc = c("registry_path", "out_root", "perfil_path", "master_path", "paths", "causas", "years",
                    "permitir_huecos", "register", "run_slug", "registry_out", "runs_root", "nombres_nivel4_path"),
  dl_export_seleccionar = c("registry_path", "runs_root", "causas", "years", "permitir_huecos", "paths"),
  dl_export_canonico = c("sel", "run_id_export"), dl_export_conteos = c("celdas", "sel"),
  dl_ode = c("theta_logi", "theta_logf", "nudos", "edad_inicio", "edad_fin", "r", "p0", "nsub"),
  dl_ode_solve = c("i_half", "f_half", "r", "p0", "nsub"), dl_cache_clear = character())

# Ajustes mínimos del ejemplo, calculados una vez para todo el archivo.
.opciones_minimas <- function() dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L,
                                                 calentamiento = 200L)
.mini <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      r <- dl_rutas_ejemplo(9100L, formato = "completo")
      b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"), r))
      f <- dl_ajustar(b, .opciones_minimas(), semilla = 3L)
      x <<- list(rutas = r, insumos = b, ajuste = f, ajuste_prior = dl_ajustar_solo_prior(b, .opciones_minimas(),
                                                                                          semilla = 3L, ajuste = f),
                 cascada = suppressWarnings(dl_cascada(f, b, semilla = 3L)))
    }
    x
  }
})

test_that("todos los nombres anteriores siguen exportados", {
  antiguos <- c("dl_config", "dl_paths", "dl_bundle", "dl_bundle_freeze", "dl_mcmc_opts", "dl_fit",
    "dl_fit_prior_only", "dl_cascade", "dl_validate_gbd", "dl_validate", "dl_validate_estimates", "dl_schema",
    "dl_schema_tabla", "dl_yld", "dl_como_factor", "dl_emr_prior", "dl_severidad_desde_split", "dl_labels",
    "dl_sensitivity", "dl_summarize", "dl_export", "dl_resumir_run", "dl_sum_hijas", "dl_export_cdc",
    "dl_export_seleccionar", "dl_export_canonico", "dl_export_conteos", "dl_ode", "dl_ode_solve", "dl_cache_clear")
  expect_setequal(antiguos, names(.nombres_anteriores))
  expect_true(all(antiguos %in% getNamespaceExports("dismodlite")))
  expect_true(all(.nombres_anteriores %in% getNamespaceExports("dismodlite")))
})

test_that("cada nombre anterior conserva sus argumentos de la versi\u00f3n 0.2.2 y llama a su funci\u00f3n nueva", {
  for (a in names(.nombres_anteriores)) {
    expect_identical(names(formals(get(a))) %||% character(), .argumentos_0_2_2[[a]], info = a)
    expect_true(.nombres_anteriores[[a]] %in% all.names(body(get(a))), info = a)
  }
  # los valores por defecto, salvo el registro y las rutas que vienen en los insumos, son los de la versión 0.2.2
  expect_identical(dl_mcmc_opts(), dl_opciones_mcmc())
  expect_identical(formals(dl_sensitivity)$opts,
                   quote(dl_mcmc_opts(draws = 200L, chains = 2L, iter = 6000L, warmup = 3000L)))
  expect_identical(formals(dl_ode)$edad_fin, 99)
  expect_identical(formals(dl_export)$ess_min, 400)
  expect_identical(formals(dl_export_cdc)$run_slug, "consolidado")
})

test_that("sin `paths`, los nombres anteriores usan las rutas de los insumos, como sus funciones nuevas", {
  expect_identical(formals(dl_validate_gbd)$paths, quote(b$rutas))
  expect_identical(formals(dl_como_factor)$paths, quote(b$rutas))
  expect_null(formals(dl_summarize)$paths)
  expect_null(formals(dl_export)$paths)
  # sin variables de entorno (setup-entorno.R), dl_paths() no tiene catálogos ni anclas: los valores por defecto
  # toman las rutas de los insumos
  m <- .mini()
  expect_identical(dl_como_factor(m$insumos), dl_factor_comorbilidad(m$insumos))
  expect_identical(suppressMessages(dl_validate_gbd(m$ajuste, m$insumos)),
                   suppressMessages(dl_validar_ancla(m$ajuste, m$insumos)))
  y <- dl_avd(m$ajuste, m$insumos, semilla = 3L)
  piezas <- list(fit = m$ajuste, yld = y, bundle = m$insumos)
  expect_identical(dl_summarize(piezas), dl_resumir(piezas))
})

test_that("missing(semilla) se propaga a trav\u00e9s del alias", {
  b <- dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"), dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo"))
  expect_error(dl_fit(b), "semilla")
  expect_error(dl_cascade(.mini()$ajuste, .mini()$insumos), "semilla")
  expect_error(dl_yld(.mini()$ajuste, .mini()$insumos), "semilla")
})

test_that("dl_fit y dl_ajustar dan el mismo ajuste y los objetos se mezclan entre nombres", {
  b <- dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"), dl_rutas_ejemplo(9100L, datos = FALSE, proxies = TRUE, formato = "completo"))
  o <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 2L, iteraciones = 400L, calentamiento = 200L)
  f_nuevo <- dl_ajustar(b, o, semilla = 1L, cache = FALSE)
  f_viejo <- dl_fit(b, opts = dl_mcmc_opts(draws = 10L, chains = 2L, iter = 400L, warmup = 200L), seed = 1L,
                    cache = FALSE)
  expect_identical(f_viejo, f_nuevo)
  casc <- suppressWarnings(dl_cascada(f_viejo, b, semilla = 1L))
  expect_s3_class(casc, "dl_cascade")
  expect_true(all(c("media", "inferior", "superior") %in% names(dl_estimaciones(casc))))
  expect_identical(suppressWarnings(dl_cascade(f_nuevo, b, seed = 1L)), casc)
})

test_that("la cache de ajustes es una sola para los nombres nuevos y los anteriores", {
  dl_limpiar_cache()
  b <- dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"), dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo"))
  f1 <- dl_ajustar(b, .opciones_minimas(), semilla = 2L)
  n <- length(ls(.dl_cache_env))
  expect_gte(n, 1L)
  f2 <- dl_fit(b, opts = dl_mcmc_opts(draws = 10L, chains = 2L, iter = 400L, warmup = 200L), seed = 2L)
  expect_identical(length(ls(.dl_cache_env)), n)   # el alias encontró el ajuste en la cache
  expect_identical(f2, f1)
  dl_cache_clear()
  expect_length(ls(.dl_cache_env), 0L)
})

test_that("configuraci\u00f3n, rutas, esquema y validaci\u00f3n: mismo resultado con los dos nombres", {
  d <- ejemplo_completo("config")
  cambios <- list(anchor = list(lambda = 0.5))
  expect_identical(dl_config(9100L, d, cambios), dl_configuracion(9100L, d, cambios))
  x <- ejemplo_completo("ancla", "prevalencia.csv")
  expect_identical(dl_paths(list(std_prior = x)), dl_rutas(cambios = list(std_prior = x)))
  expect_identical(dl_paths(), dl_rutas())
  expect_identical(dl_schema(), dl_esquema())
  expect_identical(dl_schema_tabla(dl_schema(), "datos"), dl_esquema_tabla(dl_esquema(), "datos"))
  expect_identical(dl_validate(mini_como(), "como_factor"), dl_validar_tabla(mini_como(), "como_factor"))
  expect_error(dl_validate(mini_como(factor = "alto"), "como_factor"), "como_factor[.]factor: tipo")
  m <- .mini()
  expect_identical(suppressMessages(dl_bundle(dl_configuracion_ejemplo(9100L, formato = "completo"), m$rutas)), m$insumos)
  expect_identical(dl_emr_prior(m$insumos), dl_prior_emr(m$insumos))
  expect_identical(dl_como_factor(m$insumos, m$rutas), dl_factor_comorbilidad(m$insumos, m$rutas))
  expect_identical(suppressMessages(dl_validate_gbd(m$ajuste, m$insumos, m$rutas, cascade = m$cascada)),
                   suppressMessages(dl_validar_ancla(m$ajuste, m$insumos, m$rutas, cascada = m$cascada)))
  # la partición de severidad es del formato completo (sus catálogos de secuelas y estados de salud)
  p <- ejemplo_completo("particion", "acs_v1")
  rc <- dl_rutas_ejemplo(9100L, formato = "completo")
  expect_identical(dl_severidad_desde_split(p, 9101L, paths = rc, padre = 9100L),
                   dl_severidad_desde_particion(p, 9101L, rutas = rc, padre = 9100L))
  expect_identical(dl_severidad_desde_split(p, 9101L, paths = rc, sequela_ids = c(91012L, 91013L)),
                   dl_severidad_desde_particion(p, 9101L, rutas = rc, secuelas = c(91012L, 91013L)))
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  expect_identical(dl_bundle_freeze(m$insumos, d1), dl_congelar_insumos(m$insumos, d2))
})

test_that("ajuste, carga, diagn\u00f3stico y ecuaci\u00f3n: mismo resultado con los dos nombres", {
  m <- .mini()
  o_viejo <- dl_mcmc_opts(draws = 10L, chains = 2L, iter = 400L, warmup = 200L)
  expect_identical(o_viejo, .opciones_minimas())
  expect_identical(dl_fit_prior_only(m$insumos, o_viejo, seed = 3L, fit = m$ajuste), m$ajuste_prior)
  como <- dl_factor_comorbilidad(m$insumos, m$rutas)
  expect_identical(dl_yld(m$cascada, m$insumos, como, seed = 3L), dl_avd(m$cascada, m$insumos, como, semilla = 3L))
  expect_identical(dl_labels(m$ajuste, m$ajuste_prior, m$insumos, rho_grid = 0.5, seed = 3L, cascade = m$cascada),
                   dl_etiquetas(m$ajuste, m$ajuste_prior, m$insumos, grilla_rho = 0.5, semilla = 3L,
                                cascada = m$cascada))
  grilla <- list(lambda = 1, rho = 0.5, kappa = 1)
  expect_identical(suppressWarnings(dl_sensitivity(m$insumos, grid = grilla, seed = 3L, opts = o_viejo)),
                   suppressWarnings(dl_sensibilidad(m$insumos, grilla = grilla, semilla = 3L,
                                                    opciones = .opciones_minimas())))
  y <- dl_avd(m$cascada, m$insumos, como, semilla = 3L)
  piezas <- list(fit = m$cascada, yld = y, bundle = m$insumos)
  expect_identical(dl_summarize(piezas, 0.9, m$rutas), dl_resumir(piezas, 0.9, m$rutas))
  nudos <- c(30, 50, 70, 95)
  expect_identical(dl_ode(rep(-6, 4), rep(-3, 4), nudos, 30, 99, r = 0.01),
                   dl_edo(rep(-6, 4), rep(-3, 4), nudos, 30, 99, remision = 0.01))
  ih <- rep(0.01, 2 * 5 * 20 + 1); fh <- rep(0.05, length(ih))
  expect_identical(dl_ode_solve(ih, fh, r = 0.02), dl_edo_resolver(ih, fh, remision = 0.02))
})

test_that("corrida, re-resumen, suma de hijas y consolidado: mismo resultado con los dos nombres", {
  base1 <- withr::local_tempdir(); base2 <- withr::local_tempdir()
  reg <- file.path(base1, "registro.yaml"); writeLines("datasets: []", reg)
  o <- .opciones_minimas()
  corrida_hija <- function(k) {
    r <- dl_rutas_ejemplo(k, datos = FALSE, proxies = FALSE, formato = "completo")
    b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(k, formato = "completo"), r))
    f <- dl_ajustar(b, o, semilla = 5L)
    f0 <- dl_ajustar_solo_prior(b, o, semilla = 5L, ajuste = f)
    y <- dl_avd(f, b, dl_factor_comorbilidad(b, r), semilla = 5L)
    piezas <- list(resumen = dl_resumir(list(fit = f, yld = y, bundle = b), rutas = r), fit = f, yld = y, bundle = b)
    et <- dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 5L)
    nueva <- dl_exportar_corrida(piezas, sprintf("hija-%d", k), carpeta = base1, etiquetas = et, forzar = TRUE,
                                 registro = reg, rutas = r)
    vieja <- dl_export(piezas, sprintf("hija-%d", k), out_root = base2, labels = et, force = TRUE, paths = r)
    expect_identical(vieja$manifest, nueva$manifest, info = k)
    expect_identical(vieja$run_id, nueva$run_id, info = k)
    nueva
  }
  hijas <- lapply(9101:9103, corrida_hija)
  r <- dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo")
  # re-resumen
  rr_nuevo <- dl_reresumir_corrida(hijas[[1]]$dir, carpeta = base1, nivel = 0.95, rutas = r)
  rr_viejo <- dl_resumir_run(hijas[[1]]$dir, out_root = base2, ui_level = 0.95, paths = r)
  expect_identical(rr_viejo$manifest, rr_nuevo$manifest)
  # suma de hijas (la nueva se registra: el consolidado la elige como 9100)
  dirs <- vapply(hijas, function(h) h$dir, "")
  s_nueva <- dl_sumar_hijas(dirs, 9100L, "suma", carpeta = base1, registro = reg, rutas = r)
  s_vieja <- dl_sum_hijas(dirs, 9100L, "suma", out_root = base2, paths = r)
  expect_identical(s_vieja$manifest, s_nueva$manifest)
  # consolidado y sus piezas
  perfil <- system.file("perfiles", "perfil_v1.yaml", package = "dismodlite")
  maestro <- ejemplo_completo("registro", "master_gbd.csv")
  sel_n <- dl_consolidado_seleccionar(reg, base1, anios = 2023L, rutas = r)
  sel_v <- dl_export_seleccionar(reg, base1, years = 2023L, paths = r)
  expect_identical(sel_v, sel_n)
  canon <- dl_consolidado_canonico(sel_n, "2026-01-01_prueba_v1")
  expect_identical(dl_export_canonico(sel_v, "2026-01-01_prueba_v1"), canon)
  expect_identical(dl_export_conteos(canon, sel_v), dl_consolidado_conteos(canon, sel_n))
  c_nuevo <- dl_consolidar(reg, base1, perfil, maestro, rutas = r, anios = 2023L, nombre = "consolidado-prueba")
  c_viejo <- dl_export_cdc(reg, base2, perfil, maestro, paths = r, years = 2023L, run_slug = "consolidado-prueba",
                           runs_root = base1)
  expect_identical(c_viejo$manifest, c_nuevo$manifest)
  expect_identical(length(yaml::read_yaml(reg)$datasets), 4L)   # tres hijas y la suma; el consolidado no se registra
})

test_that("dl_paths(): clave repetida vale la primera vez (0.2.2); una desconocida lista las dos familias", {
  expect_identical(dl_paths(list(std_prior = "a", std_prior = "b"))$std_prior, "a")
  expect_identical(dl_rutas(cambios = list(std_prior = "a", ancla_prevalencia = "b"))$std_prior, "b")
  expect_error(dl_paths(list(foo = "x")), "desconocida.*ancla_prevalencia.*std_prior")
  expect_error(dl_paths(list("x")), "nombres")
})
