# Motor "rcpp" (inst/cpp/dl_core.cpp y R/rcpp.R): mensajes sin Rcpp o sin compilador, paridad numérica del solver
# y de la log-posterior por componentes con la versión en R, equivalencia estadística del posterior y velocidad.

opciones_rcpp_cortas <- function() dl_opciones_mcmc(motor = "rcpp", simulaciones = 10L, cadenas = 1L,
                                                    iteraciones = 400L, calentamiento = 200L)

# Deja vacía la caché de funciones compiladas durante la prueba (y la restaura al salir), para que .dl_rcpp() pase
# por la comprobación de Rcpp y por la compilación.
sin_cache_rcpp <- function(entorno = parent.frame()) {
  guardadas <- .dl_rcpp_env$fns
  .dl_rcpp_env$fns <- NULL
  withr::defer(.dl_rcpp_env$fns <- guardadas, envir = entorno)
}

test_that("motor rcpp sin Rcpp da un mensaje claro en español", {
  local_mocked_bindings(.dl_rcpp_disponible = function() FALSE)
  b <- dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE))
  expect_error(dl_ajustar(b, dl_opciones_mcmc(motor = "rcpp", simulaciones = 10L, cadenas = 1L,
    iteraciones = 400L, calentamiento = 200L), semilla = 1L), "Rcpp.*motor = \"mh\"")
  # el mensaje nombra la función que llamó el usuario
  expect_error(dl_ajustar(b, opciones_rcpp_cortas(), semilla = 1L, cache = FALSE),
               "^dl_ajustar\\(\\): el motor \"rcpp\" requiere")
  expect_error(dl_sensibilidad(b, semilla = 1L, opciones = opciones_rcpp_cortas()),
               "^dl_sensibilidad\\(\\): el motor \"rcpp\" requiere")
  # la cascada y las etiquetas sobre un ajuste hecho con motor = "mh", pidiendo el motor rcpp
  bc <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L)))
  opciones_mh <- dl_opciones_mcmc(simulaciones = 10L, cadenas = 1L, iteraciones = 400L, calentamiento = 200L)
  f <- dl_ajustar(bc, opciones_mh, semilla = 1L)
  f0 <- dl_ajustar_solo_prior(bc, opciones_mh, semilla = 1L, ajuste = f)
  expect_error(dl_cascada(f, bc, semilla = 1L, motor = "rcpp"), "^dl_cascada\\(\\): el motor \"rcpp\" requiere")
  expect_error(dl_etiquetas(f, f0, bc, grilla_rho = 0.9, semilla = 1L, opciones = opciones_rcpp_cortas()),
               "^dl_etiquetas\\(\\): el motor \"rcpp\" requiere")
})

test_that("si el C++ no compila, el mensaje dice cómo seguir (compilador o motor = \"mh\")", {
  local_mocked_bindings(.dl_rcpp_disponible = function() TRUE,
                        .dl_rcpp_compilar = function(...) stop("Error 1 occurred building shared library."))
  sin_cache_rcpp()
  b <- dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE))
  expect_error(dl_ajustar(b, opciones_rcpp_cortas(), semilla = 1L, cache = FALSE),
               "^dl_ajustar\\(\\): no se pudo compilar el motor \"rcpp\".*Rtools.*motor = \"mh\".*Error 1 occurred")
  expect_null(.dl_rcpp_env$fns)
})

test_that("EDO: el solver en C++ reproduce dl_edo_resolver sin y con remisión (1e-14)", {
  skip_if_not_installed("Rcpp")
  edo_cpp <- .dl_rcpp()$edo
  set.seed(1)
  for (rep in 1:5) {
    n <- 2L * 370L + 1L
    i_media <- exp(rnorm(n, -6, 0.5)); f_media <- exp(rnorm(n, -4, 0.3))
    r_media <- runif(n, 0, 2); p0 <- runif(1, 0, 0.05)
    expect_equal(edo_cpp(i_media, f_media, 0, 0, 5L), dl_edo_resolver(i_media, f_media, 0, 0, 5L), tolerance = 1e-14)
    expect_equal(edo_cpp(i_media, f_media, 0.7, p0, 5L), dl_edo_resolver(i_media, f_media, 0.7, p0, 5L),
                 tolerance = 1e-14)
    expect_equal(edo_cpp(i_media, f_media, r_media, p0, 5L), dl_edo_resolver(i_media, f_media, r_media, p0, 5L),
                 tolerance = 1e-14)
  }
})

# Con las opciones por defecto el compilador puede fundir una multiplicación y una suma en una sola operación (FMA),
# que redondea una vez en lugar de dos: el solver en C++ difiere entonces del de R en el último bit. Compilado sin
# esa fusión (-ffp-contract=off), hace exactamente las mismas operaciones que R en el mismo orden.
test_that("compilado sin FMA, el solver en C++ es idéntico bit a bit al de R, también con remisión", {
  skip_if_not_installed("Rcpp")
  skip_on_cran()
  withr::local_envvar(PKG_CXXFLAGS = "-ffp-contract=off")
  entorno <- new.env()
  # En Windows, Rcpp avisa que no existe la carpeta inst/include del paquete instalado: no hace falta para compilar.
  withCallingHandlers(
    Rcpp::sourceCpp(.dl_inst("cpp", "dl_core.cpp"), env = entorno, cacheDir = withr::local_tempdir(),
                    rebuild = TRUE, verbose = FALSE, showOutput = FALSE),
    warning = function(w) if (grepl("inst/include", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning"))
  set.seed(2)
  for (rep in 1:5) {
    n <- 2L * 5L * 69L + 1L
    i_media <- exp(rnorm(n, -5, 0.5)); f_media <- exp(rnorm(n, -3, 0.5)); r_media <- runif(n, 0, 3)
    expect_identical(entorno$dl_edo_resolver_cpp(i_media, f_media, r_media, 0.01, 5L),
                     dl_edo_resolver(i_media, f_media, r_media, 0.01, 5L))
    expect_identical(entorno$dl_edo_resolver_cpp(i_media, f_media, 12, 0, 5L),
                     dl_edo_resolver(i_media, f_media, 12, 0, 5L))
  }
})

test_that("log-posterior por componentes: C++ == R (1e-9), sin y con datos, y fuera de cota", {
  skip_if_not_installed("Rcpp")
  b <- dl_insumos(cfg9100(), rutas_nacional())
  chequear <- function(bb) {
    ctx <- .dl_ctx(bb, 1L)
    lpr <- .dl_lp_rcpp(ctx)
    th0 <- .dl_theta_inicial(ctx)
    set.seed(2)
    for (rep in 1:20) {
      th <- th0 + rnorm(length(th0), 0, 0.3)
      r <- .dl_lp_componentes(th, ctx)
      cc <- lpr$componentes(th)
      expect_identical(names(cc), names(r))
      expect_equal(unname(cc), unname(r), tolerance = 1e-9)
      expect_equal(lpr$total(th), unname(r[["total"]]), tolerance = 1e-9)
    }
    th <- th0; th[length(th0)] <- log(1)          # f = 1 > cota 0.25
    expect_true(all(is.infinite(lpr$componentes(th))))
  }
  chequear(b)
  b2 <- b
  b2$datos <- rbind(fila_datos(age_start = 60, age_end = 65, sex_id = 1L, val = 0.05, se = 0.01),
                    fila_datos(dato_id = "t_2", age_start = 45, age_end = 55, sex_id = 1L, val = 0.01, se = 0.003))
  chequear(b2)
  # los tres tipos de dato por las dos rutas (log-normal con offset y conteos)
  b3 <- b; b3$cfg$medidas_entrada <- list("prev_estudio", "incidencia", "csmr")
  b3$datos <- rbind(
    fila_datos(dato_id = "p1", sex_id = 1L, age_start = 55, age_end = 60, val = 0.02, se = 0.004),
    fila_datos(dato_id = "p2", sex_id = 1L, age_start = 45, age_end = 50, val = NA_real_, se = NA_real_, x = 20L, n = 900L),
    fila_datos(dato_id = "i1", tipo_dato = "incidencia", measure_id = 6L, measure_name = "Incidence",
               sex_id = 1L, age_start = 60, age_end = 65, val = 0.003, se = 0.0006),
    fila_datos(dato_id = "i2", tipo_dato = "incidencia", measure_id = 6L, measure_name = "Incidence",
               sex_id = 1L, age_start = 65, age_end = 70, val = NA_real_, se = NA_real_, x = 9L, n = 3000L),
    fila_datos(dato_id = "c1", tipo_dato = "csmr", measure_id = 900006L, measure_name = "csmr",
               sex_id = 1L, age_start = 70, age_end = 75, val = 0.0004, se = 0.0001, acquisition_id = "fixture_csmr_datos_v1"),
    fila_datos(dato_id = "c2", tipo_dato = "csmr", measure_id = 900006L, measure_name = "csmr",
               sex_id = 1L, age_start = 75, age_end = 80, val = NA_real_, se = NA_real_, x = 5L, n = 20000L,
               acquisition_id = "fixture_csmr_datos_v1"))
  chequear(b3)
  b4 <- b3; b4$cfg$offset_lognormal <- 1e-5
  chequear(b4)
  # remisión por tramo de edad
  b5 <- b; b5$cfg$remision$por_edad <- list(list(edad_inicio = 30, edad_fin = 45, valor = 2, fuente = "prueba"))
  chequear(b5)
})

test_that("dl_ajustar engine rcpp reproduce el posterior de engine mh", {
  skip_if_not_installed("Rcpp")
  b <- dl_insumos(cfg9100(), rutas_nacional())
  f_mh  <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 100L, cadenas = 2L, iteraciones = 4000L,
                                                     calentamiento = 2000L, motor = "mh"),
                      semilla = 31L)
  f_cpp <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 100L, cadenas = 2L, iteraciones = 4000L,
                                                     calentamiento = 2000L, motor = "rcpp"),
                      semilla = 31L)
  expect_identical(f_cpp$params$engine, "rcpp")
  qm <- .dl_q_bandas(f_mh, b)[, list(q = stats::median(val)), by = list(sex_id, age_group_id)]
  qc <- .dl_q_bandas(f_cpp, b)[, list(q = stats::median(val)), by = list(sex_id, age_group_id)]
  m <- merge(qm, qc, by = c("sex_id", "age_group_id"))
  expect_lt(stats::median(abs(m$q.x - m$q.y) / m$q.x), 0.05)   # misma tolerancia que la paridad MCMC
})

# La velocidad depende de la máquina: en los servidores compartidos del CI no es una medida confiable.
test_that("el motor rcpp acelera la log-posterior >= 5x", {
  skip_if_not_installed("Rcpp")
  skip_on_cran()
  skip_on_ci()
  b <- dl_insumos(cfg9100(), rutas_nacional())
  ctx <- .dl_ctx(b, 1L); th <- .dl_theta_inicial(ctx)
  lp_r <- function(theta) .dl_log_post(theta, ctx); lp_c <- .dl_lp_rcpp(ctx)$total
  t_r <- system.time(for (k in 1:2000) lp_r(th))[["elapsed"]]
  t_c <- system.time(for (k in 1:2000) lp_c(th))[["elapsed"]]
  expect_gt(t_r / t_c, 5)   # el umbral del título (>= 5x), con margen para máquinas compartidas
})
