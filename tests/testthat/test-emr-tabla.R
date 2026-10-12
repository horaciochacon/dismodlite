# Prior de la mortalidad en exceso (EMR) dado por tabla: `mortalidad_exceso: {prior: tabla}` (emr_prior.tipo tabla en
# el formato completo) y la tabla prior_mortalidad_exceso del contrato (la duodécima): por sexo y tramo de edad, el
# centro (mu_log) y la desviación estándar (sd_log) de log f. Cada nudo toma el tramo que lo contiene y log f queda
# truncado a [techo / 1000, techo], el soporte del prior plano. El proyecto de prueba es el país ficticio
# (escribir_pais_ficticio(), helper-dismodlite.R) con la causa 7001 en 2023 y los nudos por defecto de edad_inicio 40.

NUDOS_TABLA <- c(40, 50, 60, 70, 80, 95)

# La tabla del prior de prueba: un tramo por nudo ([nudo, nudo siguiente); el último, abierto) y sexo, con
# mu_log = log(0,02 ... 0,12) (5 % más en los hombres) y sd_log 0,3.
prior_tabla_prueba <- function(sd_log = 0.3, mu = log(c(2, 2.5, 3.5, 5, 8, 12) * 1e-2)) {
  t <- data.table::CJ(sexo = c("hombres", "mujeres"), k = seq_along(NUDOS_TABLA), sorted = FALSE)
  t[, `:=`(anio = 2023L, edad_inicio = NUDOS_TABLA[k], edad_fin = c(NUDOS_TABLA[-1L], NA)[k],
           mu_log = mu[k] + ifelse(sexo == "hombres", log(1.05), 0), sd_log = sd_log, fuente = "prior inventado")]
  t[, list(anio, sexo, edad_inicio, edad_fin, mu_log, sd_log, fuente)]
}

# El país ficticio con `prior: tabla` (techo 0,5) y la tabla `tabla` en prior_mortalidad_exceso.csv (NULL: sin ella),
# más las líneas `config`. Con `prior` se cambia el prior de la configuración.
proyecto_emr_tabla <- function(tabla = prior_tabla_prueba(), config = character(), prior = "tabla", techo = "0.5",
                               env = parent.frame()) {
  d <- file.path(withr::local_tempdir(.local_envir = env), "tabla")
  escribir_pais_ficticio(d, 7001L, 2023L, c("causa: 7001", "anio: 2023", "edad_inicio: 40", "mortalidad_exceso:",
                                           sprintf("  prior: %s", prior),
                                           if (!is.null(techo)) sprintf("  techo: %s", techo), config))
  if (!is.null(tabla)) {
    # mu_log en hexadecimal: el CSV devuelve exactamente el mismo número (fwrite escribiría 15 cifras)
    tabla <- data.table::copy(tabla)[, mu_log := sub("^(-?0x1)p", "\\1.0p", sprintf("%a", mu_log))]
    escribir_texto(tabla, d, "prior_mortalidad_exceso.csv")
  }
  d
}

# ---- El contrato ----

test_that("prior_mortalidad_exceso: la duodécima tabla del contrato, con su plantilla y sus reglas de una tabla", {
  expect_identical(utils::tail(dismodlite:::.DL_TABLAS, 1L), "prior_mortalidad_exceso")
  expect_length(dismodlite:::.DL_TABLAS, 12L)
  expect_identical(dismodlite:::.dl_columnas_contrato("prior_mortalidad_exceso", "exigidas"),
                   c("edad_inicio", "edad_fin", "mu_log", "sd_log"))
  expect_identical(names(dl_plantilla("prior_mortalidad_exceso")),
                   c("causa", "anio", "sexo", "edad_inicio", "edad_fin", "mu_log", "sd_log", "fuente"))
  expect_s3_class(dl_tabla("prior_mortalidad_exceso", dl_plantilla("prior_mortalidad_exceso")), "dl_tabla")
  t <- dl_tabla("prior_mortalidad_exceso", prior_tabla_prueba())
  expect_identical(nrow(t), 12L)
  expect_identical(t$edad_fin[t$edad_inicio == 95], c(125, 125))          # la banda abierta
  expect_error(dl_tabla("prior_mortalidad_exceso", prior_tabla_prueba()[, !"sd_log"]), "sd_log")
  expect_error(dl_tabla("prior_mortalidad_exceso", prior_tabla_prueba(sd_log = -0.1)), "sd_log: hay valores menores que 0")
  solape <- prior_tabla_prueba(); solape[edad_inicio == 40, edad_fin := 55]
  expect_error(dl_tabla("prior_mortalidad_exceso", solape), "bandas de edad que se solapan")
})

# ---- Configuración ----

test_that("formato completo: emr_prior.tipo tabla exige procedencia y no admite factor_sd", {
  completa <- function(cambios) dl_configuracion(9100L, file.path(ruta_acs(), "config"), cambios = cambios)
  expect_identical(completa(list(emr_prior = list(tipo = "tabla", tipo_procedencia = "estudio de seguimiento")))$emr_prior$tipo,
                   "tabla")
  expect_error(completa(list(emr_prior = list(tipo = "tabla"))),
               "config[.]emr_prior[.]tipo_procedencia: tabla exige procedencia")
  expect_error(completa(list(emr_prior = list(tipo = "tabla", tipo_procedencia = "x",
                                              factor_sd = list(valor = 2, procedencia = "x")))),
               "config[.]emr_prior[.]tipo: tabla toma la desviación estándar del prior de su tabla")
  # la fracción aguda no cambia este prior: la sensibilidad no la admite; la de la configuración sí (mueve el techo
  # derivado del ancla)
  expect_error(completa(list(emr_prior = list(tipo = "tabla", tipo_procedencia = "x"),
                             sensibilidad = list(fraccion_aguda = c(0, 0.3)))),
               "config[.]emr_prior[.]tipo: tabla .* no admite sensibilidad[.]fraccion_aguda")
  expect_identical(completa(list(emr_prior = list(tipo = "tabla", tipo_procedencia = "x",
                                                  fraccion_aguda = list(valor = 0.3, procedencia = "x"))))$emr_prior$tipo,
                   "tabla")
})

test_that("formato simple: `prior: tabla` se traduce a emr_prior.tipo tabla y exige la tabla del proyecto", {
  expect_identical(names(.dl_vocabulario_simple("mortalidad_exceso.prior")), c("desde_ancla", "plano", "cero", "tabla"))
  cfg <- dl_proyecto(proyecto_emr_tabla())$configuracion
  expect_identical(cfg$emr_prior$tipo, "tabla")
  expect_identical(cfg$emr_prior$tipo_procedencia, .DL_PROCEDENCIA_SIMPLE)
  expect_identical(as.numeric(unlist(cfg$emr_prior$cota)), c(0, 0.5))
  e <- expect_error(dl_proyecto(proyecto_emr_tabla(tabla = NULL)), class = "dl_error")
  expect_match(e$problemas, "^mortalidad_exceso.prior: es tabla y el proyecto no trae la tabla prior_mortalidad_exceso")
  d <- proyecto_emr_tabla(tabla = NULL)
  salida <- utils::capture.output(r <- suppressMessages(dl_revisar_proyecto(d)))
  expect_identical(r$estado[r$paso == "configuración"], "error")
  e <- expect_error(dl_proyecto(proyecto_emr_tabla(config = c("sensibilidad:", "  fraccion_aguda: [0, 0.3]"))),
                    class = "dl_error")
  expect_match(e$problemas, "tabla .* no admite sensibilidad[.]fraccion_aguda")
  # dl_sensibilidad() tampoco admite el eje en su rejilla
  b <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla())))
  expect_error(dl_sensibilidad(b, grilla = list(lambda = 0.5, rho = 0.5, kappa = 0.5, fraccion_aguda = c(0, 0.3)),
                               semilla = 1L),
               "emr_prior.tipo: tabla .* no admite `fraccion_aguda` en la grilla")
})

test_that("reglas del proyecto: cada nudo en un tramo, sd_log > 0, y la tabla sin el prior es solo un aviso", {
  problema <- function(tabla, ...) {
    e <- expect_error(dl_proyecto(proyecto_emr_tabla(tabla, ...)), class = "dl_error")
    e$problemas
  }
  # un nudo sin tramo (falta el de 95 y más): el prior no se extrapola
  expect_match(problema(prior_tabla_prueba()[edad_inicio != 95]),
               "^prior_mortalidad_exceso: el/los nudo\\(s\\) 95 no cae\\(n\\) en ningún tramo de edad de hombres en 2023",
               all = FALSE)
  expect_match(problema(prior_tabla_prueba(sd_log = 0)), "sd_log un número finito mayor que 0", all = FALSE)
  expect_match(problema(prior_tabla_prueba()[sexo == "mujeres"]),
               "^prior_mortalidad_exceso: no trae el prior de la causa 7001 en 2023 para hombres")
  otro_anio <- prior_tabla_prueba(); otro_anio[, anio := 2022L]
  expect_match(problema(otro_anio), "no trae el prior de la causa 7001 en 2023", all = FALSE)
  # «ambos» vale para los dos sexos; sin la columna anio, para todos los años
  ambos <- prior_tabla_prueba()[sexo == "mujeres", !"anio"]; ambos[, sexo := "ambos"]
  b <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(ambos))))
  expect_identical(b$prior_emr_dado[sex_id == 1L, !"sex_id"], b$prior_emr_dado[sex_id == 2L, !"sex_id"])
  # sin la columna sexo, también ambos; y un solo tramo puede cubrir todos los nudos
  un_tramo <- data.table::data.table(edad_inicio = 0, edad_fin = 125, mu_log = log(0.05), sd_log = 0.3)
  b <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(un_tramo))))
  expect_identical(b$prior_emr_dado$sex_id, 1:2)
  expect_identical(.dl_ctx(b, 1L)$emr$mu_log, rep(log(0.05), 6L))
  # un tramo de «ambos» que pisa uno de un sexo: se solapan en ese sexo (dl_tabla() sola no lo ve)
  pisa <- rbind(prior_tabla_prueba(), data.table::data.table(anio = 2023L, sexo = "ambos", edad_inicio = 45,
                                                             edad_fin = 55, mu_log = -3, sd_log = 0.3, fuente = "x"))
  expect_s3_class(dl_tabla("prior_mortalidad_exceso", pisa), "dl_tabla")
  expect_match(problema(pisa), "los tramos de edad de hombres en 2023 se solapan \\(los de «ambos» cuentan en los dos sexos\\)",
               all = FALSE)
  expect_match(problema(prior_tabla_prueba(mu = c(Inf, log(c(2.5, 3.5, 5, 8, 12) * 1e-2)))),
               "mu_log debe ser un número finito", all = FALSE)
  # la puerta de R: la tabla como data.frame da los mismos insumos que el CSV
  d <- proyecto_emr_tabla()
  por_csv <- suppressMessages(dl_insumos(dl_proyecto(d)))
  unlink(file.path(d, "prior_mortalidad_exceso.csv"))
  por_r <- suppressMessages(dl_insumos(dl_proyecto(d, prior_mortalidad_exceso = as.data.frame(prior_tabla_prueba()))))
  expect_identical(por_r$prior_emr_dado, por_csv$prior_emr_dado)
  expect_identical(por_r$hash, por_csv$hash)
  # con otro prior la tabla no se usa: aviso de la revisión, y los insumos son los del proyecto sin la tabla
  con <- proyecto_emr_tabla(prior = "plano"); sin <- proyecto_emr_tabla(tabla = NULL, prior = "plano")
  salida <- utils::capture.output(r <- suppressMessages(dl_revisar_proyecto(con)))
  expect_match(r$detalle[r$estado == "aviso"], "prior_mortalidad_exceso: la tabla no se usa", all = FALSE)
  b_con <- suppressWarnings(suppressMessages(dl_insumos(dl_proyecto(con))))
  b_sin <- suppressMessages(dl_insumos(dl_proyecto(sin)))
  expect_identical(b_con$hash, b_sin$hash)
  expect_null(b_con$prior_emr_dado)
  expect_false("prior_emr_dado.csv" %in% names(b_sin$hashes))
})

# ---- Insumos y log-posterior ----

test_that("insumos con `prior: tabla`: la tabla llega exacta, entra al hash y se congela; el techo es el declarado", {
  tabla <- prior_tabla_prueba()
  b <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(tabla))))
  expect_identical(names(b$prior_emr_dado), c("sex_id", "age_start", "age_end", "mu_log", "sd_log"))
  expect_identical(b$prior_emr_dado$mu_log, tabla$mu_log)                       # sin pérdida al pasar por el CSV
  expect_identical(b$prior_emr_dado$age_end[b$prior_emr_dado$sex_id == 1L], c(50, 60, 70, 80, 95, 125))
  expect_identical(b$techo_emr$origen, "impreso")
  expect_true("prior_emr_dado.csv" %in% names(b$hashes))
  expect_identical(dl_prior_emr(b), b$prior_emr_dado)
  # otro prior en la tabla, otro hash; la misma tabla, el mismo
  otra <- data.table::copy(tabla); otra[1L, mu_log := mu_log + 1e-9]
  expect_false(identical(suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(otra))))$hash, b$hash))
  expect_identical(suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(tabla))))$hash, b$hash)
  carpeta <- file.path(withr::local_tempdir(), "congelados")
  h <- dl_congelar_insumos(b, carpeta)
  expect_true(file.exists(file.path(carpeta, "prior_emr_dado.csv")))
  expect_identical(h[["prior_emr_dado.csv"]], b$hashes[["prior_emr_dado.csv"]])
  # sin techo declarado se deriva del ancla (necesita su mortalidad), como con el prior plano
  derivado <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(tabla, techo = NULL))))
  expect_identical(derivado$techo_emr$origen, "derivado")
})

test_that("contexto: cada nudo toma su tramo, el soporte es [techo / 1000, techo] y el punto inicial cae dentro", {
  # centros que salen del soporte por los dos lados: el primero sobre el techo y el segundo bajo el piso
  mu <- c(log(0.9), log(1e-5), log(c(3.5, 5, 8, 12) * 1e-2))
  tabla <- prior_tabla_prueba(mu = mu)
  # dl_insumos() avisa de los centros fuera del soporte, con los nudos, el piso y el techo
  d <- proyecto_emr_tabla(tabla)
  expect_warning(b <- suppressMessages(dl_insumos(dl_proyecto(d))),
                 paste0("el centro, exp\\(mu_log\\), queda fuera del rango permitido, de 5e-04 \\(el piso\\) a 0.5 ",
                        "\\(el techo\\) por persona-año, en el/los nudo\\(s\\) 40, 50 en hombres; 40, 50 en mujeres: .*",
                        "logaritmo natural de una tasa por persona-año"))
  # la revisión del proyecto lo muestra como aviso del paso «insumos»
  salida <- utils::capture.output(r <- suppressMessages(dl_revisar_proyecto(d)))
  expect_match(r$detalle[r$paso == "insumos" & r$estado == "aviso"], "queda fuera del rango permitido")
  expect_false(any(r$estado == "error"))
  # con los centros dentro del soporte no hay aviso (tampoco en el borde exacto)
  expect_no_warning(suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla()))))
  borde <- prior_tabla_prueba(mu = c(log(0.5 / 1000), log(0.5), log(c(3.5, 5, 8, 12) * 1e-2)))[sexo == "mujeres"]
  expect_no_warning(suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(borde[, sexo := "ambos"])))))
  # el techo que sale del ancla también se compara: unidades por 100 000 en vez de por persona-año
  por_cien_mil <- prior_tabla_prueba(mu = log(c(2, 2.5, 3.5, 5, 8, 12) * 1e3))
  expect_warning(suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(por_cien_mil, techo = NULL)))),
                 "queda fuera del rango permitido")
  ctx <- .dl_ctx(b, 2L)
  expect_identical(ctx$emr, list(mu_log = mu, sd_log = rep(0.3, 6L), cota = c(0.5 / 1000, 0.5), plano = FALSE,
                                 dado = TRUE))
  expect_identical(.dl_ctx(b, 1L)$emr$mu_log, mu + log(1.05))
  th <- .dl_theta_inicial(ctx)
  log_f0 <- unname(th[paste0("logf_", NUDOS_TABLA)])
  expect_identical(log_f0, c(log(0.5) - 0.5, log(0.5 / 1000) + 0.5, mu[3:6]))
  expect_true(all(exp(log_f0) > ctx$emr$cota[1] & exp(log_f0) < ctx$emr$cota[2]))
  lp <- .dl_lp_componentes(th, ctx)
  expect_true(is.finite(lp[["total"]]))
  expect_equal(unname(lp[["emr"]]), sum(stats::dnorm(log_f0, mu, 0.3, log = TRUE)), tolerance = 1e-12)
  # fuera del soporte, por arriba o por abajo, la log-posterior es -Inf
  arriba <- th; arriba["logf_60"] <- log(0.5) + 1e-6
  abajo <- th; abajo["logf_60"] <- log(0.5 / 1000) - 1e-6
  expect_true(all(.dl_lp_componentes(arriba, ctx) == -Inf))
  expect_true(all(.dl_lp_componentes(abajo, ctx) == -Inf))
  # los otros priores no llevan la bandera ni el piso en el punto inicial
  plano <- .dl_ctx(suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(tabla = NULL, prior = "plano")))), 2L)
  expect_null(plano$emr$dado)
  expect_identical(unname(.dl_theta_inicial(plano)[paste0("logf_", NUDOS_TABLA)]), rep(log(0.5) - 2, 6L))
})

test_that("la log-posterior en C++ coincide con la de R (1e-9) con el prior dado por tabla", {
  skip_if_not_installed("Rcpp")
  b <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla())))
  ctx <- .dl_ctx(b, 1L)
  lpr <- .dl_lp_rcpp(ctx)
  th0 <- .dl_theta_inicial(ctx)
  set.seed(7)
  for (rep in 1:10) {
    th <- th0 + rnorm(length(th0), 0, 0.2)
    r <- .dl_lp_componentes(th, ctx)
    expect_equal(unname(lpr$componentes(th)), unname(r), tolerance = 1e-9)
    expect_equal(lpr$total(th), unname(r[["total"]]), tolerance = 1e-9)
  }
  abajo <- th0; abajo["logf_60"] <- log(0.5 / 1000) - 1e-6
  expect_identical(lpr$total(abajo), -Inf)
})

# ---- Ajuste y corrida ----

test_that("dl_ajustar() con un prior estrecho deja log f en el centro de la tabla, dentro del soporte", {
  b <- suppressMessages(dl_insumos(dl_proyecto(proyecto_emr_tabla(prior_tabla_prueba(sd_log = 0.02)))))
  o <- dl_opciones_mcmc(simulaciones = 40L, cadenas = 2L, iteraciones = 3200L, calentamiento = 1600L)
  f <- dl_ajustar(b, o, semilla = 11L, cache = FALSE)
  par <- f$draws_par[["2"]]
  log_f <- par[, paste0("logf_", NUDOS_TABLA)]
  expect_true(all(exp(log_f) >= 0.5 / 1000 & exp(log_f) <= 0.5))
  expect_equal(unname(colMeans(log_f)), log(c(2, 2.5, 3.5, 5, 8, 12) * 1e-2), tolerance = 0.03)
  # con la misma semilla, las mismas cadenas
  expect_identical(dl_ajustar(b, o, semilla = 11L, cache = FALSE)$draws_par, f$draws_par)
})

test_that("dl_correr() con `prior: tabla`: el manifiesto lo declara y el proyecto congelado repite los insumos", {
  d <- proyecto_emr_tabla()
  salida <- file.path(withr::local_tempdir(), "salida")
  run <- suppressWarnings(suppressMessages(dl_correr(d, semilla = 5L, carpeta_salida = salida, rapido = TRUE,
                                                     sensibilidad = FALSE)))
  m <- yaml::read_yaml(file.path(run$dir, "manifest.yaml"))
  expect_identical(m$params$emr_prior, "tabla")
  expect_identical(m$params$emr_cota_origen, "impreso")
  expect_match(paste(unlist(m), collapse = " "), "dado por tabla \\(emr_prior.tipo tabla\\)")
  expect_match(paste(unlist(m$limitaciones), collapse = " "), "truncada entre el piso (5e-04) y el techo (0.5)",
               fixed = TRUE)
  expect_true(file.exists(file.path(run$dir, "inputs", "prior_emr_dado.csv")))
  expect_true(file.exists(file.path(run$dir, "inputs", "contrato", "prior_mortalidad_exceso.csv")))
  b <- suppressMessages(dl_insumos(dl_proyecto(d)))
  congelado <- suppressMessages(dl_insumos(dl_proyecto(file.path(run$dir, "inputs", "contrato"))))
  expect_identical(congelado$hash, b$hash)
  expect_identical(m$inputs$bundle_hash, b$hash)
})

# ---- Formato completo ----

test_that("formato completo: dl_rutas(prior_mortalidad_exceso = ) lleva la tabla, y su lector dice qué corregir", {
  cfg <- dl_configuracion_ejemplo(9100L, cambios = list(emr_prior = list(tipo = "tabla", tipo_procedencia = "estudio de seguimiento")),
                                  formato = "completo")
  nudos <- as.numeric(unlist(cfg$nudos_incidencia))
  buena <- data.table::CJ(sex_id = 1:2, k = seq_along(nudos))
  buena[, `:=`(age_start = nudos[k], age_end = c(nudos[-1L], 125)[k], mu_log = log(0.02) + 0.2 * k, sd_log = 0.3)][, k := NULL]
  insumos <- function(tabla) {
    f <- file.path(withr::local_tempdir(), "prior.csv")
    data.table::fwrite(tabla, f)
    suppressMessages(dl_insumos(cfg, dl_rutas_ejemplo(9100L, formato = "completo", prior_mortalidad_exceso = f)))
  }
  b <- insumos(buena)
  expect_equal(as.data.frame(b$prior_emr_dado), as.data.frame(buena), tolerance = 1e-12)
  expect_identical(.dl_ctx(b, 2L)$emr$sd_log, rep(0.3, length(nudos)))
  expect_true("prior_emr_dado.csv" %in% names(b$hashes))
  # las filas de un sex_id que no es del modelo no se usan
  expect_identical(insumos(rbind(buena, buena[sex_id == 1L][, sex_id := 3L]))$hash, b$hash)
  # sin la pieza en las rutas
  expect_error(suppressMessages(dl_insumos(cfg, dl_rutas_ejemplo(9100L, formato = "completo"))),
               "emr_prior.tipo tabla necesita la tabla del prior de la mortalidad en exceso")
  expect_error(insumos(buena[, !"sd_log"]), "no trae la\\(s\\) columna\\(s\\) sd_log")
  expect_error(insumos(buena[sex_id == 2L]), "no trae filas de sex_id 1 \\(hombres\\)")
  solape <- data.table::copy(buena)[sex_id == 2L & age_start == nudos[1L], age_end := nudos[2L] + 5]
  expect_error(insumos(solape), "tramos de edad vacíos o que se solapan \\(sex_id 2 \\(mujeres\\)\\)")
  expect_error(insumos(data.table::copy(buena)[1L, age_end := NA]), "tramos de edad vacíos o que se solapan")
  expect_error(insumos(buena[age_start != nudos[2L]]),
               sprintf("no cubre el/los nudo\\(s\\) %s \\(sex_id 1 \\(hombres\\)\\)", nudos[2L]))
  expect_error(insumos(data.table::copy(buena)[2L, sd_log := 0]), "exige mu_log finito y sd_log finito y mayor que 0")
  expect_error(insumos(data.table::copy(buena)[2L, mu_log := NA]), "exige mu_log finito y sd_log finito y mayor que 0")
})
