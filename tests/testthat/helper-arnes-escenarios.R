# Arnés de compatibilidad (ver helper-arnes-api.R): variantes de los datos, piezas comunes y escenarios E1-E12.

# ---- Variantes de los datos (copias locales del arnés) ----
#
# Algunas ramas del modelo no se activan con los datos de ejemplo tal cual: la ruta de Poisson de la verosimilitud
# (incidencia con conteos), un estado de severidad cuyo intervalo no admite una Beta, una partición de severidad que
# no suma 1 exacto, una beta lineal declarada en otra escala (HAQ en 0-1) o un maestro de causas sin sumas. El arnés
# las activa con copias modificadas de unos pocos archivos, escritas en la carpeta de salida del escenario; las
# rutas de las copias entran con cambios_rutas (o carpeta_config). La referencia se genera con las mismas copias, así
# que la comparación sigue siendo entre versiones del código sobre los mismos datos.

.carpeta_variante <- function(ctx, nombre) {
  d <- file.path(ctx$carpeta, "variantes", nombre)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# CSV como texto (todas las columnas character, vacíos como ""): así una copia reescribe byte a byte las celdas que
# no se tocan.
.csv_leer_texto <- function(path)
  data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")

.csv_escribir_texto <- function(dt, path) {
  dt <- data.table::copy(dt)
  for (cn in names(dt)) data.table::set(dt, i = which(dt[[cn]] == ""), j = cn, value = NA_character_)
  data.table::fwrite(dt, path, eol = "\n", na = "")
  path
}

# Copia `origen` en `destino` reemplazando cada `buscar[k]` (texto literal, exactamente una vez) por `poner[k]`.
.copiar_reemplazando <- function(origen, destino, buscar, poner) {
  txt <- paste(readLines(origen, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (k in seq_along(buscar)) {
    n <- lengths(regmatches(txt, gregexpr(buscar[k], txt, fixed = TRUE)))
    .afirmar(n == 1L, "variante de ", basename(origen), ": \u00ab", buscar[k], "\u00bb aparece ", n, " veces")
    txt <- sub(buscar[k], poner[k], txt, fixed = TRUE)
  }
  writeLines(enc2utf8(txt), destino, useBytes = TRUE)
  destino
}

# E4: datos.csv con la cohorte de incidencia nacional en conteos (x casos en n personas-año, sin val ni se), que
# entra a la verosimilitud por la ruta de Poisson, y con los estudios débiles de .ARNES (filas prev_estudio copiadas
# de la de 60-64 años). El csmr sigue con val/se (log-normal) y los estudios de prevalencia con conteos (binomial).
.datos_e4 <- function(ctx) {
  d <- .csv_leer_texto(file.path(ctx$raiz, "datos.csv"))
  k <- which(d$tipo_dato == "incidencia" & d$location_level == "0" & d$outlier == "FALSE")
  .afirmar(length(k) >= 2L, "variante de datos: sin cohorte de incidencia nacional")
  n <- .ARNES$personas_anio_cohorte
  d$x[k] <- as.character(round(as.numeric(d$val[k]) * n))
  d$n[k] <- as.character(n)
  d$val[k] <- ""
  d$se[k] <- ""
  base <- d[d$dato_id == "prev_nac_2_17"]
  .afirmar(nrow(base) == 1L, "variante de datos: falta el estudio de prevalencia prev_nac_2_17")
  nuevas <- lapply(.ARNES$estudios_debiles, function(e) {
    f <- data.table::copy(base)
    f$dato_id <- sprintf("prev_debil_%d_%d", e$sex_id, e$age_group_id)
    f$sex_id <- as.character(e$sex_id)
    f$sex_name <- if (e$sex_id == 1L) "Male" else "Female"
    f$age_start <- as.character(e$edad_inicio)
    f$age_end <- as.character(e$edad_inicio + 5L)
    f$age_group_id <- as.character(e$age_group_id)
    f$x <- as.character(e$x)
    f$n <- as.character(e$n)
    f
  })
  d <- data.table::rbindlist(c(list(d), nuevas))
  .csv_escribir_texto(d, file.path(.carpeta_variante(ctx, "datos"), "datos.csv"))
}

# E3: severidad/<causa>.csv con el estado moderado (9802) en 5 % [0, 90 %]: su varianza (0.9 / 3.92)^2 supera
# m(1 - m), así que no admite una Beta y el AVD lo toma como constante (con aviso). El estado leve absorbe la
# diferencia, con su intervalo escalado, y las proporciones siguen sumando 1.
.severidad_intervalo_ancho <- function(ctx, causa) {
  s <- .csv_leer_texto(file.path(ctx$raiz, "severidad", sprintf("%d.csv", causa)))
  k <- which(s$health_state_id == "9802"); j <- which(s$health_state_id == "9801")
  .afirmar(length(k) == 1L && length(j) == 1L, "variante de severidad: faltan los estados 9801 o 9802")
  nueva <- 0.05
  fac <- (as.numeric(s$proportion[j]) + as.numeric(s$proportion[k]) - nueva) / as.numeric(s$proportion[j])
  for (cn in c("proportion", "prop_lower", "prop_upper")) s[[cn]][j] <- .num_texto(as.numeric(s[[cn]][j]) * fac)
  s$proportion[k] <- "0.05"; s$prop_lower[k] <- "0"; s$prop_upper[k] <- "0.9"
  d <- file.path(.carpeta_variante(ctx, "severidad"), "severidad")
  dir.create(d, showWarnings = FALSE)
  .csv_escribir_texto(s, file.path(d, sprintf("%d.csv", causa)))
}

# E7: copia de la partición de severidad con dos cambios. (1) Las filas cause_sequela del padre 9100 para la
# secuela moderada de 9101 (91013) x 1.02: la severidad de 9101 derivada desde el padre deja de coincidir con las
# filas propias de 9101. (2) Las filas cause_health_state de 9102 x (1 + 8e-7): suman 1.0000008, como las
# proporciones redondeadas de una partición real, y la derivación directa las renormaliza (val, lower y upper).
.particion_variante <- function(ctx) {
  origen <- file.path(ctx$raiz, "particion", .ARNES$particion)
  base <- .carpeta_variante(ctx, "particion")
  .afirmar(file.copy(origen, base, recursive = TRUE), "no se copi\u00f3 la partici\u00f3n de severidad")
  destino <- file.path(base, .ARNES$particion)
  escalar <- function(entidad, filas, factor) {
    p <- list.files(file.path(destino, entidad, "proportion"), pattern = "[.]csv$", full.names = TRUE)
    .afirmar(length(p) == 1L, "partici\u00f3n sin ", entidad)
    t <- .csv_leer_texto(p)
    k <- filas(t)
    .afirmar(length(k) > 0L, "variante de la partici\u00f3n: ninguna fila de ", entidad)
    for (cn in c("val", "lower", "upper")) t[[cn]][k] <- .num_texto(as.numeric(t[[cn]][k]) * factor)
    .csv_escribir_texto(t, p)
  }
  escalar("cause_sequela", function(t) which(t$cause_id == "9100" & t$sequela_id == "91013"), 1.02)
  escalar("cause_health_state", function(t) which(t$cause_id == "9102"), 1 + 8e-7)
  destino
}

# E8: configuración y extracción con la beta lineal de HAQ en la escala 0-1 (beta x 100) y escala 0.01 en la
# transformación: dX (proxy en 0-100) se multiplica por 0.01 antes de la beta.
.haqi_escala_0a1 <- function(ctx, causa) {
  d <- .carpeta_variante(ctx, "haqi_0a1")
  cfg <- file.path(d, "config")
  dir.create(cfg, showWarnings = FALSE)
  .afirmar(all(file.copy(list.files(ctx$config, full.names = TRUE), cfg)), "no se copi\u00f3 la configuraci\u00f3n")
  .copiar_reemplazando(file.path(ctx$config, sprintf("%d.yaml", causa)), file.path(cfg, sprintf("%d.yaml", causa)),
                       "    escala: 1\n    escala_procedencia: datos sint\u00e9ticos de ejemplo",
                       paste0("    escala: 0.01\n    escala_procedencia: variante del arn\u00e9s, beta de HAQ ",
                              "estimada con el \u00edndice en 0-1"))
  extr <- .copiar_reemplazando(
    file.path(ctx$raiz, "extraccion.yaml"), file.path(d, "extraccion.yaml"),
    c("beta_impreso: -0.012 (-0.018 a -0.006)", "beta_valor: '-0.012'", "beta_inferior: '-0.018'",
      "beta_superior: '-0.006'"),
    c("beta_impreso: -1.2 (-1.8 a -0.6)", "beta_valor: '-1.2'", "beta_inferior: '-1.8'", "beta_superior: '-0.6'"))
  list(config = cfg, extraccion = extr)
}

# E8: carpeta registro/ con 9100 sin hijas en master_gbd.csv: la consolidación acepta un ajuste nacional de 9100
# (restricción 7 de data-raw/LEEME.md) y así se consolida la proyección 2024 de E8. `origen`: la carpeta registro/
# que se copia (S5, en el formato simple, copia la de la traducción del proyecto).
.registro_sin_suma <- function(ctx, origen = file.path(ctx$raiz, "registro")) {
  d <- .carpeta_variante(ctx, "registro_sin_suma")
  .afirmar(all(file.copy(list.files(origen, full.names = TRUE), d)), "no se copi\u00f3 registro/")
  m <- .csv_leer_texto(file.path(d, "master_gbd.csv"))
  m$hijos[m$cause_id == "9100"] <- ""
  .csv_escribir_texto(m, file.path(d, "master_gbd.csv"))
  d
}

# Registro de corridas con las entradas de `desde` cuyo run_id cumple `patron`.
.registro_filtrado <- function(ctx, nombre, desde, patron) {
  ds <- Filter(function(d) grepl(patron, d$run_id), yaml::read_yaml(desde)$datasets)
  p <- file.path(ctx$carpeta, paste0("registro_", nombre, ".yaml"))
  yaml::write_yaml(list(datasets = ds), p)
  p
}

# ---- Piezas comunes de los escenarios ----

.contexto <- function(api, raiz, carpeta, motor = .ARNES$mcmc$motor) {
  o <- .ARNES$mcmc
  list(api = api, raiz = raiz, carpeta = carpeta, config = file.path(raiz, "config"), semilla = .ARNES$semilla,
       opciones = api$opciones_mcmc(simulaciones = o$simulaciones, cadenas = o$cadenas, iteraciones = o$iteraciones,
                                    calentamiento = o$calentamiento, adelgazamiento = o$adelgazamiento,
                                    nucleos = o$nucleos, motor = motor))
}

.registro <- function(ctx, nombre) {
  p <- file.path(ctx$carpeta, paste0("registro_", nombre, ".yaml"))
  if (!file.exists(p)) writeLines("datasets: []", p)
  p
}

.insumos <- function(ctx, causa, cambios = NULL, anio = 2023L, datos = TRUE, proxies = TRUE, sin_severidad = FALSE,
                     carpeta_config = ctx$config, cambios_rutas = NULL) {
  rutas <- rutas_acs(ctx$api, ctx$raiz, causa, anio = anio, con_datos = datos, con_proxies = proxies,
                     sin_severidad = sin_severidad, cambios_rutas = cambios_rutas)
  conf <- ctx$api$configuracion(causa = causa, carpeta_config = carpeta_config, cambios = cambios)
  b <- suppressMessages(ctx$api$insumos(configuracion = conf, rutas = rutas))
  .afirmar(nrow(b$severidad) > 0L && all(b$severidad$cause_id == b$cfg$cause_id),
           "la tabla de severidad de los insumos de ", causa, " no es de su causa")
  list(insumos = b, rutas = rutas)
}

# Evalúa `expr` y devuelve su valor con los avisos que emitió (sin mostrarlos).
.con_avisos <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(expr, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

# Corrida completa de una causa: insumos, ajuste, ajuste solo con el prior, cascada (opcional), validación contra
# el ancla, factor de comorbilidad, AVD, resumen, etiquetas y exportación (registrada, forzada: cadenas cortas).
# `ins`: insumos y rutas ya armados (list(insumos, rutas)); los escenarios S los arman desde el proyecto en el
# formato simple (helper-arnes-simple.R) y entonces no se usan los argumentos de .insumos().
.corrida <- function(ctx, causa, nombre, registro, cambios = NULL, anio = 2023L, datos = TRUE, proxies = TRUE,
                     sin_severidad = FALSE, cascada = FALSE, kappa = NULL, carpeta_config = ctx$config,
                     cambios_rutas = NULL, ins = NULL) {
  api <- ctx$api; s <- ctx$semilla
  if (is.null(ins))
    ins <- .insumos(ctx, causa, cambios, anio, datos, proxies, sin_severidad, carpeta_config, cambios_rutas)
  b <- ins$insumos; r <- ins$rutas
  f <- api$ajustar(insumos = b, opciones = ctx$opciones, semilla = s, cache = TRUE)
  f0 <- api$ajustar_solo_prior(insumos = b, opciones = ctx$opciones, semilla = s, ajuste = f, cache = TRUE)
  casc <- NULL
  if (cascada) casc <- suppressWarnings(
    if (is.null(kappa)) api$cascada(ajuste = f, insumos = b, semilla = s)
    else api$cascada(ajuste = f, insumos = b, kappa = kappa, semilla = s))
  ff <- if (is.null(casc)) f else casc
  v <- suppressMessages(api$validar_ancla(ajuste = f, insumos = b, rutas = r, cascada = casc))
  como <- api$factor_comorbilidad(insumos = b, rutas = r)
  y <- .con_avisos(api$avd(ajuste = ff, insumos = b, comorbilidad = como, semilla = s))
  res <- api$resumir(piezas = list(fit = ff, yld = y$valor, bundle = b), nivel = .ARNES$nivel, rutas = r)
  lab <- api$etiquetas(ajuste = f, ajuste_prior = f0, insumos = b, grilla_rho = .ARNES$grilla_rho, semilla = s,
                       cascada = casc)
  run <- api$exportar_corrida(piezas = list(resumen = res, fit = ff, yld = y$valor, bundle = b), nombre = nombre,
                              carpeta = ctx$carpeta, etiquetas = lab, validacion = v, guardar_simulaciones = TRUE,
                              registrar = TRUE, forzar = TRUE, ess_minimo = 400, registro = registro, rutas = r)
  list(insumos = b, rutas = r, ajuste = f, ajuste_prior = f0, cascada = casc, validacion = v, comorbilidad = como,
       avd = y$valor, avisos_avd = y$avisos, resumen = res, etiquetas = lab, corrida = run)
}

# E2 (y la base de E9): 9100 con proxies y datos; sin medidas_entrada los datos solo aportan el held-out
# departamental de 2019, que valida la amplitud de la cascada.
.pipeline_e2 <- function(ctx)
  .corrida(ctx, 9100L, nombre = "e2-proxies", registro = .registro(ctx, "e2"), cascada = TRUE, kappa = 1)

# E5 (y la base de E10): subtipos 9101-9103 con cascada y su suma 9100, registrados en un registro propio.
.pipeline_e5 <- function(ctx) {
  reg <- .registro(ctx, "e5")
  hijas <- lapply(9101:9103, function(k)
    .corrida(ctx, k, nombre = sprintf("e5-%d", k), registro = reg, datos = FALSE, cascada = TRUE))
  r <- rutas_acs(ctx$api, ctx$raiz, 9100L, con_datos = FALSE, con_proxies = FALSE)
  suma <- ctx$api$sumar_hijas(corridas_hijas = vapply(hijas, function(h) h$corrida$dir, ""), causa = 9100L,
                              nombre = "e5-suma", carpeta = ctx$carpeta, nivel = .ARNES$nivel, registrar = TRUE,
                              registro = reg, rutas = r)
  list(hijas = hijas, suma = suma, rutas = r, registro = reg)
}

# E4 (y la base de las etiquetas de E11): 9100 con los datos locales en el ajuste. La puerta de anchor_identity
# (0.05) no la salta forzar y las cadenas cortas no llegan: se declara 0.10. La cohorte de incidencia entra en
# conteos (Poisson) y offset_lognormal desplaza la log-normal del csmr (val/se).
.CAMBIOS_E4 <- list(
  medidas_entrada = c("prev_estudio", "incidencia", "csmr"),
  offset_lognormal = .ARNES$offset_lognormal,
  anchor = list(lambda = 0.5, gate_err_mediano = list(
    valor = 0.10, procedencia = "cadenas cortas del arn\u00e9s de compatibilidad")),
  decisiones = c(paste0(.ARNES$procedencia, "."),
                 "lambda 0.5: el csmr del registro vital entra al ajuste y ya inform\u00f3 el ancla"))

.insumos_e4 <- function(ctx)
  .insumos(ctx, 9100L, cambios = .CAMBIOS_E4, proxies = FALSE,
           cambios_rutas = list(datos = .datos_e4(ctx)))

.diferencia_max <- function(a, b) max(abs(unlist(a) - unlist(b)))

# Consolida `registro` con el perfil `version` y lo devuelve normalizado.
.consolidar <- function(ctx, registro, version, nombre, rutas, anios, registro_salida,
                        maestro = file.path(ctx$raiz, "registro", "master_gbd.csv")) {
  out <- ctx$api$consolidar(
    registro = registro, carpeta = ctx$carpeta, perfil = ctx$api$perfil(version), maestro = maestro, rutas = rutas,
    anios = anios, permitir_huecos = FALSE, registrar = TRUE, nombre = nombre, registro_salida = registro_salida,
    carpeta_corridas = ctx$carpeta, nombres_nivel4 = file.path(dirname(maestro), "causas_nivel4_es.csv"))
  normalizar_consolidado(out$dir)
}

.bloque <- function(n, causa) Filter(function(x) identical(as.integer(x$cause_id), as.integer(causa)),
                                     n$manifiesto$bloques)[[1]]

# ---- Escenarios ----

.ESCENARIOS <- list(
  E1 = function(ctx, acum) {
    reg <- .registro(ctx, "e1")
    x <- .corrida(ctx, 9100L, nombre = "e1-nacional", registro = reg, datos = FALSE, proxies = FALSE)
    b <- x$insumos
    .afirmar(nrow(b$datos) == 0L && nrow(b$cov_proxy) == 0L, "E1 trae datos o proxies")
    .afirmar(isTRUE(x$ajuste_prior$prior_only), "E1: el ajuste solo con el prior no qued\u00f3 marcado")
    .afirmar(all(c("anchor_identity", "implied_incidence") %in% attr(x$validacion, "resumen")$check),
             "E1: la validaci\u00f3n no trae anchor_identity e implied_incidence")
    .anotar_ajuste(acum, "E1", 9100L, x$ajuste)
    .anotar_corrida(acum, "E1", x$corrida$dir)
    .anotar_tabla(acum, "E1__comorbilidad", x$comorbilidad)
    .anotar_registro(acum, "e1", reg)
  },

  E2 = function(ctx, acum) {
    x <- .pipeline_e2(ctx)
    d <- x$insumos$datos
    .afirmar(sum(d$location_level == 1L & d$tipo_dato == "csmr") > 0L, "E2 sin csmr departamental held-out")
    .afirmar(sum(d$location_level == 0L) == 0L, "E2: datos nacionales en el ajuste sin medidas_entrada")
    .afirmar(length(x$cascada$departamentos) == 25L, "E2: la cascada no tiene 25 departamentos")
    am <- attr(x$validacion, "amplitud")
    .afirmar(!is.null(am) && nrow(am) == 2L, "E2: no se calcul\u00f3 la amplitud del csmr departamental")
    .afirmar(!is.null(x$corrida$manifest$validacion$amplitud_csmr), "E2: el manifiesto no trae amplitud_csmr")
    .anotar_ajuste(acum, "E2", 9100L, x$ajuste)
    .anotar_corrida(acum, "E2", x$corrida$dir)
    .anotar_registro(acum, "e2", .registro(ctx, "e2"))
  },

  # Cascada plana, con remisión (0.02 y un tramo de 0.1 en [30, 50)) y un estado de severidad sin Beta.
  E3 = function(ctx, acum) {
    cambios <- list(cascada = list(modo = list(valor = "plana", procedencia = .ARNES$procedencia)),
                    remision = list(valor = 0.02, fuente = .ARNES$procedencia,
                                    por_edad = list(list(edad_inicio = 30, edad_fin = 50, valor = 0.1,
                                                         fuente = .ARNES$procedencia))))
    x <- .corrida(ctx, 9100L, nombre = "e3-plana", registro = .registro(ctx, "e3"), cambios = cambios,
                  proxies = FALSE, cascada = TRUE,
                  cambios_rutas = list(severidad = .severidad_intervalo_ancho(ctx, 9100L)))
    b <- x$insumos
    .afirmar(identical(x$cascada$modo, "plana"), "E3: la cascada no es plana")
    .afirmar(length(x$cascada$departamentos) == 25L, "E3: la cascada no tiene 25 departamentos")
    .afirmar(max(abs(x$cascada$renorm$factor - 1)) < 1e-9, "E3: renormalizaci\u00f3n distinta de 1 en la cascada plana")
    .afirmar(isTRUE(all.equal(b$cfg$remision$valor, 0.02)) && length(b$cfg$remision$por_edad) == 1L,
             "E3: la remisi\u00f3n de los cambios no qued\u00f3 en la configuraci\u00f3n")
    pm <- x$corrida$manifest$params
    .afirmar(isTRUE(all.equal(pm$remision, 0.02)) && length(pm$remision_por_edad) == 1L &&
               isTRUE(all.equal(pm$remision_por_edad[[1]]$valor, 0.1)),
             "E3: el manifiesto no declara la remisi\u00f3n por tramo de edad")
    .afirmar(any(grepl("Beta", x$avisos_avd, ignore.case = TRUE)),
             "E3: el AVD no avis\u00f3 del estado de severidad sin Beta")
    .anotar_ajuste(acum, "E3", 9100L, x$ajuste)
    .anotar_corrida(acum, "E3", x$corrida$dir)
  },

  # Datos locales en el ajuste: binomial (estudios de prevalencia), Poisson (cohorte de incidencia en conteos) y
  # log-normal con offset (csmr), más el valor atípico y lambda 0.5; sus etiquetas toman los tres valores.
  E4 = function(ctx, acum) {
    x <- .corrida(ctx, 9100L, nombre = "e4-datos-locales", registro = .registro(ctx, "e4"), cambios = .CAMBIOS_E4,
                  proxies = FALSE, cambios_rutas = list(datos = .datos_e4(ctx)))
    d <- x$insumos$datos
    nac <- d[d$location_level == 0L]
    .afirmar(setequal(unique(nac$tipo_dato[!nac$outlier]), c("prev_estudio", "incidencia", "csmr")),
             "E4: faltan tipos de dato en el ajuste")
    poisson <- nac[!nac$outlier & nac$tipo_dato == "incidencia"]
    .afirmar(nrow(poisson) >= 2L && all(is.na(poisson$se) & !is.na(poisson$x) & !is.na(poisson$n)),
             "E4: la cohorte de incidencia no entra en conteos (ruta de Poisson)")
    .afirmar(all(!is.na(nac$se[!nac$outlier & nac$tipo_dato == "csmr"])), "E4: el csmr no entra con val/se")
    .afirmar(any(nac$outlier), "E4 sin el valor at\u00edpico")
    .afirmar(isTRUE(all.equal(x$insumos$cfg$anchor$lambda, 0.5)), "E4: lambda no es 0.5")
    .afirmar(isTRUE(all.equal(as.numeric(x$insumos$cfg$offset_lognormal), .ARNES$offset_lognormal)),
             "E4: offset_lognormal no qued\u00f3 en la configuraci\u00f3n")
    nt <- x$corrida$manifest$datos$n_likelihood_por_tipo
    .afirmar(length(nt) == 3L && isTRUE(nt$incidencia == nrow(poisson)),
             "E4: el manifiesto no cuenta la cohorte de incidencia en la verosimilitud")
    .afirmar(.diferencia_max(lapply(x$ajuste$draws_par, colMeans), lapply(x$ajuste_prior$draws_par, colMeans)) > 1e-3,
             "E4: los datos no mueven el ajuste respecto del prior")
    # las tres etiquetas, y una celda data-driven con contracción entre el umbral 0.3 y 0.5
    et <- x$etiquetas
    .afirmar(setequal(unique(et$etiqueta), c("prior-driven", "prior-informed", "data-driven")),
             "E4: las etiquetas no tienen los tres valores")
    .afirmar(any(et$etiqueta == "data-driven" & et$contraccion < 0.5),
             "E4: ninguna celda data-driven cerca del umbral de contracci\u00f3n")
    .anotar_ajuste(acum, "E4", 9100L, x$ajuste)
    .anotar_ajuste(acum, "E4-solo-prior", 9100L, x$ajuste_prior)
    .anotar_corrida(acum, "E4", x$corrida$dir)
  },

  E5 = function(ctx, acum) {
    p <- .pipeline_e5(ctx)
    dirs <- vapply(p$hijas, function(h) h$corrida$dir, "")
    omit <- ctx$api$sumar_hijas(corridas_hijas = dirs[1:2], causa = 9100L, nombre = "e5-suma-omitidas",
                                carpeta = ctx$carpeta, nivel = .ARNES$nivel, registrar = TRUE,
                                registro = .registro(ctx, "e5-omitidas"), rutas = p$rutas,
                                omitidas = list(list(cause_id = 9103L,
                                                     motivo = "variante del ejemplo sin el subtipo renovascular")))
    .afirmar(identical(p$suma$manifest$causa$agregacion, "suma_de_hijas") && length(p$suma$manifest$causa$hijas) == 3L,
             "E5: la suma no declara sus tres hijas")
    .afirmar(length(omit$manifest$causa$hijas) == 2L && length(omit$manifest$causa$hijas_omitidas) == 1L,
             "E5: la suma con omitidas no declara la hija omitida")
    for (i in seq_along(p$hijas)) {
      k <- 9100L + i
      .anotar_ajuste(acum, paste0("E5-", k), k, p$hijas[[i]]$ajuste)
      .anotar_corrida(acum, paste0("E5-", k), dirs[i])
    }
    .anotar_corrida(acum, "E5-suma", p$suma$dir)
    .anotar_corrida(acum, "E5-suma-omitidas", omit$dir)
    .anotar_registro(acum, "e5", p$registro)
    .anotar_registro(acum, "e5-omitidas", .registro(ctx, "e5-omitidas"))
  },

  E6 = function(ctx, acum) {
    cambios <- list(anchor = list(componente = list(sequela_ids = c(91012L, 91013L),
                                                    motivo = "solo los estados sintom\u00e1ticos (ejemplo)")),
                    severidad = list(fuente = "mod", run_id = .ARNES$particion))
    x <- .corrida(ctx, 9101L, nombre = "e6-componente", registro = .registro(ctx, "e6"), cambios = cambios,
                  datos = FALSE, proxies = FALSE, sin_severidad = TRUE)
    b <- x$insumos
    .afirmar(!is.null(b$componente), "E6: el componente no qued\u00f3 activo")
    fp <- b$componente$fraccion_prevalencia
    .afirmar(fp > 0 && fp < 1, "E6: fracci\u00f3n de prevalencia del componente fuera de (0, 1)")
    .afirmar(setequal(b$severidad$health_state_id, c(9801L, 9802L)), "E6: la severidad no es la del componente")
    .afirmar(!is.null(x$corrida$manifest$causa$componente), "E6: el manifiesto no declara el componente")
    .anotar_ajuste(acum, "E6", 9101L, x$ajuste)
    .anotar_corrida(acum, "E6", x$corrida$dir)
    .anotar_tabla(acum, "E6__comorbilidad", x$comorbilidad)
  },

  # Severidad de 9101 desde la partición del padre 9100 (copia con las filas del padre distintas de las de 9101) y,
  # sin ajuste, la de 9102 leída directo de sus filas cause_health_state (que suman 1.0000008).
  E7 = function(ctx, acum) {
    part <- .particion_variante(ctx)
    cambios <- list(severidad = list(fuente = "mod", run_id = .ARNES$particion, padre = 9100L))
    x <- .corrida(ctx, 9101L, nombre = "e7-severidad-particion", registro = .registro(ctx, "e7"), cambios = cambios,
                  datos = FALSE, proxies = FALSE, sin_severidad = TRUE, cambios_rutas = list(severity_split = part))
    b <- x$insumos
    .afirmar(identical(b$cfg$severidad$fuente, "mod"), "E7: la severidad no viene de la partici\u00f3n")
    .afirmar(nrow(b$severidad) == 3L, "E7: la severidad derivada no tiene 3 estados")
    cuota <- attr(b$severidad, "cuota_hija")
    .afirmar(!is.null(cuota), "E7: la severidad no se deriv\u00f3 desde el padre")
    propia <- .csv_leer_texto(file.path(ctx$raiz, "severidad", "9101.csv"))
    k <- match(b$severidad$health_state_id, as.integer(propia$health_state_id))
    .afirmar(!anyNA(k) && max(abs(b$severidad$proportion - as.numeric(propia$proportion[k]))) > 1e-3,
             "E7: la severidad desde el padre coincide con las filas propias de 9101")
    directa <- .insumos(ctx, 9102L, cambios = list(severidad = list(fuente = "mod", run_id = .ARNES$particion)),
                        datos = FALSE, proxies = FALSE, sin_severidad = TRUE,
                        cambios_rutas = list(severity_split = part))$insumos$severidad
    ren <- attr(directa, "renormalizacion")
    .afirmar(!is.null(ren) && abs(ren - 1) > 1e-7 && abs(ren - 1) < 1e-4,
             "E7: la severidad directa de 9102 no pas\u00f3 por la renormalizaci\u00f3n")
    .anotar_ajuste(acum, "E7", 9101L, x$ajuste)
    .anotar_corrida(acum, "E7", x$corrida$dir)
    .anotar_tabla(acum, "E7__severidad", data.table::copy(b$severidad)[, cuota_hija := cuota])
    .anotar_tabla(acum, "E7__severidad_directa_9102", data.table::copy(directa)[, renormalizacion := ren])
  },

  # 2019; proyección 2024 con el ancla de 2023, proxies y la beta de HAQ en 0-1 (escala 0.01), y su consolidado.
  E8 = function(ctx, acum) {
    reg <- .registro(ctx, "e8")
    x19 <- .corrida(ctx, 9100L, nombre = "e8-2019", registro = reg, cambios = list(years = list(ajuste = 2019L)),
                    anio = 2019L, datos = FALSE, proxies = FALSE)
    cambios24 <- list(years = list(ajuste = 2024L, ancla = list(valor = 2023L, procedencia = .ARNES$procedencia)))
    haqi <- .haqi_escala_0a1(ctx, 9100L)
    x24 <- .corrida(ctx, 9100L, nombre = "e8-proyeccion-2024", registro = reg, cambios = cambios24, anio = 2024L,
                    datos = FALSE, cascada = TRUE, carpeta_config = haqi$config,
                    cambios_rutas = list(extraction = haqi$extraccion))
    b19 <- x19$insumos; b24 <- x24$insumos
    .afirmar(all(b19$prior_gbd$year == 2019L) && all(b19$poblacion$year == 2019L),
             "E8: 2019 con insumos de otro a\u00f1o")
    .afirmar(all(b24$prior_gbd$year == 2024L) && all(b24$poblacion$year == 2024L) && all(b24$cov_proxy$year == 2024L),
             "E8: el ancla, la poblaci\u00f3n o los proxies de 2024 no quedaron en 2024")
    m24 <- x24$corrida$manifest$params
    .afirmar(m24$year == 2024L && m24$anio_ancla == 2023L, "E8: la proyecci\u00f3n no declara el ancla de 2023")
    .afirmar(length(x24$cascada$departamentos) == 25L, "E8: la cascada de 2024 no tiene 25 departamentos")
    bh <- b24$betas[tolower(b24$betas$covariate_name_short) == "haqi"]
    .afirmar(nrow(bh) == 1L && identical(bh$transformacion, "lineal") && isTRUE(all.equal(bh$escala, 0.01)) &&
               isTRUE(all.equal(bh$beta, -1.2)), "E8: la beta de HAQ no qued\u00f3 en 0-1 con escala 0.01")
    .anotar_ajuste(acum, "E8-2019", 9100L, x19$ajuste)
    .anotar_ajuste(acum, "E8-2024", 9100L, x24$ajuste)
    .anotar_corrida(acum, "E8-2019", x19$corrida$dir)
    .anotar_corrida(acum, "E8-2024", x24$corrida$dir)
    # consolidado de 2024: bloque proyectado (anio_ancla 2023) con conteos de la población de 2024
    reg_sin <- .registro_sin_suma(ctx)
    r24 <- rutas_acs(ctx$api, ctx$raiz, 9100L, anio = 2024L, con_datos = FALSE, con_proxies = FALSE,
                     cambios_rutas = list(registry = reg_sin))
    n <- .consolidar(ctx, reg, "v1", "e8-consolidado-2024", r24, anios = 2024L, .registro(ctx, "e8-consolidado"),
                     maestro = file.path(reg_sin, "master_gbd.csv"))
    bl <- n$manifiesto$bloques
    .afirmar(length(bl) == 1L && bl[[1]]$year == 2024L && bl[[1]]$anio_ancla == 2023L &&
               identical(bl[[1]]$agregacion, "fit"), "E8: el consolidado de 2024 no es la proyecci\u00f3n nacional")
    acum$manifiestos[["E8-consolidado-2024"]] <- n$manifiesto
    .anotar_tabla(acum, "E8-consolidado-2024__sumas_canonico", n$sumas, todas_ubicaciones = TRUE)
    .anotar_registro(acum, "e8", reg)
  },

  E9 = function(ctx, acum) {
    x <- .pipeline_e2(ctx)
    reg <- .registro(ctx, "e9")
    # nivel 0.95: el contrato de las celdas (dl_validate_estimates de la versión 0.2.2) exige ui_level 0.95
    rr <- ctx$api$reresumir_corrida(corrida = x$corrida$dir, carpeta = ctx$carpeta, nivel = 0.95, registrar = TRUE,
                                    registro = reg, rutas = x$rutas)
    .afirmar(grepl("_v2$", rr$run_id), "E9: el re-resumen no es la versi\u00f3n 2 de la corrida")
    .afirmar(identical(rr$manifest$resumen$run_origen, x$corrida$run_id), "E9: el re-resumen no cita su origen")
    # la corrida re-resumida lleva la misma estructura que su origen (simulaciones, etiquetas, diagnósticos,
    # insumos): lo comparan su lista de archivos, sus tablas y sus sumas de control
    .anotar_corrida(acum, "E9", rr$dir)
    .anotar_registro(acum, "e9", reg)
  },

  E10 = function(ctx, acum) {
    p <- .pipeline_e5(ctx)
    dirs <- vapply(p$hijas, function(h) h$corrida$dir, "")
    # segunda versión de 9101 (re-resumen): la consolidación elige la más nueva de (causa, año)
    rr <- ctx$api$reresumir_corrida(corrida = dirs[1], carpeta = ctx$carpeta, nivel = 0.95, registrar = TRUE,
                                    registro = p$registro, rutas = p$hijas[[1]]$rutas)
    .afirmar(grepl("_e5-9101_v2$", rr$run_id), "E10: el re-resumen de 9101 no es su versi\u00f3n 2")
    reg_salida <- .registro(ctx, "e10")
    maestro <- file.path(ctx$raiz, "registro", "master_gbd.csv")
    for (v in c("v1", "v2")) {
      n <- .consolidar(ctx, p$registro, v, paste0("e10-", v), p$rutas, anios = 2023L, reg_salida)
      .afirmar(length(n$manifiesto$bloques) == 4L, "E10: el consolidado no tiene 4 bloques (9100-9103)")
      agreg <- vapply(n$manifiesto$bloques, function(x) paste(x$cause_id, x$agregacion), "")
      .afirmar("9100 suma_de_hijas" %in% agreg, "E10: 9100 no entra como suma de sus subtipos")
      .afirmar(grepl("_e5-9101_v2$", .bloque(n, 9101L)$run_fuente), "E10: 9101 no entra con su versi\u00f3n 2")
      .afirmar(sum(startsWith(names(n$tablas), "tablas_")) == 3L, "E10: el perfil ", v, " no dio 3 tablas")
      acum$manifiestos[[paste0("E10-", v)]] <- n$manifiesto
      for (t in names(n$tablas)) {
        if (v == "v2" && startsWith(t, "canonico_")) next   # la tabla canónica no depende del perfil
        acum$tablas[[paste0("E10-", v, "__", t)]] <- n$tablas[[t]]
      }
      if (v == "v1") .anotar_tabla(acum, "E10-v1__sumas_canonico", n$sumas, todas_ubicaciones = TRUE)
    }
    # consolidado con una hija omitida: registro con 9101 (v1 y v2), 9102 y la suma de las dos sin 9103
    reg_om <- .registro_filtrado(ctx, "e10-omitidas", p$registro, "_e5-910[12]_v[0-9]+$")
    om <- ctx$api$sumar_hijas(corridas_hijas = dirs[1:2], causa = 9100L, nombre = "e10-suma-omitidas",
                              carpeta = ctx$carpeta, nivel = .ARNES$nivel, registrar = TRUE, registro = reg_om,
                              rutas = p$rutas, omitidas = list(list(cause_id = 9103L, motivo = "variante del ejemplo")))
    n <- .consolidar(ctx, reg_om, "v1", "e10-omitidas", p$rutas, anios = 2023L, reg_salida)
    .afirmar(length(n$manifiesto$bloques) == 3L, "E10: el consolidado con omitidas no tiene 3 bloques")
    b9100 <- .bloque(n, 9100L)
    .afirmar(grepl("_e10-suma-omitidas_v1$", b9100$run_fuente) && length(b9100$hijas_omitidas) == 1L &&
               b9100$hijas_omitidas[[1]]$cause_id == 9103L, "E10: el bloque de 9100 no declara 9103 omitida")
    acum$manifiestos[["E10-omitidas"]] <- n$manifiesto
    .anotar_tabla(acum, "E10-omitidas__sumas_canonico", n$sumas, todas_ubicaciones = TRUE)
    .anotar_registro(acum, "e10", reg_salida)
    .anotar_registro(acum, "e10-omitidas", reg_om)
  },

  # Sensibilidad sobre los insumos de E2 y etiquetas con la rejilla rho = (0.5, 0.9) sobre los de E4 (con datos en
  # el ajuste: la contracción depende de rho y la etiqueta más conservadora puede venir de 0.9).
  E11 = function(ctx, acum) {
    api <- ctx$api; s <- ctx$semilla
    b <- .insumos(ctx, 9100L)$insumos          # insumos de E2: datos (held-out) y proxies
    sens <- suppressMessages(suppressWarnings(api$sensibilidad(
      insumos = b, grilla = list(lambda = c(0.5, 1), rho = 0.5, kappa = c(0.5, 1)), semilla = s,
      opciones = ctx$opciones, procesos = 1L)))
    n_celdas <- nrow(unique(data.frame(sexo = sens$sex_id, edad = sens$age_group_id)))
    .afirmar(nrow(sens) == 2L * 2L * n_celdas && setequal(sens$lambda, c(0.5, 1)) && setequal(sens$kappa, c(0.5, 1)),
             "E11: la rejilla de sensibilidad no es 2 lambda x 1 rho x 2 kappa")
    .afirmar(nrow(attr(sens, "departamental")) == 2L * 2L * 25L * n_celdas,
             "E11: la sensibilidad no trae el detalle de los 25 departamentos")
    b4 <- .insumos_e4(ctx)$insumos
    f <- api$ajustar(insumos = b4, opciones = ctx$opciones, semilla = s, cache = TRUE)
    f0 <- api$ajustar_solo_prior(insumos = b4, opciones = ctx$opciones, semilla = s, ajuste = f, cache = TRUE)
    lab <- api$etiquetas(ajuste = f, ajuste_prior = f0, insumos = b4, grilla_rho = c(0.5, 0.9), semilla = s,
                         opciones = ctx$opciones)
    lab05 <- api$etiquetas(ajuste = f, ajuste_prior = f0, insumos = b4, grilla_rho = 0.5, semilla = s)
    .afirmar(nrow(lab) == n_celdas, "E11: las etiquetas no cubren las celdas nacionales")
    .afirmar(any(lab$rho_usado == 0.9), "E11: ninguna celda toma su etiqueta de rho 0.9")
    .afirmar(any(lab$etiqueta != lab05$etiqueta | lab$contraccion != lab05$contraccion),
             "E11: la rejilla de rho no cambia ninguna etiqueta ni contracci\u00f3n")
    .anotar_tabla(acum, "E11__sensibilidad", sens)
    .anotar_tabla(acum, "E11__sensibilidad_departamental", attr(sens, "departamental"))
    .anotar_tabla(acum, "E11__etiquetas", lab)
  },

  # E1 con el motor rcpp (C++ compilado al vuelo): solo si Rcpp y un compilador están disponibles.
  E12 = function(ctx, acum) {
    ctx <- .contexto(ctx$api, ctx$raiz, ctx$carpeta, motor = "rcpp")
    x <- .corrida(ctx, 9100L, nombre = "e12-rcpp", registro = .registro(ctx, "e12"), datos = FALSE, proxies = FALSE)
    .afirmar(identical(x$ajuste$params$engine, "rcpp"), "E12: el ajuste no us\u00f3 el motor rcpp")
    .anotar_ajuste(acum, "E12", 9100L, x$ajuste)
    .anotar_corrida(acum, "E12", x$corrida$dir)
  }
)

# Escenarios que corren siempre y el que exige Rcpp.
.ESCENARIOS_BASE <- paste0("E", 1:11)
.ESCENARIOS_RCPP <- "E12"

# Corre el escenario `id` (E1-E12, o S1-S5 de helper-arnes-simple.R) con `api` sobre los datos de `raiz_datos`,
# escribiendo en `carpeta_salida`. Devuelve list(parametros, celdas, manifiestos, tablas) normalizados (más
# manifiestos_crudos, sin normalizar, que usa data-raw/referencia_legado.R para detectar claves volátiles).
correr_escenario <- function(id, api, raiz_datos, carpeta_salida) {
  esc <- c(.ESCENARIOS, .ESCENARIOS_SIMPLE)[[id]]
  if (is.null(esc)) stop("correr_escenario: escenario desconocido ", id)
  # Orden alfabético "C", el mismo que fija testthat: la versión 0.2.2 ordena con split() las claves de
  # cascada.dx_por_edad.bandas_por_proxy del manifiesto, y con otro orden (es_PE, en_US) haqi va antes que LDI_pc.
  orden <- Sys.getlocale("LC_COLLATE")
  Sys.setlocale("LC_COLLATE", "C")
  on.exit(Sys.setlocale("LC_COLLATE", orden), add = TRUE)
  dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
  acum <- .acumulador()
  esc(.contexto(api, raiz_datos, carpeta_salida), acum)
  .resultado(acum)
}
