# Casos aleatorios (prueba diferencial): contextos de la log-posterior armados al azar y entradas al azar de las
# funciones sueltas, evaluados con las funciones del paquete. Lo llama pruebas/fixtures.R (antes de escribir el JSON)
# y deja todo en fx$aleatorio; las pruebas están en pruebas/aleatorias.test.mjs.
#
# Los contextos no salen de dl_insumos() sino de insumos sintéticos (una lista con las piezas que lee .dl_ctx():
# cfg, prior_gbd, poblacion, bandas_pobl, datos, loc_ancla y prior_emr_dado), para recorrer combinaciones que el
# proyecto de ejemplo no tiene: edad_inicio y edad_fin cualesquiera, nsub de 1 a 12, nudos fuera de la malla y no
# enteros, bandas de población de 1 a 15 años (y una que empieza después de edad_inicio), bandas del ancla que cruzan
# las de población y con huecos entre ellas, csmr del ancla en solo algunas bandas, remisión por tramos que se
# solapan, los cuatro tipos de prior de la EMR, techo impreso o derivado, rho de 0 a 0.999, lambda de 0.1 a 1, y
# datos de los tres tipos por las dos rutas (log-normal con o sin offset y valor 0; binomial y Poisson con x = 0,
# x = n y n_efectivo). Cada contexto pasa por .dl_ctx() y cada theta por .dl_lp_componentes(): la referencia es el
# paquete, no una copia de sus fórmulas.
#
# Requiere lo que define fixtures.R antes de llamarlo: dl, A(), filas(), ctx_json(), pesos_json(), ulp().

set.seed(20261016)

# ---- Insumos sintéticos ------------------------------------------------------------------------------------------

redondear_a <- function(x, paso) round(x / paso) * paso

# Cortes [inicio, fin) desde `desde` con anchos al azar de `anchos` hasta pasar `hasta`; la última banda es abierta
# (fin 125) si `abierta`. `hueco`: probabilidad de saltar un tramo entre dos bandas.
cortes_bandas <- function(desde, hasta, anchos, abierta = TRUE, hueco = 0) {
  inicio <- numeric(); fin <- numeric(); x <- desde
  while (x < hasta) {
    w <- sample(anchos, 1)
    inicio <- c(inicio, x); fin <- c(fin, x + w); x <- x + w
    if (runif(1) < hueco) x <- x + sample(c(1, 2, 3, 5), 1)
  }
  if (abierta) { inicio <- c(inicio, x); fin <- c(fin, 125) }
  list(inicio = inicio, fin = fin)
}

insumos_sinteticos <- function(caso) {
  edad_inicio <- sample(c(0L, 1L, 7L, 13L, 20L, 30L, 30L, 33L, 35L, 42L, 50L), 1)
  edad_fin <- sample(c(60L, 77L, 90L, 97L, 99L, 99L, 100L, 104L), 1)
  if (edad_fin < edad_inicio + 20L) edad_fin <- edad_inicio + 20L + sample(0:9, 1)
  nsub <- sample(c(1L, 2L, 3L, 4L, 5L, 5L, 5L, 7L, 10L, 12L), 1)
  K <- sample(2:9, 1)
  nudos <- sort(unique(redondear_a(runif(K + 3, edad_inicio - 8, edad_fin + 8), sample(c(0.5, 1, 5), 1))))
  if (runif(1) < 0.4) nudos <- sort(unique(c(edad_inicio, nudos)))
  nudos <- nudos[seq_len(min(length(nudos), 9))]
  if (length(nudos) < 2) nudos <- c(edad_inicio, edad_fin)

  # Población: bandas finas contiguas desde pob_inicio (a veces después de edad_inicio) hasta una banda abierta.
  pob_inicio <- if (runif(1) < 0.25) edad_inicio + sample(1:9, 1) else min(edad_inicio, sample(c(0, 0, 20, 30), 1))
  pob_inicio <- min(pob_inicio, edad_fin - 10)
  bp <- cortes_bandas(pob_inicio, sample(c(80, 90, 95), 1), c(1, 2, 3, 4, 5, 5, 5, 5, 10, 15))
  nb <- length(bp$inicio)
  bandas_pobl <- data.table(age_group_id = 1000L + seq_len(nb), age_start = bp$inicio, age_end = bp$fin)
  N <- round(runif(nb, 2e3, 2e6)) + sample(c(0, 0.5, 0.25), nb, replace = TRUE)
  poblacion <- data.table(location_id = "1", location_level = 0L, sex_id = 1L, age_group_id = bandas_pobl$age_group_id,
                          val = N)

  # Ancla: bandas que pueden cruzar las de población y dejar huecos; la última, abierta o no.
  a0 <- max(edad_inicio, pob_inicio) + sample(c(0, 0, 0, 2, 5), 1)
  ba <- cortes_bandas(a0, min(edad_fin, 95) - 1, c(1, 3, 5, 5, 5, 7, 10, 20), abierta = runif(1) < 0.7,
                      hueco = sample(c(0, 0, 0.3), 1))
  ba$fin <- pmin(ba$fin, 125)
  dentro <- ba$inicio <= edad_fin
  ba <- lapply(ba, `[`, dentro)
  n_a <- length(ba$inicio)
  if (n_a < 1L) return(NULL)
  medio <- (ba$inicio + pmin(ba$fin, edad_fin + 1)) / 2
  prev <- plogis(-6.5 + 0.06 * medio + rnorm(n_a, 0, 0.15))
  csmr <- prev * exp(rnorm(n_a, log(0.02 + 0.0015 * medio), 0.3))
  pg <- function(medida, val, sexo = 1L) data.table(
    measure_id = medida, sex_id = sexo, age_group_id = 2000L + seq_len(n_a), age_start = ba$inicio, age_end = ba$fin,
    year = 2023L, val = val, sigma_log = runif(n_a, 0.03, 0.5))
  csmr_filas <- pg(900006L, csmr)
  if (runif(1) < 0.3 && n_a > 2) csmr_filas <- csmr_filas[-sample(seq_len(n_a), 1)]   # csmr en algunas bandas
  prior_gbd <- rbind(pg(5L, prev), csmr_filas)
  if (runif(1) < 0.3) prior_gbd <- rbind(prior_gbd, pg(5L, prev * 1.1, 2L), pg(900006L, csmr * 1.4, 2L))

  # Prior de la EMR y su cota
  tipo <- sample(c("informativo_edad", "informativo_edad", "plano_cota", "tabla", "cero"), 1)
  emr_prior <- list(tipo = tipo)
  if (runif(1) < 0.3) emr_prior$fraccion_aguda <- list(valor = sample(c(0.05, 0.2), 1))
  if (runif(1) < 0.3) emr_prior$factor_sd <- list(valor = sample(c(0.7, 1.5, 2), 1))
  if (runif(1) < 0.3) emr_prior$factor_techo <- sample(c(1.5, 2, 4), 1)
  cfg <- list(nudos_incidencia = nudos, edad_inicio = edad_inicio, edad_fin = edad_fin, nsub = nsub,
              anchor = list(lambda = sample(c(1, 0.9, 0.5, 0.25, 0.1), 1),
                            rho_edad = sample(c(0, 0.3, 0.5, 0.8, 0.95, 0.999), 1), medidas = "prevalence"),
              emr_prior = emr_prior, sigma_suavidad = sample(c(0.1, 0.5, 1, 3), 1), sexos = 1L)
  impresa <- tipo == "plano_cota" || runif(1) < 0.5
  if (tipo == "cero") cfg$emr_prior$cota <- c(0, 0)
  else if (impresa) cfg$emr_prior$cota <- c(sample(c(0, 0, 1e-4, 0.003), 1), sample(c(0.15, 0.25, 0.6), 1))
  else cfg$emr_prior$cota <- dl$.dl_techo_emr(cfg, prior_gbd)$cota
  f_max <- if (tipo == "cero") 0 else cfg$emr_prior$cota[2]

  # Remisión: un valor y tramos que pueden solaparse o salir de la malla; r + f_max por debajo del límite de RK4.
  r_max <- max(0, 2.2 * nsub - f_max)
  tramos <- lapply(seq_len(sample(0:3, 1)), function(k) {
    a <- redondear_a(runif(1, edad_inicio - 5, edad_fin + 5), sample(c(0.1, 1), 1))
    list(edad_inicio = a, edad_fin = a + sample(c(0.3, 4, 12.5, 30, 200), 1),
         valor = min(sample(c(0, 0.05, 0.5, 2), 1), r_max))
  })
  cfg$remision <- list(valor = min(sample(c(0, 0, 0.01, 0.2, 1), 1), r_max), por_edad = tramos)

  # Prior dado por tabla: tramos que cubren todos los nudos (uno puede caer justo en un borde).
  dado <- NULL
  if (tipo == "tabla") {
    bordes <- sort(unique(c(min(nudos), redondear_a(runif(sample(1:3, 1), min(nudos), max(nudos)), 1),
                            if (length(nudos) > 2) nudos[2], max(nudos) + 1)))
    m <- length(bordes) - 1L
    dado <- data.table(sex_id = 1L, age_start = bordes[-length(bordes)], age_end = bordes[-1L],
                       mu_log = log(f_max) - runif(m, 0.5, 3), sd_log = runif(m, 0.1, 0.6))
  }

  # Datos locales: tres tipos, dos rutas, con los bordes de cada verosimilitud.
  eta <- sample(c(0, 0, 1e-4, 1e-2), 1)
  cfg$offset_lognormal <- eta
  n_d <- sample(0:8, 1)
  edad_dato_min <- max(edad_inicio, pob_inicio)
  datos <- rbindlist(lapply(seq_len(n_d), function(k) {
    s <- redondear_a(runif(1, edad_dato_min, edad_fin - 1), sample(c(0.5, 1, 5), 1))
    s <- max(s, edad_dato_min)
    e <- s + sample(c(1, 2, 5, 10, 30, 200), 1)
    if (ceiling(s) >= e || ceiling(s) > edad_fin) { s <- edad_dato_min; e <- edad_dato_min + 5 }
    tipo_dato <- sample(c("prev_estudio", "incidencia", "csmr"), 1, prob = if (tipo == "cero") c(0.5, 0.45, 0.05) else NULL)
    medio <- (s + min(e, edad_fin)) / 2
    q0 <- switch(tipo_dato, prev_estudio = plogis(-6.5 + 0.06 * medio), incidencia = exp(-7.5 + 0.06 * medio),
                 csmr = plogis(-6.5 + 0.06 * medio) * (0.02 + 0.0015 * medio))
    if (runif(1) < 0.5) {
      val <- if (eta > 0 && runif(1) < 0.3) 0 else q0 * exp(rnorm(1, 0, 0.3))
      data.table(tipo_dato = tipo_dato, age_start = s, age_end = e, val = val,
                 se = max(val, q0) * runif(1, 0.05, 0.5), x = NA_real_, n = NA_real_, n_efectivo = NA_real_)
    } else {
      n <- sample(c(50, 1000, 1e5), 1)
      x <- sample(c(0, round(n * q0 * exp(rnorm(1, 0, 0.3))), if (tipo_dato == "prev_estudio") n else 3), 1)
      data.table(tipo_dato = tipo_dato, age_start = s, age_end = e, val = NA_real_, se = NA_real_, x = x, n = n,
                 n_efectivo = if (runif(1) < 0.3) n / 2 else NA_real_)
    }
  }))
  if (!nrow(datos)) datos <- data.table(tipo_dato = character(), age_start = numeric(), age_end = numeric(),
                                        val = numeric(), se = numeric(), x = numeric(), n = numeric(),
                                        n_efectivo = numeric())
  datos[, `:=`(sex_id = 1L, outlier = FALSE, location_level = 0L)]

  list(cfg = cfg, prior_gbd = prior_gbd, poblacion = poblacion, loc_ancla = "1", bandas_pobl = bandas_pobl,
       datos = datos, prior_emr_dado = dado)
}

# theta al azar alrededor de valores plausibles, más los bordes: techo, piso, desborde, i casi nula, RK4 al límite.
thetas_aleatorios <- function(ctx) {
  K <- length(ctx$nudos)
  cero <- dl$.dl_ctx_emr_cero(ctx)
  base_i <- seq(log(3e-4), log(2e-2), length.out = K)
  base_f <- if (cero) numeric() else pmin(ctx$emr$mu_log, log(ctx$emr$cota[2]) - 0.3)
  dentro <- function(lf) if (cero) lf else pmin(pmax(lf, log(max(ctx$emr$cota[1], 1e-300)) + 1e-6),
                                                log(ctx$emr$cota[2]) - 1e-6)
  out <- list(list(nombre = "inicial", theta = unname(dl$.dl_theta_inicial(ctx))))
  for (k in 1:4)
    out[[length(out) + 1L]] <- list(nombre = sprintf("azar_%d", k),
                                    theta = c(base_i + rnorm(K, 0, 0.6), dentro(base_f + rnorm(length(base_f), 0, 0.4))))
  base <- out[[2L]]$theta
  if (!cero) {
    l2 <- log(ctx$emr$cota[2]); j <- K + sample(seq_len(K), 1)
    for (d in c(-1, 0, 1)) { th <- base; th[j] <- l2 + d * ulp(l2)
      out[[length(out) + 1L]] <- list(nombre = sprintf("techo_%+d_ulp", d), theta = th) }
    th <- base; th[j] <- l2 + 0.5; out[[length(out) + 1L]] <- list(nombre = "sobre_el_techo", theta = th)
    if (ctx$emr$cota[1] > 0) {
      l1 <- log(ctx$emr$cota[1])
      for (d in c(-1, 0, 1)) { th <- base; th[j] <- l1 + d * ulp(l1)
        out[[length(out) + 1L]] <- list(nombre = sprintf("piso_%+d_ulp", d), theta = th) }
    }
  }
  th <- base; th[seq_len(K)] <- -40; out[[length(out) + 1L]] <- list(nombre = "i_casi_nula", theta = th)
  th <- base; th[seq_len(K)] <- log(2.6 * ctx$nsub); out[[length(out) + 1L]] <- list(nombre = "rk4_al_limite", theta = th)
  th <- base; th[seq_len(K)] <- 30; out[[length(out) + 1L]] <- list(nombre = "desborde", theta = th)
  lapply(out, function(o) c(o["nombre"], list(theta = A(o$theta),
                                              lp = as.list(dl$.dl_lp_componentes(as.numeric(o$theta), ctx)))))
}

emr_fuente_sintetica <- function(b) {
  pg <- b$prior_gbd
  banda <- function(m) list(sex_id = A(m$sex_id), inicio = A(m$age_start), fin = A(m$age_end), val = A(m$val),
                            sigma_log = A(m$sigma_log))
  cfg <- b$cfg
  list(tipo = cfg$emr_prior$tipo, fraccion_aguda = dl$.dl_fraccion_aguda(cfg), factor_sd = dl$.dl_factor_sd_emr(cfg),
       factor_techo = as.numeric(cfg$emr_prior$factor_techo %||% 3), cota = A(cfg$emr_prior$cota),
       prev = banda(pg[measure_id == 5L]), csmr = banda(pg[measure_id == 900006L]),
       dado = if (!is.null(b$prior_emr_dado)) list(inicio = A(b$prior_emr_dado$age_start),
                                                   fin = A(b$prior_emr_dado$age_end),
                                                   mu_log = A(b$prior_emr_dado$mu_log),
                                                   sd_log = A(b$prior_emr_dado$sd_log)))
}

n_contextos <- 80L
contextos_azar <- list(); intentos <- 0L
while (length(contextos_azar) < n_contextos) {
  intentos <- intentos + 1L
  if (intentos > 10L * n_contextos) stop("demasiados insumos sintéticos inválidos")
  b <- insumos_sinteticos(length(contextos_azar) + 1L)
  if (is.null(b)) next
  ctx <- tryCatch(dl$.dl_ctx(b, 1L), error = function(e) NULL)
  if (is.null(ctx)) next
  th <- thetas_aleatorios(ctx)
  sol <- dl$.dl_edo_ctx(as.numeric(th[[2L]]$theta), ctx)
  contextos_azar[[length(contextos_azar) + 1L]] <- list(
    ctx = ctx_json(ctx, b, 1L),
    r = list(n_media = length(ctx$edades_media), r_media = A(ctx$r_media), chol_R = filas(ctx$ancla$chol_R),
             idx_anual_malla = A(ctx$idx_anual_malla - 1L), anual = A(ctx$anual),
             W_ultima = A(ctx$W[nrow(ctx$W), ]), W_primera = A(ctx$W[1L, ]),
             pesos_ancla = pesos_json(ctx$pesos_ancla), pesos_datos = pesos_json(ctx$pesos_datos),
             emr = list(mu_log = A(ctx$emr$mu_log), sd_log = A(ctx$emr$sd_log), cota = A(ctx$emr$cota)),
             theta_inicial = A(unname(dl$.dl_theta_inicial(ctx))),
             edo = list(p = A(sol$p), i = A(sol$i), f = A(sol$f))),
    emr_fuente = emr_fuente_sintetica(b),
    thetas = th)
}

# ---- EDO: dl_edo() con entradas al azar ----------------------------------------------------------------------------

edo_azar <- lapply(seq_len(40L), function(k) {
  edad_inicio <- sample(c(0, 5, 20, 30, 33, 41.5), 1)
  edad_fin <- edad_inicio + sample(c(10, 37, 59, 69, 70), 1)
  nsub <- sample(c(1L, 2L, 3L, 5L, 6L, 9L, 10L), 1)
  K <- sample(2:8, 1)
  nudos <- sort(unique(round(runif(K, edad_inicio - 5, edad_fin + 5), 1)))
  if (length(nudos) < 2) nudos <- c(edad_inicio, edad_fin)
  li <- log(runif(length(nudos), 1e-5, 0.05)); lf <- log(runif(length(nudos), 1e-4, 0.3))
  n_media <- length(dl$.dl_edades_media(edad_inicio, edad_fin, nsub))
  remision <- switch(sample(1:3, 1), 0, runif(1, 0, 0.8), runif(n_media, 0, 0.5))
  p0 <- sample(c(0, 0, 0.05, 0.3), 1)
  s <- dl_edo(li, lf, nudos, edad_inicio = edad_inicio, edad_fin = edad_fin, remision = remision, p0 = p0, nsub = nsub)
  list(nudos = A(nudos), log_i = A(li), log_f = A(lf), edad_inicio = edad_inicio, edad_fin = edad_fin, nsub = nsub,
       remision = if (length(remision) == 1L) remision else A(remision), p0 = p0,
       edades = A(s$edades), p = A(s$p), i = A(s$i), f = A(s$f))
})

# ---- Bandas: bandas finas, banda de cada edad y pesos con bandas al azar --------------------------------------------

bandas_azar <- lapply(seq_len(30L), function(k) {
  bp <- cortes_bandas(sample(c(0, 10, 30), 1), sample(c(80, 95), 1), c(1, 2, 3, 5, 5, 5, 10, 15))
  nb <- length(bp$inicio)
  lim <- data.table(age_group_id = 100L + seq_len(nb), age_start = bp$inicio, age_end = bp$fin)
  # agregados que .dl_bandas_finas debe quitar: todas las edades, uniones de dos bandas vecinas y un duplicado
  j <- sample(seq_len(nb - 1L), min(3L, nb - 1L))
  agregados <- data.table(age_group_id = 500L + seq_along(j), age_start = lim$age_start[j], age_end = lim$age_end[j + 1L])
  dup <- lim[sample(seq_len(nb), 1)][, age_group_id := 900L]
  todas <- rbind(lim, agregados, data.table(age_group_id = 22L, age_start = 0, age_end = 125),
                 if (runif(1) < 0.3) dup)
  todas <- todas[sample(nrow(todas))]
  finas <- dl$.dl_bandas_finas(todas)
  N <- round(runif(nb, 1e3, 1e6)) + 0.5
  pobl <- data.table(age_group_id = lim$age_group_id, val = N)
  a0 <- max(lim$age_start[1], sample(c(0, 20, 33, 40), 1)); aF <- sample(c(90, 97, 99, 104), 1)
  edades <- seq(a0, aF)
  intervalos <- lapply(seq_len(6L), function(m) {
    s <- round(runif(1, a0, aF - 1)); c(s, s + sample(c(1, 3, 5, 12, 40, 200), 1))
  })
  list(limites = list(age_group_id = A(todas$age_group_id), inicio = A(todas$age_start), fin = A(todas$age_end)),
       finas = A(finas$age_group_id), edades = A(edades), N = A(N),
       sonda = A(c(-1, lim$age_start[1] - 0.5, lim$age_start, lim$age_end - 1e-9, 124.99, 125, 130)),
       banda_sonda = A({ s <- c(-1, lim$age_start[1] - 0.5, lim$age_start, lim$age_end - 1e-9, 124.99, 125, 130)
                        kk <- dl$.dl_banda_de(s, lim); ifelse(is.na(kk), -1L, kk - 1L) }),
       intervalos = lapply(intervalos, function(iv) {
         pw <- tryCatch(dl$.dl_pesos_intervalo(edades, iv[1], iv[2], pobl, lim), error = function(e) NULL)
         list(inicio = iv[1], fin = iv[2], error = is.null(pw), idx = if (!is.null(pw)) A(pw$idx - 1L),
              w = if (!is.null(pw)) A(pw$w))
       }))
})

# ---- Pesos de las bandas del proxy y desplazamiento, al azar --------------------------------------------------------

proxy_azar <- lapply(seq_len(30L), function(k) {
  bp <- cortes_bandas(sample(c(0, 15, 40, 60, 86), 1), sample(c(70, 88, 96), 1), c(1, 5, 5, 10, 20), abierta = runif(1) < 0.6,
                      hueco = sample(c(0, 0.4), 1))
  if (!length(bp$inicio)) bp <- list(inicio = 60, fin = 125)
  nb <- length(bp$inicio)
  cat_p <- data.table(age_group_id = 300L + seq_len(nb), age_start = bp$inicio, age_end = bp$fin)
  ids <- sample(cat_p$age_group_id)                       # en cualquier orden: R las ordena por edad
  nsub <- sample(c(1L, 5L, 7L), 1); a0 <- sample(c(0, 30, 33), 1); aF <- sample(c(97, 99, 104), 1)
  edades <- dl$.dl_edades_media(a0, aF, nsub)
  fuera <- sample(c("cero", "vecina"), 1); interp <- sample(c("lineal", "escalon"), 1)
  P <- dl$.dl_pesos_edad(edades, ids, cat_p, fuera, interp)
  n_sim <- 2L
  dX <- data.table(draw = rep(seq_len(n_sim), each = nb), location_id = "07", age_group_id = rep(ids, n_sim),
                   dX = rnorm(nb * n_sim, 0, 0.4))
  beta <- rnorm(n_sim); kappa <- runif(1, 0.1, 1)
  delta <- dl$.dl_desplazamiento_log(dX, P, beta, kappa, 1L)[["07"]]
  list(bandas = list(age_group_id = A(ids), inicio = A(cat_p$age_start[match(ids, cat_p$age_group_id)]),
                     fin = A(cat_p$age_end[match(ids, cat_p$age_group_id)])),
       edad_inicio = a0, edad_fin = aF, nsub = nsub, fuera = fuera, interp = interp,
       filas_id = A(as.integer(rownames(P))), P = filas(P),
       dX = lapply(seq_len(n_sim), function(s) { d <- dX[draw == s]; A(d$dX[match(as.integer(rownames(P)), d$age_group_id)]) }),
       beta = A(beta), kappa = kappa, delta = filas(delta))
})

# ---- Diagnósticos y forma de la propuesta con cadenas al azar --------------------------------------------------------

ar1_azar <- function(n, phi) { x <- numeric(n); x[1] <- rnorm(1); for (t in seq_len(n)[-1]) x[t] <- phi * x[t - 1] + rnorm(1); x }
diag_azar <- lapply(seq_len(40L), function(k) {
  n_cad <- sample(1:5, 1); n_sim <- sample(c(4:12, 25, 37, 60, 101), 1); d <- sample(1:4, 1)
  phi <- runif(d, -0.95, 0.99); escala <- 10^runif(d, -6, 3); centro <- rnorm(d, 0, 10)
  cadenas <- lapply(seq_len(n_cad), function(c)
    sapply(seq_len(d), function(j) centro[j] + escala[j] * (ar1_azar(n_sim, phi[j]) + 0.3 * c * (runif(1) < 0.5))))
  cadenas <- lapply(cadenas, function(m) matrix(m, nrow = n_sim))
  max_lag <- sample(c(2L, 3L, 7L, 200L), 1)
  ess <- tryCatch(unname(dl_ess(cadenas, max_lag = max_lag)), error = function(e) NULL)
  list(cadenas = lapply(cadenas, filas), max_lag = max_lag, rhat = A(unname(dl_rhat(cadenas))),
       ess = if (is.null(ess)) NULL else A(ess), ess_error = is.null(ess),
       forma = filas(tryCatch(dl$.dl_mh_forma_propuesta(do.call(rbind, cadenas)),
                              error = function(e) matrix(NA_real_, d, d))))
})

# ---- Álgebra: chol de AR(1) con n y rho al azar, y L_ancla -------------------------------------------------------------

chol_azar <- lapply(seq_len(30L), function(k) {
  n <- sample(c(1:25, 40L), 1); rho <- sample(c(runif(1), 1 - 10^-runif(1, 1, 4), 0), 1)
  U <- dl$.dl_chol_ar1(n, rho); z <- rnorm(n, 0, 0.5); sl <- runif(n, 0.02, 0.6); lambda <- runif(1, 0.05, 1)
  list(n = n, rho = rho, U = filas(U), z = A(z), sigma_log = A(sl), lambda = lambda,
       valor = dl$.dl_lp_mvn_ar1(z, sl, U, lambda))
})

# ---- Beta por momentos al azar ----------------------------------------------------------------------------------------

beta_azar <- lapply(seq_len(60L), function(k) {
  m <- sample(c(runif(1), runif(1, 0, 0.01), 1 - runif(1, 0, 0.01), 0, 1), 1, prob = c(6, 1, 1, 0.3, 0.3))
  ancho <- sample(c(runif(1, 0, 0.5), runif(1, 0, 1e-5), 3.92 * sqrt(m * (1 - m)) * runif(1, 0.9, 1.1)), 1)
  inf <- m - ancho * runif(1); sup <- inf + ancho
  r <- suppressWarnings(dl$.dl_beta_momentos(m, inf, sup))
  c(list(media = m, inferior = inf, superior = sup), r)
})

fx$aleatorio <- list(contextos = contextos_azar, edo = edo_azar, bandas = bandas_azar, proxy = proxy_azar,
                     diagnosticos = diag_azar, cholesky = chol_azar, beta = beta_azar,
                     intentos_contextos = intentos)
cat(sprintf("casos aleatorios: %d contextos (%d intentos), %d EDO, %d bandas, %d proxies, %d diagnósticos\n",
            length(contextos_azar), intentos, length(edo_azar), length(bandas_azar), length(proxy_azar),
            length(diag_azar)))
