# Log-posterior del modelo enfermedad-muerte (notación: ver R/edo.R)
#
#   log posterior(theta) = L_suavidad + L_emr + L_ancla + L_datos  (+ constantes omitidas)
#
#   theta = (log i en los nudos, log f en los nudos); q = cantidad del modelo promediada en una banda de edad:
#   promedio de la cantidad anual (p, i (1 - p) o p f) en las edades enteras de [inicio, fin), ponderado por la
#   población de cada edad (.dl_pesos_intervalo y .dl_q_intervalos en R/bandas.R): una cuadratura por rectángulos
#   de paso 1 año con la cantidad en la edad exacta a, no a mitad del año de edad (ver la cabecera de R/bandas.R).
#
#   L_suavidad = sum_k log N(D2_k; 0, sigma)      D2_k = segunda diferencia de log i en los nudos
#   L_emr      = sum_k log N(log f_k; mu_log_k, sd_log_k)   prior de EMR por nudo (R/emr.R); 0 con el prior plano.
#                Truncamiento: si algún f_k cae fuera de [cota_min, cota_max], log posterior = -Inf.
#   L_ancla    = log MVN(z; 0, Sigma / lambda)     power prior sobre las bandas del ancla:
#                z_b = log q_b - log val_b,  Sigma = diag(sigma_log) R diag(sigma_log),  R_bc = rho^|b - c|
#   L_datos    = sum_d l_d(q_d)                    datos locales, por tipo (prev_estudio, incidencia, csmr):
#                log-normal  l = log N(log(q + eta); log(val + eta), s_log),  s_log = se / (val + eta)
#                binomial    l = x log q + (n - x) log(1 - q)
#                Poisson     l = x log(n q) - n q
#   Prior implícito de theta: log i no tiene más prior que L_suavidad, que es impropio (plano) en el nivel y la
#   pendiente de log i entre nudos, porque solo penaliza la curvatura; la posterior es propia por el ancla. log f:
#   normal por nudo truncada a [cota_min, cota_max] (prior informativo) o plana en log f dentro de la cota
#   (plano_cota). Como las filas de W son no negativas y suman 1, la cota en los nudos acota f en toda la malla h/2.
#
# Constantes omitidas porque no dependen de theta (el cociente de Metropolis-Hastings no las ve): la de
# normalización del truncamiento del prior de EMR, lchoose(n, x) de la binomial y -lgamma(x + 1) de la Poisson.
# Las normales (suavidad, EMR, log-normal, ancla) se escriben completas, con su constante.
#
# Gemela en C++: dl_lp_core() y sus funciones en inst/cpp/dl_core.cpp (motor "rcpp"); cada función de este
# archivo nombra a su gemela. Las fórmulas son las mismas, no el orden de todas las sumas: las dos log-posteriores
# coinciden a 1e-9, no bit a bit (ver la cabecera de dl_core.cpp).

# ---- Constantes ----

# Punto inicial de las cadenas (.dl_theta_inicial). log i = -6 en todos los nudos: i = e^-6, unos 2,5 casos por
# mil personas-año, una incidencia baja del orden de las enfermedades crónicas; el calentamiento lleva las cadenas
# desde ahí hasta la posterior.
.DL_LOG_I_INICIAL <- -6

# log f parte de la media del prior de EMR, pero al menos 0,5 (en log) bajo el techo: f0 <= techo e^-0,5, unas 0,61
# veces el techo. Así el punto inicial queda dentro de la región admitida (sobre el techo la log-posterior es -Inf)
# y lejos de su borde, donde casi todas las propuestas se rechazarían.
.DL_MARGEN_LOG_F_INICIAL <- 0.5

# Ruta de un dato local en la verosimilitud (columna `ruta` de .dl_datos_verosimilitud): log-normal con offset si el
# dato trae val y se, conteos (x de n) si no. Los mismos códigos en C++: RUTA_LOGNORMAL (inst/cpp/dl_core.cpp).
.DL_RUTA_LOGNORMAL <- 0L  # el dato trae val y se
.DL_RUTA_CONTEOS <- 1L    # x de n

# Nombres del vector de .dl_lp_componentes() y de su gemela en C++ (dl_lp_componentes_cpp), en este orden: los
# cuatro términos, el total y el término de datos de cada tipo de .DL_TIPOS_DATO (R/esquema.R).
.DL_LP_NOMBRES <- c("suavidad", "emr", "ancla", "datos", "total",
                    "datos_prev_estudio", "datos_incidencia", "datos_csmr")

# ---- Contexto de un sexo ----

# Datos locales que entran a la verosimilitud de un sexo: nacionales (nivel 0), no outliers, de un tipo de
# .DL_TIPOS_DATO. Añade las columnas
#   tipo_cod  código del tipo (1 prev_estudio, 2 incidencia, 3 csmr)
#   ruta      .DL_RUTA_LOGNORMAL (log-normal con offset: el dato trae val y se) o .DL_RUTA_CONTEOS (x de n)
#   eta       offset de la log-normal: offset_lognormal de la configuración (0 si falta)
#   s_log     se / (val + eta), el error en escala log (ruta log-normal; NA en la de conteos)
#   n_ef      n de los conteos: n_efectivo si viene, si no n (ruta de conteos; NA en la log-normal)
.dl_datos_verosimilitud <- function(b, sexo) {
  d <- data.table::copy(b$datos[sex_id == sexo & outlier == FALSE & location_level == 0L])
  eta <- as.numeric(b$cfg$offset_lognormal %||% 0)
  if (!nrow(d)) {
    d[, `:=`(tipo_cod = integer(), ruta = integer(), s_log = numeric(), eta = numeric(), n_ef = numeric())]
    return(d)
  }
  no_admitidos <- setdiff(unique(d$tipo_dato), .DL_TIPOS_DATO$tipo)
  if (length(no_admitidos))
    .dl_stop("el ajuste no admite datos locales de tipo_dato %s (admite %s)",
             paste(no_admitidos, collapse = ", "), paste(.DL_TIPOS_DATO$tipo, collapse = ", "))
  d[, tipo_cod := .DL_TIPOS_DATO$codigo[match(tipo_dato, .DL_TIPOS_DATO$tipo)]]
  d[, ruta := ifelse(!is.na(val) & !is.na(se), .DL_RUTA_LOGNORMAL, .DL_RUTA_CONTEOS)]
  if (any(d$ruta == .DL_RUTA_LOGNORMAL & d$val <= 0 & eta <= 0))
    .dl_stop(paste0("hay datos locales con val <= 0: la log-normal necesita un offset_lognormal mayor que 0 en la ",
                    "configuraci\u00f3n"))
  d[, eta := eta]
  d[, s_log := ifelse(ruta == .DL_RUTA_LOGNORMAL, se / (val + eta), NA_real_)]
  d[, n_ef := ifelse(ruta == .DL_RUTA_CONTEOS, ifelse(is.na(n_efectivo), as.numeric(n), as.numeric(n_efectivo)),
                     NA_real_)]
  d
}

# Bandas del ancla de prevalencia de un sexo (las filas de prior_gbd con measure_id de prevalencia), ordenadas por
# edad: el orden de la correlación AR(1). L_ancla compara estas bandas con p; anchor.medidas debe ser prevalence (con
# otra medida, sus bandas se compararían con p y la cadena AR(1) cruzaría de una medida a otra).
.dl_ancla_sexo <- function(b, sexo) {
  medidas <- vapply(unlist(b$cfg$anchor$medidas), .dl_medida_id, integer(1))
  ancla <- b$prior_gbd[measure_id %in% medidas & sex_id == sexo]
  data.table::setorder(ancla, measure_id, age_start)
  ancla
}

# chol(R) de la correlación AR(1) entre n bandas: R_bc = rho^|b - c|, rho en [0, 1). Devuelve U triangular
# superior con R = U'U.
.dl_chol_ar1 <- function(n, rho) chol(rho^abs(outer(seq_len(n), seq_len(n), "-")))

# Prior de EMR en los nudos (R/emr.R): plano dentro del techo (emr_prior.tipo plano_cota) o informativo,
# log(csmr / prevalencia) del ancla por banda. list(mu_log, sd_log, cota = c(mínimo, máximo), plano).
.dl_ctx_emr <- function(b, sexo, nudos) {
  cfg <- b$cfg
  if (identical(cfg$emr_prior$tipo, "plano_cota")) .dl_emr_plano(nudos, cfg$emr_prior$cota)
  else .dl_emr_en_nudos(.dl_prior_emr_tabla(b$prior_gbd, cfg)[sex_id == sexo], nudos, cfg$emr_prior$cota)
}

# Los datos locales de cada tipo de .DL_TIPOS_DATO como vectores simples, separados una vez: la log-posterior los
# recorre en cada evaluación y subdividir la tabla en cada una costaría más que el resto de L_datos. Una entrada por
# fila de .DL_TIPOS_DATO (NULL si el tipo no tiene datos) con el integrando del tipo, los pesos de población de cada
# dato, sus columnas ruta, val, s_log, eta, x y n_ef, y familia_conteos («binomial» o «poisson»).
.dl_datos_por_tipo <- function(datos, pesos_datos) {
  lapply(seq_len(nrow(.DL_TIPOS_DATO)), function(tipo) {
    idx <- which(datos$tipo_cod == .DL_TIPOS_DATO$codigo[tipo])
    if (!length(idx)) return(NULL)
    list(integrando = .DL_TIPOS_DATO$integrando[tipo], pesos = pesos_datos[idx], ruta = datos$ruta[idx],
         val = datos$val[idx], s_log = datos$s_log[idx], eta = datos$eta[idx], x = datos$x[idx],
         n_ef = datos$n_ef[idx], familia_conteos = .DL_TIPOS_DATO$familia_conteos[tipo])
  })
}

# Todo lo que la log-posterior de un sexo necesita y no depende de theta, calculado una vez:
#   nudos, edad_inicio, edad_fin, nsub     edades de theta y mallas (edad_fin y nsub: .DL_EDAD_FIN y .DL_NSUB si la
#                                          configuración no los fija)
#   W                                      nudos -> malla h/2 (.dl_base_interp)
#   anual, edades_media                    edades enteras (edades_anual) y edades de la malla h/2
#   idx_anual_malla, idx_anual_media       posiciones de las edades enteras en la malla h y en la malla h/2
#   ancla = list(bandas, chol_R, lambda)   bandas del ancla, chol de su correlación AR(1) y peso lambda (power prior)
#   pesos_ancla, pesos_datos               pesos de población (la de la ubicación del ancla) de cada banda del ancla
#                                          y de cada dato
#   emr                                    prior de EMR en los nudos y su cota
#   r_media                                r en la malla h/2 (.dl_remision_por_edad)
#   sigma_suavidad                         escala del prior de suavidad
#   datos, datos_por_tipo                  datos locales (.dl_datos_verosimilitud) y los mismos separados por tipo
#                                          (.dl_datos_por_tipo)
# Gemela en C++: DlCtx, que .dl_ctx_cpp() (R/rcpp.R) llena desde este contexto.
.dl_ctx <- function(b, sexo) {
  cfg <- b$cfg
  nudos <- as.numeric(unlist(cfg$nudos_incidencia))
  edad_inicio <- as.integer(cfg$edad_inicio)
  edad_fin <- as.integer(cfg$edad_fin %||% .DL_EDAD_FIN); nsub <- as.integer(cfg$nsub %||% .DL_NSUB)
  anual <- seq(edad_inicio, edad_fin)
  edades_media <- .dl_edades_media(edad_inicio, edad_fin, nsub)
  ancla <- .dl_ancla_sexo(b, sexo)
  pobl <- b$poblacion[location_id == b$loc_ancla & sex_id == sexo]
  datos <- .dl_datos_verosimilitud(b, sexo)
  pesos <- function(tabla) lapply(seq_len(nrow(tabla)), function(j)
    .dl_pesos_intervalo(anual, tabla$age_start[j], tabla$age_end[j], pobl, b$bandas_pobl))
  ctx <- list(nudos = nudos, edad_inicio = edad_inicio, edad_fin = edad_fin, nsub = nsub,
              W = .dl_base_interp(nudos, edades_media), anual = anual, edades_media = edades_media,
              idx_anual_malla = .dl_idx_anual_malla(edad_fin - edad_inicio, nsub),
              idx_anual_media = .dl_idx_anual_media(length(edades_media), nsub),
              ancla = list(bandas = ancla, chol_R = .dl_chol_ar1(nrow(ancla), cfg$anchor$rho_edad),
                           lambda = cfg$anchor$lambda),
              pesos_ancla = pesos(ancla),
              emr = .dl_ctx_emr(b, sexo, nudos),
              r_media = .dl_remision_por_edad(cfg, edades_media),
              sigma_suavidad = cfg$sigma_suavidad, datos = datos)
  ctx$pesos_datos <- pesos(datos)
  ctx$datos_por_tipo <- .dl_datos_por_tipo(datos, ctx$pesos_datos)
  ctx
}

# ---- Solución de la EDO en un contexto ----

# theta -> log i, log f en la malla h/2:  log i = W theta_i,  log f = W theta_f  (W de .dl_base_interp).
# Devuelve list(log_i, log_f). Gemela en C++: el producto por W de tasas_media().
.dl_log_tasas_media <- function(theta, ctx) {
  n_nudos <- length(ctx$nudos)
  list(log_i = as.vector(ctx$W %*% theta[seq_len(n_nudos)]),
       log_f = as.vector(ctx$W %*% theta[n_nudos + seq_len(n_nudos)]))
}

# log i, log f en la malla h/2 -> p, i, f en las edades enteras, con la malla y r del contexto:
#   con `techo_f`: log f <- min(log f, log techo_f) en cada punto; truncado = 1 si algún punto lo superaba
#   i = exp(log i), f = exp(log f); p en la malla h con `solver` desde p(edad_inicio) = 0
#   `solver`: dl_edo_resolver() o su gemela en C++ (.dl_rcpp()$edo), llamadas por posición
#   (i_media, f_media, r_media, p0, nsub).
# La cascada (R/cascada.R) le pasa log i y log f ya desplazados por departamento. Devuelve list(p, i, f, truncado).
# Gemela en C++: el exp de tasas_media(), resolver_edo() y anuales() (sin techo).
.dl_edo_log_media <- function(log_i_media, log_f_media, ctx, solver = dl_edo_resolver, techo_f = NULL) {
  truncado <- 0L
  if (!is.null(techo_f) && any(log_f_media > log(techo_f))) {
    truncado <- 1L; log_f_media <- pmin(log_f_media, log(techo_f))
  }
  i_media <- exp(log_i_media); f_media <- exp(log_f_media)
  p_malla <- solver(i_media, f_media, ctx$r_media, 0, ctx$nsub)
  anuales <- .dl_a_edades_enteras(p_malla, i_media, f_media, ctx$idx_anual_malla, ctx$idx_anual_media)
  c(anuales, list(truncado = truncado))
}

# theta -> p, i, f en las edades enteras con la malla, W y r del contexto (lo que dl_edo() hace sin contexto).
# Devuelve list(p, i, f, truncado = 0). Gemela en C++: tasas_media(), resolver_edo() y anuales() al comienzo de
# dl_lp_core().
.dl_edo_ctx <- function(theta, ctx, solver = dl_edo_resolver) {
  log_media <- .dl_log_tasas_media(theta, ctx)
  .dl_edo_log_media(log_media$log_i, log_media$log_f, ctx, solver)
}

# ---- Términos de la log-posterior ----

# Truncamiento del prior de EMR: TRUE si algún f = exp(log f) de los nudos cae fuera de [cota[1], cota[2]].
# Gemela en C++: f_fuera_de_cota().
.dl_f_fuera_de_cota <- function(log_f_nudos, cota) {
  f_nudos <- exp(log_f_nudos)
  any(f_nudos < cota[1] | f_nudos > cota[2])
}

# L_suavidad = sum_k log N(D2_k; 0, sigma),  D2_k = (log i_k+2 - log i_k+1) - (log i_k+1 - log i_k)
#   Segundas diferencias de log i en los nudos, en su orden y sin dividir por la distancia entre nudos.
#   sigma = sigma_suavidad de la configuración.
# Gemela en C++: lp_suavidad().
.dl_lp_suavidad <- function(log_i_nudos, sigma) sum(stats::dnorm(diff(diff(log_i_nudos)), 0, sigma, log = TRUE))

# L_emr = sum_k log N(log f_k; mu_log_k, sd_log_k): prior log-normal de la EMR en cada nudo. Con el prior plano
#   (emr$plano) vale 0 y solo queda el truncamiento (.dl_f_fuera_de_cota).
# Gemela en C++: lp_prior_emr().
.dl_lp_prior_emr <- function(log_f_nudos, emr)
  if (emr$plano) 0 else sum(stats::dnorm(log_f_nudos, emr$mu_log, emr$sd_log, log = TRUE))

# log MVN(z; 0, Sigma / lambda),  Sigma = diag(sigma_log) R diag(sigma_log),  R = U'U  (U = chol_R):
#   = -1/2 [lambda z' Sigma^-1 z + log det Sigma - n log lambda + n log(2 pi)]
#   z' Sigma^-1 z = |u|^2  con  U' u = z / sigma_log  (sustitución hacia adelante)
#   log det Sigma = 2 sum log diag(U) + 2 sum log sigma_log
#   lambda en (0, 1] (power prior) multiplica la precisión del ancla.
# Gemela en C++: lp_mvn_ar1().
.dl_lp_mvn_ar1 <- function(z, sigma_log, chol_R, lambda) {
  n <- length(z)
  u <- backsolve(chol_R, z / sigma_log, transpose = TRUE)
  quad <- sum(u * u)
  logdet <- 2 * sum(log(diag(chol_R))) + 2 * sum(log(sigma_log))
  -0.5 * (lambda * quad + logdet - n * log(lambda) + n * log(2 * pi))
}

# L_ancla = log MVN(z; 0, Sigma / lambda),  z_b = log q_b - log val_b: q_b es la prevalencia del modelo en la banda
#   b del ancla, val_b la de referencia y sigma_log_b su error en escala log.
# Gemela en C++: lp_ancla().
.dl_lp_ancla <- function(q_ancla, ancla)
  .dl_lp_mvn_ar1(log(q_ancla) - log(ancla$bandas$val), ancla$bandas$sigma_log, ancla$chol_R, ancla$lambda)

# l_d de los datos (vectorizadas), con q la cantidad del modelo en la banda de cada dato:
#   log-normal con offset eta (el dato trae val y se): log N(log(q + eta); log(val + eta), s_log).
#   Gemela en C++: lp_lognormal().
.dl_lp_lognormal <- function(q, val, s_log, eta) stats::dnorm(log(q + eta), log(val + eta), s_log, log = TRUE)
#   binomial (x casos de n): x log q + (n - x) log(1 - q) = dbinom(x, n, q, log = TRUE) - lchoose(n, x).
#   Gemela en C++: lp_binomial().
.dl_lp_binomial <- function(q, x, n) x * log(q) + (n - x) * log1p(-q)
#   Poisson (x eventos en n personas-año): x log(n q) - n q = dpois(x, n q, log = TRUE) + lgamma(x + 1).
#   Gemela en C++: lp_poisson().
.dl_lp_poisson <- function(q, x, n) x * log(n * q) - n * q

# L_datos por tipo de .DL_TIPOS_DATO (R/esquema.R): q_d = promedio por población del integrando anual del tipo
#   (p, i (1 - p) o p f; .dl_integrando) en la banda [inicio, fin) de cada dato, y la suma de l_d de sus datos
#   (log-normal en la ruta .DL_RUTA_LOGNORMAL; binomial o Poisson, según el tipo, en la de conteos). Algún q no
#   finito o <= 0 deja ese tipo en -Inf. q >= 1 en la binomial (p > 1 por inestabilidad de RK4 con i h grande) da
#   NaN: sin control. Devuelve c(datos_prev_estudio, datos_incidencia, datos_csmr). `tipo` es la fila de
#   .DL_TIPOS_DATO, que coincide con su código; sus datos están en ctx$datos_por_tipo[[tipo]].
# Gemela en C++: lp_datos().
.dl_lp_datos <- function(sol, ctx) {
  out <- stats::setNames(numeric(nrow(.DL_TIPOS_DATO)), paste0("datos_", .DL_TIPOS_DATO$tipo))
  for (tipo in seq_along(ctx$datos_por_tipo)) {
    d <- ctx$datos_por_tipo[[tipo]]
    if (is.null(d)) next
    q <- .dl_q_intervalos(.dl_integrando(sol, d$integrando), d$pesos)
    if (any(!is.finite(q)) || any(q <= 0)) { out[tipo] <- -Inf; next }
    l_d <- ifelse(d$ruta == .DL_RUTA_LOGNORMAL, .dl_lp_lognormal(q, d$val, d$s_log, d$eta),
                  switch(d$familia_conteos, binomial = .dl_lp_binomial(q, d$x, d$n_ef),
                         poisson = .dl_lp_poisson(q, d$x, d$n_ef)))
    out[tipo] <- sum(l_d)
  }
  out
}

# ---- Log-posterior ----

# log posterior(theta) por términos, con los nombres de .DL_LP_NOMBRES:
#   total = suavidad + emr + ancla + datos (en ese orden); datos = suma de los términos por tipo.
#   Algún f de los nudos fuera de la cota => todos -Inf (truncamiento del prior de EMR).
#   Algún q del ancla no finito o <= 0 (p no finito por desborde de exp con priors casi planos) => ancla, datos,
#   total y datos por tipo -Inf: el candidato se rechaza sin error. q >= 1 en la binomial (p > 1 por inestabilidad
#   de RK4 con i h grande) da NaN en datos y en total: sin control (ver .dl_lp_datos).
# Gemela en C++: dl_lp_core().
.dl_lp_componentes <- function(theta, ctx) {
  n_nudos <- length(ctx$nudos)
  log_i_nudos <- theta[seq_len(n_nudos)]; log_f_nudos <- theta[n_nudos + seq_len(n_nudos)]
  if (.dl_f_fuera_de_cota(log_f_nudos, ctx$emr$cota))
    return(stats::setNames(rep(-Inf, length(.DL_LP_NOMBRES)), .DL_LP_NOMBRES))
  sol <- .dl_edo_ctx(theta, ctx)
  lp_suav <- .dl_lp_suavidad(log_i_nudos, ctx$sigma_suavidad)
  lp_emr <- .dl_lp_prior_emr(log_f_nudos, ctx$emr)
  q_ancla <- .dl_q_intervalos(sol$p, ctx$pesos_ancla)
  if (any(!is.finite(q_ancla)) || any(q_ancla <= 0))
    return(stats::setNames(c(lp_suav, lp_emr, rep(-Inf, length(.DL_LP_NOMBRES) - 2L)), .DL_LP_NOMBRES))
  lp_ancla <- .dl_lp_ancla(q_ancla, ctx$ancla)
  por_tipo <- .dl_lp_datos(sol, ctx)
  lp_datos <- sum(por_tipo)
  lp_total <- lp_suav + lp_emr + lp_ancla + lp_datos
  c(suavidad = lp_suav, emr = lp_emr, ancla = lp_ancla, datos = lp_datos, total = lp_total, por_tipo)
}

# log posterior(theta) total: lo que el muestreador evalúa (motor "mh"). Gemela en C++: dl_lp_total_cpp().
.dl_log_post <- function(theta, ctx) unname(.dl_lp_componentes(theta, ctx)["total"])

# Punto inicial de las cadenas: log i = .DL_LOG_I_INICIAL en todos los nudos y log f = la media del prior de EMR,
# pero al menos .DL_MARGEN_LOG_F_INICIAL bajo log(techo). Nombres logi_<nudo> y logf_<nudo>.
.dl_theta_inicial <- function(ctx) {
  log_f0 <- pmin(ctx$emr$mu_log, log(ctx$emr$cota[2]) - .DL_MARGEN_LOG_F_INICIAL)
  theta0 <- c(rep(.DL_LOG_I_INICIAL, length(ctx$nudos)), log_f0)
  names(theta0) <- c(paste0("logi_", ctx$nudos), paste0("logf_", ctx$nudos))
  theta0
}
