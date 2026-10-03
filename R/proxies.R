# Calibración de proxies subnacionales: de un indicador de encuesta por ubicación y edición a las filas subnacionales
# de la tabla covariables, cerradas en el valor nacional (ver ?dl_calibrar_proxies).
#
# Notación
#   d          ubicación subnacional; una serie es (covariable, d, sexo, banda de edad)
#   t          año de una edición de la encuesta; a, el año que se estima
#   p_{d,t}    valor crudo del indicador; se_{d,t} su error estándar
#   N_{d,t}    población de d en el sexo y la banda, del año de `poblacion` más cercano a t
#   p̄_t        Σ_d N_{d,t} p_{d,t} / Σ_d N_{d,t}
#   g_{d,t}    gradiente: log(p_{d,t} / p̄_t) (cociente) o p_{d,t} − p̄_t (diferencia); se_g su error
#   q          varianza por año del paseo aleatorio del gradiente
#   ĝ_d, S_d   gradiente suavizado en el año a y su varianza
#   w_d        población de d en el año a, normalizada en el sexo y la banda
#   X          valor nacional de la covariable; X_d el valor calibrado de d
#
# Modelo temporal (nivel local), una serie a la vez:
#   g_t = g_{t-1} + η_t,  η_t ~ N(0, q Δt)          obs_t ~ N(g_t, se_g,t^2)
# con prior difuso: el filtro empieza en la primera observación con media obs_1 y varianza se_g,1^2. q se estima
# por covariable maximizando la suma, sobre sus series, de la log-verosimilitud de predicción de las observaciones
# 2..n de cada serie (la primera fija el prior difuso).
#
# Cierre, por sexo y banda (exacto por construcción: Σ_d w_d X_d = X):
#   cociente:    X_d = X exp(ĝ_d) / Σ_d w_d exp(ĝ_d)     se(X_d) = X_d sqrt(S_d)
#   diferencia:  X_d = X + ĝ_d − Σ_d w_d ĝ_d             se(X_d) = sqrt(S_d)
# Aproximaciones declaradas: se(X_d) no incluye la incertidumbre de la normalización ni la de X; se/p es la
# aproximación delta de la escala log.

# ---- Constantes ----

# Intervalo de búsqueda de q (varianza por año del gradiente): de prácticamente constante a un gradiente que cambia
# más de 3 unidades de log por año (más de lo que ningún proxy razonable cambia).
.DL_Q_LIMITES <- c(1e-8, 10)

# ---- Gradiente de una edición ----

# g y se_g de las ubicaciones de una edición, sexo y banda: valores `p`, errores `se`, poblaciones `N`.
.dl_gradiente_edicion <- function(p, se, N, transformacion) {
  .dl_validar_transformacion(transformacion)
  if (transformacion == "cociente" && any(p <= 0))
    .dl_stop("la transformaci\u00f3n \u00abcociente\u00bb necesita valores positivos y hay valores iguales o menores que 0; usa la \u00abdiferencia\u00bb")
  pbar <- sum(N * p) / sum(N)
  if (transformacion == "cociente") list(g = log(p / pbar), se_g = se / p)
  else list(g = p - pbar, se_g = se)
}

# Error si `transformacion` no es una de las dos que conoce el cierre y el gradiente.
.dl_validar_transformacion <- function(transformacion) {
  if (!length(transformacion) || !transformacion[1L] %in% c("cociente", "diferencia"))
    .dl_stop("transformaci\u00f3n desconocida \u00ab%s\u00bb (se esperaba \u00abcociente\u00bb o \u00abdiferencia\u00bb)",
             paste(transformacion, collapse = ", "))
}

# ---- Filtro de Kalman y suavizador RTS del nivel local ----

# Filtro sobre los tiempos ordenados `t` (observaciones `y` con error `se`) más el tiempo `t_obj` sin observación si no
# está entre ellos; empieza en el primer tiempo observado (prior difuso). Devuelve, en la malla `tt`, la media y
# varianza filtradas (m, P), las de predicción (m_pred, P_pred) y la log-verosimilitud de predicción de las
# observaciones 2..n. Los tiempos `t` pueden venir en cualquier orden pero no repetidos (una edición por año).
.dl_kalman_nivel_local <- function(t, y, se, q, t_obj) {
  if (anyDuplicated(t))
    .dl_stop("una serie tiene a\u00f1os repetidos: %s", paste(sort(unique(t[duplicated(t)])), collapse = ", "))
  o <- order(t); t <- t[o]; y <- y[o]; se <- se[o]
  tt <- sort(unique(c(t, t_obj[t_obj >= t[1L]])))
  k_obs <- match(tt, t)
  n <- length(tt)
  m <- P <- m_pred <- P_pred <- numeric(n)
  m[1L] <- m_pred[1L] <- y[1L]; P[1L] <- P_pred[1L] <- se[1L]^2
  loglik <- 0
  for (k in seq_len(n)[-1L]) {
    m_pred[k] <- m[k - 1L]
    P_pred[k] <- P[k - 1L] + q * (tt[k] - tt[k - 1L])
    j <- k_obs[k]
    if (is.na(j)) { m[k] <- m_pred[k]; P[k] <- P_pred[k]; next }
    var_innov <- P_pred[k] + se[j]^2                # varianza de la innovación
    v <- y[j] - m_pred[k]                           # innovación
    loglik <- loglik - 0.5 * (log(2 * pi * var_innov) + v^2 / var_innov)
    K <- P_pred[k] / var_innov                      # ganancia
    m[k] <- m_pred[k] + K * v
    P[k] <- (1 - K) * P_pred[k]
  }
  list(tt = tt, m = m, P = P, m_pred = m_pred, P_pred = P_pred, loglik = loglik)
}

# Suavizador RTS: la media y la varianza del gradiente en `t_obj` (un solo año) dadas todas las observaciones de la
# serie. Antes de la primera observación, el paseo aleatorio es reversible: la estimación del primer tiempo con q por
# cada año de más.
.dl_suavizar_serie <- function(t, y, se, q, t_obj) {
  f <- .dl_kalman_nivel_local(t, y, se, q, t_obj)
  n <- length(f$tt)
  ms <- f$m; Ps <- f$P
  for (k in rev(seq_len(n - 1L))) {
    C <- f$P[k] / f$P_pred[k + 1L]
    ms[k] <- f$m[k] + C * (ms[k + 1L] - f$m_pred[k + 1L])
    Ps[k] <- f$P[k] + C^2 * (Ps[k + 1L] - f$P_pred[k + 1L])
  }
  if (t_obj < f$tt[1L]) return(list(g = ms[1L], S = Ps[1L] + q * (f$tt[1L] - t_obj)))
  k <- match(t_obj, f$tt)
  list(g = ms[k], S = Ps[k])
}

# ---- Estimación de q ----

# q de una covariable: maximiza la suma de las log-verosimilitudes de predicción de sus `series` (lista de
# list(t, y, se)); NA sin ninguna serie con dos o más ediciones (no hay información sobre la deriva).
.dl_estimar_q <- function(series) {
  utiles <- Filter(function(s) length(s$t) >= 2L, series)
  if (!length(utiles)) return(NA_real_)
  menos_loglik <- function(log_q) -sum(vapply(utiles, function(s)
    .dl_kalman_nivel_local(s$t, s$y, s$se, exp(log_q), max(s$t))$loglik, 0))
  exp(stats::optimize(menos_loglik, log(.DL_Q_LIMITES))$minimum)
}

# ---- Cierre en el valor nacional ----

# X_d y su error (cabecera) desde ĝ (`g`), S, los pesos de población `w` (se normalizan) y el valor nacional X.
.dl_cerrar_proxies <- function(g, S, w, X, transformacion) {
  .dl_validar_transformacion(transformacion)
  if (!sum(w) > 0) .dl_stop("los pesos de poblaci\u00f3n del cierre suman 0 (o no son v\u00e1lidos); no se puede normalizar")
  w <- w / sum(w)
  if (transformacion == "cociente") {
    valor <- X * exp(g) / sum(w * exp(g))
    list(valor = valor, se = valor * sqrt(S))
  } else {
    list(valor = X + g - sum(w * g), se = sqrt(S))
  }
}
