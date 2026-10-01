# Transformaciones de covariables y simulaciones de sus coeficientes (betas), compartidas por dl_cascada (efecto de
# los proxies departamentales) y dl_avd (proporciones de severidad que dependen de una covariable).
#
# Notación: ver R/edo.R. Una covariable x entra al modelo en la escala de su transformación T:
#   log     T(x) = log(x)
#   logit   T(x) = log(x / (1 - x))
#   lineal  T(x) = x
# Un intervalo del 95 % [lower, upper] se lee como el de una normal en esa escala:
#   sd_T = (T(upper) - T(lower)) / 3.92.
# Una beta en modo «informativa» se simula, para s = 1, ..., n, como (N(media, desviación estándar), R/edo.R)
#   beta_s ~ N(beta, sd_beta),   sd_beta = (beta_upper - beta_lower) / 3.92,
# y una beta en modo «fija» vale beta en todas las simulaciones.

# ---- Constantes ------------------------------------------------------------------------------------------------

# Ancho de un intervalo del 95 % de una normal, en desviaciones estándar: 2 * 1.96 = 3.92, con 1.96 ~ qnorm(0.975).
.DL_ANCHO_IC95_EN_SD <- 3.92

# Las betas se sortean con semilla + 1 (dl_cascada y dl_avd), un flujo aleatorio distinto del de los otros sorteos de
# la función, que usan la semilla tal cual. Es parte del orden fijo del generador: cambiarlo cambia los resultados.
.DL_SALTO_SEMILLA_BETAS <- 1L

# ---- Funciones ------------------------------------------------------------------------------------------------

# T(x) para tipo = "log", "logit" o "lineal"; vectorizada en x. logit: .dl_logit() (R/avd.R).
.dl_transformar <- function(x, tipo) switch(tipo, log = log(x), logit = .dl_logit(x), lineal = x,
                                            .dl_stop(paste0("transformaci\u00f3n desconocida \u00ab%s\u00bb (se ",
                                                            "admiten log, logit y lineal)"), tipo))

# sd_T de una covariable desde su intervalo del 95 %: (T(upper) - T(lower)) / 3.92. Sin intervalo (NA) o con un
# intervalo degenerado (upper <= lower), la covariable se toma como conocida: sd_T = 0. Argumentos escalares.
.dl_sd_transformada <- function(lower, upper, tipo) {
  if (anyNA(c(lower, upper)) || upper <= lower) return(0)
  (.dl_transformar(upper, tipo) - .dl_transformar(lower, tipo)) / .DL_ANCHO_IC95_EN_SD
}

# Matriz n x n_cov de simulaciones de las betas: fila = simulación, columna = covariate_name_short, una columna por
# fila de `betas` (en su orden; n_cov = nrow(betas)):
#   informativa:        beta_s ~ N(beta, sd_beta),  sd_beta = (beta_upper - beta_lower) / 3.92;
#   fija (otro modo):   beta_s = beta (rnorm con sd 0 devuelve la media sin consumir números aleatorios).
# Reproducibilidad: fija la semilla (set.seed, con efecto sobre el generador global) y consume el flujo aleatorio
# columna a columna, en el orden de las filas de `betas`: cambiar ese orden cambia las simulaciones. matrix() da la
# forma n x n_cov también con n = 1, donde vapply devolvería un vector.
.dl_draws_betas <- function(betas, n, seed) {
  set.seed(seed)
  sims <- vapply(seq_len(nrow(betas)), function(k) {
    sd_beta <- if (betas$modo[k] == "informativa")
      (betas$beta_upper[k] - betas$beta_lower[k]) / .DL_ANCHO_IC95_EN_SD else 0
    stats::rnorm(n, betas$beta[k], sd_beta)
  }, numeric(n))
  sims <- matrix(sims, nrow = n)
  colnames(sims) <- betas$covariate_name_short
  sims
}
