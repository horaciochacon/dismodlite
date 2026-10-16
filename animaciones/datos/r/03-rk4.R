# Escena 03, cómo se resuelve la ecuación (RK4): las tasas de la guía «El modelo» (sección 2, valores inventados) en
# la malla h/2, la prevalencia que da dl_edo_resolver() en la malla h, las cuatro etapas de un paso (de 60 a 60.2
# años), la precisión frente a la solución exacta (el chunk «precision» de la sección 3) y una ilustración de la
# estabilidad (remisión alta, con h = 0.2 y con h = 0.1).
exportar <- function(ctx) {
  nsub <- dismodlite:::.DL_NSUB
  h <- 1 / nsub
  a0 <- 30
  aF <- dismodlite:::.DL_EDAD_FIN

  # Las tasas en la malla h/2, como las arma dl_edo() por dentro (W de los nudos a la malla, en escala log).
  nudos <- c(30, 40, 50, 60, 70, 80, 95)
  log_i <- log(c(0.5, 0.8, 1.5, 2.5, 4, 6, 7) / 1000)
  log_f <- log(c(10, 12, 15, 20, 30, 50, 90) / 1000)
  edades_media <- dismodlite:::.dl_edades_media(a0, aF, nsub)
  W <- dismodlite:::.dl_base_interp(nudos, edades_media)
  i_media <- exp(as.vector(W %*% log_i))
  f_media <- exp(as.vector(W %*% log_f))
  p_malla <- dl_edo_resolver(i_media, f_media, remision = 0, p0 = 0, nsub = nsub)
  s <- dl_edo(log_i, log_f, nudos, edad_inicio = a0)
  idx_anual <- dismodlite:::.dl_idx_anual_malla(aF - a0, nsub)
  stopifnot(identical(s$p, p_malla[idx_anual]), identical(s$i, i_media[dismodlite:::.dl_idx_anual_media(length(i_media), nsub)]))

  # Un paso: el n = 151, de a = 60 a a + h = 60.2; usa los puntos 2n - 1, 2n y 2n + 1 de la malla h/2 (base 1).
  # Las etapas con las mismas expresiones, en el mismo orden, que el bucle de dl_edo_resolver().
  n <- as.integer(round((60 - a0) / h)) + 1L
  j <- 2L * n - 1L
  p1 <- p_malla[n]
  k1 <- i_media[j] * (1 - p1) - 0 * p1 - f_media[j] * p1 * (1 - p1)
  p2 <- p1 + h / 2 * k1
  k2 <- i_media[j + 1L] * (1 - p2) - 0 * p2 - f_media[j + 1L] * p2 * (1 - p2)
  p3 <- p1 + h / 2 * k2
  k3 <- i_media[j + 1L] * (1 - p3) - 0 * p3 - f_media[j + 1L] * p3 * (1 - p3)
  p4 <- p1 + h * k3
  k4 <- i_media[j + 2L] * (1 - p4) - 0 * p4 - f_media[j + 2L] * p4 * (1 - p4)
  p_siguiente <- p1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  stopifnot(identical(p_siguiente, p_malla[n + 1L]))

  # Precisión: el chunk «precision» de la guía (sección 3), tal cual.
  i <- 0.002; f <- 0.05
  solucion <- dl_edo(rep(log(i), 2), rep(log(f), 2), nudos = c(30, 95), edad_inicio = 30)
  t <- solucion$edades - 30
  E <- exp((i - f) * t)
  exacta <- i * (E - 1) / (i * E - f)
  error_max <- max(abs(solucion$p - exacta) / pmax(exacta, 1e-12))
  t_fino <- seq(0, aF - a0, by = 0.25)
  E_fino <- exp((i - f) * t_fino)

  # Estabilidad, una ilustración con valores inventados: i = 0.05, r = 15 (una causa aguda), f = 0, desde p = 0,
  # durante 1.2 años. Con f = 0 la ecuación es lineal, dp/da = i - (i + r) p, y su solución exacta es
  # p = i / (i + r) (1 - exp(-(i + r) t)). La tasa de decaimiento es k = i + r + f (1 - 2p) = i + r.
  ei <- 0.05; er <- 15; ef <- 0; dur <- 1.2
  resolver <- function(ns) {
    m <- 2L * as.integer(round(dur * ns)) + 1L
    dl_edo_resolver(rep(ei, m), rep(ef, m), remision = er, p0 = 0, nsub = ns)
  }
  ek <- ei + er + ef
  et_fino <- seq(0, dur, by = 0.01)
  # Lo que dice dl_insumos() (la comprobación que aplica, .dl_chequear_paso_rk4) con esta remisión y sin techo de
  # EMR (f = 0): con nsub = 5 se detiene; con nsub = 10 pasa y devuelve (r + techo) / nsub.
  cfg5 <- list(remision = list(valor = er), nsub = 5L)
  mensaje <- tryCatch({ dismodlite:::.dl_chequear_paso_rk4(cfg5, 0); NA_character_ },
                      error = function(e) conditionMessage(e))
  kh10 <- dismodlite:::.dl_chequear_paso_rk4(list(remision = list(valor = er), nsub = 10L), 0)
  # El límite de estabilidad de RK4 sobre el eje real negativo: la raíz de R(z) = 1 + z + z^2/2 + z^3/6 + z^4/24 = 1,
  # z < 0 (para y' = -k y, cada paso multiplica por R(-k h)).
  limite_rk4 <- -uniroot(function(z) z + z^2 / 2 + z^3 / 6 + z^4 / 24, c(-3, -2.5), tol = 1e-14)$root

  # jsonlite escribe 15 cifras significativas: para comparar bit a bit (auditoría), las mallas y el paso van también
  # con 17 cifras, como texto, que JavaScript lee con Number() al mismo double.
  exacto <- function(x) sprintf("%.17g", x)
  list(
    exacto = list(i_media = exacto(i_media), f_media = exacto(f_media), p_malla = exacto(p_malla),
                  p1 = exacto(p1), k = exacto(c(k1, k2, k3, k4)), p_pred = exacto(c(p2, p3, p4)),
                  p_paquete = exacto(p_malla[n + 1L]), error_max = exacto(error_max)),
    fuente = "Valores inventados de la guía «El modelo» (secciones 2 y 3), resueltos con dl_edo_resolver()",
    nudos = nudos, log_i = log_i, log_f = log_f, edad_inicio = a0, edad_fin = aF, nsub = nsub, h = h,
    remision = 0,
    edades_media = edades_media, i_media = i_media, f_media = f_media, p_malla = p_malla,
    edades = s$edades, p = s$p,
    paso = list(n = n, j = j, a = edades_media[j], a_medio = edades_media[j + 1L], a_fin = edades_media[j + 2L],
                p1 = p1, p2 = p2, p3 = p3, p4 = p4, k1 = k1, k2 = k2, k3 = k3, k4 = k4,
                p_siguiente = p_siguiente, p_paquete = p_malla[n + 1L]),
    precision = list(i = i, f = f, edades = solucion$edades, p = solucion$p, exacta = exacta,
                     error_max = error_max, t_fino = t_fino, exacta_fina = i * (E_fino - 1) / (i * E_fino - f)),
    estabilidad = list(i = ei, r = er, f = ef, duracion = dur, k = ek,
                       p_h5 = resolver(5L), p_h10 = resolver(10L), kh5 = ek / 5, kh10 = ek / 10,
                       t_fino = et_fino, exacta_fina = ei / ek * (1 - exp(-ek * et_fino)),
                       mensaje = mensaje, kh10_insumos = kh10,
                       limite_paquete = dismodlite:::.DL_RK4_LIMITE_PASO, limite_rk4 = limite_rk4)
  )
}
