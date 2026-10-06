# Modelo enfermedad-muerte (DisMod-lite)
#
# Compartimentos, por edad a (años):
#   S(a)  personas sin la enfermedad        C(a)  personas con la enfermedad
# Tasas (por persona-año):
#   i(a)  incidencia en S      r(a)  remisión de C a S
#   f(a)  mortalidad en exceso en C (EMR)    m(a)  mortalidad por otras causas
#   dS/da = -(i + m) S + r C
#   dC/da =  i S - (r + m + f) C
# Con p = C / (S + C), m se cancela:
#   dp/da = i (1 - p) - r p - f p (1 - p)
# Mallas: anual (edades enteras), malla de paso h = 1/nsub (RK4) y malla media de paso h/2
# (puntos intermedios de RK4). log i y log f se interpolan linealmente entre nudos (matriz W) y
# son constantes fuera de ellos. theta = (log i en los nudos, log f en los nudos); con la mortalidad en exceso fija
# en cero (emr_prior.tipo cero), theta = (log i en los nudos) y f(a) = 0.
#   p(a) = C / (S + C) en [0, 1) (proporción). Condición inicial p(a0) = p0 en a0 = edad_inicio; el ajuste usa
#        p0 = 0.
#   r(a) no se estima: sale de la configuración (remision.valor y remision.por_edad), constante por tramos
#        [inicio, fin).
#   m(a) no hace falta: se cancela en dp/da.
# Log-posterior (R/verosimilitud.R):
#   sigma   escala del prior de suavidad de log i (sigma_suavidad)
#   lambda  peso del ancla (power prior), en (0, 1]
#   rho     correlación AR(1) entre bandas del ancla, en [0, 1)
#   eta     offset de la log-normal de los datos con val <= 0 (offset_lognormal)
#   Desviaciones estándar en escala log (ninguna es sigma): sigma_log (ancla), sd_log (prior de EMR), s_log (datos).
#   N(x; mu, s) es la normal con media mu y desviación estándar s (nunca la varianza); MVN(z; 0, Sigma) lleva la
#   matriz de covarianza.
# Índices: k nudo; j punto de la malla h/2; n paso de RK4 (en el código, `paso`); b banda de edad; d departamento
# o dato; e estado de salud; k también simulación en R/cascada.R y R/avd.R (columna draw) y ventana de adaptación
# en R/muestreador.R.
#
# Nombres en el código (R y C++), con a0 = edad_inicio, aF = edad_fin, n_anios = aF - a0 y h = 1/nsub:
#   *_anual  en las edades enteras a0, a0 + 1, ..., aF                    n_anios + 1 puntos
#   *_malla  en la malla h:   a0, a0 + h, ..., aF                          n_anios nsub + 1 puntos
#   *_media  en la malla h/2: a0, a0 + h/2, ..., aF                        2 n_anios nsub + 1 puntos
#            (medio paso: en estos nombres «media» nunca es un promedio)
#   *_nudos  en los nudos de theta
# El punto n de la malla h (n = 1, 2, ...) es el punto 2n - 1 de la malla h/2; el paso n de RK4, de la edad a a la
# a + h, usa los puntos 2n - 1 (a), 2n (a + h/2) y 2n + 1 (a + h) de la malla h/2.
#
# Gemelo en C++: inst/cpp/dl_core.cpp (motor "rcpp"). Repite dp/da y el paso de RK4 con las mismas operaciones en
# el mismo orden (compilado sin FMA, el solver es idéntico bit a bit); la log-posterior, con las mismas fórmulas
# (coincide a 1e-9; ver la cabecera de dl_core.cpp). Cada función cita a su gemela; cambiar una exige cambiar la
# otra (test-rcpp.R las compara).

# ---- Constantes ----

# Subpasos de RK4 por año cuando la configuración no fija `nsub` (h = 1/5 = 0,2 años). RK4 es de orden 4: con
# h = 0,2 el error relativo en p frente a la solución cerrada es menor que 1e-6 aun con f = 0,3 por persona-año
# (test-edo.R), muy por debajo de la incertidumbre del ancla.
.DL_NSUB <- 5L

# Estabilidad de RK4. Linealizada, la ecuación de p decae con la tasa k = i + r + f (1 - 2p), y RK4 con paso h es
# estable si k h < 2.785 (su límite para y' = -k y). Una remisión alta (causas agudas, del orden de 12 por año) con el
# paso por defecto h = 1/5 y una simulación con EMR alta superaría el límite y daría prevalencias negativas a edades
# altas. dl_insumos() aplica la cota a remisión + techo de EMR (i es despreciable frente a r en las causas agudas; en
# las crónicas r = 0 y f es mucho menor), con margen: 2.5 en lugar de 2.785.
.DL_RK4_LIMITE_PASO <- 2.5

# Última edad de la malla (años) cuando la configuración no fija `edad_fin`: la banda abierta de 95 años y más
# queda representada por las edades enteras 95 a 99, cinco edades como las bandas quinquenales.
.DL_EDAD_FIN <- 99L

# ---- Lado derecho de la EDO ----

# dp/da = i (1 - p) - r p - f p (1 - p)
#   i, r, f  tasas en la edad a (por persona-año); p  prevalencia en a (proporción en [0, 1)).
#   Devuelve dp/da (por año). Vectorizada: i, r, f y p pueden ser vectores de una misma malla.
# Es la única definición del lado derecho en R: dl_edo_resolver() escribe en línea esta misma expresión en las
# cuatro etapas de RK4 (llamar a una función dentro del bucle lo haría varias veces más lento). La prueban
# test-edo.R y la documentación. Gemela en C++: dp_da() en inst/cpp/dl_core.cpp.
.dl_dp_da <- function(i, r, f, p) i * (1 - p) - r * p - f * p * (1 - p)

# ---- Mallas ----

# Edades de la malla h/2: a0, a0 + h/2, ..., aF (años).
.dl_edades_media <- function(edad_inicio, edad_fin, nsub) seq(edad_inicio, edad_fin, by = 1 / (2 * nsub))

# Posiciones de las edades enteras a0, a0 + 1, ..., aF en la malla h: 1, 1 + nsub, 1 + 2 nsub, ...
.dl_idx_anual_malla <- function(n_anios, nsub) seq(1L, n_anios * nsub + 1L, by = nsub)

# Posiciones de las mismas edades enteras en la malla h/2 de `n_media` puntos: 1, 1 + 2 nsub, ...
.dl_idx_anual_media <- function(n_media, nsub) seq(1L, n_media, by = 2L * nsub)

# ---- Interpolación de los nudos a una malla (matriz W) ----

# W (length(edades_eval) x length(nudos)) tal que  log tasa(edades_eval) = W %*% log tasa(nudos).
#   Fila j, edad a = edades_eval[j] llevada al rango de los nudos: a <- min(max(a, nudo_1), nudo_n)
#   (extrapolación constante: fuera de los nudos vale la tasa del nudo más cercano).
#     a en [nudo_k, nudo_k+1):  W[j, k] = 1 - w,  W[j, k + 1] = w,  w = (a - nudo_k) / (nudo_k+1 - nudo_k)
#     a = nudo_n:               W[j, n] = 1
#   Cada fila suma 1 y en un nudo W devuelve el valor de ese nudo. `nudos` en años, crecientes.
.dl_base_interp <- function(nudos, edades_eval) {
  n_nudos <- length(nudos)
  W <- matrix(0, length(edades_eval), n_nudos)
  for (j in seq_along(edades_eval)) {
    a <- min(max(edades_eval[j], nudos[1]), nudos[n_nudos])
    k <- findInterval(a, nudos)
    if (k >= n_nudos) W[j, n_nudos] <- 1
    else {
      w <- (a - nudos[k]) / (nudos[k + 1] - nudos[k])
      W[j, k] <- 1 - w; W[j, k + 1] <- w
    }
  }
  W
}

# ---- Remisión en una malla ----

# r en la malla h/2 para dl_edo_resolver(): un número se repite en los n_media puntos; un vector debe traer un
# valor por punto (remisión por tramo de edad).
.dl_r_media <- function(remision, n_media) {
  if (length(remision) == 1L) return(rep(as.numeric(remision), n_media))
  if (length(remision) != n_media)
    .dl_stop("`remision` debe ser un n\u00famero o un vector de la longitud de `i_media` (%d), no de longitud %d",
             n_media, length(remision))
  remision
}

# r(a) en las edades `edades` según la configuración: remision.valor (0 si falta) en todas las edades y, en cada
# tramo de remision.por_edad, su valor en [edad_inicio, edad_fin) (incluye el inicio, excluye el fin).
.dl_remision_por_edad <- function(cfg, edades) {
  r <- rep(as.numeric(cfg$remision$valor %||% 0), length(edades))
  for (tramo in cfg$remision$por_edad %||% list())
    r[edades >= tramo$edad_inicio & edades < tramo$edad_fin] <- as.numeric(tramo$valor)
  r
}

# ---- Solución de la EDO ----

# De la solución en las mallas a las edades enteras: p en la malla h y i, f en la malla h/2, tomados en las
# posiciones de a0, a0 + 1, ..., aF (idx_malla de .dl_idx_anual_malla, idx_media de .dl_idx_anual_media). Devuelve
# list(p, i, f) anuales. Gemela en C++: anuales().
.dl_a_edades_enteras <- function(p_malla, i_media, f_media, idx_malla, idx_media)
  list(p = p_malla[idx_malla], i = i_media[idx_media], f = f_media[idx_media])

# RK4 clásico para dp/da = F(a, p) = i(a) (1 - p) - r(a) p - f(a) p (1 - p)   (F = .dl_dp_da)
#   Entrada: i, f y r en la malla h/2 (n_media = 2 n_pasos + 1 puntos, por persona-año); p0 = p(a0).
#   Paso n (edad a -> a + h), con j = 2n - 1 la posición de a en la malla h/2 (k1..k4 son las pendientes de las
#   cuatro etapas, no un índice):
#     k1 = F(a,       p1),  p1 = p(a)
#     k2 = F(a + h/2, p2),  p2 = p1 + h/2 k1
#     k3 = F(a + h/2, p3),  p3 = p1 + h/2 k2
#     k4 = F(a + h,   p4),  p4 = p1 + h k3
#     p(a + h) = p1 + h/6 (k1 + 2 k2 + 2 k3 + k4)
#   Salida: p en la malla h (n_pasos + 1 puntos, proporción), sin truncar.
# Gemela en C++: resolver_edo() en inst/cpp/dl_core.cpp (misma expresión, mismo orden en cada etapa).
#' Resolver la ecuación de la prevalencia sobre una malla
#'
#' Integra dp/da = i (1 - p) - r p - f p (1 - p) con Runge-Kutta de orden 4, paso h = 1/`nsub` años, a partir de
#' las tasas evaluadas en la malla de paso h/2 (los puntos intermedios de cada paso).
#'
#' @details
#' Es la ecuación del modelo enfermedad-muerte: con S las personas sin la enfermedad, C las personas con ella, la
#' incidencia i, la remisión r, la mortalidad en exceso f y la mortalidad por otras causas m (todas por persona-año),
#' la prevalencia p = C / (S + C) cumple dp/da = i (1 - p) - r p - f p (1 - p), sin m. Con `n_pasos` pasos de RK4,
#' la malla h/2 tiene 2 `n_pasos` + 1 puntos y el paso n, de la edad a a la a + h, usa los puntos 2n - 1 (a), 2n
#' (a + h/2) y 2n + 1 (a + h). Con h = 0.2 años (el valor por defecto) el error relativo en p frente a la solución
#' exacta es menor que 1e-6 aun con f = 0.3 por persona-año. Con `motor = "rcpp"` el ajuste usa su gemelo en C++, que
#' hace las mismas operaciones en el mismo orden; el compilador puede fundir una multiplicación y una suma en una sola
#' operación, así que los dos pueden diferir en el último bit.
#'
#' @param i_media Incidencia (por persona-año) en la malla de paso h/2: 2 n_pasos + 1 puntos (longitud impar), del
#'   inicio al fin de la malla.
#' @param f_media Mortalidad en exceso (por persona-año) en la misma malla.
#' @param remision Remisión (por persona-año): un número o un vector de la longitud de `i_media`.
#' @param p0 Prevalencia en la primera edad de la malla.
#' @param nsub Subpasos por año (h = 1/`nsub`).
#' @return Vector de prevalencias en la malla de paso h: `n_pasos` + 1 valores, el primero `p0`.
#' @seealso [dl_edo()] (desde las tasas en los nudos, en las edades enteras).
#' @family avanzado
#' @examples
#' # incidencia constante de 0.02 por persona-año, sin remisión ni mortalidad en exceso, durante 10
#' # años con h = 0.2: la prevalencia exacta es 1 - exp(-0.02 a)
#' nsub <- 5
#' n_pasos <- 10 * nsub
#' p <- dl_edo_resolver(i_media = rep(0.02, 2 * n_pasos + 1), f_media = rep(0, 2 * n_pasos + 1),
#'                      nsub = nsub)
#' edades <- c(0, 5, 10)
#' cbind(edad = edades, rk4 = p[1 + edades * nsub], exacta = 1 - exp(-0.02 * edades))
#' @export
dl_edo_resolver <- function(i_media, f_media, remision = 0, p0 = 0, nsub = 5L) {
  # nsub = 5L repite .DL_NSUB: la firma lleva el número para que la ayuda muestre el valor (test-edo.R lo exige)
  h <- 1 / nsub
  n_media <- length(i_media)
  if (length(f_media) != n_media)
    .dl_stop("`f_media` debe tener la longitud de `i_media` (%d), no %d", n_media, length(f_media))
  r_media <- .dl_r_media(remision, n_media)
  n_pasos <- (n_media - 1L) %/% 2L
  p_malla <- numeric(n_pasos + 1L); p_malla[1] <- p0
  for (paso in seq_len(n_pasos)) {
    j <- 2L * paso - 1L
    p1 <- p_malla[paso]
    # Las cuatro etapas: k_s = dp/da (.dl_dp_da) en (i, r, f)[j], [j + 1], [j + 1], [j + 2], escrita en línea.
    k1 <- i_media[j] * (1 - p1) - r_media[j] * p1 - f_media[j] * p1 * (1 - p1)
    p2 <- p1 + h / 2 * k1
    k2 <- i_media[j + 1L] * (1 - p2) - r_media[j + 1L] * p2 - f_media[j + 1L] * p2 * (1 - p2)
    p3 <- p1 + h / 2 * k2
    k3 <- i_media[j + 1L] * (1 - p3) - r_media[j + 1L] * p3 - f_media[j + 1L] * p3 * (1 - p3)
    p4 <- p1 + h * k3
    k4 <- i_media[j + 2L] * (1 - p4) - r_media[j + 2L] * p4 - f_media[j + 2L] * p4 * (1 - p4)
    p_malla[paso + 1L] <- p1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  p_malla
}

# theta -> p, i, f anuales:
#   log i(a) = W log i_nudos,  log f(a) = W log f_nudos  en la malla h/2 (W: .dl_base_interp)
#   p en la malla h con dl_edo_resolver(); p, i, f en las edades enteras edad_inicio, ..., edad_fin.
# Lo mismo que .dl_edo_ctx() (R/verosimilitud.R) hace para el muestreador, sin contexto.
#' Prevalencia por edad a partir de las tasas en los nudos
#'
#' Interpola log i y log f linealmente entre los nudos (constantes fuera de ellos), resuelve la ecuación de la
#' prevalencia con [dl_edo_resolver()] y devuelve las cantidades en las edades enteras.
#'
#' @param log_i_nudos Logaritmo de la incidencia en cada nudo.
#' @param log_f_nudos Logaritmo de la mortalidad en exceso en cada nudo.
#' @param nudos Edades de los nudos (años), crecientes.
#' @param edad_inicio,edad_fin Primera y última edad (años).
#' @inheritParams dl_edo_resolver
#' @return Lista con `edades` (las edades enteras de `edad_inicio` a `edad_fin`), `p` (prevalencia, proporción),
#'   `i` (incidencia) y `f` (mortalidad en exceso), por persona-año, en esas edades.
#' @seealso [dl_edo_resolver()] (el integrador), [dl_ajustar()] (que la resuelve con cada simulación).
#' @family avanzado
#' @examples
#' # incidencia que crece con la edad y mortalidad en exceso constante, con nudos cada 20 años
#' nudos <- c(30, 50, 70, 90)
#' r <- dl_edo(log_i_nudos = log(c(0.002, 0.005, 0.012, 0.02)), log_f_nudos = log(rep(0.05, 4)),
#'             nudos = nudos, edad_inicio = 30)
#' prev <- data.frame(edad = r$edades, p = r$p, i = r$i)
#' prev[prev$edad %in% c(30, 50, 70, 90), ]
#' plot(r$edades, r$p, type = "l", xlab = "Edad (años)", ylab = "Prevalencia",
#'      main = "Prevalencia por edad", sub = "Datos sintéticos")
#' @export
dl_edo <- function(log_i_nudos, log_f_nudos, nudos, edad_inicio, edad_fin = 99,
                   remision = 0, p0 = 0, nsub = 5L) {
  # edad_fin = 99 y nsub = 5L repiten .DL_EDAD_FIN y .DL_NSUB (test-edo.R lo exige)
  if (length(log_i_nudos) != length(nudos) || length(log_f_nudos) != length(nudos))
    .dl_stop("`log_i_nudos` y `log_f_nudos` deben traer un valor por nudo (%d nudos); traen %d y %d",
             length(nudos), length(log_i_nudos), length(log_f_nudos))
  edades_media <- .dl_edades_media(edad_inicio, edad_fin, nsub)
  W <- .dl_base_interp(nudos, edades_media)
  i_media <- exp(as.vector(W %*% log_i_nudos))
  f_media <- exp(as.vector(W %*% log_f_nudos))
  p_malla <- dl_edo_resolver(i_media, f_media, remision = remision, p0 = p0, nsub = nsub)
  anuales <- .dl_a_edades_enteras(p_malla, i_media, f_media, .dl_idx_anual_malla(edad_fin - edad_inicio, nsub),
                                  .dl_idx_anual_media(length(i_media), nsub))
  c(list(edades = seq(edad_inicio, edad_fin)), anuales)
}
