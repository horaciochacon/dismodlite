# Muestreador Metropolis-Hastings adaptativo por bloques y diagnósticos de convergencia (R-hat y ESS), escritos aquí
# sin dependencias externas.
#
# Notación: ver R/edo.R. Aquí theta es el vector de parámetros que recorre el MCMC (en dl_ajustar(), log i y log f en
# los nudos) y lp(theta) su log-posterior (R/verosimilitud.R). Basta conocer lp salvo una constante: la regla de
# aceptación solo usa diferencias lp(theta') - lp(theta).
#
# ---- Una cadena: dl_mh() ----
# Iteraciones it = 1, ..., T (T = iter); las primeras T_cal (T_cal = warmup) son el calentamiento. theta se divide en
# bloques de índices (en dl_ajustar(): bloque 1 = log i en los nudos, bloque 2 = log f en los nudos). En cada
# iteración se recorren los bloques en orden; para el bloque b, de dimensión d_b:
#   propuesta    theta' = theta, salvo  theta'[b] = theta[b] + s_b L_b z,   z ~ N(0, I_{d_b})
#   aceptación   u ~ U(0, 1);   theta <- theta'  si  log u < lp(theta') - lp(theta)
# La propuesta es simétrica (el cociente de Hastings vale 1) y una propuesta con lp = -Inf siempre se rechaza. Cada
# bloque toma del generador d_b normales y después una uniforme: con la misma semilla, la cadena se repite bit a bit.
#
# ---- Adaptación, solo durante el calentamiento (it <= T_cal) ----
#   escala inicial   s_b = 2.38 / sqrt(d_b)                                                  .DL_MH_ESCALA_OPTIMA
#   forma inicial    L_b = I
#   forma            una sola vez, en it = T_cal / 2:
#                      L_b = chol(Cov_b + 1e-8 I), triangular inferior                       .DL_MH_JITTER_COVARIANZA
#                    con Cov_b la covarianza empírica de theta[b] en las iteraciones 1, ..., T_cal / 2
#   escala           al cerrar la ventana k (it = 100 k), con a_b la aceptación del bloque b en esa ventana:
#                      s_b <- s_b exp((a_b - 0.28) / sqrt(k))                                .DL_MH_ACEPTACION_OBJETIVO
#                    (Robbins-Monro sobre log s_b: sube si se acepta más que 0,28 y baja si se acepta menos, con
#                    pasos que decrecen como 1 / sqrt(k)); después los contadores de aceptación vuelven a cero.
# T_cal es múltiplo de 200 (.DL_MH_MULTIPLO_CALENTAMIENTO) para que T_cal / 2 y T_cal cierren una ventana de 100
# (.DL_MH_VENTANA): la forma se estima con ventanas completas y la última ventana termina con el calentamiento. Desde
# it = T_cal + 1, s_b y L_b quedan fijos: la cadena es un Metropolis-Hastings ordinario, con la posterior como
# distribución estacionaria, y solo esas iteraciones se guardan. Con T_cal = 0 no hay adaptación.
#
# ---- Adelgazamiento ----
# La fila j de `draws` es theta en la iteración it = T_cal + j thin, j = 1, ..., floor((T - T_cal) / thin). La
# aceptación que se devuelve es la de todas las iteraciones posteriores al calentamiento, guardadas o no.
#
# ---- Varias cadenas: .dl_correr_cadena() y dl_mh_cadenas() ----
# Con la semilla s, la cadena c = 1, 2, ... usa dos semillas derivadas (módulo 2^31 - 1):
#   s 1000 + c         punto inicial: theta0 + N(0, 0.1) en cada coordenada (sd 0,1)             .DL_MH_SD_INICIO
#   s 1000 + c + 500   la cadena (la semilla de dl_mh())
# Cada cadena depende solo de (s, c): el resultado es el mismo en serie o en paralelo, con cualquier reparto de las
# cadenas entre procesos.
#
# ---- Diagnósticos, con n_cadenas cadenas de n_sim simulaciones guardadas cada una ----
# R-hat partido (dl_rhat; Gelman et al., Bayesian Data Analysis, 3.a ed., cap. 11). Cada cadena se parte en dos
# mitades de n = floor(n_sim / 2) simulaciones (con n_sim impar, la última queda fuera). Por parámetro, con media_k y
# var_k la media y la varianza muestral de la mitad k = 1, ..., 2 n_cadenas:
#   var_entre  = n Var_k(media_k)                   (B en Gelman et al.)
#   var_dentro = promedio_k var_k                   (W en Gelman et al.; no es la matriz W de R/edo.R)
#   R-hat      = sqrt(((n - 1) / n var_dentro + var_entre / n) / var_dentro)
# Vale cerca de 1 cuando todas las mitades muestrean la misma distribución; dl_exportar_corrida() exige R-hat < 1.01.
#
# ESS (dl_ess). Por parámetro, autocor_t es el promedio entre cadenas de la autocorrelación de rezago t de cada
# cadena (la de stats::acf: centrada en la media de la cadena y con divisor n_sim), t = 1, ..., t_max, con
# t_max = min(max_lag, n_sim - 2). Truncamiento tipo Geyer: se suman los pares P_k = autocor_{2k-1} + autocor_{2k},
# k = 1, 2, ..., desde el rezago 1 (Geyer, 1992, empieza en el rezago 0), hasta el primer par negativo (que no se
# suma) o hasta agotar los pares completos dentro de t_max:
#   ESS = n_cadenas n_sim / (1 + 2 suma_k P_k)

# ---- Constantes del muestreador ----

# Escala inicial de la propuesta: s_b = 2.38 / sqrt(d_b) es la escala óptima de un paseo aleatorio normal cuando la
# posterior es normal de dimensión d_b (Gelman, Roberts y Gilks, 1996). La adaptación la corrige durante el
# calentamiento.
.DL_MH_ESCALA_OPTIMA <- 2.38

# Aceptación objetivo de cada bloque. Para un paseo aleatorio normal, la óptima es cercana a 0,44 en dimensión 1
# (Gelman, Roberts y Gilks, 1996) y tiende a 0,234 al crecer la dimensión (Roberts, Gelman y Gilks, 1997); 0,28 queda
# entre ambas, acorde con bloques de pocos parámetros (los nudos de log i o los de log f).
.DL_MH_ACEPTACION_OBJETIVO <- 0.28

# Largo, en iteraciones, de la ventana de adaptación de la escala: al cerrar cada ventana, la escala se corrige con la
# aceptación observada en ella (100 propuestas por bloque).
.DL_MH_VENTANA <- 100L

# El calentamiento debe ser múltiplo de dos ventanas, para que su mitad (cuando se estima la forma de la propuesta) y
# su final cierren una ventana. dl_opciones_mcmc() exige lo mismo a `calentamiento`.
.DL_MH_MULTIPLO_CALENTAMIENTO <- 2L * .DL_MH_VENTANA

# Se suma a la diagonal de la covarianza empírica antes de la factorización de Cholesky: la vuelve definida positiva
# aunque un parámetro no se haya movido en la primera mitad del calentamiento (varianza 0), sin cambiar de forma
# apreciable la propuesta de los parámetros que sí se movieron.
.DL_MH_JITTER_COVARIANZA <- 1e-8

# Desviación estándar de la perturbación normal del punto inicial de cada cadena, en la escala de theta (log de las
# tasas): una desviación equivale a un factor de alrededor de 1,1 en i y en f. Así las cadenas empiezan en puntos
# distintos, lo que R-hat necesita para detectar una mezcla lenta.
.DL_MH_SD_INICIO <- 0.1

# Factor, desplazamiento y módulo de las semillas derivadas (cabecera, «Varias cadenas»). Con menos de 500 cadenas
# son todas distintas: las del punto inicial ocupan 1000 s + 1, ..., 1000 s + 499 y las de las cadenas
# 1000 s + 501, ..., 1000 s + 999, también entre semillas s consecutivas (dl_ajustar() usa semilla + sex_id en cada
# sexo). La cuenta se hace en doble precisión y módulo 2^31 - 1, el mayor entero de R: s 1000 no cabe en un entero
# cuando s es grande (una fecha AAAAMMDD, por ejemplo).
.DL_MH_FACTOR_SEMILLA <- 1000
.DL_MH_DESPLAZAMIENTO_SEMILLA <- 500
.DL_MH_MODULO_SEMILLA <- 2147483647

# ---- Una cadena ----

#' Muestreador Metropolis-Hastings adaptativo por bloques
#'
#' Corre una cadena de Metropolis-Hastings con propuestas normales por bloque. Durante el calentamiento adapta, en
#' cada bloque, la escala de la propuesta (hacia una aceptación de 0.28) y su forma (la covarianza empírica de la
#' primera mitad del calentamiento); después las deja fijas y guarda una de cada `thin` iteraciones.
#'
#' Es el muestreador de [dl_ajustar()] y sirve para cualquier log-posterior. Sus argumentos conservan los nombres de
#' versiones anteriores: `iter`, `warmup`, `seed` y `thin` son las `iteraciones`, el `calentamiento`, la `semilla` y
#' el `adelgazamiento` de [dl_opciones_mcmc()].
#'
#' @param lp Función de log-posterior: recibe `theta` y devuelve un número, o `-Inf` fuera del soporte (la propuesta
#'   se rechaza). Basta conocerla salvo una constante.
#' @param theta0 Punto inicial: vector numérico (sus nombres son los de las columnas de `draws`); `lp(theta0)` debe
#'   ser finita.
#' @param bloques Lista de vectores de índices de `theta0`; los parámetros de un mismo bloque se proponen juntos.
#' @param iter Iteraciones totales, calentamiento incluido.
#' @param warmup Iteraciones de calentamiento: múltiplo de 200 y menor que `iter` (con 0 no hay adaptación).
#' @param seed Semilla (número entero), obligatoria: con la misma semilla la cadena se repite exactamente.
#' @param thin Adelgazamiento: después del calentamiento se guarda una de cada `thin` iteraciones.
#' @return Lista con `draws` (matriz de simulaciones: una fila por iteración guardada y una columna por parámetro),
#'   `aceptacion` (tasa de aceptación de cada bloque después del calentamiento) y `escala` (escala final de la
#'   propuesta de cada bloque).
#' @seealso [dl_mh_cadenas()] (varias cadenas), [dl_rhat()] y [dl_ess()] (la convergencia) y [dl_opciones_mcmc()].
#' @family avanzado
#' @examples
#' # Normal bivariada estándar, con los dos parámetros en un mismo bloque
#' lp <- function(theta) -0.5 * sum(theta^2)
#' r <- dl_mh(lp, c(a = 0, b = 0), bloques = list(1:2), iter = 2200, warmup = 200, seed = 1)
#' colMeans(r$draws)
#' r$aceptacion
#' @export
dl_mh <- function(lp, theta0, bloques, iter, warmup, seed, thin = 1L) {
  .dl_exigir_semilla(seed, "seed", maximo = .Machine$integer.max)
  .dl_mh_exigir_argumentos(lp, theta0, bloques, iter, warmup, thin)
  set.seed(seed)
  n_par <- length(theta0)
  escala <- .dl_mh_escala_inicial(bloques)                     # s_b
  L_prop <- lapply(bloques, function(idx) diag(length(idx)))  # L_b = I hasta la mitad del calentamiento
  theta <- theta0
  lp_actual <- lp(theta)
  if (!is.finite(lp_actual))
    .dl_stop(paste0("la log-posterior no es finita en el punto inicial (lp(theta0) = %s); elige un `theta0` dentro ",
                    "del soporte"), format(lp_actual))
  n_guardadas <- (iter - warmup) %/% thin
  simulaciones <- matrix(NA_real_, n_guardadas, n_par, dimnames = list(NULL, names(theta0)))
  # Contadores por bloque: de la ventana en curso durante el calentamiento; después, de todo el muestreo.
  aceptadas <- propuestas <- numeric(length(bloques))
  theta_cal <- matrix(NA_real_, warmup, n_par)                # theta en cada iteración del calentamiento
  for (it in seq_len(iter)) {
    # Un paso de Metropolis por bloque: propuesta theta' = theta + s_b L_b z en el bloque, aceptada si
    # log u < lp(theta') - lp(theta). El orden rnorm -> lp -> runif fija las simulaciones de cada semilla.
    for (b in seq_along(bloques)) {
      idx <- bloques[[b]]
      theta_prop <- theta
      theta_prop[idx] <- theta_prop[idx] + escala[b] * as.vector(L_prop[[b]] %*% stats::rnorm(length(idx)))
      lp_prop <- lp(theta_prop)
      propuestas[b] <- propuestas[b] + 1
      if (log(stats::runif(1)) < lp_prop - lp_actual) {
        theta <- theta_prop; lp_actual <- lp_prop; aceptadas[b] <- aceptadas[b] + 1
      }
    }
    if (it <= warmup) {
      theta_cal[it, ] <- theta
      if (it == warmup %/% 2L)                                 # forma: una sola vez, a la mitad del calentamiento
        for (b in seq_along(bloques))
          L_prop[[b]] <- .dl_mh_forma_propuesta(theta_cal[seq_len(it), bloques[[b]], drop = FALSE])
      if (it %% .DL_MH_VENTANA == 0L) {                        # escala: al cerrar la ventana k
        k <- it / .DL_MH_VENTANA
        for (b in seq_along(bloques))
          escala[b] <- .dl_mh_escala_adaptada(escala[b], aceptadas[b] / propuestas[b], k)
        aceptadas[] <- 0; propuestas[] <- 0
      }
    } else if ((it - warmup) %% thin == 0L) {
      simulaciones[(it - warmup) %/% thin, ] <- theta          # fila j = (it - T_cal) / thin
    }
  }
  list(draws = simulaciones, aceptacion = aceptadas / pmax(propuestas, 1), escala = escala)
}

# Escala inicial s_b de cada bloque (cabecera, «Adaptación»).
.dl_mh_escala_inicial <- function(bloques)
  vapply(bloques, function(idx) .DL_MH_ESCALA_OPTIMA / sqrt(length(idx)), numeric(1))

# Forma L_b de la propuesta de un bloque (cabecera, «Adaptación»): L_b L_b' = Cov_b + 1e-8 I. `theta_b`: matriz de
# las iteraciones del calentamiento (filas) por los parámetros del bloque (columnas).
.dl_mh_forma_propuesta <- function(theta_b) {
  Cov_b <- stats::cov(theta_b)
  t(chol(Cov_b + diag(.DL_MH_JITTER_COVARIANZA, nrow(Cov_b))))
}

# Paso de Robbins-Monro de la escala de un bloque al cerrar la ventana k (cabecera, «Adaptación»), con a_b =
# `aceptacion` la tasa de aceptación del bloque en la ventana.
.dl_mh_escala_adaptada <- function(escala, aceptacion, k)
  escala * exp((aceptacion - .DL_MH_ACEPTACION_OBJETIVO) / sqrt(k))

# ---- Varias cadenas ----

# Semilla derivada de la semilla s (`seed`) y la cadena c (`cadena`): (s 1000 + c + desplazamiento) módulo 2^31 - 1
# (cabecera, «Varias cadenas»), con desplazamiento 0 para el punto inicial y .DL_MH_DESPLAZAMIENTO_SEMILLA para la
# cadena.
.dl_semilla_derivada <- function(seed, cadena, desplazamiento)
  as.integer((as.numeric(seed) * .DL_MH_FACTOR_SEMILLA + cadena + desplazamiento) %% .DL_MH_MODULO_SEMILLA)

# Una cadena de dl_mh() desde un punto inicial disperso, theta0 + N(0, 0.1) por coordenada, con las dos semillas
# derivadas de (seed, cadena) (cabecera, «Varias cadenas»). Es determinista sin importar en qué proceso corra:
# dl_ajustar() la llama directamente para repartir todas las combinaciones sexo x cadena en un solo mapa paralelo.
.dl_correr_cadena <- function(lp, theta0, bloques, cadena, iter, warmup, seed, thin = 1L) {
  set.seed(.dl_semilla_derivada(seed, cadena, 0))
  theta_inicio <- theta0 + stats::rnorm(length(theta0), 0, .DL_MH_SD_INICIO)
  names(theta_inicio) <- names(theta0)
  semilla_cadena <- .dl_semilla_derivada(seed, cadena, .DL_MH_DESPLAZAMIENTO_SEMILLA)
  dl_mh(lp, theta_inicio, bloques, iter, warmup, seed = semilla_cadena, thin = thin)
}

#' Varias cadenas de [dl_mh()]
#'
#' Corre `chains` cadenas de [dl_mh()]. La cadena `c` empieza en `theta0` más una perturbación normal de desviación
#' estándar 0.1 en cada coordenada y usa semillas derivadas de `seed` y de `c`, así que el resultado es el mismo en
#' serie o en paralelo.
#'
#' @inheritParams dl_mh
#' @param chains Número de cadenas.
#' @param cores Procesos en paralelo (en Windows se usa 1).
#' @return Lista de cadenas, cada una como la devuelve [dl_mh()] (`draws`, `aceptacion` y `escala`).
#' @seealso [dl_mh()], [dl_rhat()], [dl_ess()].
#' @family avanzado
#' @examples
#' lp <- function(theta) -0.5 * sum(theta^2)
#' cadenas <- dl_mh_cadenas(lp, c(a = 0, b = 0), list(1:2), chains = 2, iter = 2200, warmup = 200,
#'                          seed = 1)
#' dl_rhat(lapply(cadenas, `[[`, "draws"))
#' @export
dl_mh_cadenas <- function(lp, theta0, bloques, chains, iter, warmup, seed, cores = 1L, thin = 1L) {
  .dl_exigir_semilla(seed, "seed", maximo = Inf)
  .dl_mh_exigir_argumentos(lp, theta0, bloques, iter, warmup, thin)
  if (!.dl_es_entero1(chains) || chains < 1)
    .dl_stop("`chains` (cadenas) debe ser un n\u00famero entero >= 1; es %s", .dl_describir_objeto(chains))
  if (!.dl_es_entero1(cores) || cores < 1)
    .dl_stop("`cores` (n\u00facleos) debe ser un n\u00famero entero >= 1; es %s", .dl_describir_objeto(cores))
  correr <- function(cadena) .dl_correr_cadena(lp, theta0, bloques, cadena, iter, warmup, seed, thin)
  cadenas <- .dl_paralelo(chains, correr, cores)
  .dl_exigir_sin_fallos(cadenas, "una cadena")
  cadenas
}

# ---- Diagnósticos ----

#' R-hat de varias cadenas
#'
#' R-hat partido (*split R-hat*): cada cadena se parte en dos mitades y se compara la varianza entre las mitades con
#' la varianza dentro de ellas. Cerca de 1 indica que todas las mitades muestrean la misma distribución.
#'
#' Con `n` simulaciones por mitad (la mitad de las de cada cadena; con un número impar, la última queda fuera),
#' `var_entre` = `n` por la varianza de las medias de las mitades y `var_dentro` = el promedio de sus varianzas:
#' R-hat = `sqrt(((n - 1) / n * var_dentro + var_entre / n) / var_dentro)` (Gelman et al., *Bayesian Data
#' Analysis*, 3.a ed., cap. 11). Con menos de 4 simulaciones por cadena las mitades no tienen varianza y el
#' resultado es `NA`.
#'
#' @param draws_list Lista de matrices de simulaciones (una por cadena, con las mismas filas y columnas).
#' @return Vector con el R-hat de cada columna, con los nombres de las columnas.
#' @seealso [dl_ess()]; [dl_ajustar()] guarda el R-hat de cada parámetro en `mcmc` y [dl_exportar_corrida()] exige
#'   R-hat < 1.01.
#' @family diagnóstico
#' @examples
#' set.seed(1)
#' cadenas <- replicate(4, matrix(rnorm(2000), ncol = 2, dimnames = list(NULL, c("a", "b"))),
#'                      simplify = FALSE)
#' dl_rhat(cadenas)
#' @export
dl_rhat <- function(draws_list) {
  .dl_exigir_cadenas(draws_list)
  mitades <- do.call(c, lapply(draws_list, .dl_partir_en_mitades))
  n <- nrow(mitades[[1]])
  out <- vapply(seq_len(ncol(mitades[[1]])), function(j) {
    medias <- vapply(mitades, function(mitad) mean(mitad[, j]), numeric(1))
    varianzas <- vapply(mitades, function(mitad) stats::var(mitad[, j]), numeric(1))
    var_entre <- n * stats::var(medias)        # B en Gelman et al.
    var_dentro <- mean(varianzas)              # W en Gelman et al.
    sqrt(((n - 1) / n * var_dentro + var_entre / n) / var_dentro)
  }, numeric(1), USE.NAMES = FALSE)
  stats::setNames(out, colnames(draws_list[[1]]))
}

# Las dos mitades de una cadena (matriz de simulaciones) del R-hat partido (cabecera, «Diagnósticos»): filas 1..n y
# n+1..2n, con n = floor(filas / 2).
.dl_partir_en_mitades <- function(simulaciones) {
  n <- nrow(simulaciones) %/% 2L
  list(simulaciones[seq_len(n), , drop = FALSE], simulaciones[n + seq_len(n), , drop = FALSE])
}

#' Tamaño efectivo de muestra
#'
#' Tamaño efectivo de muestra (ESS) de varias cadenas: el número de simulaciones independientes que darían la misma
#' precisión para la media que las simulaciones autocorrelacionadas de las cadenas.
#'
#' Con `n_cadenas` cadenas de `n_sim` simulaciones, `autocor_t` es la autocorrelación de rezago `t` promediada entre
#' cadenas ([stats::acf()]), hasta `t_max = min(max_lag, n_sim - 2)`. Se suman los pares
#' `autocor_{2k-1} + autocor_{2k}` desde el rezago 1 hasta el primer par negativo, que no se suma (truncamiento tipo
#' Geyer; Geyer, 1992, empieza en el rezago 0) y ESS = `n_cadenas * n_sim / (1 + 2 * suma)`.
#'
#' @inheritParams dl_rhat
#' @param max_lag Rezago máximo de la autocorrelación (al menos 2).
#' @return Vector con el ESS de cada columna, con los nombres de las columnas.
#' @seealso [dl_rhat()]; [dl_ajustar()] guarda el ESS de cada parámetro en `mcmc` y [dl_exportar_corrida()] exige
#'   ESS >= 400 (`ess_minimo`).
#' @family diagnóstico
#' @examples
#' set.seed(1)
#' cadenas <- replicate(4, matrix(rnorm(2000), ncol = 2, dimnames = list(NULL, c("a", "b"))),
#'                      simplify = FALSE)
#' dl_ess(cadenas)   # simulaciones independientes: cerca de 4 x 1000
#' @export
dl_ess <- function(draws_list, max_lag = 200L) {
  .dl_exigir_cadenas(draws_list, min_filas = 4L)
  if (!.dl_es_entero1(max_lag) || max_lag < 2)
    .dl_stop("`max_lag` debe ser un n\u00famero entero >= 2; es %s", .dl_describir_objeto(max_lag))
  n_cadenas <- length(draws_list); n_sim <- nrow(draws_list[[1]])
  out <- vapply(seq_len(ncol(draws_list[[1]])), function(j) {
    t_max <- min(max_lag, n_sim - 2L)
    autocor <- rowMeans(vapply(draws_list, function(simulaciones)
      stats::acf(simulaciones[, j], lag.max = t_max, plot = FALSE)$acf[-1, 1, 1], numeric(t_max)))
    n_cadenas * n_sim / (1 + 2 * .dl_suma_geyer(autocor))
  }, numeric(1), USE.NAMES = FALSE)
  stats::setNames(out, colnames(draws_list[[1]]))
}

# Suma truncada tipo Geyer de las autocorrelaciones autocor_1, ..., autocor_{t_max} del ESS (cabecera,
# «Diagnósticos»): los pares P_k hasta el primer par negativo o hasta agotar los pares completos.
.dl_suma_geyer <- function(autocor) {
  suma <- 0
  for (k in seq(1, length(autocor) - 1, by = 2)) {
    par_k <- autocor[k] + autocor[k + 1]
    if (par_k < 0) break
    suma <- suma + par_k
  }
  suma
}

# ---- Comprobación de argumentos ----

.dl_mh_exigir_argumentos <- function(lp, theta0, bloques, iter, warmup, thin) {
  if (!is.function(lp)) .dl_stop("`lp` debe ser una funci\u00f3n que reciba `theta` y devuelva su log-posterior")
  if (!is.numeric(theta0) || length(theta0) == 0L)
    .dl_stop("`theta0` debe ser un vector num\u00e9rico (el punto inicial); es %s", .dl_describir_objeto(theta0))
  indices_validos <- function(idx) is.numeric(idx) && length(idx) > 0L && !anyNA(idx) && all(idx == round(idx)) &&
    all(idx >= 1 & idx <= length(theta0))
  if (!is.list(bloques) || length(bloques) == 0L || !all(vapply(bloques, indices_validos, logical(1))))
    .dl_stop("`bloques` debe ser una lista de vectores de \u00edndices de `theta0` (entre 1 y %d)", length(theta0))
  if (!.dl_es_entero1(warmup) || warmup < 0 || warmup %% .DL_MH_MULTIPLO_CALENTAMIENTO != 0)
    .dl_stop("`warmup` (calentamiento) debe ser un m\u00faltiplo de %d (0, 200, 400, 1000, ...); es %s",
             .DL_MH_MULTIPLO_CALENTAMIENTO, .dl_describir_objeto(warmup))
  if (!.dl_es_entero1(iter) || iter <= warmup)
    .dl_stop("`iter` (iteraciones) debe ser un n\u00famero entero mayor que `warmup` (%s); es %s",
             format(warmup), .dl_describir_objeto(iter))
  if (!.dl_es_entero1(thin) || thin < 1)
    .dl_stop("`thin` (adelgazamiento) debe ser un n\u00famero entero >= 1; es %s", .dl_describir_objeto(thin))
  invisible(TRUE)
}

# Una lista no vacía de matrices de simulaciones (una por cadena) con las mismas filas y columnas, y al menos
# `min_filas` filas.
.dl_exigir_cadenas <- function(draws_list, min_filas = 0L) {
  es_matriz <- function(x) length(dim(x)) == 2L
  if (!is.list(draws_list) || is.data.frame(draws_list) || length(draws_list) == 0L ||
      !all(vapply(draws_list, es_matriz, logical(1))))
    .dl_stop("`draws_list` debe ser una lista de matrices de simulaciones, una por cadena")
  filas <- vapply(draws_list, nrow, integer(1))
  columnas <- vapply(draws_list, ncol, integer(1))
  if (length(unique(filas)) > 1L || length(unique(columnas)) > 1L)
    .dl_stop("las cadenas de `draws_list` deben tener las mismas filas (simulaciones) y columnas (par\u00e1metros)")
  if (filas[1] < min_filas)
    .dl_stop("cada cadena necesita al menos %d simulaciones; tiene %d", min_filas, filas[1])
  invisible(TRUE)
}
