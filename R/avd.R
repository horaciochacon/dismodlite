# Años vividos con discapacidad (AVD) por banda de edad, sexo y simulación.
#
# Notación: ver R/edo.R. Símbolos propios de este archivo:
#   k         simulación, k = 1..n_sim (n_sim = número de simulaciones del ajuste; la columna `draw`)
#   b         banda de edad del ancla de prevalencia, intervalo [inicio, fin) en años
#   e         estado de salud de la tabla de severidad, e = 1..E
#   p_{k,b}   prevalencia promedio de la banda b: promedio de p(a) sobre las edades anuales de [inicio, fin),
#             ponderado por población (.dl_q_bandas(), R/bandas.R); proporción en [0, 1)
#   pi_{k,e}  proporción de los casos prevalentes que está en el estado e; sum_e pi_{k,e} = 1 en cada k
#   DW_{k,e}  peso de discapacidad del estado e, en [0, 1] (0 = estado sin discapacidad)
#   COMO_b    factor de comorbilidad de la banda b y el sexo (dl_factor_comorbilidad(), R/comorbilidad.R);
#             1 cuando no se aplica
# En el código, las matrices con sufijo _ke tienen una fila por simulación k y una columna por estado e.
#
# Ecuación, por simulación k, sexo, banda b y ubicación (el ancla y, con una cascada, cada departamento):
#
#   AVD_{k,b} = p_{k,b} x ( sum_e pi_{k,e} DW_{k,e} ) x COMO_b
#
# Unidades: años vividos con discapacidad por persona y año, la misma escala por persona que p (x 100 000 da la
# tasa por 100 000). Malla: bandas de edad del ancla; la integración desde la malla anual y la regla de contención
# [inicio, fin) son las de R/bandas.R. La suma sum_e pi_{k,e} DW_{k,e} es una por simulación: no depende de la
# banda ni de la ubicación, porque la tabla de severidad es la misma en todas las edades y ubicaciones.
#
# Muestreo de pi y DW por momentos: una Beta por elemento (independientes entre estados, y pi independiente de DW),
# con media `media` = valor central y el intervalo del 95 % [inferior, superior]:
#   v = ((superior - inferior) / 3.92)^2     (un intervalo del 95 % de una normal mide 3.92 = 2 x 1,96 sd;
#                                            .DL_ANCHO_IC95_EN_SD de R/betas.R)
#   forma1 = media (media (1 - media) / v - 1),   forma2 = (1 - media) forma1 / media
# Beta(forma1, forma2) tiene esa media y varianza v. Sin Beta, el elemento es constante e igual a su media:
#   - media <= 0, media >= 1 o v < 1e-12 (intervalo degenerado, como el DW 0 del estado asintomático);
#   - v >= media (1 - media): ninguna Beta con esa media tiene esa varianza (momentos infactibles; se avisa).
# Renormalización por simulación: pi_{k,e} <- pi_{k,e} / sum_e pi_{k,e}. La media de cada proporción se desplaza
# un poco respecto de la tabla; ese desplazamiento se reporta por estado (desplazamiento_splits).
#
# Canal de proporción (estados con beta_covariable): logit pi'_{k,e} = logit pi_{k,e} + beta_{k,e} dX_e, y después
# se renormaliza por simulación. beta_{k,e} es el efecto de la covariable del estado e en la simulación k
# (.dl_draws_betas(), R/betas.R); dX_e es la diferencia de la covariable entre la ubicación y la de la partición de
# severidad. Esta versión exige que la partición sea de la ubicación del ancla, así que dX = 0 en toda ubicación y
# pi' = pi.

# ---- Constantes ----

# Varianza por debajo de la cual un intervalo se trata como degenerado y el elemento como constante: equivale a un
# intervalo de ancho menor que 3.92e-6, sin incertidumbre que propagar (con v -> 0 la Beta tendría formas enormes).
.DL_VARIANZA_MINIMA_BETA <- 1e-12

# ---- Beta por momentos ----

# Formas de la Beta con media `media` y varianza v = ((superior - inferior) / 3.92)^2 (ecuaciones en la cabecera).
# Devuelve list(forma1, forma2) o, si no hay Beta posible, list(constante = media). `que` nombra el elemento en el
# aviso.
.dl_beta_momentos <- function(media, inferior, superior, que = "un estado de severidad") {
  v <- ((superior - inferior) / .DL_ANCHO_IC95_EN_SD)^2
  if (media <= 0 || media >= 1 || v < .DL_VARIANZA_MINIMA_BETA) return(list(constante = media))
  # Momentos infactibles: con esa media, toda Beta tiene varianza menor que media (1 - media). Pasa con el estado
  # residual de una partición (media alta con un intervalo casi [0, 1]): ese intervalo no describe una incertidumbre
  # creíble y el elemento entra como constante en su media, con aviso.
  if (v >= media * (1 - media)) {
    .dl_warn(paste0("%s no admite una distribuci\u00f3n Beta con su media y su intervalo (media %.4f, ",
                    "varianza %.6f >= media (1 - media)); entra como constante en su media, sin incertidumbre"), que,
             media, v)
    return(list(constante = media))
  }
  forma1 <- media * (media * (1 - media) / v - 1)
  list(forma1 = forma1, forma2 = (1 - media) * forma1 / media)
}

# n_sim simulaciones de cada elemento (Beta por momentos o constante): matriz n_sim x E, una columna por elemento,
# en el orden de `media` (ese orden fija la secuencia aleatoria).
.dl_rbeta_momentos <- function(media, inferior, superior, n_sim, que = rep("un estado de severidad", length(media))) {
  vapply(seq_along(media), function(e) {
    forma <- .dl_beta_momentos(media[e], inferior[e], superior[e], que[e])
    if (!is.null(forma$constante)) rep(forma$constante, n_sim) else stats::rbeta(n_sim, forma$forma1, forma$forma2)
  }, numeric(n_sim))
}

# ---- Proporciones y pesos por simulación ----

# pi_{k,e} y DW_{k,e} de la tabla de severidad `sev` (una fila por estado), con n_sim simulaciones desde `semilla`:
# primero las proporciones y después los pesos (el orden fija la secuencia aleatoria). Devuelve pi_ke (renormalizada:
# cada fila suma 1), dw_ke y el desplazamiento de la media de cada proporción por la renormalización.
.dl_simular_pi_dw <- function(sev, n_sim, semilla) {
  set.seed(semilla)
  pi_muestra <- .dl_rbeta_momentos(sev$proportion, sev$prop_lower, sev$prop_upper, n_sim,
                                   sprintf("la proporci\u00f3n del estado %s", sev$health_state_id))
  dw_ke <- .dl_rbeta_momentos(sev$dw_mean, sev$dw_lower, sev$dw_upper, n_sim,
                              sprintf("el peso de discapacidad del estado %s", sev$health_state_id))
  pi_ke <- pi_muestra / rowSums(pi_muestra)            # renormalización: sum_e pi_{k,e} = 1 en cada simulación k
  media_pi <- colMeans(pi_ke)
  desplazamiento <- data.table::data.table(health_state_id = sev$health_state_id,
    prop_original = sev$proportion, prop_medio_renorm = media_pi, desplazamiento = media_pi - sev$proportion)
  list(pi_ke = pi_ke, dw_ke = dw_ke, desplazamiento = desplazamiento)
}

# Estados con canal de proporción (beta_covariable no vacía; "" del CSV equivale a NA) y la fila de `betas` de su
# covariable (clave covariate_name_short), en el orden de los estados.
.dl_estados_canal_proporcion <- function(sev, betas) {
  estados <- which(!is.na(sev$beta_covariable) & nzchar(sev$beta_covariable))
  betas_estados <- betas[match(sev$beta_covariable[estados], covariate_name_short)]
  if (length(estados) && anyNA(betas_estados$beta))
    .dl_stop("la columna beta_covariable de la tabla de severidad cita covariable(s) sin beta en la tabla betas: %s",
             paste(sev$beta_covariable[estados][is.na(betas_estados$beta)], collapse = ", "))
  list(estados = estados, betas = betas_estados)
}

# logit(x) = log(x / (1 - x)), x en (0, 1).
.dl_logit <- function(x) log(x / (1 - x))

# Canal de proporción: logit pi'_{k,e} = logit pi_{k,e} + beta_{k,e} dX_e en las columnas `estados` de pi_ke, y
# renormalización por simulación. beta_ke: una columna por estado del canal (en el orden de `estados`); dX: uno por
# estado. Con dX = 0 en todos, pi_ke vuelve sin cambios.
.dl_aplicar_canal_proporcion <- function(pi_ke, estados, beta_ke, dX) {
  if (!length(estados) || all(dX == 0)) return(pi_ke)
  for (j in seq_along(estados)) {
    if (dX[j] == 0) next
    logit_pi <- .dl_logit(pi_ke[, estados[j]]) + beta_ke[, j] * dX[j]
    pi_ke[, estados[j]] <- 1 / (1 + exp(-logit_pi))              # logit inversa
  }
  pi_ke / rowSums(pi_ke)
}

# ---- AVD ----

# AVD_{k,b} = p_{k,b} x suma_pi_dw[k] x COMO_b (ecuación de la cabecera). `p_bandas`: tabla de .dl_q_bandas()
# (location_id, sex_id, age_group_id, draw, val = p_{k,b}); `suma_pi_dw`: sum_e pi_{k,e} DW_{k,e}, indexado por
# la simulación (draw = k); `comorbilidad`: tabla de dl_factor_comorbilidad() o NULL (COMO_b = 1). Devuelve la
# tabla con las mismas columnas y val = AVD_{k,b}, ordenada por ubicación, sexo, banda y simulación.
.dl_avd_bandas <- function(p_bandas, suma_pi_dw, comorbilidad) {
  avd <- data.table::copy(p_bandas)[, val := val * suma_pi_dw[draw]]
  if (!is.null(comorbilidad)) {
    # all.x: una banda o un sexo sin fila en `comorbilidad` queda con factor NA y se detiene abajo, en vez de
    # desaparecer del resultado.
    avd <- merge(avd, comorbilidad[, list(sex_id, age_group_id, factor)], by = c("sex_id", "age_group_id"),
                 all.x = TRUE)
    if (anyNA(avd$factor))
      .dl_stop("falta el factor de comorbilidad (o es NA) en alguna banda de edad del ancla o en alg\u00fan sexo")
    avd[, `:=`(val = val * factor, factor = NULL)]
  }
  data.table::setcolorder(avd, c("location_id", "sex_id", "age_group_id", "draw", "val"))
  data.table::setorder(avd, location_id, sex_id, age_group_id, draw)
  avd
}

# La severidad de la causa `causa` (`sev`, la tabla de los insumos) debe traer estados de salud: sin ellos no hay AVD.
# La exigen dl_avd(), dl_correr() (antes de ajustar) y dl_revisar_proyecto(); el error lleva su tabla (`tabla`).
.dl_exigir_severidad <- function(sev, causa) {
  if (!is.null(sev) && nrow(sev)) return(invisible(sev))
  .dl_stop(paste0("la causa %d no tiene estados de salud en la tabla `severidad`: sin ellos no hay AVD. ",
                  "Agr\u00e9galos (ver ?dl_proyecto) o, sin AVD, corre los pasos por separado: dl_ajustar(), ",
                  "dl_cascada() y dl_estimaciones()"), causa, campos = list(tabla = "severidad"))
}

#' Años vividos con discapacidad (AVD)
#'
#' Por simulación y banda: AVD = prevalencia x suma de (proporción del estado x peso de discapacidad), con las
#' proporciones y los pesos muestreados de sus intervalos y, si se da, el factor de comorbilidad por edad y sexo.
#' Con una cascada, se calculan para la nación y cada ubicación subnacional. Sin estados de salud en la severidad de
#' los insumos, es un error.
#'
#' @details
#' Los estados de salud de la causa vienen de `severidad.csv` (ver [dl_proyecto()]): la proporción de los casos
#' prevalentes en cada estado y su peso de discapacidad (DW), cada uno con su intervalo del 95 %. En cada simulación
#' se sortean las proporciones y los pesos (distribuciones Beta con la media y el intervalo dados) y las proporciones
#' se renormalizan para que sumen 1; `desplazamiento_splits` dice cuánto movió esa renormalización la proporción
#' media de cada estado. El AVD de la banda es la prevalencia de la banda por la suma de proporción x DW y, con
#' `comorbilidad`, por el factor de su sexo y su banda de edad. Los AVD son años vividos con discapacidad por persona
#' y año, en la misma escala que la prevalencia; las tablas de la corrida los dan como tasa por 100 000 (el valor
#' por persona x 100 000).
#'
#' @inheritParams dl_ajustar
#' @param ajuste Ajuste de [dl_ajustar()] o cascada de [dl_cascada()].
#' @param comorbilidad Factor de [dl_factor_comorbilidad()] (opcional; sin él, los AVD no se corrigen por
#'   comorbilidad y el manifiesto lo declara).
#' @return Objeto de clase `dl_yld`, una lista con:
#'   - `draws_yld`: tabla con los AVD por persona de cada `location_id`, `sex_id`, `age_group_id` (las bandas del
#'     ancla) y simulación (`draw`), en `val`.
#'   - `draws_prev_banda`: la prevalencia de cada banda (proporción), con las mismas columnas.
#'   - `desplazamiento_splits`: por estado de salud (`health_state_id`), la proporción de la tabla de severidad
#'     (`prop_original`), la media de las proporciones renormalizadas (`prop_medio_renorm`) y su diferencia
#'     (`desplazamiento`).
#'   - `como_aplicado`: `TRUE` si se aplicó el factor de comorbilidad.
#'   - `bundle_hash`, `seed` y `huella_ajuste` (identifican los insumos, la semilla y el ajuste de donde salen;
#'     [dl_resumir()] los compara).
#' @seealso [dl_factor_comorbilidad()], [dl_estimaciones()] (`medida = "avd"`), [dl_resumir()] (el paso
#'   siguiente) y [dl_proyecto()] (la tabla de severidad).
#' @family carga
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' # los estados de salud de la causa
#' b$severidad[, c("health_state_id", "proportion", "dw_mean")]
#' y <- dl_avd(f, b, comorbilidad = dl_factor_comorbilidad(b), semilla = 1)
#' y
#' # AVD por persona y año, por ubicación, sexo y banda de edad
#' head(dl_estimaciones(y))
#' }
#' @export
dl_avd <- function(ajuste, insumos, comorbilidad = NULL, semilla) {
  .dl_exigir_semilla(semilla)
  .dl_exigir_clase(ajuste, "dl_fit", "ajuste", "dl_ajustar() o dl_cascada()")
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_exigir_mismos_insumos(ajuste, insumos, "ajuste")
  .dl_exigir_severidad(insumos$severidad, insumos$cfg$cause_id)
  if (!is.null(comorbilidad) && (!is.data.frame(comorbilidad) ||
                                 !all(c("sex_id", "age_group_id", "factor") %in% names(comorbilidad))))
    .dl_stop(paste0("`comorbilidad` debe ser NULL o la tabla de dl_factor_comorbilidad() (columnas sex_id, ",
                    "age_group_id y factor); es %s"), .dl_describir_objeto(comorbilidad))
  if (!is.null(comorbilidad)) comorbilidad <- data.table::as.data.table(comorbilidad)
  sev <- insumos$severidad
  if (any(sev$location_id_fuente != insumos$loc_ancla))
    .dl_stop(paste0("la tabla de severidad viene de una partici\u00f3n de otra ubicaci\u00f3n (location_id_fuente ",
                    "%s) y el ancla es %s; esta versi\u00f3n solo admite particiones de severidad de la ",
                    "ubicaci\u00f3n del ancla"),
             paste(unique(sev$location_id_fuente[sev$location_id_fuente != insumos$loc_ancla]), collapse = ", "),
             insumos$loc_ancla)
  n_por_sexo <- vapply(ajuste$draws_par, nrow, integer(1))
  if (length(unique(n_por_sexo)) != 1L)
    .dl_stop("el ajuste tiene un n\u00famero distinto de simulaciones por sexo (%s)",
             paste(n_por_sexo, collapse = ", "))
  n_sim <- n_por_sexo[[1]]

  # pi_{k,e} y DW_{k,e}: Beta por momentos, pi renormalizada por simulación
  pi_dw <- .dl_simular_pi_dw(sev, n_sim, semilla)
  # Canal de proporción: beta_{k,e} de cada estado con covariable, desde semilla + 1; dX = 0 (cabecera)
  canal <- .dl_estados_canal_proporcion(sev, insumos$betas)
  beta_ke <- .dl_draws_betas(canal$betas, n_sim, semilla + .DL_SALTO_SEMILLA_BETAS)
  pi_ke <- .dl_aplicar_canal_proporcion(pi_dw$pi_ke, canal$estados, beta_ke, dX = rep(0, length(canal$estados)))
  suma_pi_dw <- rowSums(pi_ke * pi_dw$dw_ke)                     # sum_e pi_{k,e} DW_{k,e}: uno por simulación k
  # p_{k,b} por ubicación, sexo y banda; el factor de comorbilidad es el mismo en todas las ubicaciones
  p_bandas <- .dl_q_bandas(ajuste, insumos)
  avd <- .dl_avd_bandas(p_bandas, suma_pi_dw, comorbilidad)
  structure(list(draws_yld = avd, draws_prev_banda = p_bandas, desplazamiento_splits = pi_dw$desplazamiento,
                 como_aplicado = !is.null(comorbilidad), bundle_hash = insumos$hash, seed = semilla,
                 # huella del ajuste o la cascada de donde salen: dl_resumir() y dl_exportar_corrida() la comparan
                 huella_ajuste = .dl_huella_ajuste(ajuste)),
            class = "dl_yld")
}

#' @export
print.dl_yld <- function(x, ...) {
  cat(sprintf("<dl_yld> AVD por simulaci\u00f3n | insumos %s | semilla %s | factor de comorbilidad %s\n",
              substr(x$bundle_hash, 1, 12), x$seed,
              if (x$como_aplicado) "aplicado" else "no aplicado (los AVD quedan sobreestimados)"))
  cat(sprintf(paste0("  ubicaciones: %d | bandas de edad y sexo: %d | simulaciones: %d | desplazamiento medio de ",
                     "las proporciones por la renormalizaci\u00f3n: %.4f\n"),
              data.table::uniqueN(x$draws_yld$location_id),
              nrow(unique(x$draws_yld[, list(sex_id, age_group_id)])),
              max(x$draws_yld$draw), mean(abs(x$desplazamiento_splits$desplazamiento))))
  invisible(x)
}
