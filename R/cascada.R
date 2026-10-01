# Cascada departamental: lleva el ajuste nacional a los departamentos
#
# Notación: ver R/edo.R (S, C, p, i, r, f, m, theta, h, W y las mallas _anual, _malla, _media). Además:
#   k       simulación del ajuste nacional, k = 1..n; theta_k es la fila k de ajuste$draws_par[[sexo]]
#   d       departamento (ubicación de nivel 1); «nac» es el ancla nacional (insumos$loc_ancla)
#   c       covariable con proxy departamental, con su beta beta_c (la del ancla) y su canal: i si su
#           parametro_objetivo es prevalencia o incidencia, f si es emr (.DL_CANAL_BETA)
#   T       transformación de la covariable: log, logit o lineal (identidad)
#   dX      diferencia departamento menos nación de la covariable, en la escala de T, por banda de edad del proxy
#   P       pesos de las bandas del proxy sobre las edades de la malla media (.dl_pesos_edad)
#   kappa   fracción del gradiente departamental que se aplica, en (0, 1]
#   N       población por ubicación, sexo y banda de población
#   q       cantidad que se renormaliza: p, o ipop = i (1 - p) (casos nuevos por persona-año de toda la población)
#
# Para cada sexo y cada simulación k:
#   1. dX (.dl_dX), por departamento, covariable y banda del proxy:
#        X_nac ~ N(T(val), sd_T),  sd_T = (T(upper) - T(lower)) / 3.92      una vez por simulación, la misma para
#                                                                         todos los departamentos y bandas
#        X_d   ~ N(T(valor_calibrado), se_T),   se_T = valor_calibrado_se |T'(valor_calibrado)|   (método delta,
#                                                                         .dl_sd_delta)
#        dX    = (X_d - X_nac) * escala                 escala solo con T lineal (1 con log y logit)
#   2. Desplazamiento en log sobre la malla media (.dl_desplazamiento_log), en cada edad a de esa malla:
#        delta_c(a) = kappa * beta_c * sum_b dX_b P[b, a]
#        log i_d(a) = log i_nac(a) + suma de delta_c(a) sobre las covariables de canal i
#        log f_d(a) = min(log f_nac(a) + suma de delta_c(a) sobre las covariables de canal f, log f_max)
#      con (log i_nac, log f_nac) = W theta_k y f_max = emr_prior.cota[2], el techo de EMR (impreso en la
#      configuración o derivado por dl_insumos()). El mínimo lo aplica .dl_edo_log_media (R/verosimilitud.R).
#   3. Se vuelve a resolver dp/da = i (1 - p) - r p - f p (1 - p) con i_d y f_d (misma r, misma malla).
#   4. Renormalización exacta (.dl_renormalizar), por sexo, banda de población y simulación, de q = p y q = ipop:
#        factor = N_nac q_nac / sum_d N_d q_d,     q_d(a) <- q_d(a) * factor en cada edad a de la banda,
#      de modo que sum_d N_d q_d = N_nac q_nac (q de una banda = promedio simple de sus edades anuales). i y f
#      quedan como salen de la EDO: en los departamentos ipop deja de ser exactamente i (1 - p).
#   5. Comprobación: tras renormalizar, p_d debe quedar en [0, 1] (si no, error; no se trunca); aviso si la mediana
#      departamental de p supera cascada.cota_warning.
#
# Orden del generador aleatorio (fijo: cambiarlo cambia los resultados de cualquier corrida):
#   semilla      .dl_dX: set.seed(semilla); por covariable (orden de covariate_name_short en el locale C) y por
#                sexo (orden de los sexos de la configuración): primero los n valores de X_nac y después los
#                n x J de X_d, con las J filas del proxy ordenadas por (departamento, banda) y n simulaciones
#                seguidas por fila.
#   semilla + 1  .dl_draws_betas (R/betas.R; salto .DL_SALTO_SEMILLA_BETAS): n valores de beta_c por covariable,
#                en el mismo orden.
#   Una sd igual a 0 (covariable sin intervalo, beta fija) no consume números aleatorios: stats::rnorm devuelve la
#   media sin sortear. Los pasos 2 a 5 son deterministas.

# ---- Constantes --------------------------------------------------------------------------------------------------

# Canal de cada beta según su parametro_objetivo: desplaza log i (prevalencia e incidencia; la prevalencia de un
# departamento se mueve a través de su incidencia) o log f (emr). Las betas de «proporcion» no tienen canal en la
# cascada y se descartan.
.DL_CANAL_BETA <- c(prevalencia = "i", incidencia = "i", emr = "f")

# sex_id de «ambos sexos» (convención GBD): el que se usa cuando una covariable o un proxy no vienen por sexo.
.DL_CASCADA_SEXO_AMBOS <- 3L

# Extremo superior (años) con que se calcula el punto medio de una banda del proxy que termina después de esta
# edad. La banda abierta final (p. ej. 70+, age_end = 125 en el catálogo) no tiene un punto medio útil: con 85 el
# de 70+ es 77,5 y la interpolación lineal de dX no se estira hacia edades casi sin población.
.DL_CASCADA_EDAD_CIERRE_BANDA_ABIERTA <- 85

# ---- Función principal -------------------------------------------------------------------------------------------

#' Estimación subnacional (cascada)
#'
#' Lleva el ajuste nacional a las ubicaciones subnacionales (departamentos, regiones, provincias...): desplaza la
#' incidencia y la mortalidad en exceso de cada simulación según la diferencia entre el valor de cada covariable en
#' la ubicación (su proxy) y el nacional, por su beta; vuelve a resolver la ecuación de la prevalencia en cada
#' ubicación y renormaliza para que la suma ponderada por población de las ubicaciones sea igual al valor nacional.
#' Con la estimación subnacional plana (`subnacional.modo: plano`), cada ubicación recibe las tasas nacionales.
#'
#' @details
#' Para cada sexo, simulación y ubicación subnacional d:
#' 1. `dX`: la diferencia entre la covariable en d y la nacional, en la escala de su transformación (log, logit o
#'    lineal), con una simulación de cada una según su incertidumbre.
#' 2. En cada edad, log i (o log f, según `efecto_sobre`) de d es el nacional más `kappa` x beta x dX, sumado sobre
#'    las covariables; log f no pasa del techo de la mortalidad en exceso.
#' 3. Con esas tasas se vuelve a resolver la ecuación de la prevalencia.
#' 4. Renormalización, por sexo, banda de población y simulación: la prevalencia (y la incidencia poblacional) de
#'    cada ubicación se multiplica por el factor que hace que su suma ponderada por población sea la nacional.
#'
#' Las betas se simulan de su intervalo, una vez por simulación. `kappa` (en (0, 1]) aplica una fracción del
#' gradiente: con 1, el gradiente completo; con menos, las ubicaciones quedan más cerca de la nación. Las
#' ecuaciones están en `vignette("el-modelo", package = "dismodlite")`.
#'
#' @inheritParams dl_ajustar
#' @param ajuste Ajuste nacional de [dl_ajustar()].
#' @param insumos Insumos de [dl_insumos()] (los mismos del ajuste).
#' @param kappa Fracción del gradiente subnacional que se aplica, en (0, 1] (\eqn{\kappa}{κ}); por defecto, la de la
#'   configuración (`subnacional.kappa`).
#' @param motor `"mh"` (R) o `"rcpp"` (C++) para resolver la ecuación; por defecto, el del ajuste.
#' @return Objeto de clase `dl_cascade` (también `dl_fit`): el ajuste nacional, con los campos de [dl_ajustar()], y
#'   - `draws_q`: las simulaciones de la nación y de cada ubicación subnacional (las mismas columnas), ya
#'     renormalizadas; `draws_q_sin_renorm`: las de antes de renormalizar.
#'   - `dX`: tabla con la diferencia de cada covariable por simulación (`draw`), `location_id`, `sex_id`,
#'     `age_group_id` (la banda del proxy) y `covariate_name_short`; vacía en la estimación plana.
#'   - `renorm`: tabla con el factor de renormalización (`factor`) por `medida` (`prevalence` o `incidence`),
#'     `sex_id`, `age_group_id` y `draw`.
#'   - `kappa`, `departamentos` (los `location_id` subnacionales; el nombre se conserva de la versión 0.2.2),
#'     `seed_cascada` (la semilla) y `modo` (`"proxy"` o `"plana"`).
#'   - `truncados_emr`: cuántas simulaciones subnacionales (por sexo) tuvieron la mortalidad en exceso truncada al
#'     techo.
#'   - `dx_por_edad`: cómo se repartió dX entre las edades (`fuera_de_banda`, `interpolacion` y `edades_sin_banda`);
#'     `NULL` en la estimación plana.
#'   - `sustituciones`: en el formato completo, las covariables cuyo valor nacional de referencia es el de otra
#'     covariable (`covariables[].sustituye`), con los dos `covariate_id`; `NULL` si ninguna.
#' @seealso [dl_ajustar()] (el paso anterior), [dl_estimaciones()], [dl_avd()] y [dl_validar_ancla()] (los pasos
#'   siguientes) y [dl_proyecto()] (los proxies y su requisito de cierre en el valor nacional).
#' @family subnacional
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' cas <- dl_cascada(f, b, semilla = 1)
#' cas
#' # prevalencia de las ubicaciones subnacionales, por edad (b$loc_ancla es la ubicación nacional)
#' prev <- dl_estimaciones(cas)
#' head(prev[location_id != b$loc_ancla])
#' # con la mitad del gradiente las ubicaciones quedan más cerca de la nación: el rango de la
#' # prevalencia subnacional de los hombres de 60 años
#' rango_60 <- function(x)
#'   range(dl_estimaciones(x)[location_id != b$loc_ancla & sex_id == 1 & edad == 60]$media)
#' rango_60(cas)
#' rango_60(dl_cascada(f, b, kappa = 0.5, semilla = 1))
#' }
#' @export
dl_cascada <- function(ajuste, insumos, kappa = insumos$cfg$cascada$kappa, semilla, motor = ajuste$params$engine) {
  .dl_exigir_semilla(semilla)
  .dl_exigir_clase(ajuste, "dl_fit", "ajuste", "dl_ajustar()")
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  if (inherits(ajuste, "dl_cascade"))
    .dl_stop("el ajuste ya es una cascada; parte del ajuste nacional de dl_ajustar()")
  .dl_exigir_mismos_insumos(ajuste, insumos, "ajuste")
  dominio_kappa <- .DL_DOMINIO_REJILLA$kappa
  if (!.dl_es_numero1(kappa) || !dominio_kappa$dentro(kappa))
    .dl_stop("`kappa` (por defecto, cascada.kappa de la configuraci\u00f3n) debe ser un n\u00famero en %s, pero es %s",
             dominio_kappa$texto, .dl_describir_objeto(kappa))
  motor <- .dl_elegir_motor(motor)
  if (identical(insumos$cfg$cascada$modo$valor, "plana")) return(.dl_cascada_plana(ajuste, insumos, kappa, semilla))

  # Paso 1: proxies, dX y betas (los únicos sorteos; su orden está en la cabecera del archivo)
  proxies <- .dl_proxies(insumos)
  n <- nrow(ajuste$draws_par[[1]])                                       # simulaciones por sexo
  dX <- .dl_dX(insumos, proxies, n, semilla)
  beta <- .dl_draws_betas(proxies, n, semilla + .DL_SALTO_SEMILLA_BETAS)   # n x covariables
  canal <- proxies$canal[match(colnames(beta), proxies$covariate_name_short)]
  solver <- .dl_solver_edo(motor)
  sexos <- as.integer(unlist(insumos$cfg$sexos))
  techo_f <- as.numeric(unlist(insumos$cfg$emr_prior$cota))[2]          # f_max: emr_prior.cota = c(mínimo, máximo)
  dptos <- sort(unique(dX$location_id))

  # Contextos de los insumos (los del ajuste: se comprobó arriba). La cascada usa de ellos W, la malla media, la
  # remisión y nsub, que no dependen de lambda ni de rho.
  ctxs <- lapply(stats::setNames(nm = sexos), function(sexo) .dl_ctx(insumos, sexo))
  edades_media <- ctxs[[1]]$edades_media                                 # malla media (paso h/2), en años

  # Paso 2 (pesos): cada edad de la malla media recibe el dX de las bandas del proxy según
  # cascada.dx_interpolacion (lineal | escalon) y cascada.dx_fuera_de_banda (cero | vecina). La malla y los pesos
  # no dependen del sexo ni del departamento: se calculan una vez por covariable.
  fuera <- insumos$cfg$cascada$dx_fuera_de_banda$valor
  interp <- insumos$cfg$cascada$dx_interpolacion$valor
  P <- lapply(stats::setNames(nm = colnames(beta)), function(covariable)
    .dl_pesos_edad(edades_media, dX[covariate_name_short == covariable]$age_group_id, insumos$bandas_catalogo, fuera,
                   interp))
  sin_banda <- Reduce(`|`, lapply(P, function(P_c) colSums(P_c) == 0))   # edades sin banda en alguna covariable

  # Pasos 2 y 3, por sexo y departamento
  por_departamento <- unlist(lapply(sexos, function(sexo) {
    ctx <- ctxs[[as.character(sexo)]]
    theta <- ajuste$draws_par[[as.character(sexo)]]                      # n x (log i y log f en los nudos)
    log_nac <- lapply(seq_len(nrow(theta)), function(k) .dl_log_tasas_media(theta[k, ], ctx))   # W theta_k
    delta <- lapply(stats::setNames(nm = colnames(beta)), function(covariable)
      .dl_desplazamiento_log(dX[sex_id == sexo & covariate_name_short == covariable], P[[covariable]],
                             beta[, covariable], kappa, length(dptos)))
    lapply(dptos, function(d) {
      # suma de los delta_c del departamento sobre las covariables de cada canal (NULL si ninguna actúa en él)
      delta_log_i <- Reduce(`+`, lapply(delta[canal == "i"], `[[`, d))
      delta_log_f <- Reduce(`+`, lapply(delta[canal == "f"], `[[`, d))
      .dl_resolver_departamento(log_nac, ctx, solver, delta_log_i, delta_log_f, techo_f, d, sexo)
    })
  }), recursive = FALSE)
  truncados <- sum(vapply(por_departamento, `[[`, 0L, "truncados"))
  dq <- rbind(ajuste$draws_q, data.table::rbindlist(lapply(por_departamento, `[[`, "draws_q")))
  if (truncados)
    .dl_warn(paste0("en %d simulaci\u00f3n(es) subnacionales (por sexo) la mortalidad en exceso desplazada ",
                    "super\u00f3 el techo de EMR (emr_prior.cota = %s) y se trunc\u00f3 a \u00e9l"), truncados,
             format(techo_f))

  # Lo que la corrida declara del gradiente por edad (cascada.dx_por_edad) y de las covariables sustitutas
  dx_por_edad <- list(fuera_de_banda = fuera, interpolacion = interp,
                      edades_sin_banda = if (any(sin_banda)) floor(range(edades_media[sin_banda])) else NULL)
  sustituciones <- proxies[!is.na(sustituto), list(covariate_name_short, covariate_id_beta = covariate_id,
                                                   covariate_id_nacional = covariate_id_x)]
  # Pasos 4 y 5
  .dl_ensamblar_cascada(ajuste, insumos, dq, dX, kappa = kappa, semilla = semilla, dptos = dptos,
                        truncados = truncados, modo = "proxy", dx_por_edad = dx_por_edad,
                        sustituciones = sustituciones)
}

# ---- Paso 1: proxies y diferencias de covariable -----------------------------------------------------------------

# Covariables con proxy departamental declarado en cfg$covariables (covariables[].proxy), unidas por
# covariate_name_short a su fila de `betas`: (covariate_name_short, covariate_id_proxy, sustituto, columnas de
# `betas`, covariate_id_x). covariate_id_x es la covariable cuyo valor nacional ancla el proxy y se resta en dX: la
# de la beta o, si la configuración declara covariables[].sustituye, la sustituta (p. ej. la hermana estandarizada
# por edad de una beta por edad). NULL si ninguna covariable declara proxy. Lo usan la cascada (.dl_proxies) y la
# regla proxy_mapea_config (R/reglas.R).
.dl_mapa_proxies <- function(cfg, betas) {
  decl <- Filter(function(cv) !is.null(cv$proxy), cfg$covariables)
  if (!length(decl)) return(NULL)
  mapa <- data.table::rbindlist(lapply(decl, function(cv) data.table::data.table(
    covariate_name_short = cv$covariate_name_short, covariate_id_proxy = as.integer(cv$proxy$covariate_id_proxy),
    sustituto = as.integer(cv$sustituye$covariate_id %||% NA_integer_))))
  mapa <- merge(mapa, betas, by = "covariate_name_short")
  mapa[, covariate_id_x := data.table::fifelse(is.na(sustituto), covariate_id, sustituto)][]
}

# Proxies de la cascada (.dl_mapa_proxies) con la beta de cada covariable (transformación, escala, beta con su
# intervalo y modo) y su canal. Filas ordenadas por covariate_name_short: ese orden fija el del generador aleatorio
# en .dl_dX y en .dl_draws_betas.
.dl_proxies <- function(insumos) {
  px <- .dl_mapa_proxies(insumos$cfg, insumos$betas[, list(covariate_name_short, covariate_id, parametro_objetivo,
                                                           transformacion, escala, beta, beta_lower, beta_upper,
                                                           modo)])
  if (!nrow(insumos$cov_proxy) || is.null(px))
    .dl_stop(paste0("sin proxies subnacionales no hay cascada (la tabla cov_proxy est\u00e1 vac\u00eda o ninguna ",
                    "covariable de la configuraci\u00f3n declara `proxy`). Pasa `proxies` a dl_rutas() (con los ",
                    "datos de ejemplo, dl_rutas_ejemplo(causa, proxies = TRUE)) o usa la cascada plana: ",
                    "cascada.modo {valor: plana, procedencia} en la configuraci\u00f3n"))
  sin_canal <- setdiff(px$parametro_objetivo, c(names(.DL_CANAL_BETA), "proporcion"))
  if (length(sin_canal))
    .dl_stop(paste0("beta sin canal en la cascada (parametro_objetivo \u00ab%s\u00bb): prevalencia e incidencia ",
                    "desplazan la incidencia i, emr desplaza la mortalidad en exceso f"),
             paste(sin_canal, collapse = ", "))
  px <- px[parametro_objetivo != "proporcion"]
  if (!nrow(px))
    .dl_stop("ning\u00fan proxy apunta a un canal de la cascada (parametro_objetivo prevalencia, incidencia o emr)")
  px[, canal := .DL_CANAL_BETA[parametro_objetivo]]
  data.table::setorder(px, covariate_name_short)
  px
}

# Filas de `tabla` del sexo `sexo` o, si la tabla no lo trae por sexo, las de ambos sexos.
.dl_filas_del_sexo <- function(tabla, sexo)
  if (any(tabla$sex_id == sexo)) tabla[sex_id == sexo] else tabla[sex_id == .DL_CASCADA_SEXO_AMBOS]

# sd de T(X) por el método delta, desde el error estándar `se` de X (en la escala natural) y su valor x:
#   log:     sd_T = se / x                (d log x / dx = 1 / x)
#   logit:   sd_T = se / (x (1 - x))      (d logit x / dx = 1 / (x (1 - x)))
#   lineal:  sd_T = se
.dl_sd_delta <- function(x, se, tipo) switch(tipo, log = se / x, logit = se / (x * (1 - x)), lineal = se)

# dX por simulación, departamento, sexo, covariable y banda del proxy, en la escala de T:
#   X_nac[k]    ~ N(T(val), sd_T)        sd_T = (T(upper) - T(lower)) / 3.92 (.dl_sd_transformada, R/betas.R)
#   X_d[k, b]   ~ N(T(valor_calibrado), se_T)   se_T = .dl_sd_delta(valor_calibrado, valor_calibrado_se)
#   dX[k, d, b] = (X_d[k, b] - X_nac[k]) * escala
# X_nac se sortea una vez por simulación (para cada covariable y sexo) y la comparten todos los departamentos y
# bandas. val es el valor nacional de covariate_id_x en el año de ajuste; una tabla de proxies sin edad es la
# banda 22, todas las edades (.dl_proxy_con_banda, R/bandas.R). escala (betas.escala) solo se aplica con T lineal:
# la beta se estimó con la covariable en otra unidad (p. ej. HAQI en 0-1 frente al 0-100 del GHDx); con log el
# factor se cancela en la diferencia y con logit la covariable ya es una proporción.
# Llama a set.seed(semilla); el orden de los sorteos está en la cabecera del archivo. `n`: simulaciones.
# Devuelve una tabla (draw, location_id, sex_id, age_group_id, covariate_name_short, dX).
.dl_dX <- function(insumos, proxies, n, semilla) {
  set.seed(semilla)
  anio <- .dl_anio_ajuste(insumos$cfg)
  dptos <- sort(unique(insumos$cov_proxy$location_id))
  sexos <- as.integer(unlist(insumos$cfg$sexos))
  tabla_proxy <- .dl_proxy_con_banda(insumos$cov_proxy)
  data.table::rbindlist(lapply(seq_len(nrow(proxies)), function(fila) {
    nombre <- proxies$covariate_name_short[fila]
    nac <- insumos$cov_valores[covariate_id == proxies$covariate_id_x[fila] & location_id == insumos$loc_ancla &
                                 year == anio]
    prx <- tabla_proxy[covariate_id_proxy == proxies$covariate_id_proxy[fila]]
    transf <- proxies$transformacion[fila]
    data.table::rbindlist(lapply(sexos, function(sexo) {
      nac_sexo <- .dl_filas_del_sexo(nac, sexo)
      if (nrow(nac_sexo) != 1L)
        .dl_stop(paste0("cov_valores debe tener un \u00fanico valor nacional de %s (covariate_id %d) para el sexo %d ",
                        "en el a\u00f1o %d, y tiene %d"), nombre, proxies$covariate_id_x[fila], sexo, anio,
                 nrow(nac_sexo))
      prx_sexo <- .dl_filas_del_sexo(prx, sexo)
      faltan <- setdiff(dptos, prx_sexo$location_id)
      if (length(faltan))
        .dl_stop("la tabla cov_proxy no tiene el proxy de %s de %s en: %s",
                 nombre, .dl_nombres_sexo(sexo), paste(faltan, collapse = ", "))
      bandas <- sort(unique(prx_sexo$age_group_id))
      if (nrow(prx_sexo) != length(dptos) * length(bandas))
        .dl_stop("en la tabla cov_proxy, las ubicaciones no tienen las mismas bandas de edad para %s (%s)",
                 nombre, .dl_nombres_sexo(sexo))
      prx_sexo <- prx_sexo[order(location_id, age_group_id)]
      x_nac <- stats::rnorm(n, .dl_transformar(nac_sexo$val, transf),
                            .dl_sd_transformada(nac_sexo$lower, nac_sexo$upper, transf))
      escala <- if (transf == "lineal") as.numeric(proxies$escala[fila] %||% 1) else 1
      # Un solo rnorm con la media y la sd de cada fila del proxy repetidas n veces: consume el generador en el
      # mismo orden (fila a fila, n simulaciones cada una) y con la misma aritmética que rnorm(n, mu_j, sd_j) fila
      # por fila.
      J <- nrow(prx_sexo)
      sd_d <- .dl_sd_delta(prx_sexo$valor_calibrado, prx_sexo$valor_calibrado_se, transf)
      x_d <- stats::rnorm(n * J, rep(.dl_transformar(prx_sexo$valor_calibrado, transf), each = n),
                          rep(sd_d, each = n))
      data.table::data.table(draw = rep(seq_len(n), J), location_id = rep(prx_sexo$location_id, each = n),
                             sex_id = sexo, age_group_id = rep(as.integer(prx_sexo$age_group_id), each = n),
                             covariate_name_short = nombre, dX = (x_d - rep(x_nac, J)) * escala)
    }))
  }))
}

# ---- Paso 2: desplazamiento en log por edad ----------------------------------------------------------------------

# Pesos de las bandas de edad del proxy sobre las `edades` de la malla media: matriz P (bandas x edades, filas con
# nombre age_group_id) tal que dX(a) = sum_b dX_b P[b, a]. Cada columna suma 1, o 0 si la edad no tiene banda.
#   interp = "escalon": P[b, a] = 1 si a está en [age_start, age_end) de la banda b (dX constante dentro de ella).
#   interp = "lineal" (por defecto): dX se interpola linealmente entre los puntos medios
#     medio_b = (age_start + min(age_end, 85)) / 2 de bandas consecutivas y es constante entre el borde exterior de la
#     primera (o la última) banda y su punto medio; así la exposición cambia de forma continua con la edad y la
#     incidencia no salta en los cortes de banda. Solo dentro de [age_start de la primera, age_end de la última).
#   fuera = "cero": fuera de las bandas la columna es 0 (dX = 0, como el escalar SEV del GBD donde no define
#     exposición). fuera = "vecina": la banda más próxima en edad (distancia a [age_start, age_end - 1]; en un
#     empate, la más joven).
# Con una sola banda las dos interpolaciones coinciden. `catalogo`: límites de las bandas (insumos$bandas_catalogo).
.dl_pesos_edad <- function(edades, bandas, catalogo, fuera = "cero", interp = "lineal") {
  lim <- .dl_limites_bandas(catalogo, unique(as.integer(bandas)), "del proxy")
  n_bandas <- nrow(lim); columna <- seq_along(edades)
  idx <- .dl_banda_de(edades, lim)                                   # banda que contiene cada edad (NA: ninguna)
  sin_banda <- is.na(idx)
  if (identical(fuera, "vecina") && any(sin_banda)) {
    e <- edades[sin_banda]
    dist <- pmax(outer(e, lim$age_start, function(a, s) s - a), outer(e, lim$age_end - 1, "-"), 0)
    idx[sin_banda] <- max.col(-dist, ties.method = "first")
  }
  P <- matrix(0, n_bandas, length(edades), dimnames = list(lim$age_group_id, NULL))
  ok <- !is.na(idx); P[cbind(idx[ok], columna[ok])] <- 1            # escalón, y el relleno fuera de las bandas
  if (!identical(interp, "escalon") && n_bandas > 1L) {
    medio <- (lim$age_start + pmin(lim$age_end, .DL_CASCADA_EDAD_CIERRE_BANDA_ABIERTA)) / 2
    j <- which(edades >= lim$age_start[1] & edades < lim$age_end[n_bandas])   # entre el primer y el último borde
    a <- pmin(pmax(edades[j], medio[1]), medio[n_bandas])     # constante más allá de los medios extremos
    # izq: banda cuyo punto medio queda a la izquierda de a; w: peso de la banda siguiente
    izq <- pmin(findInterval(a, medio), n_bandas - 1L); w <- (a - medio[izq]) / (medio[izq + 1L] - medio[izq])
    P[, j] <- 0; P[cbind(izq, j)] <- 1 - w; P[cbind(izq + 1L, j)] <- w
  }
  P
}

# Desplazamiento en log de una covariable c para un sexo, por departamento y simulación:
#   delta_c[k, a] = kappa * beta_c[k] * sum_b dX[k, b] * P[b, a]
# dX_c: filas de .dl_dX de ese sexo y esa covariable; P: pesos de .dl_pesos_edad; beta: las n simulaciones de
# beta_c; n_dptos: número de departamentos. Devuelve una lista, con nombre de departamento, de matrices
# n x edades de la malla media (fila k = simulación k).
.dl_desplazamiento_log <- function(dX_c, P, beta, kappa, n_dptos) {
  # tabla ancha con una fila por (departamento, simulación) y una columna por banda del proxy
  ancha <- data.table::dcast(dX_c, location_id + draw ~ age_group_id, value.var = "dX")
  data.table::setorder(ancha, location_id, draw)
  # Filas en orden (departamento d, simulación k), n por departamento: rep(beta, times = n_dptos) pone beta_c[k] en
  # la fila (d, k), y el producto con P da sum_b dX[k, b] P[b, a] en cada edad a de la malla h/2.
  delta <- kappa * rep(beta, times = n_dptos) * (as.matrix(ancha[, rownames(P), with = FALSE]) %*% P)
  split.data.frame(delta, ancha$location_id)
}

# ---- Paso 3: nueva solución de la EDO por departamento -----------------------------------------------------------

# Resuelve la EDO de un departamento y un sexo en cada simulación k, sobre la malla media:
#   log i = log i_nac[k] + delta_log_i[k, ]
#   log f = min(log f_nac[k] + delta_log_f[k, ], log techo_f)       (.dl_edo_log_media, R/verosimilitud.R)
# log_nac: una entrada por simulación con log i_nac y log f_nac = W theta_k (.dl_log_tasas_media); delta_log_i y
# delta_log_f: matrices n x malla media, o NULL si ninguna covariable actúa en ese canal. Devuelve la tabla draws_q
# del departamento y en cuántas simulaciones f tocó el techo de EMR.
.dl_resolver_departamento <- function(log_nac, ctx, solver, delta_log_i, delta_log_f, techo_f, ubicacion, sexo) {
  soluciones <- lapply(seq_along(log_nac), function(k) {
    log_i <- log_nac[[k]]$log_i; log_f <- log_nac[[k]]$log_f
    if (!is.null(delta_log_i)) log_i <- log_i + delta_log_i[k, ]
    if (!is.null(delta_log_f)) log_f <- log_f + delta_log_f[k, ]
    .dl_edo_log_media(log_i, log_f, ctx, solver, techo_f)
  })
  list(draws_q = .dl_tabla_draws_q(soluciones, ubicacion, sexo, edades_anual = ctx$anual),
       truncados = sum(vapply(soluciones, function(s) as.integer(s$truncado), 0L)))
}

# Tabla draws_q de una ubicación y un sexo desde las soluciones de la EDO (una por simulación): filas (draw, edad)
# ordenadas por simulación y, dentro de cada una, por edad, con p, i y f en las edades anuales e ipop = i (1 - p)
# (.dl_integrando, R/esquema.R). La comparten dl_ajustar() y dl_cascada().
.dl_tabla_draws_q <- function(soluciones, ubicacion, sexo, edades_anual) {
  n <- length(soluciones); n_anual <- length(edades_anual)
  columna <- function(nm) unlist(lapply(soluciones, `[[`, nm), use.names = FALSE)
  sol <- list(p = columna("p"), i = columna("i"), f = columna("f"))
  data.table::data.table(location_id = ubicacion, sex_id = sexo, draw = rep(seq_len(n), each = n_anual),
                         edad = rep(edades_anual, n), p = sol$p, i = sol$i, f = sol$f,
                         ipop = .dl_integrando(sol, "ipop"))
}

# ---- Cascada plana -----------------------------------------------------------------------------------------------

# Cascada plana (cascada.modo: plana), para causas cuyo ancla no tiene ninguna covariable con proxy departamental:
# cada departamento (nivel 1 de la tabla de población) recibe las simulaciones nacionales tal cual. Es dX = 0 en
# todas las covariables, así que la EDO no cambia y no hace falta volver a resolverla; los conteos difieren solo
# por la población, y el factor de renormalización es 1 (salvo redondeo) cuando la población departamental suma la
# nacional (regla nivel1_suma_nivel0). Sin gradiente departamental: la corrida lo declara como limitación.
.dl_cascada_plana <- function(ajuste, insumos, kappa, semilla) {
  if (nrow(insumos$cov_proxy))
    .dl_stop(paste0("la cascada plana no usa proxies subnacionales, pero la tabla cov_proxy de los insumos tiene ",
                    "filas; arma los insumos sin `proxies` en dl_rutas() o usa la cascada por proxies"))
  dptos <- sort(unique(insumos$poblacion[location_level == 1L]$location_id))
  if (!length(dptos))
    .dl_stop("la cascada plana necesita ubicaciones subnacionales en la tabla de poblaci\u00f3n")
  nac <- ajuste$draws_q[location_id == insumos$loc_ancla]
  dq <- rbind(ajuste$draws_q,
              data.table::rbindlist(lapply(dptos, function(d) data.table::copy(nac)[, location_id := d])))
  dX <- data.table::data.table(draw = integer(), location_id = character(), sex_id = integer(),
                               age_group_id = integer(), covariate_name_short = character(), dX = numeric())
  .dl_ensamblar_cascada(ajuste, insumos, dq, dX, kappa = kappa, semilla = semilla, dptos = dptos, truncados = 0L,
                        modo = "plana", dx_por_edad = NULL, sustituciones = NULL)
}

# ---- Pasos 4 y 5: renormalización, cotas y objeto ----------------------------------------------------------------

# Renormalización exacta de la columna q (`columna`: "p" o "ipop") de los departamentos, por sexo, banda de
# población y simulación:
#   q_banda = promedio simple de q en las edades anuales de la banda: la banda es fina, todas sus edades pesan N_b,
#             así que es el promedio poblacional de R/bandas.R (w_a = 1/|A|)
#   factor  = N_nac q_nac / sum_d N_d q_d
#   q_d(a) <- q_d(a) * factor en cada edad a de la banda,  de modo que  sum_d N_d q_d = N_nac q_nac
# El ancla no se toca. Las edades fuera de las bandas de población (p. ej. 30-39 con población desde 40) no tienen
# denominador: quedan sin renormalizar (factor 1) y no son celdas de salida. `dq` trae la columna banda (la pone
# .dl_ensamblar_cascada) y se modifica por referencia: es la tabla más grande de la corrida y no se copia.
# Devuelve list(dq, renorm), con renorm = (sex_id, age_group_id, draw, factor).
.dl_renormalizar <- function(dq, insumos, columna) {
  loc_ancla <- insumos$loc_ancla
  pob <- insumos$poblacion[, list(location_id, sex_id, banda = age_group_id, N = val)]
  q_por_banda <- merge(dq[!is.na(banda), list(q = mean(get(columna))), by = list(location_id, sex_id, banda, draw)],
                       pob, by = c("location_id", "sex_id", "banda"))
  # factor = N_nac q_nac / sum_d N_d q_d (sumas en el orden de location_id)
  factores <- q_por_banda[, list(factor = sum(q[location_id == loc_ancla] * N[location_id == loc_ancla]) /
                                          sum(q[location_id != loc_ancla] * N[location_id != loc_ancla])),
                          by = list(sex_id, banda, draw)]
  # factor por referencia (join de actualización), sin copiar la tabla de simulaciones
  dq[factores, on = c("sex_id", "banda", "draw"), factor := i.factor]
  dq[location_id != loc_ancla & !is.na(factor), (columna) := get(columna) * factor]
  dq[, factor := NULL]
  list(dq = dq, renorm = factores[, list(sex_id, age_group_id = banda, draw, factor)])
}

# Cola común de las dos cascadas: renormalización exacta de p y de ipop, comprobación de p en [0, 1], aviso de
# cascada.cota_warning y el objeto dl_cascade (que guarda también las simulaciones sin renormalizar).
.dl_ensamblar_cascada <- function(ajuste, insumos, dq, dX, kappa, semilla, dptos, truncados, modo, dx_por_edad,
                                  sustituciones) {
  sin_renorm <- data.table::copy(dq)
  # banda de población de cada edad, una vez para las dos renormalizaciones; se quita al final
  dq[, banda := insumos$bandas_pobl$age_group_id[.dl_banda_de(edad, insumos$bandas_pobl)]]
  renorm_p <- .dl_renormalizar(dq, insumos, "p")
  renorm_ipop <- .dl_renormalizar(renorm_p$dq, insumos, "ipop")
  dq <- renorm_ipop$dq[, banda := NULL]
  data.table::setcolorder(dq, c("location_id", "sex_id", "draw", "edad", "p", "i", "f", "ipop"))
  data.table::setorder(dq, location_id, sex_id, draw, edad)
  if (any(dq$p < 0 | dq$p > 1))
    .dl_stop("tras la renormalizaci\u00f3n hay prevalencias subnacionales fuera de [0, 1]")
  medianas <- dq[location_id != insumos$loc_ancla, list(med = stats::median(p)), by = list(location_id, sex_id, edad)]
  if (any(medianas$med > insumos$cfg$cascada$cota_warning))
    .dl_warn("la mediana de la prevalencia de alguna ubicaci\u00f3n subnacional supera cascada.cota_warning = %s",
             format(insumos$cfg$cascada$cota_warning))
  renorm <- rbind(renorm_p$renorm[, medida := "prevalence"], renorm_ipop$renorm[, medida := "incidence"])
  data.table::setcolorder(renorm, c("medida", "sex_id", "age_group_id", "draw", "factor"))
  data.table::setorder(renorm, medida, sex_id, age_group_id, draw)
  out <- ajuste
  out$draws_q <- dq; out$draws_q_sin_renorm <- sin_renorm
  out$dX <- dX; out$renorm <- renorm; out$kappa <- kappa; out$departamentos <- dptos
  out$truncados_emr <- truncados; out$seed_cascada <- semilla
  out$modo <- modo; out$dx_por_edad <- dx_por_edad
  out$sustituciones <- if (!is.null(sustituciones) && nrow(sustituciones)) sustituciones else NULL
  class(out) <- c("dl_cascade", "dl_fit")
  out
}

#' @export
print.dl_cascade <- function(x, ...) {
  cat(sprintf(paste0("<dl_cascade> cascada subnacional %s | kappa %.2f | %d ubicaciones subnacionales | factor de ",
                     "renormalizaci\u00f3n [%.3f, %.3f] | simulaciones subnacionales con la EMR truncada al ",
                     "techo: %d\n"),
              if (identical(x$modo, "plana")) "plana" else "por proxies", x$kappa, length(x$departamentos),
              min(x$renorm$factor), max(x$renorm$factor), x$truncados_emr))
  print.dl_fit(x, ...)
}
