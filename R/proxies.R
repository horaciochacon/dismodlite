# Calibración de proxies subnacionales: de un indicador de encuesta por ubicación y edición a las filas subnacionales
# de la tabla covariables, cerradas en el valor nacional (ver ?dl_calibrar_proxies).
#
# Notación
#   d          ubicación subnacional; una serie es (covariable, d, sexo, banda de edad)
#   t          año de una edición de la encuesta; a, el año que se estima
#   p_{d,t}    valor crudo del indicador; se_{d,t} su error estándar
#   N_{d,t}    población de d en el sexo y la banda, del año de `poblacion` más cercano a t
#   C          ubicaciones comunes: las que están en todas las ediciones (no excluidas) de la covariable, el sexo
#              y la banda; con ediciones desbalanceadas, el promedio de referencia no se mueve con la cobertura
#   p̄_t        Σ_{d∈C} N_{d,t} p_{d,t} / Σ_{d∈C} N_{d,t}  (g se calcula para toda d presente en t)
#   g_{d,t}    gradiente: log(p_{d,t} / p̄_t) (cociente) o p_{d,t} − p̄_t (diferencia); se_g su error
#   q          varianza por año del paseo aleatorio del gradiente (en las unidades de g al cuadrado)
#   σ̄²         mediana de se_g² sobre las observaciones (serie, edición) de la covariable con se_g > 0: escala de q
#   ĝ_d, S_d   gradiente suavizado en el año a y su varianza
#   w_d        población de d en el año a, normalizada en el sexo y la banda
#   X          valor nacional de la covariable, o el de otra covariable si `valor_nacional_de` la nombra; X_d el valor
#              calibrado de d
#
# Modelo temporal (nivel local), una serie a la vez:
#   g_t = g_{t-1} + η_t,  η_t ~ N(0, q Δt)          obs_t ~ N(g_t, se_g,t^2)
# con prior difuso: el filtro empieza en la primera observación con media obs_1 y varianza se_g,1^2. q se estima
# por covariable maximizando la suma, sobre sus series, de la log-verosimilitud de predicción de las observaciones
# 2..n de cada serie (la primera fija el prior difuso), en el intervalo relativo
#   q ∈ σ̄² [r_min, r_max]                           (r_min, r_max: .DL_Q_LIMITES_REL)
# relativo porque, con diferencia, g está en las unidades del indicador: el mismo indicador ×100 da q ×10⁴, y un
# intervalo fijo cortaría a uno y no al otro. Con σ̄² como escala, el resultado no depende de las unidades.
# La búsqueda es en log q con tolerancia .DL_Q_TOL_LOG: q no depende del intervalo más allá de ~1e-7 relativo.
#
# Cierre, por sexo y banda (exacto por construcción: Σ_d w_d X_d = X):
#   cociente:    X_d = X exp(ĝ_d) / Σ_d w_d exp(ĝ_d)     se(X_d) = X_d sqrt(S_d)
#   diferencia:  X_d = X + ĝ_d − Σ_d w_d ĝ_d             se(X_d) = sqrt(S_d)
# Aproximaciones declaradas: se(X_d) no incluye la incertidumbre de la normalización ni la de X; se/p es la
# aproximación delta de la escala log; N_{d,t} y w_d usan el año de `poblacion` más cercano (con dos igual de cerca,
# el anterior), también fuera de sus años.
#
# Gradiente del año a, por método:
#   edicion:          ĝ_d = g_{d,t*}, S_d = se_g,t*^2, con t* la edición de la serie más cercana a a (empate: la
#                     anterior); en las series de salida, `usada` marca t*
#   paseo_aleatorio:  ĝ_d, S_d del suavizador RTS en a con el q de la covariable; sin q (todas las series con una sola
#                     edición), lo mismo que edicion, con un aviso

# ---- Constantes ----

# Intervalo de búsqueda de q relativo a σ̄² (cabecera): q / σ̄² ∈ [1e-6, 1e3]. Abajo, la deriva de 1e-6 veces el
# error típico de una edición es un gradiente prácticamente constante; arriba, una deriva por año de más de 30 veces
# ese error (raíz de 1e3) hace que cada edición mande sola. El intervalo absoluto de una covariable: .dl_limites_q().
.DL_Q_LIMITES_REL <- c(1e-6, 1e3)

# Tolerancia de optimize() en log q. Más fina no gana nada: el piso práctico del método de Brent es
# sqrt(.Machine$double.eps) |log q| (unos 1e-7 con |log q| de 5 a 20). Con la tolerancia por defecto
# (.Machine$double.eps^0.25, 1.2e-4) q dependía del intervalo de búsqueda en la quinta cifra.
.DL_Q_TOL_LOG <- 1e-8

# Distancia en log q a un límite del intervalo por debajo de la cual q quedó en el borde. Está entre dos escalas que
# no se tocan: con el óptimo en el borde, optimize() se detiene a unas pocas veces sqrt(eps) |log q| + .DL_Q_TOL_LOG
# de él (menos de 1e-6 aun con |log q| = 50), y un q a menos de 1e-4 en log (0,01 %) del límite no se distingue de
# él en nada que importe. No depende de la tolerancia de optimize().
.DL_Q_BORDE_LOG <- 1e-4

# ---- Gradiente de una edición ----

# g y se_g de las ubicaciones de una edición, sexo y banda: valores `p`, errores `se`, poblaciones `N`. `ref`: las
# ubicaciones (lógico) que forman p̄_t, el conjunto común C; por defecto, todas.
.dl_gradiente_edicion <- function(p, se, N, transformacion, ref = rep(TRUE, length(p))) {
  .dl_validar_transformacion(transformacion)
  if (transformacion == "cociente" && any(p <= 0))
    .dl_stop("la transformaci\u00f3n \u00abcociente\u00bb necesita valores positivos y hay valores iguales o menores que 0; usa la \u00abdiferencia\u00bb")
  pbar <- sum(N[ref] * p[ref]) / sum(N[ref])
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

# El intervalo de búsqueda de q de una covariable (cabecera): σ̄² [r_min, r_max], con σ̄² la mediana de se_g² de
# todas las observaciones de sus `series` (lista de list(t, y, se)) con se_g > 0. Las de se_g = 0 (que
# dl_calibrar_proxies() no deja pasar, pero el filtro sí acepta) no dicen nada de la escala del error y, si fueran
# la mayoría, darían σ̄² = 0 y un intervalo vacío; sin ninguna positiva no hay escala y es un error.
.dl_limites_q <- function(series) {
  se2 <- unlist(lapply(series, `[[`, "se"))^2
  if (!any(se2 > 0))
    .dl_stop("el intervalo de b\u00fasqueda de q necesita alg\u00fan error est\u00e1ndar del gradiente mayor que 0 y no hay ninguno")
  stats::median(se2[se2 > 0]) * .DL_Q_LIMITES_REL
}

# q de una covariable: maximiza la suma de las log-verosimilitudes de predicción de sus `series` (lista de
# list(t, y, se)) en log q, dentro de .dl_limites_q(series); NA sin ninguna serie con dos o más ediciones (no hay
# información sobre la deriva).
.dl_estimar_q <- function(series) {
  utiles <- Filter(function(s) length(s$t) >= 2L, series)
  if (!length(utiles)) return(NA_real_)
  menos_loglik <- function(log_q) -sum(vapply(utiles, function(s)
    .dl_kalman_nivel_local(s$t, s$y, s$se, exp(log_q), max(s$t))$loglik, 0))
  exp(stats::optimize(menos_loglik, log(.dl_limites_q(series)), tol = .DL_Q_TOL_LOG)$minimum)
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

# ---- Entradas ----

# Las tablas leídas con dl_tabla() y las ediciones de `excluir` quitadas de los crudos. Los crudos sin sexo son de
# ambos sexos y sin edades (sin las columnas o con edad_inicio vacía en una fila), de todas las edades (banda 0-125,
# que suma todas las bandas de la población); `con_edades` dice si traían las columnas (las filas que salen las
# llevan solo entonces, vacías en las de todas las edades); avisa si `excluir` nombra ediciones que no están. `ubicaciones`: las subnacionales de
# los crudos, también las de ediciones excluidas; `quitadas`: las (covariable, anio) excluidas, con su motivo;
# `covs`: las covariables de los crudos. `ubicacion_gbd`: la de dl_tabla(), para leer `covariables`.
.dl_leer_entradas_proxies <- function(crudos, covariables, poblacion, excluir, ubicacion_gbd = NULL) {
  cr <- dl_tabla("proxies_crudos", crudos)
  con_edades <- "edad_inicio" %in% names(cr)
  if (!"sexo" %in% names(cr)) data.table::set(cr, j = "sexo", value = "ambos")
  if (!con_edades) data.table::set(cr, j = c("edad_inicio", "edad_fin"), value = list(0, .DL_EDAD_ABIERTA))
  todas <- which(is.na(cr$edad_inicio))
  data.table::set(cr, i = todas, j = c("edad_inicio", "edad_fin"), value = list(0, .DL_EDAD_ABIERTA))
  excluir <- .dl_validar_excluir(excluir)
  fuera <- cr$anio %in% excluir$anio
  if (length(sobran <- setdiff(excluir$anio, cr$anio)))
    .dl_warn("`excluir` nombra ediciones que no est\u00e1n en proxies_crudos (no excluye nada): %s",
             paste(sobran, collapse = ", "))
  quitadas <- unique(cr[fuera, c("covariable", "anio"), with = FALSE])
  data.table::set(quitadas, j = "motivo", value = excluir$motivo[match(quitadas$anio, excluir$anio)])
  list(crudos = cr[!fuera], quitadas = quitadas, ubicaciones = unique(cr$ubicacion), covs = unique(cr$covariable),
       covariables = dl_tabla("covariables", covariables, ubicacion_gbd = ubicacion_gbd),
       poblacion = dl_tabla("poblacion", poblacion), con_edades = con_edades)
}

# `excluir` como data.frame(anio, motivo); vacío si es NULL. Error si no tiene esas columnas, si un año no es un
# entero finito, si una edición no trae motivo o si se repite.
.dl_validar_excluir <- function(excluir) {
  if (is.null(excluir)) return(data.frame(anio = integer(), motivo = character()))
  if (!is.data.frame(excluir) || !all(c("anio", "motivo") %in% names(excluir)) || !is.numeric(excluir$anio) ||
      !all(is.finite(excluir$anio)) || any(excluir$anio != round(excluir$anio)))
    .dl_stop("`excluir` debe ser un data.frame con las columnas anio (a\u00f1os enteros) y motivo: una fila por edici\u00f3n excluida")
  sin <- is.na(excluir$motivo) | !nzchar(trimws(as.character(excluir$motivo)))
  if (any(sin))
    .dl_stop("cada edici\u00f3n excluida lleva su motivo, y no lo trae: %s", paste(excluir$anio[sin], collapse = ", "))
  repetidas <- unique(excluir$anio[duplicated(excluir$anio)])
  if (length(repetidas))
    .dl_stop("`excluir` repite la(s) edici\u00f3n(es) %s: una fila por edici\u00f3n, con su motivo",
             paste(repetidas, collapse = ", "))
  data.frame(anio = as.integer(excluir$anio), motivo = as.character(excluir$motivo))
}

# ---- Validación de los crudos ----

# Las reglas de los crudos que la tabla no ve sola: error_estandar > 0, valores > 0 con cociente, población de cada
# ubicación y bandas que son uniones de bandas de la población. Devuelve la transformación de cada covariable.
.dl_validar_crudos <- function(e, transformacion) {
  cr <- e$crudos
  if (!nrow(cr)) .dl_stop("proxies_crudos no tiene filas")
  sin_ed <- setdiff(e$covs, cr$covariable)
  if (length(sin_ed))
    .dl_stop("proxies_crudos: la(s) covariable(s) %s no tiene(n) ediciones que no est\u00e9n excluidas",
             paste(sin_ed, collapse = ", "))
  mal <- which(!(cr$error_estandar > 0))
  if (length(mal))
    .dl_stop("proxies_crudos: el error_estandar debe ser mayor que 0 y no lo es en %s", .dl_donde_crudos(cr, mal))
  tr <- .dl_transformaciones(transformacion, unique(cr$covariable))
  mal <- which(tr[cr$covariable] == "cociente" & cr$valor <= 0)
  if (length(mal))
    .dl_stop(paste0("la transformaci\u00f3n \u00abcociente\u00bb necesita cada valor mayor que 0, y la covariable %s ",
                    "tiene valores iguales o menores que 0 (%s); para ella usa la \u00abdiferencia\u00bb"),
             cr$covariable[mal[1L]], .dl_donde_crudos(cr, mal))
  sin_pob <- setdiff(unique(cr$ubicacion), e$poblacion$ubicacion)
  if (length(sin_pob))
    .dl_stop("proxies_crudos: la(s) ubicaci\u00f3n(es) %s no tiene(n) poblaci\u00f3n; agr\u00e9gala(s) a la tabla poblacion",
             paste(sin_pob, collapse = ", "))
  for (sx in unique(cr$sexo)) if (!nrow(.dl_poblacion_sexo(e$poblacion, sx)))
    .dl_stop("la poblaci\u00f3n no trae %s, que piden proxies_crudos (covariable(s) %s)",
             if (sx == "ambos") "ambos sexos (ni hombres y mujeres para sumarlos)" else sx,
             paste(unique(cr$covariable[cr$sexo == sx]), collapse = ", "))
  bandas <- unique(cr[, c("covariable", "sexo", "edad_inicio", "edad_fin"), with = FALSE])
  for (i in seq_len(nrow(bandas))) .dl_validar_banda_crudos(bandas[i], e$poblacion)
  tr
}

# Filas `i` de los crudos en un mensaje: «A en 2019, B en 2021» (como mucho 3).
.dl_donde_crudos <- function(cr, i)
  paste0(paste(utils::head(sprintf("%s en %d", cr$ubicacion[i], cr$anio[i]), 3L), collapse = ", "),
         if (length(i) > 3L) sprintf(" y %d m\u00e1s", length(i) - 3L) else "")

# La transformación de cada covariable de `covs`: la de `transformacion` (vector o lista con nombres) o «cociente».
.dl_transformaciones <- function(transformacion, covs) {
  tr <- stats::setNames(rep("cociente", length(covs)), covs)
  if (!length(transformacion)) return(tr)
  if (is.list(transformacion)) transformacion <- unlist(transformacion)
  if (!is.character(transformacion) || is.null(names(transformacion)) || any(!nzchar(names(transformacion))))
    .dl_stop("`transformacion` debe tener un nombre por valor: covariable = \u00abcociente\u00bb o \u00abdiferencia\u00bb")
  fuera <- setdiff(names(transformacion), covs)
  if (length(fuera))
    .dl_stop("`transformacion` nombra covariables que no est\u00e1n en proxies_crudos: %s (las de los crudos: %s)",
             paste(fuera, collapse = ", "), paste(covs, collapse = ", "))
  for (x in transformacion) .dl_validar_transformacion(x)
  tr[names(transformacion)] <- transformacion
  tr
}

# Banda en los mensajes: «todas las edades» o como .dl_nombre_banda() («10-49 años», «80 años y más»).
.dl_texto_banda_crudos <- function(a0, a1)
  if (a0 == 0 && a1 == .DL_EDAD_ABIERTA) "todas las edades" else .dl_nombre_banda(a0, a1)

# Error si la banda [a0, a1) de `b` (una fila: covariable, sexo, edad_inicio, edad_fin) no es una unión de bandas de
# la población de su sexo: ninguna banda de la población la cruza y las que caen dentro la cubren entera. Todas las
# edades (0-125: sin edades o la banda de 0 y más) es la suma de todas las bandas de la población, empiece donde
# empiece.
.dl_validar_banda_crudos <- function(b, pob) {
  f <- .dl_poblacion_sexo(pob, b$sexo)
  if (b$edad_inicio == 0 && b$edad_fin == .DL_EDAD_ABIERTA && nrow(f)) return(invisible())
  B <- unique(f[, c("edad_inicio", "edad_fin"), with = FALSE])
  dentro <- B$edad_inicio >= b$edad_inicio & B$edad_fin <= b$edad_fin
  cruza <- B$edad_inicio < b$edad_fin & B$edad_fin > b$edad_inicio & !dentro
  if (any(dentro) && !any(cruza) &&
      sum(B$edad_fin[dentro] - B$edad_inicio[dentro]) == b$edad_fin - b$edad_inicio) return(invisible())
  B <- B[order(B$edad_inicio)]
  .dl_stop(paste0("la banda %s de proxies_crudos (covariable %s, sexo %s) no es una uni\u00f3n de bandas de la ",
                  "poblaci\u00f3n; las de la poblaci\u00f3n de ese sexo son: %s"),
           .dl_texto_banda_crudos(b$edad_inicio, b$edad_fin), b$covariable, b$sexo,
           if (nrow(B)) paste(.dl_nombre_banda(B$edad_inicio, B$edad_fin), collapse = ", ") else "ninguna")
}

# ---- Población ----

# Filas de la población del sexo `s`: las de ese sexo o, para «ambos» si la población no lo trae, las de hombres y
# mujeres (que se suman). Sin filas si no hay ninguna de las dos formas.
.dl_poblacion_sexo <- function(pob, s) {
  f <- pob[pob$sexo == s]
  if (!nrow(f) && s == "ambos" && all(c("hombres", "mujeres") %in% pob$sexo)) f <- pob[pob$sexo != "ambos"]
  f
}

# El año de `anios` más cercano a cada `t`; con dos igual de cerca, el anterior.
.dl_anio_mas_cercano <- function(t, anios) {
  anios <- sort(unique(as.integer(anios)))
  vapply(t, function(x) anios[which.min(abs(anios - x))], 0L, USE.NAMES = FALSE)
}

# N de cada ubicación de `u` en el sexo `s` y la banda [a0, a1) en el año `a`: la suma de las bandas de la población
# que caen en ella (y de los dos sexos, para «ambos» sin filas de ambos). Error si a una ubicación le falta.
.dl_poblacion_banda <- function(pob, u, s, a0, a1, a) {
  f <- .dl_poblacion_sexo(pob, s)
  f <- f[f$anio == a & f$edad_inicio >= a0 & f$edad_fin <= a1]
  falta <- setdiff(u, f$ubicacion)
  if (length(falta))
    .dl_stop("la ubicaci\u00f3n %s no tiene poblaci\u00f3n en %d (sexo %s, %s)", paste(falta, collapse = ", "), a, s,
             .dl_texto_banda_crudos(a0, a1))
  vapply(u, function(x) sum(f$poblacion[f$ubicacion == x]), 0, USE.NAMES = FALSE)
}

# ---- Gradientes ----

# Columnas que identifican una serie.
.DL_COLS_SERIE <- c("covariable", "ubicacion", "sexo", "edad_inicio", "edad_fin")

# g y se_g de cada fila de los crudos, por covariable, sexo y banda (.dl_gradientes_banda), con N del año de la
# población más cercano a la edición (`anio_poblacion`). `metodo`: el temporal, para el aviso de un panel desbalanceado.
.dl_gradientes <- function(cr, pob, tr, metodo) {
  cr <- data.table::copy(cr)
  data.table::set(cr, j = "anio_poblacion", value = .dl_anio_mas_cercano(cr$anio, pob$anio))
  partes <- lapply(split(cr, by = c("covariable", "sexo", "edad_inicio", "edad_fin")), function(x)
    .dl_gradientes_banda(x, pob, tr[[x$covariable[1L]]], metodo))
  data.table::rbindlist(partes)
}

# Los gradientes de una covariable, sexo y banda (`x`): p̄_t sobre las ubicaciones comunes C (cabecera), con los
# pesos de la población de cada edición; g para toda ubicación presente. Avisa qué falta si las ediciones no traen las
# mismas ubicaciones; error si C tiene menos de dos.
.dl_gradientes_banda <- function(x, pob, transformacion, metodo) {
  comunes <- .dl_ubicaciones_comunes(x, metodo)
  partes <- lapply(split(x, by = "anio"), function(y) {
    N <- .dl_poblacion_banda(pob, y$ubicacion, y$sexo[1L], y$edad_inicio[1L], y$edad_fin[1L], y$anio_poblacion[1L])
    gr <- .dl_gradiente_edicion(y$valor, y$error_estandar, N, transformacion, ref = y$ubicacion %in% comunes)
    data.table::data.table(y[, c(.DL_COLS_SERIE, "anio", "anio_poblacion"), with = FALSE], g = gr$g, se_g = gr$se_g)
  })
  data.table::rbindlist(partes)
}

# C: las ubicaciones de `x` (una covariable, sexo y banda) presentes en todas sus ediciones. Avisa con los pares
# (ubicación, edición) que faltan y qué se hace con ellos según el `metodo` temporal; error si quedan menos de dos.
.dl_ubicaciones_comunes <- function(x, metodo) {
  ediciones <- sort(unique(x$anio)); ubicaciones <- sort(unique(x$ubicacion))
  presentes <- table(factor(x$ubicacion, ubicaciones))
  comunes <- names(presentes)[presentes == length(ediciones)]
  que <- sprintf("covariable %s (sexo %s, %s)", x$covariable[1L], x$sexo[1L],
                 .dl_texto_banda_crudos(x$edad_inicio[1L], x$edad_fin[1L]))
  if (length(comunes) < 2L)
    .dl_stop(paste0("solo %d ubicaci\u00f3n(es) de la %s est\u00e1(n) en todas las ediciones (%s), y el promedio ",
                    "de referencia de cada edici\u00f3n necesita al menos dos; excluye (`excluir`) las ediciones con ",
                    "menos ubicaciones"), length(comunes), que, paste(ediciones, collapse = ", "))
  if (length(comunes) < length(ubicaciones)) {
    todos <- expand.grid(ubicacion = ubicaciones, anio = ediciones, stringsAsFactors = FALSE)
    faltan <- todos[!paste(todos$ubicacion, todos$anio) %in% paste(x$ubicacion, x$anio), ]
    con_faltan <- if (metodo == "edicion") "donde falta una ubicaci\u00f3n se usa su edici\u00f3n m\u00e1s cercana"
                   else "las que faltan se interpolan con sus otras ediciones"
    .dl_warn(paste0("las ediciones de la %s no traen las mismas ubicaciones (faltan: %s); el promedio de referencia ",
                    "de cada edici\u00f3n usa solo las que est\u00e1n en todas (%s), y %s"), que,
             .dl_donde_crudos(faltan, seq_len(nrow(faltan))), paste(comunes, collapse = ", "), con_faltan)
  }
  comunes
}

# ---- Gradiente del año que se estima ----

# ĝ y S en `anio` de cada serie de una covariable (`s`: sus gradientes por edición) con el método temporal (cabecera).
# Devuelve list(series: `s` con g_suavizado, S y usada en cada edición; estimado: una fila por serie con ĝ (g), S y
# las ediciones que entran; q; q_en_borde).
.dl_gradiente_del_anio <- function(s, anio, metodo) {
  covariable <- s$covariable[1L]
  por_serie <- split(s, by = .DL_COLS_SERIE)
  q <- NA_real_
  series <- lapply(por_serie, function(x) list(t = x$anio, y = x$g, se = x$se_g))
  if (metodo == "paseo_aleatorio") {
    q <- .dl_estimar_q(series)
    if (is.na(q))
      .dl_warn(paste0("covariable %s: todas sus series tienen una sola edici\u00f3n, as\u00ed que q no se puede ",
                      "estimar; paseo_aleatorio usa la edici\u00f3n de cada serie (como metodo = \"edicion\")"),
               covariable)
  }
  partes <- lapply(por_serie, function(x) if (is.na(q)) .dl_serie_edicion(x, anio) else .dl_serie_suavizada(x, anio, q))
  list(series = data.table::rbindlist(lapply(partes, `[[`, "series")),
       estimado = data.table::rbindlist(lapply(partes, `[[`, "estimado")),
       q = q, q_en_borde = .dl_q_en_borde(q, .dl_limites_q(series), covariable))
}

# Una serie `x` con el método edicion: la edición t* más cercana a `anio` (empate: la anterior); ĝ = g_t*, S = se_g^2.
.dl_serie_edicion <- function(x, anio) {
  t_usada <- .dl_anio_mas_cercano(anio, x$anio)
  series <- data.table::data.table(x, g_suavizado = x$g, S = x$se_g^2, usada = x$anio == t_usada)
  list(series = series, estimado = data.table::data.table(x[1L, .DL_COLS_SERIE, with = FALSE], g = x$g[series$usada],
                                                          S = x$se_g[series$usada]^2, ediciones = t_usada))
}

# Una serie `x` con el paseo aleatorio de varianza q: el suavizador RTS en cada edición (para mirar la serie) y en
# `anio` (ĝ, S). Todas las ediciones entran.
.dl_serie_suavizada <- function(x, anio, q) {
  en <- function(a) .dl_suavizar_serie(x$anio, x$g, x$se_g, q, a)
  cada <- lapply(x$anio, en)
  series <- data.table::data.table(x, g_suavizado = vapply(cada, `[[`, 0, "g"), S = vapply(cada, `[[`, 0, "S"),
                                   usada = TRUE)
  r <- en(anio)
  list(series = series, estimado = data.table::data.table(x[1L, .DL_COLS_SERIE, with = FALSE], g = r$g, S = r$S,
                                                          ediciones = paste(sort(x$anio), collapse = ", ")))
}

# ¿En qué borde de su intervalo de búsqueda (`limites`, de .dl_limites_q) quedó q? "inferior", "superior" o NA
# (dentro del intervalo, o sin q). En el inferior lo informa (un gradiente estable es un resultado, no un problema);
# en el superior avisa (el suavizado no aporta).
.dl_q_en_borde <- function(q, limites, covariable) {
  if (is.na(q)) return(NA_character_)
  d <- abs(log(q) - log(limites))
  if (d[1L] < .DL_Q_BORDE_LOG)
    .dl_message(paste0("covariable %s: q qued\u00f3 en el borde inferior de su intervalo de b\u00fasqueda (%g, ",
                    "%g veces la mediana de se_g\u00b2): el gradiente es pr\u00e1cticamente constante y el suavizado ",
                    "da la media de las ediciones ponderada por 1/se\u00b2"), covariable, limites[1L],
                .DL_Q_LIMITES_REL[1L])
  if (d[2L] < .DL_Q_BORDE_LOG)
    .dl_warn(paste0("covariable %s: q qued\u00f3 en el borde superior de su intervalo de b\u00fasqueda (%g, ",
                    "%g veces la mediana de se_g\u00b2): cada edici\u00f3n manda y el suavizado casi no aporta (el ",
                    "resultado es casi el de la edici\u00f3n m\u00e1s cercana)"), covariable, limites[2L],
             .DL_Q_LIMITES_REL[2L])
  c("inferior", "superior", NA_character_)[match(TRUE, c(d < .DL_Q_BORDE_LOG, TRUE))]
}

# ---- Cierre y filas ----

# X: el valor nacional de `covariable` en `anio_nacional` para el sexo `s` y la banda [a0, a1). Son nacionales las
# filas sin ubicacion o con una que no es subnacional de los crudos (`subnacionales`); sirven las del mismo sexo o de
# ambos y de la misma banda o de todas las edades, y gana la que coincide en más. Error si falta o si empatan varias.
# `de`: la covariable de los crudos que cierra en ese valor, si es otra (valor_nacional_de); solo para los mensajes.
.dl_valor_nacional <- function(cv, covariable, anio_nacional, s, a0, a1, subnacionales, de = covariable) {
  col <- function(nombre, defecto) if (nombre %in% names(cv)) cv[[nombre]] else rep(defecto, nrow(cv))
  ubi <- col("ubicacion", NA_character_); an <- col("anio", NA_integer_); sx <- col("sexo", "ambos")
  e0 <- col("edad_inicio", NA_real_); e1 <- col("edad_fin", NA_real_)
  todas <- is.na(e0) | (e0 == 0 & e1 == .DL_EDAD_ABIERTA)
  banda_igual <- (!is.na(e0) & e0 == a0 & e1 == a1) | (todas & a0 == 0 & a1 == .DL_EDAD_ABIERTA)
  sirve <- cv$covariable == covariable & (is.na(ubi) | !ubi %in% subnacionales) &
    (is.na(an) | an == anio_nacional) & sx %in% c(s, "ambos") & (banda_igual | todas)
  coincide <- (sx == s) + banda_igual
  k <- which(sirve & coincide == max(-1L, coincide[sirve]))
  que <- sprintf("en %d (sexo %s, %s)", anio_nacional, s, .dl_texto_banda_crudos(a0, a1))
  cual <- if (identical(de, covariable)) covariable else sprintf("%s (valor_nacional_de de %s)", covariable, de)
  if (!length(k))
    .dl_stop(paste0("la covariable %s no tiene valor nacional %s: agr\u00e9galo a covariables en una fila sin ",
                    "ubicacion, del mismo sexo o de ambos y de la misma banda o de todas las edades"), cual, que)
  if (length(k) > 1L)
    .dl_stop("la covariable %s tiene %d filas que sirven igual como valor nacional %s (ubicacion: %s): deja una",
             cual, length(k), que, paste(ifelse(is.na(ubi[k]), "vac\u00eda", ubi[k]), collapse = ", "))
  cv$valor[k]
}

# La covariable que da el valor nacional de cada covariable de los crudos (vector con nombres, covariable de los
# crudos -> covariable de `covariables`): la de `valor_nacional_de` (vector o lista con nombres) o ella misma. Error
# si no tiene un nombre por valor, si repite una covariable, si nombra una que no está en los crudos o si el valor
# nacional es de una covariable que no está en `covariables`.
.dl_nacional_de <- function(valor_nacional_de, e) {
  de <- stats::setNames(e$covs, e$covs)
  if (!length(valor_nacional_de)) return(de)
  if (is.list(valor_nacional_de)) valor_nacional_de <- unlist(valor_nacional_de)
  nombres <- names(valor_nacional_de)
  if (!is.character(valor_nacional_de) || is.null(nombres) || any(!nzchar(nombres)) || anyNA(valor_nacional_de))
    .dl_stop(paste0("`valor_nacional_de` debe tener un nombre por valor: covariable de proxies_crudos = la ",
                    "covariable de `covariables` que da su valor nacional"))
  if (anyDuplicated(nombres))
    .dl_stop("`valor_nacional_de` repite la covariable %s: un solo valor nacional por covariable",
             paste(unique(nombres[duplicated(nombres)]), collapse = ", "))
  fuera <- setdiff(nombres, e$covs)
  if (length(fuera))
    .dl_stop("`valor_nacional_de` nombra covariables que no est\u00e1n en proxies_crudos: %s (las de los crudos: %s)",
             paste(fuera, collapse = ", "), paste(e$covs, collapse = ", "))
  sin <- which(!valor_nacional_de %in% e$covariables$covariable)
  if (length(sin))
    .dl_stop("`valor_nacional_de` dice que el valor nacional de %s es el de %s, que no est\u00e1 en covariables",
             nombres[sin[1L]], valor_nacional_de[[sin[1L]]])
  de[nombres] <- valor_nacional_de
  de
}

# Error, antes de calcular nada, si a una covariable, sexo y banda de los crudos le falta su valor nacional (o tiene
# más de uno que sirve igual): .dl_valor_nacional() en cada una, con la covariable que lo da (e$nacional_de).
.dl_validar_nacionales <- function(e, anio_nacional) {
  b <- unique(e$crudos[, c("covariable", "sexo", "edad_inicio", "edad_fin"), with = FALSE])
  for (i in seq_len(nrow(b)))
    .dl_valor_nacional(e$covariables, e$nacional_de[[b$covariable[i]]], anio_nacional, b$sexo[i], b$edad_inicio[i],
                       b$edad_fin[i], e$ubicaciones, de = b$covariable[i])
  invisible()
}

# Filas calibradas de una covariable desde `est` (.dl_gradiente_del_anio()$estimado): por sexo y banda, X (el valor
# nacional de la covariable que lo da, e$nacional_de), los pesos w (población del año de `poblacion` más cercano a
# `anio`) y .dl_cerrar_proxies(). `fuente`: el texto delante de las ediciones de cada serie.
.dl_filas_calibradas <- function(est, e, tr, anio, anio_nacional, fuente) {
  anio_w <- .dl_anio_mas_cercano(anio, e$poblacion$anio)
  data.table::rbindlist(lapply(split(est, by = c("sexo", "edad_inicio", "edad_fin")), function(x) {
    X <- .dl_valor_nacional(e$covariables, e$nacional_de[[x$covariable[1L]]], anio_nacional, x$sexo[1L],
                            x$edad_inicio[1L], x$edad_fin[1L], e$ubicaciones, de = x$covariable[1L])
    w <- .dl_poblacion_banda(e$poblacion, x$ubicacion, x$sexo[1L], x$edad_inicio[1L], x$edad_fin[1L], anio_w)
    cl <- .dl_cerrar_proxies(x$g, x$S, w, X, tr)
    data.table::data.table(ubicacion = x$ubicacion, anio = as.integer(anio), sexo = x$sexo,
                           edad_inicio = x$edad_inicio, edad_fin = x$edad_fin, covariable = x$covariable,
                           valor = cl$valor, error_estandar = cl$se,
                           fuente = sprintf("%s, ediciones %s)", fuente, x$ediciones))
  }))
}

# Una covariable de principio a fin: gradiente del año, filas cerradas y su fila de `calibracion` (valor_nacional_de:
# la covariable que dio el valor nacional, si es otra). `gr`: sus gradientes (.dl_gradientes()).
.dl_calibrar_covariable <- function(gr, e, tr, anio, anio_nacional, metodo) {
  cov <- gr$covariable[1L]
  r <- .dl_gradiente_del_anio(gr, anio, metodo)
  ind <- unique(stats::na.omit(e$crudos$indicador[e$crudos$covariable == cov]))
  fuente <- sprintf("%s: calibrado (%s%s", if (length(ind)) paste(ind, collapse = " / ") else cov, metodo,
                    if (is.na(r$q)) "" else sprintf(", q = %s", format(signif(r$q, 3L))))
  ed <- unique(gr[, c("anio", "anio_poblacion"), with = FALSE])
  ed <- ed[order(ed$anio)]
  exc <- e$quitadas[e$quitadas$covariable == cov]
  exc <- exc[order(exc$anio)]
  calibracion <- data.table::data.table(
    covariable = cov, metodo = metodo, transformacion = tr, q = r$q, q_en_borde = r$q_en_borde,
    ediciones = paste(sort(unique(r$series$anio[r$series$usada])), collapse = ", "),
    excluidas = paste(sprintf("%d (%s)", exc$anio, exc$motivo), collapse = "; "),
    anios_poblacion = sprintf("ediciones: %s; cierre: %d \u2192 %d",
                              paste(sprintf("%d \u2192 %d", ed$anio, ed$anio_poblacion), collapse = ", "),
                              as.integer(anio), .dl_anio_mas_cercano(anio, e$poblacion$anio)),
    valor_nacional_de = if (identical(e$nacional_de[[cov]], cov)) NA_character_ else e$nacional_de[[cov]])
  list(filas = .dl_filas_calibradas(r$estimado, e, tr, anio, anio_nacional, fuente),
       series = r$series[, c(.DL_COLS_SERIE, "anio", "g", "se_g", "g_suavizado", "S", "usada"), with = FALSE],
       calibracion = calibracion)
}

# ---- La función exportada ----

#' Calibrar proxies subnacionales desde una encuesta
#'
#' Convierte un indicador de encuesta ya agregado por ubicación subnacional y edición (la tabla `proxies_crudos`)
#' en las filas subnacionales de la tabla `covariables` para el año que se estima: cada ubicación conserva su
#' posición relativa en la encuesta y el promedio ponderado por la población cierra exactamente en el valor
#' nacional de la covariable, que sigue en `covariables`.
#'
#' @details
#' **Gradiente de cada edición.** Por covariable, sexo, banda de edad y edición t, el gradiente de la ubicación d
#' compara su valor con \eqn{\bar p_t}{pbar_t}, el promedio de esa edición ponderado por la población del año de `poblacion` más
#' cercano a t (con dos igual de cerca, el anterior). \eqn{\bar p_t}{pbar_t} se toma sobre las ubicaciones **comunes**, las que están
#' en todas las ediciones: así la referencia no se mueve cuando una edición no trae todas las ubicaciones. Si las
#' ediciones no traen las mismas ubicaciones, la función avisa cuáles faltan; si menos de dos están en todas, es un
#' error (excluye las ediciones con menos ubicaciones).
#' - `cociente`: \eqn{g = \log(p / \bar p_t)}{g = log(p / pbar_t)}, con error se / p (aproximación delta de la escala log). Para indicadores
#'   positivos que se comparan en proporción (prevalencias, tasas); exige valores mayores que 0.
#' - `diferencia`: \eqn{g = p - \bar p_t}{g = p - pbar_t}, con error se. Para índices en los que importa la distancia en puntos (como un
#'   índice de 0 a 100) o indicadores que pueden valer 0.
#'
#' **Gradiente del año que se estima.**
#' - `edicion`: el de la edición del año o, si no hay, el de la más cercana (con dos igual de cerca, la anterior),
#'   con su varianza se_g².
#' - `paseo_aleatorio`: cada serie (covariable, ubicación, sexo y banda) sigue un paseo aleatorio,
#'   \eqn{g_t = g_{t-1} + \eta_t}{g_t = g_(t-1) + eta_t} con \eqn{\eta_t \sim N(0, q \Delta t)}{eta_t ~ N(0, q dt)},
#'   observado con su error. Un filtro de Kalman y un suavizador RTS dan
#'   el gradiente en el año que se estima, haya o no edición ese año, usando todas las ediciones de la serie. q, la
#'   varianza por año del gradiente, se estima por máxima verosimilitud, una por covariable con todas sus series.
#'
#' **Cómo leer q.** Su raíz es cuánto se mueve el gradiente en un año (en log con `cociente`, en las unidades del
#' indicador con `diferencia`). Con q muy chico el gradiente es casi constante y el resultado es la media de las
#' ediciones ponderada por 1/se²; con q grande cada edición manda y el resultado se acerca al de `edicion`. q se busca
#' entre \eqn{10^{-6}\bar\sigma^2}{1e-6 sigma2} y \eqn{10^{3}\bar\sigma^2}{1e3 sigma2}, con
#' \eqn{\bar\sigma^2}{sigma2} la mediana de se_g² de la covariable: un intervalo relativo al error de las ediciones,
#' así que el resultado no depende de las unidades del indicador (con `diferencia`, el mismo índice multiplicado por
#' 100 da los mismos valores por 100 y q por \eqn{10^4}{10^4}). Si q queda
#' en un borde de ese intervalo, `calibracion` dice cuál (`q_en_borde`: `"inferior"` o `"superior"`): en el
#' inferior (gradiente estable) la función lo informa con un mensaje; en el superior (cada edición manda) avisa.
#' Con pocas ediciones la verosimilitud de q es plana: q puede moverse órdenes de magnitud con una edición más o
#' menos. Es el grado de suavizado que eligen estos datos, no una propiedad estable del indicador: no lo compares
#' entre encuestas ni lo leas por sí solo. Si
#' todas las series de una covariable tienen una sola edición, q no se puede estimar: se usa la edición de cada serie, con un
#' aviso, y `q` es `NA`. Una serie con una sola edición entre otras que sí estiman q coincide con `edicion` solo en
#' el año de esa edición; en otro año su varianza suma \eqn{q |\Delta t|}{q |dt|}.
#'
#' **Cierre.** Por sexo y banda, con w_d la población de cada ubicación en el año de `poblacion` más cercano a `anio`
#' y X el valor nacional:
#' - `cociente`: \eqn{X_d = X e^{\hat g_d} / \sum_d w_d e^{\hat g_d}}{X_d = X exp(ghat_d) / sum_d w_d exp(ghat_d)}
#'   (con los w_d normalizados para sumar 1), con error \eqn{X_d \sqrt{S_d}}{X_d sqrt(S_d)};
#' - `diferencia`: \eqn{X_d = X + \hat g_d - \sum_d w_d \hat g_d}{X_d = X + ghat_d - sum_d w_d ghat_d}, con error
#'   \eqn{\sqrt{S_d}}{sqrt(S_d)}.
#'
#' \eqn{\sum_d w_d X_d / \sum_d w_d = X}{sum_d w_d X_d / sum_d w_d = X} por construcción. El valor nacional es la fila de `covariables` de la covariable sin
#' `ubicacion` (o con una que no está en los crudos), del año `anio_nacional`, del mismo sexo o de ambos y de la misma
#' banda o de todas las edades; gana la que coincide en más.
#'
#' **El valor nacional de otra covariable.** A veces el valor nacional con que se compara una covariable es el de
#' otra: una beta estimada por edad cuyos valores subnacionales se comparan con la versión estandarizada por edad de
#' la misma covariable. `valor_nacional_de` lo dice, covariable de los crudos = covariable de `covariables`: X es
#' entonces el valor nacional de la covariable nombrada, que se busca con las mismas reglas, y la propia no necesita
#' fila nacional. Las filas que salen llevan el nombre de la covariable de los crudos. En un proyecto es la columna
#' `valor_nacional_de` de la tabla `betas` ([dl_tablas]), y [dl_proyecto()] la pasa a la calibración.
#'
#' **Aproximaciones declaradas.** El error de X_d no incluye la incertidumbre de la normalización ni la del valor
#' nacional; se/p es la aproximación delta; la población de cada edición (y la del cierre) es la del año más cercano
#' de `poblacion`, también fuera de sus años (`calibracion$anios_poblacion` dice cuál se usó). Una ubicación que falta
#' en una edición se interpola con sus otras ediciones (con `edicion`, se usa su edición más cercana).
#'
#' @param crudos Tabla `proxies_crudos` (ver [dl_tablas]): `data.frame` o ruta de un CSV o una carpeta.
#' @param covariables Tabla `covariables`, con el valor nacional de cada covariable de los crudos (o de la que
#'   nombra `valor_nacional_de`).
#' @param poblacion Tabla `poblacion` de las ubicaciones de los crudos; sus bandas de edad (y sus sexos, para «ambos»)
#'   deben poder sumarse en las de los crudos. Una fila de los crudos sin edades (o sin las columnas de edad, o con
#'   la banda de 0 y más) es de todas las edades: su población es la de todas las bandas de `poblacion`, empiecen
#'   donde empiecen.
#' @param anio Año que se estima: el de las filas que salen.
#' @param metodo Método temporal: `"paseo_aleatorio"` (por defecto) o `"edicion"`.
#' @param transformacion Vector (o lista) con nombres, covariable = `"cociente"` o `"diferencia"`. Las covariables que
#'   no nombra usan `"cociente"`.
#' @param excluir `data.frame` con `anio` y `motivo`: ediciones que no entran. Cada una necesita su motivo.
#' @param anio_nacional Año del valor nacional que cierra las filas. Por defecto, `anio`.
#' @param ubicacion_gbd `location_id` de GBD del país, para leer `covariables` cuando es una carpeta (o un CSV) de
#'   descargas del GHDx con varias ubicaciones, como la carpeta `covariables/` de un proyecto (ver [dl_tabla()]).
#'   No hace falta si `covariables` ya es una tabla leída.
#' @param valor_nacional_de Vector (o lista) con nombres, covariable de los crudos = la covariable de `covariables`
#'   cuyo valor nacional cierra sus filas. Las covariables que no nombra cierran en su propio valor nacional.
#' @return Una [dl_tabla()] `covariables` con las filas subnacionales del año: `ubicacion`, `anio`, `sexo`, las
#'   edades (si los crudos las traen; vacías en las de todas las edades), `covariable`, `valor`, `error_estandar` y
#'   `fuente` (el indicador, el método, q y las ediciones de la serie). Tres atributos:
#'   - `calibracion`: un `data.table` por covariable con `metodo`, `transformacion`, `q`, `q_en_borde` (el borde de su
#'     intervalo de búsqueda en que quedó q, `"inferior"` o `"superior"`; `NA` si no quedó en ninguno o no hay q),
#'     `ediciones` (las usadas: con el paseo aleatorio, todas las no excluidas; con `edicion`, las elegidas en alguna
#'     serie), `excluidas` (en texto, con su motivo), `anios_poblacion` (el año de la población de cada edición y del
#'     cierre) y `valor_nacional_de` (la covariable que dio el valor nacional, si es otra; si no, vacío).
#'   - `series`: un `data.table` por serie y edición con `g`, `se_g`, `g_suavizado` y `S` (el gradiente suavizado y
#'     su varianza en el año de la edición; con `edicion`, g y se_g²) y `usada` (la edición que tomó `edicion`;
#'     con el paseo aleatorio entran todas). Sin edades en los crudos, la banda es 0-125 (todas las edades).
#'   - `excluidas`: un `data.table` con `covariable`, `anio` y `motivo`, una fila por edición excluida de cada
#'     covariable (sin filas si no se excluyó ninguna).
#' @seealso [dl_tablas] (la tabla `proxies_crudos`), [dl_tabla()], [dl_proyecto()].
#' @family proyecto
#' @examples
#' pob <- data.frame(ubicacion = rep(c("R01", "R02", "R03"), each = 2),
#'                   anio = rep(c(2019, 2023), 3),
#'                   sexo = "ambos", edad_inicio = 0, edad_fin = NA,
#'                   poblacion = c(100, 110, 300, 290, 200, 210))
#' crudos <- data.frame(ubicacion = rep(c("R01", "R02", "R03"), 3),
#'                      anio = rep(c(2019, 2021, 2023), each = 3),
#'                      covariable = "haqi", indicador = "índice de acceso (encuesta)",
#'                      valor = c(40, 60, 52, 45, 58, 50, 41, 63, 55), error_estandar = 1.5)
#' nacional <- data.frame(anio = 2023, covariable = "haqi", valor = 56.4)
#' cal <- dl_calibrar_proxies(crudos, nacional, pob, anio = 2023,
#'                            transformacion = c(haqi = "diferencia"))
#' cal
#' attr(cal, "calibracion")
#' # las series: gradiente observado (puntos) y suavizado (líneas), por ubicación
#' s <- attr(cal, "series")
#' plot(g ~ anio, s, col = factor(ubicacion), pch = 19, ylab = "gradiente")
#' for (u in unique(s$ubicacion)) lines(g_suavizado ~ anio, s[s$ubicacion == u, ])
#'
#' # el valor nacional de otra covariable: las filas de haqi cierran en el de haqi_estandarizado
#' otra <- data.frame(anio = 2023, covariable = "haqi_estandarizado", valor = 54.1)
#' cal2 <- dl_calibrar_proxies(crudos, otra, pob, anio = 2023,
#'                             transformacion = c(haqi = "diferencia"),
#'                             valor_nacional_de = c(haqi = "haqi_estandarizado"))
#' weighted.mean(cal2$valor, c(110, 290, 210))   # 54.1
#' attr(cal2, "calibracion")$valor_nacional_de
#'
#' # el proyecto de ejemplo trae una encuesta de tres ediciones: dl_proyecto() la calibra
#' p <- dl_proyecto(dl_ejemplo(), causa = 9100)
#' attr(p$calibracion, "calibracion")[, c("covariable", "transformacion", "q", "ediciones")]
#' # la misma calibración a mano, para otro año, con el método de la edición más cercana
#' t <- p$tablas
#' cal19 <- dl_calibrar_proxies(t$proxies_crudos, t$covariables, t$poblacion, anio = 2019,
#'                              metodo = "edicion", transformacion = c(haqi = "diferencia"))
#' head(cal19)
#' @export
dl_calibrar_proxies <- function(crudos, covariables, poblacion, anio, metodo = c("paseo_aleatorio", "edicion"),
                                transformacion = NULL, excluir = NULL, anio_nacional = anio, ubicacion_gbd = NULL,
                                valor_nacional_de = NULL) {
  metodo <- match.arg(metodo)
  if (!.dl_es_entero1(anio)) .dl_stop("`anio` debe ser un a\u00f1o (un entero); es %s", .dl_describir_objeto(anio))
  if (!.dl_es_entero1(anio_nacional))
    .dl_stop("`anio_nacional` debe ser un a\u00f1o (un entero); es %s", .dl_describir_objeto(anio_nacional))
  e <- .dl_leer_entradas_proxies(crudos, covariables, poblacion, excluir, ubicacion_gbd)
  tr <- .dl_validar_crudos(e, transformacion)
  e$nacional_de <- .dl_nacional_de(valor_nacional_de, e)
  .dl_validar_nacionales(e, anio_nacional)
  gr <- .dl_gradientes(e$crudos, e$poblacion, tr, metodo)
  partes <- lapply(split(gr, by = "covariable"), function(g)
    .dl_calibrar_covariable(g, e, tr[[g$covariable[1L]]], anio, anio_nacional, metodo))
  filas <- data.table::rbindlist(lapply(partes, `[[`, "filas"))
  if (!e$con_edades) data.table::set(filas, j = c("edad_inicio", "edad_fin"), value = NULL)
  else data.table::set(filas, i = which(filas$edad_inicio == 0 & filas$edad_fin == .DL_EDAD_ABIERTA),
                       j = c("edad_inicio", "edad_fin"), value = list(NA_real_, NA_real_))
  out <- dl_tabla("covariables", filas)
  data.table::setattr(out, "calibracion", data.table::rbindlist(lapply(partes, `[[`, "calibracion")))
  data.table::setattr(out, "series", data.table::rbindlist(lapply(partes, `[[`, "series")))
  q <- e$quitadas[order(e$quitadas$covariable, e$quitadas$anio)]
  data.table::setattr(out, "excluidas", data.table::data.table(covariable = q$covariable, anio = as.integer(q$anio),
                                                               motivo = q$motivo))
  out
}
