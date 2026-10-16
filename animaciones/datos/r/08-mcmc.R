# Escena 08, el muestreo: Metropolis-Hastings adaptativo por bloques, el calentamiento, las simulaciones, R-hat y ESS.
#
# Se corre dl_ajustar() en el proyecto de ejemplo con las opciones de producción (dl_opciones_mcmc() por defecto:
# 4 cadenas de 50 000 iteraciones, 10 000 de calentamiento, adelgazamiento 10, 1000 simulaciones), semilla 1, y se
# exporta lo que dibuja la escena para los hombres (sexo 1):
#   - la cadena 1, corrida con una copia instrumentada de dl_mh() que anota cada propuesta (z, theta', lp(theta'),
#     lp(theta), u, aceptada o no), la escala s_b y la aceptación de cada ventana de 100 iteraciones y la forma L_b.
#     La copia no cambia el paquete: se comprueba que da exactamente las simulaciones de .dl_correr_cadena();
#   - las 4 cadenas del sexo 1 (las de dl_ajustar(), rehechas con .dl_correr_cadena() y comprobadas contra
#     ajuste$draws_par y ajuste$mcmc) de los dos nudos de log i que se dibujan;
#   - R-hat y ESS de R (dl_rhat, dl_ess) y la tabla mcmc del ajuste;
#   - p por edad de las 1000 simulaciones (draws_q) y la estimación (dl_estimaciones(): media y percentiles 2.5 y 97.5);
#   - el contexto de la log-posterior del sexo 1 en JSON (esquema dismodlite/ctx-v1, el de pruebas/fixtures.R), para
#     que la auditoría recalcule lp(theta) con DL.modelo.logPosterior.
exportar <- function(ctx) {
  dl <- asNamespace("dismodlite")
  b <- ctx$insumos()
  semilla <- ctx$semilla
  op <- dl_opciones_mcmc(nucleos = min(4L, parallel::detectCores()))   # producción; los núcleos no cambian nada
  ajuste <- suppressMessages(dl_ajustar(b, op, semilla = semilla, cache = FALSE))

  sx <- 1L
  ctx1 <- dl$.dl_ctx(b, sx)
  lp <- function(theta) dl$.dl_log_post(theta, ctx1)
  theta0 <- dl$.dl_theta_inicial(ctx1)
  K <- length(ctx1$nudos)
  bloques <- list(seq_len(K), K + seq_len(K))
  semilla_sexo <- semilla + sx                                   # como en dl_ajustar()
  T_tot <- op$iter; T_cal <- op$warmup; thin <- op$thin

  # ---- La cadena 1, instrumentada: las mismas llamadas al generador, en el mismo orden, que dl_mh() ----
  N_DETALLE <- 40L                                               # iteraciones con todo anotado (theta completo)
  instrumentada <- function(cadena) {
    set.seed(dl$.dl_semilla_derivada(semilla_sexo, cadena, 0))
    th <- theta0 + stats::rnorm(length(theta0), 0, dl$.DL_MH_SD_INICIO)
    names(th) <- names(theta0)
    inicio <- th
    set.seed(dl$.dl_semilla_derivada(semilla_sexo, cadena, dl$.DL_MH_DESPLAZAMIENTO_SEMILLA))
    escala <- dl$.dl_mh_escala_inicial(bloques)
    L_prop <- lapply(bloques, function(idx) diag(length(idx)))
    lp_act <- lp(th)
    n_g <- (T_tot - T_cal) %/% thin
    sims <- matrix(NA_real_, n_g, length(th), dimnames = list(NULL, names(th)))
    acept <- prop_n <- numeric(length(bloques))
    theta_cal <- matrix(NA_real_, T_cal, length(th))
    detalle <- list(); ventanas <- list(); recorrido <- matrix(NA_real_, T_cal, length(th))
    acept_bloque1 <- logical(T_cal)
    for (it in seq_len(T_tot)) {
      for (bq in seq_along(bloques)) {
        idx <- bloques[[bq]]
        z <- stats::rnorm(length(idx))
        tp <- th
        tp[idx] <- tp[idx] + escala[bq] * as.vector(L_prop[[bq]] %*% z)
        lpp <- lp(tp)
        prop_n[bq] <- prop_n[bq] + 1
        u <- stats::runif(1)
        ok <- log(u) < lpp - lp_act
        if (it <= N_DETALLE)
          detalle[[length(detalle) + 1L]] <- list(it = it, bloque = bq, escala = escala[bq], z = z,
                                                  theta = unname(th), theta_prop = unname(tp), lp_actual = lp_act,
                                                  lp_prop = lpp, u = u, aceptada = ok)
        if (it <= T_cal && bq == 1L) acept_bloque1[it] <- ok
        if (ok) { th <- tp; lp_act <- lpp; acept[bq] <- acept[bq] + 1 }
      }
      if (it <= T_cal) {
        theta_cal[it, ] <- th
        recorrido[it, ] <- th
        if (it == T_cal %/% 2L)
          for (bq in seq_along(bloques))
            L_prop[[bq]] <- dl$.dl_mh_forma_propuesta(theta_cal[seq_len(it), bloques[[bq]], drop = FALSE])
        if (it %% dl$.DL_MH_VENTANA == 0L) {
          k <- it / dl$.DL_MH_VENTANA
          a_k <- acept / prop_n
          antes <- escala
          for (bq in seq_along(bloques)) escala[bq] <- dl$.dl_mh_escala_adaptada(escala[bq], a_k[bq], k)
          ventanas[[k]] <- list(k = k, aceptacion = a_k, escala_antes = antes, escala = escala)
          acept[] <- 0; prop_n[] <- 0
        }
      } else if ((it - T_cal) %% thin == 0L) {
        sims[(it - T_cal) %/% thin, ] <- th
      }
    }
    list(inicio = inicio, draws = sims, aceptacion = acept / pmax(prop_n, 1), escala = escala, L = L_prop,
         detalle = detalle, ventanas = ventanas, recorrido = recorrido, acept_bloque1 = acept_bloque1)
  }
  c1 <- instrumentada(1L)

  # Las 4 cadenas del sexo 1 con la función del paquete; la 1 debe ser idéntica a la instrumentada.
  cadenas <- lapply(seq_len(op$chains), function(cd)
    dl$.dl_correr_cadena(lp, theta0, bloques, cadena = cd, iter = T_tot, warmup = T_cal, seed = semilla_sexo,
                         thin = thin))
  stopifnot(identical(cadenas[[1]]$draws, c1$draws), identical(cadenas[[1]]$escala, c1$escala),
            identical(cadenas[[1]]$aceptacion, c1$aceptacion))
  por_cadena <- lapply(cadenas, `[[`, "draws")
  juntas <- do.call(rbind, por_cadena)
  elegidas <- unique(round(seq(1L, nrow(juntas), length.out = min(op$draws, nrow(juntas)))))
  # Son las cadenas de dl_ajustar(): las mismas simulaciones elegidas, el mismo R-hat y el mismo ESS.
  stopifnot(identical(unname(juntas[elegidas, ]), unname(ajuste$draws_par[["1"]])))
  rh <- dl_rhat(por_cadena); es <- dl_ess(por_cadena)
  m1 <- ajuste$mcmc[sex_id == sx]
  stopifnot(identical(unname(rh), m1$rhat), identical(unname(es), m1$ess))

  # Los dos nudos de log i del plano: 50 y 60 años.
  nudos_plano <- c(50, 60)
  j_plano <- match(nudos_plano, ctx1$nudos)
  nombres <- names(theta0)

  # p de las simulaciones y la estimación del paquete (sexo 1).
  dq <- ajuste$draws_q[sex_id == sx]
  edades <- sort(unique(dq$edad))
  p_sims <- lapply(sort(unique(dq$draw)), function(d) dq[draw == d][order(edad)]$p)
  est <- dl_estimaciones(ajuste, "prevalencia")[sex_id == sx][order(edad)]

  A <- function(x) I(as.vector(x))
  `%||%` <- function(a, b) if (is.null(a)) b else a
  ctx_json <- function(ctx, b, sexo) {   # el de pruebas/fixtures.R
    pobl <- b$poblacion[location_id == b$loc_ancla & sex_id == sexo]
    bp <- b$bandas_pobl
    N <- pobl$val[match(bp$age_group_id, pobl$age_group_id)]
    d <- ctx$datos
    cfg_rem <- b$cfg$remision
    list(
      esquema = "dismodlite/ctx-v1", sexo = sexo, nudos = A(ctx$nudos), edad_inicio = ctx$edad_inicio,
      edad_fin = ctx$edad_fin, nsub = ctx$nsub, sigma_suavidad = ctx$sigma_suavidad,
      remision = list(valor = as.numeric(cfg_rem$valor %||% 0),
                      por_edad = lapply(cfg_rem$por_edad %||% list(), function(t)
                        list(edad_inicio = t$edad_inicio, edad_fin = t$edad_fin, valor = t$valor))),
      emr = list(cero = isTRUE(ctx$emr$cero), plano = isTRUE(ctx$emr$plano), mu_log = A(ctx$emr$mu_log),
                 sd_log = A(ctx$emr$sd_log), cota = A(ctx$emr$cota)),
      ancla = list(age_group_id = A(ctx$ancla$bandas$age_group_id), inicio = A(ctx$ancla$bandas$age_start),
                   fin = A(ctx$ancla$bandas$age_end), val = A(ctx$ancla$bandas$val),
                   sigma_log = A(ctx$ancla$bandas$sigma_log), lambda = ctx$ancla$lambda, rho = b$cfg$anchor$rho_edad),
      poblacion = list(age_group_id = A(bp$age_group_id), inicio = A(bp$age_start), fin = A(bp$age_end), N = A(N)),
      datos = list(tipo = A(as.character(d$tipo_dato)), inicio = A(d$age_start), fin = A(d$age_end),
                   ruta = A(as.integer(d$ruta)), val = A(d$val), s_log = A(d$s_log), eta = A(d$eta),
                   x = A(as.numeric(d$x)), n = A(d$n_ef)))
  }

  redondear <- function(x, d = 5) round(x, d)
  list(
    fuente = "Datos sintéticos del ejemplo (causa 9100), sin datos locales en el ajuste; hombres; dl_ajustar() de producción, semilla 1",
    sexo = sx,
    opciones = list(simulaciones = op$draws, cadenas = op$chains, iteraciones = T_tot, calentamiento = T_cal,
                    adelgazamiento = thin, ventana = dl$.DL_MH_VENTANA, objetivo = dl$.DL_MH_ACEPTACION_OBJETIVO,
                    escala_optima = dl$.DL_MH_ESCALA_OPTIMA, sd_inicio = dl$.DL_MH_SD_INICIO,
                    jitter = dl$.DL_MH_JITTER_COVARIANZA, semilla = semilla, semilla_sexo = semilla_sexo),
    nudos = A(ctx1$nudos), nombres = A(nombres), d_bloque = A(lengths(bloques)),
    theta0 = A(unname(theta0)),
    plano = list(nudos = A(nudos_plano), j = A(j_plano - 1L)),
    ctx = ctx_json(ctx1, b, sx),
    # la cadena 1
    cadena1 = list(
      inicio = A(unname(c1$inicio)),
      detalle = c1$detalle,
      # el recorrido de los dos nudos del plano en cada iteración del calentamiento (theta después de la iteración)
      recorrido_x = A(redondear(c1$recorrido[, j_plano[1]])), recorrido_y = A(redondear(c1$recorrido[, j_plano[2]])),
      acept_bloque1 = A(as.integer(c1$acept_bloque1)),
      ventanas = list(k = A(vapply(c1$ventanas, `[[`, 0, "k")),
                      aceptacion_1 = A(vapply(c1$ventanas, function(v) v$aceptacion[1], 0)),
                      aceptacion_2 = A(vapply(c1$ventanas, function(v) v$aceptacion[2], 0)),
                      escala_1 = A(vapply(c1$ventanas, function(v) v$escala[1], 0)),
                      escala_2 = A(vapply(c1$ventanas, function(v) v$escala[2], 0))),
      L1 = lapply(seq_len(K), function(r) A(c1$L[[1]][r, ])),
      escala_final = A(c1$escala), aceptacion_final = A(c1$aceptacion)),
    # las cadenas del sexo 1, en los dos nudos del plano (todas las simulaciones guardadas, sin redondear)
    cadenas = lapply(por_cadena, function(m) list(x = A(m[, j_plano[1]]), y = A(m[, j_plano[2]]))),
    aceptacion_ajuste = list(cadena = A(ajuste$aceptacion[sex_id == sx]$cadena),
                             bloque = A(ajuste$aceptacion[sex_id == sx]$bloque),
                             tasa = A(ajuste$aceptacion[sex_id == sx]$tasa)),
    mcmc = list(parametro = A(m1$parametro), rhat = A(m1$rhat), ess = A(m1$ess)),
    # todos los parámetros de los dos sexos: la auditoría saca de aquí el máximo de R-hat y el mínimo de ESS
    mcmc_todos = list(sex_id = A(ajuste$mcmc$sex_id), parametro = A(ajuste$mcmc$parametro),
                      rhat = A(ajuste$mcmc$rhat), ess = A(ajuste$mcmc$ess),
                      rhat_max = max(ajuste$mcmc$rhat), ess_min = min(ajuste$mcmc$ess),
                      n_parametros = nrow(ajuste$mcmc), datos_locales_en_ajuste = nrow(b$datos[location_level == 0L])),
    simulaciones = list(theta = lapply(seq_len(5), function(k) A(unname(ajuste$draws_par[["1"]][k, ]))),
                        edades = A(edades), p = lapply(p_sims, function(v) A(signif(v, 7)))),
    estimacion = list(edad = A(est$edad), media = A(est$media), inferior = A(est$inferior),
                      superior = A(est$superior))
  )
}
