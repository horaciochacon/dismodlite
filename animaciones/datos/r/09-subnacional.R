# Escena 09, de lo nacional a lo subnacional: la cascada por covariables (dl_cascada()) sobre el ajuste nacional del
# ejemplo, con la misma llamada que hace dl_correr() (dl_cascada(f, b, semilla = semilla)). Se sigue un sexo (mujeres),
# una simulación (k = 1), una ubicación (Huancavelica, 09) y una banda de población (70 a 74 años).
#
# Para mostrar los valores sorteados de cada paso (X_nac, X_d, beta_c), este guion repite los sorteos de la cascada en
# su mismo orden (cabecera de R/cascada.R) con las funciones internas del paquete, y comprueba que reproducen
# exactamente cascada$dX y que el desplazamiento reproduce las tasas que resolvió el paquete. Las comprobaciones viajan
# en el archivo de datos y la escena las audita.
exportar <- function(ctx) {
  ns <- asNamespace("dismodlite")
  b <- ctx$insumos()
  f <- ctx$ajuste()
  cas <- dl_cascada(f, b, semilla = ctx$semilla)
  sexo <- 2L; k <- 1L; dpto <- "09"; banda <- 19L
  nac <- b$loc_ancla
  n <- nrow(f$draws_par[[1]])
  kappa <- cas$kappa
  techo_f <- as.numeric(unlist(b$cfg$emr_prior$cota))[2]

  # Nombres de las ubicaciones y la tabla betas tal como la escribe el proyecto (efecto_sobre)
  ubic <- data.table::fread(dl_ejemplo("ubicaciones.csv"), colClasses = "character", encoding = "UTF-8")
  nombre <- function(id) ubic$nombre[match(id, ubic$ubicacion)]
  betas_csv <- data.table::fread(dl_ejemplo("betas.csv"), encoding = "UTF-8")

  # ---- Paso 1: los sorteos de .dl_dX, repetidos y guardados por partes ----
  px <- ns$.dl_proxies(b)
  anio <- ns$.dl_anio_ajuste(b$cfg)
  dptos <- sort(unique(b$cov_proxy$location_id))
  sexos <- as.integer(unlist(b$cfg$sexos))
  tabla_proxy <- ns$.dl_proxy_con_banda(b$cov_proxy)
  set.seed(ctx$semilla)
  partes <- list(); repro <- list()
  for (fila in seq_len(nrow(px))) {
    cov <- px$covariate_name_short[fila]
    transf <- px$transformacion[fila]
    nac_c <- b$cov_valores[covariate_id == px$covariate_id_x[fila] & location_id == nac & year == anio]
    prx <- tabla_proxy[covariate_id_proxy == px$covariate_id_proxy[fila]]
    for (s in sexos) {
      nac_s <- ns$.dl_filas_del_sexo(nac_c, s)
      prx_s <- ns$.dl_filas_del_sexo(prx, s)[order(location_id, age_group_id)]
      sd_T <- ns$.dl_sd_transformada(nac_s$lower, nac_s$upper, transf)
      x_nac <- stats::rnorm(n, ns$.dl_transformar(nac_s$val, transf), sd_T)
      escala <- if (transf == "lineal") as.numeric(ns$`%||%`(px$escala[fila], 1)) else 1
      J <- nrow(prx_s)
      se_T <- ns$.dl_sd_delta(prx_s$valor_calibrado, prx_s$valor_calibrado_se, transf)
      x_d <- stats::rnorm(n * J, rep(ns$.dl_transformar(prx_s$valor_calibrado, transf), each = n), rep(se_T, each = n))
      repro[[length(repro) + 1]] <- data.table::data.table(
        draw = rep(seq_len(n), J), location_id = rep(prx_s$location_id, each = n), sex_id = s,
        age_group_id = rep(as.integer(prx_s$age_group_id), each = n), covariate_name_short = cov,
        dX = (x_d - rep(x_nac, J)) * escala)
      if (s == sexo) partes[[cov]] <- list(nac = nac_s, prx = prx_s, sd_T = sd_T, se_T = se_T, escala = escala,
                                           x_nac_k = x_nac[k], x_d_k = x_d[(seq_len(J) - 1) * n + k], transf = transf)
    }
  }
  repro <- data.table::rbindlist(repro)
  comp <- merge(repro, cas$dX, by = c("draw", "location_id", "sex_id", "age_group_id", "covariate_name_short"))
  dif_dX <- if (nrow(comp) == nrow(cas$dX)) max(abs(comp$dX.x - comp$dX.y)) else Inf
  beta <- ns$.dl_draws_betas(px, n, ctx$semilla + ns$.DL_SALTO_SEMILLA_BETAS)

  # Nombre corto y efecto_sobre (tal como en betas.csv) de cada covariable, en el orden de la cascada
  corto <- c(SEV_scalar_agestd_cvd_pvd = "SEV", LDI_pc = "LDI", haqi = "HAQ")
  covariables <- lapply(px$covariate_name_short, function(cov) {
    pt <- partes[[cov]]; fila <- px[covariate_name_short == cov]
    T <- function(x) ns$.dl_transformar(x, pt$transf)
    # la banda que se muestra: la 70-74 si el proxy viene por edad, todas las edades (22) si no
    bd <- if (banda %in% pt$prx$age_group_id) banda else pt$prx$age_group_id[1]
    sel <- pt$prx$age_group_id == bd
    pr <- pt$prx[sel]
    j_h <- which(pt$prx$location_id == dpto & pt$prx$age_group_id == bd)
    list(
      nombre = cov, corto = unname(corto[cov]),
      efecto_sobre = betas_csv[covariable == cov]$efecto_sobre, canal = fila$canal,
      transformacion = pt$transf, escala = pt$escala,
      beta = fila$beta, beta_inf = fila$beta_lower, beta_sup = fila$beta_upper,
      sd_beta = (fila$beta_upper - fila$beta_lower) / 3.92, beta_k = unname(beta[k, cov]),
      val = pt$nac$val, lower = pt$nac$lower, upper = pt$nac$upper, T_val = T(pt$nac$val), sd_T = pt$sd_T,
      x_nac_k = pt$x_nac_k,
      banda_mostrada = bd,
      ubicaciones = pr$location_id, T_valor = T(pr$valor_calibrado), se_T = pt$se_T[sel],
      valor = pr$valor_calibrado,
      x_d_k = pt$x_d_k[j_h], dX_k = (pt$x_d_k[j_h] - pt$x_nac_k) * pt$escala,
      dX_paquete = cas$dX[draw == k & location_id == dpto & sex_id == sexo & age_group_id == bd &
                            covariate_name_short == cov]$dX
    )
  })
  names(covariables) <- px$covariate_name_short

  # ---- Paso 2: pesos P_ba en las edades anuales y desplazamientos delta_c(a) de la ubicación ----
  ctx_s <- ns$.dl_ctx(b, sexo)
  edades <- ctx_s$anual
  fuera <- b$cfg$cascada$dx_fuera_de_banda$valor; interp <- b$cfg$cascada$dx_interpolacion$valor
  delta <- lapply(px$covariate_name_short, function(cov) {
    dx_c <- cas$dX[draw == k & location_id == dpto & sex_id == sexo & covariate_name_short == cov][order(age_group_id)]
    P <- ns$.dl_pesos_edad(edades, dx_c$age_group_id, b$bandas_catalogo, fuera, interp)
    dXa <- as.vector(dx_c$dX[match(rownames(P), dx_c$age_group_id)] %*% P)   # sum_b dX_b P_ba
    list(dX_a = dXa, delta = kappa * beta[k, cov] * dXa, bandas = as.integer(rownames(P)), dX_b = dx_c$dX)
  })
  names(delta) <- px$covariate_name_short
  lim_sev <- ns$.dl_limites_bandas(b$bandas_catalogo, delta$SEV_scalar_agestd_cvd_pvd$bandas, "del proxy")
  medio_sev <- (lim_sev$age_start + pmin(lim_sev$age_end, 85)) / 2

  # Tasas nacionales y de la ubicación (antes de renormalizar), mujeres, simulación k, edades anuales
  qn <- f$draws_q[location_id == nac & sex_id == sexo & draw == k][order(edad)]
  qd <- cas$draws_q_sin_renorm[location_id == dpto & sex_id == sexo & draw == k][order(edad)]
  suma_i <- Reduce(`+`, lapply(delta[px$canal == "i"], `[[`, "delta"))
  suma_f <- Reduce(`+`, lapply(delta[px$canal == "f"], `[[`, "delta"))
  dif_log_i <- max(abs(log(qd$i) - (log(qn$i) + suma_i)))
  dif_log_f <- max(abs(log(qd$f) - pmin(log(qn$f) + suma_f, log(techo_f))))

  # ---- Paso 4: la renormalización de la banda 70-74, mujeres, simulación k ----
  en_banda <- function(tabla, col) tabla[sex_id == sexo & draw == k & edad >= 70 & edad < 75,
                                         list(q = mean(get(col))), by = location_id]
  pob <- b$poblacion[sex_id == sexo & age_group_id == banda, list(location_id, N = val)]
  antes <- merge(en_banda(cas$draws_q_sin_renorm, "p"), pob, by = "location_id")
  despues <- merge(en_banda(cas$draws_q, "p"), pob, by = "location_id")
  antes_i <- merge(en_banda(cas$draws_q_sin_renorm, "ipop"), pob, by = "location_id")
  despues_i <- merge(en_banda(cas$draws_q, "ipop"), pob, by = "location_id")
  sub <- antes[location_id != nac]
  orden <- sub$location_id[order(sub$q)]                       # ranking por la prevalencia antes de renormalizar
  fila_nac <- antes[location_id == nac]
  factor_p <- cas$renorm[medida == "prevalence" & sex_id == sexo & age_group_id == banda & draw == k]$factor
  factor_i <- cas$renorm[medida == "incidence" & sex_id == sexo & age_group_id == banda & draw == k]$factor
  rango_p <- range(cas$renorm[medida == "prevalence"]$factor)

  # ---- Modo plano: como en la guía de la estimación subnacional (una copia del ejemplo con modo: plano) ----
  carpeta <- dl_ejemplo(copiar_en = file.path(tempdir(), "plano09"))
  cfg_plano <- file.path(carpeta, "config", "9100.yaml")
  lineas <- readLines(cfg_plano, encoding = "UTF-8")
  lineas <- sub("^subnacional:$", "subnacional:\n  modo: plano", lineas)
  writeLines(lineas, cfg_plano, useBytes = TRUE)
  b_plano <- suppressMessages(dl_insumos(dl_proyecto(carpeta, causa = 9100)))
  f_plano <- dl_ajustar(b_plano, ctx$opciones(), semilla = ctx$semilla)
  cas_plano <- dl_cascada(f_plano, b_plano, semilla = ctx$semilla)
  factor_plano <- cas_plano$renorm[medida == "prevalence" & sex_id == sexo & age_group_id == banda & draw == k]$factor
  rango_plano <- range(cas_plano$renorm$factor)

  list(
    fuente = paste0("Datos sintéticos del ejemplo del paquete (causa 9100, geografía del Perú); ",
                    "dl_cascada(ajuste, insumos, semilla = 1)"),
    sexo = sexo, simulacion = k, kappa = kappa, techo_f = techo_f, remision = 0,
    modo = cas$modo, n_simulaciones = n, n_ubicaciones = length(cas$departamentos),
    anio = anio, anio_validacion = b$cfg$cascada$heldout_anio$valor,
    ubicacion = list(id = dpto, nombre = nombre(dpto)),
    covariables = unname(covariables),
    nombres = stats::setNames(as.list(nombre(dptos)), dptos),
    edades = edades,
    p_nac = qn$p, i_nac = qn$i, f_nac = qn$f,
    p_d = qd$p, i_d = qd$i, f_d = qd$f,
    desplazamiento = lapply(unname(delta), function(x) list(dX_a = x$dX_a, delta = x$delta)),
    sev = list(bandas = delta$SEV_scalar_agestd_cvd_pvd$bandas, inicio = lim_sev$age_start,
               fin = lim_sev$age_end, medio = medio_sev, dX_b = delta$SEV_scalar_agestd_cvd_pvd$dX_b),
    renorm = list(
      banda = banda, edades = "70-74", nac = nac,
      N_nac = fila_nac$N, q_nac = fila_nac$q,
      ubicaciones = orden, N = sub$N[match(orden, sub$location_id)],
      q_antes = sub$q[match(orden, sub$location_id)],
      q_despues = despues$q[match(orden, despues$location_id)],
      factor = factor_p,
      ipop_nac = antes_i[location_id == nac]$q, factor_i = factor_i,
      ipop_antes = antes_i$q[match(orden, antes_i$location_id)],
      ipop_despues = despues_i$q[match(orden, despues_i$location_id)],
      rango_factor = rango_p
    ),
    plano = list(factor = factor_plano, rango = rango_plano, modo = cas_plano$modo,
                 q = en_banda(cas_plano$draws_q, "p")[location_id == nac]$q),
    comprobaciones = list(dif_dX = dif_dX, dif_log_i = dif_log_i, dif_log_f = dif_log_f,
                          filas_dX = nrow(cas$dX), truncados = cas$truncados_emr)
  )
}
