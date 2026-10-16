# Escena 10, los años vividos con discapacidad: la tabla de severidad del ejemplo, la Beta por momentos de cada
# proporción y cada peso, las simulaciones de pi y DW que sortea dl_avd() (las mismas: la misma semilla y las mismas
# funciones internas, en el mismo orden), el factor de comorbilidad por banda y los AVD por banda del ajuste nacional
# del ejemplo, como los calcula dl_correr(): dl_avd(ajuste, insumos, dl_factor_comorbilidad(insumos), semilla).
# Sexo que se muestra: mujeres (sex_id 2), como la guía «Carga: los años vividos con discapacidad».
exportar <- function(ctx) {
  ns <- asNamespace("dismodlite")
  b <- ctx$insumos()
  f <- ctx$ajuste()
  semilla <- ctx$semilla
  sexo <- 2L
  sev <- b$severidad
  stopifnot(nrow(sev) == 3L, all(is.na(sev$beta_covariable) | !nzchar(sev$beta_covariable)))

  # Nombres de los estados, del CSV del ejemplo (insumos$severidad trae solo el id)
  csv <- data.table::fread(dl_ejemplo("severidad.csv"), encoding = "UTF-8")[causa == b$cfg$cause_id]
  nombres <- csv$estado[match(sev$health_state_id, csv$id_estado)]
  stopifnot(!anyNA(nombres))

  # Beta por momentos de cada elemento (.dl_beta_momentos(), R/avd.R)
  momentos <- function(media, inferior, superior) {
    v <- ((superior - inferior) / get(".DL_ANCHO_IC95_EN_SD", ns))^2
    fm <- get(".dl_beta_momentos", ns)(media, inferior, superior)
    list(media = media, inferior = inferior, superior = superior, v = v,
         constante = !is.null(fm$constante),
         forma1 = if (is.null(fm$constante)) fm$forma1 else NULL,
         forma2 = if (is.null(fm$constante)) fm$forma2 else NULL)
  }
  estados <- lapply(seq_len(nrow(sev)), function(e) list(
    nombre = nombres[e], id = sev$health_state_id[e],
    pi = momentos(sev$proportion[e], sev$prop_lower[e], sev$prop_upper[e]),
    dw = momentos(sev$dw_mean[e], sev$dw_lower[e], sev$dw_upper[e])))

  # La densidad de la Beta de la proporción del estado leve (el ejemplo de la toma 3)
  leve <- estados[[2]]$pi
  x_beta <- seq(0.22, 0.43, by = 0.0015)
  y_beta <- stats::dbeta(x_beta, leve$forma1, leve$forma2)

  # Las simulaciones de dl_avd(): primero las proporciones, después los pesos, desde la semilla (.dl_simular_pi_dw())
  n_sim <- nrow(f$draws_par[[1]])
  set.seed(semilla)
  pi_cruda <- get(".dl_rbeta_momentos", ns)(sev$proportion, sev$prop_lower, sev$prop_upper, n_sim)
  sim <- get(".dl_simular_pi_dw", ns)(sev, n_sim, semilla)
  stopifnot(isTRUE(all.equal(pi_cruda / rowSums(pi_cruda), sim$pi_ke, tolerance = 0)))
  suma_k <- rowSums(sim$pi_ke * sim$dw_ke)                        # sum_e pi_{k,e} DW_{k,e}
  suma_central <- sum(sev$proportion * sev$dw_mean)               # sum_e pi_e DW_e

  # El factor de comorbilidad y los AVD, como en dl_correr()
  como <- dl_factor_comorbilidad(b)
  y <- dl_avd(f, b, como, semilla = semilla)
  y0 <- dl_avd(f, b, NULL, semilla = semilla)                      # sin el factor
  bandas <- b$prior_gbd[measure_id == 5L & sex_id == sexo][order(age_start)]
  rutas <- get(".dl_resolver_rutas", ns)(b$rutas, b)
  ancla_avd <- get(".dl_materializar_medida", ns)(b$cfg, "yld", rutas, archivo = get(".dl_path", ns)(rutas, "std_yld"))
  ancla_avd <- ancla_avd[sex_id == sexo][match(bandas$age_group_id, age_group_id)]
  como_b <- como[sex_id == sexo][match(bandas$age_group_id, age_group_id)]$factor
  stopifnot(isTRUE(all.equal(como_b, ancla_avd$val / (bandas$val * suma_central), tolerance = 1e-14)))

  por_banda <- function(tabla, g) tabla[location_id == b$loc_ancla & sex_id == sexo & age_group_id == g][order(draw)]$val
  p_kb <- lapply(bandas$age_group_id, function(g) por_banda(y$draws_prev_banda, g))
  avd_kb <- lapply(bandas$age_group_id, function(g) por_banda(y$draws_yld, g))
  avd0_kb <- lapply(bandas$age_group_id, function(g) por_banda(y0$draws_yld, g))
  # Las simulaciones de los AVD del paquete son p x suma x COMO, simulación por simulación
  for (j in seq_along(p_kb))
    stopifnot(isTRUE(all.equal(avd_kb[[j]], p_kb[[j]] * suma_k * como_b[j], tolerance = 1e-14)))

  K <- 6L                                                         # simulaciones que se muestran
  j70 <- which(bandas$age_start == 70)
  etiqueta <- ifelse(bandas$age_end >= 125, paste0(bandas$age_start, "+"),
                     paste0(bandas$age_start, "-", bandas$age_end - 1))
  list(
    fuente = sprintf(paste0("Datos sintéticos del ejemplo (causa 9100, Perú, %d), mujeres: dl_factor_comorbilidad() y ",
                            "dl_avd() sobre el ajuste nacional, %d simulaciones, semilla %d"), b$cfg$years$ajuste, n_sim, semilla),
    semilla = semilla, n_sim = n_sim, sexo = sexo,
    estados = estados,
    beta_leve = list(x = x_beta, y = y_beta),
    k_muestra = seq_len(K),
    pi_cruda = unname(lapply(seq_len(K), function(k) pi_cruda[k, ])),
    pi_k = unname(lapply(seq_len(K), function(k) sim$pi_ke[k, ])),
    dw_k = unname(lapply(seq_len(K), function(k) sim$dw_ke[k, ])),
    suma_k = unname(suma_k),
    suma_central = suma_central,
    desplazamiento = sim$desplazamiento$desplazamiento,
    bandas = list(
      etiqueta = etiqueta, inicio = bandas$age_start, fin = bandas$age_end, age_group_id = bandas$age_group_id,
      p_ancla = bandas$val, avd_ancla = ancla_avd$val, como = como_b,
      p_media = vapply(p_kb, mean, 0),
      avd_media = vapply(avd_kb, mean, 0),
      avd_inferior = vapply(avd_kb, stats::quantile, 0, probs = 0.025, names = FALSE),
      avd_superior = vapply(avd_kb, stats::quantile, 0, probs = 0.975, names = FALSE),
      avd_sin_factor = vapply(avd0_kb, mean, 0)
    ),
    banda_ejemplo = list(
      indice = j70 - 1L, etiqueta = etiqueta[j70],
      p_k = p_kb[[j70]][seq_len(K)], avd_k = avd_kb[[j70]][seq_len(K)]
    )
  )
}
