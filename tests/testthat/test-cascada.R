# dl_cascada(): dX por simulación, canal por beta, nueva solución de la EDO, renormalización exacta y cotas.

fit_v02 <- local({ cache <- NULL; function() {
  if (is.null(cache)) {
    b <- dl_insumos(cfg9100_datos(), rutas_completas())
    f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 30L, cadenas = 2L, iteraciones = 2000L,
                                                   calentamiento = 1000L), semilla = 7L)
    cache <<- list(b = b, f = f)
  }
  cache }})

test_that(".dl_draws_betas reproduce bit a bit el muestreo de dl_avd y respeta modo fija", {
  betas <- rbind(fila_beta(), fila_beta(covariate_name_short = "LDI_pc", parametro_objetivo = "emr", beta = -0.3,
                                        beta_lower = NA_real_, beta_upper = NA_real_, modo = "fija"))
  m <- .dl_draws_betas(betas, 50L, 11L)
  expect_identical(dim(m), c(50L, 2L)); expect_identical(colnames(m), c("sev_scalar_pad", "LDI_pc"))
  expect_true(all(m[, "LDI_pc"] == -0.3))
  set.seed(11L); ref <- stats::rnorm(50, 0.86, (0.95 - 0.79) / 3.92)
  expect_identical(unname(m[, "sev_scalar_pad"]), ref)
  expect_equal(.dl_transformar(0.25, "logit"), log(1 / 3))
  expect_equal(.dl_sd_transformada(8, 12.5, "log"), (log(12.5) - log(8)) / 3.92)
  expect_identical(.dl_sd_transformada(NA, NA, "log"), 0)
})

test_that("dl_cascada: objeto, ubicaciones, dX con media ~ desviación del proxy, y canal correcto", {
  x <- fit_v02(); casc <- dl_cascada(x$f, x$b, kappa = 1, semilla = 3L)
  expect_s3_class(casc, "dl_cascade"); expect_s3_class(casc, "dl_fit")
  expect_setequal(unique(casc$draws_q$location_id), c("123", sprintf("%02d", 1:25)))
  expect_identical(casc$draws_q[location_id == "123"], x$f$draws_q)      # el nacional no se toca
  # dX del SEV (log): media sobre las simulaciones ~ log(proxy / ancla) de cada departamento (el proxy varía por
  # banda de edad)
  dx <- casc$dX[covariate_name_short == "SEV_scalar_agestd_cvd_pvd", list(dx = mean(dX)), by = location_id]
  px <- x$b$cov_proxy[covariate_id_proxy == 900101L, list(esperado = mean(log(valor_calibrado / ancla_ghdx))),
                      by = location_id]
  m <- merge(dx, px, by = "location_id")
  expect_identical(nrow(m), 25L)
  expect_equal(m$dx, m$esperado, tolerance = 0.1)                # error relativo medio de los 25 departamentos
  expect_lt(max(abs(m$dx - m$esperado)), 0.01)                   # y cada departamento por separado (absoluto)
  # canal: SEV y LDI (prevalencia) mueven i; HAQ (emr) mueve f -> en "22" (SEV +10 %, LDI -59 %, HAQ -9,7 puntos,
  # con betas 0,62, -0,21 y -0,012 de extraccion.yaml) i y f suben
  raw <- casc$draws_q_sin_renorm[location_id == "22"]
  m <- merge(raw, x$f$draws_q, by = c("sex_id", "draw", "edad"), suffixes = c("_d", "_n"))
  expect_gt(mean(m$i_d > m$i_n), 0.9)                 # beta SEV > 0 con dX > 0 y beta LDI < 0 con dX < 0
  expect_gt(mean(m$f_d > m$f_n), 0.9)                 # beta HAQ < 0 y dX < 0 => f sube
  # canal aislado (en "22" i y f suben a la vez, así que lo anterior no distingue un cruce de canales): con solo
  # los proxies de prevalencia (SEV, LDI) f queda igual a la nacional en cada departamento, simulación y edad, y
  # con solo el de EMR (HAQ) i queda igual
  crudo_con <- function(covs) {
    b <- x$b; b$cfg$covariables <- Filter(function(cv) cv$covariate_name_short %in% covs, b$cfg$covariables)
    cc <- dl_cascada(x$f, b, kappa = 1, semilla = 3L)
    merge(cc$draws_q_sin_renorm[location_id != "123"], x$f$draws_q, by = c("sex_id", "draw", "edad"),
          suffixes = c("_d", "_n"))
  }
  m <- crudo_con(c("SEV_scalar_agestd_cvd_pvd", "LDI_pc"))
  expect_identical(nrow(m), 25L * nrow(x$f$draws_q))
  expect_identical(m$f_d, m$f_n)
  expect_gt(mean(m$i_d != m$i_n), 0.99)
  m <- crudo_con("haqi")
  expect_identical(nrow(m), 25L * nrow(x$f$draws_q))
  expect_identical(m$i_d, m$i_n)
  expect_gt(mean(m$f_d != m$f_n), 0.99)
  expect_error(dl_cascada(x$f, x$b, semilla = 3L, kappa = 0), "kappa")
  expect_error(dl_cascada(x$f, x$b, kappa = 1, semilla = 3L, motor = "stan"), "^dl_cascada\\(\\): `motor`")
  expect_error(dl_cascada(x$f, x$b, kappa = 1), "semilla")
  expect_error(dl_cascada(casc, x$b, kappa = 1, semilla = 3L), "ya es una cascada")
})

test_that("validador de canal y signo: una beta de EMR baja la prevalencia; el multiplicador ingenuo la subiría", {
  x <- fit_v02(); b <- x$b
  b$betas <- data.table::copy(x$b$betas)              # data.table: no modificar los insumos compartidos
  b$betas[covariate_name_short == "LDI_pc", parametro_objetivo := "otro"]
  expect_error(dl_cascada(x$f, b, kappa = 1, semilla = 3L), "canal")
  # bundle con SOLO el proxy de HAQ (emr): en "03" (HAQ 14 puntos bajo el nacional) f sube => p baja tras la EDO
  b2 <- x$b; b2$cfg$covariables <- Filter(function(cv) cv$covariate_name_short == "haqi", b2$cfg$covariables)
  casc <- dl_cascada(x$f, b2, kappa = 1, semilla = 3L)
  raw <- casc$draws_q_sin_renorm[location_id == "03" & edad >= 60]
  m <- merge(raw, x$f$draws_q[edad >= 60], by = c("sex_id", "draw", "edad"), suffixes = c("_d", "_n"))
  expect_gt(mean(m$f_d > m$f_n), 0.9)
  expect_gt(mean(m$p_d < m$p_n), 0.9)
  dx <- casc$dX[location_id == "03", mean(dX)]        # < 0
  beta_emr <- x$b$betas[parametro_objetivo == "emr"]$beta                 # HAQ: -0,012 por punto
  ingenuo <- mean(x$f$draws_q[edad >= 60]$p) * exp(beta_emr * dx)       # beta_emr * dX > 0: subiría p
  expect_gt(ingenuo, mean(x$f$draws_q[edad >= 60]$p))
})

test_that("renormalizaci\u00f3n exacta por medida y simulaci\u00f3n (1e-10) y cota", {
  x <- fit_v02(); b <- x$b; casc <- dl_cascada(x$f, b, kappa = 1, semilla = 3L)
  qp <- .dl_q_bandas(casc, b); qi <- .dl_ipop_bandas(casc, b)
  pob <- b$poblacion[, list(location_id, sex_id, age_group_id, N = val)]
  for (q in list(qp, qi)) {
    m <- merge(q, pob, by = c("location_id", "sex_id", "age_group_id"))
    s <- m[, list(dpto = sum(val[location_id != "123"] * N[location_id != "123"]),
                  nac = sum(val[location_id == "123"] * N[location_id == "123"])), by = list(sex_id, age_group_id, draw)]
    expect_equal(s$dpto, s$nac, tolerance = 1e-10)
  }
  expect_setequal(unique(casc$renorm$medida), c("prevalence", "incidence"))
  expect_true(all(casc$renorm$factor > 0.5 & casc$renorm$factor < 2))
  expect_true(all(casc$draws_q$p >= 0 & casc$draws_q$p <= 1))
  b2 <- b; b2$cfg$cascada$cota_warning <- 1e-6
  expect_warning(dl_cascada(x$f, b2, kappa = 1, semilla = 3L), "cota")
  expect_output(print(casc), "dl_cascade"); expect_output(print(casc), "cascada subnacional por proxies")
})

# Propiedad que define la renormalización, sobre una tabla hecha a mano: dos bandas de población (40-44 y 45-49),
# el ancla "0" y los departamentos "1" y "2", dos sexos y tres simulaciones; la edad 39 no está en ninguna banda.
test_that(".dl_renormalizar: factor = N_nac q_nac / sum_d N_d q_d y, aplicado, sum_d N_d q_d = N_nac q_nac", {
  bandas <- data.table::data.table(age_group_id = c(13L, 14L), age_start = c(40, 45), age_end = c(45, 50))
  set.seed(1)
  dq <- data.table::CJ(location_id = c("0", "1", "2"), sex_id = 1:2, draw = 1:3, edad = 39:49)
  dq[, `:=`(p = stats::runif(.N, 0.01, 0.2), ipop = stats::runif(.N, 0.001, 0.01))]
  dq[, banda := bandas$age_group_id[.dl_banda_de(edad, bandas)]]
  pob <- data.table::CJ(location_id = c("0", "1", "2"), sex_id = 1:2, age_group_id = c(13L, 14L))
  pob[, val := stats::runif(.N, 1e4, 1e5)]
  antes <- data.table::copy(dq)
  r <- .dl_renormalizar(dq, list(loc_ancla = "0", poblacion = pob), "p")
  N <- pob[, list(location_id, sex_id, banda = age_group_id, N = val)]
  q_banda <- function(tabla) merge(tabla[!is.na(banda), list(q = mean(p)), by = list(location_id, sex_id, banda, draw)],
                                   N, by = c("location_id", "sex_id", "banda"))
  # el factor, calculado aparte
  esperado <- q_banda(antes)[, list(esperado = (N * q)[location_id == "0"] / sum((N * q)[location_id != "0"])),
                             by = list(sex_id, banda, draw)]
  m <- merge(r$renorm, esperado, by.x = c("sex_id", "age_group_id", "draw"), by.y = c("sex_id", "banda", "draw"))
  expect_identical(nrow(m), 2L * 2L * 3L)
  expect_equal(m$factor, m$esperado, tolerance = 1e-12)
  # la propiedad: tras renormalizar, la suma departamental ponderada por población es la nacional
  s <- q_banda(r$dq)[, list(dpto = sum(N[location_id != "0"] * q[location_id != "0"]),
                            nac = N[location_id == "0"] * q[location_id == "0"]), by = list(sex_id, banda, draw)]
  expect_equal(s$dpto, s$nac, tolerance = 1e-10)
  expect_identical(r$dq[location_id == "0"]$p, antes[location_id == "0"]$p)   # el ancla no se toca
  expect_identical(r$dq[edad == 39]$p, antes[edad == 39]$p)                   # sin banda de población: factor 1
  expect_identical(r$dq$ipop, antes$ipop)                                     # solo cambia la columna pedida
  expect_false("factor" %in% names(r$dq))
})

test_that("cascada renormalizada: sum_d N_d q_d = N_nac q_nac por simulaci\u00f3n, sexo y banda de poblaci\u00f3n", {
  x <- fit_v02(); b <- x$b; casc <- dl_cascada(x$f, b, kappa = 1, semilla = 3L)
  ancla <- b$loc_ancla
  N <- b$poblacion[, list(location_id, sex_id, banda = age_group_id, N = val)]
  dq <- data.table::copy(casc$draws_q)[, banda := b$bandas_pobl$age_group_id[.dl_banda_de(edad, b$bandas_pobl)]]
  n_sim <- nrow(x$f$draws_par[[1]])
  for (columna in c("p", "ipop")) {
    q <- dq[!is.na(banda), list(q = mean(get(columna))), by = list(location_id, sex_id, banda, draw)]
    s <- merge(q, N, by = c("location_id", "sex_id", "banda"))[, list(
      n_dptos = sum(location_id != ancla),
      dpto = sum(N[location_id != ancla] * q[location_id != ancla]),
      nac = N[location_id == ancla] * q[location_id == ancla]), by = list(sex_id, banda, draw)]
    expect_identical(nrow(s), 2L * nrow(b$bandas_pobl) * n_sim, info = columna)
    expect_true(all(s$n_dptos == 25L), info = columna)
    expect_equal(s$dpto, s$nac, tolerance = 1e-10, info = columna)
  }
  # antes de renormalizar la igualdad no se cumple: la prueba no es trivial
  q0 <- data.table::copy(casc$draws_q_sin_renorm)[, banda := b$bandas_pobl$age_group_id[
    .dl_banda_de(edad, b$bandas_pobl)]][!is.na(banda), list(q = mean(p)), by = list(location_id, sex_id, banda, draw)]
  s0 <- merge(q0, N, by = c("location_id", "sex_id", "banda"))[, list(
    dpto = sum(N[location_id != ancla] * q[location_id != ancla]),
    nac = N[location_id == ancla] * q[location_id == ancla]), by = list(sex_id, banda, draw)]
  expect_gt(max(abs(s0$dpto / s0$nac - 1)), 1e-3)
})

test_that(".dl_sd_delta es el m\u00e9todo delta de cada transformaci\u00f3n", {
  x <- 0.3; se <- 0.01; h <- 1e-6
  for (tipo in c("log", "logit", "lineal")) {
    derivada <- (.dl_transformar(x + h, tipo) - .dl_transformar(x - h, tipo)) / (2 * h)
    expect_equal(.dl_sd_delta(x, se, tipo), se * derivada, tolerance = 1e-8, info = tipo)
  }
})

test_that(".dl_desplazamiento_log: delta[k, a] = kappa beta_k sum_b dX[k, b] P[b, a] por departamento", {
  # filas desordenadas a propósito (el departamento "02" primero): la función ordena por departamento y simulación
  dX_c <- data.table::data.table(draw = rep(1:2, 4), location_id = rep(c("02", "01"), each = 4),
                                 age_group_id = rep(rep(c(24L, 25L), each = 2), 2),
                                 dX = c(0.1, 0.2, -0.1, 0.3, 0.5, -0.5, 0.05, 0))
  P <- matrix(c(1, 0, 0.5, 0.5, 0, 1), nrow = 2, dimnames = list(c("24", "25"), NULL))
  beta <- c(2, -1); kappa <- 0.5
  d <- .dl_desplazamiento_log(dX_c, P, beta, kappa, 2L)
  expect_identical(names(d), c("01", "02"))
  for (dp in c("01", "02")) {
    X <- rbind(dX_c[location_id == dp & draw == 1L][order(age_group_id)]$dX,
               dX_c[location_id == dp & draw == 2L][order(age_group_id)]$dX)
    expect_equal(d[[dp]], kappa * beta * (X %*% P), ignore_attr = TRUE)
  }
})

test_that("dl_cascada exige cov_proxy y es determinista con la semilla", {
  x <- fit_v02()
  b0 <- dl_insumos(cfg9100(), rutas_nacional())
  f0 <- dl_ajustar(b0, opciones = dl_opciones_mcmc(simulaciones = 10L, cadenas = 1L, iteraciones = 600L,
                                                   calentamiento = 200L), semilla = 1L)
  expect_error(dl_cascada(f0, b0, kappa = 1, semilla = 1L), "cov_proxy")
  c1 <- dl_cascada(x$f, x$b, kappa = 0.5, semilla = 9L); c2 <- dl_cascada(x$f, x$b, kappa = 0.5, semilla = 9L)
  expect_identical(c1$draws_q, c2$draws_q); expect_identical(c1$kappa, 0.5)
  # kappa menor => departamentos más cerca del nacional
  c1f <- dl_cascada(x$f, x$b, kappa = 1, semilla = 9L)
  disp <- function(cc) cc$draws_q_sin_renorm[location_id != "123", stats::sd(log(p[edad == 70]))]
  expect_lt(disp(c1), disp(c1f))
})

# La cascada arma sus contextos desde los insumos: no lee los del ajuste (un ajuste de una versión anterior los trae
# con otros campos, y uno armado a mano puede no traerlos).
test_that("dl_cascada no depende de los contextos guardados en el ajuste", {
  x <- fit_v02()
  sin_contextos <- x$f; sin_contextos$ctxs <- NULL
  expect_identical(dl_cascada(sin_contextos, x$b, kappa = 1, semilla = 3L)$draws_q,
                   dl_cascada(x$f, x$b, kappa = 1, semilla = 3L)$draws_q)
})

# Escala de HAQI: dX de una covariable lineal se multiplica por betas.escala (0.01 = la beta se estimó con la
# covariable en 0-1 y el GHDx la publica en 0-100). log y logit no la usan.
test_that(".dl_dX multiplica la diferencia lineal por escala", {
  x <- fit_v02()
  px <- .dl_proxies(x$b)
  px1 <- data.table::copy(px)[, `:=`(transformacion = "lineal", escala = 1)]
  px2 <- data.table::copy(px)[, `:=`(transformacion = "lineal", escala = 0.01)]
  d1 <- .dl_dX(x$b, px1, 20L, semilla = 5L); d2 <- .dl_dX(x$b, px2, 20L, semilla = 5L)
  expect_equal(d2$dX, d1$dX * 0.01)
  expect_true("escala" %in% names(px))
})

# ---- gradiente por edad ----------------------------------------------------------------------------------------
# cov_proxy del ejemplo con el SEV (900101) por bandas 24 (15-49), 25 (50-69) y 26 (70+) —ids del catálogo— y
# gradientes declarados, cerrado en el ancla banda a banda con la población contenida; LDI y HAQ siguen en 22.
# Gradiente por departamento: los 25 del ejemplo, 0 salvo los nombrados.
gradiente <- function(g = numeric()) { v <- stats::setNames(rep(0, 25), sprintf("%02d", 1:25)); v[names(g)] <- g; v }
cov_proxy_bandas <- function(b, bandas = c(24L, 25L, 26L),
                             grad = list(`24` = gradiente(c(`01` = 0.2, `03` = -0.2)), `25` = gradiente(),
                                         `26` = gradiente(c(`01` = -0.2, `03` = 0.2)))) {
  pob <- b$poblacion[location_level == 1L]; base <- b$cov_proxy[covariate_id_proxy == 900101L]
  filas <- data.table::rbindlist(lapply(bandas, function(bd) data.table::rbindlist(lapply(1:2, function(sx) {
    N <- .dl_poblacion_en_bandas(pob[sex_id == sx], bd, b$bandas_catalogo)
    g <- grad[[as.character(bd)]][N$location_id]; ancla <- base[sex_id == sx]$ancla_ghdx[1]
    e <- exp(g); v <- ancla * e / (sum(e * N$pob) / sum(N$pob))
    r <- data.table::copy(base[sex_id == sx])[match(N$location_id, location_id)]
    r[, `:=`(age_group_id = bd, valor_calibrado = v, valor_calibrado_se = v * 0.05)]
  }))))
  rbind(filas, data.table::copy(b$cov_proxy[covariate_id_proxy != 900101L])[, age_group_id := 22L])
}
# Insumos por tabla cov_proxy. cov_proxy no entra en la verosimilitud, así que el ajuste es el de fit_v02() (mismos
# datos, opciones y semilla): se reutiliza con el hash de los nuevos insumos en vez de volver a ajustar. Las opciones
# de la cascada tampoco entran en el ajuste ni en el hash: se cambian sobre `b$cfg`. La cascada usa solo los proxies
# de SEV y HAQ: en el ejemplo LDI también mueve la incidencia, y estas pruebas aíslan el gradiente por edad del SEV
# sobre i.
fit_bandas <- local({ cache <- list(); function(mutar = identity, key = "base") {
  if (is.null(cache[[key]])) {
    x <- fit_v02(); csv <- tempfile(fileext = ".csv")
    data.table::fwrite(mutar(cov_proxy_bandas(x$b)), csv)
    p <- rutas_completas(); p$cov_proxy <- csv
    b <- dl_insumos(cfg9100_datos(), p)
    f <- x$f; f$bundle_hash <- b$hash
    b$cfg$covariables <- Filter(function(cv) cv$covariate_name_short != "LDI_pc", b$cfg$covariables)
    cache[[key]] <<- list(b = b, f = f)
  }
  cache[[key]] }})

test_that("dl_cascada por banda: dX lleva la banda y cada edad recibe el gradiente de la suya", {
  x <- fit_bandas(); casc <- suppressWarnings(dl_cascada(x$f, x$b, kappa = 1, semilla = 3L))
  expect_true("age_group_id" %in% names(casc$dX))
  d <- casc$dX[location_id == "01" & covariate_name_short == "SEV_scalar_agestd_cvd_pvd" & sex_id == 1L]
  expect_setequal(unique(d$age_group_id), c(24L, 25L, 26L))
  expect_gt(d[age_group_id == 24L, mean(dX)], 0.1); expect_lt(d[age_group_id == 26L, mean(dX)], -0.1)
  expect_equal(d[age_group_id == 25L, mean(dX)], 0, tolerance = 0.05)
  raw <- casc$draws_q_sin_renorm[location_id == "01"]
  m <- merge(raw, x$f$draws_q, by = c("sex_id", "draw", "edad"), suffixes = c("_d", "_n"))
  expect_gt(mean(m[edad %in% 40:49, i_d > i_n]), 0.9)      # banda 24: +20 %
  expect_gt(mean(m[edad >= 70, i_d < i_n]), 0.9)            # banda 26: -20 %
  expect_equal(m[edad %in% 55:64, mean(log(i_d / i_n))], 0, tolerance = 0.05)   # banda 25: sin gradiente
  expect_null(casc$dx_por_edad$edades_sin_banda); expect_equal(casc$dx_por_edad$fuera_de_banda, "cero")
  # la renormalización sigue cerrando por banda de población y simulación
  qp <- .dl_q_bandas(casc, x$b); pob <- x$b$poblacion[, list(location_id, sex_id, age_group_id, N = val)]
  s <- merge(qp, pob, by = c("location_id", "sex_id", "age_group_id"))[, list(
    dpto = sum(val[location_id != "123"] * N[location_id != "123"]), nac = sum(val[location_id == "123"] * N[location_id == "123"])),
    by = list(sex_id, age_group_id, draw)]
  expect_equal(s$dpto, s$nac, tolerance = 1e-10)
})

test_that("dl_cascada: edades sin banda -> cero deja i igual al nacional; vecina copia la banda más próxima", {
  sin24 <- function(px) px[!(covariate_id_proxy == 900101L & age_group_id == 24L)]
  x0 <- fit_bandas(sin24, key = "sin24_cero")
  c0 <- suppressWarnings(dl_cascada(x0$f, x0$b, kappa = 1, semilla = 3L))
  expect_equal(c0$dx_por_edad$edades_sin_banda, c(30, 49))
  m0 <- merge(c0$draws_q_sin_renorm[location_id == "01"], x0$f$draws_q, by = c("sex_id", "draw", "edad"), suffixes = c("_d", "_n"))
  expect_equal(m0[edad < 50]$i_d, m0[edad < 50]$i_n)
  b1 <- x0$b; b1$cfg$cascada$dx_fuera_de_banda <- list(valor = "vecina", procedencia = "test")
  c1 <- suppressWarnings(dl_cascada(x0$f, b1, kappa = 1, semilla = 3L))
  expect_equal(c1$dx_por_edad$fuera_de_banda, "vecina"); expect_null(c1$dx_por_edad$edades_sin_banda)
  m1 <- merge(c1$draws_q_sin_renorm[location_id == "03"], x0$f$draws_q, by = c("sex_id", "draw", "edad"), suffixes = c("_d", "_n"))
  # 30-49 hereda la banda 25 (50-69): el mismo cociente i_d/i_n por simulación que a los 55 (antes del punto medio
  # 59,5, donde con `lineal` empieza la interpolación hacia la banda 26)
  expect_equal(m1[edad == 40, log(i_d / i_n)], m1[edad == 55, log(i_d / i_n)], tolerance = 1e-10)
})

test_that(".dl_pesos_edad: escalon = banda que contiene; lineal interpola entre puntos medios y es constante en los extremos", {
  cat <- .dl_bandas_catalogo(rutas_nacional())
  # bandas del catálogo: 24 = [15, 50) medio 32,5; 25 = [50, 70) medio 60; 26 = [70, 125) medio (70 + 85)/2 = 77,5
  e <- c(10, 30, 32.5, 46.25, 50, 60, 68.75, 77.5, 90)
  We <- .dl_pesos_edad(e, c(24L, 25L, 26L), cat, "cero", "escalon")
  expect_equal(unname(We[, 1]), c(0, 0, 0)); expect_equal(unname(We[, 4]), c(1, 0, 0)); expect_equal(unname(We[, 5]), c(0, 1, 0))
  Wl <- .dl_pesos_edad(e, c(24L, 25L, 26L), cat, "cero", "lineal")
  expect_equal(dim(Wl), c(3L, 9L)); expect_equal(rownames(Wl), c("24", "25", "26"))
  expect_equal(unname(colSums(Wl)), c(0, 1, 1, 1, 1, 1, 1, 1, 1))     # fuera (10): nada; dentro: pesos que suman 1
  expect_equal(unname(Wl[, 2]), c(1, 0, 0))                          # 30 <= medio 32,5: constante = banda 24
  expect_equal(unname(Wl[, 4]), c(0.5, 0.5, 0))                      # 46,25 = medio entre 32,5 y 60
  expect_equal(unname(Wl[, 7]), c(0, 0.5, 0.5))                      # 68,75 = medio entre 60 y 77,5
  expect_equal(unname(Wl[, 9]), c(0, 0, 1))                          # 90 >= medio 77,5: constante = banda 26
  # vecina rellena fuera de las bandas con la más próxima; una sola banda es constante en todas partes
  Wv <- .dl_pesos_edad(e, c(25L, 26L), cat, "vecina", "lineal")
  expect_equal(unname(Wv[, 1]), c(1, 0)); expect_equal(unname(Wv[, 2]), c(1, 0))
  # sin banda con `cero`: columna vacía (30 y 32,5 quedan fuera de 25/26); con `escalon` la 90 cae en la 26
  Wc <- .dl_pesos_edad(e, c(25L, 26L), cat, "cero", "escalon")
  expect_equal(unname(colSums(Wc)[1:3]), c(0, 0, 0)); expect_equal(unname(Wc[, 9]), c(0, 1))
  expect_error(.dl_pesos_edad(e, 999L, cat, "cero", "lineal"), "sin l\u00edmites")
  W1 <- .dl_pesos_edad(e, 22L, cat, "cero", "lineal")
  expect_equal(unname(W1[1, ]), rep(1, 9))
})

test_that("dl_cascada lineal: entre bandas el efecto varía de forma continua; `escalon` lo deja constante", {
  x <- fit_bandas(); casc <- suppressWarnings(dl_cascada(x$f, x$b, kappa = 1, semilla = 3L))
  expect_equal(casc$dx_por_edad$interpolacion, "lineal")
  m <- merge(casc$draws_q_sin_renorm[location_id == "01" & sex_id == 1L], x$f$draws_q[sex_id == 1L], by = c("draw", "edad"), suffixes = c("_d", "_n"))
  lr <- m[, list(lr = mean(log(i_d / i_n))), by = edad][order(edad)]
  # 01: +0,2 en la banda 24 (medio 32,5), 0 en la 25 (medio 60), -0,2 en la 26 (medio 77,5): decreciente y sin saltos
  expect_true(all(diff(lr[edad >= 33 & edad <= 78]$lr) <= 1e-9))
  expect_lt(max(abs(diff(lr$lr))), 0.03)                              # ningún salto de banda (`escalon` daría ~0,17)
  expect_true(lr[edad == 46]$lr < lr[edad == 32]$lr && lr[edad == 46]$lr > lr[edad == 60]$lr)   # entre los dos puntos medios
  be <- x$b; be$cfg$cascada$dx_interpolacion <- list(valor = "escalon", procedencia = "test")
  ce <- suppressWarnings(dl_cascada(x$f, be, kappa = 1, semilla = 3L))
  expect_equal(ce$dx_por_edad$interpolacion, "escalon")
  me <- merge(ce$draws_q_sin_renorm[location_id == "01" & sex_id == 1L], x$f$draws_q[sex_id == 1L], by = c("draw", "edad"), suffixes = c("_d", "_n"))
  lre <- me[, list(lr = mean(log(i_d / i_n))), by = edad][order(edad)]
  expect_equal(lre[edad == 40]$lr, lre[edad == 48]$lr, tolerance = 1e-10)     # constante dentro de la banda 24
  expect_gt(abs(lre[edad == 49]$lr - lre[edad == 50]$lr), 0.1)                # y salto en el corte 49/50
})

# Cascada plana declarada: causas cuyo ancla no tiene ninguna covariable con proxy departamental. Tasas
# nacionales en cada departamento (dX = 0 en todo), conteos por población, sin gradiente; la limitación va al
# manifiesto de la corrida.
test_that("cascada plana: tasas nacionales por departamento, renorm 1, sin dX; rechaza cov_proxy", {
  x <- fit_v02(); b <- x$b
  b$cfg$cascada$modo <- list(valor = "plana", procedencia = "test")
  expect_error(dl_cascada(x$f, b, kappa = 1, semilla = 3L), "cov_proxy")
  b$cov_proxy <- b$cov_proxy[0]
  casc <- dl_cascada(x$f, b, kappa = 1, semilla = 3L)
  expect_s3_class(casc, "dl_cascade")
  dptos <- sort(unique(b$poblacion[location_level == 1L]$location_id))
  expect_identical(casc$departamentos, dptos)
  expect_setequal(unique(casc$draws_q$location_id), c(b$loc_ancla, dptos))
  for (d in dptos) {
    dq <- casc$draws_q[location_id == d][, location_id := NULL]
    expect_equal(dq, x$f$draws_q[, list(sex_id, draw, edad, p, i, f, ipop)], tolerance = 1e-12)
  }
  expect_true(all(abs(casc$renorm$factor - 1) < 1e-10))
  expect_equal(nrow(casc$dX), 0L)
  expect_identical(casc$modo, "plana")
  # el modo proxy sigue exigiendo cov_proxy
  b$cfg$cascada$modo <- list(valor = "proxy")
  expect_error(dl_cascada(x$f, b, kappa = 1, semilla = 3L), "cov_proxy est\u00e1 vac\u00eda")
})

# Sustitución declarada: con la beta en log, el ancla se cancela en dX = log(X_d) - log(X_nac); anclar el proxy
# en otra covariable (misma razón departamental, otra escala nacional) deja dX igual en media.
test_that("cascada con sustituye: dX no cambia en media al anclar el proxy en la covariable sustituta", {
  x <- fit_v02(); b <- x$b
  ref <- dl_cascada(x$f, b, kappa = 1, semilla = 3L)
  ldi <- unique(b$cov_proxy[covariate_id_proxy == 900102L]$ancla_ghdx)
  px <- data.table::copy(b$cov_proxy)
  px[covariate_id_proxy == 900101L, `:=`(valor_calibrado = valor_calibrado / ancla_ghdx * ldi,
                                          valor_calibrado_se = valor_calibrado_se / ancla_ghdx * ldi,
                                          ancla_ghdx = ldi, covariate_id_gbd = 57L)]
  b$cov_proxy <- px
  b$cfg$covariables[[1]]$sustituye <- list(covariate_id = 57L, covariate_name_short = "LDI_pc", procedencia = "test")
  casc <- dl_cascada(x$f, b, kappa = 1, semilla = 3L)
  m <- merge(ref$dX[covariate_name_short == "SEV_scalar_agestd_cvd_pvd", list(ref = mean(dX)), by = list(location_id, sex_id)],
             casc$dX[covariate_name_short == "SEV_scalar_agestd_cvd_pvd", list(sus = mean(dX)), by = list(location_id, sex_id)],
             by = c("location_id", "sex_id"))
  expect_equal(m$sus, m$ref, tolerance = 0.05)
  expect_equal(casc$sustituciones$covariate_name_short, "SEV_scalar_agestd_cvd_pvd")
  expect_equal(casc$sustituciones$covariate_id_nacional, 57L)
})

