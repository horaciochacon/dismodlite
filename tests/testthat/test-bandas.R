# Bandas de edad (R/bandas.R): bandas finas de la población, contención [inicio, fin) y promedios poblacionales
# sobre intervalos de edad.

test_that("bandas_poblacion ignora agregados (All ages, <5) y rechaza particiones solapadas", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  # población con el 80+ agregado (21) y sin sus bandas finas, que el ejemplo también trae
  pobl <- b$poblacion[!age_group_id %in% c(30L, 31L, 32L, 235L)]
  # como la población del INEI: agregados All ages (22) y <5 (1) junto a las quinquenales
  extra <- data.table::rbindlist(list(
    data.table::copy(pobl[1])[, `:=`(age_group_id = 22L, val = 1e7)],
    data.table::copy(pobl[1])[, `:=`(age_group_id = 1L, val = 3e6)]))
  bp <- .dl_bandas_poblacion(rbind(pobl, extra), .dl_bandas_catalogo(rutas_nacional()))
  expect_false(22L %in% bp$age_group_id)                 # All ages contiene a las demás: fuera
  expect_true(1L %in% bp$age_group_id)                   # <5 no contiene ninguna presente: es banda real
  expect_setequal(setdiff(bp$age_group_id, 1L), unique(pobl$age_group_id))
  # 80+ (21) junto a 80-84 (30): el agregado 21 cae y queda la fina; 85+ queda sin banda al usarla
  fina <- rbind(pobl, data.table::copy(pobl[1])[, `:=`(age_group_id = 30L, val = 1e5)])
  bp2 <- .dl_bandas_poblacion(fina, .dl_bandas_catalogo(rutas_nacional()))
  expect_true(30L %in% bp2$age_group_id && !21L %in% bp2$age_group_id)
  expect_error(.dl_pesos_intervalo(40:99, 80, 125, fina[sex_id == 1L], bp2), "sin banda")
})

test_that("contención [inicio, fin): la edad de inicio entra, la de fin no; fuera de las bandas, NA", {
  lim <- data.table::data.table(age_group_id = c(11L, 12L, 13L), age_start = c(30, 35, 40), age_end = c(35, 40, 45))
  expect_identical(.dl_banda_de(c(29, 30, 34.99, 35, 44.5, 45), lim), c(NA, 1L, 1L, 2L, 3L, NA))
})

test_that("pesos de intervalo a mano: cada edad pesa con la población de su banda fina", {
  bandas <- data.table::data.table(age_group_id = c(11L, 12L, 13L), age_start = c(30, 35, 40), age_end = c(35, 40, 45))
  pobl <- data.table::data.table(age_group_id = c(13L, 11L, 12L), val = c(600, 100, 300))   # desordenada a propósito
  anual <- 30:44
  # [33, 42): A = 33, ..., 41; 33-34 en la banda 11 (N = 100), 35-39 en la 12 (N = 300), 40-41 en la 13 (N = 600)
  pw <- .dl_pesos_intervalo(anual, 33, 42, pobl, bandas)
  N <- c(100, 100, 300, 300, 300, 300, 300, 600, 600)
  expect_identical(anual[pw$idx], 33:41)
  expect_equal(pw$w, N / 2900)
  expect_equal(sum(pw$w), 1)
  x <- anual / 100
  expect_equal(.dl_q_intervalos(x, list(pw)), sum(N * (33:41) / 100) / 2900, tolerance = 1e-12)
  # dentro de una sola banda fina, el promedio es la media simple de las edades
  pw1 <- .dl_pesos_intervalo(anual, 35, 40, pobl, bandas)
  expect_equal(pw1$w, rep(0.2, 5))
  expect_equal(.dl_q_intervalos(x, list(pw, pw1)), c(sum(N * (33:41) / 100) / 2900, mean((35:39) / 100)),
               tolerance = 1e-12)
  # una edad sin banda o un intervalo sin edades de la malla son errores, no ceros
  expect_error(.dl_pesos_intervalo(28:44, 28, 32, pobl, bandas), "sin banda")
  expect_error(.dl_pesos_intervalo(anual, 45, 50, pobl, bandas), "ninguna edad")
  expect_error(.dl_pesos_intervalo(anual, 30, 35, pobl[age_group_id != 11L], bandas), "sin banda")
})

test_that("pesos por intervalo usan los límites del catálogo y fallan si falta una banda", {
  b <- dl_insumos(cfg9100(), rutas_nacional())
  anual <- .dl_ctx(b, 1L)$anual; pobl <- b$poblacion[location_id == b$loc_ancla & sex_id == 1L]
  pw <- .dl_pesos_intervalo(anual, 80, 125, pobl, b$bandas_pobl)   # 80+ = bandas 30, 31, 32 y 235
  expect_equal(sum(pw$w), 1)
  expect_true(all(anual[pw$idx] >= 80))
  # grilla desde 30; una población sin la banda 30-34 (11) no cubre [30, 35)
  expect_error(.dl_pesos_intervalo(anual, 30, 35, pobl, b$bandas_pobl[age_group_id != 11L]), "sin banda")
})

test_that("agregación por bandas devuelve location_id y usa la población de cada ubicación", {
  b <- dl_insumos(cfg9100_datos(), rutas_completas())
  f <- dl_ajustar(b, opciones = dl_opciones_mcmc(simulaciones = 10L, cadenas = 1L, iteraciones = 600L,
                                                 calentamiento = 200L), semilla = 2L)
  q <- .dl_q_bandas(f, b)
  expect_identical(names(q), c("location_id", "sex_id", "age_group_id", "draw", "val"))
  expect_true(all(q$location_id == b$loc_ancla))
  # una segunda ubicación con p doble: su q por banda es el doble donde la banda del ancla es una sola banda de
  # población (40-44 ... 75-79)
  f2 <- f; d2 <- data.table::copy(f$draws_q)[, `:=`(location_id = "01", p = p * 2, ipop = ipop * 2)]
  f2$draws_q <- rbind(f$draws_q, d2)
  q2 <- .dl_q_bandas(f2, b)
  m <- merge(q2[location_id == "123"], q2[location_id == "01"], by = c("sex_id", "age_group_id", "draw"))
  expect_equal(m[age_group_id != 21L]$val.y, 2 * m[age_group_id != 21L]$val.x, tolerance = 1e-12)
  # en 80+ cada edad pesa con la población de SU banda fina (80-84, 85-89, 90-94, 95+) de la ubicación "01"
  pob01 <- b$poblacion[location_id == "01"]
  fina <- function(edad) c(30L, 31L, 32L, 235L)[findInterval(edad, c(80, 85, 90, 95))]
  a_mano <- d2[edad >= 80][, N := pob01$val[match(paste(sex_id, fina(edad)), paste(pob01$sex_id, pob01$age_group_id))]]
  a_mano <- a_mano[, list(esperado = sum(p * N) / sum(N)), by = list(sex_id, draw)]
  m21 <- merge(q2[location_id == "01" & age_group_id == 21L], a_mano, by = c("sex_id", "draw"))
  expect_identical(nrow(m21), nrow(q2[location_id == "01" & age_group_id == 21L]))
  expect_equal(m21$val, m21$esperado, tolerance = 1e-12)
  st <- .dl_stats_bandas(q2)
  expect_true(all(c("location_id", "sex_id", "age_group_id", "val", "lower", "upper") %in% names(st)))
  qi <- .dl_ipop_bandas(f2, b)
  expect_setequal(unique(qi$location_id), c("123", "01"))
})
