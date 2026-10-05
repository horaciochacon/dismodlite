# Reparto subnacional por razón (subnacional.modo: razon; R/razon.R): la configuración y su traducción, las reglas de
# la tabla razones, el cálculo del reparto y la corrida. El proyecto de prueba es el país ficticio con tres regiones
# (escribir_proyecto_razon(), helper-dismodlite.R).

proyecto_razon <- function(..., env = parent.frame()) {
  d <- file.path(withr::local_tempdir(.local_envir = env), "país razón")
  escribir_proyecto_razon(d, ...)
}
razones_de <- function(d) leer_texto(file.path(d, "razones.csv"))
revisar_callado <- function(...) { salida <- utils::capture.output(r <- dl_revisar_proyecto(...)); r }

# ---- La configuración y las reglas de la tabla ----

test_that("subnacional.modo: razon se traduce a cascada.modo: razon y la semilla del sorteo va en avanzado", {
  d <- proyecto_razon()
  p <- dl_proyecto(d)
  expect_identical(p$configuracion$origen$subnacional, "razon")
  expect_identical(p$configuracion$cascada$modo$valor, "razon")
  expect_null(p$configuracion$cascada$razon_semilla)
  expect_identical(names(p$tablas$razones), c("ubicacion", "anio", "razon", "error_log", "fuente"))
  expect_output(print(p), "subnacional: razon")
  b <- suppressMessages(dl_insumos(p))
  expect_identical(nrow(b$cov_proxy), 0L)
  expect_identical(b$contrato$razones$razon, c(1.4, 0.7, 0))
  # la semilla del sorteo: un entero en avanzado; otra cosa, o sin el modo razon, es un problema de la configuración
  d2 <- proyecto_razon(config = c("avanzado:", "  cascada:", "    razon_semilla: 20261002"))
  expect_identical(dl_proyecto(d2)$configuracion$cascada$razon_semilla, 20261002L)
  d3 <- proyecto_razon(config = c("avanzado:", "  cascada:", "    razon_semilla: 1.5"))
  expect_error(dl_proyecto(d3), "cascada.razon_semilla: debe ser un entero no negativo")
  cfg <- readLines(file.path(d2, "config.yaml"))
  writeLines(sub("modo: razon", "modo: plano", cfg), file.path(d2, "config.yaml"))
  expect_error(suppressWarnings(dl_proyecto(d2)), "cascada.razon_semilla: solo se usa con cascada.modo: razon")
})

test_that("modo razon sin la tabla razones, o con covariables subnacionales, es un problema de la configuración", {
  d <- proyecto_razon()
  unlink(file.path(d, "razones.csv"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_match(e$problemas, "^subnacional.modo: es razon y el proyecto no trae la tabla razones")
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "configuración" & r$estado == "error"], "no trae la tabla razones")
  # con valores subnacionales de una covariable (el ejemplo los calibra de proxies_crudos): el reparto no los usa
  d <- copia_ejemplo()
  cfg <- readLines(file.path(d, "config", "9101.yaml"), encoding = "UTF-8")
  writeLines(enc2utf8(append(cfg, "  modo: razon", match("subnacional:", cfg))), file.path(d, "config", "9101.yaml"),
             useBytes = TRUE)
  rz <- data.frame(ubicacion = sprintf("%02d", 1:25), anio = 2023L, razon = 1, error_log = 0)
  e <- expect_error(dl_proyecto(d, 9101, razones = rz), class = "dl_error")
  expect_match(e$problemas, "es razon y el proyecto trae valores subnacionales de la\\(s\\) covariable\\(s\\) .*haqi")
  expect_match(e$problemas, "el reparto por razón no usa covariables")
})

test_that("subnacional.modo y avanzado: cascada: modo no pueden discrepar respecto de razon", {
  remedio <- "declara el modo en `subnacional.modo`; `avanzado: cascada: modo` no puede cambiarlo a/desde `razon`"
  # plano en subnacional.modo y razon por avanzado (sin la tabla razones): antes se leía sin error ni aviso
  d <- proyecto_razon(config = c("avanzado:", "  cascada:", "    modo: {valor: razon, procedencia: x}",
                                 "    razon_semilla: 7"))
  cfg <- readLines(file.path(d, "config.yaml"))
  writeLines(sub("^  modo: razon", "  modo: plano", cfg), file.path(d, "config.yaml"))
  unlink(file.path(d, "razones.csv"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_identical(e$problemas, paste0("subnacional.modo: es plano y `avanzado: cascada: modo` es razon: ", remedio))
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "configuración" & r$estado == "error"],
               "subnacional.modo: es plano y `avanzado: cascada: modo` es razon")
  # el modo deducido (sin subnacional.modo) tampoco puede cambiarse a razon, ni con el valor suelto
  writeLines(c("causa: 7001", "anio: 2023", "edad_inicio: 40", "avanzado:", "  cascada:", "    modo: razon"),
             file.path(d, "config.yaml"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_identical(e$problemas, paste0("subnacional.modo: es plano y `avanzado: cascada: modo` es razon: ", remedio))
  # al revés: razon en subnacional.modo y plana por avanzado
  d <- proyecto_razon(config = c("avanzado:", "  cascada:", "    modo: {valor: plana, procedencia: x}"))
  e <- expect_error(dl_proyecto(d), class = "dl_error")
  expect_identical(e$problemas, paste0("subnacional.modo: es razon y `avanzado: cascada: modo` es plana: ", remedio))
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "configuración" & r$estado == "error"],
               "subnacional.modo: es razon y `avanzado: cascada: modo` es plana")
  # coherente: razon en subnacional.modo, la semilla por avanzado (y el mismo modo repetido en avanzado)
  d <- proyecto_razon(config = c("avanzado:", "  cascada:", "    modo: {valor: razon, procedencia: x}",
                                 "    razon_semilla: 7"))
  p <- dl_proyecto(d)
  expect_identical(p$configuracion$cascada$modo$valor, "razon")
  expect_identical(p$configuracion$cascada$razon_semilla, 7L)
  # el formato completo no cambia: cascada.modo: razon sigue siendo un valor admitido
  completa <- dl_configuracion(9100L, file.path(ruta_acs(), "config"), cambios = list(
    cascada = list(modo = list(valor = "razon", procedencia = "x"), razon_semilla = 7)))
  expect_identical(completa$cascada$modo$valor, "razon")
})

test_that("la tabla razones debe traer todas las ubicaciones subnacionales del año, y solo ellas", {
  d <- proyecto_razon()
  rz <- razones_de(d)
  problemas <- function(tabla) {
    escribir_texto(tabla, d, "razones.csv")
    expect_error(dl_proyecto(d), class = "dl_error")$problemas
  }
  # falta una ubicación: el error la nombra
  expect_identical(problemas(rz[ubicacion != "B"]), paste0(
    "razones: falta la razón de 2023 de la(s) ubicación(es) subnacional(es) B: el reparto necesita la de ",
    "todas las de la población de ese año"))
  # sobra la nacional
  sobra <- rbind(rz, data.table::data.table(anio = "2023", ubicacion = "999", razon = "1", error_log = "0",
                                            fuente = "x"))
  expect_match(problemas(sobra), "^razones: trae la razón de 2023 de 999, que no es/son")
  # todas en cero: no hay nada que repartir
  expect_match(problemas(data.table::copy(rz)[, razon := "0"]), "^razones: todas las razones de 2023 son 0")
  # una razón que no es finita
  expect_match(problemas(data.table::copy(rz)[ubicacion == "A", razon := "Inf"]),
               "^razones: razon y error_log deben ser números finitos \\(ubicación\\(es\\) A\\)")
  # el problema nombra la columna que no es finita en cada ubicación
  infinitas <- data.table::copy(rz)[ubicacion == "A", razon := "Inf"][ubicacion == "B", error_log := "Inf"]
  expect_identical(problemas(infinitas),
                   paste0("razones: razon y error_log deben ser números finitos (ubicación(es) A, B): razon en A; ",
                          "error_log en B"))
  # NaN y -Inf no llegan a la regla: la tabla los rechaza al leerla, con la columna y la fila
  expect_match(problemas(data.table::copy(rz)[ubicacion == "A", razon := "NaN"]), "^razon: NaN no es un número")
  expect_match(problemas(data.table::copy(rz)[ubicacion == "B", error_log := "NaN"]),
               "^error_log: NaN no es un número")
  expect_match(problemas(data.table::copy(rz)[ubicacion == "A", razon := "-Inf"]),
               "^razon: hay valores menores que 0")
  # una fila de la causa y otra sin causa para la misma ubicación
  dos <- rbind(data.table::copy(rz)[, causa := "7001"], data.table::copy(rz)[ubicacion == "A"][, causa := ""])
  expect_match(problemas(dos), "^razones: más de una razón de 2023 para la\\(s\\) ubicación\\(es\\) A")
  # las razones de otra causa no cuentan: la causa se queda sin razones del año
  expect_match(problemas(data.table::copy(rz)[, causa := "7002"]),
               "^razones: no trae las razones de la causa 7001 de 2023 .*años que trae: ninguno\\)$")
  # sin la población del año que se estima la regla calla: el problema es de la población, no «quítala(s)»
  d2 <- proyecto_razon(anios = 2024L, anios_poblacion = 2023L)
  e <- expect_error(suppressMessages(dl_proyecto(d2, anio = 2024)), class = "dl_error")
  expect_match(e$problemas, "^poblacion: ", all = TRUE)
  # la revisión lo dice en el paso «proyecto», sin detenerse
  escribir_texto(rz[ubicacion != "B"], d, "razones.csv")
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "error"], "^razones: falta la razón de 2023")
})

test_that("la tabla razones sin el modo razon es un aviso: no se usa", {
  d <- proyecto_razon()
  cfg <- readLines(file.path(d, "config.yaml"))
  writeLines(sub("modo: razon", "modo: plano", cfg), file.path(d, "config.yaml"))
  expect_warning(p <- dl_proyecto(d), "razones: la tabla no se usa: subnacional.modo es plano")
  expect_identical(p$configuracion$cascada$modo$valor, "plana")
  r <- revisar_callado(d)
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "aviso"], "razones: la tabla no se usa")
  # sin el modo razon las reglas de la tabla no corren: una razón infinita y la fila de la nacional siguen en aviso
  rz <- razones_de(d)
  escribir_texto(rbind(data.table::copy(rz)[ubicacion == "A", razon := "Inf"],
                       data.table::data.table(anio = "2023", ubicacion = "999", razon = "1", error_log = "Inf",
                                              fuente = "x")), d, "razones.csv")
  expect_warning(p <- dl_proyecto(d), "razones: la tabla no se usa: subnacional.modo es plano")
  r <- revisar_callado(d)
  expect_false(any(r$estado == "error"))
  expect_match(r$detalle[r$paso == "proyecto" & r$estado == "aviso"], "razones: la tabla no se usa")
})

test_that("dl_correr(anios = ) con un año sin razones se detiene antes de correr el primero", {
  # razones de 2023 y de 2024 en la misma tabla, población de 2023, 2024 y 2025 y el ancla de 2023 (2024 se proyecta)
  d <- proyecto_razon(anios = c(2023L, 2024L), anios_poblacion = 2023:2025)
  expect_identical(suppressMessages(dl_proyecto(d, anio = 2024))$configuracion$cascada$modo$valor, "razon")
  rz <- razones_de(d)
  escribir_texto(rz[anio == "2023"], d, "razones.csv")
  salida <- withr::local_tempdir()
  e <- expect_error(suppressMessages(dl_correr(d, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                               carpeta_salida = salida, anios = c(2023, 2024))),
                    "^dl_correr\\(\\): el proyecto no se puede leer para el año 2024: ", class = "dl_error")
  expect_match(conditionMessage(e), paste0("razones: no trae las razones de la causa 7001 de 2024 \\(el año que se ",
                                           "estima; años que trae: 2023\\)"))
  expect_match(conditionMessage(e), "No se corrió ningún año")
  expect_identical(e$anio, 2024L)
  expect_identical(e$escritas, integer())
  expect_false(dir.exists(file.path(salida, "mod")))
})

# ---- El cálculo del reparto ----

test_that("el reparto de una celda es la fórmula escrita aparte: razón 0 sin casos y cierre exacto en el nacional", {
  # tres ubicaciones y cuatro simulaciones, a mano; la segunda ubicación con razón 0 y la tercera con error 0
  razon <- c(1.5, 0, 0.8); error_log <- c(0.1, 0.3, 0)
  n <- 4L; semilla <- 42L
  R <- .dl_razon_simular(razon, error_log, n, semilla)
  # la misma matriz, término a término: un único rnorm() tras set.seed(), que llena la matriz por columnas
  set.seed(semilla)
  z <- stats::rnorm(3L * n)
  R_esperada <- matrix(NA_real_, 3L, n)
  for (j in 1:n) for (d in 1:3)
    R_esperada[d, j] <- if (razon[d] == 0) 0 else razon[d] * exp(error_log[d] * z[(j - 1L) * 3L + d])
  expect_identical(R, R_esperada)
  expect_identical(R[2, ], rep(0, n))
  expect_identical(R[3, ], rep(0.8, n))
  # tasa_dj = tasa_nac_j R_dj N_nac / sum_d R_dj N_d, con un bucle por simulación y por ubicación
  tasa_nac <- c(0.010, 0.012, 0.008, 0.011)
  N_sub <- c(1000, 500, 2500); N_nac <- 4000
  tasas <- .dl_razon_celda(tasa_nac, N_nac, N_sub, R)
  esperadas <- matrix(NA_real_, 3L, n)
  for (j in 1:n) {
    suma <- 0
    for (d in 1:3) suma <- suma + R[d, j] * N_sub[d]
    for (d in 1:3) esperadas[d, j] <- tasa_nac[j] * R[d, j] * N_nac / suma
  }
  expect_equal(tasas, esperadas, tolerance = 1e-14)
  expect_identical(tasas[2, ], rep(0, n))
  # cierre, simulación a simulación: los casos de las ubicaciones suman los nacionales
  expect_equal(colSums(tasas * N_sub), tasa_nac * N_nac, tolerance = 1e-14)
  # la misma semilla da la misma matriz; otra semilla, otra
  expect_identical(.dl_razon_simular(razon, error_log, n, semilla), R)
  expect_false(identical(.dl_razon_simular(razon, error_log, n, semilla + 1L), R))
})

test_that("el reparto de una medida usa la misma matriz en todas las celdas y no toca las simulaciones nacionales", {
  n <- 4L
  R <- .dl_razon_simular(c(1.5, 0, 0.8), c(0.1, 0.3, 0), n, 42L)
  rownames(R) <- c("A", "B", "C")
  celdas <- data.table::CJ(sex_id = 1:2, age_group_id = c(13L, 14L))
  nac <- celdas[, list(location_id = "999", draw = 1:n, val = 0.01 * sex_id + 0.001 * age_group_id + 0.0001 * (1:n)),
                by = list(sex_id, age_group_id)]
  # con las tasas nacionales de la cascada plana en cada ubicación, que el reparto reemplaza
  planas <- lapply(c("A", "B", "C"), function(u) data.table::copy(nac)[, location_id := u])
  draws <- rbind(nac, data.table::rbindlist(planas))
  pob <- data.table::CJ(location_id = c("A", "B", "C"), sex_id = 1:2, age_group_id = c(13L, 14L))
  pob[, pob := c(A = 1000, B = 500, C = 2500)[location_id] * sex_id + age_group_id]
  pob <- rbind(pob, pob[, list(location_id = "999", pob = sum(pob)), by = list(sex_id, age_group_id)])
  out <- .dl_razon_repartir(draws, pob, R, "999")
  expect_identical(names(out), c("sex_id", "age_group_id", "location_id", "draw", "val"))
  expect_identical(nrow(out), 4L * 4L * n)
  data.table::setcolorder(out, c("location_id", "sex_id", "age_group_id", "draw", "val"))
  expect_identical(out, data.table::setorder(data.table::copy(out), location_id, sex_id, age_group_id, draw))
  ref <- data.table::setorder(data.table::copy(nac), sex_id, age_group_id, draw)
  expect_identical(out[location_id == "999"]$val, ref$val)
  expect_true(all(out[location_id == "B"]$val == 0))
  # en cada celda y simulación: cierre exacto, y tasa_d / tasa_nac proporcional a la misma R_dj
  x <- merge(out[location_id != "999"], pob, by = c("location_id", "sex_id", "age_group_id"))
  cierre <- x[, list(casos = sum(val * pob)), by = list(sex_id, age_group_id, draw)]
  cierre <- merge(cierre, merge(ref, pob[location_id == "999"], by = c("location_id", "sex_id", "age_group_id")),
                  by = c("sex_id", "age_group_id", "draw"))
  expect_equal(cierre$casos, cierre$val * cierre$pob, tolerance = 1e-14)
  cociente <- data.table::dcast(out[location_id %in% c("A", "C")], sex_id + age_group_id + draw ~ location_id,
                                value.var = "val")
  expect_equal(cociente$A / cociente$C, rep(R["A", ] / R["C", ], times = 4L), tolerance = 1e-13)
})

test_that("una celda sin población en las ubicaciones con razón mayor que 0 es un error que nombra el sexo y la banda", {
  n <- 4L
  R <- .dl_razon_simular(c(1.5, 0, 0.8), c(0.1, 0.3, 0), n, 42L)
  rownames(R) <- c("A", "B", "C")
  celdas <- data.table::CJ(sex_id = 1:2, age_group_id = c(13L, 14L))
  nac <- celdas[, list(location_id = "999", draw = 1:n, val = 0.01 * sex_id + 0.001 * age_group_id),
                by = list(sex_id, age_group_id)]
  pob <- data.table::CJ(location_id = c("A", "B", "C"), sex_id = 1:2, age_group_id = c(13L, 14L))
  pob[, pob := c(A = 1000, B = 500, C = 2500)[location_id] * sex_id + age_group_id]
  # en la celda sexo 2, banda 14 solo tiene población B, la de razón 0: el denominador del reparto es 0
  pob[sex_id == 2L & age_group_id == 14L & location_id != "B", pob := 0]
  pob <- rbind(pob, pob[, list(location_id = "999", pob = sum(pob)), by = list(sex_id, age_group_id)])
  expect_error(.dl_razon_repartir(nac, pob, R, "999"),
               paste0("el reparto por razón no da tasas finitas en la celda sexo 2, banda 14: las ubicaciones con ",
                      "razón mayor que 0 no tienen población en esa celda"), class = "dl_error")
  # un error_log desmedido desborda la exponencial: el mismo error, en la primera celda
  R2 <- .dl_razon_simular(c(1.5, 0, 0.8), c(1e4, 0.3, 0), n, 42L)
  rownames(R2) <- c("A", "B", "C")
  pob[location_id != "999" & sex_id == 2L & age_group_id == 14L, pob := 100]
  expect_error(.dl_razon_repartir(nac, pob, R2, "999"),
               "el reparto por razón no da tasas finitas en la celda sexo 1, banda 13", class = "dl_error")
})

# La corrida mínima del país ficticio en modo razón (cadenas cortas), memorizada en la sesión: insumos, ajuste,
# ajuste solo con el ancla, cascada (la plana), AVD y reparto.
corrida_razon <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      d <- file.path(tempdir(), "corrida_razon")
      if (!dir.exists(d)) escribir_proyecto_razon(d)
      b <- suppressMessages(dl_insumos(dl_proyecto(d)))
      o <- dl_opciones_mcmc(simulaciones = 40L, cadenas = 2L, iteraciones = 2000L, calentamiento = 1000L)
      f <- dl_ajustar(b, o, semilla = 5L)
      casc <- dl_cascada(f, b, semilla = 5L)
      y <- dl_avd(casc, b, semilla = 5L)
      x <<- list(carpeta = d, insumos = b, opciones = o, ajuste = f, cascada = casc, avd = y,
                 ajuste_prior = dl_ajustar_solo_prior(b, o, semilla = 5L, ajuste = f),
                 reparto = dl_repartir_razon(list(ajuste = casc, avd = y, insumos = b)))
    }
    x
  }
})

test_that("con el modo razon la cascada es la plana y dl_repartir_razon() reparte las tres medidas con cierre", {
  m <- corrida_razon()
  b <- m$insumos; casc <- m$cascada; rep <- m$reparto
  expect_identical(casc$modo, "plana")
  expect_identical(casc$departamentos, c("A", "B", "C"))
  expect_s3_class(rep, "dl_reparto")
  expect_identical(names(rep$draws), c("prevalence", "incidence", "yld"))
  expect_identical(rep$ubicaciones, c("A", "B", "C"))
  expect_identical(rep$semilla, 5L)                      # sin otra, la de la cascada
  expect_identical(rep$anio, 2023L)
  expect_identical(rep$razones$razon, c(1.4, 0.7, 0))
  expect_identical(rep$razones$razon_media[3], 0)
  rz <- rep$razones
  expect_true(all(rz$razon_lower <= rz$razon_media & rz$razon_media <= rz$razon_upper))
  expect_output(print(rep), paste0("<dl_reparto> reparto subnacional por razón | año 2023 | 3 ubicaciones | razones ",
                                   "de 0 a 1.4 (1 en cero) | semilla 5"), fixed = TRUE)
  # las simulaciones nacionales son las del ajuste; las de C (razón 0), cero
  expect_identical(rep$draws$prevalence[location_id == "999"]$val, m$avd$draws_prev_banda[location_id == "999"]$val)
  expect_identical(rep$draws$yld[location_id == "999"]$val, m$avd$draws_yld[location_id == "999"]$val)
  for (s in names(rep$draws)) expect_true(all(rep$draws[[s]][location_id == "C"]$val == 0), info = s)
  # cierre: la suma ponderada por población de las tasas subnacionales es la nacional, simulación a simulación
  pob <- b$poblacion[, list(location_id, sex_id, age_group_id, N = val)]
  for (s in names(rep$draws)) {
    x <- merge(rep$draws[[s]], pob, by = c("location_id", "sex_id", "age_group_id"))
    sub <- x[location_id != "999", list(casos = sum(val * N)), by = list(sex_id, age_group_id, draw)]
    nac <- x[location_id == "999", list(sex_id, age_group_id, draw, casos_nac = val * N)]
    cierre <- merge(sub, nac, by = c("sex_id", "age_group_id", "draw"))
    expect_identical(nrow(cierre), 2L * 9L * 40L, info = s)
    expect_equal(cierre$casos, cierre$casos_nac, tolerance = 1e-12, info = s)
  }
  # la misma matriz de razones en las tres medidas: el cociente A / B de una celda es el mismo
  cociente <- function(s)
    rep$draws[[s]][sex_id == 1L & age_group_id == 15L, val[location_id == "A"] / val[location_id == "B"]]
  expect_equal(cociente("prevalence"), cociente("yld"), tolerance = 1e-12)
  expect_equal(cociente("prevalence"), cociente("incidence"), tolerance = 1e-12)
})

test_that("la semilla del reparto: el argumento, la de la configuración o la de la cascada", {
  m <- corrida_razon()
  piezas <- list(ajuste = m$cascada, avd = m$avd, insumos = m$insumos)
  expect_identical(dl_repartir_razon(piezas)$draws, m$reparto$draws)
  otra <- dl_repartir_razon(piezas, semilla = 20261002L)
  expect_identical(otra$semilla, 20261002L)
  expect_false(identical(otra$draws$prevalence$val, m$reparto$draws$prevalence$val))
  # la de la configuración (avanzado: cascada: razon_semilla) manda sobre la de la cascada
  b2 <- m$insumos
  b2$cfg$cascada$razon_semilla <- 20261002L
  expect_identical(dl_repartir_razon(list(ajuste = m$cascada, avd = m$avd, insumos = b2))$draws, otra$draws)
  expect_error(dl_repartir_razon(piezas, semilla = 1.5), "`semilla` debe ser un número entero")
})

test_that("dl_repartir_razon() dice qué le falta: el modo, la cascada, la tabla o una ubicación", {
  m <- corrida_razon()
  b <- m$insumos
  # un ajuste nacional en vez de la cascada
  expect_error(dl_repartir_razon(list(ajuste = m$ajuste, avd = dl_avd(m$ajuste, b, semilla = 5L), insumos = b)),
               "`ajuste` debe ser la cascada de dl_cascada\\(\\)")
  # otro modo subnacional
  mini <- corrida_mini()
  expect_error(dl_repartir_razon(list(ajuste = mini$cascada, avd = mini$avd_cascada, insumos = mini$insumos)),
               "el reparto por razón es del modo subnacional razon, y la configuración declara cascada.modo: proxy")
  # sin la tabla, o sin la razón de una ubicación (las tablas del contrato no entran en el hash de los insumos)
  sin <- b; sin$contrato$razones <- NULL
  expect_error(dl_repartir_razon(list(ajuste = m$cascada, avd = m$avd, insumos = sin)),
               "los insumos no traen la tabla razones")
  falta <- b; falta$contrato$razones <- b$contrato$razones[b$contrato$razones$ubicacion != "B"]
  expect_error(dl_repartir_razon(list(ajuste = m$cascada, avd = m$avd, insumos = falta)),
               paste0("no trae una razón de 2023 por cada ubicación subnacional de la población \\(faltan: B; ",
                      "sobran: ninguna"))
})

test_that("una prevalencia repartida mayor que 1 es un error que nombra la ubicación", {
  m <- corrida_razon()
  # toda la carga en la región C, la de menos población: su prevalencia sería la nacional por N_nac / N_C (> 4)
  b <- m$insumos
  rz <- data.table::copy(b$contrato$razones)
  rz[, razon := c(A = 0, B = 0, C = 1)[ubicacion]]
  b$contrato$razones <- rz
  e <- expect_error(dl_repartir_razon(list(ajuste = m$cascada, avd = m$avd, insumos = b)),
                    paste0("la prevalencia repartida pasa de 1 en [0-9]+ simulación\\(es\\) de la\\(s\\) ",
                           "ubicación\\(es\\) C \\(máximo [0-9.]+\\)"))
  # el conteo es de simulaciones, no de valores: con la región C casi sin población, su prevalencia pasa de 1 en
  # todas las celdas (2 sexos x 9 bandas) de las 40 simulaciones, y el mensaje dice 40
  b$poblacion <- data.table::copy(b$poblacion)[location_id == "C", val := val * 1e-4]
  expect_error(dl_repartir_razon(list(ajuste = m$cascada, avd = m$avd, insumos = b)),
               "la prevalencia repartida pasa de 1 en 40 simulación\\(es\\) de la\\(s\\) ubicación\\(es\\) C ")
})

test_that("dl_resumir() con la pieza reparto da las celdas subnacionales repartidas; sin ella, en modo razon, un error", {
  m <- corrida_razon()
  piezas <- list(ajuste = m$cascada, avd = m$avd, insumos = m$insumos)
  expect_error(dl_resumir(piezas), "falta la pieza `reparto`: con subnacional.modo: razon")
  res <- dl_resumir(c(piezas, list(reparto = m$reparto)))
  expect_s3_class(res, "dl_resumen")
  expect_identical(res$reparto$semilla, 5L)
  expect_identical(names(res$reparto), c("razones", "semilla", "ubicaciones", "anio"))
  expect_setequal(unique(res$celdas$location_id), c("999", "A", "B", "C"))
  # las celdas subnacionales, de las simulaciones repartidas: C en cero, A por encima de la nacional y B por debajo
  prev <- data.table::dcast(res$celdas[measure_id == 5L], sex_id + age_group_id ~ location_id, value.var = "val")
  expect_true(all(prev$C == 0))
  expect_true(all(prev$A > prev$`999` & prev$B < prev$`999`))
  ancho <- res$draws$prevalence
  expect_equal(unlist(ancho[location_id == "A" & sex_id == 1L & age_group_id == 15L, -(1:3)], use.names = FALSE),
               m$reparto$draws$prevalence[location_id == "A" & sex_id == 1L & age_group_id == 15L]$val, tolerance = 0)
  # el nivel nacional es el del ajuste: el mismo que da el resumen del ajuste nacional solo
  f <- m$ajuste
  nacional <- dl_resumir(list(ajuste = f, avd = dl_avd(f, m$insumos, semilla = 5L), insumos = m$insumos))
  expect_identical(res$celdas[location_id == "999"]$val, nacional$celdas$val)
  # el reparto de otras piezas, o en un proyecto de otro modo, no se admite
  otra <- dl_avd(m$cascada, m$insumos, semilla = 6L)
  expect_error(dl_resumir(list(ajuste = m$cascada, avd = otra, insumos = m$insumos, reparto = m$reparto)),
               "el `reparto` no se hizo con este `fit` y este `yld`")
  mini <- corrida_mini()
  expect_error(dl_resumir(list(ajuste = mini$cascada, avd = mini$avd_cascada, insumos = mini$insumos,
                               reparto = m$reparto)),
               "la pieza `reparto` solo va con subnacional.modo: razon")
  expect_error(dl_resumir(c(piezas, list(reparto = m$avd))), "`piezas\\$reparto` debe venir de dl_repartir_razon\\(\\)")
})

# ---- La corrida ----

# Cierre de las simulaciones guardadas de la corrida `run` (año `anio`) con la población congelada en inputs/: por
# medida, la diferencia relativa máxima entre los casos de las ubicaciones subnacionales y los nacionales.
cierre_corrida <- function(run, anio, nacional = "999") {
  pob <- data.table::fread(file.path(run$dir, "inputs", "poblacion.csv"), colClasses = list(character = "location_id"))
  vapply(c("prevalence", "incidence", "yld"), function(s) {
    x <- data.table::melt(.dl_leer_draws(run$dir, s, anio), id.vars = c("location_id", "sex_id", "age_group_id"))
    x <- merge(x, pob[, list(location_id, sex_id, age_group_id, N = val)],
               by = c("location_id", "sex_id", "age_group_id"))
    casos <- x[, list(sub = sum(value[location_id != nacional] * N[location_id != nacional]),
                      nac = value[location_id == nacional] * N[location_id == nacional]),
               by = list(sex_id, age_group_id, variable)]
    max(abs(casos$sub - casos$nac) / casos$nac)
  }, 0)
}

test_that("dl_exportar_corrida() escribe la corrida repartida: manifiesto, limitación y diagnostics/razon.csv", {
  m <- corrida_razon()
  piezas <- list(ajuste = m$cascada, avd = m$avd, insumos = m$insumos)
  res <- dl_resumir(c(piezas, list(reparto = m$reparto)))
  et <- dl_etiquetas(m$ajuste, m$ajuste_prior, m$insumos, grilla_rho = 0.5, semilla = 5L, cascada = m$cascada)
  v <- suppressMessages(dl_validar_ancla(m$ajuste, m$insumos, cascada = m$cascada))
  run <- dl_exportar_corrida(c(piezas, list(resumen = res)), nombre = "razon", carpeta = withr::local_tempdir(),
                             etiquetas = et, validacion = v, forzar = TRUE)
  expect_output(print(run), "\\(reparto subnacional por razón\\)")
  man <- run$manifest
  # el manifiesto: el modo, el bloque de la razón y su limitación (en lugar de la de la cascada plana)
  expect_identical(man$cascada$modo, "razon")
  expect_identical(man$cascada$departamentos, 3L)
  expect_identical(man$razon[c("semilla", "ubicaciones", "razon_min", "razon_max", "razones_cero")],
                   list(semilla = 5L, ubicaciones = 3L, razon_min = 0, razon_max = 1.4, razones_cero = 1L))
  expect_identical(unlist(man$razon$fuente), "indicador inventado")
  expect_length(man$razon$regla, 3L)
  lim <- unlist(man$limitaciones)
  expect_match(lim, "^reparto subnacional por razón declarado .* \\(tabla razones; fuente indicador inventado\\)",
               all = FALSE)
  expect_match(lim, "la misma razón en todas las edades y sexos \\(supuesto declarado\\)", all = FALSE)
  expect_false(any(grepl("cascada plana declarada|^sin cascada", lim)))
  escrito <- yaml::read_yaml(file.path(run$dir, "manifest.yaml"))
  expect_identical(escrito$cascada$modo, "razon")
  expect_identical(escrito$razon$semilla, 5L)
  # la tabla de la razón aplicada
  rz <- data.table::fread(file.path(run$dir, "diagnostics", "razon.csv"), colClasses = list(character = "location_id"))
  expect_identical(names(rz),
                   c("location_id", "razon", "error_log", "razon_media", "razon_lower", "razon_upper", "fuente"))
  expect_identical(rz$location_id, c("A", "B", "C"))
  expect_identical(rz$razon, c(1.4, 0.7, 0))
  # draws/ y cause/: las ubicaciones subnacionales son las repartidas (15 cifras en disco) y cierran en el nacional
  guardadas <- .dl_leer_draws(run$dir, "prevalence", 2023L)
  expect_equal(guardadas, .dl_draws_ancho(m$reparto$draws$prevalence), tolerance = 1e-13, ignore_attr = TRUE)
  expect_true(all(cierre_corrida(run, 2023L) < 1e-12))
  celdas <- data.table::fread(file.path(run$dir, "cause", "yld", paste0(run$run_id, ".csv")),
                              colClasses = list(character = "location_id"))
  media <- m$reparto$draws$yld[, list(val = 1e5 * mean(val)), by = list(location_id, sex_id, age_group_id)]
  x <- merge(celdas, media, by = c("location_id", "sex_id", "age_group_id"))
  expect_identical(nrow(x), nrow(celdas))
  expect_equal(x$val.x, x$val.y, tolerance = 1e-12)
  # la validación y las etiquetas son las del ajuste nacional; cada ubicación hereda la etiqueta nacional
  expect_true(file.exists(file.path(run$dir, "diagnostics", "validacion.csv")))
  etq <- data.table::fread(file.path(run$dir, "etiquetas", paste0(run$run_id, ".csv")),
                           colClasses = list(character = "location_id"))
  expect_setequal(etq$location_id, c("999", "A", "B", "C"))
  expect_identical(etq[location_id == "A"]$etiqueta, etq[location_id == "999"]$etiqueta)
  # la tabla razones queda congelada con el proyecto: inputs/contrato/ repite la corrida
  expect_true("razones" %in% unlist(lapply(man$inputs$contrato, `[[`, "tabla")))
  p <- dl_proyecto(file.path(run$dir, "inputs", "contrato"))
  expect_identical(p$configuracion$cascada$modo$valor, "razon")
  expect_identical(suppressMessages(dl_insumos(p))$hash, m$insumos$hash)
})

test_that("dl_correr() en modo razon escribe una sola corrida por año, ya repartida, con el nivel nacional del ajuste", {
  d <- proyecto_razon(anios = 2023:2024, anios_poblacion = 2023:2024)
  salida <- withr::local_tempdir()
  reg <- file.path(salida, "corridas.yaml")
  writeLines("datasets: []", reg)
  correr <- function(proyecto, ...) dl_correr(proyecto, semilla = 3, rapido = TRUE, sensibilidad = FALSE, ...)
  mensajes <- testthat::capture_messages(
    runs <- correr(d, carpeta_salida = salida, registro = reg, anios = 2023:2024))
  expect_match(mensajes, "reparto subnacional por razón \\(tabla razones\\)", all = FALSE)
  expect_s3_class(runs, "dl_corridas")
  expect_length(list.dirs(file.path(salida, "mod", "dismod_lite"), recursive = FALSE), 2L)   # una corrida por año
  for (a in c("2023", "2024")) {
    man <- runs[[a]]$manifest
    expect_identical(man$cascada$modo, "razon", info = a)
    expect_identical(man$razon$semilla, 3L, info = a)              # sin otra, la semilla de la corrida
    expect_true(file.exists(file.path(runs[[a]]$dir, "diagnostics", "razon.csv")), info = a)
    expect_true(all(cierre_corrida(runs[[a]], as.integer(a)) < 1e-12), info = a)
  }
  # 2024 se proyecta desde 2023: de 2024 son la población y las razones
  expect_match(unlist(runs[["2024"]]$manifest$limitaciones),
               "del 2024 son solo la población \\(poblacion\\) y las razones del reparto subnacional", all = FALSE)
  # el nivel nacional es el del ajuste: el mismo proyecto en modo plano, con la misma semilla, da las mismas
  # simulaciones nacionales; las subnacionales del plano son las nacionales y las del reparto no
  plano <- proyecto_razon()
  cfg <- readLines(file.path(plano, "config.yaml"))
  writeLines(sub("modo: razon", "modo: plano", cfg), file.path(plano, "config.yaml"))
  rp <- suppressWarnings(suppressMessages(correr(plano, carpeta_salida = withr::local_tempdir())))
  expect_identical(rp$manifest$cascada$modo, "plana")
  expect_null(rp$manifest$razon)
  for (s in c("prevalence", "incidence", "yld")) {
    a <- .dl_leer_draws(runs[["2023"]]$dir, s, 2023L); b <- .dl_leer_draws(rp$dir, s, 2023L)
    expect_identical(a[location_id == "999"], b[location_id == "999"], info = s)
    expect_false(isTRUE(all.equal(a[location_id == "A"], b[location_id == "A"])), info = s)
  }
  # la semilla del sorteo declarada en avanzado: el mismo nivel nacional, otro sorteo de las razones
  d2 <- proyecto_razon(config = c("avanzado:", "  cascada:", "    razon_semilla: 20261002"))
  r2 <- suppressMessages(correr(d2, carpeta_salida = withr::local_tempdir()))
  expect_identical(r2$manifest$razon$semilla, 20261002L)
  expect_identical(r2$manifest$params$seed, 3L)
  a <- .dl_leer_draws(runs[["2023"]]$dir, "prevalence", 2023L); b <- .dl_leer_draws(r2$dir, "prevalence", 2023L)
  expect_identical(a[location_id == "999"], b[location_id == "999"])
  expect_false(isTRUE(all.equal(a[location_id == "A"], b[location_id == "A"])))

  # lo que se hace con las corridas sigue funcionando: el re-resumen conserva el modo y el bloque de la razón
  p <- suppressMessages(dl_proyecto(d))
  nueva <- dl_reresumir_corrida(runs[["2023"]]$dir, carpeta = salida, rutas = p$rutas)
  expect_identical(nueva$manifest$cascada$modo, "razon")
  expect_identical(nueva$manifest$razon, runs[["2023"]]$manifest$razon)
  celdas_de <- function(run) data.table::fread(file.path(run$dir, "cause", "prevalence", paste0(run$run_id, ".csv")))
  viejas <- celdas_de(runs[["2023"]])
  nuevas <- celdas_de(nueva)
  expect_equal(nuevas$val, viejas$val, tolerance = 1e-12)
  # el consolidado toma las dos corridas y declara el modo de cada una
  cons <- suppressMessages(dl_consolidar(reg, salida, system.file("perfiles", "perfil_v2.yaml", package = "dismodlite"),
                                         file.path(p$rutas$registry, "master_gbd.csv"), rutas = p$rutas, causas = 7001))
  expect_identical(vapply(cons$manifest$bloques, `[[`, "", "cascada_modo"), c("razon", "razon"))
  prev <- data.table::fread(file.path(cons$dir, "tablas", "prevalencia.csv"), encoding = "UTF-8",
                            colClasses = c(ubigeo = "character"))
  expect_setequal(prev$ubigeo, c("999", "A", "B", "C"))
  expect_true(all(prev[ubigeo == "C"]$valor == 0))
})

test_that("en el formato completo el modo razon no tiene tabla: dl_correr() lo dice antes de ajustar", {
  raiz <- withr::local_tempdir()
  file.copy(ejemplo_completo(), raiz, recursive = TRUE)
  d <- file.path(raiz, basename(ejemplo_completo()))
  f <- file.path(d, "config", "9100.yaml")
  cfg <- readLines(f, encoding = "UTF-8")
  writeLines(enc2utf8(append(cfg, "  modo: {valor: razon, procedencia: prueba}", match("cascada:", cfg))), f,
             useBytes = TRUE)
  mensajes <- character()
  e <- withCallingHandlers(
    expect_error(dl_correr(d, 9100, semilla = 1, rapido = TRUE, carpeta_salida = withr::local_tempdir()),
                 "los insumos no traen la tabla razones: el reparto por razón es de un proyecto con las tablas"),
    message = function(m) { mensajes <<- c(mensajes, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("ajuste nacional", mensajes)))
})

test_that("una causa que es la suma de sus subtipos suma las corridas repartidas de un subtipo en modo razon", {
  d <- proyecto_razon()
  dir.create(file.path(d, "config"))
  file.rename(file.path(d, "config.yaml"), file.path(d, "config", "7001.yaml"))
  writeLines(enc2utf8(c("causa: 7000", "nombre: Suma inventada", "anio: 2023", "subtipos: [7001]",
                        "suma_de_subtipos: sí")), file.path(d, "config", "7000.yaml"), useBytes = TRUE)
  salida <- withr::local_tempdir()
  hija <- suppressMessages(dl_correr(d, 7001, semilla = 3, rapido = TRUE, sensibilidad = FALSE,
                                     carpeta_salida = salida))
  expect_identical(hija$manifest$cascada$modo, "razon")
  suma <- suppressMessages(dl_correr(d, 7000, rapido = TRUE, carpeta_salida = salida))
  expect_identical(suma$manifest$causa$agregacion, "suma_de_hijas")
  for (s in c("prevalence", "incidence", "yld"))
    expect_identical(.dl_leer_draws(suma$dir, s, 2023L), .dl_leer_draws(hija$dir, s, 2023L), info = s)
})

test_that("la sensibilidad de un proyecto en modo razon es la del ajuste nacional: kappa no aplica, como en plano", {
  m <- corrida_razon()
  s <- suppressMessages(dl_sensibilidad(m$insumos, grilla = list(lambda = 1, rho = 0.5, kappa = 1), semilla = 5L,
                                        opciones = m$opciones))
  expect_true(all(is.na(s$kappa)))
  expect_identical(unique(s$nota), "sin proxies subnacionales: la cascada no aplica")
  expect_identical(nrow(attr(s, "departamental")), 0L)
})
