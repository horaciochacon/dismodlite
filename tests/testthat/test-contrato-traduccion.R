tab <- function(tabla, d) dismodlite:::.dl_tabla_contrato(d, tabla, "prueba")
cfg_min <- function(agrupar = FALSE) list(cause_id = 1L, sexos = 1L, edad_inicio = 70, years = list(ajuste = 2023L),
                                          anchor = list(location_id = "P", agrupar_bandas_finas = agrupar),
                                          origen = list(formato = "simple"))
ubic <- tab("ubicaciones", data.frame(ubicacion = "P", nombre = "País", padre = NA))
pob80 <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75, 80),
                                     edad_fin = c(75, 80, NA), poblacion = c(300, 200, 100)))
ancla_fina <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75, 80, 85),
                                      edad_fin = c(75, 80, 85, NA), medida = "prevalencia",
                                      valor = c(0.1, 0.2, 0.3, 0.5), inferior = c(0.05, 0.1, 0.2, 0.4),
                                      superior = c(0.2, 0.3, 0.4, 0.6)))

test_that("los ids de banda: el de GBD si existe; si no, uno sintético en orden de edad", {
  expect_identical(dismodlite:::.dl_id_banda(c(80, 30, 32, 45), c(125, 35, 34, 60)),
                   c(21L, 11L, 990001L, 990002L))
})

test_that("ancla más fina que la población: se agrupa con poblacion_detalle", {
  det <- tab("poblacion_detalle", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(80, 85),
                                             edad_fin = c(85, NA), poblacion = c(75, 25)))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob80, ancla = ancla_fina,
                                               poblacion_detalle = det), cfg_min())
  a80 <- x$ancla[age_group_id == "21"]
  expect_identical(as.numeric(a80$val), sum(c(0.3, 0.5) * c(75, 25)) / sum(c(75, 25)))
  expect_identical(a80$metric_name, "Contrato")
})

test_that("ancla más fina sin poblacion_detalle: error que dice qué agregar", {
  expect_error(dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob80, ancla = ancla_fina),
                                                  cfg_min()), "poblacion_detalle")
})

test_that("con la población detallada, el ancla pasa tal cual (sin pesos_80mas)", {
  pob <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(70, 75, 80, 85), edad_fin = c(75, 80, 85, NA),
                                     poblacion = c(300, 200, 75, 25)))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = ancla_fina), cfg_min())
  expect_setequal(x$ancla$age_group_id, c("19", "20", "30", "990001"))
  expect_null(x$pesos_80)
})

test_that("un número pasa por la traducción y vuelve al mismo double", {
  v <- 0.1 + 0.2
  a <- ancla_fina; a$valor[1] <- v
  pob <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(70, 75, 80, 85), edad_fin = c(75, 80, 85, NA),
                                     poblacion = c(300, 200, 75, 25)))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = a), cfg_min())
  expect_identical(as.numeric(x$ancla[age_group_id == "19"]$val), v)
})

# ---- Más allá del brief: cada tabla ----

pob_det <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres",
                                       edad_inicio = c(70, 75, 80, 85), edad_fin = c(75, 80, 85, NA),
                                       poblacion = c(300, 200, 75, 25)))

test_that("una banda del ancla que cruza el límite de la población es un error", {
  # 75-84 es la unión de 75-79 y 80-84 (queda tal cual); 70-77 y 78-84 cruzan el límite de 75
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 78), edad_fin = c(78, 85),
                               medida = "prevalencia", valor = 0.1, inferior = 0.05, superior = 0.2))
  expect_error(dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob_det, ancla = a),
                                                  cfg_min()), "banda 70-77 a\u00f1os cruza el l\u00edmite")
})

test_that("las bandas de menos de un año de GBD se agrupan en [0, 1) por su ancho", {
  g <- dismodlite:::.dl_grupos_edad_referencia()
  finas <- g[age_group_id %in% c(2L, 3L, 388L, 389L)]
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(finas$age_start, 1),
                               edad_fin = c(finas$age_end, 5), medida = "prevalencia",
                               valor = c(0.1, 0.2, 0.3, 0.4, 0.5),
                               inferior = 0.01, superior = 0.9))
  pob <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres", edad_inicio = c(0, 1, 5),
                                     edad_fin = c(1, 5, NA), poblacion = c(10, 40, 50)))
  cfg <- cfg_min(); cfg$edad_inicio <- 0
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = a), cfg)
  w <- finas$age_end - finas$age_start
  expect_setequal(x$ancla$age_group_id, c("28", dismodlite:::.dl_id_banda(1, 5)))
  expect_identical(as.numeric(x$ancla[age_group_id == "28"]$val), sum(c(0.1, 0.2, 0.3, 0.4) * w) / sum(w))
})

test_that("con agrupar_bandas_finas el ancla no se agrupa, la población suma 80+ y salen los pesos", {
  pob <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(75, 80, 85, 90, 95), edad_fin = c(80, 85, 90, 95, NA),
                                     poblacion = c(200, 40, 30, 20, 10)))
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(75, 80, 85, 90, 95),
                               edad_fin = c(80, 85, 90, 95, NA), medida = "prevalencia", valor = 0.1,
                               inferior = 0.05, superior = 0.2))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = a), cfg_min(TRUE))
  expect_setequal(x$ancla$age_group_id, c("20", "30", "31", "32", "235"))
  expect_identical(x$poblacion$tabla[age_group_id == 21L]$val, "100")
  expect_identical(x$pesos_80$peso, c(40, 30, 20, 10) / 100)
})

test_that("la población: la nacional es la suma si falta, en el orden del formato completo", {
  pob <- tab("poblacion", data.frame(ubicacion = rep(c("A", "B"), each = 2), anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(70, 75), edad_fin = c(75, NA), poblacion = c(1.5, 2, 3, 4)))
  u <- tab("ubicaciones", data.frame(ubicacion = c("P", "A", "B"), nombre = c("País", "A", NA),
                                     padre = c(NA, "P", "P")))
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75), edad_fin = c(75, NA),
                               medida = "prevalencia", valor = 0.1, inferior = 0.05, superior = 0.2))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = u, poblacion = pob, ancla = a), cfg_min())
  p <- x$poblacion$tabla
  expect_identical(names(p), c("location_id", "location_level", "year", "sex_id", "age_group_id", "val",
                               "acquisition_id"))
  expect_identical(p$location_id, c("P", "P", "A", "A", "B", "B"))
  expect_identical(p$val[1:2], c("4.5", "6"))
  expect_identical(x$nombres_loc$location_name, c("País", "A", "B"))
})

test_that("covariables y betas: el GHDx nacional, la extracción y los proxies con su valor nacional", {
  u <- tab("ubicaciones", data.frame(ubicacion = c("P", "A", "B"), padre = c(NA, "P", "P")))
  pob <- tab("poblacion", data.frame(ubicacion = rep(c("A", "B"), each = 2), anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(70, 75), edad_fin = c(75, NA), poblacion = c(1, 2, 3, 4)))
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75), edad_fin = c(75, NA),
                               medida = "prevalencia", valor = 0.1, inferior = 0.05, superior = 0.2))
  cov <- tab("covariables", data.frame(ubicacion = c("P", "P", "A", "B"), anio = 2023L,
                                       covariable = c("sdi", "otra", "sdi", "sdi"), valor = c(0.5, 7, 0.4, 0.6),
                                       error_estandar = c(NA, NA, 0.01, 0.02), covariable_id = c(881L, NA, NA, NA)))
  b <- tab("betas", data.frame(covariable = c("otra", "sdi"), efecto_sobre = c("incidencia", "prevalencia"),
                               transformacion = "log", beta = c(0.1 + 0.2, -1), inferior = c(NA, -2),
                               superior = c(NA, 0)))
  tablas <- list(ubicaciones = u, poblacion = pob, ancla = a, covariables = cov, betas = b)
  ids <- dismodlite:::.dl_ids_covariable(tablas)
  expect_identical(ids, list(otra = 1L, sdi = 881L))
  cfg <- cfg_min()
  cfg$covariables <- list(list(covariate_name_short = "sdi", proxy = list(covariate_id_proxy = 900102L)))
  x <- dismodlite:::.dl_traducir_contrato(tablas, cfg)
  expect_identical(x$ids_covariable, ids)
  expect_identical(x$cov$nacional$covariate_id, c(881L, 1L))
  expect_identical(x$cov$nacional$age_group_id, c(22L, 22L))
  expect_identical(x$cov$nacional$sex_id, c(3L, 3L))
  e <- x$extraccion$covariables_gbd
  expect_identical(vapply(e, `[[`, "", "covariate_name_short"), c("otra", "sdi"))
  expect_identical(as.numeric(e[[1]]$beta_valor), 0.1 + 0.2)
  expect_null(e[[1]]$beta_inferior)
  expect_identical(e[[2]]$beta_superior, "0")
  expect_identical(e[[1]]$parametro, "Incidence")
  pr <- x$proxies
  expect_identical(pr$location_id, c("A", "B"))
  expect_identical(pr$covariate_id_gbd, c(881L, 881L))
  expect_identical(pr$ancla_ghdx, c("0.5", "0.5"))
  expect_identical(pr$valor_calibrado_se, c("0.01", "0.02"))
})

test_that("datos: medida -> tipo_dato y ubicacion -> location_id", {
  d <- tab("datos", data.frame(ubicacion = "P", anio = 2020L, sexo = "hombres", edad_inicio = 70, edad_fin = 75,
                               medida = c("prevalencia", "prevalencia_registro"), valor = c(0.1, 0.2),
                               error_estandar = 0.01))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob_det, ancla = ancla_fina,
                                               datos = d), cfg_min())
  expect_identical(x$datos$tipo_dato, c("prev_estudio", "prev_admin"))
  expect_identical(x$datos$location_name, c("País", "País"))
  expect_identical(x$datos$val, c("0.1", "0.2"))
})

test_that("severidad: los pesos de GBD; por edad o sexo, todavía no", {
  s <- tab("severidad", data.frame(estado = c("propio", "otro"), proporcion = c(0.4, 0.6), inferior = 0.1,
                                   superior = 0.9, peso_discapacidad = c(0.1, 0.2), peso_inferior = 0.05,
                                   peso_superior = 0.3))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob_det, ancla = ancla_fina,
                                               severidad = s), cfg_min())
  expect_identical(x$severidad$health_state_id, c("1", "2"))
  expect_identical(x$severidad$dw_mean, c("0.1", "0.2"))
  s2 <- tab("severidad", data.frame(sexo = c("hombres", "mujeres"), estado = "propio", proporcion = 1, inferior = 1,
                                    superior = 1, peso_discapacidad = 0.1, peso_inferior = 0.05, peso_superior = 0.3))
  expect_error(dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob_det, ancla = ancla_fina,
                                                       severidad = s2), cfg_min()), "todavía no")
})

test_that("fuentes_gbd -> list.csv del formato completo", {
  f <- tab("fuentes_gbd", data.frame(componente = c("no_fatal", "causa_de_muerte"), nid = c(10L, 20L)))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob_det, ancla = ancla_fina,
                                               fuentes_gbd = f), cfg_min())
  expect_identical(x$fuentes$component_id, c(5L, 4L))
  expect_identical(x$fuentes$location_id, c("P", "P"))
  expect_identical(x$fuentes$cause_id, c(1L, 1L))
})

# ---- Ronda de revisión 1 ----

test_that("una banda del ancla que es la unión de varias de la población queda tal cual", {
  pob <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(70, 75, 80, 85, 90, 95), edad_fin = c(75, 80, 85, 90, 95, NA),
                                     poblacion = c(300, 200, 40, 30, 20, 10)))
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 80), edad_fin = c(80, NA),
                               medida = "prevalencia", valor = c(0.1, 0.2), inferior = 0.05, superior = 0.3))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = a), cfg_min())
  expect_setequal(x$ancla$age_group_id, c(as.character(x$bandas[edad_inicio == 70 & edad_fin == 80]$age_group_id),
                                          "21"))
  expect_identical(as.numeric(x$ancla[age_group_id == "21"]$val), 0.2)
  # [0, 1) del ancla con la población en las bandas de menos de un año de GBD
  g <- dismodlite:::.dl_grupos_edad_referencia()[age_group_id %in% c(2L, 3L, 388L, 389L)]
  pob1 <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres",
                                      edad_inicio = c(g$age_start, 1), edad_fin = c(g$age_end, NA),
                                      poblacion = c(1, 2, 3, 4, 90)))
  a1 <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(0, 1), edad_fin = c(1, NA),
                                medida = "prevalencia", valor = 0.1, inferior = 0.05, superior = 0.3))
  cfg <- cfg_min(); cfg$edad_inicio <- 0
  x1 <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob1, ancla = a1), cfg)
  expect_true("28" %in% x1$ancla$age_group_id)
})

test_that("una banda del ancla fuera de la población es un error, salvo por debajo de edad_inicio", {
  pob <- tab("poblacion", data.frame(ubicacion = "P", anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75),
                                     edad_fin = c(75, 80), poblacion = c(300, 200)))
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75, 80), edad_fin = c(75, 80, NA),
                               medida = "prevalencia", valor = 0.1, inferior = 0.05, superior = 0.3))
  expect_error(dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = a), cfg_min()),
               "no cubre la banda 80 años y más")
  # 30-34 y las de menos de un año, enteramente por debajo de edad_inicio (70): pasan tal cual, sin comprobaciones
  b <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(0.5, 30, 70, 75),
                               edad_fin = c(1, 35, 75, 80), medida = "prevalencia", valor = 0.1, inferior = 0.05,
                               superior = 0.3))
  x <- dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob, ancla = b), cfg_min())
  expect_setequal(x$ancla$age_group_id, c("389", "11", "19", "20"))
})

test_that("poblacion_detalle sin una de las bandas del ancla: error que la nombra", {
  det <- tab("poblacion_detalle", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = 80, edad_fin = 85,
                                             poblacion = 75))
  expect_error(dismodlite:::.dl_traducir_contrato(list(ubicaciones = ubic, poblacion = pob80, ancla = ancla_fina,
                                                       poblacion_detalle = det), cfg_min()),
               "poblacion_detalle no trae.*hombres 85 años y más")
})

test_that("valor_nacional_de: el proxy se ancla en el valor nacional de la sustituta", {
  u <- tab("ubicaciones", data.frame(ubicacion = c("P", "A", "B"), padre = c(NA, "P", "P")))
  pob <- tab("poblacion", data.frame(ubicacion = rep(c("A", "B"), each = 2), anio = 2023L, sexo = "hombres",
                                     edad_inicio = c(70, 75), edad_fin = c(75, NA), poblacion = c(1, 2, 3, 4)))
  a <- tab("ancla", data.frame(anio = 2023L, sexo = "hombres", edad_inicio = c(70, 75), edad_fin = c(75, NA),
                               medida = "prevalencia", valor = 0.1, inferior = 0.05, superior = 0.2))
  cov <- tab("covariables", data.frame(ubicacion = c("P", "P", "A", "B"), anio = 2023L,
                                       covariable = c("sdi", "sdi_std", "sdi", "sdi"), valor = c(0.5, 0.45, 0.4, 0.6),
                                       error_estandar = c(NA, NA, 0.01, 0.02), covariable_id = c(881L, 882L, NA, NA)))
  b <- tab("betas", data.frame(covariable = "sdi", efecto_sobre = "prevalencia", transformacion = "log", beta = -1,
                               valor_nacional_de = "sdi_std"))
  tablas <- list(ubicaciones = u, poblacion = pob, ancla = a, covariables = cov, betas = b)
  expect_identical(dismodlite:::.dl_ids_covariable(tablas), list(sdi = 881L, sdi_std = 882L))
  cfg <- cfg_min()
  cfg$covariables <- list(list(covariate_name_short = "sdi", proxy = list(covariate_id_proxy = 900101L),
                               sustituye = list(covariate_id = 882L, covariate_name_short = "sdi_std")))
  x <- dismodlite:::.dl_traducir_contrato(tablas, cfg)
  expect_identical(x$proxies$covariate_id_gbd, c(882L, 882L))
  expect_identical(x$proxies$ancla_ghdx, c("0.45", "0.45"))
  expect_setequal(x$cov$nacional$covariate_name_short, c("sdi", "sdi_std"))
  # la beta no necesita la fila nacional de su propia covariable: sin ella, los mismos proxies
  cambiar <- function(t, ...) { t[...names()] <- list(...); t }
  sin_copia <- cambiar(tablas, covariables = cov[!(ubicacion == "P" & covariable == "sdi")])
  expect_identical(dismodlite:::.dl_ids_covariable(sin_copia), list(sdi = 1L, sdi_std = 882L))
  y <- dismodlite:::.dl_traducir_contrato(sin_copia, cfg)
  expect_identical(y$proxies, x$proxies)
  expect_identical(y$cov$nacional, x$cov$nacional[covariate_name_short == "sdi_std"])
  # sí la de la sustituta, y la de una beta sin valor_nacional_de
  sin_std <- cambiar(tablas, covariables = cov[covariable != "sdi_std"])
  expect_error(dismodlite:::.dl_traducir_contrato(sin_std, cfg),
               "la tabla covariables no trae el valor nacional \\(ubicación P\\) de sdi_std, que usa la tabla betas")
  b2 <- tab("betas", data.frame(covariable = "sdi", efecto_sobre = "prevalencia", transformacion = "log", beta = -1))
  expect_error(dismodlite:::.dl_traducir_contrato(cambiar(sin_copia, betas = b2), cfg_min()),
               "no trae el valor nacional \\(ubicación P\\) de sdi, que usa la tabla betas")
  cfg$covariables[[1]]$sustituye$covariate_name_short <- "otra"
  expect_error(dismodlite:::.dl_traducir_contrato(tablas, cfg), "sale de otra, que no está en la tabla betas")
})
