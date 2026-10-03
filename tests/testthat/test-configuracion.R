# Configs del ejemplo acs_peru; config_mut() escribe una copia de 9100.yaml transformada por `mutar` y devuelve su
# carpeta.
config_dir_ejemplo <- function() file.path(ruta_acs(), "config")
config_mut <- function(mutar) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  y <- yaml::read_yaml(file.path(config_dir_ejemplo(), "9100.yaml"))
  y <- mutar(y)
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  d
}

test_that("dl_configuracion carga y valida 9100", {
  cfg <- dl_configuracion(9100L, carpeta_config = config_dir_ejemplo())
  expect_s3_class(cfg, "dl_config")
  expect_identical(cfg$cause_id, 9100L)
  expect_identical(cfg$anchor$medidas, "prevalence")
  expect_length(cfg$transformaciones, 3L)
})
test_that("dl_configuracion lee el YAML en UTF-8 también con un locale C (contenedores), sin convertirlo", {
  # yaml::read_yaml() convierte el texto a la codificación de la sesión: con un locale C el YAML llegaría truncado
  esperado <- dl_configuracion(9100L, carpeta_config = config_dir_ejemplo())
  en_c <- withr::with_locale(c(LC_CTYPE = "C"), dl_configuracion(9100L, carpeta_config = config_dir_ejemplo()))
  expect_identical(en_c, esperado)
  expect_true(any(grepl("\u00e9", unlist(en_c), fixed = TRUE)))   # el texto con tildes llega entero
})
test_that("errores con ruta.al.campo", {
  d <- config_mut(function(y) { y$anchor$lambda <- 3; y })
  expect_error(dl_configuracion(9100L, d), "config[.]anchor[.]lambda")
})
test_that("anchor region está reservado", {
  d <- config_mut(function(y) { y$anchor$location <- "region"; y })
  expect_error(dl_configuracion(9100L, d), "config[.]anchor[.]location: .*reservado.*120")
})
test_that("transformacion desconocida y cause_id inconsistente", {
  d <- config_mut(function(y) { y$transformaciones[[1]]$transformacion <- "cubica"; y })
  expect_error(dl_configuracion(9100L, d), "config[.]transformaciones")
  d2 <- config_mut(function(y) { y$cause_id <- 999; y })
  expect_error(dl_configuracion(9100L, d2), "config[.]cause_id")
})
test_that("config sin phi_sinadef, edad_inicio 30 con procedencia, kappa y medidas_entrada validados", {
  cfg <- dl_configuracion_ejemplo(9100L, formato = "completo")
  expect_null(cfg$phi_sinadef)
  expect_identical(as.integer(cfg$edad_inicio), 30L)
  expect_match(cfg$edad_inicio_fuente, "datos sint")   # la procedencia declarada en la configuración del ejemplo
  expect_identical(as.integer(cfg$edad_fin %||% 99L), 99L)
  expect_identical(as.integer(cfg$nsub %||% 5L), 5L)
  d <- config_mut(function(y) {
    y$phi_sinadef <- list(modo = "factor"); y$cascada$kappa <- 1.5; y$medidas_entrada <- list("sinadef"); y })
  err <- tryCatch(dl_configuracion(9100L, d), error = function(e) conditionMessage(e))
  expect_match(err, "phi_sinadef"); expect_match(err, "cascada.kappa"); expect_match(err, "medidas_entrada")
  d2 <- config_mut(function(y) { y$edad_inicio_fuente <- NULL; y })
  expect_error(dl_configuracion(9100L, d2), "edad_inicio_fuente")
})

test_that("anchor.modelo_variante: lista de etiquetas no vacias (token sin_variante), o ausente", {
  d <- withr::local_tempdir()
  y <- yaml::read_yaml(file.path(config_dir_ejemplo(), "9100.yaml"))
  y$anchor$modelo_variante <- list("sin_variante", "Endemic model")
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_identical(dl_configuracion(9100L, d)$anchor$modelo_variante, c("sin_variante", "Endemic model"))
  y$anchor$modelo_variante <- list("step 4", "")
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_error(dl_configuracion(9100L, d), "anchor.modelo_variante")
  y$anchor$modelo_variante <- 4
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_error(dl_configuracion(9100L, d), "anchor.modelo_variante")
})

# Escala de HAQI: la beta publicada es «por unidad» de una covariable cuya escala no se publica (0-100, la nativa
# del GHDx, o 0-1). transformaciones[].escala multiplica la diferencia de covariable antes de la beta: 1 = 0-100,
# 0.01 = 0-1. Obligatoria (con procedencia) para haqi lineal.
test_that("transformaciones.escala: obligatoria con procedencia para haqi lineal, numerica > 0", {
  d <- config_mut(function(y) { y$transformaciones[[3]]$escala <- NULL; y })
  expect_error(dl_configuracion(9100L, d), "transformaciones\\[3\\][.]escala")
  d <- config_mut(function(y) { y$transformaciones[[3]]$escala <- -1; y })
  expect_error(dl_configuracion(9100L, d), "transformaciones\\[3\\][.]escala")
  d <- config_mut(function(y) { y$transformaciones[[3]]$escala_procedencia <- NULL; y })
  expect_error(dl_configuracion(9100L, d), "transformaciones\\[3\\][.]escala_procedencia")
  cfg <- dl_configuracion(9100L, config_dir_ejemplo())
  expect_identical(cfg$transformaciones[[3]]$covariate_name_short, "haqi")
  expect_identical(cfg$transformaciones[[3]]$escala, 1)
})

# Fracción aguda del csmr: emr_prior.fraccion_aguda = {valor en [0, 1), procedencia}. Opcional.
test_that("emr_prior.fraccion_aguda: valor en [0,1) con procedencia; sin el campo, sin descuento", {
  d <- config_mut(function(y) { y$emr_prior$fraccion_aguda <- list(valor = 1.2, procedencia = "x"); y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]fraccion_aguda")
  d <- config_mut(function(y) { y$emr_prior$fraccion_aguda <- list(valor = 0.3); y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]fraccion_aguda[.]procedencia")
  d <- config_mut(function(y) { y$emr_prior$fraccion_aguda <- list(valor = 0.3, procedencia = "ok"); y })
  cfg <- dl_configuracion(9100L, d)
  expect_equal(cfg$emr_prior$fraccion_aguda$valor, 0.3)
  expect_null(dl_configuracion(9100L, config_dir_ejemplo())$emr_prior$fraccion_aguda)
})

test_that("emr_prior.fraccion_aguda.cfr_30d: opcional, en (0, 1)", {
  d <- config_mut(function(y) { y$emr_prior$fraccion_aguda <- list(valor = 0.3, procedencia = "ok", cfr_30d = 1.5); y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]fraccion_aguda[.]cfr_30d")
  d <- config_mut(function(y) { y$emr_prior$fraccion_aguda <- list(valor = 0.3, procedencia = "ok", cfr_30d = 0.4); y })
  expect_equal(dl_configuracion(9100L, d)$emr_prior$fraccion_aguda$cfr_30d, 0.4)
})

# Remisión por tramo de edad: remision.por_edad = lista de tramos [edad_inicio, edad_fin) con valor y fuente; fuera
# de los tramos rige remision.valor. Tramos sin solape.
test_that("remision.por_edad: tramos [edad_inicio, edad_fin) con valor >= 0 y fuente, sin solape", {
  ok <- config_mut(function(y) { y$remision$por_edad <- list(
    list(edad_inicio = 0, edad_fin = 20, valor = 4, fuente = "fuente de prueba p. 105: remision 3-5/anio")); y })
  cfg <- dl_configuracion(9100L, ok)
  expect_equal(cfg$remision$por_edad[[1]]$valor, 4)
  d <- config_mut(function(y) { y$remision$por_edad <- list(list(edad_inicio = 0, edad_fin = 20, valor = 4)); y })
  expect_error(dl_configuracion(9100L, d), "remision[.]por_edad\\[1\\][.]fuente")
  d <- config_mut(function(y) { y$remision$por_edad <- list(list(edad_inicio = 20, edad_fin = 10, valor = 4,
                                                                 fuente = "x")); y })
  expect_error(dl_configuracion(9100L, d), "remision[.]por_edad\\[1\\]")
  d <- config_mut(function(y) { y$remision$por_edad <- list(
    list(edad_inicio = 0, edad_fin = 20, valor = 4, fuente = "x"), list(edad_inicio = 15, edad_fin = 30, valor = 1, fuente = "x")); y })
  expect_error(dl_configuracion(9100L, d), "solap")
})

# Prior de EMR más laxo: emr_prior.factor_sd multiplica la sd en escala log del prior de csmr/prevalencia.
test_that("emr_prior.factor_sd: valor >= 1 con procedencia", {
  ok <- config_mut(function(y) { y$emr_prior$factor_sd <- list(valor = 3, procedencia = "triage de prueba"); y })
  expect_equal(dl_configuracion(9100L, ok)$emr_prior$factor_sd$valor, 3)
  d <- config_mut(function(y) { y$emr_prior$factor_sd <- list(valor = 0.5, procedencia = "x"); y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]factor_sd")
  d <- config_mut(function(y) { y$emr_prior$factor_sd <- list(valor = 2); y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]factor_sd[.]procedencia")
})

# Las claves cortas del error del ancla, la fase aguda y el subtipo: su destino es una clave del formato completo y
# salen en la tabla de ?dl_configuracion; las que siguen solo en `avanzado` se aceptan tal cual.
test_that("las claves cortas ancla.error_maximo, fraccion_aguda y subtipo_de tienen destino y están en la ayuda", {
  t <- .dl_claves_simple()
  nuevas <- c("ancla.error_maximo", "mortalidad_exceso.fraccion_aguda", "sensibilidad.fraccion_aguda", "subtipo_de")
  expect_true(all(nuevas %in% t$clave))
  expect_true(all(t$destino[match(nuevas, t$clave)] %in% .DL_CLAVES_CONFIG))
  rd <- paste(.dl_rd_config_simple(), collapse = "\n")
  for (k in nuevas) expect_match(rd, sprintf("\\code{%s}", k), fixed = TRUE, info = k)
  # las de una suma de subtipos: en la ayuda, y los omitidos con su destino en el formato completo (suma.omitidas)
  suma <- c("suma_de_subtipos", "subtipos_omitidos[].causa", "subtipos_omitidos[].motivo")
  expect_true(all(suma %in% t$clave))
  for (k in suma) expect_match(rd, sprintf("\\code{%s}", k), fixed = TRUE, info = k)
  expect_identical(t$destino[match(suma, t$clave)], c("", "suma.omitidas[].cause_id", "suma.omitidas[].motivo"))
  expect_true("suma.omitidas" %in% .DL_CLAVES_CONFIG)
  base <- list(causa = 501L, anio = 2020L, edad_inicio = 40L)
  ctx <- list(ubicacion = "999", nombre = "x", subnacional = FALSE)
  cfg <- .dl_config_simple(c(base, list(avanzado = list(
    emr_prior = list(factor_techo = 4, fraccion_aguda = list(valor = 0.3, procedencia = "x", cfr_30d = 0.1)),
    nsub = 10))), "config.yaml", 501L, ctx)
  expect_identical(cfg$emr_prior$factor_techo, 4)
  expect_identical(cfg$emr_prior$fraccion_aguda$cfr_30d, 0.1)
  expect_identical(cfg$nsub, 10)
})

# Umbral de anchor_identity (error relativo mediano; 0.05 por defecto). Relajarlo exige procedencia.
test_that("anchor.gate_err_mediano: en (0, 1) con procedencia; sin el campo, 0.05", {
  expect_equal(dl_configuracion(9100L, carpeta_config = config_dir_ejemplo())$anchor$gate_err_mediano$valor, 0.05)
  ok <- config_mut(function(y) { y$anchor$gate_err_mediano <- list(valor = 0.1,
                                                                   procedencia = "decisión de prueba"); y })
  expect_equal(dl_configuracion(9100L, ok)$anchor$gate_err_mediano$valor, 0.1)
  d <- config_mut(function(y) { y$anchor$gate_err_mediano <- list(valor = 0.1); y })
  expect_error(dl_configuracion(9100L, d), "anchor[.]gate_err_mediano[.]procedencia")
  d <- config_mut(function(y) { y$anchor$gate_err_mediano <- list(valor = 2, procedencia = "x"); y })
  expect_error(dl_configuracion(9100L, d), "anchor[.]gate_err_mediano")
})

# Componente de la causa: anchor.componente = {sequela_ids, motivo}; la prevalencia del ancla y el AVD de referencia
# se escalan a las secuelas del componente (fracciones de la partición de severidad).
test_that("anchor.componente: lista de sequela_ids enteros no vacia con motivo", {
  ok <- config_mut(function(y) { y$anchor$componente <- list(sequela_ids = list(1L),
                                                             motivo = "solo la pieza aguda"); y })
  expect_identical(dl_configuracion(9100L, ok)$anchor$componente$sequela_ids, 1L)
  d <- config_mut(function(y) { y$anchor$componente <- list(sequela_ids = list(1L)); y })
  expect_error(dl_configuracion(9100L, d), "anchor[.]componente[.]motivo")
  d <- config_mut(function(y) { y$anchor$componente <- list(sequela_ids = list(), motivo = "x"); y })
  expect_error(dl_configuracion(9100L, d), "anchor[.]componente[.]sequela_ids")
  d <- config_mut(function(y) { y$anchor$componente <- list(sequela_ids = list("a"), motivo = "x"); y })
  expect_error(dl_configuracion(9100L, d), "anchor[.]componente[.]sequela_ids")
})

# csmr departamental de validación (held-out) de otro año: el registro de defunciones puede tener completitud
# publicada solo hasta un año anterior al de ajuste. cascada.heldout_anio declara ese año; sin el campo, el año de
# ajuste.
test_that("cascada.heldout_anio: entero con procedencia; sin el campo, el año de ajuste", {
  cfg <- cfg9100()
  expect_identical(.dl_anio_heldout(cfg), 2019L)                  # el ejemplo declara la validación de 2019
  cfg$cascada$heldout_anio <- NULL
  expect_identical(.dl_anio_heldout(cfg), 2023L)
  d <- config_mut(function(y) {
    y$cascada$heldout_anio <- list(valor = 2019, procedencia = "SINADEF: completitud publicada solo 2017-2019"); y })
  cfg2 <- dl_configuracion(9100L, d)
  expect_identical(.dl_anio_heldout(cfg2), 2019L)
  d <- config_mut(function(y) { y$cascada$heldout_anio <- list(valor = 2019); y })
  expect_error(dl_configuracion(9100L, d), "config[.]cascada[.]heldout_anio[.]procedencia")
  d <- config_mut(function(y) { y$cascada$heldout_anio <- list(valor = "2019", procedencia = "x"); y })
  expect_error(dl_configuracion(9100L, d), "config[.]cascada[.]heldout_anio[.]valor")
  d <- config_mut(function(y) { y$cascada$heldout_anio <- 2019; y })
  expect_error(dl_configuracion(9100L, d), "config[.]cascada[.]heldout_anio")
})

# Prior de EMR plano dentro del techo: emr_prior.tipo plano_cota, sin prior informativo. Exige procedencia; con cota
# null el techo se deriva de csmr/prevalencia.
test_that("emr_prior.tipo: informativo_edad o plano_cota; plano_cota exige procedencia", {
  ok <- config_mut(function(y) { y$emr_prior$tipo <- "plano_cota"; y$emr_prior$tipo_procedencia <- "test"; y })
  expect_identical(dl_configuracion(9100L, ok)$emr_prior$tipo, "plano_cota")
  d <- config_mut(function(y) {
    y$emr_prior$tipo <- "plano_cota"; y$emr_prior$tipo_procedencia <- "test"; y$emr_prior$cota <- NULL; y })
  expect_null(dl_configuracion(9100L, d)$emr_prior$cota)      # cota null: techo derivado de csmr/prev en dl_insumos
  d <- config_mut(function(y) { y$emr_prior$tipo <- "plano_cota"; y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]tipo_procedencia")
  d <- config_mut(function(y) { y$emr_prior$tipo <- "otro"; y })
  expect_error(dl_configuracion(9100L, d), "emr_prior[.]tipo")
})

test_that("cascada.dx_fuera_de_banda: cero por defecto, vecina exige procedencia, otro valor se rechaza", {
  expect_equal(cfg9100()$cascada$dx_fuera_de_banda$valor, "cero")
  d <- config_mut(function(y) { y$cascada$dx_fuera_de_banda <- "medio"; y })
  expect_error(dl_configuracion(9100L, d), "dx_fuera_de_banda")
  d <- config_mut(function(y) { y$cascada$dx_fuera_de_banda <- "vecina"; y })
  expect_error(dl_configuracion(9100L, d), "procedencia")
  d <- config_mut(function(y) { y$cascada$dx_fuera_de_banda <- list(valor = "vecina", procedencia = "test"); y })
  expect_equal(dl_configuracion(9100L, d)$cascada$dx_fuera_de_banda$valor, "vecina")
  d <- config_mut(function(y) { y$cascada$dx_fuera_de_banda <- "cero"; y })
  expect_equal(dl_configuracion(9100L, d)$cascada$dx_fuera_de_banda$valor, "cero")
})

test_that("cascada.dx_interpolacion: lineal por defecto, escalon exige procedencia, otro valor se rechaza", {
  expect_equal(cfg9100()$cascada$dx_interpolacion$valor, "lineal")
  d <- config_mut(function(y) { y$cascada$dx_interpolacion <- "spline"; y })
  expect_error(dl_configuracion(9100L, d), "dx_interpolacion")
  d <- config_mut(function(y) { y$cascada$dx_interpolacion <- "escalon"; y })
  expect_error(dl_configuracion(9100L, d), "procedencia")
  d <- config_mut(function(y) { y$cascada$dx_interpolacion <- list(valor = "escalon", procedencia = "test"); y })
  expect_equal(dl_configuracion(9100L, d)$cascada$dx_interpolacion$valor, "escalon")
})

test_that("cascada.modo: proxy por defecto, plana exige procedencia, otro valor se rechaza", {
  expect_equal(cfg9100()$cascada$modo$valor, "proxy")
  d <- config_mut(function(y) { y$cascada$modo <- "ecologica"; y })
  expect_error(dl_configuracion(9100L, d), "cascada.modo")
  d <- config_mut(function(y) { y$cascada$modo <- "plana"; y })
  expect_error(dl_configuracion(9100L, d), "procedencia")
  d <- config_mut(function(y) { y$cascada$modo <- list(valor = "plana", procedencia = "test"); y })
  expect_equal(dl_configuracion(9100L, d)$cascada$modo$valor, "plana")
})

test_that("suma.omitidas: cause_id entero y motivo obligatorio", {
  d <- config_mut(function(y) { y$suma <- list(omitidas = list(list(cause_id = 970L, motivo = "test"))); y })
  expect_equal(dl_configuracion(9100L, d)$suma$omitidas[[1]]$cause_id, 970L)
  d <- config_mut(function(y) { y$suma <- list(omitidas = list(list(cause_id = 970L))); y })
  expect_error(dl_configuracion(9100L, d), "omitidas.*motivo")
  d <- config_mut(function(y) { y$suma <- list(omitidas = list(list(cause_id = "x", motivo = "t"))); y })
  expect_error(dl_configuracion(9100L, d), "omitidas.*cause_id")
})

# Sustitución declarada del valor nacional de referencia: la beta puede ser de una covariable por edad sin valor
# nacional único, y el proxy se ancla en la covariable hermana estandarizada por edad.
# covariables[].sustituye = {covariate_id, covariate_name_short, procedencia}, solo con proxy.
test_that("covariables[].sustituye: exige proxy, covariate_id entero, covariate_name_short y procedencia", {
  ok <- list(covariate_id = 57L, covariate_name_short = "LDI_pc", procedencia = "test")
  d <- config_mut(function(y) { y$covariables[[1]]$sustituye <- ok; y })
  expect_equal(dl_configuracion(9100L, d)$covariables[[1]]$sustituye$covariate_id, 57L)
  # haqi sin proxy (en la configuración del ejemplo lleva 900103)
  d <- config_mut(function(y) { y$covariables[[3]]$proxy <- NULL; y$covariables[[3]]$sustituye <- ok; y })
  expect_error(dl_configuracion(9100L, d), "sustituye.*proxy")
  d <- config_mut(function(y) {
    y$covariables[[1]]$sustituye <- list(covariate_id = "x", covariate_name_short = "LDI_pc", procedencia = "t"); y })
  expect_error(dl_configuracion(9100L, d), "sustituye.covariate_id")
  d <- config_mut(function(y) { y$covariables[[1]]$sustituye <- list(covariate_id = 57L, procedencia = "t"); y })
  expect_error(dl_configuracion(9100L, d), "sustituye.covariate_name_short")
  d <- config_mut(function(y) { y$covariables[[1]]$sustituye <- list(covariate_id = 57L,
                                                                     covariate_name_short = "LDI_pc"); y })
  expect_error(dl_configuracion(9100L, d), "sustituye.procedencia")
})

# Año de ajuste sin estimaciones GBD: years.ancla nombra el año de las estimaciones que prestan el ancla, el csmr y
# las covariables GHDx; years.ajuste sigue siendo el año estimado (población, datos). Sin el campo, el de ajuste.
test_that("years.ancla: entero <= ajuste, a lo sumo un año atras, con procedencia; sin el campo, el año de ajuste", {
  cfg <- cfg9100()
  expect_identical(.dl_anio_ancla(cfg), 2023L)
  d <- config_mut(function(y) { y$years$ajuste <- list(2024L); y$years$ancla <- list(valor = 2023L,
      procedencia = "sin GBD 2024: ancla 2023 reetiquetada"); y })
  cfg2 <- dl_configuracion(9100L, d)
  expect_identical(.dl_anio_ancla(cfg2), 2023L)
  expect_identical(as.integer(unlist(cfg2$years$ajuste)), 2024L)
  d <- config_mut(function(y) { y$years$ajuste <- list(2024L); y$years$ancla <- list(valor = 2023L); y })
  expect_error(dl_configuracion(9100L, d), "config[.]years[.]ancla[.]procedencia")
  d <- config_mut(function(y) { y$years$ajuste <- list(2023L); y$years$ancla <- list(valor = 2024L,
      procedencia = "x"); y })
  expect_error(dl_configuracion(9100L, d), "config[.]years[.]ancla[.]valor")
  d <- config_mut(function(y) { y$years$ajuste <- list(2025L); y$years$ancla <- list(valor = 2023L,
      procedencia = "x"); y })
  expect_error(dl_configuracion(9100L, d), "config[.]years[.]ancla[.]valor")
  d <- config_mut(function(y) { y$years$ajuste <- list(2024L); y$years$ancla <- 2023L; y })
  expect_error(dl_configuracion(9100L, d), "config[.]years[.]ancla")
  # years.post_2023 ya no existe: se rechaza con un mensaje que remite a years.ancla
  d <- config_mut(function(y) { y$years$post_2023 <- "excluir"; y })
  expect_error(dl_configuracion(9100L, d), "config[.]years[.]post_2023.*years[.]ancla")
})

# `cambios` se funde sobre el YAML antes de validar: la validación es la misma que la del YAML.
test_that("dl_configuracion(cambios) funde sobre el yaml antes de validar", {
  cfg <- dl_configuracion(9100L, config_dir_ejemplo(), cambios = list(years = list(ajuste = list(2024L),
      ancla = list(valor = 2023L, procedencia = "t"))))
  expect_identical(.dl_anio_ajuste(cfg), 2024L); expect_identical(.dl_anio_ancla(cfg), 2023L)
  expect_error(dl_configuracion(9100L, config_dir_ejemplo(), cambios = list(years = list(ajuste = list(2025L),
      ancla = list(valor = 2023L, procedencia = "t")))),
               "config[.]years[.]ancla[.]valor")
})

# Instantánea de dl_configuracion() sobre las configuraciones del ejemplo (inst/extdata/acs_peru/config), con dos
# huellas por archivo: el config_usado.yaml que escribiría dl_exportar_corrida() (el texto de yaml::as.yaml de la
# configuración validada, que write_yaml vuelca tal cual; se toma la huella del texto y no del archivo para que no
# dependa de los fines de línea del sistema) y el objeto con sus tipos. Si una limpieza rellena claves, cambia una
# conversión de tipo o rechaza una configuración, la instantánea cambia. Regenerar solo ante un cambio intencional:
# DL_ACTUALIZAR_GOLDEN=1 (tests/testthat/_referencia/caracterizacion/).
test_that("dl_configuracion sobre los configs del ejemplo reproduce la instantanea por archivo", {
  ids <- sort(as.integer(sub("[.]yaml$", "", list.files(config_dir_ejemplo(), pattern = "[.]yaml$"))))
  expect_length(ids, 4L)
  huellas <- vapply(ids, function(id) {
    cfg <- dl_configuracion(id, config_dir_ejemplo())
    sprintf("%d,%s,%s", id, digest::digest(yaml::as.yaml(cfg), algo = "sha256", serialize = FALSE),
            digest::digest(unclass(cfg), algo = "sha256"))
  }, "")
  expect_golden(c("cause_id,sha256_config_usado,sha256_objeto", huellas), "dl_config_acs.csv")
})


# `cambios` (dl_configuracion): lo que utils::modifyList() ignoraría en silencio es un error o se aplica.
test_that("cambios: una clave mal escrita es un error que sugiere la correcta", {
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(anchor = list(lamda = 0.5)), formato = "completo"),
               "la clave `anchor.lamda` no existe.*quisiste decir `anchor.lambda`")
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(medidas_entradas = "csmr"), formato = "completo"),
               "`medidas_entradas`.*quisiste decir `medidas_entrada`")
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(anchor = list(componente = list(sequela_idss = 1L))), formato = "completo"),
               "anchor.componente.sequela_idss")
  expect_error(dl_configuracion_ejemplo(9100L, cambios = "anchor.lambda=0.5", formato = "completo"),
               "`cambios` debe ser una lista con nombres")
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(0.5), formato = "completo"), "cada elemento lleva nombre")
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(anchor = list(lambda = 0.5),
                                                              anchor = list(rho_edad = 0)), formato = "completo"),
               "clave repetida")
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(anchor = list(0.5)), formato = "completo"), "bloque con claves")
})

test_that("cambios: un valor suelto donde la configuración tiene un bloque es un error que muestra la forma", {
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(anchor = 0.5), formato = "completo"),
               paste0("`anchor` es un bloque con claves en la configuración, no un valor suelto: va como ",
                      "anchor \\{location, lambda"))
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(cascada = list(heldout_anio = 2019L)), formato = "completo"),
               paste0("`cascada.heldout_anio` es un bloque con claves en la configuración, no un valor suelto: ",
                      "va como cascada.heldout_anio \\{valor, procedencia\\}"))
  expect_error(dl_configuracion_ejemplo(9100L, cambios = list(years = 2024L), formato = "completo"), "`years` es un bloque con claves")
  # las opciones de la cascada aceptan el valor suelto en lugar del bloque {valor, procedencia}
  d <- config_mut(function(y) { y$cascada$modo <- list(valor = "plana", procedencia = "prueba"); y })
  expect_identical(dl_configuracion(9100L, d, cambios = list(cascada = list(modo = "proxy")))$cascada$modo$valor,
                   "proxy")
})

test_that("cambios: una secuencia como lista sin nombres reemplaza a la del YAML", {
  a <- dl_configuracion_ejemplo(9100L, cambios = list(medidas_entrada = list("csmr"), anchor = list(lambda = 0.5)), formato = "completo")
  b <- dl_configuracion_ejemplo(9100L, cambios = list(medidas_entrada = "csmr", anchor = list(lambda = 0.5)), formato = "completo")
  expect_identical(a, b)
  tramos <- list(list(edad_inicio = 30, edad_fin = 50, valor = 0.1, fuente = "prueba"))
  c1 <- dl_configuracion_ejemplo(9100L, cambios = list(remision = list(por_edad = tramos)), formato = "completo")
  expect_identical(c1$remision$por_edad, tramos)
  # un segundo cambio de los tramos los reemplaza enteros (utils::modifyList() los ignoraría)
  tramos2 <- list(list(edad_inicio = 40, edad_fin = 60, valor = 0.2, fuente = "prueba"))
  d <- config_mut(function(y) { y$remision$por_edad <- tramos; y })
  expect_identical(dl_configuracion(9100L, d, cambios = list(remision = list(por_edad = tramos2)))$remision$por_edad,
                   tramos2)
})

test_that("cambios válidos (los del arnés) dan la misma configuración que utils::modifyList()", {
  y <- dismodlite:::.dl_leer_yaml(file.path(config_dir_ejemplo(), "9100.yaml"))
  proc <- "prueba"
  casos <- list(
    list(cascada = list(modo = list(valor = "plana", procedencia = proc)),
         remision = list(valor = 0.02, fuente = proc, por_edad = list(list(edad_inicio = 30, edad_fin = 50,
                                                                         valor = 0.1, fuente = proc)))),
    list(medidas_entrada = c("prev_estudio", "incidencia", "csmr"), offset_lognormal = 1e-4,
         anchor = list(lambda = 0.5, gate_err_mediano = list(valor = 0.10, procedencia = proc)),
         decisiones = c("a.", "b")),
    list(anchor = list(componente = list(sequela_ids = c(91012L, 91013L), motivo = proc)),
         severidad = list(fuente = "mod", run_id = "acs_v1", padre = 9100L)),
    list(years = list(ajuste = 2019L)),
    list(years = list(ajuste = 2024L, ancla = list(valor = 2023L, procedencia = proc))),
    list(offset_lognormal = NULL))
  for (k in seq_along(casos))
    expect_identical(dismodlite:::.dl_fundir_cambios(y, casos[[k]]), utils::modifyList(y, casos[[k]]), info = k)
})

test_that("un YAML de configuración que no está en UTF-8 da un error en español con el archivo", {
  d <- withr::local_tempdir()
  txt <- readLines(file.path(config_dir_ejemplo(), "9100.yaml"), encoding = "UTF-8")
  writeLines(iconv(txt, from = "UTF-8", to = "CP1252"), file.path(d, "9100.yaml"), useBytes = TRUE)
  expect_error(dl_configuracion(9100L, d), "^dl_configuracion\\(\\): el archivo «9100.yaml» no está en UTF-8")
})

test_that("causa y carpeta_config: mensajes que nombran el argumento", {
  expect_error(dl_configuracion("abc", config_dir_ejemplo()), "`causa` debe ser un cause_id entero")
  expect_error(dl_configuracion(9100L, c("a", "b")),
               "^dl_configuracion\\(\\): `carpeta_config` debe ser la ruta de una carpeta")
  expect_error(dl_configuracion(9100L, config_dir_ejemplo(), cambios = list(anchor = list(lambda = 3))),
               paste0("^dl_configuracion\\(\\): la configuración de la causa 9100 tiene 1 problema\\(s\\):\n",
                      "  - config[.]anchor[.]lambda"))
  expect_error(dl_configuracion(502L, config_dir_ejemplo()),
               "no hay configuración de la causa 502 en «.*»: se busca 502.yaml")
  expect_identical(dl_configuracion("9100", config_dir_ejemplo()), dl_configuracion(9100L, config_dir_ejemplo()))
})

test_that("anchor.location_id admite un código de texto", {
  loc <- function(x) dl_configuracion_ejemplo(9100, formato = "completo",
                                              cambios = list(anchor = list(location_id = x, location = NULL)))
  cfg <- loc("PAIS")
  expect_identical(cfg$anchor$location_id, "PAIS")
  expect_identical(dismodlite:::.dl_loc_ancla(cfg), "PAIS")
  # solo un texto de dígitos sin cero a la izquierda pasa a entero; "007" y "0" siguen siendo códigos de texto
  expect_identical(loc("123")$anchor$location_id, 123L)
  expect_identical(loc(123)$anchor$location_id, 123L)
  expect_identical(loc("007")$anchor$location_id, "007")
  expect_identical(dismodlite:::.dl_loc_ancla(loc("007")), "007")
  expect_identical(loc("0")$anchor$location_id, "0")
  expect_error(loc(0), "un entero positivo o un texto")
  expect_error(loc(1.5), "anchor.location_id")
  expect_error(loc("  "), "anchor.location_id")
})

test_that(".dl_loc_ancla normaliza un número de una lista sin validar como antes", {
  expect_identical(dismodlite:::.dl_loc_ancla(list(anchor = list(location_id = 1e5))), "100000")
  expect_identical(dismodlite:::.dl_loc_ancla(list(anchor = list(location_id = 123L))), "123")
  expect_identical(dismodlite:::.dl_loc_ancla(list(anchor = list(location = "peru"))), "123")
})

test_that("una clave desconocida del formato completo avisa con la más parecida", {
  d <- withr::local_tempdir()
  y <- yaml::read_yaml(system.file("extdata", "acs_peru_completo", "config", "9100.yaml", package = "dismodlite"))
  y$anchr <- list(lambda = 1)
  yaml::write_yaml(y, file.path(d, "9100.yaml"))
  expect_warning(dl_configuracion(9100, d), "anchor")
})

test_that("las configuraciones del paquete no avisan de claves desconocidas", {
  for (causa in c(9100L, 9101L, 9102L, 9103L))
    expect_no_warning(dl_configuracion_ejemplo(causa, formato = "completo"))
})

test_that("anchor.agrupar_bandas_finas es lógica y vale TRUE por defecto en el formato completo", {
  expect_true(dl_configuracion_ejemplo(9100, formato = "completo")$anchor$agrupar_bandas_finas)
  expect_error(dl_configuracion_ejemplo(9100, formato = "completo",
                                        cambios = list(anchor = list(agrupar_bandas_finas = "si"))),
               "agrupar_bandas_finas")
})
