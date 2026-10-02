# Un proyecto con anchor.agrupar_bandas_finas = FALSE (lo corriente en un proyecto) corre entero y sus salidas llevan
# las bandas del ancla tal cual: 80-84 ... 95+ y, si el ancla trae una banda que no es grupo de GBD (85+), con su
# identificador sintetico y su nombre.

test_that("el ejemplo sin la agrupación de 0.2.2 usa 80-84 ... 95+ en el ancla y en las salidas", {
  b <- suppressMessages(dl_insumos(dl_proyecto(dl_ejemplo(), 9100)))
  ids <- b$prior_gbd[measure_id == 5L]$age_group_id
  expect_true(all(c(30L, 31L, 32L, 235L) %in% ids))
  expect_false(21L %in% ids)
  d <- withr::local_tempdir()
  r <- suppressMessages(dl_correr(dl_ejemplo(), 9100, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                  carpeta_salida = d))
  # las tablas de la corrida (cause/<medida>/) llevan las bandas del ancla
  f <- list.files(r$dir, "[.]csv$", recursive = TRUE, full.names = TRUE)
  prev <- data.table::fread(grep("prevalence", f, value = TRUE)[1L])
  expect_true(all(c(30L, 31L, 32L, 235L) %in% prev$age_group_id))
})

test_that("el factor de comorbilidad y los AVD se calculan en las bandas del ancla", {
  b <- suppressMessages(dl_insumos(dl_proyecto(dl_ejemplo(), 9100)))
  como <- dl_factor_comorbilidad(b)
  expect_setequal(unique(como$age_group_id), unique(b$prior_gbd[measure_id == 5L]$age_group_id))
})

# Proyecto del ejemplo con 85+ como una sola banda sintetica (poblacion y ancla agrupadas, ponderadas por poblacion).
proyecto_85_mas <- function() {
  t <- dl_proyecto(dl_ejemplo(), 9100)$tablas
  pob <- data.table::copy(t$poblacion)
  pob[edad_inicio >= 85, `:=`(edad_inicio = 85, edad_fin = 125)]
  pob <- pob[, list(poblacion = sum(poblacion)), by = list(ubicacion, anio, sexo, edad_inicio, edad_fin)]
  # ponderacion: la poblacion del ejemplo es solo subnacional, el peso nacional es su suma
  w <- t$poblacion[, list(N = sum(poblacion)), by = list(anio, sexo, edad_inicio)]
  an <- merge(data.table::copy(t$ancla), w, by = c("anio", "sexo", "edad_inicio"), all.x = TRUE)
  an[edad_inicio >= 85, `:=`(valor = sum(valor * N) / sum(N), inferior = sum(inferior * N) / sum(N),
                             superior = sum(superior * N) / sum(N)), by = list(causa, anio, sexo, medida)]
  an <- an[edad_inicio <= 85][edad_inicio == 85, edad_fin := 125][, N := NULL]
  dl_proyecto(dl_ejemplo(), 9100, poblacion = as.data.frame(pob), ancla = as.data.frame(an[!is.na(valor)]))
}

test_that("una banda sintética (85+, que no es grupo de GBD) tiene id y nombre", {
  p <- proyecto_85_mas()
  b <- suppressMessages(dl_insumos(p))
  expect_true(990001L %in% b$prior_gbd$age_group_id)
  cat_edades <- dismodlite:::.dl_bandas_catalogo(p$rutas)
  expect_identical(dismodlite:::.dl_texto_banda(990001L, cat_edades), "85 años y más")
})

test_that("la corrida con 85+ nombra la banda en sus salidas y el consolidado la lleva con su nombre", {
  p <- proyecto_85_mas()
  d <- withr::local_tempdir()
  reg <- file.path(d, "registro de corridas.yaml"); writeLines("datasets: []", reg)
  # un registro de causas donde 9100 se ajusta sola (en el del ejemplo es la suma de sus hijas)
  registro <- file.path(d, "registro"); dir.create(registro)
  file.copy(list.files(p$rutas$registry, full.names = TRUE), registro)
  writeLines(c("cause_id,nombre_es,hijos", "9100,Arteriopatía crónica sintética,"),
             file.path(registro, "master_gbd.csv"), useBytes = TRUE)
  rutas <- p$rutas; rutas$registry <- registro
  r <- suppressMessages(dl_correr(p, 9100, semilla = 1, rapido = TRUE, sensibilidad = FALSE, carpeta_salida = d,
                                  registro = reg))
  for (medida in c("prevalence", "incidence", "yld")) {
    y <- data.table::fread(file.path(r$dir, "cause", medida, paste0(r$run_id, ".csv")), encoding = "UTF-8")
    expect_true(990001L %in% y$age_group_id, info = medida)
    expect_true(all(c(30L, 990001L) %in% y$age_group_id), info = medida)
    expect_identical(unique(y[age_group_id == 990001L]$age_group_name), "85 años y más", info = medida)
  }
  et <- data.table::fread(file.path(r$dir, "etiquetas", paste0(r$run_id, ".csv")))
  expect_true(990001L %in% et$age_group_id)

  cons <- suppressMessages(dl_consolidar(reg, d, system.file("perfiles", "perfil_v1.yaml", package = "dismodlite"),
                                         file.path(registro, "master_gbd.csv"), rutas = rutas))
  tablas <- list.files(file.path(cons$dir, "tablas"), "[.]csv$", full.names = TRUE)
  expect_gt(length(tablas), 0L)
  for (f in tablas) {
    y <- data.table::fread(f, encoding = "UTF-8")
    # la banda sintética va con su nombre y las de GBD, con el de GBD
    expect_identical(unique(y[edad_id == 990001L]$edad), "85 años y más", info = basename(f))
    expect_identical(unique(y[edad_id == 30L]$edad), "80-84 years", info = basename(f))
  }
})
