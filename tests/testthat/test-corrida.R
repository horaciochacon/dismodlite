# dl_exportar_corrida(): el nombre de la corrida se valida antes de escribir, el identificador se arma con el nombre
# tal cual, el registro debe existir, el resumen debe ser de las mismas piezas y las rutas por defecto son las de los
# insumos.

.piezas_mini <- function(x = corrida_mini())
  list(resumen = dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = x$insumos)), fit = x$ajuste, yld = x$avd,
       bundle = x$insumos)

test_that("sin `rutas`, la corrida usa las de los insumos; el mismo nombre da la versión siguiente", {
  x <- corrida_mini(); d <- withr::local_tempdir()
  r1 <- dl_exportar_corrida(.piezas_mini(x), "acs-prueba", carpeta = d, etiquetas = x$etiquetas, forzar = TRUE)
  r2 <- dl_exportar_corrida(.piezas_mini(x), "acs-prueba", carpeta = d, etiquetas = x$etiquetas, forzar = TRUE)
  expect_match(r1$run_id, "^[0-9]{4}-[0-9]{2}-[0-9]{2}_acs-prueba_v1$")
  expect_match(r2$run_id, "_acs-prueba_v2$")
  expect_identical(r1$manifest$generado, substr(r1$run_id, 1L, 10L))   # la fecha del run_id, aun a medianoche
  expect_true(file.exists(file.path(r1$dir, "manifest.yaml")))
  expect_output(print(r1), "<dl_run>.*particiones canónicas.*forzar = TRUE")
  expect_output(print(r1), paste0("^<dl_run> corrida [0-9-]+_acs-prueba_v1 \\(ajuste nacional\\)\n  carpeta: .*\n",
                                  "  3 particiones canónicas \\(una por medida\\)"))
})

test_that("un valor que el manifiesto no puede escribir exacto falla antes de escribir nada", {
  x <- corrida_mini(); d <- withr::local_tempdir()
  piezas <- .piezas_mini(x)
  piezas$bundle <- dismodlite:::.dl_bundle_con(x$insumos, lambda = 1 / 3)     # mismo hash: solo cambia lambda
  expect_error(dl_exportar_corrida(piezas, "acs-prueba", carpeta = d, etiquetas = x$etiquetas, forzar = TRUE),
               "^dl_exportar_corrida\\(\\): el valor 0.3333333 de «params.lambda» .*menos decimales")
  expect_length(list.files(d, recursive = TRUE, include.dirs = TRUE), 0L)
})

test_that("si la escritura falla a medias, la carpeta de la corrida se borra (el próximo intento sigue siendo _v1)", {
  x <- corrida_mini(); d <- withr::local_tempdir()
  local_mocked_bindings(.dl_escribir_manifest = function(man, dir_run) stop("falla simulada al escribir"))
  expect_error(dl_exportar_corrida(.piezas_mini(x), "acs-prueba", carpeta = d, etiquetas = x$etiquetas,
                                   forzar = TRUE), "falla simulada")
  expect_length(list.dirs(file.path(d, "mod", "dismod_lite"), recursive = FALSE), 0L)
})

test_that("el identificador compara el nombre tal cual, sin expresiones regulares", {
  d <- withr::local_tempdir(); hoy <- format(Sys.Date())
  base <- file.path(d, "mod", "dismod_lite")
  for (v in c("a-b_v1", "a-b_v7", "a-b_vx", "a-bc_v9", "a-b-c_v3")) dir.create(file.path(base, paste0(hoy, "_", v)),
                                                                           recursive = TRUE)
  expect_identical(dismodlite:::.dl_run_id(d, "a-b"), paste0(hoy, "_a-b_v8"))
  expect_identical(dismodlite:::.dl_run_id(d, "x"), paste0(hoy, "_x_v1"))
})

test_that("el run_id se parte en fecha, nombre y versión, también con nombres de registros anteriores", {
  partes <- dismodlite:::.dl_partes_run_id
  expect_identical(partes("2026-01-02_Acs_Nac_v1_v12"), list(fecha = "2026-01-02", nombre = "Acs_Nac_v1", v = 12L))
  expect_null(partes("acs_v1"))
  expect_error(partes("acs_v1", "del registro de corridas"),
               "el run_id acs_v1 del registro de corridas no sigue el patrón")
  # la corrida vigente: la fecha manda y la versión se compara como número
  expect_true(dismodlite:::.dl_run_gana("2026-01-02_a_v10", "2026-01-02_a_v9"))
  expect_false(dismodlite:::.dl_run_gana("2026-01-01_a_v10", "2026-01-02_a_v1"))
})

test_that("un nombre con mayúsculas, guiones bajos, espacios o tildes se rechaza antes de escribir", {
  x <- corrida_mini(); d <- withr::local_tempdir()
  for (nm in c("acs_nac", "ACS Nacional 2023", "acs-años", "acs(1", ""))
    expect_error(dl_exportar_corrida(.piezas_mini(x), nm, carpeta = d, etiquetas = x$etiquetas, forzar = TRUE),
                 "^dl_exportar_corrida\\(\\): `nombre` solo admite minúsculas sin tildes", info = nm)
  expect_error(dl_sumar_hijas(d, 9100L, "e5_suma", carpeta = d), "`nombre` solo admite")
  expect_error(dl_consolidar("r.yaml", d, "p.yaml", "m.csv", nombre = "Consolidado"), "`nombre` solo admite")
  expect_length(list.files(d, recursive = TRUE), 0L)
})

test_that("un registro que no existe se rechaza antes de escribir la corrida", {
  x <- corrida_mini(); d <- withr::local_tempdir(); no <- file.path(d, "no_existe.yaml")
  expect_error(dl_exportar_corrida(.piezas_mini(x), "x", carpeta = d, etiquetas = x$etiquetas, forzar = TRUE,
                                   registro = no), "no existe el archivo del registro de corridas \\(`registro`\\)")
  expect_error(dl_export(.piezas_mini(x), "x", out_root = d, labels = x$etiquetas, force = TRUE, registry_path = no),
               "no existe el archivo del registro")
  expect_error(dl_reresumir_corrida(d, carpeta = d, registro = no), "no existe el archivo del registro")
  expect_error(dl_sumar_hijas(d, 9100L, "x", carpeta = d, registro = no), "no existe el archivo del registro")
  expect_error(dl_consolidar("r.yaml", d, "p.yaml", "m.csv", registro_salida = no),
               "no existe el archivo del registro de corridas \\(`registro_salida`\\)")
  expect_length(list.files(d, recursive = TRUE), 0L)
})

test_that("un resumen, un ajuste y unos AVD de corridas distintas no se exportan juntos", {
  x <- corrida_mini(); d <- withr::local_tempdir()
  rc <- dl_resumir(list(fit = x$cascada, yld = x$avd_cascada, bundle = x$insumos))
  expect_error(dl_exportar_corrida(list(resumen = rc, fit = x$ajuste, yld = x$avd, bundle = x$insumos), "mezcla",
                                   carpeta = d, etiquetas = x$etiquetas, forzar = TRUE),
               "el `resumen` no se hizo con este `fit`")
  expect_error(dl_exportar_corrida(list(resumen = rc, fit = x$cascada, yld = x$avd, bundle = x$insumos), "mezcla",
                                   carpeta = d, etiquetas = x$etiquetas, forzar = TRUE),
               "no salen del ajuste de `fit`")
  expect_error(dl_exportar_corrida(.piezas_mini(x), "x", carpeta = d, forzar = TRUE), "falta `etiquetas`")
  expect_error(dl_exportar_corrida(.piezas_mini(x), "x", carpeta = "", etiquetas = x$etiquetas), "falta `carpeta`")
  expect_error(dl_exportar_corrida(.piezas_mini(x), "x", carpeta = d, etiquetas = x$etiquetas),
               "no convergieron.*dl_opciones_mcmc")
  expect_length(list.files(d, recursive = TRUE), 0L)
})

test_that("insumos, corrida y re-resumen bajo una carpeta con espacios y tildes (Ana María/Mis análisis)", {
  base <- file.path(withr::local_tempdir(), "Ana María", "Mis análisis")
  dir.create(file.path(base, "datos de ejemplo"), recursive = TRUE)
  expect_true(all(file.copy(c(dl_ejemplo(), ruta_acs()), file.path(base, "datos de ejemplo"), recursive = TRUE)))
  # formato completo: los insumos se leen de la copia (configuración, ancla, covariables, población, severidad,
  # catálogos, registro) y son los mismos que desde el paquete
  raiz_c <- file.path(base, "datos de ejemplo", "acs_peru_completo")
  rc <- rutas_acs(api_nueva(), raiz_c, 9100L, con_datos = FALSE)
  expect_true(all(startsWith(unlist(unclass(rc)), raiz_c)))
  bc <- suppressMessages(dl_insumos(dl_configuracion(9100L, file.path(raiz_c, "config")), rc))
  expect_identical(bc$hash, suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"),
                                                        dl_rutas_ejemplo(9100L, datos = FALSE,
                                                                         formato = "completo")))$hash)
  # formato simple: el proyecto se lee de la copia y da los mismos insumos que corrida_mini()
  raiz <- file.path(base, "datos de ejemplo", "acs_peru")
  p <- dl_proyecto(raiz, 9100L)
  expect_identical(normalizePath(p$carpeta), normalizePath(raiz))
  r <- p$rutas; r["datos"] <- list(NULL)
  b <- suppressMessages(dl_insumos(p$configuracion, r))
  x <- corrida_mini()
  expect_identical(b$hash, x$insumos$hash)          # los mismos datos, leídos desde la carpeta con tildes
  f <- dl_ajustar(b, x$opciones, semilla = 7L)
  y <- dl_avd(f, b, semilla = 7L)
  reg <- file.path(base, "registro de corridas.yaml"); writeLines("datasets: []", reg)
  run <- dl_exportar_corrida(list(resumen = dl_resumir(list(fit = f, yld = y, bundle = b)), fit = f, yld = y,
                                  bundle = b), "acs-tildes", carpeta = base, etiquetas = x$etiquetas, forzar = TRUE,
                             registro = reg)
  expect_identical(run$dir, file.path(base, "mod", "dismod_lite", run$run_id))
  for (md in c("prevalence", "incidence", "yld"))
    expect_true(file.exists(file.path(run$dir, "cause", md, paste0(run$run_id, ".csv"))), info = md)
  expect_true(file.exists(file.path(run$dir, "draws", "prevalence_2023.csv.gz")))
  expect_true(file.exists(file.path(run$dir, "inputs", "config_usado.yaml")))
  man <- yaml::read_yaml(file.path(run$dir, "manifest.yaml"))
  expect_identical(man$run_id, run$run_id)
  # re-resumen de la corrida: versión 2 en la misma carpeta, que enlaza o copia el resto de archivos
  rr <- dl_reresumir_corrida(run$dir, carpeta = base, rutas = r, registro = reg)
  expect_match(rr$run_id, "_acs-tildes_v2$")
  expect_identical(yaml::read_yaml(file.path(rr$dir, "manifest.yaml"))$resumen$run_origen, run$run_id)
  expect_true(file.exists(file.path(rr$dir, "draws", "prevalence_2023.csv.gz")))
  expect_identical(vapply(yaml::read_yaml(reg)$datasets, function(d) d$run_id, ""), c(run$run_id, rr$run_id))
  # re-resumen escrito en otra carpeta: no repite el identificador de su origen (versión 1 de hoy -> versión 2)
  otra <- withr::local_tempdir()
  rr_otra <- dl_reresumir_corrida(run$dir, carpeta = otra, rutas = r)
  expect_false(identical(rr_otra$run_id, run$run_id))
  expect_match(rr_otra$run_id, "_acs-tildes_v2$")
  expect_identical(rr_otra$dir, file.path(otra, "mod", "dismod_lite", rr_otra$run_id))
  expect_output(print(rr_otra), sprintf("\\(re-resumen de %s\\)", run$run_id))
})

test_that("`nivel` distinto de 0.95 se rechaza al entrar, nombrando `nivel`, antes de leer o escribir", {
  x <- corrida_mini(); d <- withr::local_tempdir()
  expect_error(dl_reresumir_corrida(d, carpeta = d, nivel = 0.9),
               "^dl_reresumir_corrida\\(\\): `nivel` debe ser 0.95 para escribir la corrida")
  expect_error(dl_sumar_hijas(d, 9100L, "x", carpeta = d, nivel = 0.9),
               "^dl_sumar_hijas\\(\\): `nivel` debe ser 0.95")
  expect_error(dl_resumir_run(d, out_root = d, ui_level = 0.9), "`nivel` debe ser 0.95")
  r90 <- dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = x$insumos), nivel = 0.9)
  expect_error(dl_exportar_corrida(list(resumen = r90, fit = x$ajuste, yld = x$avd, bundle = x$insumos), "x",
                                   carpeta = d, etiquetas = x$etiquetas, forzar = TRUE),
               "^dl_exportar_corrida\\(\\): el resumen se hizo con `nivel` = 0.9 en dl_resumir\\(\\)")
  expect_length(list.files(d, recursive = TRUE), 0L)
})

# Limitaciones del manifiesto de una corrida: salen de los hechos de la corrida, sin texto fijo sobre una causa, un
# país o una ronda del ancla, y sin referencias internas.
test_that("las limitaciones de una corrida describen su cascada y su AVD sin texto fijo", {
  x <- corrida_mini(); b <- x$insumos; cfg <- b$cfg
  prohibido <- "spec|\u00a7|INEI|ENDES|GBD ?20[0-9]{2}|HAQI|CVD|: "
  res_nac <- dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = b))
  nac <- unlist(dismodlite:::.dl_limitaciones_corrida(cfg, b, res_nac, NULL, NULL, 0.05))
  expect_false(any(grepl(prohibido, nac)), label = paste(nac, collapse = "\n"))
  expect_true(any(startsWith(nac, "sin cascada \u2014")))
  expect_true(any(startsWith(nac, "sin correcci\u00f3n por comorbilidad")))       # sin factor de comorbilidad
  res_c <- dl_resumir(list(fit = x$cascada, yld = x$avd_cascada, bundle = b))
  casc <- unlist(dismodlite:::.dl_limitaciones_corrida(cfg, b, res_c, x$cascada, NULL, 0.05))
  expect_false(any(grepl(prohibido, casc)), label = paste(casc, collapse = "\n"))
  expect_length(casc, length(nac) + 1L)            # la cascada por proxies declara dos limitaciones en lugar de una
  expect_true(any(startsWith(casc, "cascada \u2014 X muestreado por simulaci\u00f3n")))
  # la severidad del ejemplo no tiene beta de covariable: no hay canal de proporción que declarar
  expect_false(any(grepl("proporciones de severidad", casc)))
  b_sev <- b; b_sev$severidad <- data.table::copy(b$severidad)
  b_sev$severidad[health_state_id == 9802L, beta_covariable := "haqi"]
  casc_sev <- unlist(dismodlite:::.dl_limitaciones_corrida(cfg, b_sev, res_c, x$cascada, NULL, 0.05))
  expect_true(any(grepl(paste("proporciones de severidad con el valor nacional de haqi en todas las ubicaciones",
                              "subnacionales"), casc_sev)))
})

test_that("la limitación de una proyección nombra la población del año, y los proxies solo si hay cascada", {
  cfg <- dl_configuracion_ejemplo(9100L, cambios = list(years = list(ajuste = 2024L, ancla = list(
    valor = 2023L, procedencia = "proyecci\u00f3n de prueba"))), formato = "completo")
  b <- list(poblacion = data.table::data.table(year = c(2023L, 2024L), acquisition_id = c("pob_2023", "pob_2024")))
  sin_casc <- dismodlite:::.dl_limitacion_ancla(cfg, b)
  expect_match(sin_casc, "^proyecci\u00f3n declarada \u2014 ancla .* de 2023 reetiquetada a 2024")
  expect_match(sin_casc, "del 2024 son solo la poblaci\u00f3n \\(pob_2024\\) \u2014 proyecci\u00f3n de prueba$")
  expect_match(dismodlite:::.dl_limitacion_ancla(cfg, b, list(modo = "proxy")),
               "poblaci\u00f3n \\(pob_2024\\) y los proxies subnacionales de la cascada")
  expect_false(grepl("proxies", dismodlite:::.dl_limitacion_ancla(cfg, b, list(modo = "plana"))))
  expect_false(grepl("INEI|ENDES|GBD", sin_casc))
  expect_null(dismodlite:::.dl_limitacion_ancla(dl_configuracion_ejemplo(9100L, formato = "completo"), b))
})

test_that("la limitación de la mortalidad de validación de otro año dice qué hizo la corrida con ella", {
  x <- corrida_mini(); b <- x$insumos; cfg <- b$cfg   # el ejemplo declara el held-out de 2019 y ajusta 2023
  expect_null(dismodlite:::.dl_limitacion_heldout(dl_configuracion_ejemplo(9100L, anio = 2019L, formato = "completo")))
  sin_casc <- dismodlite:::.dl_limitacion_heldout(cfg)
  expect_match(sin_casc, paste0("^mortalidad subnacional de validación declarada de 2019 para una ",
                                "corrida de 2023 \u2014 sin validación de amplitud \\(la corrida no trae la ",
                                "validación del ancla\\)"))
  expect_match(sin_casc, "\u2014 declarado en la configuración del proyecto$")   # la procedencia de un proyecto
  # el motivo es el que dl_validar_ancla() dejó en el atributo sin_amplitud (con las palabras del manifiesto): sin
  # cascada, o con la cascada y sin mortalidad subnacional de validación (la corrida mínima no trae datos)
  con_validacion <- vapply(list(NULL, x$cascada), function(casc) {
    v <- suppressMessages(dl_validar_ancla(x$ajuste, b, cascada = casc))
    expect_null(attr(v, "amplitud"))
    lim <- dismodlite:::.dl_limitacion_heldout(cfg, v)
    expect_match(lim, sprintf("sin validación de amplitud (%s)", attr(v, "sin_amplitud")), fixed = TRUE)
    lim
  }, "")
  expect_match(con_validacion[2], "(los datos no traen mortalidad subnacional de validación)", fixed = TRUE)
  con_amplitud <- dismodlite:::.dl_limitacion_heldout(cfg, structure(list(), amplitud = data.frame(sex_id = 1L)))
  expect_match(con_amplitud, paste0("^mortalidad subnacional de validación de 2019 contra una cascada ",
                                    "de 2023 \u2014 la validación de amplitud supone"))
  expect_false(any(grepl(": ", c(sin_casc, con_validacion, con_amplitud))))
  # la limitación aparece con o sin cascada: la condición es la misma de siempre (años de la configuración)
  res <- dl_resumir(list(fit = x$ajuste, yld = x$avd, bundle = b))
  nac <- unlist(dismodlite:::.dl_limitaciones_corrida(cfg, b, res, NULL, NULL, 0.05))
  expect_true(sin_casc %in% nac)
  nac_2019 <- unlist(dismodlite:::.dl_limitaciones_corrida(dl_configuracion_ejemplo(9100L, anio = 2019L, formato = "completo"), b, res,
                                                           NULL, NULL, 0.05))
  expect_length(nac_2019, length(nac) - 1L)
})

test_that("las limitaciones de una suma salen de lo que hizo la suma, sin texto fijo de una causa", {
  lim <- unlist(dismodlite:::.dl_limitaciones_suma(9101:9103, NULL, 2023L, 2023L, c(0, 0, 0)))
  expect_length(lim, 2L)
  expect_match(lim[1], paste0("^suma simulación a simulación de 3 causas hijas \\(9101, 9102, 9103\\) \u2014 la ",
                              "correlación entre hijas no se modela"))
  expect_match(lim[2], "^sin ajuste propio \u2014 .*inputs.runs_hijas")
  omitida <- list(list(cause_id = 9103L, motivo = "variante: sin el subtipo"))
  lim2 <- unlist(dismodlite:::.dl_limitaciones_suma(9101:9102, omitida, 2024L, 2023L, c(0.3, 0)))
  expect_length(lim2, 5L)
  expect_identical(lim2[3], "hija 9103 omitida de la suma \u2014 variante \u2014 sin el subtipo")
  expect_match(lim2[4], "las hijas llevan el ancla de 2023 reetiquetada a 2024")
  expect_match(lim2[5], "^fase aguda descontada del csmr en la\\(s\\) hija\\(s\\) 9101 \u2014")
  expect_false(any(grepl("draw|subtipos|GBD|: |\\bfit\\b|inflow|\\brun\\b", c(lim, lim2))),
               label = paste(c(lim, lim2), collapse = "\n"))
})

test_that("la corrida congela las tablas del contrato y sirven para repetirla", {
  d <- withr::local_tempdir()
  r <- suppressMessages(dl_correr(dl_ejemplo(), 9100, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                  carpeta_salida = d))
  cong <- file.path(r$dir, "inputs", "contrato")
  expect_true(all(file.exists(file.path(cong, c("ubicaciones.csv", "poblacion.csv", "ancla.csv")))))
  expect_false(dir.exists(file.path(r$dir, "inputs", "ghdx_cov")))
  man <- yaml::read_yaml(file.path(r$dir, "manifest.yaml"))
  p0 <- dl_proyecto(dl_ejemplo(), 9100)
  tablas <- Filter(Negate(is.null), lapply(man$inputs$contrato, `[[`, "tabla"))
  expect_setequal(unlist(tablas), names(p0$tablas))
  archivo <- function(x) file.path(cong, x$configuracion %||% paste0(x$tabla, ".csv"))
  for (x in man$inputs$contrato)
    expect_identical(digest::digest(file = archivo(x), algo = "sha256"), x$sha256)
  # la configuración del proyecto, tal como se leyó (no la traducida: esa es config_usado.yaml)
  expect_true("config.yaml" %in% unlist(lapply(man$inputs$contrato, `[[`, "configuracion")))
  expect_identical(dismodlite:::.dl_leer_config(file.path(cong, "config.yaml")),
                   dismodlite:::.dl_leer_config(file.path(dl_ejemplo(), "config", "9100.yaml")))
  # solo con la carpeta de la corrida (sin dl_ejemplo()): la configuración y las tablas congeladas arman los mismos
  # insumos
  csv <- list.files(cong, "[.]csv$")
  rutas <- stats::setNames(as.list(file.path(cong, csv)), tools::file_path_sans_ext(csv))
  p <- do.call(dl_proyecto, c(list(configuracion = file.path(cong, "config.yaml")), rutas))
  expect_identical(p$configuracion$cause_id, 9100L)
  expect_identical(dl_insumos(p)$hash, man$inputs$bundle_hash)
  # inputs/contrato/ es también la carpeta de un proyecto
  expect_identical(dl_insumos(dl_proyecto(cong))$hash, man$inputs$bundle_hash)
})

test_that("la corrida de un subtipo con las betas de su causa padre se repite desde inputs/contrato/", {
  d <- withr::local_tempdir()
  r <- suppressMessages(dl_correr(dl_ejemplo(), 9101, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                  carpeta_salida = d))
  man <- yaml::read_yaml(file.path(r$dir, "manifest.yaml"))
  cong <- file.path(r$dir, "inputs", "contrato")
  # betas.csv congela las betas que usó la causa (las de su padre), bajo la causa del subtipo
  b <- data.table::fread(file.path(cong, "betas.csv"))
  expect_identical(unique(b$causa), 9101L)
  expect_setequal(b$covariable, c("SEV_scalar_agestd_cvd_pvd", "LDI_pc", "haqi"))
  p <- dl_proyecto(cong)
  expect_identical(p$configuracion$extraction$cause_id, 9100L)    # dl_sumar_hijas() la exige
  expect_identical(dl_insumos(p)$hash, man$inputs$bundle_hash)
  expect_identical(man$causa$extraction_cause_id, 9100L)
})

test_that("la corrida con severidad.particion congela la partición y se repite desde inputs/contrato/", {
  config <- c("causa: 302", "anio: 2020", "edad_inicio: 40", "severidad:", "  particion: particion/mini")
  d <- escribir_particion_mini(escribir_pais_ficticio(file.path(withr::local_tempdir(), "pf"), 302L, 2020L, config))
  unlink(file.path(d, "severidad.csv"))
  r <- suppressMessages(dl_correr(d, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
                                  carpeta_salida = withr::local_tempdir()))
  man <- yaml::read_yaml(file.path(r$dir, "manifest.yaml"))
  cong <- file.path(r$dir, "inputs", "contrato")
  # cada archivo de la partición, copiado con la ruta de severidad.particion en la configuración congelada
  part <- unlist(lapply(man$inputs$contrato, `[[`, "particion"))
  expect_setequal(part, paste0("particion/mini/", c("cause_sequela", "cause_health_state"), "/proportion/mini.csv"))
  for (x in Filter(function(x) !is.null(x$particion), man$inputs$contrato))
    expect_identical(digest::digest(file = file.path(cong, x$particion), algo = "sha256"), x$sha256)
  expect_identical(dismodlite:::.dl_leer_config(file.path(cong, "config.yaml"))$severidad$particion, "particion/mini")
  # sin la carpeta original del proyecto
  unlink(d, recursive = TRUE)
  expect_identical(suppressMessages(dl_insumos(dl_proyecto(cong)))$hash, man$inputs$bundle_hash)
})

test_that("la configuración de un proyecto se congela con sus tipos: releída es la misma", {
  d <- withr::local_tempdir()
  s <- list(causa = 9101L, anio = 2023, edad_inicio = 30L, nombre = "\u00c1rbol: x",
            subnacional = list(modo = "no", kappa = 0.1 + 0.2), sensibilidad = list(peso = c(0.1, 0.5, 1)),
            notas = c("a", "b"), datos_en_ajuste = "prevalencia", avanzado = list(anchor = list(
              agrupar_bandas_finas = TRUE)))
  h <- dismodlite:::.dl_congelar_contrato(list(), d, configuracion = s)
  f <- file.path(d, "config.yaml")
  expect_identical(h[["config.yaml"]], digest::digest(file = f, algo = "sha256"))
  y <- dismodlite:::.dl_leer_config(f)
  expect_identical(y[setdiff(names(s), "sensibilidad")], s[setdiff(names(s), "sensibilidad")])
  expect_identical(unlist(y$sensibilidad$peso), s$sensibilidad$peso)
  expect_identical(y$subnacional$kappa, 0.1 + 0.2)           # el double exacto
})
