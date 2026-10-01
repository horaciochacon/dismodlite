# Arnés de compatibilidad (ver helper-arnes-api.R): escritura de las referencias en disco y comparación con ellas.

# ---- Referencias en disco ----

# Número en texto con los dígitos justos para releerlo exacto con .a_numero (15, 16 o 17 significativos; con 17
# cifras un double siempre se relee exacto con redondeo correcto).
.num_texto <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  y <- x[ok]
  s <- sprintf("%.15g", y)
  for (d in 16:17) {
    malo <- .a_numero(s) != y
    malo[is.na(malo)] <- FALSE
    if (any(malo)) s[malo] <- sprintf(paste0("%.", d, "g"), y[malo])
  }
  .afirmar(identical(.a_numero(s), y), "un n\u00famero no se relee exacto")
  out[ok] <- s
  out
}

.escribir_csv <- function(dt, path) {
  dt <- data.table::copy(dt)
  for (cn in names(dt)) if (is.double(dt[[cn]])) data.table::set(dt, j = cn, value = .num_texto(dt[[cn]]))
  data.table::fwrite(dt, path, eol = "\n")
}

# Escribe el resultado de correr_escenario() en `dir`: parametros.csv, celdas.csv, manifiestos.yaml y tablas/*.csv
# (cada archivo solo si hay contenido).
escribir_referencia <- function(res, dir) {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(res$parametros) && nrow(res$parametros)) .escribir_csv(res$parametros, file.path(dir, "parametros.csv"))
  if (!is.null(res$celdas) && nrow(res$celdas)) .escribir_csv(res$celdas, file.path(dir, "celdas.csv"))
  if (length(res$manifiestos)) yaml::write_yaml(res$manifiestos, file.path(dir, "manifiestos.yaml"), precision = 15L)
  if (length(res$tablas)) {
    dir.create(file.path(dir, "tablas"), showWarnings = FALSE)
    for (t in names(res$tablas)) .escribir_csv(res$tablas[[t]], file.path(dir, "tablas", t))
  }
  invisible(dir)
}

# Perfiles de consolidación de la versión 0.2.2 copiados para el arnés, con el identificador y la descripción
# neutros (la consolidación no los usa en las tablas; el manifiesto que los lleva no se compara en esa parte).
escribir_perfiles_legado <- function(origen, destino) {
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  for (v in c("v1", "v2")) {
    lin <- readLines(file.path(origen, sprintf("perfil_%s.yaml", v)), encoding = "UTF-8", warn = FALSE)
    k_id <- grep("^perfil:", lin); k_desc <- grep("^descripcion:", lin)
    .afirmar(length(k_id) == 1L && length(k_desc) == 1L, "perfil_", v, ": sin perfil o descripcion")
    lin[k_id] <- sprintf("perfil: legado_%s", v)
    lin[k_desc] <- sprintf("descripcion: perfil de columnas %s de la versi\u00f3n 0.2.2 (copia del arn\u00e9s)", v)
    writeLines(enc2utf8(lin), file.path(destino, sprintf("perfil_%s.yaml", v)), useBytes = TRUE)
  }
  invisible(destino)
}

# ---- Comparación con la referencia (testthat) ----

.es_numerica <- function(v) is.numeric(v) || (is.logical(v) && all(is.na(v)))
.como_texto <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }

# Diferencia relativa máxima entre dos vectores numéricos con NA en las mismas posiciones.
.dif_relativa <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) return(0)
  max(abs(y[ok] - x[ok]) / pmax(abs(x[ok]), 1e-12))
}

.comparar_numeros <- function(id, que, ref, act, clave, tol) {
  if (is.null(ref)) return(testthat::expect_true(is.null(act) || !nrow(act), label = paste(id, que, "sin referencia")))
  testthat::expect_false(is.null(act), label = paste(id, que, "ausente"))
  if (is.null(act)) return(invisible())
  m <- merge(ref, act, by = clave, suffixes = c("_ref", "_act"), all = TRUE)
  testthat::expect_false(anyNA(m), label = paste(id, que, "con filas faltantes o sobrantes"))
  for (v in setdiff(names(ref), clave))
    testthat::expect_lte(.dif_relativa(m[[paste0(v, "_ref")]], m[[paste0(v, "_act")]]), tol,
                         label = paste(id, que, v, "diferencia relativa"))
}

.comparar_tabla <- function(id, nombre, ref, act, tol) {
  et <- paste(id, nombre)
  testthat::expect_identical(names(act), names(ref), label = paste(et, "columnas"))
  testthat::expect_identical(nrow(act), nrow(ref), label = paste(et, "filas"))
  if (!identical(names(act), names(ref)) || nrow(act) != nrow(ref)) return(invisible())
  for (cn in names(ref)) {
    x <- ref[[cn]]; y <- act[[cn]]
    if (.es_numerica(x) && .es_numerica(y)) {
      testthat::expect_identical(is.na(as.numeric(y)), is.na(as.numeric(x)), label = paste(et, cn, "NA"))
      testthat::expect_lte(.dif_relativa(as.numeric(x), as.numeric(y)), tol,
                           label = paste(et, cn, "diferencia relativa"))
    } else testthat::expect_identical(.como_texto(y), .como_texto(x), label = paste(et, cn))
  }
}

# Tolerancia relativa de la comparación: 1e-8 contra las referencias guardadas y 0 con DL_ARNES_EXACTO=1 (modo
# exacto: exige números idénticos bit a bit; solo con referencias generadas en la misma máquina).
tolerancia_arnes <- function(tol = 1e-8) if (identical(Sys.getenv("DL_ARNES_EXACTO"), "1")) 0 else tol

# Plataforma (sistema y arquitectura) en la que se generaron las referencias guardadas. En otra plataforma la
# biblioteca matemática puede diferir en el último bit y el MCMC amplifica esa diferencia, así que las referencias
# guardadas solo se comparan en la suya. En otra, se regeneran ahí con el código de v0.2.2
# (`Rscript data-raw/referencia_legado.R --salida <carpeta>`, que necesita la etiqueta git v0.2.2) y la prueba las
# lee con DL_REFERENCIA_DIR. Así se comprobó el 2026-10-01, en modo exacto, en Windows, Ubuntu y macOS.
plataforma_actual <- function() paste(Sys.info()[["sysname"]], R.version$arch)
saltar_si_otra_plataforma <- function() {
  if (nzchar(Sys.getenv("DL_REFERENCIA_DIR"))) return(invisible())
  f <- testthat::test_path("_referencia", "plataforma.txt")
  generada <- if (file.exists(f)) readLines(f, warn = FALSE)[1] else "desconocida"
  if (!identical(generada, plataforma_actual()))
    testthat::skip(sprintf(paste0("referencias generadas en %s y esta m\u00e1quina es %s: reg\u00e9n\u00e9ralas aqu\u00ed con ",
                                  "data-raw/referencia_legado.R --salida <carpeta> y DL_REFERENCIA_DIR"),
                           generada, plataforma_actual()))
}

# Compara el resultado de correr_escenario() con la referencia guardada. Con DL_REFERENCIA_DIR se leen las
# referencias de esa carpeta; la tolerancia la fija tolerancia_arnes(). `fuera`: claves de los manifiestos que no se
# comparan (rutas de .quitar_clave(), quitadas de los dos lados); los escenarios S las usan para la procedencia de
# los insumos (.MANIFIESTO_FUERA_SIMPLE).
comparar_con_referencia <- function(id, res, tol = 1e-8, fuera = list()) {
  tol <- tolerancia_arnes(tol)
  raiz <- Sys.getenv("DL_REFERENCIA_DIR")
  if (!nzchar(raiz)) raiz <- testthat::test_path("_referencia")
  ref_dir <- file.path(raiz, id)
  testthat::expect_true(dir.exists(ref_dir), label = paste(id, "tiene referencia"))
  leer <- function(archivo) {
    p <- file.path(ref_dir, archivo)
    if (file.exists(p)) .leer_csv_salida(p) else NULL
  }
  .comparar_numeros(id, "parametros", leer("parametros.csv"), res$parametros,
                    c("escenario", "causa", "sex_id", "parametro"), tol)
  .comparar_numeros(id, "celdas", leer("celdas.csv"), res$celdas,
                    c("escenario", "causa", "location_id", "sex_id", "age_group_id", "medida"), tol)
  man_p <- file.path(ref_dir, "manifiestos.yaml")
  if (file.exists(man_p)) {
    sin_fuera <- function(m) {
      for (ruta in fuera) m <- lapply(m, .quitar_clave, ruta)
      m
    }
    testthat::expect_identical(sin_fuera(res$manifiestos), sin_fuera(yaml::read_yaml(man_p)),
                               label = paste(id, "manifiestos"))
  } else testthat::expect_length(res$manifiestos, 0L)
  ref_tablas <- sort(list.files(file.path(ref_dir, "tablas"), pattern = "[.]csv$"), method = "radix")
  testthat::expect_identical(sort(as.character(names(res$tablas)), method = "radix"), ref_tablas,
                             label = paste(id, "nombres de las tablas"))
  for (t in intersect(ref_tablas, names(res$tablas)))
    .comparar_tabla(id, t, .leer_csv_salida(file.path(ref_dir, "tablas", t)), res$tablas[[t]], tol)
  invisible(res)
}
