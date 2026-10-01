# El registro de corridas (un YAML con la lista `datasets`: se lee, y se le agregan entradas al final) y las tablas
# de referencia de la carpeta `registro` de dl_rutas() (master_gbd.csv, etiquetas_es.csv). Los usan dl_insumos(),
# las funciones que escriben corridas, dl_sumar_hijas() y dl_consolidar(). Las lecturas se memorizan por (ruta,
# contenido), como los catálogos (.dl_leer_memo).

# Entradas del registro de corridas: la ruta del YAML o una carpeta con datasets.yaml; NULL si no hay registro.
.dl_registry_leer <- function(registry) {
  if (is.null(registry) || !nzchar(registry)) return(NULL)
  f <- if (dir.exists(registry)) file.path(registry, "datasets.yaml") else registry
  if (!file.exists(f)) return(NULL)
  .dl_leer_memo(f, function(p) .dl_leer_yaml(p)$datasets %||% list())
}

# Agrega una entrada al final del registro de corridas, con la sangría que ya usa el archivo (listas sangradas o sin
# sangría), escrita por el emisor de los manifiestos (yaml_bloque.R). El resto del archivo no se toca: nunca se
# reescribe con yaml::write_yaml, que lo reformatearía entero.
.dl_registry_append <- function(path, entry) {
  if (!file.exists(path))
    .dl_stop("no existe el archivo del registro de corridas %s", path)
  reg <- .dl_leer_yaml(path)
  if (!is.list(reg) || !("datasets" %in% names(reg)))
    .dl_stop(paste0("el registro de corridas %s no tiene la lista datasets (un registro vac\u00edo es la l\u00ednea ",
                    "\u00abdatasets: []\u00bb)"), path)
  id_nuevo <- entry$run_id %||% entry$acquisition_id
  ids <- vapply(reg$datasets %||% list(),
                function(d) as.character(d$run_id %||% d$acquisition_id %||% ""), character(1))
  if (id_nuevo %in% ids)
    .dl_stop("el registro de corridas %s ya tiene una entrada %s", path, id_nuevo)
  txt <- readChar(path, file.size(path), useBytes = TRUE)
  lineas <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  sangria <- if (any(grepl("^- ", lineas))) 0L else 2L
  bloque <- .dl_yaml_seq(list(entry), nivel_padre = 0L, sangria_lista = sangria)
  if (grepl("^datasets:\\s*\\[\\]\\s*$", txt)) {
    txt <- "datasets:\n"                              # lista vacía: se abre el bloque
  } else if (!endsWith(txt, "\n")) {
    txt <- paste0(txt, "\n")
  }
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeChar(paste0(txt, bloque), con, eos = NULL)
  invisible(path)
}

# Tabla `nombre` de la carpeta `registro` de dl_rutas() (sequela_rei.csv, master_gbd.csv, etiquetas_es.csv...), leída
# con .dl_leer_csv() y memorizada. `...` va a data.table::fread(); cada archivo se lee siempre con las mismas opciones.
.dl_archivo_registro <- function(paths, nombre, ...)
  .dl_leer_memo(file.path(.dl_path(paths, "registry"), nombre),
                function(p) .dl_leer_csv(p, ..., pieza = "registro", en_carpeta = TRUE))

# master_gbd.csv, con nombre_es e hijos como texto: el de la carpeta `registro` de las rutas (.dl_master_padre(),
# dl_consolidado_seleccionar()) o, con `maestro`, ese archivo (el argumento de dl_consolidar()).
.dl_master_leer <- function(paths, maestro = NULL) {
  texto <- list(character = c("nombre_es", "hijos"))
  if (is.null(maestro)) return(.dl_archivo_registro(paths, "master_gbd.csv", colClasses = texto))
  .dl_leer_memo(maestro, function(p) .dl_leer_csv(p, colClasses = texto, pieza = "maestro"))
}

# etiquetas_es.csv de la carpeta `registro`: (tabla, id, name, name_es, slug, slug_es), con tabla en {measure,
# metric, age_group}; las etiquetas en español que piden los perfiles de columnas del consolidado.
.dl_etiquetas_es <- function(paths = dl_rutas()) {
  e <- .dl_archivo_registro(paths, "etiquetas_es.csv", colClasses = "character")
  faltan <- setdiff(c("tabla", "id", "name", "name_es", "slug", "slug_es"), names(e))
  if (length(faltan)) .dl_stop("a etiquetas_es.csv le faltan las columnas %s", paste(faltan, collapse = ", "))
  e
}
