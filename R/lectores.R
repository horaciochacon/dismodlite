# Lectores: convierten fuentes conocidas a una tabla del contrato (R/contrato.R). Cada archivo de una carpeta se
# reconoce por sus columnas (.DL_LECTORES), sin declarar nada. Las unidades se convierten con la misma operación que
# usaba el formato completo, para que un número leído sea el mismo double en los dos caminos.

# Firma de cada lector (las columnas que debe traer, ya en minúsculas; `alguna`: de estas, al menos una) y la tabla del
# contrato que produce.
.DL_LECTORES <- list(
  gbd_results = list(firma = c("measure_id", "metric_name", "val", "upper", "lower"),
                     alguna = c("age_id", "age_group_id"), tabla = "ancla", nombre = "descarga de GBD Results"),
  ghdx_covariables = list(firma = c("covariate_name_short", "mean_value"), tabla = "covariables",
                          nombre = "descarga de covariables del GHDx"),
  ghdx_fuentes = list(firma = c("nid", "component_id", "cause_id"), tabla = "fuentes_gbd",
                      nombre = "lista de fuentes del GHDx"))

# measure_id de GBD -> medida del contrato.
.DL_MEDIDA_GBD <- c("5" = "prevalencia", "1" = "mortalidad", "3" = "avd", "6" = "incidencia")

# component_id del GHDx -> componente del contrato.
.DL_COMPONENTE_GHDX <- c("5" = "no_fatal", "4" = "causa_de_muerte")

# Escala de las tasas de GBD Results: por 100 000.
.DL_ESCALA_RATE <- 1e5

# Columnas numéricas de las descargas: se leen tal cual (no pasan por texto, que perdería cifras en un double).
.DL_COLUMNAS_NUMERICAS_GBD <- c("val", "upper", "lower", "mean_value", "lower_value", "upper_value")

# Lector de un archivo con columnas `columnas`: el de .DL_LECTORES cuya firma trae, «contrato» si trae alguna columna
# del eje o de valores del contrato, o NA.
.dl_reconocer_lector <- function(columnas) {
  columnas <- tolower(trimws(columnas))
  for (k in names(.DL_LECTORES)) {
    l <- .DL_LECTORES[[k]]
    if (all(l$firma %in% columnas) && (is.null(l$alguna) || any(l$alguna %in% columnas))) return(k)
  }
  if ("age_id" %in% columnas || "age_group_id" %in% columnas) return(NA_character_)
  if (any(columnas %in% unique(.dl_tablas_ref()$columna))) return("contrato")
  NA_character_
}

# location_id de GBD del país entre las ubicaciones `locs` de una descarga (`de`: cuál, para el mensaje).
.dl_ubicacion_gbd <- function(locs, opciones, de) {
  locs <- unique(as.character(locs))
  u <- opciones$ubicacion_gbd
  if (!is.null(u)) {
    u <- as.character(u)
    if (!u %in% locs)
      .dl_stop("%s no trae la ubicaci\u00f3n %s (ubicacion_gbd); trae: %s", de, u, .dl_lista(locs))
    return(u)
  }
  if (length(locs) == 1L) return(locs)
  .dl_stop("%s trae varias ubicaciones (%s): declara ubicacion_gbd, el location_id de GBD del pa\u00eds", de,
           .dl_lista(locs))
}

# Límites [edad_inicio, edad_fin) de los grupos de edad de GBD `ids` (de la descarga `de`, para el mensaje). Un id que
# no está en el catálogo es un error: no se convierte en una edad vacía.
.dl_edades_gbd <- function(ids, de) {
  g <- .dl_grupos_edad_referencia()
  k <- match(suppressWarnings(as.integer(ids)), g$age_group_id)
  if (anyNA(k))
    .dl_stop("%s trae grupos de edad que no est\u00e1n en el cat\u00e1logo de GBD: %s", de,
             .dl_lista(ifelse(is.na(ids[is.na(k)]), "(vac\u00edo)", ids[is.na(k)])))
  list(edad_inicio = g$age_start[k], edad_fin = g$age_end[k])
}

# Descarga de GBD Results -> ancla del contrato (sin validar).
.dl_leer_gbd_results <- function(d, opciones, de) {
  edad <- if ("age_id" %in% names(d)) d$age_id else d$age_group_id
  percent <- identical(opciones$metrica_prevalencia, "Percent")
  metrica <- ifelse(percent & d$measure_id == "5", "Percent", "Rate")
  ubicacion <- .dl_ubicacion_gbd(d$location_id, opciones, de)   # se comprueba aunque no quede ninguna fila
  d <- d[d$location_id == ubicacion & d$metric_name == metrica & d$measure_id %in% names(.DL_MEDIDA_GBD) &
           !edad %in% as.character(.DL_BANDAS_AGREGADAS), ]
  if (!nrow(d))
    .dl_stop(paste0("%s no trae filas de Rate (o de Percent, si se pidi\u00f3 as\u00ed la prevalencia) de prevalencia, ",
                    "mortalidad, AVD o incidencia en la ubicaci\u00f3n %s: \u00bfse descarg\u00f3 la m\u00e9trica Rate, por edades ",
                    "detalladas?"), de, ubicacion)
  escala <- ifelse(percent & d$measure_id == "5", 1, .DL_ESCALA_RATE)
  e <- .dl_edades_gbd(if ("age_id" %in% names(d)) d$age_id else d$age_group_id, de)
  data.table::data.table(causa = d$cause_id, nombre_causa = d$cause_name, anio = d$year, sexo = d$sex_id,
                         edad_inicio = e$edad_inicio, edad_fin = e$edad_fin,
                         medida = unname(.DL_MEDIDA_GBD[d$measure_id]),
                         valor = as.numeric(d$val) / escala, inferior = as.numeric(d$lower) / escala,
                         superior = as.numeric(d$upper) / escala)
}

# Descarga de covariables del GHDx -> covariables del contrato, filas nacionales (sin validar).
.dl_leer_ghdx_covariables <- function(d, opciones, de) {
  if ("location_id" %in% names(d)) d <- d[d$location_id == .dl_ubicacion_gbd(d$location_id, opciones, de), ]
  if (!"age_group_id" %in% names(d)) d$age_group_id <- as.character(.DL_BANDAS_AGREGADAS[["todas"]])
  agregada <- d$age_group_id %in% as.character(.DL_BANDAS_AGREGADAS)
  # de una misma covariable, año y sexo: la estandarizada (27) antes que todas las edades (22)
  prioridad <- match(d$age_group_id, as.character(c(.DL_BANDAS_AGREGADAS[["estandarizada"]],
                                                    .DL_BANDAS_AGREGADAS[["todas"]])))
  agr <- d[agregada, ][order(prioridad[agregada]), ]
  agr <- agr[!duplicated(paste(agr$covariate_name_short, agr$year_id, agr$sex_id)), ]
  d <- rbind(d[!agregada, ], agr)
  nacional <- d$age_group_id %in% as.character(.DL_BANDAS_AGREGADAS)
  out <- data.table::data.table(anio = d$year_id, sexo = d$sex_id, covariable = d$covariate_name_short,
                                valor = d$mean_value, inferior = d$lower_value, superior = d$upper_value,
                                covariable_id = d$covariate_id)
  if (any(!nacional)) {
    e <- .dl_edades_gbd(d$age_group_id[!nacional], de)
    out[, `:=`(edad_inicio = NA_real_, edad_fin = NA_real_)]
    out[!nacional, `:=`(edad_inicio = e$edad_inicio, edad_fin = e$edad_fin)]
  }
  out
}

# Lista de fuentes del GHDx -> fuentes_gbd del contrato (sin validar).
.dl_leer_ghdx_fuentes <- function(d, opciones, de) {
  d <- d[d$component_id %in% names(.DL_COMPONENTE_GHDX), ]
  data.table::data.table(causa = d$cause_id, ubicacion = d$location_id,
                         componente = unname(.DL_COMPONENTE_GHDX[d$component_id]), nid = d$nid)
}

# Las columnas numéricas de `d` como el texto más corto que vuelve al mismo double (.dl_num_exacto): así, al juntar la
# salida de un lector con tablas del contrato leídas como texto, rbindlist() no pasa el número por as.character() (15
# cifras) y el double no cambia.
.dl_numeros_a_texto <- function(d) {
  for (cn in names(d)) if (is.double(d[[cn]])) data.table::set(d, j = cn, value = .dl_num_exacto(d[[cn]]))
  d
}

# Lee `x` (ruta a un CSV, carpeta de CSV o data.frame) como la tabla `tabla` del contrato, sin validar: cada archivo
# (o el data.frame) pasa por su lector. Error si un archivo no se reconoce o si su lector produce otra tabla.
.dl_leer_fuente_tabla <- function(x, tabla, opciones = list()) {
  una <- function(d, de) {
    d <- data.table::as.data.table(d)
    data.table::setnames(d, tolower(trimws(names(d))))
    lector <- .dl_reconocer_lector(names(d))
    if (is.na(lector))
      .dl_stop(paste0("%s no se reconoce como tabla %s: trae las columnas %s; se espera una tabla del contrato ",
                      "(ver ?dl_tablas) o %s"), de, tabla, paste(names(d), collapse = ", "),
               paste(vapply(.DL_LECTORES, `[[`, "", "nombre"), collapse = ", "))
    if (lector == "contrato") return(d)
    if (.DL_LECTORES[[lector]]$tabla != tabla)
      .dl_stop("%s es una %s: va en la tabla %s, no en %s", de, .DL_LECTORES[[lector]]$nombre,
               .DL_LECTORES[[lector]]$tabla, tabla)
    for (cn in setdiff(names(d), .DL_COLUMNAS_NUMERICAS_GBD)) data.table::set(d, j = cn, value = as.character(d[[cn]]))
    .dl_numeros_a_texto(get(paste0(".dl_leer_", lector))(d, opciones, de))
  }
  if (is.data.frame(x)) return(una(x, "el data.frame"))
  archivos <- if (dir.exists(x)) list.files(x, "[.]csv$", ignore.case = TRUE, full.names = TRUE) else x
  if (!length(archivos)) .dl_stop("la carpeta \u00ab%s\u00bb no tiene archivos CSV", x)
  data.table::rbindlist(fill = TRUE, lapply(archivos, function(f) {
    d <- tryCatch(.dl_leer_memo(f, function(p) .dl_leer_csv(p, colClasses = "character", na.strings = "", tabla = tabla)),
                  dl_error = function(e) .dl_stop("no se pudo leer la tabla %s: %s", tabla, e$detalle))
    una(d, sprintf("\u00ab%s\u00bb", basename(f)))
  }))
}
