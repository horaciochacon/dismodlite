# dl_validar_estimaciones: comprueba las celdas de salida contra el contrato estimates/v1
# (inst/schema/estimates.v1.yaml) en memoria, antes de escribir nada en disco: columnas exactas y en orden, clave
# primaria única, intervalos ordenados, run_id, y nombres coherentes con los catálogos (medida, métrica, sexo y edad
# del catálogo demográfico; ubicación; causa por su slug).

# Slug de un nombre: minúsculas ASCII y «_» (por ejemplo «Arteriopatía crónica» -> «arteriopatia_cronica»).
# iconv(//TRANSLIT) no translitera igual en todos los sistemas: las vocales con tilde, la ü y la ñ se pasan antes a
# mano.
.dl_slugify <- function(x) {
  t <- gsub("['\u2019]", "", as.character(x))
  t <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
              "aeiouunAEIOUUN", t)
  t <- iconv(t, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  t <- tolower(t); t <- gsub("[^a-z0-9]+", "_", t); gsub("^_+|_+$", "", t)
}
# Nombre normalizado para comparar con el catálogo: minúsculas, sin espacios repetidos ni en los extremos.
.dl_norm_name <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))

.dl_schema_estimates <- function() .dl_leer_memo(.dl_inst_archivo("schema", "estimates.v1.yaml"), .dl_leer_yaml)

# Columnas de las celdas de una corrida, en orden: las del contrato estimates/v1 con los reemplazos e inserciones
# que declara su sección `mod`.
.dl_cols_estimates_mod <- function(sch, entity = "cause") {
  pre <- unlist(sch$columnas$prefijo)
  pre[pre == "acquisition_id"] <- sch$mod$reemplaza$acquisition_id
  i <- match("entity", pre)
  pre <- append(pre, unlist(sch$mod$inserta_tras_entity), after = i)
  c(pre, unlist(sch$columnas$demograficas), unlist(sch$entities[[entity]]$keys),
    unlist(sch$columnas$medida), unlist(sch$columnas$valores))
}

#' Validar las celdas de una corrida
#'
#' Comprueba las celdas de salida contra el contrato `estimates/v1` antes de escribirlas: columnas y orden, clave
#' única, intervalos ordenados, identificador de corrida y nombres coherentes con los catálogos.
#'
#' @details
#' Las comprobaciones: las columnas exactas y en su orden (las de las celdas de [dl_resumir()] con `run_id`
#' primero); una sola fila por ubicación, año, banda, sexo, causa, medida y métrica; sin `NA` en la clave ni en los
#' valores; `lower <= val <= upper`; `ui_level` 0.95; un solo `run_id`, con la forma `<AAAA-MM-DD>_<nombre>_v<n>`; y
#' los nombres de edad, sexo, medida, métrica, ubicación y causa iguales a los de los catálogos (sin distinguir
#' mayúsculas; la causa, por su forma sin tildes ni espacios). [dl_exportar_corrida()], [dl_sumar_hijas()],
#' [dl_reresumir_corrida()] y [dl_consolidar()] la usan antes de escribir.
#'
#' @param datos Tabla de celdas (data.table, con las columnas del contrato `estimates/v1`).
#' @param rutas Rutas de [dl_rutas()]; se usan los catálogos.
#' @param entidad Entidad del contrato (`"cause"`).
#' @return `TRUE`, invisible, si las celdas son válidas; si no, un error con la lista de fallas.
#' @seealso [dl_resumir()] (las celdas), [dl_exportar_corrida()].
#' @family avanzado
#' @examples
#' \donttest{
#' p <- dl_proyecto(dl_ejemplo(), causa = 9100)
#' b <- dl_insumos(p)
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' res <- dl_resumir(list(ajuste = f, avd = dl_avd(f, b, semilla = 1), insumos = b))
#' # las celdas con el identificador de una corrida
#' celdas <- data.table::data.table(run_id = paste0(Sys.Date(), "_ejemplo_v1"), res$celdas)
#' isTRUE(dl_validar_estimaciones(celdas, rutas = p$rutas))
#' # un intervalo que no contiene su valor no cumple el contrato
#' celdas$lower[1] <- celdas$val[1] * 2
#' try(dl_validar_estimaciones(celdas, rutas = p$rutas))
#' }
#' @export
dl_validar_estimaciones <- function(datos, rutas = dl_rutas(), entidad = "cause") {
  rutas <- .dl_resolver_rutas(rutas)
  if (!data.table::is.data.table(datos))
    .dl_stop("`datos` debe ser una data.table con las celdas de la corrida; es %s%s",
             if (is.data.frame(datos)) "un data.frame" else .dl_describir_objeto(datos),
             if (is.data.frame(datos)) " (convi\u00e9rtela con data.table::as.data.table())" else "")
  sch <- .dl_schema_estimates()
  rx_run_id <- sch$ids$run_id
  if (is.null(rx_run_id))
    .dl_stop("el contrato estimates/v1 no define ids.run_id; vuelve a instalar dismodlite")
  esperadas <- .dl_cols_estimates_mod(sch, entidad)
  if (!identical(names(datos), esperadas))
    .dl_stop("las columnas de las celdas no son las del contrato estimates/v1.\n  esperadas: %s\n  presentes: %s",
             paste(esperadas, collapse = ","), paste(names(datos), collapse = ","))
  fallas <- character(0)
  pk <- c("location_id", "year", "age_group_id", "sex_id", "cause_id", "measure_id", "metric_id")
  dup <- datos[, .N, by = pk][N > 1L]
  if (nrow(dup)) fallas <- c(fallas, sprintf("clave primaria duplicada en %d celda(s)", nrow(dup)))
  claves <- c("run_id", pk, "val", "lower", "upper")
  for (cn in claves) if (anyNA(datos[[cn]])) fallas <- c(fallas, sprintf("NA en la columna clave \u00ab%s\u00bb", cn))
  malas <- datos[!(lower <= val & val <= upper)]
  if (nrow(malas)) fallas <- c(fallas, sprintf("%d fila(s) no cumplen lower <= val <= upper", nrow(malas)))
  if (!all(datos$ui_level == sch$ui_level))
    fallas <- c(fallas, sprintf("ui_level debe ser %s en todas las filas", sch$ui_level))
  if (length(unique(datos$run_id)) != 1L || !grepl(rx_run_id, datos$run_id[1]))
    fallas <- c(fallas, paste0("run_id no \u00fanico o fuera del patr\u00f3n AAAA-MM-DD_<nombre>_v<n> (el `nombre` ",
                               "de la corrida solo admite min\u00fasculas sin tildes, n\u00fameros y guiones)"))
  demo <- .dl_catalogo(rutas, "demograficos")
  chk_demo <- function(tabla_, id_col, name_col) {
    m <- demo[tabla == tabla_]
    u <- unique(datos[, c(id_col, name_col), with = FALSE])
    nom <- m$name[match(u[[id_col]], m$id)]
    if (anyNA(nom)) return(sprintf("%s fuera del cat\u00e1logo demogr\u00e1fico: %s", id_col,
                                   paste(u[[id_col]][is.na(nom)], collapse = ", ")))
    if (any(.dl_norm_name(nom) != .dl_norm_name(u[[name_col]])))
      return(sprintf("%s no coincide con el cat\u00e1logo demogr\u00e1fico (sin distinguir may\u00fasculas)", name_col))
    NULL
  }
  fallas <- c(fallas, chk_demo("age_group", "age_group_id", "age_group_name"),
              chk_demo("sex", "sex_id", "sex_name"),
              chk_demo("measure", "measure_id", "measure_name"),
              chk_demo("metric", "metric_id", "metric_name"))
  locs <- .dl_catalogo(rutas, "locations")
  ul <- unique(datos[, list(location_id, location_name, location_level)])
  ml <- merge(ul, unique(locs[, list(location_id, location_name, location_level)]),
              by = "location_id", all.x = TRUE)
  if (anyNA(ml$location_name.y)) {
    fallas <- c(fallas, "location_id fuera del cat\u00e1logo de ubicaciones")
  } else if (any(.dl_norm_name(ml$location_name.x) != .dl_norm_name(ml$location_name.y)) ||
             any(ml$location_level.x != ml$location_level.y)) {
    fallas <- c(fallas, "location_name o location_level no coinciden con el cat\u00e1logo de ubicaciones")
  }
  causas <- .dl_catalogo(rutas, "causas")
  uc <- unique(datos[, list(cause_id, cause_name)])
  nomc <- causas$cause_name[match(uc$cause_id, causas$cause_id)]
  if (anyNA(nomc)) {
    fallas <- c(fallas, sprintf("cause_id fuera del cat\u00e1logo de causas: %s",
                                paste(uc$cause_id[is.na(nomc)], collapse = ", ")))
  } else if (any(.dl_slugify(uc$cause_name) != nomc)) {
    fallas <- c(fallas, "cause_name no coincide con el cat\u00e1logo de causas (comparado por su slug)")
  }
  if (length(fallas))
    .dl_stop("las celdas no cumplen el contrato estimates/v1:\n  - %s", paste(fallas, collapse = "\n  - "))
  invisible(TRUE)
}
