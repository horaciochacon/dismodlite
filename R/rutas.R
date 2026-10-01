# Rutas de los insumos. Cada pieza viene de un argumento de dl_rutas(), de `cambios` o de las variables de entorno
# de respaldo DATA_ROOT y CATALOGOS_DIR (las únicas que lee el paquete); la carpeta `registro` solo viene de los
# argumentos. Si una pieza hace falta y no está, el error la nombra. El objeto conserva las claves internas de la
# versión 0.2.2 (std_prior, ghdx_cov, ...): así lo leen las demás funciones, y los scripts que arman las rutas con
# dl_paths(list(std_prior = ...)) siguen funcionando.

# Argumento de dl_rutas() -> clave interna del objeto. Las claves de las piezas del ancla (std_*) son las de la
# columna path_key de .DL_MEDIDAS (R/esquema.R).
.DL_CLAVES_RUTAS <- c(
  ancla_prevalencia = "std_prior", ancla_mortalidad = "std_csmr", ancla_avd = "std_yld",
  ancla_incidencia = "std_incidence", covariables = "ghdx_cov", covariables_std = "cov_std",
  extraccion = "extraction", proxies = "cov_proxy", poblacion = "poblacion", datos = "datos",
  pesos_80mas = "pesos_80mas", severidad = "severidad", particion_severidad = "severity_split",
  evidencia = "ghdx_store", registro = "registry", catalogos = "catalogos")

# Rutas por defecto: las que se derivan de las variables de entorno de respaldo; el resto, NULL. Con DATA_ROOT, las
# estimaciones GBD se buscan en DATA_ROOT/std/gbd/2023/cause/<medida>, las de población en DATA_ROOT/std/inei y la
# evidencia en DATA_ROOT/ghdx.
.dl_rutas_base <- function() {
  droot <- Sys.getenv("DATA_ROOT")
  std <- stats::setNames(vector("list", nrow(.DL_MEDIDAS)), .DL_MEDIDAS$path_key)
  if (nzchar(droot))
    std[] <- as.list(file.path(droot, "std", .DL_STD_SOURCE, .DL_STD_ROUND, "cause", .DL_MEDIDAS$subdir_std))
  c(std, list(
    ghdx_cov    = NULL,                       # carpeta con los CSV de covariables (formato GHDx)
    cov_std     = if (nzchar(droot)) file.path(droot, "std", "ghdx", .DL_STD_ROUND, "covariate", "value") else NULL,
                                              # estimaciones GHDx de covariables: valor nacional de las que no tienen
                                              # CSV propio en ghdx_cov
    extraction  = NULL,                       # extraccion.yaml de la causa (covariables y betas)
    poblacion   = if (nzchar(droot)) file.path(droot, "std", "inei") else NULL,
                                              # CSV de población o carpeta raíz de sus particiones
                                              # (<ronda>/population/population/)
    datos       = NULL,                       # CSV con la tabla `datos` (NULL: tabla vacía)
    cov_proxy   = NULL,                       # CSV con la tabla `cov_proxy` (NULL: tabla vacía)
    pesos_80mas = NULL,                       # CSV age_group_id, sex_id, peso de las bandas finas de 80+
    severidad   = NULL,                       # CSV con la tabla `severidad`
    severity_split = NULL,                    # carpeta de la corrida de partición de severidad de la causa
    ghdx_store  = if (nzchar(droot)) file.path(droot, "ghdx") else NULL,
    registry    = NULL,                       # carpeta `registro` (master_gbd.csv, etiquetas_es.csv, ...)
    catalogos   = if (nzchar(Sys.getenv("CATALOGOS_DIR"))) Sys.getenv("CATALOGOS_DIR") else NULL))
}

# Traduce los nombres de `cambios` (argumentos de dl_rutas() o claves internas) a claves internas. Cada elemento
# lleva nombre y una clave desconocida es un error que sugiere la más parecida; `argumento` es como se nombra la lista
# en el mensaje (`cambios` de dl_rutas(), `...` de dl_rutas_ejemplo(), `rutas` de .dl_resolver_rutas()).
.dl_claves_internas <- function(cambios, argumento = "cambios") {
  if (!length(cambios)) return(cambios)
  nm <- names(cambios)
  if (is.null(nm) || any(is.na(nm) | !nzchar(nm)))
    .dl_stop("cada elemento de `%s` lleva nombre (una lista con nombres, p. ej. datos = \"datos.csv\")", argumento)
  es <- nm %in% names(.DL_CLAVES_RUTAS)
  nm[es] <- .DL_CLAVES_RUTAS[nm[es]]
  malas <- setdiff(nm, .DL_CLAVES_RUTAS)
  if (length(malas)) {
    cerca <- .dl_sugerir_clave(malas[1L], names(.DL_CLAVES_RUTAS))
    .dl_stop(paste0("clave(s) desconocida(s) en `%s`: %s%s. Claves v\u00e1lidas: los argumentos de dl_rutas() (%s) o ",
                    "las claves de la versi\u00f3n 0.2.2 (%s)"), argumento, paste(malas, collapse = ", "),
             if (!is.null(cerca)) sprintf(" (%s)", cerca) else "",
             paste(names(.DL_CLAVES_RUTAS), collapse = ", "), paste(unname(.DL_CLAVES_RUTAS), collapse = ", "))
  }
  names(cambios) <- nm
  cambios
}

# Cada pieza dada es la ruta de un archivo o de una carpeta: un texto (NULL la deja sin dar). Un error común es pasar
# la tabla ya leída en lugar de su ruta.
.dl_exigir_rutas_texto <- function(piezas) {
  for (k in seq_along(piezas)) {
    nm <- names(piezas)[k]; v <- piezas[[k]]
    if (is.null(v) || .dl_es_texto1(v)) next
    .dl_stop("`%s` debe ser la ruta de un archivo o de una carpeta (un texto), por ejemplo %s = \"%s\"; es %s",
             .dl_nombre_ruta(nm), .dl_nombre_ruta(nm),
             if (identical(nm, "datos")) "datos.csv" else "ruta/al/archivo.csv", .dl_describir_objeto(v))
  }
  invisible(piezas)
}

# «¿quisiste decir `<prefijo><clave>`?» con la clave de `validas` más parecida a `x` (distancia de edición <= 3) o, si
# `x` es uno de los nombres de `alias`, la clave que le corresponde; NULL si no hay ninguna.
.dl_sugerir_clave <- function(x, validas, prefijo = "", alias = NULL) {
  d <- if (length(validas)) utils::adist(x, validas, ignore.case = TRUE)[1L, ]
  cerca <- if (x %in% names(alias)) alias[[x]] else if (length(d) && min(d) <= 3) validas[which.min(d)]
  if (!is.null(cerca)) sprintf("\u00bfquisiste decir `%s%s`?", prefijo, cerca)
}

# Nombre de una clave interna como argumento de dl_rutas() (para los mensajes).
.dl_nombre_ruta <- function(clave) {
  k <- match(clave, .DL_CLAVES_RUTAS)
  if (is.na(k)) clave else names(.DL_CLAVES_RUTAS)[k]
}

#' Rutas de los archivos de entrada
#'
#' Reúne las rutas de los archivos que lee [dl_insumos()] en el formato completo: el ancla, las covariables, la
#' población, los datos locales, la severidad y los archivos de apoyo. Con un proyecto en el formato simple no hace
#' falta: [dl_proyecto()] arma las rutas (las de su traducción al formato completo) y [dl_insumos()] las toma del
#' proyecto.
#'
#' @details
#' Las piezas que no se dan quedan vacías o, si están definidas, se derivan de dos variables de entorno de respaldo,
#' las únicas que lee el paquete: `DATA_ROOT` (la carpeta raíz de los datos: el ancla en
#' `DATA_ROOT/std/gbd/2023/cause/<medida>`, el valor nacional de las covariables en
#' `DATA_ROOT/std/ghdx/2023/covariate/value`, la población en `DATA_ROOT/std/inei` y la evidencia en
#' `DATA_ROOT/ghdx`; también es la carpeta de salida por defecto de [dl_exportar_corrida()], [dl_sumar_hijas()] y
#' [dl_reresumir_corrida()]) y `CATALOGOS_DIR` (la carpeta de catálogos). Una pieza que hace falta y no está detiene
#' [dl_insumos()] con un error que la nombra.
#'
#' Los archivos de cada pieza y sus columnas siguen el contrato `dismod_lite/v1` (ver [dl_esquema()]); la sección
#' «Formato completo» de [dl_proyecto()] describe la carpeta de un proyecto en ese formato.
#'
#' @param ancla_prevalencia,ancla_mortalidad,ancla_avd,ancla_incidencia Archivo CSV (o carpeta de particiones) del
#'   ancla: prevalencia, mortalidad específica por causa, AVD e incidencia.
#' @param covariables Carpeta con los CSV de covariables nacionales (descargas del GHDx).
#' @param covariables_std Carpeta de la partición de covariables con el valor nacional (opcional).
#' @param extraccion Archivo YAML de extracción con las covariables y sus betas.
#' @param proxies CSV con los valores subnacionales de las covariables (tabla `cov_proxy`).
#' @param poblacion CSV de población (nacional y subnacional).
#' @param datos CSV con los datos locales (tabla `datos`).
#' @param pesos_80mas CSV con los pesos de población de las bandas de 80-84, 85-89, 90-94 y 95 años y más.
#' @param severidad CSV con la tabla de severidad de la causa.
#' @param particion_severidad Carpeta de la corrida de partición de severidad de la causa (ver
#'   [dl_severidad_desde_particion()]).
#' @param evidencia Carpeta con las fuentes de evidencia (`list.csv`).
#' @param registro Carpeta de tablas de referencia (`master_gbd.csv`, `etiquetas_es.csv`, `sequela_rei.csv`, ...);
#'   no es el registro de corridas.
#' @param catalogos Carpeta de catálogos (causas, ubicaciones, edades, estados de salud, secuelas).
#' @param cambios Lista con nombres que reemplaza piezas después de los argumentos. Acepta los nombres de los
#'   argumentos de esta función y las claves internas de la versión 0.2.2 (`std_prior`, `ghdx_cov`, ...); un
#'   elemento `NULL` quita la pieza.
#' @return Objeto de clase `dl_paths`: una lista con la ruta de cada pieza (o `NULL`) bajo su clave interna de la
#'   versión 0.2.2: `std_prior`, `std_csmr`, `std_yld` y `std_incidence` (el ancla), `ghdx_cov` (`covariables`),
#'   `cov_std` (`covariables_std`), `extraction` (`extraccion`), `cov_proxy` (`proxies`), `poblacion`, `datos`,
#'   `pesos_80mas`, `severidad`, `severity_split` (`particion_severidad`), `ghdx_store` (`evidencia`), `registry`
#'   (`registro`) y `catalogos`. `print()` la muestra con los nombres de los argumentos.
#' @seealso [dl_proyecto()] (las rutas de un proyecto, en cualquier formato), [dl_rutas_ejemplo()] (las del
#'   ejemplo) y [dl_insumos()] (el paso siguiente).
#' @family configuración
#' @examples
#' # las rutas del ejemplo en el formato completo, pieza por pieza
#' completo <- system.file("extdata", "acs_peru_completo", package = "dismodlite")
#' en <- function(...) file.path(completo, ...)
#' r <- dl_rutas(ancla_prevalencia = en("ancla", "prevalencia.csv"),
#'               ancla_mortalidad = en("ancla", "mortalidad.csv"),
#'               ancla_avd = en("ancla", "avd.csv"), ancla_incidencia = en("ancla", "incidencia.csv"),
#'               covariables = en("covariables"), extraccion = en("extraccion.yaml"),
#'               proxies = en("proxies_departamentales.csv"), poblacion = en("poblacion.csv"),
#'               datos = en("datos.csv"), pesos_80mas = en("pesos_80mas.csv"),
#'               severidad = en("severidad", "9100.csv"), evidencia = en("evidencia"),
#'               registro = en("registro"), catalogos = en("catalogos"))
#' r
#' # `cambios` acepta los nombres de los argumentos y las claves de la versión 0.2.2
#' dl_rutas(cambios = list(poblacion = en("poblacion.csv"),
#'                         std_prior = en("ancla", "prevalencia.csv")))
#' \donttest{
#' b <- dl_insumos(dl_configuracion(9100, en("config")), r)
#' b
#' }
#' @export
dl_rutas <- function(ancla_prevalencia = NULL, ancla_mortalidad = NULL, ancla_avd = NULL, ancla_incidencia = NULL,
                     covariables = NULL, covariables_std = NULL, extraccion = NULL, proxies = NULL, poblacion = NULL,
                     datos = NULL, pesos_80mas = NULL, severidad = NULL, particion_severidad = NULL, evidencia = NULL,
                     registro = NULL, catalogos = NULL, cambios = list()) {
  base <- .dl_rutas_base()
  dados <- mget(names(.DL_CLAVES_RUTAS), envir = environment())
  .dl_exigir_rutas_texto(dados)
  for (nm in names(dados)) if (!is.null(dados[[nm]])) base[[.DL_CLAVES_RUTAS[[nm]]]] <- dados[[nm]]
  if (is.data.frame(cambios) || (!is.null(cambios) && !is.list(cambios) && !is.character(cambios)))
    .dl_stop("`cambios` debe ser una lista con nombres (p. ej. list(datos = \"datos.csv\")); es %s",
             .dl_describir_objeto(cambios))
  cambios <- .dl_claves_internas(cambios)
  .dl_exigir_rutas_texto(cambios)
  # por posición: si una pieza aparece dos veces (con su argumento y con su clave interna), vale la última
  for (k in seq_along(cambios)) base[[names(cambios)[k]]] <- cambios[[k]]
  structure(base, class = "dl_paths")
}

#' @export
print.dl_paths <- function(x, ...) {
  cat("<dl_paths> rutas de los insumos (argumentos de dl_rutas())\n")
  claves <- names(x)
  nombres <- vapply(claves, .dl_nombre_ruta, character(1))
  ancho <- max(nchar(c(nombres, names(.DL_CLAVES_RUTAS))))
  orden <- c(names(.DL_CLAVES_RUTAS), setdiff(nombres, names(.DL_CLAVES_RUTAS)))
  for (nm in orden) {
    k <- match(nm, nombres)
    valor <- if (is.na(k) || is.null(x[[k]]) || !nzchar(x[[k]][1L])) "(no dada)" else paste(x[[k]], collapse = ", ")
    cat(sprintf("  %-*s  %s\n", ancho, nm, valor))
  }
  if (!is.null(attr(x, "anio_ejemplo"))) cat(sprintf("  a\u00f1o del ejemplo: %d\n", attr(x, "anio_ejemplo")))
  invisible(x)
}

# Aclaración de una pieza en los mensajes: la carpeta `registro` de dl_rutas() (tablas de referencia) no es el
# registro de corridas, el archivo YAML que reciben en su argumento `registro` las funciones que escriben corridas.
.DL_RUTAS_ACLARACION <- c(registry = paste0(" (la carpeta de tablas de referencia con master_gbd.csv; no es el ",
                                            "registro de corridas)"))

# Rutas que recibe una función: NULL -> las de los insumos (si se pasan y las traen) o las por defecto de dl_rutas();
# un objeto de dl_rutas() tal cual; una lista con nombres -> dl_rutas(cambios = lista), con sus claves (argumentos de
# dl_rutas() o claves de la versión 0.2.2) traducidas antes, para que un error las cite en `rutas`, el argumento que
# escribió el usuario. Otra cosa es un error.
.dl_resolver_rutas <- function(rutas, insumos = NULL) {
  if (is.null(rutas)) return(insumos$rutas %||% dl_rutas())
  if (inherits(rutas, "dl_paths")) return(rutas)
  if (!is.list(rutas) || is.data.frame(rutas) || any(names(.DL_DESCRIPCION_CLASES) %in% class(rutas)))
    .dl_stop("`rutas` debe venir de dl_rutas() o dl_rutas_ejemplo(), pero es %s", .dl_describir_objeto(rutas))
  dl_rutas(cambios = .dl_claves_internas(rutas, "rutas"))
}

# Ruta de una pieza (clave interna) de las rutas. Una pieza que no se dio es NULL si es `opcional`; si no, un error que
# la nombra con su argumento de dl_rutas(), dice para qué hace falta (`motivo`) y cómo pasarla. Una pieza que se dio y
# no existe es un error; la ruta por defecto que deriva DATA_ROOT (.dl_rutas_base()) no la dio el usuario, así que una
# pieza opcional con esa ruta, si no existe, cuenta como no dada.
.dl_path <- function(paths, pieza, opcional = FALSE, motivo = NULL) {
  p <- paths[[pieza]]
  arg <- .dl_nombre_ruta(pieza)
  if (opcional && !is.null(p) && !file.exists(p) && identical(p, .dl_rutas_base()[[pieza]])) p <- NULL
  if (is.null(p) || !nzchar(p)) {
    if (opcional) return(NULL)
    acl <- if (pieza %in% names(.DL_RUTAS_ACLARACION)) .DL_RUTAS_ACLARACION[[pieza]] else ""
    .dl_stop(paste0("falta la ruta de \u00ab%s\u00bb%s en `rutas`%s. P\u00e1sala con rutas = dl_rutas(%s = ...) o, ",
                    "con los datos de ejemplo, rutas = dl_rutas_ejemplo(causa)"), arg, acl,
             if (is.null(motivo)) "" else paste0(": ", motivo), arg)
  }
  if (!file.exists(p)) .dl_stop("la ruta de \u00ab%s\u00bb no existe: %s", arg, p)
  p
}
