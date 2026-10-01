# Un proyecto: dl_proyecto() lee la configuración de una causa y las tablas del contrato de insumos (R/contrato.R) de
# la carpeta del proyecto o de sus argumentos, y arma las rutas de sus insumos: su traducción (R/contrato_traduccion.R)
# con la estructura del formato completo, escrita en la carpeta temporal de la sesión, o los archivos de un proyecto
# del formato completo de la 0.2.2. Con esas rutas, lo que sigue no cambia.

# ---- Archivos de configuración ----

# Una configuración es del formato completo si trae `schema` (dismod_lite/v1); si no, es simple.
.dl_es_config_completa <- function(y) is.list(y) && !is.null(y[["schema"]])

# Lee un archivo de configuración: uno simple con solo true y false como lógicos; uno completo, como en la 0.2.2.
.dl_leer_config <- function(path) {
  y <- .dl_leer_yaml(path, handlers = .DL_YAML_LOGICOS)
  if (.dl_es_config_completa(y)) .dl_leer_yaml(path) else y
}

# Archivo de configuración de `causa`: la ruta misma si es un archivo, o en la carpeta `carpeta` <causa>.yaml,
# config.yaml (un proyecto simple de una sola causa) o config/<causa>.yaml (la carpeta del proyecto).
.dl_archivo_config <- function(carpeta, causa) {
  if (file.exists(carpeta) && !dir.exists(carpeta)) return(carpeta)
  f <- c(file.path(carpeta, sprintf("%d.yaml", causa)), file.path(carpeta, "config.yaml"),
         file.path(carpeta, "config", sprintf("%d.yaml", causa)))
  f <- f[file.exists(f)]
  if (!length(f))
    .dl_stop(paste0("no hay configuraci\u00f3n de la causa %d en \u00ab%s\u00bb: se busca %d.yaml, config.yaml o ",
                    "config/%d.yaml"), causa, carpeta, causa, causa)
  f[1L]
}

# Carpeta del proyecto de un archivo de configuración: la suya o, si está en config/, la de arriba.
.dl_raiz_proyecto <- function(archivo) {
  d <- dirname(archivo)
  if (identical(basename(d), "config")) dirname(d) else d
}

# Las configuraciones del proyecto `carpeta` (config.yaml o config/<causa>.yaml), una fila por archivo: causa (la del
# nombre del archivo o la clave `causa` de config.yaml; NA si no se puede saber), archivo, error de lectura (NA si se
# leyó), si es simple, nombre, subtipos y covariables (vacíos en el formato completo).
.dl_configs_proyecto <- function(carpeta) {
  unica <- file.path(carpeta, "config.yaml")
  archivos <- if (file.exists(unica)) unica
              else list.files(file.path(carpeta, "config"), pattern = "^[0-9]+[.]yaml$", full.names = TRUE)
  filas <- lapply(archivos, function(f) {
    error <- NA_character_
    y <- tryCatch(.dl_leer_config(f), dl_error = function(e) { error <<- .dl_detalle(e); NULL })
    if (!is.null(y) && (!is.list(y) || is.null(names(y)))) {
      error <- sprintf("%s no es una lista de claves (clave: valor, una por l\u00ednea)", basename(f))
      y <- NULL
    }
    simple <- is.list(y) && !.dl_es_config_completa(y)
    v <- y[["causa"]] %||% y[["cause_id"]]
    causa <- if (f != unica) as.integer(sub("[.]yaml$", "", basename(f)))
             else if (is.atomic(v) && length(v)) suppressWarnings(as.integer(v[1L])) else NA_integer_
    data.table::data.table(causa = causa, archivo = f, error = error, simple = simple,
                           nombre = if (simple && .dl_es_texto1(y[["nombre"]])) y[["nombre"]] else NA_character_,
                           subtipos = list(if (simple) suppressWarnings(as.integer(unlist(y[["subtipos"]])))
                                           else integer()),
                           covariables = list(if (simple) y[["covariables"]]))
  })
  vacia <- data.table::data.table(causa = integer(), archivo = character(), error = character(), simple = logical(),
                                  nombre = character(), subtipos = list(), covariables = list())
  data.table::rbindlist(c(list(vacia), filas))
}

# Las filas de `cf` (.dl_configs_proyecto() de `carpeta`) que se leen para `causa`: la suya o, sin `causa`, la única
# (con `varias`, todas). Un error si no hay configuración, si config.yaml no se puede leer o no dice su causa y no se
# pide una, si hay varias causas y no se dice cuál o si no está la pedida.
.dl_elegir_configs <- function(cf, causa, carpeta, varias = FALSE) {
  if (!nrow(cf))
    .dl_stop(paste0("\u00ab%s\u00bb no es la carpeta de un proyecto: le falta config.yaml o config/<causa>.yaml ",
                    "(un proyecto nuevo se crea con dl_nuevo_proyecto())"), basename(carpeta))
  if (nrow(cf) == 1L && is.na(cf$causa)) {                 # config.yaml sin `causa` (o que no se puede leer)
    if (!is.na(cf$error)) .dl_stop(cf$error)
    if (is.null(causa)) .dl_stop("%s no declara `causa` (el identificador de la causa)", basename(cf$archivo))
    data.table::set(cf, j = "causa", value = causa)
  }
  if (is.null(causa) && nrow(cf) > 1L && !varias)
    .dl_stop("el proyecto tiene varias causas (%s): indica cu\u00e1l con `causa`", paste(cf$causa, collapse = ", "))
  if (is.null(causa)) return(cf)
  k <- cf$causa == causa
  if (!any(k))
    .dl_stop("el proyecto no tiene la configuraci\u00f3n de la causa %d (tiene: %s)", causa,
             paste(cf$causa, collapse = ", "))
  cf[k]
}

# ---- Las tablas del proyecto ----

# Las tablas que todo proyecto trae (ver ?dl_tablas); las demás son opcionales.
.DL_TABLAS_OBLIGATORIAS <- c("ubicaciones", "poblacion", "ancla")

# Dónde está la tabla `tabla` del proyecto: el argumento `dadas[[tabla]]` (un data.frame o una ruta); si no,
# carpeta/<tabla>.csv; si no, carpeta/<tabla>/. NULL si no está o no tiene filas (un CSV con solo el encabezado, una
# carpeta sin CSV con filas, un data.frame vacío): una tabla así es como si no estuviera.
.dl_fuente_tabla <- function(carpeta, dadas, tabla) {
  x <- dadas[[tabla]]
  if (is.data.frame(x)) return(if (nrow(x)) x)
  if (!is.null(x)) {
    if (!.dl_es_texto1(x) || !file.exists(x))
      .dl_stop("`%s` debe ser un data.frame o la ruta de un CSV o de una carpeta que exista; es %s", tabla,
               if (.dl_es_texto1(x)) sprintf("\u00ab%s\u00bb, que no existe", x) else .dl_describir_objeto(x))
    return(if (.dl_hay_filas(x)) x)
  }
  if (is.null(carpeta)) return(NULL)
  f <- file.path(carpeta, c(paste0(tabla, ".csv"), tabla))
  f <- f[file.exists(f)]
  f <- f[vapply(f, .dl_hay_filas, NA)]
  if (length(f)) f[[1L]]
}

# Dónde se busca la tabla `tabla` (para el mensaje de una tabla obligatoria que falta).
.dl_donde_tabla <- function(tabla, carpeta)
  paste(c(sprintf("el argumento `%s`", tabla),
          if (!is.null(carpeta)) sprintf("\u00ab%s\u00bb", file.path(carpeta, paste0(tabla, c(".csv", "/"))))),
        collapse = ", ")

# Las tablas del contrato del proyecto, una lista nombrada de dl_tabla sin las que no están: cada una de `dadas` o de
# `carpeta` (.dl_fuente_tabla), por su lector (`opciones`: ubicacion_gbd y metrica_prevalencia, ver
# .dl_leer_fuente_tabla) y validada sola. Sin ubicacion_gbd, el código de la ubicación nacional de ubicaciones si es
# un entero (el location_id de GBD del país). Cada tabla va por `paso(nombre, expr)`, como en .dl_traducir_contrato.
# Error si falta una tabla obligatoria, con dónde se buscó.
.dl_tablas_proyecto <- function(carpeta, dadas, opciones = list(), paso = function(nombre, expr) expr) {
  fuentes <- lapply(stats::setNames(nm = .DL_TABLAS), function(t) .dl_fuente_tabla(carpeta, dadas, t))
  faltan <- .DL_TABLAS_OBLIGATORIAS[vapply(fuentes[.DL_TABLAS_OBLIGATORIAS], is.null, NA)]
  if (length(faltan))
    .dl_stop("faltan tablas obligatorias del proyecto (o solo traen el encabezado):\n%s",
             paste(sprintf("  - %s: se busc\u00f3 en %s", faltan,
                           vapply(faltan, .dl_donde_tabla, "", carpeta = carpeta)), collapse = "\n"))
  leer <- function(t) paso(t, .dl_tabla_proyecto(fuentes[[t]], t, opciones))
  tablas <- list(ubicaciones = leer("ubicaciones"))
  if (is.null(opciones$ubicacion_gbd) && !is.null(tablas$ubicaciones))
    opciones$ubicacion_gbd <- .dl_codigo_gbd(tablas$ubicaciones)
  for (t in setdiff(names(Filter(Negate(is.null), fuentes)), "ubicaciones")) tablas[t] <- list(leer(t))
  Filter(Negate(is.null), tablas)
}

# La tabla `tabla` del contrato desde `x` (data.frame o ruta): su lector y la validación de una tabla sola, como
# dl_tabla().
.dl_tabla_proyecto <- function(x, tabla, opciones) {
  origen <- if (is.data.frame(x)) sprintf("argumento `%s` (data.frame)", tabla) else x
  .dl_tabla_contrato(.dl_leer_fuente_tabla(x, tabla, opciones), tabla, origen)
}

# location_id de GBD del país según la tabla ubicaciones: el código de la ubicación nacional si es un entero; si no,
# NULL (una descarga de GBD con varias ubicaciones pide entonces ubicacion_gbd).
.dl_codigo_gbd <- function(u) {
  nacional <- .dl_ubicacion_nacional(list(ubicaciones = u))
  if (!is.na(nacional) && grepl("^[0-9]+$", nacional)) as.integer(nacional)
}

# Opciones de los lectores desde la configuración `s` del proyecto: ubicacion_gbd y la métrica de la prevalencia de
# las descargas de GBD Results (Rate; Percent si la configuración pide anchor.metrica_prevalencia en avanzado).
.dl_opciones_lectores <- function(s)
  list(ubicacion_gbd = s[["ubicacion_gbd"]],
       metrica_prevalencia = .dl_valor_en(s, "avanzado.anchor.metrica_prevalencia.valor") %||% "Rate")

# La tabla severidad desde la partición que declara la configuración `s` (severidad.particion, relativa a `base`, la
# carpeta del proyecto), con la causa padre (severidad.padre) y las secuelas del componente (componente.secuelas).
.dl_severidad_particion <- function(s, causa, base) {
  corrida <- file.path(base, .dl_valor_en(s, "severidad.particion"))
  secuelas <- unlist(.dl_valor_en(s, "componente.secuelas"))
  padre <- .dl_valor_en(s, "severidad.padre")
  sev <- .dl_severidad_contrato_desde_particion(corrida, causa, padre = if (!is.null(padre)) as.integer(padre),
                                                secuelas = if (length(secuelas)) as.integer(secuelas))
  .dl_tabla_contrato(sev, "severidad", corrida)
}

# ---- La configuración de un proyecto ----

# Las causas con configuración del proyecto (causa, nombre, subtipos): las de `carpeta` o, sin carpeta, la de la
# configuración `s` de `causa`. Dan la causa padre de un subtipo y los nombres del catálogo de causas.
.dl_causas_config <- function(carpeta, s, causa) {
  if (!is.null(carpeta)) return(.dl_configs_proyecto(carpeta)[!is.na(causa) & simple, list(causa, nombre, subtipos)])
  data.table::data.table(causa = causa, nombre = if (.dl_es_texto1(s[["nombre"]])) s[["nombre"]] else NA_character_,
                         subtipos = list(suppressWarnings(as.integer(unlist(s[["subtipos"]])))))
}

# La causa padre de `causa`: la que la declara en `subtipos` (`causas`, .dl_causas_config), o NULL.
.dl_padre_de <- function(causas, causa) {
  k <- which(vapply(causas$subtipos, function(h) causa %in% h, NA))[1L]
  if (!is.na(k)) causas$causa[[k]]
}

# Lo que la traducción de la configuración de `causa` toma de las tablas (.dl_traducir_config_simple): la ubicación
# nacional (la que no tiene padre en ubicaciones), las betas de la causa (las de `padre` si es un subtipo sin betas
# propias), las covariables con filas subnacionales, si hay ubicaciones subnacionales, el nombre de la causa en el
# ancla, la causa padre y el covariate_id de cada covariable (para valor_nacional_de).
.dl_contexto_tablas <- function(tablas, causa, padre = NULL) {
  nacional <- .dl_ubicacion_nacional(tablas)
  betas <- .dl_betas_de_causa(tablas$betas, causa, padre)
  if (!is.null(betas) && !nrow(betas)) betas <- NULL
  cov <- tablas$covariables
  nombres <- stats::na.omit(.dl_col(.dl_filas_de_causa(tablas$ancla, causa), "nombre_causa", NA_character_))
  list(ubicacion = nacional, padre = padre, betas = betas,
       covariables_subnacionales = if (is.null(cov)) character()
                                   else unique(cov$covariable[!.dl_es_nacional(cov, nacional)]),
       subnacional = any(!is.na(.dl_col(tablas$ubicaciones, "padre", NA_character_))),
       nombre = if (length(nombres)) nombres[[1L]], ids_covariable = .dl_ids_covariable(tablas, betas))
}

# La configuración validada y las tablas del proyecto de la causa `causa`: las tablas (de `dadas` o de `carpeta`), la
# severidad de la partición si la configuración la declara, el contexto que la configuración toma de ellas y su
# traducción (.dl_config_simple, con `cambios`). `base`: la carpeta de las rutas relativas de la configuración.
.dl_preparar_contrato <- function(s, archivo, causa, carpeta, dadas = list(), cambios = NULL,
                                  base = carpeta %||% .dl_raiz_proyecto(archivo)) {
  if (!is.list(s) || is.null(names(s)))
    .dl_stop_config_simple(archivo, "el archivo no es una lista de claves (clave: valor, una por l\u00ednea)")
  causas <- .dl_causas_config(carpeta, s, causa)
  tablas <- .dl_tablas_proyecto(carpeta, dadas, .dl_opciones_lectores(s))
  if (!is.null(.dl_valor_en(s, "severidad.particion"))) tablas$severidad <- .dl_severidad_particion(s, causa, base)
  ctx <- .dl_contexto_tablas(tablas, causa, .dl_padre_de(causas, causa))
  list(cfg = .dl_config_simple(s, archivo, causa, ctx, cambios), tablas = tablas, causas = causas)
}

# ---- La traducción de un proyecto ----

# Dónde va cada pieza en un proyecto del formato completo (como inst/extdata/acs_peru_completo): clave -> ruta.
.dl_piezas_completo <- function(causa) c(
  stats::setNames(as.list(sprintf("ancla/%s.csv", tolower(.DL_MEDIDAS$nombre_es))), .DL_MEDIDAS$path_key),
  list(ghdx_cov = "covariables", extraction = "extraccion.yaml", poblacion = "poblacion.csv",
       pesos_80mas = "pesos_80mas.csv", severidad = sprintf("severidad/%d.csv", causa), ghdx_store = "evidencia",
       catalogos = "catalogos", datos = "datos.csv", cov_proxy = "proxies_departamentales.csv", registry = "registro"))

# Nombres que no están en las tablas: las causas con configuración (`cf`) y sus hijas, con el nombre de cada causa en
# el ancla, para el catálogo de causas, el registro de subtipos y las etiquetas.
.dl_causas_proyecto <- function(cf, ancla) {
  d <- cf[, list(cause_id = causa, nombre, hijos = vapply(subtipos, paste, "", collapse = "|"))]
  hijas <- unlist(cf$subtipos)
  d <- rbind(d, data.table::data.table(cause_id = setdiff(hijas, d$cause_id), nombre = NA_character_, hijos = ""))
  en_ancla <- unique(ancla[, list(cause_id = as.integer(cause_id), cause_name)])
  d[, nombre_ancla := en_ancla$cause_name[match(cause_id, en_ancla$cause_id)]]
  d[is.na(nombre), nombre := data.table::fifelse(is.na(nombre_ancla), as.character(cause_id), nombre_ancla)]
  d[is.na(nombre_ancla), nombre_ancla := nombre][]
}

# Escribe en `destino` la traducción `x` (.dl_traducir_contrato) del proyecto para `cfg`: las tablas
# (.dl_escribir_tablas_traduccion), los catálogos y el registro (.dl_escribir_catalogos_traduccion, con las causas
# `causas`) y, al final, el archivo `listo`.
.dl_escribir_traduccion <- function(cfg, destino, x, causas) {
  unlink(destino, recursive = TRUE)
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  escribir <- function(d, ...) if (!is.null(d)) {
    f <- file.path(destino, ...)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(d, f, eol = "\n", na = "")
  }
  .dl_escribir_tablas_traduccion(cfg, destino, x, escribir)
  .dl_escribir_catalogos_traduccion(cfg, destino, x, causas, escribir)
  writeLines("", file.path(destino, "listo"))
}

# Las tablas de la traducción, pieza por pieza (.dl_piezas_completo; `escribir(d, ...)`, la de un CSV): el ancla por
# medida, la población, los pesos de 80+ (solo con anchor.agrupar_bandas_finas), la extracción, los valores nacionales
# de las covariables (covariables/ghdx.csv, en el formato de la descarga del GHDx), los proxies, los datos, la severidad
# y las fuentes de GBD (evidencia/list.csv). Lo que el proyecto no trae no se escribe.
.dl_escribir_tablas_traduccion <- function(cfg, destino, x, escribir) {
  pieza <- .dl_piezas_completo(cfg$cause_id)
  for (k in seq_len(nrow(.DL_MEDIDAS))) {
    m <- x$ancla[measure_id == as.character(.DL_MEDIDAS$measure_id_gbd[k])]
    if (nrow(m)) escribir(m, pieza[[.DL_MEDIDAS$path_key[k]]])
  }
  escribir(x$poblacion$tabla, pieza$poblacion)
  escribir(x$pesos_80, pieza$pesos_80mas)
  if (!is.null(x$extraccion))
    yaml::write_yaml(x$extraccion, file.path(destino, pieza$extraction), fileEncoding = "UTF-8")
  escribir(x$cov$nacional, pieza$ghdx_cov, "ghdx.csv")
  escribir(x$proxies, pieza$cov_proxy)
  escribir(x$datos, pieza$datos)
  escribir(x$severidad, pieza$severidad)
  escribir(x$fuentes, pieza$ghdx_store, "list.csv")
}

# Los catálogos y el registro de la traducción: las edades de GBD (inst/referencia) más las bandas del proyecto que no
# son de GBD (x$bandas, con su id sintético), las ubicaciones de la población, las causas (`causas`) con sus subtipos,
# las etiquetas en español (las del paquete, las de las causas y las de esas bandas) y un registro de secuelas vacío.
.dl_escribir_catalogos_traduccion <- function(cfg, destino, x, causas, escribir) {
  pieza <- .dl_piezas_completo(cfg$cause_id)
  cat_arch <- .dl_schema_estimates()$catalogos
  edades <- file.path(destino, pieza$catalogos, cat_arch$demograficos)
  dir.create(dirname(edades), recursive = TRUE, showWarnings = FALSE)
  file.copy(.dl_inst_archivo("referencia", "catalogo_demograficos_gbd2023.csv"), edades)
  b <- x$bandas
  if (nrow(b))
    data.table::fwrite(b[, list(tabla = "age_group", id = age_group_id, name = nombre, slug = .dl_slugify(nombre),
                                age_start = edad_inicio, age_end = edad_fin)], edades, append = TRUE, eol = "\n")
  nacional <- .dl_loc_ancla(cfg)
  locs <- unique(x$poblacion$tabla[, list(location_id, location_level)])
  locs[, `:=`(location_name = x$nombres_loc$location_name[match(location_id, x$nombres_loc$location_id)],
              parent_id = data.table::fifelse(location_level == 0L, NA_character_, nacional))]
  locs[is.na(location_name), location_name := location_id]
  escribir(locs[, list(location_id, location_name, location_level, parent_id)], pieza$catalogos, cat_arch$locations)
  causas <- .dl_causas_proyecto(causas, x$ancla)
  escribir(causas[, list(cause_id, cause_name = .dl_slugify(nombre_ancla))], pieza$catalogos, cat_arch$causas)
  escribir(causas[nzchar(hijos), list(cause_id, nombre_es = nombre, hijos)], pieza$registry, "master_gbd.csv")
  escribir(rbind(.dl_etiquetas_ref(),
                 causas[, list(tabla = "cause", id = as.character(cause_id), name = nombre_ancla, name_es = nombre,
                               slug = .dl_slugify(nombre_ancla), slug_es = .dl_slugify(nombre))],
                 b[, list(tabla = "age_group", id = as.character(age_group_id), name = nombre, name_es = nombre,
                          slug = "", slug_es = "")]), pieza$registry, "etiquetas_es.csv")
  writeLines("sequela_id,cause_id,sequela_name,health_state_id,rol",
             file.path(destino, pieza$registry, "sequela_rei.csv"))
}

# Rutas del proyecto traducido para `cfg` (`tablas`: las del contrato; `causas`: .dl_causas_config), en la carpeta
# temporal <proyecto>_<causa>_<clave>. <clave> resume lo que se traduce: la versión del paquete, la configuración
# traducida, las causas y el contenido de las tablas, sin su origen (las dos puertas comparten la traducción). `donde`
# identifica el proyecto: su carpeta o, sin ella, la configuración. Una traducción nueva borra las anteriores del
# mismo proyecto y causa, y sus lecturas memorizadas.
.dl_traducir_proyecto <- function(donde, cfg, tablas, causas) {
  traducida <- cfg
  traducida$origen[c("archivo", "betas")] <- NULL   # la ruta de la configuración; las betas van en las tablas
  contenido <- lapply(tablas, function(t) digest::digest(lapply(t, identity)))   # sin sus atributos
  clave <- digest::digest(list(dl_version(), traducida, causas, contenido), algo = "xxhash64")
  prefijo <- sprintf("%s_%d_", digest::digest(normalizePath(donde, winslash = "/", mustWork = FALSE),
                                              algo = "xxhash64"), cfg$cause_id)
  raiz <- file.path(tempdir(), "dismodlite_proyectos")
  destino <- file.path(raiz, paste0(prefijo, clave))
  if (!file.exists(file.path(destino, "listo"))) {
    for (v in list.files(raiz, paste0("^", prefijo), full.names = TRUE)) {
      k <- ls(.dl_memo_env)
      rm(list = k[startsWith(k, normalizePath(v, winslash = "/"))], envir = .dl_memo_env)
      unlink(v, recursive = TRUE)
    }
    .dl_escribir_traduccion(cfg, destino, .dl_traducir_contrato(tablas, cfg), causas)
  }
  .dl_rutas_con_codigos(.dl_rutas_completo(destino, cfg$cause_id), cfg)
}

# Rutas de un proyecto en el formato completo: cada pieza de .dl_piezas_completo() que existe (las demás quedan vacías;
# tampoco se toman de las variables de entorno) y la partición de severidad, la única carpeta de particion/.
.dl_rutas_completo <- function(carpeta, causa) {
  piezas <- .dl_piezas_completo(causa)
  f <- stats::setNames(file.path(carpeta, unlist(piezas)), names(piezas))
  corridas <- list.dirs(file.path(carpeta, "particion"), recursive = FALSE)
  p <- c(as.list(f[file.exists(f)]), if (length(corridas) == 1L) list(severity_split = corridas))
  r <- dl_rutas(cambios = p)
  for (k in setdiff(names(r), names(p))) r[k] <- list(NULL)
  r
}


# ---- dl_proyecto() ----

#' Proyecto de una causa
#'
#' Lee un proyecto y devuelve, para una causa, su configuración, sus tablas del contrato de insumos y las rutas de sus
#' insumos: todo lo que [dl_insumos()] necesita (`dl_insumos(dl_proyecto(carpeta))`). Es la forma recomendada de
#' empezar.
#'
#' Un proyecto es una configuración corta (sus claves en [dl_configuracion()]) y las tablas del contrato de insumos
#' (ver [dl_tablas]). Hay dos puertas, en una sola función: las tablas se leen de la carpeta del proyecto por su nombre,
#' o se pasan como argumentos (un `data.frame` o la ruta de un CSV o de una carpeta), y las dos se pueden mezclar: lo
#' que se pasa reemplaza a lo de la carpeta.
#' ```r
#' p <- dl_proyecto("mi_proyecto")                                  # todo desde la carpeta
#' p <- dl_proyecto("mi_proyecto", poblacion = mi_pob)              # la carpeta + una tabla de R
#' p <- dl_proyecto(configuracion = "config.yaml", ubicaciones = ubic,
#'                  poblacion = mi_pob, ancla = "descargas/gbd_2023.csv")   # sin carpeta
#' ```
#'
#' Cada tabla pasa por su lector (las descargas de GBD Results y del GHDx se reconocen por sus columnas y se convierten
#' solas) y se valida sola; un problema de una tabla detiene `dl_proyecto()` con la tabla, la columna y las filas. Las
#' reglas que cruzan tablas (la población nacional suma las subnacionales, los proxies cierran en el valor nacional, los
#' datos tienen valores posibles) las comprueba [dl_insumos()].
#'
#' El proyecto se traduce al formato completo de la versión 0.2.2, el que usa el resto del paquete: la configuración y
#' las tablas a una carpeta temporal de la sesión, que se reutiliza mientras no cambien. Los números leídos pasan tal
#' cual; solo se calcula lo que sale de la población (el total nacional, si no viene) y la agrupación del ancla en las
#' bandas de la población (con `poblacion_detalle`, si el ancla es más fina).
#'
#' `print()` muestra la causa, el año, las tablas que encontró, el modo subnacional y las claves de la configuración
#' que tomaron su valor por defecto.
#'
#' @section La carpeta del proyecto:
#' ```
#' mi_proyecto/
#'   config.yaml          # una causa; o config/<causa>.yaml, una por causa
#'   ubicaciones.csv
#'   poblacion.csv
#'   ancla/               # cualquier CSV: descargas de GBD tal cual o tablas del contrato
#'   covariables/         # descargas del GHDx y/o la tabla del contrato con los proxies
#'   betas.csv
#'   datos.csv
#'   severidad.csv
#'   fuentes_gbd/
#'   poblacion_detalle.csv
#' ```
#' Cada tabla es `<tabla>.csv` o una carpeta `<tabla>/` cuyos CSV se juntan (cada uno pasa por su lector). Las rutas
#' no se declaran. Una tabla opcional ausente, o con solo el encabezado, no existe (así quedan las plantillas de
#' [dl_nuevo_proyecto()]). Las obligatorias son `ubicaciones`, `poblacion` y `ancla`: con ellas se estima la causa en
#' el país y, si hay ubicaciones subnacionales, en cada una con las tasas nacionales. [dl_correr()], que calcula los
#' años vividos con discapacidad (AVD), necesita también `severidad` (o `severidad.particion` en la configuración).
#' `covariables` y `betas` agregan las diferencias entre ubicaciones subnacionales y `datos`, los datos locales. Las
#' columnas de cada tabla, sus unidades y sus valores están en [dl_tablas].
#'
#' Con varias causas, cada una tiene su configuración en `config/<causa>.yaml` y comparten las tablas, que traen las
#' filas de todas en la columna `causa` (sin ella, una fila vale para todas). Una causa que es la suma de otras las
#' declara en `subtipos`; un subtipo sin betas propias usa las de su causa padre. Cada subtipo se corre por separado y
#' sus corridas se suman con [dl_sumar_hijas()], con las rutas del proyecto de la causa padre:
#' ```r
#' corridas <- vapply(c(1011, 1012), function(k) dl_correr(carpeta, k, semilla = 1)$dir, "")
#' dl_sumar_hijas(corridas, causa = 1010, nombre = "suma", carpeta = file.path(carpeta, "resultados"),
#'                rutas = dl_proyecto(carpeta, 1010)$rutas,
#'                omitidas = list(list(cause_id = 1013, motivo = "sin datos suficientes")))
#' ```
#'
#' @section Formato completo:
#' Un proyecto es del formato completo de la versión 0.2.2 si su configuración trae `schema: dismod_lite/v1`. Tiene
#' `config/<causa>.yaml`, `ancla/` con un CSV por medida (`prevalencia.csv`, `mortalidad.csv`, `incidencia.csv` y
#' `avd.csv`, en el formato de las estimaciones), `covariables/`, `extraccion.yaml` (las betas), `poblacion.csv`,
#' `pesos_80mas.csv`, `datos.csv`, `proxies_departamentales.csv`, `severidad/<causa>.csv`, `evidencia/`, `registro/`,
#' `catalogos/` y, si hay una, la partición de severidad en `particion/<corrida>/`. Cada tabla sigue el contrato
#' `dismod_lite/v1` (ver [dl_esquema()]); no recibe tablas en `...`. El ejemplo en este formato:
#' `system.file("extdata", "acs_peru_completo", package = "dismodlite")`.
#'
#' @param carpeta Carpeta del proyecto: la que tiene `config.yaml` o `config/` (opcional si se dan `configuracion` y
#'   las tablas).
#' @param causa Causa (`cause_id`, un entero); `NULL` si el proyecto tiene una sola o la configuración la declara.
#' @param ... Tablas del contrato por su nombre (ver [dl_tablas]: `ubicaciones`, `poblacion`, `ancla`, `covariables`,
#'   `betas`, `datos`, `severidad`, `fuentes_gbd`, `poblacion_detalle`): un `data.frame` o la ruta de un CSV o de una
#'   carpeta. Reemplazan a las de la carpeta.
#' @param configuracion Ruta del YAML de la configuración o una lista con sus claves (opcional; por defecto, la de la
#'   carpeta).
#' @return Objeto de clase `dl_proyecto`: una lista con
#'   - `configuracion`: la configuración de la causa, de clase `dl_config`, como la de [dl_configuracion()]. Trae
#'     `origen$por_defecto`: las claves tomadas por defecto, con su valor.
#'   - `rutas`: las rutas de los insumos, de clase `dl_paths`, como las de [dl_rutas()]. Apuntan a la traducción, en
#'     la carpeta temporal de la sesión: en otra sesión, o después de cambiar la configuración o una tabla (la
#'     traducción anterior se borra), vuelve a llamar a `dl_proyecto()`.
#'   - `carpeta`: la carpeta del proyecto (`NULL` sin carpeta).
#'   - `formato`: `"simple"` (un proyecto con las tablas del contrato) o `"completo"` (el formato de la 0.2.2).
#'   - `tablas`: las tablas del contrato, una lista nombrada de [dl_tabla()] (`NULL` en el formato completo).
#' @seealso [dl_tablas] (las tablas), [dl_configuracion()] (las claves de la configuración), [dl_insumos()] (el paso
#'   siguiente), [dl_ejemplo()] (el proyecto de ejemplo), [dl_nuevo_proyecto()] (crear la carpeta de un proyecto),
#'   [dl_revisar_proyecto()] (revisarla antes de correr) y [dl_correr()] (la corrida completa en una llamada).
#' @family proyecto
#' @examples
#' # el proyecto de ejemplo
#' p <- dl_proyecto(dl_ejemplo(), causa = 9100)
#' p
#' p$configuracion$origen$por_defecto
#' p$tablas$poblacion
#'
#' # un proyecto mínimo sin carpeta: la configuración y las tablas obligatorias como argumentos
#' p <- dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30),
#'                  ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = dl_ejemplo("poblacion.csv"),
#'                  ancla = dl_ejemplo("ancla"))
#' \donttest{
#' # el paso siguiente: los insumos
#' b <- dl_insumos(p)
#' }
#' @export
dl_proyecto <- function(carpeta = NULL, causa = NULL, ..., configuracion = NULL) {
  dadas <- list(...)
  nombres <- names(dadas) %||% rep("", length(dadas))
  otras <- setdiff(nombres, .DL_TABLAS)
  if (length(otras))
    .dl_stop("los argumentos de `...` son tablas del contrato por su nombre (%s); no se reconoce: %s",
             paste(.DL_TABLAS, collapse = ", "), paste(ifelse(nzchar(otras), otras, "(sin nombre)"), collapse = ", "))
  if (is.null(carpeta) && is.null(configuracion))
    .dl_stop("falta `carpeta` (la carpeta del proyecto) o `configuracion` (con las tablas como argumentos)")
  if (!is.null(carpeta)) .dl_exigir_carpeta_existente(carpeta)
  if (!is.null(causa)) causa <- .dl_exigir_causa(causa)
  cf <- .dl_config_a_leer(carpeta, causa, configuracion)
  if (cf$simple) return(.dl_proyecto_contrato(carpeta, cf, dadas))
  if (length(dadas) || is.list(configuracion))
    .dl_stop(paste0("un proyecto del formato completo de la 0.2.2 se lee de su carpeta y su archivo de ",
                    "configuraci\u00f3n: no recibe tablas en `...` ni la configuraci\u00f3n como lista"))
  .dl_proyecto_de(carpeta %||% .dl_raiz_proyecto(cf$archivo), dl_configuracion(cf$causa, cf$archivo))
}

# La configuración que lee dl_proyecto(): list(s, archivo, causa, simple). Sin `configuracion`, la de `causa` en la
# carpeta (.dl_configs_proyecto y .dl_elegir_configs); con ella, su archivo o la lista misma. La causa es la pedida o,
# sin ella, la que declara la configuración.
.dl_config_a_leer <- function(carpeta, causa, configuracion) {
  if (is.null(configuracion)) {
    cf <- .dl_elegir_configs(.dl_configs_proyecto(carpeta), causa, carpeta)
    return(list(s = .dl_leer_config(cf$archivo), archivo = cf$archivo, causa = cf$causa, simple = cf$simple))
  }
  if (!is.list(configuracion) && (!.dl_es_texto1(configuracion) || !file.exists(configuracion) ||
                                  dir.exists(configuracion)))
    .dl_stop("`configuracion` debe ser la ruta de un archivo YAML o una lista con sus claves; es %s",
             .dl_describir_objeto(configuracion))
  archivo <- if (is.list(configuracion)) "configuracion" else configuracion
  s <- if (is.list(configuracion)) configuracion else .dl_leer_config(archivo)
  if (!is.list(s) || is.null(names(s)))
    .dl_stop_config_simple(archivo, "el archivo no es una lista de claves (clave: valor, una por l\u00ednea)")
  if (is.null(causa)) {
    v <- s[["causa"]] %||% s[["cause_id"]]
    if (!.dl_es_entero1(v)) .dl_stop("%s no declara `causa` (el identificador de la causa)", basename(archivo))
    causa <- as.integer(v)
  }
  list(s = s, archivo = archivo, causa = causa, simple = !.dl_es_config_completa(s))
}

# El proyecto de las tablas del contrato: la configuración y las tablas (.dl_preparar_contrato) y las rutas de su
# traducción (.dl_traducir_proyecto), identificada por la carpeta o, sin ella, por la configuración.
.dl_proyecto_contrato <- function(carpeta, cf, dadas) {
  pre <- .dl_preparar_contrato(cf$s, cf$archivo, cf$causa, carpeta, dadas)
  structure(list(configuracion = pre$cfg,
                 rutas = .dl_traducir_proyecto(carpeta %||% cf$archivo, pre$cfg, pre$tablas, pre$causas),
                 carpeta = carpeta, formato = "simple", tablas = pre$tablas), class = "dl_proyecto")
}

# El proyecto del formato completo de `carpeta` con la configuración `cfg`: sus rutas, sin tablas del contrato.
.dl_proyecto_de <- function(carpeta, cfg)
  structure(list(configuracion = cfg, rutas = .dl_rutas_con_codigos(.dl_rutas_completo(carpeta, cfg$cause_id), cfg),
                 carpeta = carpeta, formato = "completo", tablas = NULL), class = "dl_proyecto")

#' @export
print.dl_proyecto <- function(x, ...) {
  cfg <- x$configuracion
  nombre <- cfg$origen$nombre %||% ""
  simple <- identical(x$formato, "simple")
  formato <- if (simple) "tablas del contrato" else "formato completo de la 0.2.2"
  cat(sprintf("<dl_proyecto> %scausa %d | a\u00f1o %s | %s\n", if (nzchar(nombre)) paste0(nombre, ", ") else "",
              cfg$cause_id, .dl_anio_ajuste(cfg), formato))
  cat(sprintf("  carpeta: %s\n", x$carpeta %||% "(ninguna: las tablas vienen como argumentos)"))
  arch <- if (simple) vapply(.DL_TABLAS, function(k) {
    t <- x$tablas[[k]]
    if (is.null(t)) "no" else sprintf("%d fila(s), de %s", nrow(t), basename(attr(t, "origen")))
  }, "") else vapply(.DL_CLAVES_RUTAS, function(k) if (is.null(x$rutas[[k]])) "no" else "s\u00ed", "")
  cat(if (simple) "  tablas:\n" else "  archivos:\n")
  cat(sprintf("    %-*s %s\n", max(nchar(names(arch))), names(arch), arch), sep = "")
  if (simple) {
    cat(sprintf("  subnacional: %s\n", cfg$origen$subnacional))
    pd <- cfg$origen$por_defecto
    if (length(pd)) {
      t <- .dl_claves_simple()
      cat(sprintf("  tomado por defecto (%d):\n", length(pd)))
      cat(sprintf("    %s = %s\n", .dl_con_simbolo(names(pd), t$simbolo[match(names(pd), t$clave)]), pd), sep = "")
    }
  }
  invisible(x)
}
