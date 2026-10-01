# La carpeta de un proyecto: dl_proyecto() lee la configuración de una causa y arma las rutas de sus insumos: los
# archivos del proyecto (formato completo) o su traducción (formato simple, R/formato_simple.R), escrita con la
# estructura del formato completo en la carpeta temporal de la sesión. Con esas rutas, lo que sigue no cambia.

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

# Lo que la traducción de la configuración simple `s` toma de la carpeta `proyecto`: la ubicación nacional y el nombre
# (del ancla, si no los declara; sus errores llevan su tabla, prior_gbd), la causa padre, las covariables con filas en
# proxies.csv (covariables_subnacionales) y si la población es subnacional. Un proxies.csv o un poblacion.csv que no se
# puede leer no detiene la configuración: su error lo da la lectura de la tabla. Todavía no trae `betas` ni
# `ids_covariable`: la configuración nueva ya no declara covariables.
.dl_contexto_proyecto <- function(proyecto, s, causa) {
  ctx <- list()
  cf <- .dl_configs_proyecto(proyecto)
  padre <- which(vapply(cf$subtipos, function(h) causa %in% h, NA))[1L]
  if (!is.na(padre)) ctx$padre <- cf$causa[padre]
  if ((is.null(s[["ubicacion_gbd"]]) || is.null(s[["nombre"]])) && dir.exists(file.path(proyecto, "ancla"))) {
    de_causa <- tryCatch(.dl_ancla_de_causa(.dl_ancla_simple(proyecto), causa),
                         dl_error = function(e) .dl_stop(.dl_detalle(e), campos = list(tabla = "prior_gbd")))
    locs <- unique(de_causa$location_id)
    if (is.null(s[["ubicacion_gbd"]]) && length(locs) != 1L)
      .dl_stop(paste0("no se puede saber la ubicaci\u00f3n nacional: el ancla trae la causa %d en las ubicaciones %s. ",
                      "Declara ubicacion_gbd (el location_id de GBD del pa\u00eds) en la configuraci\u00f3n"),
               causa, paste(locs, collapse = ", "))
    if (length(locs) == 1L) ctx$ubicacion <- as.integer(locs)
    ctx$nombre <- unique(de_causa$cause_name)[1L]
  }
  ubicacion <- s[["ubicacion_gbd"]] %||% ctx$ubicacion
  if (is.null(ubicacion))
    .dl_stop(paste0("falta ubicacion_gbd (el location_id de GBD del pa\u00eds) y el proyecto no tiene ancla/ ",
                    "de donde tomarla"))
  # los valores de la columna `cn` de una tabla del proyecto (NULL sin filas; NA si no se puede leer), memorizados
  columna <- function(archivo, cn) {
    f <- file.path(proyecto, archivo)
    if (.dl_hay_filas(f))
      tryCatch(.dl_leer_memo(f, function(p) unique(.dl_leer_simple(p)[[cn]]), variante = cn),
               dl_error = function(e) NA)
  }
  cs <- columna("proxies.csv", "covariable")
  ctx$covariables_subnacionales <- if (is.null(cs) || identical(cs, NA)) character() else cs
  pb <- columna("poblacion.csv", "location_id")
  if (!identical(pb, NA)) ctx$subnacional <- any(pb != as.character(ubicacion))
  ctx
}

# ---- La traducción de un proyecto simple ----

# Dónde va cada pieza en un proyecto del formato completo (como inst/extdata/acs_peru_completo): clave -> ruta.
.dl_piezas_completo <- function(causa) c(
  stats::setNames(as.list(sprintf("ancla/%s.csv", tolower(.DL_MEDIDAS$nombre_es))), .DL_MEDIDAS$path_key),
  list(ghdx_cov = "covariables", extraction = "extraccion.yaml", poblacion = "poblacion.csv",
       pesos_80mas = "pesos_80mas.csv", severidad = sprintf("severidad/%d.csv", causa), ghdx_store = "evidencia",
       catalogos = "catalogos", datos = "datos.csv", cov_proxy = "proxies_departamentales.csv", registry = "registro"))

# Nombres que no están en las tablas: las causas con configuración simple (`cf`) y sus hijas, con el nombre de cada
# causa en el ancla, para el catálogo de causas, el registro de subtipos y las etiquetas.
.dl_causas_proyecto <- function(cf, ancla) {
  d <- cf[, list(cause_id = causa, nombre, hijos = vapply(subtipos, paste, "", collapse = "|"))]
  hijas <- unlist(cf$subtipos)
  d <- rbind(d, data.table::data.table(cause_id = setdiff(hijas, d$cause_id), nombre = NA_character_, hijos = ""))
  en_ancla <- unique(ancla[, list(cause_id = as.integer(cause_id), cause_name)])
  d[, nombre_ancla := en_ancla$cause_name[match(cause_id, en_ancla$cause_id)]]
  d[is.na(nombre), nombre := data.table::fifelse(is.na(nombre_ancla), as.character(cause_id), nombre_ancla)]
  d[is.na(nombre_ancla), nombre_ancla := nombre][]
}

# Escribe en `destino` la traducción `x` (.dl_traducir_tablas) del proyecto simple `carpeta` para `cfg`, pieza por
# pieza (.dl_piezas_completo): el ancla por medida, las tablas (los textos tal cual), la extracción, los catálogos
# (edades de inst/referencia; ubicaciones y causas del proyecto, de `causas`) y el registro (subtipos y etiquetas).
.dl_escribir_traduccion <- function(carpeta, cfg, destino, x, causas) {
  pieza <- .dl_piezas_completo(cfg$cause_id)
  en <- function(...) file.path(destino, ...)
  escribir <- function(d, ...) if (!is.null(d)) {
    dir.create(dirname(en(...)), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(d, en(...), eol = "\n", na = "")
  }
  unlink(destino, recursive = TRUE)
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  for (k in seq_len(nrow(.DL_MEDIDAS))) {
    m <- x$ancla[measure_id == as.character(.DL_MEDIDAS$measure_id_gbd[k])]
    if (nrow(m)) escribir(m, pieza[[.DL_MEDIDAS$path_key[k]]])
  }
  escribir(x$poblacion$tabla, pieza$poblacion)
  pesos <- file.path(carpeta, "pesos_80mas.csv")
  if (file.exists(pesos)) file.copy(pesos, en(pieza$pesos_80mas)) else escribir(x$pesos_80, pieza$pesos_80mas)
  if (!is.null(x$extraccion)) yaml::write_yaml(x$extraccion, en(pieza$extraction), fileEncoding = "UTF-8")
  escribir(x$proxies, pieza$cov_proxy)
  escribir(x$datos, pieza$datos)
  escribir(x$severidad, pieza$severidad)
  nacional <- .dl_loc_ancla(cfg)
  cat_arch <- .dl_schema_estimates()$catalogos
  dir.create(en(pieza$catalogos))
  file.copy(.dl_inst_archivo("referencia", "catalogo_demograficos_gbd2023.csv"),
            en(pieza$catalogos, cat_arch$demograficos))
  locs <- unique(x$poblacion$tabla[, list(location_id, location_level)])
  locs[, `:=`(location_name = x$nombres_loc$location_name[match(location_id, x$nombres_loc$location_id)],
              parent_id = data.table::fifelse(location_level == 0L, NA_character_, nacional))]
  locs[is.na(location_name), location_name := location_id]
  escribir(locs[, list(location_id, location_name, location_level, parent_id)], pieza$catalogos, cat_arch$locations)
  causas <- .dl_causas_proyecto(causas, x$ancla)
  escribir(causas[, list(cause_id, cause_name = .dl_slugify(nombre_ancla))], pieza$catalogos, cat_arch$causas)
  escribir(causas[nzchar(hijos), list(cause_id, nombre_es = nombre, hijos)], pieza$registry, "master_gbd.csv")
  escribir(rbind(.dl_etiquetas_ref(), causas[, list(tabla = "cause", id = as.character(cause_id), name = nombre_ancla,
                                          name_es = nombre, slug = .dl_slugify(nombre_ancla),
                                          slug_es = .dl_slugify(nombre))]), pieza$registry, "etiquetas_es.csv")
  writeLines("sequela_id,cause_id,sequela_name,health_state_id,rol", en(pieza$registry, "sequela_rei.csv"))
  writeLines("", en("listo"))
}

# Rutas del proyecto simple `carpeta` traducido para `cfg` (`x`: sus tablas, si ya se calcularon), en la carpeta
# temporal <proyecto>_<causa>_<clave>; <clave> resume lo que se traduce (versión del paquete, configuración traducida,
# causas del proyecto y contenido de las tablas). Una traducción nueva borra las anteriores y sus lecturas memorizadas.
.dl_traducir_proyecto <- function(carpeta, cfg, x = NULL) {
  tablas <- unlist(lapply(sub("/$", "", .dl_archivos_simple()), function(f) {
    d <- file.path(carpeta, f)
    if (dir.exists(d)) file.path(f, list.files(d, "[.]csv$", ignore.case = TRUE)) else f
  }))
  tablas <- tablas[file.exists(file.path(carpeta, tablas))]
  traducida <- cfg
  traducida$origen$archivo <- NULL                    # la ruta de la configuración, no su contenido
  causas <- .dl_configs_proyecto(carpeta)[!is.na(causa) & simple, list(causa, nombre, subtipos)]
  clave <- digest::digest(list(dl_version(), traducida, causas, tablas,
                               unname(tools::md5sum(file.path(carpeta, tablas)))), algo = "xxhash64")
  prefijo <- sprintf("%s_%d_", digest::digest(normalizePath(carpeta, winslash = "/"), algo = "xxhash64"), cfg$cause_id)
  raiz <- file.path(tempdir(), "dismodlite_proyectos")
  destino <- file.path(raiz, paste0(prefijo, clave))
  if (!file.exists(file.path(destino, "listo"))) {
    for (v in list.files(raiz, paste0("^", prefijo), full.names = TRUE)) {
      k <- ls(.dl_memo_env)
      rm(list = k[startsWith(k, normalizePath(v, winslash = "/"))], envir = .dl_memo_env)
      unlink(v, recursive = TRUE)
    }
    .dl_escribir_traduccion(carpeta, cfg, destino, x %||% .dl_traducir_tablas(carpeta, cfg), causas)
  }
  r <- .dl_rutas_completo(destino, cfg$cause_id)
  r["ghdx_cov"] <- list(if (length(cfg$origen$covariables)) file.path(carpeta, "covariables"))
  .dl_rutas_con_codigos(r, cfg)
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
#' Lee la carpeta de un proyecto y devuelve, para una causa, su configuración y las rutas de sus insumos: todo lo que
#' [dl_insumos()] necesita (`dl_insumos(dl_proyecto(carpeta))`). Es la forma recomendada de empezar. La carpeta puede
#' estar en el **formato simple** (una configuración corta, las descargas de GBD Results y del GHDx tal cual y unas
#' pocas tablas planas; ver las secciones de abajo) o en el **formato completo** de la versión 0.2.2.
#'
#' Un proyecto simple se traduce al formato completo, el que usa el resto del paquete: la configuración con
#' [dl_configuracion()] y las tablas a una carpeta temporal de la sesión, que se reutiliza mientras no cambien. La
#' traducción renombra columnas y agrega constantes (los números medidos pasan tal cual); solo calcula lo que sale de la
#' población: el total nacional, la banda de 80 años y más (si no viene) y los pesos de sus bandas.
#'
#' Un problema de forma de una tabla (una columna que falta, un sexo o un grupo de edad que no se reconoce, el año que
#' se estima sin población, un CSV guardado desde Excel) detiene `dl_proyecto()`, con el archivo y la columna. Las
#' reglas que cruzan tablas (la población nacional suma las subnacionales, los proxies cierran en el valor nacional, los
#' datos tienen valores posibles) las comprueba [dl_insumos()]; [dl_revisar_proyecto()] lista todo sin detenerse.
#'
#' `print()` muestra la causa, el año, los archivos que encontró, el modo subnacional y las claves de la configuración
#' que tomaron su valor por defecto.
#'
#' @section Formato simple:
#' ```
#' mi_proyecto/
#'   config.yaml        # la configuración: una causa (o config/<causa>.yaml, una por causa)
#'   ancla/             # descargas de GBD Results (CSV), sin editar
#'   poblacion.csv      # población por ubicación, año, sexo y edad
#'   severidad.csv      # estados de salud (hacen falta para los AVD y para dl_correr())
#'   covariables/       # descargas de covariables del GHDx (CSV), sin editar; si hay covariables
#'   proxies.csv        # opcional: covariables por ubicación subnacional
#'   datos.csv          # opcional: datos locales
#'   pesos_80mas.csv    # opcional: pesos de las bandas de 80 años y más
#' ```
#' Lo mínimo para armar los insumos y ajustar es la configuración (con `causa`, `anio` y `edad_inicio`; todas sus
#' claves en [dl_configuracion()]), `ancla/` y `poblacion.csv`: con eso se estima la causa en el país y, si la población
#' trae ubicaciones subnacionales, en cada una con las tasas nacionales. [dl_correr()], que calcula los años vividos con
#' discapacidad (AVD), necesita también `severidad.csv`. `covariables/` y `proxies.csv` agregan las diferencias entre
#' ubicaciones subnacionales y `datos.csv`, los datos locales. Una tabla opcional con solo el encabezado es como si no
#' estuviera (así quedan las plantillas de [dl_nuevo_proyecto()]).
#'
#' Con varias causas, cada una tiene su configuración en `config/<causa>.yaml` y comparten las tablas: `ancla/` y
#' `proxies.csv` traen las filas de todas (cada causa usa las suyas) y `datos.csv` y `severidad.csv` llevan la columna
#' `causa`. Una causa que es la suma de otras las declara en `subtipos`; cada subtipo se corre por separado y sus
#' corridas se suman con [dl_sumar_hijas()], con las rutas del proyecto de la causa padre. Una hija que queda fuera de
#' la suma no pasa su corrida y se declara en `omitidas`, con el motivo, que queda en el manifiesto de la suma:
#' ```r
#' corridas <- vapply(c(1011, 1012), function(k) dl_correr(carpeta, k, semilla = 1)$dir, "")
#' dl_sumar_hijas(corridas, causa = 1010, nombre = "suma", carpeta = file.path(carpeta, "resultados"),
#'                rutas = dl_proyecto(carpeta, 1010)$rutas,
#'                omitidas = list(list(cause_id = 1013, motivo = "sin datos suficientes")))
#' ```
#'
#' Las tablas son CSV en UTF-8 con coma como separador y punto decimal (desde Excel, «CSV UTF-8»), con los nombres de
#' columna de abajo; las columnas de más se ignoran. Las edades de GBD y las etiquetas en español vienen con el paquete;
#' los catálogos de causas y de ubicaciones y el registro de subtipos se derivan de la configuración y de las tablas.
#'
#' @section Archivos del formato simple:
#' **`ancla/`** (obligatoria). La estimación de referencia: una o más descargas de la herramienta GBD Results del
#' IHME, en CSV, sin editar, con las columnas «ID y nombre». Se usan estas columnas:
#' - `measure_id`: la medida. 5 prevalencia (obligatoria); 1 muertes (obligatoria con el prior de la mortalidad en
#'   exceso por defecto, `mortalidad_exceso.prior: desde_ancla`, o sin `mortalidad_exceso.techo`); 6 incidencia
#'   (opcional: la usa la comprobación de la incidencia implícita de [dl_validar_ancla()]); 3 AVD (hace falta para
#'   [dl_factor_comorbilidad()]).
#' - `metric_name`: `Rate` (tasa por 100 000) en todas las medidas, también la prevalencia; las filas de otras
#'   métricas se ignoran. El `Percent` de la prevalencia en GBD Results no es la proporción de la población: divide
#'   por las personas con alguna causa (ver `anchor.metrica_prevalencia` en [dl_configuracion()]).
#' - `location_id` y `location_name`: la ubicación; se usan las filas de `ubicacion_gbd`.
#' - `sex_id` (1 hombres, 2 mujeres), `age_id` y `age_name` (el grupo de edad de GBD), `cause_id` y `cause_name`,
#'   `year`.
#' - `val`, `lower` y `upper`: la estimación y su intervalo de incertidumbre del 95 %.
#'
#' Las filas de otras causas, ubicaciones (global, regiones), años, sexos (ambos sexos, `sex_id` 3) o métricas se
#' ignoran: una descarga puede servir a todas las causas. Las bandas de 80-84, 85-89, 90-94 y 95 años y más se agregan a
#' 80 y más con los pesos de la población nacional; las bandas por debajo de `edad_inicio`, las de todas las edades o
#' estandarizadas y las que valen 0 (GBD no modela la causa a esa edad) quedan fuera, con un mensaje. El nombre de cada
#' archivo, sin la extensión, identifica la descarga en el manifiesto, y su año más reciente es la ronda de GBD que
#' registran las salidas (`round`).
#'
#' **`poblacion.csv`** (obligatorio). La población por ubicación, año, sexo y grupo de edad:
#' - `location_id`: la ubicación. La de `ubicacion_gbd` es la nacional; cualquier otra es subnacional, con
#'   cualquier código (se lee como texto: `01`, `A` o `norte` valen igual).
#' - `location_name` (opcional): el nombre de la ubicación en las salidas; sin él, el código.
#' - `anio`: el año. Hace falta el año que se estima (`anio`), para cada sexo y, si el ancla trae las bandas de 80 años
#'   y más por separado, el del ancla (`ancla.anio`).
#' - `sexo`: `hombres` o `mujeres` (o 1 y 2).
#' - `edad_inicio` y `edad_fin`: los límites de un grupo de edad de GBD, en años, sin incluir `edad_fin`: 30 y 35
#'   para 30-34 años, 95 y 125 para 95 y más, 80 y 125 para 80 y más. En su lugar puede ir `age_group_id`, el
#'   identificador de GBD. Los grupos deben cubrir las edades del modelo desde `edad_inicio`.
#' - `poblacion`: el número de personas.
#'
#' Las filas nacionales son opcionales: sin ellas, el total nacional de cada año, sexo y grupo de edad es la suma de las
#' subnacionales; si vienen, deben ser esa suma. Sin la banda de 80 años y más, es la suma de 80-84, 85-89, 90-94 y 95+
#' en cada ubicación, año y sexo. Las sumas son exactas con conteos enteros (15 cifras significativas). Un proyecto sin
#' ubicaciones subnacionales trae solo las filas nacionales. Los pesos de las bandas de 80 años y más salen de la
#' población nacional del año del ancla, por sexo (si faltan esas bandas, usa `pesos_80mas.csv`).
#'
#' **`severidad.csv`** (hace falta para los AVD y para [dl_correr()]). Los estados de salud de cada causa:
#' - `causa`: la causa (el archivo trae las de todas las causas del proyecto).
#' - `estado`: el nombre del estado de salud (por ejemplo, asintomático, leve, moderado).
#' - `id_estado` (opcional): el identificador del estado (el `health_state_id` de GBD); sin la columna, el orden en
#'   que aparece cada estado.
#' - `proporcion`, `proporcion_inferior` y `proporcion_superior`: la fracción de los casos prevalentes en el estado,
#'   con su intervalo del 95 %; las de una causa suman 1.
#' - `peso_discapacidad`, `peso_inferior` y `peso_superior`: el peso de discapacidad del estado, entre 0 y 1, con su
#'   intervalo del 95 % (0 en un estado asintomático).
#'
#' **`covariables/`** (si la configuración declara `covariables`). Las descargas del GHDx de esas covariables, en CSV,
#' sin editar, con las columnas `covariate_name_short` (el `nombre` en la configuración), `location_id`, `year_id`,
#' `sex_id` (1, 2 o 3 ambos sexos), `age_group_id`, `mean_value`, `lower_value` y `upper_value` (el valor y su
#' intervalo del 95 %, en las unidades del GHDx) y, si viene, `covariate_id`. Se leen solo las filas de la ubicación
#' nacional, que deben estar (las de global o regiones no hacen falta).
#'
#' **`proxies.csv`** (opcional). Los valores subnacionales de las covariables (los proxies): con ellos, la estimación
#' subnacional (`subnacional.modo: covariables`) mueve las tasas nacionales en cada ubicación según su diferencia
#' con el valor nacional. Solo las covariables declaradas con filas aquí producen esas diferencias.
#' - `covariable`: el `nombre` de una covariable de la configuración (las de otras causas del proyecto no se usan).
#' - `location_id`: la ubicación subnacional (los códigos de `poblacion.csv`).
#' - `anio`: el año que se estima (`anio`); las filas de otros años no se usan.
#' - `sexo`: `hombres`, `mujeres` o `ambos` (o 1, 2, 3).
#' - `edad_inicio` y `edad_fin` (o `age_group_id`): el grupo de edad de GBD; vacías, o sin estas columnas, todas las
#'   edades.
#' - `valor` y `error_estandar`: el valor de la covariable en la ubicación y su error estándar, en la escala de la
#'   descarga del GHDx.
#'
#' El promedio de las ubicaciones ponderado por su población (la de su sexo y su grupo de edad) debe ser el valor
#' nacional de la covariable, el de `covariables/` del año del ancla (el que se estima o, en una proyección, el
#' anterior). Si los valores vienen de otra fuente, llévalos a ese valor antes: con los valores `v` y las poblaciones
#' `w` de las ubicaciones y el valor nacional `X`, por desplazamiento `v + X - sum(w * v) / sum(w)`, o por escala
#' `v * X / (sum(w * v) / sum(w))` (el error estándar se escala igual). Los números no se recalibran al leerlos: si no
#' cierran, o si una ubicación no está en `poblacion.csv`, [dl_insumos()] se detiene y lo explica. La comprobación es
#' prácticamente exacta (el promedio y el valor nacional pueden diferir en menos de 1e-8): los valores recalibrados se
#' escriben sin redondear (`write.csv()` guarda 15 cifras significativas), no redondeados en una hoja de cálculo.
#'
#' **`datos.csv`** (opcional). Datos locales, una fila por dato:
#' - `tipo`: `prevalencia_estudio` (prevalencia medida en un estudio o una encuesta), `incidencia`, `mortalidad`
#'   (mortalidad por la causa) o `prevalencia_registro` (prevalencia de un registro administrativo: se lee, pero nunca
#'   entra al ajuste; si `datos_en_ajuste` no está vacío, sus filas nacionales van con `excluir: TRUE` y un `motivo`).
#' - `causa` (opcional): la causa del dato; sin la columna, cada fila es de la causa que se lee.
#' - `location_id`: la ubicación nacional o una subnacional.
#' - `sexo`: `hombres` o `mujeres` (o 1 y 2). Una fila de ambos sexos no se usa: se divide por sexo.
#' - `edad_inicio` y `edad_fin`: el intervalo de edad del dato, en años, sin incluir `edad_fin` (cualquier
#'   intervalo, no solo los grupos de GBD; 125 para «y más»).
#' - `anio`, o `anio_inicio` y `anio_fin`: el año o los años del dato. Una fila nacional se usa solo si incluye
#'   `anio`, y una subnacional, si incluye `subnacional.anio_validacion`.
#' - `valor` y `error_estandar`: la prevalencia como proporción (entre 0 y 1) y la incidencia y la mortalidad como
#'   tasa por persona-año (no por 100 000); el error estándar, mayor que 0, en las mismas unidades.
#' - En lugar de `valor` y `error_estandar`, `casos` y `muestra`: personas con la enfermedad entre `muestra`
#'   personas examinadas (prevalencia: los casos no pueden superar la muestra), o casos nuevos o muertes en `muestra`
#'   personas-año (incidencia y mortalidad). Opcional, `muestra_efectiva` reemplaza a `muestra` (por ejemplo,
#'   corregida por el efecto de diseño).
#' - `excluir` (opcional, `TRUE` o `FALSE`) y `motivo`: `TRUE` deja fuera el dato (un valor atípico) y exige su motivo.
#' - `fuente` y `definicion` (opcionales): la fuente, que identifica el dato en el manifiesto, y la definición de caso.
#'
#' Los mensajes nombran cada dato como `fila_<n>`: la fila n de datos del archivo (la línea n + 1). Qué datos se usan:
#' las filas nacionales del año que se estima (su año, o su rango de `anio_inicio` a `anio_fin`, incluye `anio`)
#' entran al ajuste si su tipo está en `datos_en_ajuste` y no se excluyen. Con `datos_en_ajuste` vacío (el valor por
#' defecto), las filas nacionales no se usan, con un mensaje; con algún tipo, toda fila nacional de ese año que no se
#' excluye debe ser de un tipo de `datos_en_ajuste`, o [dl_insumos()] se detiene: las de otros tipos, incluidas las de
#' `prevalencia_registro` (que no se puede poner en `datos_en_ajuste`), llevan `excluir: TRUE` y su `motivo`. Las filas
#' subnacionales del año `subnacional.anio_validacion` no entran al ajuste: las de mortalidad validan las estimaciones
#' subnacionales (ver [dl_validar_ancla()]). Las filas de la causa de otros años, las de ambos sexos y las que terminan
#' antes de `edad_inicio` quedan fuera con un mensaje (una encuesta nacional de un año distinto del que se estima no
#' entra al ajuste), y si con `datos_en_ajuste` no entra ninguna fila nacional, un mensaje lo dice.
#'
#' **`pesos_80mas.csv`** (opcional). Solo si `poblacion.csv` no trae las bandas de 80-84, 85-89, 90-94 y 95 años y
#' más del año del ancla: `age_group_id` (30, 31, 32 y 235), `sex_id` (1, 2) y `peso` (el peso relativo de la banda
#' en su sexo; por ejemplo, su población).
#'
#' @section Formato completo:
#' Un proyecto es del formato completo si su configuración trae `schema: dismod_lite/v1`. Tiene `config/<causa>.yaml`,
#' `ancla/` con un CSV por medida (`prevalencia.csv`, `mortalidad.csv`, `incidencia.csv` y `avd.csv`, en el formato
#' de las estimaciones), `covariables/`, `extraccion.yaml` (las betas), `poblacion.csv`, `pesos_80mas.csv`,
#' `datos.csv`, `proxies_departamentales.csv`, `severidad/<causa>.csv`, `evidencia/`, `registro/`, `catalogos/` y, si
#' hay una, la partición de severidad en `particion/<corrida>/`. Cada tabla sigue el contrato `dismod_lite/v1` (ver
#' [dl_esquema()]). Solo este formato permite modelar un componente de una causa, derivar la severidad de una
#' partición ([dl_severidad_desde_particion()]) y usar el almacén de evidencia. El ejemplo en este formato:
#' `system.file("extdata", "acs_peru_completo", package = "dismodlite")`.
#'
#' @param carpeta Carpeta del proyecto: la que tiene `config.yaml` o `config/`.
#' @param causa Causa (`cause_id`, un entero); `NULL` si el proyecto tiene una sola.
#' @return Objeto de clase `dl_proyecto`: una lista con
#'   - `configuracion`: la configuración de la causa, de clase `dl_config`, como la de [dl_configuracion()]. En el
#'     formato simple trae `origen$por_defecto`: las claves tomadas por defecto, con su valor.
#'   - `rutas`: las rutas de los insumos, de clase `dl_paths`, como las de [dl_rutas()]. En el formato simple apuntan
#'     a la traducción, en la carpeta temporal de la sesión: en otra sesión, o después de cambiar la configuración o
#'     una tabla (la traducción anterior se borra), vuelve a llamar a `dl_proyecto()`.
#'   - `carpeta`: la carpeta del proyecto.
#'   - `formato`: `"simple"` o `"completo"`.
#' @seealso [dl_configuracion()] (las claves de la configuración), [dl_insumos()] (el paso siguiente),
#'   [dl_ejemplo()] (el proyecto de ejemplo), [dl_nuevo_proyecto()] (crear la carpeta de un proyecto),
#'   [dl_revisar_proyecto()] (revisarla antes de correr) y [dl_correr()] (la corrida completa en una llamada).
#' @family proyecto
#' @examples
#' # el proyecto de ejemplo, en el formato simple
#' p <- dl_proyecto(dl_ejemplo(), causa = 9100)
#' p
#' p$configuracion$origen$por_defecto
#'
#' # las primeras líneas de sus tablas
#' readLines(dl_ejemplo("poblacion.csv"), n = 3)
#' readLines(dl_ejemplo("proxies.csv"), n = 3)
#' readLines(dl_ejemplo("datos.csv"), n = 3)
#' readLines(dl_ejemplo("severidad.csv"), n = 3)
#'
#' # un proyecto mínimo: la configuración, el ancla y la población
#' carpeta <- file.path(tempdir(), "mi_proyecto")
#' dir.create(file.path(carpeta, "ancla"), recursive = TRUE)
#' invisible(file.copy(dl_ejemplo("ancla", "sintetico_acs_v1.csv"), file.path(carpeta, "ancla")))
#' invisible(file.copy(dl_ejemplo("poblacion.csv"), carpeta))
#' writeLines(c("causa: 9101", "anio: 2023", "edad_inicio: 30"), file.path(carpeta, "config.yaml"))
#' dl_proyecto(carpeta)
#' unlink(carpeta, recursive = TRUE)
#' \donttest{
#' # el paso siguiente: los insumos
#' b <- dl_insumos(p)
#' }
#' @export
dl_proyecto <- function(carpeta, causa = NULL) {
  .dl_exigir_carpeta_existente(carpeta)
  if (!is.null(causa)) causa <- .dl_exigir_causa(causa)
  cf <- .dl_elegir_configs(.dl_configs_proyecto(carpeta), causa, carpeta)
  .dl_proyecto_de(carpeta, dl_configuracion(cf$causa, cf$archivo))
}

# El proyecto de `carpeta` con la configuración `cfg`: sus rutas (en el formato simple, las de su traducción; `x`, sus
# tablas si ya se calcularon) y su formato.
.dl_proyecto_de <- function(carpeta, cfg, x = NULL) {
  simple <- .dl_es_simple(cfg)
  structure(list(configuracion = cfg,
                 rutas = if (simple) .dl_traducir_proyecto(carpeta, cfg, x)
                         else .dl_rutas_con_codigos(.dl_rutas_completo(carpeta, cfg$cause_id), cfg),
                 carpeta = carpeta, formato = if (simple) "simple" else "completo"), class = "dl_proyecto")
}

#' @export
print.dl_proyecto <- function(x, ...) {
  cfg <- x$configuracion
  nombre <- cfg$origen$nombre %||% ""
  cat(sprintf("<dl_proyecto> %scausa %d | a\u00f1o %s | formato %s\n", if (nzchar(nombre)) paste0(nombre, ", ") else "",
              cfg$cause_id, .dl_anio_ajuste(cfg), x$formato))
  cat(sprintf("  carpeta: %s\n", x$carpeta))
  en <- function(...) file.path(x$carpeta, ...)
  n_csv <- function(d) length(list.files(en(d), pattern = "[.]csv$", ignore.case = TRUE))
  arch <- if (identical(x$formato, "simple")) vapply(.dl_archivos_simple(), function(f)
    if (endsWith(f, "/")) sprintf("%d descarga(s)", n_csv(f))
    else if (.dl_hay_filas(en(f))) "s\u00ed" else if (file.exists(en(f))) "sin filas (no se usa)" else "no", "")
  else vapply(.DL_CLAVES_RUTAS, function(k) if (is.null(x$rutas[[k]])) "no" else "s\u00ed", "")
  cat("  archivos:\n")
  cat(sprintf("    %-*s %s\n", max(nchar(names(arch))), names(arch), arch), sep = "")
  if (identical(x$formato, "simple")) {
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
