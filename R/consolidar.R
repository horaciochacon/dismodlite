# Consolidado: la corrida vigente de dismodlite por (causa, año) -> tabla canónica estimates/v1 (tasas y conteos)
# -> tablas con la forma de un perfil de columnas -> mod/consolidado/<run_id>/{canonico/, tablas/, manifest.yaml}
# y, si se pide, una entrada en el registro de corridas. Todo se valida en memoria antes de escribir.

# ¿La corrida `nuevo` es más reciente que `actual` (NULL si todavía no hay ninguna)? Por la fecha y después la versión
# de su run_id.
.dl_run_gana <- function(nuevo, actual) {
  if (is.null(actual)) return(TRUE)
  a <- .dl_partes_run_id(nuevo, "del registro de corridas"); b <- .dl_partes_run_id(actual, "del registro de corridas")
  a$fecha > b$fecha || (a$fecha == b$fecha && a$v > b$v)
}

# Corridas activas de dismod_lite en el registro -> la vigente por (causa, año). Las superadas siguen activas: no se
# marcan, solo no se eligen. Atributo `huecos` = (causa, año) sin corrida vigente. anios = NULL: todos los años con
# alguna corrida activa. Las causas que se reportan como suma de hijas son las que declaran hijas en master_gbd.csv
# (la misma fuente que usa dl_sumar_hijas()).
#' Seleccionar las corridas vigentes del registro
#'
#' Entre las corridas activas del registro elige, por causa y año, la más reciente (fecha y versión del
#' `run_id`). Las causas que `master_gbd.csv` declara como suma de hijas deben venir de [dl_sumar_hijas()].
#'
#' @details
#' Es el primer paso de [dl_consolidar()], que lo hace todo en una llamada; las funciones de cada paso sirven para
#' mirar o cambiar uno. El registro de corridas es un YAML con la lista `datasets`; un registro vacío es la línea
#' `datasets: []`. Las corridas se anotan en él con el argumento `registro` de [dl_exportar_corrida()],
#' [dl_sumar_hijas()] o [dl_reresumir_corrida()]. Las corridas superadas siguen activas en el registro: no se
#' marcan, solo no se eligen.
#'
#' @param registro Archivo YAML del registro de corridas.
#' @param carpeta_corridas Carpeta raíz donde están las corridas.
#' @param causas,anios Causas y años a seleccionar (`NULL`: todos los del registro).
#' @param permitir_huecos `TRUE` acepta pares (causa, año) sin corrida y los declara (atributo `huecos`).
#' @param rutas Rutas de [dl_rutas()]; se usa el registro (la carpeta de tablas de referencia con
#'   `master_gbd.csv`, no el registro de corridas).
#' @return Tabla (data.table) con una fila por (causa, año): `cause_id`, `year`, `run_id` (la corrida elegida),
#'   `dir` (su carpeta), `agregacion` (`"suma_de_hijas"` en una suma; `NA` en un ajuste) y `manifest` (su
#'   manifiesto, como lista). Atributo `huecos`: los pares (`cause_id`, `year`) pedidos sin corrida.
#' @seealso [dl_consolidar()], [dl_consolidado_canonico()] (el paso siguiente).
#' @family consolidado
#' @examples
#' # El ejemplo de ?dl_consolidar arma un registro de corridas y corre todos los pasos. Este es
#' # el primero:
#' # sel <- dl_consolidado_seleccionar(registro, salida, rutas = rutas)
#' # sel[, c("cause_id", "year", "run_id", "agregacion")]
#' @export
dl_consolidado_seleccionar <- function(registro, carpeta_corridas, causas = NULL, anios = NULL,
                                       permitir_huecos = FALSE, rutas = dl_rutas()) {
  rutas <- .dl_resolver_rutas(rutas)
  activos <- Filter(function(d) identical(d$method, .DL_METODO_DISMOD_LITE) && identical(d$status, "active") &&
                      !is.null(d$run_id), .dl_registry_leer(registro))
  sumas <- .dl_master_leer(rutas)[nzchar(hijos)]$cause_id
  mejor <- list()
  for (d in activos) {
    dir_run <- .dl_dir_corrida(carpeta_corridas, d$run_id)
    man <- .dl_leer_manifest(dir_run, sprintf("la corrida activa %s del registro", d$run_id))
    cid <- as.integer(man$causa$cause_id); anio <- as.integer(man$params$year)
    if (!is.null(causas) && !(cid %in% causas)) next
    if (!is.null(anios) && !(anio %in% anios)) next
    agreg <- man$causa$agregacion %||% NA_character_
    k <- paste(cid, anio)
    if (.dl_run_gana(d$run_id, mejor[[k]]$run_id))
      mejor[[k]] <- list(cause_id = cid, year = anio, run_id = d$run_id, dir = dir_run,
                         agregacion = agreg, manifest = list(man))
  }
  sel <- data.table::rbindlist(mejor)
  if (!nrow(sel))
    .dl_stop("ninguna corrida activa de dismod_lite en el registro para las causas y a\u00f1os pedidos")
  data.table::setorder(sel, cause_id, year)
  # La regla de la suma se aplica a la corrida que gana: un ajuste de la causa padre, viejo y superado por su suma,
  # no la bloquea.
  malos <- sel[cause_id %in% sumas & !(agregacion %in% "suma_de_hijas")]
  if (nrow(malos))
    .dl_stop(paste0("la causa %d se reporta como la suma de sus hijas (master_gbd.csv declara sus hijas) y la ",
                    "corrida vigente %s es un ajuste: s\u00famala con dl_sumar_hijas()"), malos$cause_id[1],
             malos$run_id[1])
  esperado <- data.table::CJ(cause_id = causas %||% unique(sel$cause_id),
                             year = as.integer(anios %||% unique(sel$year)))
  huecos <- esperado[!sel, on = c("cause_id", "year")]
  if (nrow(huecos) && !permitir_huecos)
    .dl_stop(paste0("no hay corrida vigente para estas causas/a\u00f1os: %s (permitir_huecos = TRUE los declara como ",
                    "huecos en el manifiesto)"), paste(sprintf("%d/%d", huecos$cause_id, huecos$year), collapse = ", "))
  data.table::setattr(sel, "huecos", huecos)
  sel
}

# Apila cause/<medida>/<run_id>.csv de cada corrida seleccionada: run_fuente = la corrida de origen; run_id = el del
# consolidado.
#' Tabla canónica de un consolidado
#'
#' Apila las celdas de cada corrida seleccionada, con `run_fuente` (la corrida de origen) y `run_id` (el del
#' consolidado).
#'
#' @param seleccion Selección de [dl_consolidado_seleccionar()].
#' @param id_consolidado Identificador (`run_id`) del consolidado, con la forma `<AAAA-MM-DD>_<nombre>_v<n>`.
#' @return Tabla (data.table) de celdas con las columnas del contrato `estimates/v1` (las de las celdas de
#'   [dl_resumir()], con `run_id` primero) más `run_fuente`: las tasas de cada medida (la prevalencia como
#'   proporción; la incidencia y los AVD por 100 000), sin conteos.
#' @seealso [dl_consolidado_seleccionar()] (el paso anterior), [dl_consolidado_conteos()] (el siguiente) y
#'   [dl_consolidar()] (todo en una llamada).
#' @family consolidado
#' @examples
#' # El ejemplo de ?dl_consolidar arma un registro de corridas y corre todos los pasos. Este es
#' # el segundo, con la selección del primero:
#' # canonica <- dl_consolidado_canonico(sel, paste0(Sys.Date(), "_consolidado_v1"))
#' # table(canonica$measure_name, canonica$metric_name)
#' @export
dl_consolidado_canonico <- function(seleccion, id_consolidado) {
  partes <- lapply(seq_len(nrow(seleccion)), function(i)
    data.table::rbindlist(lapply(.DL_MEDIDAS_EXPORTA$slug, function(slug)
      .dl_leer_particion_corrida(seleccion$dir[i], seleccion$run_id[i], slug)))[, run_fuente := seleccion$run_id[i]])
  ce <- data.table::rbindlist(partes)
  ce[, run_id := id_consolidado]
  ce
}

# sha256 de la tabla poblacion congelada que declara el manifiesto de una corrida (NULL en una suma: lo declaran las
# hijas).
.dl_consolidado_sha_pob <- function(man) {
  t <- Filter(function(x) identical(x$tabla, "poblacion"), man$inputs$tablas %||% list())
  if (!length(t)) NULL else t[[1]]$sha256
}

# Población congelada de una corrida seleccionada (inputs/poblacion.csv). En una suma, la de la primera hija de
# inputs.runs_hijas, después de comprobar que todas las hijas declaran el mismo sha256 de su tabla poblacion.
.dl_consolidado_poblacion <- function(fila) {
  man <- fila$manifest[[1]]
  sha_pob <- function(m) .dl_consolidado_sha_pob(m) %||% NA_character_
  dir_pob <- fila$dir
  if (identical(fila$agregacion, "suma_de_hijas")) {
    hijas <- unlist(man$inputs$runs_hijas)
    if (!length(hijas))
      .dl_stop("la suma %s no declara inputs.runs_hijas en su manifiesto", fila$run_id)
    base_mod <- dirname(fila$dir)
    shas <- vapply(hijas, function(h) sha_pob(.dl_leer_manifest(file.path(base_mod, h),
                                                               sprintf("la corrida hija %s de la suma", h))), "")
    if (anyNA(shas) || length(unique(shas)) != 1L)
      .dl_stop("la poblaci\u00f3n de las hijas de %s no es la misma (sha256 %s)",
               fila$run_id, paste(shas, collapse = " / "))
    dir_pob <- file.path(base_mod, hijas[1])
  }
  p <- file.path(dir_pob, "inputs", "poblacion.csv")
  if (!file.exists(p))
    .dl_stop("la corrida no tiene su poblaci\u00f3n congelada (inputs/poblacion.csv): %s", p)
  data.table::fread(p, colClasses = list(character = "location_id"))
}

# Filas en número de casos (metric_id 1) = tasa x población de la celda; una tasa por 100 000 se lleva antes a
# proporción. La población es fija: el intervalo del conteo es el de la tasa escalada (limitación declarada en el
# manifiesto).
#' Conteos de un consolidado
#'
#' Agrega a la tabla canónica las filas en número de casos (métrica 1): tasa x población de la celda, con la
#' población congelada en los insumos de cada corrida.
#'
#' @details
#' Una tasa por 100 000 se lleva antes a proporción. La población es fija: el intervalo de un conteo es el de su tasa
#' escalada, sin incertidumbre de población (el manifiesto del consolidado lo declara como limitación). En una suma
#' de hijas se usa la población de las hijas, que debe ser la misma en todas.
#'
#' @param celdas Tabla canónica de [dl_consolidado_canonico()].
#' @param seleccion Selección de [dl_consolidado_seleccionar()].
#' @return Tabla (data.table) con las columnas de `celdas`: sus filas de tasas y, por cada una, una fila de conteos
#'   (`metric_id` 1, `metric_name` `"Number"`), ordenadas por causa, año, medida, métrica, ubicación, sexo y banda.
#'   Atributo `poblacion_acquisition_id`: la procedencia de la población usada.
#' @seealso [dl_consolidado_canonico()] (el paso anterior), [dl_perfil_proyectar()] (el siguiente) y
#'   [dl_consolidar()] (todo en una llamada).
#' @family consolidado
#' @examples
#' # El ejemplo de ?dl_consolidar arma un registro de corridas y corre todos los pasos. Este es
#' # el tercero, con la tabla canónica y la selección:
#' # celdas <- dl_consolidado_conteos(canonica, sel)
#' # attr(celdas, "poblacion_acquisition_id")
#' @export
dl_consolidado_conteos <- function(celdas, seleccion) {
  sel <- seleccion
  # la población de un año es la misma para todas las corridas de ese año: se lee una vez por sha256 declarado en el
  # manifiesto (la clave que .dl_consolidado_poblacion ya compara entre hijas), no una vez por corrida
  memo_pob <- new.env(parent = emptyenv())
  bloques <- lapply(seq_len(nrow(sel)), function(i) {
    k <- .dl_consolidado_sha_pob(sel$manifest[[i]]) %||% sel$dir[i]
    if (is.null(memo_pob[[k]])) memo_pob[[k]] <- .dl_consolidado_poblacion(sel[i])
    pobl <- memo_pob[[k]]
    ce <- celdas[cause_id == sel$cause_id[i] & year == sel$year[i]]
    m <- merge(ce, pobl[, list(location_id, sex_id, age_group_id, poblacion = val, acq = acquisition_id)],
               by = c("location_id", "sex_id", "age_group_id"), all.x = TRUE)
    if (anyNA(m$poblacion)) {
      f <- m[is.na(poblacion)][1]
      .dl_stop("celda sin poblaci\u00f3n en %s (location_id %s, sex_id %d, age_group_id %d)",
               sel$run_id[i], f$location_id, f$sex_id, f$age_group_id)
    }
    m
  })
  m <- data.table::rbindlist(bloques)
  esc <- .DL_MEDIDAS$escala_out[match(m$measure_id, .DL_MEDIDAS$measure_id)]
  n <- data.table::copy(m)[, `:=`(val = val / esc * poblacion, lower = lower / esc * poblacion,
                                  upper = upper / esc * poblacion, metric_id = 1L, metric_name = "Number")]
  acq <- unique(m$acq)
  out <- data.table::rbindlist(list(m, n))[, `:=`(poblacion = NULL, acq = NULL)]
  data.table::setcolorder(out, names(celdas))
  data.table::setorder(out, cause_id, year, measure_id, metric_id, location_id, sex_id, age_group_id)
  data.table::setattr(out, "poblacion_acquisition_id", acq)
  out
}

# ---- Perfil de columnas (inst/perfiles/perfil_v<n>.yaml): la forma de las tablas se declara, no se programa ----
# Columnas derivadas que un perfil puede pedir además de las del contrato: cause_name_es (nombre_es del maestro),
# run_fuente, poblacion y las etiquetas en español de medida, métrica y grupo de edad (etiquetas_es.csv de la
# carpeta `registro`, el único lugar donde vive esa traducción).
.DL_PERFIL_DERIVADAS <- c("cause_name_es", "run_fuente", "poblacion", "measure_name_es", "metric_name_es",
                          "age_group_name_es")
.DL_PERFIL_ETIQUETAS <- c(measure_name_es = "measure", metric_name_es = "metric", age_group_name_es = "age_group")

# Etiqueta en español de cada id de la tabla `cual` (error si un id no está declarado: nunca se inventa una etiqueta).
.dl_etiqueta_es <- function(etiquetas, cual, id) {
  # data.frame, no data.table: dentro de `[` un argumento llamado como una columna se resolvería a la columna
  e <- as.data.frame(etiquetas); e <- e[e$tabla == cual, ]
  u <- unique(id)                                     # una búsqueda por id, no por fila
  out <- e$name_es[match(as.character(u), e$id)]
  if (anyNA(out))
    .dl_stop("etiquetas_es.csv no tiene la etiqueta \u00ab%s\u00bb de los id %s", cual,
             paste(u[is.na(out)], collapse = ", "))
  out[match(id, u)]
}
# Nombre del archivo de una medida según archivos.nombre del perfil ({measure_slug} o {measure_slug_es}).
.dl_perfil_nombre_archivo <- function(perfil, slug, etiquetas = NULL) {
  nombre <- perfil$archivos$nombre
  if (grepl("{measure_slug_es}", nombre, fixed = TRUE)) {
    if (is.null(etiquetas))
      .dl_stop("archivos.nombre del perfil usa {measure_slug_es} y no hay etiquetas_es.csv")
    e <- as.data.frame(etiquetas); m <- e[e$tabla == "measure" & e$slug == slug, ]
    if (nrow(m) != 1L || !nzchar(m$slug_es))
      .dl_stop("etiquetas_es.csv no tiene slug_es para la medida %s", slug)
    nombre <- sub("{measure_slug_es}", m$slug_es, nombre, fixed = TRUE)
  }
  sub("{measure_slug}", slug, nombre, fixed = TRUE)
}

#' Leer un perfil de columnas
#'
#' Lee y valida un perfil de columnas del consolidado: medidas, métricas, columnas de salida con su origen y
#' formato, y el nombre de los archivos.
#'
#' @details
#' Un perfil declara la forma de las tablas de `tablas/` de un consolidado, sin programarla: en YAML, `perfil` (su
#' identificador), `descripcion`, `archivos` (`por: measure`, una tabla por medida, y `nombre`, con
#' `{measure_slug}` o `{measure_slug_es}`), `medidas` (entre `prevalence`, `incidence` y `yld`), `metrics` (1
#' número de casos, 2 porcentaje, como proporción, 3 tasa por 100 000) y `columnas`: cada una con su `salida`, su
#' `origen` (una columna de la tabla canónica o una derivada: `cause_name_es`, `run_fuente`, `poblacion`,
#' `measure_name_es`, `metric_name_es` o `age_group_name_es`) y, si hace falta, un `formato` (`texto` o
#' `{decimales: n}`). Para otra forma de tabla basta copiar un perfil del paquete y editarlo.
#'
#' @param path Archivo YAML del perfil. El paquete trae dos: `system.file("perfiles", "perfil_v1.yaml", package =
#'   "dismodlite")` (columnas en español, nombres de medida, métrica y edad del catálogo) y `perfil_v2.yaml` (todo en
#'   español).
#' @return Lista con `id` (el identificador del perfil), `path` (la ruta del archivo), `sha256` (su huella, que
#'   registra el manifiesto del consolidado), `descripcion`, `archivos`, `medidas`, `metrics` (enteros) y `columnas`
#'   (una lista con `salida`, `origen` y `formato` de cada columna).
#' @seealso [dl_perfil_proyectar()] (que lo aplica) y [dl_consolidar()].
#' @family consolidado
#' @examples
#' perfil <- dl_perfil_cargar(system.file("perfiles", "perfil_v2.yaml", package = "dismodlite"))
#' perfil$descripcion
#' perfil$medidas
#' # columna de salida <- columna de origen en la tabla canónica
#' vapply(perfil$columnas, function(cn) paste(cn$salida, "<-", cn$origen), "")
#' @export
dl_perfil_cargar <- function(path) {
  if (!.dl_es_texto1(path) || !file.exists(path))
    .dl_stop(paste0("no existe el archivo del perfil de columnas \u00ab%s\u00bb: el perfil es la ruta de un archivo ",
                    "YAML. Los del paquete: system.file(\"perfiles\", \"perfil_v1.yaml\", package = \"dismodlite\") ",
                    "(o perfil_v2.yaml)"), paste(format(path), collapse = " "))
  error <- function(...) .dl_stop("el perfil de columnas %s %s", basename(path), paste0(...))
  p <- .dl_leer_yaml(path)
  canon <- .dl_cols_estimates_mod(.dl_schema_estimates(), "cause")
  if (!is.character(p$perfil) || !nzchar(p$perfil)) error("no declara `perfil` (su identificador)")
  if (!identical(p$archivos$por, "measure"))
    error("tiene archivos.por distinto de \u00abmeasure\u00bb (el \u00fanico admitido)")
  if (!is.character(p$archivos$nombre) || !grepl("\\{measure_slug(_es)?\\}", p$archivos$nombre))
    error("tiene archivos.nombre sin {measure_slug} ni {measure_slug_es}")
  medidas <- unlist(p$medidas); ok <- .DL_MEDIDAS_EXPORTA$slug
  if (!length(medidas) || !all(medidas %in% ok))
    error("tiene medidas fuera de ", paste(ok, collapse = "/"), ": ", paste(setdiff(medidas, ok), collapse = ", "))
  metrics <- as.integer(unlist(p$metrics))
  if (!length(metrics) || !all(metrics %in% 1:3)) error("tiene metrics fuera de {1, 2, 3}")
  cols <- p$columnas
  if (!length(cols)) error("no tiene columnas")
  salidas <- vapply(cols, function(cn) cn$salida %||% "", "")
  origenes <- vapply(cols, function(cn) cn$origen %||% "", "")
  if (any(!nzchar(salidas)) || any(!nzchar(origenes))) error("tiene una columna sin salida u origen")
  if (anyDuplicated(salidas)) error("repite la salida ", salidas[duplicated(salidas)][1])
  malos <- setdiff(origenes, c(canon, .DL_PERFIL_DERIVADAS))
  if (length(malos)) error("tiene un origen desconocido: ", paste(malos, collapse = ", "))
  for (cn in cols) {
    f <- cn$formato
    if (!is.null(f) && !identical(f, "texto") && is.null(f$decimales))
      error("tiene en la columna ", cn$salida, " un formato que no es \u00abtexto\u00bb ni {decimales: n}")
  }
  list(id = p$perfil, path = normalizePath(path, winslash = "/"), sha256 = digest::digest(file = path, algo = "sha256"),
       descripcion = p$descripcion %||% "", archivos = p$archivos, medidas = medidas, metrics = metrics,
       columnas = cols)
}

# Proyecta la tabla canónica (con run_fuente y, si el perfil la pide, poblacion) a la forma del perfil: una tabla por
# medida, nombrada por el slug de .DL_MEDIDAS. `etiquetas` (etiquetas_es.csv) es obligatoria si el perfil pide alguna
# derivada *_name_es.
#' Llevar la tabla canónica a la forma de un perfil
#'
#' Arma las tablas de `tablas/` de un consolidado: toma de la tabla canónica las métricas del perfil, agrega las
#' columnas derivadas que pide (nombres en español de la causa, la medida, la métrica y la edad), elige y renombra
#' las columnas y aplica sus formatos. Una columna de salida con `NA` (salvo los valores) es un error.
#'
#' @param celdas Tabla canónica (con `run_fuente`), como la de [dl_consolidado_conteos()].
#' @param perfil Perfil de [dl_perfil_cargar()].
#' @param master Tabla con `cause_id` y `nombre_es` (nombres en español de las causas).
#' @param etiquetas Etiquetas en español de medidas, métricas y edades (`etiquetas_es.csv` del registro);
#'   obligatorias si el perfil pide columnas `*_name_es`.
#' @return Lista de tablas (data.table), una por medida del perfil, con el nombre de la medida (`prevalence`,
#'   `incidence`, `yld`) y las columnas de salida del perfil, en su orden.
#' @seealso [dl_perfil_cargar()], [dl_consolidado_conteos()] (el paso anterior) y [dl_consolidar()] (todo en una
#'   llamada).
#' @family consolidado
#' @examples
#' # El ejemplo de ?dl_consolidar arma un registro de corridas y corre todos los pasos. Este es
#' # el último, con la tabla de dl_consolidado_conteos():
#' # tablas <- dl_perfil_proyectar(celdas, dl_perfil_cargar(perfil), master, etiquetas)
#' # names(tablas)
#' @export
dl_perfil_proyectar <- function(celdas, perfil, master, etiquetas = NULL) {
  ce <- celdas[metric_id %in% perfil$metrics]           # solo las métricas del perfil: una tabla nueva
  origenes <- vapply(perfil$columnas, function(cn) cn$origen, "")
  salidas <- vapply(perfil$columnas, function(cn) cn$salida, "")
  for (der in intersect(origenes, names(.DL_PERFIL_ETIQUETAS))) {
    if (is.null(etiquetas))
      .dl_stop("el perfil pide %s y no se pasaron las etiquetas (etiquetas_es.csv de la carpeta `registro`)", der)
    id_col <- sub("_name_es$", "_id", der)
    ce[, (der) := .dl_etiqueta_es(etiquetas, .DL_PERFIL_ETIQUETAS[[der]], get(id_col))]
  }
  if ("cause_name_es" %in% origenes) {
    nm <- master$nombre_es[match(ce$cause_id, master$cause_id)]
    if (anyNA(nm) || any(!nzchar(nm)))
      .dl_stop("master_gbd.csv no tiene nombre_es para las causas %s",
               paste(unique(ce$cause_id[is.na(nm) | !nzchar(nm)]), collapse = ", "))
    ce[, cause_name_es := nm]
  }
  faltan <- setdiff(origenes, names(ce))
  if (length(faltan))
    .dl_stop("la tabla can\u00f3nica no trae las columnas %s", paste(faltan, collapse = ", "))
  data.table::setorder(ce, cause_id, year, measure_id, metric_id, location_id, sex_id, age_group_id)
  con_valor <- salidas[origenes %in% c("val", "lower", "upper")]
  proyectar <- function(dt) {
    out <- dt[, origenes, with = FALSE]
    data.table::setnames(out, salidas)
    for (cn in perfil$columnas) {
      f <- cn$formato
      if (identical(f, "texto")) out[, (cn$salida) := as.character(get(cn$salida))]
      else if (!is.null(f$decimales)) out[, (cn$salida) := round(as.numeric(get(cn$salida)), as.integer(f$decimales))]
    }
    for (cn in setdiff(names(out), con_valor))
      if (anyNA(out[[cn]])) .dl_stop("NA en la columna de salida %s", cn)
    out
  }
  stats::setNames(lapply(perfil$medidas, function(slug) proyectar(ce[measure_id == .dl_medida_id(slug)])),
                  perfil$medidas)
}

# ---- Orquestador: mod/consolidado/<run_id>/{canonico/, tablas/, manifest.yaml} y registro ----

# Bloque del manifiesto por (causa, año): qué corrida alimenta sus celdas, con qué comprobaciones y sustituciones.
.dl_consolidado_bloque <- function(fila) {
  man <- fila$manifest[[1]]
  list(cause_id = fila$cause_id, year = fila$year, run_fuente = fila$run_id,
       # año del ancla de la corrida: el propio año, salvo en una proyección declarada (years.ancla)
       anio_ancla = .dl_man_anio_ancla(man, fila$year),
       agregacion = if (is.na(fila$agregacion)) "fit" else fila$agregacion,
       cascada_modo = man$cascada$modo %||% "ninguna",
       sustituciones = man$cascada$sustituciones %||% list(),
       hijas_omitidas = man$causa$hijas_omitidas %||% list(),
       force = isTRUE(man$validacion$gates$force),
       gate_ancla = man$validacion$anchor_identity$gate_err_mediano %||% NA_real_,
       rhat_max = man$validacion$gates$rhat_max %||% NA_real_)
}

# Nombres en español de las causas: master_gbd.csv más, si existe, el archivo de las causas de nivel 4 (que no tienen
# fila en el maestro). Devuelve (cause_id, nombre_es).
.dl_consolidado_nombres_es <- function(master_path, nombres_nivel4_path) {
  m <- .dl_master_leer(maestro = master_path)[, list(cause_id, nombre_es)]
  if (!is.null(nombres_nivel4_path) && file.exists(nombres_nivel4_path)) {
    n4 <- .dl_leer_csv(nombres_nivel4_path, colClasses = list(character = "nombre_es"))[, list(cause_id, nombre_es)]
    m <- data.table::rbindlist(list(m, n4[!(cause_id %in% m$cause_id)]))
  }
  m
}

# ¿La corrida (o alguna de sus hijas, si es una suma) descontó la fase aguda del csmr?
.dl_con_fraccion_aguda <- function(man) {
  fa <- c(.dl_man_fraccion_aguda(man),
          vapply(man$causa$hijas %||% list(), function(h) as.numeric(h$csmr_fraccion_aguda %||% 0), 0))
  any(fa > 0)
}

# Limitaciones del manifiesto de un consolidado, armadas con los hechos de la selección (qué causas van por suma,
# qué hijas quedaron fuera, qué años son proyección, qué corridas descontaron la fase aguda), sin texto fijo sobre
# una causa o un país. Las limitaciones de cada corrida quedan en su propio manifiesto.
.dl_consolidado_limitaciones <- function(sel, bloques, acq_pob) {
  lista <- function(x) paste(x, collapse = ", ")
  causas_suma <- sort(unique(sel[agregacion %in% "suma_de_hijas"]$cause_id))
  omitidas <- sort(unique(unlist(lapply(bloques, function(b)
    vapply(b$hijas_omitidas, function(o) as.integer(o$cause_id), 0L)))))
  proyectados <- sort(unique(vapply(Filter(function(b) b$anio_ancla != b$year, bloques), function(b) b$year, 0L)))
  con_aguda <- sort(unique(sel$cause_id[vapply(sel$manifest, .dl_con_fraccion_aguda, logical(1))]))
  c(list(
    sprintf(paste0("conteos = tasa x poblaci\u00f3n congelada en los insumos de cada corrida (%s) \u2014 sin ",
                   "incertidumbre de poblaci\u00f3n; el intervalo de los conteos es el de la tasa escalada"),
            lista(acq_pob))),
    if (length(causas_suma)) list(sprintf(paste0(
      "causa(s) %s como suma de sus hijas, simulaci\u00f3n a simulaci\u00f3n \u2014 ", .DL_LIMITACION_CORRELACION_HIJAS,
      "%s"), lista(causas_suma),
      if (length(omitidas)) sprintf("; hijas %s omitidas donde el bloque lo declara", lista(omitidas)) else "")),
    if (length(proyectados)) list(sprintf(paste0(
      "a\u00f1o(s) %s proyectado(s) \u2014 el ancla es la de un a\u00f1o anterior reetiquetada ",
      "(bloques[].anio_ancla)"), lista(proyectados))),
    if (length(con_aguda)) list(sprintf(paste0(
      "causa(s) %s con la fase aguda descontada del csmr \u2014 su incidencia es ", .DL_LIMITACION_INCIDENCIA_AGUDA,
      " (ver el manifiesto de cada run_fuente)"), lista(con_aguda))),
    list(paste0("las limitaciones de cada corrida est\u00e1n en su manifiesto (run_fuente) y no se copian ",
                "aqu\u00ed")))
}

# Comentario de cabecera del manifiesto de un consolidado: causas y años presentes, y el perfil de las tablas.
.dl_consolidado_encabezado <- function(sel, perf) {
  causas <- sort(unique(sel$cause_id)); anios <- sort(unique(sel$year))
  c(sprintf("# manifiesto de un consolidado mod/%s: %d causa(s) (%s) y %d a\u00f1o(s) (%s)", .DL_METODO_CONSOLIDADO,
            length(causas), paste(causas, collapse = ", "), length(anios), paste(anios, collapse = ", ")),
    sprintf("# canonico/ con el contrato estimates/v1; tablas/ con la forma del perfil %s", perf$id))
}

# Todo en memoria y validado antes de tocar el disco; la escritura va a una carpeta temporal que se renombra al
# final, y el registro se toca después. Las corridas se leen de carpeta_corridas (= carpeta, salvo que estén en otra).
#' Consolidar corridas en tablas por medida
#'
#' Toma la corrida vigente de cada causa y año del registro, arma la tabla canónica (tasas y conteos), la valida y
#' la escribe en `<carpeta>/mod/consolidado/<AAAA-MM-DD>_<nombre>_v<n>/`: `canonico/` (la tabla canónica por
#' medida), `tablas/` (una tabla por medida con la forma que declara el perfil de columnas) y `manifest.yaml`.
#' El consolidado se agrega al registro de corridas solo si se da `registro_salida`.
#'
#' @details
#' Los pasos, que también son funciones: [dl_consolidado_seleccionar()] (la corrida vigente de cada causa y año),
#' [dl_consolidado_canonico()] (las celdas apiladas), [dl_consolidado_conteos()] (los conteos), la validación contra
#' el contrato `estimates/v1` ([dl_validar_estimaciones()]) y [dl_perfil_proyectar()] (las tablas del perfil). Todo se
#' valida en memoria antes de escribir, y la escritura va a una carpeta temporal que se renombra al final. El
#' manifiesto declara el perfil (con su sha256), la procedencia de la población, un bloque por causa y año (la
#' corrida de origen, si es una suma, sus compuertas) y las limitaciones del consolidado.
#'
#' Usa el registro de corridas y la carpeta del registro de causas (`master_gbd.csv` y `etiquetas_es.csv`): la de un
#' proyecto la arma [dl_proyecto()] (`dl_proyecto(...)$rutas$registry`); la de un proyecto en el formato completo es
#' su carpeta `registro`.
#'
#' @param registro Archivo YAML del registro de corridas.
#' @param carpeta Carpeta raíz donde se escribe el consolidado.
#' @param perfil Archivo YAML del perfil de columnas; el paquete trae `system.file("perfiles", "perfil_v1.yaml",
#'   package = "dismodlite")` y `perfil_v2.yaml` (ver [dl_perfil_cargar()]).
#' @param maestro Archivo `master_gbd.csv` con los nombres en español de las causas. Debe ser el de `rutas` (el
#'   `master_gbd.csv` de la carpeta `registro` de [dl_rutas()]), con el que se comprueban las sumas de hijas.
#' @param rutas Rutas de [dl_rutas()]; se usan el registro (la carpeta de tablas de referencia con
#'   `master_gbd.csv` y `etiquetas_es.csv`, no el registro de corridas) y los catálogos. Hay que darlas (por
#'   ejemplo `dl_rutas_ejemplo(9100)` con los datos de ejemplo).
#' @param causas,anios Causas y años a consolidar (`NULL`: todos los del registro).
#' @param permitir_huecos `TRUE` acepta pares (causa, año) sin corrida y los declara en el manifiesto.
#' @param registrar `TRUE` agrega el consolidado al registro `registro_salida`.
#' @param nombre Nombre corto del consolidado: minúsculas sin tildes, números y guiones.
#' @param registro_salida Archivo YAML del registro donde se anota el consolidado (opcional).
#' @param carpeta_corridas Carpeta raíz donde están las corridas (por defecto, `carpeta`).
#' @param nombres_nivel4 Archivo con los nombres en español de las causas de nivel 4 (opcional).
#' @return Objeto de clase `dl_run` del consolidado, con `run_id`, `dir` (su carpeta), `manifest` (el contenido de
#'   `manifest.yaml`) y `files` (las tablas de `canonico/`, con su ruta, su sha256 y su número de filas).
#' @seealso [dl_sumar_hijas()] (las sumas que exige), [dl_exportar_corrida()] (su argumento `registro`) y
#'   [dl_perfil_cargar()] (los perfiles de columnas).
#' @family consolidado
#' @examples
#' \donttest{
#' # una corrida de prueba de la causa 9101 del ejemplo (formato completo), anotada en un
#' # registro de corridas nuevo
#' completo <- system.file("extdata", "acs_peru_completo", package = "dismodlite")
#' salida <- file.path(tempdir(), "consolidado")
#' dir.create(salida)
#' registro <- file.path(salida, "registro_corridas.yaml")
#' writeLines("datasets: []", registro)
#' run <- suppressMessages(dl_correr(completo, causa = 9101, semilla = 1, rapido = TRUE,
#'                                   sensibilidad = FALSE, carpeta_salida = salida,
#'                                   registro = registro))
#' rutas <- dl_rutas_ejemplo(9101, formato = "completo")
#' perfil <- system.file("perfiles", "perfil_v2.yaml", package = "dismodlite")
#' maestro <- file.path(completo, "registro", "master_gbd.csv")
#'
#' # el consolidado en una llamada
#' cons <- dl_consolidar(registro, salida, perfil = perfil, maestro = maestro, rutas = rutas)
#' cons
#' list.files(cons$dir, recursive = TRUE)
#' # los códigos de ubicación se leen como texto, para conservar el cero inicial
#' prev <- read.csv(file.path(cons$dir, "tablas", "prevalencia.csv"),
#'                  colClasses = c(ubigeo = "character"), fileEncoding = "UTF-8")
#' head(prev)
#'
#' # los mismos pasos, uno por uno
#' sel <- dl_consolidado_seleccionar(registro, salida, rutas = rutas)
#' sel[, c("cause_id", "year", "run_id", "agregacion")]
#' canonica <- dl_consolidado_canonico(sel, paste0(Sys.Date(), "_consolidado_v1"))
#' table(canonica$measure_name, canonica$metric_name)
#' celdas <- dl_consolidado_conteos(canonica, sel)
#' attr(celdas, "poblacion_acquisition_id")
#' # los nombres en español de la causa (las de nivel 4, como 9101, en causas_nivel4_es.csv)
#' master <- read.csv(file.path(completo, "registro", "causas_nivel4_es.csv"), fileEncoding = "UTF-8")
#' etiquetas <- read.csv(file.path(completo, "registro", "etiquetas_es.csv"),
#'                       colClasses = "character", fileEncoding = "UTF-8")
#' tablas <- dl_perfil_proyectar(celdas, dl_perfil_cargar(perfil), master, etiquetas)
#' names(tablas)
#' head(tablas$prevalence)
#' unlink(salida, recursive = TRUE)
#' }
#' @export
dl_consolidar <- function(registro, carpeta, perfil, maestro, rutas = dl_rutas(), causas = NULL, anios = NULL,
                          permitir_huecos = FALSE, registrar = !is.null(registro_salida), nombre = "consolidado",
                          registro_salida = NULL, carpeta_corridas = carpeta,
                          nombres_nivel4 = file.path(dirname(maestro), "causas_nivel4_es.csv")) {
  .dl_exigir_registro(registrar, registro_salida, "registro_salida")
  .dl_exigir_nombre(nombre)
  .dl_exigir_carpeta(carpeta, "la carpeta donde se escribe el consolidado")
  rutas <- .dl_resolver_rutas(rutas)
  perf <- dl_perfil_cargar(perfil)
  master <- .dl_consolidado_nombres_es(maestro, nombres_nivel4)
  etiquetas <- .dl_etiquetas_es(rutas)
  sel <- dl_consolidado_seleccionar(registro, carpeta_corridas, causas = causas, anios = anios,
                                    permitir_huecos = permitir_huecos, rutas = rutas)
  run_id <- .dl_run_id(carpeta, nombre, method = .DL_METODO_CONSOLIDADO)
  celdas <- dl_consolidado_conteos(dl_consolidado_canonico(sel, run_id), sel)
  acq_pob <- attr(celdas, "poblacion_acquisition_id")
  canon <- celdas[, !"run_fuente"]
  dl_validar_estimaciones(canon, rutas)
  tablas <- dl_perfil_proyectar(celdas, perf, master, etiquetas)
  huecos <- attr(sel, "huecos")
  bloques <- lapply(seq_len(nrow(sel)), function(i) .dl_consolidado_bloque(sel[i]))
  man <- c(.dl_manifiesto_base(run_id, .DL_METODO_CONSOLIDADO, as.character(celdas$round[1])), list(
    # el archivo, no su ruta: una ruta larga con espacios no cabe en una línea del manifiesto
    perfil = list(id = perf$id, archivo = basename(perf$path), sha256 = perf$sha256),
    poblacion = list(acquisition_id = acq_pob),
    params = list(years = as.list(sort(unique(sel$year))), causas = as.list(sort(unique(sel$cause_id))),
                  n_bloques = nrow(sel)),
    bloques = bloques,
    huecos = if (nrow(huecos)) lapply(seq_len(nrow(huecos)), function(i)
      list(cause_id = huecos$cause_id[i], year = huecos$year[i])) else list(),
    limitaciones = .dl_consolidado_limitaciones(sel, bloques, acq_pob)))
  .dl_yaml_block(man)   # el emisor falla aquí, antes de escribir, si un escalar no cabe
  dir_run <- .dl_dir_corrida(carpeta, run_id, .DL_METODO_CONSOLIDADO)
  dir.create(dirname(dir_run), recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(dirname(dir_run), paste0(".", run_id, ".tmp")); unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  archivos <- .dl_escribir_particiones(canon, tmp, run_id, method = .DL_METODO_CONSOLIDADO, sub = "canonico")
  dir.create(file.path(tmp, "tablas"), showWarnings = FALSE)
  for (slug in names(tablas)) {
    archivo <- .dl_perfil_nombre_archivo(perf, slug, etiquetas)
    data.table::fwrite(tablas[[slug]], file.path(tmp, "tablas", archivo), eol = "\n")
  }
  man$files <- archivos
  writeLines(c(.dl_consolidado_encabezado(sel, perf), sub("\n$", "", .dl_yaml_block(man))),
             file.path(tmp, "manifest.yaml"))
  if (!file.rename(tmp, dir_run))
    .dl_stop("no se pudo renombrar la carpeta temporal %s a %s", tmp, dir_run)
  on.exit(NULL)
  .dl_corrida_escrita(man, dir_run, if (registrar) registro_salida)
}
