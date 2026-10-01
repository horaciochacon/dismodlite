# dl_sumar_hijas(): una causa padre que no se ajusta se reporta como la suma, simulación a simulación, de las
# corridas ya exportadas de sus causas hijas (las que declara master_gbd.csv de la carpeta `registro`).
# - Entrada: la carpeta de cada hija, con draws/<medida>_<año>.csv.gz y manifest.yaml.
# - Todo se comprueba en memoria antes de escribir: mismas hijas que el maestro, misma causa de extracción, mismo
#   año, año del ancla, ronda y número de simulaciones, y las mismas celdas.
# - Salida: una corrida nueva con cause/<medida>/, draws/ y el manifiesto (causa.hijas, causa.nombre_es); el registro
#   de corridas se toca al final. No hay ajuste propio: la corrida no tiene etiquetas/, diagnostics/ ni inputs/.
# - El intervalo sale de los cuantiles de la suma de simulaciones, nunca de sumar límites. La correlación entre hijas
#   no se modela: cada hija viene de su propio ajuste y se suman las simulaciones del mismo índice.

# Corrida de una hija: su manifiesto, su año y sus simulaciones por medida (tablas anchas ordenadas por celda).
.dl_leer_run_hija <- function(dir_run) {
  man <- .dl_leer_manifest(dir_run, "la carpeta de la corrida hija")
  anio <- as.integer(man$params$year)
  draws <- lapply(stats::setNames(nm = .DL_MEDIDAS_EXPORTA$slug), function(s) .dl_leer_draws(dir_run, s, anio))
  list(dir = dir_run, run_id = man$run_id %||% basename(dir_run), man = man, anio = anio, draws = draws,
       n_draws = sum(grepl("^draw_", names(draws[[1]]))))
}

# Comprobaciones antes de sumar: las hijas son las del maestro, su causa de extracción es el padre, y coinciden en
# año, año del ancla, ronda, número de simulaciones y celdas.
.dl_chequear_hijas <- function(hijas, cause_id, hijos_master) {
  ids <- sort(vapply(hijas, function(h) as.integer(h$man$causa$cause_id), 0L))
  if (!identical(ids, hijos_master))
    .dl_stop("las hijas de %d en master_gbd.csv son %s y las corridas traen %s",
             cause_id, paste(hijos_master, collapse = "+"), paste(ids, collapse = "+"))
  for (h in hijas) {
    ext <- as.integer(h$man$causa$extraction_cause_id %||% NA_integer_)
    if (is.na(ext) || ext != cause_id)
      .dl_stop("la corrida %s declara extraction_cause_id %s y la causa padre es %d", h$run_id, ext, cause_id)
  }
  campo <- function(nm, f) {
    v <- vapply(hijas, f, "")
    if (length(unique(v)) > 1L)
      .dl_stop("las hijas no coinciden en %s: %s", nm, paste(v, collapse = " / "))
  }
  campo("a\u00f1o", function(h) as.character(h$anio))
  # proyección declarada: sin el campo, el ancla es la del propio año; una hija con el ancla de otro año no se suma
  # con otra ajustada a su año
  campo("a\u00f1o del ancla", function(h) as.character(.dl_man_anio_ancla(h$man, h$anio)))
  campo("ronda (round)", function(h) as.character(h$man$round))
  campo("n\u00famero de simulaciones", function(h) as.character(h$n_draws))
  ref <- hijas[[1]]$draws
  for (h in hijas[-1]) for (s in names(ref)) {
    a <- ref[[s]][, list(location_id, sex_id, age_group_id)]
    b <- h$draws[[s]][, list(location_id, sex_id, age_group_id)]
    if (!isTRUE(all.equal(a, b, check.attributes = FALSE)))
      .dl_stop("las celdas de %s no son las mismas en %s y en %s", s, hijas[[1]]$run_id, h$run_id)
  }
  invisible(TRUE)
}

# Fila del padre en master_gbd.csv: nombre en español e hijas declaradas (columna hijos, «a|b|c»).
.dl_master_padre <- function(paths, id) {
  m <- .dl_master_leer(paths)
  k <- match(id, m$cause_id)
  if (is.na(k) || !nzchar(m$nombre_es[k]))
    .dl_stop("master_gbd.csv no tiene fila o nombre_es para la causa %s", id)
  list(nombre_es = m$nombre_es[k], hijos = sort(as.integer(strsplit(m$hijos[k], "|", fixed = TRUE)[[1]])))
}

# cause_name de la causa en el ancla de prevalencia (un archivo o una carpeta de CSV); se leen solo las dos columnas
# que hacen falta.
.dl_cause_name_std <- function(paths, id) {
  s <- .dl_leer_std(.dl_path(paths, "std_prior"), paths$registry, select = c("cause_id", "cause_name"))
  v <- unique(s[cause_id == id]$cause_name)
  if (length(v) != 1L)
    .dl_stop(paste0("el ancla de prevalencia no trae un cause_name \u00fanico para la causa %d (trae %d); ",
                    "p\u00e1salo en `nombre_causa`"), id, length(v))
  v
}

# Limitaciones del manifiesto de una suma, armadas con lo que hizo la suma (hijas sumadas y omitidas, año del ancla,
# hijas con la fase aguda descontada). Cada condición aporta siempre el mismo número de limitaciones. Los textos no
# llevan «: » (el manifiesto los emite sin comillas).
.dl_limitaciones_suma <- function(ids_hijas, omitidas, anio, anio_ancla, fraccion_aguda) {
  c(list(
    sprintf(paste0("suma simulaci\u00f3n a simulaci\u00f3n de %d causas hijas (%s) \u2014 ",
                   .DL_LIMITACION_CORRELACION_HIJAS, " (cada hija viene de su propio ajuste)"),
            length(ids_hijas), paste(ids_hijas, collapse = ", ")),
    paste0("sin ajuste propio \u2014 la corrida de la suma no tiene etiquetas, diagn\u00f3sticos MCMC ni insumos ",
           "congelados (est\u00e1n en las corridas de las hijas, inputs.runs_hijas)")),
    lapply(omitidas %||% list(), function(o) sprintf("hija %d omitida de la suma \u2014 %s", as.integer(o$cause_id),
                                                      .dl_texto_yaml(o$motivo))),
    if (!identical(anio_ancla, as.integer(anio)))
      list(sprintf(paste0("proyecci\u00f3n declarada \u2014 las hijas llevan el ancla de %d reetiquetada a %d (ver ",
                          "las limitaciones de cada hija)"), anio_ancla, as.integer(anio))),
    if (any(fraccion_aguda > 0))
      list(sprintf(paste0("fase aguda descontada del csmr en la(s) hija(s) %s \u2014 su incidencia es ",
                          .DL_LIMITACION_INCIDENCIA_AGUDA, " del ancla, y as\u00ed entra en la suma (ver ",
                          "causa.hijas[].csmr_fraccion_aguda)"),
                   paste(ids_hijas[fraccion_aguda > 0], collapse = ", "))))
}

#' Sumar las corridas de las causas hijas
#'
#' Reporta una causa padre como la suma, simulación a simulación, de las corridas ya exportadas de sus hijas (las
#' que declara `master_gbd.csv` del registro), y escribe una corrida nueva con sus celdas, simulaciones y
#' manifiesto. El intervalo sale de los cuantiles de la suma de simulaciones.
#'
#' @details
#' En un proyecto simple, la causa padre declara sus hijas en `subtipos` (ver [dl_configuracion()]) y las rutas de
#' su proyecto, `dl_proyecto(carpeta, <padre>)$rutas`, traen ese registro. Cada hija se corre por separado (por
#' ejemplo con [dl_correr()]); antes de sumar se comprueba que las corridas sean de esas hijas, con el mismo año,
#' año del ancla, ronda de GBD, número de simulaciones y celdas (ubicaciones, sexos y bandas). La simulación k de la
#' suma es la suma de las simulaciones k de las hijas; la correlación entre hijas no se modela (cada una viene de su
#' propio ajuste). Una hija que no se corre se declara en `omitidas`, con su motivo; la suma queda por debajo de la
#' causa padre en su parte y el manifiesto lo declara como limitación. La corrida de la suma no tiene ajuste propio:
#' trae `cause/`, `draws/` y el manifiesto (con las corridas de las hijas en `causa.hijas`), sin etiquetas,
#' diagnósticos ni insumos.
#'
#' @inheritParams dl_exportar_corrida
#' @inheritParams dl_reresumir_corrida
#' @param corridas_hijas Carpetas de las corridas exportadas de las hijas.
#' @param causa Identificador de la causa padre.
#' @param nombre Nombre corto de la corrida nueva: minúsculas sin tildes, números y guiones.
#' @param nombre_causa Nombre de la causa padre en las celdas (por defecto, el del ancla de prevalencia).
#' @param rutas Rutas de [dl_rutas()]; se usan el registro (la carpeta de tablas de referencia con
#'   `master_gbd.csv`), los catálogos y el ancla. Hay que darlas (por ejemplo `dl_rutas_ejemplo(9100)` con los
#'   datos de ejemplo).
#' @param omitidas Hijas que no entran en la suma: lista de `list(cause_id = , motivo = )`. La suma queda por debajo
#'   de la causa padre en la parte de cada hija omitida, y el manifiesto lo declara con su motivo.
#' @return Objeto de clase `dl_run` de la corrida de la suma, con `run_id`, `dir`, `manifest` y `files` (como en
#'   [dl_exportar_corrida()]).
#' @seealso [dl_correr()] (las corridas de las hijas), [dl_configuracion()] (`subtipos`) y [dl_consolidar()] (que
#'   exige la suma para una causa con hijas).
#' @family corrida
#' @examples
#' \donttest{
#' # la causa 9100 del ejemplo es la suma de 9101, 9102 y 9103: una corrida de prueba de cada hija
#' salida <- file.path(tempdir(), "suma")
#' hijas <- vapply(c(9101, 9102, 9103), function(k) suppressMessages(
#'   dl_correr(dl_ejemplo(), causa = k, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
#'             carpeta_salida = salida))$dir, "")
#' basename(hijas)
#' suma <- dl_sumar_hijas(hijas, causa = 9100, nombre = "suma-ejemplo", carpeta = salida,
#'                        rutas = dl_proyecto(dl_ejemplo(), causa = 9100)$rutas)
#' suma
#' vapply(suma$manifest$causa$hijas, `[[`, "", "run_id")
#' unlink(salida, recursive = TRUE)
#' }
#' @export
dl_sumar_hijas <- function(corridas_hijas, causa, nombre, carpeta = Sys.getenv("DATA_ROOT"), nombre_causa = NULL,
                           nivel = 0.95, registrar = !is.null(registro), registro = NULL, rutas = dl_rutas(),
                           omitidas = NULL) {
  .dl_exigir_registro(registrar, registro)
  .dl_exigir_nivel(nivel, exportable = TRUE)
  if (missing(nombre)) .dl_stop("falta `nombre` (nombre corto de la corrida de la suma, p. ej. \"acs-suma\")")
  .dl_exigir_nombre(nombre)
  .dl_exigir_carpeta(carpeta)
  causa <- .dl_exigir_causa(causa)
  rutas <- .dl_resolver_rutas(rutas)
  padre <- .dl_master_padre(rutas, causa)
  omit_ids <- as.integer(vapply(omitidas %||% list(), function(o) as.integer(o$cause_id), 0L))
  if (length(omit_ids)) {
    fuera <- setdiff(omit_ids, padre$hijos)
    if (length(fuera))
      .dl_stop("`omitidas` trae causas que no son hijas de %d en master_gbd.csv: %s",
               causa, paste(fuera, collapse = ", "))
    if (any(!nzchar(vapply(omitidas, function(o) o$motivo %||% "", ""))))
      .dl_stop("cada hija de `omitidas` lleva su `motivo`: list(cause_id = ..., motivo = \"...\")")
  }
  hijas <- lapply(corridas_hijas, .dl_leer_run_hija)
  .dl_chequear_hijas(hijas, causa, setdiff(padre$hijos, omit_ids))
  anio <- hijas[[1]]$anio; ronda <- as.character(hijas[[1]]$man$round); n_draws <- hijas[[1]]$n_draws
  anio_ancla <- .dl_man_anio_ancla(hijas[[1]]$man, anio)      # igual en todas las hijas (.dl_chequear_hijas)
  fraccion_aguda <- vapply(hijas, function(h) .dl_man_fraccion_aguda(h$man), 0)
  if (is.null(nombre_causa)) nombre_causa <- .dl_cause_name_std(rutas, causa)

  # 1. Suma simulación a simulación por celda, en las unidades de las simulaciones guardadas (proporción o tasa por
  #    persona-año).
  cols_draw <- paste0("draw_", seq_len(n_draws))
  draws_suma <- lapply(stats::setNames(nm = .DL_MEDIDAS_EXPORTA$slug), function(s) {
    out <- hijas[[1]]$draws[[s]][, c("location_id", "sex_id", "age_group_id", cols_draw), with = FALSE]
    for (h in hijas[-1])
      out[, (cols_draw) := Map(`+`, .SD, h$draws[[s]][, cols_draw, with = FALSE]), .SDcols = cols_draw]
    out
  })

  # 2. Celdas del contrato (las mismas columnas que dl_resumir()), con los nombres de los catálogos.
  run_id <- .dl_run_id(carpeta, nombre)
  celdas <- .dl_con_run_id(.dl_celdas_contrato(lapply(draws_suma, .dl_stats_desde_ancho, nivel), rutas,
                                               ronda = ronda, anio = anio, causa = causa,
                                               nombre_causa = nombre_causa, nivel = nivel), run_id)
  dl_validar_estimaciones(celdas, rutas)

  # 3. Escritura: particiones, simulaciones de la suma, manifiesto; el registro al final.
  dir_run <- .dl_dir_corrida(carpeta, run_id)
  archivos <- .dl_escribir_particiones(celdas, dir_run, run_id)
  dir.create(file.path(dir_run, "draws"), showWarnings = FALSE)
  for (s in names(draws_suma)) .dl_fwrite_gz(draws_suma[[s]], .dl_archivo_draws(dir_run, s, anio))
  ids_hijas <- vapply(hijas, function(h) as.integer(h$man$causa$cause_id), 0L)
  man <- c(.dl_manifiesto_base(run_id, .DL_METODO_DISMOD_LITE, ronda), list(
    causa = list(cause_id = causa, cause_name = nombre_causa, nombre_es = padre$nombre_es,
                 agregacion = "suma_de_hijas",
                 hijas = lapply(seq_along(hijas), function(i) list(
                   cause_id = ids_hijas[i],
                   run_id = hijas[[i]]$run_id,
                   bundle_hash = hijas[[i]]$man$inputs$bundle_hash,
                   # fracción aguda descontada en la hija (0 si su manifiesto no la declara)
                   csmr_fraccion_aguda = fraccion_aguda[i])),
                 hijas_omitidas = lapply(omitidas %||% list(), function(o) list(cause_id = as.integer(o$cause_id)))),
    params = list(draws = as.integer(n_draws), year = as.integer(anio), anio_ancla = anio_ancla, ui_level = nivel,
                  estadistico_puntual = .DL_ESTADISTICO_PUNTUAL, version_paquete = dl_version()),
    inputs = list(runs_hijas = lapply(hijas, function(h) h$run_id)),
    files = archivos,
    limitaciones = .dl_limitaciones_suma(ids_hijas, omitidas, anio, anio_ancla, fraccion_aguda)))
  .dl_escribir_manifest(man, dir_run)
  .dl_corrida_escrita(man, dir_run, if (registrar) registro)
}
