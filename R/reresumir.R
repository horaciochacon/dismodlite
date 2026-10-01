# dl_reresumir_corrida(): vuelve a resumir una corrida exportada desde sus simulaciones en disco, sin volver a
# ajustar. Escribe una corrida nueva (mismo nombre, versión siguiente) con cause/<medida>/ recalculado; el resto de
# la carpeta de origen (draws/, inputs/, diagnostics/, etiquetas/) se enlaza con enlaces duros (o se copia si el
# sistema de archivos no los admite), y el manifiesto es el de origen con `resumen.run_origen`. La corrida de origen
# no se toca: las dos quedan activas y dl_consolidar() elige la más nueva.

# Enlaza (o copia) el árbol `desde` en `hasta`, archivo por archivo.
.dl_enlazar_arbol <- function(desde, hasta) {
  if (dir.exists(desde)) {
    dir.create(hasta, recursive = TRUE, showWarnings = FALSE)
    for (f in list.files(desde, all.files = TRUE, no.. = TRUE))
      .dl_enlazar_arbol(file.path(desde, f), file.path(hasta, f))
  } else if (!file.link(desde, hasta)) {
    if (!file.copy(desde, hasta))
      .dl_stop("no se pudo enlazar ni copiar %s", desde)
  }
  invisible(TRUE)
}

#' Volver a resumir una corrida exportada
#'
#' Recalcula las celdas de una corrida desde sus simulaciones guardadas, sin volver a ajustar, y escribe una
#' corrida nueva (mismo nombre, versión siguiente) que enlaza el resto de archivos de la original. La corrida
#' original no se toca.
#'
#' @details
#' Sirve para llevar una corrida escrita por una versión anterior del paquete al resumen de esta: las corridas
#' anteriores a la versión 0.2.2 daban como valor central la mediana de las simulaciones y las de ahora dan la
#' media (el intervalo no cambia); el manifiesto nuevo lo declara en `resumen` (`run_origen` y
#' `estadistico_anterior`) y, si el estadístico cambió, en `limitaciones`. Las celdas se recalculan desde `draws/`
#' (la corrida debe haberse escrito con `guardar_simulaciones = TRUE`); `draws/`, `inputs/`, `diagnostics/` y
#' `etiquetas/` se enlazan (o se copian, si el sistema de archivos no admite enlaces). Las dos corridas quedan
#' activas en el registro; [dl_consolidar()] elige la más nueva.
#'
#' @param corrida Carpeta de la corrida exportada (con `manifest.yaml` y `draws/`).
#' @inheritParams dl_exportar_corrida
#' @param nivel Nivel del intervalo de incertidumbre; debe ser 0.95, el que fija el contrato de las corridas.
#' @param rutas Rutas de [dl_rutas()] con la carpeta de catálogos, con que se validan las celdas. Hay que darlas
#'   (por ejemplo `dl_rutas_ejemplo(9100)` con los datos de ejemplo): la corrida no guarda los catálogos.
#' @return Objeto de clase `dl_run` de la corrida nueva, con `run_id`, `dir`, `manifest` y `files` (como en
#'   [dl_exportar_corrida()]) y `origen` (el `run_id` de la original).
#' @seealso [dl_exportar_corrida()], [dl_correr()].
#' @family corrida
#' @examples
#' \donttest{
#' salida <- file.path(tempdir(), "reresumen")
#' run <- dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
#'                  carpeta_salida = salida)
#' # las rutas del proyecto traen los catálogos con que se validan las celdas
#' nueva <- dl_reresumir_corrida(run$dir, carpeta = salida,
#'                               rutas = dl_proyecto(dl_ejemplo(), causa = 9100)$rutas)
#' nueva
#' nueva$origen
#' unlink(salida, recursive = TRUE)
#' }
#' @export
dl_reresumir_corrida <- function(corrida, carpeta = Sys.getenv("DATA_ROOT"), nivel = 0.95,
                                 registrar = !is.null(registro), registro = NULL, rutas = dl_rutas()) {
  .dl_exigir_registro(registrar, registro)
  .dl_exigir_nivel(nivel, exportable = TRUE)
  .dl_exigir_carpeta(carpeta)
  rutas <- .dl_resolver_rutas(rutas)
  man <- .dl_releer_manifest(corrida, "la carpeta de `corrida`")
  origen <- man$run_id
  anio <- as.integer(man$params$year)
  # versión siguiente en `carpeta`; si `carpeta` no es la del origen, también mayor que la del origen (si es de hoy)
  run_id <- .dl_run_id(carpeta, .dl_partes_run_id(origen, "de `corrida`")$nombre, otros = origen)

  # 1. Celdas: las del origen (nombres tal cual) con val, lower y upper recalculados desde las simulaciones de la
  #    misma celda.
  celdas <- data.table::rbindlist(lapply(seq_len(nrow(.DL_MEDIDAS_EXPORTA)), function(k) {
    md <- .DL_MEDIDAS_EXPORTA[k]
    viejo <- .dl_leer_particion_corrida(corrida, origen, md$slug)
    st <- .dl_stats_desde_ancho(.dl_leer_draws(corrida, md$slug, anio), nivel)
    nuevo <- merge(viejo[, setdiff(names(viejo), c("val", "lower", "upper")), with = FALSE], st,
                   by = c("location_id", "sex_id", "age_group_id"))
    if (nrow(nuevo) != nrow(viejo) || nrow(nuevo) != nrow(st))
      .dl_stop("%s \u2014 las celdas (%d) y las simulaciones (%d) de la corrida no coinciden (%d en com\u00fan)",
               md$slug, nrow(viejo), nrow(st), nrow(nuevo))
    rid <- run_id; uil <- nivel   # dentro de [] los nombres de columna taparían los argumentos
    nuevo[, `:=`(run_id = rid, val = val * md$escala_out, lower = lower * md$escala_out, upper = upper * md$escala_out,
                 ui_level = uil)]
    data.table::setcolorder(nuevo, names(viejo))
    nuevo
  }))
  data.table::setorder(celdas, measure_id, location_level, location_id, sex_id, age_group_id)
  dl_validar_estimaciones(celdas, rutas)

  # 2. Escritura: particiones nuevas, resto de la corrida enlazado, manifiesto y registro.
  dir_nuevo <- .dl_dir_corrida(carpeta, run_id)
  archivos <- .dl_escribir_particiones(celdas, dir_nuevo, run_id)
  for (sub in setdiff(list.files(corrida), c("cause", "manifest.yaml")))
    .dl_enlazar_arbol(file.path(corrida, sub), file.path(dir_nuevo, sub))
  # las corridas de versiones anteriores a la 0.2.2 no declaraban el estadístico puntual: era la mediana
  anterior <- man$params$estadistico_puntual %||% "mediana"
  man$run_id <- run_id; man$generado <- .dl_partes_run_id(run_id)$fecha; man$files <- archivos
  man$params$estadistico_puntual <- .DL_ESTADISTICO_PUNTUAL
  man$params$version_paquete <- dl_version()
  if (!is.null(man$params$ui_level)) man$params$ui_level <- nivel
  man$resumen <- list(run_origen = origen, estadistico_anterior = anterior)
  if (!identical(anterior, .DL_ESTADISTICO_PUNTUAL))
    man$limitaciones <- c(man$limitaciones %||% list(), list(sprintf(
      paste0("val re-resumido desde las simulaciones de la corrida de origen, sin volver a ajustar (estad\u00edstico ",
             "puntual %s -> %s; el intervalo no cambia) \u2014 ver resumen.run_origen"),
      anterior, .DL_ESTADISTICO_PUNTUAL)))
  .dl_escribir_manifest(man, dir_nuevo)
  if (!isTRUE(all.equal(.dl_releer_manifest(dir_nuevo), man, check.attributes = FALSE)))
    .dl_stop("el manifiesto reescrito no reproduce el de la corrida de origen %s", origen)
  .dl_corrida_escrita(man, dir_nuevo, if (registrar) registro, origen = origen)
}
