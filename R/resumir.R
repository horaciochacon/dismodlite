# dl_resumir(): lleva las simulaciones por banda de edad del ajuste (o de la cascada) y de los AVD a las celdas
# de salida del contrato estimates/v1 (sin run_id: lo pone dl_exportar_corrida()) y a las tablas anchas de
# simulaciones por medida que guarda la corrida.
# - Nombres de la causa, de la ubicación del ancla y de la ronda: tal cual los trae el ancla (b$meta).
# - Medidas que se exportan, con su métrica y su escala de salida: .DL_MEDIDAS (esquema.R).
# - Nombres de sexo, medida, métrica y ubicaciones departamentales: los catálogos de las rutas.

# Nombre de cada id en la tabla `tabla_` (sex, measure, metric, age_group) del catálogo demográfico; un id sin fila
# es un error (nunca se inventa un nombre).
.dl_nombre_demo <- function(cat, tabla_, ids) {
  m <- cat[tabla == tabla_]
  out <- m$name[match(ids, m$id)]
  if (anyNA(out))
    .dl_stop("id(s) sin fila %s en el cat\u00e1logo demogr\u00e1fico: %s",
             tabla_, paste(unique(ids[is.na(out)]), collapse = ", "))
  out
}

# Celdas del contrato estimates/v1 (sin run_id) de las medidas exportables, desde sus estadísticos (`stats`: slug ->
# tabla location_id, sex_id, age_group_id, val, lower, upper en las unidades del modelo, proporción o tasa por
# persona-año), con los nombres de los catálogos de `rutas`. `ancla` (location_id, location_name) y `bandas`
# (age_group_id, age_group_name) dan el nombre de la ubicación del ancla y de las bandas tal cual los trae el ancla;
# sin ellos, todos los nombres son los del catálogo. Las usan dl_resumir() y dl_sumar_hijas().
.dl_celdas_contrato <- function(stats, rutas, ronda, anio, causa, nombre_causa, nivel, ancla = NULL, bandas = NULL) {
  cat_demo <- .dl_catalogo(rutas, "demograficos")
  cat_loc <- .dl_catalogo(rutas, "locations")
  celdas <- data.table::rbindlist(lapply(seq_len(nrow(.DL_MEDIDAS_EXPORTA)), function(k) {
    md <- .DL_MEDIDAS_EXPORTA[k]
    st <- stats[[md$slug]]
    k_loc <- match(st$location_id, cat_loc$location_id)
    if (anyNA(k_loc))
      .dl_stop("ubicaci\u00f3n sin fila en el cat\u00e1logo de ubicaciones (locations): %s",
               paste(unique(st$location_id[is.na(k_loc)]), collapse = ", "))
    nombre_loc <- cat_loc$location_name[k_loc]
    if (!is.null(ancla)) nombre_loc <- ifelse(st$location_id == ancla$location_id, ancla$location_name, nombre_loc)
    data.table::data.table(
      source = .DL_STD_SOURCE, round = ronda, entity = "cause", method = .DL_METODO_DISMOD_LITE,
      location_id = st$location_id, location_name = nombre_loc,
      location_level = as.integer(cat_loc$location_level[k_loc]), year = anio,
      age_group_id = st$age_group_id,
      age_group_name = if (is.null(bandas)) .dl_nombre_demo(cat_demo, "age_group", st$age_group_id)
                       else bandas$age_group_name[match(st$age_group_id, bandas$age_group_id)],
      sex_id = st$sex_id, sex_name = .dl_nombre_demo(cat_demo, "sex", st$sex_id),
      cause_id = causa, cause_name = nombre_causa,
      measure_id = md$measure_id, measure_name = .dl_nombre_demo(cat_demo, "measure", md$measure_id),
      metric_id = md$metric_id_out, metric_name = .dl_nombre_demo(cat_demo, "metric", md$metric_id_out),
      val = st$val * md$escala_out, lower = st$lower * md$escala_out, upper = st$upper * md$escala_out,
      ui_level = nivel)
  }))
  data.table::setorder(celdas, measure_id, location_level, location_id, sex_id, age_group_id)
  celdas
}

# Tabla larga de simulaciones (location_id, sex_id, age_group_id, draw, val) -> tabla ancha con una columna por
# simulación (draw_1 ... draw_n), la forma en que se guardan en draws/ de la corrida.
.dl_draws_ancho <- function(draws_banda) {
  w <- data.table::dcast(draws_banda, location_id + sex_id + age_group_id ~ paste0("draw_", draw),
                         value.var = "val")
  data.table::setcolorder(w, c("location_id", "sex_id", "age_group_id",
                               paste0("draw_", sort(unique(draws_banda$draw)))))
  w
}

# ---- Piezas de una corrida (dl_resumir, dl_exportar_corrida) ----

# Claves de `piezas`: nombre interno -> sinónimos en español, qué se espera y de dónde sale.
.DL_PIEZAS <- list(
  fit = list(sinonimo = "ajuste", clase = "dl_fit", origen = "dl_ajustar() o dl_cascada()",
             como = "el ajuste de dl_ajustar() o, para las ubicaciones subnacionales, la cascada de dl_cascada()"),
  yld = list(sinonimo = "avd", clase = "dl_yld", origen = "dl_avd()",
             como = "dl_avd(ajuste, insumos, semilla = ...) con el mismo ajuste o cascada de `fit`"),
  bundle = list(sinonimo = "insumos", clase = "dl_bundle", origen = "dl_insumos()",
                como = "los insumos de dl_insumos(configuracion, rutas)"),
  resumen = list(sinonimo = "resumen", clase = "dl_resumen", origen = "dl_resumir()",
                 como = "dl_resumir(list(ajuste = ..., avd = ..., insumos = ...))"))

# Comprueba y normaliza `piezas` (lista con nombres; se aceptan los sinónimos en español) y devuelve la lista con los
# nombres internos `requeridas`. Una clave desconocida o que falta es un error que dice qué va en cada una. Los
# mensajes muestran los nombres en español (ajuste, avd, insumos) y recuerdan que valen también fit, yld y bundle.
.dl_piezas <- function(piezas, requeridas) {
  sinonimos <- vapply(.DL_PIEZAS, `[[`, "", "sinonimo")
  internos <- setdiff(requeridas, sinonimos[requeridas])
  formato <- sprintf("list(%s)%s", paste(sprintf("%s = ...", sinonimos[requeridas]), collapse = ", "),
                     if (length(internos)) sprintf(" (tambi\u00e9n valen los nombres %s)",
                                                   paste(internos, collapse = ", ")) else "")
  if (missing(piezas))
    .dl_stop("falta `piezas`, la lista con las piezas de la corrida: %s", formato)
  if (!is.list(piezas) || is.data.frame(piezas) || any(names(.DL_DESCRIPCION_CLASES) %in% class(piezas)))
    .dl_stop("`piezas` es una lista con las piezas de la corrida, %s; es %s", formato, .dl_describir_objeto(piezas))
  if (!length(piezas))
    .dl_stop("`piezas` est\u00e1 vac\u00eda: pasa las piezas de la corrida, %s", formato)
  nm <- names(piezas)
  if (is.null(nm) || any(is.na(nm) | !nzchar(nm)))
    .dl_stop("cada elemento de `piezas` lleva nombre: %s", formato)
  nm[nm %in% sinonimos] <- names(sinonimos)[match(nm[nm %in% sinonimos], sinonimos)]
  if (anyDuplicated(nm))
    .dl_stop("pieza repetida en `piezas` (con su nombre y su sin\u00f3nimo): %s",
             paste(unique(nm[duplicated(nm)]), collapse = ", "))
  # una pieza conocida que esta función no usa (el resumen en dl_resumir) se ignora; una desconocida es un error
  for (k in setdiff(nm, names(.DL_PIEZAS))) {
    pista <- if (k %in% c("cascada", "cascade")) " (la cascada va en `ajuste`, o `fit`)" else ""
    .dl_stop("`%s` no es una pieza de la corrida%s. Las piezas son %s", k, pista, formato)
  }
  names(piezas) <- nm
  for (k in requeridas) {
    d <- .DL_PIEZAS[[k]]
    if (is.null(piezas[[k]]))
      .dl_stop("falta la pieza `%s`%s: %s. Las piezas son %s",
               d$sinonimo, if (identical(d$sinonimo, k)) "" else sprintf(" (o `%s`)", k), d$como, formato)
    .dl_exigir_clase(piezas[[k]], d$clase, paste0("piezas$", k), d$origen)
  }
  piezas[requeridas]
}

# Huella de un ajuste o una cascada: identifica de qué objeto salen los AVD y el resumen, para no mezclar piezas de
# corridas distintas (el hash de los insumos no las distingue: el ajuste nacional y su cascada comparten insumos).
.dl_huella_ajuste <- function(f)
  digest::digest(list(class(f), f$bundle_hash, f$seed, f$draws_par, f$kappa, f$seed_cascada, f$modo,
                      f$departamentos), algo = "xxhash64")

# Huella de un resultado de dl_avd().
.dl_huella_avd <- function(y)
  digest::digest(list(y$huella_ajuste, y$seed, y$como_aplicado, y$draws_yld$val), algo = "xxhash64")

# Los AVD `y` salen del ajuste `f`: por su huella o, en objetos sin ella, por ubicaciones y número de simulaciones.
.dl_chequear_avd_de_ajuste <- function(f, y) {
  mal <- if (!is.null(y$huella_ajuste)) !identical(y$huella_ajuste, .dl_huella_ajuste(f))
         else !setequal(unique(y$draws_yld$location_id), unique(f$draws_q$location_id)) ||
           !identical(as.integer(max(y$draws_yld$draw)), nrow(f$draws_par[[1]]))
  if (mal)
    .dl_stop(paste0("los AVD de `yld` no salen del ajuste de `fit` (%s): calcula dl_avd() con el mismo ajuste o ",
                    "cascada que pasas en `fit`"),
             if (inherits(f, "dl_cascade")) "una cascada" else "un ajuste nacional")
  invisible()
}

#' Resumen de una corrida
#'
#' Lleva las simulaciones del ajuste (o de la cascada) y de los AVD a las celdas de salida: media e intervalo por
#' ubicación, sexo, banda y medida (prevalencia, incidencia y AVD), con los nombres de los catálogos, y a las
#' tablas anchas de simulaciones que guarda la corrida.
#'
#' @param piezas Lista con `fit` (el ajuste de [dl_ajustar()] o la cascada de [dl_cascada()]), `yld` (resultado de
#'   [dl_avd()] con ese mismo ajuste o cascada) y `bundle` (los insumos); también valen los nombres `ajuste`, `avd` e
#'   `insumos`.
#' @param nivel Nivel del intervalo de incertidumbre, entre 0 y 1 (0.95: cuantiles 2.5 % y 97.5 %). Para exportar
#'   la corrida con [dl_exportar_corrida()] debe ser 0.95, el que fija el contrato de las corridas.
#' @param rutas Rutas de [dl_rutas()]; se usan los catálogos. `NULL` (por defecto) usa las de los insumos.
#' @return Objeto de clase `dl_resumen`, una lista con:
#'   - `celdas`: tabla (data.table) con una fila por ubicación, sexo, banda de edad (las del ancla) y medida, con las
#'     columnas del contrato de las corridas `estimates/v1` salvo `run_id` (que agrega [dl_exportar_corrida()]):
#'     `source`, `round`, `entity`, `method`, `location_id`, `location_name`, `location_level` (0 nacional, 1
#'     subnacional), `year`, `age_group_id`, `age_group_name`, `sex_id`, `sex_name`, `cause_id`, `cause_name`,
#'     `measure_id`, `measure_name`, `metric_id`, `metric_name`, `val` (la media de las simulaciones), `lower`,
#'     `upper` y `ui_level`. La prevalencia es la proporción de la población, entre 0 y 1 (con la métrica 2,
#'     `Percent`, como en las versiones anteriores); la incidencia y los AVD, tasas por 100 000 (`Rate`).
#'   - `draws`: lista con una tabla ancha por medida (`prevalence`, `incidence`, `yld`): `location_id`, `sex_id`,
#'     `age_group_id` y una columna por simulación (`draw_1`, `draw_2`, ...), en las unidades del modelo
#'     (proporción o tasa por persona-año).
#'   - `mcmc` y `aceptacion`: los diagnósticos del ajuste (ver [dl_ajustar()]).
#'   - `desplazamiento_splits` y `como_aplicado`: los de [dl_avd()].
#'   - `bundle_hash`, `huella_ajuste` y `huella_avd`: identifican los insumos, el ajuste y los AVD del resumen;
#'     [dl_exportar_corrida()] comprueba que sean las mismas piezas.
#' @seealso [dl_exportar_corrida()] (el paso siguiente) y [dl_estimaciones()] (una tabla por edad simple, sin
#'   escribir nada).
#' @family corrida
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' res <- dl_resumir(list(ajuste = f, avd = dl_avd(f, b, semilla = 1), insumos = b))
#' res
#' head(res$celdas[, c("sex_name", "age_group_name", "measure_name", "val", "lower", "upper")])
#' # las simulaciones de la prevalencia, una columna por simulación
#' res$draws$prevalence[1:3, 1:6]
#' }
#' @export
dl_resumir <- function(piezas, nivel = 0.95, rutas = NULL) {
  piezas <- .dl_piezas(piezas, c("fit", "yld", "bundle"))
  .dl_exigir_nivel(nivel)
  f <- piezas$fit; y <- piezas$yld; b <- piezas$bundle
  .dl_exigir_mismos_insumos(f, b, "piezas$fit")
  .dl_exigir_mismos_insumos(y, b, "piezas$yld")
  .dl_chequear_avd_de_ajuste(f, y)
  rutas <- .dl_resolver_rutas(rutas, b)
  cfg <- b$cfg
  # Simulaciones de cada medida exportable (slug de .DL_MEDIDAS -> tabla location_id, sex_id, age_group_id, draw,
  # val), en las unidades del modelo (proporción o tasa por persona-año).
  fuentes <- list(prevalence = y$draws_prev_banda,
                  incidence  = .dl_ipop_bandas(f, b),
                  yld        = y$draws_yld)
  slugs <- stats::setNames(nm = .DL_MEDIDAS_EXPORTA$slug)
  falta <- setdiff(slugs, names(fuentes))
  if (length(falta))
    .dl_stop("medida exportable sin fuente de simulaciones: %s", paste(falta, collapse = ", "))
  # Una cascada mezcla el nivel 0 (el ancla, con su nombre tal cual lo trae el ancla) y el nivel 1 (los
  # departamentos, con nombre y nivel del catálogo de ubicaciones).
  celdas <- .dl_celdas_contrato(
    lapply(slugs, function(s) .dl_stats_bandas(fuentes[[s]], nivel)), rutas, ronda = b$meta$round,
    anio = .dl_anio_ajuste(cfg), causa = cfg$cause_id, nombre_causa = b$meta$cause_name, nivel = nivel,
    ancla = list(location_id = b$loc_ancla, location_name = b$meta$location_name),
    bandas = unique(b$prior_gbd[measure_id == .dl_medida_id("prevalence"), list(age_group_id, age_group_name)]))
  structure(list(celdas = celdas,
                 draws = lapply(slugs, function(s) .dl_draws_ancho(fuentes[[s]])),
                 mcmc = f$mcmc, aceptacion = f$aceptacion,
                 desplazamiento_splits = y$desplazamiento_splits,
                 como_aplicado = y$como_aplicado, bundle_hash = b$hash,
                 huella_ajuste = .dl_huella_ajuste(f), huella_avd = .dl_huella_avd(y)),
            class = "dl_resumen")
}

# Nombre en español de cada medida exportable (slug de .DL_MEDIDAS), para los print.
.DL_MEDIDAS_ES <- stats::setNames(.DL_MEDIDAS$nombre_es, .DL_MEDIDAS$slug)

#' @export
print.dl_resumen <- function(x, ...) {
  medidas <- .DL_MEDIDAS$slug[match(unique(x$celdas$measure_id), .DL_MEDIDAS$measure_id)]
  niveles <- unique(x$celdas$ui_level)
  cat(sprintf("<dl_resumen> resumen de una corrida (insumos %s)\n", substr(x$bundle_hash, 1, 12)))
  cat(sprintf("  %d celdas: %s | %d ubicaci\u00f3n(es) | intervalo del %s\n", nrow(x$celdas),
              paste(.DL_MEDIDAS_ES[medidas], collapse = ", "), data.table::uniqueN(x$celdas$location_id),
              paste(sprintf("%g %%", 100 * niveles), collapse = ", ")))
  cat(sprintf("  simulaciones por medida: %d\n", ncol(x$draws[[1]]) - 3L))
  invisible(x)
}
