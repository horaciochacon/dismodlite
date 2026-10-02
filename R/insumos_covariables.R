# Betas y covariables de los insumos (R/insumos.R): la tabla betas (las betas predictivas del YAML de extracción, con
# la transformación y la escala que declara la configuración) y la tabla cov_valores (el valor nacional de cada
# covariable con beta y de cada sustituta declarada).

# Parámetro que publica GBD para una beta -> canal del modelo al que se aplica.
.dl_param_objetivo <- function(impreso) {
  mapa <- c("Prevalence" = "prevalencia", "Incidence" = "incidencia",
            "Excess mortality rate" = "emr", "Proportion" = "proporcion")
  out <- mapa[impreso]
  if (anyNA(out))
    .dl_stop(paste0("el YAML de extracci\u00f3n trae betas de un par\u00e1metro desconocido (%s); ",
                    "los admitidos son Prevalence, Incidence, Excess mortality rate y Proportion"),
             paste(impreso[is.na(out)], collapse = ", "))
  unname(out)
}

# Etiqueta de modelo de una beta; las filas sin modelo_variante llevan la etiqueta reservada «sin_variante».
.DL_SIN_VARIANTE <- "sin_variante"
.dl_variante <- function(x) {
  v <- x$modelo_variante
  if (is.null(v) || is.na(v) || !nzchar(trimws(as.character(v)))) .DL_SIN_VARIANTE else trimws(as.character(v))
}

# Selección de las betas predictivas del ancla. El YAML de extracción puede guardar las tablas de todos los modelos
# de una publicación (pasos preliminares, otros desenlaces...), distinguidas por modelo_variante; solo entran las de
# anchor.modelo_variante más las covariables que cita severidad.beta_covariable (canal de AVD). Las predictivas sin
# covariate_name_short (sin id en el catálogo) no se pueden materializar: se excluyen y se declaran.
# Devuelve list(cov = filas seleccionadas, seleccion = resumen para los insumos y el manifiesto).
.dl_seleccionar_betas <- function(cfg, cov, citadas = character()) {
  sin_cov <- vapply(cov, function(x) is.null(x$covariate_name_short) || !nzchar(x$covariate_name_short), TRUE)
  sin_cov_nombres <- vapply(cov[sin_cov], function(x) x$nombre_impreso %||% "(sin nombre impreso)", character(1))
  cov <- cov[!sin_cov]
  variantes <- vapply(cov, .dl_variante, character(1))
  elegidas <- cfg$anchor$modelo_variante
  if (!is.null(elegidas)) {
    faltan <- setdiff(elegidas, variantes)
    if (length(faltan))
      .dl_stop(paste0("anchor.modelo_variante \u00ab%s\u00bb no coincide con ninguna beta predictiva del YAML de ",
                      "extracci\u00f3n; variantes disponibles: %s"), paste(faltan, collapse = "\u00bb, \u00ab"),
               paste(unique(variantes), collapse = " | "))
    keep <- variantes %in% elegidas |
      vapply(cov, function(x) x$covariate_name_short %in% citadas, TRUE)
  } else keep <- rep(TRUE, length(cov))
  list(cov = cov[keep],
       seleccion = list(modelo_variante = elegidas,
                        filas_excluidas = as.integer(sum(!keep)),
                        variantes_excluidas = unique(variantes[!keep]),
                        sin_covariable = unname(sin_cov_nombres)))
}

# Causa del YAML de extracción que aporta las betas: extraction.cause_id de la configuración o, por defecto, la propia
# causa.
.dl_extraction_cause_id <- function(cfg) as.integer(cfg$extraction$cause_id %||% cfg$cause_id)

# Tabla betas: las betas predictivas seleccionadas del YAML de extracción, con la transformación y la escala que
# declara la configuración.
.dl_materializar_betas <- function(cfg, paths, citadas = character()) {
  # sin covariables en la configuración, sin extracción y sin betas citadas por la severidad: sin betas
  if (!length(cfg$transformaciones) && is.null(.dl_path(paths, "extraction", opcional = TRUE)) && !length(citadas))
    return(.dl_tabla_vacia(dl_esquema(), "betas"))
  y <- .dl_leer_yaml(.dl_path(paths, "extraction"))
  esperado <- .dl_extraction_cause_id(cfg)
  meta_id <- suppressWarnings(as.integer(y$meta$causa_gbd$cause_id))
  if (length(meta_id) == 1L && !is.na(meta_id) && meta_id != esperado)
    .dl_stop(paste0("el YAML de extracci\u00f3n es de la causa %d y la configuraci\u00f3n espera la %d ",
                    "(extraction.cause_id o, sin ese campo, cause_id)"), meta_id, esperado)
  cov <- y$covariables_gbd
  cov <- Filter(function(x) identical(x$rol, "predictiva"), cov)
  if (!length(cov))
    .dl_stop("el YAML de extracci\u00f3n no tiene covariables predictivas (covariables_gbd con rol: predictiva)")
  sel <- .dl_seleccionar_betas(cfg, cov, citadas)
  cov <- sel$cov
  if (!length(cov)) .dl_stop("anchor.modelo_variante no deja ninguna beta predictiva")
  # Sin selección explícita, una clave repetida delata modelos distintos: se listan las variantes para la
  # configuración.
  if (is.null(cfg$anchor$modelo_variante)) {
    clave <- vapply(cov, function(x) paste(x$covariate_name_short, x$parametro, sep = "/"), character(1))
    dup <- unique(clave[duplicated(clave)])
    if (length(dup))
      .dl_stop(paste0("betas con la misma clave en varios modelos (%s); declara anchor.modelo_variante con la(s) ",
                      "etiqueta(s) del ancla. Variantes disponibles: %s"), paste(dup, collapse = ", "),
               paste(unique(vapply(cov, .dl_variante, character(1))), collapse = " | "))
  }
  trans <- do.call(rbind, lapply(cfg$transformaciones, function(t)
    data.frame(covariate_name_short = t$covariate_name_short, transformacion = t$transformacion,
               transformacion_procedencia = t$procedencia, escala = as.numeric(t$escala %||% 1),
               stringsAsFactors = FALSE)))
  filas <- lapply(cov, function(x) {
    tr <- trans[trans$covariate_name_short == x$covariate_name_short, ]
    if (is.null(tr) || !nrow(tr))
      .dl_stop("la configuraci\u00f3n no declara en `transformaciones` la transformaci\u00f3n de \u00ab%s\u00bb",
               x$covariate_name_short)
    data.table::data.table(
      cause_id = cfg$cause_id, covariate_id = as.integer(x$covariate_id),
      covariate_name_short = x$covariate_name_short, nombre_impreso = x$nombre_impreso,
      parametro_objetivo = .dl_param_objetivo(x$parametro),
      transformacion = tr$transformacion, transformacion_procedencia = tr$transformacion_procedencia,
      escala = tr$escala,
      beta = as.numeric(x$beta_valor), beta_lower = as.numeric(x$beta_inferior %||% NA),
      beta_upper = as.numeric(x$beta_superior %||% NA),
      modo = if (!is.null(x$beta_inferior) && !is.null(x$beta_superior)) "informativa" else "fija",
      rol = x$rol, nivel = x$nivel,
      fuente = paste0("extraction.yaml#covariables_gbd",
                      if (!is.null(x$tabla_impresa)) paste0(" | ", x$tabla_impresa) else ""))
  })
  out <- data.table::rbindlist(filas)
  data.table::setattr(out, "seleccion", sel$seleccion)
  out
}

# Escala de HAQI: una beta lineal de haqi con |beta| >= 0.1 y escala 1 (0-100) implica exp(beta x 20 puntos) ~ 0, así
# que la beta se estimó con la covariable en 0-1. Se rechaza salvo transformaciones[].escala_confirmada: true (por
# nombre de covariable en `confirmadas`).
.DL_ESCALA_HAQI_BETA_MAX <- 0.1
.dl_chequear_escala <- function(betas, confirmadas = list()) {
  h <- betas[tolower(covariate_name_short) == "haqi" & transformacion == "lineal" & escala == 1 &
             abs(beta) >= .DL_ESCALA_HAQI_BETA_MAX]
  h <- h[!vapply(h$covariate_name_short, function(nm) isTRUE(confirmadas[[nm]]), logical(1))]
  if (nrow(h)) {
    beta <- paste(signif(h$beta, 3), collapse = ", ")
    # en un proyecto, las betas son la tabla betas: el mensaje cita sus columnas, no las claves del formato completo
    if (isTRUE(.dl_estado$simple))
      .dl_stop(paste0("la beta de haqi (%s por unidad) con escala 1 (0-100) no es cre\u00edble: exp(beta x 20 ",
                      "puntos) es casi 0. En su fila de la tabla betas, escribe escala 0.01 (beta estimada en 0-1) ",
                      "o, si la beta es de verdad por punto de 0-100, escala_confirmada true"), beta)
    i <- match(h$covariate_name_short[1L], names(confirmadas))   # su lugar en transformaciones (en el mensaje)
    tr <- if (is.na(i)) "transformaciones[]" else sprintf("transformaciones[%d]", i)
    .dl_stop(paste0("la beta de haqi (%s por unidad) con escala 1 (0-100) no es cre\u00edble: ",
                    "exp(beta x 20 puntos) es casi 0. Declara %s.escala: 0.01 (beta estimada en 0-1) o, si la beta ",
                    "es de verdad por punto de 0-100, %s.escala_confirmada: true"), beta, tr, tr)
  }
  invisible(TRUE)
}

# Columnas que se usan de los CSV de `covariables` (formato GHDx): solo esas se leen.
.DL_COLUMNAS_COVARIABLES <- c("covariate_name_short", "location_id", "year_id", "sex_id", "age_group_id",
                              "mean_value", "lower_value", "upper_value")

# Tabla cov_valores: el valor nacional de cada covariable con beta y de cada sustituta declarada, del año del ancla
# reetiquetado al de ajuste.
.dl_materializar_cov <- function(cfg, paths, betas, loc) {
  if (!nrow(betas) && !length(Filter(function(cv) !is.null(cv$sustituye), cfg$covariables)))
    return(.dl_tabla_vacia(dl_esquema(), "cov_valores"))
  carpeta_cov <- .dl_path(paths, "ghdx_cov")
  archivos <- list.files(carpeta_cov, pattern = "[.]csv$", ignore.case = TRUE, full.names = TRUE)
  if (!length(archivos))
    .dl_stop("la carpeta de `covariables` no tiene archivos CSV: %s", carpeta_cov)
  crudo <- data.table::rbindlist(lapply(archivos, .dl_leer_csv, select = .DL_COLUMNAS_COVARIABLES,
                                        pieza = "covariables", en_carpeta = TRUE))
  # Además de las covariables con beta, las sustitutas declaradas (covariables[].sustituye): ponen el valor nacional
  # de referencia del proxy y no tienen beta propia (por ejemplo, la versión estandarizada por edad de una
  # covariable cuya beta es por edad).
  sust <- Filter(function(cv) !is.null(cv$sustituye), cfg$covariables)
  ids <- unique(rbind(betas[, list(covariate_name_short, covariate_id)],
                      data.table::rbindlist(lapply(sust, function(cv) data.table::data.table(
                        covariate_name_short = cv$sustituye$covariate_name_short,
                        covariate_id = as.integer(cv$sustituye$covariate_id))))))
  # la ubicación como texto: un código nacional que no es un número («P») también vale
  m <- merge(crudo[as.character(location_id) == as.character(loc)], ids, by = "covariate_name_short")
  out <- data.table::data.table(
    covariate_id = as.integer(m$covariate_id), covariate_name_short = m$covariate_name_short,
    location_id = rep(loc, nrow(m)), year = as.integer(m$year_id), sex_id = as.integer(m$sex_id),
    age_group_id = as.integer(m$age_group_id), val = as.numeric(m$mean_value),
    lower = as.numeric(m$lower_value), upper = as.numeric(m$upper_value),
    acquisition_id = rep("ghdx_cov_crudo_congelado", nrow(m)))
  # Las covariables sin CSV en `covariables` salen de `covariables_std` (tabla de estimaciones, con su
  # acquisition_id); el CSV de `covariables` manda cuando existe.
  faltan <- setdiff(ids$covariate_name_short, out$covariate_name_short)
  cov_std <- if (length(faltan)) .dl_path(paths, "cov_std", opcional = TRUE)
  if (!is.null(cov_std)) {
    s <- .dl_leer_std(cov_std, paths$registry, pieza = "covariables_std")
    s <- s[location_id == loc & covariate_name_short %in% faltan]
    # La clave de cov_valores no tiene edad: de una covariable por banda de edad solo entra la banda estandarizada
    # por edad (27) o la de todas las edades (22); sin ninguna de las dos, queda fuera con un mensaje (no se duplica
    # la clave).
    for (cv in unique(s$covariate_name_short)) {
      bandas <- unique(s[covariate_name_short == cv]$age_group_id)
      if (length(bandas) > 1L) {
        keep <- intersect(rev(as.character(.DL_BANDAS_AGREGADAS)), bandas)[1]   # 27 antes que 22
        if (is.na(keep)) .dl_message(paste0("la covariable %s viene por banda de edad en `covariables_std` (%d ",
                                            "bandas), sin la banda estandarizada por edad ni la de todas las edades: ",
                                            "queda fuera de cov_valores"), cv, length(bandas))
        s <- s[covariate_name_short != cv | (!is.na(keep) & age_group_id == keep)]
      }
    }
    if (nrow(s)) out <- rbind(out, data.table::data.table(
      covariate_id = as.integer(s$covariate_id), covariate_name_short = s$covariate_name_short,
      location_id = s$location_id, year = as.integer(s$year), sex_id = as.integer(s$sex_id),
      age_group_id = as.integer(s$age_group_id), val = as.numeric(s$val), lower = as.numeric(s$lower),
      upper = as.numeric(s$upper), acquisition_id = s$acquisition_id))
  }
  if (!nrow(out))
    .dl_stop("`covariables` no trae valores de la ubicaci\u00f3n del ancla (%s) para las covariables de la causa (%s)",
             loc, paste(ids$covariate_name_short, collapse = ", "))
  # Año del ancla (years.ancla): el valor nacional de ese año pasa a llevar la etiqueta del año de ajuste (la regla
  # ancla_igual_cov_valores y el cálculo de dX buscan el año de ajuste). Sin año prestado, la tabla no cambia.
  aj <- .dl_anio_ajuste(cfg); an <- .dl_anio_ancla(cfg)
  if (!identical(an, aj)) { out <- out[year != aj]; out[year == an, year := aj] }
  out
}
