# Insumos de una causa. dl_insumos() lee los archivos de entrada (con los lectores de R/lectura.R), los lleva a las
# tablas del contrato dismod_lite/v1 (inst/schema/dismod_lite.v1.yaml), valida cada tabla con sus reglas
# (R/reglas.R) y calcula el hash que identifica los insumos; dl_congelar_insumos() escribe esas tablas en disco. Las
# betas y las covariables se arman en R/insumos_covariables.R. Notación del modelo: ver R/edo.R.

# ---- Evidencia y doble conteo ----

# Evidencia: solo se lee list.csv (una fila por fuente y NID), la tabla que cruza .dl_chequear_doble_conteo().
.dl_leer_ghdx_store <- function(dir)
  .dl_leer_csv(file.path(dir, "list.csv"), colClasses = list(character = "location_id"), pieza = "evidencia",
               en_carpeta = TRUE)

# component_id de la evidencia GHDx: 5 = fuentes no fatales (prevalencia, incidencia); 4 = causas de muerte (CoD).
.DL_GHDX_COMPONENTE <- c(no_fatal = 5L, cod = 4L)

# Doble conteo entre el ancla y los datos locales. El ancla nacional de GBD ya fue informada por las fuentes del país
# que están en la evidencia (`evidencia`, la tabla list.csv), así que no vale a peso completo: lambda = 1 solo sin
# fuentes locales no fatales (0.5 por defecto con al menos una), y con csmr en medidas_entrada tampoco si el registro
# de defunciones (CoD) ya informó el ancla. Devuelve los nid no fatales y el número de filas CoD, que quedan
# declarados en los insumos. Sin evidencia, el aviso es el de .dl_avisar_datos_en_ajuste().
.dl_chequear_doble_conteo <- function(cfg, evidencia, loc) {
  if (is.null(evidencia)) return(list(nid_no_fatal = integer(), n_cod = 0L))
  nf <- evidencia[cause_id == cfg$cause_id & component_id == .DL_GHDX_COMPONENTE[["no_fatal"]] & location_id == loc]
  if (nrow(nf) && cfg$anchor$lambda >= 1)
    .dl_stop(paste0("el ancla a peso completo (lambda = 1) exige 0 fuentes locales no fatales para la causa %d y la ",
                    "evidencia tiene %d (nid %s); usa lambda < 1 (0.5 por defecto), por ejemplo cambios = ",
                    "list(anchor = list(lambda = 0.5)), y deja la raz\u00f3n en `decisiones`"), cfg$cause_id, nrow(nf),
             paste(utils::head(nf$nid, 3), collapse = ", "))
  cod <- evidencia[cause_id == cfg$cause_id & component_id == .DL_GHDX_COMPONENTE[["cod"]] & location_id == loc]
  if ("csmr" %in% unlist(cfg$medidas_entrada) && nrow(cod) && cfg$anchor$lambda >= 1)
    .dl_stop(paste0("doble conteo: el registro de defunciones ya inform\u00f3 el ancla (componente CoD, ",
                    "%d fila(s) en la evidencia) y csmr est\u00e1 en medidas_entrada. Usa lambda < 1, ",
                    "por ejemplo cambios = list(anchor = list(lambda = 0.5)), y deja la raz\u00f3n en `decisiones`"),
             nrow(cod))
  list(nid_no_fatal = as.integer(unique(nf$nid)), n_cod = nrow(cod))
}

# ---- Ancla (tabla prior_gbd) ----

# Bandas finas de 80 años y más de las estimaciones GBD (80-84, 85-89, 90-94 y 95+): se agregan a la banda de
# salida 21 (80+) con los pesos de población de `pesos_80mas`.
.DL_FINAS_80 <- list(ids = c(30L, 31L, 32L, 235L), id_salida = 21L, nombre_salida = "80+ years")
# Bandas agregadas del catálogo (22 = todas las edades, 27 = estandarizada por edad): nunca son celdas del ancla.
.DL_BANDAS_AGREGADAS <- c(todas = 22L, estandarizada = 27L)
# Bandas finas de la infancia (0-6 días, 7-27 días, 1-5 meses, 6-11 meses, 12-23 meses y 2-4 años): la malla anual y
# la población (<5) no las alojan. Se agregan a «<5 years» (id 1) con peso igual al ancho de la banda en años
# (población uniforme dentro de 0-5: aproximación declarada), salvo que `pesos_80mas` traiga esos ids (entonces
# mandan sus pesos).
.DL_FINAS_5 <- list(ids = c(2L, 3L, 388L, 389L, 238L, 34L), id_salida = 1L, nombre_salida = "<5 years")

# Pesos de población de las bandas finas (age_group_id, sex_id, peso) del CSV de `pesos_80mas`.
.dl_leer_pesos_finas <- function(paths)
  .dl_leer_csv(.dl_path(paths, "pesos_80mas"), tipos = c(age_group_id = "int", sex_id = "int", peso = "num"),
               pieza = "pesos_80mas")

# Agrega un grupo de bandas finas a su banda de salida con un promedio ponderado en escala natural. Promediar lower y
# upper así es una aproximación declarada: GBD no publica las simulaciones.
.dl_agregar_finas <- function(std, grupo, pesos) {
  finas <- std[age_group_id %in% grupo$ids]
  if (!nrow(finas)) return(std)
  m <- merge(finas, pesos, by = c("age_group_id", "sex_id"), all.x = TRUE)
  if (anyNA(m$peso))
    .dl_stop("`pesos_80mas` no trae el peso de la(s) banda(s) %s (age_group_id) para todos los sexos del ancla",
             paste(unique(m$age_group_id[is.na(m$peso)]), collapse = ", "))
  agg <- m[, list(val = sum(val * peso) / sum(peso), lower = sum(lower * peso) / sum(peso),
                  upper = sum(upper * peso) / sum(peso), acquisition_id = acquisition_id[1]),
           by = list(cause_id, sex_id, year)]
  agg[, `:=`(age_group_id = grupo$id_salida, age_group_name = grupo$nombre_salida)]
  rbind(std[!age_group_id %in% grupo$ids], agg, fill = TRUE)
}

# Materializa una medida del ancla (fila de .DL_MEDIDAS) al formato de la tabla prior_gbd. Devuelve la tabla con el
# atributo "meta": los nombres tal como vienen en las estimaciones (cause_name, location_name, round).
# `cat_bandas` y `pesos` (las bandas del catálogo y los pesos de las bandas finas) se leen si no se dan.
# `opcional`: una medida que GBD no publica para la causa (por ejemplo, la incidencia de una causa que se modela a
# partir de un deterioro) devuelve NULL en vez de detenerse.
.dl_materializar_medida <- function(cfg, slug, paths, archivo = NULL, cat_bandas = NULL, pesos = NULL,
                                    opcional = FALSE) {
  med <- .dl_medida(slug)
  met <- .dl_metrica_std(med, cfg)
  archivo <- archivo %||% .dl_path(paths, med$path_key)
  pieza <- .dl_nombre_ruta(med$path_key)
  loc <- .dl_loc_ancla(cfg)
  # Un valor no numérico en val, lower o upper deja la columna como texto: la lectura lo muestra.
  std0 <- .dl_leer_std(archivo, paths$registry, pieza = pieza, tipos = c(val = "num", lower = "num", upper = "num"),
                       ubigeo = .dl_codigos_ubigeo_rutas(paths))
  # Año del ancla (years.ancla): sus filas entran reetiquetadas al año de ajuste, así todo lo que después filtra por
  # years.ajuste (cascada, reglas, resumen, corrida) no cambia.
  std <- std0[cause_id == cfg$cause_id & metric_name == met$metric_std & location_id == loc &
              year %in% .dl_anio_ancla(cfg) & sex_id %in% unlist(cfg$sexos)]
  if (!nrow(std) && opcional) return(NULL)
  if (!nrow(std))
    .dl_stop(paste0("\u00ab%s\u00bb (`%s`) no tiene filas del ancla para la causa %d, la ubicaci\u00f3n %s, ",
                    "el a\u00f1o %d, los sexos %s y la m\u00e9trica \u00ab%s\u00bb"), basename(archivo), pieza,
             as.integer(cfg$cause_id), loc, .dl_anio_ancla(cfg), paste(unlist(cfg$sexos), collapse = " y "),
             met$metric_std)
  std[, year := .dl_anio_ajuste(cfg)]
  for (col in c("val", "lower", "upper")) std[[col]] <- std[[col]] / met$escala_std
  meta <- list(cause_name = unique(std$cause_name), location_name = unique(std$location_name),
               round = as.character(unique(std$round)))
  if (any(lengths(meta) != 1L))
    .dl_stop(paste0("\u00ab%s\u00bb (`%s`) trae m\u00e1s de un valor de cause_name, location_name o round para la ",
                    "causa: el ancla debe venir de una sola estimaci\u00f3n"), basename(archivo), pieza)
  w <- pesos %||% .dl_leer_pesos_finas(paths)
  cat_bandas <- cat_bandas %||% .dl_bandas_catalogo(paths)
  # 80+: pesos de población del archivo. <5: ancho de la banda en el catálogo, salvo que el archivo traiga los ids.
  resto <- .dl_agregar_finas(std, .DL_FINAS_80, w)
  w5 <- w[age_group_id %in% .DL_FINAS_5$ids]
  if (!nrow(w5)) {
    cb <- cat_bandas[match(.DL_FINAS_5$ids, cat_bandas$age_group_id)]
    w5 <- data.table::CJ(age_group_id = .DL_FINAS_5$ids, sex_id = unique(std$sex_id))
    w5[, peso := (cb$age_end - cb$age_start)[match(age_group_id, .DL_FINAS_5$ids)]]
  }
  resto <- .dl_agregar_finas(resto, .DL_FINAS_5, w5)
  edades <- cat_bandas[match(resto$age_group_id, cat_bandas$age_group_id), list(age_start, age_end)]
  # Las estimaciones GBD traen bandas fuera del soporte del modelo (por debajo de edad_inicio, todas las edades,
  # estandarizada por edad) y ceros estructurales (val = lower = upper = 0: GBD no modela la causa en esa banda).
  # Ambos quedan fuera del ancla: un prior log-normal no puede alojarlos, y el modelo cubre esas edades con p = 0 en
  # edad_inicio y la suavidad. Un cero no degenerado sigue exigiendo offset_lognormal. Las bandas agregadas del
  # catálogo se excluyen por su id, porque con edad_inicio 0 pasarían el filtro por age_start.
  dentro <- !is.na(edades$age_start) & edades$age_start >= as.numeric(cfg$edad_inicio) &
    !(resto$val == 0 & resto$lower == 0 & resto$upper == 0) & !resto$age_group_id %in% .DL_BANDAS_AGREGADAS
  if (!all(dentro)) {
    fuera <- unique(resto$age_group_name[!dentro])
    .dl_message(paste0("`%s`: %d banda(s) de edad quedan fuera del ancla (por debajo de edad_inicio, ",
                       "agregadas o con cero estructural): %s"), pieza, length(fuera), paste(fuera, collapse = ", "))
    resto <- resto[dentro]; edades <- edades[dentro]
    if (!nrow(resto))
      .dl_stop("`%s` no tiene ninguna banda de edad dentro del modelo (desde edad_inicio = %s)",
               pieza, format(cfg$edad_inicio))
  }
  offset <- cfg$offset_lognormal
  if (any(resto$val <= 0 | resto$lower <= 0) && is.null(offset))
    .dl_stop(paste0("`%s` tiene valores <= 0 en val o lower y la configuraci\u00f3n no declara offset_lognormal (el ",
                    "desplazamiento de la log-normal)"), pieza)
  # Las bandas del ancla de un sexo no se solapan: el prior de EMR toma la banda que contiene cada nudo
  # (.dl_emr_en_nudos, con .dl_banda_de), y esa banda debe ser una sola.
  solapan <- vapply(split(seq_len(nrow(resto)), resto$sex_id), function(k)
    .dl_hay_solape(edades$age_start[k], edades$age_end[k]), logical(1))
  if (any(solapan))
    .dl_stop(paste0("\u00ab%s\u00bb (`%s`) trae bandas de edad que se solapan (sexo %s): cada edad debe caer en una ",
                    "sola banda del ancla"), basename(archivo), pieza, names(solapan)[solapan][1L])
  # sigma_log: sd en escala log que implica el intervalo publicado, tomado como media ± 1,96 sd en escala log.
  prior <- data.table::data.table(
    cause_id = as.integer(resto$cause_id), measure_id = med$measure_id, location_id = loc,
    sex_id = as.integer(resto$sex_id), age_group_id = as.integer(resto$age_group_id),
    age_group_name = resto$age_group_name, age_start = edades$age_start, age_end = edades$age_end,
    year = as.integer(resto$year), val = resto$val, lower = resto$lower, upper = resto$upper,
    sigma_log = (log(resto$upper) - log(resto$lower)) / .DL_ANCHO_IC95_EN_SD,
    lambda = cfg$anchor$lambda, acquisition_id = resto$acquisition_id)
  data.table::setorder(prior, sex_id, age_start)
  data.table::setattr(prior, "meta", meta)
  prior
}

# El prior de EMR usa el csmr del ancla: el prior informativo csmr/prevalencia, o el techo derivado con plano_cota sin
# cota declarada.
.dl_prior_usa_csmr <- function(cfg)
  identical(cfg$emr_prior$tipo, "informativo_edad") ||
    (identical(cfg$emr_prior$tipo, "plano_cota") && is.null(cfg$emr_prior$cota))

# Tabla prior_gbd: la prevalencia del ancla y, cuando el prior de EMR la necesita, el csmr (.dl_prior_usa_csmr). Los
# pesos de las bandas finas se leen una sola vez para las dos medidas.
.dl_materializar_prior <- function(cfg, paths, cat_bandas) {
  pesos <- .dl_leer_pesos_finas(paths)
  out <- .dl_materializar_medida(cfg, "prevalence", paths, cat_bandas = cat_bandas, pesos = pesos)
  meta <- attr(out, "meta")
  if (.dl_prior_usa_csmr(cfg)) {
    archivo <- .dl_path(paths, "std_csmr", motivo = paste0(
      "emr_prior.tipo informativo_edad (o plano_cota sin cota) necesita el ancla de mortalidad, muertes por 100 000 ",
      "de GBD Results, por edad y sexo, de la causa y el a\u00f1o de ajuste"))
    out <- rbind(out, .dl_materializar_medida(cfg, "csmr", paths, archivo = archivo, cat_bandas = cat_bandas,
                                              pesos = pesos))
  }
  data.table::setorder(out, measure_id, sex_id, age_start)
  data.table::setattr(out, "meta", meta)
  out
}

# Componente de la causa (anchor.componente): la prevalencia del ancla se escala a la fracción de las secuelas del
# componente (partición de severidad en `particion_severidad`); el csmr queda tal cual (las muertes son de la causa:
# con la prevalencia del componente, el prior csmr/prevalencia sube), y la tabla de severidad debe ser la del
# componente (dl_severidad_desde_particion(secuelas =)). Escala por referencia las filas de prevalencia de `prior`
# (así entra al hash, a través de prior_gbd) y devuelve el componente; sin anchor.componente, NULL y `prior` no cambia.
.dl_escalar_componente <- function(cfg, sev, prior, paths) {
  if (is.null(cfg$anchor$componente)) return(NULL)
  ds <- .dl_path(paths, "severity_split", motivo = paste("anchor.componente la exige: es la carpeta de la corrida",
                                                         "de partici\u00f3n de severidad de la causa"))
  # las fracciones ya vienen con la severidad derivada de la partición; con una tabla CSV se calculan aquí
  comp <- attr(sev, "componente") %||%
    .dl_fracciones_componente(ds, cfg$cause_id, cfg$anchor$componente$sequela_ids, paths)
  if (!setequal(sev$health_state_id, comp$health_state_ids))
    .dl_stop(paste0("la tabla de severidad (estados %s) no es la del componente (estados %s); ",
                    "der\u00edvala con dl_severidad_desde_particion(secuelas =)"),
             paste(sort(sev$health_state_id), collapse = ","), paste(comp$health_state_ids, collapse = ","))
  fp <- comp$fraccion_prevalencia
  if (!is.finite(fp) || fp <= 0 || fp > 1)
    .dl_stop("la fracci\u00f3n de prevalencia del componente (%s) est\u00e1 fuera de (0, 1]", format(fp))
  meta_prior <- attr(prior, "meta")
  prior[measure_id == .dl_medida_id("prevalence"), `:=`(val = val * fp, lower = lower * fp, upper = upper * fp)]
  data.table::setattr(prior, "meta", meta_prior)
  comp$tabla <- NULL
  comp
}

# ---- Tablas del contrato leídas de CSV (datos, cov_proxy, severidad, población) ----

# Tabla del contrato desde su CSV, o vacía si no se dio la ruta. El paquete no depende de la fuente: la tabla llega
# ya preparada (completitud, redistribución, corrección de definiciones) con su acquisition_id. Las celdas que no se
# pueden convertir al tipo del esquema se muestran al leer. Un CSV con solo la fila de encabezado se detiene con un
# mensaje: sin él, cada columna sin valores llegaría con un tipo equivocado y la validación daría un problema por
# columna.
.dl_leer_contrato <- function(paths, pieza, sch, tabla) {
  archivo <- .dl_path(paths, pieza, opcional = TRUE)
  if (is.null(archivo)) return(.dl_tabla_vacia(sch, tabla))
  arg <- .dl_nombre_ruta(pieza)
  d <- .dl_leer_csv(archivo, colClasses = list(character = "location_id"), tipos = .dl_tipos_columnas(sch, tabla),
                    tabla = tabla, pieza = arg, ubigeo = .dl_codigos_ubigeo_rutas(paths))
  if (!nrow(d))
    .dl_stop(paste0("el archivo de `%s` (\u00ab%s\u00bb) solo tiene la fila de encabezado, sin datos. ",
                    "Si no hay tabla `%s`, no pases `%s` en dl_rutas()%s.\n  ruta: %s"),
             arg, basename(archivo), tabla, arg,
             if (arg %in% c("datos", "proxies"))
               sprintf(" (con los datos de ejemplo, dl_rutas_ejemplo(causa, %s = FALSE))", arg) else "",
             archivo)
  d
}

# Tabla severidad: el CSV de `severidad` o, con severidad.fuente = mod y sin ese CSV, la que se deriva de la corrida
# de partición de severidad (`particion_severidad`) con dl_severidad_desde_particion(): la de una hija de un padre
# (severidad.padre) o la de un componente de la causa (anchor.componente). Sin ninguna de las dos, la tabla va vacía:
# los insumos se arman igual y dl_avd() pide la severidad (.dl_exigir_severidad()).
.dl_materializar_severidad <- function(cfg, paths, sch) {
  archivo <- .dl_path(paths, "severidad", opcional = TRUE)
  particion <- if (is.null(archivo) && identical(cfg$severidad$fuente, "mod")) .dl_path(paths, "severity_split")
  if (is.null(archivo) && is.null(particion)) return(.dl_tabla_vacia(sch, "severidad"))
  if (!is.null(particion)) {
    comp_ids <- as.integer(unlist(cfg$anchor$componente$sequela_ids))
    sev <- dl_severidad_desde_particion(particion, cfg$cause_id, rutas = paths,
                                        padre = cfg$severidad$padre,
                                        secuelas = if (length(comp_ids)) comp_ids else NULL)
    # fwrite y fread no son inversas bit a bit (difieren en 1 ulp): la tabla derivada pasa por un CSV para valer
    # exactamente lo mismo que esa tabla escrita en disco y pasada en `severidad` (mismo hash de los insumos).
    archivo <- tempfile(fileext = ".csv"); on.exit(unlink(archivo), add = TRUE)
    data.table::fwrite(sev, archivo, eol = "\n")
    atrs <- attributes(sev)[c("cuota_hija", "componente", "renormalizacion")]
    origen <- "la tabla de severidad derivada de la partici\u00f3n"
  } else {
    archivo <- .dl_path(paths, "severidad"); atrs <- NULL
    origen <- sprintf("la tabla de severidad (\u00ab%s/%s\u00bb)", basename(dirname(archivo)), basename(archivo))
  }
  sev <- .dl_leer_csv(archivo, colClasses = list(character = c("beta_covariable", "location_id_fuente", "fuente")),
                      tipos = .dl_tipos_columnas(sch, "severidad"), pieza = "severidad")
  # La tabla es una por causa y se usan todas sus filas: la de otra causa daría AVD de otra causa sin ningún aviso
  # (por ejemplo, un subtipo con la severidad de la causa padre).
  otras <- if ("cause_id" %in% names(sev)) setdiff(unique(sev$cause_id[!is.na(sev$cause_id)]), cfg$cause_id)
  if (length(otras))
    .dl_stop(paste0("%s es de la causa %s y la configuraci\u00f3n es de la %d: pasa en `severidad` la tabla de la ",
                    "causa %d (con los datos de ejemplo, dl_rutas_ejemplo(%d))"), origen, paste(otras, collapse = ", "),
             cfg$cause_id, cfg$cause_id, cfg$cause_id)
  if (!"beta_covariable" %in% names(sev) || !is.character(sev$beta_covariable))
    sev[["beta_covariable"]] <- rep(NA_character_, nrow(sev))
  for (nm in names(atrs)) if (!is.null(atrs[[nm]])) data.table::setattr(sev, nm, atrs[[nm]])
  sev
}

# Tabla datos: las filas de la causa, los sexos y los años de la configuración, con los tipos del esquema.
.dl_materializar_datos <- function(cfg, paths, sch) {
  d <- .dl_leer_contrato(paths, "datos", sch, "datos")
  if (!nrow(d)) return(d)
  # Sin medidas_entrada, la tabla entra solo para validar la cascada (filas departamentales, nivel 1): las filas
  # nacionales irían a la verosimilitud y la regla tipos_declarados las rechazaría.
  if (!length(unlist(cfg$medidas_entrada)) && any(d$location_level != 1L)) {
    n_nac <- sum(d$location_level != 1L & d$cause_id %in% cfg$cause_id)
    if (n_nac)
      .dl_message(paste0("%d fila(s) nacionales de `datos` quedan fuera del ajuste porque medidas_entrada est\u00e1 ",
                         "vac\u00edo en la configuraci\u00f3n; las filas subnacionales se usan solo para validar la ",
                         "cascada"), n_nac)
    d <- d[location_level == 1L]
    if (!nrow(d)) return(.dl_coaccionar_schema(d, sch, "datos"))
  }
  anio <- .dl_anio_ajuste(cfg)
  # La verosimilitud (nivel 0) usa el año de ajuste; la validación subnacional (nivel 1) puede ser de otro año,
  # el de cascada.heldout_anio. Las filas de otras causas y las de un sexo que no se estima quedan fuera sin mensaje;
  # las de la causa de otro año o de ambos sexos, con uno.
  anio_h <- .dl_anio_heldout(cfg)
  propia <- d$cause_id %in% cfg$cause_id
  del_sexo <- d$sex_id %in% unlist(cfg$sexos)
  del_anio <- with(d, (location_level != 1L & year_start <= anio & year_end >= anio) |
                      (location_level == 1L & year_start <= anio_h & year_end >= anio_h)) %in% TRUE
  n_anio <- sum(propia & del_sexo & !del_anio)
  n_ambos <- sum(propia & d$sex_id %in% 3L)
  if (n_anio + n_ambos > 0L)
    .dl_message("%d fila(s) de `datos` de la causa %d quedan fuera: %s", n_anio + n_ambos, cfg$cause_id,
                paste(c(if (n_anio) sprintf("%d de otro a\u00f1o (%s)", n_anio,
                                            if (anio == anio_h) sprintf("deben incluir %d", anio)
                                            else sprintf("las nacionales deben incluir %d y las subnacionales %d",
                                                         anio, anio_h)),
                        if (n_ambos) sprintf("%d de ambos sexos (van por sexo: una de hombres y otra de mujeres)",
                                             n_ambos)), collapse = "; "),
                clase = "dl_mensaje_revision")
  usar <- propia & del_sexo & del_anio
  d <- d[usar]
  # Un dato que termina antes de edad_inicio no tiene predicción (la malla empieza ahí): queda fuera, con aviso.
  e0 <- as.numeric(cfg$edad_inicio %||% 0)
  if (nrow(d) && any(d$age_end <= e0)) {
    .dl_message(paste0("%d dato(s) con edad por debajo de edad_inicio %g quedan fuera (sin predicci\u00f3n en la ",
                       "malla de edades)"), sum(d$age_end <= e0), e0, clase = "dl_mensaje_revision")
    d <- d[age_end > e0]
  }
  .dl_chequear_rango_datos(d)
  .dl_coaccionar_schema(d, sch, "datos")
}

# Datos locales en el ajuste (medidas_entrada): si ninguna fila nacional de `datos` (la tabla ya filtrada) entra al
# ajuste, un mensaje lo dice; si entran, sin evidencia (las rutas no traen `evidencia` y la configuración no declara
# anchor.evidencia_ghdx) y con el ancla a peso completo (lambda = 1), un aviso de doble conteo.
.dl_avisar_datos_en_ajuste <- function(cfg, datos, con_evidencia) {
  tipos <- unlist(cfg$medidas_entrada)
  if (!length(tipos)) return(invisible(NULL))
  if (!any(datos$location_level != 1L & !(datos$outlier %in% TRUE)))
    .dl_message(paste0("medidas_entrada declara %s, pero ninguna fila nacional de `datos` del a\u00f1o %d entra al ",
                       "ajuste (sin excluir): el ajuste usa solo el ancla"), paste(tipos, collapse = ", "),
                .dl_anio_ajuste(cfg), clase = "dl_mensaje_revision")
  else if (!con_evidencia && isTRUE(cfg$anchor$lambda >= 1))
    .dl_warn(paste0("la causa %d usa datos locales en el ajuste (medidas_entrada) con anchor.lambda = 1: la ",
                    "estimaci\u00f3n de referencia puede incluir ya esos datos y contarlos dos veces. Considera ",
                    "anchor.lambda: 0.5 y anota el motivo en `decisiones`"), cfg$cause_id)
  invisible(NULL)
}

# Valores imposibles en las filas de `datos` que entran: la prevalencia es una proporción (entre 0 y 1, no un
# porcentaje), una tasa no es negativa, el error estándar y la muestra son positivos y, en una prevalencia con
# conteos, los casos no superan la muestra (el binomial de la verosimilitud no los admite). Un error lista los
# dato_id de cada problema (en el formato simple, fila_<n> es la fila n de datos.csv).
.dl_chequear_rango_datos <- function(d) {
  if (!nrow(d)) return(invisible(d))
  num <- function(cn) if (cn %in% names(d)) suppressWarnings(as.numeric(d[[cn]])) else rep(NA_real_, nrow(d))
  val <- num("val"); se <- num("se"); x <- num("x"); n <- num("n"); ne <- num("n_efectivo")
  prev <- d$measure_id == .dl_medida_id("prevalence")
  hay <- function(v) !is.na(v)
  mal <- list(
    "prevalencia fuera de [0, 1] (va como proporci\u00f3n, no como porcentaje)" = hay(val) & prev &
      !(val >= 0 & val <= 1),
    "tasa negativa o infinita" = hay(val) & !prev & !(val >= 0 & is.finite(val)),
    "error est\u00e1ndar que no es un n\u00famero positivo" = hay(se) & !(se > 0 & is.finite(se)),
    "muestra que no es un n\u00famero positivo" = (hay(n) & !(n > 0)) | (hay(ne) & !(ne > 0 & is.finite(ne))),
    "casos negativos" = hay(x) & x < 0,
    "m\u00e1s casos que muestra en una prevalencia" = prev & hay(x) & hay(n) & x > n)
  mal <- Filter(any, mal)
  if (!length(mal)) return(invisible(d))
  probs <- sprintf("datos: %s: %s", names(mal),
                   vapply(mal, function(i) paste(utils::head(d$dato_id[i], 5L), collapse = ", "), ""))
  .dl_stop("la tabla datos tiene valores imposibles:\n%s", paste0("  - ", probs, collapse = "\n"),
           campos = list(problemas = probs, tabla = "datos"))
}

# Tabla cov_proxy: los proxies departamentales del año de ajuste. La tabla puede traer los de varias causas: solo
# entran los que la configuración declara; si falta uno declarado, lo detiene dl_cascada(), no dl_insumos().
.dl_materializar_proxy <- function(cfg, paths, sch) {
  px <- .dl_leer_contrato(paths, "cov_proxy", sch, "cov_proxy")
  if (nrow(px)) px <- px[year %in% unlist(cfg$years$ajuste)]
  decl <- as.integer(unlist(lapply(cfg$covariables, function(cv) cv$proxy$covariate_id_proxy)))
  if (nrow(px) && length(decl)) px <- px[covariate_id_proxy %in% decl]
  px
}

# Tabla poblacion: un CSV o una carpeta de estimaciones de población (nacional y departamentos), del año de ajuste,
# con los niveles 0 y 1 en una sola tabla.
.dl_materializar_poblacion <- function(cfg, paths) {
  pobl <- .dl_leer_std(.dl_path(paths, "poblacion"), paths$registry, subruta = "population/population",
                       pieza = "poblacion", tipos = c(val = "num"), ubigeo = .dl_codigos_ubigeo_rutas(paths))
  if ("measure_id" %in% names(pobl))                       # formato estimates/v1 -> tabla poblacion
    pobl <- pobl[, list(location_id, location_level = as.integer(location_level), year = as.integer(year),
                        sex_id = as.integer(sex_id), age_group_id = as.integer(age_group_id),
                        val = as.numeric(val), acquisition_id)]
  # Una columna ausente se nombra aquí: la conversión de val la crearía llena de NA y la validación diría «NA en
  # columna obligatoria».
  faltan <- setdiff(c("location_id", "location_level", "year", "sex_id", "age_group_id", "val"), names(pobl))
  if (length(faltan))
    .dl_stop("falta la(s) columna(s) %s en la tabla de poblaci\u00f3n (`poblacion` de dl_rutas())",
             paste(faltan, collapse = ", "))
  data.table::set(pobl, j = "val", value = as.numeric(pobl$val))
  pobl[year %in% unlist(cfg$years$ajuste) & location_level %in% c(0L, 1L)]
}

# ---- Hash, dl_insumos() y dl_congelar_insumos() ----

# Hash de los insumos: el sha256 de los sha256 de cada tabla escrita con fwrite (la misma escritura de
# dl_congelar_insumos(), que reutiliza estos sha256 en vez de volver a leer los archivos). Atributo "tablas": el
# sha256 de cada tabla.
.dl_hash_bundle <- function(b, tablas) {
  hs <- vapply(tablas, function(nm) {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    data.table::fwrite(b[[nm]], tmp)
    digest::digest(file = tmp, algo = "sha256")
  }, character(1))
  list(hash = digest::digest(paste(tablas, hs, collapse = ";"), algo = "sha256"),
       tablas = stats::setNames(as.list(hs), paste0(tablas, ".csv")))
}

# Tablas de los insumos que entran al hash y se congelan, en este orden.
.DL_TABLAS_BUNDLE <- c("datos", "prior_gbd", "betas", "crosswalks", "cov_valores",
                       "cov_proxy", "severidad", "poblacion", "sequela_map")

# Detiene la corrida si el paso del RK4, con la remisión más alta de la configuración y el techo de EMR (`cota`),
# supera .DL_RK4_LIMITE_PASO (R/edo.R) con el nsub de la configuración (por defecto .DL_NSUB).
.dl_chequear_paso_rk4 <- function(cfg, cota) {
  r_max <- max(as.numeric(cfg$remision$valor %||% 0),
               vapply(cfg$remision$por_edad %||% list(), function(t) as.numeric(t$valor), 0))
  k_h <- (r_max + max(cota)) / as.numeric(cfg$nsub %||% .DL_NSUB)
  if (k_h > .DL_RK4_LIMITE_PASO)
    .dl_stop(paste0("el paso de la EDO es inestable: (remisi\u00f3n %.3g + techo de EMR %.3g) / nsub %d = %.2f > %g ",
                    "(el l\u00edmite de RK4 es 2.785). Sube nsub en la configuraci\u00f3n (el paso es h = 1/nsub) ",
                    "para que la prevalencia no oscile a valores negativos"), r_max, max(cota),
             as.integer(cfg$nsub %||% .DL_NSUB), k_h, .DL_RK4_LIMITE_PASO)
  invisible(k_h)
}

#' Insumos de una causa
#'
#' Lee todos los archivos de entrada de una causa (ancla, covariables y betas, población, datos locales, proxies,
#' severidad), los lleva a las tablas del contrato `dismod_lite/v1`, valida cada tabla y calcula el hash que
#' identifica los insumos. Es el primer paso de una corrida: su resultado lo usan [dl_ajustar()] y las demás etapas.
#'
#' Lo que falta se reporta donde hace falta: sin tabla de severidad, los insumos se arman igual y [dl_avd()] la pide;
#' con datos locales en el ajuste y el ancla a peso completo, sin almacén de evidencia (`evidencia` de [dl_rutas()];
#' obligatoria si la configuración declara `anchor.evidencia_ghdx`), un aviso de doble conteo; si ninguna fila nacional
#' de `datos` entra al ajuste, un mensaje lo dice en su lugar. Con un proyecto simple
#' (`dl_insumos(dl_proyecto(...))`), los mensajes citan sus claves y sus archivos (`datos_en_ajuste`, `proxies.csv`,
#' ...) en lugar de los del formato completo; si su traducción ya no está (cambió la configuración o una tabla y se
#' volvió a traducir, o es de otra sesión), un error pide volver a llamar a [dl_proyecto()].
#'
#' @param configuracion Configuración de [dl_configuracion()], o un proyecto de [dl_proyecto()]: entonces `rutas`,
#'   si no se da, son las del proyecto.
#' @param rutas Rutas de [dl_rutas()] (con los datos de ejemplo, [dl_rutas_ejemplo()]).
#' @return Objeto de clase `dl_bundle`, una lista con:
#'   - las tablas del contrato `dismod_lite/v1` (ver [dl_esquema()]), validadas: `datos` (los datos locales),
#'     `prior_gbd` (el ancla, por medida, sexo y banda de edad), `betas` (las betas de las covariables),
#'     `crosswalks` (vacía en esta versión), `cov_valores` (el valor nacional de cada covariable), `cov_proxy` (los
#'     valores subnacionales de las covariables), `severidad` (los estados de salud de la causa), `poblacion` y
#'     `sequela_map` (las secuelas de la causa y su rol, de `sequela_rei.csv` de la carpeta `registro`; vacía si
#'     no hay);
#'   - `cfg`: la configuración ([dl_configuracion()]), con el techo de la mortalidad en exceso ya resuelto en
#'     `emr_prior$cota`;
#'   - `loc_ancla`: el `location_id` de la ubicación nacional; `meta`: el nombre de la causa (`cause_name`) y de la
#'     ubicación (`location_name`) según el ancla y la ronda de GBD (`round`);
#'   - `seleccion_betas` (de dónde salió cada beta), `techo_emr` (el techo de la mortalidad en exceso, `cota`, y su
#'     `origen`: impreso en la configuración o derivado del ancla), `fuentes_locales` (en el formato completo con
#'     almacén de evidencia, las fuentes del país que ya informaron el ancla: la base del aviso de doble conteo),
#'     `componente` (el componente modelado, solo en el formato completo; `NULL` si no hay), `bandas_pobl` y
#'     `bandas_catalogo` (los límites de las bandas de edad de la población y del catálogo);
#'   - `hash`: el sha256 que identifica los insumos (el de las tablas, `hashes`, combinado); los ajustes y las
#'     corridas lo registran;
#'   - `rutas`: las rutas con que se armaron (no entran en el hash); las etapas siguientes las usan cuando no se les
#'     pasan otras.
#' @seealso [dl_proyecto()] (el argumento habitual), [dl_ajustar()] (el paso siguiente), [dl_revisar_proyecto()]
#'   (todos los problemas de un proyecto juntos) y [dl_congelar_insumos()] (las tablas en disco).
#' @family insumos
#' @examples
#' \donttest{
#' # un proyecto (formato simple o completo)
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' b
#' # o la configuración y las rutas por separado
#' b <- dl_insumos(dl_configuracion_ejemplo(9100), dl_rutas_ejemplo(9100, datos = FALSE))
#' }
#' @export
dl_insumos <- function(configuracion, rutas = dl_rutas()) {
  if (missing(configuracion))
    .dl_stop("falta `configuracion` (la de dl_configuracion() o dl_configuracion_ejemplo())")
  # con un proyecto simple, los mensajes en sus palabras
  simple <- inherits(configuracion, "dl_proyecto") && identical(configuracion$formato, "simple")
  if (inherits(configuracion, "dl_proyecto")) {
    if (simple && !file.exists(file.path(dirname(configuracion$rutas$poblacion), "listo")))
      .dl_stop(paste0("la traducci\u00f3n de este proyecto ya no est\u00e1 (cambi\u00f3 la configuraci\u00f3n o una ",
                      "tabla, o es de otra sesi\u00f3n): vuelve a llamar a dl_proyecto(\"%s\")"), configuracion$carpeta)
    if (missing(rutas)) rutas <- configuracion$rutas
    configuracion <- configuracion$configuracion
  }
  # una lista con cause_id también vale (la versión 0.2.2 no exigía la clase); otro objeto del paquete, no
  if (!is.list(configuracion) || is.null(configuracion$cause_id) ||
      (!inherits(configuracion, "dl_config") && any(names(.DL_DESCRIPCION_CLASES) %in% class(configuracion))))
    .dl_exigir_clase(configuracion, "dl_config", "configuracion", "dl_configuracion()")
  .dl_en_simple(.dl_armar_insumos(configuracion, rutas), simple)
}

# Los insumos de la configuración `cfg` con las rutas `rutas` (dl_insumos()).
.dl_armar_insumos <- function(cfg, rutas) {
  # las rutas llevan la regla de los códigos subnacionales (.dl_codigos_ubigeo) a los lectores
  paths <- .dl_rutas_con_codigos(.dl_resolver_rutas(rutas), cfg)
  # Rutas de dl_rutas_ejemplo(anio =): el año de la corrida es el de la configuración y debe coincidir.
  anio_rutas <- attr(paths, "anio_ejemplo")
  if (!is.null(anio_rutas) && !identical(as.integer(anio_rutas), .dl_anio_ajuste(cfg)))
    .dl_stop(paste0("las rutas son de dl_rutas_ejemplo(anio = %d) y la configuraci\u00f3n es del a\u00f1o %d ",
                    "(years.ajuste): el a\u00f1o lo fija la configuraci\u00f3n, usa dl_configuracion_ejemplo(%d, ",
                    "anio = %d)"), as.integer(anio_rutas), .dl_anio_ajuste(cfg), cfg$cause_id, as.integer(anio_rutas))
  sch <- dl_esquema()
  loc <- .dl_loc_ancla(cfg)
  # la evidencia se lee si las rutas la traen; si la configuración declara anchor.evidencia_ghdx, es obligatoria
  evidencia <- .dl_path(paths, "ghdx_store", opcional = is.null(cfg$anchor$evidencia_ghdx))
  if (!is.null(evidencia)) evidencia <- .dl_leer_ghdx_store(evidencia)
  fuentes_locales <- .dl_chequear_doble_conteo(cfg, evidencia, loc)
  sev   <- .dl_materializar_severidad(cfg, paths, sch)
  citadas <- unique(sev$beta_covariable[!is.na(sev$beta_covariable) & nzchar(sev$beta_covariable)])
  betas <- .dl_materializar_betas(cfg, paths, citadas)
  .dl_chequear_escala(betas, stats::setNames(
    lapply(cfg$transformaciones, function(t) isTRUE(t$escala_confirmada)),
    vapply(cfg$transformaciones, function(t) t$covariate_name_short, "")))
  pobl  <- .dl_materializar_poblacion(cfg, paths)
  # En sequela_rei.csv un texto vacío equivale a NA (sin canal de proporción); se conserva tal cual, porque cambiarlo
  # cambiaría el hash de los insumos, y dl_avd() lo trata con nzchar().
  smap <- .dl_archivo_registro(paths, "sequela_rei.csv", colClasses = list(character = "rol"),
                               tipos = .dl_tipos_columnas(sch, "sequela_map"), tabla = "sequela_map")
  # con los tipos del esquema: un archivo sin secuelas (solo el encabezado) se lee con columnas lógicas
  smap <- .dl_coaccionar_schema(smap[cause_id == cfg$cause_id], sch, "sequela_map")
  # límites de todas las bandas del catálogo (también las de los proxies): las bandas del ancla, la banda de cada edad
  # en la cascada por edad y el cierre por banda de la regla promedio_cierra_ancla
  cat_bandas <- .dl_bandas_catalogo(paths)
  prior <- .dl_materializar_prior(cfg, paths, cat_bandas)
  comp <- .dl_escalar_componente(cfg, sev, prior, paths)
  # Techo de EMR: el declarado en la configuración o el derivado del ancla. Se resuelve una sola vez aquí y queda en
  # cfg$emr_prior$cota para la verosimilitud y la cascada; no entra en las tablas congeladas (el hash no cambia).
  techo <- .dl_techo_emr(cfg, prior)
  cfg$emr_prior$cota <- techo$cota
  .dl_chequear_paso_rk4(cfg, techo$cota)
  datos <- .dl_materializar_datos(cfg, paths, sch)
  .dl_avisar_datos_en_ajuste(cfg, datos, con_evidencia = !is.null(evidencia))
  b <- list(datos = datos,
            prior_gbd = prior,
            betas = betas,
            crosswalks = .dl_tabla_vacia(sch, "crosswalks"),
            cov_valores = .dl_materializar_cov(cfg, paths, betas, loc),
            cov_proxy = .dl_materializar_proxy(cfg, paths, sch),
            severidad = sev, poblacion = pobl, sequela_map = smap, cfg = cfg,
            loc_ancla = loc, meta = attr(prior, "meta"), seleccion_betas = attr(betas, "seleccion"),
            techo_emr = techo, fuentes_locales = fuentes_locales, componente = comp,
            bandas_pobl = .dl_bandas_poblacion(pobl[location_level == 0L], cat_bandas),
            bandas_catalogo = cat_bandas)
  exentos <- vapply(Filter(function(t) isTRUE(t$token_exento), cfg$transformaciones),
                    function(t) t$covariate_name_short, character(1))
  # contexto de las reglas que cruzan tablas; tol: tolerancia de las sumas que deben dar 1 y de los promedios que
  # deben cerrar con el ancla
  ctx <- list(tol = 1e-8, poblacion = pobl, token_exentos = exentos, loc_ancla = loc, cfg = cfg, betas = betas,
              cov_valores = b$cov_valores, bandas_catalogo = b$bandas_catalogo,
              acq_prior_csmr = unique(prior[measure_id == .dl_medida_id("csmr")]$acquisition_id))
  # población primero: es insumo de las reglas de cov_proxy (el orden del hash no cambia)
  for (nm in c("poblacion", setdiff(.DL_TABLAS_BUNDLE, "poblacion"))) dl_validar_tabla(b[[nm]], nm, sch, ctx)
  h <- .dl_hash_bundle(b, .DL_TABLAS_BUNDLE)
  b$hash <- h$hash; b$hashes <- h$tablas
  b$rutas <- paths
  structure(b, class = "dl_bundle")
}

#' Congelar los insumos en disco
#'
#' Escribe cada tabla de los insumos como CSV en `carpeta` y un `hash.json` con el sha256 de cada archivo. Es lo que
#' guarda [dl_exportar_corrida()] en `inputs/`.
#'
#' Los archivos son las tablas del contrato `dismod_lite/v1` (`datos.csv`, `prior_gbd.csv`, `betas.csv`,
#' `crosswalks.csv`, `cov_valores.csv`, `cov_proxy.csv`, `severidad.csv`, `poblacion.csv` y `sequela_map.csv`), tal
#' como las usa el modelo: con ellas y la configuración se puede auditar o repetir una corrida.
#'
#' @inheritParams dl_ajustar
#' @param carpeta Carpeta de destino (se crea si no existe).
#' @return Lista con el sha256 de cada archivo escrito (nombres: los archivos), invisible.
#' @seealso [dl_insumos()], [dl_exportar_corrida()].
#' @family insumos
#' @examples
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' carpeta <- file.path(tempdir(), "insumos_congelados")
#' h <- dl_congelar_insumos(b, carpeta)
#' list.files(carpeta)
#' str(h[1:2])
#' unlink(carpeta, recursive = TRUE)
#' @export
dl_congelar_insumos <- function(insumos, carpeta) {
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_exigir_carpeta(carpeta, "la carpeta de destino")
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  hs <- list()
  # Si los insumos traen los sha256 de dl_insumos() (`hashes`), deben coincidir: una tabla editada en memoria
  # después de dl_insumos() es un error, porque el hash de los insumos ya no describiría lo congelado.
  for (nm in .DL_TABLAS_BUNDLE) {
    f <- file.path(carpeta, paste0(nm, ".csv"))
    data.table::fwrite(insumos[[nm]], f)
    hs[[paste0(nm, ".csv")]] <- digest::digest(file = f, algo = "sha256")
    esperado <- insumos$hashes[[paste0(nm, ".csv")]]
    if (!is.null(esperado) && !identical(esperado, hs[[paste0(nm, ".csv")]]))
      .dl_stop("la tabla %s cambi\u00f3 despu\u00e9s de dl_insumos() (su sha256 no es el del hash de los insumos)", nm)
  }
  jsonlite::write_json(hs, file.path(carpeta, "hash.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(hs)
}

# Qué es cada tabla de los insumos, en print(); crosswalks, siempre vacía en esta versión, no se muestra.
.DL_TABLAS_DESCRIPCION <- c(
  datos = "datos locales (para el ajuste o la validaci\u00f3n)",
  prior_gbd = "ancla, por medida, sexo y banda de edad",
  betas = "betas de las covariables",
  cov_valores = "valores nacionales de las covariables",
  cov_proxy = "valores subnacionales de las covariables (proxies)",
  severidad = "estados de salud de la causa",
  poblacion = "poblaci\u00f3n por ubicaci\u00f3n, sexo y edad",
  sequela_map = "secuelas de la causa (formato completo)")

#' @export
print.dl_bundle <- function(x, ...) {
  cat(sprintf("<dl_bundle> insumos de la causa %d (%s) | ancla en la ubicaci\u00f3n %s | hash %s\n", x$cfg$cause_id,
              x$meta$cause_name, x$loc_ancla, substr(x$hash, 1, 12)))
  for (nm in names(.DL_TABLAS_DESCRIPCION))
    cat(sprintf("  %-12s %5d filas  %s\n", nm, nrow(x[[nm]]), .DL_TABLAS_DESCRIPCION[[nm]]))
  invisible(x)
}
