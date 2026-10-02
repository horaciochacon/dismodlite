# dl_exportar_corrida(): escribe una corrida en <carpeta>/mod/dismod_lite/<run_id>/.
# Orden: primero las comprobaciones (convergencia de las cadenas, error del ancla y celdas validadas contra el
# contrato, todo en memoria); después la carpeta de la corrida (cause/<medida>/, draws/, etiquetas/, diagnostics/,
# inputs/) y el manifiesto; el registro de corridas se toca al final, para que un error no deje una entrada que
# apunte a una corrida a medio escribir.

# ---- Limitaciones del manifiesto de una corrida ----
# Se arman con los hechos de la corrida (configuración, insumos, cascada y validación), sin texto fijo sobre una
# causa, un país o una ronda del ancla. Cada condición aporta siempre el mismo número de limitaciones: una que
# aparece o desaparece es un cambio de comportamiento. Los textos no llevan «: » (el manifiesto los emite sin
# comillas); la procedencia de la configuración pasa por .dl_texto_yaml().

# Dos hechos que se declaran igual en las limitaciones de una corrida, de una suma de hijas y de un consolidado: qué
# es la incidencia con la fase aguda descontada del csmr, y que la suma de hijas no modela su correlación. Las frases
# que arman (con valores de ejemplo):
#   corrida      «fase aguda descontada del csmr (fraccion_aguda 0.3) — la incidencia exportada es <INCIDENCIA_AGUDA>
#                del ancla» (.dl_limitacion_fraccion_aguda)
#   suma         «suma simulación a simulación de 3 causas hijas (9101, 9102, 9103) — <CORRELACION_HIJAS> (cada hija
#                viene de su propio ajuste)» y «fase aguda descontada del csmr en la(s) hija(s) 9101 — su incidencia
#                es <INCIDENCIA_AGUDA> del ancla, y así entra en la suma (...)» (.dl_limitaciones_suma, R/sumar.R)
#   consolidado  «causa(s) 9100 como suma de sus hijas, simulación a simulación — <CORRELACION_HIJAS>» y «causa(s) 9101
#                con la fase aguda descontada del csmr — su incidencia es <INCIDENCIA_AGUDA> (ver el manifiesto de
#                cada run_fuente)» (.dl_consolidado_limitaciones, R/consolidar.R)
.DL_LIMITACION_INCIDENCIA_AGUDA <- paste0("el flujo de sobrevivientes a 28 d\u00edas del modelo cr\u00f3nico, no la ",
                                          "incidencia de primer evento")
.DL_LIMITACION_CORRELACION_HIJAS <- "la correlaci\u00f3n entre hijas no se modela"

# Fase aguda descontada del csmr: la incidencia exportada del modelo crónico es el flujo de sobrevivientes a 28 días,
# no la incidencia de primer evento del ancla.
.dl_limitacion_fraccion_aguda <- function(cfg) {
  fa <- .dl_fraccion_aguda(cfg)
  if (!(fa > 0)) return(NULL)
  cfr <- cfg$emr_prior$fraccion_aguda$cfr_30d
  sprintf(paste0("fase aguda descontada del csmr (fraccion_aguda %g) \u2014 la incidencia exportada es ",
                 .DL_LIMITACION_INCIDENCIA_AGUDA, " del ancla%s"),
          fa, if (!is.null(cfr)) sprintf(paste0(" (referencia del chequeo implied_incidence = incidencia del ancla ",
                                                "\u00d7 (1 - cfr_30d %g))"), cfr) else "")
}

# Mortalidad subnacional de validación (held-out) de otro año (cascada.heldout_anio distinto del año de ajuste). La
# limitación aparece con esa condición de la configuración; el texto dice qué hizo la corrida con esa mortalidad:
# con la validación de amplitud (atributo `amplitud` de dl_validar_ancla()), que compara años distintos; sin ella,
# por qué no se calculó (atributo `sin_amplitud`, o que la corrida no trae la validación).
.dl_limitacion_heldout <- function(cfg, validacion = NULL) {
  anio <- .dl_anio_ajuste(cfg); anio_h <- .dl_anio_heldout(cfg)
  if (identical(anio_h, anio)) return(NULL)
  procedencia <- .dl_texto_yaml(cfg$cascada$heldout_anio$procedencia)
  if (!is.null(attr(validacion, "amplitud")))
    return(sprintf(paste("mortalidad subnacional de validaci\u00f3n de %d contra una cascada de %d \u2014",
                         "la validaci\u00f3n de amplitud supone el gradiente subnacional estable entre ambos",
                         "a\u00f1os \u2014 %s"),
                   anio_h, anio, procedencia))
  motivo <- attr(validacion, "sin_amplitud") %||% "la corrida no trae la validaci\u00f3n del ancla"
  sprintf(paste("mortalidad subnacional de validaci\u00f3n declarada de %d para una corrida de %d \u2014",
                "sin validaci\u00f3n de amplitud (%s) \u2014 %s"),
          anio_h, anio, motivo, procedencia)
}

# Proyección declarada (years.ancla): el nivel nacional y su incertidumbre son los del año ancla; del año de ajuste
# son solo la población (con su acquisition_id) y, con una cascada por proxies, los proxies subnacionales.
.dl_limitacion_ancla <- function(cfg, b, casc = NULL) {
  anio <- .dl_anio_ajuste(cfg); anio_a <- .dl_anio_ancla(cfg)
  if (identical(anio_a, anio)) return(NULL)
  acq <- unique(b$poblacion$acquisition_id[b$poblacion$year == anio])
  if (!length(acq)) acq <- unique(b$poblacion$acquisition_id)
  con_proxies <- !is.null(casc) && !identical(casc$modo, "plana")
  sprintf(paste("proyecci\u00f3n declarada \u2014 ancla (prevalencia, csmr, covariables) de %d reetiquetada a %d",
                "\u2014 el nivel nacional y su incertidumbre son los de %d; del %d son solo la poblaci\u00f3n (%s)%s",
                "\u2014 %s"),
          anio_a, anio, anio_a, anio, paste(acq, collapse = ", "),
          if (con_proxies) " y los proxies subnacionales de la cascada" else "",
          .dl_texto_yaml(cfg$years$ancla$procedencia))
}

# Todas las limitaciones de una corrida, en el orden del manifiesto.
.dl_limitaciones_corrida <- function(cfg, b, res, casc, validacion, gate_ancla) {
  # estados de severidad con beta de covariable: dl_avd() usa en ellos el valor nacional (dX = 0) en toda ubicación
  sev_cov <- b$severidad$beta_covariable
  canal_prop <- sort(unique(sev_cov[!is.na(sev_cov) & nzchar(sev_cov)]))
  c(
    if (res$como_aplicado)
      list(paste0("AVD calibrado por edad al AVD del ancla \u2014 factor emp\u00edrico avd/(prev*sum(pi*dw)) sin ",
                  "tope (composici\u00f3n etaria y comorbilidad, inseparables con proporciones de severidad iguales ",
                  "en todas las edades)"))
    else list("sin correcci\u00f3n por comorbilidad (COMO) \u2014 el AVD queda sobreestimado"),
    list("pesos de discapacidad (DW) muestreados de forma independiente entre estados de salud"),
    as.list(.dl_limitacion_fraccion_aguda(cfg)),
    as.list(.dl_limitacion_heldout(cfg, validacion)),
    as.list(.dl_limitacion_ancla(cfg, b, casc)),
    if (isTRUE(attr(validacion, "sin_incidencia_gbd")))
      list(paste0("incidencia exportada sin referencia \u2014 el ancla de incidencia no trae filas de la causa; se ",
                  "omite el chequeo implied_incidence")),
    if (!is.null(b$componente))
      list(sprintf(paste0("componente de la causa \u2014 esta corrida cubre solo las secuelas %s (%s); prevalencia ",
                          "del ancla \u00d7 %.4f y AVD de referencia \u00d7 %.4f seg\u00fan la partici\u00f3n de ",
                          "severidad; el resto de la causa no est\u00e1 en esta corrida"),
                   paste(b$componente$sequela_ids, collapse = ", "), .dl_texto_yaml(cfg$anchor$componente$motivo),
                   b$componente$fraccion_prevalencia, b$componente$fraccion_yld)),
    if (gate_ancla > .DL_GATE_ERR_MEDIANO_DEFECTO)
      list(sprintf("umbral de anchor_identity relajado a %.3f (0.05 por defecto) \u2014 %s", gate_ancla,
                   .dl_texto_yaml(cfg$anchor$gate_err_mediano$procedencia))),
    if (.dl_factor_sd_emr(cfg) > 1)
      list(sprintf(paste("prior de la mortalidad en exceso (EMR) relajado \u2014 sd_log \u00d7 %g",
                         "(emr_prior.factor_sd) \u2014 %s"),
                   .dl_factor_sd_emr(cfg), .dl_texto_yaml(cfg$emr_prior$factor_sd$procedencia))),
    if (length(cfg$remision$por_edad))
      list(sprintf("remisi\u00f3n por tramo de edad \u2014 %s (fuera de los tramos, remisi\u00f3n %g)",
                   paste(vapply(cfg$remision$por_edad, function(t)
                     sprintf("%g/a\u00f1o en [%g, %g)", t$valor, t$edad_inicio, t$edad_fin), ""), collapse = "; "),
                   as.numeric(cfg$remision$valor %||% 0))),
    if (is.null(casc))
      list("sin cascada \u2014 corrida solo nacional, con las covariables en su valor medio (X sin simular)")
    else if (identical(casc$modo, "plana"))
      list(sprintf(paste0("cascada plana declarada \u2014 tasas nacionales por edad y sexo en cada ubicaci\u00f3n ",
                          "subnacional (dX = 0 en todas las covariables; conteos solo por poblaci\u00f3n); sin ",
                          "gradiente subnacional \u2014 %s"),
                   .dl_texto_yaml(cfg$cascada$modo$procedencia)))
    else c(
      if (!is.null(casc$sustituciones)) lapply(seq_len(nrow(casc$sustituciones)), function(i) {
        su <- casc$sustituciones
        cv <- Filter(function(x) identical(x$covariate_name_short, su$covariate_name_short[i]), cfg$covariables)[[1]]
        sprintf(paste0("cascada \u2014 valor nacional de referencia sustituido para %s (beta de la covariable %d, ",
                       "dX contra la covariable %d %s; con beta en log el ancla se cancela y el gradiente no ",
                       "cambia) \u2014 %s"),
                su$covariate_name_short[i], su$covariate_id_beta[i], su$covariate_id_nacional[i],
                cv$sustituye$covariate_name_short, .dl_texto_yaml(cv$sustituye$procedencia))
      }),
      list(paste0("cascada \u2014 X muestreado por simulaci\u00f3n (el valor nacional una vez por simulaci\u00f3n); ",
                  "supuesto ecol\u00f3gico y kappa declarados; renormalizaci\u00f3n de arriba hacia abajo (a ",
                  "diferencia de GBD)",
                  if (length(canal_prop))
                    sprintf(paste("; proporciones de severidad con el valor nacional de %s en todas las",
                                  "ubicaciones subnacionales"), paste(canal_prop, collapse = ", ")) else ""),
           sprintf(paste0("cascada \u2014 dX por banda de edad del proxy (%s, desplazamiento sobre la malla; ",
                          "edades sin banda con el criterio %s%s) \u2014 an\u00e1logo subnacional de la ",
                          "covariable por edad de DisMod-MR"),
                   if (identical(casc$dx_por_edad$interpolacion, "escalon")) "constante dentro de cada banda"
                   else "interpolado linealmente entre los puntos medios de las bandas",
                   casc$dx_por_edad$fuera_de_banda,
                   if (is.null(casc$dx_por_edad$edades_sin_banda)) ""
                   else sprintf(" en %d-%d", casc$dx_por_edad$edades_sin_banda[1],
                                casc$dx_por_edad$edades_sin_banda[2])))))
}

# ---- Piezas comunes de las corridas (dl_exportar_corrida, dl_reresumir_corrida, dl_sumar_hijas, dl_consolidar) ----
# Disposición de una corrida: <carpeta>/mod/<método>/<run_id>/ con manifest.yaml, cause/<medida>/<run_id>.csv (las
# celdas del contrato estimates/v1) y draws/<medida>_<año>.csv.gz (las simulaciones por celda).

# Métodos de las corridas: subcarpeta de mod/ y `method` de sus celdas, su manifiesto y su entrada en el registro.
.DL_METODO_DISMOD_LITE <- "dismod_lite"
.DL_METODO_CONSOLIDADO <- "consolidado"

# Carpeta de las corridas de un método (sin `run_id`) o de una corrida. Con carpeta = NULL, la ruta relativa a la
# carpeta raíz de las corridas (la que declaran el manifiesto y el registro).
.dl_dir_corrida <- function(carpeta, run_id = NULL, metodo = .DL_METODO_DISMOD_LITE)
  paste(c(carpeta, "mod", metodo, run_id), collapse = "/")

# Archivos de las celdas y de las simulaciones de una medida en la carpeta de una corrida.
.dl_archivo_particion <- function(dir_run, run_id, slug) file.path(dir_run, "cause", slug, paste0(run_id, ".csv"))
.dl_archivo_draws <- function(dir_run, slug, anio) file.path(dir_run, "draws", sprintf("%s_%d.csv.gz", slug, anio))

# Partes de un identificador de corrida AAAA-MM-DD_<nombre>_v<n>: list(fecha, nombre, v). Es la forma de ids.run_id
# del contrato con el nombre libre, porque los registros de versiones anteriores admitían otros nombres. Si no sigue
# esa forma: NULL o, con `donde` (de quién es el identificador, para el mensaje), un error.
.dl_partes_run_id <- function(run_id, donde = NULL) {
  m <- regmatches(run_id, regexec("^([0-9]{4}-[0-9]{2}-[0-9]{2})_(.*)_v([0-9]+)$", run_id))[[1]]
  if (length(m) == 4L) return(list(fecha = m[2], nombre = m[3], v = as.integer(m[4])))
  if (!is.null(donde))
    .dl_stop("el run_id %s %s no sigue el patr\u00f3n AAAA-MM-DD_<nombre>_v<n>", run_id, donde)
  NULL
}

# Identificador de una corrida nueva: AAAA-MM-DD_<nombre>_v<n>, con n = 1 + la mayor versión de hoy con ese nombre
# en <carpeta>/mod/<method>/. El nombre se compara tal cual (no como expresión regular). `otros`: identificadores
# que cuentan como existentes aunque no estén en la carpeta (el de la corrida de origen de un re-resumen escrito en
# otra carpeta, para que la corrida nueva no repita su identificador).
.dl_run_id <- function(out_root, run_slug, method = .DL_METODO_DISMOD_LITE, otros = character()) {
  fecha <- format(Sys.Date())
  previos <- c(list.dirs(.dl_dir_corrida(out_root, metodo = method), recursive = FALSE, full.names = FALSE), otros)
  v <- vapply(previos, function(id) {
    p <- .dl_partes_run_id(id)
    if (identical(p$fecha, fecha) && identical(p$nombre, run_slug)) p$v else 0L
  }, 0L)
  sprintf("%s_%s_v%d", fecha, run_slug, 1L + max(c(0L, v)))
}

# `x` (una tabla de celdas o de etiquetas) copiado, con la columna run_id primero.
.dl_con_run_id <- function(x, run_id) {
  x <- data.table::as.data.table(x)
  data.table::set(x, j = "run_id", value = run_id)
  data.table::setcolorder(x, "run_id")
}

# Escribe las celdas de cada medida exportable en cause/<medida>/<run_id>.csv bajo dir_run (o bajo dir_run/<sub>) y
# devuelve las particiones que declaran el manifiesto y el registro, con la ruta relativa a la carpeta raíz de las
# corridas: mod/<method>/<run_id>/[<sub>/]cause/<medida>/<run_id>.csv, su sha256 y su número de filas.
.dl_escribir_particiones <- function(celdas, dir_run, run_id, method = .DL_METODO_DISMOD_LITE, sub = NULL) {
  raiz <- if (is.null(sub)) dir_run else file.path(dir_run, sub)
  rel <- paste(c(.dl_dir_corrida(NULL, run_id, method), sub), collapse = "/")
  lapply(seq_len(nrow(.DL_MEDIDAS_EXPORTA)), function(k) {
    slug <- .DL_MEDIDAS_EXPORTA$slug[k]
    p <- .dl_archivo_particion(raiz, run_id, slug)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    parte <- celdas[measure_id == .DL_MEDIDAS_EXPORTA$measure_id[k]]
    data.table::fwrite(parte, p, eol = "\n")
    list(path = .dl_archivo_particion(rel, run_id, slug), sha256 = digest::digest(file = p, algo = "sha256"),
         rows = nrow(parte))
  })
}

# fwrite y gzip con R base: no todas las compilaciones de data.table traen zlib para fwrite(compress = "gzip").
.dl_fwrite_gz <- function(dt, destino) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  data.table::fwrite(dt, tmp, eol = "\n")
  inn <- file(tmp, "rb"); out <- gzfile(destino, "wb")
  on.exit({ close(inn); close(out) }, add = TRUE)
  while (length(chunk <- readBin(inn, raw(), 1048576L))) writeBin(chunk, out)
  invisible(destino)
}

# Lee un CSV comprimido (gzip) con location_id como texto: el ubigeo departamental lleva cero inicial ("01") y leído
# como número (1) no se encuentra en el catálogo de ubicaciones. El archivo entero se descomprime en memoria.
.dl_leer_gz <- function(p, colClasses = list(character = "location_id"), ...)
  data.table::fread(text = rawToChar(memDecompress(readBin(p, "raw", file.size(p)), "gzip")),
                    colClasses = colClasses, ...)

# Celdas de una medida de una corrida escrita, con location_id, round y run_id como texto.
.dl_leer_particion_corrida <- function(dir_run, run_id, slug) {
  p <- .dl_archivo_particion(dir_run, run_id, slug)
  if (!file.exists(p)) .dl_stop("la corrida %s no tiene %s", run_id, p)
  data.table::fread(p, colClasses = list(character = c("location_id", "round", "run_id")))
}

# Simulaciones guardadas de una medida (una columna draw_<k> por simulación), ordenadas por celda.
.dl_leer_draws <- function(dir_run, slug, anio) {
  p <- .dl_archivo_draws(dir_run, slug, anio)
  if (!file.exists(p))
    .dl_stop(paste0("la corrida %s no guard\u00f3 las simulaciones de %s (%s); exp\u00f3rtala con ",
                    "guardar_simulaciones = TRUE"), basename(dir_run), slug, p)
  data.table::setorder(.dl_leer_gz(p), location_id, sex_id, age_group_id)
}

# manifest.yaml de una corrida (`que` la nombra si falta). .dl_leer_manifest() lo memoriza; los valores por defecto
# de los manifiestos anteriores los pone quien lee cada clave. .dl_releer_manifest() lo lee para volver a escribirlo
# (dl_reresumir_corrida()), sin memorizar y con las secuencias como listas: yaml::read_yaml pliega una de un elemento
# a un escalar, y el emisor la reescribiría como escalar.
.dl_leer_manifest <- function(dir_run, que = "la carpeta de la corrida")
  .dl_leer_memo(.dl_ruta_manifest(dir_run, que), .dl_leer_yaml)
.dl_releer_manifest <- function(dir_run, que = "la carpeta de la corrida")
  .dl_leer_yaml(.dl_ruta_manifest(dir_run, que), handlers = list(seq = function(x) as.list(x)))
.dl_ruta_manifest <- function(dir_run, que) {
  p <- file.path(dir_run, "manifest.yaml")
  if (!file.exists(p)) .dl_stop("%s no tiene manifest.yaml: %s", que, dir_run)
  p
}

# Fracción aguda del csmr que declara el manifiesto de una corrida (0 si no la declara).
.dl_man_fraccion_aguda <- function(man) as.numeric(man$params$csmr_fraccion_aguda %||% 0)

# Primeras claves del manifiesto de toda corrida (contrato estimates/v1). `generado` es la fecha del run_id: la
# misma aunque la corrida se escriba pasada la medianoche.
.dl_manifiesto_base <- function(run_id, metodo, ronda)
  list(schema = "estimates/v1", run_id = run_id, method = metodo, source = .DL_STD_SOURCE, round = ronda,
       entities = list("cause"), generado = .dl_partes_run_id(run_id)$fecha)

# Escribe <dir_run>/manifest.yaml: una línea de comentario y el manifiesto en YAML de bloque.
.dl_escribir_manifest <- function(man, dir_run) {
  writeLines(c("# manifiesto de una corrida mod/dismod_lite (contrato estimates/v1)",
               sub("\n$", "", .dl_yaml_block(man))), file.path(dir_run, "manifest.yaml"))
}

# Cierre de una corrida escrita: con `registro`, su entrada (tomada del manifiesto) al final del registro de corridas;
# devuelve el objeto dl_run, con los campos de `...` (`origen`, en un re-resumen).
.dl_corrida_escrita <- function(man, dir_run, registro = NULL, ...) {
  if (!is.null(registro))
    .dl_registry_append(registro, list(run_id = man$run_id, method = man$method, source = man$source,
                                       round = as.character(man$round), generado = man$generado,
                                       status = "active", partitions = man$files))
  structure(list(run_id = man$run_id, dir = dir_run, manifest = man, files = man$files, ...), class = "dl_run")
}

# Clave cascada.haqi_nacional del manifiesto: FALSE solo si la cascada aplicó un proxy departamental del HAQ
# (covariate_name_short "haqi"); una cascada plana o sin ese proxy usa el valor nacional en todos los departamentos.
.dl_haqi_nacional <- function(casc) !("haqi" %in% unique(casc$dX$covariate_name_short))

# Manifiesto (manifest.yaml) de una corrida de dl_exportar_corrida(): identificación, causa, parámetros, insumos,
# particiones, cascada, datos, validación y limitaciones. `celdas` son las de la corrida (con run_id);
# `convergencia` = list(rhat_max, ess_min, force). Los números calculados van redondeados: el emisor exige que el
# texto vuelva al mismo número. `archivos` y `tablas_hash` (particiones e insumos congelados) solo se conocen
# después de escribirlos; sin ellos, el manifiesto se prueba antes de escribir nada.
.dl_manifiesto_corrida <- function(celdas, f, b, res, validacion, convergencia, gate_ancla, archivos = list(),
                                   tablas_hash = list(), contrato_hash = list()) {
  cfg <- b$cfg
  casc <- if (inherits(f, "dl_cascade")) f else NULL
  amplitud <- attr(validacion, "amplitud")
  c(.dl_manifiesto_base(celdas$run_id[1], .DL_METODO_DISMOD_LITE, as.character(celdas$round[1])), list(
    causa = list(cause_id = as.integer(cfg$cause_id), cause_name = celdas$cause_name[1],
                 # causa de la extracción de las betas (extraction.cause_id) cuando no es la causa modelada (por
                 # ejemplo, un subtipo con la extracción de su causa padre)
                 extraction_cause_id = as.integer(cfg$extraction$cause_id %||% cfg$cause_id),
                 # componente de la causa: secuelas modeladas y fracciones de prevalencia y AVD de la partición
                 # de severidad
                 componente = if (!is.null(b$componente)) list(
                   sequela_ids = as.list(as.integer(b$componente$sequela_ids)),
                   fraccion_prevalencia = round(as.numeric(b$componente$fraccion_prevalencia), 6),
                   fraccion_yld = round(as.numeric(b$componente$fraccion_yld), 6)) else NULL),
    params = list(seed = as.integer(f$seed), draws = as.integer(f$params$draws),
                  chains = as.integer(f$params$chains), iter = as.integer(f$params$iter),
                  warmup = as.integer(f$params$warmup), thin = as.integer(f$params$thin),
                  year = as.integer(celdas$year[1]), anio_ancla = .dl_anio_ancla(cfg), remision = cfg$remision$valor,
                  # remisión por tramo de edad: [edad_inicio, edad_fin) con su valor; fuera de los tramos rige
                  # `remision`
                  remision_por_edad = if (length(cfg$remision$por_edad))
                    lapply(cfg$remision$por_edad, function(t)
                      list(edad_inicio = as.numeric(t$edad_inicio), edad_fin = as.numeric(t$edad_fin),
                           valor = as.numeric(t$valor))) else NULL,
                  # factor sobre sd_log del prior de la EMR; 1 = el prior csmr/prev tal cual
                  emr_sd_factor = .dl_factor_sd_emr(cfg),
                  emr_prior = cfg$emr_prior$tipo,
                  # fracción aguda del csmr descontada del prior de la EMR y de su techo
                  csmr_fraccion_aguda = .dl_fraccion_aguda(cfg),
                  # techo de la EMR: el de la configuración («impreso») o derivado del ancla (k * max csmr/prev)
                  emr_cota = as.list(round(as.numeric(cfg$emr_prior$cota), 6)),
                  emr_cota_origen = b$techo_emr$origen %||% "impreso",
                  emr_cota_k = if (identical(b$techo_emr$origen, "derivado")) as.numeric(b$techo_emr$k) else NULL,
                  lambda = as.numeric(cfg$anchor$lambda),
                  # fuentes locales no fatales que informaron el ancla (con alguna, lambda debe ser < 1)
                  fuentes_locales_no_fatales = as.list(b$fuentes_locales$nid_no_fatal %||% integer()),
                  rho = as.numeric(cfg$anchor$rho_edad),
                  kappa = if (is.null(casc)) NULL else as.numeric(casc$kappa),
                  anchor = cfg$anchor$location %||% .dl_loc_ancla(cfg), engine = f$params$engine,
                  opts = unclass(f$params),
                  # val = media de las simulaciones; lower y upper = sus cuantiles
                  estadistico_puntual = .DL_ESTADISTICO_PUNTUAL,
                  version_paquete = dl_version()),
    inputs = c(list(bundle_hash = b$hash,
                  tablas = lapply(names(tablas_hash), function(nm)
                    list(tabla = sub("[.]csv$", "", nm), sha256 = tablas_hash[[nm]]))),
                  # las tablas del contrato congeladas en inputs/contrato/ (sin la clave en el formato completo)
                  if (length(contrato_hash)) list(contrato = lapply(names(contrato_hash), function(nm)
                    list(tabla = nm, sha256 = contrato_hash[[nm]]))),
                  list(
                  # qué modelos de la extracción entraron a las betas y cuántas filas quedaron fuera
                  betas = if (!is.null(b$seleccion_betas)) list(
                    modelo_variante = as.list(b$seleccion_betas$modelo_variante %||% "(todas)"),
                    filas_excluidas = as.integer(b$seleccion_betas$filas_excluidas),
                    variantes_excluidas = as.list(b$seleccion_betas$variantes_excluidas),
                    sin_covariable = as.list(b$seleccion_betas$sin_covariable)),
                  # escala de la covariable con que se estimó cada beta lineal (por ejemplo, 0-100 o 0-1)
                  escalas = lapply(seq_len(nrow(b$betas)), function(i) list(
                    covariate_name_short = b$betas$covariate_name_short[i],
                    parametro_objetivo = b$betas$parametro_objetivo[i], escala = as.numeric(b$betas$escala[i]))))),
    files = archivos,
    cascada = if (is.null(casc)) NULL else list(
      # con la cascada plana, su procedencia va en las limitaciones (el emisor no pliega escalares largos)
      modo = casc$modo %||% "proxy",
      departamentos = length(casc$departamentos),
      proxies = as.list(unique(casc$dX$covariate_name_short)),
      renorm_min = round(min(casc$renorm$factor), 6), renorm_max = round(max(casc$renorm$factor), 6),
      truncados_emr = as.integer(casc$truncados_emr), supuesto_ecologico = !identical(casc$modo, "plana"),
      haqi_nacional = .dl_haqi_nacional(casc),
      # valor nacional de referencia sustituido (covariables[].sustituye); la procedencia va en las limitaciones
      sustituciones = if (is.null(casc$sustituciones)) list() else
        lapply(seq_len(nrow(casc$sustituciones)), function(i) list(
          covariate_name_short = casc$sustituciones$covariate_name_short[i],
          covariate_id_beta = as.integer(casc$sustituciones$covariate_id_beta[i]),
          covariate_id_nacional = as.integer(casc$sustituciones$covariate_id_nacional[i]))),
      dx_por_edad = if (is.null(casc$dx_por_edad)) NULL else list(
        bandas_por_proxy = lapply(split(casc$dX$age_group_id, casc$dX$covariate_name_short),
                                  function(v) as.integer(sort(unique(v)))),
        fuera_de_banda = casc$dx_por_edad$fuera_de_banda,
        interpolacion = casc$dx_por_edad$interpolacion,
        edades_sin_banda = if (is.null(casc$dx_por_edad$edades_sin_banda)) "ninguna"
                           else paste(casc$dx_por_edad$edades_sin_banda, collapse = "-"))),
    datos = list(n_likelihood_por_tipo = as.list(table(b$datos[location_level == 0L & !outlier]$tipo_dato)),
                 n_heldout = nrow(b$datos[location_level == 1L]),
                 heldout_anio = .dl_anio_heldout(cfg)),
    # el emisor no cita «: »; sin decisiones, una lista vacía
    decisiones = if (length(unlist(cfg$decisiones))) as.list(.dl_texto_yaml(unlist(cfg$decisiones))) else list()),
    # configuración simple: su formato y las claves tomadas por defecto, con su valor (la clave solo existe en ese
    # formato)
    if (.dl_es_simple(cfg)) list(configuracion = list(formato = "simple", por_defecto = lapply(
      as.list(cfg$origen$por_defecto), .dl_texto_yaml))),
    list(
    validacion = c(
      list(gates = list(rhat_max = round(convergencia$rhat_max, 6), ess_min = round(convergencia$ess_min, 1),
                        force = convergencia$force)),
      if (!is.null(validacion) && !is.null(attr(validacion, "resumen"))) {
        rv <- attr(validacion, "resumen"); vd <- data.table::as.data.table(validacion)
        c(list(anchor_identity = list(
            err_rel_mediano = round(rv[check == "anchor_identity"]$err_rel_mediano, 6),
            celdas_fuera_ic95 = as.integer(vd[check == "anchor_identity", sum(!cubierto_ic95)]),
            gate_err_mediano = gate_ancla)),
          if ("implied_incidence" %in% rv$check)
            list(implied_incidence = list(
              err_rel_mediano = round(rv[check == "implied_incidence"]$err_rel_mediano, 6),
              cobertura = round(rv[check == "implied_incidence"]$cobertura, 6))))
      },
      if (!is.null(amplitud)) list(amplitud_csmr = lapply(split(amplitud, amplitud$sex_id), function(r)
        list(sex_id = as.integer(r$sex_id), kappa = as.numeric(r$kappa), pendiente = round(r$pendiente, 4),
             pendiente_lower = round(r$pendiente_lower, 4), pendiente_upper = round(r$pendiente_upper, 4),
             spearman = round(r$spearman, 4), n_celdas_spearman = as.integer(r$n_celdas_spearman),
             n_celdas = as.integer(r$n_celdas),
             n_departamentos = as.integer(r$n_departamentos),
             pendiente_std = round(r$pendiente_std, 4), pendiente_std_lower = round(r$pendiente_std_lower, 4),
             pendiente_std_upper = round(r$pendiente_std_upper, 4), spearman_std = round(r$spearman_std, 4),
             n_departamentos_std = as.integer(r$n_departamentos_std))))),
    limitaciones = .dl_limitaciones_corrida(cfg, b, res, casc, validacion, gate_ancla)))
}

# Compuerta de la convergencia de las cadenas del ajuste `f`: R-hat < 1.01 y ESS >= ess_minimo, o un error.
# forzar = TRUE la salta y queda en el manifiesto (validacion.gates.force), que registra lo que devuelve. La usan
# dl_exportar_corrida() y dl_correr(), justo después del ajuste; `remedio`: qué hacer, con los argumentos de cada una.
.dl_compuerta_convergencia <- function(f, forzar, ess_minimo = 400, remedio = paste0(
  "Aumenta `iteraciones` y `calentamiento` en dl_opciones_mcmc(); forzar = TRUE exporta igual (solo para pruebas) ",
  "y lo declara en el manifiesto")) {
  convergencia <- list(rhat_max = max(f$mcmc$rhat), ess_min = min(f$mcmc$ess), force = forzar)
  if (!(convergencia$rhat_max < 1.01 && convergencia$ess_min >= ess_minimo) && !forzar)
    .dl_stop(paste0("las cadenas no convergieron: R-hat m\u00e1ximo %.4f (debe ser < 1.01) y ESS m\u00ednimo %.0f ",
                    "(debe ser >= %s). %s"), convergencia$rhat_max, convergencia$ess_min, format(ess_minimo), remedio)
  convergencia
}

# Compuerta del error del ancla de la validación `validacion` (anchor_identity: error relativo mediano de la
# prevalencia por banda): un error si pasa el máximo, que devuelve. forzar no la salta: el máximo solo se relaja en la
# configuración (anchor.gate_err_mediano, con su procedencia), y entonces queda declarado entre las limitaciones. La
# usan dl_exportar_corrida() y dl_correr(), justo después de la validación; `remedio`: dónde se declara el máximo y
# qué no salta la compuerta, con los argumentos de cada una.
.dl_compuerta_ancla <- function(cfg, validacion, remedio = paste0(
  "declara un m\u00e1ximo mayor en la configuraci\u00f3n, con su procedencia: anchor: {gate_err_mediano: {valor: ..., ",
  "procedencia: ...}} (en un proyecto simple, dentro de avanzado; ver ?dl_configuracion); forzar no la salta")) {
  gate_ancla <- as.numeric(cfg$anchor$gate_err_mediano$valor %||% .DL_GATE_ERR_MEDIANO_DEFECTO)
  rv <- attr(validacion, "resumen")
  err_ancla <- if (!is.null(rv)) rv[check == "anchor_identity"]$err_rel_mediano
  if (length(err_ancla) && is.finite(err_ancla) && err_ancla > gate_ancla)
    .dl_stop(paste0("la prevalencia del ajuste se aleja del ancla (error relativo mediano %.3f, m\u00e1ximo %.3f). ",
                    "Revisa el ajuste (cadenas m\u00e1s largas) y los datos o %s"), err_ancla, gate_ancla, remedio)
  gate_ancla
}

#' Exportar una corrida
#'
#' Escribe la corrida en `<carpeta>/mod/dismod_lite/<AAAA-MM-DD>_<nombre>_v<n>/`: celdas por medida (`cause/`),
#' simulaciones (`draws/`), etiquetas, diagnósticos, insumos congelados (`inputs/`) y `manifest.yaml`. Antes de
#' escribir comprueba la convergencia (R-hat < 1.01 y ESS >= `ess_minimo`, salvo `forzar = TRUE`) y el error del
#' ancla, y valida las celdas contra el contrato. Con `registro`, agrega la corrida al registro de corridas.
#'
#' @details
#' La carpeta de la corrida:
#' - `cause/<medida>/<run_id>.csv`: las celdas de `prevalence`, `incidence` y `yld` (las de [dl_resumir()], con
#'   `run_id`), validadas contra el contrato `estimates/v1` ([dl_validar_estimaciones()]).
#' - `draws/<medida>_<año>.csv.gz`: las simulaciones por celda (con `guardar_simulaciones = TRUE`); con ellas
#'   [dl_reresumir_corrida()] y [dl_sumar_hijas()] trabajan sin volver a ajustar.
#' - `etiquetas/<run_id>.csv`: las etiquetas de [dl_etiquetas()].
#' - `diagnostics/`: `mcmc.csv` (R-hat y ESS), `aceptacion.csv`, `desplazamiento_splits.csv`, `validacion.csv`
#'   ([dl_validar_ancla()]), `sensibilidad.csv` ([dl_sensibilidad()], si se da) y, con una cascada, `renorm.csv`
#'   (los factores de renormalización), `dx_bandas.csv` (la mediana y el intervalo de dX por ubicación, sexo,
#'   covariable y banda) y `amplitud.csv` (la validación de la amplitud, si se calculó).
#' - `inputs/`: los insumos congelados ([dl_congelar_insumos()]), la configuración usada (`config_usado.yaml`) y las
#'   descargas de covariables (`ghdx_cov/`, solo con los insumos del formato completo). Con un proyecto de las tablas
#'   del contrato ([dl_proyecto()]), `inputs/contrato/<tabla>.csv` guarda las tablas que se usaron (con las
#'   covariables nacionales; los números escritos exactos) y el manifiesto registra su sha256 en `inputs$contrato`:
#'   con ellas la corrida se repite sin la carpeta original, también si las tablas se dieron como data.frame.
#' - `manifest.yaml`: la descripción de la corrida (abajo).
#'
#' El identificador `<AAAA-MM-DD>_<nombre>_v<n>` lleva la fecha del día y la versión siguiente a la mayor de ese día
#' con ese nombre en `carpeta`: una corrida nunca reemplaza a otra. Si algo falla a mitad de la escritura, la carpeta
#' a medio escribir se borra.
#'
#' `manifest.yaml` describe la corrida: identificación, causa, parámetros, insumos (con el sha256 de cada tabla
#' congelada), cascada, datos, `decisiones` (las de la configuración; una lista vacía si no hay), validación y
#' limitaciones; en un proyecto con las tablas del contrato de insumos, además, `configuracion` (el formato y las
#' claves que tomaron su valor por defecto). En la cascada, `haqi_nacional` se conserva por compatibilidad con la versión 0.2.2: es `false` solo si la
#' cascada aplicó un proxy subnacional de una covariable llamada `haqi`; si no (también en un proyecto sin esa
#' covariable), es `true`.
#'
#' `forzar = TRUE` salta la convergencia, no el error del ancla: si la prevalencia ajustada se aleja de la del ancla
#' (error relativo mediano mayor que `anchor.gate_err_mediano`, 0.05 por defecto), la corrida no se escribe. Ese
#' máximo se declara, con su procedencia, en la configuración (en un proyecto, en `avanzado`; ver
#' [dl_configuracion()]).
#'
#' @param piezas Lista con `resumen` ([dl_resumir()]), `fit` (ajuste o cascada), `yld` ([dl_avd()]) y `bundle`
#'   (insumos), las mismas con que se hizo el resumen; también valen los nombres `ajuste`, `avd` e `insumos`.
#' @param nombre Nombre corto de la corrida: minúsculas sin tildes, números y guiones (por ejemplo
#'   `"acs-nacional"`).
#' @param carpeta Carpeta raíz de las corridas.
#' @param etiquetas Etiquetas de [dl_etiquetas()] (obligatorias).
#' @param validacion Resultado de [dl_validar_ancla()] (opcional).
#' @param sensibilidad Resultado de [dl_sensibilidad()] (opcional).
#' @param guardar_simulaciones `TRUE` guarda las simulaciones por celda en `draws/`.
#' @param registrar `TRUE` agrega la corrida al registro `registro`.
#' @param forzar `TRUE` exporta aunque falle la convergencia (queda declarado en el manifiesto).
#' @param ess_minimo Tamaño efectivo de muestra mínimo.
#' @param registro Archivo YAML del registro de corridas, que ya existe (un registro nuevo es un archivo con la
#'   línea `datasets: []`); opcional.
#' @param rutas Rutas de [dl_rutas()]; se usan los catálogos y la carpeta de covariables. `NULL` (por defecto) usa
#'   las de los insumos.
#' @return Objeto de clase `dl_run`, una lista con `run_id` (el identificador de la corrida), `dir` (su carpeta),
#'   `manifest` (el contenido de `manifest.yaml`, como lista) y `files` (las tablas de celdas escritas, una por
#'   medida: `path`, relativa a `carpeta`, `sha256` y `rows`).
#' @seealso [dl_correr()] (todas las etapas en una llamada), [dl_resumir()] (el paso anterior),
#'   [dl_reresumir_corrida()], [dl_sumar_hijas()] y [dl_consolidar()] (lo que se hace con las corridas).
#' @family corrida
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' f0 <- dl_ajustar_solo_prior(b, op, semilla = 1, ajuste = f)
#' cas <- dl_cascada(f, b, semilla = 1)
#' y <- dl_avd(cas, b, comorbilidad = dl_factor_comorbilidad(b), semilla = 1)
#' et <- dl_etiquetas(f, f0, b, grilla_rho = 0.5, semilla = 1, cascada = cas)
#' v <- dl_validar_ancla(f, b, cascada = cas)
#' piezas <- list(ajuste = cas, avd = y, insumos = b)
#' piezas$resumen <- dl_resumir(piezas)
#' # cadenas cortas: forzar = TRUE escribe la corrida aunque no hayan convergido (el manifiesto
#' # lo declara); una corrida para publicar no lo usa
#' salida <- file.path(tempdir(), "corridas")
#' run <- dl_exportar_corrida(piezas, nombre = "ejemplo", carpeta = salida, etiquetas = et,
#'                            validacion = v, forzar = TRUE)
#' run
#' list.files(run$dir, recursive = TRUE)
#' run$manifest$validacion
#' unlink(salida, recursive = TRUE)
#' }
#' @export
dl_exportar_corrida <- function(piezas, nombre, carpeta = Sys.getenv("DATA_ROOT"), etiquetas,
                                validacion = NULL, sensibilidad = NULL, guardar_simulaciones = TRUE,
                                registrar = !is.null(registro), forzar = FALSE, ess_minimo = 400,
                                registro = NULL, rutas = NULL) {
  .dl_exigir_registro(registrar, registro)
  if (missing(nombre)) .dl_stop("falta `nombre` (nombre corto de la corrida, p. ej. \"acs-nacional\")")
  .dl_exigir_nombre(nombre)
  .dl_exigir_carpeta(carpeta)
  piezas <- .dl_piezas(piezas, c("resumen", "fit", "yld", "bundle"))
  res <- piezas$resumen; f <- piezas$fit; y <- piezas$yld; b <- piezas$bundle
  niv <- unique(res$celdas$ui_level); contrato <- .dl_schema_estimates()$ui_level
  if (!isTRUE(all(niv == contrato)))
    .dl_stop(paste0("el resumen se hizo con `nivel` = %s en dl_resumir(); para escribir la corrida, ",
                    "el contrato estimates/v1 exige nivel = %s: vuelve a correr dl_resumir() con ese nivel"),
             paste(format(niv), collapse = ", "), format(contrato))
  if (missing(etiquetas) || !is.data.frame(etiquetas)) .dl_stop("falta `etiquetas` (la tabla de dl_etiquetas())")
  for (k in c("resumen", "fit", "yld")) .dl_exigir_mismos_insumos(piezas[[k]], b, paste0("piezas$", k))
  .dl_chequear_avd_de_ajuste(f, y)
  if ((!is.null(res$huella_ajuste) && !identical(res$huella_ajuste, .dl_huella_ajuste(f))) ||
      (!is.null(res$huella_avd) && !identical(res$huella_avd, .dl_huella_avd(y))))
    .dl_stop("el `resumen` no se hizo con este `fit` y este `yld`: vuelve a correr dl_resumir() con las mismas piezas")
  rutas <- .dl_resolver_rutas(rutas, b)
  carpeta_cov <- .dl_path(rutas, "ghdx_cov", opcional = TRUE)   # se copia a inputs/ghdx_cov
  cfg <- b$cfg
  casc <- if (inherits(f, "dl_cascade")) f else NULL

  # 1. Las compuertas: la convergencia de las cadenas y el error del ancla.
  convergencia <- .dl_compuerta_convergencia(f, forzar, ess_minimo)
  gate_ancla <- .dl_compuerta_ancla(cfg, validacion)

  # 2. Identificador AAAA-MM-DD_<nombre>_v<n> y celdas con su run_id, validadas contra el contrato en memoria.
  run_id <- .dl_run_id(carpeta, nombre)
  celdas <- .dl_con_run_id(res$celdas, run_id)
  dl_validar_estimaciones(celdas, rutas)
  manifiesto <- function(archivos = list(), tablas_hash = list(), contrato_hash = list())
    .dl_manifiesto_corrida(celdas, f, b, res, validacion, convergencia, gate_ancla, archivos, tablas_hash,
                           contrato_hash)
  # El emisor falla aquí, antes de escribir, si un valor no se puede escribir en el manifiesto (p. ej. lambda = 1/3).
  .dl_yaml_block(manifiesto())

  # 3. Carpeta de la corrida. Si algo falla antes de escribir el manifiesto, la carpeta a medio escribir se borra:
  #    así el próximo intento no la deja como una versión más (_v2).
  dir_run <- .dl_dir_corrida(carpeta, run_id)
  escrita <- FALSE
  if (!dir.exists(dir_run)) on.exit(if (!escrita) unlink(dir_run, recursive = TRUE), add = TRUE)
  archivos <- .dl_escribir_particiones(celdas, dir_run, run_id)
  if (guardar_simulaciones) {
    dir.create(file.path(dir_run, "draws"), showWarnings = FALSE)
    for (m in names(res$draws)) .dl_fwrite_gz(res$draws[[m]], .dl_archivo_draws(dir_run, m, celdas$year[1]))
  }
  d <- file.path(dir_run, "etiquetas"); dir.create(d, showWarnings = FALSE)
  data.table::fwrite(.dl_con_run_id(etiquetas, run_id), file.path(d, paste0(run_id, ".csv")), eol = "\n")
  d <- file.path(dir_run, "diagnostics"); dir.create(d, showWarnings = FALSE)
  escribir <- function(x, archivo)
    if (!is.null(x)) data.table::fwrite(data.table::as.data.table(x), file.path(d, archivo), eol = "\n")
  escribir(res$mcmc, "mcmc.csv")
  escribir(res$aceptacion, "aceptacion.csv")
  escribir(res$desplazamiento_splits, "desplazamiento_splits.csv")
  escribir(validacion, "validacion.csv")
  escribir(sensibilidad, "sensibilidad.csv")
  escribir(casc$renorm, "renorm.csv")
  # dX por departamento, sexo, covariable y banda del proxy (mediana e intervalo del 95 % de las simulaciones): la
  # tabla que hace trazable el reparto del gradiente departamental por edad
  if (!is.null(casc) && nrow(casc$dX))
    escribir(casc$dX[, { q <- stats::quantile(dX, c(0.5, 0.025, 0.975), names = FALSE)
                         list(dX_mediana = q[1], dX_lower = q[2], dX_upper = q[3]) },
                     by = list(location_id, sex_id, covariate_name_short, age_group_id)], "dx_bandas.csv")
  escribir(attr(validacion, "amplitud"), "amplitud.csv")
  inputs_dir <- file.path(dir_run, "inputs")
  tablas_hash <- dl_congelar_insumos(b, inputs_dir)
  yaml::write_yaml(cfg, file.path(inputs_dir, "config_usado.yaml"))
  # las tablas del contrato del proyecto (si los insumos vienen de uno): con ellas la corrida se repite sin la carpeta
  # original, también cuando las tablas se dieron como data.frame. Cada columna numérica se escribe con el texto
  # exacto (.dl_num_exacto), no con los 15 dígitos de fwrite: al releerlas vuelven los mismos números
  contrato_hash <- .dl_congelar_contrato(b$contrato, file.path(inputs_dir, "contrato"))
  # con un proyecto del contrato, las covariables nacionales ya van en contrato/covariables.csv: la copia de las
  # descargas crudas (ghdx_cov/) es solo del formato completo
  if (!is.null(carpeta_cov) && !length(b$contrato)) {
    dir.create(file.path(inputs_dir, "ghdx_cov"), showWarnings = FALSE)
    file.copy(list.files(carpeta_cov, pattern = "[.]csv$", ignore.case = TRUE, full.names = TRUE),
              file.path(inputs_dir, "ghdx_cov"))
  }

  # 4. manifest.yaml y, al final, la entrada en el registro de corridas (sin reescribir las anteriores).
  man <- manifiesto(archivos, tablas_hash, contrato_hash)
  .dl_escribir_manifest(man, dir_run)
  escrita <- TRUE
  .dl_corrida_escrita(man, dir_run, if (registrar) registro)
}

# Escribe las tablas del contrato (lista nombrada de dl_tabla) en `dir` como <tabla>.csv y devuelve su sha256 (lista
# nombrada). Las columnas numéricas van con su texto exacto (.dl_num_exacto): releerlas da los mismos doubles. Sin
# tablas (formato completo), no escribe nada y devuelve list().
.dl_congelar_contrato <- function(contrato, dir) {
  hash <- list()
  if (!length(contrato)) return(hash)
  dir.create(dir, showWarnings = FALSE)
  for (nm in names(contrato)) {
    d <- data.table::as.data.table(unclass(contrato[[nm]]))
    for (j in names(d)) if (is.double(d[[j]])) data.table::set(d, j = j, value = .dl_num_exacto(d[[j]]))
    f <- file.path(dir, paste0(nm, ".csv"))
    data.table::fwrite(d, f, eol = "\n", na = "")
    hash[[nm]] <- digest::digest(file = f, algo = "sha256")
  }
  hash
}

# Qué es una corrida, según su manifiesto: un consolidado, una suma de hijas, un re-resumen, o el ajuste nacional o
# la cascada que exportó dl_exportar_corrida().
.dl_tipo_corrida <- function(x) {
  man <- x$manifest
  origen <- x$origen %||% man$resumen$run_origen
  if (identical(man$method, .DL_METODO_CONSOLIDADO)) "consolidado"
  else if (identical(man$causa$agregacion, "suma_de_hijas")) "suma de las hijas"
  else if (!is.null(origen)) sprintf("re-resumen de %s", origen)
  else if (!is.null(man$cascada)) "cascada subnacional"
  else "ajuste nacional"
}

#' @export
print.dl_run <- function(x, ...) {
  forzada <- x$manifest$validacion$gates$force
  cat(sprintf("<dl_run> corrida %s (%s)\n", x$run_id, .dl_tipo_corrida(x)))
  cat(sprintf("  carpeta: %s\n", x$dir))
  cat(sprintf("  %d particiones can\u00f3nicas (una por medida)%s\n", length(x$files),
              if (is.null(forzada)) "" else sprintf(" | exportada con forzar = %s", forzada)))
  invisible(x)
}
