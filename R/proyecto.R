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
          # la barra de la carpeta va fuera de file.path(): en Windows, file.path() quita la barra final
          if (!is.null(carpeta)) sprintf("\u00ab%s\u00bb", c(file.path(carpeta, paste0(tabla, ".csv")),
                                                             paste0(file.path(carpeta, tabla), "/")))),
        collapse = ", ")

# Las tablas del contrato del proyecto, una lista nombrada de dl_tabla sin las que no están: cada una de `dadas` o de
# `carpeta` (.dl_fuente_tabla), por su lector (`opciones`: ubicacion_gbd y metrica_prevalencia, ver
# .dl_leer_fuente_tabla) y validada sola; los lectores reciben también el código de la ubicación nacional de
# ubicaciones (`codigo_nacional`, ver .dl_ubicacion_gbd). Error si falta una tabla obligatoria, con dónde se buscó. Con `paso`
# (la revisión: paso(nombre, expr), como en .dl_traducir_contrato), cada tabla va por su paso, también el error de
# una obligatoria que falta, y no se detiene.
.dl_tablas_proyecto <- function(carpeta, dadas, opciones = list(), paso = NULL) {
  fuentes <- lapply(stats::setNames(nm = .DL_TABLAS), function(t) .dl_fuente_tabla(carpeta, dadas, t))
  faltan <- .DL_TABLAS_OBLIGATORIAS[vapply(fuentes[.DL_TABLAS_OBLIGATORIAS], is.null, NA)]
  donde <- vapply(faltan, .dl_donde_tabla, "", carpeta = carpeta)
  if (length(faltan) && is.null(paso))
    .dl_stop("faltan tablas obligatorias del proyecto (o solo traen el encabezado):\n%s",
             paste(sprintf("  - %s: se busc\u00f3 en %s", faltan, donde), collapse = "\n"))
  paso <- paso %||% function(nombre, expr) expr
  for (t in faltan)
    paso(t, .dl_stop("falta la tabla (o solo trae el encabezado): va en %s.csv o en la carpeta %s/ del proyecto",
                     t, t))
  leer <- function(t) if (!is.null(fuentes[[t]])) paso(t, .dl_tabla_proyecto(fuentes[[t]], t, opciones))
  tablas <- list(ubicaciones = leer("ubicaciones"))
  if (!is.null(tablas$ubicaciones)) {
    nacional <- .dl_ubicacion_nacional(tablas)
    if (!is.na(nacional)) opciones$codigo_nacional <- nacional
  }
  for (t in setdiff(names(Filter(Negate(is.null), fuentes)), "ubicaciones")) tablas[t] <- list(leer(t))
  Filter(Negate(is.null), tablas)
}

# La tabla `tabla` del contrato desde `x` (data.frame o ruta): su lector y la validación de una tabla sola, como
# dl_tabla(). Atributos `lectores` y `ubicacion_gbd`: los de .dl_leer_fuente_tabla (vacíos si todo era del contrato).
.dl_tabla_proyecto <- function(x, tabla, opciones) {
  origen <- if (is.data.frame(x)) sprintf("argumento `%s` (data.frame)", tabla) else x
  d <- .dl_leer_fuente_tabla(x, tabla, opciones)
  t <- .dl_tabla_contrato(d, tabla, origen)
  data.table::setattr(t, "lectores", attr(d, "lectores"))
  data.table::setattr(t, "ubicacion_gbd", attr(d, "ubicacion_gbd"))
  t
}

# Opciones de los lectores desde la configuración `s` del proyecto: ubicacion_gbd y la métrica de la prevalencia de
# las descargas de GBD Results (Rate; Percent si la configuración pide anchor.metrica_prevalencia en avanzado).
.dl_opciones_lectores <- function(s)
  list(ubicacion_gbd = s[["ubicacion_gbd"]],
       metrica_prevalencia = .dl_valor_en(s, "avanzado.anchor.metrica_prevalencia.valor") %||% "Rate")

# La tabla severidad desde la corrida de partición `corrida` (la de severidad.particion de la configuración `s`), con
# la causa padre (severidad.padre) y las secuelas del componente (componente.secuelas). Sus límites (inferior y
# superior) pueden pasar de 1: en una hija o un componente son los de la partición divididos por su cuota. Se aceptan,
# como en la versión 0.2.2, con un aviso (.dl_avisar_limites_particion); la tabla severidad que escribe el usuario
# sigue en [0, 1].
.dl_severidad_particion <- function(s, causa, corrida) {
  secuelas <- unlist(.dl_valor_en(s, "componente.secuelas"))
  padre <- .dl_valor_en(s, "severidad.padre")
  sev <- .dl_severidad_contrato_desde_particion(corrida, causa, padre = if (!is.null(padre)) as.integer(padre),
                                                secuelas = if (length(secuelas)) as.integer(secuelas))
  .dl_avisar_limites_particion(sev, corrida)
  .dl_tabla_contrato(sev, "severidad", corrida, sin_maximo = .DL_LIMITES_PARTICION)
}

# Los límites de la proporción de la severidad que sale de una partición, que pueden pasar de 1.
.DL_LIMITES_PARTICION <- c("inferior", "superior")

# Aviso: los estados de la severidad `sev` (de la partición `corrida`) con un límite de su proporción mayor que 1.
.dl_avisar_limites_particion <- function(sev, corrida) {
  lim <- pmax(sev$inferior, sev$superior, na.rm = TRUE)
  k <- which(lim > 1)
  if (!length(k)) return(invisible())
  .dl_warn(paste0("severidad.particion: el l\u00edmite superior de la proporci\u00f3n pasa de 1 en el/los estado(s) %s ",
                  "(hasta %s) de la partici\u00f3n %s. Sale de dividir los l\u00edmites de la partici\u00f3n por la ",
                  "cuota de la causa hija (severidad.padre) o del componente (componente.secuelas) en ella. Se acepta ",
                  "como en la versi\u00f3n 0.2.2: los AVD toman de ese intervalo la incertidumbre de la proporci\u00f3n ",
                  "y, si es demasiado ancho para su media, el estado entra como constante en su media (con otro aviso)"),
           paste(sev$estado[k], collapse = ", "), format(signif(max(lim[k]), 4L)), basename(corrida))
}

# ---- La configuración de un proyecto ----

# Las causas con configuración del proyecto (causa, nombre, subtipos): las de `carpeta` o, sin carpeta, la de la
# configuración `s` de `causa`. Dan la causa padre de un subtipo y los nombres del catálogo de causas.
.dl_causas_config <- function(carpeta, s, causa) {
  if (!is.null(carpeta)) return(.dl_configs_proyecto(carpeta)[!is.na(causa) & simple, list(causa, nombre, subtipos)])
  data.table::data.table(causa = causa, nombre = if (.dl_es_texto1(s[["nombre"]])) s[["nombre"]] else NA_character_,
                         subtipos = list(suppressWarnings(as.integer(unlist(s[["subtipos"]])))))
}

# Las causas que declaran a `causa` en `subtipos` (`causas`, .dl_causas_config).
.dl_padres_de <- function(causas, causa) causas$causa[vapply(causas$subtipos, function(h) causa %in% h, NA)]

# La causa padre de `causa`: la que la declara en `subtipos`, o NULL.
.dl_padre_de <- function(causas, causa) {
  padres <- .dl_padres_de(causas, causa)
  if (length(padres)) padres[[1L]]
}

# La causa cuyas betas usa un subtipo sin filas propias: la que declara la configuración `s` (un subtipo en su propia
# carpeta, sin la configuración de su padre) en avanzado.extraction.cause_id, que manda, o en subtipo_de; o NULL. Un
# valor que no es un entero lo rechaza después la validación. Sin ella, la causa padre es la que declara al subtipo
# en `subtipos`.
.dl_padre_extraction <- function(s) {
  ec <- .dl_valor_en(s, "avanzado.extraction.cause_id") %||% .dl_valor_en(s, "subtipo_de")
  if (.dl_es_entero1(ec)) as.integer(ec)
}

# Error si el subtipo_de de la configuración `s` de `causa` dice una causa padre y la configuración de otra causa del
# proyecto (`causas`) declara a `causa` en `subtipos`: la relación se dice en un solo sitio o en los dos igual.
.dl_comprobar_subtipo_de <- function(s, archivo, causa, causas) {
  declarado <- .dl_valor_en(s, "subtipo_de")
  if (!.dl_es_entero1(declarado)) return(invisible())
  otros <- setdiff(.dl_padres_de(causas, causa), as.integer(declarado))
  if (length(otros))
    .dl_stop_config_simple(archivo, sprintf(paste0(
      "subtipo_de: es %d y la configuraci\u00f3n de la causa %d declara a la causa %d en su clave `subtipos`: los dos ",
      "deben decir la misma causa padre"), as.integer(declarado), otros[1L], causa))
}

# Lo que la traducción de la configuración de `causa` toma de las tablas (.dl_traducir_config_simple): la ubicación
# nacional (la que no tiene padre en ubicaciones), las betas de la causa (las de `padre` si es un subtipo sin betas
# propias), las covariables con filas subnacionales, si hay ubicaciones subnacionales, el nombre de la causa en el
# ancla, la causa padre, los años con prevalencia de la causa en el ancla (para el año del ancla), las causas con filas
# en la tabla betas, el covariate_id de cada covariable (para valor_nacional_de) y los location_id de GBD que los
# lectores tomaron como el país (ubicacion_gbd; vacío si ninguna tabla vino de una descarga).
.dl_contexto_tablas <- function(tablas, causa, padre = NULL) {
  nacional <- .dl_ubicacion_nacional(tablas)
  betas <- .dl_betas_de_causa(tablas$betas, causa, padre)
  if (!is.null(betas) && !nrow(betas)) betas <- NULL
  cov <- tablas$covariables
  nombres <- if (!is.null(tablas$ancla))          # la revisión traduce la configuración aunque el ancla falle
    stats::na.omit(.dl_col(.dl_filas_de_causa(tablas$ancla, causa), "nombre_causa", NA_character_))
  list(ubicacion = nacional, padre = padre, betas = betas, anios_ancla = .dl_anios_ancla(tablas, causa),
       causas_betas = if (!is.null(tablas$betas)) sort(unique(stats::na.omit(.dl_col(tablas$betas, "causa")))),
       covariables_subnacionales = if (is.null(cov)) character()
                                   else unique(cov$covariable[!.dl_es_nacional(cov, nacional)]),
       subnacional = any(!is.na(.dl_col(tablas$ubicaciones, "padre", NA_character_))),
       nombre = if (length(nombres)) nombres[[1L]], ids_covariable = .dl_ids_covariable(tablas, betas),
       ubicacion_gbd = unique(unlist(lapply(tablas, attr, "ubicacion_gbd"))))
}

# La configuración validada y las tablas del proyecto de la causa `causa`: las tablas (de `dadas` o de `carpeta`), la
# severidad de la partición si la configuración la declara, el contexto que la configuración toma de ellas y su
# traducción (.dl_config_simple, con `cambios`); y la carpeta de esa partición (`particion`, o NULL), que dl_insumos()
# necesita para las fracciones de un componente. `base`: la carpeta de las rutas relativas de la configuración.
.dl_preparar_contrato <- function(s, archivo, causa, carpeta, dadas = list(), cambios = NULL,
                                  base = carpeta %||% .dl_raiz_proyecto(archivo)) {
  if (!is.list(s) || is.null(names(s)))
    .dl_stop_config_simple(archivo, "el archivo no es una lista de claves (clave: valor, una por l\u00ednea)")
  tablas <- .dl_tablas_proyecto(carpeta, dadas, .dl_opciones_lectores(s))
  .dl_config_de_tablas(s, archivo, causa, tablas, .dl_causas_config(carpeta, s, causa), base, cambios,
                       .dl_calibracion_proyecto(s, archivo, tablas, cambios, causa))
}

# La configuración de `causa` con las tablas ya leídas (lo que sigue a su lectura en .dl_preparar_contrato; la
# revisión lo hace en su propio paso): la severidad de la partición, si la configuración la declara (error si su
# carpeta no existe), el contexto y la traducción. `calibracion`: la de proxies_crudos (.dl_calibracion_proyecto, de
# las tablas tal como se leyeron; NULL sin ellos), que hace quien llama: .dl_preparar_contrato o el paso «proxies» de
# la revisión. El contexto y la traducción ven las tablas del modelo (`tablas_modelo`: covariables con las filas
# calibradas); `tablas` son las del proyecto tal como vinieron.
.dl_config_de_tablas <- function(s, archivo, causa, tablas, causas, base, cambios = NULL, calibracion = NULL) {
  particion <- .dl_valor_en(s, "severidad.particion")
  if (!is.null(particion)) {
    ruta <- normalizePath(file.path(base, particion), winslash = "/", mustWork = FALSE)
    if (!dir.exists(ruta))
      .dl_stop_config_simple(archivo, sprintf(paste0("severidad.particion: no existe la carpeta \u00ab%s\u00bb (%s, ",
                                                     "relativa a la carpeta del proyecto)"), particion, ruta))
    particion <- ruta
    tablas$severidad <- .dl_severidad_particion(s, causa, particion)
  }
  .dl_comprobar_subtipo_de(s, archivo, causa, causas)
  tm <- .dl_tablas_modelo(tablas, calibracion)
  ctx <- .dl_contexto_tablas(tm, causa, .dl_padre_extraction(s) %||% .dl_padre_de(causas, causa))
  list(cfg = .dl_config_simple(s, archivo, causa, ctx, cambios), tablas = tablas, tablas_modelo = tm,
       calibracion = calibracion, causas = causas, particion = particion)
}

# ---- El año del ancla ----

# Los años en que la tabla ancla trae la prevalencia de la causa `causa`, en orden (ninguno si el ancla no se leyó).
.dl_anios_ancla <- function(tablas, causa) {
  if (is.null(tablas$ancla)) return(integer())
  a <- .dl_filas_de_causa(tablas$ancla, causa)
  sort(unique(as.integer(a$anio[a$medida %in% "prevalencia"])))
}

# El año del ancla de un proyecto que estima `anio`: list(anio, proyectado). `declarado`: ancla.anio de la
# configuración (o NULL); `anios_ancla`: los años con prevalencia de la causa en la tabla ancla (.dl_anios_ancla).
#   - con ancla.anio, el menor de los dos: la misma configuración sirve para ese año y los anteriores. El menor solo
#     lo toma el argumento `anio` de dl_proyecto() (.dl_config_a_leer); el `anio` de la configuración no baja un
#     ancla.anio posterior (.dl_anio_ancla_leido), que es un error del validador;
#   - sin ella, `anio` si el ancla lo trae; si no, el último año anterior que trae (`proyectado`: se anuncia);
#   - si no trae `anio` ni ninguno anterior, `anio`: las reglas entre tablas dicen qué falta.
.dl_anio_ancla_proyecto <- function(anio, declarado, anios_ancla) {
  anio <- as.integer(anio)
  if (!is.null(declarado)) return(list(anio = min(anio, as.integer(declarado)), proyectado = FALSE))
  antes <- anios_ancla[anios_ancla < anio]
  if (anio %in% anios_ancla || !length(antes)) return(list(anio = anio, proyectado = FALSE))
  list(anio = as.integer(max(antes)), proyectado = TRUE)
}

# El año del ancla de una configuración ya leída (su `anio` y su ancla.anio, `declarado`, son los que valen):
# list(anio, proyectado). ancla.anio va tal cual; sin ella, la regla de .dl_anio_ancla_proyecto.
.dl_anio_ancla_leido <- function(anio, declarado, anios_ancla) {
  if (!is.null(declarado)) return(list(anio = declarado, proyectado = FALSE))
  .dl_anio_ancla_proyecto(anio, NULL, anios_ancla)
}

# La procedencia de years.ancla cuando el año del ancla se proyecta sin que la configuración lo declare.
.dl_procedencia_proyeccion <- function(anio, desde)
  sprintf(paste0("proyecci\u00f3n: la tabla ancla no trae la prevalencia de la causa en %d; se proyecta desde %d, ",
                 "el \u00faltimo a\u00f1o anterior que trae"), anio, desde)

# Lo que se anuncia de una configuración `cfg` cuyo año del ancla se proyectó sin declararlo (su years.ancla lleva
# la procedencia de .dl_procedencia_proyeccion): el texto del mensaje de dl_proyecto() y del aviso de la revisión.
# NULL si el año del ancla es el que se estima o lo declara la configuración (ancla.anio, `avanzado` o `cambios`).
.dl_proyeccion_anunciada <- function(cfg) {
  anio <- .dl_anio_ajuste(cfg); desde <- .dl_anio_ancla(cfg)
  if (!identical(cfg$years$ancla$procedencia, .dl_procedencia_proyeccion(anio, desde))) return(NULL)
  sprintf(paste0("el ancla no trae %d: se proyecta desde %d, el \u00faltimo a\u00f1o anterior con la prevalencia ",
                 "de la causa %d (el nivel nacional es el de %d; para otro a\u00f1o, declara ancla: {anio: ...})"),
          anio, desde, cfg$cause_id, desde)
}

# ---- Los proxies crudos del proyecto ----

# La calibración de proxies_crudos (dl_calibrar_proxies) con la configuración `s` (de `archivo`) y los `cambios` del
# formato completo (los de dl_configuracion()): el año que se estima (years.ajuste de `cambios` o anio), el del valor
# nacional (years.ancla de `cambios` o, sin ella, el año del ancla del proyecto: .dl_anio_ancla_leido, con
# ancla.anio y los años del ancla de `causa`) y las claves proxies.metodo, proxies.transformacion y proxies.excluir.
# Los años salen de `s`, `cambios` y la tabla ancla, no de la configuración traducida, que necesita la calibración
# para su contexto. Los avisos de la calibración siguen su curso y quedan también en su atributo `avisos`, para la
# revisión de un proyecto ya leído. NULL si el proyecto no trae proxies_crudos.
.dl_calibracion_proyecto <- function(s, archivo, tablas, cambios = NULL, causa = s[["causa"]]) {
  if (is.null(tablas$proxies_crudos)) return(NULL)
  s <- .dl_claves_config_simple(s, archivo)
  .dl_exigir_proxies_proyecto(s, tablas)
  anio <- as.integer(cambios$years$ajuste %||% s$anio)
  ex <- .dl_valor_en(s, "proxies.excluir")
  avisos <- character()
  cal <- withCallingHandlers(
    dl_calibrar_proxies(tablas$proxies_crudos, tablas$covariables, tablas$poblacion, anio,
                        metodo = .dl_valor_en(s, "proxies.metodo") %||% "paseo_aleatorio",
                        transformacion = unlist(.dl_valor_en(s, "proxies.transformacion")),
                        excluir = if (length(ex)) data.table::rbindlist(ex, fill = TRUE),
                        anio_nacional = as.integer(cambios$years$ancla$valor %||% .dl_anio_ancla_leido(
                          anio, .dl_valor_en(s, "ancla.anio"), .dl_anios_ancla(tablas, causa))$anio)),
    warning = function(w) avisos <<- c(avisos, w$detalle %||% conditionMessage(w)))
  if (length(avisos)) data.table::setattr(cal, "avisos", avisos)
  cal
}

# Lo que la calibración del proyecto necesita antes de empezar, en palabras del proyecto: la tabla covariables (con
# el valor nacional), ninguna covariable con subnacionales en las dos tablas (.dl_regla_proxies_dos_tablas) y las
# covariables de proxies.transformacion en proxies_crudos.
.dl_exigir_proxies_proyecto <- function(s, tablas) {
  covs <- unique(tablas$proxies_crudos$covariable)
  if (is.null(tablas$covariables))
    .dl_stop(paste0("proxies_crudos: falta la tabla covariables, con el valor nacional de cada covariable de los ",
                    "crudos (%s)"), paste(covs, collapse = ", "))
  pr <- .dl_regla_proxies_dos_tablas(tablas)
  if (length(pr)) .dl_stop("%s", paste(pr, collapse = "\n"))
  fuera <- setdiff(names(.dl_valor_en(s, "proxies.transformacion")), covs)
  if (length(fuera))
    .dl_stop("%s", paste(sprintf(paste0("proxies.transformacion.%s: la covariable no est\u00e1 en proxies_crudos ",
                                        "(las de los crudos: %s)"), fuera, paste(covs, collapse = ", ")),
                         collapse = "; "))
}

# Las tablas que ve el modelo: las del proyecto con covariables más las filas calibradas de los proxies crudos
# (`calibracion`, de .dl_calibracion_proyecto; sin ella, las mismas tablas). La tabla unida conserva el origen, los
# lectores y la ubicacion_gbd de covariables.
.dl_tablas_modelo <- function(tablas, calibracion) {
  if (is.null(calibracion)) return(tablas)
  cv <- tablas$covariables
  t <- .dl_tabla_contrato(data.table::rbindlist(list(cv, calibracion), fill = TRUE), "covariables", attr(cv, "origen"))
  for (a in c("lectores", "ubicacion_gbd")) data.table::setattr(t, a, attr(cv, a))
  tablas$covariables <- t
  tablas
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
  # los de las particiones de severidad (los mismos con que se leyó la tabla severidad de una partición)
  for (k in c("health_states", "sequelas"))
    file.copy(.dl_inst_archivo("referencia", cat_arch[[k]]), file.path(destino, pieza$catalogos, cat_arch[[k]]))
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
  # todas las causas, con su nombre en español (el consolidado lo usa); `hijos`, solo en las que son suma de otras
  escribir(causas[, list(cause_id, nombre_es = nombre, hijos)], pieza$registry, "master_gbd.csv")
  escribir(rbind(.dl_etiquetas_ref(),
                 causas[, list(tabla = "cause", id = as.character(cause_id), name = nombre_ancla, name_es = nombre,
                               slug = .dl_slugify(nombre_ancla), slug_es = .dl_slugify(nombre))],
                 .dl_etiquetas_edades_gbd(),
                 b[, list(tabla = "age_group", id = as.character(age_group_id), name = nombre, name_es = nombre,
                          slug = "", slug_es = "")]), pieza$registry, "etiquetas_es.csv")
  writeLines("sequela_id,cause_id,sequela_name,health_state_id,rol",
             file.path(destino, pieza$registry, "sequela_rei.csv"))
}

# Etiquetas en español de los grupos de edad de GBD que etiquetas_es.csv del paquete no trae (80-84, ..., 95+, los de
# menos de 5 años...): un proyecto usa las bandas del ancla y de la población tal cual, y el consolidado necesita el
# nombre de cada una. El nombre sale del de GBD, con la forma de las etiquetas del paquete («40 a 44 años»):
# «80-84 years» -> «80 a 84 años», «95+ years» -> «95 años y más», «1-5 months» -> «1 a 5 meses», «<1 year» ->
# «menores de 1 año».
.dl_etiquetas_edades_gbd <- function() {
  cat <- .dl_grupos_edad_catalogo()
  ref <- .dl_etiquetas_ref()
  cat <- cat[!cat$id %in% ref$id[ref$tabla == "age_group"]]
  es <- cat$name
  propios <- c("Age-standardized" = "Estandarizada por edad", "Post Neonatal" = "Posneonatal")
  es[es %in% names(propios)] <- propios[es[es %in% names(propios)]]
  es <- sub("^<(.*)$", "menores de \\1", es)
  es <- sub("^([0-9]+)-([0-9]+) ", "\\1 a \\2 ", es)          # «80-84 years» como «40 a 44 años» del paquete
  es <- sub("^([0-9]+)[+] (years|year)$", "\\1 \\2 y m\u00e1s", es)
  for (k in list(c("years", "a\u00f1os"), c("year", "a\u00f1o"), c("months", "meses"), c("days", "d\u00edas")))
    es <- gsub(sprintf("\\b%s\\b", k[1L]), k[2L], es, perl = TRUE)
  data.table::data.table(tabla = "age_group", id = cat$id, name = cat$name, name_es = es, slug = "", slug_es = "")
}

# Grupos de edad del catálogo demográfico de GBD (id y nombre, como texto).
.dl_grupos_edad_catalogo <- function() {
  d <- .dl_leer_memo(.dl_inst_archivo("referencia", "catalogo_demograficos_gbd2023.csv"),
                     function(p) .dl_leer_csv(p, colClasses = "character"))
  d[d$tabla == "age_group", c("id", "name"), with = FALSE]
}

# Rutas del proyecto traducido para `cfg` (`tablas`: las del contrato; `causas`: .dl_causas_config), en la carpeta
# temporal <proyecto>_<causa>_<clave>. <clave> resume lo que se traduce: la versión del paquete, la configuración
# traducida, las causas y el contenido de las tablas, sin su origen (las dos puertas comparten la traducción). `donde`
# identifica el proyecto (.dl_proyecto_contrato): su carpeta, el archivo de su configuración o un resumen de la
# lista. Una traducción nueva borra las anteriores del mismo proyecto, causa y año que se estima, y sus lecturas
# memorizadas: las de otros años del mismo proyecto (dl_proyecto(anio = )) conviven. El nombre de la carpeta lleva el
# año después de la clave (<proyecto>_<causa>_<clave>_<año>), y las anteriores se buscan por los dos extremos.
.dl_traducir_proyecto <- function(donde, cfg, tablas, causas) {
  traducida <- cfg
  traducida$origen[c("archivo", "betas")] <- NULL   # la ruta de la configuración; las betas van en las tablas
  contenido <- lapply(tablas, function(t) digest::digest(lapply(t, identity)))   # sin sus atributos
  clave <- digest::digest(list(dl_version(), traducida, causas, contenido), algo = "xxhash64")
  if (dir.exists(donde)) donde <- normalizePath(donde, winslash = "/")
  prefijo <- sprintf("%s_%d_", digest::digest(donde, algo = "xxhash64"), cfg$cause_id)
  raiz <- file.path(tempdir(), "dismodlite_proyectos")
  sufijo <- sprintf("_%d", .dl_anio_ajuste(cfg))
  destino <- file.path(raiz, paste0(prefijo, clave, sufijo))
  if (!file.exists(file.path(destino, "listo"))) {
    for (v in list.files(raiz, paste0("^", prefijo, "[^_]*", sufijo, "$"), full.names = TRUE)) {
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
#' solas) y se valida sola; un problema de una tabla detiene `dl_proyecto()` con la tabla, la columna y las filas.
#' Después, `dl_proyecto()` comprueba las reglas que cruzan tablas (que haya una sola ubicación nacional y que las
#' ubicaciones de las demás tablas estén en `ubicaciones`, que la población y el ancla cubran el año, los sexos y las
#' edades del modelo, que cada beta tenga su valor nacional, que la severidad sume 1...): se detiene con todos sus
#' problemas juntos (en el campo `problemas` del error) y avisa de sus sospechas (una mortalidad de `datos` que parece
#' por 100 000, una ubicación subnacional sin proxies...). [dl_revisar_proyecto()] comprueba lo mismo sin detenerse en
#' el primer problema, y [dl_insumos()] lo que necesita las tablas ya traducidas (la población nacional suma las
#' subnacionales, los proxies cierran en el valor nacional, los datos tienen valores posibles).
#'
#' El proyecto se traduce al formato completo de la versión 0.2.2, el que usa el resto del paquete: la configuración y
#' las tablas a una carpeta temporal de la sesión, que se reutiliza mientras no cambien. Los números leídos pasan tal
#' cual; solo se calcula lo que sale de la población (el total nacional, si no viene) y la agrupación del ancla en las
#' bandas de la población (con `poblacion_detalle`, si el ancla es más fina).
#'
#' `print()` muestra la causa, el año, las tablas que encontró, el modo subnacional, la calibración de
#' `proxies_crudos` (por covariable: el método, q y las ediciones) y las claves de la configuración que tomaron su
#' valor por defecto.
#'
#' `anio` lee el proyecto para otro año que el de su configuración, sin editarla: es como escribir ese `anio` en el
#' archivo (los proxies de `proxies_crudos` se calibran para ese año). Las lecturas de años distintos del mismo
#' proyecto conviven en la sesión. El año del ancla, el de la estimación de referencia que se usa, sale de una sola
#' regla:
#' - sin `ancla.anio` en la configuración, es el año que se estima si la tabla `ancla` trae la prevalencia de la causa
#'   en ese año; si no la trae, el último año anterior que trae, y un mensaje lo anuncia («el ancla no trae 2024: se
#'   proyecta desde 2023»). La proyección queda en la procedencia del año del ancla (`years.ancla`) y en las
#'   limitaciones del manifiesto de la corrida, y [dl_revisar_proyecto()] la muestra como aviso;
#' - con `ancla.anio`, ese año, que no puede ser posterior al `anio` de la configuración. Con el argumento `anio`
#'   es el menor entre los dos: una configuración con `anio: 2024` y `ancla: {anio: 2023}` sirve para 2024
#'   (proyectado desde 2023) y, con `anio = 2023` o un año anterior, para ese año con su propia ancla;
#' - el ancla es, a lo sumo, de un año antes del que se estima: mantiene el nivel nacional, no lo extrapola. Si el
#'   ancla no trae el año ni ninguno anterior, las reglas entre tablas dicen qué falta.
#'
#' Sin carpeta, las rutas relativas de la configuración (`severidad.particion`) se resuelven contra el directorio de
#' trabajo; con carpeta, contra la carpeta del proyecto. Las secuelas y los estados de salud de una partición de
#' severidad deben estar en los catálogos de GBD 2023 del paquete: una partición con secuelas propias todavía no se
#' puede leer en un proyecto (sí en el formato completo, con sus catálogos).
#'
#' @section La carpeta del proyecto:
#' ```
#' mi_proyecto/
#'   config.yaml          # una causa; o config/<causa>.yaml, una por causa
#'   ubicaciones.csv
#'   poblacion.csv
#'   ancla/               # cualquier CSV: descargas de GBD tal cual o tablas del contrato
#'   covariables/         # descargas del GHDx y/o la tabla del contrato con los proxies ya calibrados
#'   betas.csv
#'   datos.csv
#'   severidad.csv
#'   fuentes_gbd/
#'   poblacion_detalle.csv
#'   proxies_crudos.csv   # un indicador de encuesta por ubicación y edición: se calibra al leer
#'   particion/<corrida>/ # opcional: la partición de severidad que nombra severidad.particion
#' ```
#' Cada tabla es `<tabla>.csv` o una carpeta `<tabla>/` cuyos CSV se juntan (cada uno pasa por su lector), como
#' `poblacion_detalle/` con un CSV por fuente. Las rutas no se declaran, salvo la de la partición de severidad
#' (`severidad.particion`, relativa a la carpeta). Los demás archivos de la carpeta (un `LEEME.md`, la carpeta
#' `resultados/` que escribe [dl_correr()]) no se leen. Una tabla opcional ausente, o con solo el encabezado, no existe (así quedan las plantillas de
#' [dl_nuevo_proyecto()]). Las obligatorias son `ubicaciones`, `poblacion` y `ancla`: con ellas se estima la causa en
#' el país y, si hay ubicaciones subnacionales, en cada una con las tasas nacionales. [dl_correr()], que calcula los
#' años vividos con discapacidad (AVD), necesita también `severidad` (o `severidad.particion` en la configuración).
#' `covariables` y `betas` agregan las diferencias entre ubicaciones subnacionales y `datos`, los datos locales. Las
#' columnas de cada tabla, sus unidades y sus valores están en [dl_tablas]. Con `proxies_crudos`, `dl_proyecto()`
#' calibra los valores subnacionales de sus covariables para el año que se estima ([dl_calibrar_proxies()], con las
#' claves `proxies.*` de la configuración) y los agrega a `covariables`; los de cada covariable vienen de una sola de
#' las dos tablas.
#'
#' Con varias causas, cada una tiene su configuración en `config/<causa>.yaml` y comparten las tablas, que traen las
#' filas de todas en la columna `causa` (sin ella, una fila vale para todas). Una causa que es la suma de otras las
#' declara en `subtipos`; un subtipo sin betas propias usa las de su causa padre. El subtipo encuentra a su padre por
#' esa clave, en otra configuración del mismo proyecto; leído solo (en su propia carpeta), lo declara con
#' `subtipo_de: <padre>` y, sin betas propias, usa las de esa causa (ver [dl_sumar_hijas()]); si las dos configuraciones
#' dicen su padre, deben decir la misma causa. La causa padre se lee con `dl_proyecto()` aunque solo se sume: necesita su prevalencia en el
#' ancla (y su mortalidad, con el prior por defecto de la mortalidad en exceso). Cada subtipo se corre por separado y
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
#'   `betas`, `datos`, `severidad`, `fuentes_gbd`, `poblacion_detalle`, `proxies_crudos`): un `data.frame` o la ruta
#'   de un CSV o de una carpeta. Reemplazan a las de la carpeta.
#' @param configuracion Ruta del YAML de la configuración o una lista con sus claves (opcional; por defecto, la de la
#'   carpeta).
#' @param anio Año que se estima (un entero), en lugar del `anio` de la configuración; `NULL` (por defecto) deja el
#'   de la configuración. Ver Detalles para el año del ancla.
#' @return Objeto de clase `dl_proyecto`: una lista con
#'   - `configuracion`: la configuración de la causa, de clase `dl_config`, como la de [dl_configuracion()]. Trae
#'     `origen$por_defecto`: las claves tomadas por defecto, con su valor.
#'   - `rutas`: las rutas de los insumos, de clase `dl_paths`, como las de [dl_rutas()]. Apuntan a la traducción, en
#'     la carpeta temporal de la sesión: en otra sesión, o después de cambiar la configuración o una tabla (la
#'     traducción anterior del mismo año se borra), vuelve a llamar a `dl_proyecto()`.
#'   - `carpeta`: la carpeta del proyecto (`NULL` sin carpeta).
#'   - `formato`: `"simple"` (un proyecto con las tablas del contrato) o `"completo"` (el formato de la 0.2.2).
#'   - `tablas`: las tablas del contrato, una lista nombrada de [dl_tabla()] (`NULL` en el formato completo), tal
#'     como vinieron (`covariables` sin las filas calibradas).
#'   - `calibracion`: solo si el proyecto trae `proxies_crudos`, el resultado de [dl_calibrar_proxies()] (las filas
#'     calibradas, con los atributos `calibracion`, `series` y `excluidas`).
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
#' # el mismo proyecto leído para otro año, sin editar la configuración
#' dl_proyecto(dl_ejemplo(), 9101, anio = 2019)
#'
#' # un proyecto mínimo sin carpeta: la configuración y las tablas obligatorias como argumentos
#' p <- dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30),
#'                  ubicaciones = dl_ejemplo("ubicaciones.csv"),
#'                  poblacion = dl_ejemplo("poblacion.csv"), ancla = dl_ejemplo("ancla"))
#' \donttest{
#' # el paso siguiente: los insumos
#' b <- dl_insumos(p)
#' }
#' @export
dl_proyecto <- function(carpeta = NULL, causa = NULL, ..., configuracion = NULL, anio = NULL) {
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
  if (!is.null(anio)) anio <- .dl_exigir_anio(anio)
  cf <- .dl_config_a_leer(carpeta, causa, configuracion, anio)
  if (cf$simple) return(.dl_proyecto_contrato(carpeta, cf, dadas))
  if (length(dadas) || is.list(configuracion))
    .dl_stop(paste0("un proyecto del formato completo de la 0.2.2 se lee de su carpeta y su archivo de ",
                    "configuraci\u00f3n: no recibe tablas en `...` ni la configuraci\u00f3n como lista"))
  .dl_proyecto_de(carpeta %||% .dl_raiz_proyecto(cf$archivo),
                  dl_configuracion(cf$causa, cf$archivo,
                                   cambios = if (!is.null(anio)) list(years = list(ajuste = anio))))
}

# `anio` (el argumento de dl_proyecto()): un año, un número entero. Devuelve el entero.
.dl_exigir_anio <- function(anio) {
  if (!.dl_es_entero1(anio) || anio < 1 || anio > .Machine$integer.max)
    .dl_stop("`anio` debe ser un a\u00f1o, un solo n\u00famero entero (por ejemplo 2023); es %s",
             .dl_describir_objeto(anio))
  as.integer(anio)
}

# La configuración que lee dl_proyecto(): list(s, archivo, causa, simple, origen: la lista dada o NULL). Sin `configuracion`, la de `causa` en la
# carpeta (.dl_configs_proyecto y .dl_elegir_configs); con ella, su archivo o la lista misma. La causa es la pedida o,
# sin ella, la que declara la configuración. Con `anio`, una configuración simple se lee con ese año en su clave
# `anio`, como si el archivo lo trajera (la del formato completo lo recibe después, como un cambio de years.ajuste),
# y un ancla.anio posterior baja a ese año (.dl_anio_ancla_proyecto): la configuración sirve para los años anteriores.
.dl_config_a_leer <- function(carpeta, causa, configuracion, anio = NULL) {
  con_anio <- function(cf) {
    if (is.null(anio) || !cf$simple || !is.list(cf$s)) return(cf)
    cf$s[["anio"]] <- anio
    declarado <- .dl_valor_en(cf$s, "ancla.anio")
    if (.dl_es_entero1(declarado)) cf$s$ancla$anio <- .dl_anio_ancla_proyecto(anio, declarado, integer())$anio
    cf
  }
  if (is.null(configuracion)) {
    cf <- .dl_elegir_configs(.dl_configs_proyecto(carpeta), causa, carpeta)
    return(con_anio(list(s = .dl_leer_config(cf$archivo), archivo = cf$archivo, causa = cf$causa,
                         simple = cf$simple)))
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
  con_anio(list(s = s, archivo = archivo, causa = causa, simple = !.dl_es_config_completa(s),
                origen = if (is.list(configuracion)) configuracion))
}

# El proyecto de las tablas del contrato: la configuración y las tablas (.dl_preparar_contrato) y las rutas de su
# traducción (.dl_traducir_proyecto), más la partición de severidad si la configuración la declara. La traducción se
# identifica por la carpeta o, sin ella, por el archivo de la configuración o el contenido de la lista. Un año del
# ancla proyectado sin declararlo (.dl_proyeccion_anunciada) se anuncia con un mensaje.
.dl_proyecto_contrato <- function(carpeta, cf, dadas) {
  pre <- .dl_preparar_contrato(cf$s, cf$archivo, cf$causa, carpeta, dadas)
  .dl_exigir_reglas_proyecto(pre)
  proyeccion <- .dl_proyeccion_anunciada(pre$cfg)
  if (!is.null(proyeccion)) .dl_message("%s", proyeccion)
  donde <- carpeta %||% if (is.list(cf$origen)) sprintf("configuracion_%s", digest::digest(cf$origen))
                        else normalizePath(cf$archivo, winslash = "/")
  p <- .dl_proyecto_armado(carpeta, donde, pre)
  p$configuracion_dada <- cf$origen     # la lista tal como se dio, para leer otro año; sin ella, NULL
  p
}

# El mismo proyecto `p` leído para el año `anio` (dl_proyecto(anio = )): de su carpeta o, si sus tablas vinieron como
# argumentos, de esas tablas con la configuración dada (la lista o su archivo). El que ya es de ese año se devuelve tal
# cual.
.dl_proyecto_de_anio <- function(p, anio) {
  if (identical(.dl_anio_ajuste(p$configuracion), as.integer(anio))) return(p)
  causa <- p$configuracion$cause_id
  archivo <- p$configuracion$origen$archivo
  configuracion <- p$configuracion_dada %||% if (.dl_es_texto1(archivo) && file.exists(archivo)) archivo
  if (!is.null(p$carpeta)) return(dl_proyecto(p$carpeta, causa, configuracion = configuracion, anio = anio))
  do.call(dl_proyecto, c(list(causa = causa, configuracion = configuracion, anio = anio),
                         Filter(Negate(is.null), p$tablas)))
}

# Las reglas entre tablas (.dl_problemas_proyecto) de `pre` (.dl_preparar_contrato), antes de traducir: sus avisos,
# uno por uno; sus problemas, todos juntos en un error (campo `problemas`), como los de una tabla sola.
# dl_revisar_proyecto() las corre en su propio paso, sin detenerse.
.dl_exigir_reglas_proyecto <- function(pre) {
  pr <- .dl_problemas_proyecto(pre$tablas_modelo, pre$cfg, max(1L, nrow(pre$causas)), originales = pre$tablas)
  for (a in pr$avisos) .dl_warn("%s", a)
  if (length(pr$problemas))
    .dl_stop(paste0("el proyecto tiene %d problema(s) entre tablas:\n%s\n  (dl_revisar_proyecto() ",
                    "revisa todo el proyecto sin detenerse)"), length(pr$problemas),
             paste0("  - ", pr$problemas, collapse = "\n"), campos = list(problemas = pr$problemas))
}

# El objeto dl_proyecto de lo que prepara .dl_preparar_contrato (`pre`), con las rutas de su traducción (`donde`:
# lo que identifica el proyecto, ver .dl_traducir_proyecto; se traducen las tablas del modelo) y la partición de
# severidad, si la hay. Lleva las tablas tal como vinieron y, con proxies_crudos, su calibración (`calibracion`).
.dl_proyecto_armado <- function(carpeta, donde, pre) {
  rutas <- .dl_traducir_proyecto(donde, pre$cfg, pre$tablas_modelo, pre$causas)
  if (!is.null(pre$particion)) rutas["severity_split"] <- list(pre$particion)
  p <- structure(list(configuracion = pre$cfg, rutas = rutas, carpeta = carpeta, formato = "simple",
                      tablas = pre$tablas), class = "dl_proyecto")
  p$calibracion <- pre$calibracion      # sin proxies_crudos, el objeto no lleva el campo
  p
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
    if (!is.null(x$calibracion)) {
      cat(sprintf("  proxies calibrados de proxies_crudos (a\u00f1o %d):\n", x$calibracion$anio[1L]))
      cat(sprintf("    %s\n", strsplit(.dl_linea_proxies(x$calibracion), "\n", fixed = TRUE)[[1L]]), sep = "")
    }
    pd <- cfg$origen$por_defecto
    if (length(pd)) {
      t <- .dl_claves_simple()
      cat(sprintf("  tomado por defecto (%d):\n", length(pd)))
      cat(sprintf("    %s = %s\n", .dl_con_simbolo(names(pd), t$simbolo[match(names(pd), t$clave)]), pd), sep = "")
    }
  }
  invisible(x)
}
