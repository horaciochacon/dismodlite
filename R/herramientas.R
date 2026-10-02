# Herramientas para la carpeta de un proyecto (R/proyecto.R y el contrato de insumos, R/contrato.R):
# dl_nuevo_proyecto() la crea (la configuración comentada, desde la tabla de claves, y las plantillas de las tablas
# del contrato); dl_revisar_proyecto() la revisa con los lectores y validadores de siempre, tabla por tabla, y con las
# reglas entre tablas (.dl_problemas_proyecto), sin detenerse en el primer problema; y dl_correr() hace una corrida en
# una llamada.

# ---- dl_nuevo_proyecto() ----

# Una línea de YAML «clave: valor» (el texto entre comillas si hace falta; un número entero sin decimales).
.dl_linea_yaml <- function(clave, valor) {
  if (is.numeric(valor) && isTRUE(all(valor == round(valor)))) valor <- as.integer(valor)
  sub("\n$", "", yaml::as.yaml(stats::setNames(list(valor), clave)))
}

# Líneas de la configuración comentada de un proyecto nuevo, desde la tabla de claves y en su orden: cada clave con
# su descripción, su símbolo y su valor por defecto en un comentario que empieza con la clave entre comillas
# invertidas (así no se confunde con una clave comentada), y después la línea de YAML. Sin comentar: las claves de
# `dadas` (lista clave -> valor) y las obligatorias (vacías si no se dan). Las demás van comentadas, dentro de su
# bloque (ancla: peso: 1) o como un registro de su lista (covariables: - nombre: ...), con su valor por defecto o, si
# no tiene un valor fijo (el defecto es una regla o no hay), con un ejemplo (el de una clave cuyo defecto es `anio`,
# anio - 1 si se da `anio`: vale para ancla.anio). Quitar el «# » de una línea deja el YAML con la sangría correcta.
.dl_plantilla_config <- function(dadas) {
  t <- .dl_claves_simple()
  if (.dl_es_entero1(dadas$anio)) t$ejemplo[t$defecto == "anio"] <- format(dadas$anio - 1L)
  superior <- sub("[.[].*$", "", t$clave)                     # ancla de ancla.peso, covariables de covariables[].beta
  campo <- sub("^.*[.]", "", t$clave)                         # peso de ancla.peso, beta de covariables[].beta
  fijo <- !vapply(t$valor, is.null, NA)
  defecto <- .dl_defecto_en_palabras(t)
  # strwrap no parte dentro de [...]: sus espacios van como \001 mientras se parte la línea
  partir <- function(x) {
    m <- gregexpr("\\[[^]]*\\]", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(v) gsub(" ", "\001", v, fixed = TRUE))
    gsub("\001", " ", strwrap(x, width = 110, prefix = "# ", exdent = 4), fixed = TRUE)
  }
  descripcion <- function(i) {
    cola <- if (t$defecto[i] == "obligatoria") sprintf("Obligatoria; por ejemplo %s.", t$ejemplo[i])
            else sprintf("Por defecto, %s.%s", defecto[i],
                         if (!fijo[i] && nzchar(t$ejemplo[i]) && is.null(dadas[[t$clave[i]]])) " Abajo, un ejemplo."
                         else "")
    partir(sprintf("%s: %s %s", .dl_con_simbolo(sprintf("`%s`", t$clave[i]), t$simbolo[i]), t$descripcion[i], cola))
  }
  linea <- function(i, sangria)
    sub(" +$", "", sprintf("# %s%s: %s", sangria, campo[i], if (fijo[i]) t$defecto[i] else t$ejemplo[i]))
  out <- c(
    "# Configuraci\u00f3n de una causa de un proyecto de dismodlite (la escribi\u00f3 dl_nuevo_proyecto()).",
    "# Cada clave va con qu\u00e9 es, su s\u00edmbolo en el modelo y su valor por defecto:",
    "# - las obligatorias van sin \u00ab#\u00bb: completa las que est\u00e1n vac\u00edas;",
    "# - las dem\u00e1s est\u00e1n comentadas con su valor por defecto o, si no tienen un valor fijo, con un",
    "#   ejemplo. Para usar otro valor, quita el \u00ab# \u00bb de su l\u00ednea (y el de su",
    "#   bloque, como `ancla:` para ancla.peso), deja la sangr\u00eda de dos espacios y escribe el valor.",
    "# Referencia de las claves: ?dl_configuracion. Para revisar el proyecto: dl_revisar_proyecto().")
  for (g in unique(superior)) {
    k <- which(superior == g)
    out <- c(out, "", unlist(lapply(k, descripcion)))
    hijas <- k[t$clave[k] != g]
    if (!length(hijas)) {                                       # una clave sola
      out <- c(out, if (!is.null(dadas[[g]])) .dl_linea_yaml(g, dadas[[g]])
                    else if (t$defecto[k] == "obligatoria") paste0(g, ":")
                    else linea(k, ""))
    } else {                                                    # un bloque o una lista de registros
      registro <- any(grepl("[]]", t$clave[hijas]))
      sangria <- if (registro) c("  - ", rep("    ", length(hijas) - 1L)) else rep("  ", length(hijas))
      out <- c(out, paste0("# ", g, ":"), vapply(seq_along(hijas), function(j) linea(hijas[j], sangria[j]), ""))
    }
  }
  out
}

# Las tablas con plantilla en un proyecto nuevo (solo el encabezado: una tabla opcional sin filas es como si no
# estuviera) y las carpetas para las descargas tal cual.
.DL_PLANTILLAS_NUEVO <- c("ubicaciones", "poblacion", "betas", "datos", "severidad", "poblacion_detalle")
.DL_CARPETAS_NUEVO <- c("ancla", "covariables", "fuentes_gbd")

# LEEME.md de un proyecto nuevo: la carpeta del proyecto (la de ?dl_proyecto), los pasos, con las direcciones de GBD
# Results y del GHDx del contrato estimates/v1 (inst/schema), y las llamadas para revisar y correr el proyecto.
.dl_plantilla_leeme <- function(carpeta, causa, archivo_config) {
  url <- .dl_schema_estimates()$sources
  ruta <- gsub("\\", "/", carpeta, fixed = TRUE)
  c(sprintf("# Proyecto de dismodlite: causa %d", causa), "",
    "Las tablas y sus columnas, unidades y valores: `?dl_tablas`. La carpeta del proyecto (`?dl_proyecto`):", "",
    "```",
    sprintf("%-22s # la configuraci\u00f3n (las claves: ?dl_configuracion)", archivo_config),
    "ubicaciones.csv        # la nacional (sin padre) y las subnacionales (con la nacional de padre)",
    "poblacion.csv          # por ubicaci\u00f3n, a\u00f1o, sexo y banda de edad: las bandas del modelo",
    "ancla/                 # descargas de GBD Results tal cual, o la tabla ancla",
    "covariables/           # descargas del GHDx (valor nacional) y la tabla con los proxies subnacionales",
    "betas.csv              # el efecto de cada covariable",
    "datos.csv              # datos locales",
    "severidad.csv          # estados de salud y sus proporciones (para los AVD)",
    "fuentes_gbd/           # la lista de fuentes del GHDx que GBD ya us\u00f3",
    "poblacion_detalle.csv  # poblaci\u00f3n nacional con m\u00e1s detalle de edad, si el ancla es m\u00e1s fina",
    "```", "",
    "Una tabla con solo el encabezado no se usa: las obligatorias son `ubicaciones`, `poblacion` y `ancla`.", "",
    sprintf("1. Completa `%s`.", archivo_config),
    "2. Llena `ubicaciones.csv` y `poblacion.csv`.",
    sprintf("3. Pon en `ancla/` las descargas de GBD Results (%s), sin editar.", url$gbd$url),
    sprintf(paste0("4. Si usas covariables, llena `betas.csv` y pon en `covariables/` sus descargas del GHDx (%s), ",
                   "sin editar, y la tabla de sus proxies subnacionales."), url$ghdx$url),
    "5. Llena las dem\u00e1s tablas que uses (`severidad.csv`, para los AVD; `datos.csv`; ...).",
    sprintf("6. Revisa el proyecto: `dl_revisar_proyecto(\"%s\")`.", ruta),
    sprintf(paste0("7. Pru\u00e9balo con `dl_correr(\"%s\", semilla = 1, rapido = TRUE)`; la corrida final, con ",
                   "`rapido = FALSE`."), ruta))
}

#' Crear la carpeta de un proyecto nuevo
#'
#' Crea la carpeta de un proyecto (ver [dl_proyecto()]) con lo que hay que llenar: la configuración comentada, las
#' plantillas de las tablas del contrato de insumos ([dl_tablas]), las carpetas de las descargas y un `LEEME.md` con
#' los pasos. No sobrescribe nada: un archivo que ya existe queda como está.
#'
#' @details
#' Lo que se crea en `carpeta`:
#' - `config.yaml` (o `config/<causa>.yaml`, si el proyecto ya tiene la carpeta `config/` de un proyecto con varias
#'   causas): cada clave de la configuración con qué es, su símbolo en el modelo y su valor por defecto (la
#'   tabla de [dl_configuracion()]). Las claves obligatorias (`causa`, `anio`, `edad_inicio`) van sin comentar, con el
#'   valor dado o vacías; `nombre`, si se da, también. Las demás van comentadas con su valor por defecto o, si no
#'   tienen un valor fijo, con un ejemplo. Para usar otro valor se quita el `#` de la línea (y el de su
#'   bloque, como `ancla:` para `ancla.peso`).
#' - `ubicaciones.csv`, `poblacion.csv`, `betas.csv`, `datos.csv`, `severidad.csv` y `poblacion_detalle.csv`, con
#'   solo el encabezado (las columnas de [dl_plantilla()]). Una tabla con solo el encabezado es como si no estuviera:
#'   las opcionales que no se llenan no se usan.
#' - `ancla/`, `covariables/` y `fuentes_gbd/`, vacías: ahí van las descargas de GBD Results y del GHDx, sin editar
#'   (o las tablas del contrato).
#' - `LEEME.md`: la carpeta del proyecto, los pasos, de dónde se descarga cada archivo y cómo revisar y correr el
#'   proyecto.
#'
#' Después: llenar las tablas, revisar el proyecto con [dl_revisar_proyecto()] y correrlo con [dl_correr()]. Los
#' valores de `nombre`, `anio` y `edad_inicio` van a la configuración tal como se dan: la revisión dice si alguno no
#' vale.
#'
#' Un proyecto de una sola causa tiene `config.yaml`. Para agregar otra causa, crea la carpeta `config/`, mueve ahí
#' la configuración como `config/<causa>.yaml` y vuelve a llamar a `dl_nuevo_proyecto()` con la causa nueva: escribe
#' `config/<causa nueva>.yaml` y deja lo demás como está. Las tablas son las mismas para todas las causas: sus filas
#' dicen de qué causa son en la columna `causa`.
#'
#' @param carpeta Carpeta del proyecto; se crea si no existe.
#' @param causa Identificador de la causa: un número entero (el `cause_id` de GBD si la causa existe en GBD).
#' @param nombre Nombre de la causa (opcional; sin él, la configuración toma el de la descarga del ancla).
#' @param anio Año que se estima (opcional aquí; la configuración lo exige).
#' @param edad_inicio Primera edad del modelo, en años (opcional aquí; la configuración la exige).
#' @return La ruta de `carpeta`, invisible. Un mensaje lista lo que se creó, lo que ya existía y los pasos
#'   siguientes.
#' @seealso [dl_tablas] (las tablas y sus columnas), [dl_proyecto()] (la carpeta), [dl_configuracion()] (las claves
#'   de la configuración), [dl_revisar_proyecto()] y [dl_correr()] (los pasos siguientes) y [dl_ejemplo()] (un
#'   proyecto lleno, para comparar).
#' @family proyecto
#' @examples
#' carpeta <- file.path(tempdir(), "proyecto_nuevo")
#' dl_nuevo_proyecto(carpeta, causa = 1234, nombre = "Enfermedad de ejemplo",
#'                   anio = 2023, edad_inicio = 30)
#' list.files(carpeta, recursive = TRUE, include.dirs = TRUE)
#' cat(readLines(file.path(carpeta, "config.yaml"), n = 20, encoding = "UTF-8"), sep = "\n")
#' # la revisión dice qué falta: las tablas obligatorias
#' dl_revisar_proyecto(carpeta)
#' unlink(carpeta, recursive = TRUE)
#' @export
dl_nuevo_proyecto <- function(carpeta, causa, nombre = NULL, anio = NULL, edad_inicio = NULL) {
  .dl_exigir_carpeta(carpeta, "la carpeta del proyecto nuevo")
  if (file.exists(carpeta) && !dir.exists(carpeta)) .dl_stop("`carpeta` es un archivo, no una carpeta: %s", carpeta)
  causa <- .dl_exigir_causa(causa)
  # la configuración: config.yaml o, en un proyecto con la carpeta config/, config/<causa>.yaml
  archivo_config <- if (dir.exists(file.path(carpeta, "config"))) sprintf("config/%d.yaml", causa) else "config.yaml"
  otra <- if (archivo_config == "config.yaml") stats::na.omit(.dl_configs_proyecto(carpeta)$causa)
  if (length(otra) && otra[1L] != causa)
    .dl_stop(paste0("la carpeta ya es el proyecto de la causa %d (config.yaml). Para agregar la causa %d, crea ",
                    "la carpeta config/, mueve ah\u00ed config.yaml como config/%d.yaml y vuelve a llamar a ",
                    "dl_nuevo_proyecto()"), otra[1L], causa, otra[1L])
  dadas <- Filter(Negate(is.null), list(causa = causa, nombre = nombre, anio = anio, edad_inicio = edad_inicio))
  plantillas <- lapply(stats::setNames(.DL_PLANTILLAS_NUEVO, paste0(.DL_PLANTILLAS_NUEVO, ".csv")),
                       function(t) paste(names(dl_plantilla(t)), collapse = ","))
  archivos <- c(stats::setNames(list(.dl_plantilla_config(dadas)), archivo_config),
                list(LEEME.md = .dl_plantilla_leeme(carpeta, causa, archivo_config)), plantillas)
  for (d in .DL_CARPETAS_NUEVO) dir.create(file.path(carpeta, d), recursive = TRUE, showWarnings = FALSE)
  nuevos <- !file.exists(file.path(carpeta, names(archivos)))
  for (a in names(archivos)[nuevos]) {
    dir.create(dirname(file.path(carpeta, a)), recursive = TRUE, showWarnings = FALSE)
    writeLines(enc2utf8(archivos[[a]]), file.path(carpeta, a), useBytes = TRUE)
  }
  .dl_message(paste0("proyecto de la causa %d en %s\n  archivos nuevos: %s\n  ya exist\u00edan (no se tocaron): %s\n",
                     "  siguientes pasos: lee LEEME.md, completa %s, llena las tablas, pon las descargas en ancla/ ",
                     "(y en covariables/) y revisa el proyecto con dl_revisar_proyecto()"),
              causa, carpeta, .dl_lista(names(archivos)[nuevos]), .dl_lista(names(archivos)[!nuevos]), archivo_config)
  invisible(carpeta)
}

# ---- dl_revisar_proyecto() ----

# Símbolos de la revisión: en orden, aviso, error, omitido y la flecha de la sugerencia (en ASCII si la sesión no es
# UTF-8).
.dl_simbolos_revision <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) c(ok = "\u2713", aviso = "!", error = "\u2717", omitido = "-", flecha = "\u2192")
  else c(ok = "v", aviso = "!", error = "x", omitido = "-", flecha = "->")
}

# Evalúa `expr` y devuelve list(valor, error, tabla, avisos): su valor (NULL si falló), los problemas de su error del
# paquete (el campo `problemas` de un error que es una lista de problemas, o su detalle), la tabla del error (`tabla`,
# si la trae) y el detalle de cada aviso y de cada mensaje de clase dl_mensaje_revision (las filas de datos que
# quedan fuera), sin mostrarlos; los demás mensajes se callan. Un error que no es del paquete sigue su curso: no es
# un problema del proyecto sino del paquete.
.dl_recoger <- function(expr) {
  avisos <- character(); e <- NULL
  valor <- withCallingHandlers(
    tryCatch(expr, dl_error = function(x) { e <<- x; NULL }),
    warning = function(w) { avisos <<- c(avisos, w$detalle %||% conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) {
      if (inherits(m, "dl_mensaje_revision")) avisos <<- c(avisos, m$detalle)
      invokeRestart("muffleMessage")
    })
  list(valor = valor, error = if (!is.null(e)) e$problemas %||% .dl_detalle(e), tabla = e$tabla, avisos = avisos)
}

# Una fila de la revisión. La sugerencia de un problema, por lo que nombra: ?dl_configuracion si es de la
# configuración o nombra una de sus claves (ancla.peso, datos_en_ajuste...); si es de una tabla (su paso, o en el paso
# «proyecto» la tabla con que empieza: «poblacion: ...»), sus columnas; si no, ?dl_proyecto. Ninguna si está en orden
# o se omitió.
.dl_fila_revision <- function(causa, paso, estado, detalle) {
  claves <- setdiff(grep("^[a-z_]+[._][a-z_.]+$", .dl_claves_simple()$clave, value = TRUE), .dl_tablas_ref()$columna)
  clave <- grepl(sprintf("(?<![\\w.])(%s)(?!\\w)", paste(gsub(".", "[.]", claves, fixed = TRUE), collapse = "|")),
                 detalle, perl = TRUE)
  tabla <- if (paso == "proyecto") sub("^([a-z_]+): .*$", "\\1", detalle) else paso
  sugerencia <- if (estado %in% c("ok", "omitido")) ""
                else if (paso == "configuraci\u00f3n" || clave) "ver ?dl_configuracion"
                else if (tabla %in% .DL_TABLAS)
                  sprintf("columnas %s (ver ?dl_tablas)", paste(names(dl_plantilla(tabla)), collapse = ", "))
                else "ver ?dl_proyecto"
  data.frame(causa = causa, paso = paso, estado = estado, detalle = detalle, sugerencia = sugerencia,
             stringsAsFactors = FALSE)
}

# Revisión de una causa del proyecto `carpeta` (`cf`: su fila de .dl_configs_proyecto()): una fila por comprobación
# (causa, paso, estado, detalle, sugerencia). Un proyecto con las tablas del contrato se revisa por pasos
# (.dl_revisar_contrato); uno del formato completo, su configuración (dl_configuracion()). Al final, si nada falló, los
# insumos (dl_insumos()) y la severidad que dl_correr() necesita. Un problema que lleva su tabla (`tabla` del error,
# también la de una tabla interna de los insumos) va al paso de esa tabla, sin la tabla delante.
.dl_revisar_causa <- function(carpeta, cf) {
  rel <- if (basename(dirname(cf$archivo)) == "config") file.path("config", basename(cf$archivo))
         else basename(cf$archivo)
  .dl_revisar_pasos(cf$causa, cf$simple, function(revisar, anotar, errores) {
    if (cf$simple) return(.dl_revisar_contrato(carpeta, cf, rel, revisar, anotar, errores))
    cfg <- revisar("configuraci\u00f3n", dl_configuracion(cf$causa, cf$archivo),
                   function(cfg) sprintf("%s: formato completo", rel))
    if (!is.null(cfg)) function() .dl_proyecto_de(carpeta, cfg)
  })
}

# Las filas de la revisión de la causa `causa`: los pasos de `pasos(revisar, anotar, errores)`, que devuelve la
# función que arma el proyecto (NULL si algo falló), y al final los insumos y la severidad. `revisar(paso, expr, ok)`
# evalúa `expr` en el paso `paso` y anota sus avisos, sus problemas y, si no falló, la línea en orden `ok(valor)`
# (NULL: ninguna); devuelve el valor (NULL si falló). En un proyecto con las tablas del contrato (`simple`), un
# problema que lleva su tabla va al paso de esa tabla, sin la tabla delante.
.dl_revisar_pasos <- function(causa, simple, pasos) {
  filas <- list()
  anotar <- function(paso, estado, detalle)
    filas[[length(filas) + 1L]] <<- .dl_fila_revision(causa, paso, estado, detalle)
  interna <- attr(.dl_claves_simple(), "tablas")          # tabla interna de los insumos -> tabla del contrato
  revisar <- function(paso, expr, ok) {
    r <- .dl_recoger(expr)
    donde <- if (simple && isTRUE(r$tabla %in% names(interna))) interna[[r$tabla]]
             else if (simple && isTRUE(r$tabla %in% .DL_TABLAS)) r$tabla else paso
    for (a in r$avisos) anotar(paso, "aviso", a)
    for (e in r$error) anotar(donde, "error", sub(paste0("^", donde, ": "), "", e))
    if (length(r$error) && donde != paso && paso == "configuraci\u00f3n")
      anotar(paso, "omitido", sprintf("su traducci\u00f3n espera a la tabla %s", donde))
    if (!length(r$error) && !is.null(r$valor) && !is.null(ok)) anotar(paso, "ok", ok(r$valor))
    r$valor
  }
  errores <- function() any(vapply(filas, function(f) f$estado == "error", NA))
  p <- pasos(revisar, anotar, errores)
  if (errores() || is.null(p)) {
    anotar("insumos", "omitido", "no se armaron: primero corrige los errores de arriba")
  } else {
    b <- revisar("insumos", dl_insumos(p()),
                 function(b) sprintf("dl_insumos() los arma y los valida (hash %s)", substr(b$hash, 1L, 12L)))
    if (!is.null(b)) revisar("insumos", .dl_en_simple(.dl_exigir_severidad(b$severidad, causa), simple), NULL)
  }
  do.call(rbind, filas)
}

# La línea en orden de una tabla leída `t`: sus filas, de dónde (con `origen`) y, si pasó por un lector, cuál.
.dl_linea_tabla <- function(t, origen = FALSE) {
  o <- attr(t, "origen")
  sprintf("le\u00edda: %d fila(s)%s%s", nrow(t),
          if (origen && length(o)) sprintf(", de %s", if (file.exists(o)) basename(o) else o) else "",
          if (length(attr(t, "lectores"))) sprintf(" (%s)", paste(attr(t, "lectores"), collapse = ", ")) else "")
}

# Las reglas entre tablas de `pre` (list(tablas, cfg, causas)) en el paso «proyecto».
.dl_revisar_reglas <- function(pre, anotar) {
  pr <- .dl_problemas_proyecto(pre$tablas, pre$cfg, max(1L, nrow(pre$causas)))
  for (a in pr$avisos) anotar("proyecto", "aviso", a)
  for (e in pr$problemas) anotar("proyecto", "error", e)
  if (!length(pr$problemas)) anotar("proyecto", "ok", "las reglas entre tablas se cumplen")
}

# Los pasos de un proyecto ya armado `p` (dl_proyecto(): sus tablas ya se leyeron y su configuración se tradujo): cada
# tabla en orden, con sus avisos, la configuración, las reglas entre tablas y, para los insumos, el mismo proyecto. Del
# formato completo, solo la configuración.
.dl_revisar_objeto <- function(p, revisar, anotar, errores) {
  cfg <- p$configuracion
  if (!identical(p$formato, "simple")) {
    anotar("configuraci\u00f3n", "ok", "formato completo")
    return(function() p)
  }
  for (t in names(p$tablas)) {
    for (a in attr(p$tablas[[t]], "avisos")) anotar(t, "aviso", a)
    anotar(t, "ok", .dl_linea_tabla(p$tablas[[t]], origen = TRUE))
  }
  archivo <- cfg$origen$archivo
  anotar("configuraci\u00f3n", "ok", sprintf("%s: proyecto", if (identical(archivo, "configuracion"))
    "la configuraci\u00f3n dada como lista" else basename(archivo)))
  causas <- if (!is.null(p$carpeta)) .dl_causas_config(p$carpeta, NULL, cfg$cause_id)
  .dl_revisar_reglas(list(tablas = p$tablas, cfg = cfg, causas = causas), anotar)
  if (!errores()) function() p
}

# Los pasos de un proyecto con las tablas del contrato (`revisar`, `anotar` y `errores`: los de .dl_revisar_causa):
#   1. cada tabla, leída por su lector y validada sola (las obligatorias que faltan, un error en su paso);
#   2. la configuración: sus claves y, si las tablas de las que toma algo (ubicaciones, betas, covariables) se leyeron,
#      su traducción, con la severidad de la partición si la declara;
#   3. «proyecto»: las reglas entre tablas (.dl_problemas_proyecto), si nada falló antes.
# Devuelve la función que arma el proyecto (como dl_proyecto(), con su traducción: dentro del paso «insumos», donde un
# error de la traducción queda en la revisión) o NULL si algo falló.
.dl_revisar_contrato <- function(carpeta, cf, rel, revisar, anotar, errores) {
  s <- .dl_leer_config(cf$archivo)
  fallidas <- character()
  tablas <- .dl_tablas_proyecto(carpeta, list(), .dl_opciones_lectores(s), paso = function(t, expr) {
    v <- revisar(t, expr, .dl_linea_tabla)
    if (is.null(v)) fallidas <<- c(fallidas, t)
    for (a in attr(v, "avisos")) anotar(t, "aviso", a)
    v
  })
  omitir <- function() {
    anotar("proyecto", "omitido", "las reglas entre tablas esperan a que se corrijan los errores de arriba")
    NULL
  }
  s <- revisar("configuraci\u00f3n", .dl_claves_config_simple(s, cf$archivo), NULL)
  if (is.null(s)) return(omitir())
  espera <- intersect(c("ubicaciones", "betas", "covariables"), fallidas)
  if (length(espera)) {
    anotar("configuraci\u00f3n", "omitido", sprintf("su traducci\u00f3n espera a la tabla %s", espera[1L]))
    return(omitir())
  }
  pre <- revisar("configuraci\u00f3n",
                 .dl_config_de_tablas(s, cf$archivo, cf$causa, tablas, .dl_causas_config(carpeta, s, cf$causa),
                                      carpeta),
                 function(pre) sprintf("%s: proyecto", rel))
  if (is.null(pre) || errores()) return(omitir())
  .dl_revisar_reglas(pre, anotar)
  function() .dl_proyecto_armado(carpeta, carpeta, pre)
}

# Imprime la revisión `r` de las causas del proyecto `nombre` («carpeta», o lo que dice que no la tiene): una línea
# por comprobación y la sugerencia de las que no están en orden; al final, el total.
.dl_imprimir_revision <- function(r, nombre) {
  s <- .dl_simbolos_revision()
  for (causa in unique(r$causa)) {
    x <- r[r$causa %in% causa, ]
    cat(sprintf("Revisi\u00f3n del proyecto %s%s\n", nombre,
                if (is.na(causa)) "" else sprintf(", causa %d", causa)))
    for (i in seq_len(nrow(x))) {
      cat(sprintf("  %s %s: %s\n", s[[x$estado[i]]], x$paso[i], gsub("\n", "\n      ", x$detalle[i], fixed = TRUE)))
      if (x$estado[i] != "ok" && nzchar(x$sugerencia[i])) cat(sprintf("      %s %s\n", s[["flecha"]], x$sugerencia[i]))
    }
  }
  n <- table(factor(r$estado, c("error", "aviso")))
  cat(if (n[["error"]] == 0L && n[["aviso"]] == 0L) "Todo en orden.\n"
      else sprintf("%d error(es) y %d aviso(s)%s.\n", n[["error"]], n[["aviso"]],
                   if (n[["error"]]) ": corrige los errores y vuelve a revisar" else ""))
  invisible(r)
}

#' Revisar la carpeta de un proyecto
#'
#' Revisa la carpeta de un proyecto antes de correrlo, sin detenerse en el primer problema: cada tabla del contrato
#' de insumos ([dl_tablas]), la configuración de cada causa, las reglas que cruzan tablas y, al final, los insumos
#' completos. Muestra una línea por comprobación, marcada como en orden, aviso (`!`) o error, con la corrección
#' sugerida. No ajusta nada.
#'
#' @details
#' La revisión usa los mismos lectores y validadores que [dl_proyecto()] y [dl_insumos()]: lo que pasa la revisión es
#' lo que ellos aceptan. Sus pasos, en orden:
#' 1. cada tabla (`ubicaciones`, `poblacion`, `ancla`, ...), leída por su lector y validada sola, como en [dl_tabla()]:
#'    una columna que falta, un CSV guardado desde Excel con «;» o coma decimal, un sexo, una medida o un número que no
#'    se reconoce, bandas de edad que se solapan... La línea en orden dice cuántas filas trae
#'    (`leída: N fila(s)`) y, si pasó por un lector, cuál (`descarga de GBD Results`). Una tabla obligatoria que falta
#'    (o que solo trae el encabezado) es un error;
#' 2. `configuración`: sus claves y valores, como los lee [dl_configuracion()]. Su traducción toma cosas de las
#'    tablas (la ubicación nacional, las betas): si `ubicaciones`, `betas` o `covariables` tienen un error, espera
#'    (`-`);
#' 3. `proyecto`: las reglas que cruzan tablas, cada problema con su tabla delante: que toda ubicación esté en
#'    `ubicaciones` (una sola sin `padre`, la nacional; las subnacionales con la nacional de padre); que la población
#'    traiga el año que se estima y los sexos del modelo, con las mismas bandas de edad en todas las ubicaciones, años
#'    y sexos, seguidas (sin huecos) y desde `edad_inicio`; que el ancla traiga la prevalencia de la causa (y la mortalidad, si la usa el prior
#'    de la mortalidad en exceso) en el año del ancla y en cada sexo, y la columna `causa` si el proyecto tiene varias;
#'    que cada banda del ancla sea una unión de bandas de la población o se pueda agrupar con `poblacion_detalle`; que
#'    cada covariable de `betas` (y cada `valor_nacional_de`) tenga su valor nacional en el año del ancla y que
#'    `escala` vaya solo con la transformación lineal; que cada ubicación subnacional con proxies los traiga de todas
#'    las covariables; que las proporciones de `severidad` sumen 1. Avisa (`!`) de una covariable con proxies y sin
#'    beta (no se usa), de una ubicación subnacional sin proxies (queda fuera de la estimación subnacional) y de
#'    valores de mortalidad de `datos` que parecen tasas por 100 000 en vez de por persona-año (mayores que 1, o más de
#'    1000 veces la mortalidad del ancla en la misma causa, año, sexo y banda);
#' 4. `insumos`: si nada falló, los insumos completos ([dl_insumos()]), con las reglas que necesitan todo armado: que
#'    la población nacional sea la suma de las subnacionales, que el promedio de los proxies, ponderado por la
#'    población, sea el valor nacional de la covariable o que los datos locales tengan valores posibles; y la
#'    severidad: una causa sin estados de salud es un error, porque sin ellos no hay AVD y [dl_correr()] no puede
#'    escribir la corrida. Un problema de los insumos que es de una tabla va al paso de esa tabla. Avisa también de lo
#'    que no detiene los insumos pero conviene mirar (datos locales en el ajuste con `ancla.peso` 1, un posible doble
#'    conteo, las filas de `datos` que quedan fuera).
#'
#' Cada problema lleva una sugerencia: [dl_configuracion()] si nombra una clave de la configuración, las columnas de
#' su tabla ([dl_tablas]) o [dl_proyecto()]. Un paso que espera a que se corrija otro (`-`) no cuenta como error ni
#' como aviso.
#'
#' En un proyecto en el formato completo de la 0.2.2 se revisan la configuración, los insumos completos y la
#' severidad.
#'
#' En vez de una carpeta se puede dar un proyecto ya leído con [dl_proyecto()], por ejemplo uno armado con tablas de
#' R (`dl_proyecto(configuracion = ..., ubicaciones = ..., poblacion = ...)`). Sus tablas ya se leyeron, su
#' configuración ya se tradujo y las reglas entre tablas se cumplen (si no, `dl_proyecto()` se habría detenido con
#' todos sus problemas): la revisión dice de dónde salió cada tabla y sus avisos, los avisos de las reglas entre tablas
#' y sigue con los insumos, para su única causa.
#'
#' @param carpeta Carpeta del proyecto, o un proyecto de [dl_proyecto()].
#' @param causa Causa que se revisa (`cause_id`); `NULL` (por defecto) revisa todas las causas con configuración. Con
#'   un proyecto de [dl_proyecto()], que ya es de una causa, `NULL` o esa misma causa.
#' @return Una tabla (data.frame), invisible, con una fila por comprobación: `causa`, `paso` (la tabla, la
#'   configuración, `proyecto` o `insumos`), `estado` (`"ok"`, `"aviso"`, `"error"` u `"omitido"`: un paso que espera a
#'   que se corrija otro), `detalle` y `sugerencia` (la corrección; vacía si está en orden o se omitió).
#' @seealso [dl_tablas] (las tablas y sus columnas), [dl_proyecto()] (la carpeta), [dl_configuracion()] (las claves),
#'   [dl_nuevo_proyecto()] y [dl_correr()].
#' @family proyecto
#' @examples
#' r <- dl_revisar_proyecto(dl_ejemplo(), causa = 9100)
#' table(r$estado)
#'
#' # una copia con un problema de una tabla (la población sin la columna `sexo`)
#' carpeta <- dl_ejemplo(copiar_en = file.path(tempdir(), "proyecto_con_errores"))
#' pob <- read.csv(file.path(carpeta, "poblacion.csv"), colClasses = c(ubicacion = "character"))
#' write.csv(pob[names(pob) != "sexo"], file.path(carpeta, "poblacion.csv"), row.names = FALSE)
#' r <- dl_revisar_proyecto(carpeta, causa = 9100)
#' r[r$estado == "error", c("paso", "detalle", "sugerencia")]
#'
#' # y uno entre tablas (un dato de una ubicación que no está en ubicaciones.csv)
#' write.csv(pob, file.path(carpeta, "poblacion.csv"), row.names = FALSE)
#' datos <- read.csv(file.path(carpeta, "datos.csv"), colClasses = c(ubicacion = "character"))
#' datos$ubicacion[1] <- "99"
#' write.csv(datos, file.path(carpeta, "datos.csv"), row.names = FALSE, na = "")
#' r <- dl_revisar_proyecto(carpeta, causa = 9100)
#' r[r$estado == "error", c("paso", "detalle", "sugerencia")]
#' unlink(carpeta, recursive = TRUE)
#'
#' # un proyecto con las tablas en R (la puerta de los data.frame): con un problema entre tablas,
#' # dl_proyecto() se detiene con todos juntos (read.csv() sin colClasses lee el código «01» como 1)
#' pob <- read.csv(dl_ejemplo("poblacion.csv"))
#' e <- tryCatch(dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30),
#'                           ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = pob,
#'                           ancla = dl_ejemplo("ancla")),
#'               error = function(e) e)
#' e$problemas
#' # con los códigos como texto, el proyecto se arma y se revisa
#' pob <- read.csv(dl_ejemplo("poblacion.csv"), colClasses = c(ubicacion = "character"))
#' p <- dl_proyecto(configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30),
#'                  ubicaciones = dl_ejemplo("ubicaciones.csv"), poblacion = pob,
#'                  ancla = dl_ejemplo("ancla"))
#' r <- dl_revisar_proyecto(p)
#' @export
dl_revisar_proyecto <- function(carpeta, causa = NULL) {
  if (!is.null(causa)) causa <- .dl_exigir_causa(causa)
  if (inherits(carpeta, "dl_proyecto")) {
    p <- carpeta
    k <- p$configuracion$cause_id
    if (!is.null(causa) && causa != k) .dl_stop("`causa` es %d y el proyecto es de la causa %d", causa, k)
    r <- .dl_revisar_pasos(k, identical(p$formato, "simple"),
                           function(revisar, anotar, errores) .dl_revisar_objeto(p, revisar, anotar, errores))
    rownames(r) <- NULL
    return(.dl_imprimir_revision(r, if (is.null(p$carpeta)) "(sin carpeta: las tablas vienen como argumentos)"
                                    else sprintf("\u00ab%s\u00bb", basename(p$carpeta))))
  }
  .dl_exigir_carpeta_existente(carpeta)
  cf <- .dl_recoger(.dl_elegir_configs(.dl_configs_proyecto(carpeta), causa, carpeta, varias = TRUE))
  r <- if (length(cf$error)) .dl_fila_revision(causa %||% NA_integer_, "configuraci\u00f3n", "error", cf$error)
       else do.call(rbind, lapply(seq_len(nrow(cf$valor)), function(j) .dl_revisar_causa(carpeta, cf$valor[j])))
  rownames(r) <- NULL
  .dl_imprimir_revision(r, sprintf("\u00ab%s\u00bb", basename(carpeta)))
}

# ---- dl_correr() ----

# Opciones del muestreo de una corrida de prueba (dl_correr(rapido = TRUE)): cadenas cortas, que no llegan a
# converger; la corrida se escribe con forzar = TRUE.
.DL_OPCIONES_PRUEBA <- list(simulaciones = 100L, cadenas = 2L, iteraciones = 2000L, calentamiento = 1000L)

#' Correr un proyecto de principio a fin
#'
#' El recorrido estándar de una corrida en una sola llamada: lee el proyecto, arma los insumos, ajusta, lleva el
#' ajuste a las ubicaciones subnacionales si corresponde, valida contra el ancla, calcula los AVD, las etiquetas y
#' la sensibilidad, resume y escribe la carpeta de la corrida. Cada paso sigue disponible por separado, con la
#' función que se nombra en Detalles.
#'
#' @details
#' Los pasos, en orden:
#' 1. los insumos ([dl_proyecto()] y [dl_insumos()]);
#' 2. el ajuste nacional ([dl_ajustar()]) y el ajuste solo con el ancla ([dl_ajustar_solo_prior()]);
#' 3. la cascada subnacional ([dl_cascada()]), si hay proxies del año que se estima o `subnacional.modo: plano`
#'    (`cascada.modo: plana` en el formato completo); si no, un mensaje lo dice y la corrida es solo nacional;
#' 4. la validación contra el ancla ([dl_validar_ancla()]);
#' 5. el factor de comorbilidad ([dl_factor_comorbilidad()], si el ancla trae AVD) y los AVD ([dl_avd()]);
#' 6. las etiquetas de cuánto informan los datos ([dl_etiquetas()]), con la grilla de `sensibilidad.correlacion_edad`
#'    de la configuración;
#' 7. la sensibilidad ([dl_sensibilidad()]) con la grilla `sensibilidad` de la configuración, si
#'    `sensibilidad = TRUE`;
#' 8. el resumen ([dl_resumir()]) y la carpeta de la corrida ([dl_exportar_corrida()]), llamada `causa-<causa>`;
#'    con `registro`, la corrida queda además en el registro de corridas.
#'
#' Por defecto las cadenas son las de producción de [dl_opciones_mcmc()]: 1000 simulaciones, 4 cadenas de 50 000
#' iteraciones con 10 000 de calentamiento, motor `"mh"`; la sensibilidad usa las de [dl_sensibilidad()].
#'
#' La corrida no se escribe en dos casos, que `dl_correr()` comprueba en cuanto puede, con las mismas compuertas que
#' [dl_exportar_corrida()]. Si las cadenas no convergieron (R-hat < 1.01 y ESS >= 400), justo después del ajuste:
#' aumenta `iteraciones` y `calentamiento` con `opciones`. Y si la prevalencia ajustada se aleja de la del ancla
#' (error relativo mediano mayor que 0.05), justo después de la validación, también con `rapido = TRUE`: revisa el
#' ajuste y los datos o, con su procedencia, declara un máximo mayor en la configuración, en `avanzado: {anchor:
#' {gate_err_mediano: {valor, procedencia}}}` (en el formato completo, sin `avanzado`; ver [dl_configuracion()]).
#'
#' `rapido = TRUE` es una prueba, para ver que el proyecto corre de principio a fin: cadenas cortas (100
#' simulaciones, 2 cadenas de 2000 iteraciones), las mismas en la sensibilidad, etiquetas solo con la
#' `ancla.correlacion_edad` de la configuración y la corrida escrita con `forzar = TRUE`, sin exigir la convergencia
#' (el manifiesto lo declara). Su nombre termina en `-prueba` y sus números no sirven para publicar.
#'
#' La causa debe tener severidad (sus estados de salud en `severidad.csv`): sin ella los AVD serían cero y la corrida
#' no se escribe. `dl_correr()` lo comprueba antes de ajustar. Los ajustes quedan en la caché de la sesión:
#' [dl_ajustar()] con los mismos insumos, opciones y semilla devuelve el de la corrida sin volver a muestrear. Con un
#' proyecto con las tablas del contrato, los mensajes de todos los pasos citan sus claves y sus tablas (como [dl_insumos()]).
#'
#' @inheritParams dl_ajustar
#' @param proyecto Carpeta del proyecto (con las tablas del contrato de insumos o en el formato completo) o un
#'   proyecto de [dl_proyecto()].
#' @param causa Causa (`cause_id`); `NULL` si el proyecto tiene una sola (o si `proyecto` ya es un `dl_proyecto`).
#' @param carpeta_salida Carpeta raíz de las corridas: la corrida se escribe en
#'   `<carpeta_salida>/mod/dismod_lite/<AAAA-MM-DD>_causa-<causa>_v<n>/`. `NULL` (por defecto) es la carpeta
#'   `resultados` del proyecto.
#' @param rapido `TRUE` para una corrida de prueba (ver Detalles); `FALSE` (por defecto), la de producción.
#' @param sensibilidad `TRUE` (por defecto) corre el análisis de sensibilidad y lo guarda en la corrida
#'   (`diagnostics/sensibilidad.csv`); `FALSE` lo omite.
#' @param opciones Opciones de [dl_opciones_mcmc()] del ajuste nacional; `NULL` (por defecto) usa las de producción
#'   o, con `rapido = TRUE`, las cortas. Con `rapido = TRUE`, las `opciones` que se den reemplazan a las cortas y la
#'   corrida sigue siendo una prueba.
#' @param registro Archivo YAML del registro de corridas, que ya existe (uno nuevo es un archivo con la línea
#'   `datasets: []`): la corrida se agrega al final. Se comprueba antes de ajustar. `NULL` (por defecto) no registra.
#' @return Objeto de clase `dl_run`, como el de [dl_exportar_corrida()], una lista con `run_id` (el identificador de
#'   la corrida, `<AAAA-MM-DD>_causa-<causa>_v<n>`, o `..._causa-<causa>-prueba_v<n>` con `rapido = TRUE`), `dir`
#'   (su carpeta), `manifest` (el contenido de su `manifest.yaml`, como lista) y `files` (las tablas de celdas
#'   escritas, una por medida, con su ruta, su sha256 y su número de filas). Los mensajes dicen qué paso corre y, al
#'   final, dónde quedó la corrida.
#' @seealso [dl_proyecto()], [dl_revisar_proyecto()] (antes de correr), [dl_opciones_mcmc()] (las cadenas),
#'   [dl_estimaciones()] y [dl_exportar_corrida()] (el contenido de la carpeta de la corrida); para los subtipos de una
#'   causa, [dl_sumar_hijas()].
#' @family proyecto
#' @examples
#' \donttest{
#' # una prueba con el ejemplo; la corrida se escribe en una carpeta temporal
#' salida <- file.path(tempdir(), "resultados")
#' run <- dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
#'                  carpeta_salida = salida)
#' run
#' list.files(run$dir, recursive = TRUE)
#' # la prevalencia por banda de edad, nacional y subnacional
#' prev <- read.csv(file.path(run$dir, "cause", "prevalence", paste0(run$run_id, ".csv")),
#'                  fileEncoding = "UTF-8")
#' head(prev[, c("location_name", "sex_name", "age_group_name", "val", "lower", "upper")])
#' # lo que registra el manifiesto: las claves de la configuración tomadas por defecto
#' run$manifest$configuracion$por_defecto
#' unlink(salida, recursive = TRUE)
#'
#' # la corrida de producción de un proyecto propio: las cadenas por defecto (1000 simulaciones,
#' # 4 cadenas de 50 000 iteraciones) o las mismas con el motor en C++, más rápido
#' # dl_correr("mi_proyecto", semilla = 1)
#' # dl_correr("mi_proyecto", semilla = 1, opciones = dl_opciones_mcmc(motor = "rcpp"))
#' }
#' @export
dl_correr <- function(proyecto, causa = NULL, semilla, carpeta_salida = NULL, rapido = FALSE, sensibilidad = TRUE,
                      opciones = NULL, registro = NULL) {
  if (missing(proyecto)) .dl_stop("falta `proyecto` (la carpeta del proyecto o un proyecto de dl_proyecto())")
  .dl_exigir_semilla(semilla)
  .dl_exigir_si_no(rapido, "rapido")
  .dl_exigir_si_no(sensibilidad, "sensibilidad")
  if (!is.null(opciones)) .dl_exigir_clase(opciones, "dl_mcmc_opts", "opciones", "dl_opciones_mcmc()")
  if (!is.null(carpeta_salida)) .dl_exigir_carpeta(carpeta_salida, argumento = "carpeta_salida")
  .dl_exigir_registro(!is.null(registro), registro)
  p <- if (inherits(proyecto, "dl_proyecto")) proyecto
       else if (.dl_es_texto1(proyecto) && dir.exists(proyecto)) dl_proyecto(proyecto, causa)
       else .dl_stop("`proyecto` debe ser la carpeta de un proyecto o un proyecto de dl_proyecto(); es %s",
                     .dl_describir_objeto(proyecto))
  cfg <- p$configuracion
  if (!is.null(causa) && .dl_exigir_causa(causa) != cfg$cause_id)
    .dl_stop("`causa` es %d y el proyecto es de la causa %d", .dl_exigir_causa(causa), cfg$cause_id)
  if (is.null(carpeta_salida)) {
    # el proyecto de ejemplo, instalado con el paquete, no es un lugar para escribir
    if (.dl_en_paquete(p$carpeta))
      .dl_stop("el proyecto est\u00e1 dentro de la instalaci\u00f3n del paquete (%s): da `carpeta_salida`", p$carpeta)
    carpeta_salida <- file.path(p$carpeta, "resultados")
  }
  # con un proyecto simple, los mensajes en sus palabras
  .dl_en_simple(simple = .dl_es_simple(cfg), datos = p$tablas$datos, {
    o <- opciones %||% if (rapido) do.call(dl_opciones_mcmc, .DL_OPCIONES_PRUEBA) else dl_opciones_mcmc()
    if (rapido)
      .dl_message(paste0("corrida de prueba (rapido = TRUE): %d cadena(s) de %d iteraciones y forzar = TRUE, que la ",
                         "escribe aunque las cadenas no converjan. Sus n\u00fameros no sirven para publicar: para la ",
                         "corrida final, rapido = FALSE"), o$chains, o$iter)

    .dl_message("causa %d: insumos", cfg$cause_id)
    b <- dl_insumos(p)
    .dl_exigir_severidad(b$severidad, cfg$cause_id)

    .dl_message("ajuste nacional: %d cadena(s) de %d iteraciones por sexo (motor %s)", o$chains, o$iter, o$engine)
    f <- dl_ajustar(b, o, semilla = semilla)
    .dl_compuerta_convergencia(f, forzar = rapido, remedio = paste0(
      "Aumenta las iteraciones y el calentamiento con opciones = dl_opciones_mcmc(iteraciones = ..., ",
      "calentamiento = ...); para una prueba, rapido = TRUE"))
    f0 <- dl_ajustar_solo_prior(b, o, semilla = semilla, ajuste = f)
    casc <- NULL
    if (nrow(b$cov_proxy) > 0L || identical(b$cfg$cascada$modo$valor, "plana")) {
      .dl_message("cascada subnacional")
      casc <- dl_cascada(f, b, semilla = semilla)
    } else {
      .dl_message("sin estimaci\u00f3n subnacional: %s; la corrida es solo nacional",
                  if (!identical(cfg$origen$subnacional, "no"))
                    "no hay proxies del a\u00f1o que se estima y la estimaci\u00f3n subnacional no es plana"
                  else if ("subnacional.modo" %in% names(cfg$origen$por_defecto))
                    "subnacional.modo es no, porque el proyecto no trae proxies ni poblaci\u00f3n subnacional"
                  else "la configuraci\u00f3n declara subnacional.modo: no")
    }
    ajuste <- casc %||% f

    .dl_message("validaci\u00f3n contra el ancla y AVD")
    validacion <- dl_validar_ancla(f, b, cascada = casc)
    ruta <- "anchor: {gate_err_mediano: {valor: ..., procedencia: ...}}"
    .dl_compuerta_ancla(b$cfg, validacion, remedio = sprintf(paste0(
      "declara un m\u00e1ximo mayor en la configuraci\u00f3n, con su procedencia: %s (ver ?dl_configuracion); ",
      "rapido = TRUE no la salta"), if (.dl_es_simple(cfg)) sprintf("avanzado: {%s}", ruta) else ruta))
    comorbilidad <- if (!is.null(b$rutas$std_yld)) dl_factor_comorbilidad(b)
    if (is.null(comorbilidad))
      .dl_message("el ancla no trae AVD: los AVD no se corrigen por comorbilidad (el manifiesto lo declara)")
    avd <- dl_avd(ajuste, b, comorbilidad, semilla = semilla)

    .dl_message("etiquetas de cu\u00e1nto informan los datos")
    etiquetas <- if (rapido) dl_etiquetas(f, f0, b, grilla_rho = b$cfg$anchor$rho_edad, semilla = semilla,
                                          cascada = casc)
                 else dl_etiquetas(f, f0, b, semilla = semilla, cascada = casc)
    sens <- NULL
    if (sensibilidad) {
      .dl_message("sensibilidad (grilla `sensibilidad` de la configuraci\u00f3n)")
      sens <- if (rapido) dl_sensibilidad(b, semilla = semilla, opciones = o) else dl_sensibilidad(b, semilla = semilla)
    }

    resumen <- dl_resumir(list(fit = ajuste, yld = avd, bundle = b))
    run <- dl_exportar_corrida(list(resumen = resumen, fit = ajuste, yld = avd, bundle = b),
                               nombre = sprintf("causa-%d%s", cfg$cause_id, if (rapido) "-prueba" else ""),
                               carpeta = carpeta_salida, etiquetas = etiquetas, validacion = validacion,
                               sensibilidad = sens, forzar = rapido, registro = registro)
    .dl_message("corrida escrita en %s%s", run$dir, if (rapido) " (prueba: no sirve para publicar)" else "")
    run
  })
}
