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
.DL_PLANTILLAS_NUEVO <- c("ubicaciones", "poblacion", "betas", "datos", "severidad", "poblacion_detalle",
                          "proxies_crudos", "razones")
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
    "covariables/           # descargas del GHDx (valor nacional) y los proxies ya calibrados, si los hay",
    "betas.csv              # el efecto de cada covariable",
    "datos.csv              # datos locales",
    "severidad.csv          # estados de salud y sus proporciones (para los AVD)",
    "fuentes_gbd/           # la lista de fuentes del GHDx que GBD ya us\u00f3",
    "poblacion_detalle.csv  # poblaci\u00f3n nacional con m\u00e1s detalle de edad, si el ancla es m\u00e1s fina",
    "proxies_crudos.csv     # un indicador de encuesta por ubicaci\u00f3n y edici\u00f3n (se calibra al leer)",
    "razones.csv            # la raz\u00f3n de cada ubicaci\u00f3n subnacional (con subnacional.modo: razon)",
    "particion/<corrida>/   # (opcional) la partici\u00f3n de severidad que nombra severidad.particion",
    "```", "",
    "Una tabla con solo el encabezado no se usa: las obligatorias son `ubicaciones`, `poblacion` y `ancla`.", "",
    sprintf("1. Completa `%s`.", archivo_config),
    "2. Llena `ubicaciones.csv` y `poblacion.csv`.",
    sprintf("3. Pon en `ancla/` las descargas de GBD Results (%s), sin editar.", url$gbd$url),
    sprintf(paste0("4. Si usas covariables, llena `betas.csv` y pon en `covariables/` sus descargas del GHDx (%s), ",
                   "sin editar. Sus valores subnacionales salen de una sola de dos fuentes por covariable: ",
                   "`proxies_crudos.csv` (un indicador de encuesta, que se calibra al leer; ver ",
                   "`?dl_calibrar_proxies`) o una tabla en `covariables/` con los proxies ya calibrados."),
            url$ghdx$url),
    paste0("5. Si repartes por raz\u00f3n (`subnacional: {modo: razon}` en la configuraci\u00f3n), llena ",
           "`razones.csv`: la raz\u00f3n de cada ubicaci\u00f3n subnacional respecto de la nacional y el error de ",
           "su logaritmo, en el a\u00f1o que se estima. Ese modo no lleva valores subnacionales de covariables."),
    "6. Llena las dem\u00e1s tablas que uses (`severidad.csv`, para los AVD; `datos.csv`; ...).",
    sprintf("7. Revisa el proyecto: `dl_revisar_proyecto(\"%s\")`.", ruta),
    sprintf(paste0("8. Pru\u00e9balo con `dl_correr(\"%s\", semilla = 1, rapido = TRUE)`; la corrida final, con ",
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
#' - `ubicaciones.csv`, `poblacion.csv`, `betas.csv`, `datos.csv`, `severidad.csv`, `poblacion_detalle.csv`,
#'   `proxies_crudos.csv` y `razones.csv`, con solo el encabezado (las columnas de [dl_plantilla()]). Una tabla con
#'   solo el encabezado es como si no estuviera: las opcionales que no se llenan no se usan.
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
  suma <- cf$simple && is.na(cf$error) && .dl_es_suma(.dl_leer_config(cf$archivo))
  .dl_revisar_pasos(cf$causa, cf$simple, function(revisar, anotar, errores) {
    if (cf$simple) return(.dl_revisar_contrato(carpeta, cf, rel, revisar, anotar, errores))
    cfg <- revisar("configuraci\u00f3n", dl_configuracion(cf$causa, cf$archivo),
                   function(cfg) sprintf("%s: formato completo", rel))
    if (!is.null(cfg)) function() .dl_proyecto_de(carpeta, cfg)
  }, insumos = !suma)
}

# Las filas de la revisión de la causa `causa`: los pasos de `pasos(revisar, anotar, errores)`, que devuelve la
# función que arma el proyecto (NULL si algo falló), y al final los insumos y la severidad. `revisar(paso, expr, ok)`
# evalúa `expr` en el paso `paso` y anota sus avisos, sus problemas y, si no falló, la línea en orden `ok(valor)`
# (NULL: ninguna); devuelve el valor (NULL si falló). En un proyecto con las tablas del contrato (`simple`), un
# problema que lleva su tabla va al paso de esa tabla, sin la tabla delante. Sin `insumos` (una suma de subtipos, que
# no se ajusta), la revisión termina con los pasos.
.dl_revisar_pasos <- function(causa, simple, pasos, insumos = TRUE) {
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
  if (!insumos) return(do.call(rbind, filas))
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

# Las reglas entre tablas de `pre` (list(tablas, tablas_modelo, cfg, causas)) en el paso «proyecto»; un año del
# ancla proyectado sin declararlo (.dl_proyeccion_anunciada) es un aviso.
.dl_revisar_reglas <- function(pre, anotar) {
  pr <- .dl_problemas_proyecto(pre$tablas_modelo %||% pre$tablas, pre$cfg, max(1L, nrow(pre$causas)),
                               originales = pre$tablas)
  for (a in c(.dl_proyeccion_anunciada(pre$cfg), pr$avisos)) anotar("proyecto", "aviso", a)
  for (e in pr$problemas) anotar("proyecto", "error", e)
  if (!length(pr$problemas)) anotar("proyecto", "ok", "las reglas entre tablas se cumplen")
}

# La línea en orden del paso «proxies»: por covariable de la calibración `cal` (dl_calibrar_proxies), el método, q
# (con 3 cifras; si quedó en un borde de su intervalo, cuál), las ediciones usadas, las excluidas y, si cierra en el
# valor nacional de otra covariable (valor_nacional_de), cuál.
.dl_linea_proxies <- function(cal) {
  k <- attr(cal, "calibracion")
  borde <- c(inferior = " (en el borde inferior: el gradiente es pr\u00e1cticamente constante)",
             superior = " (en el borde superior: cada edici\u00f3n manda)")[k$q_en_borde]
  borde[is.na(borde)] <- ""
  q <- ifelse(is.na(k$q), "no se estima (una sola edici\u00f3n por serie)",
              vapply(k$q, function(x) format(signif(x, 3L)), ""))
  paste(sprintf("%s: %s, q = %s%s, ediciones %s%s%s", k$covariable, k$metodo, q, borde, k$ediciones,
                ifelse(nzchar(k$excluidas), paste0("; excluidas: ", k$excluidas), ""),
                ifelse(is.na(k$valor_nacional_de), "", paste0("; cierra en el valor nacional de ", k$valor_nacional_de))),
        collapse = "\n")
}

# El paso «proxies» de la revisión de una carpeta: la calibración de proxies_crudos (.dl_calibracion_proyecto), con
# sus avisos y sus errores, cuando las tablas de las que toma algo se leyeron (`fallidas`: las que no). Devuelve
# list(ok, calibracion): `ok` es FALSE si la calibración falló o espera a una tabla; sin proxies_crudos, la
# calibración es NULL y no hay paso, salvo un aviso si la configuración trae claves proxies.* (no se usan). `padre`:
# la causa que declara a `causa` en `subtipos` (solo se evalúa al calibrar, dentro del paso).
.dl_revisar_proxies <- function(s, archivo, tablas, fallidas, revisar, anotar, causa = s[["causa"]], padre = NULL) {
  if (is.null(tablas$proxies_crudos)) {
    .dl_avisar_proxies_sin_crudos(s, anotar)
    return(list(ok = TRUE, calibracion = NULL))
  }
  espera <- intersect(c("covariables", "poblacion"), fallidas)
  if (length(espera)) {
    anotar("proxies", "omitido", sprintf("la calibraci\u00f3n espera a la tabla %s", espera[1L]))
    return(list(ok = FALSE))
  }
  cal <- revisar("proxies", .dl_calibracion_proyecto(s, archivo, tablas, causa = causa, padre = padre),
                 .dl_linea_proxies)
  list(ok = !is.null(cal), calibracion = cal)
}

# Lo propio de una suma de subtipos (.dl_es_suma) en la revisión, con su configuración `s`. En «configuración», un
# aviso con las claves que no se usan (las del modelo: la causa no se ajusta).
.dl_avisar_claves_suma <- function(s, anotar) {
  fuera <- setdiff(names(s), .DL_CLAVES_SUMA)
  if (length(fuera))
    anotar("configuraci\u00f3n", "aviso", sprintf(paste0(
      "la causa es una suma de subtipos (suma_de_subtipos: s\u00ed) y no se ajusta: no se usa(n) la(s) clave(s) %s"),
      paste(fuera, collapse = ", ")))
}

# En «configuración», un aviso por cada clave corta de la configuración `s` que el bloque `avanzado` también declara,
# en la clave del formato completo a la que se traduce (su `destino` en la tabla de claves), con otro valor: rige el
# de `avanzado`, que se aplica sobre la traducción. Se comparan las claves de números, que pasan tal cual; de
# `avanzado`, el valor de la clave o el de su campo `valor`.
.dl_avisar_avanzado <- function(s, anotar) {
  avanzado <- s[["avanzado"]]
  if (!is.list(avanzado)) return(invisible())
  t <- .dl_claves_simple()
  t <- t[nzchar(t$destino) & !grepl("[", t$destino, fixed = TRUE) &
           t$tipo %in% c("entero", "n\u00famero", "lista de n\u00fameros", "lista de enteros"), ]
  texto <- function(v) paste(vapply(unlist(v), format, ""), collapse = ", ")
  for (i in seq_len(nrow(t))) {
    corto <- .dl_valor_en(s, t$clave[i])
    largo <- .dl_valor_en(avanzado, t$destino[i])
    if (.dl_es_mapa(largo)) largo <- largo[["valor"]]
    if (is.null(corto) || is.null(largo) || !is.numeric(unlist(corto)) || !is.numeric(unlist(largo)) ||
        identical(as.numeric(unlist(corto)), as.numeric(unlist(largo)))) next
    anotar("configuraci\u00f3n", "aviso", sprintf(
      "%s (%s) y avanzado: %s (%s) dicen valores distintos: rige el de `avanzado`; deja uno de los dos",
      t$clave[i], texto(corto), t$destino[i], texto(largo)))
  }
}

# En «proyecto», un aviso con los subtipos que entran en la suma y no tienen configuración en el proyecto (`causas`:
# las que la tienen, .dl_causas_config; NULL sin carpeta), cuyas corridas pueden venir de otra carpeta, y la línea en
# orden que dice qué se suma.
.dl_revisar_subtipos <- function(s, causas, anotar) {
  sin <- setdiff(.dl_subtipos_suma(s)$entran, causas$causa)
  if (length(sin))
    anotar("proyecto", "aviso", sprintf(paste0(
      "el/los subtipo(s) %s no tiene(n) configuraci\u00f3n en el proyecto: puede(n) estar en otra carpeta (con ",
      "subtipo_de: %d); dl_correr() busca sus corridas en la carpeta de las corridas de esta causa"),
      paste(sin, collapse = ", "), as.integer(s[["causa"]])))
  anotar("proyecto", "ok", sprintf("%s: dl_correr() suma las corridas de sus subtipos", .dl_texto_suma(s)))
}

# Aviso del paso «proxies» cuando la configuración `s` trae claves proxies.* y el proyecto no trae proxies_crudos:
# esas claves no se usan (solo en la revisión; dl_proyecto() no avisa).
.dl_avisar_proxies_sin_crudos <- function(s, anotar) {
  if (length(s[["proxies"]]))
    anotar("proxies", "aviso", "las claves proxies.* no se usan: el proyecto no trae proxies_crudos")
}

# Los pasos de un proyecto ya armado `p` (dl_proyecto(): sus tablas ya se leyeron y su configuración se tradujo): cada
# tabla en orden, con sus avisos, la calibración de proxies_crudos (si la trae), la configuración, las reglas entre
# tablas y, para los insumos, el mismo proyecto. Del formato completo, solo la configuración.
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
  suma <- .dl_es_suma(cfg)
  if (!is.null(p$calibracion)) {
    for (a in attr(p$calibracion, "avisos")) anotar("proxies", "aviso", a)
    anotar("proxies", "ok", .dl_linea_proxies(p$calibracion))
  } else if (suma) .dl_avisar_claves_suma(cfg$origen$configuracion, anotar)
  else .dl_avisar_proxies_sin_crudos(cfg$origen$configuracion, anotar)
  if (!suma) .dl_avisar_avanzado(cfg$origen$configuracion, anotar)
  archivo <- cfg$origen$archivo
  anotar("configuraci\u00f3n", "ok", sprintf("%s: proyecto", if (identical(archivo, "configuracion"))
    "la configuraci\u00f3n dada como lista" else basename(archivo)))
  causas <- if (!is.null(p$carpeta)) .dl_causas_config(p$carpeta, NULL, cfg$cause_id)
  .dl_revisar_reglas(list(tablas = p$tablas, tablas_modelo = .dl_tablas_modelo(p$tablas, p$calibracion), cfg = cfg,
                          causas = causas), anotar)
  if (suma) .dl_revisar_subtipos(cfg$origen$configuracion, causas, anotar)
  if (!errores()) function() p
}

# Los pasos de un proyecto con las tablas del contrato (`revisar`, `anotar` y `errores`: los de .dl_revisar_causa):
#   1. cada tabla, leída por su lector y validada sola (las obligatorias que faltan, un error en su paso);
#   2. «proxies», si el proyecto trae proxies_crudos: su calibración, con las claves de la configuración ya revisadas
#      (.dl_revisar_proxies);
#   3. la configuración: sus claves y, si las tablas de las que toma algo (ubicaciones, betas, covariables) se leyeron,
#      su traducción, con la severidad de la partición si la declara;
#   4. «proyecto»: las reglas entre tablas (.dl_problemas_proyecto), si nada falló antes.
# Devuelve la función que arma el proyecto (como dl_proyecto(), con su traducción: dentro del paso «insumos», donde un
# error de la traducción queda en la revisión) o NULL si algo falló. De una suma de subtipos (.dl_es_suma) solo se
# leen sus tablas (ubicaciones y poblacion), no hay paso «proxies» y el paso «proyecto» dice además qué subtipos no
# tienen configuración en el proyecto (.dl_revisar_subtipos).
.dl_revisar_contrato <- function(carpeta, cf, rel, revisar, anotar, errores) {
  s <- .dl_leer_config(cf$archivo)
  suma <- .dl_es_suma(s)
  fallidas <- character()
  tablas <- .dl_tablas_proyecto(carpeta, list(), .dl_opciones_lectores(s), cuales = .dl_tablas_de(s),
                                paso = function(t, expr) {
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
  if (suma) .dl_avisar_claves_suma(s, anotar) else .dl_avisar_avanzado(s, anotar)
  espera <- intersect(c("ubicaciones", "betas", "covariables"), fallidas)
  if (length(espera)) {
    anotar("configuraci\u00f3n", "omitido", sprintf("su traducci\u00f3n espera a la tabla %s", espera[1L]))
    return(omitir())
  }
  px <- if (suma) list(ok = TRUE)
        else .dl_revisar_proxies(s, cf$archivo, tablas, fallidas, revisar, anotar, cf$causa,
                                 .dl_padre_de(.dl_causas_config(carpeta, s, cf$causa), cf$causa))
  if (!px$ok) {
    anotar("configuraci\u00f3n", "omitido", "su traducci\u00f3n espera a la calibraci\u00f3n de proxies_crudos")
    return(omitir())
  }
  pre <- revisar("configuraci\u00f3n",
                 .dl_config_de_tablas(s, cf$archivo, cf$causa, tablas, .dl_causas_config(carpeta, s, cf$causa),
                                      carpeta, calibracion = px$calibracion),
                 function(pre) sprintf("%s: proyecto", rel))
  if (is.null(pre) || errores()) return(omitir())
  .dl_revisar_reglas(pre, anotar)
  if (suma) .dl_revisar_subtipos(s, pre$causas, anotar)
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
#' lo que ellos aceptan. Sus pasos, en orden (los de una causa que se ajusta; los de una suma de subtipos, más abajo):
#' 1. cada tabla (`ubicaciones`, `poblacion`, `ancla`, ...), leída por su lector y validada sola, como en [dl_tabla()]:
#'    una columna que falta, un CSV guardado desde Excel con «;» o coma decimal, un sexo, una medida o un número que no
#'    se reconoce, bandas de edad que se solapan... La línea en orden dice cuántas filas trae
#'    (`leída: N fila(s)`) y, si pasó por un lector, cuál (`descarga de GBD Results`). Una tabla obligatoria que falta
#'    (o que solo trae el encabezado) es un error;
#' 2. `configuración`: sus claves y valores, como los lee [dl_configuracion()]. Su traducción toma cosas de las
#'    tablas (la ubicación nacional, las betas): si `ubicaciones`, `betas` o `covariables` tienen un error, espera
#'    (`-`). Avisa (`!`) de una clave que el bloque `avanzado` también declara, en su clave del formato completo, con
#'    otro valor (por ejemplo `ancla.error_maximo` y `avanzado: anchor.gate_err_mediano`): rige el de `avanzado`, y
#'    [dl_proyecto()] no lo dice;
#' 3. `proyecto`: las reglas que cruzan tablas, cada problema con su tabla delante: que toda ubicación esté en
#'    `ubicaciones` (sin códigos repetidos, una sola sin `padre`, la nacional; las subnacionales con la nacional de
#'    padre); que la población traiga el año que se estima y los sexos del modelo, con las mismas bandas de edad en
#'    todas las ubicaciones, años y sexos, seguidas (sin huecos) y desde `edad_inicio`; que el ancla traiga la
#'    prevalencia de la causa (y la mortalidad, si la usa el prior de la mortalidad en exceso) en el año del ancla y en
#'    cada sexo, y la columna `causa` si el proyecto tiene varias; que cada banda del ancla sea una unión de bandas de
#'    la población o se pueda agrupar con `poblacion_detalle`; que cada covariable de `betas` tenga su valor nacional
#'    en el año del ancla (el suyo o, con `valor_nacional_de` y valores subnacionales de la covariable, el de la
#'    covariable que nombra) y que `escala` vaya solo con la transformación lineal; que cada ubicación subnacional
#'    con proxies los traiga de todas las covariables y que el valor nacional en que se anclan traiga su intervalo
#'    (`inferior` y `superior`); que las proporciones de `severidad` sumen 1; con `subnacional.modo: razon`, que la
#'    tabla `razones` traiga, en el año que se estima, la razón de cada ubicación subnacional de la población y de
#'    ninguna otra, con números finitos y alguna razón mayor que 0. Avisa (`!`) de una covariable con
#'    proxies y sin beta (no se usa), de una ubicación subnacional sin proxies (queda fuera de la estimación
#'    subnacional), de una tabla `razones` sin el modo `razon` (no se usa) y de valores de mortalidad de `datos` que
#'    parecen tasas por 100 000 en vez de por persona-año (mayores que 1, o más de 1000 veces la mortalidad del ancla
#'    en la misma causa, año, sexo y banda);
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
#' De una causa que es la suma de sus subtipos (`suma_de_subtipos: sí`; ver [dl_proyecto()]) se revisan sus dos
#' tablas (`ubicaciones` y `poblacion`), su configuración y, en `proyecto`, las reglas de esas tablas; no hay paso
#' `insumos`, porque la causa no se ajusta. Avisa (`!`) de las claves de la configuración que una suma no usa y de
#' los subtipos que entran en la suma y no tienen configuración en el proyecto: pueden estar en otra carpeta, y
#' [dl_correr()] busca sus corridas al sumar.
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
                           function(revisar, anotar, errores) .dl_revisar_objeto(p, revisar, anotar, errores),
                           insumos = !.dl_es_suma(p))
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
#' 3. la cascada subnacional ([dl_cascada()]), si hay proxies del año que se estima o `subnacional.modo` es `plano`
#'    o `razon` (`cascada.modo: plana` o `razon` en el formato completo); si no, un mensaje lo dice y la corrida es
#'    solo nacional;
#' 4. la validación contra el ancla ([dl_validar_ancla()]);
#' 5. el factor de comorbilidad ([dl_factor_comorbilidad()], si el ancla trae AVD), los AVD ([dl_avd()]) y, con
#'    `subnacional.modo: razon`, el reparto por razón ([dl_repartir_razon()]), con la semilla de la corrida o la de
#'    `avanzado: {cascada: {razon_semilla: ...}}`: las tasas subnacionales de la corrida son las repartidas, y la
#'    validación, las etiquetas y la sensibilidad son las del ajuste nacional;
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
#' (error relativo mediano mayor que 0.05), justo después de la validación, también con `rapido = TRUE` o
#' `forzar = TRUE`: revisa el ajuste y los datos o declara un máximo mayor en la configuración, `ancla: {error_maximo:
#' ...}` en un proyecto (en el formato completo, `anchor: {gate_err_mediano: {valor, procedencia}}`; ver
#' [dl_configuracion()]).
#'
#' `forzar = TRUE` salta la primera compuerta, la de la convergencia, y solo esa: la corrida se escribe con las
#' cadenas que se pidieron aunque no hayan convergido, un mensaje da su R-hat y su ESS y el manifiesto lo declara
#' (`validacion.gates.force`), como en [dl_exportar_corrida()]. El nombre de la corrida no cambia. Sirve para mirar
#' una corrida que aún no converge; sus números no sirven para publicar.
#'
#' `rapido = TRUE` es una prueba, para ver que el proyecto corre de principio a fin: cadenas cortas (100
#' simulaciones, 2 cadenas de 2000 iteraciones), las mismas en la sensibilidad, etiquetas solo con la
#' `ancla.correlacion_edad` de la configuración y la corrida escrita con `forzar = TRUE`, sin exigir la convergencia
#' (el manifiesto lo declara). Su nombre termina en `-prueba` y sus números no sirven para publicar.
#'
#' Con `anios`, la causa se corre una vez por año, en orden, y el proyecto se vuelve a leer para cada uno
#' (`dl_proyecto(anio = )`, con la regla del año del ancla de [dl_proyecto()]): un proyecto sirve para varios años sin
#' editar su configuración. Las comprobaciones de los argumentos se hacen una vez, antes del primer año; el nombre de
#' cada corrida lleva el año (`causa-<causa>-<año>`, y `-prueba` con `rapido = TRUE`), y cada corrida es de por sí
#' una corrida completa, como la de un `dl_correr()` sin `anios`. El proyecto de todos los años se lee antes de correr
#' el primero: si el de alguno no se puede leer (el ancla no trae ese año ni el anterior, falta su población...),
#' `dl_correr()` se detiene ahí, con el año y el problema, sin haber escrito ninguna corrida. Si un año falla al
#' correr, se detiene con el error de ese año y dice qué años quedaron escritos; esas corridas siguen en su carpeta.
#'
#' La causa debe tener severidad (sus estados de salud en `severidad.csv` o desde `severidad.particion`): sin ella los
#' AVD serían cero y la corrida no se escribe. `dl_correr()` lo comprueba antes de ajustar.
#'
#' Una causa que es la suma de sus subtipos (`suma_de_subtipos: sí` en su configuración; ver [dl_proyecto()]) no se
#' ajusta: `dl_correr()` busca en `carpeta_salida` la corrida más reciente de cada subtipo que entra en la suma, del
#' año que se estima, y las suma simulación a simulación con [dl_sumar_hijas()], con el `nombre` y los
#' `subtipos_omitidos` de la configuración. Las corridas se reconocen por su manifiesto (la causa y el año), así que
#' valen las de cualquier proyecto escritas en esa carpeta. Con `rapido = TRUE` suma las corridas de prueba (las que
#' terminan en `-prueba`) y la suma también es de prueba; con `rapido = FALSE`, las de producción, nunca las de
#' prueba. Si falta la corrida de un subtipo, el error dice cuál, de qué año y dónde se buscó. Con `anios`, suma cada
#' año con las corridas de ese año. `semilla`, `sensibilidad`, `opciones` y `forzar` no se usan: no hay ajuste. El
#' nombre de la corrida sigue la misma regla (`causa-<causa>`, con el año y `-prueba` si corresponde), y la corrida es
#' la de [dl_sumar_hijas()]: celdas, simulaciones y manifiesto, sin diagnósticos.
#'
#' La corrida más reciente de un subtipo es la del último día (la fecha de su nombre) y, en ese día, la de mayor
#' versión. Si ese día hay corridas del subtipo con nombres distintos (por ejemplo `causa-<causa>` y
#' `causa-<causa>-<año>`), gana la que se escribió última, por la fecha de modificación de su `manifest.yaml`: copiar
#' la carpeta de las corridas sin conservar las fechas de los archivos puede cambiar ese desempate. Los mensajes dicen
#' qué corrida se tomó de cada subtipo y cuáles se escribieron con `forzar = TRUE`, que en una suma de producción
#' quedan también entre las limitaciones de su manifiesto; para elegirlas a mano, [dl_sumar_hijas()]. Los subtipos de
#' una suma son causas que se ajustan: una suma no puede ser subtipo de otra suma.
#'
#' Los ajustes quedan en la caché de la sesión: [dl_ajustar()] con los mismos insumos, opciones y semilla devuelve el
#' de la corrida sin volver a muestrear. Con un proyecto con las tablas del contrato, los mensajes de todos los pasos
#' citan sus claves y sus tablas (como [dl_insumos()]).
#'
#' @param proyecto Carpeta del proyecto (con las tablas del contrato de insumos o en el formato completo) o un
#'   proyecto de [dl_proyecto()].
#' @param causa Causa (`cause_id`); `NULL` si el proyecto tiene una sola (o si `proyecto` ya es un `dl_proyecto`).
#' @param semilla Semilla (entero), obligatoria: el resultado es reproducible con la misma semilla. Una causa que es
#'   la suma de sus subtipos no se ajusta y no la usa.
#' @param carpeta_salida Carpeta raíz de las corridas: la corrida se escribe en
#'   `<carpeta_salida>/mod/dismod_lite/<AAAA-MM-DD>_causa-<causa>_v<n>/`. `NULL` (por defecto) es la carpeta
#'   `resultados` del proyecto. Una suma de subtipos busca ahí las corridas de sus subtipos.
#' @param rapido `TRUE` para una corrida de prueba (ver Detalles); `FALSE` (por defecto), la de producción.
#' @param sensibilidad `TRUE` (por defecto) corre el análisis de sensibilidad y lo guarda en la corrida
#'   (`diagnostics/sensibilidad.csv`); `FALSE` lo omite.
#' @param opciones Opciones de [dl_opciones_mcmc()] del ajuste nacional; `NULL` (por defecto) usa las de producción
#'   o, con `rapido = TRUE`, las cortas. Con `rapido = TRUE`, las `opciones` que se den reemplazan a las cortas y la
#'   corrida sigue siendo una prueba.
#' @param registro Archivo YAML del registro de corridas, que ya existe (uno nuevo es un archivo con la línea
#'   `datasets: []`): la corrida se agrega al final. Se comprueba antes de ajustar. `NULL` (por defecto) no registra.
#' @param forzar `TRUE` escribe la corrida aunque las cadenas no hayan convergido (queda declarado en el manifiesto;
#'   ver Detalles); no salta la compuerta del ancla. `FALSE` (por defecto) exige la convergencia, salvo con
#'   `rapido = TRUE`, que la salta siempre.
#' @param anios Vector de años enteros, sin `NA` ni repetidos (por ejemplo `2019:2023`): corre la causa una vez por
#'   año, en orden, con el proyecto leído para cada uno (ver Detalles). `NULL` (por defecto) corre una sola vez, el
#'   año de la configuración del proyecto.
#' @return Sin `anios`, un objeto de clase `dl_run`, como el de [dl_exportar_corrida()], una lista con `run_id` (el
#'   identificador de la corrida, `<AAAA-MM-DD>_causa-<causa>_v<n>`, o `..._causa-<causa>-prueba_v<n>` con
#'   `rapido = TRUE`), `dir` (su carpeta), `manifest` (el contenido de su `manifest.yaml`, como lista) y `files` (las
#'   tablas de celdas escritas, una por medida, con su ruta, su sha256 y su número de filas). Los mensajes dicen qué
#'   paso corre y, al final, dónde quedó la corrida.
#'
#'   Con `anios`, una lista de clase `dl_corridas` con un `dl_run` por año, con los años como nombres
#'   (`corridas[["2023"]]`); su identificador es `<AAAA-MM-DD>_causa-<causa>-<año>_v<n>`
#'   (`..._causa-<causa>-<año>-prueba_v<n>` con `rapido = TRUE`). `print()` muestra una línea por año.
#' @seealso [dl_proyecto()], [dl_revisar_proyecto()] (antes de correr), [dl_opciones_mcmc()] (las cadenas),
#'   [dl_estimaciones()] y [dl_exportar_corrida()] (el contenido de la carpeta de la corrida); para sumar a mano las
#'   corridas de los subtipos de una causa, [dl_sumar_hijas()].
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
#' # la misma causa para dos años: una corrida por año, que el proyecto relee para cada uno
#' corridas <- dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE, sensibilidad = FALSE,
#'                       carpeta_salida = salida, anios = c(2019, 2023))
#' corridas
#' corridas[["2023"]]$run_id
#' unlink(salida, recursive = TRUE)
#'
#' # la corrida de producción de un proyecto propio: las cadenas por defecto (1000 simulaciones,
#' # 4 cadenas de 50 000 iteraciones) o las mismas con el motor en C++, más rápido
#' # dl_correr("mi_proyecto", semilla = 1)
#' # dl_correr("mi_proyecto", semilla = 1, opciones = dl_opciones_mcmc(motor = "rcpp"))
#' # y, para mirar una corrida cuyas cadenas aún no convergen, sin la compuerta de la convergencia
#' # dl_correr("mi_proyecto", semilla = 1, forzar = TRUE)
#' # y una corrida de producción por año
#' # dl_correr("mi_proyecto", semilla = 1, anios = 2018:2023)
#'
#' # una causa que es la suma de sus subtipos: en una copia del ejemplo, la causa 9100, que declara
#' # sus subtipos (9101, 9102 y 9103), deja de ajustarse con `suma_de_subtipos: sí`
#' carpeta <- dl_ejemplo(copiar_en = file.path(tempdir(), "proyecto_con_suma"))
#' con <- file(file.path(carpeta, "config", "9100.yaml"), open = "a", encoding = "UTF-8")
#' writeLines("suma_de_subtipos: sí", con)
#' close(con)
#' dl_proyecto(carpeta, causa = 9100)
#' # primero una corrida de cada subtipo (aquí, de prueba) y después la suma, que las busca en la
#' # carpeta de las corridas (`resultados`, en la carpeta del proyecto)
#' for (k in c(9101, 9102, 9103))
#'   dl_correr(carpeta, causa = k, semilla = 1, rapido = TRUE, sensibilidad = FALSE)
#' suma <- dl_correr(carpeta, causa = 9100, rapido = TRUE)
#' vapply(suma$manifest$causa$hijas, `[[`, "", "run_id")
#' unlink(carpeta, recursive = TRUE)
#' }
#' @export
dl_correr <- function(proyecto, causa = NULL, semilla, carpeta_salida = NULL, rapido = FALSE, sensibilidad = TRUE,
                      opciones = NULL, registro = NULL, forzar = FALSE, anios = NULL) {
  if (missing(proyecto)) .dl_stop("falta `proyecto` (la carpeta del proyecto o un proyecto de dl_proyecto())")
  # la semilla se exige abajo, con el proyecto leído: una suma de subtipos no la usa
  if (!missing(semilla)) .dl_exigir_semilla(semilla)
  .dl_exigir_si_no(rapido, "rapido")
  .dl_exigir_si_no(sensibilidad, "sensibilidad")
  .dl_exigir_si_no(forzar, "forzar")
  if (!is.null(opciones)) .dl_exigir_clase(opciones, "dl_mcmc_opts", "opciones", "dl_opciones_mcmc()")
  if (!is.null(carpeta_salida)) .dl_exigir_carpeta(carpeta_salida, argumento = "carpeta_salida")
  .dl_exigir_registro(!is.null(registro), registro)
  if (!is.null(anios)) anios <- .dl_exigir_anios(anios)
  p <- .dl_proyecto_a_correr(proyecto, causa, anios)
  cfg <- p$configuracion
  if (!is.null(causa) && .dl_exigir_causa(causa) != cfg$cause_id)
    .dl_stop("`causa` es %d y el proyecto es de la causa %d", .dl_exigir_causa(causa), cfg$cause_id)
  suma <- .dl_es_suma(cfg)
  if (!suma && missing(semilla)) .dl_exigir_semilla()
  if (is.null(carpeta_salida)) {
    # el proyecto de ejemplo, instalado con el paquete, no es un lugar para escribir
    if (.dl_en_paquete(p$carpeta))
      .dl_stop("el proyecto est\u00e1 dentro de la instalaci\u00f3n del paquete (%s): da `carpeta_salida`", p$carpeta)
    carpeta_salida <- file.path(p$carpeta, "resultados")
  }
  o <- opciones %||% if (rapido) do.call(dl_opciones_mcmc, .DL_OPCIONES_PRUEBA) else dl_opciones_mcmc()
  if (rapido && !suma)
    .dl_message(paste0("corrida de prueba (rapido = TRUE): %d cadena(s) de %d iteraciones y forzar = TRUE, que la ",
                       "escribe aunque las cadenas no converjan. Sus n\u00fameros no sirven para publicar: para la ",
                       "corrida final, rapido = FALSE"), o$chains, o$iter)
  nombre <- function(anio) sprintf("causa-%d%s%s", cfg$cause_id, if (is.null(anio)) "" else paste0("-", anio),
                                   if (rapido) "-prueba" else "")
  correr <- function(p, anio)
    if (suma) .dl_correr_suma(p, carpeta_salida, rapido, registro, nombre(anio))
    else .dl_correr_proyecto(p, semilla, carpeta_salida, rapido, sensibilidad, o, registro, forzar, nombre(anio))
  if (is.null(anios)) return(correr(p, NULL))
  .dl_correr_anios(p, anios, correr)
}

# El proyecto que corre dl_correr(): el `dl_proyecto` dado o el de la carpeta `proyecto`, leído para el primero de
# `anios` (con el error de la lectura de ese año, .dl_en_anio) o, sin `anios`, para el año de su configuración.
.dl_proyecto_a_correr <- function(proyecto, causa, anios) {
  if (inherits(proyecto, "dl_proyecto")) return(proyecto)
  if (!.dl_es_texto1(proyecto) || !dir.exists(proyecto))
    .dl_stop("`proyecto` debe ser la carpeta de un proyecto o un proyecto de dl_proyecto(); es %s",
             .dl_describir_objeto(proyecto))
  if (is.null(anios)) return(dl_proyecto(proyecto, causa))
  .dl_en_anio(anios[1L], list(), dl_proyecto(proyecto, causa, anio = anios[1L]), lectura = TRUE)
}

# Las corridas de `anios` (.dl_exigir_anios), una por año y en orden: `correr(p, anio)` corre el proyecto `p`, leído
# para ese año (.dl_proyecto_de_anio). Los proyectos de todos los años se leen antes de correr el primero: un año que
# no se puede leer detiene dl_correr() sin haber escrito ninguna corrida, y los mensajes de cada lectura salen una
# sola vez. Devuelve un `dl_corridas` con una corrida por año.
.dl_correr_anios <- function(p, anios, correr) {
  proyectos <- lapply(anios, function(anio) .dl_en_anio(anio, list(), .dl_proyecto_de_anio(p, anio), lectura = TRUE))
  corridas <- list()
  for (k in seq_along(anios)) {
    .dl_message("a\u00f1o %d (%d de %d)", anios[k], k, length(anios))
    corridas[[as.character(anios[k])]] <- .dl_en_anio(anios[k], corridas, correr(proyectos[[k]], anios[k]))
  }
  structure(corridas, class = "dl_corridas")
}

# Evalúa `expr`, que corre el proyecto del año `anio` o, con `lectura`, lo lee antes de correr ninguno. Si falla, el
# error es el mismo con el año delante y dice qué años quedaron escritos (`corridas`: las que van) o, en la lectura,
# que no se corrió ninguno; conserva las clases del error original, delante de las de todo error del paquete, y sus
# campos (`problemas`, `faltan`...), y lleva los campos `anio` y `escritas`.
.dl_en_anio <- function(anio, corridas, expr, lectura = FALSE) {
  tryCatch(expr, error = function(e) {
    hechas <- vapply(corridas, function(r) r$run_id, "")
    suyos <- unclass(e)[setdiff(names(e), c("message", "call", "detalle", "funcion", "anio", "escritas"))]
    .dl_stop(paste0(if (lectura) "el proyecto no se puede leer para el a\u00f1o %d"
                    else "la corrida del a\u00f1o %d fall\u00f3", ": %s\n  %s"), anio, .dl_detalle(e),
             if (lectura) "No se corri\u00f3 ning\u00fan a\u00f1o: corrige ese a\u00f1o o qu\u00edtalo de `anios`."
             else if (length(hechas)) sprintf("Quedaron escritas las corridas de: %s.",
                                              paste(sprintf("%s (%s)", names(hechas), hechas), collapse = ", "))
             else "No qued\u00f3 escrita ninguna corrida.",
             clase = setdiff(class(e), c("dl_error", "error", "condition")),
             campos = c(list(anio = anio, escritas = as.integer(names(corridas))), suyos))
  })
}

# `anios` de dl_correr(): un vector de años enteros, sin NA ni repetidos. Devuelve los enteros.
.dl_exigir_anios <- function(anios) {
  if (!is.numeric(anios) || !length(anios) || anyNA(anios) || !all(is.finite(anios)) || any(anios != round(anios)) ||
      any(anios < 1) || any(anios > .Machine$integer.max))
    .dl_stop("`anios` debe ser un vector de a\u00f1os, n\u00fameros enteros sin NA (por ejemplo 2018:2023); es %s",
             .dl_describir_objeto(anios))
  if (anyDuplicated(anios))
    .dl_stop("`anios` no debe repetir a\u00f1os: se repite %s", paste(unique(anios[duplicated(anios)]), collapse = ", "))
  as.integer(anios)
}

#' @export
print.dl_corridas <- function(x, ...) {
  cat(sprintf("<dl_corridas> %d corrida(s), una por a\u00f1o\n", length(x)))
  cat(sprintf("  %s: %s\n", names(x), vapply(x, function(r) r$run_id, "")), sep = "")
  invisible(x)
}

# La corrida de un proyecto ya leído (`p`, para el año que se estima) con las opciones `o` del ajuste nacional y la
# carpeta del resultado llamada `nombre`: los pasos de dl_correr(), que ya comprobó sus argumentos.
.dl_correr_proyecto <- function(p, semilla, carpeta_salida, rapido, sensibilidad, o, registro, forzar, nombre) {
  cfg <- p$configuracion
  # con un proyecto simple, los mensajes en sus palabras
  .dl_en_simple(simple = .dl_es_simple(cfg), datos = p$tablas$datos, {
    .dl_message("causa %d: insumos", cfg$cause_id)
    b <- dl_insumos(p)
    .dl_exigir_severidad(b$severidad, cfg$cause_id)
    # con el reparto por razón, la tabla razones se comprueba antes de ajustar
    razon <- identical(b$cfg$cascada$modo$valor, "razon")
    if (razon) .dl_razones_reparto(b)

    .dl_message("ajuste nacional: %d cadena(s) de %d iteraciones por sexo (motor %s)", o$chains, o$iter, o$engine)
    f <- dl_ajustar(b, o, semilla = semilla)
    ess_minimo <- formals(.dl_compuerta_convergencia)$ess_minimo
    convergencia <- .dl_compuerta_convergencia(f, forzar = forzar || rapido, ess_minimo = ess_minimo, remedio = paste0(
      "Aumenta las iteraciones y el calentamiento con opciones = dl_opciones_mcmc(iteraciones = ..., ",
      "calentamiento = ...); para una prueba, rapido = TRUE; forzar = TRUE la escribe igual y lo declara en el ",
      "manifiesto"))
    if (forzar && !.dl_cadenas_convergieron(convergencia, ess_minimo))
      .dl_message(paste0("forzar = TRUE: las cadenas no convergieron (R-hat m\u00e1ximo %.4f, debe ser < 1.01; ESS ",
                         "m\u00ednimo %.0f, debe ser >= %s) y la corrida se escribe igual; el manifiesto lo declara ",
                         "(validacion.gates.force)"), convergencia$rhat_max, convergencia$ess_min, format(ess_minimo))
    f0 <- dl_ajustar_solo_prior(b, o, semilla = semilla, ajuste = f)
    casc <- NULL
    if (nrow(b$cov_proxy) > 0L || identical(b$cfg$cascada$modo$valor, "plana") || razon) {
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
    .dl_compuerta_ancla(b$cfg, validacion,
                        remedio = .dl_remedio_compuerta_ancla(cfg, "ni rapido = TRUE ni forzar = TRUE la saltan"))
    comorbilidad <- if (!is.null(b$rutas$std_yld)) dl_factor_comorbilidad(b)
    if (is.null(comorbilidad))
      .dl_message("el ancla no trae AVD: los AVD no se corrigen por comorbilidad (el manifiesto lo declara)")
    avd <- dl_avd(ajuste, b, comorbilidad, semilla = semilla)
    reparto <- NULL
    if (razon) {
      .dl_message("reparto subnacional por raz\u00f3n (tabla razones)")
      reparto <- dl_repartir_razon(list(fit = casc, yld = avd, bundle = b),
                                   semilla = b$cfg$cascada$razon_semilla %||% semilla)
    }

    .dl_message("etiquetas de cu\u00e1nto informan los datos")
    etiquetas <- if (rapido) dl_etiquetas(f, f0, b, grilla_rho = b$cfg$anchor$rho_edad, semilla = semilla,
                                          cascada = casc)
                 else dl_etiquetas(f, f0, b, semilla = semilla, cascada = casc)
    sens <- NULL
    if (sensibilidad) {
      .dl_message("sensibilidad (grilla `sensibilidad` de la configuraci\u00f3n)")
      sens <- if (rapido) dl_sensibilidad(b, semilla = semilla, opciones = o) else dl_sensibilidad(b, semilla = semilla)
    }

    resumen <- dl_resumir(c(list(fit = ajuste, yld = avd, bundle = b), if (razon) list(reparto = reparto)))
    run <- dl_exportar_corrida(list(resumen = resumen, fit = ajuste, yld = avd, bundle = b),
                               nombre = nombre,
                               carpeta = carpeta_salida, etiquetas = etiquetas, validacion = validacion,
                               sensibilidad = sens, forzar = forzar || rapido, registro = registro)
    .dl_message("corrida escrita en %s%s", run$dir, if (rapido) " (prueba: no sirve para publicar)" else "")
    run
  })
}

# Dice qué corrida se tomó de cada subtipo de una suma (`corridas`: sus carpetas, en el orden de `subtipos`) y, en una
# suma de producción, qué subtipos entran con una corrida escrita con forzar = TRUE (validacion.gates.force de su
# manifiesto); las de prueba se escriben todas así. Devuelve esos subtipos (ninguno en una suma de prueba), que el
# manifiesto de la suma declara entre sus limitaciones.
.dl_decir_corridas_suma <- function(subtipos, corridas, rapido) {
  for (i in seq_along(corridas)) .dl_message("%d: %s", subtipos[i], basename(corridas[i]))
  forzadas <- vapply(corridas, function(d) isTRUE(.dl_leer_manifest(d)$validacion$gates$force), NA)
  if (rapido || !any(forzadas)) return(invisible(integer()))
  .dl_message(paste0("las corridas de los subtipos %s se escribieron con forzar = TRUE (sin exigir la ",
                     "convergencia): la suma las toma igual"), paste(subtipos[forzadas], collapse = ", "))
  invisible(as.integer(subtipos[forzadas]))
}

# Lo que dl_sumar_hijas() exige de la corrida de cada subtipo de la suma de la causa `causa` (`corridas`: sus
# carpetas, en el orden de `subtipos`), en palabras del proyecto: que sea de una causa que se ajusta (una suma, con
# causa.agregacion en su manifiesto, no es subtipo de otra suma) y que declare a `causa` como su causa padre.
.dl_exigir_subtipos_de <- function(subtipos, corridas, causa) {
  manifiestos <- lapply(corridas, .dl_leer_manifest)
  de <- function(k) sprintf("la(s) corrida(s) %s (subtipo(s) %s)", paste(basename(corridas[k]), collapse = ", "),
                            paste(subtipos[k], collapse = ", "))
  sumas <- which(vapply(manifiestos, function(m) !is.null(m$causa$agregacion), NA))
  if (length(sumas))
    .dl_stop(paste0("%s es/son la suma de otras corridas: una suma no puede ser subtipo de otra suma. Los subtipos ",
                    "de la causa %d deben ser causas que se ajustan: declara en su clave `subtipos`, en lugar de ",
                    "esa suma, los subtipos que ella suma"), de(sumas), causa)
  padres <- vapply(manifiestos, function(m) as.integer(m$causa$extraction_cause_id %||% NA_integer_), 0L)
  otro <- which(!padres %in% causa)
  if (length(otro))
    .dl_stop(paste0("%s no declara(n) a la causa %d como su causa padre: el subtipo se corre desde un proyecto que ",
                    "tenga la configuraci\u00f3n de la causa %d, con \u00e9l en `subtipos`, o con subtipo_de: %d en ",
                    "su propia configuraci\u00f3n"), de(otro), causa, causa, causa)
}

# La corrida de una causa que es la suma de sus subtipos (`p`, su proyecto leído para el año que se estima): busca en
# `carpeta_salida` la corrida más reciente de ese año de cada subtipo que entra (.dl_corridas_de_subtipos: las de
# prueba con `rapido`, las de producción sin él) y las suma con dl_sumar_hijas(), con el nombre de la causa y los
# subtipos omitidos de la configuración y las rutas del proyecto. Antes dice cuáles tomó y comprueba, en palabras del
# proyecto, lo que la suma exige de cada corrida (.dl_exigir_subtipos_de). Los subtipos de una suma de producción
# escritos con forzar = TRUE quedan entre las limitaciones de su manifiesto.
.dl_correr_suma <- function(p, carpeta_salida, rapido, registro, nombre) {
  cfg <- p$configuracion
  s <- cfg$origen$configuracion
  anio <- .dl_anio_ajuste(cfg)
  .dl_message("causa %d: %s, con sus corridas %s de %d en %s", cfg$cause_id, .dl_texto_suma(s),
              if (rapido) "de prueba" else "de producci\u00f3n", anio, .dl_dir_corrida(carpeta_salida))
  subtipos <- .dl_subtipos_suma(s)$entran
  corridas <- .dl_corridas_de_subtipos(carpeta_salida, subtipos, anio, prueba = rapido)
  forzadas <- .dl_decir_corridas_suma(subtipos, corridas, rapido)
  .dl_exigir_subtipos_de(subtipos, corridas, cfg$cause_id)
  run <- .dl_sumar_corridas(corridas, causa = cfg$cause_id, nombre = nombre, carpeta = carpeta_salida,
                            nombre_causa = cfg$origen$nombre, registro = registro, rutas = p$rutas,
                            omitidas = cfg$suma$omitidas, forzadas = forzadas)
  .dl_message("corrida escrita en %s%s", run$dir, if (rapido) " (prueba: no sirve para publicar)" else "")
  run
}
