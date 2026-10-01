# Herramientas para la carpeta de un proyecto (R/proyecto.R y R/formato_simple.R): dl_nuevo_proyecto() la crea en el
# formato simple (la configuración comentada y las plantillas, desde las dos tablas de inst/referencia);
# dl_revisar_proyecto() la revisa con los lectores y validadores de siempre, sin detenerse en el primer problema; y
# dl_correr() hace una corrida en una llamada. Su única comprobación propia es un aviso de la revisión: la mortalidad de
# datos.csv que parece una tasa por 100 000 (.dl_aviso_unidades).

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
    "# Configuraci\u00f3n de una causa en el formato simple de dismodlite (la escribi\u00f3 dl_nuevo_proyecto()).",
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

# LEEME.md de un proyecto nuevo: los pasos, con las direcciones de GBD Results y del GHDx del contrato estimates/v1
# (inst/schema) y las llamadas para revisar y correr el proyecto.
.dl_plantilla_leeme <- function(carpeta, causa, archivo_config) {
  url <- .dl_schema_estimates()$sources
  ruta <- gsub("\\", "/", carpeta, fixed = TRUE)
  c(sprintf("# Proyecto de dismodlite: causa %d", causa), "",
    sprintf("1. Completa `%s` (las claves: `?dl_configuracion`).", archivo_config),
    sprintf("2. Pon en `ancla/` las descargas de GBD Results (%s), sin editar.", url$gbd$url),
    sprintf("3. Si declaras covariables, pon en `covariables/` sus descargas del GHDx (%s), sin editar.", url$ghdx$url),
    paste0("4. Llena `poblacion.csv`, `severidad.csv` y, si los usas, `proxies.csv` y `datos.csv` (las columnas y ",
           "sus unidades: `?dl_proyecto`)."),
    sprintf("5. Revisa el proyecto: `dl_revisar_proyecto(\"%s\")`.", ruta),
    sprintf(paste0("6. Pru\u00e9balo con `dl_correr(\"%s\", semilla = 1, rapido = TRUE)`; la corrida final, con ",
                   "`rapido = FALSE`."), ruta))
}

#' Crear la carpeta de un proyecto nuevo
#'
#' Crea la carpeta de un proyecto en el formato simple (ver [dl_proyecto()]) con lo que hay que llenar: la
#' configuración comentada, las carpetas de las descargas, las plantillas de las tablas y un `LEEME.md` con los
#' pasos. No sobrescribe nada: un archivo que ya existe queda como está.
#'
#' @details
#' Lo que se crea en `carpeta`:
#' - `config.yaml` (o `config/<causa>.yaml`, si el proyecto ya tiene la carpeta `config/` de un proyecto con varias
#'   causas): cada clave de la configuración simple con qué es, su símbolo en el modelo y su valor por defecto (la
#'   tabla de [dl_configuracion()]). Las claves obligatorias (`causa`, `anio`, `edad_inicio`) van sin comentar, con el
#'   valor dado o vacías; `nombre`, si se da, también. Las demás van comentadas con su valor por defecto o, si no
#'   tienen un valor fijo, con un ejemplo. Para usar otro valor se quita el `#` de la línea (y el de su
#'   bloque, como `ancla:` para `ancla.peso`).
#' - `ancla/` y `covariables/`, vacías: ahí van las descargas de GBD Results y del GHDx, sin editar.
#' - `poblacion.csv`, `severidad.csv`, `proxies.csv` y `datos.csv`, con el encabezado (las columnas de
#'   [dl_proyecto()]). Las dos últimas son opcionales: con solo el encabezado no se usan.
#' - `LEEME.md`: los pasos, de dónde se descarga cada archivo y cómo revisar y correr el proyecto.
#'
#' Después: llenar los archivos, revisar el proyecto con [dl_revisar_proyecto()] y correrlo con [dl_correr()]. Los
#' valores de `nombre`, `anio` y `edad_inicio` van a la configuración tal como se dan: la revisión dice si alguno no
#' vale.
#'
#' Un proyecto de una sola causa tiene `config.yaml`. Para agregar otra causa, crea la carpeta `config/`, mueve ahí
#' la configuración como `config/<causa>.yaml` y vuelve a llamar a `dl_nuevo_proyecto()` con la causa nueva: escribe
#' `config/<causa nueva>.yaml` y deja lo demás como está. Agrega a `ancla/` la descarga de la causa nueva antes de
#' revisar el proyecto.
#'
#' @param carpeta Carpeta del proyecto; se crea si no existe.
#' @param causa Identificador de la causa: un número entero (el `cause_id` de GBD si la causa existe en GBD).
#' @param nombre Nombre de la causa (opcional; sin él, la configuración toma el de la descarga del ancla).
#' @param anio Año que se estima (opcional aquí; la configuración lo exige).
#' @param edad_inicio Primera edad del modelo, en años (opcional aquí; la configuración la exige).
#' @return La ruta de `carpeta`, invisible. Un mensaje lista lo que se creó, lo que ya existía y los pasos
#'   siguientes.
#' @seealso [dl_proyecto()] (los archivos y sus columnas), [dl_configuracion()] (las claves de la configuración),
#'   [dl_revisar_proyecto()] y [dl_correr()] (los pasos siguientes) y [dl_ejemplo()] (un proyecto lleno, para
#'   comparar).
#' @family proyecto
#' @examples
#' carpeta <- file.path(tempdir(), "proyecto_nuevo")
#' dl_nuevo_proyecto(carpeta, causa = 1234, nombre = "Enfermedad de ejemplo",
#'                   anio = 2023, edad_inicio = 30)
#' list.files(carpeta, recursive = TRUE, include.dirs = TRUE)
#' cat(readLines(file.path(carpeta, "config.yaml"), n = 20, encoding = "UTF-8"), sep = "\n")
#' # la revisión dice qué falta: las descargas del ancla, la población y la severidad
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
  # las tablas con sus columnas obligatorias y las opcionales más usadas: una tabla opcional con solo el encabezado
  # es como si no estuviera
  plantillas <- lapply(stats::setNames(nm = grep("[.]csv$", .dl_archivos_simple(), value = TRUE)), function(f)
    paste(.dl_columnas_simple(f, c("obligatoria", "plantilla")), collapse = ","))
  plantillas <- plantillas[nzchar(plantillas)]
  archivos <- c(stats::setNames(list(.dl_plantilla_config(dadas)), archivo_config),
                list(LEEME.md = .dl_plantilla_leeme(carpeta, causa, archivo_config)), plantillas)
  for (d in sub("/$", "", grep("/$", .dl_archivos_simple(), value = TRUE)))
    dir.create(file.path(carpeta, d), recursive = TRUE, showWarnings = FALSE)
  nuevos <- !file.exists(file.path(carpeta, names(archivos)))
  for (a in names(archivos)[nuevos]) {
    dir.create(dirname(file.path(carpeta, a)), recursive = TRUE, showWarnings = FALSE)
    writeLines(enc2utf8(archivos[[a]]), file.path(carpeta, a), useBytes = TRUE)
  }
  .dl_message(paste0("proyecto de la causa %d en %s\n  archivos nuevos: %s\n  ya exist\u00edan (no se tocaron): %s\n",
                     "  siguientes pasos: lee LEEME.md, completa %s, pon las descargas en ancla/ (y en ",
                     "covariables/), llena las tablas y revisa el proyecto con dl_revisar_proyecto()"),
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
# configuración o nombra una de sus claves (ancla.peso, datos_en_ajuste...), las columnas de su tabla si es de una
# tabla del proyecto con plantilla, si no ?dl_proyecto; ninguna si está en orden o se omitió.
.dl_fila_revision <- function(causa, paso, estado, detalle) {
  columnas <- .dl_columnas_simple(paso, c("obligatoria", "plantilla"))
  claves <- setdiff(grep("^[a-z_]+[._][a-z_.]+$", .dl_claves_simple()$clave, value = TRUE), .dl_columnas_ref()$columna)
  clave <- grepl(sprintf("(?<![\\w.])(%s)(?!\\w)", paste(gsub(".", "[.]", claves, fixed = TRUE), collapse = "|")),
                 detalle, perl = TRUE)
  sugerencia <- if (estado %in% c("ok", "omitido")) ""
                else if (paso == "configuraci\u00f3n" || clave) "ver ?dl_configuracion"
                else if (endsWith(paso, ".csv") && length(columnas))
                  sprintf("columnas %s (ver ?dl_proyecto)", paste(columnas, collapse = ", "))
                else "ver ?dl_proyecto"
  data.frame(causa = causa, paso = paso, estado = estado, detalle = detalle, sugerencia = sugerencia,
             stringsAsFactors = FALSE)
}

# Aviso de los valores de mortalidad de la causa de `cfg` en `datos` (la tabla traducida de datos.csv) que parecen
# tasas por 100 000 y no por persona-año, o NULL: mayores que 1, o más de 1000 veces la mortalidad del ancla (`ancla`:
# muertes, Rate por 100 000, en la ubicación nacional) de la misma causa, sexo, grupo de edad y año. Una tasa por
# 100 000 es 100 000 veces la de persona-año: con el margen de 1000, un dato por persona-año pasa aunque sea hasta 1000
# veces el del ancla, y uno por 100 000 se detecta salvo que su valor verdadero sea menos de la centésima parte del
# ancla. Cita cada valor por su fila (fila_<n>, como los demás mensajes de datos.csv).
.dl_aviso_unidades <- function(datos, ancla, cfg) {
  med <- .dl_medida("csmr")
  m <- datos[datos$tipo_dato == "csmr" & !is.na(datos$val) & datos$cause_id == as.character(cfg$cause_id)]
  a <- ancla[ancla$measure_id == med$measure_id_gbd & ancla$metric_name == med$metric_std &
               ancla$location_id == .dl_loc_ancla(cfg)]
  clave <- function(d, anio) paste(d$cause_id, d$sex_id, d$age_group_id, anio)
  v <- as.numeric(m$val)
  ref <- as.numeric(a$val[match(clave(m, m$year_start), clave(a, a$year))]) / med$escala_std
  k <- which(v > 1 | (!is.na(ref) & v > 1000 * ref))
  if (!length(k)) return(NULL)
  sprintf(paste0("%d valor(es) de mortalidad parecen tasas por 100 000, no por persona-a\u00f1o ",
                 "(%s; por ejemplo %s%s): divide valor y error_estandar por 100 000"), length(k),
          paste(utils::head(m$dato_id[k], 5L), collapse = ", "), format(v[k[1L]]),
          if (is.na(ref[k[1L]])) "" else sprintf(", donde el ancla da %s", format(signif(ref[k[1L]], 3L))))
}

# Revisión de una causa del proyecto `carpeta` (`cf`: su fila de .dl_configs_proyecto()): una fila por comprobación
# (causa, paso, estado, detalle, sugerencia). En orden: la configuración (dl_configuracion()); en el formato simple,
# los pasos de la traducción de las tablas (.dl_traducir_tablas, sin detenerse; sin la configuración, solo la lectura
# de la población) y el aviso de las unidades; si nada falló, los insumos (dl_insumos(), con esa misma traducción) y la
# severidad que dl_correr() necesita. En el formato simple, un problema que lleva su tabla (`tabla` del error) va al
# paso del archivo de donde sale, sin el archivo delante; una configuración que no se tradujo por un problema de otro
# archivo queda omitida.
.dl_revisar_causa <- function(carpeta, cf) {
  filas <- list()
  anotar <- function(paso, estado, detalle)
    filas[[length(filas) + 1L]] <<- .dl_fila_revision(cf$causa, paso, estado, detalle)
  archivo <- attr(.dl_claves_simple(), "tablas")          # tabla del contrato -> archivo del formato simple
  # la línea en orden de un archivo del proyecto: las filas que trae el archivo (los pesos de 80+ se calculan)
  leido <- function(paso, f = file.path(carpeta, sub("/$", "", paso)))
    if (file.exists(f)) sprintf("le\u00eddo: %d fila(s)", .dl_filas_csv(f)) else "calculados desde poblacion.csv"
  # evalúa `expr` en el paso `paso` y anota sus avisos, sus problemas y, si no falló, la línea en orden `ok(valor)`
  # (NULL: ninguna); devuelve el valor (NULL si falló)
  revisar <- function(paso, expr, ok = function(v) leido(paso)) {
    r <- .dl_recoger(expr)
    donde <- if (cf$simple && isTRUE(r$tabla %in% names(archivo))) archivo[[r$tabla]] else paso
    if (!donde %in% c(paso, .dl_archivos_simple())) donde <- "configuraci\u00f3n"     # las betas: sus covariables
    for (a in r$avisos) anotar(paso, "aviso", a)
    for (e in r$error) anotar(donde, "error", sub(paste0("^", gsub("([.])", "[.]", donde), ": "), "", e))
    if (length(r$error) && donde != paso && paso == "configuraci\u00f3n")
      anotar(paso, "omitido", sprintf("su traducci\u00f3n espera a %s", donde))
    if (!length(r$error) && !is.null(r$valor) && !is.null(ok)) anotar(paso, "ok", ok(r$valor))
    r$valor
  }
  rel <- if (basename(dirname(cf$archivo)) == "config") file.path("config", basename(cf$archivo))
         else basename(cf$archivo)
  cfg <- revisar("configuraci\u00f3n", dl_configuracion(cf$causa, cf$archivo),
                 function(cfg) sprintf("%s: formato %s", rel, if (cf$simple) "simple" else "completo"))
  x <- NULL
  if (cf$simple && is.null(cfg)) revisar("poblacion.csv", .dl_leer_poblacion_simple(carpeta))
  if (cf$simple && !is.null(cfg)) {
    x <- .dl_traducir_tablas(carpeta, cfg, paso = revisar)
    u <- if (!is.null(x$datos) && !is.null(x$ancla)) .dl_aviso_unidades(x$datos, x$ancla, cfg)
    if (!is.null(u)) anotar("datos.csv", "aviso", u)
  }
  if (any(vapply(filas, function(f) f$estado == "error", NA))) {
    anotar("insumos", "omitido", "no se armaron: primero corrige los errores de arriba")
  } else {
    b <- revisar("insumos", dl_insumos(.dl_proyecto_de(carpeta, cfg, x)),
                 function(b) sprintf("dl_insumos() los arma y los valida (hash %s)", substr(b$hash, 1L, 12L)))
    if (!is.null(b)) revisar("insumos", .dl_en_simple(.dl_exigir_severidad(b$severidad, cf$causa), cf$simple), NULL)
  }
  do.call(rbind, filas)
}

# Imprime la revisión `r` de las causas del proyecto `carpeta`: una línea por comprobación y la sugerencia de las que
# no están en orden; al final, el total.
.dl_imprimir_revision <- function(r, carpeta) {
  s <- .dl_simbolos_revision()
  for (causa in unique(r$causa)) {
    x <- r[r$causa %in% causa, ]
    cat(sprintf("Revisi\u00f3n del proyecto \u00ab%s\u00bb%s\n", basename(carpeta),
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
#' Revisa la carpeta de un proyecto antes de correrlo, sin detenerse en el primer problema: la configuración de
#' cada causa, cada archivo (ancla, población, pesos de 80 años y más, covariables, proxies, datos, severidad) y, al
#' final, los insumos completos. Muestra una línea por comprobación, marcada como en orden, aviso (`!`) o error, con la
#' corrección sugerida. No ajusta nada.
#'
#' @details
#' La revisión usa los mismos lectores y validadores que [dl_proyecto()] y [dl_insumos()], en el mismo orden: lo que
#' pasa la revisión es lo que ellos aceptan. En orden:
#' 1. la configuración, como la lee [dl_configuracion()] (claves, valores admitidos y dominios);
#' 2. en el formato simple, cada archivo, para reportar juntos los problemas de archivos distintos: una columna que
#'    falta, un CSV guardado desde Excel con «;» o coma decimal, un sexo o un grupo de edad que no se reconoce, una
#'    medida, un año o una ubicación que el ancla no trae, la población del año que se estima, una covariable que
#'    falta en las descargas, proxies de otras ubicaciones o de otro año...; sin la configuración, solo la población;
#' 3. si nada de eso falló, los insumos completos, con las reglas que cruzan tablas: por ejemplo, que la población
#'    nacional sea la suma de las subnacionales, que el promedio de los proxies, ponderado por la población, sea el
#'    valor nacional de la covariable (?dl_proyecto da la receta para llevarlos a ese valor) o que los datos locales
#'    tengan valores posibles (una prevalencia entre 0 y 1, casos que no superan la muestra); y la severidad: una
#'    causa sin estados de salud es un error, porque sin ellos no hay AVD y [dl_correr()] no puede escribir la
#'    corrida (los insumos sí se arman).
#'
#' Cada problema va en el paso de su archivo (en el formato simple, también los de las reglas de los insumos), con una
#' sugerencia: [dl_configuracion()] si nombra una clave de la configuración, las columnas de la tabla o [dl_proyecto()].
#' Una línea en orden (`leído: N fila(s)`) dice cuántas filas de datos trae el archivo (o los CSV de la carpeta): una
#' regla de los insumos puede encontrar después un error en ese mismo archivo. Avisa (`!`) de lo que no detiene los
#' insumos pero conviene mirar: los avisos de la lectura y de los insumos (por ejemplo, datos locales en el ajuste con
#' `ancla.peso` 1, un posible doble conteo), las filas de `datos.csv` de la causa que quedan fuera (de otro año, de
#' ambos sexos o por debajo de `edad_inicio`), un `datos_en_ajuste` sin ninguna fila nacional que entre al ajuste y
#' los valores de mortalidad de `datos.csv` que parecen tasas por 100 000 en vez de por persona-año: mayores que 1, o
#' más de 1000 veces la mortalidad del ancla en la misma causa, sexo, grupo de edad y año. Un paso que espera a que se
#' corrija otro (`-`) no cuenta como error ni como aviso: los insumos, si algo falló antes, o la configuración, si su
#' traducción necesita un archivo con problemas (como `ancla/`).
#'
#' En un proyecto en el formato completo se revisan la configuración, los insumos completos y la severidad.
#'
#' @param carpeta Carpeta del proyecto.
#' @param causa Causa que se revisa (`cause_id`); `NULL` (por defecto) revisa todas las causas con configuración.
#' @return Una tabla (data.frame), invisible, con una fila por comprobación: `causa`, `paso` (la configuración, el
#'   archivo o `insumos`), `estado` (`"ok"`, `"aviso"`, `"error"` u `"omitido"`: un paso que espera a que se corrija
#'   otro), `detalle` y `sugerencia` (la corrección; vacía si está en orden o se omitió).
#' @seealso [dl_proyecto()] (los archivos y sus columnas, y la receta para que los proxies cierren en el valor
#'   nacional), [dl_configuracion()] (las claves), [dl_nuevo_proyecto()] y [dl_correr()].
#' @family proyecto
#' @examples
#' r <- dl_revisar_proyecto(dl_ejemplo(), causa = 9100)
#' table(r$estado)
#'
#' # un proyecto con un problema: la población sin la columna `sexo`
#' carpeta <- file.path(tempdir(), "proyecto_con_error")
#' dir.create(file.path(carpeta, "ancla"), recursive = TRUE)
#' invisible(file.copy(dl_ejemplo("ancla", "sintetico_acs_v1.csv"), file.path(carpeta, "ancla")))
#' writeLines(c("causa: 9101", "anio: 2023", "edad_inicio: 30"), file.path(carpeta, "config.yaml"))
#' pob <- read.csv(dl_ejemplo("poblacion.csv"), colClasses = c(location_id = "character"))
#' write.csv(pob[names(pob) != "sexo"], file.path(carpeta, "poblacion.csv"), row.names = FALSE)
#' r <- dl_revisar_proyecto(carpeta)
#' r[r$estado == "error", c("paso", "sugerencia")]
#' unlink(carpeta, recursive = TRUE)
#' @export
dl_revisar_proyecto <- function(carpeta, causa = NULL) {
  .dl_exigir_carpeta_existente(carpeta)
  if (!is.null(causa)) causa <- .dl_exigir_causa(causa)
  cf <- .dl_recoger(.dl_elegir_configs(.dl_configs_proyecto(carpeta), causa, carpeta, varias = TRUE))
  r <- if (length(cf$error)) .dl_fila_revision(causa %||% NA_integer_, "configuraci\u00f3n", "error", cf$error)
       else do.call(rbind, lapply(seq_len(nrow(cf$valor)), function(j) .dl_revisar_causa(carpeta, cf$valor[j])))
  rownames(r) <- NULL
  .dl_imprimir_revision(r, carpeta)
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
#' proyecto simple, los mensajes de todos los pasos citan sus claves y sus archivos (como [dl_insumos()]).
#'
#' @inheritParams dl_ajustar
#' @param proyecto Carpeta del proyecto (formato simple o completo) o un proyecto de [dl_proyecto()].
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
  .dl_en_simple(simple = .dl_es_simple(cfg), {
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
