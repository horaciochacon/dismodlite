# Validación de una tabla contra el esquema (dl_validar_tabla) y comprobaciones de los argumentos de las funciones
# exportadas. Cada problema de una tabla se reporta con tabla.columna o con el nombre de la regla (R/reglas.R); los
# tipos de columna del esquema están en R/esquema.R (.DL_TIPOS_ESQUEMA).

#' Validar una tabla contra el esquema
#'
#' Comprueba columnas, tipos, valores permitidos, clave única y las reglas que el esquema declara para la tabla.
#' Los problemas se reportan todos juntos, cada uno con `tabla.columna`.
#'
#' @details
#' [dl_insumos()] valida así cada tabla de los insumos, con el contexto completo. Llamada directamente, sirve para
#' revisar una tabla del formato completo antes de armar los insumos. Las reglas que cruzan tablas (por ejemplo, que
#' la población nacional sea la suma de la subnacional) usan la información de `contexto`: sin ella, algunas no se
#' comprueban, otras lo reportan como un problema y la de las ubicaciones supone las de la versión 0.2.2 (la
#' ubicación nacional 123 y códigos subnacionales de dos dígitos). En el formato simple, [dl_revisar_proyecto()]
#' revisa todo el proyecto en palabras de sus archivos.
#'
#' @param datos Tabla (data.frame o data.table) a validar; se convierte a data.table por referencia.
#' @param tabla Nombre de la tabla en el esquema (por ejemplo `"datos"`).
#' @param esquema Esquema devuelto por [dl_esquema()].
#' @param contexto Lista con la información que usan las reglas que cruzan tablas (población, betas, ...).
#' @return `datos`, invisible, si la tabla es válida; si no, un error con la lista de problemas.
#' @seealso [dl_esquema()], [dl_esquema_tabla()] y [dl_revisar_proyecto()].
#' @family avanzado
#' @examples
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' # la tabla de datos de los insumos es válida: sin error (devuelve la tabla, invisible)
#' dl_validar_tabla(b$datos, "datos")
#' # un dato marcado para excluir sin su motivo no lo es
#' malos <- b$datos[1:3]
#' malos$outlier[1] <- TRUE
#' try(dl_validar_tabla(malos, "datos"))
#' @export
dl_validar_tabla <- function(datos, tabla, esquema = dl_esquema(), contexto = list()) {
  if (!is.data.frame(datos))
    .dl_stop("`datos` debe ser una tabla (data.frame o data.table); es %s", .dl_describir_objeto(datos))
  .dl_exigir_clase(esquema, "dl_schema", "esquema", "dl_esquema()")
  td <- dl_esquema_tabla(esquema, tabla)
  data.table::setDT(datos)
  probs <- character()
  p <- function(fmt, ...) probs <<- c(probs, sprintf(fmt, ...))
  extra <- setdiff(names(datos), names(td$columnas))
  for (cn in extra) p("%s.%s: no est\u00e1 en el esquema", tabla, cn)
  for (cn in names(td$columnas)) {
    cd <- td$columnas[[cn]]
    tipo <- .dl_tipo_columna(cd)
    opcional <- !is.character(cd) && isTRUE(cd$opcional)
    # Una columna opcional puede faltar entera (una tabla anterior a la columna), no solo traer NA.
    if (!cn %in% names(datos)) { if (!opcional) p("%s.%s: falta", tabla, cn); next }
    v <- datos[[cn]]
    if (!opcional && anyNA(v)) p("%s.%s: NA en columna obligatoria", tabla, cn)
    if (tipo == "enum") {
      vals <- .dl_valores_enum(esquema, cd)
      malos <- setdiff(unique(v[!is.na(v)]), vals)
      if (length(malos))
        p("%s.%s: valor(es) no admitido(s) (%s); los admitidos son: %s", tabla, cn, paste(malos, collapse = ", "),
          paste(vals, collapse = ", "))
    } else if (!.DL_TIPOS_ESQUEMA[[tipo]]$es(v))
      p("%s.%s: tipo esperado %s (%s), pero la columna es %s", tabla, cn, tipo, .DL_TIPOS_ESQUEMA[[tipo]]$prosa,
        class(v)[1L])
  }
  if (!length(probs)) {
    if (nrow(datos) && anyDuplicated(datos, by = intersect(td$clave, names(datos))))
      p("%s: clave duplicada (%s)", tabla, paste(td$clave, collapse = ", "))
    if (!is.null(td$unicidad_semantica) && nrow(datos) && anyDuplicated(datos, by = td$unicidad_semantica))
      p("%s: unicidad sem\u00e1ntica violada (%s)", tabla, paste(td$unicidad_semantica, collapse = ", "))
    for (rg in td$reglas %||% character()) {
      f <- get0(rg, envir = .dl_reglas)
      probs <- c(probs, if (is.null(f)) sprintf("%s: regla no implementada \u00ab%s\u00bb", tabla, rg)
                        else f(datos, tabla, esquema, contexto))
    }
  }
  if (length(probs))
    .dl_stop("la tabla %s tiene %d problema(s):\n%s", tabla, length(probs), paste0("  - ", probs, collapse = "\n"),
             campos = list(problemas = probs, tabla = tabla))
  invisible(datos)
}

# ---- Errores, avisos y notas para el usuario ----
# Todo mensaje del paquete sale por .dl_stop() (error), .dl_warn() (aviso) o .dl_message() (nota), con la función que
# llamó el usuario delante: «dl_insumos(): falta ...», o «dismodlite: ...» si no hay ninguna. `fmt` va a sprintf()
# con los argumentos de `...`; sin argumentos, el texto va tal cual. La función se busca en la pila solo al emitir
# el mensaje (.dl_funcion_usuario()). Con un nombre anterior (dl_fit()), el mensaje cita los argumentos de la función
# nueva y lo dice en una última línea (.DL_NOMBRES_ANTERIORES, R/nombres_anteriores.R). El error es una condición de
# clase `dl_error` (el aviso, `dl_warning`; la nota, `dl_message`) con dos campos además del mensaje: `detalle` (el
# mensaje sin la función ni esa línea) y `funcion` (su nombre, o NULL); `clase` agrega clases propias delante y
# `campos`, otros campos: un error que es una lista de problemas lleva `problemas` (y `tabla`, la de un contrato), que
# dl_revisar_proyecto() lee sin tener que interpretar el texto. Con un proyecto simple (.dl_en_simple()), el detalle y
# los problemas van en sus palabras (.dl_texto_simple()).

# Estado de la sesión que leen los mensajes: `simple`, si salen en palabras del formato simple (.dl_en_simple()).
.dl_estado <- new.env(parent = emptyenv())

.dl_condicion <- function(tipo, fmt, args, clase = NULL, campos = NULL) {
  detalle <- paste(if (length(args)) do.call(sprintf, c(list(fmt), args)) else fmt, collapse = "")
  if (isTRUE(.dl_estado$simple)) {
    detalle <- .dl_texto_simple(detalle)
    if (!is.null(campos$problemas)) campos$problemas <- .dl_texto_simple(campos$problemas)
  }
  funcion <- .dl_funcion_usuario()
  mensaje <- paste(if (is.null(funcion)) "dismodlite:" else paste0(funcion, "():"), detalle)
  nueva <- .DL_NOMBRES_ANTERIORES[funcion %||% ""]                  # NA si no es un nombre anterior
  if (!is.na(nueva))
    mensaje <- sprintf(paste0("%s\n(%s() es el nombre anterior de %s(): el mensaje usa los argumentos de %s(); ",
                              "ver ?dl_nombres_anteriores)"), mensaje, funcion, nueva, nueva)
  structure(c(list(message = if (tipo == "message") paste0(mensaje, "\n") else mensaje, call = NULL,
                   detalle = detalle, funcion = funcion), campos),
            class = c(clase, paste0("dl_", tipo), tipo, "condition"))
}
.dl_stop <- function(fmt, ..., clase = NULL, campos = NULL)
  stop(.dl_condicion("error", fmt, list(...), clase, campos))
.dl_warn <- function(fmt, ..., clase = NULL) warning(.dl_condicion("warning", fmt, list(...), clase))
# Con clase dl_mensaje_revision, dl_revisar_proyecto() lo muestra como aviso (los demás mensajes los calla).
.dl_message <- function(fmt, ..., clase = NULL) message(.dl_condicion("message", fmt, list(...), clase))

# Mensaje de un error sin la función delante: el `detalle` de un error del paquete, o el mensaje de cualquier otro.
.dl_detalle <- function(e) if (inherits(e, "dl_error")) e$detalle else conditionMessage(e)

# Función del paquete que llamó el usuario, para encabezar los mensajes: la más interna de la pila que es una función
# exportada y que no llamó otra función del paquete. Así un error que sale de una función interna nombra la función
# que el usuario escribió, también
#   - en llamadas anidadas o con |>: el argumento que es otra llamada se evalúa en el entorno del usuario, así que
#     dl_ajustar(dl_insumos(cfg, rutas)) nombra dl_insumos() si el error es de los insumos;
#   - con una variable (f <- dl_insumos; f(...)) o con do.call(): se reconoce la función, no el nombre escrito;
#   - con un nombre anterior (dl_bundle()): es la función exportada que llamó el usuario, aunque ella llame a la nueva;
# y nunca nombra una función propia del usuario (aunque su nombre empiece con «dl_»). Las funciones de otros paquetes
# que están en medio (lapply, do.call, tryCatch, mclapply...) no cuentan: se mira quién las llamó. NULL si no hay
# ninguna.
.dl_funcion_usuario <- function() {
  ns <- environment(.dl_funcion_usuario)
  n <- sys.nframe() - 1L                                     # sin el marco de esta función
  if (n < 1L) return(NULL)
  marcos <- sys.frames()
  padres <- sys.parents()
  exportadas <- getNamespaceExports(ns)
  # TRUE si f es del paquete: definida en el espacio de nombres o dentro de una función del paquete que sigue activa
  del_paquete <- function(f, profundidad = 0L) {
    if (!is.function(f) || is.primitive(f)) return(FALSE)
    e <- environment(f)
    if (identical(e, ns)) return(TRUE)
    k <- which(vapply(marcos[seq_len(n)], identical, logical(1), e))
    length(k) > 0L && profundidad < 50L && del_paquete(sys.function(k[1L]), profundidad + 1L)
  }
  # Nombre exportado de la función del marco i, o NULL
  nombre_exportado <- function(i) {
    f <- sys.function(i)
    if (!identical(environment(f), ns)) return(NULL)
    simbolo <- sys.call(i)[[1L]]
    if (is.call(simbolo) && length(simbolo) == 3L && as.character(simbolo[[1L]])[1L] %in% c("::", ":::"))
      simbolo <- simbolo[[3L]]
    if (is.symbol(simbolo) && as.character(simbolo) %in% exportadas &&
        identical(get(as.character(simbolo), envir = ns), f)) return(as.character(simbolo))
    for (nm in exportadas) if (identical(get(nm, envir = ns), f)) return(nm)
    NULL
  }
  # TRUE si la función del marco i la llamó el usuario: subiendo por quien la llamó, saltando funciones de otros
  # paquetes, se llega al entorno global, a código evaluado fuera de una función o a una función que no es del paquete
  llamada_por_usuario <- function(i) {
    p <- padres[i]
    repeat {
      if (p < 1L || p >= i) return(TRUE)
      f <- sys.function(p)
      if (del_paquete(f)) return(FALSE)
      entorno <- if (is.primitive(f)) baseenv() else topenv(environment(f))
      if (!isNamespace(entorno) && !identical(entorno, baseenv())) return(TRUE)
      if (identical(entorno, ns)) return(TRUE)                # p. ej. código de prueba evaluado bajo el paquete
      i <- p; p <- padres[p]
    }
  }
  for (i in rev(seq_len(n))) {
    nm <- nombre_exportado(i)
    if (!is.null(nm) && llamada_por_usuario(i)) return(nm)
  }
  NULL
}

# ---- Comprobaciones de los argumentos de las funciones exportadas ----

# Cómo se describe en un mensaje lo que se recibió en un argumento. El orden importa: una cascada también es un ajuste.
.DL_DESCRIPCION_CLASES <- c(
  dl_config = "una configuraci\u00f3n (de dl_configuracion())",
  dl_proyecto = "un proyecto (de dl_proyecto())",
  dl_paths = "un objeto de rutas (de dl_rutas())",
  dl_bundle = "un objeto de insumos (de dl_insumos())",
  dl_mcmc_opts = "un objeto de opciones MCMC (de dl_opciones_mcmc())",
  dl_cascade = "una cascada (de dl_cascada())",
  dl_fit = "un ajuste (de dl_ajustar())",
  dl_yld = "un resultado de dl_avd()",
  dl_resumen = "un resumen (de dl_resumir())",
  dl_run = "una corrida (de dl_exportar_corrida())",
  dl_schema = "un esquema (de dl_esquema())")

.dl_describir_objeto <- function(x) {
  cl <- intersect(class(x), names(.DL_DESCRIPCION_CLASES))
  if (length(cl)) return(.DL_DESCRIPCION_CLASES[[cl[1L]]])
  if (is.null(x)) return("NULL")
  if (is.data.frame(x)) return("una tabla")
  if (is.character(x) && length(x) == 1L) return(sprintf("el texto \"%s\"", x))
  if (is.numeric(x) && length(x) == 1L) return(sprintf("el n\u00famero %s", format(x)))
  if (is.list(x)) return("una lista")
  sprintf("un objeto de clase %s", class(x)[1L])
}

# Un solo valor: un número (no NA), un número entero finito, un texto (no NA).
.dl_es_numero1 <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
.dl_es_entero1 <- function(x) .dl_es_numero1(x) && is.finite(x) && x == round(x)
.dl_es_texto1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

# Exige que el argumento `argumento` sea de la clase `clase`; `origen` es la función que produce ese objeto. El
# mensaje dice qué se recibió y, si es otro objeto del paquete, sugiere revisar el orden de los argumentos. Un
# argumento obligatorio que el usuario no pasó también se reporta aquí (missing() ve a través del argumento de la
# función que llama), en vez del «argument ... is missing» de R.
.dl_exigir_clase <- function(x, clase, argumento, origen) {
  if (missing(x)) .dl_stop("falta `%s` (debe venir de %s)", argumento, origen)
  if (inherits(x, clase)) return(invisible(x))
  pista <- if (identical(clase, "dl_bundle") && inherits(x, "dl_config"))
    ": arma los insumos con dl_insumos(configuracion, rutas)"
  else if (any(names(.DL_DESCRIPCION_CLASES) %in% class(x))) ": revisa el orden de los argumentos" else ""
  .dl_stop("`%s` debe venir de %s, pero es %s%s", argumento, origen, .dl_describir_objeto(x), pista)
}

# `x` es TRUE o FALSE (el argumento `argumento`).
.dl_exigir_si_no <- function(x, argumento) {
  if (isTRUE(x) || isFALSE(x)) return(invisible(x))
  .dl_stop("`%s` debe ser TRUE o FALSE; es %s", argumento, .dl_describir_objeto(x))
}

# `carpeta` es la ruta de una carpeta que existe (la de un proyecto que se lee).
.dl_exigir_carpeta_existente <- function(carpeta, que = "la carpeta del proyecto") {
  if (missing(carpeta)) .dl_stop("falta `carpeta` (%s)", que)
  if (!.dl_es_texto1(carpeta) || !dir.exists(carpeta))
    .dl_stop("`carpeta` debe ser la ruta de una carpeta que existe; es %s", .dl_describir_objeto(carpeta))
  invisible(carpeta)
}

# `causa`: un cause_id, un número entero positivo o su texto («9100»). Devuelve el entero.
.dl_exigir_causa <- function(causa) {
  if (missing(causa)) .dl_stop("falta `causa` (el cause_id, por ejemplo 9100)")
  if (.dl_es_texto1(causa) && grepl("^[0-9]+$", causa)) causa <- as.numeric(causa)
  if (!.dl_es_entero1(causa) || causa < 1 || causa > .Machine$integer.max)
    .dl_stop("`causa` debe ser un cause_id entero (por ejemplo 9100); es %s", .dl_describir_objeto(causa))
  as.integer(causa)
}

# `x` (un ajuste, una cascada, un resumen...) se hizo con estos `insumos`: su hash es el de los insumos.
.dl_exigir_mismos_insumos <- function(x, insumos, argumento) {
  if (!identical(x$bundle_hash, insumos$hash))
    .dl_stop("`%s` se hizo con otros insumos: pasa las piezas de una misma corrida, con los mismos insumos", argumento)
  invisible(x)
}

# La cascada es un dl_cascade de los mismos insumos.
.dl_chequear_cascada <- function(cascada, insumos) {
  .dl_exigir_clase(cascada, "dl_cascade", "cascada", "dl_cascada()")
  .dl_exigir_mismos_insumos(cascada, insumos, "cascada")
}

# Semilla obligatoria: un número entero con |semilla| <= maximo. El máximo por defecto deja margen para los
# desplazamientos pequeños que se le suman (el sexo, el salto de las betas) antes de set.seed(), que exige un entero
# de R. `argumento`: su nombre en la función del usuario (`seed` en dl_mh() y dl_mh_cadenas()).
.dl_exigir_semilla <- function(semilla, argumento = "semilla", maximo = .Machine$integer.max - 1000) {
  nombre <- if (argumento == "semilla") "`semilla`" else sprintf("`%s` (semilla)", argumento)
  if (missing(semilla))
    .dl_stop("falta %s: es obligatoria y no tiene valor por defecto; por ejemplo %s = 1", nombre, argumento)
  if (!.dl_es_entero1(semilla))
    .dl_stop("%s debe ser un n\u00famero entero, por ejemplo %s = 1; es %s",
             nombre, argumento, .dl_describir_objeto(semilla))
  if (abs(semilla) > maximo)
    .dl_stop("%s debe estar entre -%s y %s; es %s",
             nombre, format(maximo), format(maximo), .dl_describir_objeto(semilla))
  invisible(semilla)
}

# Dominio de cada parámetro que recorre la rejilla de dl_sensibilidad(), el mismo que exige dl_configuracion() a
# anchor.lambda, anchor.rho_edad, cascada.kappa y emr_prior.fraccion_aguda: el único lugar donde se escribe.
.DL_DOMINIO_REJILLA <- list(
  lambda = list(texto = "(0, 1]", dentro = function(v) v > 0 & v <= 1),
  rho = list(texto = "[0, 1)", dentro = function(v) v >= 0 & v < 1),
  kappa = list(texto = "(0, 1]", dentro = function(v) v > 0 & v <= 1),
  fraccion_aguda = list(texto = "[0, 1)", dentro = function(v) v >= 0 & v < 1))

# Exige que `valores` (un vector o una lista de números, como en el YAML) tenga uno o más números en el dominio del
# eje `eje` de la rejilla. `argumento` es como se nombra en el mensaje.
.dl_exigir_rejilla <- function(valores, eje, argumento) {
  v <- unlist(valores)
  dominio <- .DL_DOMINIO_REJILLA[[eje]]
  if (is.numeric(v) && length(v) && !anyNA(v) && all(dominio$dentro(v))) return(invisible(valores))
  recibido <- if (is.numeric(v) && length(v)) paste(v, collapse = ", ") else .dl_describir_objeto(valores)
  .dl_stop("`%s` debe tener uno o m\u00e1s n\u00fameros en %s; es %s", argumento, dominio$texto, recibido)
}

# Nombre corto de una corrida: forma parte del identificador AAAA-MM-DD_<nombre>_v<n> que exige el contrato.
.dl_exigir_nombre <- function(nombre) {
  if (.dl_es_texto1(nombre) && grepl("^[a-z0-9-]+$", nombre)) return(invisible(nombre))
  .dl_stop(paste0("`nombre` solo admite min\u00fasculas sin tildes, n\u00fameros y guiones (por ejemplo ",
                  "\"acs-nacional\"); es %s"), .dl_describir_objeto(nombre))
}

# `nivel` del intervalo de incertidumbre: un número en (0, 1). Con `exportable`, además el que fija el contrato
# estimates/v1 (ui_level) para escribir una corrida: se comprueba al entrar, antes de leer simulaciones o escribir.
.dl_exigir_nivel <- function(nivel, exportable = FALSE) {
  if (!.dl_es_numero1(nivel) || nivel <= 0 || nivel >= 1)
    .dl_stop("`nivel` debe ser un n\u00famero entre 0 y 1 (0.95 para escribir una corrida); es %s",
             .dl_describir_objeto(nivel))
  if (!exportable) return(invisible(nivel))
  contrato <- .dl_schema_estimates()$ui_level
  if (nivel != contrato)
    .dl_stop(paste0("`nivel` debe ser %s para escribir la corrida (el contrato estimates/v1 fija ese nivel del ",
                    "intervalo); es %s"), format(contrato), .dl_describir_objeto(nivel))
  invisible(nivel)
}

# `carpeta` donde se escribe (`destino` la describe en el mensaje): la ruta de una carpeta, un texto no vacío.
# `argumento`: su nombre en la función del usuario.
.dl_exigir_carpeta <- function(carpeta, destino = "la carpeta donde se escriben las corridas", argumento = "carpeta") {
  if (missing(carpeta) || identical(carpeta, "")) .dl_stop("falta `%s` (%s)", argumento, destino)
  if (!.dl_es_texto1(carpeta))
    .dl_stop("`%s` debe ser la ruta de una carpeta (%s), un texto no vac\u00edo; es %s",
             argumento, destino, .dl_describir_objeto(carpeta))
  invisible(carpeta)
}

# Registro de corridas: con registrar = TRUE, `registro` debe ser un archivo YAML que ya existe. Se comprueba antes
# de escribir nada, para no dejar una corrida escrita y sin registrar.
.dl_exigir_registro <- function(registrar, registro, argumento = "registro") {
  if (!isTRUE(registrar)) return(invisible())
  if (is.null(registro))
    .dl_stop("registrar = TRUE exige `%s` (archivo YAML del registro de corridas)", argumento)
  if (!.dl_es_texto1(registro) || !file.exists(registro) || dir.exists(registro))
    .dl_stop(paste0("no existe el archivo del registro de corridas (`%s`): %s. ",
                    "Cr\u00e9alo con la l\u00ednea \u00abdatasets: []\u00bb o no pases `%s`"), argumento,
             format(registro), argumento)
  invisible()
}
