# El contrato de insumos (ver ?dl_tablas): las tablas que el modelo necesita, con un eje común de dimensiones
# (causa, ubicación, año, sexo y banda de edad) y sus valores. Su única definición es inst/referencia/tablas.csv; de
# ahí salen la validación, las plantillas, la ayuda y los mensajes. Los lectores de fuentes conocidas (GBD Results,
# GHDx, particiones de severidad) están en R/lectores.R; la traducción al formato completo, en
# R/contrato_traduccion.R.

# ---- Constantes ----

# Las tablas del contrato, en el orden en que se documentan.
.DL_TABLAS <- c("ubicaciones", "poblacion", "ancla", "covariables", "betas", "datos", "severidad", "fuentes_gbd",
                "poblacion_detalle", "proxies_crudos", "razones")

# El eje: las dimensiones que comparten las tablas. Una columna del eje ausente en una tabla significa que la tabla
# no varía en esa dimensión (sin causa: todas; sin ubicacion: la nacional; sin anio: todos; sin sexo: ambos; sin
# edades: todas las edades).
.DL_EJE <- c("causa", "ubicacion", "anio", "sexo", "edad_inicio", "edad_fin")

# Fin de la banda de edad abierta («80 y más»): el age_end del catálogo de edades de GBD. Un edad_fin vacío se lee así.
.DL_EDAD_ABIERTA <- 125

# Valores de la columna sexo: el código del contrato (en palabras) de cada forma admitida, sin distinguir mayúsculas.
.DL_SEXOS_CONTRATO <- c(hombres = "hombres", mujeres = "mujeres", ambos = "ambos", "1" = "hombres", "2" = "mujeres",
                        "3" = "ambos", male = "hombres", female = "mujeres", both = "ambos")

# ---- La definición ----

# Las columnas de las tablas del contrato (inst/referencia/tablas.csv), una fila por tabla y columna.
.dl_tablas_ref <- function() .dl_leer_memo(.dl_inst_archivo("referencia", "tablas.csv"), function(p)
  utils::read.csv(p, colClasses = "character", na.strings = character(), encoding = "UTF-8"))

# Columnas de `tabla`: todas, las que exige o las del eje.
.dl_columnas_contrato <- function(tabla, cuales = c("todas", "exigidas", "eje")) {
  cuales <- match.arg(cuales)
  t <- .dl_tablas_ref()
  t <- t[t$tabla == tabla, ]
  switch(cuales, todas = t$columna, exigidas = t$columna[t$exige == "si"], eje = t$columna[t$rol == "eje"])
}

# Nombres canónicos de las columnas de `d` (una tabla de `tabla`): sin espacios ni mayúsculas y con los alias de
# tablas.csv. Las columnas que no son del contrato quedan con su nombre (se ignoran después). Error si un alias y su
# nombre canónico vienen los dos.
.dl_normalizar_nombres <- function(d, tabla) {
  d <- data.table::as.data.table(d)
  data.table::setnames(d, tolower(trimws(names(d))))
  t <- .dl_tablas_ref()
  t <- t[t$tabla == tabla & nzchar(t$alias), ]
  for (i in seq_len(nrow(t))) {
    alias <- intersect(strsplit(t$alias[i], "|", fixed = TRUE)[[1L]], names(d))
    if (!length(alias)) next
    if (t$columna[i] %in% names(d) || length(alias) > 1L)
      .dl_stop("la tabla %s trae a la vez %s: son la misma columna; deja solo %s", tabla,
               paste(c(intersect(t$columna[i], names(d)), alias), collapse = " y "), t$columna[i])
    data.table::setnames(d, alias, t$columna[i])
  }
  d
}

# Sexo en el contrato: «hombres», «mujeres» o «ambos» desde cualquiera de las formas de .DL_SEXOS_CONTRATO; NA si no
# es ninguna.
.dl_sexo_contrato <- function(x) unname(.DL_SEXOS_CONTRATO[tolower(trimws(as.character(x)))])

# ---- Validación de una tabla ----

# Columnas clave (además del eje) de cada tabla: con el eje, identifican una fila.
.DL_CLAVES_TABLA <- list(ancla = "medida", covariables = "covariable", betas = "covariable", severidad = "estado",
                         fuentes_gbd = c("componente", "nid"), proxies_crudos = "covariable")

# Tablas en las que las bandas de edad de un mismo grupo no pueden solaparse.
.DL_TABLAS_SIN_SOLAPE <- c("poblacion", "ancla", "covariables", "severidad", "poblacion_detalle", "proxies_crudos")

# Filas para un mensaje: «fila(s) 2, 5, 7» (como mucho 5). De un archivo, la línea del CSV (el encabezado es la
# 1: la fila i es la línea i + 1); de un data.frame, su número de fila, «fila(s) 1, 4 del data.frame». `df` dice si
# la tabla vino de un data.frame; por defecto, lo que fijó .dl_tabla_contrato() para la tabla que valida.
.dl_contexto_filas <- new.env(parent = emptyenv())
.dl_filas_msg <- function(i, df = isTRUE(.dl_contexto_filas$data_frame)) {
  sprintf("fila(s) %s%s%s", paste(utils::head(i + if (df) 0L else 1L, 5L), collapse = ", "),
          if (length(i) > 5L) sprintf(" y %d m\u00e1s", length(i) - 5L) else "", if (df) " del data.frame" else "")
}

# ¿El origen de una tabla (el texto de sus mensajes) es un data.frame? «argumento `x` (data.frame)».
.dl_origen_df <- function(origen) isTRUE(endsWith(as.character(origen)[1L], "(data.frame)"))

# Convierte las columnas de `d` al tipo de tablas.csv. Devuelve list(d, problemas).
.dl_tipar_tabla <- function(d, tabla) {
  t <- .dl_tablas_ref()
  t <- t[t$tabla == tabla & t$columna %in% names(d), ]
  probs <- character()
  for (i in seq_len(nrow(t))) {
    cn <- t$columna[i]
    x <- d[[cn]]
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) x[!nzchar(trimws(x))] <- NA_character_
    # el texto NA (lo que escribe write.csv() en una celda vacía) es vacío en una columna de números o lógica; en
    # una de texto es un valor (NA puede ser el código de una ubicación)
    if (is.character(x) && t$tipo[i] != "texto") x[trimws(x) == "NA"] <- NA_character_
    y <- switch(t$tipo[i],
                texto = trimws(as.character(x)),
                numero = suppressWarnings(as.numeric(x)),
                entero = { v <- suppressWarnings(as.numeric(x)); ifelse(!is.na(v) & v == round(v), v, NA_real_) },
                logico = as.logical(toupper(trimws(as.character(x)))))
    # una columna exigida no puede tener celdas vac\u00edas (edad_fin vac\u00edo es la banda abierta)
    vacias <- if (t$exige[i] == "si" && cn != "edad_fin") which(is.na(x))
    if (length(vacias))
      probs <- c(probs, sprintf("%s: la columna es obligatoria y hay celdas vac\u00edas (%s)", cn, .dl_filas_msg(vacias)))
    malos <- which(!is.na(x) & is.na(y))
    if (length(malos))
      probs <- c(probs, sprintf("%s: %s no es %s (%s)", cn, paste(utils::head(unique(x[malos]), 3L), collapse = ", "),
                                c(texto = "texto", numero = "un n\u00famero", entero = "un entero",
                                  logico = "true o false")[[t$tipo[i]]], .dl_filas_msg(malos)))
    if (t$tipo[i] == "entero") y <- as.integer(y)
    data.table::set(d, j = cn, value = y)
  }
  list(d = d, problemas = probs)
}

# TRUE donde [inicio, fin) es un grupo de edad de GBD (las bandas de menos de un año no son de años enteros).
.dl_es_banda_gbd <- function(inicio, fin) !is.na(.dl_grupo_edad(inicio, fin))

# Problemas y avisos de la tabla `d` (ya normalizada y tipada) de `tabla`. `sin_maximo`: columnas cuyo máximo de
# tablas.csv no se exige (los límites de la severidad que sale de una partición; ver .dl_severidad_particion).
.dl_problemas_tabla <- function(d, tabla, sin_maximo = character()) {
  t <- .dl_tablas_ref()
  t <- t[t$tabla == tabla, ]
  probs <- character()
  avisos <- character()
  p <- function(...) probs <<- c(probs, sprintf(...))
  faltan <- setdiff(t$columna[t$exige == "si"], names(d))
  if (length(faltan)) p("faltan las columnas %s (trae: %s)", paste(faltan, collapse = ", "),
                        paste(names(d), collapse = ", "))
  if ("sexo" %in% names(d)) {
    s <- .dl_sexo_contrato(d$sexo)
    malos <- which(!is.na(d$sexo) & is.na(s))
    if (length(malos)) p("sexo: %s no es hombres, mujeres ni ambos (%s)", d$sexo[malos[1L]], .dl_filas_msg(malos))
    data.table::set(d, j = "sexo", value = s)
  }
  for (i in which(t$columna %in% names(d))) {
    cn <- t$columna[i]
    x <- d[[cn]]
    if (nzchar(t$valores_alias[i])) {
      va <- strsplit(strsplit(t$valores_alias[i], "|", fixed = TRUE)[[1L]], "=", fixed = TRUE)
      for (par in va) x[x %in% par[1L]] <- par[2L]
      data.table::set(d, j = cn, value = x)
    }
    if (nzchar(t$vocabulario[i])) {
      voc <- strsplit(t$vocabulario[i], "|", fixed = TRUE)[[1L]]
      malos <- which(!is.na(x) & !x %in% voc)
      if (length(malos)) p("%s: %s no es uno de %s (%s)", cn, x[malos[1L]], paste(voc, collapse = ", "),
                           .dl_filas_msg(malos))
    }
    if (nzchar(t$minimo[i]) && any(x < as.numeric(t$minimo[i]), na.rm = TRUE))
      p("%s: hay valores menores que %s (%s)", cn, t$minimo[i], .dl_filas_msg(which(x < as.numeric(t$minimo[i]))))
    if (nzchar(t$maximo[i]) && !cn %in% sin_maximo && any(x > as.numeric(t$maximo[i]), na.rm = TRUE))
      p("%s: hay valores mayores que %s (%s)", cn, t$maximo[i], .dl_filas_msg(which(x > as.numeric(t$maximo[i]))))
  }
  if (all(c("anio_inicio", "anio_fin") %in% names(d))) {
    malos <- which(d$anio_inicio > d$anio_fin)
    if (length(malos)) p("anio_inicio debe ser menor o igual que anio_fin (%s)", .dl_filas_msg(malos))
  }
  claves <- intersect(c(.DL_EJE, .DL_CLAVES_TABLA[[tabla]]), names(d))
  grupo <- setdiff(claves, c("edad_inicio", "edad_fin"))
  if (all(c("edad_inicio", "edad_fin") %in% names(d)) && !length(faltan)) {
    abierta <- is.na(d$edad_fin) & !is.na(d$edad_inicio)
    # filas de la banda de mayor edad_inicio de su grupo (por posición de fila, no en el orden de los grupos)
    ultima <- seq_len(nrow(d)) %in% d[, .I[edad_inicio %in% max(c(-Inf, edad_inicio), na.rm = TRUE)], by = grupo]$V1
    if (any(abierta & !ultima))
      p("edad_fin vac\u00edo (banda abierta) en una banda que no es la \u00faltima de su grupo (%s)",
        .dl_filas_msg(which(abierta & !ultima)))
    data.table::set(d, which(abierta), "edad_fin", .DL_EDAD_ABIERTA)
    malas <- which(d$edad_inicio >= d$edad_fin)
    if (length(malas)) p("edad_inicio debe ser menor que edad_fin (%s)", .dl_filas_msg(malas))
    no_enteras <- which((d$edad_inicio != round(d$edad_inicio) | d$edad_fin != round(d$edad_fin)) &
                          !.dl_es_banda_gbd(d$edad_inicio, d$edad_fin))
    if (length(no_enteras))
      p("las bandas de edad son de a\u00f1os enteros (salvo los grupos de edad de GBD): %s", .dl_filas_msg(no_enteras))
    # un grupo no mezcla filas sin edad (todas las edades) con filas por banda (donde la edad no es obligatoria)
    mezcla <- d[, .I[any(is.na(edad_inicio)) & !all(is.na(edad_inicio))], by = grupo]$V1
    if (length(mezcla) && !"edad_inicio" %in% .dl_columnas_contrato(tabla, "exigidas"))
      p("un mismo grupo (%s) mezcla filas sin edad (todas las edades) con filas por banda de edad (%s)",
        paste(grupo, collapse = ", "), .dl_filas_msg(mezcla))
    if (tabla %in% .DL_TABLAS_SIN_SOLAPE) {
      con_edad <- which(!is.na(d$edad_inicio) & !is.na(d$edad_fin))   # las filas sin edad no se solapan con nada
      x <- d[con_edad]
      orden <- con_edad[x[, .I[order(edad_inicio)], by = grupo]$V1]
      x <- d[orden]
      solapa <- x[, c(FALSE, utils::head(edad_fin, -1L) > utils::tail(edad_inicio, -1L)), by = grupo]$V1
      if (any(solapa)) p("hay bandas de edad que se solapan en un mismo grupo (%s)", .dl_filas_msg(orden[solapa]))
    }
  }
  if (tabla != "datos" && length(claves) && !length(faltan)) {
    rep <- which(duplicated(d[, claves, with = FALSE]))
    if (length(rep)) p("filas repetidas en %s (%s)", paste(claves, collapse = ", "), .dl_filas_msg(rep))
  }
  prev <- if (tabla %in% c("ancla", "datos") && all(c("medida", "valor") %in% names(d)))
    which(d$medida %in% "prevalencia" & d$valor > 1)
  if (length(prev))
    avisos <- c(avisos, sprintf("prevalencia mayor que 1 (%s): \u00bfest\u00e1 por 100 000? El contrato la pide en proporci\u00f3n",
                                .dl_filas_msg(prev)))
  list(d = d, problemas = probs, avisos = avisos)
}

# Tabla del contrato `tabla` validada desde `d` (de `origen`, un texto para los mensajes): nombres, tipos y reglas de
# una tabla sola. Error con todos los problemas juntos (campos: tabla, problemas). `sin_maximo`: columnas sin el
# máximo de tablas.csv (.dl_problemas_tabla).
.dl_tabla_contrato <- function(d, tabla, origen, sin_maximo = character()) {
  previo <- .dl_contexto_filas$data_frame
  .dl_contexto_filas$data_frame <- .dl_origen_df(origen)
  on.exit(.dl_contexto_filas$data_frame <- previo, add = TRUE)
  d <- .dl_normalizar_nombres(d, tabla)
  ti <- .dl_tipar_tabla(d, tabla)
  r <- .dl_problemas_tabla(ti$d, tabla, sin_maximo)
  probs <- c(ti$problemas, r$problemas)
  if (length(probs))
    .dl_stop("la tabla %s tiene %d problema(s):\n%s\n  origen: %s", tabla, length(probs),
             paste0("  - ", probs, collapse = "\n"), origen, campos = list(tabla = tabla, problemas = probs))
  out <- r$d[, intersect(.dl_columnas_contrato(tabla), names(r$d)), with = FALSE]
  data.table::setattr(out, "tabla", tabla)
  data.table::setattr(out, "origen", origen)
  data.table::setattr(out, "avisos", r$avisos)
  data.table::setattr(out, "class", c("dl_tabla", "data.table", "data.frame"))
  out
}

#' Una tabla del contrato de insumos
#'
#' Lee, convierte y valida una tabla del contrato de insumos (ver [dl_tablas]): una ruta a un CSV, una carpeta de CSV
#' o un `data.frame`. Si el archivo es una descarga de GBD Results o del GHDx, la convierte primero con su lector.
#' Sirve para preparar las tablas en R y comprobar que están bien antes de armar el proyecto.
#'
#' @param tabla Nombre de la tabla: una de las de [dl_tablas].
#' @param x Ruta a un CSV, carpeta de CSV o `data.frame` (también un tibble).
#' @param ubicacion_gbd `location_id` de GBD del país, para leer descargas de GBD o del GHDx que traen varias
#'   ubicaciones (opcional).
#' @param metrica_prevalencia Métrica de la prevalencia en una descarga de GBD Results: `"Rate"` (por defecto) o
#'   `"Percent"` (solo para reproducir corridas anteriores a la 1.0.1).
#' @return Un `data.table` con clase `dl_tabla`: las columnas del contrato, con sus tipos, el sexo en palabras y las
#'   bandas abiertas con `edad_fin` 125. Atributos: `tabla`, `origen` y `avisos`.
#' @seealso [dl_tablas] (el contrato), [dl_plantilla()], [dl_proyecto()].
#' @family proyecto
#' @examples
#' pob <- data.frame(ubicacion = "01", anio = 2023, sexo = c("hombres", "mujeres"),
#'                   edad_inicio = 30, edad_fin = 35, poblacion = c(1200, 1300))
#' dl_tabla("poblacion", pob)
#' # una descarga de GBD Results, tal cual
#' dl_tabla("ancla", dl_ejemplo("ancla"))
#' @export
dl_tabla <- function(tabla, x, ubicacion_gbd = NULL, metrica_prevalencia = "Rate") {
  tabla <- .dl_exigir_tabla(tabla)
  origen <- if (is.data.frame(x)) "argumento `x` (data.frame)" else .dl_exigir_ruta_tabla(x)
  d <- .dl_leer_fuente_tabla(x, tabla, list(ubicacion_gbd = ubicacion_gbd, metrica_prevalencia = metrica_prevalencia))
  .dl_tabla_contrato(d, tabla, origen)
}

# `tabla` es el nombre de una tabla del contrato.
.dl_exigir_tabla <- function(tabla) {
  if (!.dl_es_texto1(tabla) || !tabla %in% .DL_TABLAS)
    .dl_stop("`tabla` debe ser una de las tablas del contrato: %s", paste(.DL_TABLAS, collapse = ", "))
  tabla
}

# `x` es la ruta de un archivo o carpeta que existe.
.dl_exigir_ruta_tabla <- function(x) {
  if (!.dl_es_texto1(x) || !file.exists(x))
    .dl_stop("`x` debe ser un data.frame o la ruta de un CSV o de una carpeta que exista; es %s",
             .dl_describir_objeto(x))
  x
}

#' Plantilla de una tabla del contrato
#'
#' La tabla vacía con sus columnas (las que exige y las opcionales más usadas) y una fila de ejemplo. Con `archivo`,
#' la escribe como CSV.
#'
#' @param tabla Nombre de la tabla (ver [dl_tablas]).
#' @param archivo Ruta del CSV que se escribe (opcional).
#' @return La plantilla (`data.frame`); con `archivo`, invisible.
#' @seealso [dl_tablas], [dl_tabla()], [dl_nuevo_proyecto()].
#' @family proyecto
#' @examples
#' dl_plantilla("covariables")
#' @export
dl_plantilla <- function(tabla, archivo = NULL) {
  tabla <- .dl_exigir_tabla(tabla)
  ej <- .DL_EJEMPLOS_PLANTILLA[[tabla]]
  d <- as.data.frame(ej, stringsAsFactors = FALSE)
  if (is.null(archivo)) return(d)
  data.table::fwrite(d, archivo, na = "")
  invisible(d)
}

# Una fila de ejemplo de cada tabla (genérica: ubicaciones con códigos de ejemplo). Las columnas en el orden de
# tablas.csv: las que exige y las opcionales de uso común.
.DL_EJEMPLOS_PLANTILLA <- list(
  ubicaciones = list(ubicacion = c("PAIS", "R01"), nombre = c("Pa\u00eds", "Regi\u00f3n 1"), padre = c(NA, "PAIS")),
  poblacion = list(ubicacion = "R01", anio = 2023L, sexo = "mujeres", edad_inicio = 40, edad_fin = 45,
                   poblacion = 12500),
  ancla = list(causa = 1234L, anio = 2023L, sexo = "mujeres", edad_inicio = 40, edad_fin = 45, medida = "prevalencia",
               valor = 0.012, inferior = 0.009, superior = 0.016),
  covariables = list(ubicacion = "R01", anio = 2023L, covariable = "haqi", valor = 61.2, error_estandar = 1.4,
                     fuente = "proxy: indicador de ejemplo"),
  betas = list(causa = 1234L, covariable = "haqi", efecto_sobre = "mortalidad_exceso", transformacion = "lineal",
               escala = 1, beta = -0.012, inferior = -0.018, superior = -0.006, fuente = "ap\u00e9ndice de GBD"),
  datos = list(causa = 1234L, ubicacion = "R01", anio = 2023L, sexo = "mujeres", edad_inicio = 40, edad_fin = 45,
               medida = "prevalencia", valor = 0.011, error_estandar = 0.002, fuente = "encuesta de ejemplo"),
  severidad = list(causa = 1234L, estado = "Estado leve", proporcion = 0.6, inferior = 0.5, superior = 0.7,
                   peso_discapacidad = 0.02, peso_inferior = 0.012, peso_superior = 0.031),
  fuentes_gbd = list(causa = 1234L, ubicacion = "PAIS", componente = "no_fatal", nid = 123456L),
  poblacion_detalle = list(anio = 2023L, sexo = "mujeres", edad_inicio = 80, edad_fin = 85, poblacion = 30500),
  proxies_crudos = list(ubicacion = c("R01", "R01"), anio = c(2023L, 2024L), covariable = c("haqi", "haqi"),
                        indicador = c("indicador de ejemplo", "indicador de ejemplo"), valor = c(61.2, 62.5),
                        error_estandar = c(1.4, 1.5)),
  razones = list(causa = 1234L, ubicacion = "R01", anio = 2023L, razon = 1.15, error_log = 0.08,
                 fuente = "indicador de ejemplo"))

#' El contrato de insumos
#'
#' Las tablas que dismodlite necesita para estimar una causa. Todas comparten un **eje**: `causa`, `ubicacion`,
#' `anio`, `sexo`, `edad_inicio` y `edad_fin`. Una columna del eje que no viene significa que la tabla no varía en
#' esa dimensión: sin `causa`, vale para todas las causas; sin `ubicacion`, es la nacional; sin `anio`, todos los
#' años; sin `sexo`, ambos sexos; sin las edades, todas las edades. Las unidades son fijas: la prevalencia y las
#' proporciones en proporción (0 a 1), la incidencia, la mortalidad y los AVD por persona-año, la población en
#' personas. Las bandas de edad son `[edad_inicio, edad_fin)` en años enteros; `edad_fin` vacío es la banda abierta.
#'
#' Cada tabla es un CSV (o una carpeta de CSV) de la carpeta del proyecto con su nombre, o un `data.frame` que se pasa
#' a [dl_proyecto()]. Las descargas de GBD Results y del GHDx se reconocen por sus columnas y se convierten solas.
#'
#' El ancla puede traer filas de ambos sexos, pero el modelo no las usa: si son más finas que la población y
#' `poblacion_detalle` no trae `ambos`, se dejan fuera en vez de pedir ese detalle (el de hombres y mujeres sí hace
#' falta); con detalle de `ambos`, se agrupan como las de cada sexo.
#'
#' @seealso [dl_tabla()], [dl_plantilla()], [dl_proyecto()].
#' @family proyecto
#' @name dl_tablas
NULL

# Las columnas de cada tabla van en un bloque sin markdown (@noMd) para que el \ifelse{} de .dl_rd_tablas() llegue tal
# cual al Rd (ver .dl_rd_config_simple() en R/configuracion.R); roxygen deja las secciones en el orden de sus bloques.

#' @name dl_tablas
#' @rdname dl_tablas
#' @noMd
#' @eval .dl_rd_tablas()
NULL

# Secciones Rd de ?dl_tablas: por tabla, sus columnas (tipo, si la exige, unidad y descripci\u00f3n). En HTML una tabla;
# en texto y en PDF, donde las celdas de una tabla no se parten en l\u00edneas, una lista con lo mismo.
.dl_rd_tablas <- function() {
  t <- .dl_tablas_ref()
  # % abre un comentario en Rd y las llaves y la barra deben ir escapadas
  esc <- function(x) gsub("([%{}\\\\])", "\\\\\\1", x)
  tipo <- c(texto = "texto", numero = "n\u00famero", entero = "entero", logico = "l\u00f3gico")
  unlist(lapply(.DL_TABLAS, function(tb) {
    f <- t[t$tabla == tb, ]
    que <- esc(paste0(f$descripcion, ifelse(nzchar(f$unidad), paste0(" (", f$unidad, ")"), "")))
    exige <- ifelse(f$exige == "si", "s\u00ed", "no")
    tabla <- c("\\tabular{llll}{",
               "\\strong{columna} \\tab \\strong{tipo} \\tab \\strong{exige} \\tab \\strong{qu\u00e9 es}\\cr",
               sprintf("\\code{%s} \\tab %s \\tab %s \\tab %s\\cr", f$columna, tipo[f$tipo], exige, que),
               "}")
    lista <- c("\\describe{",
               sprintf("\\item{\\code{%s}}{%s. %s%s.}", f$columna, que,
                       paste0(toupper(substr(tipo[f$tipo], 1L, 1L)), substring(tipo[f$tipo], 2L)),
                       ifelse(f$exige == "si", ", obligatoria", ", opcional")),
               "}")
    c(sprintf("@section Tabla \\code{%s}:", tb), "\\ifelse{html}{", tabla, "}{", lista, "}")
  }))
}

# ---- Utilidades de las tablas ----

# Filas de datos (líneas no vacías, sin el encabezado) del CSV `f` o de los CSV de la carpeta `f`, leyendo a lo sumo
# `n` líneas de cada uno; 0 si no existe. Una tabla opcional sin filas es como si no estuviera (así sirve de plantilla).
.dl_filas_csv <- function(f, n = -1L) {
  if (dir.exists(f)) f <- list.files(f, "[.]csv$", ignore.case = TRUE, full.names = TRUE)
  sum(vapply(f[file.exists(f)], function(a) max(0L, sum(grepl("[^[:space:]]", useBytes = TRUE,
    readLines(a, n = n, warn = FALSE, encoding = "UTF-8"))) - 1L), 1L))
}
.dl_hay_filas <- function(archivo) .dl_filas_csv(archivo, 50L) > 0L

# Números calculados -> su texto en data.table::fwrite (15 cifras): valen lo mismo que en un CSV del formato completo.
.dl_num_texto <- function(x) {
  if (!length(x)) return(character())
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  data.table::fwrite(list(x = x), f, eol = "\n", na = "NA")
  out <- readLines(f)[-1L]
  out[out == "NA"] <- NA_character_
  out
}

# Números -> su texto exacto: el que vuelve exactamente al mismo double al leerlo con as.numeric() y con fread(). Es
# el decimal más corto (15, 16 o 17 cifras significativas) o, si alguno de `x` no tiene decimal que vuelva, el texto
# hexadecimal de todos (.dl_num_hex): sin long double (R en arm64, como en los Mac con procesador Apple), as.numeric() y
# fread() no redondean bien todos los decimales de 17 cifras, y el double de una tasa / 100 000 puede no tener ningún
# texto decimal que vuelva a él. Toda la columna va en un mismo formato porque fread() lee como texto una columna que
# mezcla decimales y hexadecimales; por eso `x` es siempre una columna entera. Si tampoco el hexadecimal vuelve a `x`
# (no debería pasar), un error: nunca se escribe en silencio un número distinto.
.dl_num_exacto <- function(x) {
  s <- vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    for (d in 15:17) { s <- format(v, digits = d); if (as.numeric(s) == v) return(s) }
    NA_character_
  }, "", USE.NAMES = FALSE)
  if (identical(is.na(s), is.na(x)) && identical(.dl_leer_numeros(s), as.numeric(x))) return(s)
  h <- .dl_num_hex(x)
  y <- .dl_leer_numeros(h)
  vuelve <- !is.null(y) && identical(is.na(y), is.na(x)) && all(y[!is.na(x)] == x[!is.na(x)])
  if (!vuelve)
    .dl_stop(paste0("no hay un texto que vuelva exactamente a los n\u00fameros %s (ni decimal ni hexadecimal): la ",
                    "traducci\u00f3n no puede escribirlos sin cambiarlos. Es un error del paquete: rep\u00f3rtalo con ",
                    "estos valores"), paste(utils::head(format(x[!is.na(x)], digits = 17L), 3L), collapse = ", "))
  h
}

# Números -> su texto hexadecimal, exacto, en la forma que leen fread() y as.numeric() (fread() pide la parte
# fraccionaria y lee el cero solo como 0x0.0p-1022).
.dl_num_hex <- function(x) {
  h <- sub("^(-?0x1)p", "\\1.0p", sprintf("%a", as.numeric(x)))
  h[!is.na(x) & x == 0] <- "0x0.0p-1022"
  h[is.na(x)] <- NA_character_
  h
}

# Textos de números `s` leídos como lee fread() una columna de un CSV, detectando su tipo (vacío = NA): un vector
# numérico, o NULL si fread() no la lee como números.
.dl_leer_numeros <- function(s) {
  if (all(is.na(s))) return(rep(NA_real_, length(s)))
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  writeLines(c("x", ifelse(is.na(s), "", s)), f)
  x <- data.table::fread(f, na.strings = "", showProgress = FALSE)$x
  if (is.numeric(x)) as.numeric(x)
}

# Grupos de edad de GBD (el catálogo de inst/referencia): age_group_id y límites [age_start, age_end).
.dl_grupos_edad_referencia <- function()
  .dl_bandas_catalogo(d = .dl_leer_memo(.dl_inst_archivo("referencia", "catalogo_demograficos_gbd2023.csv"),
                                        function(p) .dl_leer_csv(p, colClasses = "character")))

# Grupo de edad de GBD con los límites [inicio, fin), sin el estandarizado por edad; NA si no hay ninguno.
.dl_grupo_edad <- function(inicio, fin) {
  g <- .dl_grupos_edad_referencia()[age_group_id != .DL_BANDAS_AGREGADAS[["estandarizada"]]]
  g$age_group_id[match(paste(as.numeric(inicio), as.numeric(fin)), paste(g$age_start, g$age_end))]
}

# Los valores de `v`, sin repetir y en orden, para un mensaje («2019, 2023»); `vacio` si no hay ninguno.
.dl_lista <- function(v, vacio = "ninguno")
  if (length(v)) paste(sort(unique(v), method = "radix"), collapse = ", ") else vacio

# ---- Reglas entre tablas ----
# Lo que una tabla sola no puede comprobar (el nivel «proyecto» de ?dl_tablas): cada regla recibe las tablas del
# proyecto (`tablas`, lista nombrada de dl_tabla sin las que no están; trae siempre las obligatorias) y la
# configuración traducida `cfg` de una causa, y devuelve sus problemas como texto, cada uno con su tabla delante
# («poblacion: ...»). Las reglas de la configuración que cruzan claves están en .dl_problemas_config_simple; las que
# necesitan las tablas del formato completo (las subnacionales suman la nacional, los proxies cierran en el valor
# nacional, los datos tienen valores posibles), en R/reglas.R: las corre dl_insumos().

# Problemas (list(problemas, avisos)) de las tablas de un proyecto para la causa de `cfg`. `n_causas`: cuántas causas
# tienen configuración en el proyecto (con más de una, el ancla dice de qué causa es cada fila). `tablas` son las del
# modelo (covariables con las filas calibradas de proxies_crudos, .dl_tablas_modelo); `originales`, las del proyecto
# tal como vinieron, para la regla que compara covariables con proxies_crudos. Una causa que es la suma de sus
# subtipos (.dl_es_suma) no se ajusta: solo le tocan las reglas de sus tablas, ubicaciones y poblacion.
.dl_problemas_proyecto <- function(tablas, cfg, n_causas = 1L, originales = tablas) if (.dl_es_suma(cfg)) list(
  problemas = c(.dl_regla_ubicaciones(tablas), .dl_regla_ubicaciones_conocidas(tablas),
                .dl_regla_poblacion(tablas, cfg)),
  avisos = character()) else list(
  problemas = c(.dl_regla_proxies_dos_tablas(originales),
                .dl_regla_ubicaciones(tablas), .dl_regla_ubicaciones_conocidas(tablas),
                .dl_regla_poblacion(tablas, cfg), .dl_regla_ancla(tablas, cfg, n_causas),
                .dl_regla_bandas_ancla(tablas, cfg), .dl_regla_betas(tablas, cfg),
                .dl_regla_proxies_incompletos(tablas, cfg), .dl_regla_intervalo_nacional(tablas, cfg),
                .dl_regla_severidad(tablas, cfg)),
  avisos = c(.dl_regla_covariables_sin_beta(tablas, cfg), .dl_regla_ubicaciones_sin_proxy(tablas, cfg),
             .dl_regla_unidades_datos(tablas, cfg)))

# Los sexos del modelo (cfg$sexos) en palabras del contrato: «hombres», «mujeres».
.dl_sexos_modelo <- function(cfg) unname(.DL_SEXOS_CONTRATO[as.character(unlist(cfg$sexos))])

# Unos pocos valores para un mensaje: «01, 02, 03, 04, 05 y 20 más».
.dl_unos <- function(v, n = 5L) {
  v <- sort(unique(v), method = "radix")
  paste0(paste(utils::head(v, n), collapse = ", "), if (length(v) > n) sprintf(" y %d m\u00e1s", length(v) - n) else "")
}

# ubicaciones: una sola fila sin padre (la nacional); el padre de las demás es la nacional; sin códigos repetidos.
.dl_regla_ubicaciones <- function(tablas) {
  u <- tablas$ubicaciones
  padre <- .dl_col(u, "padre", NA_character_)
  raiz <- u$ubicacion[is.na(padre)]
  otros <- unique(padre[!is.na(padre) & !padre %in% raiz])
  repetidos <- unique(u$ubicacion[duplicated(u$ubicacion)])
  c(if (length(raiz) != 1L)
      sprintf(paste0("ubicaciones: %s; solo la nacional va sin padre y las subnacionales llevan la nacional como ",
                     "padre"),
              if (!length(raiz)) "todas las filas tienen padre" else sprintf("%d filas van sin padre (%s)",
                                                                              length(raiz), .dl_unos(raiz))),
    if (length(raiz) == 1L && length(otros))
      sprintf(paste0("ubicaciones: el padre de una ubicaci\u00f3n subnacional es la nacional (%s), y trae %s: el ",
                     "modelo usa dos niveles, la nacional y sus subnacionales"), raiz, .dl_unos(otros)),
    if (length(repetidos)) sprintf("ubicaciones: c\u00f3digos repetidos: %s", .dl_unos(repetidos)))
}

# Toda ubicación de poblacion, covariables, datos, fuentes_gbd y razones está en ubicaciones.
.dl_regla_ubicaciones_conocidas <- function(tablas) {
  codigos <- tablas$ubicaciones$ubicacion
  con_ubicacion <- c("poblacion", "covariables", "datos", "fuentes_gbd", "razones")
  unlist(lapply(intersect(con_ubicacion, names(tablas)), function(t) {
    x <- setdiff(stats::na.omit(.dl_col(tablas[[t]], "ubicacion", NA_character_)), codigos)
    if (length(x))
      sprintf(paste0("%s: la(s) ubicaci\u00f3n(es) %s no est\u00e1(n) en la tabla ubicaciones: ",
                     "agr\u00e9gala(s) ah\u00ed o corrige el c\u00f3digo"), t, .dl_unos(x))
  }))
}

# Los huecos entre las bandas de edad [inicio, fin) (las distintas, en orden): las edades [fin_i, inicio_i+1) donde
# una banda termina antes de que empiece la siguiente, como data.table (edad_inicio, edad_fin); sin filas si no hay.
.dl_huecos_bandas <- function(inicio, fin) {
  b <- unique(data.table::data.table(edad_inicio = inicio, edad_fin = fin))[order(edad_inicio, edad_fin)]
  k <- which(utils::head(b$edad_fin, -1L) < utils::tail(b$edad_inicio, -1L))
  data.table::data.table(edad_inicio = b$edad_fin[k], edad_fin = b$edad_inicio[k + 1L])
}

# poblacion: trae el año que se estima en los sexos del modelo; sus bandas (las del modelo) son las mismas en todas
# las ubicaciones, años y sexos, van seguidas (sin huecos) y la primera empieza en edad_inicio o antes (una suma de
# subtipos no tiene edad_inicio).
.dl_regla_poblacion <- function(tablas, cfg) {
  p <- tablas$poblacion
  anio <- .dl_anio_ajuste(cfg)
  huecos <- .dl_huecos_bandas(p$edad_inicio, p$edad_fin)
  falta <- setdiff(.dl_sexos_modelo(cfg), p$sexo[p$anio == anio])
  grupo <- paste(p$ubicacion, p$anio, p$sexo)
  bandas <- tapply(paste(p$edad_inicio, p$edad_fin), grupo, function(b) paste(sort(b), collapse = ";"))
  comunes <- names(which.max(table(bandas)))
  inicio <- min(p$edad_inicio)
  c(if (length(falta))
      sprintf("poblacion: no trae la poblaci\u00f3n de %s de %d (el a\u00f1o que se estima; a\u00f1os que trae: %s)",
              paste(falta, collapse = " y "), anio, .dl_lista(p$anio)),
    if (length(unique(bandas)) > 1L)
      sprintf(paste0("poblacion: las bandas de edad (las del modelo) deben ser las mismas en todas las ubicaciones, ",
                     "a\u00f1os y sexos; %s no traen las mismas que las dem\u00e1s"),
              .dl_unos(names(bandas)[bandas != comunes], 3L)),
    if (nrow(huecos))
      sprintf(paste0("poblacion: las bandas de edad dejan un hueco: falta(n) %s; las bandas van seguidas, de la ",
                     "primera a la \u00faltima"),
              paste(.dl_nombre_banda(huecos$edad_inicio, huecos$edad_fin), collapse = ", ")),
    if (isTRUE(inicio > as.numeric(cfg$edad_inicio)))
      sprintf(paste0("poblacion: la primera banda empieza a los %g a\u00f1os, despu\u00e9s de edad_inicio (%g) de la ",
                     "configuraci\u00f3n: agrega las edades desde %g o sube edad_inicio"), inicio,
              as.numeric(cfg$edad_inicio), as.numeric(cfg$edad_inicio)))
}

# ancla: trae la prevalencia de la causa en el año del ancla y en cada sexo del modelo y, si la usa el prior de la
# mortalidad en exceso (.dl_prior_usa_csmr), la mortalidad. Con más de una causa en el proyecto, trae la columna causa.
# El año del ancla ya es el del proyecto (.dl_anio_ancla_proyecto): si el ancla no trae la prevalencia del año que se
# estima, el último anterior. Lo que falta aquí es una medida o un sexo de ese año (la pista: declarar ancla.anio con
# el año anterior, que sí los trae) o un año sin ninguno anterior desde el que proyectar.
.dl_regla_ancla <- function(tablas, cfg, n_causas = 1L) {
  sin_causa <- if (n_causas > 1L && !"causa" %in% names(tablas$ancla))
    sprintf(paste0("ancla: falta la columna causa: el proyecto tiene %d causas con configuraci\u00f3n y cada fila del ",
                   "ancla dice de cu\u00e1l es"), n_causas)
  a <- .dl_filas_de_causa(tablas$ancla, cfg$cause_id)
  anio <- .dl_anio_ancla(cfg)
  pista <- c(prevalencia = "", mortalidad = paste0(
    ": la usa el prior de la mortalidad en exceso (desde_ancla, por defecto, o plano sin techo); agr\u00e9gala a la ",
    "tabla ancla o declara mortalidad_exceso: {prior: plano, techo: ...}, por persona-a\u00f1o"))
  c(sin_causa, unlist(lapply(c("prevalencia", if (.dl_prior_usa_csmr(cfg)) "mortalidad"), function(m) {
    x <- a[a$medida %in% m]
    falta <- setdiff(.dl_sexos_modelo(cfg), x$sexo[x$anio == anio])
    if (!length(falta)) return(NULL)
    hay <- x$anio[x$sexo %in% falta]
    sprintf("ancla: no trae la %s de la causa %d %s%s%s", m, cfg$cause_id,
            if (!nrow(x)) sprintf("(medidas que trae de la causa: %s)", .dl_lista(a$medida, "ninguna"))
            else sprintf("de %s de %d (a\u00f1os que trae: %s)", paste(falta, collapse = " y "), anio, .dl_lista(hay)),
            if ((anio - 1L) %in% hay && is.null(cfg$years$ancla))
              sprintf("; si %d a\u00fan no tiene estimaci\u00f3n de GBD, proyecta desde %d con ancla: {anio: %d}", anio,
                      anio - 1L, anio - 1L)
            else if (m == "prevalencia" && length(hay) && all(hay > anio))
              "; solo se proyecta desde un a\u00f1o anterior, y el ancla no trae ninguno" else "", pista[[m]])
  })))
}

# Bandas del ancla: cada una es una unión de bandas de la población o se agrupa en una de ellas con poblacion_detalle
# (.dl_ancla_en_bandas, lo mismo que hace la traducción; sin anchor.agrupar_bandas_finas). Con un hueco en las bandas
# de la población, la regla de la población lo nombra: esta espera.
.dl_regla_bandas_ancla <- function(tablas, cfg) {
  if (isTRUE(cfg$anchor$agrupar_bandas_finas)) return(character())
  if (nrow(.dl_huecos_bandas(tablas$poblacion$edad_inicio, tablas$poblacion$edad_fin))) return(character())
  a <- data.table::as.data.table(as.data.frame(.dl_filas_de_causa(tablas$ancla, cfg$cause_id)))
  if (!nrow(a)) return(character())
  e <- tryCatch({ .dl_ancla_en_bandas(a, tablas, cfg); NULL }, dl_error = .dl_detalle)
  if (is.null(e)) character()
  else if (startsWith(e, "poblacion_detalle ")) sub("^poblacion_detalle ", "poblacion_detalle: ", e)
  else paste0("ancla: ", sub("^ancla: ", "", e))
}

# Covariables con valor nacional en la tabla covariables en `anio` (o sin año).
.dl_covariables_nacionales <- function(tablas, anio) {
  cov <- tablas$covariables
  if (is.null(cov)) return(character())
  a <- .dl_col(cov, "anio", NA_integer_)
  unique(cov$covariable[.dl_es_nacional(cov, .dl_ubicacion_nacional(tablas)) & (is.na(a) | a == anio)])
}

# Covariables con valores subnacionales: filas subnacionales en covariables o filas en proxies_crudos.
.dl_covariables_subnacionales <- function(tablas) {
  cov <- tablas$covariables
  unique(c(if (!is.null(cov)) cov$covariable[!.dl_es_nacional(cov, .dl_ubicacion_nacional(tablas))],
           tablas$proxies_crudos$covariable))
}

# Las covariables de las betas `b` que necesitan su propia fila nacional: las de una beta sin valor_nacional_de y las
# de una que lo declara si la covariable no tiene valores subnacionales (sin ellos no hay proxy que anclar en el valor
# nacional de la otra, y el modelo usa el suyo).
.dl_betas_con_fila_propia <- function(tablas, b) {
  vn <- .dl_col(b, "valor_nacional_de", NA_character_)
  unique(b$covariable[is.na(vn) | !b$covariable %in% .dl_covariables_subnacionales(tablas)])
}

# betas (las de la causa): cada covariable tiene valor nacional en el año del ancla, el suyo o, si la fila declara
# valor_nacional_de y la covariable tiene valores subnacionales, el de la covariable que nombra (la fila nacional
# propia no hace falta: .dl_betas_con_fila_propia); escala solo con la transformación lineal (con log o logit, vacía
# o 1).
.dl_regla_betas <- function(tablas, cfg) {
  b <- .dl_betas_de_causa(tablas$betas, cfg$cause_id, cfg$extraction$cause_id)
  if (is.null(b) || !nrow(b)) return(character())
  anio <- .dl_anio_ancla(cfg)
  nac <- .dl_covariables_nacionales(tablas, anio)
  donde <- sprintf("de %d (el a\u00f1o del ancla) en la tabla covariables%s", anio,
                   if (is.null(tablas$covariables)) ", que no est\u00e1" else "")
  vn <- .dl_col(b, "valor_nacional_de", NA_character_)
  sin <- setdiff(.dl_betas_con_fila_propia(tablas, b), nac)
  vn_sin <- setdiff(vn[!is.na(vn)], nac)
  escala <- .dl_col(b, "escala", NA_real_)
  mal <- b$transformacion %in% c("log", "logit") & !is.na(escala) & escala != 1
  c(if (length(sin)) sprintf("betas: %s no tiene(n) valor nacional %s: agrega su fila nacional", .dl_unos(sin), donde),
    if (length(vn_sin)) sprintf("betas: valor_nacional_de nombra %s, sin valor nacional %s", .dl_unos(vn_sin), donde),
    if (any(mal))
      sprintf(paste0("betas: escala solo se usa con la transformaci\u00f3n lineal; con log o logit va vac\u00eda o 1 ",
                     "(%s)"), .dl_unos(b$covariable[mal])))
}

# Proxies del año que se estima (filas subnacionales de covariables) de las covariables con proxy de la configuración
# (cfg$covariables, en el modo subnacional por covariables): una lista ubicación subnacional -> covariables que trae.
# NULL si la estimación subnacional no es por covariables.
.dl_proxies_por_ubicacion <- function(tablas, cfg) {
  if (!identical(cfg$origen$subnacional, "covariables") || is.null(tablas$covariables)) return(NULL)
  decl <- vapply(cfg$covariables, function(cv) cv$covariate_name_short, "")
  cov <- tablas$covariables
  a <- .dl_col(cov, "anio", NA_integer_)
  k <- !.dl_es_nacional(cov, .dl_ubicacion_nacional(tablas)) & (is.na(a) | a == .dl_anio_ajuste(cfg)) &
    cov$covariable %in% decl
  u <- tablas$ubicaciones
  subs <- u$ubicacion[!is.na(.dl_col(u, "padre", NA_character_))]
  lapply(stats::setNames(nm = subs), function(s) unique(cov$covariable[k & cov$ubicacion %in% s]))
}

# Una ubicación subnacional con proxies de unas covariables y no de otras: la estimación subnacional por covariables
# necesita el de todas (sin ninguno, la ubicación queda fuera: .dl_regla_ubicaciones_sin_proxy).
.dl_regla_proxies_incompletos <- function(tablas, cfg) {
  px <- .dl_proxies_por_ubicacion(tablas, cfg)
  if (is.null(px)) return(character())
  decl <- vapply(cfg$covariables, function(cv) cv$covariate_name_short, "")
  unlist(lapply(decl, function(cv) {
    faltan <- names(px)[lengths(px) > 0L & !vapply(px, function(x) cv %in% x, NA)]
    if (length(faltan))
      sprintf(paste0("covariables: %s no trae(n) el proxy de %s de %d (el a\u00f1o que se estima) y s\u00ed ",
                     "el de otras covariables: la estimaci\u00f3n subnacional necesita el de todas"), .dl_unos(faltan), cv,
              .dl_anio_ajuste(cfg))
  }))
}

# El valor nacional en que se anclan los proxies (el de la covariable o el de su valor_nacional_de, en el año del
# ancla) trae su intervalo (inferior y superior): la estimación subnacional sortea el valor nacional de él.
.dl_regla_intervalo_nacional <- function(tablas, cfg) {
  if (is.null(.dl_proxies_por_ubicacion(tablas, cfg))) return(character())
  ref <- unique(vapply(cfg$covariables, function(cv) cv$sustituye$covariate_name_short %||% cv$covariate_name_short, ""))
  cov <- tablas$covariables
  a <- .dl_col(cov, "anio", NA_integer_)
  k <- .dl_es_nacional(cov, .dl_ubicacion_nacional(tablas)) & (is.na(a) | a == .dl_anio_ancla(cfg)) &
    cov$covariable %in% ref
  sin <- unique(cov$covariable[k & (is.na(.dl_col(cov, "inferior", NA_real_)) |
                                      is.na(.dl_col(cov, "superior", NA_real_)))])
  if (length(sin))
    sprintf(paste0("covariables: el valor nacional de %s de %d (el a\u00f1o del ancla) no trae su intervalo (inferior ",
                   "y superior; en una descarga del GHDx, lower_value y upper_value): la estimaci\u00f3n subnacional ",
                   "lo usa"), .dl_unos(sin), .dl_anio_ancla(cfg))
  else character()
}

# severidad (de la causa): las proporciones suman 1, por sexo y banda si los trae, con la tolerancia del redondeo.
.dl_regla_severidad <- function(tablas, cfg) {
  s <- if (!is.null(tablas$severidad)) .dl_filas_de_causa(tablas$severidad, cfg$cause_id)
  if (is.null(s) || !nrow(s)) return(character())
  por <- intersect(c("sexo", "edad_inicio", "edad_fin"), names(s))
  grupo <- if (length(por)) do.call(paste, unname(as.list(s[, por, with = FALSE]))) else rep("", nrow(s))
  suma <- tapply(s$proporcion, grupo, sum)
  mal <- which(abs(suma - 1) > .DL_TOLERANCIA_SUMA_PARTICION)
  if (length(mal))
    sprintf("severidad: las proporciones de la causa %d suman %s%s; deben sumar 1", cfg$cause_id,
            format(signif(suma[[mal[1L]]], 6L)), if (length(por)) sprintf(" en %s %s", paste(por, collapse = ", "),
                                                                           names(suma)[mal[1L]]) else "")
  else character()
}

# proxies_crudos y covariables: las filas subnacionales de una covariable vienen de una sola de las dos tablas (las de
# proxies_crudos se calibran y se agregan a covariables). Sobre las tablas tal como vinieron.
.dl_regla_proxies_dos_tablas <- function(tablas) {
  cr <- tablas$proxies_crudos; cv <- tablas$covariables
  if (is.null(cr) || is.null(cv)) return(character())
  sub <- unique(cv$covariable[!.dl_es_nacional(cv, .dl_ubicacion_nacional(tablas))])
  sprintf(paste0("proxies_crudos: la covariable %s tiene filas subnacionales en las dos tablas (covariables y ",
                 "proxies_crudos): deja las de una sola; las de proxies_crudos se calibran y se agregan a covariables"),
          intersect(unique(cr$covariable), sub))
}

# Aviso: una covariable con filas subnacionales (proxies) y sin beta de la causa no se usa.
.dl_regla_covariables_sin_beta <- function(tablas, cfg) {
  cov <- tablas$covariables
  if (is.null(cov)) return(character())
  con_proxy <- unique(cov$covariable[!.dl_es_nacional(cov, .dl_ubicacion_nacional(tablas))])
  b <- .dl_betas_de_causa(tablas$betas, cfg$cause_id, cfg$extraction$cause_id)
  sin <- setdiff(con_proxy, b$covariable)
  sprintf("covariables: la covariable %s tiene proxies pero no tiene beta: no se usa", sin)
}

# Aviso: una ubicación subnacional sin ningún proxy del año que se estima queda fuera de la estimación subnacional
# por covariables (si ninguna los trae, la traducción lo dice como error).
.dl_regla_ubicaciones_sin_proxy <- function(tablas, cfg) {
  px <- .dl_proxies_por_ubicacion(tablas, cfg)
  sin <- names(px)[!lengths(px)]
  if (is.null(px) || !length(sin) || length(sin) == length(px)) return(character())
  sprintf(paste0("covariables: la(s) ubicaci\u00f3n(es) subnacional(es) %s no tiene(n) proxies de %d (el a\u00f1o ",
                 "que se estima): queda(n) fuera de la estimaci\u00f3n subnacional por covariables"), .dl_unos(sin),
          .dl_anio_ajuste(cfg))
}

# Aviso de los valores de mortalidad de la causa en datos que parecen tasas por 100 000 y no por persona-año: mayores
# que 1, o más de 1000 veces la mortalidad del ancla de la misma causa, año, sexo y banda. Una tasa por 100 000 es
# 100 000 veces la de persona-año: con el margen de 1000, un dato por persona-año pasa aunque sea hasta 1000 veces el
# del ancla, y uno por 100 000 se detecta salvo que su valor verdadero sea menos de la centésima parte del ancla.
.dl_regla_unidades_datos <- function(tablas, cfg) {
  d <- tablas$datos
  if (is.null(d) || !"valor" %in% names(d)) return(character())
  causa <- .dl_col(d, "causa", NA_integer_)
  k <- which(d$medida == "mortalidad" & !is.na(d$valor) & (is.na(causa) | causa == cfg$cause_id))
  a <- .dl_filas_de_causa(tablas$ancla, cfg$cause_id)
  a <- a[a$medida %in% "mortalidad"]
  anio <- ifelse(is.na(.dl_col(d, "anio", NA_integer_)), .dl_col(d, "anio_inicio", NA_integer_), .dl_col(d, "anio"))
  clave <- function(x, anio) paste(anio, x$sexo, x$edad_inicio, x$edad_fin)
  ref <- a$valor[match(clave(d[k], anio[k]), clave(a, a$anio))]
  v <- d$valor[k]
  mal <- which(v > 1 | (!is.na(ref) & v > 1000 * ref))
  if (!length(mal)) return(character())
  sprintf(paste0("datos: %d valor(es) de mortalidad parecen tasas por 100 000, no por persona-a\u00f1o (%s; por ",
                 "ejemplo %s%s): divide valor y error_estandar por 100 000"), length(mal),
          .dl_filas_msg(k[mal], df = .dl_origen_df(attr(d, "origen"))),
          format(v[mal[1L]]), if (is.na(ref[mal[1L]])) "" else sprintf(", donde el ancla da %s",
                                                                       format(signif(ref[mal[1L]], 3L))))
}
