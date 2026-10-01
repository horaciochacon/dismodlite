# El contrato de insumos (ver ?dl_tablas): las tablas que el modelo necesita, con un eje común de dimensiones
# (causa, ubicación, año, sexo y banda de edad) y sus valores. Su única definición es inst/referencia/tablas.csv; de
# ahí salen la validación, las plantillas, la ayuda y los mensajes. Los lectores de fuentes conocidas (GBD Results,
# GHDx, particiones de severidad) están en R/lectores.R; la traducción al formato completo, en
# R/contrato_traduccion.R.

# ---- Constantes ----

# Las tablas del contrato, en el orden en que se documentan.
.DL_TABLAS <- c("ubicaciones", "poblacion", "ancla", "covariables", "betas", "datos", "severidad", "fuentes_gbd",
                "poblacion_detalle")

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
                         fuentes_gbd = c("componente", "nid"))

# Tablas en las que las bandas de edad de un mismo grupo no pueden solaparse.
.DL_TABLAS_SIN_SOLAPE <- c("poblacion", "ancla", "covariables", "severidad", "poblacion_detalle")

# Filas para un mensaje: «filas 2, 5, 7» (como mucho 5), contando la del encabezado como 1.
.dl_filas_msg <- function(i) sprintf("fila(s) %s%s", paste(utils::head(i + 1L, 5L), collapse = ", "),
                                     if (length(i) > 5L) sprintf(" y %d m\u00e1s", length(i) - 5L) else "")

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

# Problemas y avisos de la tabla `d` (ya normalizada y tipada) de `tabla`.
.dl_problemas_tabla <- function(d, tabla) {
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
    if (nzchar(t$maximo[i]) && any(x > as.numeric(t$maximo[i]), na.rm = TRUE))
      p("%s: hay valores mayores que %s (%s)", cn, t$maximo[i], .dl_filas_msg(which(x > as.numeric(t$maximo[i]))))
  }
  claves <- intersect(c(.DL_EJE, .DL_CLAVES_TABLA[[tabla]]), names(d))
  grupo <- setdiff(claves, c("edad_inicio", "edad_fin"))
  if (all(c("edad_inicio", "edad_fin") %in% names(d)) && !length(faltan)) {
    abierta <- is.na(d$edad_fin) & !is.na(d$edad_inicio)
    # filas de la banda de mayor edad_inicio de su grupo (por posición de fila, no en el orden de los grupos)
    ultima <- seq_len(nrow(d)) %in% d[, .I[edad_inicio == max(edad_inicio, na.rm = TRUE)], by = grupo]$V1
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
    if (tabla %in% .DL_TABLAS_SIN_SOLAPE) {
      orden <- d[, .I[order(edad_inicio)], by = grupo]$V1
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
# una tabla sola. Error con todos los problemas juntos (campos: tabla, problemas).
.dl_tabla_contrato <- function(d, tabla, origen) {
  d <- .dl_normalizar_nombres(d, tabla)
  ti <- .dl_tipar_tabla(d, tabla)
  r <- .dl_problemas_tabla(ti$d, tabla)
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
  poblacion_detalle = list(anio = 2023L, sexo = "mujeres", edad_inicio = 80, edad_fin = 85, poblacion = 30500))

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
