# Emisor YAML de los manifiestos y del registro de corridas, con el formato exacto, byte a byte, del SafeDumper de
# PyYAML en estilo de bloque: listas sangradas bajo su clave, nunca en estilo flow, claves en el orden dado. Con
# sangria_lista = 0 escribe las listas sin sangría, como yaml::write_yaml (así agrega .dl_registry_append, en
# registro.R, una entrada a un registro escrito sin sangría).

# Texto libre para el manifiesto: el emisor no pone entre comillas un escalar con «: », así que se cambia por un
# guion largo.
.dl_texto_yaml <- function(x) gsub(": ", " \u2014 ", x %||% "", fixed = TRUE)

# Textos que YAML leería como otro tipo (entero, número, fecha, lógico o nulo): se escriben entre comillas simples.
.DL_RX_INT   <- "^[-+]?[0-9]+$"
.DL_RX_FLOAT <- "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$"
.DL_RX_FECHA <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}([Tt ].*)?$"
.DL_RX_BOOL  <- "^(yes|Yes|YES|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF)$"
.DL_RX_NULL  <- "^(~|null|Null|NULL)?$"

# Ancho máximo de una línea «clave: valor» cuyo valor tiene espacios: con este ancho, PyYAML partiría un escalar más
# largo en varias líneas, y este emisor no lo hace.
.DL_YAML_ANCHO <- 100L

# Un escalar de R como texto YAML. `clave`: dónde está el valor en el manifiesto (p. ej. «params.lambda»), para los
# mensajes de error.
.dl_yaml_escalar <- function(v, clave = NULL) {
  donde <- if (is.null(clave)) "" else sprintf(" de \u00ab%s\u00bb", clave)
  if (is.null(v)) return("null")
  if (length(v) != 1L)
    .dl_stop("el valor%s no se puede escribir en el manifiesto: debe ser un solo valor y tiene %d", donde, length(v))
  if (is.na(v)) return("null")     # NA o NaN de cualquier tipo: null (PyYAML lo lee como None)
  if (is.logical(v)) return(if (isTRUE(v)) "true" else "false")
  if (is.integer(v)) return(as.character(v))
  if (is.numeric(v)) {
    # un número entero se escribe con «.0» (como PyYAML un float); los demás, con 15 cifras significativas, y el texto
    # debe volver exactamente al mismo número
    s <- if (v == round(v) && abs(v) < 1e15) sprintf("%.1f", v)
         else format(v, scientific = FALSE, trim = TRUE, digits = 15)
    if (abs(as.numeric(s) - v) > 0)
      .dl_stop(paste0("el valor %s%s (de la configuraci\u00f3n o de los argumentos, p. ej. lambda o kappa) no se ",
                      "puede escribir exacto en el manifiesto: usa un n\u00famero con menos decimales (p. ej. ",
                      "0.33 en lugar de 1/3)"), format(v), donde)
    return(s)
  }
  v <- as.character(v)
  if (grepl(.DL_RX_INT, v) || grepl(.DL_RX_FLOAT, v) || grepl(.DL_RX_FECHA, v) ||
      grepl(.DL_RX_BOOL, v) || grepl(.DL_RX_NULL, v))
    return(paste0("'", gsub("'", "''", v), "'"))
  # Caracteres de control: PyYAML los escribe entre comillas dobles con escapes; este emisor no lo admite.
  if (grepl("[\n\t\r]", v, perl = TRUE))
    .dl_stop("un texto para el manifiesto o el registro lleva un salto de l\u00ednea o un tabulador: %s", v)
  # Comillas simples (con '' por ') donde PyYAML no admite el estilo plano en bloque: «: » o « #» dentro, un
  # indicador al inicio (# , [ ] { } & * ! | > ' " % @ `), «? », «: » o «- » al inicio (o el indicador solo), «:» al
  # final, o un espacio al inicio o al final.
  if (grepl("(: )|( #)|(^[ !&*\\[\\]\\{\\}#|>@`\"'%,])|(^[?:-]( |$))|(:$)|( $)", v, perl = TRUE))
    return(paste0("'", gsub("'", "''", v), "'"))
  v
}

.dl_es_mapa <- function(x) is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))
.dl_es_seq  <- function(x) (is.list(x) && is.null(names(x))) || (is.atomic(x) && length(x) > 1L)

# Una secuencia bajo una clave de nivel `nivel_padre`: cada elemento en su línea «- », con `sangria_lista` espacios
# más que la clave (2 como PyYAML; 0 como yaml::write_yaml). Un elemento que es un mapa empieza en la línea del guion.
# `ruta`: la clave de la secuencia en el documento (para los mensajes de error).
.dl_yaml_seq <- function(items, nivel_padre, sangria_lista, ruta = NULL) {
  if (is.atomic(items)) items <- as.list(items)
  col_guion <- nivel_padre * 2L + sangria_lista
  pad <- strrep(" ", col_guion)
  nivel_item <- col_guion %/% 2L + 1L
  out <- character(0)
  for (it in items) {
    if (.dl_es_mapa(it)) {
      cuerpo <- .dl_yaml_mapa(it, nivel_item, sangria_lista, ruta)
      cuerpo <- sub(paste0("^", strrep(" ", nivel_item * 2L)), paste0(pad, "- "), cuerpo)
      out <- c(out, cuerpo)
    } else if (.dl_es_seq(it)) {
      .dl_stop("el valor de \u00ab%s\u00bb no se puede escribir en el manifiesto: es una lista de listas sin nombres",
               ruta %||% "(lista)")
    } else {
      out <- c(out, paste0(pad, "- ", .dl_yaml_escalar(it, ruta), "\n"))
    }
  }
  paste0(out, collapse = "")
}

# Un mapa con nombres en el nivel `nivel` (2 espacios por nivel): «clave: escalar», «clave:» y su mapa o su
# secuencia debajo, «clave: []» para una secuencia vacía y «clave: null» para NULL. `ruta`: la clave del mapa en el
# documento (NULL en el primer nivel), para los mensajes de error.
.dl_yaml_mapa <- function(x, nivel, sangria_lista, ruta = NULL) {
  pad <- strrep(" ", nivel * 2L)
  out <- character(0)
  for (k in names(x)) {
    v <- x[[k]]
    clave <- if (is.null(ruta)) k else paste0(ruta, ".", k)
    if (is.null(v)) {
      out <- c(out, paste0(pad, k, ": null\n"))
    } else if (.dl_es_mapa(v)) {
      out <- c(out, paste0(pad, k, ":\n"), .dl_yaml_mapa(v, nivel + 1L, sangria_lista, clave))
    } else if (.dl_es_seq(v)) {
      if (!length(v)) out <- c(out, paste0(pad, k, ": []\n"))
      else out <- c(out, paste0(pad, k, ":\n"), .dl_yaml_seq(v, nivel, sangria_lista, clave))
    } else {
      linea <- paste0(pad, k, ": ", .dl_yaml_escalar(v, clave), "\n")
      # la línea lleva el salto final: .DL_YAML_ANCHO caracteres más «\n»
      if (nchar(linea) > .DL_YAML_ANCHO + 1L && grepl(" ", .dl_yaml_escalar(v, clave)))
        .dl_stop(paste0("el valor de \u00ab%s\u00bb en el manifiesto no cabe en una l\u00ednea de %d caracteres ",
                        "(tiene espacios): ac\u00f3rtalo"), clave, .DL_YAML_ANCHO)
      out <- c(out, linea)
    }
  }
  paste0(out, collapse = "")
}

# Documento YAML de bloque de `x`, que debe ser un mapa con nombres (por ejemplo, un manifiesto). sangria_lista = 2
# reproduce PyYAML.
.dl_yaml_block <- function(x, sangria_lista = 2L) {
  if (!.dl_es_mapa(x))
    .dl_stop("el manifiesto debe ser una lista con nombres en el primer nivel")
  .dl_yaml_mapa(x, 0L, sangria_lista)
}
