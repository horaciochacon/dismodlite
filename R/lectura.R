# Lectura de archivos: los que trae el paquete instalado (inst/), los YAML, los CSV que da el usuario (con las
# comprobaciones de los formatos que deja Excel), las tablas de estimaciones (un CSV o una carpeta de particiones) y
# los catálogos. Las lecturas repetidas de un mismo archivo se memorizan (.dl_leer_memo).

# ---- Archivos del paquete instalado ----

# Carpeta del paquete instalado: siempre la de system.file(), sin variables de entorno que la redirijan. Con
# pkgload::load_all() (devtools::test()), system.file() da la carpeta inst/ del árbol fuente, que tiene la misma
# estructura que el paquete instalado.
.dl_pkg_dir <- function() {
  d <- system.file(package = "dismodlite")
  if (!nzchar(d))
    .dl_stop("no se encuentra la carpeta del paquete instalado; vuelve a instalar dismodlite")
  normalizePath(d, winslash = "/")
}
# TRUE si la carpeta `ruta`, exista o no, está dentro de la del paquete instalado (o es ella), que no es un lugar
# para escribir. Una ruta que no existe se resuelve desde su antepasado más cercano que existe: normalizePath() no
# resuelve los enlaces ni la ruta relativa de una carpeta que todavía no está.
.dl_en_paquete <- function(ruta) {
  resto <- character()
  while (!file.exists(ruta) && !identical(dirname(ruta), ruta)) {
    resto <- c(basename(ruta), resto)
    ruta <- dirname(ruta)
  }
  ruta <- paste(c(sub("/+$", "", normalizePath(ruta, "/", FALSE)), resto), collapse = "/")
  paquete <- sub("/+$", "", normalizePath(.dl_pkg_dir(), "/"))
  if (.Platform$OS.type == "windows") { ruta <- tolower(ruta); paquete <- tolower(paquete) }
  startsWith(paste0(ruta, "/"), paste0(paquete, "/"))
}
# Archivo o carpeta de inst/ (schema/, perfiles/, cpp/, extdata/) en el paquete instalado.
.dl_inst <- function(...) file.path(.dl_pkg_dir(), ...)
# Lo mismo, con un error en español si el archivo no está (instalación incompleta).
.dl_inst_archivo <- function(...) {
  p <- .dl_inst(...)
  if (!file.exists(p))
    .dl_stop("falta el archivo %s del paquete instalado; vuelve a instalar dismodlite", paste(c(...), collapse = "/"))
  p
}

# ---- Lecturas memorizadas ----

# Lecturas memorizadas por ruta: los esquemas y los catálogos se leen decenas de veces por corrida (insumos, resumen,
# validación, corrida) y no cambian durante ella; las tablas de un proyecto simple, varias veces al leerlo. Una entrada
# por ruta (y `variante`, si la misma ruta se lee de dos maneras), con el md5 del archivo: un archivo con otro
# contenido sustituye su entrada, aunque conserve la fecha de modificación (cp -p, rsync -t, dos cambios en el mismo
# segundo), y la memoria no crece. Las tablas se devuelven copiadas, para que ninguna edición por referencia de quien
# las usa contamine la memoria.
.dl_memo_env <- new.env(parent = emptyenv())
.dl_leer_memo <- function(path, leer, variante = NULL) {
  key <- paste(c(normalizePath(path, winslash = "/", mustWork = FALSE), variante), collapse = "|")
  md5 <- unname(tools::md5sum(path))
  v <- .dl_memo_env[[key]]
  if (is.null(v) || !identical(v$md5, md5)) { v <- list(md5 = md5, valor = leer(path)); .dl_memo_env[[key]] <- v }
  if (data.table::is.data.table(v$valor)) data.table::copy(v$valor) else v$valor
}

# ---- YAML ----

# Lee un YAML en UTF-8 (configuraciones, extracción, esquemas, perfiles, registro, manifiestos). yaml::read_yaml()
# abre el archivo con encoding = "UTF-8" y convierte el texto a la codificación de la sesión: con un locale C
# (contenedores), o uno sin algún carácter del archivo, la conversión falla y el YAML llega truncado.
# readLines(encoding = "UTF-8") solo marca el texto como UTF-8, sin convertirlo; con un locale UTF-8 el resultado es
# el mismo que el de yaml::read_yaml(). `...` va a yaml::yaml.load() (por ejemplo, handlers). Un archivo guardado en
# otra codificación (Windows-1252 al editar un texto con tildes en el Bloc de notas) se detiene con un mensaje en
# español en vez del error del lector de YAML. Un error de sintaxis (el error más común al editar un YAML a mano)
# llega en español, con la línea y la columna que da el lector.
.dl_leer_yaml <- function(path, ...) {
  lineas <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (!all(validUTF8(lineas)))
    .dl_stop(paste0("el archivo \u00ab%s\u00bb no est\u00e1 en UTF-8 (l\u00ednea %d): \u00e1brelo en un editor de ",
                    "texto y gu\u00e1rdalo con codificaci\u00f3n UTF-8.\n  archivo: %s"), basename(path),
             which(!validUTF8(lineas))[1L], path)
  tryCatch(yaml::yaml.load(paste(lineas, collapse = "\n"), ...), error = function(e)
    .dl_stop(paste0("el archivo \u00ab%s\u00bb no es un YAML v\u00e1lido (%s). Revisa la sangr\u00eda (espacios, ",
                    "no tabuladores) y los \u00ab:\u00bb\n  archivo: %s"), basename(path),
             .dl_detalle_yaml(conditionMessage(e)), path))
}

# Detalle de un error del lector de YAML para el mensaje: la línea y la columna, en español, sin el texto en inglés
# del lector cuando lo trae («Scanner error: ... at line 5, column 15» -> «línea 5, columna 15»).
.dl_detalle_yaml <- function(msg) {
  pos <- regmatches(msg, regexec("line ([0-9]+), column ([0-9]+)", msg))[[1]]
  if (length(pos) == 3L) sprintf("l\u00ednea %s, columna %s", pos[2], pos[3]) else "error de sintaxis"
}

# ---- CSV de entrada ----
# Todo CSV que da el usuario pasa por .dl_leer_csv(): se comprueba que el archivo exista y no esté vacío, se lee en
# UTF-8 (el texto queda marcado como UTF-8, sin convertirlo, igual que los YAML en .dl_leer_yaml) y se detectan los
# formatos que deja Excel con configuración regional en español, que de otro modo terminan en errores de tipo poco
# claros más adelante:
#   - «;» como separador de columnas (Excel lo usa cuando la coma es el separador decimal);
#   - números con coma decimal (0,02; -1,5; 4,98E-05) o con separador de miles (1.174.097);
#   - texto que no está en UTF-8 (Windows-1252 o Latin-1);
#   - valores lógicos como VERDADERO/FALSO en vez de TRUE/FALSE;
#   - códigos de departamento sin el cero inicial: Excel lee «01» como el número 1;
#   - columnas vacías sin nombre al final (V31, V32...: celdas con formato fuera de la tabla);
# y, si se le dan los tipos del esquema, las celdas que no se pueden convertir al tipo de su columna.
# Todos los problemas de formato de un archivo van en un solo mensaje, con la forma de corregirlos en Excel o en R.
# Un archivo válido se lee exactamente igual que con data.table::fread(): las comprobaciones solo pueden detenerse.

# Número escrito con coma decimal (0,02; -1,5; 4,98E-05) y número en general (con punto o con coma).
.DL_RX_COMA_DECIMAL <- "^\\s*[-+]?[0-9]+,[0-9]+([eE][-+]?[0-9]+)?\\s*$"
.DL_RX_NUMERO <- "^\\s*[-+]?([0-9]+([.,][0-9]*)?|[.,][0-9]+)([eE][-+]?[0-9]+)?\\s*$"
# Número con separador de miles: al menos dos grupos de tres cifras (1.174.097 o 1,174,097), con decimales o sin
# ellos. Un solo grupo (12.345) no se distingue de un decimal y no se marca.
.DL_RX_MILES <- "^\\s*[-+]?[0-9]{1,3}([.,][0-9]{3}){2,}([.,][0-9]+)?\\s*$"
.DL_RX_CIFRAS <- "^\\s*[-+]?[0-9]+([.,][0-9]+)*\\s*$"
# Lógicos de Excel en español: el primer valor con alguna de estas formas; todos, VERDADERO o FALSO en mayúsculas.
.DL_RX_LOGICO_EXCEL <- "^\\s*(VERDADERO|FALSO|verdadero|falso|Verdadero|Falso)\\s*$"
.DL_LOGICOS_EXCEL <- c("VERDADERO", "FALSO")

# Valores de texto no vacíos de una columna. useBytes en las expresiones: los patrones son ASCII y el texto puede no
# ser UTF-8 válido (un CSV de Excel en español suele venir en Windows-1252).
.dl_valores_texto <- function(v) v[!is.na(v) & grepl("[^[:space:]]", v, useBytes = TRUE)]

# Formato de Excel de una columna de texto `v`: "coma" si sus valores son números y alguno lleva coma decimal,
# "miles" si son cifras con separadores y alguno tiene separador de miles (y no es "coma"), "logico" si son todos
# VERDADERO o FALSO, y "" si nada de eso (o si no tiene valores). Atajos para no recorrer entera una columna de texto
# común (nombres de causa, de ubicación...): el primer valor no vacío (entre los 50 primeros; si no hay ninguno, no
# descarta nada) debe tener la forma buscada, y la coma decimal y el separador de miles exigen que la columna tenga
# alguna coma o algún punto. Los valores no vacíos se calculan una sola vez, y solo si algún atajo no descarta la
# columna.
.dl_formato_excel <- function(v) {
  w <- utils::head(v[!is.na(v)], 50L)
  primero <- w[grepl("[^[:space:]]", w, useBytes = TRUE)][1L]
  empieza <- function(rx) is.na(primero) || grepl(rx, primero, useBytes = TRUE)
  tiene <- function(car) any(grepl(car, v, fixed = TRUE, useBytes = TRUE))
  coma <- empieza(.DL_RX_NUMERO) && tiene(",")
  miles <- empieza(.DL_RX_CIFRAS) && (tiene(",") || tiene("."))
  logico <- empieza(.DL_RX_LOGICO_EXCEL)
  if (!coma && !miles && !logico) return("")
  w <- .dl_valores_texto(v)
  if (!length(w)) return("")
  todos <- function(rx) all(grepl(rx, w, useBytes = TRUE))
  alguno <- function(rx) any(grepl(rx, w, useBytes = TRUE))
  if (coma && todos(.DL_RX_NUMERO) && alguno(.DL_RX_COMA_DECIMAL)) return("coma")
  if (miles && todos(.DL_RX_CIFRAS) && alguno(.DL_RX_MILES)) return("miles")
  if (logico && all(validUTF8(w)) && all(toupper(trimws(w)) %in% .DL_LOGICOS_EXCEL)) return("logico")
  ""
}

# Problemas de formato de una tabla leída de un CSV: en cada elemento, las columnas con ese problema (en `ubigeo`, los
# códigos de departamento mal escritos: las filas de location_level 1 o, en la tabla cov_proxy, que no trae
# location_level, todas; `revisar_ubigeo = FALSE` no los revisa: con anchor.location_id los códigos son libres).
.dl_problemas_csv <- function(dt, tabla = NULL, revisar_ubigeo = TRUE) {
  texto <- names(dt)[vapply(dt, is.character, logical(1))]
  formato <- vapply(texto, function(cn) .dl_formato_excel(dt[[cn]]), "")
  # solo las columnas sin nombre (V31, V32...) pueden ser las vacías que deja Excel
  vacias <- grep("^V[0-9]+$", names(dt), value = TRUE)
  vacias <- vacias[vapply(vacias, function(cn) {
    v <- dt[[cn]]
    all(is.na(v) | (is.character(v) & !grepl("[^[:space:]]", v, useBytes = TRUE)))
  }, logical(1))]
  ubigeo <- character()
  if (revisar_ubigeo && is.character(dt[["location_id"]])) {
    dpto <- if ("location_level" %in% names(dt)) !is.na(dt$location_level) & as.character(dt$location_level) == "1"
            else rep(identical(tabla, "cov_proxy"), nrow(dt))
    ids <- dt$location_id[dpto & !is.na(dt$location_id)]
    ubigeo <- unique(ids[!grepl("^[0-9]{2}$", ids)])
  }
  list(no_utf8 = texto[vapply(texto, function(cn) !all(validUTF8(dt[[cn]])), logical(1))],
       coma = texto[formato == "coma"], miles = texto[formato == "miles"], logicos = texto[formato == "logico"],
       vacias = vacias, ubigeo = ubigeo)
}

# Mensaje con todos los problemas de un CSV, qué hacer en Excel y la receta en R para ese archivo. `punto_y_coma`:
# el archivo usa «;» como separador (la tabla `dt` se leyó con sep = ";" y todo como texto).
.dl_mensaje_csv <- function(archivo, dt, pr, punto_y_coma = FALSE) {
  cols <- function(x) paste(x, collapse = ", ")
  # primer valor de la columna que cumple `rx`, como ejemplo
  ej <- function(cn, rx) {
    v <- dt[[cn]]
    sprintf(" (p. ej. \u00ab%s\u00bb)", trimws(v[!is.na(v) & grepl(rx, v, useBytes = TRUE)][1L]))
  }
  q <- function(x) paste(sprintf("\"%s\"", x), collapse = ", ")
  sin_cero <- any(grepl("^[0-9]$", pr$ubigeo))
  items <- c(
    if (punto_y_coma) "usa \u00ab;\u00bb como separador de columnas; el paquete lee CSV separados por comas",
    if (length(pr$coma))
      sprintf("tiene n\u00fameros con coma decimal en la(s) columna(s) %s%s; el paquete espera punto decimal",
              cols(pr$coma), ej(pr$coma[1L], .DL_RX_COMA_DECIMAL)),
    if (length(pr$miles))
      sprintf("tiene n\u00fameros con separador de miles en la(s) columna(s) %s%s; escr\u00edbelos sin separadores",
              cols(pr$miles), ej(pr$miles[1L], .DL_RX_MILES)),
    if (length(pr$no_utf8))
      sprintf("no est\u00e1 en UTF-8 (columna(s) %s): parece Windows-1252 o Latin-1", cols(pr$no_utf8)),
    if (length(pr$logicos))
      sprintf("la(s) columna(s) %s tiene(n) VERDADERO/FALSO; el paquete espera TRUE/FALSE", cols(pr$logicos)),
    if (length(pr$ubigeo))
      sprintf(paste0("la columna location_id tiene c\u00f3digos de departamento que no son de dos d\u00edgitos: %s. ",
                     "Se esperan los del ubigeo, de \u00ab01\u00bb a \u00ab25\u00bb%s"),
              paste0("\u00ab", utils::head(pr$ubigeo, 5L), "\u00bb", collapse = ", "),
              if (sin_cero) paste0(": al abrir el CSV, Excel convierte \u00ab01\u00bb en el n\u00famero 1 y pierde el ",
                                   "cero inicial") else ""),
    if (length(pr$vacias))
      sprintf(paste0("tiene columnas vac\u00edas sin nombre al final (%s): Excel las deja cuando hubo celdas con ",
                     "formato fuera de la tabla; b\u00f3rralas"), cols(pr$vacias)))
  de_excel <- punto_y_coma || length(pr$coma) || length(pr$no_utf8)
  excel <- if (de_excel)
    paste0("En Excel: Archivo > Opciones > Avanzadas, desmarca \u00abUsar separadores del sistema\u00bb y pon ",
           "\u00ab.\u00bb como separador decimal (o cambia el separador de listas a \u00ab,\u00bb en la ",
           "configuraci\u00f3n regional de Windows) y guarda como \u00abCSV UTF-8 (delimitado por comas)\u00bb.")
  # receta en R para este archivo, solo con lo que hace falta; los separadores de miles se corrigen a mano
  ruta <- q(gsub("\\", "/", archivo, fixed = TRUE))
  args_leer <- c(ruta, if (punto_y_coma) "sep = \";\"", if (punto_y_coma && length(pr$coma)) "dec = \",\"",
                 if (length(pr$no_utf8)) "encoding = \"Latin-1\"",
                 if ("location_id" %in% names(dt)) "colClasses = list(character = \"location_id\")")
  coma_texto <- if (punto_y_coma) character() else pr$coma
  dpto <- if ("location_level" %in% names(dt)) "location_level == 1" else ""
  receta <- c(
    sprintf("d <- data.table::fread(%s)", paste(args_leer, collapse = ", ")),
    if (length(coma_texto))
      sprintf("for (v in c(%s)) data.table::set(d, j = v, value = as.numeric(sub(\",\", \".\", d[[v]], fixed = TRUE)))",
              q(coma_texto)),
    if (length(pr$logicos))
      sprintf("for (v in c(%s)) data.table::set(d, j = v, value = toupper(d[[v]]) == \"VERDADERO\")", q(pr$logicos)),
    if (length(pr$ubigeo)) sprintf("d[%s, location_id := sprintf(\"%%02d\", as.integer(location_id))]", dpto),
    if (length(pr$vacias)) sprintf("d[, c(%s) := NULL]", q(pr$vacias)),
    sprintf("data.table::fwrite(d, %s%s)", ruta, if (length(pr$no_utf8)) ", encoding = \"UTF-8\"" else ""))
  paste0("el archivo \u00ab", basename(archivo), "\u00bb",
         if (de_excel) " parece guardado desde Excel con configuraci\u00f3n regional en espa\u00f1ol" else "", ":\n",
         paste0("  - ", items, collapse = "\n"),
         if (!is.null(excel)) paste0("\n", excel) else "",
         if (!length(pr$miles)) paste0("\nPara corregirlo en R:\n", paste0("  ", receta, collapse = "\n")) else "",
         "\n  archivo: ", archivo)
}

# «;» como separador: la cabecera tiene más «;» que comas. Antes de leer, para no devolver columnas pegadas; el
# archivo se lee con sep = ";" y todo como texto para informar de todos sus problemas a la vez.
.dl_chequear_separador <- function(archivo, tabla = NULL) {
  cab <- readLines(archivo, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(cab)) return(invisible())
  n_pc <- sum(charToRaw(cab) == charToRaw(";"))
  n_c <- sum(charToRaw(cab) == charToRaw(","))
  if (n_pc == 0L || n_pc < n_c) return(invisible())
  d <- tryCatch(suppressWarnings(data.table::fread(archivo, sep = ";", colClasses = "character", encoding = "UTF-8",
                                                   na.strings = "")),
                error = function(e) data.table::data.table())
  .dl_stop(.dl_mensaje_csv(archivo, d, .dl_problemas_csv(d, tabla), punto_y_coma = TRUE))
}

# El archivo existe, no es una carpeta y no está vacío. `pieza`: argumento de dl_rutas() de donde viene la ruta;
# `en_carpeta`: el archivo va dentro de la carpeta de esa pieza (registro/sequela_rei.csv) y no es la pieza misma.
.dl_chequear_archivo <- function(archivo, pieza = NULL, en_carpeta = FALSE) {
  de <- if (is.null(pieza)) sprintf("\u00ab%s\u00bb", basename(archivo))
        else if (en_carpeta) sprintf("\u00ab%s\u00bb de la carpeta de `%s`", basename(archivo), pieza)
        else sprintf("de `%s` (\u00ab%s\u00bb)", pieza, basename(archivo))
  if (dir.exists(archivo))
    .dl_stop("el archivo %s es una carpeta; se espera un archivo CSV.\n  ruta: %s", de, archivo)
  if (!file.exists(archivo))
    .dl_stop("no existe el archivo %s.\n  ruta: %s", de, archivo)
  if (isTRUE(file.size(archivo) == 0))
    .dl_stop("el archivo %s est\u00e1 vac\u00edo.\n  ruta: %s", de, archivo)
  invisible(archivo)
}

# Lee un CSV de entrada. `...` va a data.table::fread() (colClasses, select, ...). `tipos`: el tipo del esquema de
# las columnas que se van a usar (como en .dl_tipos_columnas()); una columna que debe ser de números, enteros o
# lógicos y llegó como texto (fread deja la columna entera como texto si una celda no se puede convertir) detiene la
# lectura mostrando esas celdas, en vez de dejar que la validación diga solo «tipo esperado num». La tabla leída no
# cambia. `tabla` nombra la tabla en ese mensaje (por defecto, la pieza); con cov_proxy, los códigos de departamento
# se revisan en todas las filas (.dl_problemas_csv). `pieza` y `en_carpeta` solo sirven para los mensajes (ver
# .dl_chequear_archivo). Los avisos de fread se retienen hasta pasar las comprobaciones: si el archivo tiene un
# problema conocido, el error en español lo explica; si no, se emiten con la función del usuario delante (en español
# el de la última línea descartada; los demás, con el texto de fread). `ubigeo`: ver .dl_problemas_csv().
.dl_leer_csv <- function(archivo, ..., tipos = NULL, tabla = pieza, pieza = NULL, en_carpeta = FALSE, ubigeo = TRUE) {
  .dl_chequear_archivo(archivo, pieza, en_carpeta)
  .dl_chequear_separador(archivo, tabla)
  avisos <- character()
  dt <- withCallingHandlers(data.table::fread(archivo, encoding = "UTF-8", ...), warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  pr <- .dl_problemas_csv(dt, tabla, ubigeo)
  if (any(lengths(pr) > 0L)) .dl_stop(.dl_mensaje_csv(archivo, dt, pr))
  for (cn in intersect(names(tipos), names(dt)))
    if (is.character(dt[[cn]]) && tipos[[cn]] != "str") .dl_coaccionar(dt[[cn]], tipos[[cn]], cn, tabla)
  for (a in avisos) {
    if (grepl("Discarded single-line footer", a, fixed = TRUE))
      .dl_warn(paste0("se descart\u00f3 la \u00faltima l\u00ednea de \u00ab%s\u00bb porque no tiene el n\u00famero ",
                      "de columnas de la tabla: %s"), basename(archivo), sub("^.*footer: ?", "", a))
    else .dl_warn("data.table::fread() avis\u00f3 al leer \u00ab%s\u00bb: %s", basename(archivo), a)
  }
  dt
}

# ---- Tablas de estimaciones ----

# Estado de cada adquisición registrada (acquisition_id -> status) en datasets.yaml de la carpeta `registro`; NULL
# sin registro.
.dl_registry_status <- function(registry) {
  ds <- .dl_registry_leer(registry)
  if (is.null(ds)) return(NULL)
  ds <- Filter(function(d) !is.null(d$acquisition_id), ds)
  stats::setNames(vapply(ds, function(d) d$status %||% "active", ""), vapply(ds, function(d) d$acquisition_id, ""))
}

# Lee una tabla de estimaciones: un CSV o una carpeta de particiones (un CSV por adquisición y parte:
# <acquisition_id>[.<parte>].csv). Con `registry` (la carpeta `registro` de dl_rutas()), el CSV de una adquisición
# registrada cuyo estado no es «active» queda fuera; una adquisición sin registrar entra. La carpeta puede conservar
# el archivo de una adquisición sustituida y, sin este filtro, una nueva descarga que cubre los mismos años
# duplicaría la clave del ancla.
# `subruta`: con una carpeta raíz, se baja recursivamente y solo entran los CSV cuya ruta contiene ese fragmento
# (population/population): qué ronda de la población entra lo decide el registro, no la ruta.
# `select`: columnas a leer (todas por defecto); el CSV del ancla es grande y a veces solo hace falta una.
# `tipos`: tipos de las columnas que se van a usar, comprobados en cada CSV (ver .dl_leer_csv); `ubigeo`: ver
# .dl_problemas_csv().
.dl_leer_std <- function(archivo, registry = NULL, subruta = NULL, select = NULL, pieza = NULL, tipos = NULL,
                         ubigeo = TRUE) {
  leer <- function(f) .dl_leer_csv(f, colClasses = list(character = "location_id"), select = select, tipos = tipos,
                                   pieza = pieza, en_carpeta = dir.exists(archivo), ubigeo = ubigeo)
  if (dir.exists(archivo)) {
    de <- if (is.null(pieza)) sprintf("la carpeta \u00ab%s\u00bb", basename(archivo))
          else sprintf("la carpeta de `%s`", pieza)
    archivos <- list.files(archivo, pattern = "[.]csv$", full.names = TRUE, recursive = !is.null(subruta))
    if (!is.null(subruta)) archivos <- archivos[grepl(subruta, archivos, fixed = TRUE)]
    if (!length(archivos))
      .dl_stop(paste0("%s no tiene archivos CSV%s; se espera un archivo CSV o una carpeta con un CSV por ",
                      "adquisici\u00f3n.\n  ruta: %s"), de,
               if (!is.null(subruta)) sprintf(" bajo \u00ab%s\u00bb", subruta) else "", archivo)
    st <- .dl_registry_status(registry)
    if (!is.null(st)) {
      ids <- sub("[.].*$", "", basename(archivos))          # <acquisition_id>[.<parte>].csv
      fuera <- ids %in% names(st) & st[ids] != "active"
      if (any(fuera))
        .dl_message(paste0("%d archivo(s) de \u00ab%s\u00bb quedan fuera porque su adquisici\u00f3n no est\u00e1 ",
                           "activa en el registro (datasets.yaml): %s"), sum(fuera), basename(archivo),
                    paste(ids[fuera], collapse = ", "))
      archivos <- archivos[!fuera]
      if (!length(archivos))
        .dl_stop("%s no tiene archivos CSV de adquisiciones activas en el registro (datasets.yaml).\n  ruta: %s",
                 de, archivo)
    }
    return(data.table::rbindlist(lapply(archivos, leer), fill = TRUE))
  }
  leer(archivo)
}

# ---- Catálogos ----

# Catálogos (causas, ubicaciones, demográficos...) de la carpeta `catalogos`: los nombres de archivo salen del
# contrato estimates/v1 (inst/schema/estimates.v1.yaml), no se repiten aquí. El ubigeo de dos dígitos se exige según
# la regla de la configuración, que llega con las rutas (.dl_codigos_ubigeo_rutas).
.dl_catalogo <- function(paths, nombre) {
  archivo <- .dl_schema_estimates()$catalogos[[nombre]]
  if (is.null(archivo))
    .dl_stop("el contrato estimates/v1 no define el cat\u00e1logo \u00ab%s\u00bb", nombre)
  ubigeo <- .dl_codigos_ubigeo_rutas(paths)
  .dl_leer_memo(file.path(.dl_path(paths, "catalogos"), archivo), variante = if (!ubigeo) "codigos_libres",
                function(p) .dl_leer_csv(p, colClasses = if (nombre == "locations") list(character = "location_id"),
                                         pieza = "catalogos", en_carpeta = TRUE, ubigeo = ubigeo))
}

# Bandas de edad GBD (id -> [age_start, age_end)) del catálogo demográfico de `paths` o del ya leído `d` (el de
# inst/referencia, en el formato simple).
.dl_bandas_catalogo <- function(paths, d = .dl_catalogo(paths, "demograficos")) {
  d <- d[tabla == "age_group"]
  out <- data.table::data.table(age_group_id = as.integer(d$id),
                                age_start = suppressWarnings(as.numeric(d$age_start)),
                                age_end = suppressWarnings(as.numeric(d$age_end)))
  out[!is.na(age_start) & !is.na(age_end)]
}
