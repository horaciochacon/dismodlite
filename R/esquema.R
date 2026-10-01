# Contrato de las tablas de insumos (dismod_lite/v1, inst/schema/dismod_lite.v1.yaml): dl_esquema() lo lee y
# comprueba que esté bien formado; dl_esquema_tabla() da la definición de una tabla. Aquí viven también los tipos de
# columna del esquema (cómo se reconocen, se convierten y se nombran) y el vocabulario único del paquete (medidas,
# ubicación del ancla, tipos de dato de la verosimilitud).
.dl_schema_default_path <- function() .dl_inst_archivo("schema", "dismod_lite.v1.yaml")

#' Esquema de las tablas de insumos
#'
#' Lee y valida el contrato `dismod_lite/v1`: las tablas de insumos, sus columnas, tipos, claves y reglas.
#'
#' @details
#' El contrato es el formato de las tablas internas del paquete, las del formato completo y las que
#' [dl_insumos()] arma desde un proyecto simple (y [dl_congelar_insumos()] escribe en cada corrida). Cada tabla
#' declara sus columnas con su tipo (`int`, `num`, `str`, `lgl`, o una lista cerrada de valores) y si son
#' opcionales, su clave (las columnas que identifican una fila) y sus reglas, que [dl_validar_tabla()] comprueba. El
#' archivo del paquete está en `system.file("schema", "dismod_lite.v1.yaml", package = "dismodlite")`.
#'
#' @param archivo Archivo YAML del esquema; `NULL` usa el que trae el paquete.
#' @return Objeto de clase `dl_schema`: la lista del YAML, con `schema` (`"dismod_lite/v1"`), los vocabularios
#'   (`tipos_dato`, `parametros_objetivo`, `transformaciones`, `roles_secuela`) y `tablas` (una entrada por tabla,
#'   como la de [dl_esquema_tabla()]).
#' @seealso [dl_esquema_tabla()], [dl_validar_tabla()].
#' @family avanzado
#' @examples
#' esquema <- dl_esquema()
#' esquema
#' names(esquema$tablas)
#' @export
dl_esquema <- function(archivo = NULL) {
  if (!is.null(archivo) && (!.dl_es_texto1(archivo) || !file.exists(archivo) || dir.exists(archivo)))
    .dl_stop("`archivo` debe ser la ruta de un archivo YAML de esquema que existe; es %s",
             .dl_describir_objeto(archivo))
  archivo <- archivo %||% .dl_schema_default_path()
  sch <- .dl_leer_memo(archivo, .dl_leer_yaml)
  mal <- function(detalle) .dl_stop("el esquema \u00ab%s\u00bb no es v\u00e1lido: %s", basename(archivo), detalle)
  if (!identical(sch$schema, "dismod_lite/v1")) mal("el encabezado debe ser \u00abschema: dismod_lite/v1\u00bb")
  if (!length(sch$tablas)) mal("no define ninguna tabla")
  tipos_ok <- names(.DL_TIPOS_ESQUEMA)
  for (nm in names(sch$tablas)) {
    t <- sch$tablas[[nm]]
    if (!length(t$columnas)) mal(sprintf("la tabla %s no tiene columnas", nm))
    if (!length(t$clave))    mal(sprintf("la tabla %s no tiene clave", nm))
    if (!all(t$clave %in% names(t$columnas)))
      mal(sprintf("la clave de la tabla %s nombra columnas que no existen", nm))
    for (cn in names(t$columnas)) {
      cd <- t$columnas[[cn]]
      tipo <- .dl_tipo_columna(cd)
      if (!tipo %in% c(tipos_ok, "enum"))
        mal(sprintf("la columna %s de la tabla %s tiene un tipo desconocido \u00ab%s\u00bb (tipos: %s)",
                    cn, nm, tipo, paste(c(tipos_ok, "enum"), collapse = ", ")))
      if (identical(tipo, "enum") && !length(.dl_valores_enum(sch, cd)))
        mal(sprintf("la columna %s de la tabla %s tiene un enum vac\u00edo o un vocabulario que no existe", cn, nm))
    }
    if (!is.null(t$unicidad_semantica) && !all(t$unicidad_semantica %in% names(t$columnas)))
      mal(sprintf("unicidad_semantica de la tabla %s nombra columnas que no existen", nm))
  }
  structure(sch, class = "dl_schema")
}

#' Definición de una tabla del esquema
#'
#' La definición de una tabla del contrato `dismod_lite/v1`: sus columnas, su clave y sus reglas.
#'
#' @param esquema Esquema devuelto por [dl_esquema()].
#' @param nombre Nombre de la tabla (por ejemplo `"datos"`).
#' @return Lista con `columnas` (el tipo de cada columna: un texto como `"int"`, o una lista con `tipo` u `enum` y
#'   `opcional`), `clave` (las columnas que identifican una fila) y, si las declara, `unicidad_semantica` (las
#'   columnas que no pueden repetirse juntas) y `reglas` (los nombres de las reglas que cruzan columnas o tablas).
#' @seealso [dl_esquema()], [dl_validar_tabla()].
#' @family avanzado
#' @examples
#' datos <- dl_esquema_tabla(dl_esquema(), "datos")
#' names(datos$columnas)
#' datos$clave
#' datos$reglas
#' @export
dl_esquema_tabla <- function(esquema, nombre) {
  .dl_exigir_clase(esquema, "dl_schema", "esquema", "dl_esquema()")
  if (!.dl_es_texto1(nombre))
    .dl_stop("el nombre de la tabla debe ser un texto (por ejemplo \"datos\"); es %s", .dl_describir_objeto(nombre))
  if (!nombre %in% names(esquema$tablas))
    .dl_stop("tabla desconocida \u00ab%s\u00bb; las tablas del esquema son: %s",
             nombre, paste(names(esquema$tablas), collapse = ", "))
  esquema$tablas[[nombre]]
}

#' @export
print.dl_schema <- function(x, ...) {
  cat(sprintf("<dl_schema> esquema %s | %d tablas: %s\n", format(x$schema), length(x$tablas),
              paste(names(x$tablas), collapse = ", ")))
  invisible(x)
}

# ---- Tipos de columna del esquema ----

# Los cuatro tipos de columna del esquema (un `enum` es texto con valores admitidos), en una sola tabla:
#   es                   la columna tiene el tipo (un entero puede llegar como número sin decimales)
#   convertir            convierte una columna al tipo
#   vacio                columna vacía del tipo
#   prosa, prosa_plural  el tipo en los mensajes («tipo esperado num (número)», «valores que no son números»)
.DL_TIPOS_ESQUEMA <- list(
  str = list(es = is.character, convertir = as.character, vacio = character(),
             prosa = "texto", prosa_plural = "texto"),
  int = list(es = function(v) is.integer(v) || (is.numeric(v) && all(v == as.integer(v), na.rm = TRUE)),
             convertir = as.integer, vacio = integer(), prosa = "entero", prosa_plural = "enteros"),
  num = list(es = is.numeric, convertir = as.numeric, vacio = numeric(),
             prosa = "n\u00famero", prosa_plural = "n\u00fameros"),
  lgl = list(es = is.logical, convertir = as.logical, vacio = logical(),
             prosa = "l\u00f3gico, TRUE/FALSE", prosa_plural = "l\u00f3gicos (TRUE/FALSE)"))

# Tipo de una columna a partir de su definición en el esquema (`int`, o una lista con `tipo` o con `enum`): "str",
# "int", "num", "lgl" o "enum".
.dl_tipo_columna <- function(cd) if (is.character(cd)) cd else if (!is.null(cd$enum)) "enum" else cd$tipo

# Valores admitidos de una columna enum: la lista escrita en la columna o el vocabulario del esquema que nombra.
.dl_valores_enum <- function(sch, cd) if (is.character(cd$enum) && length(cd$enum) == 1L) sch[[cd$enum]] else cd$enum

# Tipo de cada columna de una tabla, para leerla o convertirla (un enum es texto).
.dl_tipos_columnas <- function(sch, tabla) {
  tipos <- vapply(dl_esquema_tabla(sch, tabla)$columnas, .dl_tipo_columna, "")
  tipos[tipos == "enum"] <- "str"
  tipos
}

# Tabla vacía con las columnas y los tipos del esquema.
.dl_tabla_vacia <- function(sch, tabla)
  data.table::as.data.table(lapply(.dl_tipos_columnas(sch, tabla), function(tipo) .DL_TIPOS_ESQUEMA[[tipo]]$vacio))

# Convierte `v` al tipo `tipo` y se detiene si un valor no vacío no se puede convertir, mostrándolo (sin esto quedaría
# NA y la validación diría «NA en columna obligatoria» sin mostrar el valor). Es la única comprobación de conversión:
# la usan la lectura de un CSV con tipos (.dl_leer_csv) y la conversión de una tabla a los tipos del esquema.
.dl_coaccionar <- function(v, tipo, columna, tabla) {
  nuevo <- suppressWarnings(.DL_TIPOS_ESQUEMA[[tipo]]$convertir(v))
  malos <- is.na(nuevo) & !is.na(v) & grepl("[^[:space:]]", as.character(v), useBytes = TRUE)
  if (any(malos))
    .dl_stop("la columna %s de la tabla %s tiene valores que no son %s: %s", columna, tabla,
             .DL_TIPOS_ESQUEMA[[tipo]]$prosa_plural,
             paste0("\u00ab", utils::head(unique(as.character(v[malos])), 3L), "\u00bb", collapse = ", "))
  nuevo
}

# Convierte cada columna presente al tipo que declara el esquema. fread() lee como lógica una columna sin ningún
# valor: una tabla `datos` hecha de tasas de GBD deja vacías n_efectivo y completitud (una tasa no tiene tamaño
# muestral efectivo ni factor de completitud), y sin esta conversión la validación rechazaría la tabla entera.
# Derivarlo del esquema hace que una columna nueva del contrato quede cubierta sin tocar este código.
.dl_coaccionar_schema <- function(d, sch, tabla) {
  tipos <- .dl_tipos_columnas(sch, tabla)
  for (cn in intersect(names(tipos), names(d))) d[[cn]] <- .dl_coaccionar(d[[cn]], tipos[[cn]], cn, tabla)
  d
}

# ---- Vocabulario único del paquete: una fila o entrada por concepto ----

# Medidas del ancla y de la salida. Agregar una medida = agregar una fila aquí.
#   slug, nombre_es           nombre interno de la medida y su nombre en español
#   measure_id                id GBD de la medida (900006 = csmr, propio del paquete)
#   measure_id_gbd            su measure_id en las descargas de GBD Results (el csmr es la tasa de muertes, 1)
#   metric_std, escala_std    métrica y escala con que se leen de las estimaciones GBD: Rate, por 100 000, en todas
#                             las medidas (se divide por escala_std). La prevalencia también es Rate: su Percent no es
#                             la proporción de la población (ver .dl_metrica_std)
#   subdir_std                carpeta de sus particiones bajo DATA_ROOT/std/gbd/<ronda>/cause/
#   path_key                  clave interna de su ruta en dl_rutas()
#   metric_id_out, escala_out métrica y escala con que se escriben en la corrida
#   exporta                   si la corrida la escribe
#   measure_name              su measure_name en la tabla datos
.DL_MEDIDAS <- data.table::data.table(
  slug           = c("prevalence", "incidence", "yld", "csmr"),
  nombre_es      = c("prevalencia", "incidencia", "AVD", "mortalidad"),
  measure_id     = c(5L, 6L, 3L, 900006L),
  measure_id_gbd = c(5L, 6L, 3L, 1L),
  metric_std     = c("Rate", "Rate", "Rate", "Rate"),
  escala_std     = c(1e5, 1e5, 1e5, 1e5),
  subdir_std     = c("prevalence", "incidence", "yld", "death"),
  path_key       = c("std_prior", "std_incidence", "std_yld", "std_csmr"),
  metric_id_out  = c(2L, 3L, 3L, 3L),
  escala_out     = c(1, 1e5, 1e5, 1e5),
  exporta        = c(TRUE, TRUE, TRUE, FALSE),
  measure_name   = c("Prevalence", "Incidence", "YLDs (Years Lived with Disability)", "csmr"))
# Medidas que escribe una corrida (exporta = TRUE): las de cause/<medida>/ y draws/.
.DL_MEDIDAS_EXPORTA <- .DL_MEDIDAS[.DL_MEDIDAS$exporta]
# Fuente y ronda de las estimaciones GBD bajo DATA_ROOT (DATA_ROOT/std/gbd/2023/...).
.DL_STD_SOURCE <- "gbd"
.DL_STD_ROUND  <- "2023"

.dl_medida <- function(s) {
  i <- which(.DL_MEDIDAS[["slug"]] == s)     # sin DT[]: dentro de DT[] la columna `slug` taparía el argumento
  if (length(i) != 1L)
    .dl_stop("medida desconocida \u00ab%s\u00bb; las medidas son: %s", s, paste(.DL_MEDIDAS$slug, collapse = ", "))
  .DL_MEDIDAS[i]
}
.dl_medida_id <- function(slug) .dl_medida(slug)$measure_id

# Métrica y escala con que se lee una medida del ancla (fila de .DL_MEDIDAS): las de la tabla. La prevalencia se lee
# en Rate / 100 000: casos sobre la población. En GBD Results su Percent no es esa proporción: divide los casos de la
# causa por las personas con alguna causa (la prevalencia de todas las causas), no por la población, así que la
# supera en la inversa de esa prevalencia (en Perú 2023: nada desde los 75 años, menos de 1 % entre los 20 y los 60,
# de 1,5 % a 6 % entre los 2 y los 19 y de 22 % a 37 % antes de los 2). Las versiones hasta la 1.0.0 leían Percent;
# anchor.metrica_prevalencia: {valor: Percent, procedencia} lo repite para reproducir sus corridas.
.DL_METRICA_PREVALENCIA_ANTERIOR <- list(metric_std = "Percent", escala_std = 1)
.dl_metrica_std <- function(med, cfg) {
  if (identical(med$slug, "prevalence") && identical(cfg$anchor$metrica_prevalencia$valor, "Percent"))
    return(.DL_METRICA_PREVALENCIA_ANTERIOR)
  list(metric_std = med$metric_std, escala_std = med$escala_std)
}

# Ubicación del ancla (location_id, como texto): anchor.location_id de la configuración (el location_id de GBD del
# país; lo trae toda configuración simple) o, en el formato completo de la versión 0.2.2, anchor.location: peru = 123
# (Perú); region = 120 (reservado).
.DL_LOC_ANCLA <- c(peru = "123", region = "120")
.dl_loc_ancla <- function(cfg) {
  if (!is.null(cfg$anchor$location_id)) return(as.character(as.integer(cfg$anchor$location_id)))
  id <- .DL_LOC_ANCLA[cfg$anchor$location]
  if (is.na(id))
    .dl_stop("anchor.location debe ser %s", paste(names(.DL_LOC_ANCLA), collapse = " o "))
  unname(id)
}

# Códigos subnacionales de dos dígitos (el ubigeo de los departamentos del Perú): solo se exigen con el ancla de la
# versión 0.2.2 (anchor.location); con anchor.location_id los códigos son libres. Es la única regla: los lectores que
# no reciben la configuración (los catálogos) la toman de las rutas, que dl_proyecto() y dl_insumos() marcan con el
# atributo codigos_libres.
.dl_codigos_ubigeo <- function(cfg) is.null(cfg$anchor$location_id)
.dl_rutas_con_codigos <- function(rutas, cfg) {
  if (!.dl_codigos_ubigeo(cfg)) attr(rutas, "codigos_libres") <- TRUE
  rutas
}
.dl_codigos_ubigeo_rutas <- function(rutas) !isTRUE(attr(rutas, "codigos_libres"))

# Tipos de dato de la verosimilitud (notación: ver R/edo.R): el integrando anual que se compara con el dato, la
# familia de su ruta de conteos (x de n) y su measure_id. Agregar un tipo = agregar una fila aquí y su rama en
# inst/cpp/dl_core.cpp (la prueba de paridad R/C++ lo vigila).
#   p     prevalencia                               p(a)
#   ipop  incidencia poblacional                    i(a) (1 - p(a))
#   pf    mortalidad específica por la causa (csmr)  p(a) f(a)
.DL_TIPOS_DATO <- data.table::data.table(
  tipo            = c("prev_estudio", "incidencia", "csmr"),
  codigo          = 1:3,
  integrando      = c("p", "ipop", "pf"),
  familia_conteos = c("binomial", "poisson", "poisson"),
  measure_id      = c(5L, 6L, 900006L))

# Integrando `nombre` de .DL_TIPOS_DATO evaluado sobre una solución de la EDO (`sol`: p, i, f en las edades
# enteras). Gemela en C++: integrando() en inst/cpp/dl_core.cpp.
.dl_integrando <- function(sol, nombre) switch(nombre,
  p = sol$p, ipop = sol$i * (1 - sol$p), pf = sol$p * sol$f,
  .dl_stop("integrando desconocido \u00ab%s\u00bb", nombre))
