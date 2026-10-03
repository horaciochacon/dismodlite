# Datos sintéticos de ejemplo: la arteriopatía crónica sintética (ACS, causa 9100) y sus subtipos 9101-9103, en
# Perú y sus 25 departamentos, en dos formatos con los mismos números (salvo los proxies departamentales: una encuesta
# que se calibra al leer en el primero, ya calibrados en el segundo): inst/extdata/acs_peru (el proyecto en el
# contrato de insumos, el que se copia para empezar uno) e inst/extdata/acs_peru_completo (el formato completo de la
# versión 0.2.2). Todos los números son sintéticos.

# Causas y años que cubren los datos de ejemplo.
.DL_CAUSAS_EJEMPLO <- 9100:9103
.DL_ANIOS_EJEMPLO <- c(2019L, 2023L, 2024L)
# Año de la configuración de las causas del ejemplo (`anio` en config/<causa>.yaml) y el último con ancla y valor
# nacional de las covariables.
.DL_ANIO_CONFIG_EJEMPLO <- 2023L
# Carpeta de cada formato del ejemplo (en inst/extdata) y corrida de partición de severidad del formato completo
# (particion/<corrida>/).
.DL_CARPETAS_EJEMPLO <- c(simple = "acs_peru", completo = "acs_peru_completo")
.DL_PARTICION_EJEMPLO <- "acs_v1"

#' Datos de ejemplo del paquete
#'
#' Ruta a la carpeta del proyecto de ejemplo o a un archivo dentro de ella. El ejemplo es un proyecto con las tablas
#' del contrato de insumos ([dl_tablas]), listo para leer con [dl_proyecto()] y para copiar como punto de partida de
#' uno propio: con `copiar_en`, `dl_ejemplo()` lo copia en una carpeta nueva, donde se puede editar. Todos los números
#' son sintéticos.
#'
#' La enfermedad es ficticia: una arteriopatía crónica (la causa 9100) que es la suma de tres subtipos (9101, 9102 y
#' 9103), con estimaciones de referencia de 2019 y 2023 y población de 2019, 2023 y 2024. La geografía es la de un
#' país real, Perú (código 123, su `location_id` de GBD), con sus 25 departamentos (códigos 01 a 25), pero ningún
#' número describe a ese país ni a sus departamentos.
#'
#' @section Las dos carpetas del ejemplo:
#' El ejemplo viene en dos carpetas, una por formato, con los mismos números salvo los proxies departamentales. La de
#' `dl_ejemplo()` es `acs_peru`, con las tablas del contrato ([dl_tablas]):
#' - `config/9100.yaml` ... `config/9103.yaml`: la configuración de cada causa; la de 9100 declara sus subtipos.
#' - `ubicaciones.csv`: el país (código 123, sin padre) y sus 25 departamentos.
#' - `poblacion.csv`: la población de los departamentos por año, sexo y grupo de edad, sin las filas nacionales (el
#'   paquete las calcula como su suma).
#' - `ancla/sintetico_acs_v1.csv`: una descarga de GBD Results tal cual, con la prevalencia, la incidencia, las
#'   muertes y los AVD de las cuatro causas, por edad y sexo, de 2019 y 2023.
#' - `covariables/`: tres descargas del GHDx tal cual (SEV, LDI y HAQ), con los valores nacionales y, como en una
#'   descarga real, los globales y los regionales, que no se usan.
#' - `proxies_crudos.csv`: una encuesta de hogares sintética con tres ediciones (2019, 2021 y 2023) que mide un
#'   indicador de cada covariable por departamento (el del SEV por sexo y grupo de edad), con su error estándar.
#'   [dl_proyecto()] la calibra al leer ([dl_calibrar_proxies()]): da las filas departamentales de las tres
#'   covariables en el año que se estima, cuyo promedio ponderado por la población es el valor nacional. La
#'   configuración declara el HAQ en `diferencia` (`proxies.transformacion`).
#' - `betas.csv`: las betas de las tres covariables, de la causa 9100 (los subtipos usan las de su causa padre).
#' - `datos.csv` (solo de la causa 9100): filas nacionales de 2023 (mortalidad, un estudio de prevalencia en casos y
#'   muestra, una cohorte de incidencia y un valor atípico excluido) y filas departamentales de 2019 (mortalidad y
#'   prevalencia), reservadas para validar.
#' - `severidad.csv`: tres estados de salud por causa, con su proporción y su peso de discapacidad.
#' - `LEEME.md`: la descripción de la carpeta.
#' - `verdad.csv`: no es un insumo. Las curvas con que se generaron los datos, para comparar con las estimaciones: la
#'   prevalencia `p`, la incidencia `i` y la mortalidad en exceso `f` (por persona-año) por edad simple, de 30 a 99
#'   años, de cada causa (`cause_id`), ubicación nacional y departamental (`location_id`), sexo (`sex_id`) y año
#'   (`anio`, 2019 y 2023).
#'
#' La otra, `acs_peru_completo`, es el mismo proyecto en el formato completo (el de la versión 0.2.2), con los
#' proxies departamentales ya calibrados en lugar de la encuesta, el YAML de extracción, el almacén de evidencia, el
#' registro, los catálogos, una tabla de severidad por causa y una partición de severidad. Su ruta es
#' `system.file("extdata", "acs_peru_completo", package = "dismodlite")`.
#'
#' Las columnas de cada tabla están en [dl_tablas], la carpeta en [dl_proyecto()] y las claves de la configuración en
#' [dl_configuracion()].
#' Con [dl_rutas_ejemplo()] y [dl_configuracion_ejemplo()] se toman las rutas y la configuración de una causa del
#' proyecto de ejemplo o de su versión en el formato completo.
#'
#' @param ... Partes de la ruta dentro de la carpeta del ejemplo (por ejemplo `"config"`, `"9100.yaml"`); sin
#'   ninguna, la carpeta.
#' @param copiar_en `NULL` (por defecto) o la ruta de una carpeta nueva o vacía: copia ahí el proyecto de ejemplo
#'   entero (crea la carpeta si no existe) para editarlo, por ejemplo para probar otra
#'   configuración. No escribe en una carpeta que ya tiene archivos. Va sin partes de la ruta en `...`.
#' @return La ruta del archivo o de la carpeta (si el archivo no existe, un error); con `copiar_en`, la ruta de la
#'   copia (`copiar_en`).
#' @seealso [dl_proyecto()], [dl_rutas_ejemplo()], [dl_configuracion_ejemplo()]; [dl_nuevo_proyecto()] para empezar
#'   un proyecto desde una carpeta vacía.
#' @family proyecto
#' @examples
#' # la carpeta del proyecto de ejemplo y sus archivos
#' dl_ejemplo()
#' list.files(dl_ejemplo(), recursive = TRUE)
#' readLines(dl_ejemplo("config", "9101.yaml"))
#' dl_proyecto(dl_ejemplo(), causa = 9101)
#'
#' # las curvas verdaderas
#' head(read.csv(dl_ejemplo("verdad.csv")))
#'
#' # el mismo proyecto en el formato completo
#' completo <- system.file("extdata", "acs_peru_completo", package = "dismodlite")
#' list.files(completo)
#'
#' # una copia del ejemplo para editarla (por ejemplo, otra configuración de la causa 9101)
#' copia <- dl_ejemplo(copiar_en = file.path(tempdir(), "mi_proyecto"))
#' list.files(copia)
#' cat("ancla:\n  peso: 0.5\n", file = file.path(copia, "config", "9101.yaml"), append = TRUE)
#' dl_proyecto(copia, causa = 9101)
#' unlink(copia, recursive = TRUE)
#' @export
dl_ejemplo <- function(..., copiar_en = NULL) {
  if (!is.null(copiar_en)) return(.dl_copiar_ejemplo(copiar_en, ...))
  p <- .dl_inst("extdata", .DL_CARPETAS_EJEMPLO[["simple"]], ...)
  if (!file.exists(p))
    .dl_stop("no existe \u00ab%s\u00bb en los datos de ejemplo", paste(c(...), collapse = "/"))
  p
}

# Copia del proyecto de ejemplo en `destino` (dl_ejemplo(copiar_en =)): crea la carpeta si no
# existe y no escribe en una que tiene archivos ni dentro de la instalación del paquete (lo comprueba antes de crear
# nada). Los archivos quedan con los permisos por defecto (no con los de la instalación del paquete, que pueden ser de
# solo lectura), así que se pueden editar. Devuelve `destino`.
.dl_copiar_ejemplo <- function(destino, ...) {
  .dl_exigir_carpeta(destino, "la carpeta donde se copia el ejemplo", "copiar_en")
  if (...length())
    .dl_stop("con `copiar_en` se copia el proyecto entero: sin partes de la ruta (%s)", paste(c(...), collapse = "/"))
  if (.dl_en_paquete(destino))
    .dl_stop(paste0("`copiar_en` est\u00e1 dentro de la instalaci\u00f3n del paquete (%s): copia el ejemplo en una ",
                    "carpeta propia, por ejemplo file.path(tempdir(), \"mi_proyecto\")"), destino)
  if (file.exists(destino) && !dir.exists(destino)) .dl_stop("`copiar_en` es un archivo, no una carpeta: %s", destino)
  if (length(list.files(destino, all.files = TRUE, no.. = TRUE)))
    .dl_stop("la carpeta %s ya tiene archivos: `copiar_en` debe ser una carpeta nueva o vac\u00eda", destino)
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  origen <- list.files(.dl_carpeta_ejemplo("simple"), full.names = TRUE)
  ok <- file.copy(origen, destino, recursive = TRUE, copy.mode = FALSE)
  if (!all(ok))
    .dl_stop("no se pudo copiar el ejemplo en %s: %s", destino, paste(basename(origen[!ok]), collapse = ", "))
  destino
}

# Carpeta del ejemplo en un formato ("simple" o "completo").
.dl_carpeta_ejemplo <- function(formato) .dl_inst("extdata", .DL_CARPETAS_EJEMPLO[[formato]])

# `formato` de las funciones del ejemplo: "simple" (por defecto) o "completo".
.dl_formato_ejemplo <- function(formato) {
  if (identical(formato, c("simple", "completo"))) return("simple")
  if (.dl_es_texto1(formato) && formato %in% names(.DL_CARPETAS_EJEMPLO)) return(formato)
  .dl_stop("`formato` debe ser \"simple\" o \"completo\"; es %s", .dl_describir_objeto(formato))
}

# Causa de los datos de ejemplo: un número entero entre 9100 y 9103 (o su texto) o una configuración (su cause_id).
.dl_causa_ejemplo <- function(causa) {
  causa <- .dl_exigir_causa(if (inherits(causa, "dl_config")) causa$cause_id else causa)
  if (causa %in% .DL_CAUSAS_EJEMPLO) return(causa)
  .dl_stop("`causa` debe ser 9100, 9101, 9102 o 9103 (o una configuraci\u00f3n de esas causas), pero es %s",
           .dl_describir_objeto(causa))
}

# Año de los datos de ejemplo: 2019, 2023 o 2024 (o NULL: el de la configuración).
.dl_anio_ejemplo <- function(anio) {
  if (is.null(anio)) return(NULL)
  if (.dl_es_numero1(anio) && anio %in% .DL_ANIOS_EJEMPLO) return(as.integer(anio))
  .dl_stop("`anio` debe ser 2019, 2023 o 2024, pero es %s", .dl_describir_objeto(anio))
}

#' Rutas de los datos de ejemplo
#'
#' Las rutas completas de [dl_rutas()] para los datos de ejemplo de una causa. Por defecto (`formato = "simple"`)
#' son las del proyecto de ejemplo, `dl_ejemplo()`, traducido al formato completo, como las de [dl_proyecto()]; en el completo,
#' las de los archivos de `acs_peru_completo`. La causa es obligatoria porque la tabla de severidad es la de cada
#' causa.
#'
#' @param causa Causa del ejemplo: 9100 (la causa padre) o 9101, 9102, 9103 (subtipos); también se acepta una
#'   configuración de [dl_configuracion()] (se usa su `cause_id`).
#' @param anio Año de la corrida: 2019, 2023 o 2024, o `NULL` (el de la configuración). El año de la corrida lo fija
#'   la configuración (`years.ajuste`, o el argumento `anio` de [dl_configuracion_ejemplo()]); si se da aquí,
#'   [dl_insumos()] comprueba que coincida con el de la configuración. En el formato simple, los proxies
#'   departamentales son los de la calibración de `proxies_crudos` para ese año (ver [dl_calibrar_proxies()]).
#' @param datos `TRUE` usa los datos locales del ejemplo (`datos.csv`), `FALSE` o `NULL` los dejan fuera; también
#'   acepta la ruta de un CSV propio con la tabla `datos` del formato completo.
#' @param proxies `TRUE` usa los proxies departamentales del ejemplo, `FALSE` o `NULL` los dejan fuera (sin ellos no
#'   hay cascada por proxies); también acepta la ruta de un CSV propio con la tabla `cov_proxy` del formato completo.
#' @param ... Cambios sobre las rutas del ejemplo, con los nombres de los argumentos de [dl_rutas()] (por ejemplo
#'   `poblacion = "mi_poblacion.csv"`, un CSV del formato completo); `NULL` quita la pieza (en el formato completo,
#'   `severidad = NULL` deriva la severidad de la partición).
#' @param formato `"simple"` (por defecto: el proyecto de ejemplo, el de [dl_ejemplo()]) o `"completo"` (el mismo
#'   ejemplo en el formato completo de la versión 0.2.2, `acs_peru_completo`).
#' @return Objeto de clase `dl_paths`, como el de [dl_rutas()]. Con `anio`, lleva el atributo `anio_ejemplo`, que
#'   [dl_insumos()] compara con el año de la configuración.
#' @seealso [dl_ejemplo()] (los archivos del ejemplo), [dl_configuracion_ejemplo()].
#' @family configuración
#' @examples
#' dl_rutas_ejemplo(9100)
#' dl_rutas_ejemplo(9100, datos = FALSE, proxies = FALSE, formato = "completo")
#' @export
dl_rutas_ejemplo <- function(causa, anio = NULL, datos = TRUE, proxies = TRUE, ..., formato = c("simple", "completo")) {
  if (missing(causa))
    .dl_stop(paste0("falta `causa` (9100, 9101, 9102 o 9103): la tabla de severidad es una por causa y debe ser la ",
                    "de la configuraci\u00f3n"))
  causa <- .dl_causa_ejemplo(causa)
  anio <- .dl_anio_ejemplo(anio)
  formato <- .dl_formato_ejemplo(formato)
  base <- if (formato == "simple") .dl_proyecto_ejemplo_anio(causa, anio)$rutas
          else .dl_rutas_completo(.dl_carpeta_ejemplo("completo"), causa)
  pieza <- function(valor, clave, argumento) {
    if (isTRUE(valor)) return(base[[clave]])
    if (is.null(valor) || isFALSE(valor)) return(NULL)
    if (.dl_es_texto1(valor)) return(valor)
    .dl_stop("`%s` debe ser TRUE, FALSE o la ruta de un archivo", argumento)
  }
  # las piezas del ejemplo con los nombres de los argumentos de dl_rutas()
  p <- stats::setNames(lapply(.DL_CLAVES_RUTAS, function(k) base[[k]]), names(.DL_CLAVES_RUTAS))
  p$datos <- pieza(datos, "datos", "datos")
  p$proxies <- pieza(proxies, "cov_proxy", "proxies")
  p <- Filter(Negate(is.null), p)
  cambios <- list(...)
  .dl_claves_internas(cambios, "...")          # solo para revisar los nombres: dl_rutas() los traduce abajo
  p[names(cambios)] <- cambios   # un NULL queda como elemento: dl_rutas() quita esa pieza
  r <- dl_rutas(cambios = p)
  # Sin covariables_std: el ejemplo trae todas sus covariables y no toma rutas de la variable de entorno DATA_ROOT;
  # el formato simple tampoco toma de ahí ninguna otra pieza.
  dadas <- .dl_claves_internas(p, "...")
  vacias <- if (formato == "simple") setdiff(names(r), names(dadas)) else setdiff("cov_std", names(dadas))
  for (k in vacias) r[k] <- list(NULL)
  attr(r, "codigos_libres") <- attr(base, "codigos_libres")   # la regla de los códigos subnacionales del proyecto
  if (!is.null(anio)) attr(r, "anio_ejemplo") <- anio
  r
}

# El proyecto de ejemplo de `causa` leído para el año `anio` (dl_rutas_ejemplo(anio =)): con NULL o el año de su
# configuración, dl_proyecto(dl_ejemplo()); con otro, una copia del ejemplo en la carpeta temporal de la sesión (se
# hace una vez por año) con `anio: <anio>` en la configuración de cada causa y, después del último año con ancla,
# `ancla.anio` en ese año. Así proxies_crudos se calibra para el año que se estima, con el valor nacional del ancla.
# La copia se arma en una carpeta temporal y se renombra al final: una carpeta del año, si existe, está completa.
.dl_proyecto_ejemplo_anio <- function(causa, anio) {
  if (is.null(anio) || anio == .DL_ANIO_CONFIG_EJEMPLO) return(dl_proyecto(dl_ejemplo(), causa))
  raiz <- file.path(tempdir(), "dismodlite_ejemplo")
  d <- file.path(raiz, anio)
  if (!dir.exists(d)) {
    dir.create(raiz, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(sprintf("%d_", anio), tmpdir = raiz)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    .dl_copiar_ejemplo(tmp)
    for (f in list.files(file.path(tmp, "config"), pattern = "[.]yaml$", full.names = TRUE)) {
      l <- readLines(f, encoding = "UTF-8")
      k <- grep("^anio: [0-9]+$", l)
      if (length(k) != 1L)
        .dl_stop("la configuraci\u00f3n del ejemplo %s no trae una sola l\u00ednea \u00abanio: <a\u00f1o>\u00bb",
                 basename(f))
      l[k] <- sprintf("anio: %d", anio)
      if (anio > .DL_ANIO_CONFIG_EJEMPLO) l <- c(l, "", "ancla:", sprintf("  anio: %d", .DL_ANIO_CONFIG_EJEMPLO))
      writeLines(enc2utf8(l), f, useBytes = TRUE)
    }
    if (!file.rename(tmp, d) && !dir.exists(d))
      .dl_stop("no se pudo preparar la copia del ejemplo para %d en %s", anio, d)
  }
  dl_proyecto(d, causa)
}

#' Configuración de ejemplo
#'
#' La configuración de una causa de los datos de ejemplo: `dl_configuracion(causa, <config del ejemplo>, cambios)`,
#' con la configuración del proyecto de `dl_ejemplo("config")` (por defecto) o la completa de `acs_peru_completo`.
#' Difieren en una decisión: el proyecto usa las bandas del ancla como vienen (80-84 ... 95+, las de su población) y
#' el formato completo las agrupa en 80+ como la versión 0.2.2 (`anchor.agrupar_bandas_finas`); y en el texto de las
#' procedencias, en cómo se declara la ubicación del ancla y en el campo `origen`, que solo trae la del proyecto.
#'
#' Con [dl_rutas_ejemplo()] de la misma causa y el mismo formato, son los dos argumentos de [dl_insumos()].
#'
#' @param causa Causa del ejemplo: 9100, 9101, 9102 o 9103.
#' @param cambios Cambios sobre la configuración, como en [dl_configuracion()] (claves del formato completo).
#' @param anio Año de la corrida: 2019, 2023 o 2024 (`years.ajuste`); `NULL` deja el de la configuración (2023).
#'   Con 2024, que no tiene ancla, el ancla es la de 2023 (`years.ancla`). Equivale a
#'   `cambios = list(years = list(ajuste = anio))`; lo que se pase en `cambios` va después.
#' @param formato `"simple"` (por defecto: el proyecto de ejemplo, el de [dl_ejemplo()]) o `"completo"` (el mismo
#'   ejemplo en el formato completo de la versión 0.2.2, `acs_peru_completo`).
#' @return Objeto de clase `dl_config`, como el de [dl_configuracion()].
#' @seealso [dl_ejemplo()] (los archivos del ejemplo), [dl_rutas_ejemplo()].
#' @family configuración
#' @examples
#' dl_configuracion_ejemplo(9100)
#' dl_configuracion_ejemplo(9101, anio = 2019)
#' # con un cambio (claves del formato completo)
#' dl_configuracion_ejemplo(9100, cambios = list(anchor = list(lambda = 0.5)))$anchor$lambda
#' \donttest{
#' # la configuración y las rutas del ejemplo, juntas
#' b <- dl_insumos(dl_configuracion_ejemplo(9100), dl_rutas_ejemplo(9100))
#' }
#' @export
dl_configuracion_ejemplo <- function(causa = 9100L, cambios = NULL, anio = NULL, formato = c("simple", "completo")) {
  causa <- .dl_causa_ejemplo(causa)
  anio <- .dl_anio_ejemplo(anio)
  formato <- .dl_formato_ejemplo(formato)
  if (!is.null(anio)) {
    anios <- list(ajuste = anio)
    if (anio == 2024L)
      anios$ancla <- list(valor = 2023L, procedencia = paste0("datos sint\u00e9ticos de ejemplo: el ancla de 2023 se ",
                                                              "mantiene en 2024"))
    if (is.null(cambios)) cambios <- list(years = anios)
    else if (is.list(cambios) && !is.null(names(cambios))) {
      if (!is.null(cambios$years) && (!is.list(cambios$years) || is.null(names(cambios$years))))
        .dl_stop("con `anio`, `cambios$years` debe ser una lista con nombres")
      cambios$years <- if (is.null(cambios$years)) anios else utils::modifyList(anios, cambios$years)
    }
  }
  dl_configuracion(causa, file.path(.dl_carpeta_ejemplo(formato), "config"), cambios)
}
