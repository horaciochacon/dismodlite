#' dismodlite: estimación de carga de enfermedad por edad, sexo y ubicación
#'
#' Estima la prevalencia, la incidencia, la mortalidad en exceso y los años vividos con discapacidad (AVD) de una
#' causa, por edad y sexo, en un país y en sus ubicaciones subnacionales (departamentos, regiones, provincias...),
#' con un modelo enfermedad-muerte (DisMod-lite). El modelo se ancla en una estimación de referencia externa (el
#' ancla; por ejemplo, la de GBD para el país), la combina con los datos locales que se declaren y lleva el resultado
#' nacional a las ubicaciones subnacionales según las diferencias de sus covariables. La incertidumbre se propaga con
#' simulaciones MCMC, y cada corrida se escribe en una carpeta reproducible, con un manifiesto que registra sus
#' insumos y sus decisiones.
#'
#' @section Tres comandos:
#' Un proyecto es una carpeta en el **formato simple** (ver [dl_proyecto()]): una configuración corta, las descargas
#' de GBD Results (el ancla) y del GHDx (las covariables) tal como se descargan, y unas pocas tablas planas. Tres
#' funciones cubren el trabajo:
#' 1. [dl_nuevo_proyecto()] crea la carpeta con la configuración comentada, las plantillas de las tablas y un
#'    `LEEME.md` que dice de dónde se descarga cada archivo.
#' 2. [dl_revisar_proyecto()], con la carpeta llena, revisa cada archivo y cada regla sin detenerse en el primer
#'    problema y dice cómo corregir cada uno.
#' 3. [dl_correr()] hace la corrida en una llamada: primero con `rapido = TRUE`, una prueba de principio a fin que
#'    tarda segundos; después, la corrida de producción.
#'
#' ```r
#' dl_nuevo_proyecto("mi_proyecto", causa = 1234, nombre = "Mi enfermedad", anio = 2023,
#'                   edad_inicio = 30)
#' # copiar las descargas a ancla/ y covariables/ y llenar las tablas; después:
#' dl_revisar_proyecto("mi_proyecto")
#' prueba <- dl_correr("mi_proyecto", semilla = 1, rapido = TRUE)
#' corrida <- dl_correr("mi_proyecto", semilla = 1)
#' ```
#'
#' [dl_ejemplo()] es un proyecto completo con datos sintéticos (una enfermedad ficticia, con la geografía del Perú
#' como ejemplo), para leerlo, correrlo y copiarlo como punto de partida. [dl_configuracion()] documenta cada clave de
#' la configuración y [dl_proyecto()], cada archivo y sus columnas.
#'
#' @section Etapas de una corrida:
#' [dl_correr()] hace en orden las etapas de una causa. Cada etapa es también una función que se puede llamar por
#' separado, para mirar o cambiar un paso:
#' 1. Proyecto e insumos: [dl_proyecto()] lee la carpeta y [dl_insumos()] lee y valida todas las tablas
#'    ([dl_configuracion()] y [dl_rutas()] arman la configuración y las rutas por separado).
#' 2. Ajuste nacional, por sexo: [dl_ajustar()], con las opciones del muestreo de [dl_opciones_mcmc()], y
#'    [dl_ajustar_solo_prior()], el mismo ajuste sin los datos locales. [dl_estimaciones()] da la media y el intervalo
#'    por edad, sexo y ubicación de un ajuste, de una cascada o de los AVD.
#' 3. Estimación subnacional: [dl_cascada()].
#' 4. Validación contra el ancla y contra los datos subnacionales reservados: [dl_validar_ancla()].
#' 5. Carga: [dl_avd()], con [dl_factor_comorbilidad()], que calibra los AVD al AVD del ancla.
#' 6. Diagnóstico: [dl_etiquetas()] (cuánto informan los datos a cada celda) y [dl_sensibilidad()]; [dl_rhat()] y
#'    [dl_ess()] miden la convergencia de las cadenas.
#' 7. Resumen y carpeta de la corrida: [dl_resumir()] y [dl_exportar_corrida()].
#'
#' Después de las corridas, [dl_sumar_hijas()] suma las corridas de los subtipos de una causa,
#' [dl_reresumir_corrida()] vuelve a resumir una corrida sin volver a ajustar y [dl_consolidar()] reúne las corridas
#' de varias causas en tablas por medida.
#'
#' @section Guías:
#' - `vignette("primeros-pasos", package = "dismodlite")`: el proyecto de ejemplo de principio a fin.
#' - `vignette("el-modelo", package = "dismodlite")`: el modelo, su notación y sus supuestos.
#' - `vignette("preparar-datos", package = "dismodlite")`: un proyecto propio, archivo por archivo.
#' - En el sitio del paquete, <https://horaciochacon.github.io/dismodlite/>, las guías de temas avanzados: la
#'   estimación subnacional, los datos locales, los subtipos y su suma, la carga (AVD), las corridas y el
#'   consolidado, el diagnóstico y la sensibilidad, y el formato completo.
#'
#' @section Formato completo y nombres anteriores:
#' Los proyectos de la versión 0.2.2 (el **formato completo**, con `schema: dismod_lite/v1` en la configuración) se
#' leen igual y dan los mismos números. Es el formato de las opciones avanzadas (ver la sección «Formato completo» de
#' [dl_proyecto()] y de [dl_configuracion()]). Las funciones tienen nombres en español; los nombres de la versión
#' 0.2.2 siguen funcionando (ver [dl_nombres_anteriores]).
#'
#' @examples
#' # el proyecto de ejemplo, en el formato simple
#' p <- dl_proyecto(dl_ejemplo(), causa = 9100)
#' p
#' \donttest{
#' # una corrida de prueba, escrita en una carpeta temporal
#' salida <- file.path(tempdir(), "resultados")
#' run <- dl_correr(p, semilla = 1, rapido = TRUE, sensibilidad = FALSE, carpeta_salida = salida)
#' run
#'
#' # paso a paso: insumos, un ajuste corto (una prueba, no una estimación para publicar) y sus
#' # estimaciones
#' b <- dl_insumos(p)
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' head(dl_estimaciones(f))
#' unlink(salida, recursive = TRUE)
#' }
#' @keywords internal
#' @importFrom data.table :=
"_PACKAGE"

# El código usa la sintaxis de data.table (`[.data.table` con :=, .N, by): este aviso hace que data.table trate
# las tablas del paquete como data.table y no como data.frame.
.datatable.aware <- TRUE
# La importación de `:=` (arriba) hace además que library(dismodlite) cargue el espacio de nombres de data.table.
# Sin eso, hasta la primera llamada a data.table:: el método `[` de data.table no está registrado y `[` sobre las
# tablas constantes del paquete (p. ej. .DL_MEDIDAS) cae en la semántica de data.frame.

# a %||% b: b si a es NULL (base R lo trae desde la versión 4.4; el paquete admite R >= 4.1).
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Versión del paquete
#'
#' La versión instalada de dismodlite, la misma que registra el manifiesto de cada corrida (`params$version_paquete`).
#'
#' @return Texto con la versión instalada de dismodlite (por ejemplo `"1.0.0"`).
#' @seealso [utils::packageVersion()], que da la misma versión como objeto `package_version`.
#' @family utilidades
#' @examples
#' dl_version()
#' @export
dl_version <- function()
  unname(read.dcf(system.file("DESCRIPTION", package = "dismodlite", mustWork = TRUE), fields = "Version")[1, 1])
