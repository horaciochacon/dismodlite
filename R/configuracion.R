# Configuración de una causa: dl_configuracion() lee <carpeta_config>/<causa>.yaml, le funde los `cambios` y valida
# todos los campos. Cada problema se reporta con la ruta del campo (config.anchor.lambda,
# config.remision.por_edad[1].fuente, ...) y todos van juntos en un solo error.

# ---- Los tres años de una configuración ----
# Cada año tiene un solo dueño, aquí:
#   - año de ajuste (years.ajuste): el año que se estima;
#   - año de validación de la cascada (cascada.heldout_anio.valor): el del csmr departamental reservado para validar
#     (held-out), que puede ser anterior al de ajuste; sin el campo, el año de ajuste;
#   - año del ancla (years.ancla.valor): el de las estimaciones GBD que prestan el ancla, el csmr y las covariables
#     cuando el año de ajuste aún no las tiene; sin el campo, el año de ajuste. dl_insumos() reetiqueta esas tablas al
#     año de ajuste.
.dl_anio_ajuste <- function(cfg) as.integer(unlist(cfg$years$ajuste))[1]
.dl_anio_heldout <- function(cfg)
  as.integer(cfg$cascada$heldout_anio$valor %||% .dl_anio_ajuste(cfg))
.dl_anio_ancla <- function(cfg)
  as.integer(cfg$years$ancla$valor %||% .dl_anio_ajuste(cfg))
# La misma regla del año del ancla, leída del manifiesto de una corrida ya escrita (params.anio_ancla; sin el campo,
# el año de la corrida).
.dl_man_anio_ancla <- function(man, anio) as.integer(man$params$anio_ancla %||% anio)

# Umbral por defecto de anchor_identity: error relativo mediano máximo entre la prevalencia posterior y la del ancla
# para escribir la corrida. Relajarlo exige anchor.gate_err_mediano con procedencia.
.DL_GATE_ERR_MEDIANO_DEFECTO <- 0.05

# ---- Claves de la configuración ----
# Claves que el paquete lee (ruta con puntos). Un `cambios` puede traer estas claves o las que ya están en el YAML;
# cualquier otra es un error, para que un nombre mal escrito no se ignore en silencio. Dentro de las secuencias de
# registros (transformaciones, covariables, remision.por_edad, suma.omitidas) no se revisa. phi_sinadef y
# years.post_2023 están para que la validación los rechace con un mensaje que explica qué los reemplaza.
.DL_CLAVES_CONFIG <- c(
  "schema", "cause_id", "sexos", "edad_inicio", "edad_inicio_fuente", "edad_fin", "nsub", "nudos_incidencia",
  "sigma_suavidad", "offset_lognormal", "medidas_entrada", "fuente_admin_principal", "transformaciones",
  "covariables", "decisiones", "phi_sinadef",
  "years", "years.ajuste", "years.ancla", "years.ancla.valor", "years.ancla.procedencia", "years.post_2023",
  "remision", "remision.valor", "remision.fuente", "remision.por_edad",
  "emr_prior", "emr_prior.tipo", "emr_prior.tipo_procedencia", "emr_prior.cota", "emr_prior.fuente_cota",
  "emr_prior.factor_techo", "emr_prior.factor_sd", "emr_prior.factor_sd.valor", "emr_prior.factor_sd.procedencia",
  "emr_prior.fraccion_aguda", "emr_prior.fraccion_aguda.valor", "emr_prior.fraccion_aguda.procedencia",
  "emr_prior.fraccion_aguda.cfr_30d",
  "anchor", "anchor.location", "anchor.location_id", "anchor.lambda", "anchor.rho_edad", "anchor.medidas",
  "anchor.evidencia_ghdx",
  "anchor.modelo_variante", "anchor.componente", "anchor.componente.sequela_ids", "anchor.componente.motivo",
  "anchor.gate_err_mediano", "anchor.gate_err_mediano.valor", "anchor.gate_err_mediano.procedencia",
  "anchor.metrica_prevalencia", "anchor.metrica_prevalencia.valor", "anchor.metrica_prevalencia.procedencia",
  "anchor.agrupar_bandas_finas",
  "cascada", "cascada.kappa", "cascada.escala", "cascada.cota_warning",
  "cascada.heldout_anio", "cascada.heldout_anio.valor", "cascada.heldout_anio.procedencia",
  "cascada.dx_fuera_de_banda", "cascada.dx_fuera_de_banda.valor", "cascada.dx_fuera_de_banda.procedencia",
  "cascada.dx_interpolacion", "cascada.dx_interpolacion.valor", "cascada.dx_interpolacion.procedencia",
  "cascada.modo", "cascada.modo.valor", "cascada.modo.procedencia",
  "severidad", "severidad.fuente", "severidad.procedencia", "severidad.run_id", "severidad.padre",
  "sensibilidad", "sensibilidad.lambda", "sensibilidad.rho", "sensibilidad.kappa", "sensibilidad.fraccion_aguda",
  "suma", "suma.omitidas", "extraction", "extraction.cause_id", "extraction.motivo")

# Opciones de la cascada con la forma {valor, procedencia} que también aceptan el valor suelto (cascada.modo: plana
# equivale a cascada.modo: {valor: plana}); ver `opcion` en dl_configuracion().
.DL_CLAVES_VALOR_SUELTO <- c("cascada.dx_fuera_de_banda", "cascada.dx_interpolacion", "cascada.modo")

# Funde `cambios` sobre la configuración leída del YAML, antes de validar: así la única regla de validación es la de
# dl_configuracion(). Por ejemplo, list(years = list(ajuste = 2024L, ancla = list(valor = 2023L, procedencia = "...")))
# cambia el año de ajuste y declara el año del ancla. Hace lo mismo que utils::modifyList() (una lista con nombres se
# funde clave a clave; cualquier otro valor reemplaza; NULL quita la clave) salvo en lo que modifyList() ignoraría en
# silencio o dejaría para un error confuso más adelante:
#   - una clave que no está en el YAML ni en .DL_CLAVES_CONFIG es un error que sugiere la clave más parecida;
#   - una lista sin nombres es una secuencia y reemplaza entera a la del YAML, como en el YAML: list("csmr") vale
#     como c("csmr") y list(list(...)) son registros (remision.por_edad);
#   - un valor suelto donde el YAML tiene un bloque con claves (anchor = 0.5) es un error que muestra la forma
#     correcta, salvo en las opciones de .DL_CLAVES_VALOR_SUELTO;
#   - `cambios` que no es una lista con nombres, o una clave repetida, es un error.
# Los mensajes de lo que también puede traer el bloque `avanzado` de una configuración simple (una clave que no
# existe, una forma que no es la del YAML) nombran la clave con su ruta, sin la sintaxis de R.
.dl_fundir_cambios <- function(cfg, cambios) {
  if (is.null(cambios) || (is.list(cambios) && !length(cambios) && !is.data.frame(cambios))) return(cfg)
  if (!is.list(cambios) || is.data.frame(cambios))
    .dl_stop("`cambios` debe ser una lista con nombres, por ejemplo cambios = list(anchor = list(lambda = 0.5)); es %s",
             .dl_describir_objeto(cambios))
  ruta_txt <- function(ruta) paste(ruta, collapse = ".")
  # nombres de una lista de `cambios`: todos presentes y sin repetir
  nombres <- function(val, ruta) {
    nm <- names(val)
    donde <- if (length(ruta)) sprintf(" (en %s)", ruta_txt(ruta)) else ""
    if (is.null(nm) || any(is.na(nm) | !nzchar(nm)))
      .dl_stop("en `cambios`%s cada elemento lleva nombre, por ejemplo cambios = list(anchor = list(lambda = 0.5))",
               donde)
    if (anyDuplicated(nm))
      .dl_stop("clave repetida en `cambios`%s: %s", donde, paste(unique(nm[duplicated(nm)]), collapse = ", "))
    nm
  }
  # la clave `ruta` existe en el YAML (existentes) o la lee el paquete
  conocida <- function(ruta, existentes) {
    if (ruta[length(ruta)] %in% existentes || ruta_txt(ruta) %in% .DL_CLAVES_CONFIG) return(invisible())
    prefijo <- if (length(ruta) > 1L) paste0(ruta_txt(ruta[-length(ruta)]), ".") else ""
    hermanas <- unique(c(existentes, sub(".*[.]", "", grep(paste0("^", gsub(".", "[.]", prefijo, fixed = TRUE),
                                                                   "[^.]+$"), .DL_CLAVES_CONFIG, value = TRUE))))
    cerca <- .dl_sugerir_clave(ruta[length(ruta)], hermanas, prefijo)
    en <- if (nzchar(prefijo)) paste0(" en ", sub("[.]$", "", prefijo)) else ""
    .dl_stop("la clave `%s` no existe en la configuraci\u00f3n%s", ruta_txt(ruta),
             if (!is.null(cerca)) paste0("; ", cerca)
             else sprintf(". Claves posibles%s: %s", en, paste(sort(hermanas), collapse = ", ")))
  }
  es_secuencia <- function(v) is.list(v) && !is.data.frame(v) && is.null(names(v))
  es_bloque <- function(v) is.list(v) && !es_secuencia(v)
  # valor que se asigna tal cual: se revisan sus claves (no hay YAML debajo) y las secuencias de escalares pasan a
  # vector, como las lee el YAML
  valor <- function(v, ruta) {
    if (!is.list(v) || is.data.frame(v)) return(v)
    if (es_secuencia(v)) {
      escalares <- length(v) && all(vapply(v, function(e) is.atomic(e) && length(e) == 1L, logical(1)))
      return(if (escalares) unlist(v) else v)
    }
    for (k in nombres(v, ruta)) {
      conocida(c(ruta, k), character())
      if (!is.null(v[[k]])) v[[k]] <- valor(v[[k]], c(ruta, k))
    }
    v
  }
  fundir <- function(x, val, ruta) {
    for (k in nombres(val, ruta)) {
      campo <- c(ruta, k)
      conocida(campo, names(x))
      nuevo <- val[[k]]
      actual <- x[[k]]
      if (is.list(nuevo) && !es_secuencia(nuevo) && es_secuencia(actual) && length(actual))
        .dl_stop(paste0("`%s` es una secuencia en la configuraci\u00f3n: va como una lista de valores o de ",
                        "registros ([...] en YAML; c(...) o list(list(...)) en R)"), ruta_txt(campo))
      if (es_secuencia(nuevo) && length(nuevo) && is.list(actual) && !es_secuencia(actual))
        .dl_stop(paste0("`%s` es un bloque con claves en la configuraci\u00f3n: va como {clave: valor} (en R, ",
                        "list(clave = valor))"), ruta_txt(campo))
      if (!is.null(nuevo) && !is.list(nuevo) && es_bloque(actual) && length(actual) &&
          !ruta_txt(campo) %in% .DL_CLAVES_VALOR_SUELTO)
        .dl_stop("`%s` es un bloque con claves en la configuraci\u00f3n, no un valor suelto: va como %s {%s}",
                 ruta_txt(campo), ruta_txt(campo), paste(names(actual), collapse = ", "))
      x[[k]] <- if (is.list(nuevo) && !es_secuencia(nuevo) && is.list(actual) && !es_secuencia(actual))
        fundir(actual, nuevo, campo)
      else valor(nuevo, campo)
    }
    x
  }
  fundir(cfg, cambios, character())
}

#' Configuración de una causa
#'
#' Lee la configuración de una causa, le aplica los `cambios` y valida todos los campos. Lee los dos formatos: la
#' configuración de un **proyecto** (corta, con las claves de la tabla de abajo) y el formato **completo** (`schema:
#' dismod_lite/v1`). La configuración de un proyecto se traduce a la completa y pasa por la misma validación; sus
#' errores citan su clave y, entre paréntesis, la del formato completo (`ancla.peso (anchor.lambda): ...`). Los errores
#' de una configuración completa citan la ruta del campo (`config.anchor.lambda`, ...).
#'
#' @section Configuración de un proyecto:
#' Lo mínimo son tres claves: `causa`, `anio` y `edad_inicio` (y `ubicacion_gbd`, el `location_id` de GBD del
#' país, si las descargas traen más de una ubicación y el código nacional de `ubicaciones` no es su `location_id`).
#' Todo lo demás tiene un valor por defecto, que [dl_proyecto()] muestra y cada corrida registra. Las covariables
#' con efecto y sus betas no se declaran aquí: son la tabla `betas` del proyecto (ver [dl_tablas]). La configuración
#' guarda decisiones; los números con su fuente van en las tablas. Por eso, con la carpeta de un proyecto,
#' `dl_configuracion()` lee también sus tablas (de ellas salen la ubicación nacional, las betas y lo que la
#' traducción necesita): una tabla con un error lo detiene, como a [dl_proyecto()]. Un ejemplo con
#' datos locales en el ajuste, una partición de severidad y subtipos:
#' ```yaml
#' causa: 1234
#' nombre: Enfermedad de ejemplo
#' anio: 2023
#' edad_inicio: 30
#' ubicacion_gbd: 130
#' ancla:
#'   peso: 0.5                  # los datos locales ya informaron la estimación de referencia
#' nudos: [30, 40, 50, 60, 70, 80, 95]
#' mortalidad_exceso:
#'   techo: 0.25                # por persona-año
#' datos_en_ajuste: [prevalencia, mortalidad]
#' severidad:
#'   particion: particion/mi_corrida   # carpeta de la corrida de partición, relativa al proyecto
#'   padre: 1230                       # la causa cuya partición reparte las secuelas
#' componente:
#'   secuelas: [5001, 5002]            # las secuelas que forman el componente que se modela
#' subtipos: [1235, 1236, 1237]
#' notas:
#'   - ancla.peso 0.5 porque el estudio de prevalencia entró a la estimación de referencia.
#' ```
#' Las claves con punto van dentro de su bloque (`ancla.peso` es `peso:` bajo `ancla:`). Las covariables actúan solo
#' en la estimación subnacional: con sus valores por ubicación en la tabla `covariables`, la diferencia de log i (o de
#' log f, con `efecto_sobre: mortalidad_exceso` en la tabla `betas`) de cada ubicación es `beta` por su diferencia con
#' el valor nacional; el ajuste nacional no cambia. `avanzado:` pasa claves del formato completo tal cual y se aplica
#' al final (para expertos). El bloque `subnacional` también se puede llamar `departamentos`, su nombre anterior. La
#' carpeta del proyecto y sus archivos: ver [dl_proyecto()]. Todas las claves, con su valor por defecto, están en la
#' tabla de la sección siguiente.
#'
#' @param causa Identificador de la causa (`cause_id`).
#' @param carpeta_config La carpeta que tiene la configuración (`<causa>.yaml`, o `config.yaml` en un proyecto
#'   de una sola causa), la carpeta de un proyecto (con `config/<causa>.yaml`) o la ruta del archivo de configuración.
#' @param cambios Lista con nombres que se funde sobre la configuración antes de validar, con las claves del formato
#'   completo (también para la configuración de un proyecto: se aplica sobre su traducción), por ejemplo
#'   `list(anchor = list(lambda = 0.5))`: cada clave reemplaza a la de la configuración y una lista con nombres se
#'   funde clave a clave; `NULL` quita la clave. Una secuencia (`c("csmr", "incidencia")`, o `list(list(...))` para
#'   registros como `remision.por_edad`) reemplaza entera a la de la configuración. Una clave que la configuración no
#'   tiene es un error.
#' @return Objeto de clase `dl_config`: la configuración validada, en el formato completo, una lista con sus claves
#'   (`cause_id`, `years`, `sexos`, `edad_inicio`, `remision`, `emr_prior`, `nudos_incidencia`, `sigma_suavidad`,
#'   `anchor`, `medidas_entrada`, `cascada`, `transformaciones`, `covariables`, `severidad`, `sensibilidad`,
#'   `decisiones` y las demás que declare; la columna «Formato completo» de la tabla de claves dice de qué clave del
#'   proyecto sale cada una). La de un proyecto trae además `origen`: `formato` (`"simple"`), `archivo`, `nombre`
#'   (el de la causa), `subnacional` (el modo subnacional: `covariables`, `plano` o `no`), `unidades`
#'   (`"contrato"`: las tablas del proyecto ya vienen en las unidades del modelo), `betas` (la tabla `betas` de la
#'   causa), `por_defecto` (las claves tomadas por defecto, con su valor, como texto) y `configuracion` (la
#'   configuración del proyecto tal como se leyó, con los nombres de clave de ahora: la que la corrida congela en
#'   `inputs/contrato/config.yaml`).
#' @seealso [dl_proyecto()] (la carpeta del proyecto y sus archivos), [dl_nuevo_proyecto()] (una configuración
#'   comentada con todas las claves), [dl_configuracion_ejemplo()] y [dl_insumos()] (el paso siguiente).
#' @family configuración
#' @examples
#' cfg <- dl_configuracion(9100, file.path(dl_ejemplo(), "config"))
#' cfg$anchor$lambda
#' cfg$origen$por_defecto
#' # con cambios (claves del formato completo)
#' cfg <- dl_configuracion(9100, file.path(dl_ejemplo(), "config"),
#'                         cambios = list(anchor = list(lambda = 0.5)))
#' @export
dl_configuracion <- function(causa, carpeta_config = NULL, cambios = NULL) {
  causa <- .dl_exigir_causa(causa)
  if (is.null(carpeta_config))
    .dl_stop(paste0("falta `carpeta_config` (la carpeta con <causa>.yaml)",
                    if (causa %in% .DL_CAUSAS_EJEMPLO)
                      paste0(". Para los datos de ejemplo usa dl_configuracion_ejemplo(", causa, ")")
                    else ""))
  if (!.dl_es_texto1(carpeta_config))
    .dl_stop("`carpeta_config` debe ser la ruta de una carpeta (un texto); es %s", .dl_describir_objeto(carpeta_config))
  path <- .dl_archivo_config(carpeta_config, causa)
  cfg <- .dl_leer_config(path)
  # la de un proyecto toma de sus tablas lo que no declara (la ubicación nacional, las betas, ...)
  if (!.dl_es_config_completa(cfg))
    return(.dl_preparar_contrato(cfg, path, causa, .dl_raiz_proyecto(path), cambios = cambios)$cfg)
  .dl_avisar_claves_desconocidas(cfg)
  cfg <- .dl_fundir_cambios(cfg, cambios)
  v <- .dl_validar_config(cfg, causa)
  if (length(v$problemas))
    .dl_stop("la configuraci\u00f3n de la causa %d tiene %d problema(s):\n%s",
             causa, length(v$problemas), paste0("  - ", v$problemas, collapse = "\n"),
             campos = list(problemas = v$problemas))
  v$cfg
}

# Avisa de cada clave del YAML de una configuración completa que el paquete no lee (no está en .DL_CLAVES_CONFIG),
# con la más parecida: un nombre mal escrito (`anchr`) no se ignora en silencio. Es un aviso y no un error, para no
# romper las configuraciones de la 0.2.2 que traen claves de documentación. No entra en las claves de dentro de una
# clave desconocida (ya se avisó de ella), ni de las claves que la lista trae como hojas (phi_sinadef: la validación
# la rechaza con su propio mensaje), ni de las secuencias de registros (transformaciones, covariables, ...).
.dl_avisar_claves_desconocidas <- function(cfg) {
  desconocidas <- character()
  recorrer <- function(x, ruta) {
    if (!is.list(x) || is.data.frame(x) || is.null(names(x))) return()
    for (k in names(x)) {
      campo <- c(ruta, k)
      txt <- paste(campo, collapse = ".")
      if (!txt %in% .DL_CLAVES_CONFIG) desconocidas <<- c(desconocidas, txt)
      else if (any(startsWith(.DL_CLAVES_CONFIG, paste0(txt, ".")))) recorrer(x[[k]], campo)
    }
  }
  recorrer(cfg, character())
  for (ruta in desconocidas) {
    k <- strsplit(ruta, ".", fixed = TRUE)[[1L]]
    prefijo <- if (length(k) > 1L) paste0(paste(k[-length(k)], collapse = "."), ".") else ""
    hermanas <- sub(".*[.]", "", grep(paste0("^", gsub(".", "[.]", prefijo, fixed = TRUE), "[^.]+$"),
                                     .DL_CLAVES_CONFIG, value = TRUE))
    cerca <- .dl_sugerir_clave(k[length(k)], hermanas, prefijo)
    .dl_warn("la clave `%s` de la configuraci\u00f3n no existe y se ignora%s", ruta,
             if (!is.null(cerca)) paste0("; ", cerca) else "")
  }
  invisible(desconocidas)
}

# La tabla de las claves simples va en ?dl_configuracion justo después de «Formato simple» y antes de «Formato
# completo»: roxygen deja las secciones de un tema en el orden de sus bloques, y la tabla necesita un bloque sin
# markdown (@noMd) para que el \ifelse{} de .dl_rd_config_simple() llegue tal cual al Rd.

#' @name dl_configuracion
#' @rdname dl_configuracion
#' @noMd
#' @eval .dl_rd_config_simple()
NULL

#' @name dl_configuracion
#' @rdname dl_configuracion
#' @section Formato completo:
#' Una configuración es del formato completo si trae `schema: dismod_lite/v1`; si no, es la de un proyecto. Es el
#' formato de la versión 0.2.2: cada decisión va con su procedencia (`edad_inicio_fuente`, `remision.fuente`,
#' `emr_prior.fuente_cota`, los bloques `{valor, procedencia}`) y la ubicación del ancla se declara con
#' `anchor.location_id` (las configuraciones escritas para esa versión usan `anchor.location`). La configuración
#' completa del ejemplo: `system.file("extdata", "acs_peru_completo", "config", "9100.yaml", package = "dismodlite")`.
#'
#' La configuración de un proyecto se traduce a la completa: cada clave va a la de la columna «Formato completo» de la
#' tabla de claves, cada procedencia que el formato completo exige dice «declarado en la configuración del proyecto»,
#' y las claves tomadas por defecto quedan en `origen$por_defecto` (y en el manifiesto de cada corrida, en
#' `configuracion.por_defecto`). Sobre esa traducción se aplican, en este orden, el bloque `avanzado:` de la
#' configuración y el argumento `cambios`, los dos con claves del formato completo. Por ejemplo, para aceptar un error
#' mayor entre la prevalencia ajustada y la del ancla antes de escribir la corrida:
#' ```yaml
#' avanzado:
#'   anchor:
#'     gate_err_mediano: {valor: 0.08, procedencia: el ancla tiene pocas bandas de edad}
#' ```
#' Algunas claves del formato completo sin equivalente en la configuración de un proyecto, útiles en `avanzado` (entre
#' paréntesis, el valor cuando la clave no está):
#' - `offset_lognormal`: desplazamiento de la verosimilitud log-normal (0). Hace falta si un dato local con `valor` y
#'   `error_estandar` vale 0, o si el ancla trae un valor o un límite inferior 0 en una banda donde GBD modela la
#'   causa.
#' - `edad_fin`: la última edad del modelo (99).
#' - `nsub`: los subpasos por año del método numérico que resuelve el modelo (5, es decir, pasos de 0.2 años).
#' - `remision.por_edad`: remisión por tramo de edad, una lista de `{edad_inicio, edad_fin, valor, fuente}`; fuera de
#'   los tramos rige `remision`.
#' - `emr_prior.factor_techo`: sin `mortalidad_exceso.techo`, el techo de la EMR es este factor por la EMR máxima del
#'   ancla (3).
#' - `emr_prior.factor_sd`: `{valor, procedencia}`, con valor mayor o igual que 1: multiplica la desviación estándar
#'   del prior de la EMR (1: el prior tal cual).
#' - `emr_prior.fraccion_aguda`: `{valor, procedencia}`: la fracción de las muertes por la causa que ocurre en su fase
#'   aguda; se descuenta de la mortalidad del ancla en el prior y el techo de la EMR (0).
#' - `anchor.gate_err_mediano`: `{valor, procedencia}`: el error relativo mediano máximo entre la prevalencia
#'   ajustada y la del ancla con que se escribe la corrida (0.05).
#' - `anchor.metrica_prevalencia`: `{valor, procedencia}`: la métrica en que se lee la prevalencia del ancla. `Rate`
#'   (por defecto) es la proporción de la población, por 100 000. `Percent` repite lo que hacían las versiones hasta la
#'   1.0.0 y sirve solo para reproducir sus corridas: en GBD Results divide los casos por las personas con alguna causa,
#'   no por la población, y sobrestima la prevalencia (menos de 1 % en adultos, hasta 37 % antes de los 2 años).
#' - `anchor.agrupar_bandas_finas`: `true` agrupa las bandas de 80-84 a 95+ en 80+ y las de menos de 5 años en <5,
#'   como hasta la 1.0.0, con los pesos de su población (en un proyecto, la población nacional de la tabla
#'   `poblacion` en el año del ancla, que entonces debe traer esas bandas; en el formato completo, `pesos_80mas`);
#'   `false` usa las bandas tal cual llegan (así las deja la traducción de un proyecto, que agrupa con
#'   `poblacion_detalle` solo las que la población no tiene). Por defecto `true` en el formato completo.
#' - `extraction`: `{cause_id, motivo}`: la causa padre de un subtipo que se lee sin la configuración de su padre
#'   (en su propia carpeta). La corrida la declara en su manifiesto y [dl_sumar_hijas()] la exige para sumar el
#'   subtipo; no cambia las betas, que van en `betas` bajo la causa del subtipo. En un proyecto con la configuración
#'   del padre (`subtipos`) sale sola, y la corrida congelada de un subtipo la escribe.
#' - `cascada.dx_fuera_de_banda`: en las edades sin grupo de edad del proxy, `cero` (sin diferencia con el valor
#'   nacional) o `vecina` (la del grupo más próximo, con procedencia) (`cero`).
#' - `cascada.dx_interpolacion`: entre los grupos de edad del proxy, `lineal` (interpolada entre sus puntos medios)
#'   o `escalon` (constante en cada grupo, con procedencia) (`lineal`).
NULL

# La sección de ?dl_configuracion con las claves de la configuración simple (roxygen @eval), una entrada por clave de
# inst/referencia/config_simple.yaml: en HTML (la ayuda de RStudio y el sitio del paquete) una tabla; en texto y en
# PDF, donde las celdas de una tabla no se parten en líneas, una lista con lo mismo. Va como Rd tal cual, en un bloque
# sin markdown (@noMd): el markdown de roxygen no conserva los bloques \ifelse{}.
.dl_rd_config_simple <- function() {
  t <- .dl_claves_simple()
  # % abre un comentario en Rd y las llaves deben ir escapadas; `x` va como código
  rd <- function(x) gsub("`([^`]*)`", "\\\\code{\\1}", gsub("([{}%])", "\\\\\\1", x))
  cod <- function(x) sprintf("\\code{%s}", rd(x))
  latex <- c("\u03bb" = "lambda", "\u03c1" = "rho", "\u03c3" = "sigma", "\u03ba" = "kappa")[t$simbolo]
  defecto <- .dl_defecto_en_palabras(t, cod, rd)
  que <- paste0(rd(t$descripcion), ifelse(nzchar(t$ejemplo), paste0(" Ejemplo: ", cod(t$ejemplo), "."), ""))
  destino <- ifelse(nzchar(t$destino), cod(t$destino), "")
  tabla <- c("\\tabular{lllll}{",
             paste0(paste(sprintf("\\strong{%s}", c("Clave", "Tipo", "Qu\u00e9 es", "Por defecto", "Formato completo")),
                          collapse = " \\tab "), " \\cr"),
             sprintf("%s \\tab %s \\tab %s \\tab %s \\tab %s \\cr", .dl_con_simbolo(cod(t$clave), t$simbolo), t$tipo,
                     que, defecto, destino),
             "}")
  lista <- c("\\describe{",
             sprintf("\\item{%s}{%s %s%s.%s}",
                     .dl_con_simbolo(cod(t$clave), t$simbolo, sprintf("\\eqn{\\%s}{%s}", latex, t$simbolo)), que,
                     paste0(toupper(substr(t$tipo, 1L, 1L)), substring(t$tipo, 2L)),
                     ifelse(t$defecto == "obligatoria", ", obligatoria", paste0("; por defecto, ", defecto)),
                     ifelse(nzchar(destino), paste0(" Formato completo: ", destino, "."), "")),
             "}")
  guia <- paste(
    "Entre par\u00e9ntesis, el s\u00edmbolo del par\u00e1metro en el modelo. \u00abPor defecto\u00bb es el valor de",
    "la clave cuando la configuraci\u00f3n no la trae; \u00abFormato completo\u00bb, d\u00f3nde queda el valor en",
    "la traducci\u00f3n al formato completo: la clave que citan los errores y que se puede ajustar en",
    "`avanzado`, con la forma del formato completo",
    "(`emr_prior.cota` es `[0, techo]`; `years.ancla` y `cascada.heldout_anio` son `{valor, procedencia}`;",
    "`cascada.modo` solo admite `plana`), o el archivo de la traducci\u00f3n donde queda.")
  c("@section Claves de la configuraci\u00f3n de un proyecto:", rd(guia), "", "\\ifelse{html}{", tabla, "}{", lista, "}")
}


# Valida una configuración del formato completo (la lista leída del YAML, ya con los cambios, o la traducción de una
# simple): list(cfg, problemas, campos, mensajes). Sin problemas, cfg es el dl_config con los valores convertidos;
# cada problema es «config.<campo>: <mensaje>», y van todos juntos.
.dl_validar_config <- function(cfg, causa) {
  sch <- dl_esquema()
  campos <- character(); mensajes <- character()
  p <- function(campo, msg) { campos <<- c(campos, campo); mensajes <<- c(mensajes, msg) }
  # Escalar numérico válido que cumple `ok`; en_dominio: en el dominio de un parámetro (.DL_DOMINIO_REJILLA).
  num1 <- function(v, ok = function(v) TRUE) .dl_es_numero1(v) && isTRUE(ok(v))
  dominio <- .DL_DOMINIO_REJILLA
  en_dominio <- function(v, eje) num1(v, dominio[[eje]]$dentro)
  # Bloque {valor, procedencia} de una decisión declarada (fraccion_aguda, factor_sd, gate_err_mediano, heldout_anio,
  # years.ancla): valor escalar que cumple `ok`, convertido con `coerce`, y procedencia no vacía. Los problemas van a
  # campo (forma), campo.valor y campo.procedencia; devuelve el bloque convertido.
  bloque <- function(x, campo, ok, ayuda, ayuda_proc, coerce = as.numeric) {
    if (!is.list(x)) { p(campo, "debe ser un bloque {valor, procedencia}"); return(x) }
    if (!num1(x$valor, ok)) p(paste0(campo, ".valor"), ayuda) else x$valor <- coerce(x$valor)
    if (is.null(x$procedencia) || !nzchar(x$procedencia)) p(paste0(campo, ".procedencia"), ayuda_proc)
    x
  }
  if (!identical(cfg$schema, "dismod_lite/v1")) p("schema", "debe ser dismod_lite/v1")
  if (!identical(as.integer(cfg$cause_id), causa))
    p("cause_id", if (is.null(cfg$cause_id)) "falta"
                  else sprintf("es %s y se pidi\u00f3 la causa %d (el nombre del archivo o el argumento `causa`)",
                               format(cfg$cause_id), causa))
  # Ubicación del ancla: anchor.location_id (el location_id de GBD del país, que lo declara la configuración simple; o
  # el código de texto de la ubicación nacional en ubicaciones.csv) o, en el formato completo de la versión 0.2.2,
  # anchor.location: peru (ubicación 123). Un número entero positivo, o un texto de dígitos sin cero a la izquierda
  # ("123"), se guarda como entero; cualquier otro texto no vacío ("007", "PAIS"), como texto.
  li <- cfg$anchor$location_id
  if (!is.null(li)) {
    if (.dl_es_texto1(li) && grepl("^[1-9][0-9]*$", li) && as.numeric(li) <= .Machine$integer.max)
      li <- as.numeric(li)
    if (.dl_es_entero1(li) && li >= 1) cfg$anchor$location_id <- as.integer(li)
    else if (!.dl_es_texto1(li) || !nzchar(trimws(li)))
      p("anchor.location_id", paste0("debe ser el c\u00f3digo de la ubicaci\u00f3n nacional: un entero positivo ",
                                     "o un texto"))
  } else if (identical(cfg$anchor$location, "region"))
    p("anchor.location", paste0("\u00abregion\u00bb est\u00e1 reservado para un ancla regional (ubicaci\u00f3n 120) ",
                                "que esta versi\u00f3n ",
                                "a\u00fan no admite; usa peru"))
  else if (!identical(cfg$anchor$location, "peru"))
    p("anchor.location", paste0("valor admitido: peru (ubicaci\u00f3n 123; \u00abregion\u00bb est\u00e1 reservado), o ",
                                "anchor.location_id con el location_id de GBD del pa\u00eds"))
  # Agrupar las bandas finas del ancla (80-84 ... 95+ en 80+, las de menos de 5 años en <5): sí si no se declara.
  af <- cfg$anchor$agrupar_bandas_finas
  if (is.null(af)) cfg$anchor$agrupar_bandas_finas <- TRUE
  else if (!isTRUE(af) && !isFALSE(af))
    p("anchor.agrupar_bandas_finas", "debe ser true o false")
  if (!en_dominio(cfg$anchor$lambda, "lambda"))
    p("anchor.lambda", sprintf("debe estar en %s (peso del ancla)", dominio$lambda$texto))
  if (!en_dominio(cfg$anchor$rho_edad, "rho"))
    p("anchor.rho_edad", sprintf("debe estar en %s (correlaci\u00f3n entre edades del ancla)", dominio$rho$texto))
  # Betas del ancla: el YAML de extracción puede traer las tablas de varios modelos de la misma publicación,
  # distinguidas por modelo_variante. anchor.modelo_variante lista las etiquetas exactas cuyas betas predictivas
  # entran al ancla; «sin_variante» elige las filas sin etiqueta. Sin el campo entran todas (y dl_insumos() se
  # detiene si dos betas comparten clave).
  mv <- cfg$anchor$modelo_variante
  if (!is.null(mv)) {
    mv <- unlist(mv)
    if (!is.character(mv) || !length(mv) || anyNA(mv) || !all(nzchar(trimws(mv))) || anyDuplicated(mv))
      p("anchor.modelo_variante", paste0("debe ser una lista de etiquetas de modelo_variante no vac\u00edas y ",
                                         "sin repetir ",
                                         "(\u00absin_variante\u00bb elige las filas sin etiqueta)"))
    else cfg$anchor$modelo_variante <- mv
  }
  # Tipo del prior de EMR: informativo_edad (un prior por banda a partir de csmr/prevalencia del ancla) o plano_cota
  # (sin prior informativo, solo el techo: el de emr_prior.cota o, si es null, el derivado de csmr/prevalencia).
  # plano_cota sirve para anclas cuya caída con la edad exige una EMR muy por encima de csmr/prevalencia, y exige
  # tipo_procedencia.
  tp <- cfg$emr_prior$tipo
  if (!identical(tp, "informativo_edad") && !identical(tp, "plano_cota"))
    p("emr_prior.tipo", "valores admitidos: informativo_edad o plano_cota")
  if (identical(tp, "plano_cota") &&
      (is.null(cfg$emr_prior$tipo_procedencia) || !nzchar(cfg$emr_prior$tipo_procedencia)))
    p("emr_prior.tipo_procedencia",
      "plano_cota exige procedencia (por qu\u00e9 no se usa el prior de mortalidad/prevalencia)")
  # Techo de EMR: [min, max] declarado, o null para que dl_insumos() lo derive del ancla (factor_techo por la EMR
  # máxima de csmr/prevalencia).
  ct <- cfg$emr_prior$cota
  if (!is.null(ct) && (!is.numeric(unlist(ct)) || length(unlist(ct)) != 2L || anyNA(unlist(ct)) ||
                       unlist(ct)[1] < 0 || unlist(ct)[2] <= unlist(ct)[1]))
    p("emr_prior.cota", "debe ser null (techo derivado del ancla) o [min, max] num\u00e9rico con 0 <= min < max")
  # Fracción aguda del csmr: GBD reparte las muertes de algunas causas entre una fase aguda (los primeros 28 días,
  # con su propio modelo) y la fase crónica, con una razón que no publica. El prior de EMR y el techo descuentan esa
  # fracción del csmr.
  fa <- cfg$emr_prior$fraccion_aguda
  if (!is.null(fa)) {
    cfg$emr_prior$fraccion_aguda <- fa <- bloque(fa, "emr_prior.fraccion_aguda", dominio$fraccion_aguda$dentro,
      paste("valor debe ser un n\u00famero en", dominio$fraccion_aguda$texto,
            "(fracci\u00f3n de las muertes de la causa que ocurren en la fase aguda)"),
      "la fracci\u00f3n aguda exige procedencia (GBD no la publica: fuente y c\u00e1lculo)")
    # Letalidad a 30 días (opcional): al modelo crónico entran los sobrevivientes a 28 días, no la incidencia total,
    # así que la comprobación implied_incidence de dl_validar_ancla() compara con incidencia GBD x (1 - cfr_30d).
    cf <- fa$cfr_30d
    if (!is.null(cf) && !num1(cf, function(v) v > 0 && v < 1))
      p("emr_prior.fraccion_aguda.cfr_30d", paste0("letalidad a 30 d\u00edas en (0, 1) (opcional; escala la ",
                                                   "incidencia ",
                                                   "GBD de la comprobaci\u00f3n implied_incidence)"))
    if (is.numeric(cf)) cfg$emr_prior$fraccion_aguda$cfr_30d <- as.numeric(cf)
  }
  ft <- cfg$emr_prior$factor_techo
  if (!is.null(ft) && !num1(ft, function(v) v > 1))
    p("emr_prior.factor_techo", "debe ser un n\u00famero > 1 (multiplica la EMR m\u00e1xima del ancla; 3 por defecto)")
  # Prior de EMR más laxo: factor_sd {valor >= 1, procedencia} multiplica la sd en escala log del prior de
  # csmr/prevalencia.
  if (!is.null(cfg$emr_prior$factor_sd))
    cfg$emr_prior$factor_sd <- bloque(cfg$emr_prior$factor_sd, "emr_prior.factor_sd", function(v) v >= 1,
      "valor debe ser un n\u00famero >= 1 (multiplica la sd en escala log del prior de EMR; 1 = prior tal cual)",
      "relajar el prior de EMR exige procedencia (por qu\u00e9 y con qu\u00e9 evidencia)")
  # Remisión por tramo de edad: por_edad es una lista de tramos {edad_inicio, edad_fin, valor, fuente}, cada uno
  # [edad_inicio, edad_fin), sin solape; fuera de ellos rige remision.valor.
  pe <- cfg$remision$por_edad
  if (!is.null(pe)) {
    if (!is.list(pe) || !length(pe))
      p("remision.por_edad", "debe ser una lista no vac\u00eda de tramos {edad_inicio, edad_fin, valor, fuente}")
    tr_ok <- list()
    for (i in seq_along(pe)) {
      tr <- pe[[i]]; ok <- TRUE
      for (campo in c("edad_inicio", "edad_fin", "valor"))
        if (!num1(tr[[campo]])) {
          p(sprintf("remision.por_edad[%d].%s", i, campo), "n\u00famero obligatorio"); ok <- FALSE
        }
      if (ok && (tr$edad_inicio < 0 || tr$edad_fin <= tr$edad_inicio || tr$valor < 0)) {
        p(sprintf("remision.por_edad[%d]", i), "exige 0 <= edad_inicio < edad_fin y valor >= 0"); ok <- FALSE
      }
      if (is.null(tr$fuente) || !nzchar(tr$fuente))
        p(sprintf("remision.por_edad[%d].fuente", i), "la remisi\u00f3n por tramo exige la fuente del valor")
      if (ok) tr_ok[[length(tr_ok) + 1L]] <- c(tr$edad_inicio, tr$edad_fin)
    }
    if (length(tr_ok) > 1L) {
      o <- order(vapply(tr_ok, `[`, numeric(1), 1L)); s <- tr_ok[o]
      for (i in seq_len(length(s) - 1L)) if (s[[i + 1L]][1] < s[[i]][2]) p("remision.por_edad", "tramos con solape")
    }
  }
  # Componente de la causa: anchor.componente {sequela_ids, motivo} modela solo ese subconjunto de secuelas; la
  # prevalencia del ancla y el AVD de referencia se escalan con la partición de severidad de la causa.
  cp <- cfg$anchor$componente
  if (!is.null(cp)) {
    ids <- unlist(cp$sequela_ids)
    if (!length(ids) || !is.numeric(ids) || anyNA(ids) || any(ids != as.integer(ids)))
      p("anchor.componente.sequela_ids",
        "debe ser una lista no vac\u00eda de sequela_id enteros (las secuelas que forman la unidad de modelado)")
    else cfg$anchor$componente$sequela_ids <- as.integer(ids)
    if (is.null(cp$motivo) || !nzchar(cp$motivo))
      p("anchor.componente.motivo", "modelar un componente de la causa exige motivo con procedencia")
  }
  # Umbral de anchor_identity: error relativo mediano máximo para escribir la corrida.
  cfg$anchor$gate_err_mediano <- if (is.null(cfg$anchor$gate_err_mediano))
    list(valor = .DL_GATE_ERR_MEDIANO_DEFECTO)
  else bloque(cfg$anchor$gate_err_mediano, "anchor.gate_err_mediano", function(v) v > 0 && v < 1,
    "valor debe estar en (0, 1) (error relativo mediano m\u00e1ximo de anchor_identity; 0.05 por defecto)",
    "relajar el umbral de anchor_identity exige procedencia (decisi\u00f3n documentada)")
  # Métrica de la prevalencia del ancla: Rate si no se declara (.dl_metrica_std). Percent reproduce las corridas de las
  # versiones hasta la 1.0.0 y, como toda decisión que se aparta del valor por defecto, exige procedencia.
  mp <- cfg$anchor$metrica_prevalencia
  if (!is.null(mp)) {
    if (!is.list(mp)) p("anchor.metrica_prevalencia", "debe ser un bloque {valor, procedencia}")
    else {
      if (!.dl_es_texto1(mp$valor) || !mp$valor %in% c("Rate", "Percent"))
        p("anchor.metrica_prevalencia.valor",
          "debe ser Rate (por defecto) o Percent (como las versiones hasta la 1.0.0)")
      if (identical(mp$valor, "Percent") && (is.null(mp$procedencia) || !nzchar(mp$procedencia)))
        p("anchor.metrica_prevalencia.procedencia",
          "leer la prevalencia en otra m\u00e9trica exige procedencia (decisi\u00f3n documentada)")
    }
  }
  # Unidad de modelado distinta de la unidad de extracción: la configuración de una causa hija puede usar el YAML de
  # extracción de la causa padre. extraction.cause_id declara la causa de ese YAML y dl_insumos() lo cruza con su
  # meta.causa_gbd.
  ec <- cfg$extraction$cause_id
  if (!is.null(cfg$extraction) && !.dl_es_entero1(ec))
    p("extraction.cause_id", paste0("debe ser un entero: el cause_id del YAML de extracci\u00f3n que aporta las betas ",
                                    "(por ejemplo, el de la causa padre)"))
  if (.dl_es_numero1(ec) && as.integer(ec) != causa &&
      is.null(cfg$extraction$motivo))
    p("extraction.motivo", "usar el YAML de extracci\u00f3n de otra causa exige motivo")
  # Año del ancla (years.ancla {valor, procedencia}): el de las estimaciones GBD que prestan el ancla cuando el año de
  # ajuste aún no las tiene. A lo sumo un año antes: más lejos ya no es mantener el nivel, es extrapolar.
  if (!is.null(cfg$years$post_2023))
    p("years.post_2023", paste0("ya no existe: un a\u00f1o de ajuste sin estimaciones GBD se declara con years.ancla ",
                                "{valor, procedencia}"))
  if (!is.null(cfg$years$ancla)) {
    aj <- suppressWarnings(as.integer(unlist(cfg$years$ajuste)[1]))
    cfg$years$ancla <- bloque(cfg$years$ancla, "years.ancla",
      function(v) v == as.integer(v) && !is.na(aj) && v <= aj && aj - v <= 1L,
      sprintf(paste0("debe ser un entero igual al a\u00f1o de ajuste o un a\u00f1o antes (ajuste %s): ",
                     "el nivel nacional se ",
                     "mantiene, no se extrapola"), aj),
      "tomar el ancla de otro a\u00f1o exige procedencia (por qu\u00e9 y de d\u00f3nde)", coerce = as.integer)
  }
  if (!is.null(cfg$phi_sinadef))
    p("phi_sinadef", paste0("ya no existe: la completitud del registro de defunciones se corrige al preparar la ",
                            "tabla `datos`, antes de dismodlite"))
  if (is.null(cfg$edad_inicio_fuente))
    p("edad_inicio_fuente", "falta: edad_inicio exige su procedencia (por qu\u00e9 el modelo empieza en esa edad)")
  for (campo in c("edad_fin", "nsub"))
    if (!is.null(cfg[[campo]]) && !is.numeric(cfg[[campo]])) p(campo, "debe ser un n\u00famero")
  if (!en_dominio(cfg$cascada$kappa, "kappa")) p("cascada.kappa", sprintf("debe estar en %s", dominio$kappa$texto))
  # Año del csmr departamental de validación (held-out): puede ser anterior al de ajuste cuando el registro de
  # defunciones solo tiene completitud publicada hasta ese año. Sin el campo, el año de ajuste.
  if (!is.null(cfg$cascada$heldout_anio))
    cfg$cascada$heldout_anio <- bloque(cfg$cascada$heldout_anio, "cascada.heldout_anio", function(v) v == as.integer(v),
      "debe ser un entero: el a\u00f1o de la mortalidad subnacional reservada para validar la cascada",
      "validar con la mortalidad de otro a\u00f1o exige procedencia (decisi\u00f3n documentada)", coerce = as.integer)
  # Opciones de la cascada, con la forma {valor, procedencia} del resto de la configuración (el valor suelto también
  # vale): el primer valor de `valores` es el de defecto y no exige procedencia; los demás sí.
  opcion <- function(x, campo, valores, ayuda) {
    if (is.null(x)) x <- list(valor = valores[1])
    if (is.character(x) && length(x) == 1L) x <- list(valor = x)
    if (!is.list(x) || !isTRUE(x$valor %in% valores)) { p(campo, ayuda); return(x) }
    if (x$valor != valores[1] && !nzchar(x$procedencia %||% ""))
      p(paste0(campo, ".procedencia"), sprintf("%s exige procedencia (decisi\u00f3n documentada)", x$valor))
    x
  }
  # Edades de la malla sin banda del proxy: `cero` (dX = 0: el escalar SEV de GBD vale 0 donde no define
  # exposición) o `vecina` (la banda más próxima; análisis de sensibilidad).
  cfg$cascada$dx_fuera_de_banda <- opcion(
    cfg$cascada$dx_fuera_de_banda, "cascada.dx_fuera_de_banda", c("cero", "vecina"),
    paste0("valores admitidos: cero (por defecto: sin diferencia con el valor nacional en las edades sin banda del ",
           "proxy) o vecina (la banda m\u00e1s pr\u00f3xima; exige procedencia)"))
  # Forma de dX entre bandas: `lineal` (interpolado entre los puntos medios de las bandas; la incidencia no salta en
  # los cortes) o `escalon` (constante dentro de cada banda).
  cfg$cascada$dx_interpolacion <- opcion(
    cfg$cascada$dx_interpolacion, "cascada.dx_interpolacion", c("lineal", "escalon"),
    paste0("valores admitidos: lineal (por defecto: la diferencia con el valor nacional, interpolada entre los ",
           "puntos medios de las bandas) o escalon (constante por banda; exige procedencia)"))
  # Modo de la cascada: `proxy` (gradiente departamental por los proxies declarados) o `plana` (las tasas nacionales
  # por edad y sexo en cada departamento, dX = 0 en todas las covariables; los conteos cambian solo por la
  # población). La plana es para causas cuyo ancla no tiene ninguna covariable con un proxy departamental defendible,
  # y el manifiesto de la corrida la declara como limitación.
  cfg$cascada$modo <- opcion(cfg$cascada$modo, "cascada.modo", c("proxy", "plana"),
    paste0("valores admitidos: proxy (por defecto: gradiente por los proxies declarados) o plana (tasas nacionales en ",
           "cada ubicaci\u00f3n subnacional; exige procedencia)"))
  # Sustitución declarada del valor nacional de referencia: la beta puede ser de una covariable por edad sin un
  # valor nacional único, y el proxy departamental se ancla entonces en la covariable hermana estandarizada por
  # edad. Con la beta en escala log el valor nacional se cancela en dX, así que la sustitución no mueve el
  # gradiente; se declara para que la regla proxy_mapea_config acepte el par y la corrida lo registre.
  for (i in seq_along(cfg$covariables)) {
    s <- cfg$covariables[[i]]$sustituye
    if (is.null(s)) next
    campo <- sprintf("covariables[%d].sustituye", i)
    if (is.null(cfg$covariables[[i]]$proxy)) p(campo, "solo tiene sentido con proxy (es el ancla nacional del proxy)")
    if (!is.list(s)) { p(campo, "debe ser un bloque {covariate_id, covariate_name_short, procedencia}"); next }
    cid <- s$covariate_id
    if (!.dl_es_entero1(cid))
      p(paste0(campo, ".covariate_id"),
        "debe ser un entero: el covariate_id GBD de la covariable que da el valor nacional")
    else cfg$covariables[[i]]$sustituye$covariate_id <- as.integer(cid)
    if (!is.character(s$covariate_name_short) || !nzchar(s$covariate_name_short))
      p(paste0(campo, ".covariate_name_short"),
        "falta: el nombre corto GHDx de la covariable sustituta (con \u00e9l se lee su valor nacional)")
    if (is.null(s$procedencia) || !nzchar(s$procedencia))
      p(paste0(campo, ".procedencia"), "una sustituci\u00f3n exige procedencia (decisi\u00f3n documentada)")
  }
  # suma.omitidas: hijas que no entran en la suma del padre (dl_sumar_hijas()), cada una con su motivo.
  for (i in seq_along(cfg$suma$omitidas)) {
    o <- cfg$suma$omitidas[[i]]
    if (!is.list(o) || !.dl_es_numero1(o$cause_id))
      p(sprintf("suma.omitidas[%d].cause_id", i), "debe ser un entero: el cause_id de la hija que no entra en la suma")
    else cfg$suma$omitidas[[i]]$cause_id <- as.integer(o$cause_id)
    if (!is.list(o) || is.null(o$motivo) || !nzchar(o$motivo))
      p(sprintf("suma.omitidas[%d].motivo", i), "una hija omitida exige motivo (decisi\u00f3n documentada)")
  }
  me <- unlist(cfg$medidas_entrada)
  if (length(me) && !all(me %in% sch$tipos_dato))
    p("medidas_entrada", sprintf("valores fuera de los tipos de dato admitidos (%s)",
                                 paste(sch$tipos_dato, collapse = ", ")))
  for (i in seq_along(cfg$transformaciones)) {
    tr <- cfg$transformaciones[[i]]
    for (campo in c("covariate_name_short", "transformacion", "procedencia"))
      if (is.null(tr[[campo]])) p(sprintf("transformaciones[%d].%s", i, campo), "falta")
    if (!is.null(tr$transformacion) && !tr$transformacion %in% sch$transformaciones)
      p(sprintf("transformaciones[%d].transformacion", i),
        sprintf("\u00ab%s\u00bb no es una transformaci\u00f3n admitida (%s)", tr$transformacion,
                paste(sch$transformaciones, collapse = ", ")))
    # Escala de la covariable con que se estimó la beta: multiplica la diferencia de covariable antes de la beta
    # (1 = escala nativa 0-100 del GHDx; 0.01 = escala 0-1). Obligatoria, con procedencia, para haqi lineal: las
    # publicaciones dan betas «por unidad» sin decir la unidad.
    es <- tr$escala
    if (!is.null(es) && !num1(es, function(v) v > 0))
      p(sprintf("transformaciones[%d].escala", i),
        "debe ser un n\u00famero > 0 (1 = escala nativa del GHDx; 0.01 = beta estimada en 0-1)")
    haqi_lineal <- identical(tolower(tr$covariate_name_short %||% ""), "haqi") && identical(tr$transformacion, "lineal")
    if (haqi_lineal && is.null(es))
      p(sprintf("transformaciones[%d].escala", i),
        "haqi lineal exige escala (1 = 0-100, 0.01 = 0-1) con escala_procedencia")
    if (!is.null(es) && is.null(tr$escala_procedencia))
      p(sprintf("transformaciones[%d].escala_procedencia", i),
        "escala exige procedencia (de d\u00f3nde sale la unidad de la beta)")
    if (!is.null(es) && is.numeric(es)) cfg$transformaciones[[i]]$escala <- as.numeric(es)
    if (!is.null(tr$escala_confirmada) && !isTRUE(tr$escala_confirmada))
      p(sprintf("transformaciones[%d].escala_confirmada", i),
        "solo admite true (permite |beta| >= 0.1 con escala 1 en haqi)")
  }
  if (is.null(cfg$remision$fuente)) p("remision.fuente", "falta: la remisi\u00f3n exige la fuente del valor")
  rv <- cfg$remision$valor
  if (!is.null(rv) && !num1(rv, function(v) v >= 0))
    p("remision.valor", "debe ser un n\u00famero >= 0 (por persona-a\u00f1o)")
  if (!is.null(cfg$sigma_suavidad) && !num1(cfg$sigma_suavidad, function(v) v > 0))
    p("sigma_suavidad", paste("debe ser un n\u00famero > 0 (la desviaci\u00f3n est\u00e1ndar de las segundas",
                              "diferencias de log i)"))
  nudos <- unlist(cfg$nudos_incidencia)
  if (!length(nudos)) p("nudos_incidencia", "falta")
  else if (!is.numeric(nudos) || anyNA(nudos) || any(diff(nudos) <= 0))
    p("nudos_incidencia", "deben ser edades crecientes y sin repetir")
  for (campo in c("lambda", "rho", "kappa"))
    if (!length(cfg$sensibilidad[[campo]]))
      p(sprintf("sensibilidad.%s", campo), "falta: la grilla del an\u00e1lisis de sensibilidad es obligatoria")
  if (length(campos))
    return(list(cfg = cfg, problemas = sprintf("config.%s: %s", campos, mensajes), campos = campos,
                mensajes = mensajes))
  cfg$cause_id <- as.integer(cfg$cause_id)
  list(cfg = structure(cfg, class = "dl_config"), problemas = character())
}

#' @export
print.dl_config <- function(x, ...) {
  cat(sprintf(paste0("<dl_config> configuraci\u00f3n de la causa %d | ancla %s (lambda %.2f, rho %.2f) | ",
                     "a\u00f1o de ajuste: %s | %d transformaciones de covariables\n"),
      x$cause_id, x$anchor$location %||% .dl_loc_ancla(x), x$anchor$lambda, x$anchor$rho_edad,
      paste(unlist(x$years$ajuste), collapse = ","), length(x$transformaciones)))
  invisible(x)
}
