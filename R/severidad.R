# Tabla de severidad desde una partición de severidad.
#
# Notación: ver R/edo.R y R/avd.R (pi_e, DW_e). Aquí q es una secuela, no la cantidad por banda de
# R/verosimilitud.R. Una partición de severidad es la corrida que reparte la prevalencia de una causa entre sus
# secuelas q y sus estados de salud e: por cada fila, una proporción val con su intervalo
# [lower, upper] sobre la prevalencia de la causa partida (ambos sexos, todas las edades, una ubicación). Se lee la
# tabla <entidad>/proportion de la corrida, con entidad cause_health_state (por estado) o cause_sequela (por
# secuela; el catálogo de secuelas da la causa propia y el estado de salud de cada una).
#
# pi_e de la causa, según los argumentos de dl_severidad_desde_particion():
#   directa      pi_e = val_e (filas cause_health_state de la causa)
#   hija         (`padre`) la partición del padre reparte prev(padre) entre las secuelas de todas sus hijas:
#                  cuota = sum_{q de la hija} val_q                          (= prev(hija) / prev(padre))
#                  pi_e  = sum_{q de la hija con estado e} val_q / cuota
#   componente   (`secuelas`: el subconjunto Q de secuelas de la causa que se modela como unidad):
#                  pi_e  = sum_{q en Q con estado e} val_q / sum_{q en Q} val_q
#                  fracción de prevalencia = sum_{q en Q} val_q / sum_q val_q
#                  fracción de AVD         = sum_{q en Q} val_q DW_q / sum_q val_q DW_q     (1 si el denominador es 0)
#                (el AVD sin corrección por comorbilidad es proporcional a val x DW en cada secuela; R/comorbilidad.R)
# Las secuelas de fase aguda entran como las demás: pi se mide sobre la prevalencia total de la causa y el factor de
# comorbilidad absorbe la diferencia. lower y upper se agregan y dividen igual que val (aproximación: la partición no
# trae simulaciones).
#
# Renormalización final, en los tres casos: con s = sum_e pi_e, pi_e <- pi_e / s (lower y upper por el mismo s). La
# partición viene redondeada (s = 1 +- 1e-6) y la regla proporciones_suman_uno de la tabla exige 1e-8; s queda en el
# atributo "renormalizacion". DW_e (media e intervalo) sale del catálogo de estados de salud.

# ---- Constantes ----

# Distancia máxima entre s = sum_e pi_e y 1 que se toma como redondeo de la partición y se corrige renormalizando.
# El redondeo observado es del orden de 1e-6; una suma más lejos de 1 (p. ej. filas repetidas o faltantes) es un
# error de la partición y se detiene.
.DL_TOLERANCIA_SUMA_PARTICION <- 1e-4

# ---- Lectura de la partición ----

# Tabla <entidad>/proportion de la partición en la carpeta `corrida` (un único CSV).
.dl_leer_particion_split <- function(corrida, entidad) {
  d <- file.path(corrida, entidad, "proportion")
  archivos <- list.files(d, pattern = "[.]csv$", full.names = TRUE)
  if (length(archivos) != 1L)
    .dl_stop("se esperaba un \u00fanico CSV de la partici\u00f3n %s/proportion en %s; hay %d",
             entidad, corrida, length(archivos))
  .dl_leer_csv(archivos, colClasses = list(character = "location_id"))
}

# Filas del catálogo de secuelas (`nombre` «sequelas», por sequela_id) o de estados de salud («health_states», por
# healthstate_id) de los `ids`, en su orden. Error con los ids que el catálogo no trae.
.dl_filas_catalogo <- function(rutas, nombre, ids) {
  de <- switch(nombre, sequelas = list(columna = "sequela_id", que = "secuela(s)", catalogo = "secuelas"),
               health_states = list(columna = "healthstate_id", que = "estado(s) de salud",
                                    catalogo = "estados de salud"))
  catalogo <- .dl_catalogo(rutas, nombre)
  fila <- match(ids, catalogo[[de$columna]])
  if (anyNA(fila))
    .dl_stop("%s sin fila en el cat\u00e1logo de %s: %s", de$que, de$catalogo,
             paste(ids[is.na(fila)], collapse = ", "))
  catalogo[fila]
}

# Secuelas de `causa` en la partición cause_sequela de `corrida`, con la causa propia y el estado de salud de cada
# una según el catálogo de secuelas (columnas causa_propia y estado). `de` nombra la causa en el error sin secuelas
# («padre 9100», «9101 (componente)»).
.dl_secuelas_con_estado <- function(corrida, causa, rutas, de) {
  secuelas <- .dl_leer_particion_split(corrida, "cause_sequela")[cause_id == causa]
  if (!nrow(secuelas))
    .dl_stop("la corrida %s no trae secuelas de la causa %s", basename(corrida), de)
  catalogo <- .dl_filas_catalogo(rutas, "sequelas", secuelas$sequela_id)
  secuelas[, `:=`(causa_propia = as.integer(catalogo$cause_id), estado = as.integer(catalogo$healthstate_id))][]
}

# Estados de salud a partir de filas por secuela (`filas_secuelas`: columnas estado, val, lower, upper,
# location_id): pi_e = sum_{q con estado e} val_q / cuota, y lo mismo con lower y upper.
.dl_estados_desde_secuelas <- function(filas_secuelas, cuota) {
  filas_secuelas[, list(val = sum(val) / cuota, lower = sum(lower) / cuota, upper = sum(upper) / cuota,
                        location_id = location_id[1]), by = list(health_state_id = estado)]
}

# ---- Los tres casos ----

# Hija: estados de salud de `causa` desde la partición cause_sequela de `padre`, divididos por su cuota (cabecera).
# Devuelve (health_state_id, val, lower, upper, location_id) con la cuota en el atributo "cuota_hija".
.dl_estados_hija <- function(corrida, causa, padre, rutas) {
  secuelas_causa <- .dl_secuelas_con_estado(corrida, padre, rutas, sprintf("padre %d", padre))
  filas_secuelas <- secuelas_causa[causa_propia == causa]
  if (!nrow(filas_secuelas))
    .dl_stop("ninguna secuela de la corrida %s pertenece a la causa hija %d seg\u00fan el cat\u00e1logo de secuelas",
             basename(corrida), causa)
  cuota <- sum(filas_secuelas$val)
  if (!(cuota > 0))
    .dl_stop("la causa hija %d tiene cuota nula en la partici\u00f3n del padre", causa)
  out <- .dl_estados_desde_secuelas(filas_secuelas, cuota)
  data.table::setattr(out, "cuota_hija", cuota)
  out
}

# Componente: fracciones de prevalencia y de AVD de las `secuelas` dentro de la partición cause_sequela de `causa`
# (cabecera), estados de salud del componente y la tabla por secuela (con estado y dw). Se llama desde
# dl_severidad_desde_particion(secuelas =) y desde dl_insumos() (anchor.componente con una tabla de severidad en
# CSV).
.dl_fracciones_componente <- function(corrida, causa, secuelas, rutas) {
  secuelas <- as.integer(unlist(secuelas))
  causa <- as.integer(causa)
  secuelas_causa <- .dl_secuelas_con_estado(corrida, causa, rutas, sprintf("%d (componente)", causa))
  estados <- .dl_filas_catalogo(rutas, "health_states", secuelas_causa$estado)
  secuelas_causa[, dw := as.numeric(estados$dw_mean)]                 # DW_q: el DW medio del estado de la secuela
  falta <- setdiff(secuelas, secuelas_causa$sequela_id)
  if (length(falta))
    .dl_stop("secuela(s) del componente ausentes de la partici\u00f3n de la causa: %s", paste(falta, collapse = ", "))
  en_q <- secuelas_causa$sequela_id %in% secuelas                    # q en Q
  val_dw_causa <- sum(secuelas_causa$val * secuelas_causa$dw)         # sum_q val_q DW_q
  list(sequela_ids = sort(secuelas),
       fraccion_prevalencia = sum(secuelas_causa$val[en_q]) / sum(secuelas_causa$val),
       fraccion_yld = if (val_dw_causa > 0) sum(secuelas_causa$val[en_q] * secuelas_causa$dw[en_q]) / val_dw_causa
                      else 1,
       health_state_ids = sort(unique(secuelas_causa$estado[en_q])), tabla = secuelas_causa)
}

# ---- Renormalización ----

# pi_e <- pi_e / s con s = sum_e pi_e (pi_e en la columna val; lower y upper por el mismo s), si s dista de 1 a lo
# sumo .DL_TOLERANCIA_SUMA_PARTICION. Devuelve la tabla renormalizada y s.
.dl_renormalizar_particion <- function(estados) {
  s <- sum(estados$val)
  if (abs(s - 1) > .DL_TOLERANCIA_SUMA_PARTICION)
    .dl_stop("las proporciones de la partici\u00f3n suman %.4g; se esperaba 1 salvo redondeo (tolerancia %g)",
             s, .DL_TOLERANCIA_SUMA_PARTICION)
  list(estados = data.table::copy(estados)[, `:=`(val = val / s, lower = lower / s, upper = upper / s)], s = s)
}

#' Tabla de severidad desde una partición de severidad
#'
#' Arma la tabla `severidad` de una causa a partir de una corrida de partición de severidad: proporciones por
#' estado de salud (de la propia causa, de una hija dentro de la partición del padre o de un componente formado por
#' algunas secuelas) y pesos de discapacidad del catálogo de estados de salud. Recibe las rutas del formato completo.
#' En un proyecto, la severidad es la tabla `severidad` (ver [dl_tablas]) o, si la configuración declara
#' `severidad.particion`, sale de la partición con este mismo cálculo.
#'
#' @details
#' Una partición de severidad es una corrida que reparte la prevalencia de una causa (ambos sexos, todas las edades,
#' una ubicación) entre sus secuelas y sus estados de salud, con una proporción y su intervalo por fila. Tres casos:
#' - la causa misma: las proporciones de sus estados de salud, tal cual;
#' - una hija (`padre`): la partición del padre reparte su prevalencia entre las secuelas de todas sus hijas; la
#'   cuota de la hija es la suma de sus secuelas y cada estado recibe la suma de sus secuelas dividida por la cuota;
#' - un componente (`secuelas`): lo mismo sobre el subconjunto de secuelas que se modela como unidad.
#'
#' Al final las proporciones se renormalizan para que sumen 1 (la partición viene redondeada); el divisor queda en el
#' atributo `renormalizacion`. Los pesos de discapacidad (media e intervalo) salen del catálogo de estados de salud.
#' Con la ruta `particion_severidad` de [dl_rutas()] y sin tabla de severidad, [dl_insumos()] la arma así.
#'
#' @param corrida Carpeta de la corrida de partición de severidad.
#' @param causa Identificador de la causa (`cause_id`).
#' @param rutas Rutas de [dl_rutas()]; se usan los catálogos.
#' @param beta_covariable Vector con nombres `<health_state_id> = <covariate_name_short>` para el canal de
#'   proporción de los AVD (opcional).
#' @param padre Causa padre cuya partición reparte las secuelas de `causa` (opcional).
#' @param secuelas Secuelas que forman el componente modelado (opcional; excluyente con `padre`).
#' @return Tabla `severidad` del contrato `dismod_lite/v1` (data.table), una fila por estado de salud: `cause_id`,
#'   `health_state_id`, `proportion`, `prop_lower` y `prop_upper` (la proporción de los casos en el estado y su
#'   intervalo), `dw_mean`, `dw_lower` y `dw_upper` (el peso de discapacidad y su intervalo), `beta_covariable` (la
#'   covariable del canal de proporción, o `NA`), `location_id_fuente` (la ubicación de la partición) y `fuente`.
#'   Atributos: `renormalizacion` (la suma de las proporciones antes de renormalizar) y, según el caso,
#'   `cuota_hija` (la fracción de la prevalencia del padre que es de la hija) o `componente` (`sequela_ids`,
#'   `health_state_ids`, y las fracciones de prevalencia y de AVD del componente, `fraccion_prevalencia` y
#'   `fraccion_yld`).
#' @seealso [dl_avd()] (que usa la tabla), [dl_rutas()] (`particion_severidad`).
#' @family avanzado
#' @examples
#' # la partición de severidad del ejemplo en el formato completo
#' completo <- system.file("extdata", "acs_peru_completo", package = "dismodlite")
#' corrida <- file.path(completo, "particion", "acs_v1")
#' rutas <- dl_rutas_ejemplo(9100, formato = "completo")
#' # la causa padre
#' dl_severidad_desde_particion(corrida, causa = 9100, rutas = rutas)
#' # una hija, desde la partición del padre
#' hija <- dl_severidad_desde_particion(corrida, causa = 9101, rutas = rutas, padre = 9100)
#' hija[, c("health_state_id", "proportion", "dw_mean")]
#' attr(hija, "cuota_hija")
#' @export
dl_severidad_desde_particion <- function(corrida, causa, rutas = dl_rutas(), beta_covariable = NULL, padre = NULL,
                                         secuelas = NULL) {
  causa <- .dl_exigir_causa(causa)
  rutas <- .dl_resolver_rutas(rutas)
  componente <- NULL
  cuota <- NULL
  if (!is.null(secuelas)) {
    if (!is.null(padre))
      .dl_stop("`secuelas` (un componente) y `padre` (una causa hija) son excluyentes; pasa solo uno de los dos")
    componente <- .dl_fracciones_componente(corrida, causa, secuelas, rutas)
    filas_secuelas <- componente$tabla[sequela_id %in% componente$sequela_ids]
    estados <- .dl_estados_desde_secuelas(filas_secuelas, sum(filas_secuelas$val))
  } else if (is.null(padre)) {
    estados <- .dl_leer_particion_split(corrida, "cause_health_state")
    estados <- estados[cause_id == causa]
    if (!nrow(estados))
      .dl_stop("la corrida %s no trae filas de la causa %d", basename(corrida), causa)
  } else {
    estados <- .dl_estados_hija(corrida, causa, as.integer(padre), rutas)
    cuota <- attr(estados, "cuota_hija")
  }
  if (data.table::uniqueN(estados$location_id) != 1L)
    .dl_stop("la partici\u00f3n trae m\u00e1s de una ubicaci\u00f3n; se admite una sola (queda en location_id_fuente)")
  if (anyDuplicated(estados$health_state_id))
    .dl_stop("health_state_id repetido en la partici\u00f3n (\u00bfvarias celdas de edad o sexo?)")
  renormalizada <- .dl_renormalizar_particion(estados)
  estados <- renormalizada$estados
  dw <- .dl_filas_catalogo(rutas, "health_states", estados$health_state_id)
  beta_cov <- rep(NA_character_, nrow(estados))
  if (length(beta_covariable)) {
    ids <- as.integer(names(beta_covariable))
    falta <- setdiff(ids, estados$health_state_id)
    if (length(falta))
      .dl_stop("`beta_covariable` cita estado(s) de salud ausentes de la partici\u00f3n: %s",
               paste(falta, collapse = ", "))
    beta_cov[match(ids, estados$health_state_id)] <- unname(beta_covariable)
  }
  out <- data.table::data.table(
    cause_id = causa, health_state_id = as.integer(estados$health_state_id),
    proportion = as.numeric(estados$val), prop_lower = as.numeric(estados$lower),
    prop_upper = as.numeric(estados$upper),
    dw_mean = as.numeric(dw$dw_mean), dw_lower = as.numeric(dw$dw_lower), dw_upper = as.numeric(dw$dw_upper),
    beta_covariable = beta_cov, location_id_fuente = as.character(estados$location_id[1]), fuente = "mod")
  data.table::setorder(out, -proportion)
  if (!is.null(cuota)) data.table::setattr(out, "cuota_hija", cuota)
  if (!is.null(componente))
    data.table::setattr(out, "componente", componente[c("sequela_ids", "fraccion_prevalencia", "fraccion_yld",
                                                        "health_state_ids")])
  data.table::setattr(out, "renormalizacion", renormalizada$s)
  out
}

# ---- Catálogos de GBD del paquete y la severidad del contrato ----

# Catálogo de GBD 2023 de inst/referencia (`nombre`: health_states, sequelas o demograficos), leído una vez.
.dl_catalogo_referencia <- function(nombre) {
  archivo <- .dl_inst_archivo("referencia", sprintf("catalogo_%s_gbd2023.csv", nombre))
  .dl_leer_memo(archivo, function(p) .dl_leer_csv(p, colClasses = "character"))
}

# Severidad del contrato con los pesos de discapacidad completos: los estados sin peso toman el de GBD (por id_estado,
# o por nombre: healthstate_name o healthstate_name_pretty, sin distinguir mayúsculas); `catalogo` por defecto el del
# paquete. Error con los estados sin peso que no son de GBD.
.dl_pesos_gbd <- function(sev, catalogo = .dl_catalogo_referencia("health_states")) {
  sev <- data.table::copy(sev)
  # columnas ausentes con NA del tipo que recibirán (si no, asignar a un subconjunto de filas las fuerza a lógico);
  # las presentes se llevan al mismo tipo (una columna vacía llega como lógica)
  tipos <- list(id_estado = NA_integer_, peso_discapacidad = NA_real_, peso_inferior = NA_real_,
                peso_superior = NA_real_)
  for (cn in names(tipos)) {
    if (!cn %in% names(sev)) sev[, (cn) := tipos[[cn]]]
    else if (is.logical(sev[[cn]])) sev[, (cn) := rep(tipos[[cn]], .N)]
  }
  falta <- is.na(sev$peso_discapacidad)
  if (!any(falta)) return(sev)
  nombre <- tolower(trimws(sev$estado))
  k <- match(as.character(sev$id_estado), catalogo$healthstate_id)
  k[is.na(k)] <- match(nombre[is.na(k)], tolower(catalogo$healthstate_name))
  k[is.na(k)] <- match(nombre[is.na(k)], tolower(catalogo$healthstate_name_pretty))
  sin <- falta & is.na(k)
  if (any(sin))
    .dl_stop(paste0("severidad: el estado %s no trae peso_discapacidad y no es un estado de salud de GBD; escribe sus ",
                    "tres pesos (peso_discapacidad, peso_inferior, peso_superior)"),
             paste(unique(sev$estado[sin]), collapse = ", "))
  sev[falta, `:=`(id_estado = as.integer(catalogo$healthstate_id[k[falta]]),
                  peso_discapacidad = as.numeric(catalogo$dw_mean[k[falta]]),
                  peso_inferior = as.numeric(catalogo$dw_lower[k[falta]]),
                  peso_superior = as.numeric(catalogo$dw_upper[k[falta]]))]
  sev[]
}

# Severidad del contrato de `causa` desde la corrida de partición `corrida` (los tres casos de
# dl_severidad_desde_particion: la causa, una hija de `padre` o un componente de `secuelas`). `catalogos`: carpeta de
# catálogos propia (opcional; por defecto los del paquete).
.dl_severidad_contrato_desde_particion <- function(corrida, causa, padre = NULL, secuelas = NULL, catalogos = NULL) {
  rutas <- dl_rutas(catalogos = catalogos %||% .dl_inst_archivo("referencia"))
  s <- dl_severidad_desde_particion(corrida, causa = causa, rutas = rutas, padre = padre, secuelas = secuelas)
  out <- data.table::data.table(causa = as.integer(s$cause_id), estado = as.character(s$health_state_id),
                                id_estado = as.integer(s$health_state_id), proporcion = s$proportion,
                                inferior = s$prop_lower, superior = s$prop_upper, peso_discapacidad = s$dw_mean,
                                peso_inferior = s$dw_lower, peso_superior = s$dw_upper)
  data.table::setattr(out, "componente", attr(s, "componente"))
  out
}
