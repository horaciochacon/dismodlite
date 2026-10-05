# Reparto subnacional por razón (subnacional.modo: razon en un proyecto; cascada.modo: razon en el formato completo).
# Para causas cuyo patrón entre ubicaciones no sale de covariables sino de una razón ya calculada (la tabla razones
# del contrato): la tasa de cada ubicación subnacional es la nacional por la razón de la ubicación, con cierre exacto
# en el valor nacional. El modelo es el nacional y la cascada es la plana (R/cascada.R); el reparto se aplica después
# de los AVD, a las simulaciones nacionales por banda de las tres medidas (prevalencia, incidencia y AVD).
#
# Notación: d = 1..D las ubicaciones subnacionales, en el orden de sus códigos; j = 1..n las simulaciones; N la
# población de una celda (sexo y banda de edad) del año que se estima.
#
#   z_dj ~ N(0, 1)                                       una sola matriz D x n, de un único rnorm() tras set.seed()
#   R_dj = razon_d exp(error_log_d z_dj)                 log-normal con mediana en la razón; R_dj = 0 si razon_d = 0
#   tasa_dj = tasa_nac_j R_dj N_nac / sum_d R_dj N_d     por medida, sexo y banda
#
# de modo que sum_d tasa_dj N_d = tasa_nac_j N_nac en cada simulación: los casos de las ubicaciones suman los
# nacionales por medida, sexo y banda. La misma z en los dos sexos, todas las bandas y las tres medidas: la razón es
# una por ubicación y año (la misma en todas las edades y sexos, supuesto que la corrida declara). El nivel nacional
# no cambia.
#
# Reproducibilidad: el orden de las ubicaciones (el de sus códigos en el locale C) y el del sorteo (por columnas: las
# D ubicaciones de la simulación 1, las de la 2, ...) fijan R; la semilla es la del argumento, la de la configuración
# (cascada.razon_semilla) o la de la cascada.

# Matriz R (D x n) de razones por simulación: R_dj = razon_d exp(error_log_d z_dj), con z de un único rnorm() tras
# set.seed(semilla) (efecto sobre el generador global, como .dl_dX); las filas de razón 0 quedan en 0.
.dl_razon_simular <- function(razon, error_log, n, semilla) {
  set.seed(semilla)
  z <- matrix(stats::rnorm(length(razon) * n), length(razon), n)
  R <- razon * exp(error_log * z)
  R[razon == 0, ] <- 0
  R
}

# Reparto de una celda: `x` = las n simulaciones nacionales de la tasa, `N_nac` y `N_sub` (D) = la población de la
# celda, `R` = la matriz D x n de razones. Devuelve la matriz D x n de tasas subnacionales
#   tasa_dj = R_dj (x_j N_nac / sum_d R_dj N_d).
.dl_razon_celda <- function(x, N_nac, N_sub, R) {
  den <- as.vector(crossprod(N_sub, R))                    # sum_d R_dj N_d, por simulación
  R * rep(x * N_nac / den, each = nrow(R))
}

# Reparto de una medida: `draws_banda` = sus simulaciones por banda (location_id, sex_id, age_group_id, draw, val; se
# usan las nacionales, las de `loc_nac`), `pob` = la población por celda (location_id, sex_id, age_group_id, pob) y
# `R` = la matriz de razones, con los códigos de las ubicaciones como nombres de fila. Devuelve la tabla con las
# simulaciones nacionales tal cual y las de cada ubicación de `R`, ordenada por ubicación, sexo, banda y simulación.
.dl_razon_repartir <- function(draws_banda, pob, R, loc_nac) {
  ubicaciones <- rownames(R)
  nac <- draws_banda[location_id == loc_nac]
  data.table::setorder(nac, sex_id, age_group_id, draw)
  celdas <- unique(nac[, list(sex_id, age_group_id)])
  sub <- data.table::rbindlist(lapply(seq_len(nrow(celdas)), function(i) {
    sx <- celdas$sex_id[i]; ag <- celdas$age_group_id[i]
    x <- nac[sex_id == sx & age_group_id == ag]
    pc <- pob[sex_id == sx & age_group_id == ag]
    N_nac <- pc$pob[pc$location_id == loc_nac]
    N_sub <- pc$pob[match(ubicaciones, pc$location_id)]
    if (nrow(x) != ncol(R) || length(N_nac) != 1L || anyNA(N_sub))
      .dl_stop("el reparto por raz\u00f3n no tiene la poblaci\u00f3n o las simulaciones de la celda sexo %s, banda %s",
               sx, ag)
    m <- .dl_razon_celda(x$val, N_nac, N_sub, R)
    # denominador 0 (las ubicaciones con razón > 0 sin población en la celda) o una razón sorteada que desborda
    if (!all(is.finite(m)))
      .dl_stop(paste0("el reparto por raz\u00f3n no da tasas finitas en la celda sexo %s, banda %s: las ubicaciones ",
                      "con raz\u00f3n mayor que 0 no tienen poblaci\u00f3n en esa celda (o el error_log de la ",
                      "tabla razones es desmedido)"), sx, ag)
    data.table::data.table(location_id = rep(ubicaciones, times = ncol(R)), sex_id = sx, age_group_id = ag,
                           draw = rep(x$draw, each = nrow(R)), val = as.vector(m))
  }))
  out <- rbind(nac, sub)
  data.table::setorder(out, location_id, sex_id, age_group_id, draw)
  out
}

# Las razones del reparto de los insumos `b`: las de la tabla razones del proyecto (b$contrato) para la causa y el año
# que se estima, una fila por ubicación subnacional de la población, en el orden de sus códigos (location_id, razon,
# error_log, fuente). Error si los insumos no traen la tabla o si no trae exactamente esas ubicaciones (lo que la
# revisión del proyecto dice antes, .dl_regla_razones).
.dl_razones_reparto <- function(b) {
  rz <- b$contrato$razones
  if (is.null(rz))
    .dl_stop(paste0("los insumos no traen la tabla razones: el reparto por raz\u00f3n es de un proyecto con las ",
                    "tablas del contrato (dl_insumos(dl_proyecto(...)), con la tabla razones; ver ?dl_tablas)"))
  anio <- .dl_anio_ajuste(b$cfg)
  r <- .dl_razones_del_anio(rz, b$cfg)
  ubicaciones <- sort(unique(b$poblacion[location_level == 1L]$location_id), method = "radix")
  faltan <- setdiff(ubicaciones, r$ubicacion)
  sobran <- setdiff(r$ubicacion, ubicaciones)
  if (length(faltan) || length(sobran) || anyDuplicated(r$ubicacion))
    .dl_stop(paste0("la tabla razones no trae una raz\u00f3n de %d por cada ubicaci\u00f3n subnacional de la ",
                    "poblaci\u00f3n (faltan: %s; sobran: %s; repetidas: %s)"), anio, .dl_lista(faltan, "ninguna"),
             .dl_lista(sobran, "ninguna"), .dl_lista(r$ubicacion[duplicated(r$ubicacion)], "ninguna"))
  r <- r[match(ubicaciones, r$ubicacion)]
  if (any(!is.finite(r$razon) | r$razon < 0 | !is.finite(r$error_log) | r$error_log < 0) || all(r$razon == 0))
    .dl_stop(paste0("la tabla razones debe traer razones y errores finitos y no negativos, con alguna raz\u00f3n ",
                    "mayor que 0 (a\u00f1o %d)"), anio)
  data.table::data.table(location_id = r$ubicacion, razon = r$razon, error_log = r$error_log,
                         fuente = .dl_col(r, "fuente", NA_character_))
}

#' Reparto subnacional por razón
#'
#' Con `subnacional.modo: razon`, da a cada ubicación subnacional la tasa nacional por la razón de la ubicación (la
#' tabla `razones` del proyecto, ver [dl_tablas]), simulación a simulación y con cierre exacto en el valor nacional.
#' Se aplica a las simulaciones nacionales de la prevalencia, la incidencia y los AVD, después de [dl_avd()]; su
#' resultado es la pieza `reparto` de [dl_resumir()]. [dl_correr()] lo hace solo.
#'
#' @details
#' Sirve para una causa cuyo patrón entre ubicaciones no sale de covariables con beta sino de una razón ya calculada
#' (de una encuesta, de la población en riesgo, de casos notificados...): la tasa de cada ubicación dividida por la
#' nacional, con el error estándar de su logaritmo. El modelo es el nacional y la cascada es la plana (cada ubicación
#' con las tasas nacionales, ver [dl_cascada()]); el reparto reemplaza después esas tasas subnacionales.
#'
#' Para las ubicaciones subnacionales d, en el orden de sus códigos, y las simulaciones j:
#' 1. `R_dj = razon_d * exp(error_log_d * z_dj)`, con `z_dj` normal estándar: una sola matriz de ubicaciones por
#'    simulaciones, sorteada de una vez con la semilla del reparto. Con `razon_d = 0`, `R_dj = 0`: la ubicación
#'    queda sin casos. Con `error_log_d = 0`, la razón no se sortea.
#' 2. Por medida, sexo, banda de edad y simulación: `tasa_dj = tasa_nac_j * R_dj * N_nac / sum_d(R_dj * N_d)`, con
#'    `N` la población del año que se estima. Así la suma de los casos de las ubicaciones es la de los casos
#'    nacionales en cada simulación.
#'
#' La misma `z` vale para los dos sexos, todas las bandas y las tres medidas: la razón es una por ubicación y año,
#' la misma en todas las edades y sexos, y el manifiesto de la corrida lo declara como limitación. El nivel nacional
#' no cambia. Si alguna prevalencia repartida pasa de 1, es un error que nombra las ubicaciones: la razón no cabe en
#' una proporción.
#'
#' La semilla del sorteo es, en este orden, la del argumento `semilla`, la de la configuración (`avanzado: {cascada:
#' {razon_semilla: ...}}`) o la de la cascada.
#'
#' @param piezas Lista con `ajuste` (la cascada de [dl_cascada()], que con `subnacional.modo: razon` es la plana),
#'   `avd` (el resultado de [dl_avd()] con esa cascada) e `insumos` (los de [dl_insumos()] de un proyecto con la tabla
#'   `razones`); también valen los nombres `fit`, `yld` y `bundle`.
#' @param semilla Semilla del sorteo de las razones (un entero); `NULL` (por defecto) usa la de la configuración o,
#'   sin ella, la de la cascada.
#' @return Objeto de clase `dl_reparto`, una lista con:
#'   - `draws`: una tabla por medida (`prevalence`, `incidence`, `yld`) con las simulaciones por `location_id`,
#'     `sex_id`, `age_group_id` y `draw` (en `val`): las nacionales, tal cual, y las de cada ubicación subnacional,
#'     repartidas. En las unidades del modelo (proporción o tasa por persona-año).
#'   - `razones`: la razón aplicada, una fila por ubicación: `location_id`, `razon` y `error_log` (los de la tabla),
#'     `razon_media`, `razon_lower` y `razon_upper` (la media y los cuantiles 2.5 % y 97.5 % de las razones
#'     sorteadas) y `fuente`.
#'   - `semilla` (la del sorteo), `ubicaciones` (los códigos, en el orden del sorteo) y `anio`.
#'   - `bundle_hash`, `huella_ajuste` y `huella_avd`: identifican los insumos, la cascada y los AVD de donde sale;
#'     [dl_resumir()] los compara.
#' @seealso [dl_tablas] (la tabla `razones`), [dl_configuracion()] (`subnacional.modo`), [dl_cascada()] y [dl_avd()]
#'   (los pasos anteriores), [dl_resumir()] (el paso siguiente) y [dl_correr()] (todos los pasos en una llamada).
#' @family subnacional
#' @examples
#' \donttest{
#' # el ejemplo en modo razón: una copia sin los proxies de la encuesta y con una tabla de razones
#' # (aquí, inventadas: de 0.7 a 1.3 veces la tasa nacional, con un error del 5 %)
#' carpeta <- dl_ejemplo(copiar_en = file.path(tempdir(), "proyecto_razon"))
#' unlink(file.path(carpeta, "proxies_crudos.csv"))
#' ubicaciones <- read.csv(file.path(carpeta, "ubicaciones.csv"), colClasses = "character")
#' subnacionales <- ubicaciones$ubicacion[ubicaciones$padre != ""]
#' razones <- data.frame(ubicacion = subnacionales, anio = 2023,
#'                       razon = seq(0.7, 1.3, length.out = length(subnacionales)),
#'                       error_log = 0.05, fuente = "razones inventadas")
#' p <- dl_proyecto(carpeta, razones = razones,
#'                  configuracion = list(causa = 9101, anio = 2023, edad_inicio = 30,
#'                                       mortalidad_exceso = list(techo = 0.2),
#'                                       subnacional = list(modo = "razon")))
#' b <- dl_insumos(p)
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' cas <- dl_cascada(f, b, semilla = 1)
#' y <- dl_avd(cas, b, semilla = 1)
#' reparto <- dl_repartir_razon(list(ajuste = cas, avd = y, insumos = b))
#' reparto
#' head(reparto$razones)
#' # el resumen con el reparto: las celdas subnacionales son las repartidas
#' res <- dl_resumir(list(ajuste = cas, avd = y, insumos = b, reparto = reparto))
#' prev <- res$celdas[measure_id == 5 & sex_id == 2 & age_group_id == 15]
#' head(prev[, c("location_name", "val", "lower", "upper")])
#' unlink(carpeta, recursive = TRUE)
#' }
#' @export
dl_repartir_razon <- function(piezas, semilla = NULL) {
  piezas <- .dl_piezas(piezas, c("fit", "yld", "bundle"))
  f <- piezas$fit; y <- piezas$yld; b <- piezas$bundle
  .dl_exigir_mismos_insumos(f, b, "piezas$fit")
  .dl_exigir_mismos_insumos(y, b, "piezas$yld")
  .dl_chequear_avd_de_ajuste(f, y)
  cfg <- b$cfg
  if (!identical(cfg$cascada$modo$valor, "razon"))
    .dl_stop(paste0("el reparto por raz\u00f3n es del modo subnacional razon, y la configuraci\u00f3n declara ",
                    "cascada.modo: %s. Declara subnacional: {modo: razon} en la configuraci\u00f3n del proyecto"),
             cfg$cascada$modo$valor %||% "proxy")
  if (!inherits(f, "dl_cascade"))
    .dl_stop(paste0("`ajuste` debe ser la cascada de dl_cascada() (con subnacional.modo: razon, la plana: las ",
                    "tasas nacionales en cada ubicaci\u00f3n, que el reparto reemplaza); es %s"),
             .dl_describir_objeto(f))
  semilla <- semilla %||% cfg$cascada$razon_semilla %||% f$seed_cascada
  .dl_exigir_semilla(semilla)
  razones <- .dl_razones_reparto(b)
  n <- nrow(f$draws_par[[1]])
  R <- .dl_razon_simular(razones$razon, razones$error_log, n, semilla)
  rownames(R) <- razones$location_id
  # simulaciones por banda de cada medida exportable, en las unidades del modelo (las mismas fuentes de dl_resumir())
  fuentes <- list(prevalence = y$draws_prev_banda, incidence = .dl_ipop_bandas(f, b), yld = y$draws_yld)
  bandas <- unique(unlist(lapply(fuentes, function(d) d$age_group_id)))
  pob <- .dl_poblacion_en_bandas(b$poblacion, bandas, b$bandas_catalogo)
  draws <- lapply(fuentes, .dl_razon_repartir, pob = pob, R = R, loc_nac = b$loc_ancla)
  mayor <- draws$prevalence[location_id != b$loc_ancla & val > 1]
  if (nrow(mayor))
    .dl_stop(paste0("la prevalencia repartida pasa de 1 en %d simulaci\u00f3n(es) de la(s) ubicaci\u00f3n(es) %s ",
                    "(m\u00e1ximo %.3f): la raz\u00f3n no cabe en una proporci\u00f3n; revisa la tabla razones"),
             data.table::uniqueN(mayor$draw), .dl_unos(mayor$location_id), max(mayor$val))
  cuantil <- function(p) apply(R, 1L, stats::quantile, probs = p, names = FALSE)
  razones <- data.table::data.table(location_id = razones$location_id, razon = razones$razon,
                                    error_log = razones$error_log, razon_media = rowMeans(R),
                                    razon_lower = cuantil(0.025), razon_upper = cuantil(0.975),
                                    fuente = razones$fuente)
  structure(list(draws = draws, razones = razones, semilla = as.integer(semilla),
                 ubicaciones = razones$location_id, anio = .dl_anio_ajuste(cfg), bundle_hash = b$hash,
                 huella_ajuste = .dl_huella_ajuste(f), huella_avd = .dl_huella_avd(y)),
            class = "dl_reparto")
}

#' @export
print.dl_reparto <- function(x, ...) {
  cat(sprintf(paste0("<dl_reparto> reparto subnacional por raz\u00f3n | a\u00f1o %d | %d ubicaciones | razones de ",
                     "%s a %s (%d en cero) | semilla %d\n"),
              x$anio, length(x$ubicaciones), format(signif(min(x$razones$razon), 3L)),
              format(signif(max(x$razones$razon), 3L)), sum(x$razones$razon == 0), x$semilla))
  cat(sprintf("  simulaciones por medida: %d | insumos %s\n", max(x$draws[[1L]]$draw), substr(x$bundle_hash, 1L, 12L)))
  invisible(x)
}
