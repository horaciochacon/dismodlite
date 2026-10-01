# Opciones del muestreo MCMC (dl_opciones_mcmc()) y cache de ajustes de la sesión (dl_limpiar_cache()).
#
# Los campos del objeto dl_mcmc_opts conservan sus nombres en inglés por compatibilidad (draws, chains, iter,
# warmup, thin, cores, engine); cada uno corresponde a un argumento de dl_opciones_mcmc().

# El calentamiento debe ser múltiplo de .DL_MH_MULTIPLO_CALENTAMIENTO (200, R/muestreador.R). dl_mh() adapta la
# escala de las propuestas en ventanas de 100 iteraciones y estima su covarianza en la mitad del calentamiento: con
# un múltiplo de 200, esa mitad y el final del calentamiento caen en el borde de una ventana, y la tasa de aceptación
# que se reporta cuenta solo iteraciones posteriores al calentamiento.

# Motores de la log-posterior; el primero es el valor por defecto.
.DL_MOTORES <- c("mh", "rcpp")

#' Opciones del muestreo MCMC
#'
#' Reúne las opciones del muestreador en un objeto que usan [dl_ajustar()], [dl_ajustar_solo_prior()],
#' [dl_etiquetas()], [dl_sensibilidad()] y [dl_correr()].
#'
#' @details
#' Los valores por defecto son los de una corrida de producción: 4 cadenas de 50 000 iteraciones, 10 000 de
#' calentamiento y una de cada 10 guardada, es decir, 4 x 4000 = 16 000 simulaciones guardadas por sexo, de las que
#' se toman 1000 equiespaciadas. Si `simulaciones` es mayor que las que guardan las cadenas
#' (`cadenas` x (`iteraciones` - `calentamiento`) / `adelgazamiento`), un aviso lo dice y el ajuste tiene las que
#' hay. Para una prueba rápida sirven cadenas cortas, como las de `dl_correr(rapido = TRUE)` (100 simulaciones, 2
#' cadenas de 2000 iteraciones con 1000 de calentamiento), pero no llegan a converger: sus números no sirven para
#' publicar.
#'
#' Los dos motores evalúan la misma log-posterior. `"mh"`, en R, es la referencia y el valor por defecto; `"rcpp"`,
#' en C++, la evalúa varias veces más rápido y conviene en las corridas de producción. Los dos motores coinciden
#' hasta el redondeo, no bit a bit (la log-posterior, hasta 1e-9; la ecuación de la prevalencia, hasta el último
#' bit): dan estimaciones equivalentes, no idénticas. Por eso el motor no se elige solo según lo que haya instalado:
#' una corrida se repite exactamente con el mismo motor, la misma semilla y las mismas opciones. `"rcpp"` necesita
#' el paquete Rcpp y un compilador de C++ (en Windows, Rtools); el núcleo se compila la primera vez que se usa y
#' queda guardado para las sesiones siguientes.
#'
#' `nucleos` reparte las cadenas entre procesos (con `parallel`); el resultado no cambia con el número de procesos.
#' En Windows se usa 1.
#'
#' @param simulaciones Número de simulaciones posteriores que se guardan por sexo.
#' @param cadenas Número de cadenas.
#' @param iteraciones Iteraciones por cadena (incluye el calentamiento).
#' @param calentamiento Iteraciones de calentamiento (adaptación) por cadena; múltiplo de 200 y menor que
#'   `iteraciones`.
#' @param adelgazamiento Se guarda una de cada `adelgazamiento` iteraciones después del calentamiento.
#' @param nucleos Procesos en paralelo para las cadenas (en Windows se usa 1).
#' @param motor `"mh"` (por defecto: la log-posterior en R) o `"rcpp"` (la misma en C++, más rápida; requiere
#'   Rcpp y un compilador).
#' @return Objeto de clase `dl_mcmc_opts`, una lista con los campos `draws` (`simulaciones`), `chains`
#'   (`cadenas`), `iter` (`iteraciones`), `warmup` (`calentamiento`), `thin` (`adelgazamiento`), `cores`
#'   (`nucleos`), todos enteros, y `engine` (`motor`). Los campos conservan sus nombres de la versión 0.2.2.
#' @seealso [dl_ajustar()], [dl_mh()] (el muestreador), [dl_rhat()] y [dl_ess()] (la convergencia).
#' @family ajuste
#' @examples
#' # las opciones de producción
#' dl_opciones_mcmc()
#' # cadenas cortas para una prueba (las de dl_correr(rapido = TRUE))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' op
#' op$draws
#' # el motor en C++ (se compila al ajustar)
#' dl_opciones_mcmc(motor = "rcpp", nucleos = 4)
#' # simulaciones de más: un aviso, y el ajuste tendrá las que guardan las cadenas
#' dl_opciones_mcmc(simulaciones = 500, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' @export
dl_opciones_mcmc <- function(simulaciones = 1000L, cadenas = 4L, iteraciones = 50000L, calentamiento = 10000L,
                             adelgazamiento = 10L, nucleos = 1L, motor = c("mh", "rcpp")) {
  motor <- .dl_elegir_motor(motor)
  # Valor recibido para cada campo del objeto y nombre del argumento que lo da (para los mensajes).
  valores <- list(draws = simulaciones, chains = cadenas, iter = iteraciones, warmup = calentamiento,
                  thin = adelgazamiento, cores = nucleos)
  argumento <- c(draws = "simulaciones", chains = "cadenas", iter = "iteraciones", warmup = "calentamiento",
                 thin = "adelgazamiento", cores = "nucleos")
  o <- c(lapply(valores, .dl_entero_opcion), list(engine = motor))
  for (campo in names(argumento))
    if (is.na(o[[campo]]) || o[[campo]] < 1L)
      .dl_stop("`%s` debe ser un entero >= 1; es %s", argumento[[campo]], .dl_describir_objeto(valores[[campo]]))
  if (o$warmup >= o$iter)
    .dl_stop("`calentamiento` (%d) debe ser menor que `iteraciones` (%d)", o$warmup, o$iter)
  if (o$warmup %% .DL_MH_MULTIPLO_CALENTAMIENTO != 0L)
    .dl_stop("`calentamiento` debe ser m\u00faltiplo de %d (por ejemplo 1000); es %d",
             .DL_MH_MULTIPLO_CALENTAMIENTO, o$warmup)
  # dl_ajustar() no puede devolver más simulaciones que las que guardan todas las cadenas juntas.
  guardadas <- .dl_simulaciones_guardadas(o)
  if (o$draws > guardadas)
    .dl_warn(paste0("se piden %d simulaciones y las cadenas guardan %d (cadenas x (iteraciones - calentamiento) / ",
                    "adelgazamiento): el ajuste tendr\u00e1 %d. Para tener m\u00e1s, aumenta `iteraciones` o ",
                    "`cadenas`"), o$draws, guardadas, guardadas)
  structure(o, class = "dl_mcmc_opts")
}

# `motor` (de dl_opciones_mcmc() o dl_cascada()) como lo resolvería match.arg(): el vector por defecto completo
# elige el primero y se aceptan abreviaturas ("r" es "rcpp"). Cualquier otro valor se rechaza con un mensaje en
# español.
.dl_elegir_motor <- function(motor) {
  if (identical(motor, .DL_MOTORES)) return(.DL_MOTORES[1L])
  k <- if (.dl_es_texto1(motor)) pmatch(motor, .DL_MOTORES, nomatch = 0L) else 0L
  if (k == 0L) .dl_stop("`motor` debe ser \"mh\" o \"rcpp\", pero es %s", .dl_describir_objeto(motor))
  .DL_MOTORES[k]
}

# Resolvedor de la EDO del motor: dl_edo_resolver() o, con "rcpp", su gemelo en C++ (misma firma), que se compila
# (o se carga ya compilado) aquí.
.dl_solver_edo <- function(motor) if (motor == "rcpp") .dl_rcpp()$edo else dl_edo_resolver

# Un argumento entero de dl_opciones_mcmc() convertido a entero, o NA si no es un solo número entero: un número con
# decimales (2.5) no se trunca, se rechaza. Como en las versiones anteriores, se acepta el texto de un número ("100").
.dl_entero_opcion <- function(v) {
  if (!is.atomic(v) || length(v) != 1L) return(NA_integer_)
  x <- suppressWarnings(as.numeric(v))
  if (is.na(x) || x != round(x) || abs(x) > .Machine$integer.max) NA_integer_ else as.integer(x)
}

# Simulaciones que guardan todas las cadenas juntas con las opciones `o` (campos chains, iter, warmup y thin): cada
# cadena guarda (iter - warmup) %/% thin (dl_mh()).
.dl_simulaciones_guardadas <- function(o) o$chains * ((o$iter - o$warmup) %/% o$thin)

#' @export
print.dl_mcmc_opts <- function(x, ...) {
  cat(sprintf(paste0("<dl_mcmc_opts> simulaciones %d | cadenas %d | iteraciones %d | calentamiento %d | ",
                     "adelgazamiento %d | n\u00facleos %d | motor %s\n"),
              x$draws, x$chains, x$iter, x$warmup, x$thin, x$cores, x$engine))
  invisible(x)
}

# ---- Cache de ajustes de la sesión ----
#
# dl_ajustar() y dl_ajustar_solo_prior() guardan aquí cada ajuste que calculan. La clave (.dl_cache_key) incluye,
# además del hash de los insumos, la configuración y los datos tal como están en memoria, porque la sensibilidad y
# las etiquetas modifican los insumos sin recalcular el hash (.dl_bundle_con); también las opciones del muestreo
# salvo `cores` (el reparto entre procesos no cambia el resultado), la semilla y si es un ajuste solo con el ancla.
# Los nombres anteriores (dl_fit(), ...) llaman a las funciones nuevas, así que comparten esta cache.
.dl_cache_env <- new.env(parent = emptyenv())
.dl_cache_key <- function(insumos, opciones, semilla, prior_only)
  digest::digest(list(insumos$hash, unclass(insumos$cfg), as.data.frame(insumos$datos),
                      unclass(opciones)[c("draws", "chains", "iter", "warmup", "thin", "engine")],
                      as.numeric(semilla), isTRUE(prior_only)), algo = "sha256")

#' Vaciar la caché de ajustes
#'
#' [dl_ajustar()] y [dl_ajustar_solo_prior()] guardan en la sesión cada ajuste que calculan (clave: los insumos, las
#' opciones del muestreo salvo `nucleos`, la semilla y si es un ajuste solo con el ancla) y lo devuelven sin volver a
#' muestrear. Esta función vacía esa caché, por ejemplo para liberar memoria después de muchas corridas. La caché
#' vive en la sesión: una sesión nueva empieza vacía.
#'
#' @return `NULL`, invisible.
#' @seealso [dl_ajustar()] (su argumento `cache`).
#' @family utilidades
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' # con la caché vacía, el ajuste muestrea; la segunda llamada devuelve el ajuste guardado
#' dl_limpiar_cache()
#' system.time(f <- dl_ajustar(b, op, semilla = 1))
#' system.time(dl_ajustar(b, op, semilla = 1))
#' }
#' @export
dl_limpiar_cache <- function() { rm(list = ls(.dl_cache_env), envir = .dl_cache_env); invisible(NULL) }
