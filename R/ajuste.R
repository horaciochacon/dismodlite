# Ajuste nacional por sexo: dl_ajustar() y dl_ajustar_solo_prior(). Notación: ver R/edo.R.
#
# Para cada sexo se muestrea con MCMC la posterior de theta = (log i en los nudos, log f en los nudos) y, con cada
# simulación guardada, se resuelve la EDO de la prevalencia para tener p, i y f por edad anual. Con la EMR fija en
# cero (emr_prior.tipo cero), theta = (log i en los nudos) y f = 0. Piezas:
#   contexto del sexo (mallas, W, remisión, ancla, datos) y log-posterior de theta   R/verosimilitud.R
#   cadenas de Metropolis-Hastings adaptativo por bloques                            R/muestreador.R
#   reparto de las cadenas entre procesos                                            R/paralelo.R
#   opciones del muestreo y cache de ajustes de la sesión                            R/opciones.R
# Motores: "mh" evalúa la log-posterior en R y es la referencia; "rcpp" evalúa la misma log-posterior en C++
# (R/rcpp.R), más rápido, y coincide con la de R hasta el redondeo.

#' Ajuste nacional por sexo
#'
#' Muestrea con MCMC la posterior de la incidencia y la mortalidad en exceso por edad (en los nudos) de cada sexo,
#' anclada en la prevalencia de referencia y, si la configuración lo declara (`datos_en_ajuste`), en los datos
#' locales. Resuelve la ecuación diferencial del modelo para cada simulación. Los ajustes se guardan en una caché de
#' la sesión.
#'
#' @details
#' Los parámetros son `theta` = (log i en los nudos, log f en los nudos), con i la incidencia y f la mortalidad en
#' exceso, por persona-año; entre los nudos, log i y log f se interpolan linealmente. La log-posterior suma el
#' prior de suavidad de log i (`incidencia.suavidad`), el prior de log f ([dl_prior_emr()], o plano bajo el techo
#' con `mortalidad_exceso.prior: plano`), la verosimilitud del ancla (con su peso `ancla.peso` y su correlación entre
#' edades `ancla.correlacion_edad`) y la de los datos locales que entran al ajuste. La notación y las ecuaciones están
#' en `vignette("el-modelo", package = "dismodlite")`.
#'
#' Con `mortalidad_exceso.prior: cero` la mortalidad en exceso queda fija en 0 y no se estima: los parámetros son
#' solo log i en los nudos (un bloque del muestreador), la ecuación se resuelve con f = 0 y la log-posterior no tiene
#' término de mortalidad en exceso. Sirve para una causa sin muertes: sin ella, log f no tendría información y sus
#' cadenas no convergerían.
#'
#' Cada sexo se muestrea con cadenas de Metropolis-Hastings adaptativo por bloques ([dl_mh()]), con log i y log f
#' en bloques separados. La semilla de un sexo es `semilla + sex_id` y la de cada cadena se deriva de ella: el
#' resultado no depende de `nucleos`. De todas las cadenas juntas se guardan `simulaciones` equiespaciadas
#' y con cada una se resuelve la ecuación de la prevalencia en las edades enteras.
#'
#' Con cadenas cortas (como en los ejemplos) el ajuste es una prueba: el R-hat y el ESS de `mcmc` dicen si las
#' cadenas convergieron (R-hat < 1.01 y ESS >= 400, las compuertas de [dl_exportar_corrida()]).
#'
#' @param insumos Insumos de [dl_insumos()].
#' @param opciones Opciones de [dl_opciones_mcmc()]: simulaciones, cadenas, iteraciones, calentamiento,
#'   adelgazamiento, núcleos y motor. Por defecto, las de producción.
#' @param semilla Semilla (entero), obligatoria: el resultado es reproducible con la misma semilla.
#' @param cache `TRUE` (por defecto) reutiliza un ajuste idéntico (los mismos insumos, opciones y semilla) ya
#'   calculado en la sesión; `FALSE` vuelve a muestrear.
#' @return Objeto de clase `dl_fit`, una lista con:
#'   - `draws_par`: lista con una matriz por sexo (nombres `"1"` y `"2"`): una fila por simulación y una columna por
#'     parámetro (`logi_<nudo>` y `logf_<nudo>`: log i y log f en cada nudo; con la mortalidad en exceso fija en 0,
#'     solo `logi_<nudo>`).
#'   - `draws_q`: tabla (data.table) con una fila por ubicación, sexo, simulación y edad entera: `location_id`,
#'     `sex_id`, `draw`, `edad`, `p` (prevalencia), `i` (incidencia), `f` (mortalidad en exceso) e `ipop`
#'     (incidencia poblacional, i (1 - p): casos nuevos por persona-año de toda la población). Las tasas son por
#'     persona-año.
#'   - `grid`: lista con `edades` (las edades enteras de `draws_q`) y `nudos`.
#'   - `mcmc`: tabla con el R-hat (`rhat`) y el tamaño efectivo de muestra (`ess`) de cada `parametro` y sexo.
#'   - `aceptacion`: tabla con la tasa de aceptación (`tasa`) de cada sexo, `cadena` y `bloque` (1: log i; 2: log f,
#'     que no existe con la mortalidad en exceso fija en 0).
#'   - `bundle_hash`: el hash de los insumos.
#'   - `seed`: la semilla.
#'   - `prior_only`: `FALSE` (`TRUE` en [dl_ajustar_solo_prior()]).
#'   - `params`: las opciones del muestreo (el objeto de [dl_opciones_mcmc()]).
#'   - `ctxs`: el contexto de cada sexo (mallas, interpolación, ancla y datos), que el paquete no vuelve a leer; se
#'     conserva por compatibilidad del objeto.
#' @seealso [dl_opciones_mcmc()] (las cadenas), [dl_estimaciones()] (la tabla de estimaciones), [dl_cascada()] (el
#'   paso siguiente), [dl_limpiar_cache()] (vaciar la caché) y [dl_correr()] (todas las etapas en una llamada).
#' @family ajuste
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' # cadenas cortas, las de dl_correr(rapido = TRUE): una prueba, no una estimación para publicar
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' f
#' head(f$draws_q)
#' f$mcmc
#' # prevalencia por edad, con su intervalo del 95 %
#' prev <- dl_estimaciones(f)
#' head(prev)
#' }
#' @export
dl_ajustar <- function(insumos, opciones = dl_opciones_mcmc(), semilla, cache = TRUE) {
  .dl_exigir_semilla(semilla)
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_exigir_clase(opciones, "dl_mcmc_opts", "opciones", "dl_opciones_mcmc()")
  .dl_exigir_cache(cache)
  clave <- .dl_cache_key(insumos, opciones, semilla, prior_only = FALSE)
  if (cache && !is.null(.dl_cache_env[[clave]])) return(.dl_cache_env[[clave]])

  # Motor rcpp: el núcleo C++ se compila (o se carga ya compilado) antes de repartir las cadenas entre procesos,
  # para que todos hereden el mismo binario.
  motor <- opciones$engine
  resolver_edo <- .dl_solver_edo(motor)

  # Contexto y log-posterior lp(theta) de cada sexo.
  sexos <- as.integer(unlist(insumos$cfg$sexos))
  ctxs <- stats::setNames(lapply(sexos, function(sx) .dl_ctx(insumos, sx)), sexos)
  lps <- lapply(ctxs, function(ctx)
    if (motor == "rcpp") .dl_lp_rcpp(ctx)$total else function(theta) .dl_log_post(theta, ctx))
  # Bloques de Metropolis-Hastings: log i en los nudos y log f en los nudos se proponen por separado. Con la EMR
  # fija en cero (emr_prior.tipo cero) theta solo trae log i: un bloque.
  nk <- length(ctxs[[1]]$nudos)
  bloques <- if (.dl_ctx_emr_cero(ctxs[[1]])) list(seq_len(nk)) else list(seq_len(nk), nk + seq_len(nk))

  # Una tarea por (cadena, sexo), todas en un solo reparto. La semilla de un sexo es semilla + sexo y la de cada
  # cadena se deriva de ella (.dl_correr_cadena): el resultado no depende de cómo se repartan las tareas.
  tareas <- expand.grid(cadena = seq_len(opciones$chains), sx = sexos, KEEP.OUT.ATTRS = FALSE)
  correr <- function(j) {
    sx <- as.character(tareas$sx[j])
    .dl_correr_cadena(lps[[sx]], .dl_theta_inicial(ctxs[[sx]]), bloques, cadena = tareas$cadena[j],
                      iter = opciones$iter, warmup = opciones$warmup, seed = semilla + tareas$sx[j],
                      thin = opciones$thin)
  }
  cadenas <- .dl_paralelo(nrow(tareas), correr, opciones$cores)
  .dl_exigir_sin_fallos(cadenas, "una cadena")

  # Por sexo: de las simulaciones de todas sus cadenas juntas se toman `simulaciones` equiespaciadas, y la EDO de
  # cada una da p, i y f por edad anual (draws_q). Diagnósticos por parámetro (R-hat y ESS con las cadenas por
  # separado) y tasa de aceptación por cadena y bloque.
  draws_par <- list(); draws_q <- list(); mcmc <- list(); aceptacion <- list()
  for (sx in sexos) {
    sexo <- as.character(sx)
    ctx <- ctxs[[sexo]]
    del_sexo <- cadenas[tareas$sx == sx]
    por_cadena <- lapply(del_sexo, `[[`, "draws")
    juntas <- do.call(rbind, por_cadena)
    elegidas <- unique(round(seq(1L, nrow(juntas), length.out = min(opciones$draws, nrow(juntas)))))
    draws_par[[sexo]] <- juntas[elegidas, , drop = FALSE]
    draws_q[[sexo]] <- .dl_tabla_draws_q(lapply(elegidas, function(k) .dl_edo_ctx(juntas[k, ], ctx, resolver_edo)),
                                         insumos$loc_ancla, sx, ctx$anual)
    mcmc[[sexo]] <- data.table::data.table(sex_id = sx, parametro = colnames(juntas),
                                           rhat = dl_rhat(por_cadena), ess = dl_ess(por_cadena))
    aceptacion[[sexo]] <- data.table::rbindlist(lapply(seq_along(del_sexo), function(cd)
      data.table::data.table(sex_id = sx, cadena = cd, bloque = seq_along(del_sexo[[cd]]$aceptacion),
                             tasa = del_sexo[[cd]]$aceptacion)))
  }
  ctx1 <- ctxs[[1]]
  out <- structure(list(draws_par = draws_par,
                        draws_q = data.table::rbindlist(draws_q),
                        grid = list(edades = ctx1$anual, nudos = ctx1$nudos),
                        mcmc = data.table::rbindlist(mcmc),
                        aceptacion = data.table::rbindlist(aceptacion),
                        bundle_hash = insumos$hash, seed = semilla, prior_only = FALSE,
                        params = opciones,
                        # contextos por sexo: nada del paquete los lee (la cascada rehace los suyos desde los
                        # insumos); quedan por compatibilidad del objeto devuelto
                        ctxs = ctxs),
                   class = "dl_fit")
  if (cache) .dl_cache_env[[clave]] <- out
  out
}

#' Ajuste solo con el ancla (sin datos locales)
#'
#' El mismo ajuste de [dl_ajustar()] sin los datos locales en la verosimilitud: la referencia contra la que
#' [dl_etiquetas()] mide cuánto movieron los datos cada celda.
#'
#' @details
#' Sin datos locales en el ajuste (`datos_en_ajuste` vacío, el valor por defecto), los dos ajustes son el mismo: con
#' `ajuste` se devuelve ese ajuste marcado, sin volver a muestrear. Con datos en el ajuste se muestrea de nuevo, con
#' las mismas opciones y la misma semilla, sin las filas de los datos locales.
#'
#' @inheritParams dl_ajustar
#' @param ajuste Ajuste de [dl_ajustar()] de los mismos insumos y semilla (opcional): sin datos locales en el
#'   ajuste, el ajuste solo con el ancla es el mismo y se devuelve marcado, sin volver a muestrear.
#' @return Objeto de clase `dl_fit` con los campos de [dl_ajustar()] y `prior_only = TRUE`; `bundle_hash` es el de
#'   `insumos` (con los datos locales).
#' @seealso [dl_ajustar()], [dl_etiquetas()] (que lo usa).
#' @family ajuste
#' @examples
#' \donttest{
#' b <- dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100))
#' op <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' f <- dl_ajustar(b, op, semilla = 1)
#' # el ejemplo no pone datos locales en el ajuste: es el mismo ajuste, marcado
#' f0 <- dl_ajustar_solo_prior(b, op, semilla = 1, ajuste = f)
#' f0$prior_only
#' identical(f0$draws_q, f$draws_q)
#' }
#' @export
dl_ajustar_solo_prior <- function(insumos, opciones = dl_opciones_mcmc(), semilla, ajuste = NULL, cache = TRUE) {
  .dl_exigir_semilla(semilla)
  .dl_exigir_clase(insumos, "dl_bundle", "insumos", "dl_insumos()")
  .dl_exigir_clase(opciones, "dl_mcmc_opts", "opciones", "dl_opciones_mcmc()")
  .dl_exigir_cache(cache)
  # Sin datos locales en la verosimilitud, dl_ajustar() ya es el ajuste solo con el ancla: con el ajuste de los
  # mismos insumos y semilla se devuelve ese ajuste marcado, sin volver a muestrear.
  if (!nrow(insumos$datos[location_level == 0L]) && !is.null(ajuste)) {
    .dl_exigir_clase(ajuste, "dl_fit", "ajuste", "dl_ajustar()")
    .dl_exigir_mismos_insumos(ajuste, insumos, "ajuste")
    if (!identical(ajuste$seed, semilla))
      .dl_stop("`ajuste` se hizo con la semilla %s y aqu\u00ed se pide %s; usa la misma semilla",
               format(ajuste$seed), format(semilla))
    ajuste$prior_only <- TRUE
    return(ajuste)
  }
  clave <- .dl_cache_key(insumos, opciones, semilla, prior_only = TRUE)
  if (cache && !is.null(.dl_cache_env[[clave]])) return(.dl_cache_env[[clave]])
  sin_datos <- insumos
  sin_datos$datos <- insumos$datos[0]
  f <- dl_ajustar(sin_datos, opciones = opciones, semilla = semilla, cache = FALSE)
  f$prior_only <- TRUE
  f$bundle_hash <- insumos$hash
  if (cache) .dl_cache_env[[clave]] <- f
  f
}

# `cache` de dl_ajustar() y dl_ajustar_solo_prior(): TRUE o FALSE.
.dl_exigir_cache <- function(cache) .dl_exigir_si_no(cache, "cache")

#' @export
print.dl_fit <- function(x, ...) {
  cat(sprintf("<dl_fit>%s insumos %s | semilla %s | motor %s\n",
              if (isTRUE(x$prior_only)) " (solo con el ancla)" else "", substr(x$bundle_hash, 1, 12), x$seed,
              x$params$engine))
  cat(sprintf("  sexos: %s | simulaciones por sexo: %d | edades %d-%d\n",
              paste(names(x$draws_par), collapse = ", "),
              nrow(x$draws_par[[1]]), min(x$grid$edades), max(x$grid$edades)))
  cat(sprintf("  R-hat m\u00e1ximo %.3f | ESS m\u00ednimo %.0f | aceptaci\u00f3n %.2f-%.2f\n",
              max(x$mcmc$rhat), min(x$mcmc$ess), min(x$aceptacion$tasa), max(x$aceptacion$tasa)))
  invisible(x)
}
