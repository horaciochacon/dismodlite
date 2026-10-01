# Reparto de tareas independientes entre procesos: las cadenas de dl_ajustar() y de dl_mh_cadenas() y las
# combinaciones de la rejilla de dl_sensibilidad().
#
# En Unix las tareas corren en procesos bifurcados (fork, parallel::mclapply). Windows no admite fork: con más de un
# proceso se avisa y se corre en serie. El resultado no depende del reparto porque cada tarea fija su propia semilla
# (R/muestreador.R) y mclapply no toca el generador aleatorio de los procesos hijos (mc.set.seed = FALSE).

# Aplica FUN a 1, ..., n en `cores` procesos y devuelve la lista de resultados en ese orden. Con fork, una tarea que
# falla deja en su lugar un objeto de clase "try-error" (ver .dl_exigir_sin_fallos); en serie, el error se propaga
# tal cual. `.os` permite probar la rama de Windows desde cualquier sistema.
.dl_paralelo <- function(n, FUN, cores = 1L, .os = .Platform$OS.type) {
  if (cores > 1L && identical(.os, "windows")) {
    .dl_warn(paste0("Windows no admite procesos bifurcados (fork): se usa 1 proceso en lugar de %s. ",
                    "Los resultados no cambian, solo el tiempo."), format(cores))
    cores <- 1L
  }
  if (cores > 1L)
    # El aviso de mclapply cuando una tarea falla («scheduled cores ... encountered errors») queda en silencio: el
    # fallo lo reporta .dl_exigir_sin_fallos(), en español y con el mensaje de la tarea.
    withCallingHandlers(parallel::mclapply(seq_len(n), FUN, mc.cores = cores, mc.set.seed = FALSE),
                        warning = function(w)
                          if (.dl_es_aviso_de_fallo(conditionMessage(w))) invokeRestart("muffleWarning"))
  else lapply(seq_len(n), FUN)
}

# TRUE si `msg` es el aviso con que mclapply anuncia que alguna tarea falló (en inglés o en la traducción de la
# sesión). Los demás avisos (p. ej. un proceso que no devolvió resultado) siguen llegando.
.dl_es_aviso_de_fallo <- function(msg) {
  grepl("encountered errors? in user code|function calls? resulted in an error", msg) ||
    msg %in% gettext("all scheduled cores encountered errors in user code", domain = "R-parallel")
}

# Se detiene si alguna tarea de .dl_paralelo() falló, con el mensaje de la primera que falló (sin la función delante:
# el mensaje nombra una sola función, la del usuario). `tarea` las nombra en el mensaje ("una cadena", ...).
.dl_exigir_sin_fallos <- function(resultados, tarea) {
  fallo <- vapply(resultados, inherits, logical(1), "try-error")
  if (!any(fallo)) return(invisible(resultados))
  primero <- resultados[[which(fallo)[1L]]]
  condicion <- attr(primero, "condition")
  detalle <- if (inherits(condicion, "condition")) .dl_detalle(condicion) else trimws(as.character(primero))
  .dl_stop("fall\u00f3 %s: %s", tarea, detalle)
}
