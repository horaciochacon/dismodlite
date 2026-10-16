# Escena 11, una corrida de principio a fin: dl_correr() del proyecto de ejemplo con la corrida de prueba
# (rapido = TRUE, semilla 1), escrita en una carpeta temporal. Exporta lo que la escena dibuja, todo leído de la
# corrida real: los pasos en el orden del código de .dl_correr_proyecto() (comprobado contra los mensajes que emitió
# la corrida), la revisión del proyecto, las cadenas de prueba y las de producción, las dos compuertas, la cascada,
# las etiquetas, la sensibilidad, una celda de cada medida, el árbol de la carpeta y los campos del manifiesto.
# Los diagnósticos son los de la corrida de prueba: cadenas cortas que no convergen, como dice la corrida misma.
exportar <- function(ctx) {
  # ---- 1. La revisión del proyecto (dl_revisar_proyecto) ----
  utils::capture.output(revision <- suppressWarnings(dl_revisar_proyecto(dl_ejemplo(), causa = 9100)))
  revision <- as.data.frame(revision)
  tablas_contrato <- c("ubicaciones", "poblacion", "ancla", "covariables", "betas", "datos", "severidad",
                       "proxies_crudos")
  filas_de <- function(paso) {
    d <- revision$detalle[revision$paso == paso][1]
    as.integer(sub("^leída: ([0-9]+) fila.*$", "\\1", d))
  }
  tablas <- lapply(tablas_contrato, function(x) list(tabla = x, estado = revision$estado[revision$paso == x][1],
                                                     filas = filas_de(x)))

  # ---- 2. La corrida de prueba, con sus mensajes ----
  salida <- file.path(tempdir(), "escena-11")
  unlink(salida, recursive = TRUE)
  dir.create(salida, recursive = TRUE)
  mensajes <- character()
  run <- withCallingHandlers(
    suppressWarnings(dl_correr(dl_ejemplo(), causa = 9100, semilla = ctx$semilla, rapido = TRUE,
                               carpeta_salida = salida)),
    message = function(m) {
      mensajes <<- c(mensajes, sub("\n$", "", sub("^dl_correr\\(\\): ", "", conditionMessage(m))))
      invokeRestart("muffleMessage")
    })
  m <- run$manifest
  d <- run$dir
  # la carpeta temporal no dice nada: en los mensajes queda como <carpeta_salida>
  mensajes <- gsub(normalizePath(salida), "<carpeta_salida>", gsub(salida, "<carpeta_salida>", mensajes, fixed = TRUE),
                   fixed = TRUE)

  # ---- 3. Los pasos, en el orden del código ----
  # Cada paso es la llamada que lo hace dentro de .dl_correr_proyecto(); `mensaje` es el comienzo del mensaje con que
  # la corrida lo anuncia (NA: el paso no emite mensaje propio).
  etapas <- list(
    list(clave = "insumos", llamada = "dl_insumos(", funcion = "dl_insumos()", mensaje = "causa 9100: insumos",
         escena = "04"),
    list(clave = "ajuste", llamada = "dl_ajustar(", funcion = "dl_ajustar()", mensaje = "ajuste nacional",
         escena = "02-08"),
    list(clave = "compuerta1", llamada = ".dl_compuerta_convergencia(", funcion = "compuerta 1: convergencia",
         mensaje = NA, escena = "08"),
    list(clave = "solo_prior", llamada = "dl_ajustar_solo_prior(", funcion = "dl_ajustar_solo_prior()",
         mensaje = NA, escena = "05"),
    list(clave = "cascada", llamada = "dl_cascada(", funcion = "dl_cascada()", mensaje = "cascada subnacional",
         escena = "09"),
    list(clave = "validacion", llamada = "dl_validar_ancla(", funcion = "dl_validar_ancla()",
         mensaje = "validación contra el ancla", escena = "05"),
    list(clave = "compuerta2", llamada = ".dl_compuerta_ancla(", funcion = "compuerta 2: error del ancla",
         mensaje = NA, escena = "05"),
    list(clave = "avd", llamada = "dl_avd(", funcion = "dl_avd()", mensaje = NA, escena = "10"),
    list(clave = "etiquetas", llamada = "dl_etiquetas(", funcion = "dl_etiquetas()", mensaje = "etiquetas",
         escena = "07"),
    list(clave = "sensibilidad", llamada = "dl_sensibilidad(", funcion = "dl_sensibilidad()",
         mensaje = "sensibilidad", escena = "05, 09"),
    list(clave = "resumen", llamada = "dl_resumir(", funcion = "dl_resumir()", mensaje = NA, escena = "04"),
    list(clave = "carpeta", llamada = "dl_exportar_corrida(", funcion = "dl_exportar_corrida()",
         mensaje = "corrida escrita", escena = "11")
  )
  # El orden del código: la primera aparición de cada llamada en el cuerpo de .dl_correr_proyecto().
  codigo <- paste(deparse(dismodlite:::.dl_correr_proyecto, width.cutoff = 500L), collapse = "\n")
  posicion <- vapply(etapas, function(e) regexpr(e$llamada, codigo, fixed = TRUE)[[1]], 0)
  if (any(posicion < 0)) stop("una llamada no está en .dl_correr_proyecto(): ",
                              etapas[[which(posicion < 0)[1]]]$llamada)
  if (is.unsorted(posicion)) stop("los pasos de la escena no siguen el orden del código de dl_correr()")
  # Y el de la corrida: los mensajes de los pasos que los emiten aparecen en el mismo orden.
  donde <- vapply(etapas, function(e) if (is.na(e$mensaje)) NA_real_
                  else as.numeric(which(startsWith(mensajes, e$mensaje))[1]), 0)
  if (anyNA(donde[!vapply(etapas, function(e) is.na(e$mensaje), NA)])) stop("falta el mensaje de un paso en la corrida")
  if (is.unsorted(donde[!is.na(donde)])) stop("los mensajes de la corrida no siguen el orden de los pasos")
  for (k in seq_along(etapas)) {
    etapas[[k]]$posicion_codigo <- posicion[k]
    etapas[[k]]$orden_mensaje <- if (is.na(donde[k])) NULL else donde[k]
    etapas[[k]]$llamada <- NULL
  }

  # ---- 4. Las cadenas: prueba y producción ----
  prueba <- m$params$opts
  produccion <- formals(dl_opciones_mcmc)
  produccion <- list(draws = as.integer(produccion$simulaciones), chains = as.integer(produccion$cadenas),
                     iter = as.integer(produccion$iteraciones), warmup = as.integer(produccion$calentamiento))

  # ---- 5. Diagnósticos y cascada, desde la carpeta ----
  leer <- function(...) utils::read.csv(file.path(d, ...), fileEncoding = "UTF-8",
                                        colClasses = c(location_id = "character"))
  mcmc <- leer("diagnostics", "mcmc.csv")
  renorm <- leer("diagnostics", "renorm.csv")
  etiquetas <- leer("etiquetas", paste0(run$run_id, ".csv"))
  sens <- leer("diagnostics", "sensibilidad.csv")
  grilla <- unique(sens[, c("lambda", "rho", "kappa")])
  celda <- function(medida) {
    x <- leer("cause", medida, paste0(run$run_id, ".csv"))
    f <- x[x$location_id == "123" & x$sex_id == 2 & x$age_group_name == "70-74 years", ]
    if (nrow(f) != 1L) stop("no hay una celda nacional de mujeres de 70 a 74 en ", medida)
    sub <- x[x$location_level == 1 & x$sex_id == 2 & x$age_group_name == "70-74 years", ]
    list(medida = medida, metrica = f$metric_name, banda = f$age_group_name, sexo = f$sex_name, val = f$val,
         lower = f$lower, upper = f$upper, filas = nrow(x), ubicaciones = length(unique(x$location_id)),
         sexos = length(unique(x$sex_id)), bandas = length(unique(x$age_group_id)), subnacional = sub$val[order(sub$val)],
         subnacional_nombre = sub$location_name[order(sub$val)])
  }

  # ---- 6. El árbol de la carpeta ----
  archivos <- list.files(d, recursive = TRUE)
  primer <- sub("/.*$", "", archivos)
  arbol <- lapply(unique(primer), function(x) list(nombre = if (any(archivos == x)) x else paste0(x, "/"),
                                                    archivos = sum(primer == x & archivos != x)))
  arbol <- arbol[order(vapply(arbol, function(a) a$nombre == "manifest.yaml", NA))]

  list(
    fuente = "Corrida de prueba del ejemplo (datos sintéticos): dl_correr(rapido = TRUE, semilla = 1)",
    llamada = "dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE)",
    comandos = c("dl_nuevo_proyecto()", "dl_revisar_proyecto()", "dl_correr()"),
    revision = list(tablas = tablas, errores = sum(revision$estado == "error"),
                    avisos = sum(revision$estado == "aviso"), comprobaciones = nrow(revision)),
    mensajes = mensajes,
    etapas = etapas,
    prueba = prueba, produccion = produccion,
    datos_fuera = as.integer(sub("^([0-9]+) fila.*$", "\\1", grep("^[0-9]+ fila\\(s\\) nacionales", mensajes, value = TRUE)[1])),
    bundle_hash = m$inputs$bundle_hash,
    compuertas = list(rhat_max = m$validacion$gates$rhat_max, ess_min = m$validacion$gates$ess_min,
                      force = isTRUE(m$validacion$gates$force), rhat_limite = 1.01,
                      ess_minimo = as.numeric(formals(dismodlite:::.dl_compuerta_convergencia)$ess_minimo),
                      err_ancla = m$validacion$anchor_identity$err_rel_mediano,
                      err_maximo = m$validacion$anchor_identity$gate_err_mediano,
                      parametros = nrow(mcmc),
                      rhat_max_csv = max(mcmc$rhat), ess_min_csv = min(mcmc$ess)),
    cascada = list(departamentos = m$cascada$departamentos, renorm_min = m$cascada$renorm_min,
                   renorm_max = m$cascada$renorm_max, renorm_min_csv = min(renorm$factor),
                   renorm_max_csv = max(renorm$factor)),
    etiquetas = list(celdas = nrow(etiquetas), nacionales = sum(etiquetas$location_id == "123"),
                     tabla = as.list(table(etiquetas$etiqueta))),
    sensibilidad = list(lambda = sort(unique(grilla$lambda)), rho = sort(unique(grilla$rho[!is.na(grilla$rho)])),
                        kappa = sort(unique(grilla$kappa[!is.na(grilla$kappa)])), filas = nrow(sens)),
    sensibilidad_corrida = list(lambda = m$params$lambda, rho = m$params$rho, kappa = m$params$kappa),
    celdas = list(prevalencia = celda("prevalence"), avd = celda("yld")),
    run_id = run$run_id,
    arbol = arbol, n_archivos = length(archivos),
    por_defecto = names(m$configuracion$por_defecto),
    por_defecto_valores = unname(unlist(m$configuracion$por_defecto)),
    limitaciones = unlist(m$limitaciones),
    decisiones = unlist(m$decisiones),
    files = lapply(m$files, function(f) list(path = basename(dirname(f$path)), sha256 = f$sha256, rows = f$rows))
  )
}
