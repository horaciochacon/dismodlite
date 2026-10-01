# Comprueba que la versión 0.2.2 del paquete (etiqueta git v0.2.2) lee el conjunto de datos de ejemplo
# inst/extdata/acs_peru_completo (el ejemplo en el formato completo): para cada causa (9100-9103) y cada variante de
# corrida arma el bundle con el código de esa versión y verifica que la variante se aplicó. Después revisa la plausibilidad de los datos: prevalencia del padre a
# los 70-74 años, incidencia implícita de un ajuste corto de 9100 frente al ancla, validación de amplitud de la
# cascada con el held-out departamental y factor COMO por edad.
#
# Uso, desde la raíz del repositorio:
#   Rscript data-raw/verificar_acs_legado.R
# El código de v0.2.2 se carga desde un worktree temporal de git (cargar_legado() de data-raw/legado.R) y las rutas,
# la semilla, la procedencia y la partición de severidad son las del arnés de compatibilidad (rutas_acs() y .ARNES
# de tests/testthat/helper-arnes-api.R). Sale con estado 1 si algún bundle no se arma o alguna comprobación falla.
#
# Las variantes se expresan como `cambios` (overrides) de dl_config(). Ojo: utils::modifyList ignora las listas sin
# nombres, así que una clave que es una secuencia en el YAML (medidas_entrada, decisiones, sexos) se reemplaza con un
# vector atómico (c(...)), nunca con list(...): list("csmr") dejaría la clave del YAML intacta sin avisar.

source(file.path("data-raw", "legado.R"))   # %||%, cargar_legado(), cargar_arnes()

OPC_MINIMAS <- list(draws = 10L, chains = 1L, iter = 600L, warmup = 200L)   # ajuste mínimo para correr la cascada

# Secuelas de los estados sintomáticos (leve y moderada) de una causa; las del padre son las de sus subtipos.
secuelas_sintomaticas <- function(causa) {
  subtipos <- if (causa == 9100L) 9101:9103 else causa
  as.integer(c(rbind(subtipos * 10L + 2L, subtipos * 10L + 3L)))
}

# Cada variante: función de la causa que devuelve los cambios, los argumentos de rutas_acs() y una comprobación
# comprobar(b, ctx) que devuelve los problemas encontrados (character(0) si la variante quedó aplicada). ctx trae la
# causa, la raíz de los datos, el entorno e con el código 0.2.2 y armar(), que arma otro bundle de la misma causa.
# `nota` explica lo que la variante no puede comprobar en esa causa.
VARIANTES <- list(
  nacional = function(causa) list(
    datos = FALSE, proxies = FALSE,
    comprobar = function(b, ctx) c(if (nrow(b$datos)) "trae datos", if (nrow(b$cov_proxy)) "trae proxies")),
  datos_y_proxies = function(causa) list(
    nota = if (causa != 9100L) "solo proxies y configuraci\u00f3n: datos.csv es de 9100",
    comprobar = function(b, ctx) c(
      if (nrow(b$cov_proxy) != 25L * (2L * 9L + 2L)) "proxies incompletos",
      if (causa == 9100L && nrow(b$datos[tipo_dato == "csmr" & location_level == 1L]) != 120L)
        "sin el csmr departamental held-out (10 departamentos x 2 sexos x 6 bandas)",
      if (causa == 9100L && !nrow(b$datos[tipo_dato == "prev_estudio" & location_level == 1L]))
        "sin la prevalencia departamental held-out",
      if (causa == 9100L && !all(b$datos$year_start == 2019L)) "held-out de otro a\u00f1o",
      if (nrow(b$datos[location_level == 0L])) "datos nacionales sin medidas_entrada")),
  plana = function(causa) list(
    proxies = FALSE,
    cambios = list(cascada = list(modo = list(valor = "plana", procedencia = .ARNES$procedencia))),
    comprobar = function(b, ctx) {
      prob <- c(if (!identical(b$cfg$cascada$modo$valor, "plana")) "modo plana no aplicado",
                if (nrow(b$cov_proxy)) "trae proxies")
      if (length(prob)) return(prob)
      # Cascada plana sobre un ajuste mínimo: 25 departamentos y renormalización 1 (dX = 0 y la población
      # departamental suma la nacional).
      f <- ctx$e$dl_fit(b, opts = do.call(ctx$e$dl_mcmc_opts, OPC_MINIMAS), seed = .ARNES$semilla, cache = FALSE)
      cs <- ctx$e$dl_cascade(f, b, seed = .ARNES$semilla)
      c(if (!identical(cs$modo, "plana")) "la cascada no es plana",
        if (length(cs$departamentos) != 25L) "la cascada no tiene 25 departamentos",
        if (max(abs(cs$renorm$factor - 1)) > 1e-9) "renormalizaci\u00f3n distinta de 1 en la cascada plana")
    }),
  datos_locales = function(causa) list(
    nota = if (causa != 9100L) "solo configuraci\u00f3n: datos.csv es de 9100",
    # El arnés corre este ajuste con cadenas cortas (2000/1000), que no llegan a la puerta 0.05 de anchor_identity:
    # la relaja a 0.10 con su procedencia (ver data-raw/LEEME.md); aquí se comprueba que la versión 0.2.2 la acepta.
    cambios = list(medidas_entrada = c("prev_estudio", "incidencia", "csmr"),
                   anchor = list(lambda = 0.5, gate_err_mediano = list(
                     valor = 0.10,
                     procedencia = "cadenas cortas del ejemplo; con cadenas largas el error baja de 0.05")),
                   decisiones = c(paste0(.ARNES$procedencia, "."),
                                  "lambda 0.5: el csmr del registro vital entra al ajuste y ya inform\u00f3 el ancla")),
    comprobar = function(b, ctx) c(
      if (!setequal(unlist(b$cfg$medidas_entrada), c("prev_estudio", "incidencia", "csmr")))
        "medidas_entrada no aplicada",
      if (!isTRUE(all.equal(b$cfg$anchor$lambda, 0.5))) "lambda no aplicado",
      if (!isTRUE(all.equal(b$cfg$anchor$gate_err_mediano$valor, 0.10))) "gate_err_mediano no aplicado",
      if (causa == 9100L && !setequal(b$datos[location_level == 0L & !outlier]$tipo_dato,
                                      c("prev_estudio", "incidencia", "csmr"))) "faltan tipos en la likelihood",
      if (causa == 9100L && !any(b$datos$outlier)) "sin el valor at\u00edpico")),
  componente = function(causa) list(
    sin_severidad = TRUE,
    cambios = list(anchor = list(componente = list(sequela_ids = secuelas_sintomaticas(causa),
                                                   motivo = "solo los estados sintom\u00e1ticos (ejemplo)")),
                   severidad = list(fuente = "mod", run_id = .ARNES$particion)),
    comprobar = function(b, ctx) c(
      if (is.null(b$componente)) "sin componente",
      if (!setequal(b$severidad$health_state_id, c(9801L, 9802L))) "la severidad no es la del componente",
      if (!(b$componente$fraccion_prevalencia > 0 && b$componente$fraccion_prevalencia < 1))
        "fracci\u00f3n de prevalencia fuera de (0, 1)")),
  severidad_particion = function(causa) list(
    sin_severidad = TRUE,
    cambios = list(severidad = c(list(fuente = "mod", run_id = .ARNES$particion),
                                 if (causa != 9100L) list(padre = 9100L))),
    comprobar = function(b, ctx) {
      tabla <- data.table::fread(file.path(ctx$raiz, "severidad", sprintf("%d.csv", causa)))
      m <- merge(b$severidad[, list(health_state_id, proportion)],
                 tabla[, list(health_state_id, prop_tabla = proportion)],
                 by = "health_state_id")
      c(if (nrow(m) != 3L) "estados distintos de la tabla",
        if (nrow(m) && max(abs(m$proportion - m$prop_tabla)) > 1e-9) "proporciones distintas de la tabla",
        if (causa != 9100L && is.null(attr(b$severidad, "cuota_hija"))) "sin cuota de la hija")
    }),
  anio_2019 = function(causa) list(
    anio = 2019L,
    cambios = list(years = list(ajuste = 2019L)),
    comprobar = function(b, ctx) c(if (!all(b$prior_gbd$year == 2019L)) "ancla de otro a\u00f1o",
                                   if (!all(b$poblacion$year == 2019L)) "poblaci\u00f3n de otro a\u00f1o",
                                   if (!all(b$cov_proxy$year == 2019L)) "proxies de otro a\u00f1o")),
  proyeccion_2024 = function(causa) list(
    anio = 2024L,
    cambios = list(years = list(ajuste = 2024L, ancla = list(valor = 2023L, procedencia = .ARNES$procedencia))),
    comprobar = function(b, ctx) {
      # El ancla y las covariables nacionales de 2024 son las de 2023 reetiquetadas, con los mismos valores.
      b23 <- ctx$armar(anio = 2023L)
      igual <- function(x, y, claves) {
        m <- merge(x, y, by = claves, suffixes = c("_24", "_23"))
        nrow(m) == nrow(x) && nrow(m) == nrow(y) &&
          identical(m$val_24, m$val_23) && identical(m$lower_24, m$lower_23) && identical(m$upper_24, m$upper_23)
      }
      c(if (!all(b$prior_gbd$year == 2024L)) "ancla no reetiquetada a 2024",
        if (!igual(b$prior_gbd[, !"year"], b23$prior_gbd[, !"year"],
                   c("measure_id", "location_id", "sex_id", "age_group_id"))) "ancla de 2024 distinta de la de 2023",
        if (!all(b$poblacion$year == 2024L)) "poblaci\u00f3n de otro a\u00f1o",
        if (isTRUE(all.equal(sum(b$poblacion$val), sum(b23$poblacion$val))))
          "poblaci\u00f3n de 2024 igual a la de 2023",
        if (!nrow(b$cov_proxy) || !all(b$cov_proxy$year == 2024L)) "proxies de 2024 ausentes",
        if (!any(b$cov_valores$year == 2024L)) "covariables no reetiquetadas a 2024",
        if (!igual(b$cov_valores[year == 2024L, list(covariate_id, sex_id, age_group_id, val, lower, upper)],
                   b23$cov_valores[year == 2023L, list(covariate_id, sex_id, age_group_id, val, lower, upper)],
                   c("covariate_id", "sex_id", "age_group_id"))) "covariables de 2024 distintas de las de 2023")
    }))

main <- function() {
  options(width = 200L)
  raiz_repo <- normalizePath(getwd(), winslash = "/")
  raiz <- file.path(raiz_repo, "inst", "extdata", "acs_peru_completo")
  carpeta_config <- file.path(raiz, "config")
  if (!dir.exists(raiz)) stop("verificar_acs_legado.R: ejecutar desde la ra\u00edz del repositorio, tras generar datos")
  e <- cargar_legado(raiz_repo)$e
  cargar_arnes(raiz_repo)   # en el entorno global: .ARNES, api_legado(), rutas_acs(), ...
  api <- api_legado(e)
  cat("\n")

  armar <- function(causa, cambios = NULL, anio = 2023L, datos = TRUE, proxies = TRUE, sin_severidad = FALSE) {
    r <- rutas_acs(api, raiz, causa, anio = anio, con_datos = datos, con_proxies = proxies,
                   sin_severidad = sin_severidad)
    b <- suppressMessages(e$dl_bundle(e$dl_config(causa, carpeta_config, cambios), r))
    attr(b, "rutas") <- r
    b
  }

  # ---- 1. Un bundle por causa y variante ----
  filas <- list()
  for (causa in 9100:9103) for (v in names(VARIANTES)) {
    var <- VARIANTES[[v]](causa)
    t0 <- proc.time()[["elapsed"]]
    res <- tryCatch({
      b <- armar(causa, var$cambios, anio = var$anio %||% 2023L, datos = var$datos %||% TRUE,
                 proxies = var$proxies %||% TRUE, sin_severidad = isTRUE(var$sin_severidad))
      ctx <- list(causa = causa, raiz = raiz, e = e,
                  armar = function(...) armar(causa, datos = FALSE, proxies = var$proxies %||% TRUE, ...))
      # La tabla de severidad del bundle es la de la causa del config: el paquete no lo comprueba (usa todas las
      # filas del archivo) y otra causa daría AVD y COMO equivocados sin ningún error.
      prob <- c(if (!nrow(b$severidad) || !all(b$severidad$cause_id == b$cfg$cause_id))
                  "la severidad no es de la causa del config",
                suppressMessages(suppressWarnings(var$comprobar(b, ctx))))
      list(estado = if (length(prob)) "FALLA" else "ok", detalle = paste(prob, collapse = "; "),
           prior = nrow(b$prior_gbd), datos = nrow(b$datos), proxies = nrow(b$cov_proxy),
           poblacion = nrow(b$poblacion), severidad = nrow(b$severidad))
    }, error = function(err) list(estado = "ERROR", detalle = gsub("\n", " | ", conditionMessage(err))))
    filas[[length(filas) + 1L]] <- data.table::data.table(
      causa = causa, variante = v, estado = res$estado, prior = res$prior %||% NA_integer_,
      datos = res$datos %||% NA_integer_, proxies = res$proxies %||% NA_integer_,
      poblacion = res$poblacion %||% NA_integer_, severidad = res$severidad %||% NA_integer_,
      segundos = round(proc.time()[["elapsed"]] - t0, 1), nota = var$nota %||% "", detalle = res$detalle)
  }
  tabla <- data.table::rbindlist(filas)
  cat("== Bundles con el c\u00f3digo 0.2.2 ==\n")
  print(tabla[, !"detalle"], nrows = 100L)
  malos <- tabla[estado != "ok"]
  if (nrow(malos)) {
    cat("\nProblemas:\n")
    for (i in seq_len(nrow(malos))) cat(sprintf("  %d / %s: %s\n", malos$causa[i], malos$variante[i], malos$detalle[i]))
  }

  # ---- 2. Plausibilidad ----
  cat("\n== Plausibilidad ==\n")
  # Bundle de 9100 con los datos (sin medidas_entrada solo entra el held-out departamental, que no toca el ajuste) y
  # los proxies, para validar también la cascada.
  b <- armar(9100L)
  pl <- attr(b, "rutas")
  prev <- b$prior_gbd[measure_id == 5L & age_group_id == 19L, list(sex_id, val)]
  ok_prev <- all(prev$val > 0.02 & prev$val < 0.10)
  cat(sprintf("Prevalencia de 9100 a los 70-74 a\u00f1os (ancla 2023): %s -> %s (se exige entre 2 %% y 10 %%)\n",
              paste(sprintf("sexo %d: %.2f %%", prev$sex_id, 100 * prev$val), collapse = ", "),
              if (ok_prev) "ok" else "FALLA"))
  t0 <- proc.time()[["elapsed"]]
  f <- e$dl_fit(b, opts = e$dl_mcmc_opts(draws = 40L, chains = 2L, iter = 4000L, warmup = 2000L), seed = .ARNES$semilla)
  cs <- suppressWarnings(e$dl_cascade(f, b, seed = .ARNES$semilla))
  v <- suppressMessages(e$dl_validate_gbd(f, b, pl, cascade = cs))
  res <- attr(v, "resumen")
  err <- res[check == "implied_incidence"]$err_rel_mediano
  ok_inc <- length(err) == 1L && is.finite(err) && err < 0.25
  cat(sprintf(paste("Ajuste corto de 9100 (40 simulaciones, 2 cadenas, 4000 iteraciones) y cascada (%.0f s):",
                    "R-hat m\u00e1ximo %.3f\n"),
              proc.time()[["elapsed"]] - t0, max(f$mcmc$rhat)))
  cat(sprintf("  anchor_identity: error relativo mediano %.3f\n", res[check == "anchor_identity"]$err_rel_mediano))
  cat(sprintf("  implied_incidence: error relativo mediano %.3f -> %s (se exige < 0.25)\n", err,
              if (ok_inc) "ok" else "FALLA"))
  # Validación de amplitud: gradiente departamental predicho por la cascada (2023) frente al csmr departamental
  # held-out (2019); la verdad departamental usa los mismos betas, así que la pendiente debe rondar 1.
  am <- attr(v, "amplitud")
  ok_amp <- !is.null(am) && nrow(am) == 2L && all(am$n_departamentos_std == 10L) &&
    all(am$pendiente_std > 0.75 & am$pendiente_std < 1.25) && all(am$spearman_std > 0.5)
  if (is.null(am)) cat("  amplitud_csmr: no se calcul\u00f3 -> FALLA\n") else
    for (i in seq_len(nrow(am)))
      cat(sprintf(paste("  amplitud_csmr sexo %d: pendiente %.3f [%.3f, %.3f], estandarizada %.3f [%.3f, %.3f];",
                        "Spearman %.2f, estandarizado %.2f; %d departamentos\n"),
                  am$sex_id[i], am$pendiente[i], am$pendiente_lower[i], am$pendiente_upper[i], am$pendiente_std[i],
                  am$pendiente_std_lower[i], am$pendiente_std_upper[i], am$spearman[i], am$spearman_std[i],
                  am$n_departamentos_std[i]))
  cat(sprintf("  amplitud_csmr -> %s (se exige pendiente estandarizada entre 0.75 y 1.25 y Spearman > 0.5)\n",
              if (ok_amp) "ok" else "FALLA"))
  # Factor COMO empírico yld / (prev x sum pi DW): varía con la edad y queda por debajo de 1 en todas las causas.
  ok_como <- TRUE
  for (causa in 9100:9103) {
    bc <- if (causa == 9100L) b else armar(causa, datos = FALSE, proxies = FALSE)
    cf <- e$dl_como_factor(bc, attr(bc, "rutas"))
    rango <- diff(range(cf$factor))
    ok <- max(cf$factor) < 1 && (causa != 9100L || rango > 0.1)
    ok_como <- ok_como && ok
    cat(sprintf("Factor COMO de %d por banda: de %.3f a %.3f (rango %.3f) -> %s\n", causa, min(cf$factor),
                max(cf$factor), rango, if (ok) "ok" else "FALLA"))
  }
  cat("  (se exige m\u00e1ximo < 1 en todas las causas y rango > 0.1 en 9100)\n")

  todo_ok <- !nrow(malos) && ok_prev && ok_inc && ok_amp && ok_como
  cat(sprintf("\nResultado: %s\n", if (todo_ok) "todo en orden" else "HAY FALLAS"))
  if (todo_ok) 0L else 1L
}

estado <- main()
quit(save = "no", status = estado)
