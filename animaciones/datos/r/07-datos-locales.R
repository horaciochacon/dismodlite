# Escena 07, los datos locales: qué filas de la tabla datos entran al ajuste nacional, con qué cantidad del modelo se
# compara cada dato (p, i (1 - p) o p f, promediada en su intervalo de edad), cuánto vale su término l_d y qué cambia
# en la prevalencia cuando los datos entran.
#
# Dos ajustes nacionales del ejemplo (causa 9100, datos sintéticos):
#   sin datos  ctx$ajuste(): la configuración del ejemplo, datos_en_ajuste vacío (el valor por defecto).
#   con datos  una copia del ejemplo con datos_en_ajuste: [prevalencia, incidencia, mortalidad] y ancla.peso 0.5, como
#              en la guía «Datos locales» (sección «El doble conteo y ancla.peso»); 4 cadenas de 8000 iteraciones
#              (4000 de calentamiento), semilla 1. Con 4000 iteraciones, como ctx$ajuste(), su R-hat máximo pasaba
#              de 1.15; con 8000 queda bajo 1.06, como el del ajuste sin datos.
# La escena muestra a los hombres (sexo 1): con ellos el estudio de prevalencia de 80 años y más queda bajo el ancla
# y la curva se mueve hacia él.
#
# Los l_d se calculan en theta = la media posterior de theta del ajuste con datos (hombres), con las funciones del
# paquete (.dl_lp_lognormal, .dl_lp_binomial), y su suma por tipo se compara con .dl_lp_componentes(): deben coincidir
# a 1e-9.
exportar <- function(ctx) {
  ns <- asNamespace("dismodlite")
  sexo <- 1L

  # ---- La tabla datos del ejemplo, tal como viene ----
  datos <- data.table::fread(dl_ejemplo("datos.csv"), colClasses = list(character = "ubicacion"), encoding = "UTF-8")
  nac <- datos[ubicacion == "123" & anio == 2023]
  sub <- datos[ubicacion != "123"]

  # ---- El ajuste con datos ----
  copia <- dl_ejemplo(copiar_en = file.path(tempdir(), "escena07_con_datos"))
  yaml <- file.path(copia, "config", "9100.yaml")
  writeLines(c(readLines(yaml, encoding = "UTF-8"), "", "ancla:", "  peso: 0.5", "",
               "datos_en_ajuste: [prevalencia, incidencia, mortalidad]"), yaml, useBytes = TRUE)
  insumos_datos <- suppressMessages(dl_insumos(dl_proyecto(copia, causa = 9100)))
  opciones_datos <- dl_opciones_mcmc(simulaciones = 1000, cadenas = 4, iteraciones = 8000, calentamiento = 4000)
  ajuste_datos <- suppressMessages(dl_ajustar(insumos_datos, opciones_datos, semilla = ctx$semilla))
  ajuste_ancla <- ctx$ajuste()
  stopifnot(ajuste_datos$ctxs[[as.character(sexo)]]$ancla$lambda == 0.5,
            ajuste_ancla$ctxs[[as.character(sexo)]]$ancla$lambda == 1,
            nrow(ajuste_ancla$ctxs[[as.character(sexo)]]$datos) == 0)

  # Filas que entran al ajuste con datos (las de .dl_datos_verosimilitud de cada sexo).
  entran <- rbind(ajuste_datos$ctxs[["1"]]$datos, ajuste_datos$ctxs[["2"]]$datos)

  # ---- Los l_d de los hombres en la media posterior de theta ----
  cx <- ajuste_datos$ctxs[[as.character(sexo)]]
  theta <- colMeans(ajuste_datos$draws_par[[as.character(sexo)]])
  sol <- ns$.dl_edo_ctx(theta, cx)
  comp <- ns$.dl_lp_componentes(theta, cx)
  d <- cx$datos
  integ <- ns$.DL_TIPOS_DATO$integrando[d$tipo_cod]
  q <- vapply(seq_len(nrow(d)), function(j)
    ns$.dl_q_intervalos(ns$.dl_integrando(sol, integ[j]), cx$pesos_datos[j]), numeric(1))
  l <- ifelse(d$ruta == ns$.DL_RUTA_LOGNORMAL, ns$.dl_lp_lognormal(q, d$val, d$s_log, d$eta),
              ns$.dl_lp_binomial(q, d$x, d$n_ef))
  por_tipo <- tapply(l, d$tipo_dato, sum)
  for (tp in names(por_tipo))
    stopifnot(abs(por_tipo[[tp]] - comp[[paste0("datos_", tp)]]) < 1e-9)
  stopifnot(abs(sum(l) - comp[["datos"]]) < 1e-9)
  nombre_medida <- c(prev_estudio = "prevalencia", incidencia = "incidencia", csmr = "mortalidad")
  # 95 % de la log-normal de un dato con valor y error: exp(log val ± 1.96 s_log); con casos y muestra, x / n solo.
  datos_h <- data.frame(
    medida = unname(nombre_medida[d$tipo_dato]), inicio = d$age_start, fin = d$age_end,
    ruta = ifelse(d$ruta == ns$.DL_RUTA_LOGNORMAL, "lognormal", "binomial"),
    val = d$val, se = d$se, s_log = d$s_log, eta = d$eta, x = d$x, n = d$n_ef,
    obs = ifelse(is.na(d$val), d$x / d$n_ef, d$val),
    inf95 = ifelse(is.na(d$val), NA_real_, (d$val + d$eta) * exp(-1.96 * d$s_log) - d$eta),
    sup95 = ifelse(is.na(d$val), NA_real_, (d$val + d$eta) * exp(1.96 * d$s_log) - d$eta),
    q = q, l = l)
  datos_h <- datos_h[order(match(datos_h$medida, c("prevalencia", "incidencia", "mortalidad")), datos_h$inicio), ]

  # ---- La prevalencia de los dos ajustes (hombres, nacional) ----
  prev <- function(aj) {
    e <- dl_estimaciones(aj)[location_id == "123" & sex_id == sexo & medida == "prevalencia"]
    data.table::setorder(e, edad)
    list(media = e$media, inferior = e$inferior, superior = e$superior)
  }
  est_ancla <- prev(ajuste_ancla)
  est_datos <- prev(ajuste_datos)
  # q_d de cada estudio de prevalencia en los dos ajustes: el promedio por población de p en su intervalo, simulación
  # por simulación (.dl_q_intervalos con los pesos del dato), y su media posterior. Es lo comparable con x / n.
  q_estudio <- function(aj) {
    dq <- aj$draws_q[location_id == "123" & sex_id == sexo]
    data.table::setorder(dq, draw, edad)
    idx <- which(d$tipo_dato == "prev_estudio")
    m <- vapply(split(dq$p, dq$draw), function(p) ns$.dl_q_intervalos(p, cx$pesos_datos[idx]), numeric(length(idx)))
    m <- matrix(m, nrow = length(idx))
    data.frame(inicio = d$age_start[idx], fin = d$age_end[idx], obs = d$x[idx] / d$n_ef[idx], media = rowMeans(m))
  }
  estudios <- list(sin_datos = q_estudio(ajuste_ancla), con_datos = q_estudio(ajuste_datos))
  edades <- cx$anual
  ancla <- insumos_datos$prior_gbd[measure_id == 5 & sex_id == sexo]
  data.table::setorder(ancla, age_start)
  rhat <- function(aj) max(aj$mcmc$rhat)

  list(
    fuente = paste0("Ejemplo sintético del paquete (causa 9100), hombres de Perú, 2023: dl_ajustar() sin datos y con ",
                    "datos_en_ajuste y ancla.peso 0.5"),
    sexo = "hombres",
    # Filas nacionales de 2023 de la tabla datos (los dos sexos), tal como vienen en datos.csv.
    filas = lapply(seq_len(nrow(nac)), function(k) list(
      sexo = nac$sexo[k], medida = nac$medida[k], inicio = nac$edad_inicio[k], fin = nac$edad_fin[k],
      valor = nac$valor[k], ee = nac$error_estandar[k], casos = nac$casos[k], muestra = nac$muestra[k],
      excluir = isTRUE(nac$excluir[k]), motivo = if (isTRUE(nac$excluir[k])) nac$motivo[k] else NULL,
      fuente = nac$fuente[k])),
    n_nacionales = nrow(nac), n_excluidas = sum(nac$excluir),
    n_entran = nrow(entran), entran_por_medida = as.list(table(unname(nombre_medida[entran$tipo_dato]))),
    n_subnacionales = nrow(sub), anio_subnacional = unique(sub$anio),
    # Los datos de los hombres en el ajuste, con q_d y l_d en la media posterior de theta.
    datos = datos_h,
    L_datos = sum(l), L_por_medida = as.list(stats::setNames(as.numeric(por_tipo), nombre_medida[names(por_tipo)])),
    L_componentes = as.list(comp),
    # Las curvas en esa theta, edad por edad: p, i (1 - p) y p f.
    edades = edades, curva_p = sol$p, curva_ipop = sol$i * (1 - sol$p), curva_pf = sol$p * sol$f,
    # La prevalencia estimada (media e intervalo del 95 %), sin datos y con datos.
    sin_datos = est_ancla, con_datos = est_datos, estudios = estudios,
    ancla = list(inicio = ancla$age_start, fin = ancla$age_end, val = ancla$val, inferior = ancla$lower,
                 superior = ancla$upper),
    lambda = list(sin_datos = 1, con_datos = 0.5),
    mcmc = list(sin_datos = list(cadenas = 4, iteraciones = 4000, calentamiento = 2000, rhat_max = rhat(ajuste_ancla)),
                con_datos = list(cadenas = 4, iteraciones = 8000, calentamiento = 4000, rhat_max = rhat(ajuste_datos)))
  )
}
