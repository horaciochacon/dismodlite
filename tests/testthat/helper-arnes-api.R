# Arnés de compatibilidad con la versión 0.2.2 (etiqueta git v0.2.2): constantes, API de los escenarios y rutas
# del conjunto acs_peru_completo (el ejemplo en el formato completo).
#
# Los escenarios E1-E12 se escriben una sola vez contra una API con los nombres nuevos de las funciones y de sus
# argumentos (semilla, simulaciones, carpeta, registro, ...). Dos adaptadores construyen esa API:
#   api_legado(entorno, perfiles)  envuelve las funciones con los nombres anteriores (dl_fit, dl_bundle, ...) que
#                                  hay en `entorno`: el código de la etiqueta v0.2.2 cargado con dismodlite_load(), o
#                                  el código actual mientras conserve esos nombres;
#   api_nueva(perfiles)            toma directamente las funciones nuevas del paquete, sin adaptar nada; por
#                                  defecto, con los perfiles de consolidación del paquete (inst/perfiles).
# data-raw/referencia_legado.R corre los escenarios con el código de v0.2.2 y guarda las referencias en
# tests/testthat/_referencia/<escenario>/; test-compatibilidad-legado.R los vuelve a correr con el código actual y
# compara: tolerancia relativa 1e-8 en los números e igualdad exacta en los manifiestos normalizados. Con
# DL_ARNES_EXACTO=1 la tolerancia es 0 (modo exacto: referencias generadas en la misma máquina; es el que respalda
# que un cambio de código deja los números idénticos bit a bit).
#
# El arnés son los cinco archivos helper-arnes-*.R (testthat los carga en orden alfabético; este va primero porque
# los escenarios usan .ARNES al cargarse):
#   api         este archivo: constantes, API y rutas;
#   escenarios  variantes de los datos, piezas comunes y escenarios E1-E12 (correr_escenario());
#   normalizar  lectura y normalización de corridas, consolidados y manifiestos, y el acumulador de resultados;
#   referencia  escritura de las referencias y comparación con ellas (comparar_con_referencia());
#   simple      escenarios S1-S5: el ejemplo en el contrato de insumos por dl_proyecto(), comparado con las
#               referencias de E1, E2, E4, E5 y E8 (test-compatibilidad-simple.R).
# No usan testthat al cargarse (solo la comparación lo usa): data-raw/legado.R los carga para los scripts que
# corren el código de v0.2.2.

# ---- Constantes del arnés ----

.ARNES <- list(
  semilla = 20260929L,
  # cadenas cortas: los escenarios prueban que las cuentas no cambian, no que el MCMC converja (forzar = TRUE)
  mcmc = list(simulaciones = 40L, cadenas = 2L, iteraciones = 2000L, calentamiento = 1000L, adelgazamiento = 10L,
              nucleos = 1L, motor = "mh"),
  # rho de la configuración: las etiquetas de las corridas no reajustan; E11 sí (rejilla 0.5 y 0.9)
  grilla_rho = 0.5,
  nivel = 0.95,
  # celdas y tablas guardadas: nacional y tres departamentos (Amazonas, Lima, Ucayali), para que las referencias
  # pesen poco; las 22 ubicaciones restantes las vigilan las sumas de control por ubicación (tablas *__sumas)
  ubicaciones = c("123", "01", "15", "25"),
  # corrida de la partición de severidad (particion/<corrida>/ de acs_peru_completo)
  particion = "acs_v1",
  procedencia = "datos sint\u00e9ticos de ejemplo",
  # E4: personas-año de la cohorte de incidencia pasada a conteos (ruta de Poisson de la verosimilitud)
  personas_anio_cohorte = 20000,
  # E4: dos estudios de prevalencia pequeños y algo por encima del ancla (casos x de n), que informan poco pero mueven
  # la mediana: dan una celda prior-informed y una data-driven cerca del umbral de contracción 0.3
  estudios_debiles = list(list(sex_id = 2L, edad_inicio = 55L, age_group_id = 16L, x = 12L, n = 400L),
                          list(sex_id = 1L, edad_inicio = 50L, age_group_id = 15L, x = 8L, n = 300L)),
  # E4: desplazamiento de la log-normal de los datos val/se (offset_lognormal)
  offset_lognormal = 1e-4
)

.afirmar <- function(condicion, ...) if (!isTRUE(condicion)) stop("arn\u00e9s: ", ..., call. = FALSE)

# ---- API de los escenarios ----

# Llama a `fun` con `args` (lista con nombres) sin meter los objetos en la llamada: los mensajes de error no
# imprimen un ajuste entero.
.llamar <- function(fun, args) {
  if (!length(args)) return(fun())
  e <- list2env(args, parent = baseenv())
  assign(".fun", fun, envir = e)
  eval(as.call(c(as.name(".fun"), lapply(stats::setNames(nm = names(args)), as.name))), e)
}

# Envuelve una función con nombres anteriores: acepta solo argumentos con nombre, con los nombres nuevos del mapa
# (nuevo = "anterior"), y los traduce. Los argumentos que no se pasan quedan ausentes (rigen los valores por
# defecto y los missing() de la función anterior).
.adaptar <- function(fun, mapa, nombre) {
  force(fun); force(mapa); force(nombre)
  function(...) {
    a <- list(...)
    nm <- names(a)
    if (length(a) && (is.null(nm) || any(!nzchar(nm))))
      stop("api_legado$", nombre, ": el arn\u00e9s pasa todos los argumentos con nombre", call. = FALSE)
    fuera <- setdiff(nm, names(mapa))
    if (length(fuera))
      stop("api_legado$", nombre, ": argumento(s) sin equivalente: ", paste(fuera, collapse = ", "), call. = FALSE)
    names(a) <- unname(mapa[nm])
    .llamar(fun, a)
  }
}

# API con nombres nuevos sobre las funciones anteriores de `entorno` (un entorno de dismodlite_load() o un espacio
# de nombres). `perfiles`: carpeta con perfil_v1.yaml y perfil_v2.yaml de la consolidación (solo la usa consolidar).
api_legado <- function(entorno, perfiles = NULL) {
  f <- function(nombre) get(nombre, envir = entorno, inherits = FALSE)
  list(
    configuracion = .adaptar(f("dl_config"), c(causa = "cause_id", carpeta_config = "config_dir",
                                               cambios = "overrides"), "configuracion"),
    rutas = .adaptar(f("dl_paths"), c(cambios = "overrides"), "rutas"),
    insumos = .adaptar(f("dl_bundle"), c(configuracion = "cfg", rutas = "paths"), "insumos"),
    opciones_mcmc = .adaptar(f("dl_mcmc_opts"), c(simulaciones = "draws", cadenas = "chains", iteraciones = "iter",
                                                  calentamiento = "warmup", adelgazamiento = "thin",
                                                  nucleos = "cores", motor = "engine"), "opciones_mcmc"),
    ajustar = .adaptar(f("dl_fit"), c(insumos = "b", opciones = "opts", semilla = "seed", cache = "cache"),
                       "ajustar"),
    ajustar_solo_prior = .adaptar(f("dl_fit_prior_only"), c(insumos = "b", opciones = "opts", semilla = "seed",
                                                            ajuste = "fit", cache = "cache"), "ajustar_solo_prior"),
    cascada = .adaptar(f("dl_cascade"), c(ajuste = "f", insumos = "b", kappa = "kappa", semilla = "seed",
                                          motor = "engine"), "cascada"),
    validar_ancla = .adaptar(f("dl_validate_gbd"), c(ajuste = "f", insumos = "b", rutas = "paths",
                                                     cascada = "cascade"), "validar_ancla"),
    factor_comorbilidad = .adaptar(f("dl_como_factor"), c(insumos = "b", rutas = "paths"), "factor_comorbilidad"),
    avd = .adaptar(f("dl_yld"), c(ajuste = "f", insumos = "b", comorbilidad = "como", semilla = "seed"), "avd"),
    etiquetas = .adaptar(f("dl_labels"), c(ajuste = "f", ajuste_prior = "f0", insumos = "b", grilla_rho = "rho_grid",
                                           semilla = "seed", opciones = "opts", cascada = "cascade"), "etiquetas"),
    sensibilidad = .adaptar(f("dl_sensitivity"), c(insumos = "b", grilla = "grid", semilla = "seed",
                                                   opciones = "opts", procesos = "workers"), "sensibilidad"),
    resumir = .adaptar(f("dl_summarize"), c(piezas = "piezas", nivel = "ui_level", rutas = "paths"), "resumir"),
    exportar_corrida = .adaptar(f("dl_export"), c(
      piezas = "piezas", nombre = "run_slug", carpeta = "out_root", etiquetas = "labels", validacion = "validacion",
      sensibilidad = "sensibilidad", guardar_simulaciones = "keep_draws", registrar = "register", forzar = "force",
      ess_minimo = "ess_min", registro = "registry_path", rutas = "paths"), "exportar_corrida"),
    reresumir_corrida = .adaptar(f("dl_resumir_run"), c(
      corrida = "dir_run", carpeta = "out_root", nivel = "ui_level", registrar = "register",
      registro = "registry_path", rutas = "paths"), "reresumir_corrida"),
    sumar_hijas = .adaptar(f("dl_sum_hijas"), c(
      corridas_hijas = "runs_hijas", causa = "cause_id", nombre = "run_slug", carpeta = "out_root",
      nombre_causa = "cause_name", nivel = "ui_level", registrar = "register", registro = "registry_path",
      rutas = "paths", omitidas = "omitidas"), "sumar_hijas"),
    consolidar = .adaptar(f("dl_export_cdc"), c(
      registro = "registry_path", carpeta = "out_root", perfil = "perfil_path", maestro = "master_path",
      rutas = "paths", causas = "causas", anios = "years", permitir_huecos = "permitir_huecos",
      registrar = "register", nombre = "run_slug", registro_salida = "registry_out", carpeta_corridas = "runs_root",
      nombres_nivel4 = "nombres_nivel4_path"), "consolidar"),
    limpiar_cache = f("dl_cache_clear"),
    perfil = function(version) file.path(perfiles, sprintf("perfil_%s.yaml", version)))
}

# La misma API con las funciones nuevas del paquete (a partir de la API en español). Los escenarios ya usan los
# nombres nuevos, así que no hay nada que traducir. `perfiles`: carpeta de perfiles de la consolidación (por
# defecto, los del paquete). Solo esta API trae `proyecto` (dl_proyecto(), que la versión 0.2.2 no tiene): la usan los
# escenarios S del contrato de insumos.
api_nueva <- function(perfiles = system.file("perfiles", package = "dismodlite"), ns = asNamespace("dismodlite")) {
  g <- function(nombre) get(nombre, envir = ns, inherits = FALSE)
  # La versión 0.2.2 leía la prevalencia del ancla en Percent; la nueva la lee en Rate (R/esquema.R,
  # .dl_metrica_std). En el ejemplo sintético las dos son la misma proporción, pero Rate / 100 000 no siempre es el
  # mismo número de punto flotante: para comparar con las referencias, la configuración del formato completo lee
  # Percent, como quien reproduce una corrida anterior (anchor.metrica_prevalencia). Los proyectos del contrato lo
  # declaran en su propia configuración (avanzado, .proyecto_0_2_2 en helper-arnes-simple.R): el lector de GBD Results
  # toma la métrica de ahí, así que dl_proyecto() va sin cambios.
  como_0_2_2 <- function(cfg) {
    if (!identical(cfg$origen$formato, "simple"))
      cfg$anchor$metrica_prevalencia <- list(valor = "Percent", procedencia = .ARNES$procedencia)
    cfg
  }
  configuracion <- function(causa, carpeta_config = NULL, cambios = NULL)
    como_0_2_2(g("dl_configuracion")(causa, carpeta_config, cambios))
  list(
    configuracion = configuracion, rutas = g("dl_rutas"), insumos = g("dl_insumos"),
    opciones_mcmc = g("dl_opciones_mcmc"), ajustar = g("dl_ajustar"), ajustar_solo_prior = g("dl_ajustar_solo_prior"),
    cascada = g("dl_cascada"), validar_ancla = g("dl_validar_ancla"), factor_comorbilidad = g("dl_factor_comorbilidad"),
    avd = g("dl_avd"), etiquetas = g("dl_etiquetas"), sensibilidad = g("dl_sensibilidad"), resumir = g("dl_resumir"),
    exportar_corrida = g("dl_exportar_corrida"), reresumir_corrida = g("dl_reresumir_corrida"),
    sumar_hijas = g("dl_sumar_hijas"), consolidar = g("dl_consolidar"), limpiar_cache = g("dl_limpiar_cache"),
    proyecto = g("dl_proyecto"),
    perfil = function(version) file.path(perfiles, sprintf("perfil_%s.yaml", version)))
}

# Carpeta acs_peru_completo del paquete: el ejemplo en el formato completo (con devtools::test(), la del árbol
# fuente).
ruta_acs <- function() system.file("extdata", "acs_peru_completo", package = "dismodlite", mustWork = TRUE)

# Rutas del conjunto acs_peru_completo con las claves de dl_paths() de la versión 0.2.2 (las que el paquete sigue
# usando por dentro), armadas con api$rutas(cambios = ...): valen con api_legado() y con api_nueva(). Las usan los
# escenarios y data-raw/verificar_acs_legado.R; las pruebas del paquete usan dl_rutas_ejemplo(formato =
# "completo"), que da las mismas rutas.
# `causa` es obligatoria: la tabla de severidad es una por causa y el paquete usa todas sus filas sin mirar
# cause_id. `anio` solo se valida (cada archivo trae todos sus años). `sin_severidad` quita la tabla
# (severidad.fuente: mod la deriva de la partición). Las piezas excluidas no se pasan. `cambios_rutas`: claves que
# apuntan a una copia modificada (sección «Variantes de los datos» de helper-arnes-escenarios.R).
rutas_acs <- function(api, raiz, causa, anio = 2023L, con_datos = TRUE, con_proxies = TRUE, sin_severidad = FALSE,
                      cambios_rutas = NULL) {
  if (missing(causa)) stop("rutas_acs: falta causa (9100-9103); la tabla de severidad es una por causa")
  anio <- as.integer(anio)
  causa <- as.integer(causa)
  if (!dir.exists(raiz)) stop("rutas_acs: no existe la carpeta ", raiz)
  if (length(anio) != 1L || !anio %in% c(2019L, 2023L, 2024L)) stop("rutas_acs: anio debe ser 2019, 2023 o 2024")
  if (length(causa) != 1L || !causa %in% 9100:9103) stop("rutas_acs: causa debe ser 9100, 9101, 9102 o 9103")
  r <- function(...) file.path(raiz, ...)
  p <- list(
    std_prior      = r("ancla", "prevalencia.csv"),
    std_csmr       = r("ancla", "mortalidad.csv"),
    std_yld        = r("ancla", "avd.csv"),
    std_incidence  = r("ancla", "incidencia.csv"),
    ghdx_cov       = r("covariables"),
    extraction     = r("extraccion.yaml"),
    poblacion      = r("poblacion.csv"),
    pesos_80mas    = r("pesos_80mas.csv"),
    severidad      = if (isTRUE(sin_severidad)) NULL else r("severidad", sprintf("%d.csv", causa)),
    ghdx_store     = r("evidencia"),
    catalogos      = r("catalogos"),
    datos          = if (isTRUE(con_datos)) r("datos.csv") else NULL,
    cov_proxy      = if (isTRUE(con_proxies)) r("proxies_departamentales.csv") else NULL,
    registry       = r("registro"),
    severity_split = r("particion", .ARNES$particion))
  for (k in names(cambios_rutas)) {
    .afirmar(k %in% names(p), "rutas_acs: clave desconocida en cambios_rutas: ", k)
    p[[k]] <- cambios_rutas[[k]]
  }
  api$rutas(cambios = Filter(Negate(is.null), p))
}
