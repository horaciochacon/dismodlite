# Nombres anteriores de las funciones (versión 0.2.2), en el orden de las etapas de una corrida. Cada uno conserva
# sus argumentos y llama a la función nueva, así que devuelve el mismo objeto y comparte la cache de ajustes.
# Valores por defecto que cambian respecto de la versión 0.2.2, los mismos de las funciones nuevas:
# - el registro de corridas: `registry_path` (y `registry_out` en dl_export_cdc) valen NULL y `register` sigue a que
#   se dé el registro; el nombre por defecto del consolidado es "consolidado";
# - las rutas de dl_validate_gbd, dl_como_factor, dl_summarize y dl_export: sin `paths`, las de los insumos
#   (b$rutas), y no dl_paths(). Con insumos armados con las rutas por defecto son las mismas.
# dl_paths(): una clave repetida vale la primera vez que aparece, como en la versión 0.2.2; una clave desconocida o
# una lista sin nombres es un error (antes se guardaba o se ignoraba en silencio).

#' Nombres anteriores de las funciones
#'
#' Desde la versión 1.0.0 las funciones tienen nombres y argumentos en español. Los nombres anteriores siguen
#' funcionando, con sus argumentos originales, para que los scripts existentes no cambien: cada uno llama a la
#' función nueva y devuelve el mismo resultado.
#'
#' | Nombre anterior | Función nueva |
#' |---|---|
#' | `dl_config()` | [dl_configuracion()] |
#' | `dl_paths()` | [dl_rutas()] |
#' | `dl_bundle()` | [dl_insumos()] |
#' | `dl_bundle_freeze()` | [dl_congelar_insumos()] |
#' | `dl_mcmc_opts()` | [dl_opciones_mcmc()] |
#' | `dl_fit()` | [dl_ajustar()] |
#' | `dl_fit_prior_only()` | [dl_ajustar_solo_prior()] |
#' | `dl_cascade()` | [dl_cascada()] |
#' | `dl_validate_gbd()` | [dl_validar_ancla()] |
#' | `dl_validate()` | [dl_validar_tabla()] |
#' | `dl_validate_estimates()` | [dl_validar_estimaciones()] |
#' | `dl_schema()` | [dl_esquema()] |
#' | `dl_schema_tabla()` | [dl_esquema_tabla()] |
#' | `dl_yld()` | [dl_avd()] |
#' | `dl_como_factor()` | [dl_factor_comorbilidad()] |
#' | `dl_emr_prior()` | [dl_prior_emr()] |
#' | `dl_severidad_desde_split()` | [dl_severidad_desde_particion()] |
#' | `dl_labels()` | [dl_etiquetas()] |
#' | `dl_sensitivity()` | [dl_sensibilidad()] |
#' | `dl_summarize()` | [dl_resumir()] |
#' | `dl_export()` | [dl_exportar_corrida()] |
#' | `dl_resumir_run()` | [dl_reresumir_corrida()] |
#' | `dl_sum_hijas()` | [dl_sumar_hijas()] |
#' | `dl_export_cdc()` | [dl_consolidar()] |
#' | `dl_export_seleccionar()` | [dl_consolidado_seleccionar()] |
#' | `dl_export_canonico()` | [dl_consolidado_canonico()] |
#' | `dl_export_conteos()` | [dl_consolidado_conteos()] |
#' | `dl_ode()` | [dl_edo()] |
#' | `dl_ode_solve()` | [dl_edo_resolver()] |
#' | `dl_cache_clear()` | [dl_limpiar_cache()] |
#'
#' Los argumentos de cada nombre anterior corresponden a los de la función nueva así: `b` → `insumos`, `f` →
#' `ajuste`, `f0` → `ajuste_prior`, `cfg` → `configuracion`, `paths` → `rutas`, `opts` → `opciones`, `seed` →
#' `semilla`, `draws` → `simulaciones`, `chains` → `cadenas`, `iter` → `iteraciones`, `warmup` → `calentamiento`,
#' `thin` → `adelgazamiento`, `cores` → `nucleos`, `engine` → `motor`, `workers` → `procesos`, `cause_id` → `causa`,
#' `config_dir` → `carpeta_config`, `overrides` → `cambios`, `out_root` → `carpeta`, `run_slug` → `nombre`,
#' `keep_draws` → `guardar_simulaciones`, `register` → `registrar`, `registry_path` → `registro`, `force` →
#' `forzar`, `dir_run` → `corrida`, `runs_hijas` → `corridas_hijas`, `ui_level` → `nivel`, `grid` → `grilla`,
#' `labels` → `etiquetas`, `perfil_path` → `perfil`, `master_path` → `maestro`, `sch` → `esquema`. Un nombre
#' anterior no acepta los argumentos nuevos ni al revés: `dl_fit(b, semilla = 1)` y `dl_ajustar(b, seed = 1)` dan
#' el error «unused argument». Los mensajes nombran la función que se llamó (`dl_fit(): ...`), citan los argumentos
#' con su nombre nuevo (`semilla`, `rutas`), el de la tabla de arriba, y terminan con una línea que lo recuerda y
#' remite a esta página.
#'
#' Diferencias con la versión 0.2.2:
#' * Una corrida se agrega al registro de corridas solo si se da el registro (`registry_path`, o `registry_out` en
#'   `dl_export_cdc()`), y ese archivo debe existir; antes se registraba siempre que existiera un registro por
#'   defecto. El nombre por defecto de un consolidado es `"consolidado"`.
#' * `dl_validate_gbd()`, `dl_como_factor()`, `dl_summarize()` y `dl_export()` sin `paths` usan las rutas de los
#'   insumos, como sus funciones nuevas (antes, `dl_paths()`). Con insumos armados con `dl_bundle(cfg)`, sin
#'   `paths`, son las mismas rutas.
#' * `dl_export_cdc()` escribe el consolidado en `mod/consolidado/<id>/`, con las tablas del perfil en `tablas/` y
#'   la tabla canónica en `canonico/` (el contenido de las tablas no cambia; la carpeta y la subcarpeta de las
#'   tablas tenían otros nombres). Los perfiles incluidos están en `system.file("perfiles", package =
#'   "dismodlite")`.
#' * `dl_paths()`: una clave desconocida en `overrides` o una lista sin nombres es un error (antes se guardaba o se
#'   ignoraba en silencio); una clave repetida sigue valiendo la primera vez que aparece.
#'
#' @param cause_id,config_dir,overrides Causa, carpeta de configuraciones y cambios (`causa`, `carpeta_config`,
#'   `cambios`). En `dl_paths()`, `overrides` son los cambios de rutas (`cambios` de [dl_rutas()]).
#' @param cfg Configuración (`configuracion`).
#' @param paths Rutas (`rutas`). En `dl_validate_gbd()`, `dl_como_factor()`, `dl_summarize()` y `dl_export()`,
#'   por defecto las de los insumos.
#' @param b Insumos (`insumos`).
#' @param dir Carpeta de destino (`carpeta` de [dl_congelar_insumos()]).
#' @param draws,chains,iter,warmup,thin,cores,engine Opciones MCMC: `simulaciones`, `cadenas`, `iteraciones`,
#'   `calentamiento`, `adelgazamiento`, `nucleos` y `motor`.
#' @param opts Opciones MCMC (`opciones`).
#' @param seed Semilla (`semilla`).
#' @param cache Usar la caché de ajustes (`cache`).
#' @param fit Ajuste (`ajuste` de [dl_ajustar_solo_prior()]).
#' @param f Ajuste o cascada (`ajuste`).
#' @param f0 Ajuste solo con el ancla (`ajuste_prior`).
#' @param kappa Fracción del gradiente subnacional (`kappa`).
#' @param cascade Cascada (`cascada`).
#' @param dt Tabla a validar (`datos`).
#' @param tabla Nombre de la tabla del esquema (`tabla`).
#' @param sch Esquema (`esquema`).
#' @param ctx Contexto de las reglas (`contexto`).
#' @param entity Entidad del contrato (`entidad`).
#' @param path Archivo del esquema (`archivo`).
#' @param nombre Nombre de la tabla del esquema (`nombre`).
#' @param como Factor de comorbilidad (`comorbilidad`).
#' @param dir_run Carpeta de una corrida (`corrida`).
#' @param beta_covariable,padre Canal de proporción y causa padre de la partición de severidad (mismos nombres).
#' @param sequela_ids Secuelas del componente (`secuelas`).
#' @param rho_grid Grilla de rho de las etiquetas (`grilla_rho`).
#' @param grid Grilla de sensibilidad (`grilla`).
#' @param workers Procesos de la sensibilidad (`procesos`).
#' @param piezas Piezas de la corrida (`piezas`).
#' @param ui_level Nivel del intervalo (`nivel`).
#' @param run_slug Nombre corto de la corrida o del consolidado (`nombre`).
#' @param out_root Carpeta raíz de las corridas (`carpeta`).
#' @param labels Etiquetas (`etiquetas`).
#' @param validacion,sensibilidad Validación y sensibilidad de la corrida (mismos nombres).
#' @param keep_draws Guardar las simulaciones (`guardar_simulaciones`).
#' @param register Agregar al registro de corridas (`registrar`).
#' @param force Exportar aunque falle la convergencia (`forzar`).
#' @param ess_min Tamaño efectivo de muestra mínimo (`ess_minimo`).
#' @param registry_path Archivo del registro de corridas (`registro`).
#' @param runs_hijas Corridas de las hijas (`corridas_hijas`).
#' @param cause_name Nombre de la causa padre (`nombre_causa`).
#' @param omitidas Hijas omitidas de la suma (`omitidas`).
#' @param perfil_path Perfil de columnas (`perfil`).
#' @param master_path Archivo `master_gbd.csv` (`maestro`).
#' @param causas,years,permitir_huecos Causas, años y huecos del consolidado (`causas`, `anios`,
#'   `permitir_huecos`).
#' @param registry_out Registro donde se anota el consolidado (`registro_salida`).
#' @param runs_root Carpeta raíz donde están las corridas (`carpeta_corridas`).
#' @param nombres_nivel4_path Nombres de las causas de nivel 4 (`nombres_nivel4`).
#' @param sel Selección de corridas (`seleccion`).
#' @param run_id_export Identificador del consolidado (`id_consolidado`).
#' @param celdas Tabla canónica (`celdas`).
#' @param theta_logi,theta_logf Logaritmo de la incidencia y de la mortalidad en exceso en los nudos
#'   (`log_i_nudos`, `log_f_nudos`).
#' @param nudos,edad_inicio,edad_fin,p0,nsub Nudos, edades, prevalencia inicial y subpasos (mismos nombres).
#' @param r Remisión (`remision`).
#' @param i_half,f_half Incidencia y mortalidad en exceso en la malla de paso h/2 (`i_media`, `f_media`).
#' @return Lo mismo que la función nueva.
#' @seealso [dismodlite] (la lista de las funciones por etapa).
#' @examples
#' # el nombre anterior y el nuevo dan el mismo resultado
#' anterior <- dl_mcmc_opts(draws = 100, chains = 2, iter = 2000, warmup = 1000)
#' nueva <- dl_opciones_mcmc(simulaciones = 100, cadenas = 2, iteraciones = 2000, calentamiento = 1000)
#' identical(anterior, nueva)
#' identical(dl_schema(), dl_esquema())
#' # un nombre anterior no acepta los argumentos nuevos
#' try(dl_mcmc_opts(simulaciones = 100))
#' @name dl_nombres_anteriores
#' @keywords internal
NULL

# Nombre anterior -> función nueva: la tabla de arriba. Los mensajes de un nombre anterior (.dl_condicion(), en
# R/validar.R) terminan con una línea que remite a la función nueva y a esta página. Una prueba comprueba que son los
# nombres documentados y que cada uno llama a su función nueva.
.DL_NOMBRES_ANTERIORES <- c(
  dl_config = "dl_configuracion", dl_paths = "dl_rutas", dl_bundle = "dl_insumos",
  dl_bundle_freeze = "dl_congelar_insumos", dl_mcmc_opts = "dl_opciones_mcmc", dl_fit = "dl_ajustar",
  dl_fit_prior_only = "dl_ajustar_solo_prior", dl_cascade = "dl_cascada", dl_validate_gbd = "dl_validar_ancla",
  dl_validate = "dl_validar_tabla", dl_validate_estimates = "dl_validar_estimaciones", dl_schema = "dl_esquema",
  dl_schema_tabla = "dl_esquema_tabla", dl_yld = "dl_avd", dl_como_factor = "dl_factor_comorbilidad",
  dl_emr_prior = "dl_prior_emr", dl_severidad_desde_split = "dl_severidad_desde_particion",
  dl_labels = "dl_etiquetas", dl_sensitivity = "dl_sensibilidad", dl_summarize = "dl_resumir",
  dl_export = "dl_exportar_corrida", dl_resumir_run = "dl_reresumir_corrida", dl_sum_hijas = "dl_sumar_hijas",
  dl_export_cdc = "dl_consolidar", dl_export_seleccionar = "dl_consolidado_seleccionar",
  dl_export_canonico = "dl_consolidado_canonico", dl_export_conteos = "dl_consolidado_conteos", dl_ode = "dl_edo",
  dl_ode_solve = "dl_edo_resolver", dl_cache_clear = "dl_limpiar_cache")

# ---- Configuración ----

#' @rdname dl_nombres_anteriores
#' @export
dl_config <- function(cause_id, config_dir = NULL, overrides = NULL)
  dl_configuracion(causa = cause_id, carpeta_config = config_dir, cambios = overrides)

#' @rdname dl_nombres_anteriores
#' @export
dl_paths <- function(overrides = list()) {
  if (length(overrides) && !is.null(names(overrides))) overrides <- overrides[!duplicated(names(overrides))]
  dl_rutas(cambios = overrides)
}

# ---- Insumos ----

#' @rdname dl_nombres_anteriores
#' @export
dl_bundle <- function(cfg, paths = dl_paths()) dl_insumos(configuracion = cfg, rutas = paths)

#' @rdname dl_nombres_anteriores
#' @export
dl_bundle_freeze <- function(b, dir) dl_congelar_insumos(insumos = b, carpeta = dir)

# ---- Ajuste ----

#' @rdname dl_nombres_anteriores
#' @export
dl_mcmc_opts <- function(draws = 1000L, chains = 4L, iter = 50000L, warmup = 10000L, thin = 10L,
                         cores = 1L, engine = c("mh", "rcpp"))
  dl_opciones_mcmc(simulaciones = draws, cadenas = chains, iteraciones = iter, calentamiento = warmup,
                   adelgazamiento = thin, nucleos = cores, motor = engine)

#' @rdname dl_nombres_anteriores
#' @export
dl_fit <- function(b, opts = dl_mcmc_opts(), seed, cache = TRUE)
  dl_ajustar(insumos = b, opciones = opts, semilla = seed, cache = cache)

#' @rdname dl_nombres_anteriores
#' @export
dl_fit_prior_only <- function(b, opts = dl_mcmc_opts(), seed, fit = NULL, cache = TRUE)
  dl_ajustar_solo_prior(insumos = b, opciones = opts, semilla = seed, ajuste = fit, cache = cache)

# ---- Departamentos ----

#' @rdname dl_nombres_anteriores
#' @export
dl_cascade <- function(f, b, kappa = b$cfg$cascada$kappa, seed, engine = f$params$engine)
  dl_cascada(ajuste = f, insumos = b, kappa = kappa, semilla = seed, motor = engine)

# ---- Validación ----

#' @rdname dl_nombres_anteriores
#' @export
dl_validate_gbd <- function(f, b, paths = b$rutas, cascade = NULL)
  dl_validar_ancla(ajuste = f, insumos = b, rutas = paths, cascada = cascade)

#' @rdname dl_nombres_anteriores
#' @export
dl_validate <- function(dt, tabla, sch = dl_schema(), ctx = list())
  dl_validar_tabla(datos = dt, tabla = tabla, esquema = sch, contexto = ctx)

#' @rdname dl_nombres_anteriores
#' @export
dl_validate_estimates <- function(dt, paths = dl_paths(), entity = "cause")
  dl_validar_estimaciones(datos = dt, rutas = paths, entidad = entity)

#' @rdname dl_nombres_anteriores
#' @export
dl_schema <- function(path = NULL) dl_esquema(archivo = path)

#' @rdname dl_nombres_anteriores
#' @export
dl_schema_tabla <- function(sch, nombre) dl_esquema_tabla(esquema = sch, nombre = nombre)

# ---- Carga ----

#' @rdname dl_nombres_anteriores
#' @export
dl_yld <- function(f, b, como = NULL, seed) dl_avd(ajuste = f, insumos = b, comorbilidad = como, semilla = seed)

#' @rdname dl_nombres_anteriores
#' @export
dl_como_factor <- function(b, paths = b$rutas) dl_factor_comorbilidad(insumos = b, rutas = paths)

#' @rdname dl_nombres_anteriores
#' @export
dl_emr_prior <- function(b) dl_prior_emr(insumos = b)

#' @rdname dl_nombres_anteriores
#' @export
dl_severidad_desde_split <- function(dir_run, cause_id, paths = dl_paths(), beta_covariable = NULL, padre = NULL,
                                     sequela_ids = NULL)
  dl_severidad_desde_particion(corrida = dir_run, causa = cause_id, rutas = paths, beta_covariable = beta_covariable,
                               padre = padre, secuelas = sequela_ids)

# ---- Diagnóstico ----

#' @rdname dl_nombres_anteriores
#' @export
dl_labels <- function(f, f0, b, rho_grid = as.numeric(unlist(b$cfg$sensibilidad$rho)), seed,
                      opts = NULL, cascade = NULL)
  dl_etiquetas(ajuste = f, ajuste_prior = f0, insumos = b, grilla_rho = rho_grid, semilla = seed, opciones = opts,
               cascada = cascade)

#' @rdname dl_nombres_anteriores
#' @export
dl_sensitivity <- function(b, grid = b$cfg$sensibilidad, seed,
                           opts = dl_mcmc_opts(draws = 200L, chains = 2L, iter = 6000L, warmup = 3000L),
                           workers = 1L)
  dl_sensibilidad(insumos = b, grilla = grid, semilla = seed, opciones = opts, procesos = workers)

# ---- Resumen y corrida ----

#' @rdname dl_nombres_anteriores
#' @export
dl_summarize <- function(piezas, ui_level = 0.95, paths = NULL)
  dl_resumir(piezas = piezas, nivel = ui_level, rutas = paths)

#' @rdname dl_nombres_anteriores
#' @export
dl_export <- function(piezas, run_slug, out_root = Sys.getenv("DATA_ROOT"), labels,
                      validacion = NULL, sensibilidad = NULL, keep_draws = TRUE,
                      register = !is.null(registry_path), force = FALSE, ess_min = 400,
                      registry_path = NULL, paths = NULL)
  dl_exportar_corrida(piezas = piezas, nombre = run_slug, carpeta = out_root, etiquetas = labels,
                      validacion = validacion, sensibilidad = sensibilidad, guardar_simulaciones = keep_draws,
                      registrar = register, forzar = force, ess_minimo = ess_min, registro = registry_path,
                      rutas = paths)

#' @rdname dl_nombres_anteriores
#' @export
dl_resumir_run <- function(dir_run, out_root = Sys.getenv("DATA_ROOT"), ui_level = 0.95,
                           register = !is.null(registry_path), registry_path = NULL, paths = dl_paths())
  dl_reresumir_corrida(corrida = dir_run, carpeta = out_root, nivel = ui_level, registrar = register,
                       registro = registry_path, rutas = paths)

#' @rdname dl_nombres_anteriores
#' @export
dl_sum_hijas <- function(runs_hijas, cause_id, run_slug, out_root = Sys.getenv("DATA_ROOT"),
                         cause_name = NULL, ui_level = 0.95, register = !is.null(registry_path),
                         registry_path = NULL, paths = dl_paths(), omitidas = NULL)
  dl_sumar_hijas(corridas_hijas = runs_hijas, causa = cause_id, nombre = run_slug, carpeta = out_root,
                 nombre_causa = cause_name, nivel = ui_level, registrar = register, registro = registry_path,
                 rutas = paths, omitidas = omitidas)

# ---- Consolidado ----

#' @rdname dl_nombres_anteriores
#' @export
dl_export_cdc <- function(registry_path, out_root, perfil_path, master_path, paths = dl_paths(),
                          causas = NULL, years = NULL, permitir_huecos = FALSE, register = !is.null(registry_out),
                          run_slug = "consolidado", registry_out = NULL, runs_root = out_root,
                          nombres_nivel4_path = file.path(dirname(master_path), "causas_nivel4_es.csv"))
  dl_consolidar(registro = registry_path, carpeta = out_root, perfil = perfil_path, maestro = master_path,
                rutas = paths, causas = causas, anios = years, permitir_huecos = permitir_huecos,
                registrar = register, nombre = run_slug, registro_salida = registry_out,
                carpeta_corridas = runs_root, nombres_nivel4 = nombres_nivel4_path)

#' @rdname dl_nombres_anteriores
#' @export
dl_export_seleccionar <- function(registry_path, runs_root, causas = NULL, years = NULL,
                                  permitir_huecos = FALSE, paths = dl_paths())
  dl_consolidado_seleccionar(registro = registry_path, carpeta_corridas = runs_root, causas = causas, anios = years,
                             permitir_huecos = permitir_huecos, rutas = paths)

#' @rdname dl_nombres_anteriores
#' @export
dl_export_canonico <- function(sel, run_id_export)
  dl_consolidado_canonico(seleccion = sel, id_consolidado = run_id_export)

#' @rdname dl_nombres_anteriores
#' @export
dl_export_conteos <- function(celdas, sel) dl_consolidado_conteos(celdas = celdas, seleccion = sel)

# ---- Avanzado y utilidades ----

#' @rdname dl_nombres_anteriores
#' @export
dl_ode <- function(theta_logi, theta_logf, nudos, edad_inicio, edad_fin = 99,
                   r = 0, p0 = 0, nsub = 5L)
  dl_edo(log_i_nudos = theta_logi, log_f_nudos = theta_logf, nudos = nudos, edad_inicio = edad_inicio,
         edad_fin = edad_fin, remision = r, p0 = p0, nsub = nsub)

#' @rdname dl_nombres_anteriores
#' @export
dl_ode_solve <- function(i_half, f_half, r = 0, p0 = 0, nsub = 5L)
  dl_edo_resolver(i_media = i_half, f_media = f_half, remision = r, p0 = p0, nsub = nsub)

#' @rdname dl_nombres_anteriores
#' @export
dl_cache_clear <- function() dl_limpiar_cache()
