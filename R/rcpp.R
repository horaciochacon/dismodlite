# Motor "rcpp": el solver de la EDO y la log-posterior en C++ (inst/cpp/dl_core.cpp), compilados al vuelo con
# Rcpp::sourceCpp() la primera vez que se piden en la sesión. El binario queda en la carpeta de caché del usuario
# (tools::R_user_dir("dismodlite", "cache")) para no recompilar en cada sesión. La versión en R (R/edo.R y
# R/verosimilitud.R) es la referencia; este archivo compila el C++ y le pasa el contexto aplanado.

.dl_rcpp_env <- new.env(parent = emptyenv())

# TRUE si Rcpp está instalado. Función aparte para que las pruebas simulen su ausencia.
.dl_rcpp_disponible <- function() requireNamespace("Rcpp", quietly = TRUE)

# Compila `cpp` y deja sus funciones exportadas en `entorno`. Función aparte para que las pruebas simulen un error
# de compilación.
.dl_rcpp_compilar <- function(cpp, entorno, cache)
  Rcpp::sourceCpp(cpp, env = entorno, cacheDir = cache, verbose = FALSE, showOutput = FALSE)

# Funciones de C++ de la sesión (se compilan la primera vez): list(edo, construir, componentes, total)
#   edo          dl_edo_resolver_cpp(i_media, f_media, remision, p0, nsub): gemela de dl_edo_resolver()
#   construir    dl_ctx_construir_cpp(cx): copia a C++ el contexto de .dl_ctx_cpp() y devuelve un puntero externo
#   componentes  dl_lp_componentes_cpp(theta, puntero): gemela de .dl_lp_componentes()
#   total        dl_lp_total_cpp(theta, puntero): gemela de .dl_log_post()
# Los errores nombran la función que llamó el usuario (dl_ajustar(), dl_cascada(), dl_sensibilidad(), ...).
.dl_rcpp <- function() {
  if (!.dl_rcpp_disponible())
    .dl_stop(paste0("el motor \"rcpp\" requiere el paquete Rcpp (install.packages(\"Rcpp\")) y un compilador de C++ ",
                    "(Rtools en Windows); usa motor = \"mh\" como alternativa (la misma log-posterior en R, ",
                    "m\u00e1s lenta)"))
  if (!is.null(.dl_rcpp_env$fns)) return(.dl_rcpp_env$fns)
  cpp <- .dl_inst_archivo("cpp", "dl_core.cpp")
  cache <- file.path(tools::R_user_dir("dismodlite", "cache"), "rcpp")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  entorno <- new.env(parent = globalenv())
  tryCatch(.dl_rcpp_compilar(cpp, entorno, cache), error = function(e)
    .dl_stop(paste0("no se pudo compilar el motor \"rcpp\" (cpp/dl_core.cpp). Instala un compilador de C++ (Rtools ",
                    "en Windows; en macOS, xcode-select --install; en Linux, g++) o usa motor = \"mh\". Detalle: %s"),
             conditionMessage(e)))
  .dl_rcpp_env$fns <- list(edo = get("dl_edo_resolver_cpp", envir = entorno),
                           construir = get("dl_ctx_construir_cpp", envir = entorno),
                           componentes = get("dl_lp_componentes_cpp", envir = entorno),
                           total = get("dl_lp_total_cpp", envir = entorno))
  .dl_rcpp_env$fns
}

# Log-posterior en C++ sobre `ctx` copiado una sola vez a memoria C++: list(total, componentes), cada una
# function(theta). Se arma antes de repartir las cadenas entre procesos, que heredan el puntero con el fork.
.dl_lp_rcpp <- function(ctx) {
  cpp <- .dl_rcpp()
  puntero <- cpp$construir(.dl_ctx_cpp(ctx))
  list(total = function(theta) cpp$total(theta, puntero),
       componentes = function(theta) cpp$componentes(theta, puntero))
}

# Contexto de .dl_ctx() aplanado para DlCtx (inst/cpp/dl_core.cpp): vectores simples con los mismos nombres que los
# campos de DlCtx, índices en base 0 y los pesos de las bandas concatenados (los de la banda b van de off[b] a
# off[b + 1] - 1). dato_familia: 0 binomial (FAMILIA_BINOMIAL en C++), 1 Poisson. Con el prior de EMR plano,
# emr_sd_log es NA, como en R: emr_plano omite el término y C++ no la lee.
.dl_ctx_cpp <- function(ctx) {
  aplanar <- function(pesos)
    list(idx = as.integer(unlist(lapply(pesos, function(pesos_banda) pesos_banda$idx - 1L))),
         w = as.numeric(unlist(lapply(pesos, `[[`, "w"))),
         off = as.integer(c(0L, cumsum(vapply(pesos, function(pesos_banda) length(pesos_banda$idx), integer(1))))))
  ancla_plana <- aplanar(ctx$pesos_ancla); datos_planos <- aplanar(ctx$pesos_datos)
  list(n_nudos = length(ctx$nudos), W = ctx$W, nsub = as.integer(ctx$nsub),
       idx_anual_malla = as.integer(ctx$idx_anual_malla - 1L), r_media = as.numeric(ctx$r_media),
       sigma_suavidad = as.numeric(ctx$sigma_suavidad),
       cota = as.numeric(ctx$emr$cota), emr_mu_log = as.numeric(ctx$emr$mu_log),
       emr_sd_log = as.numeric(ctx$emr$sd_log), emr_plano = ctx$emr$plano,
       chol_R = ctx$ancla$chol_R, lambda = as.numeric(ctx$ancla$lambda),
       ancla_val = as.numeric(ctx$ancla$bandas$val), ancla_sigma_log = as.numeric(ctx$ancla$bandas$sigma_log),
       ancla_idx = ancla_plana$idx, ancla_w = ancla_plana$w, ancla_off = ancla_plana$off,
       dato_tipo = as.integer(ctx$datos$tipo_cod), dato_ruta = as.integer(ctx$datos$ruta),
       dato_familia = as.integer(.DL_TIPOS_DATO$familia_conteos[ctx$datos$tipo_cod] == "poisson"),
       dato_val = as.numeric(ctx$datos$val), dato_s_log = as.numeric(ctx$datos$s_log),
       dato_x = as.numeric(ctx$datos$x), dato_n = as.numeric(ctx$datos$n_ef), dato_eta = as.numeric(ctx$datos$eta),
       dato_idx = datos_planos$idx, dato_w = datos_planos$w, dato_off = datos_planos$off)
}
