#!/usr/bin/env Rscript
# Valores de referencia de dismodlite para las pruebas de la versión en JavaScript (lib/modelo.js).
#
# Uso, desde la carpeta animaciones/:
#   Rscript pruebas/fixtures.R          escribe pruebas/fixtures.json
#   node --test pruebas/                compara lib/modelo.js con esos valores
#
# Cada bloque del JSON es la salida de una función del paquete (exportada o interna, con dismodlite:::) sobre:
#   (a) el ejemplo de la viñeta «El modelo» (sección 2: nudos 30..95 y sus log i, log f) y su ejemplo de precisión;
#   (b) el proyecto de ejemplo, dl_proyecto(dl_ejemplo(), causa = 9100): los contextos de la log-posterior de los dos
#       sexos (exportados como JSON, el formato que lee prepararCtx() de modelo.js) en cinco variantes de la
#       configuración, y varios theta (simulaciones de un dl_ajustar() corto, perturbadas, sobre el techo de la EMR,
#       bajo el piso del prior plano y con desborde) con la log-posterior de R por términos;
#   (c) configuraciones de borde de la EDO: nsub 10, remisión constante y por tramos, edad_inicio 0 y 20, edad_fin 90,
#       p0 > 0 y f = 0;
#   (d) casos adversarios, para buscar infidelidades del port: variantes del proyecto con edad_inicio 33 (bandas de
#       población con distinto número de edades del modelo), edad_fin 97, nsub 7 y 3, remisión por tramos que no caen
#       en la malla, rho 0.95 y 0.999, lambda 0.25, offset con un dato de valor 0, binomiales con x = 0 y x = n,
#       n_efectivo, techo derivado del ancla (fracción aguda, factor_sd, factor_techo), prior plano con piso declarado,
#       prior por tabla y EMR cero con datos; theta en el techo y en el piso (y a un ulp), con q = 0 y con RK4 inestable;
#       y pruebas sueltas de chol con rho cerca de 1, cadenas cortas e impares, bandas del proxy con huecos y empates,
#       los bordes de la Beta por momentos y el techo de la cascada justo en log(techo).
# Los números se escriben con 17 cifras significativas (jsonlite digits = I(17)): un double de R vuelve al mismo
# double en JavaScript. digits = NA de jsonlite escribe 15 y no siempre vuelve (el script lo comprueba).
# No-finitos: jsonlite con na = "string" escribe "Inf", "-Inf", "NaN" y "NA" (DL.modelo.reviverR los convierte).

suppressMessages({
  library(dismodlite)
  library(data.table)
  library(jsonlite)
})
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))   # la configuración de ejemplo trae acentos
dl <- asNamespace("dismodlite")

# Carpeta de este script: el JSON se escribe a su lado.
argumentos <- commandArgs(trailingOnly = FALSE)
archivo_script <- sub("^--file=", "", argumentos[grep("^--file=", argumentos)])
carpeta <- if (length(archivo_script)) dirname(normalizePath(archivo_script)) else getwd()
salida <- file.path(carpeta, "fixtures.json")

# Vectores que deben ser arreglos en el JSON aunque tengan largo 1 (con auto_unbox, un largo 1 sería un escalar).
A <- function(x) I(as.vector(x))
`%||%` <- function(a, b) if (is.null(a)) b else a
filas <- function(m) { m <- as.matrix(m); dimnames(m) <- NULL; m }   # matriz -> arreglo de filas
ulp <- function(x) 2^(floor(log2(abs(x))) - 52)                      # distancia al double siguiente (x normal)

fx <- list(generado_con = list(r = R.version.string, dismodlite = as.character(packageVersion("dismodlite")),
                               blas = unname(extSoftVersion()["BLAS"])))

# ==== (a) La viñeta: nudos, W, EDO, RK4, solución exacta ==========================================================

nudos <- c(30, 40, 50, 60, 70, 80, 95)
log_i <- log(c(0.5, 0.8, 1.5, 2.5, 4, 6, 7) / 1000)
log_f <- log(c(10, 12, 15, 20, 30, 50, 90) / 1000)
sol <- dl_edo(log_i, log_f, nudos, edad_inicio = 30)
edades_media <- dl$.dl_edades_media(30, 99, 5L)
W <- dl$.dl_base_interp(nudos, edades_media)
i_media <- exp(as.vector(W %*% log_i))
f_media <- exp(as.vector(W %*% log_f))
p_malla <- dl_edo_resolver(i_media, f_media)
stopifnot(identical(p_malla[seq(1, length(p_malla), by = 5)], sol$p))

# Un paso de RK4 con las expresiones de dl_edo_resolver(), etapa por etapa (pasos de R en base 1).
paso_rk4 <- function(p_malla, i_media, f_media, r, paso, nsub) {
  h <- 1 / nsub; j <- 2L * paso - 1L; p1 <- p_malla[paso]
  k1 <- i_media[j] * (1 - p1) - r * p1 - f_media[j] * p1 * (1 - p1)
  p2 <- p1 + h / 2 * k1
  k2 <- i_media[j + 1L] * (1 - p2) - r * p2 - f_media[j + 1L] * p2 * (1 - p2)
  p3 <- p1 + h / 2 * k2
  k3 <- i_media[j + 1L] * (1 - p3) - r * p3 - f_media[j + 1L] * p3 * (1 - p3)
  p4 <- p1 + h * k3
  k4 <- i_media[j + 2L] * (1 - p4) - r * p4 - f_media[j + 2L] * p4 * (1 - p4)
  p_sig <- p1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  stopifnot(identical(p_sig, p_malla[paso + 1L]))   # el mismo double que dl_edo_resolver()
  list(paso = paso, j = j - 1L, p1 = p1, k1 = k1, p2 = p2, k2 = k2, p3 = p3, k3 = k3, p4 = p4, k4 = k4,
       p_siguiente = p_sig)
}

edades_sonda <- c(0, 25, 29.9, 30, 33.3, 40, 41.7, 59.99, 60, 87.5, 94.9, 95, 99, 120)
fx$vineta <- list(
  nudos = A(nudos), log_i = A(log_i), log_f = A(log_f), edad_inicio = 30, edad_fin = 99, nsub = 5,
  edades_media = A(edades_media), W = filas(W), i_media = A(i_media), f_media = A(f_media), p_malla = A(p_malla),
  edo = list(edades = A(sol$edades), p = A(sol$p), i = A(sol$i), f = A(sol$f)),
  edades_sonda = A(edades_sonda), W_sonda = filas(dl$.dl_base_interp(nudos, edades_sonda)),
  interpolado_sonda = A(as.vector(dl$.dl_base_interp(nudos, edades_sonda) %*% log_i)),
  pasos_rk4 = lapply(c(1L, 2L, 50L, 173L, 345L), function(paso) paso_rk4(p_malla, i_media, f_media, 0, paso, 5)),
  dp_da = list(i = A(c(0.002, 0.01, 0.5)), r = A(c(0, 0.1, 2)), f = A(c(0.05, 0.3, 0)), p = A(c(0.1, 0.5, 0.99)),
               valor = A(dl$.dl_dp_da(c(0.002, 0.01, 0.5), c(0, 0.1, 2), c(0.05, 0.3, 0), c(0.1, 0.5, 0.99)))))

# Precisión (sección 3 de la viñeta): i y f constantes, r = 0, frente a la solución exacta.
i0 <- 0.002; f0 <- 0.05
sol_c <- dl_edo(rep(log(i0), 2), rep(log(f0), 2), nudos = c(30, 95), edad_inicio = 30)
t_c <- sol_c$edades - 30
E <- exp((i0 - f0) * t_c)
fx$precision <- list(i = i0, f = f0, t = A(t_c), exacta = A(i0 * (E - 1) / (i0 * E - f0)), p = A(sol_c$p),
                     error_relativo_max = max(abs(sol_c$p - i0 * (E - 1) / (i0 * E - f0)) /
                                                pmax(i0 * (E - 1) / (i0 * E - f0), 1e-12)))

# ==== (c) EDO en configuraciones de borde ===========================================================================

caso_edo <- function(nombre, edad_inicio, edad_fin = 99, nsub = 5L, remision = 0, remision_cfg = NULL, p0 = 0,
                     f_cero = FALSE) {
  em <- dl$.dl_edades_media(edad_inicio, edad_fin, nsub)
  Wc <- dl$.dl_base_interp(nudos, em)
  if (!is.null(remision_cfg)) remision <- dl$.dl_remision_por_edad(list(remision = remision_cfg), em)
  if (f_cero) {
    # f = 0 exacto: lo que hace la log-posterior con la EMR fija en cero (.dl_log_tasas_media, log f = -Inf)
    im <- exp(as.vector(Wc %*% log_i))
    pm <- dl_edo_resolver(im, rep(0, length(im)), remision = remision, p0 = p0, nsub = nsub)
    an <- dl$.dl_a_edades_enteras(pm, im, rep(0, length(im)), dl$.dl_idx_anual_malla(edad_fin - edad_inicio, nsub),
                                  dl$.dl_idx_anual_media(length(im), nsub))
    s <- c(list(edades = seq(edad_inicio, edad_fin)), an)
  } else {
    s <- dl_edo(log_i, log_f, nudos, edad_inicio = edad_inicio, edad_fin = edad_fin, remision = remision, p0 = p0,
                nsub = nsub)
    pm <- dl_edo_resolver(exp(as.vector(Wc %*% log_i)), exp(as.vector(Wc %*% log_f)), remision = remision, p0 = p0,
                          nsub = nsub)
  }
  list(nombre = nombre, edad_inicio = edad_inicio, edad_fin = edad_fin, nsub = nsub,
       remision = if (!is.null(remision_cfg)) remision_cfg else if (length(remision) == 1L) remision else A(remision),
       r_media = if (!is.null(remision_cfg)) A(remision) else NULL, p0 = p0, f_cero = f_cero,
       n_media = length(em), edades = A(s$edades), p = A(s$p), i = A(s$i), f = A(s$f), p_malla = A(pm))
}
tramos <- list(valor = 0.05, por_edad = list(list(edad_inicio = 60, edad_fin = 80, valor = 0.2)))
fx$edo_bordes <- list(
  caso_edo("nsub10", 30, nsub = 10L),
  caso_edo("remision_0.1", 30, remision = 0.1),
  caso_edo("edad_inicio_0", 0),
  caso_edo("edad_inicio_20_remision_por_tramos", 20, remision_cfg = tramos),
  caso_edo("edad_fin_90_p0", 30, edad_fin = 90, p0 = 0.01),
  caso_edo("f_cero", 30, f_cero = TRUE),
  caso_edo("f_cero_remision_nsub10", 30, nsub = 10L, remision = 0.3, f_cero = TRUE),
  # (d) adversarios: h = 1/3 y 1/7 (h/2 no exacto en binario), p0 alto, remisión con un valor por punto de la malla
  # h/2 y edades no enteras (la malla anual es 30.5, 31.5, ...; la malla h/2 termina en 98.7)
  caso_edo("nsub3_p0_0.5", 30, nsub = 3L, p0 = 0.5),
  caso_edo("nsub7_33_97_remision_vector", 33, edad_fin = 97, nsub = 7L,
           remision = 0.05 + 0.02 * sin(dl$.dl_edades_media(33, 97, 7L))),
  caso_edo("edades_no_enteras", 30.5, edad_fin = 98.7))

# ==== Funciones sueltas de la log-posterior ===========================================================================

set.seed(11)
x_dn <- c(-1.3, 0, 0.7, 2.5, 40, -Inf, Inf, NaN)
mu_dn <- c(0, 0, 0.5, -1, 0, 0, Inf, 0)
s_dn <- c(1, 0.5, 2, 0.1, 1e-3, 1, 1, 1)
U8 <- chol(0.8^abs(outer(1:6, 1:6, "-")))
z6 <- rnorm(6, 0, 0.3); sl6 <- runif(6, 0.1, 0.3)
Sigma6 <- diag(sl6) %*% (0.8^abs(outer(1:6, 1:6, "-"))) %*% diag(sl6)
Sg <- crossprod(matrix(rnorm(16), 4)) + diag(4)
zg <- rnorm(4)
q_d <- c(0.012, 0.05, 0.3); val_d <- c(0.010, 0.06, 0.25); slog_d <- c(0.1, 0.2, 0.15)
fx$funciones <- list(
  dnorm = list(x = A(x_dn), mu = A(mu_dn), sigma = A(s_dn), valor = A(suppressWarnings(dnorm(x_dn, mu_dn, s_dn, log = TRUE)))),
  suavidad = list(log_i = A(log_i), sigma = 0.5, valor = dl$.dl_lp_suavidad(log_i, 0.5),
                  d2 = A(diff(diff(log_i)))),
  prior_emr = list(log_f = A(log_f), mu_log = A(log_f + 0.2), sd_log = A(seq(0.2, 0.5, length.out = 7)),
                   valor = dl$.dl_lp_prior_emr(log_f, list(plano = FALSE, mu_log = log_f + 0.2,
                                                               sd_log = seq(0.2, 0.5, length.out = 7)))),
  mvn_ar1 = list(z = A(z6), sigma_log = A(sl6), rho = 0.8, chol_R = filas(U8), lambda = 0.6,
                 valor = dl$.dl_lp_mvn_ar1(z6, sl6, U8, 0.6), Sigma = filas(Sigma6)),
  mvn = list(z = A(zg), Sigma = filas(Sg),
             valor = as.numeric(-0.5 * (t(zg) %*% solve(Sg) %*% zg + as.numeric(determinant(Sg)$modulus) +
                                          4 * log(2 * pi)))),
  cholesky = lapply(c(0, 0.5, 0.8, 0.95), function(rho)
    list(n = 12L, rho = rho, R = filas(rho^abs(outer(1:12, 1:12, "-"))), U = filas(dl$.dl_chol_ar1(12L, rho)))),
  lognormal = list(q = A(q_d), val = A(val_d), s_log = A(slog_d), eta = A(c(0, 1e-4, 0.01)),
                   valor = A(dl$.dl_lp_lognormal(q_d, val_d, slog_d, c(0, 1e-4, 0.01)))),
  binomial = list(q = A(q_d), x = A(c(10, 47, 300)), n = A(c(800, 1000, 1000)),
                  valor = A(dl$.dl_lp_binomial(q_d, c(10, 47, 300), c(800, 1000, 1000)))),
  poisson = list(q = A(q_d), x = A(c(10, 47, 300)), n = A(c(800, 1000, 1000)),
                 valor = A(dl$.dl_lp_poisson(q_d, c(10, 47, 300), c(800, 1000, 1000)))),
  fuera_de_cota = list(log_f = A(log(c(0.01, 0.2, 0.3))), cota = A(c(0, 0.25)),
                       valor = dl$.dl_f_fuera_de_cota(log(c(0.01, 0.2, 0.3)), c(0, 0.25))),
  sigma_log = list(inferior = A(c(0.008964407, 0.5, 1e-4)), superior = A(c(0.01613938, 2, 3e-4)),
                   valor = A((log(c(0.01613938, 2, 3e-4)) - log(c(0.008964407, 0.5, 1e-4))) / 3.92)))

# ==== Bandas de edad y promedios poblacionales (R/bandas.R) ===========================================================

# Bandas finas de una tabla con agregados (todas las edades, 80+, 50-69) y dos bandas iguales.
lim <- data.table(age_group_id = c(22L, 13L, 14L, 21L, 30L, 31L, 32L, 235L, 25L, 15L, 16L, 17L, 18L, 99L, 98L),
                  age_start = c(0, 40, 45, 80, 80, 85, 90, 95, 50, 50, 55, 60, 65, 100, 100),
                  age_end = c(125, 45, 50, 125, 85, 90, 95, 125, 70, 55, 60, 65, 70, 110, 110))
finas <- dl$.dl_bandas_finas(lim)

# Pesos de intervalo con bandas de distinto número de edades (quinquenios y 80+ abierta): la rama que divide por n_b.
bandas_p <- data.table(age_group_id = c(9:20, 21L), age_start = c(seq(20, 75, 5), 80), age_end = c(seq(25, 80, 5), 125))
pobl_p <- data.table(age_group_id = bandas_p$age_group_id,
                     val = c(1300, 1250, 1236, 1062, 913, 785, 675, 581, 488, 410, 344, 289, 616) * 1000 + 0.5)
edades_p <- 20:99
x_p <- 0.002 * (edades_p - 19)^1.3 / (1 + 0.01 * (edades_p - 19))
intervalos <- list(c(60, 125), c(20, 125), c(40, 45), c(78, 83), c(75, 90), c(95, 125), c(42, 43))
fx$bandas <- list(
  limites = list(age_group_id = A(lim$age_group_id), inicio = A(lim$age_start), fin = A(lim$age_end)),
  finas = list(age_group_id = A(finas$age_group_id), inicio = A(finas$age_start), fin = A(finas$age_end)),
  banda_de = list(edades = A(c(19, 20, 24.9, 25, 79, 80, 99, 124.9, 125, 130)),
                  bandas = list(inicio = A(bandas_p$age_start), fin = A(bandas_p$age_end)),
                  valor = A({ k <- dl$.dl_banda_de(c(19, 20, 24.9, 25, 79, 80, 99, 124.9, 125, 130), bandas_p)
                              ifelse(is.na(k), -1L, k - 1L) })),   # base 0; -1 sin banda (NA en R)
  pesos = list(edades_anual = A(edades_p), bandas = list(inicio = A(bandas_p$age_start), fin = A(bandas_p$age_end)),
               N = A(pobl_p$val), x = A(x_p),
               intervalos = lapply(intervalos, function(iv) {
                 pw <- dl$.dl_pesos_intervalo(edades_p, iv[1], iv[2], pobl_p, bandas_p)
                 list(inicio = iv[1], fin = iv[2], idx = A(pw$idx - 1L), w = A(pw$w),
                      q = dl$.dl_q_intervalos(x_p, list(pw)))
               })))

# Integrandos (prevalencia, incidencia poblacional, mortalidad) de la solución de la viñeta, promediados en las bandas
# del ancla del ejemplo con la población de los hombres.
fx$integrandos <- list(p = A(dl$.dl_integrando(sol, "p")), ipop = A(dl$.dl_integrando(sol, "ipop")),
                       pf = A(dl$.dl_integrando(sol, "pf")))

# ==== (b) El proyecto de ejemplo: contextos y log-posterior ===========================================================

ej <- dl_ejemplo()
cfg0 <- yaml::yaml.load(paste(readLines(file.path(ej, "config", "9100.yaml"), warn = FALSE, encoding = "UTF-8"),
                              collapse = "\n"))
datos0 <- fread(file.path(ej, "datos.csv"), colClasses = list(character = "ubicacion"), encoding = "UTF-8")
# Filas nacionales de prueba que el ejemplo no trae: conteos de incidencia y de mortalidad (Poisson) y una
# prevalencia con valor y error estándar sobre bandas de distinto número de edades.
extra <- data.table(causa = 9100L, ubicacion = "123", anio = 2023L, sexo = c("hombres", "mujeres", "hombres", "mujeres"),
                    edad_inicio = c(50, 50, 75, 40), edad_fin = c(60, 60, 125, 125),
                    medida = c("incidencia", "incidencia", "mortalidad", "prevalencia"),
                    valor = c(NA, NA, NA, 0.05), error_estandar = c(NA, NA, NA, 0.01), casos = c(40, 25, 300, NA),
                    muestra = c(10000, 9000, 120000, NA), excluir = FALSE, motivo = "",
                    fuente = "fila de prueba de las animaciones", definicion = "prueba")
datos1 <- rbind(datos0, extra, fill = TRUE)

insumos_de <- function(cfg, datos = NULL) {
  args <- list(ej, causa = 9100)
  if (!is.null(cfg)) args$configuracion <- cfg   # NULL: la configuración del proyecto tal cual
  if (!is.null(datos)) args$datos <- as.data.frame(datos)
  suppressWarnings(suppressMessages(dl_insumos(do.call(dl_proyecto, args))))
}

cfg_datos <- cfg0
cfg_datos$datos_en_ajuste <- c("prevalencia", "incidencia", "mortalidad")
cfg_datos$ancla <- list(peso = 0.5, correlacion_edad = 0.8)
cfg_datos$avanzado <- list(offset_lognormal = 1e-4)
cfg_plano <- cfg0; cfg_plano$mortalidad_exceso <- list(prior = "plano", techo = 0.25)
cfg_cero <- cfg0; cfg_cero$mortalidad_exceso <- list(prior = "cero")
cfg_nsub <- cfg0
cfg_nsub$edad_inicio <- 35; cfg_nsub$remision <- 0.05; cfg_nsub$nudos <- c(35, 50, 65, 80, 95)
cfg_nsub$datos_en_ajuste <- c("prevalencia", "incidencia", "mortalidad")
cfg_nsub$ancla <- list(correlacion_edad = 0)
cfg_nsub$avanzado <- list(nsub = 10L, edad_fin = 100L,
                          remision = list(por_edad = list(list(edad_inicio = 60, edad_fin = 80, valor = 0.2,
                                                               fuente = "prueba de las animaciones"))))

# El factor de comorbilidad lee el ancla de AVD de la carpeta temporal del proyecto traducido, que las variantes
# siguientes reescriben: se calcula antes de armarlas.
b_ejemplo <- insumos_de(NULL)
como <- dl_factor_comorbilidad(b_ejemplo)
variantes <- list(
  ejemplo = list(b = b_ejemplo, nota = "el proyecto de ejemplo tal cual: sin datos en el ajuste"),
  datos = list(b = insumos_de(cfg_datos, datos1),
               nota = "datos nacionales en el ajuste (log-normal, binomial y Poisson), offset 1e-4, peso 0.5, rho 0.8"),
  plano = list(b = insumos_de(cfg_plano), nota = "prior plano de la EMR bajo el techo 0.25"),
  cero = list(b = insumos_de(cfg_cero), nota = "EMR fija en cero: theta = log i"),
  nsub10 = list(b = insumos_de(cfg_nsub, datos1),
                nota = paste("edad_inicio 35, edad_fin 100 (95+ con 6 edades), nsub 10, remisión 0.05 y 0.2 en [60, 80),",
                             "rho 0, datos en el ajuste")))

# ---- (d) Variantes adversarias ----
# Filas nacionales de borde: un valor 0 con offset (log-normal), binomiales con x = 0 y x = n, n_efectivo distinto de n,
# Poisson con x = 0 sobre una banda abierta, y datos que cruzan bandas de población con distinto número de edades del
# modelo: con edad_inicio 33 la banda [30, 35) tiene 2 edades (33, 34) y, con edad_fin 97, la de 95+ tiene 3.
extra_adv <- data.table(
  causa = 9100L, ubicacion = "123", anio = 2023L,
  sexo = c("hombres", "mujeres", "hombres", "mujeres", "hombres", "mujeres", "hombres", "mujeres", "hombres", "mujeres"),
  edad_inicio = c(33, 30, 50, 41, 60, 75, 33, 41, 75, 30),
  edad_fin =    c(45, 45, 60, 43, 125, 125, 35, 42, 125, 125),
  medida = c("prevalencia", "prevalencia", "prevalencia", "prevalencia", "incidencia", "mortalidad", "incidencia",
             "prevalencia", "prevalencia", "incidencia"),
  valor = c(0, 0, NA, NA, NA, 0.004, 0.0004, NA, 0.2, NA),
  error_estandar = c(0.002, 0.001, NA, NA, NA, 0.001, 0.0002, NA, 0.05, NA),
  casos = c(NA, NA, 0, 400, 0, NA, NA, 30, NA, 0), muestra = c(NA, NA, 500, 400, 10000, NA, NA, 900, NA, 1e5),
  muestra_efectiva = c(NA, NA, NA, NA, NA, NA, NA, 450, NA, NA),
  excluir = FALSE, motivo = "", fuente = "fila adversaria de las animaciones", definicion = "prueba")
datos_adv <- rbind(datos0, extra_adv, fill = TRUE)
cfg_adv <- cfg0
cfg_adv$edad_inicio <- 33
cfg_adv$nudos <- c(33, 37.5, 50, 65, 80, 90, 97)          # 33 y 37.5 fuera de las bandas del ancla (desde 40)
cfg_adv$remision <- 0.02
cfg_adv$datos_en_ajuste <- c("prevalencia", "incidencia", "mortalidad")
cfg_adv$ancla <- list(peso = 0.25, correlacion_edad = 0.95)
cfg_adv$avanzado <- list(nsub = 7L, edad_fin = 97L, offset_lognormal = 1e-3,
  remision = list(por_edad = list(list(edad_inicio = 47.3, edad_fin = 61.7, valor = 0.3, fuente = "prueba"),
                                  list(edad_inicio = 90, edad_fin = 200, valor = 0.1, fuente = "prueba"))))
cfg_rho <- cfg0                                            # techo derivado: sin mortalidad_exceso.techo
cfg_rho$ancla <- list(peso = 1, correlacion_edad = 0.999)
cfg_rho$mortalidad_exceso <- list(fraccion_aguda = 0.1)
cfg_rho$incidencia <- list(suavidad = 2)
cfg_rho$nudos <- c(30, 45, 60, 75, 90, 99)
cfg_rho$avanzado <- list(emr_prior = list(factor_sd = list(valor = 1.5, procedencia = "prueba"), factor_techo = 2))
cfg_piso <- cfg0
cfg_piso$mortalidad_exceso <- list(prior = "plano")
cfg_piso$avanzado <- list(emr_prior = list(cota = c(0.002, 0.4), fuente_cota = "prueba"))   # piso 0.002 > 0.4 / 1000
cfg_tabla <- cfg0
cfg_tabla$mortalidad_exceso <- list(prior = "tabla", techo = 0.3)
tabla_emr_dada <- data.frame(sexo = rep(c("hombres", "mujeres"), each = 3), edad_inicio = rep(c(30, 52, 80), 2),
                             edad_fin = rep(c(52, 80, 125), 2), mu_log = c(-4.2, -3.1, -2.2, -4.5, -3.3, -2.4),
                             sd_log = c(0.3, 0.25, 0.4, 0.35, 0.2, 0.5))   # el nudo 80 cae justo en un borde
cfg_cero_datos <- cfg0
cfg_cero_datos$mortalidad_exceso <- list(prior = "cero")
cfg_cero_datos$datos_en_ajuste <- c("prevalencia", "incidencia")
cfg_cero_datos$avanzado <- list(nsub = 3L)
insumos_tabla <- function() suppressWarnings(suppressMessages(dl_insumos(dl_proyecto(
  ej, causa = 9100, configuracion = cfg_tabla, prior_mortalidad_exceso = tabla_emr_dada))))
variantes <- c(variantes, list(
  adversario = list(b = insumos_de(cfg_adv, datos_adv),
                    nota = paste("edad_inicio 33, edad_fin 97, nsub 7, nudos fuera de las bandas del ancla, remisión",
                                 "por tramos [47.3, 61.7) y [90, 200), rho 0.95, lambda 0.25, offset 1e-3, datos con",
                                 "valor 0, x = 0, x = n, n_efectivo y bandas de población con 2, 3 y 5 edades")),
  rho_alto = list(b = insumos_de(cfg_rho),
                  nota = paste("rho 0.999, techo derivado del ancla (factor_techo 2), fracción aguda 0.1, factor_sd",
                               "1.5, suavidad 2, nudo en 99")),
  plano_piso = list(b = insumos_de(cfg_piso), nota = "prior plano con cota [0.002, 0.4]: el piso declarado manda"),
  tabla = list(b = insumos_tabla(), nota = "prior de la EMR dado por tabla, techo 0.3, un nudo en el borde 80"),
  cero_datos = list(b = insumos_de(cfg_cero_datos, datos0[medida != "mortalidad"]),
                    nota = "EMR fija en cero, nsub 3, datos de prevalencia e incidencia en el ajuste")))

# Contexto de un sexo (.dl_ctx) como JSON: lo que lee prepararCtx() de modelo.js.
ctx_json <- function(ctx, b, sexo) {
  pobl <- b$poblacion[location_id == b$loc_ancla & sex_id == sexo]
  bp <- b$bandas_pobl
  N <- pobl$val[match(bp$age_group_id, pobl$age_group_id)]
  d <- ctx$datos
  cfg_rem <- b$cfg$remision
  list(
    esquema = "dismodlite/ctx-v1", sexo = sexo, nudos = A(ctx$nudos), edad_inicio = ctx$edad_inicio,
    edad_fin = ctx$edad_fin, nsub = ctx$nsub, sigma_suavidad = ctx$sigma_suavidad,
    remision = list(valor = as.numeric(cfg_rem$valor %||% 0),
                    por_edad = lapply(cfg_rem$por_edad %||% list(), function(t)
                      list(edad_inicio = t$edad_inicio, edad_fin = t$edad_fin, valor = t$valor))),
    emr = list(cero = isTRUE(ctx$emr$cero), plano = isTRUE(ctx$emr$plano), mu_log = A(ctx$emr$mu_log),
               sd_log = A(ctx$emr$sd_log), cota = A(ctx$emr$cota)),
    ancla = list(age_group_id = A(ctx$ancla$bandas$age_group_id), inicio = A(ctx$ancla$bandas$age_start),
                 fin = A(ctx$ancla$bandas$age_end), val = A(ctx$ancla$bandas$val),
                 sigma_log = A(ctx$ancla$bandas$sigma_log), lambda = ctx$ancla$lambda, rho = b$cfg$anchor$rho_edad),
    poblacion = list(age_group_id = A(bp$age_group_id), inicio = A(bp$age_start), fin = A(bp$age_end), N = A(N)),
    datos = list(tipo = A(as.character(d$tipo_dato)), inicio = A(d$age_start), fin = A(d$age_end),
                 ruta = A(as.integer(d$ruta)), val = A(d$val), s_log = A(d$s_log), eta = A(d$eta),
                 x = A(as.numeric(d$x)), n = A(d$n_ef)))
}
pesos_json <- function(pesos) lapply(pesos, function(p) list(idx = A(p$idx - 1L), w = A(p$w)))

# De dónde sale ctx$emr (R/emr.R), para rehacerlo en JavaScript desde el ancla: el tipo de prior, las bandas de
# prevalencia y csmr del ancla (los dos sexos: el techo derivado toma el máximo de csmr / p sobre ambos), la fracción
# aguda, factor_sd, factor_techo, el origen del techo, la cota de la configuración y, con el prior por tabla, la tabla.
emr_fuente <- function(b) {
  cfg <- b$cfg
  pg <- b$prior_gbd
  banda <- function(m) list(sex_id = A(m$sex_id), inicio = A(m$age_start), fin = A(m$age_end), val = A(m$val),
                            sigma_log = A(m$sigma_log))
  dado <- b$prior_emr_dado
  list(tipo = cfg$emr_prior$tipo, fraccion_aguda = dl$.dl_fraccion_aguda(cfg), factor_sd = dl$.dl_factor_sd_emr(cfg),
       factor_techo = as.numeric(cfg$emr_prior$factor_techo %||% 3), techo_origen = b$techo_emr$origen,
       cota_cfg = A(as.numeric(unlist(cfg$emr_prior$cota))),
       prev = banda(pg[measure_id == 5L]), csmr = banda(pg[measure_id == 900006L]),
       dado = if (!is.null(dado)) list(sex_id = A(dado$sex_id), inicio = A(dado$age_start), fin = A(dado$age_end),
                                       mu_log = A(dado$mu_log), sd_log = A(dado$sd_log)))
}

# Ajuste corto del ejemplo (las simulaciones son theta posteriores reales) y su cascada (la renormalización).
op <- dl_opciones_mcmc(simulaciones = 50, cadenas = 2, iteraciones = 1000, calentamiento = 400)
ajuste <- suppressWarnings(suppressMessages(dl_ajustar(variantes$ejemplo$b, op, semilla = 1)))

set.seed(2024)
thetas_de <- function(nombre, ctx, sexo) {
  K <- length(ctx$nudos)
  cero <- dl$.dl_ctx_emr_cero(ctx)
  t0 <- unname(dl$.dl_theta_inicial(ctx))
  out <- list(list(nombre = "inicial", theta = A(t0)))
  if (nombre %in% c("ejemplo", "datos", "plano", "cero")) {
    dp <- unname(ajuste$draws_par[[as.character(sexo)]])
    for (k in c(1L, 17L, 50L)) {
      th <- dp[k, ]
      if (cero) th <- th[seq_len(K)]
      out[[length(out) + 1L]] <- list(nombre = sprintf("posterior_%d", k), theta = A(th))
      out[[length(out) + 1L]] <- list(nombre = sprintf("perturbada_%d", k), theta = A(th + rnorm(length(th), 0, 0.25)))
    }
  } else {
    for (k in 1:4) out[[length(out) + 1L]] <- list(nombre = sprintf("perturbada_%d", k),
                                                   theta = A(t0 + c(seq(-0.5, 1.5, length.out = K) + rnorm(K, 0, 0.3),
                                                                    rnorm(K, 0, 0.3))))
  }
  base <- as.numeric(out[[length(out)]]$theta)
  if (!cero) {
    techo <- base; techo[K + 3L] <- log(ctx$emr$cota[2] * 1.2)
    out[[length(out) + 1L]] <- list(nombre = "sobre_el_techo", theta = A(techo))
    justo <- base; justo[K + 3L] <- log(ctx$emr$cota[2]) - 1e-9
    out[[length(out) + 1L]] <- list(nombre = "bajo_el_techo", theta = A(justo))
    if (isTRUE(ctx$emr$plano)) {
      piso <- base; piso[K + 1L] <- log(ctx$emr$cota[1] / 2)
      out[[length(out) + 1L]] <- list(nombre = "bajo_el_piso", theta = A(piso))
    }
    # (d) log f de un nudo en log(techo) y a un ulp de él: exp(log f) puede quedar justo en el techo o a un ulp, y la
    # cota es cerrada (fuera solo si f > techo o f < piso). Lo mismo en el piso, si es mayor que 0.
    l2 <- log(ctx$emr$cota[2])
    for (d in -1:1) {
      th <- base; th[K + 2L] <- l2 + d * ulp(l2)
      out[[length(out) + 1L]] <- list(nombre = sprintf("techo_%+d_ulp", d), theta = A(th))
    }
    if (ctx$emr$cota[1] > 0) {
      l1 <- log(ctx$emr$cota[1])
      for (d in -1:1) {
        th <- base; th[K + 1L] <- l1 + d * ulp(l1)
        out[[length(out) + 1L]] <- list(nombre = sprintf("piso_%+d_ulp", d), theta = A(th))
      }
    }
  }
  desborde <- base; desborde[seq_len(K)] <- 50
  out[[length(out) + 1L]] <- list(nombre = "desborde", theta = A(desborde))
  rugosa <- base; rugosa[seq_len(K)] <- rugosa[seq_len(K)] + c(0.8, -0.8)[(seq_len(K) %% 2) + 1L]
  out[[length(out) + 1L]] <- list(nombre = "rugosa", theta = A(rugosa))
  # (d) i = exp(-800) = 0: p = 0, q del ancla 0 (ancla, datos y total -Inf; suavidad y EMR finitas). Y RK4 inestable:
  # i h = 3 supera el límite 2.785 de RK4 y p oscila con amplitud creciente.
  q_cero <- base; q_cero[seq_len(K)] <- -800
  out[[length(out) + 1L]] <- list(nombre = "q_cero", theta = A(q_cero))
  inestable <- base; inestable[seq_len(K)] <- log(3 * ctx$nsub)
  out[[length(out) + 1L]] <- list(nombre = "rk4_inestable", theta = A(inestable))
  lapply(out, function(o) {
    lp <- dl$.dl_lp_componentes(as.numeric(o$theta), ctx)
    c(o, list(lp = as.list(lp)))
  })
}

fx$contextos <- lapply(names(variantes), function(nombre) {
  b <- variantes[[nombre]]$b
  sexos <- lapply(as.integer(unlist(b$cfg$sexos)), function(sexo) {
    ctx <- dl$.dl_ctx(b, sexo)
    th <- thetas_de(nombre, ctx, sexo)
    sol1 <- dl$.dl_edo_ctx(as.numeric(th[[2L]]$theta), ctx)
    filas_W <- unique(c(seq(1L, nrow(ctx$W), by = 13L), nrow(ctx$W)))
    list(sexo = sexo, ctx = ctx_json(ctx, b, sexo),
         r = list(  # lo que R calcula en .dl_ctx(), para comparar con prepararCtx()
           n_media = length(ctx$edades_media), edades_media_extremos = A(range(ctx$edades_media)),
           filas_W = A(filas_W - 1L), W_filas = filas(ctx$W[filas_W, , drop = FALSE]),
           edades_media_filas = A(ctx$edades_media[filas_W]),
           r_media = A(ctx$r_media), idx_anual_malla = A(ctx$idx_anual_malla - 1L),
           idx_anual_media = A(ctx$idx_anual_media - 1L), anual = A(ctx$anual), chol_R = filas(ctx$ancla$chol_R),
           pesos_ancla = pesos_json(ctx$pesos_ancla), pesos_datos = pesos_json(ctx$pesos_datos),
           theta_inicial = A(unname(dl$.dl_theta_inicial(ctx))),
           edo_theta2 = list(p = A(sol1$p), i = A(sol1$i), f = A(sol1$f))),
         thetas = th)
  })
  list(nombre = nombre, nota = variantes[[nombre]]$nota, sexos = sexos, emr_fuente = emr_fuente(b))
})
names(fx$contextos) <- names(variantes)

# Las simulaciones del ajuste: p anual de la simulación 1 (dl_ajustar resuelve la EDO con cada theta guardado).
fx$ajuste <- lapply(c("1", "2"), function(sx) {
  dq <- ajuste$draws_q[sex_id == as.integer(sx) & draw == 1L]
  list(sexo = as.integer(sx), theta = A(unname(ajuste$draws_par[[sx]][1, ])), edad = A(dq$edad), p = A(dq$p),
       i = A(dq$i), f = A(dq$f), ipop = A(dq$ipop))
})

# El techo de la EMR en la cascada (.dl_edo_log_media con techo_f): log f desplazado +3 supera log(0.25) en las edades
# altas y se recorta (truncado = 1); con +0, no.
fx$techo_cascada <- lapply(c(0, 3), function(desplazamiento) {
  ctx <- dl$.dl_ctx(variantes$ejemplo$b, 1L)
  theta <- unname(ajuste$draws_par[["1"]][1, ])
  lm <- dl$.dl_log_tasas_media(theta, ctx)
  s <- dl$.dl_edo_log_media(lm$log_i, lm$log_f + desplazamiento, ctx, techo_f = 0.25)
  list(theta = A(theta), desplazamiento = desplazamiento, techo_f = 0.25, log_i_media = A(lm$log_i),
       log_f_media = A(lm$log_f), truncado = s$truncado, p = A(s$p), i = A(s$i), f = A(s$f))
})

# ==== Prior de la EMR (R/emr.R) ======================================================================================

b <- variantes$ejemplo$b
pg <- b$prior_gbd
prev <- pg[measure_id == 5L]; csmr <- pg[measure_id == 900006L]
cfg_fa <- b$cfg
cfg_fa$emr_prior$fraccion_aguda <- list(valor = 0.1)
cfg_fa$emr_prior$factor_sd <- list(valor = 1.5)
tabla_emr <- dl_prior_emr(b)
tabla_fa <- dl$.dl_prior_emr_tabla(pg, cfg_fa)
cfg_sin_cota <- b$cfg; cfg_sin_cota$emr_prior$cota <- NULL
techo <- dl$.dl_techo_emr(cfg_sin_cota, pg)
cfg_sin_cota$emr_prior$factor_techo <- 2
techo2 <- dl$.dl_techo_emr(cfg_sin_cota, pg)
nudos_fuera <- c(30, 40, 44.9, 45, 80, 95, 99, 130)
fx$emr <- list(
  ancla = list(sex_id = A(prev$sex_id), inicio = A(prev$age_start), fin = A(prev$age_end), prev = A(prev$val),
               prev_lower = A(prev$lower), prev_upper = A(prev$upper), sigma_log_prev = A(prev$sigma_log),
               csmr = A(csmr$val[match(paste(prev$sex_id, prev$age_group_id), paste(csmr$sex_id, csmr$age_group_id))]),
               sigma_log_csmr = A(csmr$sigma_log[match(paste(prev$sex_id, prev$age_group_id),
                                                       paste(csmr$sex_id, csmr$age_group_id))])),
  tabla = list(sex_id = A(tabla_emr$sex_id), inicio = A(tabla_emr$age_start), fin = A(tabla_emr$age_end),
               mu_log = A(tabla_emr$mu_log), sd_log = A(tabla_emr$sd_log)),
  tabla_fraccion_aguda = list(fraccion_aguda = 0.1, factor_sd = 1.5, mu_log = A(tabla_fa$mu_log),
                              sd_log = A(tabla_fa$sd_log)),
  techo = list(k = 3, cota = A(techo$cota), emr_max = techo$emr_max_ancla),
  techo_k2 = list(k = 2, cota = A(techo2$cota)),
  en_nudos = lapply(1:2, function(sx) {
    e <- dl$.dl_emr_en_nudos(tabla_emr[sex_id == sx], nudos_fuera, c(0, 0.25))
    list(sexo = sx, nudos = A(nudos_fuera), mu_log = A(e$mu_log), sd_log = A(e$sd_log))
  }),
  plano = { e <- dl$.dl_emr_plano(nudos, c(0, 0.25)); list(nudos = A(nudos), cota_entrada = A(c(0, 0.25)),
                                                          mu_log = A(e$mu_log), cota = A(e$cota)) },
  plano_piso = { e <- dl$.dl_emr_plano(nudos, c(0.01, 0.25)); list(cota_entrada = A(c(0.01, 0.25)), cota = A(e$cota)) },
  dado = {
    tab <- data.table(sex_id = 1L, age_start = c(30, 50, 70), age_end = c(50, 70, 125), mu_log = c(-4, -3.5, -2.5),
                      sd_log = c(0.3, 0.25, 0.4))
    e <- dl$.dl_emr_dado_en_nudos(tab, nudos, c(0, 0.25))
    list(inicio = A(tab$age_start), fin = A(tab$age_end), mu_log_tabla = A(tab$mu_log), sd_log_tabla = A(tab$sd_log),
         nudos = A(nudos), mu_log = A(e$mu_log), sd_log = A(e$sd_log))
  })

# ==== Muestreo y diagnósticos (R/muestreador.R) =======================================================================

set.seed(7)
ar1 <- function(n, phi, sd = 1) { x <- numeric(n); x[1] <- rnorm(1, 0, sd / sqrt(1 - phi^2))
  for (t in 2:n) x[t] <- phi * x[t - 1] + rnorm(1, 0, sd); x }
cadenas_grandes <- lapply(1:4, function(c) {
  m <- cbind(a = ar1(301, 0.9), b = rnorm(301), c = ar1(301, 0.5) + 0.3 * c, d = ar1(301, -0.6))
  m
})
cadenas_chicas <- lapply(1:3, function(c) cbind(x = ar1(9, 0.3), y = rnorm(9) + c))
cadenas_mh <- lapply(1:2, function(c) cbind(u = cumsum(rnorm(40, 0, 0.1)) + c / 10))
fx$diagnosticos <- list(
  grandes = list(cadenas = lapply(cadenas_grandes, filas), rhat = A(unname(dl_rhat(cadenas_grandes))),
                 ess = A(unname(dl_ess(cadenas_grandes))), ess_lag10 = A(unname(dl_ess(cadenas_grandes, max_lag = 10L))),
                 acf_a1 = A(stats::acf(cadenas_grandes[[1]][, "a"], lag.max = 30, plot = FALSE)$acf[-1, 1, 1])),
  chicas = list(cadenas = lapply(cadenas_chicas, filas), rhat = A(unname(dl_rhat(cadenas_chicas))),
                ess = A(unname(dl_ess(cadenas_chicas)))),
  una_columna = list(cadenas = lapply(cadenas_mh, filas), rhat = A(unname(dl_rhat(cadenas_mh))),
                     ess = A(unname(dl_ess(cadenas_mh, max_lag = 5L)))),
  geyer = list(autocor = A(c(0.5, 0.3, 0.2, 0.05, -0.1, -0.2, 0.3)),
               valor = dl$.dl_suma_geyer(c(0.5, 0.3, 0.2, 0.05, -0.1, -0.2, 0.3))))
theta_cal <- cbind(cumsum(rnorm(200, 0, 0.1)), rnorm(200, 2, 0.5), cumsum(rnorm(200, 0, 0.05)))
theta_cal[, 3] <- theta_cal[, 3] + 0.5 * theta_cal[, 1]
fx$mh <- list(
  escala_inicial = list(d = A(c(1, 5, 7)), valor = A(dl$.dl_mh_escala_inicial(list(1L, 1:5, 1:7)))),
  escala_adaptada = list(escala = 1.2, aceptacion = 0.35, k = 3, valor = dl$.dl_mh_escala_adaptada(1.2, 0.35, 3)),
  forma = list(theta_b = filas(theta_cal), L = filas(dl$.dl_mh_forma_propuesta(theta_cal))),
  semillas = list(semilla = A(c(1, 2, 20231231)), cadena = A(c(1, 3, 2)), desplazamiento = A(c(0, 500, 500)),
                  valor = A(mapply(dl$.dl_semilla_derivada, c(1, 2, 20231231), c(1, 3, 2), c(0, 500, 500)))))

# ==== Estimación subnacional (R/cascada.R, R/betas.R) =================================================================

cas <- suppressWarnings(suppressMessages(dl_cascada(ajuste, b, semilla = 1)))
sin_renorm <- cas$draws_q_sin_renorm
pob <- b$poblacion[, list(location_id, sex_id, banda = age_group_id, N = val)]
caso_renorm <- function(sexo, banda, inicio, fin, draw_k, columna, medida) {
  dq <- sin_renorm[sex_id == sexo & draw == draw_k & edad >= inicio & edad < fin]
  # mismo orden de ubicaciones que .dl_renormalizar (merge por location_id)
  locs <- sort(unique(dq$location_id), method = "radix")
  dptos <- setdiff(locs, b$loc_ancla)
  edades_de <- function(l) dq[location_id == l][order(edad)][[columna]]
  N_de <- function(l) pob[location_id == l & sex_id == sexo & banda == banda_id, N]
  banda_id <- banda
  ren <- cas$draws_q[sex_id == sexo & draw == draw_k & edad >= inicio & edad < fin]
  list(sexo = sexo, banda = banda, inicio = inicio, fin = fin, draw = draw_k, columna = columna,
       q_nac_edades = A(edades_de(b$loc_ancla)), N_nac = N_de(b$loc_ancla),
       dptos = A(dptos), q_d_edades = lapply(dptos, function(l) A(edades_de(l))), N_d = A(vapply(dptos, N_de, 0)),
       factor = cas$renorm[medida == (if (columna == "p") "prevalence" else "incidence") & sex_id == sexo &
                             age_group_id == banda & draw == draw_k, factor],
       q_d_renorm = lapply(dptos, function(l) A(ren[location_id == l][order(edad)][[columna]])))
}
proxy_bandas <- b$bandas_catalogo[age_group_id %in% c(13L, 15L, 17L, 19L, 30L)][order(age_start)]
proxy_contiguas <- b$bandas_catalogo[age_group_id %in% c(24L, 25L, 26L)][order(age_start)]
edades_P <- seq(0, 100, by = 0.5)
P_caso <- function(bandas, fuera, interp)
  list(bandas = list(age_group_id = A(bandas$age_group_id), inicio = A(bandas$age_start), fin = A(bandas$age_end)),
       fuera = fuera, interp = interp,
       P = filas(dl$.dl_pesos_edad(edades_P, bandas$age_group_id, b$bandas_catalogo, fuera, interp)))
P_lineal <- dl$.dl_pesos_edad(edades_P, proxy_bandas$age_group_id, b$bandas_catalogo, "cero", "lineal")
dX_c <- data.table(draw = rep(1:2, each = 5), location_id = "07", age_group_id = rep(proxy_bandas$age_group_id, 2),
                   dX = c(0.3, 0.1, -0.2, 0.05, 0.4, -0.1, 0.2, 0.25, -0.3, 0.15))
delta <- dl$.dl_desplazamiento_log(dX_c, P_lineal, beta = c(0.8, -1.1), kappa = 0.7, n_dptos = 1L)[["07"]]
fx$subnacional <- list(
  renormalizacion = list(caso_renorm(1L, 17L, 60, 65, 1L, "p"), caso_renorm(1L, 17L, 60, 65, 2L, "p"),
                         caso_renorm(2L, 30L, 80, 85, 7L, "p"), caso_renorm(1L, 14L, 45, 50, 3L, "ipop"),
                         caso_renorm(2L, 235L, 95, 125, 50L, "ipop")),
  edades_P = A(edades_P),
  pesos_edad = list(P_caso(proxy_bandas, "cero", "lineal"), P_caso(proxy_bandas, "vecina", "lineal"),
                    P_caso(proxy_bandas, "cero", "escalon"), P_caso(proxy_bandas, "vecina", "escalon"),
                    P_caso(proxy_contiguas, "cero", "lineal"),
                    P_caso(b$bandas_catalogo[age_group_id == 22L], "cero", "lineal")),
  desplazamiento = list(dX = lapply(1:2, function(k) A(dX_c[draw == k, dX])), beta = A(c(0.8, -1.1)), kappa = 0.7,
                        delta = filas(delta)),
  transformaciones = list(
    x = A(c(0.2, 0.5, 0.9, 0.97)), tipos = A(c("log", "logit", "lineal")),
    T = lapply(c("log", "logit", "lineal"), function(t) A(dl$.dl_transformar(c(0.2, 0.5, 0.9, 0.97), t))),
    sd_T = lapply(c("log", "logit", "lineal"), function(t) dl$.dl_sd_transformada(0.2, 0.6, t)),
    sd_T_degenerada = c(dl$.dl_sd_transformada(0.5, 0.5, "log"), dl$.dl_sd_transformada(NA, 0.5, "log")),
    sd_delta = lapply(c("log", "logit", "lineal"), function(t) dl$.dl_sd_delta(0.3, 0.02, t))))

# ==== AVD (R/avd.R) ==================================================================================================

sev <- b$severidad
casos_beta <- data.table(media = c(sev$proportion, sev$dw_mean, 0, 1, 0.5, 0.95, 0.3, 0.02, 0.5),
                         inferior = c(sev$prop_lower, sev$dw_lower, 0, 1, 0.5, 0, 0.2, 0.012, 0.4999999),
                         superior = c(sev$prop_upper, sev$dw_upper, 0, 1, 0.5, 1, 0.4, 0.031, 0.5000001))
n_sim <- nrow(ajuste$draws_par[[1]])
pi_dw <- dl$.dl_simular_pi_dw(sev, n_sim, 1)
set.seed(1)
pi_muestra <- suppressWarnings(dl$.dl_rbeta_momentos(sev$proportion, sev$prop_lower, sev$prop_upper, n_sim))
stopifnot(identical(pi_muestra / rowSums(pi_muestra), pi_dw$pi_ke))
yld <- dl_avd(ajuste, b, comorbilidad = como, semilla = 1)
fx$avd <- list(
  beta_momentos = lapply(seq_len(nrow(casos_beta)), function(k) {
    m <- suppressWarnings(dl$.dl_beta_momentos(casos_beta$media[k], casos_beta$inferior[k], casos_beta$superior[k]))
    c(list(media = casos_beta$media[k], inferior = casos_beta$inferior[k], superior = casos_beta$superior[k]), m)
  }),
  proporciones = lapply(1:3, function(k) list(pi_muestra = A(pi_muestra[k, ]), pi = A(pi_dw$pi_ke[k, ]),
                                              dw = A(pi_dw$dw_ke[k, ]))),
  bandas = lapply(list(c(1L, 13L, 1L), c(1L, 13L, 2L), c(2L, 235L, 3L), c(2L, 18L, 1L)), function(s) {
    p <- yld$draws_prev_banda[location_id == b$loc_ancla & sex_id == s[1] & age_group_id == s[2] & draw == s[3], val]
    list(sexo = s[1], banda = s[2], draw = s[3], p_banda = p,
         como = como[sex_id == s[1] & age_group_id == s[2], factor],
         avd = yld$draws_yld[location_id == b$loc_ancla & sex_id == s[1] & age_group_id == s[2] & draw == s[3], val])
  }))

# ==== (d) Casos adversarios sueltos ==================================================================================

set.seed(99)
adv <- list()

# chol() de la correlación AR(1) (LAPACK dpotrf) con rho cerca de 1, n = 1 y rho = 0, y L_ancla con lambda 0.25.
adv$cholesky <- lapply(list(c(1, 0.5), c(2, 0.99), c(3, 0.999), c(13, 0.95), c(13, 0.999), c(20, 0.9999), c(20, 0)),
  function(caso) {
    n <- as.integer(caso[1]); rho <- caso[2]
    U <- dl$.dl_chol_ar1(n, rho); z <- rnorm(n, 0, 0.3); sl <- runif(n, 0.05, 0.4)
    list(n = n, rho = rho, U = filas(U), z = A(z), sigma_log = A(sl), lambda = 0.25,
         valor = dl$.dl_lp_mvn_ar1(z, sl, U, 0.25))
  })
# chol() de matrices simétricas definidas positivas cualesquiera (la forma de la propuesta usa chol de una covarianza)
adv$cholesky_general <- lapply(c(1L, 2L, 5L, 9L), function(n) {
  A0 <- crossprod(matrix(rnorm(n * (n + 2)), n + 2)) + diag(1e-3, n)
  list(A = filas(A0), U = filas(chol(A0)))
})

# Cadenas cortas: n_sim 4 (el mínimo de dl_ess), 5 y 7 (impares: la última simulación queda fuera del R-hat), t_max
# impar (el último rezago no forma par) y una cadena anticorrelacionada (el primer par de Geyer es negativo).
cadenas_de <- function(n_cad, n_sim, gen) lapply(seq_len(n_cad), function(c) gen(n_sim, c))
cad <- list(
  n4 = cadenas_de(3, 4, function(n, c) cbind(a = ar1(n, 0.5), b = rnorm(n))),
  n5 = cadenas_de(2, 5, function(n, c) cbind(a = ar1(n, -0.9), b = ar1(n, 0.95))),
  n7 = cadenas_de(4, 7, function(n, c) cbind(a = ar1(n, 0.2), b = rnorm(n, c))),
  anti = cadenas_de(3, 60, function(n, c) cbind(a = ar1(n, -0.95), b = ar1(n, 0.99) + c)))
adv$diagnosticos <- lapply(cad, function(cs) list(
  cadenas = lapply(cs, filas), rhat = A(unname(dl_rhat(cs))), ess = A(unname(dl_ess(cs))),
  ess_lag2 = A(unname(dl_ess(cs, max_lag = 2L))), ess_lag3 = A(unname(dl_ess(cs, max_lag = 3L)))))
adv$geyer <- lapply(list(c(-0.1, 0.05), c(0.3, -0.4, 0.2), c(0.2, 0.1, 0.05, -0.2, 0.9), c(0.5, 0.4),
                         c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, -0.5, -0.4)),
                    function(a) list(autocor = A(a), valor = dl$.dl_suma_geyer(a)))
# Forma de la propuesta con un parámetro que no se movió en la primera mitad del calentamiento (varianza 0: solo el
# 1e-8 de la diagonal la hace definida positiva).
theta_quieta <- cbind(cumsum(rnorm(100, 0, 0.1)), rep(-3, 100), rnorm(100))
adv$forma_quieta <- list(theta_b = filas(theta_quieta), L = filas(dl$.dl_mh_forma_propuesta(theta_quieta)))

# Pesos de las bandas del proxy con huecos ([30, 40) y [50, 60)), una banda abierta (60+, punto medio con 85), bandas
# pasadas sin ordenar, edades más allá de la última banda y empates de distancia (34.5 entre [20, 30) y [40, 45); 54.5
# entre [45, 50) y [60, 125)): con fuera = vecina gana la más joven.
cat_adv <- data.table(age_group_id = c(901L, 902L, 903L, 904L), age_start = c(20, 40, 60, 45),
                      age_end = c(30, 45, 125, 50))
ids_desordenados <- c(903L, 901L, 904L, 902L)
edades_P2 <- seq(0, 130, by = 0.5)
adv$pesos_edad <- list(
  edades = A(edades_P2),
  bandas = list(age_group_id = A(ids_desordenados),
                inicio = A(cat_adv$age_start[match(ids_desordenados, cat_adv$age_group_id)]),
                fin = A(cat_adv$age_end[match(ids_desordenados, cat_adv$age_group_id)])),
  casos = lapply(list(c("cero", "lineal"), c("vecina", "lineal"), c("cero", "escalon"), c("vecina", "escalon")),
                 function(o) {
                   P <- dl$.dl_pesos_edad(edades_P2, ids_desordenados, cat_adv, o[1], o[2])
                   list(fuera = o[1], interp = o[2], filas_id = A(as.integer(rownames(P))), P = filas(P))
                 }))
# desplazamiento con esas bandas (filas de dX en el orden de las bandas ordenadas, como rownames(P))
P_adv <- dl$.dl_pesos_edad(edades_P2, ids_desordenados, cat_adv, "vecina", "lineal")
dX_adv <- data.table(draw = rep(1:3, each = 4), location_id = "15", age_group_id = rep(ids_desordenados, 3),
                     dX = c(0.3, -0.1, 0.2, 0.05, -0.4, 0.25, 0.1, -0.05, 0, 0.5, -0.5, 0.2))
delta_adv <- dl$.dl_desplazamiento_log(dX_adv, P_adv, beta = c(0.8, -1.1, 2.5), kappa = 0.35, n_dptos = 1L)[["15"]]
adv$desplazamiento <- list(
  dX = lapply(1:3, function(k) { d <- dX_adv[draw == k]; A(d$dX[match(as.integer(rownames(P_adv)), d$age_group_id)]) }),
  beta = A(c(0.8, -1.1, 2.5)), kappa = 0.35, delta = filas(delta_adv))

# Bordes de la Beta por momentos: v = ((superior - inferior) / 3.92)^2 justo en media (1 - media) (>=: infactible) y
# justo en 1e-12 (<: degenerado), a unos ulps de distancia; y un intervalo invertido (superior < inferior).
beta_bordes <- list()
for (m in c(0.5, 0.3, 0.9)) {
  s <- 3.92 * sqrt(m * (1 - m))
  for (d in -3:3) beta_bordes[[length(beta_bordes) + 1L]] <- c(m, 0, s + d * ulp(s))
}
s <- 3.92e-6
for (d in -3:3) beta_bordes[[length(beta_bordes) + 1L]] <- c(0.5, 0, s + d * ulp(s))
beta_bordes[[length(beta_bordes) + 1L]] <- c(0.3, 0.4, 0.2)
adv$beta_bordes <- lapply(beta_bordes, function(x) {
  m <- suppressWarnings(dl$.dl_beta_momentos(x[1], x[2], x[3]))
  v <- ((x[3] - x[2]) / 3.92)^2
  c(list(media = x[1], inferior = x[2], superior = x[3], v = v,
         motivo = if (is.null(m$constante)) "beta"
                  else if (x[1] <= 0 || x[1] >= 1 || v < 1e-12) "degenerado" else "infactible"), m)
})

# El techo de la cascada justo en log(techo): .dl_edo_log_media trunca solo si log f > log(techo) en algún punto. Con
# el máximo de log f igual a log(techo) no trunca; a un ulp por encima, sí.
ctx_e <- dl$.dl_ctx(variantes$ejemplo$b, 1L)
lm_e <- dl$.dl_log_tasas_media(unname(ajuste$draws_par[["1"]][1, ]), ctx_e)
adv$techo_cascada <- lapply(c(0.3, 0.07), function(techo) {
  lt <- log(techo)
  igual <- pmin(lm_e$log_f + 2, lt)
  arriba <- igual; arriba[which.max(igual)] <- lt + ulp(lt)
  list(techo_f = techo, log_techo = lt, log_i_media = A(lm_e$log_i),
       casos = lapply(list(igual, arriba), function(lf) {
         s <- dl$.dl_edo_log_media(lm_e$log_i, lf, ctx_e, techo_f = techo)
         list(log_f_media = A(lf), truncado = s$truncado, p = A(s$p), f = A(s$f))
       }))
})
fx$adversario <- adv

# ==== Escritura ======================================================================================================

texto <- toJSON(fx, digits = I(17), na = "string", auto_unbox = TRUE, null = "null", matrix = "rowmajor")
# Las 17 cifras significativas devuelven cada double tal cual (digits = NA, con 15, no siempre).
prueba <- c(fx$vineta$p_malla, fx$vineta$i_media, unlist(fx$contextos$datos$sexos[[1]]$ctx$ancla$sigma_log))
stopifnot(identical(fromJSON(toJSON(prueba, digits = I(17))), prueba))
writeLines(texto, salida, useBytes = TRUE)
cat(sprintf("fixtures.json: %s (%.0f KB)\n", salida, file.size(salida) / 1024))
