# Genera los datos de ejemplo del paquete, en sus dos formatos y con los mismos números:
#   inst/extdata/acs_peru/           el formato simple (config/<causa>.yaml simple, una descarga de GBD Results en
#                                    ancla/, covariables/, poblacion.csv sin filas nacionales, proxies.csv, datos.csv,
#                                    severidad.csv, LEEME.md y verdad.csv);
#   inst/extdata/acs_peru_completo/  el formato completo (el de la versión 0.2.2), que usan el arnés de
#                                    compatibilidad y las guías avanzadas;
# y las tablas de referencia del paquete (inst/referencia/: grupos de edad de GBD y etiquetas en español), las mismas
# del catálogo del formato completo. Lo que el formato simple calcula al leer el proyecto (las filas nacionales de la
# población, que son la suma de las departamentales, y los pesos de 80+, que son las cuotas de la población
# nacional) se escribe en el formato completo con las mismas cuentas y el mismo escritor (data.table::fwrite), así
# que los dos formatos dan exactamente los mismos números.
#
# Todo es sintético: una enfermedad ficticia, la «arteriopatía crónica sintética» (ACS, causa 9100) con tres
# subtipos (9101 miembros inferiores, 9102 carotídea, 9103 renovascular), sobre la geografía real del Perú
# (nacional y 25 departamentos con su ubigeo). Ningún número proviene de datos reales.
#
# Uso, desde la raíz del repositorio:
#   Rscript data-raw/generar_acs_peru.R
#
# Reproducible: semilla fija, sin fechas ni rutas dentro de los archivos; dos corridas escriben archivos idénticos
# byte a byte (se comprueba con `git status --porcelain` tras una segunda corrida). Dependencias: R base,
# data.table y yaml. La EDO se resuelve con `dl_edo_resolver` del propio paquete (el código de este árbol, cargado con
# pkgload, o el paquete instalado), de modo que la verdad usa las mismas cuentas que el modelo.
#
# Modelo de verdad (detalle y restricciones en data-raw/LEEME.md):
#   subtipo k, sexo s, año y, edad a (años):
#     i_k(a) = I_k exp(g_k (a - 60)) M_k^[s = 1] T_y        incidencia por persona-año
#     f_k(a) = F_k exp(h_k (a - 60))                         mortalidad en exceso, r = 0, p(30) = 0
#   La causa padre 9100 es la suma de sus subtipos (prevalencia, casos incidentes, muertes y AVD).
#   Valores por banda = promedio de la verdad anual ponderado por la población de cada edad (la de su banda),
#   la misma regla que usa el paquete para comparar el modelo con el ancla.
#   AVD por banda = prevalencia x sum_h pi_h DW_h x COMO(a), COMO(a) = 1 - 0.004 (a - 30).
#   Departamentos: log i_d = log i + beta_SEV dX_SEV,d(a) + beta_LDI dX_LDI,d ; log f_d = log f + beta_HAQ dX_HAQ,d.

SEMILLA <- 20260929L

raiz_repo <- normalizePath(getwd(), winslash = "/")
if (!file.exists(file.path(raiz_repo, "DESCRIPTION")) || !dir.exists(file.path(raiz_repo, "data-raw")))
  stop("generar_acs_peru.R: ejecutar desde la ra\u00edz del repositorio de dismodlite")
salida <- file.path(raiz_repo, "inst", "extdata", "acs_peru_completo")
salida_simple <- file.path(raiz_repo, "inst", "extdata", "acs_peru")
referencia <- file.path(raiz_repo, "inst", "referencia")

# Solver de la EDO del paquete (RK4 sobre la malla h/2), del código de este árbol: con pkgload (devtools) se carga
# el árbol fuente; sin pkgload, el paquete instalado (de la misma versión). Un árbol anterior a la versión 1.0.0
# (con el cargador load.R) se carga con su propio cargador, donde el solver se llamaba dl_ode_solve. Las cuentas son
# las mismas en los tres casos (dl_edo_resolver(i_media, f_media, remision, p0, nsub), argumentos por posición).
resolver_edo <- local({
  cargador <- file.path(raiz_repo, "load.R")
  if (file.exists(cargador)) {
    source(cargador, local = TRUE)
    dismodlite_load(raiz_repo, registrar_print = FALSE)$dl_ode_solve
  } else {
    if (requireNamespace("pkgload", quietly = TRUE))
      pkgload::load_all(raiz_repo, export_all = FALSE, helpers = FALSE, attach = FALSE, quiet = TRUE)
    getExportedValue("dismodlite", "dl_edo_resolver")
  }
})

PROC <- "datos sint\u00e9ticos de ejemplo"

# Las carpetas se rehacen enteras: ningún archivo de una versión anterior sobrevive.
unlink(c(salida, salida_simple), recursive = TRUE)
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# ---- Escritura -----------------------------------------------------------------------------------------------
# Todo se escribe bajo `salida`: la carpeta del formato completo y, en la última sección, la del simple.
ruta <- function(...) file.path(salida, ...)
preparar <- function(f) { dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE); f }
escribir_csv <- function(dt, ...) {
  data.table::fwrite(dt, preparar(ruta(...)), eol = "\n")
  invisible(NULL)
}
# Texto en UTF-8 con fin de línea \n en todos los sistemas (writeLines en modo texto escribe \r\n en Windows).
escribir_texto <- function(lineas, ...) {
  con <- file(preparar(ruta(...)), open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(enc2utf8(lineas), collapse = "\n"), "\n"))), con)
  invisible(NULL)
}
escribir_yaml <- function(x, ..., cabecera = NULL)
  escribir_texto(c(cabecera, sub("\n$", "", yaml::as.yaml(x, indent.mapping.sequence = TRUE))), ...)

# ---- Constantes del modelo de verdad (las marcadas «ajustable» se pueden retocar si una comprobación falla) ----
EDAD_INICIO <- 30L
EDAD_FIN <- 99L
NSUB <- 5L                                        # subpasos RK4 por año (h = 1/5), como el paquete por defecto
EDADES <- EDAD_INICIO:EDAD_FIN
MALLA_MEDIA <- seq(EDAD_INICIO, EDAD_FIN, by = 1 / (2 * NSUB))                  # malla h/2
IDX_ANUAL_MALLA <- seq(1L, (EDAD_FIN - EDAD_INICIO) * NSUB + 1L, by = NSUB)      # edades enteras en la malla h
IDX_ANUAL_MEDIA <- seq(1L, length(MALLA_MEDIA), by = 2L * NSUB)                 # edades enteras en la malla h/2

# Constantes por subtipo. I: ajustable. Con I = 0.0040, 0.0015 y 0.0008 la prevalencia del padre a los 70-74 años
# era 17 % (hombres) y 14 % (mujeres), fuera del rango plausible 2-10 %; se multiplicaron por 0.4.
SUBTIPOS <- data.table::data.table(
  cause_id = 9101:9103,
  I = c(0.0016, 0.0006, 0.0003),
  g = c(0.045, 0.050, 0.040),
  F = c(0.020, 0.030, 0.050),
  h = c(0.030, 0.035, 0.030),
  M = c(1.20, 1.30, 1.10))
T_ANIO <- c(`2019` = 0.97, `2023` = 1)            # tendencia de la incidencia (ajustable)
ANIOS_VERDAD <- c(2019L, 2023L)
ANIOS_POBLACION <- c(2019L, 2023L, 2024L)

CAUSAS <- data.table::data.table(
  cause_id = 9100:9103,
  nombre = c("Arteriopat\u00eda cr\u00f3nica sint\u00e9tica", "ACS de miembros inferiores", "ACS carot\u00eddea",
             "ACS renovascular"),
  slug = c("arteriopatia_cronica_sintetica", "acs_de_miembros_inferiores", "acs_carotidea", "acs_renovascular"),
  nivel = c(3L, 4L, 4L, 4L),
  padre = c(491L, 9100L, 9100L, 9100L))
nombre_causa <- function(id) CAUSAS$nombre[match(id, CAUSAS$cause_id)]

# Severidad: estados de salud, pesos de discapacidad (DW) y proporciones por subtipo.
ESTADOS <- data.table::data.table(
  health_state_id = c(799L, 9801L, 9802L),
  nivel = c("asintomatica", "leve", "moderada"),
  healthstate_name = c("asymptomatic", "arteriopatia_sintomatica_leve", "arteriopatia_sintomatica_moderada"),
  dw = c(0, 0.020, 0.070), dw_lower = c(0, 0.012, 0.046), dw_upper = c(0, 0.031, 0.102))
PROPORCIONES <- data.table::data.table(
  cause_id = rep(9101:9103, each = 3L),
  health_state_id = rep(ESTADOS$health_state_id, 3L),
  proportion = c(0.50, 0.35, 0.15, 0.60, 0.25, 0.15, 0.55, 0.30, 0.15))
FACTOR_UI_PROPORCION <- 0.15                      # prop_lower/upper = proporción x (1 -/+ 0.15)
COMO_PENDIENTE <- 0.004                           # COMO(a) = 1 - 0.004 (a - 30)
como <- function(a) 1 - COMO_PENDIENTE * (a - 30)

# Ruido del ancla: valor = verdad x exp(N(0, 0.05)); intervalo = valor x exp(-/+ 1.96 x 0.15). El AVD repite el error
# de la prevalencia de su celda y suma uno propio de desviación 0.01.
SD_RUIDO_ANCLA <- 0.05
SD_RUIDO_AVD <- 0.01
SIGMA_LOG_ANCLA <- 0.15

# Betas de las covariables (extraccion.yaml) y efecto departamental de los proxies.
BETA <- c(sev = 0.62, ldi = -0.21, haq = -0.012)
BETA_IC <- list(sev = c(0.55, 0.70), ldi = c(-0.30, -0.12), haq = c(-0.018, -0.006))

# ---- Geografía y población ------------------------------------------------------------------------------------
DEPARTAMENTOS <- data.table::data.table(
  location_id = sprintf("%02d", 1:25),
  location_name = c("Amazonas", "\u00c1ncash", "Apur\u00edmac", "Arequipa", "Ayacucho", "Cajamarca", "Callao",
                    "Cusco", "Huancavelica", "Hu\u00e1nuco", "Ica", "Jun\u00edn", "La Libertad", "Lambayeque", "Lima",
                    "Loreto", "Madre de Dios", "Moquegua", "Pasco", "Piura", "Puno", "San Mart\u00edn", "Tacna",
                    "Tumbes", "Ucayali"),
  cuota = c(0.013, 0.036, 0.013, 0.045, 0.020, 0.045, 0.034, 0.042, 0.011, 0.025, 0.029, 0.042, 0.063, 0.039,
            0.330, 0.030, 0.005, 0.006, 0.008, 0.063, 0.039, 0.028, 0.011, 0.008, 0.018))
DEPARTAMENTOS[, cuota := cuota / sum(cuota)]
LOC_NACIONAL <- "123"
NOMBRE_NACIONAL <- "Per\u00fa"

# Índice de desarrollo sintético: un orden aproximado de los departamentos (1 = más desarrollado), no un dato real.
# Ordena los efectos departamentales de los proxies (fuerte en LDI y HAQ, débil en SEV) para que la geografía se vea
# verosímil: Lima, Callao, Moquegua, Arequipa y Tacna arriba; Huancavelica, Huánuco y Apurímac abajo.
ORDEN_DESARROLLO <- c("15", "07", "18", "04", "23", "11", "17", "14", "13", "24", "20", "02", "12", "08", "25",
                      "22", "19", "16", "21", "01", "05", "06", "03", "10", "09")
DEPARTAMENTOS[, desarrollo := {
  rango <- match(location_id, ORDEN_DESARROLLO)
  (mean(rango) - rango) / stats::sd(rango)            # puntaje centrado, desviación estándar 1 (+ = más desarrollado)
}]

# Inclinación etaria de cada departamento (ajustable): log del multiplicador de la población de la banda más vieja
# (95+) frente a la estructura nacional, lineal en la banda y opuesto en la más joven (30-34). Positivo = población más
# envejecida (costa urbana), negativo = más joven (Amazonía); aproximado, no un dato real. Se centra con las cuotas
# para que la estructura nacional (la suma) quede cerca de la de referencia.
INCLINACION_EDAD <- c(`15` = 0.15, `07` = 0.12, `04` = 0.12, `18` = 0.10, `23` = 0.10, `11` = 0.06, `14` = 0.05,
                      `13` = 0.04, `16` = -0.20, `25` = -0.18, `17` = -0.22, `22` = -0.12, `01` = -0.10)
DEPARTAMENTOS[, inclinacion := {
  x <- unname(INCLINACION_EDAD[location_id]); x[is.na(x)] <- 0
  x - sum(cuota * x)
}]

# Bandas de población (finas, desde 30 años) y la banda agregada 80+ (21), que también se escribe porque los
# conteos de las tablas consolidadas buscan la población de cada celda de salida (40-44 ... 75-79 y 80+).
BANDAS_POB <- data.table::data.table(
  age_group_id = c(11:20, 30L, 31L, 32L, 235L),
  age_start = c(seq(30, 75, by = 5), 80, 85, 90, 95),
  age_end = c(seq(35, 80, by = 5), 85, 90, 95, 125))
FINAS_80 <- c(30L, 31L, 32L, 235L)
POB_30MAS <- 15e6                                 # población nacional de 30 años y más en 2023 (ajustable)
RAZON_EDAD <- 0.86                                # razón entre bandas quinquenales de 30-34 a 75-79 (ajustable)
# Desde los 80 años la población cae más rápido: razón de cada banda a la anterior (80-84/75-79, 85-89/80-84,
# 90-94/85-89 y 95+/90-94; ajustable). Da pesos de 80+ cercanos a 0.47, 0.30, 0.16 y 0.07 y un 80+ de ~6.5 % de
# los 30 años y más.
RAZON_80MAS <- c(0.70, 0.65, 0.55, 0.40)
# Proporción de hombres por banda: 0.49 de 30 a 59 años, luego baja en línea recta hasta 0.40 a los 95+ (48 % en
# conjunto; ajustable).
FRAC_HOMBRES <- c(rep(0.49, 6L), seq(0.49, 0.40, length.out = 9L)[-1L])
FACTOR_ANIO_POB <- c(`2019` = 0.95, `2023` = 1, `2024` = 1.01)

peso_banda <- c(RAZON_EDAD^(0:9), RAZON_EDAD^9 * cumprod(RAZON_80MAS))
n_banda <- POB_30MAS * peso_banda / sum(peso_banda)
objetivo <- data.table::rbindlist(list(
  data.table::data.table(sex_id = 1L, age_group_id = BANDAS_POB$age_group_id, k = seq_along(n_banda),
                         n = n_banda * FRAC_HOMBRES),
  data.table::data.table(sex_id = 2L, age_group_id = BANDAS_POB$age_group_id, k = seq_along(n_banda),
                         n = n_banda * (1 - FRAC_HOMBRES))))
# Cada departamento: su cuota de la población de 30 años y más, repartida con la estructura nacional inclinada por
# su inclinación etaria (el multiplicador exp(inclinación x t), con t de -1 en 30-34 a +1 en 95+).
t_banda <- (seq_along(n_banda) - (length(n_banda) + 1) / 2) / ((length(n_banda) - 1) / 2)
dep_2023 <- data.table::rbindlist(lapply(seq_len(nrow(DEPARTAMENTOS)), function(j) {
  m <- exp(DEPARTAMENTOS$inclinacion[j] * t_banda[objetivo$k])
  objetivo[, list(location_id = DEPARTAMENTOS$location_id[j], sex_id, age_group_id,
                  val = round(POB_30MAS * DEPARTAMENTOS$cuota[j] * n * m / sum(n * m)))]
}))
pob_dep <- data.table::rbindlist(lapply(ANIOS_POBLACION, function(y)
  dep_2023[, list(location_id, year = y, sex_id, age_group_id,
                  val = if (y == 2023L) val else round(val * FACTOR_ANIO_POB[[as.character(y)]]))]))
pob_dep <- rbind(pob_dep, pob_dep[age_group_id %in% FINAS_80, list(age_group_id = 21L, val = sum(val)),
                                  by = list(location_id, year, sex_id)], use.names = TRUE)
pob_nac <- pob_dep[, list(location_id = LOC_NACIONAL, val = sum(val)), by = list(year, sex_id, age_group_id)]
poblacion <- rbind(pob_nac[, list(location_id, location_level = 0L, year, sex_id, age_group_id, val)],
                   pob_dep[, list(location_id, location_level = 1L, year, sex_id, age_group_id, val)])
poblacion[, `:=`(val = as.integer(val), acquisition_id = "sintetico_poblacion_v1")]
poblacion[, orden_banda := match(age_group_id, c(11:20, 21L, FINAS_80))]
data.table::setorder(poblacion, location_level, location_id, year, sex_id, orden_banda)
poblacion[, orden_banda := NULL]

# Peso de cada edad anual = población de su banda fina (regla del paquete para promediar sobre un intervalo).
pesos_anuales <- function(loc, sexo, anio) {
  pb <- poblacion[location_id == loc & sex_id == sexo & year == anio & age_group_id %in% BANDAS_POB$age_group_id]
  k <- findInterval(EDADES, BANDAS_POB$age_start)
  as.numeric(pb$val[match(BANDAS_POB$age_group_id[k], pb$age_group_id)])
}
promedio_banda <- function(x, w, inicio, fin) {
  idx <- which(EDADES >= inicio & EDADES < fin)
  sum(x[idx] * w[idx]) / sum(w[idx])
}

# ---- Covariables nacionales (formato GHDx) y proxies departamentales ------------------------------------------
UBIC_COV <- data.table::data.table(location_id = c(1L, 120L, 123L),
                                   location_name = c("Global", "Andean Latin America", NOMBRE_NACIONAL))
COV_NAC <- data.table::rbindlist(list(
  data.table::data.table(covariate_name_short = "SEV_scalar_agestd_cvd_pvd", archivo = "SEV_SCALAR_CVD_PVD.csv",
                         location_id = rep(c(1L, 120L, 123L), each = 4L),
                         year_id = rep(c(2019L, 2019L, 2023L, 2023L), 3L),
                         age_group_id = 27L, age_group_name = "Age-standardized",
                         sex_id = rep(c(1L, 2L), 6L), sex = rep(c("Male", "Female"), 6L),
                         mean_value = c(1.204, 1.094, 1.256, 1.146, 1.228, 1.117, 1.279, 1.168,
                                        1.216, 1.106, 1.268, 1.158), ui = 0.03, redondeo = 4L),
  data.table::data.table(covariate_name_short = "LDI_pc", archivo = "LDI_PC.csv",
                         location_id = rep(c(1L, 120L, 123L), each = 2L), year_id = rep(c(2019L, 2023L), 3L),
                         age_group_id = 22L, age_group_name = "All Ages", sex_id = 3L, sex = "Both",
                         mean_value = c(15840, 17210, 9212, 10452, 9800, 11040), ui = 0.042, redondeo = 1L),
  data.table::data.table(covariate_name_short = "haqi", archivo = "HAQI.csv",
                         location_id = rep(c(1L, 120L, 123L), each = 2L), year_id = rep(c(2019L, 2023L), 3L),
                         age_group_id = 22L, age_group_name = "All Ages", sex_id = 3L, sex = "Both",
                         mean_value = c(54.2, 56.8, 44.5, 48.1, 47.3, 50.9), ui = 0.045, redondeo = 2L)),
  use.names = TRUE)
COV_NAC[, `:=`(location_name = UBIC_COV$location_name[match(location_id, UBIC_COV$location_id)],
               lower_value = round(mean_value * (1 - ui), redondeo),
               upper_value = round(mean_value * (1 + ui), redondeo))]
# Valores inventados (también los de Global y la región): la columna acquisition_id lo declara; el paquete lee solo
# las columnas que necesita.
for (a in unique(COV_NAC$archivo))
  escribir_csv(COV_NAC[archivo == a, list(covariate_name_short, location_id, location_name, year_id, age_group_id,
                                          age_group_name, sex_id, sex, mean_value, lower_value, upper_value,
                                          acquisition_id = "sintetico_cov_v1")],
               "covariables", a)

# Valor nacional de referencia de cada proxy: el GHDx del año; 2024 no tiene GHDx y se ancla en 2023 (proyección).
ancla_cov <- function(nombre, anio, sexo) {
  a <- if (anio == 2024L) 2023L else anio
  COV_NAC[covariate_name_short == nombre & location_id == 123L & year_id == a & sex_id == sexo]$mean_value
}

PROXIES <- data.table::data.table(
  clave = c("sev", "ldi", "haq"),
  covariate_id_proxy = c(900101L, 900102L, 900103L),
  covariate_id_gbd = c(785L, 57L, 1099L),
  covariate_name_short = c("SEV_scalar_agestd_cvd_pvd", "LDI_pc", "haqi"))
BANDAS_SEV <- c(13:20, 21L)                                   # el SEV varía por banda de edad (21 = 80+)
PESO_EDAD_SEV <- seq(0.5, 1.5, length.out = length(BANDAS_SEV)) # el efecto departamental crece con la edad
SD_EFECTO <- c(sev = 0.10, ldi = 0.30, haq = 6)                 # log (SEV, LDI) y puntos (HAQ); ajustable
# Correlación del efecto departamental con el índice de desarrollo sintético (el resto es ruido propio); ajustable.
COR_DESARROLLO <- c(sev = 0.5, ldi = 0.9, haq = 0.85)
SD_RUIDO_ANIO <- c(sev = 0.01, ldi = 0.02, haq = 1)
CV_SE_PROXY <- c(sev = 0.06, ldi = 0.08)                        # se = cv x valor; HAQ: se fijo en puntos
SE_HAQ <- 2

set.seed(SEMILLA + 1L)
efecto <- DEPARTAMENTOS[, list(location_id)]
for (cv in c("sev", "ldi", "haq"))
  efecto[[cv]] <- SD_EFECTO[[cv]] * (COR_DESARROLLO[[cv]] * DEPARTAMENTOS$desarrollo +
                                       sqrt(1 - COR_DESARROLLO[[cv]]^2) * stats::rnorm(nrow(efecto)))
ruido_banda <- data.table::CJ(location_id = DEPARTAMENTOS$location_id, sex_id = 1:2, age_group_id = BANDAS_SEV)
ruido_banda[, e := stats::rnorm(.N, 0, 0.02)]
ruido_anio <- data.table::CJ(clave = c("sev", "ldi", "haq"), location_id = DEPARTAMENTOS$location_id,
                             year = ANIOS_POBLACION)
ruido_anio[, e := stats::rnorm(.N, 0, 1) * SD_RUIDO_ANIO[clave]]

# Cierre: desplazamiento aditivo para que el promedio ponderado por la población del departamento sea exactamente el
# valor nacional (regla promedio_cierra_ancla). Pesos = población de la banda y sexo del proxy (sexo 3 = ambos).
cerrar <- function(crudo, peso, ancla) crudo + (ancla - sum(crudo * peso) / sum(peso))
peso_proxy <- function(anio, sexo, banda) {
  pb <- pob_dep[year == anio]
  pb <- if (banda == 22L) pb[age_group_id %in% BANDAS_POB$age_group_id] else pb[age_group_id == banda]
  if (sexo != 3L) pb <- pb[sex_id == sexo]
  pb <- pb[, list(w = sum(as.numeric(val))), by = location_id]
  pb$w[match(DEPARTAMENTOS$location_id, pb$location_id)]
}
proxies <- data.table::rbindlist(lapply(ANIOS_POBLACION, function(y) {
  ra <- function(cv) ruido_anio[clave == cv & year == y]$e[match(DEPARTAMENTOS$location_id,
                                                                  ruido_anio[clave == cv & year == y]$location_id)]
  sev <- data.table::rbindlist(lapply(1:2, function(s) data.table::rbindlist(lapply(seq_along(BANDAS_SEV), function(j) {
    b <- BANDAS_SEV[j]; anc <- ancla_cov("SEV_scalar_agestd_cvd_pvd", y, s)
    eb <- ruido_banda[sex_id == s & age_group_id == b]
    e_banda <- eb$e[match(DEPARTAMENTOS$location_id, eb$location_id)]
    crudo <- anc * exp(efecto$sev * PESO_EDAD_SEV[j] + e_banda + ra("sev"))
    cal <- cerrar(crudo, peso_proxy(y, s, b), anc)
    data.table::data.table(clave = "sev", location_id = DEPARTAMENTOS$location_id, year = y, sex_id = s,
                           age_group_id = b, valor_crudo = crudo, valor_calibrado = cal,
                           valor_calibrado_se = CV_SE_PROXY[["sev"]] * cal, ancla_ghdx = anc)
  }))))
  anc_ldi <- ancla_cov("LDI_pc", y, 3L)
  crudo_ldi <- anc_ldi * exp(efecto$ldi + ra("ldi"))
  cal_ldi <- cerrar(crudo_ldi, peso_proxy(y, 3L, 22L), anc_ldi)
  anc_haq <- ancla_cov("haqi", y, 3L)
  crudo_haq <- anc_haq + efecto$haq + ra("haq")
  cal_haq <- cerrar(crudo_haq, peso_proxy(y, 3L, 22L), anc_haq)
  rbind(sev,
        data.table::data.table(clave = "ldi", location_id = DEPARTAMENTOS$location_id, year = y, sex_id = 3L,
                               age_group_id = 22L, valor_crudo = crudo_ldi, valor_calibrado = cal_ldi,
                               valor_calibrado_se = CV_SE_PROXY[["ldi"]] * cal_ldi, ancla_ghdx = anc_ldi),
        data.table::data.table(clave = "haq", location_id = DEPARTAMENTOS$location_id, year = y, sex_id = 3L,
                               age_group_id = 22L, valor_crudo = crudo_haq, valor_calibrado = cal_haq,
                               valor_calibrado_se = SE_HAQ, ancla_ghdx = anc_haq))
}))
proxies <- merge(proxies, PROXIES[, list(clave, covariate_id_proxy, covariate_id_gbd)], by = "clave")
proxies[, n_efectivo := round(500 + 5000 * DEPARTAMENTOS$cuota[match(location_id, DEPARTAMENTOS$location_id)])]
proxies[, `:=`(metodo_calibracion = "desplazamiento_al_ancla_nacional", acquisition_id = "sintetico_proxy_v1")]
data.table::setorder(proxies, covariate_id_proxy, year, sex_id, age_group_id, location_id)
escribir_csv(proxies[, list(covariate_id_proxy, covariate_id_gbd, location_id, year, sex_id, age_group_id, valor_crudo,
                            valor_calibrado, valor_calibrado_se, n_efectivo, metodo_calibracion, ancla_ghdx,
                            acquisition_id)],
             "proxies_departamentales.csv")

# dX de la cascada (escala de la transformación): log para SEV y LDI, lineal (escala 1) para HAQ. El dX por banda
# del SEV se lleva a la malla h/2 como la cascada por defecto: interpolación lineal entre los puntos medios de las
# bandas (80+ con extremo 85), constante más allá de los medios extremos y 0 fuera de las bandas (antes de 40 años).
MEDIOS_SEV <- (c(seq(40, 75, by = 5), 80) + pmin(c(seq(45, 80, by = 5), 125), 85)) / 2
dx_sev_malla <- function(dx_bandas) {
  a <- pmin(pmax(MALLA_MEDIA, MEDIOS_SEV[1]), MEDIOS_SEV[length(MEDIOS_SEV)])
  v <- stats::approx(MEDIOS_SEV, dx_bandas, xout = a)$y
  v[MALLA_MEDIA < 40] <- 0
  v
}
desplazamientos <- function(loc, sexo, anio) {
  px <- proxies[location_id == loc & year == anio]
  sev <- px[clave == "sev" & sex_id == sexo][match(BANDAS_SEV, age_group_id)]
  dx_sev <- log(sev$valor_calibrado) - log(ancla_cov("SEV_scalar_agestd_cvd_pvd", anio, sexo))
  dx_ldi <- log(px[clave == "ldi"]$valor_calibrado) - log(ancla_cov("LDI_pc", anio, 3L))
  dx_haq <- px[clave == "haq"]$valor_calibrado - ancla_cov("haqi", anio, 3L)
  list(i = BETA[["sev"]] * dx_sev_malla(dx_sev) + BETA[["ldi"]] * dx_ldi, f = BETA[["haq"]] * dx_haq)
}

# ---- Verdad anual -----------------------------------------------------------------------------------------------
verdad_subtipo <- function(st, sexo, anio, loc = LOC_NACIONAL) {
  i_media <- st$I * exp(st$g * (MALLA_MEDIA - 60)) * (if (sexo == 1L) st$M else 1) * T_ANIO[[as.character(anio)]]
  f_media <- st$F * exp(st$h * (MALLA_MEDIA - 60))
  if (loc != LOC_NACIONAL) {
    d <- desplazamientos(loc, sexo, anio)
    i_media <- i_media * exp(d$i)
    f_media <- f_media * exp(d$f)
  }
  p <- resolver_edo(i_media, f_media, 0, 0, NSUB)
  data.table::data.table(cause_id = st$cause_id, location_id = loc, sex_id = sexo, anio = anio, edad = EDADES,
                         p = p[IDX_ANUAL_MALLA], i = i_media[IDX_ANUAL_MEDIA], f = f_media[IDX_ANUAL_MEDIA])
}
verdad_sub <- data.table::rbindlist(lapply(c(LOC_NACIONAL, DEPARTAMENTOS$location_id), function(loc)
  data.table::rbindlist(lapply(ANIOS_VERDAD, function(y) data.table::rbindlist(lapply(1:2, function(s)
    data.table::rbindlist(lapply(seq_len(nrow(SUBTIPOS)), function(k) verdad_subtipo(SUBTIPOS[k], s, y, loc)))))))))
# Padre 9100: p = sum p_k; su incidencia (entre susceptibles) y su EMR son las que reproducen los casos incidentes
# sum i_k (1 - p_k) y las muertes sum p_k f_k de los subtipos. A los 30 años p = 0 y la EMR del padre es su límite
# cuando p -> 0 (cerca del inicio p_k es proporcional a i_k): sum i_k f_k / sum i_k.
verdad_padre <- verdad_sub[, list(cause_id = 9100L, p = sum(p), ipop = sum(i * (1 - p)), pf = sum(p * f),
                                  f0 = sum(i * f) / sum(i)),
                           by = list(location_id, sex_id, anio, edad)]
verdad_padre[, `:=`(i = ipop / (1 - p), f = data.table::fifelse(p > 0, pf / p, f0))]
verdad <- rbind(verdad_sub, verdad_padre[, list(cause_id, location_id, sex_id, anio, edad, p, i, f)])

verdad_csv <- data.table::copy(verdad)
verdad_csv[, nivel := data.table::fifelse(location_id == LOC_NACIONAL, 0L, 1L)]
data.table::setorder(verdad_csv, cause_id, nivel, location_id, anio, sex_id, edad)
verdad_csv[, `:=`(p = signif(p, 6), i = signif(i, 6), f = signif(f, 6))]
# verdad.csv va solo en el formato simple (no es un insumo del modelo; ver la última sección).

# ---- Severidad ----------------------------------------------------------------------------------------------------
PROPORCIONES <- merge(PROPORCIONES, ESTADOS, by = "health_state_id")
data.table::setorder(PROPORCIONES, cause_id, health_state_id)
dwp <- PROPORCIONES[, list(dwp = sum(proportion * dw)), by = cause_id]
# Mezcla del padre: proporciones ponderadas por los casos prevalentes nacionales de 2023 (ambos sexos).
casos <- verdad_sub[location_id == LOC_NACIONAL & anio == 2023L, {
  w <- pesos_anuales(LOC_NACIONAL, sex_id[1], 2023L)
  list(casos = sum(p * w / 5))
}, by = list(cause_id, sex_id)][, list(casos = sum(casos)), by = cause_id]
casos[, cuota := casos / sum(casos)]
mezcla <- merge(PROPORCIONES, casos[, list(cause_id, cuota)], by = "cause_id")[
  , list(proportion = sum(proportion * cuota)), by = health_state_id]
severidad <- rbind(mezcla[, list(cause_id = 9100L, health_state_id, proportion)],
                   PROPORCIONES[, list(cause_id, health_state_id, proportion)])
severidad <- merge(severidad, ESTADOS, by = "health_state_id")
severidad[, `:=`(prop_lower = proportion * (1 - FACTOR_UI_PROPORCION),
                 prop_upper = proportion * (1 + FACTOR_UI_PROPORCION),
                 beta_covariable = NA_character_, location_id_fuente = LOC_NACIONAL, fuente = "mod")]
data.table::setorder(severidad, cause_id, health_state_id)
for (id in CAUSAS$cause_id)
  escribir_csv(severidad[cause_id == id, list(cause_id, health_state_id, proportion, prop_lower, prop_upper,
                                              dw_mean = dw, dw_lower, dw_upper, beta_covariable, location_id_fuente,
                                              fuente)],
               "severidad", sprintf("%d.csv", id))

# ---- Ancla (estimaciones de referencia) -------------------------------------------------------------------------
BANDAS_ANCLA <- data.table::data.table(
  age_group_id = c(13:20, FINAS_80),
  age_group_name = c(sprintf("%d-%d years", seq(40, 75, by = 5), seq(44, 79, by = 5)),
                     "80-84 years", "85-89 years", "90-94 years", "95+ years"),
  inicio = c(seq(40, 75, by = 5), 80, 85, 90, 95),
  fin = c(seq(45, 80, by = 5), 85, 90, 95, 125))
MEDIDAS_ANCLA <- data.table::data.table(
  medida = c("prevalencia", "incidencia", "mortalidad", "avd"),
  measure_id = c(5L, 6L, 1L, 3L),
  measure_name = c("Prevalence", "Incidence", "Deaths", "YLDs (Years Lived with Disability)"),
  metric_id = c(2L, 3L, 3L, 3L),
  metric_name = c("Percent", "Rate", "Rate", "Rate"),
  escala = c(1, 1e5, 1e5, 1e5))
nac <- verdad_sub[location_id == LOC_NACIONAL]
ancla_verdad <- nac[, {
  w <- pesos_anuales(LOC_NACIONAL, sex_id[1], anio[1])
  k <- cause_id[1]
  data.table::rbindlist(lapply(seq_len(nrow(BANDAS_ANCLA)), function(j) {
    b <- BANDAS_ANCLA[j]
    data.table::data.table(age_group_id = b$age_group_id, medida = MEDIDAS_ANCLA$medida,
                           verdad = c(promedio_banda(p, w, b$inicio, b$fin),
                                      promedio_banda(i * (1 - p), w, b$inicio, b$fin) * 1e5,
                                      promedio_banda(p * f, w, b$inicio, b$fin) * 1e5,
                                      promedio_banda(p * como(edad), w, b$inicio, b$fin) *
                                        dwp$dwp[dwp$cause_id == k] * 1e5))
  }))
}, by = list(cause_id, anio, sex_id)]
ancla_verdad[, orden_medida := match(medida, MEDIDAS_ANCLA$medida)]
data.table::setorder(ancla_verdad, orden_medida, cause_id, anio, sex_id, age_group_id)
set.seed(SEMILLA + 2L)
ancla_verdad[, e := stats::rnorm(.N, 0, SD_RUIDO_ANCLA)]
# El AVD lleva el error de la prevalencia de su celda más un término propio pequeño: así el factor COMO empírico
# yld / (prev x sum pi DW) recupera la curva verdadera COMO(a), suave y menor que 1, en vez del cociente de dos
# errores independientes.
ancla_verdad[ancla_verdad[medida == "prevalencia"], e_prev := i.e, on = c("cause_id", "anio", "sex_id", "age_group_id")]
ancla_verdad[medida == "avd", e := e_prev + stats::rnorm(.N, 0, SD_RUIDO_AVD)]
ancla_verdad[, val := verdad * exp(e)]
# El ancla del padre es la suma de las anclas de sus subtipos, como en el GBD.
ancla <- rbind(ancla_verdad[, list(cause_id, anio, sex_id, age_group_id, medida, orden_medida, val)],
               ancla_verdad[, list(cause_id = 9100L, val = sum(val)),
                            by = list(anio, sex_id, age_group_id, medida, orden_medida)], use.names = TRUE)
ancla <- merge(ancla, MEDIDAS_ANCLA, by = "medida")
ancla <- merge(ancla, BANDAS_ANCLA[, list(age_group_id, age_group_name)], by = "age_group_id")
ancla[, `:=`(lower = val * exp(-1.96 * SIGMA_LOG_ANCLA), upper = val * exp(1.96 * SIGMA_LOG_ANCLA))]
ancla[, orden_banda := match(age_group_id, BANDAS_ANCLA$age_group_id)]
data.table::setorder(ancla, orden_medida, cause_id, anio, orden_banda, sex_id)
ancla_csv <- ancla[, list(acquisition_id = "sintetico_acs_v1", source = "gbd", round = 2023L, entity = "cause",
                          location_id = LOC_NACIONAL, location_name = NOMBRE_NACIONAL, location_level = 0L,
                          year = anio, age_group_id, age_group_name, sex_id,
                          sex_name = data.table::fifelse(sex_id == 1L, "Male", "Female"),
                          cause_id, cause_name = nombre_causa(cause_id), measure_id, measure_name, metric_id,
                          metric_name, val, lower, upper, ui_level = 0.95, medida)]
for (m in MEDIDAS_ANCLA$medida) escribir_csv(ancla_csv[medida == m][, medida := NULL], "ancla", paste0(m, ".csv"))

pesos_80 <- poblacion[location_id == LOC_NACIONAL & year == 2023L & age_group_id %in% FINAS_80]
pesos_80[, peso := val / sum(val), by = sex_id]
pesos_80[, orden_banda := match(age_group_id, FINAS_80)]
data.table::setorder(pesos_80, sex_id, orden_banda)
escribir_csv(pesos_80[, list(age_group_id, sex_id, peso)], "pesos_80mas.csv")
escribir_csv(poblacion, "poblacion.csv")

# ---- Datos locales (datos.csv), causa 9100 ------------------------------------------------------------------------
set.seed(SEMILLA + 3L)
v9100 <- verdad[cause_id == 9100L]
# (argumento `y` y no `anio`: dentro de [.data.table el nombre de la columna taparía al argumento)
verdad_banda <- function(loc, sexo, y, inicio, fin, integrando) {
  v <- v9100[location_id == loc & sex_id == sexo & anio == y]
  data.table::setorder(v, edad)
  x <- switch(integrando, p = v$p, ipop = v$i * (1 - v$p), pf = v$p * v$f)
  promedio_banda(x, pesos_anuales(loc, sexo, y), inicio, fin)
}
fila_dato <- function(dato_id, tipo_dato, measure_id, measure_name, loc, sexo, inicio, fin, age_group_id, anio,
                      val = NA_real_, se = NA_real_, x = NA_integer_, n = NA_integer_, definicion,
                      ajuste_completitud = FALSE, completitud = NA_real_, outlier = FALSE,
                      outlier_motivo = NA_character_, acquisition_id) {
  nivel <- if (loc == LOC_NACIONAL) 0L else 1L
  data.table::data.table(
    dato_id = dato_id, cause_id = 9100L, tipo_dato = tipo_dato, measure_id = measure_id, measure_name = measure_name,
    location_id = loc,
    location_name = if (nivel == 0L) NOMBRE_NACIONAL else DEPARTAMENTOS$location_name[DEPARTAMENTOS$location_id == loc],
    location_level = nivel, sex_id = sexo, sex_name = if (sexo == 1L) "Male" else "Female",
    age_start = inicio, age_end = fin, age_group_id = age_group_id, year_start = anio, year_end = anio,
    val = val, se = se, x = x, n = n, n_efectivo = NA_real_, definicion = definicion, es_referencia = TRUE,
    crosswalk_id = NA_character_, ajuste_completitud = ajuste_completitud, completitud = completitud,
    outlier = outlier, outlier_motivo = outlier_motivo, acquisition_id = acquisition_id, nid_ghdx = NA_integer_,
    cita = PROC)
}
DEF_PREV <- "tamizaje vascular estandarizado (sint\u00e9tico)"
DEF_RV <- "registro vital, causa b\u00e1sica (sint\u00e9tico)"
datos <- list()
# Mortalidad nacional del registro vital (ya corregida por completitud), 50-54 ... 75-79 y 80+.
bandas_rv <- data.table::data.table(inicio = c(seq(50, 75, by = 5), 80), fin = c(seq(55, 80, by = 5), 125),
                                    age_group_id = c(15:20, 21L))
for (s in 1:2) for (j in seq_len(nrow(bandas_rv))) {
  b <- bandas_rv[j]
  v <- verdad_banda(LOC_NACIONAL, s, 2023L, b$inicio, b$fin, "pf") * exp(stats::rnorm(1, 0, 0.05))
  datos[[length(datos) + 1L]] <- fila_dato(sprintf("csmr_nac_%d_%d", s, b$age_group_id), "csmr", 900006L, "csmr",
    LOC_NACIONAL, s, b$inicio, b$fin, b$age_group_id, 2023L, val = v, se = 0.10 * v,
    definicion = DEF_RV, ajuste_completitud = TRUE,
    completitud = round(stats::runif(1, 0.85, 0.95), 4), acquisition_id = "sintetico_rv_v1")
}
# Estudio de prevalencia con conteos (x de n = 800), ruta binomial.
bandas_prev <- data.table::data.table(inicio = c(60, 70, 80), fin = c(65, 75, 125), age_group_id = c(17L, 19L, 21L))
for (s in 1:2) for (j in seq_len(nrow(bandas_prev))) {
  b <- bandas_prev[j]
  q <- verdad_banda(LOC_NACIONAL, s, 2023L, b$inicio, b$fin, "p")
  datos[[length(datos) + 1L]] <- fila_dato(sprintf("prev_nac_%d_%d", s, b$age_group_id), "prev_estudio", 5L,
    "Prevalence", LOC_NACIONAL, s, b$inicio, b$fin, b$age_group_id, 2023L,
    x = as.integer(stats::rbinom(1, 800L, q)), n = 800L, definicion = DEF_PREV, acquisition_id = "sintetico_estudio_v1")
}
# Cohorte de incidencia (incidencia poblacional i (1 - p)), 60-69 y 70-79.
bandas_inc <- data.table::data.table(inicio = c(60, 70), fin = c(70, 80))
for (s in 1:2) for (j in seq_len(nrow(bandas_inc))) {
  b <- bandas_inc[j]
  v <- verdad_banda(LOC_NACIONAL, s, 2023L, b$inicio, b$fin, "ipop") * exp(stats::rnorm(1, 0, 0.10))
  datos[[length(datos) + 1L]] <- fila_dato(sprintf("inc_nac_%d_%d", s, b$inicio), "incidencia", 6L, "Incidence",
    LOC_NACIONAL, s, b$inicio, b$fin, NA_integer_, 2023L, val = v, se = 0.20 * v,
    definicion = "primer diagn\u00f3stico en una cohorte (sint\u00e9tico)", acquisition_id = "sintetico_cohorte_v1")
}
# Un valor atípico, marcado con su motivo (no entra al ajuste).
q_out <- verdad_banda(LOC_NACIONAL, 2L, 2023L, 65, 70, "p") * 3
datos[[length(datos) + 1L]] <- fila_dato("prev_nac_atipico", "prev_estudio", 5L, "Prevalence", LOC_NACIONAL, 2L, 65, 70,
  18L, 2023L, val = q_out, se = 0.20 * q_out, definicion = DEF_PREV, outlier = TRUE,
  outlier_motivo = "valor at\u00edpico sint\u00e9tico para el ejemplo", acquisition_id = "sintetico_estudio_v1")
# Held-out departamental de 2019 (año cascada.heldout_anio del config) en 10 departamentos, desde la verdad
# departamental. Nunca entra al ajuste. La validación de amplitud de la cascada usa solo la mortalidad (csmr); la
# prevalencia departamental queda como ejemplo de held-out de otro tipo (cuenta en n_heldout del manifiesto).
DEP_ENCUESTA <- c("01", "04", "06", "08", "13", "15", "16", "20", "21", "25")
bandas_dep <- data.table::data.table(inicio = c(50, 65), fin = c(65, 80))
for (d in DEP_ENCUESTA) for (s in 1:2) for (j in seq_len(nrow(bandas_dep))) {
  b <- bandas_dep[j]
  q <- verdad_banda(d, s, 2019L, b$inicio, b$fin, "p")
  datos[[length(datos) + 1L]] <- fila_dato(sprintf("prev_%s_%d_%d", d, s, b$inicio), "prev_estudio", 5L, "Prevalence",
    d, s, b$inicio, b$fin, NA_integer_, 2019L, x = as.integer(stats::rbinom(1, 600L, q)), n = 600L,
    definicion = DEF_PREV, acquisition_id = "sintetico_encuesta_v1")
}
# Mortalidad departamental del registro vital (csmr) de 2019, bandas quinquenales 50-54 ... 75-79 para que la
# pendiente estandarizada por edad de la validación de amplitud esté definida; ruido y completitud como el nacional.
bandas_rv_dep <- data.table::data.table(inicio = seq(50, 75, by = 5), fin = seq(55, 80, by = 5), age_group_id = 15:20)
for (d in DEP_ENCUESTA) for (s in 1:2) for (j in seq_len(nrow(bandas_rv_dep))) {
  b <- bandas_rv_dep[j]
  v <- verdad_banda(d, s, 2019L, b$inicio, b$fin, "pf") * exp(stats::rnorm(1, 0, 0.05))
  datos[[length(datos) + 1L]] <- fila_dato(sprintf("csmr_%s_%d_%d", d, s, b$age_group_id), "csmr", 900006L, "csmr",
    d, s, b$inicio, b$fin, b$age_group_id, 2019L, val = v, se = 0.10 * v, definicion = DEF_RV,
    ajuste_completitud = TRUE, completitud = round(stats::runif(1, 0.85, 0.95), 4),
    acquisition_id = "sintetico_rv_dep_v1")
}
escribir_csv(data.table::rbindlist(datos), "datos.csv")

# ---- Partición de severidad (layout de una corrida mod/severity_split) -------------------------------------------
# Identificador corto: la ruta más larga del paquete, dismodlite/inst/extdata/acs_peru_completo/particion/<run>/
# cause_health_state/proportion/<run>.csv, debe caber en los 100 bytes que un tarball portable admite
# (R CMD build y R CMD check lo exigen); con "acs_v1" mide 99.
RUN_SPLIT <- "acs_v1"
SECUELAS <- PROPORCIONES[, list(cause_id, health_state_id, healthstate_name, nivel, proportion)]
SECUELAS[, `:=`(sequela_id = cause_id * 10L + match(health_state_id, ESTADOS$health_state_id),
                sequela_name = paste0(CAUSAS$slug[match(cause_id, CAUSAS$cause_id)], "_", nivel))]
SECUELAS[, me_id := 99000L + seq_len(.N)]
fila_split <- function(entidad, cause_id, id_col, ids, nombres, val) {
  dt <- data.table::data.table(run_id = RUN_SPLIT, source = "gbd", round = 2023L, entity = entidad,
                               method = "severity_split", location_id = LOC_NACIONAL, location_name = NOMBRE_NACIONAL,
                               location_level = 0L, year = 2023L, age_group_id = 22L, age_group_name = "All ages",
                               sex_id = 3L, sex_name = "Both", cause_id = cause_id, cause_name = nombre_causa(cause_id))
  dt <- dt[rep(1L, length(ids))]
  data.table::set(dt, j = id_col, value = ids)
  data.table::set(dt, j = sub("_id$", "_name", id_col), value = nombres)
  dt[, `:=`(measure_id = 900001L, measure_name = "Proportion", metric_id = 2L, metric_name = "Percent", val = val,
            lower = val * (1 - FACTOR_UI_PROPORCION), upper = val * (1 + FACTOR_UI_PROPORCION), ui_level = 0.95)]
  dt[]
}
chs <- data.table::rbindlist(lapply(CAUSAS$cause_id, function(id) {
  s <- severidad[cause_id == id]
  fila_split("cause_health_state", id, "health_state_id", s$health_state_id, s$healthstate_name, s$proportion)
}))
cuota_padre <- casos$cuota[match(SECUELAS$cause_id, casos$cause_id)]
csq <- rbind(fila_split("cause_sequela", 9100L, "sequela_id", SECUELAS$sequela_id, SECUELAS$sequela_name,
                        SECUELAS$proportion * cuota_padre),
             data.table::rbindlist(lapply(SUBTIPOS$cause_id, function(id) {
               s <- SECUELAS[cause_id == id]
               fila_split("cause_sequela", id, "sequela_id", s$sequela_id, s$sequela_name, s$proportion)
             })))
escribir_csv(chs, "particion", RUN_SPLIT, "cause_health_state", "proportion", paste0(RUN_SPLIT, ".csv"))
escribir_csv(csq, "particion", RUN_SPLIT, "cause_sequela", "proportion", paste0(RUN_SPLIT, ".csv"))
escribir_yaml(list(schema = "estimates/v1", run_id = RUN_SPLIT, method = "severity_split", source = "gbd",
                   round = "2023", entities = list("cause_health_state", "cause_sequela"),
                   causa = list(cause_id = 9100L, hijas = as.list(SUBTIPOS$cause_id)),
                   ubicacion = LOC_NACIONAL, nota = PROC),
              "particion", RUN_SPLIT, "manifest.yaml",
              cabecera = paste("# Partici\u00f3n de severidad sint\u00e9tica: proporci\u00f3n de la prevalencia",
                               "por estado de salud y por secuela."))

# ---- Catálogos ----------------------------------------------------------------------------------------------------
escribir_csv(data.table::data.table(
  cause_id = c(294L, 409L, 491L, CAUSAS$cause_id),
  cause_name = c("all_causes", "non_communicable_diseases", "cardiovascular_diseases", CAUSAS$slug),
  level = c(0L, 1L, 2L, CAUSAS$nivel), parent_id = c(NA, 294L, 409L, CAUSAS$padre),
  reportable = c(0L, 0L, 0L, 1L, 1L, 1L, 1L), yll_only = 0L), "catalogos", "catalogo_causas_gbd2023.csv")
LINEAS_DEMOGRAFICOS <- c(
  "tabla,id,name,slug,age_start,age_end",
  "age_group,2,0-6 days,0_6_days,0,0.0191780821917808",
  "age_group,3,7-27 days,7_27_days,0.0191780821917808,0.0767123287671233",
  "age_group,388,1-5 months,1_5_months,0.0767123287671233,0.5",
  "age_group,389,6-11 months,6_11_months,0.5,1",
  "age_group,238,12-23 months,12_23_months,1,2",
  "age_group,34,2-4 years,2_4_years,2,5",
  sprintf("age_group,%d,%d-%d years,%d_%d_years,%d,%d", 6:20, seq(5, 75, by = 5), seq(9, 79, by = 5),
          seq(5, 75, by = 5), seq(9, 79, by = 5), seq(5, 75, by = 5), seq(10, 80, by = 5)),
  "age_group,30,80-84 years,80_84_years,80,85",
  "age_group,31,85-89 years,85_89_years,85,90",
  "age_group,32,90-94 years,90_94_years,90,95",
  "age_group,235,95+ years,95_years,95,125",
  "age_group,22,All ages,all_ages,0,125",
  "age_group,27,Age-standardized,age_standardized,0,125",
  "age_group,28,<1 year,1_year,0,1",
  "age_group,42,Neonatal,neonatal,0,0.0767123287671233",
  "age_group,4,Post Neonatal,post_neonatal,0.0767123287671233,1",
  "age_group,5,1-4 years,1_4_years,1,5",
  "age_group,1,<5 years,5_years,0,5",
  "age_group,23,5-14 years,5_14_years,5,15",
  "age_group,24,15-49 years,15_49_years,15,50",
  "age_group,25,50-69 years,50_69_years,50,70",
  "age_group,26,70+ years,70_years,70,125",
  "age_group,21,80+ years,80_years,80,125",
  "age_group,39,0-14 years,0_14_years,0,15",
  "sex,1,Male,male,,", "sex,2,Female,female,,", "sex,3,Both,both,,",
  "measure,1,Deaths,death,,", "measure,2,DALYs (Disability-Adjusted Life Years),daly,,",
  "measure,3,YLDs (Years Lived with Disability),yld,,", "measure,4,YLLs (Years of Life Lost),yll,,",
  "measure,5,Prevalence,prevalence,,", "measure,6,Incidence,incidence,,", "measure,900002,Population,population,,",
  "metric,1,Number,number,,", "metric,2,Percent,percent,,", "metric,3,Rate,rate,,")
escribir_texto(LINEAS_DEMOGRAFICOS, "catalogos", "catalogo_demograficos_gbd2023.csv")
escribir_csv(data.table::data.table(
  healthstate_id = ESTADOS$health_state_id, healthstate_name = ESTADOS$healthstate_name,
  healthstate_name_pretty = c("Asintom\u00e1tico", "Arteriopat\u00eda sintom\u00e1tica leve",
                              "Arteriopat\u00eda sintom\u00e1tica moderada"),
  # Descripciones genéricas: valen para los tres subtipos (extremidades, carótida y riñón).
  lay_description = c("no tiene s\u00edntomas.",
                      paste("tiene s\u00edntomas leves por falta de irrigaci\u00f3n de una extremidad o un",
                            "\u00f3rgano; no le impiden sus actividades diarias."),
                      paste("tiene s\u00edntomas moderados por falta de irrigaci\u00f3n de una extremidad o un",
                            "\u00f3rgano; tiene alguna dificultad para sus actividades diarias.")),
  dw_mean = ESTADOS$dw, dw_lower = ESTADOS$dw_lower, dw_upper = ESTADOS$dw_upper),
  "catalogos", "catalogo_health_states_gbd2023.csv")
escribir_csv(SECUELAS[, list(cause_id, cause_name = CAUSAS$slug[match(cause_id, CAUSAS$cause_id)], sequela_id,
                             sequela_name, me_id, healthstate_id = health_state_id, healthstate_name)],
             "catalogos", "catalogo_sequelas_gbd2023.csv")
# Ubicaciones: el país y sus 25 departamentos (los niveles que usa el paquete); parent_id y ubigeo como texto con
# ceros a la izquierda, como location_id.
escribir_csv(data.table::data.table(
  location_id = c(LOC_NACIONAL, DEPARTAMENTOS$location_id),
  location_name = c(NOMBRE_NACIONAL, DEPARTAMENTOS$location_name),
  location_level = c(0L, rep(1L, 25L)),
  parent_id = c(NA_character_, rep(LOC_NACIONAL, 25L)),
  ubigeo = c(NA_character_, DEPARTAMENTOS$location_id),
  gbd_location_id = c(123L, rep(NA_integer_, 25L))), "catalogos", "catalogo_locations_peru.csv")

# ---- Registro -----------------------------------------------------------------------------------------------------
escribir_csv(data.table::data.table(cause_id = 9100L, nombre_es = CAUSAS$nombre[1],
                                    hijos = paste(SUBTIPOS$cause_id, collapse = "|")),
             "registro", "master_gbd.csv")
escribir_csv(CAUSAS[nivel == 4L, list(cause_id, nombre_es = nombre)], "registro", "causas_nivel4_es.csv")
# Etiquetas en español de las medidas, métricas y grupos de edad (las de inst/referencia) y, en el formato completo,
# de las causas.
LINEAS_ETIQUETAS <- c(
  "tabla,id,name,name_es,slug,slug_es",
  "measure,5,Prevalence,Prevalencia,prevalence,prevalencia",
  "measure,6,Incidence,Incidencia,incidence,incidencia",
  "measure,3,YLDs (Years Lived with Disability),AVD (a\u00f1os vividos con discapacidad),yld,avd",
  "metric,1,Number,N\u00famero,,", "metric,2,Percent,Proporci\u00f3n,,", "metric,3,Rate,Tasa por 100 000,,",
  "age_group,1,<5 years,Menores de 5 a\u00f1os,,",
  sprintf("age_group,%d,%d-%d years,%d a %d a\u00f1os,,", 6:20, seq(5, 75, by = 5), seq(9, 79, by = 5),
          seq(5, 75, by = 5), seq(9, 79, by = 5)),
  "age_group,21,80+ years,80 a\u00f1os a m\u00e1s,,",
  "age_group,22,All ages,Todas las edades,,")
escribir_texto(c(LINEAS_ETIQUETAS,
                 sprintf("cause,%d,%s,%s,%s,%s", CAUSAS$cause_id, CAUSAS$nombre, CAUSAS$nombre, CAUSAS$slug,
                         CAUSAS$slug)),
               "registro", "etiquetas_es.csv")
# Sin secuelas con deterioro en la ACS: las columnas opcionales rei_id y rei_id_severidad se omiten (una columna
# vacía en todas las filas se leería como lógica y el contrato la rechaza por no ser entera; ver data-raw/LEEME.md).
escribir_csv(SECUELAS[, list(sequela_id, cause_id, sequela_name, health_state_id, rol = "directa")],
             "registro", "sequela_rei.csv")
# Modelos de proporción de los deterioros: la ACS no tiene deterioros, así que el registro va vacío (solo la
# cabecera). dl_insumos() no lo lee; se escribe porque el dl_bundle de v0.2.2 lo exige (ver data-raw/LEEME.md).
escribir_csv(data.table::data.table(rei_id = integer(), proportion_model = character(), cause_id = integer(),
                                    cause_name = character(), fuente = character()),
             "registro", "modelos_proporcion_deterioro.csv")
escribir_texto("datasets: []", "registro", "datasets.yaml")

# ---- Evidencia (formato del almacén GHDx: list.csv a nivel NID, rows.csv con las citas) ---------------------------
escribir_csv(data.table::data.table(
  round = 2023L, component_id = 4L, cause_id = CAUSAS$cause_id, rei_id = NA_integer_, covariate_id = NA_integer_,
  location_id = LOC_NACIONAL, nid = 990001L, title = "Registro vital sint\u00e9tico", type = "Vital registration",
  citation = paste0(NOMBRE_NACIONAL, " - registro vital (", PROC, ")"), private = NA, metadata_count = NA,
  primary_nid = NA, primary_title = NA, primary_type = NA, secondary_nid = NA, secondary_title = NA,
  secondary_type = NA, underlying_nid = NA, underlying_title = NA, underlying_type = NA, ghdx_url = NA,
  acquisition_id = "sintetico_evidencia_v1"), "evidencia", "list.csv")
filas <- data.table::CJ(cause_id = CAUSAS$cause_id, component_id = c(4L, 5L))
escribir_csv(filas[, list(
  round = 2023L, component_id, location_id = data.table::fifelse(component_id == 4L, LOC_NACIONAL, "1"),
  cause_name = nombre_causa(cause_id), cause_id, rei_id = NA,
  citation = data.table::fifelse(component_id == 4L, paste0(NOMBRE_NACIONAL, " - registro vital (", PROC, ")"),
                                 paste0("Cohorte global sint\u00e9tica (", PROC, ")")),
  ghdx_url = NA, provider = NA, provider_url = NA, publication_status = NA, data_collection_method = NA,
  year_start = NA, year_end = NA, sex = NA, age_start = NA, age_end = NA, age_type = NA, representativeness = NA,
  urbanicity_type = NA, population_representativeness_covariates = NA, sample_size = NA, sample_size_unit = NA,
  standard_error = NA, subcomponent = NA, acquisition_id = "sintetico_evidencia_v1")], "evidencia", "rows.csv")

# ---- Extracción (covariables y betas del ancla) --------------------------------------------------------------------
DECIMALES_BETA <- c(sev = 2L, ldi = 2L, haq = 3L)              # decimales impresos de cada beta
beta_yaml <- function(covariate_id, nombre_corto, nombre_impreso, parametro, clave) {
  b <- BETA[[clave]]; ic <- BETA_IC[[clave]]
  fmt <- function(x) sprintf("%.*f", DECIMALES_BETA[[clave]], x)
  campos <- list(
    covariate_id = as.character(covariate_id), covariate_name_short = nombre_corto, nombre_impreso = nombre_impreso,
    nivel = "pais", rol = "predictiva", componente_modelo = "DisMod-MR", modelo_variante = NULL,
    parametro = parametro, tabla_impresa = "Tabla sint\u00e9tica de covariables",
    beta_impreso = sprintf("%s (%s a %s)", fmt(b), fmt(ic[1]), fmt(ic[2])),
    beta_valor = fmt(b), beta_inferior = fmt(ic[1]), beta_superior = fmt(ic[2]),
    exponenciado_impreso = sprintf("%.2f (%.2f a %.2f)", exp(b), exp(ic[1]), exp(ic[2])),
    exponenciado_valor = sprintf("%.2f", exp(b)), exponenciado_inferior = sprintf("%.2f", exp(ic[1])),
    exponenciado_superior = sprintf("%.2f", exp(ic[2])))
  campos$procedencia <- stats::setNames(lapply(names(campos), function(nm) list(tag = "app_convention", origen = PROC)),
                                        names(campos))
  campos
}
escribir_yaml(list(
  meta = list(causa_gbd = list(cause_id = 9100L, cause_name = CAUSAS$nombre[1]), fuente = PROC),
  covariables_gbd = list(
    beta_yaml(785L, "SEV_scalar_agestd_cvd_pvd", "Escalar sint\u00e9tico de riesgo vascular (log-transformado)",
              "Prevalence", "sev"),
    beta_yaml(57L, "LDI_pc", "Ingreso sint\u00e9tico per c\u00e1pita (log-transformado)", "Prevalence", "ldi"),
    beta_yaml(1099L, "haqi", "\u00cdndice sint\u00e9tico de acceso y calidad de la atenci\u00f3n",
              "Excess mortality rate", "haq"))),
  "extraccion.yaml",
  cabecera = c("# Covariables del ancla y sus betas (datos sint\u00e9ticos de ejemplo), en el formato de",
               "# una extracci\u00f3n de los ap\u00e9ndices del GBD. Lo usan las cuatro causas (los subtipos",
               "# declaran extraction.cause_id: 9100)."))

# ---- Configuraciones ---------------------------------------------------------------------------------------------
# Techo de la EMR: unas 3 veces la EMR máxima de la verdad (para el padre, la EMR efectiva sum p_k f_k / sum p_k).
emr_max <- verdad[location_id == LOC_NACIONAL, list(f = max(f)), by = cause_id]
techo <- function(id) ceiling(3 * emr_max$f[emr_max$cause_id == id] * 100) / 100
config_yaml <- function(id) {
  sub <- id != 9100L
  c(sprintf("# %s (causa %d): configuraci\u00f3n de ejemplo con datos sint\u00e9ticos.", nombre_causa(id), id),
    "schema: dismod_lite/v1",
    sprintf("cause_id: %d", id),
    "years:",
    "  ajuste: 2023",
    "sexos: [1, 2]",
    "edad_inicio: 30",
    sprintf("edad_inicio_fuente: %s", PROC),
    "remision:",
    "  valor: 0",
    sprintf("  fuente: %s", PROC),
    "emr_prior:",
    "  tipo: informativo_edad",
    sprintf("  cota: [0, %s]", format(techo(id), nsmall = 2)),
    sprintf("  fuente_cota: %s", PROC),
    "nudos_incidencia: [30, 40, 50, 60, 70, 80, 95]",
    "sigma_suavidad: 0.5",
    "offset_lognormal: ~",
    "anchor:",
    "  location: peru",
    "  lambda: 1.0",
    "  rho_edad: 0.5",
    "  medidas: prevalence",
    "  evidencia_ghdx: sintetico_evidencia_v1",
    "medidas_entrada: []",
    "fuente_admin_principal: ~",
    if (sub) c("extraction:", "  cause_id: 9100",
               "  motivo: subtipo de la ACS; comparte covariables con la causa padre"),
    "cascada:",
    "  kappa: 1.0",
    "  escala: natural",
    "  cota_warning: 0.5",
    "  heldout_anio:",
    "    valor: 2019",
    sprintf("    procedencia: %s", PROC),
    "transformaciones:",
    "  - covariate_name_short: SEV_scalar_agestd_cvd_pvd",
    "    transformacion: log",
    sprintf("    procedencia: %s", PROC),
    "  - covariate_name_short: LDI_pc",
    "    transformacion: log",
    "    token_exento: true",
    sprintf("    procedencia: %s", PROC),
    "  - covariate_name_short: haqi",
    "    transformacion: lineal",
    "    escala: 1",
    sprintf("    escala_procedencia: %s", PROC),
    sprintf("    procedencia: %s", PROC),
    "covariables:",
    unlist(lapply(seq_len(nrow(PROXIES)), function(j) c(
      sprintf("  - covariate_name_short: %s", PROXIES$covariate_name_short[j]),
      "    proxy:",
      sprintf("      covariate_id_proxy: %d", PROXIES$covariate_id_proxy[j]),
      sprintf("      justificacion: %s", PROC)))),
    "severidad:",
    "  fuente: tabla",
    sprintf("  procedencia: %s", PROC),
    "sensibilidad:",
    "  lambda: [0.1, 0.5, 1.0]",
    "  rho: [0.0, 0.5, 0.9]",
    "  kappa: [0.5, 1.0]",
    if (!sub) c("suma:", "  omitidas: []"),
    "decisiones:",
    "  - Datos sint\u00e9ticos de ejemplo (dismodlite).")
}
for (id in CAUSAS$cause_id) escribir_texto(config_yaml(id), "config", sprintf("%d.yaml", id))

# ---- LEEME ----------------------------------------------------------------------------------------------------------
INTRO_LEEME <- c(
  "**Todos los datos son sintéticos.** La enfermedad es ficticia: la *arteriopatía crónica",
  "sintética* (ACS, causa 9100) con tres subtipos: 9101 miembros inferiores, 9102 carotídea y 9103",
  "renovascular. La geografía es la real del Perú (nacional 123 y 25 departamentos, ubigeo 01-25);",
  "ningún número proviene de datos reales. Los genera `data-raw/generar_acs_peru.R` con una semilla fija.")
escribir_texto(c(
  "# acs_peru_completo: datos de ejemplo de dismodlite en el formato completo",
  "",
  INTRO_LEEME,
  "",
  "Es el mismo proyecto que `acs_peru` (el formato simple), con los mismos números, en el formato completo: el",
  "que usan las guías avanzadas y el arnés de compatibilidad con la versión 0.2.2.",
  "",
  "- `config/`: configuración de cada causa (`9100.yaml` ... `9103.yaml`); los subtipos usan la",
  "  extracción de 9100.",
  "- `ancla/`: estimaciones de referencia (prevalencia, incidencia, mortalidad y AVD) por edad y sexo, 2019 y 2023.",
  "- `covariables/`: SEV, LDI y HAQ inventados para Perú (123), Global (1) y la región (120), formato GHDx;",
  "  `extraccion.yaml`: sus betas.",
  "- `proxies_departamentales.csv`: proxies de 2019, 2023 y 2024 (el del SEV varía por banda de edad); sus",
  "  diferencias entre departamentos siguen un índice sintético y no describen a los departamentos reales.",
  "- `poblacion.csv` y `pesos_80mas.csv`: población nacional y departamental (2019, 2023, 2024) y pesos de 80+.",
  "- `datos.csv` (solo la causa 9100): mortalidad nacional, estudio de prevalencia, cohorte de incidencia y un valor",
  "  atípico (2023); mortalidad y prevalencia departamentales de 2019 que no entran al ajuste: sirven para",
  "  validar.",
  "- `severidad/<causa>.csv`: proporciones por estado de salud; `particion/`: una corrida de partición",
  "  (proporción por estado de salud de las cuatro causas y por secuela).",
  "- `evidencia/`, `registro/` y `catalogos/`: fuentes citadas por el ancla, registros (nombres, subtipos y",
  "  secuelas; sin deterioros) y catálogos (Perú y sus 25 departamentos).",
  "",
  "Las curvas verdaderas (`verdad.csv`) están en `acs_peru`."),
  "LEEME.md")

# ---- Tablas de referencia del paquete (inst/referencia) ------------------------------------------------------------
# Los grupos de edad, sexos, medidas y métricas de GBD y sus etiquetas en español: las que usa el formato simple
# cuando el proyecto no trae catálogos. Son las mismas del catálogo del formato completo (sin las causas). Desde aquí,
# ruta() escribe en inst/referencia.
salida <- referencia
unlink(salida, recursive = TRUE)
escribir_texto(LINEAS_DEMOGRAFICOS, "catalogo_demograficos_gbd2023.csv")
# La banda 80+ dice «80 años y más»; el formato completo conserva la etiqueta de la versión 0.2.2 («80 años a más»),
# que llevan las salidas de sus referencias.
escribir_texto(c(sub("80 a\u00f1os a m\u00e1s", "80 a\u00f1os y m\u00e1s", LINEAS_ETIQUETAS, fixed = TRUE),
                 "measure,1,Deaths,Muertes,death,muertes", "sex,1,Male,Hombres,male,hombres",
                 "sex,2,Female,Mujeres,female,mujeres", "sex,3,Both,Ambos sexos,both,ambos"),
               "etiquetas_es.csv")

# ---- Formato simple (inst/extdata/acs_peru) ------------------------------------------------------------------------
# Los mismos valores, como los tendría quien empieza un proyecto: las descargas tal cual y tablas planas. Desde aquí,
# ruta() escribe en la carpeta del formato simple (antes, en la de las tablas de referencia).
salida <- salida_simple
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# Configuración simple de cada causa: la misma configuración que la completa, con las claves del formato simple (lo
# que coincide con el valor por defecto no se escribe).
BETAS_TEXTO <- lapply(c(sev = "sev", ldi = "ldi", haq = "haq"), function(k)
  sprintf("[%s]", paste(sprintf("%.*f", DECIMALES_BETA[[k]], c(BETA[[k]], BETA_IC[[k]])), collapse = ", ")))
config_simple <- function(id) {
  sub <- id != 9100L
  c(sprintf("# %s (causa %d): configuración de ejemplo (formato simple) con datos sintéticos.",
            nombre_causa(id), id),
    "# Las claves que no aparecen toman su valor por defecto: ver ?dl_configuracion y print(dl_proyecto(...)).",
    sprintf("causa: %d", id),
    sprintf("nombre: %s", nombre_causa(id)),
    "anio: 2023",
    "edad_inicio: 30",
    "ubicacion_nacional: 123",
    "",
    "mortalidad_exceso:",
    sprintf("  techo: %s                  # por persona-año", format(techo(id), nsmall = 2)),
    "",
    "covariables:",
    "  - nombre: SEV_scalar_agestd_cvd_pvd",
    "    efecto_sobre: prevalencia",
    "    transformacion: log",
    sprintf("    beta: %s", BETAS_TEXTO$sev),
    "  - nombre: LDI_pc",
    "    efecto_sobre: prevalencia",
    "    transformacion: log",
    sprintf("    beta: %s", BETAS_TEXTO$ldi),
    "  - nombre: haqi",
    "    efecto_sobre: mortalidad_exceso",
    "    transformacion: lineal",
    sprintf("    beta: %s", BETAS_TEXTO$haq),
    "",
    "subnacional:",
    "  anio_validacion: 2019          # año de los datos subnacionales reservados para validar",
    "",
    "sensibilidad:",
    "  peso: [0.1, 0.5, 1.0]",
    if (!sub) c("", sprintf("subtipos: [%s]", paste(SUBTIPOS$cause_id, collapse = ", "))),
    "",
    "notas:",
    "  - Datos sintéticos de ejemplo (dismodlite).")
}
for (id in CAUSAS$cause_id) escribir_texto(config_simple(id), "config", sprintf("%d.yaml", id))

# ancla/: una descarga de GBD Results con «ID y nombre» (sus columnas, en su orden) con las cuatro medidas. El nombre
# del archivo es el identificador de la adquisición del formato completo.
escribir_csv(ancla_csv[, list(measure_id, measure_name, location_id, location_name, sex_id, sex_name,
                              age_id = age_group_id, age_name = age_group_name, cause_id, cause_name, metric_id,
                              metric_name, year, val, upper, lower)],
             "ancla", "sintetico_acs_v1.csv")

# covariables/: las mismas descargas del GHDx, con la columna covariate_id que traen las descargas reales.
ID_COVARIABLE <- c(SEV_scalar_agestd_cvd_pvd = 785L, LDI_pc = 57L, haqi = 1099L)
for (a in unique(COV_NAC$archivo))
  escribir_csv(COV_NAC[archivo == a, list(covariate_id = ID_COVARIABLE[covariate_name_short], covariate_name_short,
                                          location_id, location_name, year_id, age_group_id, age_group_name, sex_id,
                                          sex, mean_value, lower_value, upper_value)],
               "covariables", a)

# poblacion.csv: solo los departamentos (las filas nacionales son su suma y las calcula el paquete), por grupo de
# edad quinquenal (sin la banda agregada 80+, que también se calcula), con el nombre de cada departamento.
pob_s <- poblacion[location_level == 1L & age_group_id %in% BANDAS_POB$age_group_id]
pob_s <- merge(pob_s, BANDAS_POB, by = "age_group_id")
pob_s[, orden_banda := match(age_group_id, BANDAS_POB$age_group_id)]
data.table::setorder(pob_s, location_id, year, sex_id, orden_banda)
escribir_csv(pob_s[, list(location_id,
                          location_name = DEPARTAMENTOS$location_name[match(location_id, DEPARTAMENTOS$location_id)],
                          anio = year, sexo = c("hombres", "mujeres")[sex_id], edad_inicio = age_start,
                          edad_fin = age_end, poblacion = val)],
             "poblacion.csv")

# proxies.csv: el valor de cada covariable por departamento (ya cierra en el valor nacional) y su error estándar;
# edades vacías = todas las edades.
bandas_px <- rbind(BANDAS_ANCLA[, list(age_group_id, inicio, fin)],
                   data.table::data.table(age_group_id = 21L, inicio = 80, fin = 125))
px_s <- merge(proxies, PROXIES[, list(clave, covariable = covariate_name_short)], by = "clave")
px_s <- merge(px_s, bandas_px, by = "age_group_id", all.x = TRUE)
data.table::setorder(px_s, covariate_id_proxy, year, sex_id, age_group_id, location_id)
escribir_csv(px_s[, list(covariable, location_id, anio = year,
                         sexo = c("hombres", "mujeres", "ambos")[sex_id], edad_inicio = inicio, edad_fin = fin,
                         valor = valor_calibrado, error_estandar = valor_calibrado_se)],
             "proxies.csv")

# datos.csv: los mismos datos locales, con el vocabulario del formato simple.
FUENTES <- c(sintetico_rv_v1 = "Registro vital sintético", sintetico_estudio_v1 = "Estudio de prevalencia sintético",
             sintetico_cohorte_v1 = "Cohorte de incidencia sintética",
             sintetico_encuesta_v1 = "Encuesta departamental sintética",
             sintetico_rv_dep_v1 = "Registro vital departamental sintético")
TIPO_SIMPLE <- c(prev_estudio = "prevalencia_estudio", incidencia = "incidencia", csmr = "mortalidad")
d_s <- data.table::rbindlist(datos)
escribir_csv(d_s[, list(causa = cause_id, tipo = TIPO_SIMPLE[tipo_dato], location_id,
                        sexo = c("hombres", "mujeres")[sex_id], edad_inicio = age_start, edad_fin = age_end,
                        anio = year_start, valor = val, error_estandar = se, casos = x, muestra = n,
                        excluir = outlier, motivo = outlier_motivo, fuente = FUENTES[acquisition_id], definicion)],
             "datos.csv")

# severidad.csv: las proporciones de las cuatro causas (se usan las de la causa de cada configuración).
NOMBRE_ESTADO <- c(`799` = "Asintomático", `9801` = "Arteriopatía sintomática leve",
                   `9802` = "Arteriopatía sintomática moderada")
escribir_csv(severidad[, list(causa = cause_id, estado = NOMBRE_ESTADO[as.character(health_state_id)],
                              id_estado = health_state_id, proporcion = proportion, proporcion_inferior = prop_lower,
                              proporcion_superior = prop_upper, peso_discapacidad = dw, peso_inferior = dw_lower,
                              peso_superior = dw_upper)],
             "severidad.csv")

escribir_csv(verdad_csv[, list(cause_id, location_id, sex_id, anio, edad, p, i, f)], "verdad.csv")

escribir_texto(c(
  "# acs_peru: datos de ejemplo de dismodlite (formato simple)",
  "",
  INTRO_LEEME,
  "",
  "Es un proyecto en el formato simple: cópialo para empezar el tuyo. Se lee con",
  "`dl_proyecto(dl_ejemplo(), causa = 9100)`; ver `?dl_proyecto` (los archivos y sus columnas) y",
  "`?dl_configuracion` (las claves de la configuración).",
  "",
  "- `config/`: la configuración de cada causa (`9100.yaml` ... `9103.yaml`); 9100 es la suma de sus subtipos.",
  "- `ancla/`: la estimación de referencia, una descarga de GBD Results con «ID y nombre»: prevalencia,",
  "  incidencia, mortalidad y AVD por edad y sexo de las cuatro causas, 2019 y 2023.",
  "- `covariables/`: descargas del GHDx de SEV, LDI y HAQ (valores inventados) para Perú (123), Global (1) y la",
  "  región (120).",
  "- `poblacion.csv`: población de los 25 departamentos (2019, 2023 y 2024) por sexo y grupo de edad; la",
  "  nacional es su suma y la calcula el paquete.",
  "- `proxies.csv`: las tres covariables por departamento (2019, 2023 y 2024; el SEV por grupo de edad); su",
  "  promedio ponderado por la población es el valor nacional. Siguen un índice sintético y no",
  "  describen a los departamentos reales.",
  "- `datos.csv` (solo la causa 9100): mortalidad nacional, un estudio de prevalencia, una cohorte de incidencia y",
  "  un valor atípico (2023); mortalidad y prevalencia departamentales de 2019 que sirven para validar.",
  "- `severidad.csv`: proporciones y pesos de discapacidad por estado de salud de cada causa.",
  "- `verdad.csv`: curvas verdaderas p, i y f por edad, nacionales y departamentales, de 2019 y 2023 (no es un",
  "  insumo del modelo).",
  "",
  "El mismo proyecto en el formato completo está en `acs_peru_completo`."),
  "LEEME.md")

invisible(TRUE)
