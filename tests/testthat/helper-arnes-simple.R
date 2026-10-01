# Arnés de compatibilidad (ver helper-arnes-api.R): escenarios S1-S5, la compuerta de equivalencia del formato simple.
#
# Cada escenario S corre el proyecto de ejemplo en el formato simple (dl_ejemplo(), o una copia con una variante) por
# la API nueva: dl_proyecto() lee la configuración y traduce las tablas al formato completo, dl_insumos() arma los
# insumos, y desde ahí repite los pasos de un escenario E sobre el formato completo:
#   S1 = E1  nacional;
#   S2 = E2  cascada por proxies;
#   S3 = E4  datos locales en el ajuste, declarados con las claves simples (datos_en_ajuste, ancla.peso, avanzado);
#   S4 = E5  subtipos y su suma (cada subtipo toma las betas de la causa que lo declara en `subtipos`);
#   S5 = E8  año 2019, proyección 2024 (ancla.anio y la beta de HAQ con covariables[].escala) y su consolidado.
# Sus resultados llevan los nombres de las piezas del escenario E (E1, E1__comorbilidad, registro-e1, ...) y se
# comparan con la referencia de ese escenario (_referencia/E<n>, generada con la versión 0.2.2 sobre
# acs_peru_completo). Los dos formatos del ejemplo dan insumos idénticos bit a bit, así que los parámetros, las celdas
# y las tablas deben coincidir en modo exacto (DL_ARNES_EXACTO=1). Los manifiestos se comparan sin las claves de
# .MANIFIESTO_FUERA_SIMPLE. Solo corren con api_nueva(): la versión 0.2.2 no tiene dl_proyecto().

# Escenario E con el que se compara cada escenario S.
.EQUIVALENTE_SIMPLE <- c(S1 = "E1", S2 = "E2", S3 = "E4", S4 = "E5", S5 = "E8")

# Claves de los manifiestos que el formato simple cambia sin cambiar ningún número: la procedencia de los insumos y
# cómo se declaró la configuración. Se quitan de los dos lados antes de comparar (comparar_con_referencia(fuera = )).
.MANIFIESTO_FUERA_SIMPLE <- list(
  # hash de los insumos, que incluye sus columnas de procedencia (acquisition_id de la población, fuente y
  # nombre_impreso de las betas, fuente de la severidad, cita de los datos, ...)
  c("inputs", "bundle_hash"),
  # sha256 de cada tabla congelada en inputs/: las mismas columnas de procedencia
  c("inputs", "tablas", "*", "sha256"),
  # suma de subtipos: el hash de los insumos de cada hija, copiado de su manifiesto
  c("causa", "hijas", "*", "bundle_hash"),
  # nombre de la ubicación del ancla: «peru» en el formato completo, su location_id (123) en el simple; que sea la
  # misma ubicación lo comprueba .insumos_simple()
  c("params", "anchor"),
  # bloque que solo escribe el formato simple: su formato y las claves tomadas por defecto
  "configuracion",
  # consolidado: acquisition_id de la población (el nombre de su archivo)
  c("poblacion", "acquisition_id"))

# ---- Proyectos, rutas e insumos en el formato simple ----

# Copia del proyecto simple del ejemplo (ctx$raiz) en variantes/<nombre>/, con la configuración de `causa`
# reescrita por `cambiar` (una función de la configuración leída, una lista, a la nueva). Devuelve la carpeta.
.proyecto_variante <- function(ctx, nombre, causa, cambiar) {
  d <- .carpeta_variante(ctx, nombre)
  .afirmar(all(file.copy(list.files(ctx$raiz, full.names = TRUE), d, recursive = TRUE)),
           "no se copi\u00f3 el proyecto simple")
  archivo <- file.path(d, "config", sprintf("%d.yaml", causa))
  yaml::write_yaml(cambiar(yaml::read_yaml(archivo)), archivo)
  d
}

# Rutas sin datos.csv (`datos = FALSE`) o sin proxies.csv (`proxies = FALSE`): lo que rutas_acs() deja fuera en los
# escenarios E con con_datos y con_proxies.
.rutas_sin <- function(rutas, datos = TRUE, proxies = TRUE) {
  if (!datos) rutas["datos"] <- list(NULL)
  if (!proxies) rutas["cov_proxy"] <- list(NULL)
  rutas
}

# Insumos de `causa` del proyecto simple `carpeta` por la API nueva: la configuración y las rutas de dl_proyecto()
# (sin las piezas que quitan `datos` y `proxies`). Con `cambios` (claves del formato completo, como en los escenarios
# E), la configuración es la de dl_configuracion() sobre el mismo archivo, con esos cambios; las rutas siguen siendo las
# de la traducción del proyecto.
.insumos_simple <- function(ctx, causa, carpeta = ctx$raiz, cambios = NULL, datos = TRUE, proxies = TRUE) {
  api <- ctx$api
  .afirmar(!is.null(api$proyecto), "los escenarios S usan dl_proyecto(): corren solo con api_nueva()")
  p <- api$proyecto(carpeta, causa)
  .afirmar(identical(p$formato, "simple"), "el proyecto ", carpeta, " no est\u00e1 en el formato simple")
  rutas <- .rutas_sin(p$rutas, datos, proxies)
  conf <- if (is.null(cambios)) p$configuracion
          else api$configuracion(causa = causa, carpeta_config = dirname(p$configuracion$origen$archivo),
                                 cambios = cambios)
  # sin cambios, la llamada de quien usa el paquete: dl_insumos(dl_proyecto(carpeta, causa))
  b <- suppressMessages(if (is.null(cambios) && datos && proxies) api$insumos(configuracion = p)
                        else api$insumos(configuracion = conf, rutas = rutas))
  .afirmar(identical(b$cfg$origen$formato, "simple"), "los insumos de ", causa, " no vienen del formato simple")
  # la ubicación del ancla es la de las referencias («peru» es la 123): por eso params.anchor puede quedar fuera de
  # la comparación de los manifiestos
  .afirmar(identical(as.integer(b$cfg$anchor$location_id), 123L), "la ubicaci\u00f3n del ancla de ", causa,
           " no es la 123")
  .afirmar(nrow(b$severidad) > 0L && all(b$severidad$cause_id == b$cfg$cause_id),
           "la tabla de severidad de los insumos de ", causa, " no es de su causa")
  list(insumos = b, rutas = rutas)
}

# ---- Variantes del proyecto simple (las de los escenarios E, en el formato simple) ----

# S3 (E4): la configuración de 9100 con los cambios de .CAMBIOS_E4 escritos con las claves simples (datos_en_ajuste,
# ancla.peso, notas y, sin clave simple, offset_lognormal y el máximo del error del ancla en `avanzado`) y datos.csv
# con los cambios de .datos_e4(): la cohorte de incidencia nacional en conteos (casos en `muestra` personas-año, sin
# valor ni error estándar) y los estudios débiles de .ARNES, copiados del estudio de prevalencia nacional de mujeres de
# 60-64 años.
.proyecto_e4_simple <- function(ctx) {
  e4 <- .CAMBIOS_E4
  d <- .proyecto_variante(ctx, "e4_simple", 9100L, function(s) {
    s$datos_en_ajuste <- c("prevalencia_estudio", "incidencia", "mortalidad")   # prev_estudio, incidencia, csmr
    s$ancla <- list(peso = e4$anchor$lambda)
    s$notas <- e4$decisiones
    s$avanzado <- list(offset_lognormal = e4$offset_lognormal,
                       anchor = list(gate_err_mediano = e4$anchor$gate_err_mediano))
    s
  })
  archivo <- file.path(d, "datos.csv")
  x <- .csv_leer_texto(archivo)
  nacional <- x$location_id == "123"   # la ubicación nacional del ejemplo
  k <- which(x$tipo == "incidencia" & nacional & x$excluir == "FALSE")
  .afirmar(length(k) >= 2L, "variante simple de datos: sin cohorte de incidencia nacional")
  n <- .ARNES$personas_anio_cohorte
  x$casos[k] <- as.character(round(as.numeric(x$valor[k]) * n))
  x$muestra[k] <- as.character(n)
  x$valor[k] <- ""
  x$error_estandar[k] <- ""
  base <- x[x$tipo == "prevalencia_estudio" & nacional & x$sexo == "mujeres" & x$edad_inicio == "60"]
  .afirmar(nrow(base) == 1L, "variante simple de datos: falta el estudio de prevalencia de mujeres de 60-64 a\u00f1os")
  nuevas <- lapply(.ARNES$estudios_debiles, function(e) {
    f <- data.table::copy(base)
    f$sexo <- c("hombres", "mujeres")[e$sex_id]
    f$edad_inicio <- as.character(e$edad_inicio)
    f$edad_fin <- as.character(e$edad_inicio + 5L)
    f$casos <- as.character(e$x)
    f$muestra <- as.character(e$n)
    f
  })
  .csv_escribir_texto(data.table::rbindlist(c(list(x), nuevas)), archivo)
  d
}

# S5 (E8): la proyección 2024 como la declara un proyecto simple: anio 2024 con el ancla de 2023 (ancla.anio) y la
# beta lineal de HAQ en la escala 0-1 (x 100, como .haqi_escala_0a1()) con covariables[].escala 0.01.
.proyecto_e8_simple <- function(ctx)
  .proyecto_variante(ctx, "e8_proyeccion_simple", 9100L, function(s) {
    s$anio <- 2024L
    s$ancla <- list(anio = 2023L)
    k <- which(vapply(s$covariables, function(cv) identical(cv$nombre, "haqi"), NA))
    .afirmar(length(k) == 1L, "variante simple de 9100: sin la covariable haqi")
    s$covariables[[k]]$escala <- 0.01
    s$covariables[[k]]$beta <- c(-1.2, -1.8, -0.6)
    s
  })

# ---- Escenarios ----

.ESCENARIOS_SIMPLE <- list(
  S1 = function(ctx, acum) {
    reg <- .registro(ctx, "e1")
    x <- .corrida(ctx, 9100L, nombre = "e1-nacional", registro = reg,
                  ins = .insumos_simple(ctx, 9100L, datos = FALSE, proxies = FALSE))
    .afirmar(nrow(x$insumos$datos) == 0L && nrow(x$insumos$cov_proxy) == 0L, "S1 trae datos o proxies")
    .anotar_ajuste(acum, "E1", 9100L, x$ajuste)
    .anotar_corrida(acum, "E1", x$corrida$dir)
    .anotar_tabla(acum, "E1__comorbilidad", x$comorbilidad)
    .anotar_registro(acum, "e1", reg)
  },

  S2 = function(ctx, acum) {
    reg <- .registro(ctx, "e2")
    x <- .corrida(ctx, 9100L, nombre = "e2-proxies", registro = reg, cascada = TRUE, kappa = 1,
                  ins = .insumos_simple(ctx, 9100L))
    .afirmar(length(x$cascada$departamentos) == 25L, "S2: la cascada no tiene 25 departamentos")
    .anotar_ajuste(acum, "E2", 9100L, x$ajuste)
    .anotar_corrida(acum, "E2", x$corrida$dir)
    .anotar_registro(acum, "e2", reg)
  },

  S3 = function(ctx, acum) {
    ins <- .insumos_simple(ctx, 9100L, carpeta = .proyecto_e4_simple(ctx), proxies = FALSE)
    cfg <- ins$insumos$cfg
    .afirmar(identical(unlist(cfg$medidas_entrada), .CAMBIOS_E4$medidas_entrada) &&
               identical(cfg$anchor$lambda, .CAMBIOS_E4$anchor$lambda) &&
               identical(as.numeric(cfg$offset_lognormal), .ARNES$offset_lognormal),
             "S3: la configuraci\u00f3n simple no da los cambios de E4")
    x <- .corrida(ctx, 9100L, nombre = "e4-datos-locales", registro = .registro(ctx, "e4"), ins = ins)
    .anotar_ajuste(acum, "E4", 9100L, x$ajuste)
    .anotar_ajuste(acum, "E4-solo-prior", 9100L, x$ajuste_prior)
    .anotar_corrida(acum, "E4", x$corrida$dir)
  },

  S4 = function(ctx, acum) {
    api <- ctx$api
    reg <- .registro(ctx, "e5")
    hijas <- lapply(9101:9103, function(k)
      .corrida(ctx, k, nombre = sprintf("e5-%d", k), registro = reg, cascada = TRUE,
               ins = .insumos_simple(ctx, k, datos = FALSE)))
    .afirmar(all(vapply(hijas, function(h) identical(h$insumos$cfg$extraction$cause_id, 9100L), NA)),
             "S4: los subtipos no toman las betas de su causa padre")
    dirs <- vapply(hijas, function(h) h$corrida$dir, "")
    r <- .rutas_sin(api$proyecto(ctx$raiz, 9100L)$rutas, datos = FALSE, proxies = FALSE)
    suma <- api$sumar_hijas(corridas_hijas = dirs, causa = 9100L, nombre = "e5-suma", carpeta = ctx$carpeta,
                            nivel = .ARNES$nivel, registrar = TRUE, registro = reg, rutas = r)
    omit <- api$sumar_hijas(corridas_hijas = dirs[1:2], causa = 9100L, nombre = "e5-suma-omitidas",
                            carpeta = ctx$carpeta, nivel = .ARNES$nivel, registrar = TRUE,
                            registro = .registro(ctx, "e5-omitidas"), rutas = r,
                            omitidas = list(list(cause_id = 9103L,
                                                 motivo = "variante del ejemplo sin el subtipo renovascular")))
    for (i in seq_along(hijas)) {
      k <- 9100L + i
      .anotar_ajuste(acum, paste0("E5-", k), k, hijas[[i]]$ajuste)
      .anotar_corrida(acum, paste0("E5-", k), dirs[i])
    }
    .anotar_corrida(acum, "E5-suma", suma$dir)
    .anotar_corrida(acum, "E5-suma-omitidas", omit$dir)
    .anotar_registro(acum, "e5", reg)
    .anotar_registro(acum, "e5-omitidas", .registro(ctx, "e5-omitidas"))
  },

  # 2019 con el año en `cambios`, como E8: un proyecto cuya configuración dijera anio 2019 tomaría los pesos de 80+
  # de la población de 2019, y el ejemplo completo usa los de 2023 en todos los años (pesos_80mas.csv).
  S5 = function(ctx, acum) {
    reg <- .registro(ctx, "e8")
    x19 <- .corrida(ctx, 9100L, nombre = "e8-2019", registro = reg,
                    ins = .insumos_simple(ctx, 9100L, cambios = list(years = list(ajuste = 2019L)), datos = FALSE,
                                          proxies = FALSE))
    v24 <- .proyecto_e8_simple(ctx)
    x24 <- .corrida(ctx, 9100L, nombre = "e8-proyeccion-2024", registro = reg, cascada = TRUE,
                    ins = .insumos_simple(ctx, 9100L, carpeta = v24, datos = FALSE))
    m24 <- x24$corrida$manifest$params
    .afirmar(m24$year == 2024L && m24$anio_ancla == 2023L, "S5: la proyecci\u00f3n no declara el ancla de 2023")
    bh <- x24$insumos$betas[tolower(x24$insumos$betas$covariate_name_short) == "haqi"]
    .afirmar(nrow(bh) == 1L && isTRUE(all.equal(bh$escala, 0.01)) && isTRUE(all.equal(bh$beta, -1.2)),
             "S5: la beta de HAQ no qued\u00f3 en 0-1 con escala 0.01")
    .anotar_ajuste(acum, "E8-2019", 9100L, x19$ajuste)
    .anotar_ajuste(acum, "E8-2024", 9100L, x24$ajuste)
    .anotar_corrida(acum, "E8-2019", x19$corrida$dir)
    .anotar_corrida(acum, "E8-2024", x24$corrida$dir)
    # consolidado de 2024, con la carpeta registro/ de la traducción del proyecto y 9100 sin hijas
    reg_sin <- .registro_sin_suma(ctx, origen = x24$rutas$registry)
    r24 <- .rutas_sin(x24$rutas, datos = FALSE, proxies = FALSE)
    r24["registry"] <- list(reg_sin)
    n <- .consolidar(ctx, reg, "v1", "e8-consolidado-2024", r24, anios = 2024L, .registro(ctx, "e8-consolidado"),
                     maestro = file.path(reg_sin, "master_gbd.csv"))
    acum$manifiestos[["E8-consolidado-2024"]] <- n$manifiesto
    .anotar_tabla(acum, "E8-consolidado-2024__sumas_canonico", n$sumas, todas_ubicaciones = TRUE)
    .anotar_registro(acum, "e8", reg)
  }
)
