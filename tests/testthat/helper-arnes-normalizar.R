# Arnés de compatibilidad (ver helper-arnes-api.R): lectura y normalización de lo que escribe un escenario
# (corridas, consolidados, manifiestos y registros) y el acumulador de sus resultados.

# Columnas de texto libre que la versión 1.0.0 reescribe (no se comparan). `fuente` es prosa en unas tablas (el
# factor de comorbilidad) y un valor cerrado en otras (la tabla de severidad: tabla o mod): se compara solo en el
# segundo caso.
.COLUMNAS_PROSA <- c("nota")
.FUENTES_CERRADAS <- c("tabla", "mod")

# Columnas que se leen como texto aunque parezcan números (ubigeos con cero inicial, identificadores de corrida).
.COLUMNAS_TEXTO <- c("location_id", "ubigeo", "round", "run_id", "run_fuente", "run_origen")

# Claves del manifiesto de una corrida que no se comparan. Las dos corridas de cada escenario con la versión 0.2.2
# (data-raw/referencia_legado.R, con los datos y la carpeta de salida en rutas distintas) no difieren en ninguna
# otra clave; estas cambian por diseño entre versiones o entre días:
.MANIFIESTO_FUERA <- list(
  "generado",                      # fecha de la corrida
  c("params", "version_paquete"),  # versión del paquete
  "limitaciones",                  # prosa que la versión 1.0.0 reescribe: se compara solo cuántas son
  c("files", "*", "sha256")        # el CSV lleva la columna run_id, que empieza con la fecha
)
# Además, en todo texto la fecha de los identificadores de corrida (AAAA-MM-DD_<nombre>_v<n>) se cambia por FECHA_.

# Del manifiesto de un consolidado solo se comparan estas claves (y de files, la medida y las filas, más los nombres
# de las tablas del perfil): el resto (método, perfil, rutas, correspondencias, prosa) cambia de nombre por diseño en
# la versión 1.0.0.
.CONSOLIDADO_CLAVES <- c("schema", "source", "round", "entities", "poblacion", "params", "bloques", "huecos")

# Nombre de archivo de una tabla de perfil -> medida (perfil_v1 usa el slug en inglés y perfil_v2 en español).
.SLUG_MEDIDA <- c(prevalence = "prevalence", prevalencia = "prevalence", incidence = "incidence",
                  incidencia = "incidence", yld = "yld", avd = "yld")

# ---- Normalización ----

.roundtrip_yaml <- function(x) yaml::yaml.load(yaml::as.yaml(x, precision = 15L))

.normalizar_fechas <- function(x) {
  if (is.list(x)) {
    for (i in seq_along(x)) if (!is.null(x[[i]])) x[[i]] <- .normalizar_fechas(x[[i]])
    return(x)
  }
  if (is.character(x)) return(gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}_", "FECHA_", x))
  x
}

# Quita la clave `ruta` (vector de nombres; "*" = cada elemento de una lista sin nombres).
.quitar_clave <- function(x, ruta) {
  if (!is.list(x) || !length(ruta)) return(x)
  k <- ruta[1]
  if (length(ruta) == 1L) {
    if (!identical(k, "*")) x[[k]] <- NULL
    return(x)
  }
  if (identical(k, "*")) {
    for (i in seq_along(x)) x[[i]] <- .quitar_clave(x[[i]], ruta[-1])
  } else if (!is.null(x[[k]])) x[[k]] <- .quitar_clave(x[[k]], ruta[-1])
  x
}

# Manifiesto de una corrida (lista leída con yaml::read_yaml) sin las claves volátiles, con las fechas de los
# identificadores de corrida neutralizadas y pasado una vez por YAML (así su forma es la de la referencia leída).
# De las limitaciones (prosa) queda cuántas son: una que aparece o desaparece es un cambio de comportamiento.
normalizar_manifiesto <- function(m) {
  m$n_limitaciones <- length(m$limitaciones)
  for (ruta in .MANIFIESTO_FUERA) m <- .quitar_clave(m, ruta)
  .roundtrip_yaml(.normalizar_fechas(m))
}

# Texto -> double con redondeo correcto: el strtod de C, a través del lector JSON de jsonlite. Ni as.numeric ni fread
# redondean bien todos los números de 17 cifras (en esta máquina, uno de cada seis queda a 1 ulp), y el modo exacto
# necesita releer las referencias bit a bit. NA, vacío, Inf y NaN se reconocen aparte (no son JSON).
.a_numero <- function(v) {
  v <- as.character(v)
  out <- rep(NA_real_, length(v))
  especial <- c("Inf" = Inf, "-Inf" = -Inf, "NaN" = NaN)
  k <- v %in% names(especial)
  out[k] <- especial[v[k]]
  ok <- !is.na(v) & nzchar(v) & v != "NA" & !k
  if (any(ok)) out[ok] <- jsonlite::parse_json(paste0("[", paste(v[ok], collapse = ","), "]"), simplifyVector = TRUE)
  out
}

# CSV de una corrida o de una referencia: las columnas numéricas con decimales se releen como texto y se convierten
# con .a_numero (la conversión con la que .num_texto() comprueba que lo escrito se relee exacto).
.leer_csv_salida <- function(path) {
  cab <- names(data.table::fread(path, nrows = 0L, encoding = "UTF-8"))
  texto <- intersect(cab, .COLUMNAS_TEXTO)
  dt <- data.table::fread(path, colClasses = if (length(texto)) list(character = texto), encoding = "UTF-8")
  dbl <- names(dt)[vapply(dt, is.double, logical(1))]
  if (length(dbl)) {
    txt <- data.table::fread(path, select = dbl, colClasses = "character", encoding = "UTF-8", na.strings = NULL)
    for (cn in dbl) data.table::set(dt, j = cn, value = .a_numero(txt[[cn]]))
  }
  dt
}

# Tabla lista para comparar: sin columnas de prosa, solo las ubicaciones de referencia (salvo `todas_ubicaciones`),
# fechas neutralizadas.
.normalizar_tabla <- function(dt, todas_ubicaciones = FALSE) {
  dt <- data.table::as.data.table(data.table::copy(dt))
  fuera <- intersect(names(dt), .COLUMNAS_PROSA)
  if ("fuente" %in% names(dt) && !all(is.na(dt$fuente) | dt$fuente %in% .FUENTES_CERRADAS)) fuera <- c(fuera, "fuente")
  if (length(fuera)) dt[, (fuera) := NULL]
  loc <- intersect(c("location_id", "ubigeo"), names(dt))[1]
  if (!is.na(loc) && !todas_ubicaciones) dt <- dt[as.character(dt[[loc]]) %in% .ARNES$ubicaciones]
  for (cn in names(dt)) if (is.character(dt[[cn]])) data.table::set(dt, j = cn, value = .normalizar_fechas(dt[[cn]]))
  data.table::setattr(dt, "sorted", NULL)
  data.table::setindex(dt, NULL)
  dt[]
}

# Sumas de control de una tabla de celdas por las claves `por` (entre ellas la ubicación): suma de val, lower y
# upper sobre las demás claves (edades, causas). Las filas se ordenan antes de sumar, para que el resultado no
# dependa del orden del archivo. Vigilan las ubicaciones que las referencias no guardan celda por celda.
.sumas_por_ubicacion <- function(dt, por) {
  dt <- data.table::as.data.table(dt)
  claves <- intersect(c(por, "cause_id", "age_group_id"), names(dt))
  o <- do.call(order, c(unname(as.list(dt[, claves, with = FALSE])), method = "radix"))
  dt[o, list(val = sum(val), lower = sum(lower), upper = sum(upper)), keyby = por]
}

# Simulaciones guardadas de una corrida (draws/<medida>_<año>.csv.gz), con las columnas draw_* leídas exactas.
.leer_simulaciones <- function(path) {
  con <- gzfile(path, "rt")
  on.exit(close(con))
  w <- data.table::fread(text = readLines(con, encoding = "UTF-8"), colClasses = "character", na.strings = NULL)
  for (cn in c("sex_id", "age_group_id")) data.table::set(w, j = cn, value = as.integer(w[[cn]]))
  for (cn in grep("^draw_", names(w), value = TRUE)) data.table::set(w, j = cn, value = .a_numero(w[[cn]]))
  w
}

# Sumas de control de una corrida sobre todas sus ubicaciones: por medida, ubicación y sexo, la suma sobre las
# edades de val, lower y upper de las celdas (cause/) y de la media y los cuantiles 2.5 % y 97.5 % de cada celda de
# las simulaciones guardadas (sim_*). Las simulaciones se escriben aparte de las celdas (la suma de hijas y el
# re-resumen también las escriben o enlazan) y las usan el re-resumen, la suma y quien las lea.
.sumas_corrida <- function(dir, celdas) {
  por <- c("location_id", "sex_id")
  out <- data.table::rbindlist(lapply(names(celdas), function(md)
    .sumas_por_ubicacion(celdas[[md]], por)[, medida := md]))
  sims <- sort(list.files(file.path(dir, "draws"), pattern = "[.]csv[.]gz$", full.names = TRUE))
  if (length(sims)) {
    si <- data.table::rbindlist(lapply(sims, function(p) {
      w <- .leer_simulaciones(p)
      m <- as.matrix(w[, grep("^draw_", names(w), value = TRUE), with = FALSE])
      q <- apply(m, 1L, stats::quantile, probs = c(0.025, 0.975), names = FALSE)
      x <- data.table::data.table(location_id = w$location_id, sex_id = w$sex_id, age_group_id = w$age_group_id,
                                  val = rowMeans(m), lower = q[1L, ], upper = q[2L, ])
      s <- .sumas_por_ubicacion(x, por)
      data.table::setnames(s, c("val", "lower", "upper"), c("sim_media", "sim_q025", "sim_q975"))
      s[, medida := sub("_[0-9]{4}[.]csv[.]gz$", "", basename(p))]
    }))
    out <- merge(out, si, by = c("medida", por), all = TRUE)
  }
  data.table::setcolorder(out, c("medida", por))
  data.table::setorderv(out, c("medida", por))
  out[]
}

# Estructura de la carpeta de una corrida: rutas relativas de sus archivos, con la fecha neutralizada. Es parte del
# contrato (mod/dismod_lite/<corrida>/) y el re-resumen y la suma de hijas dependen de ella.
.archivos_corrida <- function(dir) sort(.normalizar_fechas(list.files(dir, recursive = TRUE)), method = "radix")

# Consolidado en `dir` (mod/export_cdc/<id>/ de la versión 0.2.2 o mod/consolidado/<id>/ de la 1.0.0) como lista
# neutra: manifiesto (claves .CONSOLIDADO_CLAVES, más los nombres de las tablas del perfil), tablas con nombres
# independientes de carpetas y archivos (canonico_<medida>.csv y tablas_<medida>.csv, de la carpeta entrega/ o
# tablas/ según la versión) y sumas de control de la tabla canónica sobre todas las ubicaciones (por medida,
# ubicación, sexo y métrica: los conteos usan la población de cada departamento).
normalizar_consolidado <- function(dir) {
  m <- yaml::read_yaml(file.path(dir, "manifest.yaml"))
  man <- m[intersect(.CONSOLIDADO_CLAVES, names(m))]
  man$files <- lapply(m$files, function(x) list(medida = basename(dirname(x$path)), rows = x$rows))
  sub <- intersect(c("tablas", "entrega"), list.dirs(dir, recursive = FALSE, full.names = FALSE))
  if (length(sub) != 1L) stop("normalizar_consolidado: se esperaba una carpeta tablas/ o entrega/ en ", dir)
  arch_tablas <- sort(list.files(file.path(dir, sub), pattern = "[.]csv$"), method = "radix")
  man$archivos_tablas <- arch_tablas
  man <- .roundtrip_yaml(.normalizar_fechas(man))
  tablas <- list(); sumas <- list()
  for (p in list.files(file.path(dir, "canonico"), pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)) {
    md <- basename(dirname(p))
    ce <- .leer_csv_salida(p)
    tablas[[sprintf("canonico_%s.csv", md)]] <- .normalizar_tabla(ce)
    sumas[[md]] <- .sumas_por_ubicacion(ce, c("location_id", "sex_id", "metric_id"))[, medida := md]
  }
  for (a in arch_tablas) {
    base <- sub("[.]csv$", "", a)
    slug <- if (base %in% names(.SLUG_MEDIDA)) .SLUG_MEDIDA[[base]] else base
    tablas[[sprintf("tablas_%s.csv", slug)]] <- .normalizar_tabla(.leer_csv_salida(file.path(dir, sub, a)))
  }
  sumas <- data.table::rbindlist(sumas[order(names(sumas))])
  data.table::setcolorder(sumas, "medida")
  list(manifiesto = man, tablas = tablas[order(names(tablas))], sumas = sumas)
}

# ---- Acumulador de resultados de un escenario ----

.acumulador <- function() {
  a <- new.env(parent = emptyenv())
  a$parametros <- list(); a$celdas <- list(); a$manifiestos <- list(); a$crudos <- list(); a$tablas <- list()
  a
}

.anotar_tabla <- function(acum, nombre, dt, todas_ubicaciones = FALSE) {
  if (!grepl("[.]csv$", nombre)) nombre <- paste0(nombre, ".csv")
  acum$tablas[[nombre]] <- .normalizar_tabla(dt, todas_ubicaciones)
}

# Medias posteriores de los parámetros (log i y log f en los nudos) por sexo.
.anotar_ajuste <- function(acum, pieza, causa, ajuste) {
  dp <- ajuste$draws_par
  acum$parametros[[pieza]] <- data.table::rbindlist(lapply(names(dp), function(sx) data.table::data.table(
    escenario = pieza, causa = as.integer(causa), sex_id = as.integer(sx), parametro = colnames(dp[[sx]]),
    media = unname(colMeans(dp[[sx]])))))
}

# Una corrida escrita en disco: celdas (cause/<medida>/<run_id>.csv) de las ubicaciones de referencia, sumas de
# control de todas las ubicaciones (celdas y simulaciones), lista de archivos, manifiesto y, si `tablas`,
# diagnósticos, etiquetas y un resumen de la renormalización de la cascada (la tabla por simulación pesa demasiado).
.anotar_corrida <- function(acum, pieza, dir, tablas = TRUE) {
  run_id <- basename(dir)
  medidas <- sort(list.dirs(file.path(dir, "cause"), recursive = FALSE, full.names = FALSE), method = "radix")
  .afirmar(length(medidas) > 0L, pieza, ": la corrida no tiene cause/")
  todas <- stats::setNames(lapply(medidas, function(md)
    .leer_csv_salida(file.path(dir, "cause", md, paste0(run_id, ".csv")))), medidas)
  acum$celdas[[pieza]] <- data.table::rbindlist(lapply(medidas, function(md) {
    ce <- todas[[md]]
    ce <- ce[ce$location_id %in% .ARNES$ubicaciones]
    data.table::data.table(escenario = pieza, causa = as.integer(ce$cause_id), location_id = ce$location_id,
                           sex_id = as.integer(ce$sex_id), age_group_id = as.integer(ce$age_group_id), medida = md,
                           media = ce$val, inferior = ce$lower, superior = ce$upper)
  }))
  .anotar_tabla(acum, paste0(pieza, "__sumas"), .sumas_corrida(dir, todas), todas_ubicaciones = TRUE)
  man <- yaml::read_yaml(file.path(dir, "manifest.yaml"))
  acum$crudos[[pieza]] <- man
  acum$manifiestos[[pieza]] <- normalizar_manifiesto(man)
  acum$manifiestos[[paste0(pieza, "-archivos")]] <- .roundtrip_yaml(.archivos_corrida(dir))
  if (!tablas) return(invisible())
  diag <- file.path(dir, "diagnostics")
  for (a in sort(setdiff(list.files(diag, pattern = "[.]csv$"), "renorm.csv"), method = "radix"))
    .anotar_tabla(acum, paste0(pieza, "__", a), .leer_csv_salida(file.path(diag, a)))
  if (file.exists(file.path(diag, "renorm.csv"))) {
    rn <- .leer_csv_salida(file.path(diag, "renorm.csv"))
    .anotar_tabla(acum, paste0(pieza, "__renorm_resumen"),
                  rn[, list(minimo = min(factor), media = mean(factor), maximo = max(factor)),
                     by = c("medida", "sex_id", "age_group_id")])
  }
  # etiquetas/<run_id>.csv; en un re-resumen el archivo enlazado conserva el nombre de la corrida de origen
  et <- list.files(file.path(dir, "etiquetas"), pattern = "[.]csv$", full.names = TRUE)
  .afirmar(length(et) <= 1L, pieza, ": m\u00e1s de un archivo de etiquetas")
  if (length(et)) .anotar_tabla(acum, paste0(pieza, "__etiquetas"), .leer_csv_salida(et))
}

# Registro de corridas: identificadores (fecha neutralizada) y filas por partición.
.anotar_registro <- function(acum, clave, path) {
  ds <- yaml::read_yaml(path)$datasets
  acum$manifiestos[[paste0("registro-", clave)]] <- .roundtrip_yaml(lapply(ds, function(d) list(
    run_id = .normalizar_fechas(d$run_id), status = d$status,
    filas = as.integer(vapply(d$partitions, function(p) as.integer(p$rows), 0L)))))
}

.resultado <- function(acum) {
  pegar <- function(x) if (length(x)) data.table::rbindlist(unname(x)) else NULL
  list(parametros = pegar(acum$parametros), celdas = pegar(acum$celdas),
       manifiestos = if (length(acum$manifiestos)) acum$manifiestos else list(),
       tablas = if (length(acum$tablas)) acum$tablas[order(names(acum$tablas))] else list(),
       manifiestos_crudos = acum$crudos)
}
