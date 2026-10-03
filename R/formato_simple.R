# La configuración de un proyecto (ver ?dl_proyecto y ?dl_configuracion): una configuración corta, traducida a la del
# formato completo (dl_config) según inst/referencia/config_simple.yaml, con lo que toma de las tablas del contrato (el
# contexto, R/proyecto.R). La validación es la del formato completo, con los mensajes en las palabras de la
# configuración y de las tablas (.dl_texto_simple). Las tablas: R/contrato.R y R/contrato_traduccion.R.

# Procedencia de lo que la configuración completa exige declarar (edad_inicio_fuente, remision.fuente, ...).
.DL_PROCEDENCIA_SIMPLE <- "declarado en la configuraci\u00f3n del proyecto"

# La configuración viene del formato simple: su traducción trae el campo `origen`.
.dl_es_simple <- function(cfg) identical(cfg$origen$formato, "simple")

# ---- Las tablas de referencia ----

# Lectura de los YAML del formato simple: solo true y false son lógicos (el YAML 1.1 lee también yes, no, on, off, y
# y n como lógicos: `modo: no` sería FALSE).
.DL_YAML_LOGICOS <- list("bool#yes" = function(x) if (tolower(x) == "true") TRUE else x,
                         "bool#no" = function(x) if (tolower(x) == "false") FALSE else x)

# Las claves de la configuración simple (inst/referencia/config_simple.yaml explica cada campo), una fila por clave;
# `defecto` lleva el texto de `valor` si es fijo. Atributos: las otras partes del archivo (alias, sugerencias, ...).
.dl_claves_simple <- function() .dl_leer_memo(.dl_inst_archivo("referencia", "config_simple.yaml"), function(p) {
  y <- .dl_leer_yaml(p, handlers = .DL_YAML_LOGICOS)
  campo <- function(cn, f = function(x) as.character(x %||% "")) lapply(y$claves, function(k) f(k[[cn]]))
  texto <- c("clave", "tipo", "destino", "descripcion", "defecto", "simbolo", "ejemplo", "error")
  t <- as.data.frame(lapply(stats::setNames(nm = texto), function(cn) unlist(campo(cn))), stringsAsFactors = FALSE)
  t$valor <- campo("valor", identity)
  t$vocabulario <- campo("vocabulario", function(v) { v <- unlist(v); if (is.null(names(v))) names(v) <- v; v })
  t$largo <- campo("largo", function(v) as.integer(unlist(v)))
  t$unico <- unlist(campo("unico", isTRUE))
  fijo <- !vapply(t$valor, is.null, NA)
  t$defecto[fijo] <- vapply(t$valor[fijo], function(v)                  # como en el YAML: false, no FALSE
    sprintf(if (length(v) > 1L) "[%s]" else "%s", paste(if (is.logical(v)) tolower(v) else v, collapse = ", ")), "")
  structure(t, alias = unlist(y$alias), sugerencias = unlist(y$sugerencias), movidas = unlist(y$movidas),
            tablas = unlist(y$tablas), rutas = unlist(y$rutas))
})

# Vocabulario de la clave `clave`: el valor admitido -> su valor en el formato completo.
.dl_vocabulario_simple <- function(clave) { t <- .dl_claves_simple(); t$vocabulario[[match(clave, t$clave)]] }

# `x` (una clave) con su símbolo en el modelo, si lo tiene: «ancla.peso (λ)»; `mostrar`, el símbolo como se escribe.
.dl_con_simbolo <- function(x, simbolo, mostrar = simbolo)
  paste0(x, ifelse(!is.na(simbolo) & nzchar(simbolo), paste0(" (", mostrar, ")"), ""))

# El valor por defecto de cada clave de `t` en palabras: el fijo (como lo escribe `codigo`), «el valor de» otra
# clave, una regla (como la escribe `texto`), «obligatoria» o «sin valor» (la clave no tiene valor por defecto).
.dl_defecto_en_palabras <- function(t, codigo = identity, texto = identity)
  ifelse(!vapply(t$valor, is.null, NA), codigo(t$defecto),
         ifelse(t$defecto %in% t$clave, paste("el valor de", codigo(t$defecto)),
                ifelse(nzchar(t$defecto), texto(t$defecto), "sin valor")))

# Los tipos de datos.csv -> su tipo_dato: los de datos_en_ajuste y prevalencia_registro (se lee, no entra al ajuste).
.dl_tipos_datos_simple <- function() c(.dl_vocabulario_simple("datos_en_ajuste"), prevalencia_registro = "prev_admin")

# Las etiquetas de inst/referencia/etiquetas_es.csv y sus sexos: id, name, name_es y slug_es (hombres, mujeres, ambos).
.dl_etiquetas_ref <- function() .dl_leer_memo(.dl_inst_archivo("referencia", "etiquetas_es.csv"),
                                              function(p) .dl_leer_csv(p, colClasses = "character"))
.dl_sexos_ref <- function() .dl_etiquetas_ref()[tabla == "sex"]

# Códigos de sexo (1, 2, 3) desde su slug_es o su código (NA si no es ninguno); y los códigos en palabras.
.dl_codigos_sexo <- function(x) {
  s <- .dl_sexos_ref()
  x <- tolower(trimws(as.character(unlist(x))))
  as.integer(rep(s$id, 2L)[match(x, c(s$slug_es, s$id))])
}
.dl_nombres_sexo <- function(s) paste(tolower(.dl_sexos_ref()$name_es)[match(s, .dl_sexos_ref()$id)], collapse = " y ")

# ---- La configuración ----

# TRUE si `x` es un valor del `tipo` de una clave.
.dl_es_tipo_simple <- function(x, tipo) {
  v <- unlist(x)
  lista <- is.atomic(v) && length(v) > 0L && !anyNA(v) && all(lengths(x) == 1L)
  switch(tipo, "entero" = .dl_es_entero1(x), "n\u00famero" = .dl_es_numero1(x),
         "texto" = .dl_es_texto1(x) && nzchar(trimws(x)), "l\u00f3gico" = isTRUE(x) || isFALSE(x),
         "bloque" = is.list(x) && !is.null(names(x)),
         "lista de n\u00fameros" = lista && is.numeric(v),
         "lista de enteros" = lista && is.numeric(v) && all(v == round(v)), "lista de textos" = lista, TRUE)
}

# El valor en la ruta `ruta` de la lista `x` (ancla.peso, transformaciones[1].escala), o NULL.
.dl_valor_en <- function(x, ruta) {
  for (k in strsplit(gsub("\\[([0-9]+)\\]", ".\\1", ruta), ".", fixed = TRUE)[[1L]])
    x <- if (!is.list(x)) NULL else if (grepl("^[0-9]+$", k)) x[as.integer(k)][[1L]] else x[[k]]
  x
}

# Problemas de forma de la configuración simple `s` según la tabla de claves («clave: problema»): claves desconocidas
# u obligatorias que faltan y valores fuera de su tipo, vocabulario o largo. Los dominios, el validador completo.
.dl_problemas_config_simple <- function(s) {
  t <- .dl_claves_simple()
  probs <- character()
  p <- function(clave, msg) probs <<- c(probs, sprintf("%s: %s", clave, msg))
  debajo <- function(k, sep) sub(paste0("^", k, sep), "", grep(paste0("^", k, sep), t$clave, value = TRUE))
  # una clave que cambió de lugar (incidencia.nudos) dice dónde está ahora; las demás, la más parecida o las posibles
  movidas <- attr(t, "movidas")
  desconocida <- function(k, validas, prefijo = "", alias = NULL) {
    ruta <- paste0(prefijo, k)
    nueva <- if (ruta %in% names(movidas)) movidas[[ruta]]
    movida <- if (!is.null(nueva))
      sprintf("`%s` ahora es `%s`%s", ruta, nueva, if (grepl(".", nueva, fixed = TRUE)) "" else ", en el primer nivel")
    p(ruta, paste0("clave desconocida; ", movida %||% .dl_sugerir_clave(k, validas, prefijo, alias) %||%
                     sprintf("las claves posibles son: %s", paste(validas, collapse = ", "))))
  }
  # el valor `x` de la clave `k` de la tabla, citada como `como`
  revisar <- function(k, x, como) {
    f <- t[t$clave == k, ]
    voc <- f$vocabulario[[1L]]
    malos <- setdiff(as.character(unlist(x)), names(voc))
    if (!.dl_es_tipo_simple(x, f$tipo) || (length(f$largo[[1L]]) && !length(unlist(x)) %in% f$largo[[1L]]))
      p(como, if (nzchar(f$error)) f$error
              else sprintf("debe ser %s %s%s", if (startsWith(f$tipo, "lista")) "una" else "un", f$tipo,
                           if (nzchar(f$ejemplo)) sprintf(" (por ejemplo %s)", f$ejemplo) else ""))
    else if (length(voc) && length(malos))
      p(como, sprintf("valor(es) no admitido(s): %s; los admitidos son: %s", paste(malos, collapse = ", "),
                      paste(names(voc), collapse = ", ")))
  }
  superiores <- unique(sub("[.[].*$", "", t$clave))
  for (k in names(s)) {
    x <- s[[k]]
    bloque <- debajo(k, "[.]")
    registro <- debajo(k, "\\[\\][.]")
    if (k %in% t$clave[grepl("^[a-z_]+[.]", t$clave)])       # ancla.peso: 0.5, escrita como en los mensajes
      p(k, sprintf("va dentro de su bloque: %s: {%s: %s}", sub("[.].*$", "", k), sub("^[^.]*[.]", "", k),
                   if (length(unlist(x)) == 1L) unlist(x) else sprintf("[%s]", paste(unlist(x), collapse = ", "))))
    else if (!k %in% superiores) desconocida(k, superiores, alias = attr(t, "sugerencias"))
    else if (is.null(x)) next                              # un bloque o una lista sin nada debajo: como si no estuviera
    else if (length(bloque)) {
      if (!is.list(x) || is.null(names(x)))
        p(k, sprintf("es un bloque con claves (%s), por ejemplo %s: {%s: ...}", paste(bloque, collapse = ", "), k,
                     bloque[1L]))
      else for (j in names(x))
        if (!j %in% bloque) desconocida(j, bloque, paste0(k, "."))
        else if (!is.null(x[[j]])) revisar(paste0(k, ".", j), x[[j]], paste0(k, ".", j))
    } else if (length(registro)) {
      if (!is.list(x) || !is.null(names(x)) || !all(vapply(x, function(e) is.list(e) && !is.null(names(e)), NA))) {
        p(k, sprintf("es una lista de registros, cada uno con las claves %s", paste(registro, collapse = ", ")))
        next
      }
      for (i in seq_along(x)) for (j in union(names(x[[i]]), registro)) {
        f <- t[t$clave == paste0(k, "[].", j), ]
        como <- sprintf("%s[%d].%s", k, i, j)
        if (!nrow(f)) desconocida(j, registro, sprintf("%s[%d].", k, i))
        else if (!is.null(x[[i]][[j]])) revisar(f$clave, x[[i]][[j]], como)
        else if (f$defecto == "obligatoria") p(como, "falta (obligatoria)")
      }
      for (j in registro[t$unico[match(paste0(k, "[].", registro), t$clave)]]) {
        v <- unlist(lapply(x, `[[`, j))
        if (anyDuplicated(v)) p(k, sprintf("%s repetido: %s", j, paste(unique(v[duplicated(v)]), collapse = ", ")))
      }
    } else revisar(k, x, k)
  }
  for (k in t$clave[t$defecto == "obligatoria" & !grepl("[.[]", t$clave)])
    if (is.null(s[[k]])) p(k, sprintf("falta (obligatoria): %s", t$descripcion[t$clave == k]))
  c(probs, .dl_problemas_particion(s), .dl_problemas_proxies(s))
}

# severidad.padre y componente.secuelas se leen de la corrida de partición: sin severidad.particion no tienen de
# dónde salir.
.dl_problemas_particion <- function(s) {
  if (!is.null(.dl_valor_en(s, "severidad.particion"))) return(character())
  c(if (!is.null(.dl_valor_en(s, "severidad.padre")))
      paste0("severidad.padre: exige severidad.particion (la carpeta de la corrida de partici\u00f3n que reparte las ",
             "secuelas de la causa padre)"),
    if (!is.null(.dl_valor_en(s, "componente.secuelas")))
      paste0("componente.secuelas: exige severidad.particion (la carpeta de la corrida de partici\u00f3n con las ",
             "fracciones de esas secuelas)"))
}

# proxies.transformacion y proxies.excluir: la tabla de claves solo sabe que son un bloque y una lista de registros; sus
# claves y sus campos son de proxies_crudos (las covariables) y se revisan aqui, solo si su forma ya es la que toca (la
# forma la revisa .dl_problemas_config_simple).
.DL_PROXIES_TRANSFORMACIONES <- c("cociente", "diferencia")
.DL_PROXIES_REGISTRO <- c("anio", "motivo")

.dl_problemas_proxies <- function(s) {
  probs <- character()
  p <- function(clave, msg) probs <<- c(probs, sprintf("%s: %s", clave, msg))
  tr <- .dl_valor_en(s, "proxies.transformacion")
  if (is.list(tr) && !is.null(names(tr)))
    for (j in names(tr)) {
      v <- tr[[j]]
      if (is.null(v))
        p(paste0("proxies.transformacion.", j),
          sprintf("falta el valor; los admitidos son: %s", paste(.DL_PROXIES_TRANSFORMACIONES, collapse = ", ")))
      else if (!(.dl_es_texto1(v) && v %in% .DL_PROXIES_TRANSFORMACIONES))
        p(paste0("proxies.transformacion.", j),
          sprintf("valor no admitido: %s; los admitidos son: %s", paste(unlist(v), collapse = ", "),
                  paste(.DL_PROXIES_TRANSFORMACIONES, collapse = ", ")))
    }
  ex <- .dl_valor_en(s, "proxies.excluir")
  registros <- is.list(ex) && is.null(names(ex)) && all(vapply(ex, function(e) is.list(e) && !is.null(names(e)), NA))
  if (!is.null(ex) && !registros)
    p("proxies.excluir", sprintf("es una lista de registros, cada uno con las claves %s",
                                 paste(.DL_PROXIES_REGISTRO, collapse = ", ")))
  if (registros)
    for (i in seq_along(ex)) {
      e <- ex[[i]]
      como <- function(j) sprintf("proxies.excluir[%d].%s", i, j)
      for (j in setdiff(names(e), .DL_PROXIES_REGISTRO))
        p(como(j), sprintf("clave desconocida; las claves posibles son: %s", paste(.DL_PROXIES_REGISTRO, collapse = ", ")))
      if (is.null(e$anio)) p(como("anio"), "falta (obligatoria): el a\u00f1o de la edici\u00f3n que no entra")
      else if (!.dl_es_entero1(e$anio)) p(como("anio"), "debe ser un entero (por ejemplo 2021)")
      if (is.null(e$motivo)) p(como("motivo"), "falta (obligatoria): por qu\u00e9 no entra")
      else if (!(.dl_es_texto1(e$motivo) && nzchar(trimws(e$motivo))))
        p(como("motivo"), "debe ser un texto (por ejemplo cambio de modo de la encuesta)")
    }
  anios <- if (registros) unlist(lapply(ex, function(e) if (.dl_es_entero1(e$anio)) as.integer(e$anio)))
  for (a in unique(anios[duplicated(anios)]))
    p("proxies.excluir", sprintf("la edici\u00f3n %d se repite: un registro por edici\u00f3n, con su motivo", a))
  probs
}

# Error con la lista de problemas de la configuración del proyecto `archivo`.
.dl_stop_config_simple <- function(archivo, probs)
  .dl_stop("la configuraci\u00f3n del proyecto \u00ab%s\u00bb tiene %d problema(s):\n%s\n  archivo: %s",
           basename(archivo), length(probs), paste0("  - ", probs, collapse = "\n"), archivo, campos = list(problemas = probs))

# Problemas del validador completo (`campo`, `msg`) en palabras de `s`: «ancla.peso (anchor.lambda): ...», con el
# `error` de la clave si su valor viene de ella; uno de `avanzado` (el campo o su bloque), «avanzado: campo: ...».
.dl_problema_en_simple <- function(campo, msg, s) {
  t <- .dl_claves_simple()
  k <- match(gsub("\\[[0-9]+\\]", "[]", campo), t$destino)
  en <- function(x, ruta) !is.null(.dl_valor_en(x, ruta))
  avanzado <- unname(vapply(campo, function(x)            # el campo o, dentro de un bloque, su bloque
    en(s[["avanzado"]], x) || (grepl("[.].*[.]", x) && en(s[["avanzado"]], sub("[.][^.]*$", "", x))), NA))
  propio <- !is.na(k) & nzchar(t$error[k]) & vapply(t$clave[k], en, NA, x = s)
  clave <- .dl_texto_simple(campo)
  ifelse(avanzado, sprintf("avanzado: %s: %s", campo, msg),
         sprintf("%s: %s", ifelse(clave == campo, campo, sprintf("%s (%s)", clave, campo)),
                 ifelse(propio, t$error[k], .dl_texto_simple(msg))))
}

# Mientras se evalúa `expr` (si `simple`), .dl_condicion() pasa los mensajes del paquete por .dl_texto_simple().
# `datos`: la tabla datos del proyecto (o NULL), para contar sus filas en los mensajes como en los de las tablas
# (.dl_filas_de_datos).
.dl_en_simple <- function(expr, simple = TRUE, datos = NULL) {
  if (!isTRUE(simple) || isTRUE(.dl_estado$simple)) return(expr)
  .dl_estado$simple <- TRUE
  .dl_estado$datos_df <- .dl_origen_df(attr(datos, "origen"))
  on.exit({ .dl_estado$simple <- FALSE; .dl_estado$datos_df <- FALSE })
  expr
}

# Los dato_id de la tabla datos de un proyecto («fila_<n>»: la fila n de la tabla, ver .dl_trad_datos) en un mensaje,
# como en los mensajes de las tablas (.dl_filas_msg): «fila_3, fila_7» -> «fila(s) 4, 8» (la línea del CSV) o, si la
# tabla vino de un data.frame (`df`), «fila(s) 3, 7 del data.frame». El dato_id no cambia: entra al hash de los insumos.
.dl_filas_de_datos <- function(x, df) {
  m <- gregexpr("\\bfila_[0-9]+(, fila_[0-9]+)*\\b", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) vapply(v, function(s)
    .dl_filas_msg(as.integer(regmatches(s, gregexpr("[0-9]+", s))[[1L]]), df), ""))
  x
}

# Columnas de las tablas del formato completo que vienen de una columna de una tabla del contrato (por su nombre en el
# contrato), para citarlas en los mensajes: tabla.columna del formato completo -> columna del contrato. La tabla del
# contrato de cada una es la del atributo `tablas` de .dl_claves_simple().
.DL_COLUMNAS_DEL_CONTRATO <- c(
  datos.location_id = "ubicacion", datos.cause_id = "causa", datos.age_start = "edad_inicio",
  datos.age_end = "edad_fin", datos.val = "valor", datos.se = "error_estandar", datos.x = "casos", datos.n = "muestra",
  datos.n_efectivo = "muestra_efectiva", datos.outlier_motivo = "motivo", datos.definicion = "definicion",
  poblacion.location_id = "ubicacion", poblacion.year = "anio", poblacion.val = "poblacion",
  cov_proxy.location_id = "ubicacion", cov_proxy.valor_calibrado = "valor",
  cov_proxy.valor_calibrado_se = "error_estandar", severidad.health_state_id = "id_estado",
  severidad.proportion = "proporcion", severidad.prop_lower = "inferior", severidad.prop_upper = "superior",
  severidad.dw_mean = "peso_discapacidad", severidad.dw_lower = "peso_inferior", severidad.dw_upper = "peso_superior",
  prior_gbd.val = "valor", prior_gbd.lower = "inferior", prior_gbd.upper = "superior")

# El texto `x` de un mensaje en palabras del proyecto: cada palabra exacta del formato completo (nunca dentro de otra
# ni de una ruta) por la suya (clave de la configuración, tabla y columna del contrato, argumento de dl_rutas(), tipo
# de dato), sin el nombre de la regla del contrato delante del problema («datos: pareja_val_se_o_x_n — »).
.dl_texto_simple <- function(x) {
  t <- .dl_claves_simple()
  tablas <- attr(t, "tablas")
  rutas <- attr(t, "rutas")
  col <- .DL_COLUMNAS_DEL_CONTRATO
  k <- t[grepl("^[a-z_.]+(\\[\\][.a-z_]+)?$", t$destino) & t$destino != t$clave, ]
  k <- k[order(-nchar(k$destino)), ]                  # transformaciones[].escala antes que transformaciones
  tipos <- .dl_tipos_datos_simple()
  exacta <- function(x) sprintf("(?<![\\w./\\\\])%s(?![\\w/\\\\]|[.]\\w)", x)
  punto <- function(x) gsub(".", "[.]", x, fixed = TRUE)
  cambios <- c(
    stats::setNames(sub("[]", "[\\1]", k$clave, fixed = TRUE),
                    exacta(paste0(gsub("\\[\\]", "\\\\[([0-9]+)\\\\]", punto(k$destino)), "(?:[.]valor)?"))),
    stats::setNames(sprintf("%s, columna %s", tablas[sub("[.].*$", "", names(col))], col), exacta(punto(names(col)))),
    stats::setNames(paste("la tabla", tablas), exacta(paste("la tabla", names(tablas)))),
    stats::setNames(paste0("\\1", tablas, ":"), sprintf("(?m)^(\\s*-?\\s*)%s:", names(tablas))),
    # «la tabla `severidad`» -> «la tabla severidad», no «la tabla la tabla severidad»
    stats::setNames(rutas, sprintf("%s(?:`%s`|\u00ab%s\u00bb)(?: de dl_rutas\\(\\))?",
                                   ifelse(startsWith(rutas, "la tabla "), "(?:la tabla )?", ""), names(rutas),
                                   names(rutas))),
    stats::setNames(names(tipos), exacta(tipos)),
    "(?m)^(\\s*-?\\s*[^\\s:]+: )[a-z0-9_]+ \u2014 " = "\\1")
  for (i in seq_along(cambios)) x <- gsub(names(cambios)[i], cambios[[i]], x, perl = TRUE)
  .dl_filas_de_datos(x, isTRUE(.dl_estado$datos_df))
}

# Nudos por defecto de la incidencia: cada 10 años desde edad_inicio hasta 80, y 95 (enteros si lo son, como el YAML).
.dl_nudos_defecto <- function(edad_inicio) {
  n <- sort(unique(c(seq(edad_inicio, max(edad_inicio, 80), by = 10), 95)))
  if (all(n == round(n))) as.integer(n) else n
}

# Por qué la causa `causa` no tiene betas aunque la tabla betas trae filas (las de otras causas,
# contexto$causas_betas): el texto que se agrega al error, o "" si la tabla no trae filas con causa o la causa tiene
# las suyas. Nombra la causa de la que un subtipo toma las betas (contexto$padre) y lo que hay que hacer.
.dl_betas_de_otras_causas <- function(contexto, causa) {
  if (!is.null(contexto$betas) || !length(contexto$causas_betas)) return("")
  sprintf(paste0("; la tabla betas no trae filas de la causa %s%s (trae las de %s): agrega las de la causa o, si es ",
                 "un subtipo que usa las de otra causa, decl\u00e1rala en avanzado: extraction: cause_id"),
          causa, if (!is.null(contexto$padre)) sprintf(" ni de la causa %d, de la que toma las betas", contexto$padre)
                 else "", paste(contexto$causas_betas, collapse = ", "))
}

# Configuración completa (sin validar) desde la simple `s` de `archivo` y lo que se toma del proyecto (`contexto`):
#   ubicacion (el código nacional), nombre, betas (la tabla betas de la causa, ya resuelta para un subtipo),
#   covariables_subnacionales (las covariables con filas subnacionales), subnacional (si la población lo es) e
#   ids_covariable (covariable -> covariate_id, para valor_nacional_de).
# `origen`: formato, archivo, nombre, modo subnacional, betas y claves tomadas por defecto (con su valor); y las
# unidades: «contrato», las de las tablas del proyecto (proporción o por persona-año, sin conversión; .dl_metrica_std).
.dl_traducir_config_simple <- function(s, archivo, contexto) {
  t <- .dl_claves_simple()
  dado <- function(clave) .dl_valor_en(s, clave)
  val <- function(clave) dado(clave) %||% t$valor[[match(clave, t$clave)]]
  voc <- function(clave, x) unname(.dl_vocabulario_simple(clave)[as.character(unlist(x))])
  num <- function(x) { u <- unlist(x); if (is.numeric(u)) as.numeric(u) else x }
  ubicacion <- contexto$ubicacion
  # una fila por covariable de la tabla betas: transformaciones[k]; las que tienen filas subnacionales, además un proxy
  betas <- contexto$betas
  nombres <- if (is.null(betas)) character() else betas$covariable
  con_proxy <- which(nombres %in% contexto$covariables_subnacionales)
  modo <- dado("subnacional.modo") %||%
    if (length(con_proxy)) "covariables" else if (isTRUE(contexto$subnacional)) "plano" else "no"
  if (modo == "plano" && identical(contexto$subnacional, FALSE))
    .dl_stop_config_simple(archivo, paste0("subnacional.modo: es plano y la tabla poblacion no trae ubicaciones ",
                                           "subnacionales: agr\u00e9galas o usa subnacional.modo: no"))
  if (modo != "covariables") con_proxy <- integer()
  else if (!length(con_proxy))
    .dl_stop_config_simple(archivo, sprintf(paste0(
      "subnacional.modo: es covariables y ninguna covariable de la tabla betas (%s) tiene filas subnacionales en la ",
      "tabla covariables (trae: %s): revisa los nombres o usa subnacional.modo: plano (las tasas nacionales en cada ",
      "ubicaci\u00f3n)%s"), .dl_lista(nombres, "ninguna"), .dl_lista(contexto$covariables_subnacionales, "ninguna"),
      .dl_betas_de_otras_causas(contexto, s[["causa"]])))
  nudos <- unlist(dado("nudos")) %||% .dl_nudos_defecto(s[["edad_inicio"]])
  pd <- t[!grepl("\\[\\]", t$clave) & nzchar(t$defecto) & t$defecto != "obligatoria", c("clave", "defecto")]
  pd <- pd[vapply(pd$clave, function(k) is.null(dado(k)), NA), ]
  # ubicacion_gbd: la que tomaron los lectores de las descargas (.dl_ubicacion_gbd); sin descargas, no se usó
  if (!length(contexto$ubicacion_gbd)) pd <- pd[pd$clave != "ubicacion_gbd", ]
  # proxies.*: sin destino en el formato completo; su valor por defecto lo informa la calibracion de los proxies
  pd <- pd[!startsWith(pd$clave, "proxies."), ]
  reglas <- c(nombre = contexto$nombre, ubicacion_gbd = paste(contexto$ubicacion_gbd, collapse = ", "),
              subnacional.modo = modo, nudos = sprintf("[%s]", paste(nudos, collapse = ", ")))
  por_defecto <- stats::setNames(ifelse(pd$clave %in% names(reglas), reglas[pd$clave],
                                        ifelse(pd$defecto == "anio", format(s[["anio"]]), pd$defecto)), pd$clave)
  prior <- val("mortalidad_exceso.prior")
  techo <- dado("mortalidad_exceso.techo")
  proc <- .DL_PROCEDENCIA_SIMPLE
  cfg <- list(
    schema = "dismod_lite/v1", cause_id = as.integer(s[["causa"]]),
    years = c(list(ajuste = s[["anio"]]),
              if (!is.null(dado("ancla.anio"))) list(ancla = list(valor = dado("ancla.anio"), procedencia = proc))),
    # los sexos son un conjunto: en su orden (el de las simulaciones de la cascada y de los AVD) no en el escrito
    sexos = sort(unique(.dl_codigos_sexo(val("sexos")))), edad_inicio = s[["edad_inicio"]], edad_inicio_fuente = proc,
    remision = list(valor = val("remision"), fuente = proc),
    emr_prior = c(list(tipo = voc("mortalidad_exceso.prior", prior)),
                  if (prior == "plano") list(tipo_procedencia = proc),
                  if (!is.null(techo)) list(cota = c(0, num(techo)), fuente_cota = proc)),
    nudos_incidencia = nudos, sigma_suavidad = num(val("incidencia.suavidad")),
    # agrupar_bandas_finas: un proyecto trae las bandas como son (las finas de 80 a\u00f1os y m\u00e1s no se agrupan)
    anchor = c(list(location_id = ubicacion, lambda = num(val("ancla.peso")),
                    rho_edad = num(val("ancla.correlacion_edad")), medidas = "prevalence",
                    agrupar_bandas_finas = FALSE),
               if (length(dado("componente.secuelas")))
                 list(componente = list(sequela_ids = as.integer(unlist(dado("componente.secuelas"))),
                                        motivo = proc))),
    medidas_entrada = if (length(s[["datos_en_ajuste"]])) voc("datos_en_ajuste", s[["datos_en_ajuste"]]) else list(),
    # escala y cota_warning: los valores de la cascada del formato completo, sin clave simple
    cascada = c(list(kappa = num(val("subnacional.kappa")), escala = "natural", cota_warning = 0.5),
                if (!is.null(dado("subnacional.anio_validacion")))
                  list(heldout_anio = list(valor = dado("subnacional.anio_validacion"), procedencia = proc)),
                if (modo == "plano") list(modo = list(valor = "plana", procedencia = proc))),
    # la transformaci\u00f3n la declara la tabla betas: la regla que la busca en el nombre publicado no aplica; la escala
    # es 1 si la fila no la trae (solo interviene con la transformaci\u00f3n lineal)
    transformaciones = lapply(seq_along(nombres), function(k) c(list(
      covariate_name_short = nombres[k], transformacion = betas$transformacion[k], procedencia = proc,
      escala = if (is.na(betas$escala[k] %||% NA)) 1 else as.numeric(betas$escala[k]), escala_procedencia = proc,
      token_exento = TRUE), if (isTRUE(betas$escala_confirmada[k])) list(escala_confirmada = TRUE))),
    # proxies: covariate_id_proxy por la posici\u00f3n de la covariable en la tabla betas (900101, 900102, ...), ids
    # internos que el usuario no ve; valor_nacional_de: el valor nacional que ancla el proxy es el de otra covariable
    covariables = lapply(con_proxy, function(k) c(list(
      covariate_name_short = nombres[k], proxy = list(covariate_id_proxy = 900100L + k, justificacion = proc)),
      if (!is.na(betas$valor_nacional_de[k] %||% NA))
        list(sustituye = list(covariate_id = contexto$ids_covariable[[betas$valor_nacional_de[k]]],
                              covariate_name_short = betas$valor_nacional_de[k], procedencia = proc)))),
    extraction = if (!is.null(contexto$padre))
      list(cause_id = contexto$padre, motivo = sprintf("subtipo de la causa %d", contexto$padre)),
    severidad = if (!is.null(dado("severidad.particion")))
                  c(list(fuente = "mod", run_id = basename(dado("severidad.particion")), procedencia = proc),
                    if (!is.null(dado("severidad.padre"))) list(padre = as.integer(dado("severidad.padre"))))
                else list(fuente = "tabla", procedencia = proc),
    sensibilidad = list(lambda = num(val("sensibilidad.peso")), rho = num(val("sensibilidad.correlacion_edad")),
                        kappa = num(val("sensibilidad.kappa"))),
    decisiones = if (length(s[["notas"]])) as.character(unlist(s[["notas"]])),
    # `configuracion`: la configuración del proyecto tal como se leyó, con los nombres de clave de ahora (la corrida
    # la congela en inputs/contrato/config.yaml, para repetirla)
    origen = list(formato = "simple", archivo = archivo, nombre = s[["nombre"]] %||% contexto$nombre,
                  subnacional = modo, unidades = "contrato", betas = betas, por_defecto = por_defecto,
                  configuracion = s))
  Filter(Negate(is.null), cfg)
}

# Las claves de la configuración simple `s` (de `archivo`), con los nombres anteriores (alias) cambiados por los de
# ahora: error con sus problemas de forma (.dl_problemas_config_simple), que no dependen de las tablas.
.dl_claves_config_simple <- function(s, archivo) {
  if (!is.list(s) || is.null(names(s)))
    .dl_stop_config_simple(archivo, "el archivo no es una lista de claves (clave: valor, una por l\u00ednea)")
  alias <- attr(.dl_claves_simple(), "alias")
  for (a in intersect(names(alias), names(s))) {
    if (!is.null(s[[alias[[a]]]]))
      .dl_stop_config_simple(archivo, sprintf("%1$s: es el nombre anterior de `%2$s`; deja solo `%2$s`", a, alias[[a]]))
    names(s)[names(s) == a] <- alias[[a]]
  }
  probs <- .dl_problemas_config_simple(s)
  if (length(probs)) .dl_stop_config_simple(archivo, probs)
  s
}

# Configuración simple `s` (de `archivo`) -> dl_config, con lo que toma de las tablas del proyecto (`contexto`, ver
# .dl_traducir_config_simple), validado por el validador completo con los errores citados por la clave simple.
# `cambios` (claves del formato completo) van después de `avanzado`.
.dl_config_simple <- function(s, archivo, causa, contexto, cambios = NULL) {
  s <- .dl_claves_config_simple(s, archivo)
  cfg <- .dl_traducir_config_simple(s, archivo, contexto)
  cfg <- tryCatch(.dl_fundir_cambios(cfg, s[["avanzado"]]),
                  dl_error = function(e) .dl_stop_config_simple(archivo, paste("avanzado:", .dl_detalle(e))))
  cfg <- .dl_fundir_cambios(cfg, cambios)
  v <- .dl_validar_config(cfg, causa)
  if (length(v$problemas))
    .dl_stop_config_simple(archivo, .dl_problema_en_simple(v$campos, v$mensajes, s))
  v$cfg
}
