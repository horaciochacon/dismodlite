# Formato simple de un proyecto (ver ?dl_proyecto): una configuración corta y tablas planas, traducidas al formato
# completo (dl_config y las tablas del contrato dismod_lite/v1) según config_simple.yaml y columnas_simple.csv de
# inst/referencia. Los números no cambian y la validación es la del formato completo. Las rutas: R/proyecto.R.

# Procedencia de lo que la configuración completa exige declarar (edad_inicio_fuente, remision.fuente, ...).
.DL_PROCEDENCIA_SIMPLE <- "declarado en la configuraci\u00f3n simple"

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

# Las columnas de las tablas (inst/referencia/columnas_simple.csv): `destino`, la del formato completo a la que pasa
# tal cual; `tipo` (texto, num, int, lgl), el que exige el lector; `uso`: obligatoria, plantilla u opcional.
.dl_columnas_ref <- function() .dl_leer_memo(.dl_inst_archivo("referencia", "columnas_simple.csv"), function(p)
  utils::read.csv(p, colClasses = "character", na.strings = character(), encoding = "UTF-8"))

# Columnas de `archivo` (poblacion.csv, ...) con alguno de los `usos`.
.dl_columnas_simple <- function(archivo, usos = c("obligatoria", "plantilla", "opcional")) {
  t <- .dl_columnas_ref()
  t$columna[t$archivo == archivo & t$uso %in% usos]
}

# Los archivos de un proyecto simple: los de columnas_simple.csv (ancla/ y covariables/ son carpetas) y pesos_80mas.csv.
.dl_archivos_simple <- function() c(unique(.dl_columnas_ref()$archivo), "pesos_80mas.csv")

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
  probs
}

# Error con la lista de problemas de la configuración simple `archivo`.
.dl_stop_config_simple <- function(archivo, probs)
  .dl_stop("la configuraci\u00f3n simple \u00ab%s\u00bb tiene %d problema(s):\n%s\n  archivo: %s", basename(archivo),
           length(probs), paste0("  - ", probs, collapse = "\n"), archivo, campos = list(problemas = probs))

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
.dl_en_simple <- function(expr, simple = TRUE) {
  if (!isTRUE(simple) || isTRUE(.dl_estado$simple)) return(expr)
  .dl_estado$simple <- TRUE
  on.exit(.dl_estado$simple <- FALSE)
  expr
}

# El texto `x` de un mensaje en palabras del formato simple: cada palabra exacta del formato completo (nunca dentro de
# otra ni de una ruta) por la suya en las dos tablas (clave, tabla.columna, tabla, argumento de dl_rutas(), tipo de
# dato), sin el nombre de la regla del contrato delante del problema («datos: pareja_val_se_o_x_n — »).
.dl_texto_simple <- function(x) {
  t <- .dl_claves_simple()
  tablas <- attr(t, "tablas")
  rutas <- attr(t, "rutas")
  col <- .dl_columnas_ref()
  col <- col[col$archivo %in% tablas & nzchar(col$destino), ]
  k <- t[grepl("^[a-z_.]+(\\[\\][.a-z_]+)?$", t$destino) & t$destino != t$clave, ]
  k <- k[order(-nchar(k$destino)), ]                  # transformaciones[].escala antes que transformaciones
  tipos <- .dl_tipos_datos_simple()
  exacta <- function(x) sprintf("(?<![\\w./\\\\])%s(?![\\w/\\\\]|[.]\\w)", x)
  punto <- function(x) gsub(".", "[.]", x, fixed = TRUE)
  cambios <- c(
    stats::setNames(sub("[]", "[\\1]", k$clave, fixed = TRUE),
                    exacta(paste0(gsub("\\[\\]", "\\\\[([0-9]+)\\\\]", punto(k$destino)), "(?:[.]valor)?"))),
    stats::setNames(sprintf("%s, columna %s", col$archivo, col$columna),
                    exacta(sprintf("%s[.]%s", names(tablas)[match(col$archivo, tablas)], col$destino))),
    stats::setNames(tablas, exacta(paste("la tabla", names(tablas)))),
    stats::setNames(paste0("\\1", tablas, ":"), sprintf("(?m)^(\\s*-?\\s*)%s:", names(tablas))),
    stats::setNames(rutas, sprintf("(?:`%s`|\u00ab%s\u00bb)(?: de dl_rutas\\(\\))?", names(rutas), names(rutas))),
    stats::setNames(names(tipos), exacta(tipos)),
    "(?m)^(\\s*-?\\s*[^\\s:]+: )[a-z0-9_]+ \u2014 " = "\\1")
  for (i in seq_along(cambios)) x <- gsub(names(cambios)[i], cambios[[i]], x, perl = TRUE)
  x
}

# Nudos por defecto de la incidencia: cada 10 años desde edad_inicio hasta 80, y 95 (enteros si lo son, como el YAML).
.dl_nudos_defecto <- function(edad_inicio) {
  n <- sort(unique(c(seq(edad_inicio, max(edad_inicio, 80), by = 10), 95)))
  if (all(n == round(n))) as.integer(n) else n
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
      "ubicaci\u00f3n)"), .dl_lista(nombres, "ninguna"), .dl_lista(contexto$covariables_subnacionales, "ninguna")))
  nudos <- unlist(dado("nudos")) %||% .dl_nudos_defecto(s[["edad_inicio"]])
  pd <- t[!grepl("\\[\\]", t$clave) & nzchar(t$defecto) & t$defecto != "obligatoria", c("clave", "defecto")]
  pd <- pd[vapply(pd$clave, function(k) is.null(dado(k)), NA), ]
  reglas <- c(nombre = contexto$nombre, ubicacion_gbd = format(ubicacion), subnacional.modo = modo,
              nudos = sprintf("[%s]", paste(nudos, collapse = ", ")))
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
    origen = list(formato = "simple", archivo = archivo, nombre = s[["nombre"]] %||% contexto$nombre,
                  subnacional = modo, unidades = "contrato", betas = betas, por_defecto = por_defecto))
  Filter(Negate(is.null), cfg)
}

# Configuración simple `s` (de `archivo`, en la carpeta `proyecto`) -> dl_config, validado por el validador completo
# con los errores citados por la clave simple. `cambios` (claves del formato completo) van después de `avanzado`.
.dl_config_simple <- function(s, archivo, causa, proyecto, cambios = NULL) {
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
  cfg <- .dl_traducir_config_simple(s, archivo, .dl_contexto_proyecto(proyecto, s, as.integer(s[["causa"]])))
  cfg <- tryCatch(.dl_fundir_cambios(cfg, s[["avanzado"]]),
                  dl_error = function(e) .dl_stop_config_simple(archivo, paste("avanzado:", .dl_detalle(e))))
  cfg <- .dl_fundir_cambios(cfg, cambios)
  v <- .dl_validar_config(cfg, causa)
  if (length(v$problemas))
    .dl_stop_config_simple(archivo, .dl_problema_en_simple(v$campos, v$mensajes, s))
  v$cfg
}

# ---- Las tablas ----

# Lee un CSV del proyecto (`tabla`: su nombre en columnas_simple.csv) como texto, vacíos como NA (los números valen lo
# mismo que en un CSV del formato completo), con las columnas obligatorias y los tipos de columnas_simple.csv.
.dl_leer_simple <- function(archivo, tabla = basename(archivo)) {
  t <- .dl_columnas_ref()
  t <- t[t$archivo == tabla & t$tipo != "texto", ]
  d <- .dl_leer_csv(archivo, colClasses = "character", na.strings = "", tabla = basename(archivo),
                    tipos = stats::setNames(t$tipo, t$columna))
  faltan <- setdiff(.dl_columnas_simple(tabla, "obligatoria"), names(d))
  if (length(faltan))
    .dl_stop("a \u00ab%s\u00bb le falta(n) la(s) columna(s) %s (tiene: %s).\n  archivo: %s", basename(archivo),
             paste(faltan, collapse = ", "), paste(names(d), collapse = ", "), archivo)
  for (cn in names(d)) data.table::set(d, which(!nzchar(trimws(d[[cn]]))), cn, NA_character_)
  d
}

# Renombra por referencia las columnas de `d` (leída de `tabla`) que pasan tal cual al formato completo.
.dl_renombrar_simple <- function(d, tabla) {
  t <- .dl_columnas_ref()
  t <- t[t$archivo == tabla & nzchar(t$destino) & t$columna != t$destino & t$columna %in% names(d), ]
  data.table::setnames(d, t$columna, t$destino)
  d
}

# Filas de datos (líneas no vacías, sin el encabezado) del CSV `f` o de los CSV de la carpeta `f`, leyendo a lo sumo
# `n` líneas de cada uno; 0 si no existe. Una tabla opcional sin filas es como si no estuviera (así sirve de plantilla).
.dl_filas_csv <- function(f, n = -1L) {
  if (dir.exists(f)) f <- list.files(f, "[.]csv$", ignore.case = TRUE, full.names = TRUE)
  sum(vapply(f[file.exists(f)], function(a) max(0L, sum(grepl("[^[:space:]]", useBytes = TRUE,
    readLines(a, n = n, warn = FALSE, encoding = "UTF-8"))) - 1L), 1L))
}
.dl_hay_filas <- function(archivo) .dl_filas_csv(archivo, 50L) > 0L

# Números calculados -> su texto en data.table::fwrite (15 cifras): valen lo mismo que en un CSV del formato completo.
.dl_num_texto <- function(x) {
  if (!length(x)) return(character())
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  data.table::fwrite(list(x = x), f, eol = "\n", na = "NA")
  out <- readLines(f)[-1L]
  out[out == "NA"] <- NA_character_
  out
}

# Números -> el texto más corto que vuelve exactamente al mismo número (15, 16 o 17 cifras significativas).
.dl_num_exacto <- function(x) vapply(x, function(v) {
  if (is.na(v)) return(NA_character_)
  for (d in 15:17) { s <- format(v, digits = d); if (as.numeric(s) == v) return(s) }
  s
}, "", USE.NAMES = FALSE)

# Grupos de edad de GBD (el catálogo de inst/referencia): age_group_id y límites [age_start, age_end).
.dl_grupos_edad_referencia <- function()
  .dl_bandas_catalogo(d = .dl_leer_memo(.dl_inst_archivo("referencia", "catalogo_demograficos_gbd2023.csv"),
                                        function(p) .dl_leer_csv(p, colClasses = "character")))

# Grupo de edad de GBD con los límites [inicio, fin), sin el estandarizado por edad; NA si no hay ninguno.
.dl_grupo_edad <- function(inicio, fin) {
  g <- .dl_grupos_edad_referencia()[age_group_id != .DL_BANDAS_AGREGADAS[["estandarizada"]]]
  g$age_group_id[match(paste(as.numeric(inicio), as.numeric(fin)), paste(g$age_start, g$age_end))]
}

# age_group_id de cada fila: la columna age_group_id o el grupo de GBD de edad_inicio y edad_fin. `vacio`: el grupo de
# las filas sin edad (todas las edades), o NULL si la edad es obligatoria.
.dl_edades_simple <- function(d, archivo, vacio = NULL) {
  if ("age_group_id" %in% names(d)) return(as.integer(d$age_group_id))
  if (!all(c("edad_inicio", "edad_fin") %in% names(d))) {
    if (!is.null(vacio)) return(rep(vacio, nrow(d)))
    .dl_stop("a \u00ab%s\u00bb le faltan las columnas edad_inicio y edad_fin (o age_group_id).\n  archivo: %s",
             basename(archivo), archivo)
  }
  sin_edad <- is.na(d$edad_inicio) & is.na(d$edad_fin)
  id <- .dl_grupo_edad(d$edad_inicio, d$edad_fin)
  if (!is.null(vacio)) id[sin_edad] <- vacio
  malas <- is.na(id)
  if (any(malas))
    .dl_stop(paste0("\u00ab%s\u00bb tiene bandas de edad que no son grupos de edad de GBD: %s. Usa sus l\u00edmites ",
                    "(por ejemplo 40 y 45 para 40-44 a\u00f1os, 80 y 125 para 80 y m\u00e1s).\n  archivo: %s"),
             basename(archivo), paste(utils::head(unique(paste0(d$edad_inicio[malas], "-", d$edad_fin[malas])), 3L),
                                      collapse = ", "), archivo)
  id
}

# Códigos de sexo de la columna `sexo` de `archivo`.
.dl_sexo_simple <- function(x, archivo) {
  s <- .dl_codigos_sexo(x)
  if (anyNA(s))
    .dl_stop("la columna sexo de \u00ab%s\u00bb admite %s; tiene: %s.\n  archivo: %s", basename(archivo),
             with(.dl_sexos_ref(), paste(sprintf("%s (%s)", slug_es, id), collapse = ", ")), x[is.na(s)][1L], archivo)
  s
}

# Los valores de `v`, sin repetir y en orden, para un mensaje («2019, 2023»); `vacio` si no hay ninguno.
.dl_lista <- function(v, vacio = "ninguno")
  if (length(v)) paste(sort(unique(v), method = "radix"), collapse = ", ") else vacio

# Nivel de cada ubicación: 0 la nacional (la del ancla), 1 cualquier otra.
.dl_nivel_simple <- function(location_id, cfg) ifelse(location_id == .dl_loc_ancla(cfg), 0L, 1L)

# ancla/: las descargas de GBD Results («ID y nombre»), cada una leída una vez, en el formato completo: columnas
# renombradas y constantes (acquisition_id, el nombre del archivo; round, su año más reciente; location_level 0).
.dl_ancla_simple <- function(carpeta) {
  dir_ancla <- file.path(carpeta, "ancla")
  archivos <- list.files(dir_ancla, pattern = "[.]csv$", ignore.case = TRUE, full.names = TRUE)
  if (!length(archivos))
    .dl_stop("la carpeta ancla/ del proyecto no tiene archivos CSV (las descargas de GBD Results).\n  carpeta: %s",
             dir_ancla)
  d <- data.table::rbindlist(fill = TRUE, lapply(archivos, function(f) .dl_leer_memo(f, function(f) {
    x <- .dl_leer_simple(f, "ancla/")
    x[, `:=`(acquisition_id = tools::file_path_sans_ext(basename(f)), round = as.character(max(as.integer(year))))]
  })))
  .dl_renombrar_simple(d, "ancla/")
  opcional <- function(cn) if (cn %in% names(d)) d[[cn]] else NA_character_
  d[, list(acquisition_id, source = .DL_STD_SOURCE, round, entity = "cause", location_id, location_name,
           location_level = "0", year, age_group_id, age_group_name, sex_id, sex_name = opcional("sex_name"),
           cause_id, cause_name, measure_id, measure_name = opcional("measure_name"),
           metric_id = opcional("metric_id"), metric_name, val, lower, upper, ui_level = "0.95")]
}

# Las filas de la causa `causa` en el ancla leída (.dl_ancla_simple); sin ninguna, un error con las causas que trae.
.dl_ancla_de_causa <- function(ancla, causa) {
  a <- ancla[cause_id == as.character(causa)]
  if (!nrow(a))
    .dl_stop(paste0("ancla/ no trae filas de la causa %s (causas que trae: %s): agrega a ancla/ su descarga de GBD ",
                    "Results o revisa `causa`"), causa, paste(sort(unique(ancla$cause_id)), collapse = ", "))
  a
}

# Lo que el ajuste necesita del ancla de la causa (`a`) en la ubicación nacional, el año del ancla y cada sexo, y no
# está: la prevalencia y, si la usa el prior de la mortalidad en exceso, la mortalidad (con su medida de .DL_MEDIDAS).
.dl_ancla_faltante <- function(a, cfg) {
  anio <- .dl_anio_ancla(cfg)
  pista <- c(prevalence = "", csmr = paste0(": la usa el prior de la mortalidad en exceso (desde_ancla, por defecto, ",
    "o plano sin techo); agr\u00e9gala a ancla/ o declara mortalidad_exceso: {prior: plano, techo: ...}, por ",
    "persona-a\u00f1o"))
  unlist(lapply(c("prevalence", if (.dl_prior_usa_csmr(cfg)) "csmr"), function(slug) {
    med <- .dl_medida(slug)
    met <- .dl_metrica_std(med, cfg)
    m <- a[measure_id == as.character(med$measure_id_gbd)]
    mn <- m[metric_name == met$metric_std & location_id == .dl_loc_ancla(cfg)]
    fs <- setdiff(unlist(cfg$sexos), as.integer(mn$sex_id[mn$year == anio]))
    hay <- mn$year[as.integer(mn$sex_id) %in% fs]                   # los años de los sexos que faltan
    if (nrow(mn) && !length(fs)) return(NULL)
    sprintf("ancla/ no trae la %s (measure_id %d, m\u00e9trica %s) de la causa %d %s%s%s", med$nombre_es,
            med$measure_id_gbd, met$metric_std, cfg$cause_id,
            if (!nrow(mn)) sprintf("en la ubicaci\u00f3n %s (m\u00e9tricas que trae: %s; ubicaciones: %s)",
                                   .dl_loc_ancla(cfg), .dl_lista(m$metric_name, "ninguna"),
                                   .dl_lista(m$location_id, "ninguna"))
            else sprintf("de %s para %d (a\u00f1os que trae: %s)", .dl_nombres_sexo(fs), anio, .dl_lista(hay)),
            if (as.character(anio - 1L) %in% hay && is.null(cfg$years$ancla))
              sprintf("; si %d a\u00fan no tiene estimaci\u00f3n de GBD, proyecta desde %d con ancla: {anio: %d}", anio,
                      anio - 1L, anio - 1L) else "", pista[[slug]])
  }))
}

# poblacion.csv en las columnas del formato completo (tabla) y los nombres de sus ubicaciones, sin la configuración.
.dl_leer_poblacion_simple <- function(carpeta) {
  archivo <- file.path(carpeta, "poblacion.csv")
  d <- .dl_renombrar_simple(.dl_leer_memo(archivo, .dl_leer_simple), "poblacion.csv")
  if (!nrow(d)) .dl_stop("poblacion.csv solo tiene el encabezado, sin filas.\n  archivo: %s", archivo)
  list(tabla = data.table::data.table(location_id = d$location_id, year = d$year,
                                      sex_id = .dl_sexo_simple(d$sexo, archivo),
                                      age_group_id = .dl_edades_simple(d, archivo), val = d$val),
       nombres = if ("location_name" %in% names(d)) unique(d[!is.na(location_name), list(location_id, location_name)])
                 else data.table::data.table(location_id = character(), location_name = character()))
}

# poblacion.csv -> la tabla poblacion del formato completo (con el año que se estima para cada sexo) y los nombres. Sin
# filas nacionales, son la suma de las demás (si vienen, la regla nivel1_suma_nivel0 lo comprueba); sin la banda 80+,
# la suma de 80-84 a 95+. Las sumas se escriben como fwrite (15 cifras): exactas con conteos enteros.
.dl_poblacion_simple <- function(carpeta, cfg) {
  p <- .dl_leer_poblacion_simple(carpeta)
  out <- p$tabla
  anio <- as.character(.dl_anio_ajuste(cfg))
  falta <- setdiff(unlist(cfg$sexos), out$sex_id[out$year == anio])
  if (length(falta))
    .dl_stop(paste0("poblacion.csv no trae la poblaci\u00f3n de %s para %s (anio, el a\u00f1o que se estima; ",
                    "a\u00f1os que trae: %s).\n  archivo: %s"), .dl_nombres_sexo(falta), anio,
             .dl_lista(out$year[out$sex_id %in% falta]), file.path(carpeta, "poblacion.csv"))
  sumar <- function(x, por) {
    s <- x[, list(v = sum(as.numeric(val))), by = por]
    s[, val := .dl_num_texto(v)][, v := NULL][]
  }
  nacional <- .dl_loc_ancla(cfg)
  if (!any(out$location_id == nacional))
    out <- rbind(sumar(out, c("year", "sex_id", "age_group_id"))[, location_id := nacional], out, use.names = TRUE)
  finas <- .DL_FINAS_80$ids
  if (!any(out$age_group_id == .DL_FINAS_80$id_salida)) {
    f <- out[age_group_id %in% finas]
    f <- f[f[, .N, by = list(location_id, year, sex_id)][N == length(finas)], on = c("location_id", "year", "sex_id")]
    out <- rbind(out, sumar(f, c("location_id", "year", "sex_id"))[, age_group_id := .DL_FINAS_80$id_salida],
                 use.names = TRUE)
  }
  g <- .dl_grupos_edad_referencia()
  k <- match(out$age_group_id, g$age_group_id)
  out[, `:=`(location_level = .dl_nivel_simple(location_id, cfg), a0 = g$age_start[k], a1 = g$age_end[k])]
  # el orden del formato completo: la nacional primero; una banda antes que las más finas que contiene
  data.table::setorder(out, location_level, location_id, year, sex_id, a0, -a1)
  list(tabla = out[, list(location_id, location_level, year, sex_id, age_group_id, val, acquisition_id = "poblacion")],
       nombres = p$nombres)
}

# Pesos de las bandas de 80-84, 85-89, 90-94 y 95+ (age_group_id, sex_id, peso) desde la población nacional `pobl` del
# año del ancla: la cuota de cada banda en el 80+ de su sexo, en los sexos para los que el ancla trae esas bandas.
.dl_pesos_80_simple <- function(pobl, cfg, ancla) {
  finas <- .DL_FINAS_80$ids
  anio <- .dl_anio_ancla(cfg)
  w <- pobl[location_level == 0L & year == as.character(anio) & age_group_id %in% finas]
  w[, peso := as.numeric(val) / sum(as.numeric(val)), by = sex_id]
  data.table::setorder(w[, orden := match(age_group_id, finas)], sex_id, orden)
  a <- ancla[cause_id == as.character(cfg$cause_id) & location_id == .dl_loc_ancla(cfg) &
             year == as.character(anio) & age_group_id %in% as.character(finas)]
  sexos <- intersect(unlist(cfg$sexos), as.integer(a$sex_id))
  falta <- setdiff(sexos, w[, .N, by = sex_id][N == length(finas)]$sex_id)
  if (length(falta))
    .dl_stop(paste0("el ancla trae las bandas de 80-84, 85-89, 90-94 y 95+ a\u00f1os y la poblaci\u00f3n nacional ",
                    "de %d (el a\u00f1o del ancla) no las tiene para %s: hacen falta para agregarlas a 80+. ",
                    "Agr\u00e9galas a poblacion.csv o da los pesos en pesos_80mas.csv (age_group_id, sex_id, peso)"),
             anio, .dl_nombres_sexo(falta))
  w[, list(age_group_id, sex_id, peso)]
}

# covariables/: las descargas del GHDx (el lector del formato completo las usa tal cual). De aquí salen el covariate_id
# de cada covariable declarada (el de la descarga o su posición) y sus valores nacionales. NULL sin covariables.
.dl_covariables_simple <- function(carpeta, cfg) {
  nombres <- vapply(cfg$origen$covariables, function(cv) cv[["nombre"]], "")
  if (!length(nombres)) return(NULL)
  dir_cov <- file.path(carpeta, "covariables")
  archivos <- list.files(dir_cov, pattern = "[.]csv$", ignore.case = TRUE, full.names = TRUE)
  if (!length(archivos))
    .dl_stop(paste0("la configuraci\u00f3n declara covariables y la carpeta covariables/ del proyecto no tiene ",
                    "descargas del GHDx (archivos CSV).\n  carpeta: %s"), dir_cov)
  d <- data.table::rbindlist(fill = TRUE, lapply(archivos, .dl_leer_simple, tabla = "covariables/"))
  faltan <- setdiff(nombres, d$covariate_name_short)
  if (length(faltan))
    .dl_stop("covariables/ no trae %s (covariate_name_short de la descarga del GHDx).\n  carpeta: %s",
             paste(faltan, collapse = ", "), dir_cov)
  ids <- vapply(seq_along(nombres), function(k) {
    v <- unique(d$covariate_id[d$covariate_name_short == nombres[k]])
    if (length(v) == 1L && !is.na(v)) as.integer(v) else k
  }, 1L)
  list(ids = stats::setNames(ids, nombres),
       nacional = d[location_id == .dl_loc_ancla(cfg) & covariate_name_short %in% nombres])
}

# La extracción (el YAML de las betas) desde covariables[], con cada beta como el texto más corto de su número.
.dl_extraccion_simple <- function(cfg, cov) {
  efecto <- .dl_vocabulario_simple("covariables[].efecto_sobre")
  list(meta = list(causa_gbd = list(cause_id = .dl_extraction_cause_id(cfg)), fuente = .DL_PROCEDENCIA_SIMPLE),
       covariables_gbd = lapply(cfg$origen$covariables, function(cv) {
         b <- .dl_num_exacto(as.numeric(unlist(cv[["beta"]])))
         c(list(covariate_id = as.character(cov$ids[[cv[["nombre"]]]]), covariate_name_short = cv[["nombre"]],
                nombre_impreso = cv[["nombre"]], nivel = "pais", rol = "predictiva",
                parametro = unname(efecto[cv[["efecto_sobre"]]]), beta_valor = b[1L]),
           if (length(b) == 3L) list(beta_inferior = b[2L], beta_superior = b[3L]))
       }))
}

# proxies.csv -> tabla cov_proxy (covariables declaradas, año que se estima). ancla_ghdx: el valor nacional (año del
# ancla; mismo sexo o ambos) en que debe cerrar el promedio ponderado (regla promedio_cierra_ancla); no se recalibra.
.dl_proxies_simple <- function(carpeta, cfg, cov) {
  archivo <- file.path(carpeta, "proxies.csv")
  decl <- vapply(cfg$covariables, function(cv) cv$covariate_name_short, "")
  d <- .dl_leer_simple(archivo)[covariable %in% decl]
  ajuste <- as.character(.dl_anio_ajuste(cfg))
  if (!ajuste %in% d$anio)
    .dl_stop(paste0("proxies.csv no trae filas de %s (anio, el a\u00f1o que se estima) de las covariables %s ",
                    "(a\u00f1os que trae: %s). Sin ellas no hay estimaci\u00f3n subnacional por covariables: agrega ",
                    "los valores de ese a\u00f1o o usa subnacional.modo: plano.\n  archivo: %s"), ajuste,
             paste(decl, collapse = ", "), .dl_lista(d$anio), archivo)
  d <- d[anio == ajuste]
  sexo <- .dl_sexo_simple(d$sexo, archivo)
  # el valor nacional, una vez por covariable y sexo
  anio <- .dl_anio_ancla(cfg)
  nac <- cov$nacional[year_id == as.character(anio)]
  clave <- paste(d$covariable, sexo)
  una <- which(!duplicated(clave))
  valor <- vapply(una, function(i) {
    x <- .dl_filas_del_sexo(nac[covariate_name_short == d$covariable[i]], sexo[i])
    if (nrow(x) != 1L)
      .dl_stop(paste0("covariables/ trae %d valor(es) nacional(es) de %s de %d (el a\u00f1o del ancla) para %s: ",
                      "proxies.csv necesita uno"), nrow(x), d$covariable[i], anio, .dl_nombres_sexo(sexo[i]))
    x$mean_value
  }, "")
  ids_proxy <- vapply(cfg$covariables, function(cv) as.integer(cv$proxy$covariate_id_proxy), 1L)
  edad <- .dl_edades_simple(d, archivo, .DL_BANDA_TODAS_LAS_EDADES)
  d <- .dl_renombrar_simple(d, "proxies.csv")
  out <- data.table::data.table(
    covariate_id_proxy = ids_proxy[match(d$covariable, decl)], covariate_id_gbd = unname(cov$ids[d$covariable]),
    location_id = d$location_id, year = d$year, sex_id = sexo, age_group_id = edad,
    valor_crudo = d$valor_calibrado, valor_calibrado = d$valor_calibrado, valor_calibrado_se = d$valor_calibrado_se,
    metodo_calibracion = "valores_de_proxies_csv", ancla_ghdx = valor[match(clave, clave[una])],
    acquisition_id = "proxies")
  data.table::setorder(out, covariate_id_proxy, year, sex_id, age_group_id, location_id)
}

# datos.csv -> tabla datos (dato_id «fila_<n>»: la línea n + 1 del archivo); sin la columna `causa`, cada fila es de la
# causa de la configuración. Las reglas de la tabla (valores posibles, un motivo por dato excluido) son de dl_insumos().
.dl_datos_simple <- function(carpeta, cfg, nombres_loc) {
  archivo <- file.path(carpeta, "datos.csv")
  d <- .dl_leer_simple(archivo)
  col <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA_character_, nrow(d))
  tipos <- .dl_tipos_datos_simple()
  malos <- setdiff(d$tipo, names(tipos))
  if (length(malos))
    .dl_stop("la columna tipo de \u00abdatos.csv\u00bb admite %s; tiene: %s.\n  archivo: %s",
             paste(names(tipos), collapse = ", "), paste(malos, collapse = ", "), archivo)
  y0 <- ifelse(is.na(col("anio_inicio")), col("anio"), col("anio_inicio"))
  y1 <- ifelse(is.na(col("anio_fin")), col("anio"), col("anio_fin"))
  if (anyNA(y0) || anyNA(y1))
    .dl_stop("\u00abdatos.csv\u00bb: falta anio (o anio_inicio y anio_fin) en %s.\n  archivo: %s",
             paste(utils::head(sprintf("fila_%d", which(is.na(y0) | is.na(y1))), 5L), collapse = ", "), archivo)
  tipo <- unname(tipos[d$tipo])
  med <- .DL_TIPOS_DATO$measure_id[match(tipo, .DL_TIPOS_DATO$tipo)]
  med[tipo == "prev_admin"] <- .dl_medida_id("prevalence")
  sexo <- .dl_sexo_simple(d$sexo, archivo)
  fuente <- col("fuente")
  k <- match(d$location_id, nombres_loc$location_id)
  .dl_renombrar_simple(d, "datos.csv")
  data.table::data.table(
    dato_id = sprintf("fila_%d", seq_len(nrow(d))),
    cause_id = ifelse(is.na(col("cause_id")), cfg$cause_id, col("cause_id")), tipo_dato = tipo, measure_id = med,
    measure_name = .DL_MEDIDAS$measure_name[match(med, .DL_MEDIDAS$measure_id)],
    location_id = d$location_id, location_name = ifelse(is.na(k), d$location_id, nombres_loc$location_name[k]),
    location_level = .dl_nivel_simple(d$location_id, cfg), sex_id = sexo,
    sex_name = .dl_sexos_ref()$name[match(sexo, as.integer(.dl_sexos_ref()$id))], age_start = d$age_start,
    age_end = d$age_end, age_group_id = .dl_grupo_edad(d$age_start, d$age_end), year_start = y0, year_end = y1,
    val = col("val"), se = col("se"), x = col("x"), n = col("n"), n_efectivo = col("n_efectivo"),
    definicion = ifelse(is.na(col("definicion")), d$tipo, col("definicion")), es_referencia = TRUE,
    crosswalk_id = NA_character_, ajuste_completitud = FALSE, completitud = NA_character_,
    outlier = as.logical(col("excluir")) %in% TRUE, outlier_motivo = col("outlier_motivo"),
    acquisition_id = ifelse(is.na(fuente), "datos_locales", .dl_slugify(fuente)), nid_ghdx = NA_character_,
    cita = ifelse(is.na(fuente), "datos.csv", fuente))
}

# severidad.csv -> la tabla severidad de la causa (health_state_id: id_estado o el orden de aparición del estado);
# NULL si el archivo no trae la causa.
.dl_severidad_simple <- function(carpeta, cfg) {
  archivo <- file.path(carpeta, "severidad.csv")
  d <- .dl_leer_simple(archivo)
  ids <- if ("id_estado" %in% names(d)) d$id_estado else as.character(match(d$estado, unique(d$estado)))
  d <- .dl_renombrar_simple(d[, id_estado := ids][causa == as.character(cfg$cause_id)], "severidad.csv")
  if (!nrow(d)) return(NULL)
  d[, list(cause_id, health_state_id, proportion, prop_lower, prop_upper, dw_mean, dw_lower, dw_upper,
           beta_covariable = NA_character_, location_id_fuente = .dl_loc_ancla(cfg), fuente = "extraction")]
}

# Las tablas del proyecto simple `carpeta` para `cfg`, en el formato completo (ancla, poblacion, pesos_80, cov,
# extraccion, proxies, datos, severidad, nombres_loc). Cada paso va por `paso(nombre, expr)`: dl_revisar_proyecto()
# pasa uno que anota el problema y devuelve NULL, y los pasos que necesitan ese valor no corren.
.dl_traducir_tablas <- function(carpeta, cfg, paso = function(nombre, expr) expr) {
  en <- function(f) file.path(carpeta, f)
  nacional <- .dl_loc_ancla(cfg)
  x <- list()
  x$ancla <- paso("ancla/", {
    a <- .dl_ancla_simple(carpeta)
    falta <- .dl_ancla_faltante(.dl_ancla_de_causa(a, cfg$cause_id), cfg)
    if (length(falta)) .dl_stop("%s", paste(falta, collapse = "\n"), campos = list(problemas = falta))
    a[location_id == nacional]
  })
  x$poblacion <- paso("poblacion.csv", .dl_poblacion_simple(carpeta, cfg))
  if (!file.exists(en("pesos_80mas.csv")) && !is.null(x$poblacion) && !is.null(x$ancla))
    x$pesos_80 <- paso("pesos de 80+", .dl_pesos_80_simple(x$poblacion$tabla, cfg, x$ancla))
  if (length(cfg$origen$covariables)) x$cov <- paso("covariables/", .dl_covariables_simple(carpeta, cfg))
  if (!is.null(x$cov)) x$extraccion <- .dl_extraccion_simple(cfg, x$cov)
  if (length(cfg$covariables) && !is.null(x$cov))
    x$proxies <- paso("proxies.csv", .dl_proxies_simple(carpeta, cfg, x$cov))
  x$nombres_loc <- data.table::rbindlist(list(
    data.table::data.table(location_id = character(), location_name = character()),
    if (!is.null(x$ancla)) unique(x$ancla[, list(location_id, location_name)])[1L],
    if (!is.null(x$poblacion)) x$poblacion$nombres[location_id != nacional]))
  if (.dl_hay_filas(en("datos.csv"))) x$datos <- paso("datos.csv", .dl_datos_simple(carpeta, cfg, x$nombres_loc))
  if (.dl_hay_filas(en("severidad.csv"))) x$severidad <- paso("severidad.csv", .dl_severidad_simple(carpeta, cfg))
  x
}
