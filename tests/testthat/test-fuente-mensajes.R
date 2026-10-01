# Revisión estática de los mensajes del código fuente (solo desde el árbol fuente: se omite en el paquete
# instalado): todo mensaje sale por .dl_stop(), .dl_warn() o .dl_message() (R/validar.R), que le ponen la función
# que llamó el usuario delante, y está en español. Las pruebas que llaman a las funciones están en test-mensajes.R.

# Funciones que emiten los mensajes del paquete: las únicas que llaman a stop(), warning() y message().
.EMISORES <- c(".dl_stop", ".dl_warn", ".dl_message")
.EMISORES_BASE <- c(".dl_condicion", .EMISORES)

# Funciones definidas en el nivel superior de los archivos de R/ (nombre -> expresión `function(...) cuerpo`).
.definiciones <- function(archivos) {
  defs <- list()
  for (a in archivos) {
    for (e in parse(text = readLines(a, warn = FALSE, encoding = "UTF-8"), keep.source = FALSE)) {
      if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]]) && is.call(e[[3L]]) &&
          identical(e[[3L]][[1L]], as.name("function")))
        defs[[as.character(e[[2L]])]] <- e[[3L]]
    }
  }
  defs
}

# Llamadas a `funciones` en un archivo: list(linea, llamada, dentro), con `dentro` la definición de nivel superior
# que la contiene.
.llamadas_a <- function(archivo, funciones) {
  texto <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  exprs <- parse(text = texto, keep.source = TRUE)
  rangos <- vapply(attr(exprs, "srcref"), function(r) c(r[1L], r[3L]), integer(2))
  nombres <- vapply(exprs, function(e)
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) as.character(e[[2L]]) else "", "")
  pd <- utils::getParseData(exprs)
  simbolos <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% funciones, ]
  lapply(seq_len(nrow(simbolos)), function(k) {
    llamada <- pd$parent[pd$id == simbolos$parent[k]]
    linea <- pd$line1[pd$id == llamada]
    list(linea = linea, llamada = str2lang(utils::getParseText(pd, llamada)),
         dentro = nombres[which(rangos[1L, ] <= linea & rangos[2L, ] >= linea)[1L]])
  })
}

# Comienzo del texto de un mensaje, reconstruido sin evaluar el código: un texto literal queda tal cual; paste(),
# paste0() y c() se concatenan hasta la primera parte que no es un texto; una función auxiliar del paquete aporta el
# comienzo de su última expresión. NA si el comienzo no se puede saber.
.comienzo <- function(e, defs, profundidad = 0L) {
  if (is.character(e)) return(e[1L])
  if (!is.call(e) || !is.name(e[[1L]])) return(NA_character_)
  f <- as.character(e[[1L]])
  args <- as.list(e)[-1L]
  sin_nombre <- if (is.null(names(args))) args else args[!nzchar(names(args))]
  if (f %in% c("paste", "paste0", "c")) {
    partes <- vapply(sin_nombre, .comienzo, "", defs = defs, profundidad = profundidad)
    fin <- match(NA_character_, partes, nomatch = length(partes) + 1L)
    if (fin == 1L) return(NA_character_)
    return(paste(partes[seq_len(fin - 1L)], collapse = if (f == "paste") " " else ""))
  }
  if (startsWith(f, ".dl_") && !is.null(defs[[f]]) && profundidad < 3L) {
    cuerpo <- defs[[f]][[3L]]
    ultima <- if (is.call(cuerpo) && identical(cuerpo[[1L]], as.name("{"))) cuerpo[[length(cuerpo)]] else cuerpo
    return(.comienzo(ultima, defs, profundidad + 1L))
  }
  NA_character_
}

# Todos los textos literales de una expresión.
.textos <- function(e) {
  if (is.character(e)) return(e)
  if (is.call(e)) return(unlist(lapply(as.list(e), .textos)))
  character()
}

test_that("stop(), warning() y message() solo dentro de .dl_stop(), .dl_warn() y .dl_message()", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  hallazgos <- character()
  for (a in list.files(file.path(r, "R"), pattern = "[.]R$", full.names = TRUE)) {
    for (x in .llamadas_a(a, c("stop", "warning", "message", ".dl_funcion_usuario"))) {
      permitido <- if (identical(as.character(x$llamada[[1L]]), ".dl_funcion_usuario")) ".dl_condicion"
                   else .EMISORES_BASE
      if (!x$dentro %in% permitido)
        hallazgos <- c(hallazgos, sprintf("R/%s:%d: %s() fuera de %s", basename(a), x$linea,
                                          as.character(x$llamada[[1L]]), paste(permitido, collapse = ", ")))
    }
  }
  testthat::expect(length(hallazgos) == 0L,
                   paste(c(sprintf("%d llamada(s) por corregir:", length(hallazgos)), hallazgos), collapse = "\n"))
})

test_that("cada mensaje de R/ está en español, sin la función escrita a mano ni variables de entorno", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  archivos <- list.files(file.path(r, "R"), pattern = "[.]R$", full.names = TRUE)
  defs <- .definiciones(archivos)
  hallazgos <- character()
  n <- 0L
  for (a in archivos) {
    for (x in .llamadas_a(a, .EMISORES)) {
      if (x$dentro %in% .EMISORES_BASE) next
      n <- n + 1L
      donde <- sprintf("R/%s:%d", basename(a), x$linea)
      # el comienzo del mensaje no nombra una función: .dl_stop() ya la pone delante
      cab <- .comienzo(x$llamada[[2L]], defs)
      if (!is.na(cab) && grepl("^(%s|dl_[a-z0-9_]+|dismodlite)\\(?\\)?: ", cab))
        hallazgos <- c(hallazgos, sprintf("%s empieza con «%s»: la función ya la pone %s()", donde,
                                          substr(cab, 1L, 40L), as.character(x$llamada[[1L]])))
      textos <- .textos(x$llamada)
      ingles <- grepl(paste0("\\b(the|is|are|must|should|not|invalid|missing|found|expected|cannot|does|was|",
                             "were|with|and|or|of|to|too|for|only|than|this|that|which|please)\\b"), textos,
                      perl = TRUE)
      if (any(ingles))
        hallazgos <- c(hallazgos, sprintf("%s parece en inglés: «%s»", donde, substr(textos[ingles][1L], 1L, 50L)))
      if (any(grepl("DATA_ROOT|CATALOGOS_DIR|DISMODLITE_DIR|TOOLS_REPO|Sys[.]getenv", textos)))
        hallazgos <- c(hallazgos, paste(donde, "nombra una variable de entorno"))
      if (any(grepl("\\bdraws?\\b(?!/)", textos, perl = TRUE)))
        hallazgos <- c(hallazgos, paste(donde, "dice «draws» en lugar de «simulaciones»"))
    }
  }
  expect_gt(n, 200L)                                  # la revisión encontró los mensajes
  testthat::expect(length(hallazgos) == 0L,
                   paste(c(sprintf("%d mensaje(s) por corregir:", length(hallazgos)), hallazgos), collapse = "\n"))
})
