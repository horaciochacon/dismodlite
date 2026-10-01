# Higiene del código fuente: sin términos prohibidos ni jerga interna en lo que ve el usuario, código R en ASCII
# fuera de los comentarios (los acentos de los textos van como \uXXXX) y comentarios sin fechas de bitácora ni
# referencias a documentos de trabajo. Estas pruebas leen el árbol fuente: se omiten en el paquete instalado
# (R CMD check), donde R/ ya no existe.

# Falla con la lista completa de hallazgos («archivo:línea: motivo»), para corregirlos todos de una vez.
expect_sin_hallazgos <- function(hallazgos, que) {
  testthat::expect(length(hallazgos) == 0L,
                   paste(c(sprintf("%d hallazgo(s) de %s:", length(hallazgos), que), hallazgos), collapse = "\n"))
}

leer_utf8 <- function(archivo) readLines(archivo, warn = FALSE, encoding = "UTF-8")

# Todos los textos literales de una expresión.
.textos_de <- function(e) {
  if (is.character(e)) return(e)
  if (is.call(e) || is.pairlist(e)) return(unlist(lapply(as.list(e), .textos_de)))
  character()
}

# TRUE para los textos con palabras en inglés frecuentes en mensajes (sin las siglas R-hat y ESS, que se usan igual
# en español). Una prueba barata: no reemplaza leer los mensajes, pero atrapa un mensaje escrito en inglés.
.en_ingles <- function(textos)
  grepl(paste0("\\b(the|is|are|must|should|not|invalid|missing|found|expected|cannot|does|was|were|with|and|or|of|",
               "to|too|for|only|than|this|that|which|please|chains|draws)\\b"), textos, perl = TRUE)

# Tokens de un archivo R (getParseData): distingue los comentarios del código aunque un «#» vaya dentro de un texto.
tokens_r <- function(archivo) {
  pd <- utils::getParseData(parse(text = leer_utf8(archivo), keep.source = TRUE))
  pd[pd$terminal, c("line1", "token", "text")]
}

# Archivos que ve el usuario: el código, los archivos instalados, la ayuda, las guías (Quarto), el estilo del sitio
# y la portada del paquete.
archivos_visibles <- function(r) {
  arch <- c(list.files(file.path(r, c("R", "inst", "vignettes", "man", "pkgdown")), recursive = TRUE,
                       full.names = TRUE, pattern = "[.](R|Rmd|qmd|Rd|yaml|yml|md|csv|cpp|scss|css)$"),
            file.path(r, c("README.Rmd", "README.md", "DESCRIPTION", "NEWS.md", "_pkgdown.yml")))
  arch[file.exists(arch)]
}

# Términos prohibidos en lo que ve el usuario (expresiones regulares de Perl, con su motivo). «cdc» se busca sin
# distinguir mayúsculas y sin una letra o un número delante, para no confundirlo con una suma de control hexadecimal.
.PROHIBIDOS <- c(
  "(?i)(?<![a-z0-9])cdc"                            = "nombre de una institución",
  "(?i)(?<![a-z0-9])c[.] ?d[.] ?c[.]"               = "nombre de una institución",
  "(?i)entrega"                                     = "vocabulario de un encargo (entrega, entregable)",
  "spec \u00a7|\u00a7 ?[0-9]"                       = "referencia a un documento de trabajo",
  "(?i)\\bfase [0-9]"                               = "fase de un plan de trabajo",
  "(?i)\\bdesviaci(o|\u00f3)n [0-9]+(?![.,]?[0-9])" = "desviación numerada de un plan de trabajo",
  "toy_pad"                                         = "nombre de datos internos",
  "(?i)cuyabamba"                                   = "datos de ejemplo de una versión anterior",
  "\\brunner\\b"                                    = "jerga interna",
  "\\brenv\\b"                                      = "jerga interna",
  "\\bVault\\b"                                     = "jerga interna",
  "(?i)monorepo"                                    = "jerga interna",
  "Review Focus"                                    = "referencia a un documento de trabajo",
  "20[0-9]{2}-[01][0-9]-[0-3][0-9]:"                = "entrada fechada de una bitácora",
  "\\.dev/"                                         = "carpeta de trabajo local",
  "(?i)\\btarea [0-9]"                              = "tarea de un plan de trabajo",
  "(?i)\\btask [0-9]"                               = "tarea de un plan de trabajo",
  "\\bWF[0-9]"                                      = "flujo de trabajo interno",
  "\\bcompat/"                                      = "carpeta de una versión anterior")

# El alias dl_export_cdc() (nombre anterior de dl_consolidar()) es la única excepción, y solo en su archivo y en su
# página de ayuda.
.PERMITIDOS <- list("R/nombres_anteriores.R" = "dl_export_cdc", "man/dl_nombres_anteriores.Rd" = "dl_export_cdc")

test_that("sin términos prohibidos ni jerga interna en el código, la ayuda, las guías y los archivos instalados", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  hallazgos <- character()
  for (a in archivos_visibles(r)) {
    rel <- substring(a, nchar(r) + 2L)
    lin <- leer_utf8(a)
    for (permitido in .PERMITIDOS[[rel]]) lin <- gsub(permitido, "", lin, fixed = TRUE)
    for (p in names(.PROHIBIDOS)) {
      n <- grep(p, lin, perl = TRUE)
      if (length(n)) hallazgos <- c(hallazgos, sprintf("%s:%d: %s", rel, n, .PROHIBIDOS[[p]]))
    }
  }
  expect_sin_hallazgos(hallazgos, "términos prohibidos")
})

test_that("sin términos prohibidos en los nombres de archivos y carpetas", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  rel <- list.files(file.path(r, c("R", "inst", "vignettes", "man", "pkgdown")), recursive = TRUE, include.dirs = TRUE)
  hallazgos <- character()
  for (p in names(.PROHIBIDOS)) {
    n <- grep(p, rel, perl = TRUE, value = TRUE)
    if (length(n)) hallazgos <- c(hallazgos, sprintf("%s: %s", n, .PROHIBIDOS[[p]]))
  }
  expect_sin_hallazgos(hallazgos, "nombres prohibidos")
})

test_that("el código R es ASCII fuera de los comentarios", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  hallazgos <- character()
  for (a in list.files(file.path(r, "R"), pattern = "[.]R$", full.names = TRUE)) {
    tk <- tokens_r(a)
    codigo <- tk[tk$token != "COMMENT", ]
    n <- unique(codigo$line1[grepl("[^\\x01-\\x7F]", codigo$text, perl = TRUE)])
    if (length(n)) hallazgos <- c(hallazgos, sprintf("R/%s:%d: carácter no ASCII (escríbelo como \\uXXXX)",
                                                     basename(a), n))
  }
  expect_sin_hallazgos(hallazgos, "código no ASCII")
})

test_that("el código no usa stopifnot(): sus mensajes son en inglés y no nombran la función del usuario", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  hallazgos <- character()
  for (a in list.files(file.path(r, "R"), pattern = "[.]R$", full.names = TRUE)) {
    tk <- tokens_r(a)
    n <- unique(tk$line1[tk$token == "SYMBOL_FUNCTION_CALL" & tk$text == "stopifnot"])
    if (length(n)) hallazgos <- c(hallazgos, sprintf("R/%s:%d: stopifnot() (usa un stop() en español)", basename(a), n))
  }
  expect_sin_hallazgos(hallazgos, "stopifnot()")
})

test_that("los métodos print() hablan español", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  hallazgos <- character()
  for (a in list.files(file.path(r, "R"), pattern = "[.]R$", full.names = TRUE)) {
    for (e in parse(text = leer_utf8(a), keep.source = FALSE)) {
      if (!(is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]]) &&
            startsWith(as.character(e[[2L]]), "print."))) next
      textos <- .textos_de(e[[3L]])
      malos <- textos[.en_ingles(textos)]
      if (length(malos))
        hallazgos <- c(hallazgos, sprintf("R/%s: %s: «%s»", basename(a), as.character(e[[2L]]), malos))
    }
  }
  expect_sin_hallazgos(hallazgos, "textos en inglés en métodos print()")
})

test_that("los comentarios no llevan fechas de bitácora ni referencias a documentos de trabajo", {
  r <- raiz_fuente(); skip_if(is.null(r), "solo desde el código fuente")
  prohibido <- c("20[0-9]{2}-[01][0-9]-[0-3][0-9]"                 = "fecha",
                 "\u00a7"                                          = "sección de un documento de trabajo",
                 "\\bspec\\b"                                      = "referencia a una especificación",
                 "(?i)\\bdesviaci(o|\u00f3)n [0-9]+(?![.,]?[0-9])" = "desviación numerada de un plan de trabajo",
                 "\\bP[12]\\b"                                     = "nombre interno de un producto")
  comentarios <- function(a) {
    if (grepl("[.]R$", a)) {
      tk <- tokens_r(a); tk <- tk[tk$token == "COMMENT", ]
      return(stats::setNames(tk$text, tk$line1))
    }
    lin <- leer_utf8(a)                        # C++: todo el archivo (sus textos también los ve el usuario)
    stats::setNames(lin, seq_along(lin))
  }
  hallazgos <- character()
  for (a in c(list.files(file.path(r, "R"), pattern = "[.]R$", full.names = TRUE),
              list.files(file.path(r, "inst", "cpp"), full.names = TRUE))) {
    com <- comentarios(a)
    for (p in names(prohibido)) {
      n <- grep(p, com, perl = TRUE)
      if (length(n))
        hallazgos <- c(hallazgos, sprintf("%s:%s: %s", substring(a, nchar(r) + 2L), names(com)[n], prohibido[[p]]))
    }
  }
  expect_sin_hallazgos(hallazgos, "comentarios con historia interna")
})
