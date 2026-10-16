# Exporta desde dismodlite los números que dibujan las animaciones.
#
# Uso (desde animaciones/):
#   Rscript datos/exportar.R                     todas las escenas que tienen datos/r/<escena>.R
#   Rscript datos/exportar.R 01-enfermedad-muerte 08-mcmc
#
# Cada escena tiene su guion de datos en datos/r/<escena>.R, que define exportar(ctx) y devuelve una lista. Este
# script la escribe en datos/<escena>.js como globalThis.DL_DATOS["<escena>"], un script clásico que la escena carga
# con <script src> (funciona abriendo el HTML con doble clic, sin servidor).
#
# ctx da los objetos del proyecto de ejemplo del paquete, calculados una sola vez y solo si una escena los pide:
#   ctx$proyecto()   dl_proyecto(dl_ejemplo(), causa = 9100)
#   ctx$insumos()    dl_insumos() de ese proyecto
#   ctx$ajuste()     dl_ajustar() con 4 cadenas de 4000 iteraciones (2000 de calentamiento), 1000 simulaciones,
#                    semilla 1: el ajuste nacional del ejemplo
#   ctx$semilla      1
# Todos los números del ejemplo son sintéticos (la «arteriopatía crónica sintética», causa 9100, con la geografía de
# Perú): cada escena que los usa lo dice en su pie.
suppressPackageStartupMessages(library(dismodlite))
# Los textos de los guiones de datos llevan tildes: UTF-8 aunque Rscript arranque en el locale C.
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

argumentos <- commandArgs(trailingOnly = TRUE)
este_archivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
raiz <- normalizePath(file.path(dirname(este_archivo), ".."))

memo <- function(f) {
  valor <- NULL
  function() {
    if (is.null(valor)) valor <<- f()
    valor
  }
}

ctx <- new.env()
ctx$semilla <- 1
ctx$proyecto <- memo(function() dl_proyecto(dl_ejemplo(), causa = 9100))
ctx$insumos <- memo(function() suppressMessages(dl_insumos(ctx$proyecto())))
ctx$opciones <- memo(function() dl_opciones_mcmc(simulaciones = 1000, cadenas = 4, iteraciones = 4000,
                                                 calentamiento = 2000))
ctx$ajuste <- memo(function() dl_ajustar(ctx$insumos(), ctx$opciones(), semilla = ctx$semilla))

escribir <- function(escena, lista) {
  lista$meta <- list(escena = escena, dismodlite = as.character(utils::packageVersion("dismodlite")),
                     generado_por = paste0("datos/r/", escena, ".R"))
  json <- jsonlite::toJSON(lista, auto_unbox = TRUE, digits = NA, na = "null", null = "null")
  destino <- file.path(raiz, "datos", paste0(escena, ".js"))
  writeLines(c(paste0("// Generado por datos/exportar.R desde datos/r/", escena, ".R. No editar a mano."),
               paste0("(globalThis.DL_DATOS = globalThis.DL_DATOS || {})[\"", escena, "\"] = ", json, ";")),
             destino, useBytes = TRUE)
  message("escrito ", destino, " (", format(file.size(destino), big.mark = " "), " bytes)")
}

guiones <- sort(list.files(file.path(raiz, "datos", "r"), pattern = "\\.R$", full.names = TRUE))
escenas <- sub("\\.R$", "", basename(guiones))
if (length(argumentos)) {
  faltan <- setdiff(argumentos, escenas)
  if (length(faltan)) stop("No hay datos/r/", faltan[1], ".R")
  guiones <- guiones[escenas %in% argumentos]
  escenas <- escenas[escenas %in% argumentos]
}
for (k in seq_along(guiones)) {
  entorno <- new.env(parent = globalenv())
  eval(parse(guiones[k], encoding = "UTF-8"), envir = entorno)
  if (!exists("exportar", envir = entorno, inherits = FALSE)) stop(guiones[k], " no define exportar(ctx)")
  escribir(escenas[k], entorno$exportar(ctx))
}
