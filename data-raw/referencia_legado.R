# Genera las referencias del arnés de compatibilidad (tests/testthat/_referencia/E1 ... E12) con el código de la
# versión 0.2.2 (etiqueta git v0.2.2) sobre los datos de ejemplo inst/extdata/acs_peru_completo. Los escenarios, la
# normalización y la escritura viven en el arnés tests/testthat/helper-arnes-*.R, el mismo que usa la prueba
# tests/testthat/test-compatibilidad-legado.R: aquí solo se corren con el código de v0.2.2 (api_legado).
#
# Uso, desde la raíz del repositorio:
#   Rscript data-raw/referencia_legado.R [--salida <carpeta>] [--una-vez] [--escenarios E1,E5]
#
#   --salida      carpeta de las referencias (por defecto tests/testthat/_referencia). La prueba lee de ahí o de la
#                 carpeta que diga DL_REFERENCIA_DIR (así se regeneran las referencias en cada sistema operativo).
#   --una-vez     corre cada escenario una sola vez, sin la comprobación de reproducibilidad.
#   --escenarios  subconjunto de escenarios separados por comas (por defecto E1-E11, y E12 si Rcpp está instalado:
#                 E12 corre E1 con el motor rcpp; sin Rcpp se omite y su referencia no se toca).
#
# El código de v0.2.2 se carga desde un worktree temporal de git (cargar_legado() de data-raw/legado.R, que también
# fija las variables de entorno que ese código lee). Los escenarios pasan todas las rutas de forma explícita.
#
# Reproducibilidad: corre cada escenario dos veces; la segunda con una copia de los datos en otra carpeta, otra
# carpeta de salida y la cache de ajustes vacía. Exige números, tablas y manifiestos normalizados idénticos y lista
# las claves de los manifiestos sin normalizar que cambiaron entre las dos corridas (tienen que estar entre las que
# normalizar_manifiesto() quita o neutraliza). Las claves que dependen de la fecha no cambian dentro de un mismo día:
# esas se quitan por diseño (ver .MANIFIESTO_FUERA en helper-arnes-normalizar.R).
#
# Antes de correr copia los perfiles de consolidación de la etiqueta (compat/config/export_cdc/) a
# <salida>/perfiles_legado/, con el identificador y la descripción neutros (escribir_perfiles_legado del arnés), y
# corre la consolidación con esas copias (la prueba usa los perfiles del paquete, con las mismas columnas). Al final
# vuelve a leer lo escrito y lo compara con lo corrido en modo exacto (tolerancia 0: los números se escriben con los
# dígitos justos para releerlos exactos) e informa el tamaño de las referencias.

source(file.path("data-raw", "legado.R"))   # %||%, cargar_legado(), cargar_arnes()

argumento <- function(args, nombre) {
  k <- match(nombre, args)
  if (is.na(k)) return(NULL)
  if (k == length(args)) stop("referencia_legado.R: falta el valor de ", nombre)
  args[k + 1L]
}

# Rutas (a$b$c) en las que dos listas difieren.
diferencias <- function(a, b, ruta = "") {
  if (is.list(a) && is.list(b)) {
    na <- names(a); nb <- names(b)
    if (is.null(na) && is.null(nb) && length(a) == length(b))
      return(unlist(lapply(seq_along(a), function(i) diferencias(a[[i]], b[[i]], sprintf("%s[%d]", ruta, i)))))
    claves <- union(na, nb)
    if (is.null(claves)) return(ruta)
    return(unlist(lapply(claves, function(k)
      diferencias(a[[k]], b[[k]], if (nzchar(ruta)) paste0(ruta, "$", k) else k))))
  }
  if (identical(a, b)) character() else ruta
}

igual_tabla <- function(a, b) identical(names(a), names(b)) && identical(as.list(a), as.list(b))

main <- function() {
  options(width = 160L, warn = 1L)
  args <- commandArgs(TRUE)
  raiz_repo <- normalizePath(getwd(), winslash = "/")
  raiz_datos <- file.path(raiz_repo, "inst", "extdata", "acs_peru_completo")
  if (!dir.exists(raiz_datos))
    stop("referencia_legado.R: ejecutar desde la ra\u00edz del repositorio (falta inst/extdata/acs_peru_completo)")
  salida <- argumento(args, "--salida") %||% file.path(raiz_repo, "tests", "testthat", "_referencia")
  dos_veces <- !("--una-vez" %in% args)
  ids <- argumento(args, "--escenarios")
  ids <- if (is.null(ids)) NULL else strsplit(ids, ",", fixed = TRUE)[[1]]

  legado <- cargar_legado(raiz_repo)
  cargar_arnes(raiz_repo)   # en el entorno global: correr_escenario(), api_legado(), escribir_referencia(), ...
  if (is.null(ids)) {
    ids <- .ESCENARIOS_BASE
    if (requireNamespace("Rcpp", quietly = TRUE)) ids <- c(ids, .ESCENARIOS_RCPP)
    else cat("Rcpp no est\u00e1 instalado: se omite", .ESCENARIOS_RCPP, "(motor rcpp)\n")
  }
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  perfiles <- escribir_perfiles_legado(file.path(legado$dir, "compat", "config", "export_cdc"),
                                       file.path(salida, "perfiles_legado"))
  api <- api_legado(legado$e, perfiles)

  correr <- function(raiz, carpeta, vuelta) {
    res <- list()
    for (id in ids) {
      t0 <- proc.time()[["elapsed"]]
      res[[id]] <- correr_escenario(id, api, raiz, file.path(carpeta, id))
      cat(sprintf("  [%d] %-4s %5.1f s | %3d par\u00e1metros | %4d celdas | %2d manifiestos | %2d tablas\n", vuelta, id,
                  proc.time()[["elapsed"]] - t0, NROW(res[[id]]$parametros), NROW(res[[id]]$celdas),
                  length(res[[id]]$manifiestos), length(res[[id]]$tablas)))
    }
    res
  }

  tmp <- tempfile("referencia_legado_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat("\n== Primera corrida ==\n")
  t0 <- proc.time()[["elapsed"]]
  res1 <- correr(raiz_datos, file.path(tmp, "salida1"), 1L)
  cat(sprintf("  total %.0f s\n", proc.time()[["elapsed"]] - t0))

  if (dos_veces) {
    cat("\n== Segunda corrida (copia de los datos en otra carpeta, cache vac\u00eda) ==\n")
    raiz2 <- file.path(tmp, "otra carpeta", "acs_peru_completo")
    dir.create(dirname(raiz2), recursive = TRUE)
    if (!file.copy(raiz_datos, dirname(raiz2), recursive = TRUE)) stop("no se pudo copiar ", raiz_datos)
    api$limpiar_cache()
    t0 <- proc.time()[["elapsed"]]
    res2 <- correr(raiz2, file.path(tmp, "salida2"), 2L)
    cat(sprintf("  total %.0f s\n", proc.time()[["elapsed"]] - t0))
    problemas <- character()
    for (id in ids) {
      a <- res1[[id]]; b <- res2[[id]]
      if (!igual_tabla(a$parametros, b$parametros)) problemas <- c(problemas, paste(id, "par\u00e1metros distintos"))
      if (!igual_tabla(a$celdas, b$celdas)) problemas <- c(problemas, paste(id, "celdas distintas"))
      if (!identical(names(a$tablas), names(b$tablas))) problemas <- c(problemas, paste(id, "tablas distintas"))
      else for (t in names(a$tablas)) if (!igual_tabla(a$tablas[[t]], b$tablas[[t]]))
        problemas <- c(problemas, paste(id, "tabla distinta:", t))
      d <- diferencias(a$manifiestos, b$manifiestos)
      if (length(d)) problemas <- c(problemas, paste(id, "manifiesto normalizado distinto en", d))
      crudas <- diferencias(a$manifiestos_crudos, b$manifiestos_crudos)
      if (length(crudas)) cat(sprintf("  %s: claves del manifiesto sin normalizar que cambian entre corridas: %s\n",
                                      id, paste(crudas, collapse = ", ")))
    }
    if (length(problemas)) stop("las dos corridas con la versi\u00f3n 0.2.2 difieren:\n  ",
                                paste(problemas, collapse = "\n  "))
    cat("  Las dos corridas dan los mismos n\u00fameros, tablas y manifiestos normalizados.\n")
  }

  cat(sprintf("\n== Escritura en %s ==\n", salida))
  for (id in ids) escribir_referencia(res1[[id]], file.path(salida, id))
  # la plataforma de estas referencias: la prueba guardada solo las compara en ella (saltar_si_otra_plataforma)
  writeLines(plataforma_actual(), file.path(salida, "plataforma.txt"))

  # Lo escrito se relee igual que en la prueba y coincide con lo corrido bit a bit (modo exacto).
  Sys.setenv(DL_REFERENCIA_DIR = salida, DL_ARNES_EXACTO = "1")
  for (id in ids) comparar_con_referencia(id, res1[[id]])
  Sys.unsetenv(c("DL_REFERENCIA_DIR", "DL_ARNES_EXACTO"))
  cat("  Las referencias rele\u00eddas coinciden exactamente con lo corrido.\n")

  archivos <- list.files(salida, recursive = TRUE, full.names = TRUE)
  tam <- tapply(file.size(archivos), sub("/.*$", "", substring(archivos, nchar(salida) + 2L)), sum)
  print(round(tam / 1024, 1))
  total <- sum(file.size(archivos)) / 2^20
  cat(sprintf("  Tama\u00f1o total: %.2f MB en %d archivos%s\n", total, length(archivos),
              if (total > 2) " (por encima del objetivo de 2 MB)" else ""))
  invisible(0L)
}

main()
