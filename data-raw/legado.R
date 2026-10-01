# Piezas comunes de los scripts que corren el código de la versión 0.2.2 (etiqueta git v0.2.2) sobre los datos de
# ejemplo: referencia_legado.R y verificar_acs_legado.R. Se cargan con source() desde la raíz del repositorio.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Carga el código de v0.2.2 desde un worktree temporal de git (en tempdir(): TMPDIR decide dónde), que se borra al
# salir de la función que llama (`envir`). Las variables de entorno que lee ese código se fijan solo en este
# proceso: DISMODLITE_DIR = el worktree (de ahí salen los esquemas y la versión) y CDC_TOOLS_REPO, DATA_ROOT y
# CATALOGOS_DIR vacías, para que ninguna ruta por defecto apunte fuera de los datos. Devuelve list(e = entorno con
# las funciones de v0.2.2, dir = carpeta del worktree).
cargar_legado <- function(raiz_repo, envir = parent.frame()) {
  git <- function(...) {
    out <- suppressWarnings(system2("git", c("-C", shQuote(raiz_repo), ...), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0L)
      stop("git ", paste(c(...), collapse = " "), ":\n", paste(out, collapse = "\n"))
    out
  }
  wt <- tempfile("dismodlite_v0.2.2_")
  git("worktree", "add", "--detach", shQuote(wt), "v0.2.2")
  withr::defer(try({ git("worktree", "remove", "--force", shQuote(wt)); git("worktree", "prune") }), envir = envir)
  cat(sprintf("C\u00f3digo de la versi\u00f3n 0.2.2: %s (worktree %s)\n", git("rev-parse", "v0.2.2^{commit}"), wt))
  Sys.setenv(DISMODLITE_DIR = wt, CDC_TOOLS_REPO = "", DATA_ROOT = "", CATALOGOS_DIR = "")
  source(file.path(wt, "load.R"), local = TRUE)
  e <- dismodlite_load(wt)
  cat(sprintf("dl_version() = %s\n", e$dl_version()))
  list(e = e, dir = wt)
}

# Carga en el entorno global el arnés de compatibilidad (tests/testthat/helper-arnes-*.R): escenarios, API
# (api_legado(), rutas_acs()), normalización y escritura de las referencias.
cargar_arnes <- function(raiz_repo) {
  arnes <- sort(Sys.glob(file.path(raiz_repo, "tests", "testthat", "helper-arnes-*.R")), method = "radix")
  if (!length(arnes)) stop("no se encontr\u00f3 el arn\u00e9s (tests/testthat/helper-arnes-*.R) en ", raiz_repo)
  for (f in arnes) source(f)
}
