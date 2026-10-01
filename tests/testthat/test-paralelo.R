test_that(".dl_paralelo en Windows avisa y da el mismo resultado que un núcleo", {
  f <- function(i) i^2
  expect_warning(r <- .dl_paralelo(4L, f, cores = 2L, .os = "windows"), "Windows")
  expect_identical(r, lapply(1:4, f))
})
test_that("el aviso de Windows empieza con la función que llamó el usuario", {
  original <- dismodlite:::.dl_paralelo
  local_mocked_bindings(.dl_paralelo = function(n, FUN, cores = 1L, .os) original(n, FUN, cores, .os = "windows"))
  lp <- function(theta) -0.5 * sum(theta^2)
  expect_warning(dl_mh_cadenas(lp, c(a = 0, b = 0), list(1:2), chains = 2, iter = 400, warmup = 200, seed = 1,
                               cores = 3),
                 "^dl_mh_cadenas\\(\\): Windows .*1 proceso en lugar de 3")
})
test_that("una cadena que falla en un proceso hijo: un solo mensaje, sin el aviso en inglés de mclapply", {
  skip_on_os("windows")
  avisos <- character()
  error <- tryCatch(withCallingHandlers(
    dl_mh_cadenas(function(theta) -Inf, c(a = 0, b = 0), list(1:2), chains = 2, iter = 400, warmup = 200, seed = 1,
                  cores = 2),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = conditionMessage)
  expect_match(error, "^dl_mh_cadenas\\(\\): fall\u00f3 una cadena: la log-posterior no es finita")
  expect_length(avisos, 0L)
})
test_that(".dl_paralelo con un núcleo no avisa", {
  expect_silent(r <- .dl_paralelo(3L, function(i) i, cores = 1L, .os = "windows"))
  expect_identical(unlist(r), 1:3)
})
test_that(".dl_paralelo con fork (unix) conserva el orden", {
  skip_on_os("windows")
  expect_identical(unlist(.dl_paralelo(6L, function(i) i * 10L, cores = 2L)), (1:6) * 10L)
})
test_that("una tarea que falla en un proceso hijo detiene con su mensaje, sin repetir la función", {
  skip_on_os("windows")
  r <- suppressWarnings(.dl_paralelo(2L, function(i) if (i == 2L) stop("sin memoria") else i, cores = 2L))
  # llamada directamente (sin una función exportada en la pila), el mensaje empieza con «dismodlite:»
  expect_error(.dl_exigir_sin_fallos(r, "una cadena"), "^dismodlite: fall\u00f3 una cadena: sin memoria$",
               class = "dl_error")
  # un error del paquete en la tarea llega con su detalle, sin su función delante
  r <- suppressWarnings(.dl_paralelo(2L, function(i) .dl_stop("sin %s", "disco"), cores = 2L))
  expect_error(.dl_exigir_sin_fallos(r, "una cadena"), "^dismodlite: fall\u00f3 una cadena: sin disco$")
  expect_identical(.dl_exigir_sin_fallos(list(1L, 2L), "una cadena"), list(1L, 2L))
})
