# Entorno de las pruebas: se fija antes de correrlas y se restaura al terminar (teardown_env()).
#
# - Sin las variables de entorno que el paquete lee como respaldo de las rutas (carpeta de datos DATA_ROOT y carpeta
#   de catálogos CATALOGOS_DIR): así el resultado no depende de la sesión de quien corre las pruebas. Las pruebas que
#   necesitan una de estas variables la fijan con withr::local_envvar().
# - Caché de usuario (tools::R_user_dir(), donde el motor rcpp guarda el C++ compilado) en una carpeta temporal:
#   R CMD check no debe dejar archivos en la carpeta personal del usuario. Compilar de nuevo tarda unos segundos.
withr::local_envvar(c(DATA_ROOT = NA, CATALOGOS_DIR = NA),
                    .local_envir = testthat::teardown_env())
withr::local_envvar(
  R_USER_CACHE_DIR = withr::local_tempdir(pattern = "cache-usuario-", .local_envir = testthat::teardown_env()),
  .local_envir = testthat::teardown_env())
