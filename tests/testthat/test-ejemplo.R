# Ayudantes de los datos de ejemplo (dl_ejemplo, dl_rutas_ejemplo, dl_configuracion_ejemplo) y argumentos en
# español de dl_rutas().

test_that("dl_ejemplo devuelve rutas que existen y falla con un nombre desconocido", {
  expect_true(dir.exists(dl_ejemplo()))
  # dl_ejemplo() es el proyecto en el contrato de insumos; el mismo proyecto en el formato completo está al lado
  expect_identical(dl_ejemplo(), system.file("extdata", "acs_peru", package = "dismodlite"))
  expect_identical(dirname(dl_ejemplo()), dirname(ruta_acs()))
  for (f in c("config/9100.yaml", "ubicaciones.csv", "poblacion.csv", "proxies_crudos.csv", "betas.csv",
              "datos.csv", "severidad.csv", "verdad.csv"))
    expect_true(file.exists(dl_ejemplo(f)), info = f)
  expect_length(list.files(dl_ejemplo("ancla")), 1L)
  # cada tabla del proyecto es una tabla del contrato válida (sus descargas, por su lector)
  for (tabla in c("ubicaciones", "poblacion", "betas", "datos", "severidad", "proxies_crudos"))
    expect_s3_class(dl_tabla(tabla, dl_ejemplo(paste0(tabla, ".csv"))), "dl_tabla")
  expect_s3_class(dl_tabla("ancla", dl_ejemplo("ancla")), "dl_tabla")
  expect_s3_class(dl_tabla("covariables", dl_ejemplo("covariables"), ubicacion_gbd = 123), "dl_tabla")
  # la configuración no declara lo que dicen las tablas
  s <- yaml::read_yaml(dl_ejemplo("config", "9100.yaml"))
  expect_false(any(c("covariables", "ubicacion_nacional", "ubicacion_gbd") %in% names(s)))
  expect_true(dir.exists(ejemplo_completo("particion", "acs_v1")))
  expect_error(dl_ejemplo("no_existe.csv"), "no_existe[.]csv")
})

test_that("dl_ejemplo(copiar_en =) copia el proyecto entero en una carpeta nueva o vacía, y no sobrescribe", {
  destino <- file.path(withr::local_tempdir(), "nueva", "mi proyecto")        # la crea, con las intermedias
  expect_identical(dl_ejemplo(copiar_en = destino), destino)
  expect_setequal(list.files(destino, recursive = TRUE), list.files(dl_ejemplo(), recursive = TRUE))
  expect_identical(unname(tools::md5sum(file.path(destino, "datos.csv"))),
                   unname(tools::md5sum(dl_ejemplo("datos.csv"))))
  expect_true(file.access(file.path(destino, "config", "9100.yaml"), 2L) == 0L)   # se puede editar
  expect_s3_class(dl_proyecto(destino, 9100), "dl_proyecto")
  # una carpeta con archivos no se toca
  expect_error(dl_ejemplo(copiar_en = destino), "ya tiene archivos: `copiar_en` debe ser una carpeta nueva o vacía")
  vacia <- withr::local_tempdir()
  expect_identical(dl_ejemplo(copiar_en = vacia), vacia)
  expect_error(dl_ejemplo(copiar_en = file.path(destino, "datos.csv")), "es un archivo, no una carpeta")
  expect_error(dl_ejemplo("datos.csv", copiar_en = withr::local_tempdir()), "sin partes de la ruta \\(datos.csv\\)")
  expect_error(dl_ejemplo(copiar_en = 3), "`copiar_en` debe ser la ruta de una carpeta")
})

test_that("dl_ejemplo(copiar_en =) no escribe dentro de la instalación del paquete, y no crea nada antes de negarse", {
  error <- "`copiar_en` está dentro de la instalación del paquete"
  copia <- file.path(dl_ejemplo(), "copia")
  withr::defer(unlink(c(copia, file.path(dl_ejemplo("config"), "copia")), recursive = TRUE))
  expect_error(dl_ejemplo(copiar_en = file.path(copia, "mi proyecto")), error, fixed = TRUE)
  expect_false(file.exists(copia))
  expect_error(dl_ejemplo(copiar_en = dl_ejemplo()), error, fixed = TRUE)                  # el ejemplo mismo
  expect_error(dl_ejemplo(copiar_en = system.file(package = "dismodlite")), error, fixed = TRUE)
  # una ruta relativa, también con `..`, se resuelve antes de comprobarla
  withr::with_dir(dl_ejemplo("config"), {
    expect_error(dl_ejemplo(copiar_en = "copia"), error, fixed = TRUE)
    expect_error(dl_ejemplo(copiar_en = file.path("..", "..", "copia")), error, fixed = TRUE)
  })
  expect_false(file.exists(file.path(dl_ejemplo("config"), "copia")))
  # fuera del paquete sí, también en una carpeta cuyo nombre empieza como el del paquete
  expect_false(.dl_en_paquete(file.path(tempdir(), "copia")))
  expect_false(.dl_en_paquete(paste0(system.file(package = "dismodlite"), "_copia")))
})

test_that("dl_rutas_ejemplo(causa) arma los insumos de cada causa con la severidad de esa causa", {
  for (k in 9100:9103) {
    b <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(k), dl_rutas_ejemplo(k)))
    expect_s3_class(b, "dl_bundle")
    expect_true(nrow(b$severidad) > 0L && all(b$severidad$cause_id == k), info = k)
  }
})

test_that("dl_rutas_ejemplo exige la causa, la valida y acepta una configuraci\u00f3n", {
  expect_error(dl_rutas_ejemplo(), "causa")
  expect_error(dl_rutas_ejemplo(9999L), "causa")
  expect_error(dl_rutas_ejemplo(9100L, anio = 2020L), "anio")
  cfg <- dl_configuracion_ejemplo(9102L)
  expect_identical(dl_rutas_ejemplo(cfg), dl_rutas_ejemplo(9102L))
  expect_identical(dl_rutas_ejemplo(9102L, formato = "completo")$severidad, ejemplo_completo("severidad", "9102.csv"))
  expect_error(dl_rutas_ejemplo(9102L, formato = "otro"), "`formato` debe ser \"simple\" o \"completo\"")
  r19 <- dl_rutas_ejemplo(9100L, anio = 2019L)
  expect_identical(attr(r19, "anio_ejemplo"), 2019L)
  expect_identical(names(r19), names(dl_rutas_ejemplo(9100L)))
})

test_that("dl_rutas_ejemplo(anio =) trae los proxies calibrados para ese año", {
  for (a in c(2019L, 2024L)) {
    b <- b24 <- suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L, anio = a),
                                            dl_rutas_ejemplo(9100L, anio = a)))
    expect_gt(nrow(b$cov_proxy), 0L)
    expect_identical(unique(as.integer(b$cov_proxy$year)), a)
    expect_setequal(unique(b$cov_proxy$location_id), sprintf("%02d", 1:25))
  }
  # 2024 no tiene valor nacional de las covariables: cierra en el de 2023, el año del ancla
  h <- b24$cov_proxy[as.integer(covariate_id_gbd) == 1099L]          # haqi
  # la copia de cada año se arma aparte y se renombra: en la carpeta solo quedan las de los años, completas
  raiz <- file.path(tempdir(), "dismodlite_ejemplo")
  expect_true(all(c("2019", "2024") %in% list.files(raiz)))
  expect_true(all(list.files(raiz) %in% c("2019", "2024")))
  cfg19 <- readLines(file.path(raiz, "2019", "config", "9101.yaml"))
  expect_identical(grep("^anio:", cfg19, value = TRUE), "anio: 2019")
  expect_identical(unique(as.character(h$ancla_ghdx)), "50.9")
})

test_that("dl_rutas_ejemplo: datos y proxies se quitan o se reemplazan, y ... cambia cualquier pieza", {
  # en el formato completo, las mismas rutas que arma el arnés de compatibilidad con las claves de la versión 0.2.2
  expect_identical(dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE, formato = "completo"),
                   rutas_acs(api_nueva(), ruta_acs(), 9100L, con_datos = FALSE, con_proxies = FALSE))
  expect_identical(dl_rutas_ejemplo(9100L, formato = "completo"), rutas_acs(api_nueva(), ruta_acs(), 9100L))
  sin <- dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE)
  expect_null(sin$datos)
  expect_null(sin$cov_proxy)
  expect_identical(dl_rutas_ejemplo(9100L, datos = "mis_datos.csv")$datos, "mis_datos.csv")
  expect_identical(dl_rutas_ejemplo(9100L, proxies = "mis_proxies.csv")$cov_proxy, "mis_proxies.csv")
  expect_null(dl_rutas_ejemplo(9101L, severidad = NULL)$severidad)
  expect_identical(dl_rutas_ejemplo(9100L, poblacion = "p.csv")$poblacion, "p.csv")
  expect_identical(dl_rutas_ejemplo(9100L, std_prior = "a.csv")$std_prior, "a.csv")
  expect_error(dl_rutas_ejemplo(9100L, datos = 3), "datos")
  expect_error(dl_rutas_ejemplo(9100L, "x.csv"), "anio")
})

test_that("dl_rutas: cada argumento en espa\u00f1ol va a su clave interna y cambios acepta las dos", {
  x <- ejemplo_completo("ancla", "prevalencia.csv")
  expect_identical(dl_rutas(ancla_prevalencia = x)$std_prior, x)
  expect_identical(dl_rutas(cambios = list(std_prior = x))$std_prior, x)
  expect_identical(dl_rutas(cambios = list(ancla_prevalencia = x)), dl_rutas(ancla_prevalencia = x))
  expect_s3_class(dl_rutas(), "dl_paths")
  for (arg in names(.DL_CLAVES_RUTAS)) {
    r <- do.call(dl_rutas, stats::setNames(list("x.csv"), arg))
    expect_identical(r[[.DL_CLAVES_RUTAS[[arg]]]], "x.csv", info = arg)
  }
  expect_setequal(unname(.DL_CLAVES_RUTAS), names(dl_rutas()))
  expect_identical(dl_rutas(datos = "a.csv", cambios = list(datos = "b.csv"))$datos, "b.csv")   # cambios va último
  expect_null(dl_rutas(datos = "a.csv", cambios = list(datos = NULL))$datos)
  expect_error(dl_rutas(cambios = list(prevalencia = x)), "desconocida")
  expect_error(dl_rutas(cambios = list(x)), "nombres")
})

test_that("una ruta que falta o no existe se nombra con el argumento de dl_rutas()", {
  expect_error(.dl_path(dl_rutas(), "std_prior"), "ancla_prevalencia")
  expect_error(.dl_path(dl_rutas(ancla_prevalencia = "/no/existe.csv"), "std_prior"), "no existe")
  expect_error(suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L, formato = "completo"),
                                           dl_rutas_ejemplo(9100L, evidencia = NULL, formato = "completo"))),
               "evidencia")
})

test_that("dl_configuracion_ejemplo lee la configuraci\u00f3n del ejemplo con cambios", {
  cfg <- dl_configuracion_ejemplo(9101L, cambios = list(anchor = list(lambda = 0.5)))
  expect_s3_class(cfg, "dl_config")
  expect_identical(cfg$cause_id, 9101L)
  expect_equal(cfg$anchor$lambda, 0.5)
  expect_identical(dl_configuracion_ejemplo(), dl_configuracion(9100L, dl_ejemplo("config")))
})

test_that("la severidad de otra causa se detiene: un subtipo no toma la mezcla de la causa padre", {
  expect_error(suppressMessages(dl_insumos(dl_configuracion_ejemplo(9101L, formato = "completo"),
                                           dl_rutas_ejemplo(9100L, formato = "completo"))),
               "severidad/9100[.]csv.*causa 9100 y la configuración es de la 9101.*dl_rutas_ejemplo\\(9101\\)")
  expect_error(suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9101L))),
               "es de la causa 9101 y la configuración es de la 9100")
})

test_that("el año de la corrida lo fija la configuración; dl_rutas_ejemplo(anio =) solo lo comprueba", {
  expect_error(suppressMessages(dl_insumos(dl_configuracion_ejemplo(9100L), dl_rutas_ejemplo(9100L, anio = 2019L))),
               "dl_rutas_ejemplo\\(anio = 2019\\).*del año 2023.*dl_configuracion_ejemplo\\(9100, anio = 2019\\)")
  c19 <- dl_configuracion_ejemplo(9100L, anio = 2019L)
  expect_identical(c19, dl_configuracion_ejemplo(9100L, cambios = list(years = list(ajuste = 2019L))))
  b <- suppressMessages(dl_insumos(c19, dl_rutas_ejemplo(9100L, anio = 2019L, datos = FALSE, proxies = FALSE)))
  expect_true(all(b$prior_gbd$year == 2019L) && all(b$poblacion$year == 2019L))
  c24 <- dl_configuracion_ejemplo(9100L, anio = 2024L)
  expect_identical(c(as.integer(unlist(c24$years$ajuste)), c24$years$ancla$valor), c(2024L, 2023L))
  expect_true(nzchar(c24$years$ancla$procedencia))
  # lo que va en cambios se aplica después de `anio`
  expect_identical(as.integer(unlist(dl_configuracion_ejemplo(9100L, cambios = list(years = list(ajuste = 2023L)),
                                                              anio = 2019L)$years$ajuste)), 2023L)
  expect_error(dl_configuracion_ejemplo(9100L, anio = 2020L), "`anio` debe ser 2019, 2023 o 2024")
})

test_that("causa, datos, proxies y ... se validan con mensajes de los ayudantes", {
  # un cause_id que no es entero, con el mensaje de toda función (.dl_exigir_causa); uno entero que no es del ejemplo,
  # con las causas del ejemplo
  expect_error(dl_rutas_ejemplo(9100.7), "^dl_rutas_ejemplo\\(\\): `causa` debe ser un cause_id entero.*9100[.]7")
  expect_error(dl_rutas_ejemplo(dl_rutas_ejemplo(9100L)), "`causa` debe ser.*objeto de rutas")
  expect_identical(dl_rutas_ejemplo("9101"), dl_rutas_ejemplo(9101L))
  expect_error(dl_configuracion_ejemplo(502L), "^dl_configuracion_ejemplo\\(\\): `causa` debe ser 9100, 9101")
  expect_identical(dl_rutas_ejemplo(9100L, datos = NULL, proxies = NULL),
                   dl_rutas_ejemplo(9100L, datos = FALSE, proxies = FALSE))
  expect_error(dl_rutas_ejemplo(9100L, poblaccion = "x.csv"),
               "^dl_rutas_ejemplo\\(\\): clave.* desconocida.* en `...`: poblaccion.*quisiste decir `poblacion`")
})

test_that("las rutas de ejemplo no toman covariables_std de la variable de entorno DATA_ROOT", {
  withr::local_envvar(DATA_ROOT = withr::local_tempdir())
  expect_null(dl_rutas_ejemplo(9100L)$cov_std)
  expect_true("cov_std" %in% names(dl_rutas_ejemplo(9100L)))
  expect_identical(dl_rutas_ejemplo(9100L, covariables_std = "c")$cov_std, "c")
})

test_that("print de las rutas: cada pieza con su argumento de dl_rutas()", {
  r <- dl_rutas_ejemplo(9100L, anio = 2019L, datos = FALSE)
  expect_output(print(r), "<dl_paths>")
  expect_output(print(r), "ancla_prevalencia +[^\n]*prevalencia[.]csv")
  expect_output(print(r), "datos +\\(no dada\\)")
  expect_output(print(r), "año del ejemplo: 2019")
})

test_that("una ruta que falta nombra la función que llamó el usuario y el argumento de las rutas", {
  completa <- dl_configuracion_ejemplo(9100L, formato = "completo")
  expect_error(dl_insumos(completa),
               "^dl_insumos\\(\\): falta la ruta de «evidencia» en `rutas`.*dl_rutas_ejemplo\\(causa\\)")
  # un proyecto simple no tiene evidencia: la primera pieza que falta es otra
  expect_error(dl_insumos(dl_configuracion_ejemplo(9100L)), "^dl_insumos\\(\\): falta la ruta de «extraccion»")
  # con un nombre anterior, el mensaje nombra esa función y cita el argumento con su nombre nuevo (`rutas`)
  expect_error(dl_bundle(completa), "^dl_bundle\\(\\): falta la ruta .* en `rutas`")
  m <- tryCatch(dl_insumos(completa), error = conditionMessage)
  expect_false(grepl("clave interna|ghdx_store", m))
})
