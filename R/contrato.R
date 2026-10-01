# El contrato de insumos (ver ?dl_tablas): las tablas que el modelo necesita, con un eje común de dimensiones
# (causa, ubicación, año, sexo y banda de edad) y sus valores. Su única definición es inst/referencia/tablas.csv; de
# ahí salen la validación, las plantillas, la ayuda y los mensajes. Los lectores de fuentes conocidas (GBD Results,
# GHDx, particiones de severidad) están en R/lectores.R; la traducción al formato completo, en
# R/contrato_traduccion.R.

# ---- Constantes ----

# Las tablas del contrato, en el orden en que se documentan.
.DL_TABLAS <- c("ubicaciones", "poblacion", "ancla", "covariables", "betas", "datos", "severidad", "fuentes_gbd",
                "poblacion_detalle")

# El eje: las dimensiones que comparten las tablas. Una columna del eje ausente en una tabla significa que la tabla
# no varía en esa dimensión (sin causa: todas; sin ubicacion: la nacional; sin anio: todos; sin sexo: ambos; sin
# edades: todas las edades).
.DL_EJE <- c("causa", "ubicacion", "anio", "sexo", "edad_inicio", "edad_fin")

# Fin de la banda de edad abierta («80 y más»): el age_end del catálogo de edades de GBD. Un edad_fin vacío se lee así.
.DL_EDAD_ABIERTA <- 125

# Valores de la columna sexo: el código del contrato (en palabras) de cada forma admitida, sin distinguir mayúsculas.
.DL_SEXOS_CONTRATO <- c(hombres = "hombres", mujeres = "mujeres", ambos = "ambos", "1" = "hombres", "2" = "mujeres",
                        "3" = "ambos", male = "hombres", female = "mujeres", both = "ambos")

# ---- La definición ----

# Las columnas de las tablas del contrato (inst/referencia/tablas.csv), una fila por tabla y columna.
.dl_tablas_ref <- function() .dl_leer_memo(.dl_inst_archivo("referencia", "tablas.csv"), function(p)
  utils::read.csv(p, colClasses = "character", na.strings = character(), encoding = "UTF-8"))

# Columnas de `tabla`: todas, las que exige o las del eje.
.dl_columnas_contrato <- function(tabla, cuales = c("todas", "exigidas", "eje")) {
  cuales <- match.arg(cuales)
  t <- .dl_tablas_ref()
  t <- t[t$tabla == tabla, ]
  switch(cuales, todas = t$columna, exigidas = t$columna[t$exige == "si"], eje = t$columna[t$rol == "eje"])
}

# Nombres canónicos de las columnas de `d` (una tabla de `tabla`): sin espacios ni mayúsculas y con los alias de
# tablas.csv. Las columnas que no son del contrato quedan con su nombre (se ignoran después). Error si un alias y su
# nombre canónico vienen los dos.
.dl_normalizar_nombres <- function(d, tabla) {
  d <- data.table::as.data.table(d)
  data.table::setnames(d, tolower(trimws(names(d))))
  t <- .dl_tablas_ref()
  t <- t[t$tabla == tabla & nzchar(t$alias), ]
  for (i in seq_len(nrow(t))) {
    alias <- intersect(strsplit(t$alias[i], "|", fixed = TRUE)[[1L]], names(d))
    if (!length(alias)) next
    if (t$columna[i] %in% names(d) || length(alias) > 1L)
      .dl_stop("la tabla %s trae a la vez %s: son la misma columna; deja solo %s", tabla,
               paste(c(intersect(t$columna[i], names(d)), alias), collapse = " y "), t$columna[i])
    data.table::setnames(d, alias, t$columna[i])
  }
  d
}

# Sexo en el contrato: «hombres», «mujeres» o «ambos» desde cualquiera de las formas de .DL_SEXOS_CONTRATO; NA si no
# es ninguna.
.dl_sexo_contrato <- function(x) unname(.DL_SEXOS_CONTRATO[tolower(trimws(as.character(x)))])
