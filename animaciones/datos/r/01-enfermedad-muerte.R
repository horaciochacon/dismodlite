# Escena 01, el modelo enfermedad-muerte: la curva de la prevalencia que resuelve dl_edo() con las tasas de la
# guía «El modelo» (sección 2, valores inventados para ilustrar), y las tasas en cada edad entera.
exportar <- function(ctx) {
  nudos <- c(30, 40, 50, 60, 70, 80, 95)
  log_i <- log(c(0.5, 0.8, 1.5, 2.5, 4, 6, 7) / 1000)
  log_f <- log(c(10, 12, 15, 20, 30, 50, 90) / 1000)
  s <- dl_edo(log_i, log_f, nudos, edad_inicio = 30)
  list(
    fuente = "Valores inventados de la guía «El modelo» (sección 2), resueltos con dl_edo()",
    nudos = nudos, log_i = log_i, log_f = log_f, edad_inicio = 30, remision = 0,
    edades = s$edades, p = s$p, i = s$i, f = s$f
  )
}
