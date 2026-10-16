# Escena 02, las tasas por edad: los nudos y la interpolación. Las tasas de la guía «El modelo» (sección 2, valores
# inventados para ilustrar) resueltas con dl_edo(), la matriz W que construye el paquete (.dl_base_interp) en la malla
# h/2 que usa dl_edo() y en unas edades sueltas, y la solución con el nudo de i en 60 movido (log i_60 + log 3, la
# incidencia de ese nudo al triple), para que la auditoría compare con el paquete lo que la escena calcula cuadro a
# cuadro con DL.modelo.edo().
exportar <- function(ctx) {
  nudos <- c(30, 40, 50, 60, 70, 80, 95)
  log_i <- log(c(0.5, 0.8, 1.5, 2.5, 4, 6, 7) / 1000)
  log_f <- log(c(10, 12, 15, 20, 30, 50, 90) / 1000)
  edad_inicio <- 30
  s <- dl_edo(log_i, log_f, nudos, edad_inicio = edad_inicio)

  # W en la malla h/2 de dl_edo() (edad_fin = 99, nsub = 5): las filas que recorre el cursor de la escena.
  edades_media <- dismodlite:::.dl_edades_media(edad_inicio, 99, 5L)
  W_media <- dismodlite:::.dl_base_interp(nudos, edades_media)
  # Filas sueltas: la edad 63 (entre los nudos 60 y 70), un nudo (60) y una edad después del último (97).
  edades_fila <- c(63, 60, 97)
  W_filas <- dismodlite:::.dl_base_interp(nudos, edades_fila)

  # El nudo que se mueve: log i en 60 sube log 3 (la incidencia de ese nudo al triple).
  k_mueve <- which(nudos == 60)
  delta <- log(3)
  log_i_mov <- log_i
  log_i_mov[k_mueve] <- log_i_mov[k_mueve] + delta
  s_mov <- dl_edo(log_i_mov, log_f, nudos, edad_inicio = edad_inicio)

  list(
    fuente = "Valores inventados de la guía «El modelo» (sección 2), resueltos con dl_edo(); el nudo ×3 es ilustrativo",
    nudos = nudos, log_i = log_i, log_f = log_f, edad_inicio = edad_inicio, remision = 0,
    edades = s$edades, p = s$p, i = s$i, f = s$f,
    edades_media = edades_media,
    W_media = lapply(seq_len(nrow(W_media)), function(j) W_media[j, ]),
    edades_fila = edades_fila,
    W_filas = lapply(seq_len(nrow(W_filas)), function(j) W_filas[j, ]),
    nudo_mueve = 60, delta_log_i = delta, log_i_mov = log_i_mov,
    p_mov = s_mov$p, i_mov = s_mov$i, f_mov = s_mov$f
  )
}
