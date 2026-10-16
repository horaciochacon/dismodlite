# Escena 04, de las edades simples a las bandas: la media posterior por edad simple de p, i (1 - p) y p f del ajuste
# nacional del ejemplo (hombres), la población nacional de hombres de 2023 por banda fina y, para cada intervalo que
# muestra la escena, los pesos w_a y el promedio q que calcula el propio paquete (R/bandas.R):
#   - .dl_pesos_intervalo() y .dl_q_intervalos(), con la media posterior por edad;
#   - .dl_agregar_bandas() sobre las simulaciones del ajuste, promediada después sobre las simulaciones (la media es
#     lineal: debe coincidir con lo anterior). La escena recalcula los pesos en JavaScript y la auditoría compara.
exportar <- function(ctx) {
  b <- ctx$insumos()
  f <- ctx$ajuste()
  sexo <- 1L
  loc <- b$loc_ancla

  # Media posterior por edad de cada cantidad (calculada simulación por simulación y promediada).
  dq <- f$draws_q[location_id == loc & sex_id == sexo]
  medias <- dq[, list(p = mean(p), ipop = mean(ipop), pf = mean(p * f)), by = edad][order(edad)]
  edades <- medias$edad

  bandas <- b$bandas_pobl
  pobl <- b$poblacion[location_id == loc & sex_id == sexo]
  N <- pobl$val[match(bandas$age_group_id, pobl$age_group_id)]

  # La misma población con las bandas de 80 años y más juntas en una banda abierta (80+, age_group_id 21): las mismas
  # personas, otra partición. Así se ve el caso de bandas con distinto número de edades (80+ tiene 20 en la malla).
  finas80 <- bandas$age_start >= 80
  bandas80 <- rbind(bandas[!finas80], data.table::data.table(age_group_id = 21L, age_start = 80, age_end = 125))
  pobl80 <- rbind(pobl[!age_group_id %in% bandas$age_group_id[finas80], list(age_group_id, val)],
                  data.table::data.table(age_group_id = 21L, val = sum(N[finas80])))

  # Promedio del paquete con la media posterior: pesos y q de R/bandas.R.
  caso <- function(inicio, fin, bnd, pob) {
    pw <- dismodlite:::.dl_pesos_intervalo(edades, inicio, fin, pob, bnd)
    list(inicio = inicio, fin = fin, edades = edades[pw$idx], w = pw$w,
         q_p = dismodlite:::.dl_q_intervalos(medias$p, list(pw)),
         q_ipop = dismodlite:::.dl_q_intervalos(medias$ipop, list(pw)),
         q_pf = dismodlite:::.dl_q_intervalos(medias$pf, list(pw)))
  }
  # Promedio del paquete sobre las simulaciones del ajuste (.dl_agregar_bandas), y su media.
  por_simulacion <- function(inicio, fin, bb, integrando) {
    bnd <- data.table::data.table(sex_id = sexo, age_group_id = 0L, age_start = inicio, age_end = fin)
    mean(dismodlite:::.dl_agregar_bandas(f, bb, bnd, integrando, locs = loc)$val)
  }
  b80 <- b
  b80$bandas_pobl <- bandas80
  b80$poblacion <- rbind(b$poblacion[!(location_id == loc & sex_id == sexo)],
                         pobl80[, list(location_id = loc, location_level = 0L, year = pobl$year[1], sex_id = sexo,
                                       age_group_id, val, acquisition_id = "poblacion")])

  c1 <- caso(40, 45, bandas, pobl)
  c2 <- caso(50, 65, bandas, pobl)
  c3 <- caso(60, 125, bandas80, pobl80)
  c1$q_sim <- por_simulacion(40, 45, b, function(d) d$p)
  c2$q_sim <- por_simulacion(50, 65, b, function(d) d$p)
  c2$q_sim_ipop <- por_simulacion(50, 65, b, function(d) d$ipop)
  c2$q_sim_pf <- por_simulacion(50, 65, b, function(d) d$p * d$f)
  c3$q_sim <- por_simulacion(60, 125, b80, function(d) d$p)
  c3$q_sim_ipop <- por_simulacion(60, 125, b80, function(d) d$ipop)
  c3$q_sim_pf <- por_simulacion(60, 125, b80, function(d) d$p * d$f)
  # El promedio de la banda 40-44 del ancla con la función de las tablas de salida (.dl_q_bandas).
  c1$q_bandas_ancla <- mean(dismodlite:::.dl_q_bandas(f, b, locs = loc)[sex_id == sexo & age_group_id == 13L]$val)
  # Pesos sin dividir por n_b (la regla de la versión 2.2.0), para mostrar el error que corrige la división.
  m3 <- pobl80$val[match(bandas80$age_group_id[dismodlite:::.dl_banda_de(c3$edades, bandas80)], pobl80$age_group_id)]
  c3$w_sin_n <- m3 / sum(m3)

  ancla <- b$prior_gbd[measure_id == 5L & sex_id == sexo][order(age_start)]
  list(
    fuente = paste0("Datos sintéticos del ejemplo (causa 9100, hombres, 2023): media posterior por edad del ajuste ",
                    "nacional, ancla y población. Caso 3: 80-84 … 95+ juntas en 80+"),
    edad_inicio = b$cfg$edad_inicio, edades = edades, p = medias$p, ipop = medias$ipop, pf = medias$pf,
    bandas = list(id = bandas$age_group_id, inicio = bandas$age_start, fin = bandas$age_end, N = N),
    bandas80 = list(id = bandas80$age_group_id, inicio = bandas80$age_start, fin = bandas80$age_end,
                    N = pobl80$val[match(bandas80$age_group_id, pobl80$age_group_id)]),
    ancla = list(inicio = ancla$age_start, fin = ancla$age_end, val = ancla$val, lower = ancla$lower,
                 upper = ancla$upper),
    casos = list(c1, c2, c3)
  )
}
