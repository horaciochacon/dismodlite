# Genera el logo del paquete (hexágono): man/figures/logo.png y man/figures/logo.svg.
#
# El dibujo es una figura real del modelo, sin ejes: la prevalencia por edad (30-99 años, hombres, 2023) del proyecto
# de ejemplo (datos sintéticos, causa 9100), ajustada con dl_ajustar() y llevada a los 25 departamentos con
# dl_cascada():
#   - las 25 curvas departamentales, líneas finas y translúcidas que salen juntas de p(30) = 0 y se abren con la edad;
#   - la incertidumbre de la curva nacional como un degradado: los intervalos del 10 % al 95 % de sus simulaciones,
#     superpuestos y translúcidos (suavizados con un spline, solo para el dibujo), más densos en el centro;
#   - la curva nacional (la media) en ámbar, con un halo;
#   - la banda y las curvas departamentales se desvanecen en las últimas edades (máscara con un degradado).
# Paleta de la marca (la del tema del sitio): verde azulado #0E6B62 (principal), tinta #16211E, fondo #F5F7F6,
# verde azulado claro #4FC2B3 y ámbar #E0A030. Letra: IBM Plex Sans (Google Fonts), convertida en trazos, así el SVG
# no depende de las fuentes instaladas.
#
# Uso, desde la raíz del repositorio (tarda 1-2 minutos, casi todo el ajuste; necesita conexión para la letra):
#   Rscript data-raw/logo.R
#
# Dependencias solo de desarrollo (no van en DESCRIPTION): pkgload, ragg, showtext y sysfonts; R con Cairo (svg()).
# Reproducible: semillas fijas; el PNG sale igual en cada corrida en la misma máquina.

stopifnot(file.exists("DESCRIPTION"), read.dcf("DESCRIPTION", "Package")[1, 1] == "dismodlite")
for (p in c("pkgload", "ragg", "showtext", "sysfonts"))
  if (!requireNamespace(p, quietly = TRUE)) stop("falta el paquete de desarrollo ", p, call. = FALSE)
suppressPackageStartupMessages(library(grid))
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

# ---- Datos: ajuste nacional y cascada del proyecto de ejemplo ---------------------------------------------------

logo_datos <- function(sexo = 1L) {
  insumos <- suppressMessages(dl_insumos(dl_proyecto(dl_ejemplo(), causa = 9100)))
  opciones <- dl_opciones_mcmc(simulaciones = 1000, cadenas = 4, iteraciones = 20000, calentamiento = 10000)
  ajuste <- dl_ajustar(insumos, opciones, semilla = 1, cache = FALSE)
  cascada <- dl_cascada(ajuste, insumos, semilla = 1)
  estimaciones <- as.data.frame(dl_estimaciones(cascada))
  estimaciones <- estimaciones[estimaciones$sex_id == sexo, ]
  simulaciones <- as.data.frame(cascada$draws_q)
  simulaciones <- simulaciones[simulaciones$sex_id == sexo & simulaciones$location_id == insumos$loc_ancla, ]
  nacional <- estimaciones[estimaciones$location_id == insumos$loc_ancla, ]
  nacional <- nacional[order(nacional$edad), ]
  departamentos <- estimaciones[estimaciones$location_id != insumos$loc_ancla, ]
  departamentos <- departamentos[order(departamentos$location_id, departamentos$edad), ]
  list(nacional = nacional, departamentos = split(departamentos, departamentos$location_id),
       simulaciones = simulaciones)
}

# Intervalos centrales de las simulaciones nacionales por edad: colas `colas` (0.025 = el 95 %), suavizados.
logo_bandas <- function(simulaciones, colas = seq(0.025, 0.45, length.out = 24), gl = 10) {
  edades <- sort(unique(simulaciones$edad))
  suave <- function(y) pmax(0, stats::predict(stats::smooth.spline(edades, y, df = gl), edades)$y)
  cuantil <- function(pr) suave(tapply(simulaciones$p, simulaciones$edad, stats::quantile, pr)[as.character(edades)])
  list(edad = edades, inferior = sapply(colas, cuantil), superior = sapply(1 - colas, cuantil))
}

# ---- Dibujo -----------------------------------------------------------------------------------------------------

PALETA <- list(principal = "#0E6B62", tinta = "#16211E", fondo = "#F5F7F6", claro = "#4FC2B3", ambar = "#E0A030")
alfa <- function(col, a) grDevices::adjustcolor(col, alpha.f = a)
mezcla <- function(a, b, t) grDevices::colorRampPalette(c(a, b))(101)[round(t * 100) + 1]

# Hexágono con un vértice arriba, centrado en 0; el dispositivo mide 2 pulgadas de alto y 1 unidad = 1 pulgada.
hexagono <- function(r, ...) {
  a <- (90 + 60 * (0:5)) * pi / 180
  polygonGrob(r * cos(a), r * sin(a), default.units = "native", ...)
}

# Línea con halo: copias más anchas y casi transparentes debajo del trazo.
linea_halo <- function(x, y, col, lwd, extra = 12, capas = 10, a = 0.05) {
  for (k in capas:1)
    grid.lines(x, y, default.units = "native",
               gp = gpar(col = alfa(col, a), lwd = lwd + extra * k / capas, lineend = "round", linejoin = "round"))
  grid.lines(x, y, default.units = "native", gp = gpar(col = col, lwd = lwd, lineend = "round", linejoin = "round"))
}

# "dismod" en seminegrita y "lite" en regular, centrado como una sola palabra.
logotipo <- function(y, tam, col) {
  g1 <- textGrob("dismod", gp = gpar(fontfamily = "plex_semi", fontsize = tam))
  g2 <- textGrob("lite", gp = gpar(fontfamily = "plex_reg", fontsize = tam))
  a1 <- convertWidth(grobWidth(g1), "native", TRUE)
  a2 <- convertWidth(grobWidth(g2), "native", TRUE)
  x0 <- -(a1 + a2 + 0.006) / 2
  grid.text("dismod", x = x0, y = y, just = "left", default.units = "native",
            gp = gpar(fontfamily = "plex_semi", fontsize = tam, col = col))
  grid.text("lite", x = x0 + a1 + 0.006, y = y, just = "left", default.units = "native",
            gp = gpar(fontfamily = "plex_reg", fontsize = tam, col = col))
}

logo_dibujar <- function(datos, caja = c(-0.62, 0.56, -0.34, 0.70), desvanecer = c(0.8, 1), y_texto = -0.565,
                         tam_texto = 14.5, capas = 24, a_banda = 0.055, lwd_nacional = 3.6, filete = 0.5) {
  xs <- current.viewport()$xscale
  borde <- 0.05                                     # ancho del borde (pulgadas)
  r_int <- 1 - borde / cos(pi / 6)
  grid.draw(hexagono(0.995, gp = gpar(fill = PALETA$tinta, col = NA)))
  grid.draw(hexagono(r_int, gp = gpar(col = NA, fill = radialGradient(
    c(mezcla(PALETA$principal, PALETA$claro, 0.30), PALETA$principal, mezcla(PALETA$principal, PALETA$tinta, 0.55)),
    stops = c(0, 0.45, 1), cx1 = 0.66, cy1 = 0.70, r1 = 0, cx2 = 0.62, cy2 = 0.62, r2 = 0.9))))
  pushViewport(viewport(xscale = xs, yscale = c(-1, 1), clip = hexagono(r_int)))

  nac <- datos$nacional
  b <- logo_bandas(datos$simulaciones, colas = seq(0.025, 0.45, length.out = capas))
  p_ref <- 1.25 * max(nac$media)                    # la curva nacional termina al 80 % de la altura de la caja
  X <- function(edad) caja[1] + (edad - 30) / (99 - 30) * (caja[2] - caja[1])
  Y <- function(p) caja[3] + p / p_ref * (caja[4] - caja[3])

  # banda y curvas departamentales en un grupo, para que la máscara actúe sobre el conjunto (aplicada capa por capa,
  # el redondeo a 8 bits del alfa de cada capa deja franjas verticales)
  col_banda <- mezcla(PALETA$fondo, PALETA$claro, 0.35)
  capas_banda <- lapply(seq_len(ncol(b$inferior)), function(k)
    polygonGrob(X(c(b$edad, rev(b$edad))), Y(c(b$inferior[, k], rev(b$superior[, k]))), default.units = "native",
                gp = gpar(fill = alfa(col_banda, a_banda), col = NA)))
  curvas_dep <- lapply(datos$departamentos, function(d)
    linesGrob(X(d$edad), Y(d$media), default.units = "native",
              gp = gpar(col = alfa(PALETA$fondo, 0.45), lwd = 0.8, lineend = "round")))
  # máscara: opaca hasta el `desvanecer[1]` del rango de edades, transparente al final
  npc <- function(x) (x - xs[1]) / diff(xs)
  fx <- npc(X(30) + desvanecer * (X(99) - X(30)))
  mascara <- rectGrob(gp = gpar(col = NA, fill = linearGradient(
    c("black", "black", "#00000000", "#00000000"), stops = c(0, fx, 1), x1 = 0, x2 = 1, y1 = 0.5, y2 = 0.5)))
  pushViewport(viewport(xscale = xs, yscale = c(-1, 1), mask = mascara))
  grid.group(gTree(children = do.call(gList, c(capas_banda, unname(curvas_dep)))))
  popViewport()

  linea_halo(X(nac$edad), Y(nac$media), PALETA$ambar, lwd = lwd_nacional)
  popViewport()
  if (filete > 0)                                   # filete claro por dentro del borde
    grid.draw(hexagono(r_int - 0.004, gp = gpar(fill = NA, col = alfa(PALETA$claro, filete), lwd = 1.2)))
  logotipo(y_texto, tam_texto, PALETA$fondo)
}

# Dibuja en un PNG (ragg, `alto` píxeles) o en un SVG (el dispositivo svg() de Cairo: svglite no aplica la máscara
# al grupo); el texto se convierte en trazos con showtext.
logo_guardar <- function(datos, archivo, alto = 1200, ...) {
  ppp <- alto / 2                                   # el hexágono mide 2 pulgadas de alto
  if (grepl("\\.svg$", archivo)) {
    ancho_pulg <- ceiling(144 * sqrt(3) / 2) / 72   # Cairo redondea el ancho a puntos enteros: 125 pt
    grDevices::svg(archivo, width = ancho_pulg, height = 2, bg = "transparent")
    showtext::showtext_opts(dpi = 72)
  } else {
    ancho <- round(alto * sqrt(3) / 2)
    ancho_pulg <- ancho / ppp
    ragg::agg_png(archivo, width = ancho, height = alto, res = ppp, background = "transparent")
    showtext::showtext_opts(dpi = ppp)
  }
  on.exit(grDevices::dev.off())
  showtext::showtext_begin()
  on.exit(showtext::showtext_end(), add = TRUE, after = FALSE)
  grid.newpage()
  m <- ancho_pulg / 2
  pushViewport(viewport(xscale = c(-m, m), yscale = c(-1, 1)))
  logo_dibujar(datos, ...)
  popViewport()
  invisible(archivo)
}

# ---- Corrida ------------------------------------------------------------------------------------------------------

sysfonts::font_add_google("IBM Plex Sans", "plex_semi", regular.wt = 600)
sysfonts::font_add_google("IBM Plex Sans", "plex_reg", regular.wt = 400)
datos <- logo_datos()
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
logo_guardar(datos, "man/figures/logo.png")
logo_guardar(datos, "man/figures/logo.svg")
