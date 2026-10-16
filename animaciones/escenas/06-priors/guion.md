# 06 · Los priors: suavidad y mortalidad en exceso — guion

El guion dirigido de la escena, con la plantilla del laboratorio. Cada adjetivo es un número. Si cambias la escena,
cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 5 («Suavidad de la incidencia» y «Prior de la mortalidad
en exceso»); `R/emr.R` (`dl_prior_emr()`, `.dl_emr_en_nudos()`, `.dl_emr_plano()`, techo), `R/verosimilitud.R`
(`.dl_lp_suavidad()`, `.dl_ctx_emr()`) y la documentación de `incidencia.suavidad` y `mortalidad_exceso.*` en
`R/configuracion.R`.
**Datos:** `datos/r/06-priors.R`: hombres (sex_id 1) del proyecto de ejemplo (causa 9100, datos sintéticos):
`dl_prior_emr()` por banda, el prior en los nudos con el paso banda → nudo del paquete (`.dl_ctx_emr()`), el techo
de `dl_insumos()`, la media posterior de θ de `dl_ajustar()` (4 cadenas, semilla 1) y L_suavidad, L_emr calculadas
en R. Valores ilustrativos, rotulados así en el pie: las tres deformaciones de log i (+0.8; +0.25 por nudo; +0.6 en
el nudo de 60) y σ = 1.

```text
OBJETIVO: quien mira entiende (1) que el prior de suavidad penaliza solo la curvatura de log i entre nudos
(D2_k, segundas diferencias nudo a nudo, sin dividir por la distancia), con N(0, σ), σ = 0.5: subir o inclinar
log i no cambia D2, doblarla sí, y un σ mayor deja doblarla más barato; y (2) que el prior de f sale del ancla,
f = csmr / p por banda (μ_log, sd_log), que cada nudo toma la banda que lo contiene (fuera de las bandas, la más
cercana con sd × 2), que L_emr suma log N(log f_k; μ_log,k, sd_log,k), que sobre el techo f_max la log-posterior
es -∞, y qué cambian los priors plano, cero y tabla.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 82.5 s a 30 cuadros/s = 2475 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera, gráfico
de log i en los nudos con sus siete puntos y el pie). El final se sostiene 2 s (más de los 50-80 s pedidos: la escena cubre dos priors y tres variantes).

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, marca 0E6B62.
Series: log i en 2a78d6 (puntos de 9 px y trazo de 4 px), f posterior en eb6834 (puntos de 8 px, trazo de 3 px,
bigote de 2.5 px). El prior de f viene del ancla: va en tinta, con forma propia (cajas huecas de 2 px para las
bandas, bigote de 3 px con rombo hueco para el prior en cada nudo). Las barras D2_k en tinta suave; la toma activa
en marca. La franja ±σ en rejilla; la densidad N(0, σ) de lado, trazo de 2.5 px en tinta suave. El techo: línea de
tinta de 2.5 px y rayado de tinta al 18 %. El prior plano: franja de marca al 14 %. Ninguna letra en color de serie:
en las fórmulas, i, f y p llevan un subrayado de 4 px de su color. Cabecera Source Serif 4 600 a 54 px; fórmulas
KaTeX de 30 a 40 px; etiquetas 24-26 px; ejes 22 px; narración 36 px. Números en IBM Plex Mono.
Retícula, parte 1: gráfico de log i en x 170-930, y 300-780; columna derecha desde x 1040 (fórmulas en y 300 y 362,
lecturas en y 405 y 445, barras D2 en x 1110-1670, y 520-810, densidad en x 1690-1790).
Parte 2: gráfico de f en x 170-990, y 300-800 (escala log, 5 a 600 por 1000; tras la cámara, 0.1 a 600); columna
derecha desde x 1120 (fórmulas en y 300, 410, 470; nota en 512; L_emr en 620; luego tres tarjetas de 640 x 132 px en
y 296, 448 y 600).

MOVIMIENTO: textos y fórmulas entran en 0.3-0.4 s (EASE.entrarSuave) y salen en 0.24 s (EASE.salir). Barrido de
D2_k: un triplete cada 0.6 s; la barra k crece en 0.4 s (EASE.entrar) cuando el barrido llega. Deformaciones de
log i: suben en 0.9 s y vuelven en 0.9 s (EASE.mover), 0.9 s quietas entre ida y vuelta; la de doblar se queda.
σ de 0.5 a 1 en 1.0 s (EASE.mover). Bandas del ancla escalonadas 60 ms (ESCALONADO.item), entran en 0.4 s. Nudos
del prior: uno cada 0.45 s. Flecha del nudo 30 en 0.8 s (EASE.mover). Curva posterior de f trazada lineal en la
edad en 1.6 s (MS.trazo). Dos cámaras, entre frases: el eje de las barras D2 se abre de ±0.08 a ±1.8 (11.95-12.85 s, EASE.camara; las
D2 del ejemplo, 0.02-0.05, se leen acercadas y luego se ven diminutas junto a σ), y el eje y de f pasa de [5, 600] a
[0.1, 600] en escala log en 0.9 s (67.5-68.4 s, EASE.camara). El cambio de la parte 1 a la parte 2 es salida (0.24 s) y entrada, sin mover nada.

TOMAS (segundos):
0.0       cabecera + gráfico de log i (siete puntos, media posterior) + pie (cuadro 0).
0.4-5.8   narración 1. log i en los nudos, equiespaciados en x.
6.1-11.9  L_suavidad y D2_k (KaTeX 34 px) entran; ejes de D2 (acercados a ±0.08) en 7.0; barrido 7.4-10.4: recta de
          k y k+1 prolongada a trazos hasta k+2, la separación en marca, la barra k crece y su valor (26 px) aparece.
11.95-12.85 cámara: el eje de D2 se abre a ±1.8.
12.2-18.2 franja ±σ y densidad N(0, σ) de lado (12.9); lectura L_suavidad = -1.15 con σ = 0.5 (13.3).
18.5-25.0 subir log i +0.8 (18.8-21.5) y luego inclinarlo +0.25 por nudo (22.0-24.7), con la curva original en guía:
          los valores de D2 van en marca como testigos y «Las cinco D2_k y L_suavidad: sin cambio» (19.9).
25.3-30.3 «nivel y pendiente por nudo los fija el ancla»; doblar: +0.6 en el nudo de 60 (26.8-27.7, se queda);
          D2_2, D2_3 y D2_4 cambian (en marca); lectura «doblarla resta 4.29» (28.0, 28 px, en marca).
30.6-35.0 σ de 0.5 a 1 (30.9-31.9): la franja y la densidad se ensanchan; lectura «con σ = 1, solo 1.07» (32.1).
35.0-35.24 la parte 1 sale.
35.6-41.9 csmr = f p (KaTeX 40 px, 35.8) y su lectura «muertes por persona-año de toda la población» (36.4).
42.2-48.4 ⇒ f = csmr / p (42.3); μ_log y sd_log (43.2, 43.6); ejes de f (43.0); bandas del ancla escalonadas (44.4):
          centro exp(μ_log) y su 95 %, exp(μ_log ± 1.96 sd_log); nota sd_log 0.212, fracción aguda 0, factor_sd 1 (46.2).
48.7-56.2 nudos 40 a 95 (48.9, uno cada 0.45 s): guía vertical, la banda que lo contiene resaltada en marca, bigote
          del prior dentro de esa banda; nudo 30 (52.2): flecha desde la banda 40-45, bigote del doble, «sd_log × 2».
56.5-62.2 L_emr (56.7); las guías de los nudos salen; media posterior de f e intervalo del 95 % en la edad del nudo
          (57.6), curva log-lineal entre nudos (58.0-59.6); leyenda en la columna derecha.
62.4-67.4 techo: línea en 250 por 1000 y rayado encima (62.5), rótulo sobre placa mate, «f_max = 0.25:
          mortalidad_exceso.techo del ejemplo» y «sin mortalidad_exceso.techo, 3 × la mayor csmr/p (dos sexos): 0.234».
67.5-68.4 cámara: el eje y se abre a [0.1, 600]; la columna derecha sale.
68.5-73.5 tarjeta «plano» (68.9); bandas y prior por nudo al 25 %, la posterior (ajustada con desde_ancla) sale;
          franja uniforme de 0.25 a 250 por 1000 (69.4), con sus dos bordes rotulados.
73.8-80.5 tarjeta «cero» (74.2): el gráfico baja al 25 % y la franja plana sale; tarjeta «tabla» (76.6): vuelven
          las bandas y el prior por nudo, bajo el techo.
80.5-82.5 se sostiene.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, más de
tres destellos por segundo, números que no salgan del paquete (o rotulados ilustrativos), notación distinta de la
guía «El modelo», D2 dividida por la distancia entre nudos, una inclinación «en la edad» presentada como invariante
(con nudos no equiespaciados no lo es: la inclinación es por nudo), el prior de f en color de serie.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs):
las D2 dibujadas son las segundas diferencias del log i exportado (y las de R, a 1e-12); al subir y al inclinar
las D2 dibujadas no cambian y L_suavidad = la de R; al doblar, D2_2, D2_3 y D2_4 cambian en +0.6, -1.2 y +0.6; el
prior de cada nudo (μ_log, sd_log) calculado en la escena desde las bandas es el del paquete (.dl_ctx_emr), y el
nudo 30 lleva sd × 2; L_emr calculada desde lo dibujado = la de R; el techo dibujado es b$techo_emr; el piso del
prior plano es f_max / 1000.
```
