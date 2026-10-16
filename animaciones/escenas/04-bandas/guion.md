# 04 · De las edades simples a las bandas — guion

El guion dirigido con que se hizo la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO,
MOVIMIENTO, TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 4; la cabecera y las funciones de `R/bandas.R`
(`.dl_pesos_intervalo()`, `.dl_q_intervalos()`, `.dl_agregar_bandas()`); `insumos$bandas_pobl` de `dl_insumos()`.
**Datos:** `datos/r/04-bandas.R`: la media posterior por edad simple de p, i (1 - p) y p f del ajuste nacional del
ejemplo (`ctx$ajuste()`, hombres), la población del ejemplo (2023, hombres, suma de los 25 departamentos) por banda
fina (todo sintético, y el pie lo dice), y para cada intervalo los pesos y el promedio que calcula el propio paquete.

```text
OBJETIVO: quien mira entiende que el modelo da cantidades por edad simple a (30 a 99) y que, para compararlas con el
ancla y los datos, que vienen por bandas [inicio, fin), las promedia con pesos w_a = (N_b(a)/n_b(a)) / suma: en una
sola banda las edades pesan igual; en varios quinquenios n_b se cancela y pesa N_b; con bandas de distinto ancho una
banda completa cuenta por su población, no por su población por su número de edades. Y que x(a) es p, i(1 - p) o
p f según la medida.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 72 s a 30 cuadros/s = 2160 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y los
ejes de p por edad a todo lo ancho, con su título) y el final se sostiene 3.7 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, marca 0E6B62. La prevalencia p (puntos
de 4.5 px de radio por edad simple y el segmento del promedio q, 5 px) en 1baf7a. El ancla en tinta: caja hueca de
2 px de lower a upper, raya de 3 px en val. La población en tinta: cajas por banda fina, relleno blanco, borde tinta
suave de 2 px; alto = N_b / n_b (personas por año de edad). Los pesos w_a, lo que se explica, en la marca 0E6B62
(barras con relleno al 100 %; el intervalo A con un velo de la marca al 8 % y bordes punteados de 2 px). Los pesos
erróneos (sin dividir por n_b), contorno punteado de tinta suave. Ninguna letra en color de serie: en la tabla de
medidas, i, f y p llevan un subrayado de 4 px de su color. Títulos Source Serif 4 600 a 54 px; texto Source Sans 3;
números IBM Plex Mono; fórmulas KaTeX 32 a 34 px. Ejes a 22 px; narración 36 px; nada legible por debajo de 22 px.

MOVIMIENTO: los puntos de p se encienden de 30 a 99 a ritmo constante (lineal en la edad) en 2.2 s; las cajas del
ancla entran escalonadas (ESCALONADO.item = 60 ms) con EASE.entrarSuave en 0.4 s; cámara declarada de 1.0 s
(EASE.camara) que estrecha el gráfico de 1560 x 500 a 950 x 220 px; las cajas de población crecen desde la base
(EASE.entrarSuave, 0.5 s, escalonadas 60 ms); el intervalo A se desliza de un caso al siguiente con EASE.mover en
0.75 s (MS.heroe); las barras w_a crecen desde la base en 0.5 s escalonadas 30 ms; el segmento q se traza de
izquierda a derecha en 0.5 s (EASE.mover); la fusión de 80-84 ... 95+ en 80+ es la altura de las cuatro cajas
yendo a su promedio N_80+/20 en 0.75 s (EASE.mover). Textos: entran en 0.3-0.4 s (EASE.entrarSuave), salen en
0.24 s. Nada se mueve mientras se lee.

TOMAS (segundos):
0.0        cabecera, ejes de p (x 200-1760, y 300-800), título «Edad (años)» (cuadro 0).
0.4-5.9    narración 1. Los 70 puntos de p (media posterior, hombres) se encienden de 30 a 99 (0.6-2.8 s, lineal).
6.2-10.9   narración 2. Las 12 cajas del ancla (40-44 ... 95+, la abierta dibujada en [95, 100)) entran
           escalonadas; leyenda «ancla, por banda [inicio, fin)».
11.1-12.1  CÁMARA: el gráfico se estrecha a la izquierda (x 200-1150, y 280-500).
12.3-17.7  narración 3. A la derecha, q = Σ w_a x(a), A = {...} y w_a (las tres de la guía), 0.7 s entre ellas.
17.9-24.4  narración 4. Panel de población (y 610-810, desde 18.3 s): 14 cajas de bandas finas, escalonadas.
24.7-31.0  narración 5. Las rayas que separan las edades del modelo dentro de cada caja (n_b = 5); la caja 95+ se
           señala: «95 y más (abierta): edades 95 a 99». Rayas de 18 x 3 px (legibles en un teléfono).
31.3-38.3  narración 6. Caso 1, [40, 45): velo A (31.5) y, 200 ms después, la tarjeta con 5 barras w_a = 0.200;
           la leyenda del gráfico se va; los puntos fuera de A bajan al 40 %.
38.6-43.6  narración 7. El segmento q sobre [40, 45) (38.8) y q = 0.0120 en la tarjeta; desde 39.0 s la caja 40-44
           del ancla queda al 100 % y las demás bajan al 35 %; lectura «ancla 0.0120 · q 0.0120» con guía a la caja.
43.9-50.6  narración 8. Caso 2, [50, 65): el velo se desliza (44.1); tarjeta a 44.3: 15 barras en tres escalones
           (N_b); w_a ∝ N_b. La tarjeta del caso 2 se va en 50.9, cuando empieza el caso 3.
50.9-57.6  narración 9. Caso 3: las cajas 80-84 ... 95+ (al 100 %: son la toma) se funden en 80+ (alto N_80+/20);
           fantasma punteado del alto sin dividir (N_80+/5, cuatro veces: «20 edades frente a 5»).
57.9-64.6  narración 10. El velo pasa a [60, 125); 40 barras y, encima, un escalón punteado con los pesos sin
           dividir por n_b (las 40 edades, en la misma escala); sumas:
           80+ pesa 0.214 = N_80+ / Σ N_b (sin dividir: 0.521); q de 60 y más.
64.9-72.0  narración 11. La tarjeta cambia a la tabla de medidas: p, i(1 - p), p f y su q en [60, 125) con la
           población del caso 3 (así lo dice la cabecera); el segmento q de 60 y más se queda hasta el final.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo», texto en color de serie, decir que 95+
tiene más edades que un quinquenio (en la malla 30-99 tiene cinco: 95 a 99).

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs): los 70 puntos
dibujados son la media posterior exportada; en cada caso, los pesos que calcula la escena en JavaScript coinciden
con los de .dl_pesos_intervalo() y suman 1, y q dibujado coincide a 1e-12 con .dl_q_intervalos() y con la media de
.dl_agregar_bandas() sobre las simulaciones (en el caso 1, además, con .dl_q_bandas(), el promedio de las tablas de
salida para la banda 40-44 del ancla); en el caso 1 los cinco pesos son iguales; en el caso 2 w_a ∝ N_b; en el caso
3 la banda 80+ suma N_80+ / Σ N_b; los q de i(1 - p) y p f de la tabla coinciden con los del paquete.
```
