# 03 · Cómo se resuelve la ecuación: RK4 — guion

El guion dirigido de la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO, MOVIMIENTO,
TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 3 (y su chunk «precision»); la cabecera de `R/edo.R`
(las mallas anual, h y h/2; `.DL_NSUB` = 5; `.DL_RK4_LIMITE_PASO` = 2.5 y su comentario sobre k h < 2.785;
`dl_edo_resolver()`); `inst/cpp/dl_core.cpp` (`resolver_edo()`, la gemela); `R/insumos.R`
(`.dl_chequear_paso_rk4()`, la comprobación que hace `dl_insumos()`).
**Datos:** `datos/r/03-rk4.R`: las tasas de la sección 2 de la guía (valores inventados) en la malla h/2, como las
arma `dl_edo()`; `dl_edo_resolver()` sobre ellas; las cuatro etapas del paso n = 151 (de 60 a 60.2 años); el chunk
«precision» de la sección 3 tal cual (i = 0.002, f = 0.05); una ilustración de estabilidad con valores inventados
(i = 0.05, r = 15, f = 0) resuelta con `dl_edo_resolver()` con n_sub = 5 y n_sub = 10, y el mensaje real de
`.dl_chequear_paso_rk4()`.

```text
OBJETIVO: quien mira entiende que dp/da = i(1 - p) - r p - f p (1 - p) se integra con RK4 (orden 4) en pasos de
h = 1/n_sub = 0.2 años desde p(a0) = 0; que cada paso evalúa cuatro pendientes (k1 en a, k2 y k3 en a + h/2 con
las predicciones de medio paso, k4 en a + h) con las tasas de la malla h/2 (puntos 2n - 1, 2n, 2n + 1) y las
promedia con pesos 1, 2, 2, 1; que las edades enteras son cada quinto punto de la malla h; que el error frente a
la solución exacta es despreciable (8.5e-11, el número de R); y que con una remisión alta el paso se vuelve
inestable, por eso dl_insumos() exige k h <= 2.5 y pide más pasos (avanzado: {nsub: 10}).

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js y lib/modelo.js.

TIEMPO: 80 s a 30 cuadros/s = 2400 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera, la
ecuación en su recuadro con p(a0) = 0 y los ejes de p con el punto de partida) y el final se sostiene 6 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD. Verde azulado de la marca
0E6B62 para lo que se explica: las pendientes k1..k4 (segmentos, puntos y el promedio), el recuadro del paso.
Series: i 2a78d6 y f eb6834 solo como subrayado en la ecuación; p 1baf7a para la prevalencia (curva, puntos de la
malla h). La solución exacta en tinta suave (otra fuente de la misma cantidad: trazo fino, no color de serie).
Ningún texto en color de serie. Títulos Source Serif 4 600 a 54 px; texto Source Sans 3 (etiquetas 24-28 px);
números IBM Plex Mono; fórmulas KaTeX 30-42 px; narración 42 px en una línea.
Retícula: columna izquierda x 120-800 (ecuación arriba, y 252-356; etapas o fórmulas en y 440-700; el recuadro del
paso en y 736-836); columna derecha x 1010-1800 (los gráficos).

MOVIMIENTO: textos que entran en 0.3-0.4 s (EASE.entrarSuave) y salen en 0.24 s; segmentos de pendiente que se
dibujan en 0.6 s (EASE.mover); la curva p de 30 a 99 años lineal en la edad (3.6 s); los puntos que caen, lineales
en el número de paso. Tres movimientos de cámara declarados (EASE.camara), entre frases o con la narración quieta:
10.4-12.9 s el gráfico pasa de [30, 99] años a [59.94, 60.26] (zoom geométrico, 216x en la edad) y se achica a la
mitad de arriba; 36.9-39.0 s sale a [59.85, 61.15] y vuelve a su alto. Acento final con EASE.pop (0.4 s) en el
borde del recuadro del paso, de 3 a 6 px. Nada se mueve mientras se lee.

TOMAS (segundos):
0.0       cabecera; ecuación en un recuadro de la marca (660 x 104 px) con «con p(a0) = 0 en a0 = 30 años»; ejes
          de p (30-99 años, 0-10 %), punto p(30) = 0.
0.4-4.6   narración: se integra con RK4.
4.9-9.2   «h = 1/n_sub = 0.2 años (n_sub = 5 pasos por año)» bajo la ecuación (5.1); la curva p de la malla h se
          traza de 30 a 99 (5.3-8.9 s), lineal.
9.5-13.9  recuadro de la marca alrededor de 60 (9.8); cámara 10.4-12.9: zoom a [59.94, 60.26]. Los puntos de la
          malla h aparecen cuando su separación pasa de 8 px. Tras el zoom, lo posterior a 60 queda al 25 %
          («aún no calculado»).
14.2-19.0 panel de pendientes debajo (por 1000 al año, eje que no empieza en 0, rotulado), eje de edad compartido
          con a, a + h/2, a + h y los puntos 301, 302, 303 de la malla h/2 (escalonado 0.12 s).
19.3-22.9 k1: su línea a la izquierda (k1 = F(a, p1), 1.82503); segmento desde p1 con pendiente k1 hasta a + h/2,
          punto hueco p2; punto k1 en el panel de pendientes. El movimiento principal es el segmento.
23.2-27.8 k2 (23.5) y k3 (25.6): ídem, en a + h/2; k2 y k3 casi coinciden (1.83012, 1.83011).
28.1-31.7 k4 en a + h, con p4 = p1 + h k3.
32.0-36.6 el promedio (k1 + 2k2 + 2k3 + k4)/6 (línea de la marca en el panel, 32.3), pesos ×1 ×2 ×2 ×1, y el
          recuadro del paso p(a + h) = p(a) + (h/6)(k1 + 2k2 + 2k3 + k4) (32.6); el segmento final y el punto
          p(60.2) = 0.0312103 caen sobre el punto fantasma de dl_edo_resolver() (33.6). Etiqueta «= dl_edo_resolver(),
          punto 152».
36.7      salen las etapas y el panel de pendientes (0.24 s).
36.9-39.0 cámara: el gráfico sale a [59.85, 61.15] y vuelve a su alto; lo posterior a 60.2 deja de ser fantasma.
39.0-43.3 los seis puntos de la malla h de 60 a 61, cinco arcos numerados 1-5 (escalonado 0.12 s), las marcas de la
          malla h/2 sobre el eje y las edades enteras 60 y 61 en grande: cada quinto punto.
43.2      sale el gráfico; 43.6-47.9 la solución exacta (fórmula de la guía) a la izquierda; ejes de 30-99 años a la
          derecha; la exacta se traza (44.4-46.4).
48.2-55.4 los 70 puntos RK4 de dl_edo() (escalonados en 1.2 s); recuadro «error relativo máximo 8.5 × 10^-11»
          (49.6); narración: muchísimo menor que la incertidumbre.
55.5      sale lo de la precisión; 55.7-59.6 k = i + r + f(1 - 2p) a la izquierda (56.0).
59.9-64.0 «RK4 es estable si k h < 2.785» (60.2) y «dl_insumos(): (r + techo de f) h <= 2.5» (61.4).
64.3-68.9 ilustración (valores inventados, i = 0.05, r = 15, f = 0) a la derecha: la exacta (64.6), los 6 pasos con
          h = 0.2 (k h = 3.01) que caen a negativos (65.4-67.4, un punto por paso).
69.2-73.4 el mensaje real de dl_insumos() (69.5); con avanzado: {nsub: 10} (k h = 1.51) los 12 pasos siguen a la
          exacta (70.4-72.4); la curva inestable baja al 40 %.
73.7-80.0 todo lo demás baja al 40 %; el recuadro del paso y la ecuación quedan; pop del borde del recuadro del paso.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete (o de la guía, rotulados), una notación distinta de la guía «El modelo». Exagerar en
secreto la diferencia entre las pendientes: el panel de pendientes tiene su propio eje, rotulado, que no empieza en
0. Decir que RK4 «oscila»: sobre el eje real su factor por paso es positivo y la inestabilidad se dispara sin
oscilar. Una tasa sobre el eje de la prevalencia.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos y auditoría (auditar.mjs). La auditoría
mide lo dibujado contra algo independiente: las pendientes dibujadas son las de DL.modelo.pasoRk4 y las que exporta
R (<= 1e-14); cada punto del panel de pendientes y cada segmento están en su lugar según el eje; el punto final es
el p de dl_edo_resolver() en a + h (bit a bit) y su píxel el del eje; los puntos 301-303 de la malla h/2 están en
a, a + h/2 y a + h; las edades enteras son cada quinto punto de la malla h y coinciden con dl_edo(); el error
relativo máximo escrito es el de R y el que se recalcula con DL.modelo y la fórmula exacta; la curva inestable es
la de dl_edo_resolver() con n_sub = 5 (y se recalcula con resolverEdo) y se vuelve negativa; k h escrito es
(i + r + f)/n_sub; el límite 2.785 es la raíz de R(z) = 1.
```
