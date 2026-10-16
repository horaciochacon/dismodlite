# 01 · El modelo enfermedad-muerte — guion

El guion dirigido de la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO, MOVIMIENTO,
TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 1, y la cabecera de `R/edo.R`.
**Datos:** `datos/r/01-enfermedad-muerte.R`: `dl_edo()` con las tasas de la sección 2 de la guía (valores inventados).

```text
OBJETIVO: quien mira entiende los dos compartimentos (S sin la enfermedad, C con ella), las cuatro tasas
(i, r, f, m) y por qué basta una ecuación, dp/da = i(1 - p) - r p - f p (1 - p): m sigue en el sistema de S y C, pero
se anula al derivar el cociente p = C/(S + C). Sale sabiendo que el paquete estima i y f en los nudos y que de ellas
sale p.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 63 s a 30 cuadros/s = 1890 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y los
dos compartimentos, grandes) y el final se sostiene 6 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, verde azulado de la marca 0E6B62 para lo
que se señala (las m tachadas, la tangente, la pendiente). Series: i 2a78d6, f eb6834, p 1baf7a; r y m en tinta
suave. Ninguna letra en color de serie: en las fórmulas, cada tasa lleva un subrayado de 4 px de su color (sin
cortes alrededor de los trazos). Títulos Source Serif 4 600 a 54 px; texto Source Sans 3 (etiquetas a 28 px);
fórmulas KaTeX a 38-42 px; narración a 42 px, en una línea.

MOVIMIENTO: flechas que se dibujan en 0.8 s (EASE.mover); textos que entran en 0.3 s con EASE.entrarSuave y salen
en 0.24 s; dos movimientos de cámara declarados, entre frases (EASE.camara): 22.5-23.2 s el diagrama pasa de 1.4x a
1x y sube; 40.5-41.3 s se corre 445 px a la izquierda. La curva p se traza lineal en la edad (3.4 s). Las m se
tachan con un trazo de 0.5 s (EASE.mover). Acento final con EASE.pop (0.4 s) sobre el grosor de las flechas i y f,
de 4 a 8 px. Nada se mueve mientras se lee.

TOMAS (segundos):
0.0       cabecera + cajas S y C, a 1.4x y centradas (cuadro 0).
0.4-4.3   narración 1. Los compartimentos.
4.6-8.3   flecha i (S -> C), azul, arriba entre las cajas; «i · incidencia».
8.6-12.0  flecha r (C -> S), tinta suave, curva por abajo; «r · remisión / no se estima (aquí, 0)».
12.3-16.4 flecha f desde C hacia abajo, naranja; «f · mortalidad en exceso».
16.7-22.4 flechas m desde S y desde C, tinta suave; «m · mortalidad por otras causas». La narración dice que m
          igual en los dos grupos es un supuesto.
22.5-23.2 cámara: el diagrama a 1x.
23.3-26.8 sistema dS/da, dC/da (KaTeX 38 px), una ecuación por vez (0.7 s entre ellas). El sistema no se toca.
27.1-30.9 p = C / (S + C).
31.2-36.6 en lugar de p = C/(S + C), la derivada del cociente:
          dp/da = i (1 - p) - (r + m + f) p + p (m + f p). De 33.4 a 33.9 s se tachan sus dos m en verde azulado (el
          movimiento principal); 200 ms después las flechas m bajan al 35 %.
36.9-40.4 el sistema y la derivada se van (0.24 s) y entra dp/da = i(1 - p) - r p - f p (1 - p) en un recuadro de la
          marca de 630 x 120 px.
40.5-41.3 cámara: el bloque a la izquierda.
41.4-46.1 ejes de p por edad a la derecha (41.2), punto p(30) = 0 (41.7), «con p(a0) = 0 en a0 = edad_inicio» (42.1),
          la curva de 30 a 99 (42.4-45.8). La narración: avanza en pasos de 0.2 años (RK4, h = 0.2; se dibujan las
          edades enteras).
46.4-51.0 en la edad 60: guía vertical, punto y tangente de 48 a 72 años, de 4 px. Narración: aquí r = 0.
51.3-56.4 cascada en un recuadro propio, arriba a la izquierda del gráfico (edades 30-64, p > 4 %), por 1000 al año:
          sube i(1 - p) = 2.423 (azul), resta f p (1 - p) = 0.598 (naranja, desde la punta de lo que entra) y queda
          dp/da = 1.825 (verde azulado), una flecha por vez, 1 s entre ellas. f p (1 - p) es el efecto neto de las
          muertes en exceso sobre la proporción (las muertes son f p = 0.617 por 1000): no se la llama «las muertes».
56.7-63.0 las flechas i y f del diagrama engordan (pop de 0.4 s); el resto, salvo la curva p, baja al 40 %.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo». Tachar m en el sistema de S y C. Dibujar
una tasa de cambio sobre el eje de la prevalencia.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos y auditoría (auditar.mjs). La auditoría
mide lo dibujado contra algo independiente: la curva cumple la ecuación (la diferencia central de p frente a
i(1-p) - r p - f p (1-p), fuera de los nudos, a menos del 1 % de la pendiente máxima) y cada punto está en su lugar
del eje; p(30) = 0; la tangente tiene la pendiente de la curva (< 1 %); en la cascada, entra - resta es la pendiente
de la curva (< 1 %) y las flechas cierran; lo que resta es f p (1 - p); m sigue en el sistema y se tacha solo en la
derivada del cociente.

REVISIÓN ADVERSARIAL (metodólogo y diseñador, en paralelo): corrigió el tachado de m en el sistema, «las muertes en
exceso» para f p (1 - p), las flechas de flujo sobre el eje de p, «avanza año a año», dos comprobaciones que no
podían fallar, el primer acto chico, el final que no aterrizaba, el subrayado cortado de f, la narración chica para
un teléfono y la etiqueta duplicada de p(a0) = 0.
```
