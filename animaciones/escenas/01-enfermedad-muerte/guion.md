# 01 · El modelo enfermedad-muerte — guion

El guion dirigido con que se hizo la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO,
MOVIMIENTO, TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 1, y la cabecera de `R/edo.R`.
**Datos:** `datos/r/01-enfermedad-muerte.R`: `dl_edo()` con las tasas de la sección 2 de la guía (valores inventados).

```text
OBJETIVO: quien mira entiende los dos compartimentos (S sin la enfermedad, C con ella), las cuatro tasas
(i, r, f, m) y por qué basta una ecuación, dp/da = i(1 - p) - r p - f p (1 - p): m se cancela. Sale sabiendo que
el paquete estima i y f, y que de ellas sale p.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 60 s a 30 cuadros/s = 1800 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y los
dos compartimentos) y el final se sostiene 5 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, verde azulado de la marca 0E6B62 para lo
que se señala. Series: i 2a78d6, f eb6834, p 1baf7a (las tres validadas para daltonismo); r y m en tinta suave.
Ninguna letra en color de serie: en las fórmulas, cada término lleva un subrayado de 4 px del color de su tasa.
Títulos Source Serif 4 600 a 54 px; texto Source Sans 3; números IBM Plex Mono. Narración abajo a 36 px.

MOVIMIENTO: flechas que se dibujan en 0.8 s (EASE.mover); textos que entran en 0.3 s con EASE.entrarSuave y salen
en 0.24 s; la curva p se traza lineal en la edad (la edad avanza a ritmo constante) en 3.4 s; acento final con
EASE.pop sobre el grosor de las flechas i y f. Nada se mueve mientras se lee.

TOMAS (segundos):
0.0       cabecera + cajas S y C quietas (cuadro 0).
0.4-4.3   narración 1. Los compartimentos.
4.6-8.3   flecha i (S -> C), azul, arriba entre las cajas; etiqueta «i incidencia».
8.6-12.0  flecha r (C -> S), tinta suave, curva por abajo; etiqueta «r remisión (dato; 0 por defecto)».
12.3-16.4 flecha f desde C hacia abajo, naranja; etiqueta «f en exceso».
16.7-22.0 flechas m desde S y desde C hacia abajo, tinta suave; etiqueta «m otras causas».
22.3-25.8 sistema dS/da, dC/da (KaTeX 38 px), una ecuación por vez (0.7 s entre ellas).
26.1-29.9 p = C / (S + C).
30.2-35.6 los términos m del sistema se tachan (\cancel), las flechas m bajan al 35 %.
35.9-39.4 el sistema se va (0.24 s) y entra dp/da = i(1 - p) - r p - f p (1 - p) en un recuadro de la marca.
39.7-44.4 ejes de p por edad a la derecha; punto p(a0) = 0 en 30; la curva p se traza de 30 a 99.
44.7-49.4 en la edad 60: guía vertical, punto y tangente (pendiente = dp/da con las tasas de 60).
49.7-54.2 dos flechas desde el punto: entra i(1 - p) = 2.42 por 1000 (azul, arriba), sale f p (1 - p) = 0.60
          por 1000 (naranja, abajo); sus valores en tinta.
54.5-60.0 las flechas i y f del diagrama se acentúan (pop de 6 cuadros), el resto baja al 40 %; se sostiene.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo».

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs): la curva
dibujada es la de dl_edo() (p en 99 igual a 1e-12), p(30) = 0, la pendiente dibujada en 60 coincide con la
diferencia central de p a menos del 1 %, m tachada en la marca «cancela».
```
