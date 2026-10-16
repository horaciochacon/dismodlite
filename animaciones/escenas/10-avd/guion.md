# 10 · Los años vividos con discapacidad — guion

El guion dirigido con que se hizo la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO,
MOVIMIENTO, TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 8; `vignettes/articles/carga-avd.qmd` (la ecuación, la
incertidumbre de la severidad, el factor de comorbilidad y «el valor central sigue al AVD del ancla»); las cabeceras
de `R/avd.R` y `R/comorbilidad.R`.
**Datos:** `datos/r/10-avd.R`: la tabla `severidad` del ejemplo (causa 9100), las formas de la Beta de
`.dl_beta_momentos()`, las simulaciones de `.dl_simular_pi_dw()` con la semilla 1 (las mismas que sortea `dl_avd()`),
`dl_factor_comorbilidad()` y `dl_avd()` sobre el ajuste nacional del ejemplo (`ctx$ajuste()`, 800 simulaciones),
como en `dl_correr()`. Mujeres (sex_id 2), como la guía de carga.

```text
OBJETIVO: quien mira entiende que AVD_{k,b} = p_{k,b} x (sum_e pi_{k,e} DW_{k,e}) x COMO_b se calcula por banda b y
simulación k; que cada proporción y cada peso sale de una Beta con la media y la varianza de su intervalo del 95 %,
v = ((superior - inferior)/3.92)^2, y que las proporciones se renormalizan a 1 en cada simulación; que COMO_b calibra
al AVD del ancla (absorbe la comorbilidad y el patrón por edad de la severidad, puede superar 1) y que, por eso, el
valor central sigue al AVD del ancla y la tabla de severidad pesa en la incertidumbre.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 82 s a 30 cuadros/s = 2460 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y la
ecuación de los AVD con su unidad) y el final se sostiene ~1 s después de la última frase leída.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, verde azulado de la marca
0E6B62 para lo que se explica (la Beta, la línea del 1, el valor central, la banda del ejemplo). Solo p lleva color
de serie (1baf7a, subrayado de 4 px bajo p en las fórmulas); pi, DW, COMO y los AVD no son i, f ni p: van en tinta.
Los estados de salud, en tres tonos de tinta (C9D4D0 asintomático, 8FA19B leve, 4E5E58 moderado) con separación
blanca de 2 px. El ancla en tinta con su propia forma (aspa x de 18 px, trazo 2.5 px); el modelo, punto lleno de 6 px
con bigote del IC 95 %; sin COMO, círculo hueco de tinta suave relleno de fondo. AVD, DW y COMO van rectos
(\mathrm), como en la guía de carga (\text{}), y «ancla» en los superíndices como texto: los mismos símbolos de la
guía «El modelo», sin el espaciado de letras sueltas de KaTeX. Títulos Source Serif 4 600 a 54 px; texto Source Sans 3 (26 px
en tablas, 22 px en ejes y notas); números IBM Plex Mono; fórmulas KaTeX (46 px la ecuación, 30-38 px las demás).
Narración abajo a 36 px.

MOVIMIENTO: textos y fórmulas entran en 0.4 s con EASE.entrarSuave y salen en 0.24 s con EASE.salir; las filas de la
tabla, escalonadas 120 ms (ESCALONADO.fila), su cabecera 0.3 s después de la narración; la densidad Beta se traza en 1.4 s con EASE.mover; las barras crecen en
0.5 s (EASE.entrarSuave, escalonado 60 ms); la renormalización (héroe de la toma) lleva cada barra a 1 en 0.75 s con
EASE.mover, escalonada 60 ms (270 px por unidad); el histograma crece en 0.75 s; la cámara (52.1-52.9 s, EASE.camara) sube el cálculo de
la banda 110 px y lo escala a 0.85, y se mueve sola: lo siguiente entra desde 52.9 s. El foco de la ecuación: los términos fuera de foco bajan al 30 % en 0.4 s.
Nada se mueve mientras se lee, salvo dentro de la cámara declarada.

TOMAS (segundos; una cosa principal por toma):
0.0        cabecera + ecuación AVD_{k,b} = p_{k,b} x (sum_e pi_{k,e} DW_{k,e}) x COMO_b (x = 96, y = 292) y la unidad
           «por persona y año · x 100 000 = por 100 000» a la derecha (cuadro 0).
0.4-6.0    narración 1. La ecuación sola.
6.3-12.9   foco en el término de severidad (p y COMO al 30 %); la tabla de severidad entra a la izquierda (x 96-920,
           filas a y 460, 536, 612): estado, pi_e con su IC, DW_e con su IC; el DW del asintomático dice «constante».
13.2-18.2  sum_e pi_e DW_e = 0.0169 (KaTeX 34 px) en un recuadro de la marca bajo la tabla.
18.5-24.4  a la derecha (x 1090-1770, y 410-660), la densidad Beta de la proporción sorteada del estado leve (media
           pi_e) se traza; el eje marca solo inferior 0.274, media 0.322 y superior 0.371; la celda pi_e leve se
           recuadra en la marca.
24.7-28.8  bajo el gráfico: v = ((0.37063 - 0.27394)/3.92)^2 = 0.000608 y Beta(115.4, 242.6): media 0.3223, v 0.000608.
29.1-34.7  la Beta se va; seis barras apiladas (k = 1..6, base y = 760, 270 px por unidad) crecen con las proporciones
           sorteadas antes de renormalizar; sus sumas en una fila fija (1.071, 0.995, …); la línea del 1 punteada.
35.0-39.7  las sumas se van (0.24 s) y las barras se reescalan a 1 (héroe, 0.75 s); el título pasa a pi_{k,e}.
40.0-45.9  la tabla se va; a la izquierda, el histograma de sum_e pi_{k,e} DW_{k,e} en las 800 simulaciones con la
           línea del valor central 0.0169; bajo cada barra, su peso medio por caso (0.0181, 0.0166, …).
46.2-52.1  todo lo anterior se va; foco de vuelta a toda la ecuación; el cálculo de la banda de 70 a 74 años, mujeres:
           k = 1, 2, 3 con p_{k,b} x sum x COMO_b = AVD_{k,b} y su valor por 100 000 (79.5, 69.2, 77.8).
52.1-52.9  cámara: el cálculo sube 110 px y se escala a 0.85 para dejar sitio abajo. Se mueve sola.
52.9-66.9  foco en COMO_b; la columna COMO_b del cálculo se recuadra. Abajo a la izquierda, COMO_b = AVD^ancla_b /
           (p^ancla_b sum_e pi_e DW_e) y, desde 54.2 s, la cuenta de 70-74 (0.0008026 / (0.05686 x 0.016946) = 0.833);
           desde 55.4 s a la derecha, COMO_b por banda (la banda 70-74 en la marca, la línea del 1 punteada y, desde
           60.6 s, «aquí, entre 0.74 y 0.95»).
67.2-74.1  el cálculo y COMO se van; foco de vuelta a toda la ecuación. A la izquierda, la sustitución AVD_{k,b} =
           AVD^ancla_b (p_{k,b}/p^ancla_b)(sum pi_{k,e} DW_{k,e} / sum pi_e DW_e) y «el último cociente vale 1.006 en
           promedio: cerca de 1»; desde 69.0 s a la derecha, los AVD por 100 000 (mujeres, Perú 2023): aspas del
           ancla y, desde 70.4 s, puntos del modelo con su IC 95 %.
74.4-82.0  aparecen los círculos huecos sin COMO, por encima del ancla (aquí COMO_b < 1); se sostiene.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee (fuera de la cámara declarada),
3D, sombras, emojis, números que no salgan del paquete, una notación distinta de la guía «El modelo», texto en el
color de una serie, color de serie en pi, DW, COMO o los AVD.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos y auditoría (auditar.mjs):
- las formas de cada Beta reproducen la media y v = ((superior - inferior)/3.92)^2 de su intervalo (1e-12);
- las barras renormalizadas suman 1 y son las proporciones crudas divididas por su suma (1e-12);
- el histograma cuenta las 800 simulaciones y cada peso medio mostrado es sum_e pi_{k,e} DW_{k,e} (1e-12);
- cada AVD mostrado es p_{k,b} x sum x COMO_b recalculado en JS y coincide con dl_avd() (1e-12);
- COMO_b = AVD^ancla_b / (p^ancla_b sum_e pi_e DW_e) recalculado en JS con la tabla (1e-12);
- la sustitución AVD^ancla_b (p_{k,b}/p^ancla_b)(sum_k/sum) da el AVD de dl_avd() (1e-12);
- los puntos del gráfico final, leídos de vuelta desde la escala del eje, son las medias e IC de dl_avd() y el ancla,
  caben en el eje, y los AVD sin COMO quedan sobre el ancla en cada banda;
- las cuentas en pantalla (v y COMO de 70-74) cierran con sus operandos redondeados; el rango 0.74-0.95 y el
  cociente 1.006 son los de los datos; el histograma no deja simulaciones fuera del eje.
```
