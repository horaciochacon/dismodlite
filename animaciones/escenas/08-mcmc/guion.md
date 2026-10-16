# 08 · El muestreo: Metropolis-Hastings adaptativo — guion

El guion dirigido de la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO, MOVIMIENTO,
TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 6; la cabecera de `R/muestreador.R` (bloques, propuesta,
aceptación, adaptación, adelgazamiento, semillas, R-hat y ESS); `R/ajuste.R` (`dl_ajustar()`: los bloques y las
simulaciones elegidas); `R/estimaciones.R` (media y percentiles 2.5 y 97.5); `vignettes/articles/diagnostico-y-sensibilidad.qmd`
(las compuertas R-hat < 1.01 y ESS ≥ 400, las opciones de producción).
**Datos:** `datos/r/08-mcmc.R`: `dl_ajustar()` del ejemplo con `dl_opciones_mcmc()` por defecto (producción: 4 cadenas
de 50 000 iteraciones, 10 000 de calentamiento, adelgazamiento 10, 1000 simulaciones), semilla 1; los hombres (sexo 1).
La cadena 1 se corre además con una copia instrumentada de `dl_mh()` que anota cada propuesta; el script comprueba que
da bit a bit las simulaciones del paquete, y que las cuatro cadenas dan `draws_par`, el R-hat y el ESS de `dl_ajustar()`.

```text
OBJETIVO: quien mira entiende que la posterior de θ (log i y log f en los 7 nudos, por sexo) se recorre con un
Metropolis-Hastings por bloques: se propone θ' = θ + s_b L_b z y se acepta con probabilidad
min(1, e^{log post(θ') - log post(θ)}); que en el calentamiento s_b se corrige hacia una aceptación de 0.28 cada 100
iteraciones y L_b se fija una vez, a la mitad, con la covarianza recorrida; que después se guarda una de cada 10
iteraciones; que la convergencia se mide con R-hat partido y ESS (compuertas 1.01 y 400); y que con cada θ guardado
se resuelve la ecuación: las 1000 simulaciones de p (de 16 000 guardados) dan la media y el intervalo (2.5 y 97.5).

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js y lib/modelo.js.

TIEMPO: 90 s a 30 cuadros/s = 2700 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y la tira
de θ con sus dos bloques) y el final se sostiene 3 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, verde azulado 0E6B62 para lo que se señala
(los dos nudos del plano, la propuesta aceptada, la línea de 0.28, la forma L_b, las mitades, las compuertas, la guía
de los 80 años). Series: lo que es de i va en 2a78d6 (el bloque log i: su fila, su escala, su aceptación), lo que es de
f en eb6834 (el bloque log f) y p en 1baf7a (las simulaciones de p, su media y su banda). Las cadenas NO son
cantidades del modelo: van en tinta, con cuatro tonos (16211E, 55645F, 8A9893, B4C2BD), a 1 px y 70 %, con etiqueta
directa. Ningún texto en color de serie: cada fila de la tira lleva un subrayado de 4 px de su color. Títulos Source
Serif 4 600 a 54 px; texto Source Sans 3 (etiquetas 24-28 px); números IBM Plex Mono; fórmulas KaTeX 30-44 px;
narración 42 px, en una línea.

DISPOSICIÓN: columna izquierda x 96-900 (la tira de θ arriba, y 236-418; debajo las fórmulas y la lectura, o los
gráficos de la adaptación); columna derecha x 1230-1750 (el plano de dos nudos, 520 x 520, y 290-810). Las tomas 5 y 6
usan el ancho completo (x 200-1730).

MOVIMIENTO: textos que entran en 0.3 s (EASE.entrarSuave) y salen en 0.24 s; la propuesta se traza en 0.45 s
(EASE.mover) y el punto que se acepta viaja en 0.5 s (EASE.mover). Las iteraciones avanzan LINEALES: 2 a 9 a 2 por
segundo; el calentamiento (iteraciones 10 a 10 000) en 10 s; el muestreo (10 001 a 50 000) en 3.5 s; las trazas de las
cuatro cadenas en 3 s; las 1000 simulaciones de p en 4 s. Un movimiento de cámara declarado: 45.5-46.1 s el plano se
acerca de [-8.2, -3] x [-9.5, -4.3] a [-7.3, -5.3] x [-6.9, -4.9] (EASE.camara), justo antes de que la elipse tome la
forma L_b (iteración 5000, 46.2 s). Ninguna marca de aceptar/rechazar se enciende más de 2 veces por segundo. Las
salidas de acto se escalonan (tira 55.9, gráficos 56.1, plano 56.3) y el acto siguiente entra encimado (56.2, 71.65).

TOMAS (segundos):
0.0       cabecera + la tira de θ: fila «log i» (7 celdas: 30 40 50 60 70 80 95) y fila «log f» (cuadro 0).
0.4-4.6   N0. La posterior de θ se recorre por sexo con Metropolis-Hastings adaptativo.
4.9-9.6   N1. Dos bloques: llave «bloque 1» sobre log i (5.3) y «bloque 2» sobre log f (7.2).
9.9-15.2  N2. Entra el plano (9.9): log i a los 50 (x) y a los 60 años (y); las celdas 50 y 60 se marcan (10.3); el
          punto inicial de la cadena 1 con su etiqueta (10.7).
15.5-20.3 N3. θ' = θ + s_b L_b z, z ~ N(0, I) (15.7), KaTeX 40 px; cada bloque, por turno.
20.6-25.9 N4. min(1, e^{log post(θ') - log post(θ)}) (20.8), 44 px.
26.3-31.4 N5. Iteración 1 en detalle: la propuesta (hueca) se traza (26.4), la lectura (26.9): log post(θ') -
          log post(θ) = -31.49, log u = -1.87; «se rechaza» (27.9); la propuesta baja al 25 %.
31.6-35.4 N6. Iteraciones 2-9, una cada 0.5 s, todas rechazadas; un contador «rechazadas: k».
35.6-40.4 N7. Iteración 10: +6.68 > log u = -2.09; «se acepta» (36.6); la propuesta, de 12 px y en verde azulado; el
          punto viaja (37.3-37.8).
40.6      salen las fórmulas, la lectura y las propuestas (0.24 s).
40.8-51.2 N8, N9. Calentamiento: el gráfico de aceptación por ventana (40.8) y el de s_b (41.4) se llenan ventana a
          ventana, lineales con las iteraciones (10 a 10 000 en 41.2-51.2); en el plano, el recorrido de las últimas
          300 iteraciones, la elipse de una desviación de la propuesta proyectada (s_b^2 (L_b L_b')[J, J]) y un contador
          de iteraciones de 28 px. Cámara 45.5-46.1. En la iteración 5000 (46.2) la elipse toma la forma L_b y aparece
          la marca «forma L_b» en k = 50.
51.2-56.1 N10. Fin del calentamiento: etiquetas log i / log f al final de las escalas; la aceptación de la cadena 1
          después del calentamiento (0.35 log i, 0.33 log f); se estampan las 4000 simulaciones guardadas de la
          cadena 1 (52.3-55.8).
56.5-61.8 N11. Trazas de log i a los 50 años, 4 cadenas x 4000 (56.5-59.5); la línea de las mitades en 2000 (60.0) y
          R-hat = 1.002 (61.0).
62.1-66.0 N12. ESS = 5503 (62.4), «la misma precisión para la media».
66.3-71.4 N13. Compuertas en los 28 parámetros (dos sexos): R-hat máximo 1.002 < 1.01; ESS mínimo 4202 ≥ 400 (66.6).
71.8-76.4 N14. Ejes de p por edad (71.65); la primera simulación sola, 3 px, trazada lineal en la edad (72.3-73.1);
          desde 74.0 las 1000 simulaciones, lineales hasta 78.0, cada una al 7 % (la primera baja con ellas).
76.7-81.3 N15. Se toman 1000 de los 16 000 θ guardados, equiespaciados.
81.6-87.0 N16. Banda del 95 % (81.9) y media (82.5-83.7, con un borde de 6.5 px del fondo); las simulaciones bajan al
          1.2 %; a los 80 años (84.0): guía y marcas en verde azulado, punto en la media, guía hasta la lectura (media e
          intervalo).
87.0-90.0 final quieto.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, más de 3
destellos por segundo, números que no salgan del paquete, notación distinta de la guía «El modelo». Colorear las
cadenas con los matices de i, f o p. Decir que la aceptación final es 0.28 (es la meta de la adaptación; después del
calentamiento la cadena 1 acepta 0.35 y 0.33). Decir que cada θ guardado es una de las simulaciones (son 1000 de 16 000). Contornos de una posterior que no es la del paquete.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos y auditoría (auditar.mjs). La auditoría
mide lo dibujado contra algo independiente: lp(θ) y lp(θ') de cada propuesta dibujada se recalculan con
DL.modelo.logPosterior sobre el contexto exportado; θ' = θ + s_b z se recalcula con DL.modelo.propuestaMh; la decisión
con DL.modelo.aceptaMh y la u anotada; la cadena es coherente (θ siguiente = θ' si se aceptó, θ si no); las escalas por
ventana se rehacen con DL.modelo.escalaAdaptada desde 2.38/√7; la aceptación de cada ventana es la cuenta de las
aceptadas anotadas; L_b L_b' coincide con la covarianza del recorrido de las iteraciones 1-5000; R-hat y ESS dibujados
= DL.modelo.rhat/ess sobre las cadenas exportadas = dl_rhat/dl_ess de R; las curvas de p de las simulaciones son
DL.modelo.edoCtx(θ); la media y la banda = la media y los percentiles 2.5 y 97.5 (tipo 7) recalculados = dl_estimaciones().
```
