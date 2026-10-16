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
iteraciones; que la convergencia se mide con R-hat partido y ESS (compuertas 1.01 y 400); y que cada θ guardado es
una simulación de p, cuya media e intervalo (percentiles 2.5 y 97.5) son la estimación.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js y lib/modelo.js.

TIEMPO: 88 s a 30 cuadros/s = 2640 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y la tira
de θ con sus dos bloques) y el final se sostiene 4 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, verde azulado 0E6B62 para lo que se señala
(los dos nudos del plano, la aceptación, la línea de 0.28, la forma L_b, las compuertas). Series: lo que es de i va en
2a78d6 (el bloque log i: su fila, su escala, su aceptación), lo que es de f en eb6834 (el bloque log f) y p en 1baf7a
(las simulaciones de p, su media y su banda). Las cadenas NO son cantidades del modelo: van en tinta, con cuatro tonos
(16211E, 55645F, 8A9893, B4C2BD) y etiqueta directa. Ningún texto en color de serie: cada fila de la tira lleva un
subrayado de 4 px de su color. Títulos Source Serif 4 600 a 54 px; texto Source Sans 3 (etiquetas 26-28 px);
números IBM Plex Mono; fórmulas KaTeX 34-40 px; narración 42 px, en una línea.

DISPOSICIÓN: columna izquierda x 96-900 (la tira de θ arriba, y 262-430; debajo las fórmulas o los gráficos de la
adaptación); columna derecha x 1190-1750 (el plano de dos nudos, 560 x 560, y 270-830). Las tomas 5 y 6 usan el ancho
completo (x 200-1800).

MOVIMIENTO: textos que entran en 0.3 s (EASE.entrarSuave) y salen en 0.24 s; la propuesta se traza en 0.45 s
(EASE.mover) y el punto que se acepta viaja en 0.5 s (EASE.mover). Las iteraciones avanzan LINEALES: 2 a 9 a 2 por
segundo; el calentamiento (iteraciones 10 a 10 000) en 10 s; el muestreo (10 001 a 50 000) en 3.5 s; las trazas de las
cuatro cadenas en 3 s; las 1000 simulaciones de p en 5 s. Un movimiento de cámara declarado: 51.4-52.2 s el plano se
acerca de [-8, -3] x [-9.5, -4.5] a [-7.3, -5.3] x [-6.9, -4.9] (EASE.camara). Ninguna marca de aceptar/rechazar se
enciende más de 2 veces por segundo.

TOMAS (segundos):
0.0       cabecera + la tira de θ: fila «log i» (7 celdas: 30 40 50 60 70 80 95) y fila «log f» (cuadro 0).
0.4-4.6   N0. La posterior de θ se recorre por sexo con Metropolis-Hastings adaptativo.
4.9-9.6   N1. Dos bloques: llave «bloque 1» sobre log i (5.3) y «bloque 2» sobre log f (7.2).
9.9-14.8  N2. θ' = θ + s_b L_b z, z ~ N(0, I) (10.1), KaTeX 40 px.
15.1-20.2 N3. min(1, e^{log post(θ') - log post(θ)}) (15.3).
20.4-26.0 N4. Entra el plano (20.4): log i a los 50 (x) y a los 60 años (y); las celdas 50 y 60 de la tira se marcan
          en verde azulado (20.8); el punto inicial de la cadena 1 (θ0 + N(0, 0.1)) con su etiqueta (21.2).
26.3-31.4 N5. Iteración 1 en detalle: la propuesta (hueca) se traza (26.4), la lectura (26.9): Δ = log post(θ') -
          log post(θ) = -31.49, log u = -1.87; «se rechaza» (27.9); la propuesta baja al 25 %.
31.6-35.6 N6. Iteraciones 2-9, una cada 0.5 s, todas rechazadas; un contador «rechazadas: k».
35.6-40.6 N7. Iteración 10: Δ = +6.68 > log u = -2.09; se acepta (36.9) y el punto viaja (37.0-37.5).
40.6      salen las fórmulas y la lectura (0.24 s).
40.8-51.2 N8, N9. Calentamiento: los gráficos de aceptación por ventana (y 470-640) y de s_b (y 700-860) se llenan
          ventana a ventana, lineales con las iteraciones (10 a 10 000 en 41.2-51.2); en el plano, el recorrido de las
          últimas 300 iteraciones y la elipse de una desviación de la propuesta (s_b^2 L_b L_b' proyectada). En la
          iteración 5000 (46.2) la elipse toma la forma L_b y aparece la marca «forma L_b» en k = 50.
51.2-56.2 N10. Fin del calentamiento: la aceptación de la cadena 1 después del calentamiento (0.35 log i, 0.33 log f).
          Cámara 51.4-52.2; luego se estampan las 4000 simulaciones guardadas de la cadena 1 (52.3-55.8).
56.3      sale todo menos la cabecera.
56.6-61.8 N11. Trazas de log i a los 50 años, 4 cadenas x 4000 (56.9-59.9); la línea de las mitades en 2000 (60.0) y
          R-hat = 1.002 (61.0).
62.1-66.0 N12. ESS = 5503 (62.4).
66.3-71.6 N13. Compuertas en los 28 parámetros (dos sexos): R-hat máximo 1.002 < 1.01; ESS mínimo 4202 ≥ 400 (66.6).
71.8      sale la toma.
72.1-78.0 N14. Ejes de p por edad (72.1); las 1000 simulaciones de p, lineales (72.6-77.6), cada una al 7 %.
78.3-84.0 N15. Banda del 95 % (78.6) y media (79.0-80.2); las simulaciones bajan al 3 %; a los 80 años: media e
          intervalo (81.0).
84.0-88.0 final quieto.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, más de 3
destellos por segundo, números que no salgan del paquete, notación distinta de la guía «El modelo». Colorear las
cadenas con los matices de i, f o p. Decir que la aceptación final es 0.28 (es la meta de la adaptación; después del
calentamiento la cadena 1 acepta 0.35 y 0.33). Contornos de una posterior que no es la del paquete.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos y auditoría (auditar.mjs). La auditoría
mide lo dibujado contra algo independiente: lp(θ) y lp(θ') de cada propuesta dibujada se recalculan con
DL.modelo.logPosterior sobre el contexto exportado; θ' = θ + s_b z se recalcula con DL.modelo.propuestaMh; la decisión
con DL.modelo.aceptaMh y la u anotada; la cadena es coherente (θ siguiente = θ' si se aceptó, θ si no); las escalas por
ventana se rehacen con DL.modelo.escalaAdaptada desde 2.38/√7; la aceptación de cada ventana es la cuenta de las
aceptadas anotadas; L_b L_b' coincide con la covarianza del recorrido de las iteraciones 1-5000; R-hat y ESS dibujados
= DL.modelo.rhat/ess sobre las cadenas exportadas = dl_rhat/dl_ess de R; las curvas de p de las simulaciones son
DL.modelo.edoCtx(θ); la media y la banda = la media y los percentiles 2.5 y 97.5 (tipo 7) recalculados = dl_estimaciones().
```
