# 11 · Una corrida, de principio a fin — guion

El guion dirigido con que se hizo la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO,
MOVIMIENTO, TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `README.md` («Cómo se usa: tres comandos»), `R/herramientas.R` (`dl_correr()` y
`.dl_correr_proyecto()`: el orden de los pasos), `R/corrida.R` (las dos compuertas, la carpeta y el manifiesto),
`vignettes/articles/corridas-y-consolidado.qmd`, `vignettes/articles/diagnostico-y-sensibilidad.qmd` y la sección 8
de `vignettes/el-modelo.qmd` (la fórmula de los AVD).
**Datos:** `datos/r/11-corrida.R`: `dl_revisar_proyecto(dl_ejemplo(), causa = 9100)` y
`dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE)` en una carpeta temporal; de la corrida se leen
los mensajes, `manifest.yaml`, `diagnostics/`, `etiquetas/` y `cause/`. El orden de los pasos se comprueba en R contra
el cuerpo de `.dl_correr_proyecto()` y contra el orden de los mensajes de la corrida.

```text
OBJETIVO: quien mira sabe que una corrida son tres comandos (dl_nuevo_proyecto, dl_revisar_proyecto, dl_correr) y
puede recitar los trece pasos de esta corrida de dl_correr() en su orden real: insumos, ajuste nacional, compuerta de
convergencia, ajuste solo con el ancla, cascada, validación, compuerta del ancla, factor de comorbilidad, AVD,
etiquetas, sensibilidad, resumen y carpeta con su manifiesto. Sabe qué escena de la serie explica cada paso, que
rapido = TRUE es una prueba (cadenas cortas, salta la primera compuerta y lo declara) y que la de producción usa
1000 simulaciones y 4 cadenas de 50 000 iteraciones.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 90 s a 30 cuadros/s = 2700 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y las
tres tarjetas de los comandos); la carpeta entra en 81.8 s, los trece pasos vuelven al 100 % desde 86.6 s y se
sostiene hasta 90 s.

ASPECTO: fondo F5F7F6, superficie FFFFFF, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, verde
azulado de la marca 0E6B62 para lo que se señala (el paso activo, las compuertas, la tarjeta activa). Color de
serie solo para p (1baf7a: los 25 departamentos de la cascada, el punto de la prevalencia del resumen y el subrayado
de p en la fórmula de los AVD, sin cortes); i y f no aparecen. Diagnósticos, cadenas, factores y celdas en tinta o
guía, con forma propia (barras, puntos huecos, cuadritos). Títulos de panel Source Serif 4 600 a 40 px; funciones y
rutas IBM Plex Mono 22-30 px; las lecturas de los diagnósticos (2.31, 11.4, 0.033) en mono 40 px; texto Source Sans 3
24-28 px; narración 42 px en una línea. Nada legible bajo 22 px.
Retícula: columna izquierda 96-800 px (tarjetas, luego la lista de pasos: filas de 44 px desde y = 306); panel derecho
880-1824 px (título en y = 288; «corrida de prueba» fija arriba a la derecha desde el primer paso).

MOVIMIENTO: textos que entran en 0.4 s con EASE.entrarSuave y salen en 0.24 s con EASE.salir; los elementos de un
grupo, escalonados 0.12-0.15 s para que nunca entren tres en 0.1 s. El marcador del paso activo (barra de 6 x 34 px
de la marca y fondo blanco de la fila) baja de fila en 0.5 s con EASE.mover y llega cuando empieza la frase; el
panel nuevo entra 0.15 s después y el anterior sale 0.5 s antes. Filas futuras al 40 %, la activa al 100 % (las
compuertas en la marca solo cuando están activas), las hechas al 70 %; la ficha de escena solo en la fila activa.
Las barras de producción crecen en 1.2 s (EASE.mover); los puntos de los diagnósticos recorren su escala en
0.75 s (MS.heroe) desde el umbral; los 25 departamentos salen de la línea nacional hacia su valor en 0.75 s,
escalonados 0.04 s. Un movimiento de cámara declarado, 16.1-16.9 s (EASE.camara): la tarjeta de dl_correr() sube
de y = 560 a y = 236 y queda como encabezado de la lista; las otras dos salen antes. Nada se mueve mientras se lee.

TOMAS (segundos; cada una empieza con su frase):
0.0        cabecera + tres tarjetas quietas (cuadro 0).
0.4-3.5    tres comandos, en este orden.
3.8-7.8    tarjeta 1 activa; a la derecha, las 8 tablas que trae dl_ejemplo().
8.1-12.4   tarjeta 2 activa; cada tabla recibe su marca y sus filas (dl_revisar_proyecto()); «16 comprobaciones,
           0 errores».
12.7-15.9  tarjeta 3 activa; la llamada exacta, dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE,
           carpeta_salida = tempdir()), y el aviso de la corrida «Sus números no sirven para publicar». Se queda
           hasta 21.4 s.
16.1-16.9  cámara: tarjetas 1 y 2 salen, la 3 sube a encabezado.
17.0-21.6  los 13 pasos entran escalonados (0.12 s) y después sus fichas de escena.
21.9       1 dl_insumos(): hash (12 cifras); 25 filas nacionales no entran, 160 subnacionales solo validan.
25.4       2 dl_ajustar(): cadenas a escala (50 000 iteraciones = 860 px); primero la prueba (2 x 2000), en 29.6
           crecen las de producción (4 x 50 000).
33.8       3 compuerta 1: R-hat máximo 2.31 (límite 1.01), ESS mínimo 11.4 (mínimo 400), 28 parámetros (14 por sexo);
           en 38.5 «no pasa» y validacion.gates.force: true.
43.1       4 dl_ajustar_solo_prior(): con datos frente a solo con el ancla; en el ejemplo, el mismo ajuste.
47.4       5 dl_cascada(): 25 departamentos (prevalencia, mujeres de 70 a 74) desde la línea nacional (5.59 %);
           renormalización 0.933 a 1.029.
51.7       6 dl_validar_ancla(): error relativo mediano 0.033 en 0-0.06; también incidencia implícita y amplitud.
55.7       7 compuerta 2: máximo 0.05 (en la configuración); «pasa»; ni rapido ni forzar la saltan.
60.3       8 dl_factor_comorbilidad(): COMO_b = AVD^ancla_b / (p^ancla_b Σ_e π_e DW_e) y sus 24 valores (0.73-0.95).
64.2       9 dl_avd(): AVD_{k,b} = p_{k,b} (Σ_e π_{k,e} DW_{k,e}) COMO_b; 79.4 por 100 000 (mujeres de 70 a 74).
68.3       10 dl_etiquetas(): 624 cuadritos, todos prior-driven; 24 nacionales, las subnacionales heredan.
72.8       11 dl_sensibilidad(): grilla 3 x 3 de (λ, ρ), 2 valores de κ; 9 ajustes × 2 κ = 18 cascadas.
77.8       12 dl_resumir(): 3 tablas de 624 filas (26 ubicaciones, 2 sexos, 12 bandas); prevalencia nacional 5.59 %.
81.8-90.0  13 dl_exportar_corrida(): árbol (6 entradas, 37 archivos) unido a manifest.yaml (inputs.bundle_hash,
           configuracion.por_defecto, validacion.gates.force, limitaciones, decisiones); inputs/contrato/ repite la
           corrida; desde 86.6 s los trece pasos vuelven al 100 %, uno por uno.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo», un orden de los pasos distinto del código,
colores de serie en algo que no sea p, diagnósticos o resultados de la prueba presentados como de producción, una
llamada que no corre tal como se muestra.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs) contra fuentes
independientes del dibujo: las filas bajan en el orden de las llamadas en el código de .dl_correr_proyecto() y en el
de los mensajes de la corrida; el punto de cada compuerta está del lado del umbral que dicen los números (R-hat y
ESS del manifiesto = los de diagnostics/mcmc.csv); un punto de cascada por ubicación subnacional de la tabla
ubicaciones; la grilla de sensibilidad = la de la configuración, y sensibilidad.csv tiene una fila por ajuste, κ, sexo
y banda; las barras a escala del eje; celdas = ubicaciones × sexos × bandas; el árbol suma todos los archivos.
```
