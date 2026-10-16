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
puede recitar los doce pasos de dl_correr() en su orden real: insumos, ajuste nacional, compuerta de convergencia,
ajuste solo con el ancla, cascada, validación, compuerta del ancla, AVD, etiquetas, sensibilidad, resumen y carpeta
con su manifiesto. Sabe qué escena de la serie explica cada paso, que rapido = TRUE es una prueba (cadenas cortas,
salta la primera compuerta y lo declara) y que la de producción usa 1000 simulaciones y 4 cadenas de 50 000
iteraciones.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 90 s a 30 cuadros/s = 2700 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera y las
tres tarjetas de los comandos) y la carpeta final se sostiene 4 s después de que entra.

ASPECTO: fondo F5F7F6, superficie FFFFFF, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, verde
azulado de la marca 0E6B62 para lo que se señala (el paso activo, las compuertas, la tarjeta activa). Color de
serie solo para p (1baf7a: los 25 departamentos de la cascada y el subrayado de p en la fórmula de los AVD); i y f
no aparecen como series. Diagnósticos, cadenas y celdas en tinta o guía, con forma propia (barras, puntos huecos,
cuadritos). Títulos de panel Source Serif 4 600 a 40 px; funciones y rutas IBM Plex Mono 24-30 px; texto
Source Sans 3 24-28 px; narración 36 px. Nada legible bajo 22 px.
Retícula: columna izquierda 96-800 px (tarjetas, luego la lista de pasos: filas de 47 px desde y = 300);
panel derecho 880-1824 px (contenido del paso activo, encabezado en y = 280).

MOVIMIENTO: textos que entran en 0.4 s con EASE.entrarSuave y salen en 0.24 s con EASE.salir; los elementos de un
grupo, escalonados 0.12 s (ESCALONADO.fila) para que nunca entren tres en el mismo cuadro. El marcador del paso
activo (barra de 6 x 34 px de la marca y fondo blanco de la fila) baja de fila en 0.5 s con EASE.mover. Las barras
de las cadenas de producción crecen en 1.2 s con EASE.mover (el movimiento principal de su toma). Los puntos de los
diagnósticos recorren su escala en 0.75 s (MS.heroe) con EASE.mover. Un movimiento de cámara declarado, 18.5-19.3 s
(EASE.camara): la tarjeta de dl_correr() sube de y = 560 a y = 236 y queda como encabezado de la lista de pasos;
las otras dos tarjetas salen antes, en 0.24 s. Nada se mueve mientras se lee.

TOMAS (segundos):
0.0        cabecera + tres tarjetas quietas (cuadro 0).
0.4-3.6    narración 1. Tres comandos, en este orden.
3.9-8.6    tarjeta 1 activa (borde de la marca); a la derecha, las 8 tablas del contrato del ejemplo, escalonadas.
9.2-13.6   tarjeta 2 activa; cada tabla recibe su marca y su número de filas (de dl_revisar_proyecto()); al pie,
           «16 comprobaciones: 0 error(es) y 4 aviso(s)».
13.9-18.3  tarjeta 3 activa; las tablas salen; entra la llamada dl_correr(dl_ejemplo(), causa = 9100, semilla = 1,
           rapido = TRUE) en mono 28.
18.5-19.3  cámara: tarjetas 1 y 2 salen, la 3 sube a encabezado.
19.4-24.3  los 12 pasos entran escalonados (0.12 s), y después sus fichas de escena (esc. 04, 02-08, ...).
24.6-28.5  paso 1 dl_insumos(): hash de los insumos (12 cifras) y las 25 filas nacionales de datos que quedan fuera.
28.8-36.8  paso 2 dl_ajustar(): barras de las cadenas a escala (50 000 iteraciones = 860 px), calentamiento en guía y
           muestreo en la marca; primero la prueba (2 x 2000), después crecen las de producción (4 x 50 000).
37.1-47.0  paso 3 compuerta 1: R-hat máximo 2.31 en la escala 1-2.5 (límite 1.01) y ESS mínimo 11.4 en 0-500
           (mínimo 400); «no pasa»; validacion.gates.force: true.
47.3-51.0  paso 4 dl_ajustar_solo_prior(): con datos frente a solo con el ancla, la referencia de las etiquetas.
51.3-55.2  paso 5 dl_cascada(): los 25 departamentos (prevalencia de mujeres de 70 a 74) como puntos p, escalonados
           de menor a mayor, y la línea nacional; factores de renormalización 0.933 a 1.029.
55.5-59.3  paso 6 dl_validar_ancla(): error relativo mediano 0.033 en la escala 0-0.06.
59.6-63.6  paso 7 compuerta 2: el máximo 0.05; «pasa»; ni rapido ni forzar la saltan.
63.9-68.1  paso 8 dl_avd(): AVD_{k,b} = p_{k,b} (Σ_e π_{k,e} DW_{k,e}) COMO_b y la celda de mujeres de 70 a 74.
68.4-72.9  paso 9 dl_etiquetas(): 624 cuadritos (26 ubicaciones x 2 sexos x 12 bandas), todos prior-driven.
73.2-77.1  paso 10 dl_sensibilidad(): la grilla 3 x 3 de (λ, ρ) y los 2 valores de κ.
77.4-81.6  paso 11 dl_resumir(): las tres tablas de celdas (624 filas cada una) y la prevalencia nacional de mujeres
           de 70 a 74 con su intervalo.
81.9-90.0  paso 12 dl_exportar_corrida(): el árbol de la carpeta (6 entradas, 37 archivos) y el manifiesto (hash,
           14 claves por defecto, gates.force, 5 limitaciones); inputs/contrato/ repite la corrida. Se sostiene.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo», un orden de los pasos distinto del código,
colores de serie en algo que no sea p, diagnósticos de la prueba presentados como de producción.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs): el orden
dibujado de los pasos es el exportado (que R comprobó contra el código y los mensajes); en cada marca de paso, la
fila activa es la de ese paso; los números en pantalla (R-hat, ESS, error del ancla, cadenas, filas, celdas, grilla,
departamentos, archivos) son los exportados; la prueba no pasa la compuerta 1 y pasa la 2; 624 celdas = 26 x 2 x 12.
```
