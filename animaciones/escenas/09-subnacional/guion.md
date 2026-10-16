# 09 · De lo nacional a lo subnacional — guion

El guion dirigido con que se hizo la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO,
MOVIMIENTO, TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 7; `vignettes/articles/desagregacion-subnacional.qmd`; la
cabecera de `R/cascada.R` (orden de los sorteos, `.dl_dX`, `.dl_pesos_edad`, `.dl_renormalizar`).
**Datos:** `datos/r/09-subnacional.R`: `dl_cascada(ajuste, insumos, semilla = 1)` sobre `ctx$ajuste()`, la misma
llamada que hace `dl_correr()`. Se sigue a las mujeres (`sex_id` 2), la simulación k = 1, Huancavelica (09) y la banda
de población de 70 a 74 años (`age_group_id` 19). Los sorteos de X_nac, X_d y β_c se repiten con las funciones
internas del paquete y se comprueba que reproducen `cascada$dX` (diferencia máxima 0 en las 440 000 filas) y las tasas
que resolvió el paquete (diferencia máxima de log i y log f: 0). El modo plano sale de una copia del ejemplo con
`subnacional.modo: plano`, como en la guía. Datos sintéticos (causa 9100) con la geografía real del Perú; no hay mapa
en el paquete, así que las 25 ubicaciones van en un gráfico de puntos ordenado.

```text
OBJETIVO: quien mira entiende los cuatro pasos del modo covariables, para cada sexo y simulación k: (1) la diferencia
dX entre la covariable de la ubicación y la nacional, en la escala de su transformación T y con ambos valores
sorteados; (2) el desplazamiento δ_c(a) = κ β_c Σ_b dX_b P_ba de log i (SEV, LDI) o de log f (HAQ, bajo su techo);
(3) la nueva solución de dp/da; (4) la renormalización, que hace que los casos de las ubicaciones sumen exactamente los
nacionales. Sale sabiendo además qué hacen los modos plano y razón y que los datos subnacionales del año de
validación no entran al ajuste.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js, lib/movimiento.js y
lib/dibujo.js. Sin lib/modelo.js: todo número viene de datos/09-subnacional.js.

TIEMPO: 90 s a 30 cuadros/s = 2700 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera, la fila de
pasos, el texto del punto de partida y la curva nacional de p). La última frase se sostiene hasta el final (6.3 s).

ASPECTO: fondo F5F7F6, superficie FFFFFF, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, marca
0E6B62. Series solo para las tres cantidades del modelo: i 2a78d6, f eb6834, p 1baf7a; la nacional en trazo de
guiones 10 6 y 3 px, la de la ubicación en trazo lleno de 4 px, del mismo matiz. Lo que viene de fuera del modelo va
en tinta con forma propia: el valor nacional de cada covariable es una raya vertical de 3 px con su intervalo del
95 % como franja (guía al 55 %); cada ubicación, un círculo hueco de radio 6; la población (los casos N q) son
segmentos verdes de p (son casos prevalentes) con separadores blancos de 1.5 px, y el total nacional, una caja de borde
tinta de 2.5 px. La marca (0E6B62) señala a Huancavelica (anillo de 3 px, bigote de su error), las flechas dX y el
paso activo. Ningún texto en color de serie: en las fórmulas, i, f y p llevan un subrayado de 4 px de su color.
Títulos Source Serif 4 600 (cabecera 54 px, títulos de bloque 34 px); texto Source Sans 3 (26 px, notas 24 px);
números IBM Plex Mono 24-26 px; fórmulas KaTeX a 32-34 px; narración 36 px; nada legible por debajo de 22 px.
Retícula: columna izquierda x 96-960 (fórmulas y texto), gráfico a la derecha x 1080-1800, y 300-900. La fila de
pasos va bajo la cabecera, y 226-262: «1 · Diferencia», «2 · Desplazamiento», «3 · Nueva solución»,
«4 · Renormalización», «Otros modos»; el activo en tinta 600 con una barra de marca de 4 px debajo, el resto en tinta
suave.

MOVIMIENTO: entradas de texto 0.4 s con EASE.entrarSuave (visible()) y salidas de 0.24 s con EASE.salir. Lo
secundario arranca 0.15-0.45 s después de lo principal. Los puntos de un grupo entran escalonados 30 ms
(ESCALONADO.letra) y los segmentos de la barra, 20 ms. Las franjas de la covariable crecen desde su raya en 0.5 s
(EASE.entrar). Las flechas dX se dibujan en 0.6 s (EASE.mover), escalonadas 0.2 s. La curva de dX(a) del SEV y la
curva de p de la ubicación se trazan lineales en la edad (la edad avanza a ritmo constante), en 1.5 s y 3.0 s. El
desplazamiento de log i (héroe) se interpola en log de la curva nacional a la de la ubicación en 0.8 s con EASE.mover;
el de log f, 0.9 s después, en 0.6 s. La barra apilada se escala por el factor en 0.75 s (MS.heroe, EASE.mover) y los
puntos se corren 0.15 s después en 0.6 s; en el modo plano, los puntos se juntan en el valor nacional en 0.8 s
(EASE.mover). El cambio de gráfico entre pasos es un fundido en el mismo lugar (sale 0.24 s, entra 0.4 s), nunca un
desplazamiento: no hay movimientos de cámara. Nada se mueve mientras se lee.

TOMAS (segundos; las frases de la narración, abajo, marcan los cortes; narración a 42 px, en una línea):
0.0-11.3   punto de partida. Derecha: p nacional por edad (mujeres, k = 1), quieta desde el cuadro 0. Izquierda: el
           texto del ajuste nacional; en 5.6 entra la tabla betas (SEV, LDI: prevalencia, log, mueven log i; HAQ:
           mortalidad_exceso, lineal, mueve log f), una fila cada 0.15 s.
11.54-30.1 paso 1. Derecha: tres franjas en la escala de T (SEV de 70-74 años, con la raya nacional estandarizada por
           edad; LDI y HAQ de todas las edades), escalonadas 0.3 s, con las 25 ubicaciones (círculos huecos, por
           rango) y la raya nacional. Tres tiempos para la frase de los sorteos: 18.1 crecen las franjas del
           intervalo nacional; 19.8 Huancavelica con su bigote ±1.96 se_T; 21.3 los sorteos de la simulación 1
           (triángulo: X_nac; punto: X_d), con su leyenda. 24.8: flechas dX con su valor. Izquierda: X_nac y sd_T
           (12.45), X_d (18.1), dX (24.8).
30.34-37.5 paso 2, P_ba. Derecha: dX_b del SEV de Huancavelica en el punto medio de cada banda y la línea exacta
           Σ_b dX_b P_ba (vértices en los puntos medios), trazada de 30 a 99 (31.4-32.9); «vale 0 antes de los 40».
37.74-49.0 paso 2, desplazamiento. Izquierda: β_c ~ N(β, sd_β), δ_c(a) y κ = 1; la tabla β_c (k = 1),
           Σ_b dX_b P_ba y δ_c a los 70 años; 43.85 log i_d, 44.75 log f_d y la regla de efecto_sobre. Derecha: i y
           f por edad en escala log (por 1000), nacional en guiones; 43.7-44.5 la curva de i se desplaza (héroe);
           44.4 el techo f_max = 250 por 1000 («aquí no se alcanza»); 44.6-45.2 la curva de f.
49.24-55.2 paso 3. Izquierda: dp/da con i_d y f_d, misma remisión. Derecha: p nacional en guiones y p de
           Huancavelica trazada 50.0-53.0; 53.2 la banda de 70 a 74 años marcada en el eje, con 6.27 % y 5.27 %.
55.44-71.6 paso 4. Derecha: las 25 ubicaciones por su prevalencia media de 70-74 años antes de renormalizar
           (círculos huecos; nombres solo de los extremos, Lima y Huancavelica), con la raya nacional. Izquierda: la
           caja de N_nac q_nac y la barra apilada Σ_d N_d q_d, 2.0 % más larga. 61.1 el factor (y el de i(1-p)).
           67.2-67.95 la barra se escala por el factor y cierra exactamente en la caja (héroe); 67.35 los puntos se
           corren y se llenan.
71.9-77.9  modo plano: los puntos se juntan en el valor nacional (72.3-73.1); factor = 1.
78.2-83.4  modo razón: R_dj y q_dj (fórmulas de la guía); la cascada es la plana; el gráfico baja al 40 %.
83.7-90.0  validación: los datos subnacionales no entran al ajuste; con una cascada por covariables, los de 2019
           validan. 84.1-84.9 los puntos vuelven al gradiente renormalizado, al 100 %: es la imagen final.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, un mapa (no
hay geometría en el paquete), números que no salgan del paquete, una notación distinta de la guía «El modelo», texto
en color de serie, i nacional y de la ubicación en matices distintos.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs) con estas
comprobaciones: los sorteos repetidos reproducen cascada$dX (diferencia 0) y la dX que se muestra es la del paquete;
log i_d − log i_nac = Σ δ_c de i y log f_d = min(log f_nac + Σ δ_c de f, log f_max) (diferencia 0) y la curva
dibujada al final del desplazamiento es la del paquete; la p dibujada de Huancavelica cumple dp/da (diferencia central fuera de los nudos, < 1 %); la línea Σ_b dX_b P_ba es la de .dl_pesos_edad();
el factor recalculado, N_nac q_nac / Σ_d N_d q_d, es el del paquete (1e-12) y el que se muestra es su redondeo;
después de renormalizar, Σ_d N_d q_d = N_nac q_nac a 1e-10 y q_d coincide con cascada$draws_q; en el modo plano el
factor es 1 y los puntos quedan en el valor nacional; κ en pantalla es el de la cascada.

REVISIÓN ADVERSARIAL (metodólogo y diseñador, en paralelo): ver notas.md. Cambió la leyenda de los sorteos (X_nac y
X_d por separado), P_ba pasó al paso 2, la tabla del paso 2 lleva Σ_b dX_b P_ba, el techo va en las unidades del eje,
la renormalización dice «un factor para p y otro para i(1-p)», el modo razón dice que la cascada es la plana, la
validación exige una cascada por covariables, el paso 3 muestra la banda de 70-74 años (los números del paso 4), los
fundidos encadenados sin cuadros vacíos, los sorteos en tres tiempos, el gráfico de puntos con menos nombres y el
final con el gradiente.
```
