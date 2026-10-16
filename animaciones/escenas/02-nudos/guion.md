# 02 · Las tasas por edad: nudos e interpolación — guion

El guion dirigido de la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO, MOVIMIENTO,
TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 2; `R/edo.R` (cabecera, `.dl_base_interp()`, `dl_edo()`);
`R/configuracion.R` (los `nudos` por defecto).
**Datos:** `datos/r/02-nudos.R`: `dl_edo()` con las tasas de la sección 2 de la guía (valores inventados), la matriz
W del paquete (`dismodlite:::.dl_base_interp()`) en la malla h/2 y en las edades 63, 60 y 97, y `dl_edo()` con
log i en 60 + log 3.

```text
OBJETIVO: quien mira entiende que los parámetros son θ = (log i_1, …, log i_K, log f_1, …, log f_K), los logaritmos
de las dos tasas en los K nudos; que entre nudos log i y log f son lineales y fuera de ellos constantes, lo que en una
malla es multiplicar por W (cada fila suma 1, en un nudo devuelve su valor); que por eso i crece o decrece
exponencialmente entre nudos; que dl_edo() interpola, resuelve (con r y p(a0) dados) y devuelve p, i y f por edad; y
que mover un nudo cambia la tasa solo entre sus vecinos pero p desde ahí en adelante.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js y lib/modelo.js.

TIEMPO: 78 s a 30 cuadros/s = 2340 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera, los ejes
de las tasas en escala log con los nudos marcados en el eje de la edad) y el final se sostiene 6.5 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, verde azulado de la marca
0E6B62 para lo que se señala (el cursor de la edad a_j, la fila de W, el nudo que se mueve, los tramos que cambian).
Series: i 2a78d6, f eb6834, p 1baf7a, y nada más lleva esos colores. Ninguna letra en color de serie: en las
fórmulas, log i y log f llevan un subrayado de 4 px de su color (sin cortes). Títulos Source Serif 4 600 a 54 px;
texto Source Sans 3 (etiquetas a 24-28 px); números y la configuración en IBM Plex Mono; fórmulas KaTeX a 34-40 px;
narración a 42 px, en una línea.
Retícula: a la izquierda, las tasas por 1000 personas-año en escala log (x 200-1000, y 290-790, edades 27-101,
0.4-120 por 1000); a la derecha (x 1110-1824), primero la configuración, θ y W; después, la cadena de dl_edo() y la
prevalencia (x 1180-1800, y 430-790, mismas edades que las tasas, para que los tramos se lean en vertical).

MOVIMIENTO: los textos entran en 0.4 s con EASE.entrarSuave y salen en 0.24 s. Las guías de los nudos bajan
escalonadas 120 ms (0.5 s cada una, EASE.entrar); los puntos de los nudos, escalonados 90 ms (EASE.pop sobre el radio,
nunca sobre la opacidad). Los tramos entre nudos se trazan lineales en la edad (2.4 s). El cursor de la edad va de
nudo en nudo de 30 a 99 (4.5 s, 0.64 s por tramo: la mitad se mueve con EASE.mover y la otra se queda en el nudo),
siempre sobre un punto a_j de la malla h/2 (redondeado a 0.1 años). El cambio de escala log -> lineal -> log, 1.4 s
cada uno con EASE.camara: cada punto se mueve entre su altura en una escala y en la otra; las marcas del eje y los
títulos se relevan (uno sale en la primera mitad, el otro entra en la segunda); f se apaga en el primer 15 % del
cambio y sus puntos van recortados al gráfico. Cuando entra W, la configuración y θ bajan al 40 %. El nudo sube y baja
en 1.2 s con EASE.mover; i y p se recalculan en cada cuadro con DL.modelo.edo(); mientras sube, la p queda al 40 %
(su momento es la frase siguiente); el anillo y el valor del nudo entran y salen con visible(). Final: lo que no es θ
ni las curvas baja al 40 % en 0.4 s; los 14 puntos de θ engordan 5 px, escalonados 60 ms de izquierda a derecha (pop
de 0.4 s), y «θ: estos 14 puntos, en escala log» entra arriba a la izquierda del gráfico.

TOMAS (segundos):
0.0       cabecera + ejes de las tasas (escala log), con las edades de los nudos en el eje x (cuadro 0).
0.4-5.2   narración 1. Las guías de los 7 nudos bajan (0.7-1.6); la configuración `nudos: [30, …, 95]` y «por
          defecto, cada 10 años desde edad_inicio hasta 80, y 95» a la derecha (2.0).
5.5-9.8   los puntos de i (5.8) y de f (6.8) en los nudos; la leyenda, abajo a la derecha del gráfico, entra con
          cada serie (su fondo tapa las guías y el cursor).
10.1-14.4 θ = (log i_1, …, log i_K, log f_1, …, log f_K) a la derecha, 38 px (el movimiento principal). La narración
          dice K = 7 y 14 números (θ por defecto; con la EMR fija en cero, θ = log i en los nudos).
14.7-19.0 los tramos de log i y log f se trazan de 30 a 95 (15.0-17.4), rectos en escala log.
19.3-24.2 de 95 a 99, el tramo constante (19.6-20.2) y dos corchetes de la marca (i y f) con «constante» (20.3). Aquí
          el primer nudo es la edad de inicio: lo constante antes del primero no se ve (lo dice la fórmula de W).
24.5-29.2 log i(a_j) = Σ_k W_jk log i_k; W_jk = 1 − w, W_j,k+1 = w; w = (a_j − nudo_k)/(nudo_k+1 − nudo_k); «para a_j
          entre nudo_k y nudo_k+1, con a_j llevada primero al rango de los nudos». Una línea por vez, 0.7 s entre ellas;
          la configuración y θ bajan al 40 %.
29.5-35.4 el cursor de la marca baja en a_j = 63 (29.8); puntos en i y f (30.1); la fila de W en 63, siete celdas
          con su nudo debajo: 0, 0, 0, 0.70, 0.30, 0, 0 (30.5), «Σ = 1.00» (31.0); i(63) = 2.88 por 1000 (31.4).
35.7-41.6 el cursor vuelve a 30 (36.0-36.6, EASE.mover) y va de nudo en nudo hasta 99 (36.8-41.3); la fila cambia con
          él y la suma se queda en 1. En cada nudo se detiene: un 1 bajo ese nudo; desde 95, un 1 en el último.
42.0-46.4 lo de W se va (42.0); i pasa a escala lineal, 0 a 8 por 1000 (42.4-43.8); f se apaga y la leyenda dice
          «f (10 a 90 por 1000) no cabe en esta escala». Los tramos se curvan: crecimiento exponencial.
46.7-50.6 de vuelta a escala log (47.0-48.4); f vuelve.
51.0-56.6 a la derecha, θ →(W, exp) i(a), f(a) →(RK4) p(a) (51.3) y «dl_edo(): RK4 con paso h = 0.2 (escena 03),
          r = 0 y p(30) = 0» (51.7); los ejes de p (52.2) y la curva p se traza de 30 a 99 (52.8-55.2, lineal).
57.0-62.8 el nudo de i en 60 sube a 3x, de 2.5 a 7.5 por 1000 (57.4-58.6, el movimiento principal), con su valor
          pegado (seguir) y un anillo de la marca; la i original queda en trazos al 45 %; un tramo de la marca sobre
          50-70 en las tasas (59.0) con «solo entre 50 y 70». La p, al 40 %.
63.1-68.6 la p vuelve al 100 %; en la prevalencia, el tramo de 50 a 99 (63.4) con «desde 50 en adelante» y el área
          entre la p original y la nueva (63.6). La narración: p guarda los casos nuevos, que f va quitando (la
          diferencia llega a 3.1 puntos en 70 y baja a 0.6 en 99).
69.0-78.0 el nudo baja (69.4-70.6) y todo vuelve; los tramos se van; lo que no es θ ni las curvas baja al 40 %
          (71.0); los 14 puntos de θ engordan y «θ: estos 14 puntos, en escala log» (71.5). Se sostiene hasta 78.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo». Una tasa de cambio sobre un eje de nivel.
Decir que i es lineal (lo lineal es log i, por tramos). Una etiqueta de serie pegada a una curva que se mueve con el
cambio de escala. Un número de W que no sea la fila del paquete. Puntos que salen del gráfico en el cambio de escala.
Decir que θ solo fija p (p también depende de r, p(a0), la edad de inicio y el paso).

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos y auditoría (auditar.mjs). La auditoría
mide lo dibujado contra algo independiente: los puntos de los nudos y las curvas dibujadas están donde los pone la
escala log del eje (recalculada en la auditoría, no tomada de G.y) para los valores de dl_edo() exportados de R; los
tramos dibujados son rectos en píxeles entre nudos y planos de 95 a 99; la fila de W dibujada en 63 y en cada
posición del cursor es la fila de .dl_base_interp() del paquete, suma 1 y w = (63 − 60)/(70 − 60); i(63) mostrada es
la i de dl_edo() en 63; en escala lineal, las alturas son las de la escala lineal; con el nudo arriba, la p dibujada
(DL.modelo.edo() en el cuadro) es la de dl_edo() con log i_60 + log 3 a < 1e-12, la i dibujada cambia solo en
(50, 70) y la p dibujada no cambia hasta 50 y cambia desde 51 hasta 99; las bandas están en 50-70 y 50-99 de su
eje; los textos que se leen (2.88, 7.5, las celdas y Σ) son los números de R con su formato; con el cursor en el
nudo 40 la fila es un 1 bajo el 40 y desde 95 un 1 en el último; a mitad del cambio de escala f está apagada y los
puntos de i quedan dentro del gráfico; al final, la p dibujada es otra vez la de dl_edo().

REVISIÓN ADVERSARIAL (metodólogo y diseñador, en paralelo): corrigió los puntos de f que volaban sobre el título en
el cambio de escala, la condición «para a_j entre nudo_k y nudo_k+1» que faltaba en W, los supuestos de p que no se
decían (r = 0, p(30) = 0, paso h = 0.2), «RK4 en la malla h/2», el exp que faltaba en la flecha de W, «acumula lo que
entró antes» (f va quitando), «crece» por «crece o decrece», «se fijan» por «se describen», f apagada sin decir por
qué, el barrido que nunca se detenía en un nudo, el cursor sobre la leyenda, la columna derecha sin jerarquía, dos
movimientos principales a la vez al subir el nudo, el anillo y el valor que aparecían de golpe, un final que no
nombraba θ, K = 7 sin decir, números chicos para un teléfono, etiquetas sobre las guías y cinco comprobaciones que
no podían fallar o que medían menos de lo que decían.
```
