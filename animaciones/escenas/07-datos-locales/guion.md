# 07 · Los datos locales — guion

El guion dirigido con que se hizo la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO,
MOVIMIENTO, TOMAS, PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 5 («Los datos locales»); `vignettes/articles/datos-locales.qmd`
(`datos_en_ajuste`, qué filas entran, `excluir`, el doble conteo y `ancla.peso`); `R/verosimilitud.R`
(`.dl_datos_verosimilitud`, `.dl_lp_lognormal`, `.dl_lp_binomial`, `.dl_lp_poisson`, `.dl_lp_datos`).
**Datos:** `datos/r/07-datos-locales.R`: la tabla `datos.csv` del ejemplo (causa 9100, sintética); dos ajustes
nacionales con `dl_ajustar()`: `ctx$ajuste()` (sin datos, `datos_en_ajuste` vacío, λ = 1; 4 cadenas de 4000
iteraciones) y una copia del ejemplo con `datos_en_ajuste: [prevalencia, incidencia, mortalidad]` y `ancla.peso: 0.5`
(4 cadenas de 8000 iteraciones, semilla 1). q_d y ℓ_d de los hombres en la media posterior de θ del ajuste con datos,
con las funciones del paquete; su suma coincide con `.dl_lp_componentes()` a 1e-9.

```text
OBJETIVO: quien mira entiende qué filas de la tabla datos entran al ajuste nacional (las nacionales del año, de las
medidas de datos_en_ajuste, sin excluir; por defecto ninguna: solo validan; las covariables nunca), contra qué
cantidad del modelo se compara cada dato (q_d: p, i(1-p) o p f promediada en su intervalo de edad), cómo entra
(log-normal con valor y error; binomial o Poisson con casos y muestra; L_datos = Σ ℓ_d), por qué se baja λ (doble
conteo) y qué cambia: la curva se acerca a los datos y su intervalo se estrecha.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 84.5 s a 30 cuadros/s = 2535 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera, la
configuración con datos_en_ajuste vacío y la rejilla de las 25 filas nacionales) y el final se sostiene 2.6 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, guía B4C2BD, verde azulado de la marca
0E6B62 para lo que se señala (la clave que cambia, λ). Series: p 1baf7a (la prevalencia y su q_d), i 2a78d6 (i(1-p)),
f eb6834 (p f). Los datos y el ancla, en tinta: las filas son cajas de 84 x 44 px (contorno suave 2 px fuera del
ajuste, relleno de tinta dentro, tachadas si se excluyen); los datos son círculos huecos de radio 8 (borde de tinta
3 px) con un segmento horizontal que cubre su intervalo de edad y, si traen error, un bigote vertical con el 95 % de
su log-normal; el ancla son cajas de relleno rejilla con borde guía (su intervalo del 95 % en cada banda). q_d es un
segmento de 6 px del color de su cantidad sobre el intervalo del dato. Ninguna letra en color de serie. Títulos
Source Serif 4 600 a 54 px; texto Source Sans 3 (rótulos de panel 28 px, notas 22-26 px); números y YAML en IBM Plex
Mono 26 px. Narración abajo a 36 px.

MOVIMIENTO: textos que entran en 0.3-0.4 s con EASE.entrarSuave y salen en 0.24 s (EASE.salir); las cajas de las
filas se llenan escalonadas 60 ms (ESCALONADO.item), 0.3 s cada una; las curvas se trazan lineales en la edad
(1.2-1.6 s); el recuadro de la marca se estira con EASE.mover en 0.5 s; el héroe de la toma final, la banda de la
prevalencia que pasa de sin datos a con datos, con EASE.mover en 1.4 s. Cada toma sale entera en 0.24 s y la
siguiente entra 0.2-0.3 s después; no hay movimientos de cámara (las tomas se reemplazan, no se corren).

TOMAS (segundos):
0.0       cabecera; tarjeta YAML (anio: 2023, datos_en_ajuste: []); rejilla de las 25 filas nacionales de 2023 por
          medida y sexo, en contorno; la caja de las 160 filas subnacionales de 2019, punteada (cuadro 0, sin fundido).
0.4-5.3   narración 1. Nada se mueve: se lee la rejilla.
5.6-11.6  recuadro de la marca alrededor de «datos_en_ajuste: []» (0.4 s). Los datos solo validan.
11.9-17.1 «[]» sale; entra la lista (prevalencia, incidencia, mortalidad) escalonada 120 ms; el recuadro se estira;
          las 24 cajas que entran se llenan de tinta, escalonadas 60 ms; la del dato excluido se tacha y su nota entra.
17.4-21.1 la ficha «covariables: solo en la estimación subnacional», punteada, bajo la tarjeta. Sale la toma.
21.4-26.2 tres paneles (hombres, edades 50-99), el primero: prevalencia. Ejes, la curva p (1.6 s), los tres
          estudios (x/n) y sus q_d en verde.
26.5-31.1 el panel i(1-p) (azul, 26.8 s) con las dos cohortes de incidencia y el panel p f (naranja, 28.0 s) con
          las siete filas del registro vital; curvas de 1.0 s. Sale la toma.
31.4-35.3 tarjeta izquierda: «valor y error estándar → log-normal», ℓ_d y s_log,d (KaTeX 32 y 30 px), la nota de η.
35.6-39.8 su gráfico: la densidad N(log x; log val_d, s_log,d) de la incidencia de 70 a 79 años, val_d y q_d;
          los números (val_d, se_d, s_log,d; q_d, ℓ_d) y, al final, «altura en q_d: e^ℓ_d».
40.1-45.4 tarjeta derecha: «casos y muestra → conteos»: la binomial, y 0.9 s después la Poisson (el ejemplo no
          trae conteos de incidencia ni de mortalidad, y la tarjeta lo dice).
45.7-50.6 su gráfico: ℓ(q) del estudio de 80 años y más (70 de 800), su máximo en x/n y q_d; los números.
50.9-55.7 la franja de la suma: L_datos = Σ_d ℓ_d, los 12 datos de los hombres en la media posterior de θ; la
          narración nombra las dos constantes que se omiten (log C(n,x) y -log x!). Sale la toma.
56.0-65.9 doble conteo: log posterior(θ) = L_suavidad + L_emr + L_ancla + L_datos (L_ancla subrayada); la tarjeta
          YAML «ancla: peso: 1» pasa a «peso: 0.5» (60.6 s, recuadro de la marca); L_ancla = log MVN(z; 0, Σ/λ); las
          dos lecturas de λ, una por vez (la de λ = 1 baja al 40 % cuando entra la de 0.5). Sale la toma.
66.2-70.9 la prevalencia de los hombres por edad: ejes, cajas del ancla, la media sin datos (trazo de 1.6 s, punteada)
          y su banda del 95 %; entran los tres estudios y el q_d del estudio de 80 y más.
71.2-76.6 héroe: la banda, la media (cruce de punteada a sólida) y el q_d de 80+ pasan de sin datos a con datos
          (1.4 s, EASE.mover); 0.3 s antes entran, punteados, la media, el intervalo y el q_d sin datos; la tabla de
          lectura entra con la fila de q_d (escalonada 60 ms). La narración dice «aun con λ = 0.5»: λ = 0.5 ensancharía.
76.9-84.5 la guía en 80 años; los dos intervalos («sin», «con») a la izquierda de la guía, sobre un recuadro; la
          tabla se estira (0.5 s, EASE.mover) con la fila del ancho en puntos porcentuales; se sostiene.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo», datos o ancla en color de serie, letras en
color de serie.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs): las cajas
llenas son las 24 filas que entran al ajuste del paquete (1 excluida, 160 subnacionales fuera); ℓ_d de la incidencia
de 70-79 (log-normal) y del estudio de 80+ (binomial), recalculados en JS con la fórmula de la guía desde val_d, se_d,
η, x, n y q_d exportados, iguales al ℓ_d del paquete a 1e-9; s_log,d = se_d / (val_d + η); la suma de los ℓ_d igual
a L_datos de .dl_lp_componentes() a 1e-9; los anchos del intervalo en 80 años dibujados son los de dl_estimaciones()
y el de con datos es menor; el q_d de cada estudio de prevalencia (promedio por población de p en su intervalo,
simulación por simulación) queda más cerca de su x/n con datos que sin datos; la caja vacía en «vacio».

REVISIÓN ADVERSARIAL (metodólogo y diseñador, en paralelo; ver notas.md): corrigió «los datos solo validan» (las
filas nacionales quedan fuera; validan las subnacionales), «se omiten las constantes» (solo log C(n,x) y -log x!),
«esa densidad» (es la normal de log x), la falta de aviso de que q_d y ℓ_d están en la media posterior de θ del ajuste
con datos, «con λ = 0.5 se estrecha» (es por los datos, aun con λ = 0.5), las unidades por persona-año, los valores
por defecto en el YAML; y en el movimiento, las barras de 80 años que se rompían, la media sin datos que
desaparecía, el salto de punteada a sólida a mitad del héroe, las dos lecturas de λ a la vez, la franja de la suma
fuera de la escena, la leyenda que mostraba lo que aún no había, la caja excluida delatada desde el cuadro 0 y las
escalas desparejas de los paneles.
```
