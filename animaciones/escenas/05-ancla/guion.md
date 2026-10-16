# 05 · El ancla — guion

El guion dirigido de la escena, con la plantilla del laboratorio (OBJETIVO, SALIDA, TIEMPO, ASPECTO, MOVIMIENTO, TOMAS,
PROHIBIDO, REVISIÓN). Cada adjetivo es un número. Si cambias la escena, cambia primero el guion.

**Fuente del método:** `vignettes/el-modelo.qmd`, sección 5, «El ancla»; `R/verosimilitud.R` (`.dl_ancla_sexo`,
`.dl_chol_ar1`, `.dl_lp_mvn_ar1`, `.dl_lp_ancla`); `R/insumos.R` (`sigma_log`); `R/sensibilidad.R` y
`vignettes/articles/diagnostico-y-sensibilidad.qmd` (cómo se reajusta con otros λ y ρ).
**Datos:** `datos/r/05-ancla.R`: las 12 bandas de prevalencia del ancla de los hombres (`dl_insumos()`, causa 9100,
sintética), la matriz R que arma el paquete con ρ = 0.5, la prevalencia por banda del ajuste del ejemplo
(`ctx$ajuste()`: 4 cadenas × 4000 iteraciones, 2000 de calentamiento) y seis ajustes cortos con λ ∈ {1, 0.5} y
ρ ∈ {0, 0.5, 0.9}, hechos como `dl_sensibilidad()` (`.dl_bundle_con`, 2 cadenas × 6000 iteraciones, 3000 de
calentamiento, 200 simulaciones).

```text
OBJETIVO: quien mira entiende que el ancla es una estimación publicada de la prevalencia por banda, que su
intervalo del 95 % se lee como una log-normal (σ_log,b = (log upper_b − log lower_b) / 3.92), que el modelo se
compara con ella en logaritmos (z_b = log q_b − log val_b) con todas las bandas a la vez en una normal multivariada
cuya correlación R_bc = ρ^|b−c| se cuenta en bandas, y que λ escala su información. Sale sabiendo qué hacen ρ y λ
con el intervalo del ajuste.

SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11 (jsDelivr), 1920 x 1080, con lib/contrato.js.

TIEMPO: 75 s a 30 cuadros/s = 2250 cuadros. Sin bucle: el cuadro 0 es una composición terminada (cabecera, los
ejes de la prevalencia por edad con sus títulos y las 12 rayas val_b del ancla); el final se sostiene 3.6 s.

ASPECTO: fondo F5F7F6, tinta 16211E, tinta suave 55645F, rejilla DCE4E1, marca 0E6B62. El ancla va en tinta: una
caja por banda de lower_b a upper_b (relleno FFFFFF, borde 16211E de 2 px, 4 px de margen a cada lado de la banda)
con una raya de 3 px en val_b. Lo del modelo, q_b, en verde p 1baf7a: raya de 4 px en la media, con 44 % del ancho
de la banda; su IC 95 % como caja verde al 30 % de opacidad con topes verdes de 2 px. La matriz R en la marca 0E6B62 con opacidad = R_bc
(celdas de 30 px, 2 px de separación). Ninguna letra en color de serie. Títulos Source Serif 4 600 a 54 px (la
cabecera); texto Source Sans 3; números IBM Plex Mono; fórmulas KaTeX a 34 px (la principal) y 30 px (las de
apoyo); etiquetas a 24-26 px; nada por debajo de 22 px. Narración abajo a 36 px.
Disposición: gráfico a la izquierda (área de datos x 200-1000, y 300-800), panel a la derecha (x 1100-1824). El
panel cambia de contenido tres veces (lupa de una banda, fórmula y matriz, anchos de intervalo); entre contenidos,
lo que sale se va en 0.24 s antes de que entre lo nuevo.

MOVIMIENTO: cajas del ancla que crecen desde val_b hacia lower_b y upper_b en 0.4 s (EASE.entrarSuave), escalonadas
60 ms (ESCALONADO.item). Textos que entran en 0.3-0.4 s (EASE.entrarSuave) y salen en 0.24 s (EASE.salir). El paso
de escala lineal a logarítmica en y es un movimiento de cámara declarado de 1.0 s (EASE.camara) entre la narración
1 y la 2. La campana log-normal se traza en 0.8 s (EASE.mover). Las rayas q_b entran escalonadas 60 ms. Las
diagonales de R entran de a una (0.4 s cada una, 1.2 s entre ellas). Las cajas del IC del ajuste crecen desde su
media en 0.75 s (MS.heroe, EASE.entrarSuave); el paso de un ajuste a otro es un movimiento de 0.75 s (EASE.mover)
de los bordes de las cajas. Las barras de ancho crecen en 0.5 s (EASE.entrarSuave). Nada se mueve mientras se lee.

TOMAS (segundos):
0.0        cabecera + ejes (y lineal, proporción de 0 a 0.22) + las 12 rayas val_b, quietos (cuadro 0).
0.4-5.2    narración 0. Las 12 cajas del ancla se abren desde su val_b, escalonadas, desde 0.8 s.
5.5-10.0   narración 1. La banda 65-69 se marca (borde de la marca, 3 px); las demás bajan al 35 %. A la derecha,
           la lupa: una regla vertical con lower_b, val_b y upper_b y sus valores.
10.0-11.0  CÁMARA: el eje y pasa a escala logarítmica (0.007 a 0.25); la regla de la lupa también. Las marcas
           lineales se van antes de u = 1/3 y las logarítmicas llegan después de u = 1/2. En log, las 12
           cajas miden lo mismo (σ_log = 0.15 en las 12 bandas del ejemplo).
11.1-16.8  narración 2. La campana normal (en log) se traza junto a la regla, con ±1.96 σ_log; debajo,
           σ_log,b = (log upper_b − log lower_b) / 3.92 y su cuenta con los números de la banda.
17.2-22.4  narración 3. Las rayas verdes q_b (media posterior del ajuste del ejemplo) entran en las 12 bandas; en
           la lupa, q_b sobre la regla. Leyenda del gráfico arriba a la izquierda.
22.7-26.4  narración 4. En la lupa, el tramo z_b entre val_b y q_b; la fórmula σ se va y entra
           z_b = log q_b − log val_b con su cuenta.
26.4-26.7  la lupa se va (0.24 s); el gráfico baja al 55 %.
26.7-30.4  narración 5. L_ancla = log MVN(z; 0, Σ/λ) y Σ = diag(σ_log) R diag(σ_log) a la derecha.
30.7-35.6  narración 6. R_bc = ρ^|b−c|, ρ = 0.5; la matriz 12 x 12 vacía (bordes de rejilla) y su diagonal (1).
35.9-41.4  narración 7. Entran las diagonales a 1, 2 y 3 bandas (0.5, 0.25, 0.125) con su leyenda, y después el
           resto de una vez.
41.7-46.8  narración 8. Las bandas 90-94 y 95+ se marcan en el gráfico y su celda en la matriz: a una banda,
           correlación 0.5, aunque 95+ sea abierta (se desmarcan a 47.0 s).
47.1-52.0  narración 9. Σ/λ se subraya con la marca; al lado, λ = 1 peso entero, λ = 0.5 la mitad.
52.5-58.1  narración 10. El doble conteo; Σ, R y la matriz bajan al 40 % desde 52.8 s: solo L_ancla y la nota de λ
           quedan enteras.
58.1-58.4  la fórmula y la matriz se van; el gráfico vuelve al 100 %.
58.4-63.3  narración 11. Las cajas verdes del IC 95 % del ajuste (λ = 1, ρ = 0.5) crecen sobre las del ancla;
           las rayas (medias) caen dentro de las cajas del ancla;
           rótulo de estado arriba a la derecha del gráfico. Desde 61.0 s se arma el panel de barras, vacío
           (título, nota, filas ρ, leyenda), para que en 64.0 s solo se muevan las cajas y su barra.
63.6-69.1  narración 12. Las cajas pasan a los ajustes cortos λ = 1 con ρ = 0, 0.5 y 0.9 (64.0, 65.8, 67.6 s); a
           la derecha, una barra de ancho medio del IC por ρ (λ = 1) entra con cada paso.
69.4-73.8  narración 13. Las cajas pasan a λ = 0.5, ρ = 0.5 (69.8 s); entran las barras de λ = 0.5 (la de ρ = 0.5
           primero, 70.0 s; las otras dos 0.6 s después). Se sostiene hasta 75.0.

PROHIBIDO: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, números
que no salgan del paquete, una notación distinta de la guía «El modelo», el ancla o los datos en color de serie.

REVISIÓN: fotogramas en cada marca (render.mjs --marcas), hoja de contactos, auditoría (auditar.mjs): σ_log
recalculado desde lower y upper es el sigma_log del paquete (1e-12) en las 12 bandas; cada celda de la matriz
dibujada es ρ^|b−c| (1e-12) y es la R = U'U de .dl_chol_ar1(); el σ_log y la z_b de la lupa son las de la banda
65-69; L_ancla de la fórmula densa coincide con .dl_lp_mvn_ar1 (1e-9); las cajas verdes son las del ajuste del
estado que dice el rótulo; las barras son el ancho relativo medio exportado, y con λ = 1 crecen con ρ.
```
