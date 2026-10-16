# 05 · El ancla — notas

## Decisiones tomadas sin consulta

- **La lupa en el panel, no etiquetas sobre el gráfico.** Las etiquetas de lower_b, val_b y upper_b junto a la banda
  65-69 tapaban las bandas vecinas. Por eso la banda se amplía en una regla a la derecha, unida a su caja por el borde
  de la marca y un cuadrito del mismo color delante del título.
- **El paso a escala log es el movimiento principal de la narración 2**, declarado como cámara en [10.0, 11.0]. En
  log, las 12 cajas del ancla miden lo mismo porque σ_log = 0.15 en todas las bandas del ejemplo sintético. La
  auditoría lo comprueba («log»).
- **z_b en la lupa usa la media posterior de q_b.** z_b depende de θ y la log-posterior la evalúa en cada θ; la cifra
  −0.041 es log(media de q_b) − log val_b y la etiqueta de q_b dice «media posterior».
- **La sensibilidad son seis ajustes cortos.** λ ∈ {1, 0.5} × ρ ∈ {0, 0.5, 0.9}, hechos como `dl_sensibilidad()`:
  `.dl_bundle_con` y `.DL_OPCIONES_SENSIBILIDAD` (2 × 6000 iteraciones, 3000 de calentamiento, 200 simulaciones,
  semilla 1). La medida es el ancho relativo medio del IC 95 %, (superior − inferior) / valor, como en
  `diagnostico-y-sensibilidad.qmd`. Con una sola semilla y 200 simulaciones, los cuantiles 2.5/97.5 traen ruido de
  Monte Carlo. Aun así, la diferencia entre ρ = 0, 0.5 y 0.9 (0.32, 0.45, 0.60) y la razón ≈ √2 con λ = 0.5
  (0.45 → 0.64) son claras.
- **En el ejemplo no hay datos locales en el ajuste** (`datos_en_ajuste` vacío). Que las medias caigan dentro del
  intervalo del ancla se espera por construcción. La narración dice «la media de cada banda», no «el intervalo»: el
  IC de 80-84 sobresale un poco del intervalo del ancla.
- **Duración: 75 s**, el borde del objetivo de 45-75 s, porque la escena tiene cuatro ideas: σ_log, z_b, R con ρ, y λ.

## (a) Revisión adversarial

**Metodólogo** (15 hallazgos; corregidos los válidos):

- **La leyenda tapaba datos.** Su fondo borraba el borde superior de las cajas de 70-74 y 75-79. Ahora son dos
  rectángulos más angostos que no llegan a las cajas (también lo señaló el diseñador).
- **«la prevalencia de cada banda queda dentro de los intervalos del ancla»** era engañoso: el IC de 80-84 y el de
  40-44 se salen apenas. Ahora dice «la media de cada banda cae dentro del intervalo del ancla». La auditoría
  comprueba las medias.
- **«Se baja cuando los datos locales ya estaban…»** omitía «que entran al ajuste». Ahora: «Se baja si los datos del
  ajuste ya estaban en la referencia: no cuentan dos veces».
- **«el promedio ponderado de la escena 04»:** la escena 04 aún no existe y no decía los pesos. Ahora: «el promedio
  de $p$ en la banda, ponderado por la población».
- **«aunque 95+ abarque 30 años»:** el modelo representa la banda abierta con 95-99. Ahora: «aunque 95+ sea abierta».
- **El intervalo «es» el de una normal en log:** es una lectura. Ahora: «se lee como el de una normal», como la guía.
- **z_b con la media posterior sin decirlo:** la etiqueta de q_b dice ahora «media posterior».
- **Unidades mezcladas:** los ejes estaban en % y la lupa en proporciones. Ahora todo va en proporciones.
- **Sin etiqueta $z_b$ junto al tramo, y la guía de q_b cruzaba la campana:** se añadió la etiqueta y la guía arranca
  en la campana.
- **El título de las barras no decía «relativo»:** corregido.
- **El pie no decía las simulaciones guardadas.** El ajuste del ejemplo pide 1000 y guarda 800 (adelgazamiento 10).
  Ahora el pie dice «4 × 4000 iteraciones (2000 de calentamiento), 800 simulaciones · sensibilidad: 2 × 6000 (3000),
  200 simulaciones», con el 800 tomado del ajuste. El comentario de `datos/exportar.R` («1000 simulaciones») queda
  inexacto; no lo toqué porque está fuera de esta escena (ver b).
- **El guion decía «1 % a 25 %» para la escala log:** ahora dice 0.007 a 0.25.

No apliqué dos hallazgos:

- Cambiar «una estimación publicada» por «la estimación de referencia (como GBD)», porque el encargo pide
  «publicada» y el título ya dice «la estimación de referencia».
- Aclarar que la grilla de λ difiere de la de la configuración (0.1/0.25, 0.5, 1). La escena no dice que use la
  grilla del proyecto: la nota dice «cadenas de dl_sensibilidad()».

**Diseñador de movimiento** (10 notas, todas aplicadas):

1. El fondo de la leyenda, más angosto (cuadro 1890).
2. El panel de barras se arma vacío desde 61.0 s, así en 64.0 s solo se mueven las cajas y su barra. Esto quitó el
   aviso de «simultáneos» del cuadro 1921.
3. Durante la cámara (cuadros 305-315), las marcas lineales se van antes de u = 1/3 y las logarítmicas llegan después
   de u = 1/2. Ya no se ve «0 %40».
4. El IC del ajuste lleva topes verdes de 2 px: el ancho se lee en un teléfono.
5. 77 → 75 s; el final se sostiene 3.6 s.
6. Los valores de las barras pasan de 22 a 26 px y la nota de 22 a 24.
7. De 52.8 a 58.1 s, Σ, R y la matriz bajan al 40 % y Σ/λ queda como único foco.
8. 90-94 y 95+ se marcan y desmarcan con transición (no en un cuadro), y la matriz y la nota se desmarcan a la vez
   que el gráfico, a 47.0 s.
9. Un cuadrito de la marca une el título de la lupa con la caja marcada, y el panel se alinea en x = 1110.
10. Las 12 rayas val_b están desde el cuadro 0 y las cajas se abren a su alrededor.

Al aplicar los cambios, tres narraciones quedaron cortas para su número de palabras. Se acortaron sin perder el
contenido: la de q_b, la de bandas y años, y la del doble conteo.

## (b) Cambios que sugiero a la biblioteca común

- **`D.ejes` con una mezcla lineal ↔ log** (`op.yMezcla(u)`), con fundido escalonado de las marcas, para un cambio de
  escala como cámara. Lo implementé aquí con `gy(v, u)` y dos juegos de marcas.
- **El `0` en una escala log:** `D.ejes` produce NaN si una marca lineal llega a log (log 0). Conviene que lo
  descarte.
- **`D.matriz(valores, op)`**: una matriz de celdas con opacidad, rótulos de fila y entrada por diagonales (o por
  función de la celda). Sirve también para la escena 08 (correlaciones de las cadenas).
- **`D.leyenda(items, op)`** con fondo automático del ancho del texto. Taparle datos a mano fue el primer hallazgo de
  las dos revisiones.
- **La auditoría podría avisar cuando un relleno opaco de un texto tapa una marca de datos**, no solo cuando dos textos
  chocan.
- **El comentario de `datos/exportar.R`** dice «1000 simulaciones», pero `ctx$ajuste()` guarda 800 (4 cadenas ×
  2000 / 10). Habría que corregirlo o subir las iteraciones.
- **`ctx$sensibilidad(lambda, rho)`** en `exportar.R`, memoizado, para que las escenas que reajustan con otros λ o ρ
  no llamen a funciones internas (`.dl_bundle_con`).

## (c) Auditoría final

```
Auditoría de «05-ancla» (2250 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (9/9)
Todo en orden.
```

Las 9 comprobaciones de la escena:

- σ_log recalculado desde lower y upper es el sigma_log del paquete (1e-12) en las 12 bandas, y la lupa muestra el de
  65-69.
- Las cajas miden lo mismo en log.
- z_b de la lupa.
- Las rayas q_b son las del ajuste.
- L_ancla con la matriz densa es igual a `.dl_lp_mvn_ar1()` (1e-9).
- Cada celda dibujada de R es ρ^|b−c| (1e-12), y es la U'U de `.dl_chol_ar1()`.
- El IC del ajuste es el del ejemplo, con las medias dentro del ancla.
- ρ = 0.9 en pantalla y los anchos crecen con ρ.
- λ = 0.5 en pantalla y su IC es más ancho que el de λ = 1.

## (d) Tiempos

- Render completo (2250 cuadros, MP4 y hoja): 2 min 36 s.
- Datos (`Rscript datos/exportar.R 05-ancla`, con el ajuste de producción y los seis ajustes cortos): 2 min 4 s.
- Auditoría: unos 40 s.
