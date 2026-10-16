# 04 · De las edades simples a las bandas — notas

## Decisiones

- **La curva:** media posterior por edad simple de `f$draws_q` del ajuste nacional del ejemplo (`ctx$ajuste()`,
  hombres, ubicación 123). Para p f se promedia p·f simulación por simulación, no la media de p por la media de f.
  Como los pesos son fijos y el promedio es lineal, q calculado sobre las medias es igual a la media de los q por
  simulación. La auditoría lo comprueba contra `.dl_agregar_bandas()`.
- **La población:** la tabla nacional de `dl_insumos()` (`location_level` 0, 2023, hombres), que el paquete calcula
  como suma de los 25 departamentos. Es sintética como todo el ejemplo, y el pie lo dice.
- **El tercer caso de la guía.** Con la población del ejemplo no ocurre: sus 14 bandas finas tienen n_b = 5 en la
  malla 30-99, también 95+, que va de 95 a 99. Para mostrarlo, la escena junta 80-84 … 95+ en una banda abierta 80+:
  las mismas personas en otra partición, con 20 edades del modelo. Es el ejemplo de la guía (un dato de 60 y más
  sobre quinquenios más 80+). La narración lo plantea como condicional («si la tabla trae…»), y la cabecera de la
  tabla final y el pie dicen que esos q usan esa población. Los pesos y el q de ese caso los calcula el paquete con
  esa tabla (`.dl_pesos_intervalo()`, y `.dl_agregar_bandas()` con un `b` modificado).
- **Los pesos sin dividir por n_b** (la regla anterior a la versión 2.2.0, según la cabecera de `R/bandas.R`) se
  exportan desde R para el escalón punteado. La auditoría comprueba la suma de 80+: 20·N80/(5·ΣN_q + 20·N80).
- **La banda abierta** se dibuja en [95, 100): el eje termina en 100, y la rotulan «95 y más (abierta): edades 95
  a 99».
- **La cabecera** nombra `dl_insumos()`, como pide el encargo, aunque el promedio corre en la verosimilitud
  (`dl_ajustar()`) y en las tablas de salida. `dl_insumos()` arma `bandas_pobl` y la población.

## (a) Revisión adversarial

Hubo dos revisores independientes: un metodólogo, que trató de refutar cada fórmula, número y frase, y un diseñador
de movimiento, que dio 10 notas. Abajo, lo que encontraron y cómo se corrigió.

**Metodólogo**

1. El rótulo del fantasma decía «$N_{80+}$ en cada edad, ×4», pero el alto dibujado es N80/5 en un eje de personas
   por año de edad. → «sin dividir por $n_b$: ×4 (20 edades frente a 5)».
2. En la tarjeta del caso 3, el fantasma iba solo sobre las barras de 80+ y en otra normalización: era 2.44 veces
   la barra, mientras el texto decía ×4. → Ahora es un escalón punteado con los pesos sin dividir de las 40 edades,
   en la misma escala que las barras: los quinquenios quedan por debajo de sus barras y 80+ por encima.
3. La narración 10, «no por su población por su número de edades», podía leerse como «por año». → «multiplicada
   por», como en la guía.
4. Los q de «60 y más» de la tabla salen de la población con 80+ junta y no lo decían. Con la partición del
   ejemplo, q de p sería 0.0695 en lugar de 0.0723. → La cabecera dice «q en [60, 125), caso 3» y el pie, «Caso 3:
   80-84 … 95+ juntas en 80+».
5. El pie daba a entender que la población era real. → «Datos sintéticos del ejemplo (causa 9100, hombres, 2023):
   media posterior …, ancla y población».
6. La tarjeta del caso 2 seguía en pantalla durante la narración del caso 3. → Sale en 50.9 s. El diseñador hizo la
   misma nota.
7. «Para compararlos, el modelo promedia las edades» tenía mal el género y el objeto. → «Para compararlas, el modelo
   promedia $p$ sobre las edades de cada banda…».
8. A n_b le faltaba el matiz de la guía. → «$n_{b(a)} = 5$ edades del modelo en cada banda».
9. La banda abierta no se distinguía de las demás. → Rótulo «95 y más (abierta)».
10. La leyenda no decía qué es la caja del ancla. → «ancla: val e intervalo del 95 %, por banda [inicio, fin)».
11. Las fórmulas se veían en línea, no como en la guía. → `\displaystyle` en q, w_a y la q de la tarjeta.
12. La auditoría comprobaba menos de lo que decía. → Tres comprobaciones nuevas:
    - las cajas de población miden N_b/5, con n_b = 5 en toda la malla;
    - las cajas fundidas miden N80/20 y el fantasma, 4 veces eso;
    - la suma de 80+ sin dividir coincide con la fórmula cerrada.

    (La comprobación anterior, que los pesos sin dividir sumaran 1, era trivial.)
13. El guion y el código no coincidían en cuándo se atenúa el ancla. → Se atenúa en 39.0 s, en el tiempo de la
    narración 7, como dice el guion.
14. Faltaba un matiz en «como las publica GBD»: GBD publica por 100 000. → «con denominador en toda la población,
    como GBD (aquí por persona-año)».

El metodólogo confirmó como correctos:
- las tres fórmulas, verbatim de la guía;
- los pesos en JavaScript frente a `.dl_pesos_intervalo()`;
- 0.200, 0.077, 0.067, 0.056, 0.214 y 0.521;
- q = 0.0120, 0.0328 y 0.0723;
- que ipop es igual a i(1 − p);
- que n_b = 5 también en 95+;
- los puntos en la edad exacta a.

**Diseñador de movimiento.** Se aplicaron las 10 notas.

1. Tarjetas 200 ms después del velo (31.7, 44.3 y 58.3 s); la del caso 2 sale en 50.9 s.
2. La fusión de 80+ era el movimiento principal y se veía al 35 %. → Ahora va al 100 %.
3. En el caso 1, q y val del ancla caían en el mismo píxel, así que la comparación no se veía. → Lectura «ancla
   0.0120 · q 0.0120» con una guía a la caja del ancla.
4. Los bordes de A cruzaban el título del eje y los números de las marcas. → El velo se dibuja en dos tramos, uno
   por panel.
5. Demasiadas entradas en 31.5 s. → El ancla se atenúa en 39.0 s.
6. La «q» caía dentro de la caja siguiente del ancla. → Va arriba a la izquierda del segmento.
7. La leyenda quedaba sobre las líneas de la rejilla. → Rectángulo del color del fondo detrás (no es una sombra).
8. En un teléfono no se veían ni las rayas de las edades ni el fantasma. → Rayas de 18 × 3 px; el fantasma de la
   tarjeta es ahora un solo escalón de 2.5 px.
9. Los subíndices de w_a medían unos 19 px. → `\displaystyle` a 34 px. El título del eje inferior pasa a
   $N_b / n_b$.
10. Los puntos saltaban al pasar el borde de A, y había un punto suelto en el cuadro 0. → Rampa de un año en cada
    borde de A; los puntos empiezan en 0.6 s.

Las notas menores del diseñador (columna derecha vacía de 12.5 a 31.5 s; el 80+ y el 95 sobre la línea de 100 000)
quedan como están. El segmento q del caso 3 ahora se queda con la tabla hasta el final.

## (b) Sugerencias para la biblioteca compartida (no se tocó `lib/`)

- `dibujo.js`:
  - una función `velo(ejes, x0, x1, op)` para un intervalo [inicio, fin) con borde izquierdo continuo y derecho
    punteado (cerrado y abierto), en tramos por panel;
  - `escalon(xs, ys)` para un trazo en escalera;
  - una opción `fondo` en `parrafo()` para separar un rótulo de la rejilla, sin sombra.
- `ejes()`: aceptar `xTitulo: false` y geometría animable sin reconstruir el objeto (la cámara de esta escena
  reconstruye los ejes en cada cuadro con x, y, ancho y alto interpolados).
- `contrato.js`: exponer `c` a los ayudantes sin pasar por `DL.__c`. Esta escena lo usa para registrar con `c.dato`
  los altos de las cajas desde una función auxiliar.
- `auditar.mjs`: que la auditoría pueda comparar cadenas impresas (por ejemplo, que «0.214» sea `D.num(peso80, 3)`).
  Hoy solo compara números del estado.
- `modelo.js`: cuando llegue, `pesosIntervalo(edades, inicio, fin, bandas)` para que esta escena deje de definir
  `pesos()` por su cuenta.

## (c) Auditoría final

```
Auditoría de «04-bandas» (2160 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (7/7)
Todo en orden.
```

Comprobaciones de la escena:
- los 70 puntos;
- las cajas de población miden N_b/5, con n_b = 5;
- 80+ fundida mide N80/20 y el fantasma 4 veces eso;
- en los casos 1, 2 y 3, los pesos coinciden con `.dl_pesos_intervalo()` y q con `.dl_q_intervalos()` y con la media
  de `.dl_agregar_bandas()`, a 1e-12. En el caso 1, además con `.dl_q_bandas()`; en el caso 2, w ∝ N_b; en el
  caso 3, 80+ pesa N80/ΣN_b, y sin dividir, 20·N80/(5·ΣN_q + 20·N80);
- la tabla: q de i(1 − p) y de p f iguales a los del paquete.

## (d) Tiempo de render

El render completo tomó 152 s: 2160 cuadros en 124.9 s con 3 páginas, más ffmpeg y la hoja de contactos. La salida
es `salida/04-bandas/04-bandas.mp4`, de 2.0 MB. El ajuste del ejemplo, en `datos/exportar.R`, tarda unos 23 s.
Guarda 800 simulaciones, no 1000; `dl_opciones_mcmc()` lo avisa.
