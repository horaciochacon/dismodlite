# 08 · El muestreo — notas

## Datos y procedencia

`datos/r/08-mcmc.R` corre `dl_ajustar()` del ejemplo con `dl_opciones_mcmc()` por defecto (producción: 4 cadenas de
50 000 iteraciones, 10 000 de calentamiento, adelgazamiento 10, 1000 simulaciones), semilla 1, y exporta los hombres.
La cadena 1 se corre además con una copia instrumentada de `dl_mh()` (mismo orden rnorm → lp → runif) que anota
cada propuesta de las iteraciones 1-40, el recorrido del calentamiento, la escala y la aceptación de cada ventana y
L_b. El script exige con `stopifnot`:

- que la copia dé exactamente las simulaciones, la escala y la aceptación de `.dl_correr_cadena()`;
- que las 4 cadenas, juntas y elegidas como en `dl_ajustar()`, sean `ajuste$draws_par[["1"]]`;
- que su `dl_rhat()` / `dl_ess()` sean `ajuste$mcmc`.

`datos/08-mcmc.js` pesa 1.6 MB. El ejemplo no tiene datos locales en el ajuste: el pie lo dice.

## (a) Revisión adversarial y lo que se corrigió

**Metodólogo** (2 importantes y 7 menores; ningún bloqueo; todos los números coinciden con el paquete):

1. «Cada θ guardado es una simulación» era falso tal como se decía: `dl_ajustar()` toma 1000 equiespaciados de los
   16 000 guardados. Ahora la narración tiene dos frases: «Con cada θ guardado se resuelve la ecuación: sale una
   curva de p» y «Se toman 1000 de los 16 000 θ guardados, equiespaciados: las simulaciones». La leyenda dice «una
   simulación (1 de 1000)».
2. La forma L_b no se veía: la elipse medía unos 4 px con la cámara lejos. La cámara se adelantó a 45.5-46.1 s, antes
   de la iteración 5000. La etiqueta ahora dice «una desviación de la propuesta, proyectada».
3. «Cada iteración propone mover un bloque entero» pasó a «Cada iteración propone mover cada bloque, por turno, con
   z normal estándar».
4. «Se acepta siempre» pasó a «una propuesta así siempre se acepta».
5. La etiqueta de la forma pasó a «L_b L_b': la covarianza de θ_b en las iteraciones 1 a 5000». L_b es el factor de
   Cholesky.
6. Gráficos de la adaptación:
   - el título dice «ventana k de 100 iteraciones»;
   - la aceptación final nombra la cadena: «después del calentamiento, la cadena 1 acepta 0.35 (log i) y 0.33
     (log f)».
7. ESS: «… la misma precisión para la media».
8. Comprobaciones tautológicas:
   - `final` recalcula la media y los percentiles (tipo 7) a los 80 años desde las 1000 simulaciones;
   - `forma` rehace s_b con la recursión, no con `escalaEn`;
   - `compuertas` toma el máximo y el mínimo de la tabla `mcmc` de los dos sexos (28 filas, exportada para esto);
   - se comprueba la altura del último punto de aceptación contra la cuenta de aceptadas anotadas;
   - una comprobación nueva verifica que la primera curva sea `edoCtx(θ)`.
9. Cosas menores:
   - se borraron los fotogramas viejos;
   - el dominio lejano subió a -4.3, para que la propuesta 8 no se recorte;
   - la etiqueta «inicio» dice «inicio de la cadena 1», sin el θ_0 que no estaba definido, y se va al empezar la
     iteración 2.

**Diseñador** (10 notas, todas aplicadas):

1. La cámara antes de la forma (ver arriba). La etiqueta de la forma dura hasta el muestreo.
2. El primer acto tenía la mitad derecha vacía 20 s. Ahora el plano entra a 10.15 s, con «La cadena 1…», antes de
   las fórmulas (15.7 y 20.8 s).
3. Los cambios de acto pasaban por un cuadro vacío.
   - Las salidas se escalonan: tira 55.9, gráficos 56.1, plano 56.3.
   - Las trazas entran a 56.2 y se dibujan en 56.5-59.5.
   - El gráfico de p entra a 71.65.
4. Nada unía θ con p. Ahora la primera simulación se dibuja sola (72.3-73.1 s, 3 px) y luego entran las 1000
   (74-78 s).
5. El final no aterrizaba. A los 80 años: guía y marcas del intervalo en verde azulado, punto en la media y una guía
   hasta la lectura.
6. El borde de la media parecía un brillo: bajó a 6.5 px y la media entra 0.6 s después de la banda.
7. La etiqueta «inicio» chocaba con las propuestas (ver arriba).
8. Iteración 10: el punto viaja a 37.3 s, después de «se acepta» (36.6), y la propuesta aceptada mide 12 px con
   borde de 4 px.
9. Legibilidad en teléfono:
   - contador de 28 px en tinta, dentro del plano;
   - elipse de 3 px y recorrido de 2 px;
   - etiquetas a 24 px;
   - trazas a 1 px y 70 %, para que se distingan las 4 cadenas.
10. La entrada del calentamiento era todo a la vez:
    - las propuestas salen con las fórmulas;
    - el gráfico B y su título entran 0.6 s después del A;
    - la línea de 0.28 entra con su rótulo;
    - las etiquetas log i / log f quedan fijas al final de las escalas y entran al terminar el calentamiento (antes
      se deslizaban 10 s).

**Coordinación** (hallazgos recurrentes de otras escenas):

- el pie dice «sin datos locales en el ajuste»;
- intervalo «del 95 %», no «IC»;
- la estimación es la media de las simulaciones;
- R-hat y ESS se comparan con DL.modelo y con R;
- ninguna marca de aceptar/rechazar se enciende más de 2 veces por segundo.

## (b) Cambios que sugeriría a la biblioteca (implementados aquí, localmente)

- `D.ejes`: ticks y dominio animables. Hoy, para la cámara, el plano se rehace en cada cuadro con el dominio
  interpolado y un cambio de paso de los ticks a mitad de camino.
- `D.elipse(G, centro, cov)`: la elipse de una desviación de una normal 2-D en coordenadas del gráfico.
- Un trazo de muchos puntos como un solo `<path>` (`D.nube(G, xs, ys, op)`), para estampar miles de simulaciones sin
  miles de elementos.
- `DL.modelo.cuantil(x, p)`, tipo 7 como `stats::quantile()`. Lo usan la auditoría de esta escena y, seguramente, la
  de la 10.
- Formulario de lectura de una iteración de MH (Δ, log u, decisión) reutilizable si otra escena muestra el muestreo.

## (c) Auditoría final

```text
Auditoría de «08-mcmc» (2700 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (15/15)
Todo en orden.
```

Sin avisos de composición. Las 15 comprobaciones recalculan con DL.modelo:

- lp(θ) y lp(θ');
- θ' = θ + s_b z;
- la decisión log u < Δ;
- la coherencia de la cadena;
- la recursión de s_b;
- la aceptación por ventana;
- L_b L_b' frente a la covarianza del recorrido;
- R-hat y ESS frente a R;
- las compuertas en los 28 parámetros;
- `edoCtx` de las simulaciones;
- la media y los percentiles 2.5 y 97.5.

Fuera de la escena: después de unir la rama base, `node --test pruebas/*.test.mjs` da 106 pruebas que pasan y 1 que
falla. La que falla es `pruebas/aleatorias.test.mjs`, «ess caso 11», con una diferencia relativa de 3e-11 frente a
una tolerancia de 1e-12. Viene de la rama base, no de esta escena.

## (d) Tiempo de render

El render completo tarda 266 s: 2700 cuadros en 224 s, más ffmpeg y la hoja. El MP4 pesa 3.4 MB. Exportar los datos
(la corrida de producción de los dos sexos, con 4 núcleos, más la cadena instrumentada) tarda unos 5 min.
