# 03 · Cómo se resuelve la ecuación (RK4) — notas

Escena de 80 s (2400 cuadros a 30 por segundo). Guion: `guion.md`. Datos: `datos/r/03-rk4.R` →
`datos/03-rk4.js` (`Rscript datos/exportar.R 03-rk4`).

## Decisiones

- **El paso que se muestra** es el n = 151, de 60 a 60.2 años. Usa las tasas de la sección 2 de la guía (valores
  inventados), las mismas de la escena 01. Así k1 = 1.825 ‰ por año es la pendiente que la escena 01 dibuja en la
  edad 60.
- **Las tasas en la malla h/2** se arman en R como lo hace `dl_edo()` por dentro (`.dl_edades_media`,
  `.dl_base_interp`, exp de W log θ). El guion de datos comprueba, con `identical()`, que `dl_edo()$p` es
  `dl_edo_resolver()` tomado cada 5 puntos, y que el p tras el paso, calculado etapa por etapa con las expresiones
  de `dl_edo_resolver()`, es `p_malla[152]`.
- **17 cifras.** `jsonlite` escribe 15 cifras significativas. Para comparar bit a bit, las mallas, las etapas y el
  error máximo también se exportan como texto con `%.17g` (`datos.exacto`). Con ellos, `DL.modelo.pasoRk4` y
  `resolverEdo` dan exactamente los mismos doubles que R.
- **Las cuatro pendientes difieren en menos del 0.6 %.** Con h = 0.2 años el paso es casi recto: las predicciones
  p2, p3 y p4 caen sobre el segmento final. Esto no se exagera en secreto.
  - El panel de pendientes tiene su propio eje, rotulado «el eje no empieza en 0», donde las diferencias se ven.
  - El panel de p muestra la geometría tal cual es.
- **El zoom** pasa de [30, 99] a [59.94, 60.26] años, unas 216 veces en la edad. Es geométrico: el ancho del
  dominio se interpola en escala log. Los puntos de la malla h aparecen cuando su separación pasa de 8 px.
- **La ilustración de estabilidad** usa valores inventados (i = 0.05, r = 15, f = 0), está rotulada y se resuelve
  con `dl_edo_resolver()` con nsub = 5 y nsub = 10.
  - Con f = 0 la ecuación es lineal y la exacta es i/k (1 − e^{−kt}).
  - El factor por paso de RK4, R(−k h), es positivo para todo k h real. Por eso la inestabilidad se dispara sin
    oscilar, y en pantalla no se dice «oscila».
  - El 2.785 que se escribe es la raíz de R(z) = 1, calculada en R con `uniroot`.
- **Precisión.** El chunk «precision» de la guía se copia tal cual. Su error relativo máximo es 8.4872e-11
  (sobre las edades 30-99) y en pantalla se muestra como 8.5 × 10⁻¹¹.

## (a) Revisión adversarial

Dos subagentes en paralelo, un metodólogo y un diseñador de movimiento. Leyeron los fotogramas de cada marca, la
hoja de contactos, `index.html` y las fuentes (R/edo.R, dl_core.cpp, R/insumos.R, la guía).

### Metodólogo

No encontró nada bloqueante. Verificó como correctos:

- las etapas y los pesos;
- los índices 301/302/303 (malla h/2) y 152 (malla h, base 1);
- las edades enteras en los puntos 1 + 5k de la malla h;
- la fórmula exacta y el 8.5e-11;
- la raíz 2.785;
- la ilustración: negativa desde el primer paso y monótona.

Sus hallazgos y lo que se hizo con cada uno:

1. **Dos «k h» distintos sin nombre.** El gráfico mostraba 3.01 = (i + r + f)/5, con i; el mensaje del paquete
   dice 3.00 = (r + techo)/nsub, sin i.
   - El recuadro del mensaje ahora se titula «dl_insumos() se detiene (extracto; usa r + techo de f, sin i)».
   - La auditoría comprueba que k h − i h es el número del mensaje.
   - Con nsub = 10 se escribe 1.505, no 1.51. El 1.51 venía de `toFixed(2)` sobre 1.5050000000000001.
2. **La narración decía «el paquete exige 2.5» como si acotara k h.** Ahora dice «dl_insumos() acota
   (max r + techo de f) h a 2.5», y la fórmula de la izquierda usa max r.
3. **El mensaje del paquete se mostraba recortado sin marcarlo** y con el prefijo cambiado.
   - Ahora conserva «dismodlite:» y los cortes van con «…».
   - Una comprobación verifica que cada línea, sin los «…», está en el mensaje en orden, y que todo corte está
     marcado.
   - Para arreglar aguas arriba, en `R/insumos.R`: el mensaje dice «para que la prevalencia no oscile a valores
     negativos». Con RK4 sobre el eje real no oscila, diverge; la escena no repite esa palabra.
4. **«por 1000 al año» era ambiguo**: se lee como una tasa por 1000 personas, junto a i = 2.5 por 1000. Ahora dice
   «‰ de p por año» en el panel y en las etapas.
5. **«Pesos 1, 2, 2 y 1: el promedio da el valor del paquete».** El promedio es una pendiente. Ahora dice «Con
   pesos 1, 2, 2 y 1, p1 + h · promedio es el p del paquete». Los «×1 ×2 ×2 ×1» del guion se quitaron: el
   promedio ya los escribe.
6. **«la ecuación decae con la tasa k»** pasó a «una perturbación de p decae al ritmo k». También se derivó
   ∂F/∂p = −i − r − f(1 − 2p).
7. **La solución exacta pide p(a0) = 0**, ahora en la narración. El recuadro del error dice «edades 30-99».
8. **«cada quinto punto»** pasó a «uno de cada cinco puntos». La etiqueta del punto 152 dice ahora «de la malla h».
9. **El p4 hueco quedaba oculto bajo el punto final.** Ahora es un anillo dibujado encima: p4 y p(a + h) difieren
   en 1e-9.
10. **Pequeños.** El título de la ilustración lleva «por persona-año». Las edades a, a + h/2 y a + h van en KaTeX.
    «con p(a0) = 0» también baja al 40 % al final. Las edades del gráfico de p ya no desaparecen tras el zoom: se
    quedan hasta que llega el eje compartido.
11. **Comprobaciones tautológicas.**
    - Se quitó «error_valor == lo exportado».
    - Los dominios dibujados se comparan con constantes escritas en la comprobación.
    - Se cambió «índices separados por NSUB», que era cierto por construcción, por la comprobación del dominio y
      de la edad.
    - Se agregaron: las predicciones p2, p3 y p4 en su píxel; la x de la línea del promedio y de los puntos RK4;
      p con h = 0.1 frente a `resolverEdo`; las curvas exactas de R frente a `prevalenciaExacta` y a i/k (1 − e^{−kt});
      k h frente al número del paquete.

### Diseñador

Sus 10 notas, todas aplicadas:

1. **Etapas atropelladas**: siete entradas en 1.3 s y en tres zonas. Ahora entran en orden: fórmula (t0),
   segmento (t0 + 0.4, el principal), punto hueco (t0 + 1.0), punto k en el panel (t0 + 1.2) y valor (t0 + 1.5).
2. **Mitad derecha vacía de 55.5 a 64 s.** Los ejes de la ilustración y la exacta, trazada en 2 s, entran con
   «decae al ritmo k».
3. **El final no aterrizaba.** La ilustración sale a los 74.7 s y vuelve el año de cinco pasos junto al recuadro
   del paso. El pop del recuadro pasa a los 75.4 s.
4. **El zoom no se leía como cámara.** El recuadro de 60 crece con el zoom hasta 12.6 s. Las edades vuelven tras
   el zoom.
5. **La salida de cámara no tenía ancla.** El paso calculado (p1, el segmento y p(a + h)) queda quieto durante la
   cámara 2 y se va al aparecer el arco 1.
6. **Trazos cortos de pendiente que ensuciaban.** Queda solo el de k4, la única etapa sin segmento, y se va con el
   promedio.
7. **Choque**: la guía de a + h cruzaba la etiqueta del promedio. Las guías se apagan al entrar el promedio.
8. **Choque**: p1 estaba sobre la curva. Se movió abajo, a la izquierda del punto.
9. **Números chicos para un teléfono.** Los k h pasaron a 30 px, p(a + h) a 32 px, la fuente de p(a + h) a 24 px y
   los valores de k a 30 px.
10. **Final inconsistente y una entrada temprana.** «con p(a0) = 0» ahora baja al 40 % como lo demás. La línea de h
    entra a los 5.45 s, después de la curva.

## (b) Cambios que sugeriría en lib/ (hechos aquí, en la escena)

- **Un `D.ejes` con dominio variable.** `panel()` en esta escena: dominio y rectángulo por cuadro, marcas
  «redondas» automáticas (`pasoRedondo`, `marcas`) y opacidad separada para las etiquetas de x y de y.
- **`zoomDom(d0, d1, foco, u)`**: el zoom geométrico de un dominio, con un valor foco que se mueve de su fracción en
  el primero a su fracción en el segundo. Sirve para cualquier escena con lupa.
- **`D.pct` y `D.cientifico`.** Un formato de porcentaje con decimales según el paso de las marcas, y la mantisa y
  el exponente en KaTeX (8.5 × 10⁻¹¹).
- **`D.ejes` podría devolver su dominio y su rectángulo como dato**, para que la auditoría lo compare con
  constantes.
- **Exportar con 17 cifras.** `exportar.R` podría escribir con `digits = I(17)`, o dar una ayuda `exacto()`.
  Hoy cada escena que quiere comparar bit a bit tiene que exportar texto con `%.17g`.
- **El aviso de «choque» compara cajas de foreignObject.** Una fórmula en modo bloque (`display: true`) tiene una
  caja más alta que su tinta. El aviso «exacta-e y exacta-p se tapan» es de cajas y no se ve en el fotograma.

## (c) Auditoría final (`node herramientas/auditar.mjs escenas/03-rk4`, tras fusionar `animaciones-metodologicas`)

```text
Auditoría de «03-rk4» (2400 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (12/12)
  ! composición
      choque: exacta-e y exacta-p se tapan desde el cuadro 1343
      simultáneos: p-ytitulo, p-xtitulo, paso-n-1, paso-n-2, paso-n-3, paso-n-4, paso-n-5, et-60, et-61, leyenda-mallas entran juntos en el cuadro 2252
Todo en orden.
```

Los dos avisos se revisaron a ojo:

- El choque es de cajas: la fórmula en modo bloque es más alta que su tinta, y en el fotograma no se tapan.
- Lo «simultáneo» es el regreso del año de pasos al final: es un solo grupo, ya visto en 39-43 s, que vuelve con
  un fundido.

Las 12 comprobaciones numéricas:

1. la curva es p_malla (resolverEdo) y sus edades enteras son dl_edo()$p, en su píxel;
2. los puntos 301, 302 y 303 de la malla h/2 están en a, a + h/2 y a + h;
3. k1..k4 dibujadas = `pasoRk4` = R (17 y 15 cifras, ≤ 1e-14), con su texto;
4. cada k y el promedio en su altura del eje;
5. los segmentos y las predicciones p2, p3 y p4 de R, en su píxel, con el dominio del paso;
6. p(a + h) = `dl_edo_resolver()` en 60.2, bit a bit, en su píxel y en su texto;
7. las edades enteras = cada 5 puntos de la malla h = dl_edo(), con el dominio de un año;
8. el error máximo escrito = el de R = el recalculado con `DL.modelo.edo` y `prevalenciaExacta`, y los 70 puntos
   RK4 en su píxel;
9. las curvas exactas de R son las fórmulas;
10. 2.785 es la raíz de R(z) = 1;
11. con h = 0.2 los pasos = R = resolverEdo, son negativos y k h − i h es el 3.00 del paquete;
12. con h = 0.1 los pasos = resolverEdo y siguen a la exacta, 1.505 − i h = el 1.5 que acepta dl_insumos(), y el
    extracto del mensaje está marcado.

`node --test pruebas/*.test.mjs` da 105 pruebas bien y 2 mal. Las 2 son de `pruebas/aleatorias.test.mjs`, que
llegó con la fusión del base: `lpComponentes` en el contexto 26 (`techo_+1_ulp`) y `ess` en el caso 11 (3e-11 >
1e-12). Son funciones de `lib/modelo.js` que esta escena no usa, y no las toqué (lib/ queda fuera de esta rama).
Las pruebas de `pasoRk4`, `resolverEdo` y `edo` pasan.

## (d) Tiempo de render

El render completo (`node herramientas/render.mjs escenas/03-rk4`, 3 páginas en paralelo) hizo 2400 cuadros en
139.9 s, y 171 s en total con el MP4 (2.6 MB) y la hoja de contactos. La auditoría tarda unos 2 min.
