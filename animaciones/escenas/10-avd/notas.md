# 10 · Los años vividos con discapacidad — notas

Bitácora de la escena: qué encontró la revisión adversarial y cómo se corrigió, qué cambiaría en `lib/`, la auditoría
final y el tiempo de render.

## Decisiones tomadas sin consulta

- **Sexo y banda.** Mujeres (sex_id 2), como la guía «Carga: los años vividos con discapacidad»; la banda del ejemplo
  es la de 70 a 74 años.
- **Simulaciones.** El ajuste del laboratorio (`ctx$ajuste()`: 4 cadenas de 4000 iteraciones, 2000 de calentamiento)
  guarda 800 simulaciones, no 1000: `dl_opciones_mcmc()` adelgaza 1 de cada 10. La escena dice 800. El comentario
  de `datos/exportar.R` (línea 14) dice «1000 simulaciones»; no se tocó porque está fuera de esta escena (ver abajo).
- **Las simulaciones que se muestran son las del paquete.** `datos/r/10-avd.R` llama a `.dl_simular_pi_dw()` con la
  semilla de `dl_avd()` y comprueba que las proporciones crudas, divididas por su suma, son las renormalizadas, y
  que `dl_avd()` da p × suma × COMO simulación por simulación (1e-14).
- **Como en `dl_correr()`.** `dl_avd(ajuste, insumos, dl_factor_comorbilidad(insumos), semilla = 1)` sobre el ajuste
  nacional; sin cascada (la nación de la cascada coincide con el ajuste nacional, lo comprobó la revisión).
- **Notación.** AVD, DW y COMO van rectos (`\mathrm`) y «ancla» en los superíndices como texto, como en la guía de
  carga (`\text{}`). Son los símbolos de la guía «El modelo», sección 8; en cursiva, KaTeX los separa letra a letra
  («AV D»).
- **Sin cámara en la fase A.** Las tomas cambian de disposición con fundidos; la única cámara declarada es la del
  cálculo de la banda (52.1-52.9 s), que sube y se escala para dejar sitio a COMO.
- **Duración.** 84 s (el objetivo era 50-80 s; el máximo es 90). Los segundos de más vienen de las frases que la
  revisión metodológica pidió completar (las condiciones de «el valor central sigue al ancla» y de «por encima del
  ancla»), que con la narración a una sola línea quedaron en dos frases.

## Después de unir la base (8098d7d)

La base subió la narración a 42 px y la auditoría ahora falla con una línea partida. Tres frases se acortaron:
«Absorbe comorbilidad, severidad por edad y otras diferencias: puede superar 1.», «La tabla se cancela en el valor
central: si $p$ sigue al ancla, los AVD también.», y la última se partió en «La tabla de severidad pesa en la
incertidumbre, no en el valor central.» y «Aquí COMO_b < 1: sin él, los AVD quedarían sobre el ancla.». De las
lecciones de la escena 01: el subrayado de p lleva `text-decoration-skip-ink:none`, el final baja al 40 % lo que no
es la conclusión, y se revisó que la escena no dibuje tasas sobre un eje de nivel ni anime transformaciones de
ecuaciones fuera de la sustitución, que la revisión metodológica ya rehízo y la auditoría compara con `dl_avd()`.

## (a) Revisión adversarial

Dos revisores independientes (subagentes) sobre la escena terminada: una metodóloga que leyó `R/avd.R`,
`R/comorbilidad.R`, `R/severidad.R`, las dos guías y rehízo los números en R, y un diseñador de movimiento sobre la
hoja de contactos y los fotogramas. Se aplicaron todos los hallazgos válidos; luego se volvió a renderizar y auditar.

### Metodología

No encontró errores numéricos: todos los números en pantalla coinciden con el paquete. Hallazgos y correcciones:

1. **La cuenta de COMO no cerraba con lo mostrado.** `0.000803 / (0.0569 × 0.0169)` da 0.835, no 0.833 (los
   operandos estaban redondeados). Ahora: `0.0008026 / (0.05686 × 0.016946) = 0.833`, y la auditoría rehace la cuenta
   desde las cadenas mostradas.
2. **Lo mismo en v.** `((0.3706 − 0.2739)/3.92)²` da 0.000609. Ahora con cinco decimales (0.37063 − 0.27394), y la
   auditoría lo comprueba.
3. **«Sin COMO, los AVD quedarían por encima del ancla» como regla general.** Solo vale porque aquí COMO_b < 1.
   Ahora: «Aquí $\mathrm{COMO}_b < 1$: sin él, los AVD quedarían por encima del ancla». La auditoría exige
   max COMO_b < 1.
4. **«Los AVD siguen a los del ancla» sin condición.** También hace falta que la prevalencia ajustada siga a la del
   ancla (en 80-84 no lo hace del todo: modelo 113.5 frente a ancla 105.8). Ahora: «si la prevalencia sigue a la del
   ancla, los AVD también»; y «1.006 en promedio: cerca de 1».
5. **«Absorbe la comorbilidad…» incompleto.** Ahora: «la corrección por comorbilidad, el patrón por edad de la
   severidad y otras diferencias de la tabla» (cabecera de `R/comorbilidad.R`).
6. **Las barras crudas no son π_{k,e}.** En `R/avd.R`, π_{k,e} es la proporción ya renormalizada. El título dice
   ahora «Proporciones sorteadas, antes de renormalizar» y pasa a «Proporciones renormalizadas π_{k,e}» después de
   la renormalización.
7. **La Beta no es «de π_e».** π_e es el valor central. Ahora: «La Beta de la proporción sorteada, estado leve
   (media π_e)».
8. **Faltaba el contexto del gráfico final.** Ahora: «AVD por 100 000, mujeres, Perú 2023», el eje «edad (años)» y
   el pie «Datos sintéticos del ejemplo (causa 9100, Perú, 2023), mujeres: … 800 simulaciones, semilla 1». El año
   sale de `insumos$cfg$years$ajuste`.
9. **El ancla en caja, no en aspa.** Se cambió a aspa (también lo pidió el diseñador).
10. **«ancla» en cursiva en los superíndices.** Leía como un producto de letras; ahora `^{\text{ancla}}`.
11. **Excepción del muestreo no dicha** (elementos constantes). Opcional; no se agregó a la narración, porque la
    tabla ya marca «constante» el DW del asintomático y la frase pasaría del tiempo de lectura.
12. **Comentario de `datos/exportar.R`** («1000 simulaciones»; son 800). Fuera del alcance de esta rama; queda en (b).
13. **Comprobaciones débiles o tautológicas en la auditoría.** Se rehicieron:
    - el histograma ya no recorta: cuenta las simulaciones fuera del eje y exige 0;
    - la banda exige que las piezas sean las exportadas del paquete (p_{k,b}, la suma de la simulación, COMO de la
      tabla) y que su producto coincida con `dl_avd()` a 1e-12;
    - el gráfico final lee de vuelta cada punto desde la escala del eje y lo compara con la media, el IC y el ancla
      de `dl_avd()`, y exige que todo quepa en el eje;
    - se agregaron comprobaciones de las cuentas en pantalla (v, COMO de 70-74), del rango 0.74-0.95 y del
      cociente 1.006.

### Movimiento

Las diez notas, todas aplicadas:

1. **El ancla tapaba al modelo.** El cuadrado de 16 px encerraba el punto de 5 px. Ahora el ancla es un aspa de
   18 px y el modelo un punto de 6 px (también en la leyenda).
2. **Demasiadas entradas dentro y justo después de la cámara** (cuadros 1563-1620). La cámara (52.1-52.9 s) ahora va
   sola: el foco en COMO, su narración y el recuadro arrancan en 52.9-53.0 s, el gráfico de COMO en 55.4 s, la línea
   del 1 en 55.8 s y el trazo en 56.0 s.
3. **La sustitución y el gráfico de AVD entraban casi juntos** (cuadros 2019-2080). Ahora el gráfico entra en
   69.0 s, las aspas en 69.4 s y el modelo en 70.4 s.
4. **«por 100 000» quedaba en 20.4 px** después de la escala 0.85. Ahora va a 26 px (22.1 px tras la escala).
5. **Las sumas crudas pegadas a las barras.** 270 px por unidad en vez de 300; las sumas, en una fila fija.
6. **El recuadro de la fila leve abarcaba también DW.** Ahora enmarca solo la celda de π_e.
7. **El escalonado de las filas** no seguía el guion. Vuelve a 120 ms (`ESCALONADO.fila`).
8. **La columna izquierda del final quedaba alta.** La sustitución baja a y = 600 y su nota a y = 730.
9. **Los círculos «sin COMO» parecían remates del bigote.** Ahora van rellenos del fondo.
10. **El gráfico de COMO quedaba pegado al cálculo.** Ahora va en y = 690, con 160 px de alto.

Además, de la primera vuelta propia:

- **Fracciones ilegibles en línea.** Se pasaron a `\dfrac`.
- **El foco de la ecuación atenuaba otras fórmulas.** La clase `.tp` del `<style>` de una fórmula es global en el
  documento, así que apagaba p en el cálculo y en la sustitución. Esas fórmulas usan ahora `.tq`.
- **Choque de textos.** El nombre del estado moderado chocaba con el IC de π, y luego los dos IC entre sí. Las
  columnas pasaron a x = 680 y x = 920.

Quedan como avisos (no fallas) las filas de la tabla y del cálculo que entran como una unidad: una fila se lee entera.

## (b) Cambios que sugeriría a `lib/`

(Implementados localmente o esquivados en `index.html`; `lib/` no se tocó.)

- **`D.formula` con `op.estilos` escribe un `<style>` global.** Una clase de una fórmula afecta a todas las demás
  que la usan en el mismo cuadro. Convendría que `formula()` prefije las clases con el `id` de la fórmula
  (p. ej. `.ecuacion .tp`). Aquí se esquivó con nombres distintos (`tp`, `tq`).
- **Una ayuda para la cuenta que cierra.** Algo como `D.cuenta([operandos], operacion, digitos)` que formatee los
  operandos y el resultado y registre con `c.dato` las cadenas mostradas, para que la auditoría compruebe que la
  cuenta cierra con los operandos redondeados. Aquí se hizo a mano (`vTxt`, `cTxt`).
- **`ejes()` debería exponer la inversa de la escala** (`G.xInv`, `G.yInv`), para que las auditorías lean de vuelta
  lo dibujado. Aquí se escribió `inv()` local.
- **Un marcador de aspa en `dibujo.js`.** `D.aspa(x, y, r, op)` y su par en la leyenda; el ancla lo usa en varias
  escenas.
- **`lectura()` cuenta una fórmula en línea como una palabra.** Una fórmula larga
  (`$v = ((\text{superior} - \text{inferior})/3.92)^2$`) pide más tiempo; podría contar sus símbolos o un mínimo
  por fórmula.
- **El comentario de `datos/exportar.R`** dice «1000 simulaciones»; el ajuste guarda 800 (adelgazamiento 10).
- **`.tp` y el subrayado de p.** El subrayado de 4 px de p queda corto con KaTeX (solo bajo la letra) y casi
  desaparece al atenuar el término. Un subrayado con `border-bottom` sobre el `\htmlClass` se vería mejor.

## (c) Auditoría final

```text
Auditoría de «10-avd» (2520 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (10/10)
  ! composición
      simultáneos: tab-estado, tab-pi, tab-dw entran juntos en el cuadro 200
      simultáneos: filas de la tabla (cuadros 215, 219, 222) y del cálculo de la banda (cuadros 1400, 1418, 1432, 1445):
      cada fila entra como una unidad
Todo en orden.
```

## (d) Tiempo de render

- Render completo (`node herramientas/render.mjs escenas/10-avd`: 2520 cuadros, MP4 y hoja de contactos):
  **3 min 27 s** (real) en el contenedor de la sesión.
- Fotogramas en las marcas (`--marcas`): unos 5 s.
- Auditoría: alrededor de 1 min.
- Exportar los datos (`Rscript datos/exportar.R 10-avd`, con el ajuste de 4 × 4000 iteraciones): unos 40 s.
