# 02 · Las tasas por edad: nudos e interpolación — notas

## (a) Revisión adversarial y lo que se corrigió

Dos revisiones independientes sobre el borrador (commit 5a7ce5f): un metodólogo (contra `vignettes/el-modelo.qmd`,
`R/edo.R` y `R/configuracion.R`, rehaciendo los números en R) y un diseñador de movimiento (contra el guion, las reglas
del laboratorio, la hoja de contactos y cuadros sueltos). Las dos encontraron, por separado, el peor defecto.

Metodólogo:

| # | Hallazgo | Arreglo |
|---|---|---|
| 1 | En el cambio de escala log → lineal, los puntos de f (no recortados) volaban sobre el título (cuadros ~1275-1305 y ~1410-1452) | Puntos recortados al gráfico; f se apaga en el primer 15 % del cambio; comprobación nueva en la marca `cambio` |
| 2 | A W le faltaba «para a_j entre nudo_k y nudo_k+1» (no decía qué k ni que lo demás es 0) | La nota completa, como en la guía |
| 3 | Los supuestos de p no se decían (r = 0, p(30) = 0) y el final decía que θ sola fija p | Nota «dl_edo(): RK4 con paso h = 0.2 (escena 03), r = 0 y p(30) = 0»; final: «θ fija i y f; con r = 0 y p(30) = 0, también p» |
| 4 | «RK4 en la malla h/2» era impreciso (RK4 da pasos h; en h/2 solo se evalúan las tasas) | La misma nota; guion al día |
| 5 | «p acumula lo que entró antes» exagera: Δp llega a 3.1 puntos en 70 y baja a 0.6 en 99 | «…pero p cambia desde 50: guarda los casos nuevos, que f va quitando» |
| 6 | «i crece exponencialmente» como regla general | «Como log i es lineal por tramos, i crece o decrece exponencialmente» (la guía dice «crece o decrece») |
| 7 | f desaparecía en la escala lineal sin decir por qué | «f (10 a 90 por 1000) no cabe en esta escala» en la leyenda mientras dura |
| 8 | «se fijan» sugiere que los valores son conocidos | «se describen» (la guía) |
| 9 | «Fuera de los nudos» se lee como «lejos de los nudos» | «Fuera del rango de los nudos»; el guion anota que aquí el primer nudo es la edad de inicio |
| 10 | Comprobaciones que no podían fallar o medían menos de lo que decían: la de W en `final` no miraba lo dibujado; la segunda de `sube` comparaba R con R; `tramos` pasaba sin comparar nada; «planas en píxeles» comparaba valores; los textos leídos y los puntos del barrido no se comprobaban | `final` (W) renombrada como prueba del modelo; `sube` compara la i y la p dibujadas y la banda; `tramos` exige 12 tramos; `constante` mide píxeles y cuenta puntos; nuevas: textos leídos (2.88, 7.5, celdas, Σ), punto de i bajo el cursor contra la fila de R, fila en el nudo 40 y desde 95, bandas en su eje, f apagada a mitad del cambio |
| 11 | La flecha de W olvidaba exp (W da log i) | `θ →(W, exp) i(a), f(a)` |
| 12 | El pie no decía que el nudo ×3 es ilustrativo | Agregado en `datos/r/02-nudos.R` |
| 14 | El final nombraba θ sin mostrarla | «θ: estos 14 puntos, en escala log» y los 14 puntos engordan |
| 15 | «2K = 14» supone el prior por defecto | Se deja (es el caso por defecto); el guion lo anota (con la EMR fija en cero, θ = log i) |
| 16 | «constante» sobre la guía de 95 y el corchete solo en i | Etiqueta con fondo, alineada a la derecha; corchete también en f |

No se cambió: 13 (las unidades ya están en el título del eje).

Diseñador (10 notas, todas aplicadas):

1. Los puntos de f sobre el título en el cambio de escala (igual que el 1 del metodólogo).
2. El barrido nunca se detenía en un nudo: no se veía «en un nudo devuelve su valor». Ahora va de nudo en nudo (0.64 s por tramo, la mitad quieto). Comprobación en `barrido_nudo` (37.3 s, a_j = 40).
3. El cursor cortaba la leyenda: la leyenda se dibuja después del cursor y su fondo lo tapa.
4. La columna derecha sin jerarquía: la configuración y θ bajan al 40 % cuando entra W.
5. Dos movimientos principales al subir el nudo (la p se hinchaba a la vez): la p queda al 40 % hasta su frase.
6. El anillo y el valor del nudo aparecían de golpe: entran y salen con `visible()`.
7. El final no nombraba lo que queda: los 14 puntos engordan 5 px, escalonados 60 ms, y entra «θ: estos 14 puntos».
8. K = 7 no se decía: «con K = 7 nudos, θ reúne 14 números».
9. Números chicos para un teléfono: celdas de W y Σ en mono a 26-28 px; etiquetas de las bandas a 28 px.
10. Etiquetas sobre las guías: fondo detrás de «constante» y de «i(63) = 2.88 por 1000».

Además, en la vuelta de corrección: las marcas del eje y los títulos de las dos escalas se relevan (antes se
cruzaban y se encimaban a mitad del cambio).

Después, con la lista de hallazgos recurrentes de la sesión coordinadora (escenas 04, 05, 06 y 10): «por 1000» sin
denominador en tres etiquetas (i(63), el valor del nudo, la nota de f) pasó a «por 1000 personas-año»; i(63) se movió
a la izquierda del cursor, en dos líneas, y el valor del nudo a su derecha, para no chocar con la leyenda ni con f. Lo
demás de la lista ya se cumplía: valores ilustrativos de la guía (no el ajuste del ejemplo) y dicho en el pie; ningún
«igual» para un ≈; las comprobaciones miden lo dibujado; la cabecera nombra dl_edo(), que hace lo que se muestra.

## (b) Cambios que sugeriría en lib/ (hechos aquí, en index.html)

- `D.ejes` con escala mezclable: una opción `yMezcla(u)` (log ↔ lineal) que mueva marcas, rejilla y título. Aquí lo
  hacen `yTasa()` y `ejesTasas()`.
- Una opción `recortar` en `D.circulo` (o un `G.punto(x, y, op)` que recorte al área del gráfico), para que los puntos de
  una serie no salgan de los ejes, como ya pasa con `G.curva`.
- Una opción `fondo` en `D.texto` / `D.parrafo` (un rectángulo del color del fondo detrás de la etiqueta). Aquí hay
  cuatro rectángulos a mano, uno con el color de la banda calculado a mano (`FONDO_BANDA`).
- Un helper `D.leyenda(x, y, entradas, op)` con fondo, fija, para no pegar etiquetas de serie a curvas que se mueven.
- Un `D.filaMatriz(valores, etiquetas, op)` para mostrar una fila de W (sirve también para las bandas o los pesos de
  la escena 04).
- En `auditar.mjs`, un aviso cuando un elemento con color de serie se dibuja fuera del cuadro o sobre la cabecera:
  la auditoría no vio los puntos volando (solo mira textos).

## (c) Auditoría final

```
Auditoría de «02-nudos» (2340 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (18/18)
Todo en orden.
```

Las comprobaciones muerden: con tres mutaciones a propósito (el dominio del eje log de 120 a 125, la fila de W
evaluada en a + 0.05, el nudo movido 1.0 en lugar de log 3) la auditoría falló 4, 2 y 1 comprobaciones.

`node --test pruebas/*.test.mjs` después de unir la base: 105 pasan y 2 fallan, ambas en
`pruebas/aleatorias.test.mjs` (log-posterior con techo + 1 ulp y ESS a 3e-11). Vienen de la base («2 pruebas fallan,
en investigación», commit 4a2511f) y no tocan `edo` ni `matrizW`, que son lo que usa esta escena.

## (d) Tiempo de render

Render completo (`node herramientas/render.mjs escenas/02-nudos`): 2340 cuadros en 145 s, 177 s con el MP4 y la
hoja de contactos. La auditoría tarda unos 2 minutos.
