# 07 · Los datos locales — notas

## Decisiones

- **Los hombres.** Con ellos el estudio de prevalencia de 80 años y más queda por debajo del ancla, y la curva con
  datos se mueve visiblemente hacia él. Con las mujeres el ancla y los datos cuentan casi la misma historia y solo se
  estrecha el intervalo; la guía «Datos locales» lo comenta así. La rejilla de la toma 1 muestra los dos sexos (las
  25 filas nacionales); el resto, solo los hombres, y el pie lo dice.
- **Los dos ajustes.** «Sin datos» es `ctx$ajuste()`: 4 cadenas de 4000 iteraciones, R-hat máximo 1.057. «Con datos»
  es una copia del ejemplo con `datos_en_ajuste: [prevalencia, incidencia, mortalidad]` y `ancla.peso: 0.5`, como en
  la guía. Con 4000 iteraciones su R-hat máximo pasaba de 1.15; con 8000 (4000 de calentamiento) queda en 1.048. El
  pie da las dos longitudes de cadena.
  - Los dos ajustes difieren en dos cosas: los datos y λ. La narración dice «aun con λ = 0.5», porque bajar λ por sí
    solo ensancharía el intervalo (guía «Datos locales»).
- **q_d y ℓ_d de las tomas 2 y 3.** Se evalúan en la media posterior de θ del ajuste con datos, con las funciones
  internas del paquete (`.dl_edo_ctx`, `.dl_q_intervalos`, `.dl_integrando`, `.dl_lp_lognormal`, `.dl_lp_binomial`).
  - Su suma coincide con `.dl_lp_componentes()["datos"]` a 1e-9; el script de R lo exige con `stopifnot`.
  - La pantalla lo avisa debajo de los paneles y en la franja de la suma.
  - En la toma 5, el q_d de 80 y más es otra cosa: la media posterior del promedio por población, simulación por
    simulación (9.8 %). Por eso no coincide con el 9.68 % de q(θ̄), y la tabla lo rotula «q_d medio».
- **La Poisson.** El ejemplo no trae conteos de incidencia ni de mortalidad, así que la Poisson se muestra sin un
  caso numérico, y la tarjeta lo dice.
- **Sin movimientos de cámara.** Las tomas se reemplazan con un fundido de salida de 0.24 s y no se corre nada, así
  que `camaras` queda vacío.

## (a) Revisión adversarial

**Metodólogo**: 17 hallazgos, todos aplicados.

- Avisar que las curvas, q_d y ℓ_d están en la media posterior de θ del ajuste con datos.
- «Con los datos y λ = 0.5 … se estrecha» pasó a «Con los datos, aun con λ = 0.5, …».
- «Se omiten las constantes que no dependen de θ» era falso en general: las normales van completas. Ahora la
  narración nombra solo log C(n,x) y −log x!.
- «El logaritmo de esa densidad» (que se leía como la log-normal de x) pasó a «la densidad normal de log x en log q_d».
- «Los datos solo validan» era falso para las filas nacionales: `dl_insumos()` las deja fuera y solo validan las
  subnacionales. La narración dice ahora «el ajuste usa solo el ancla y los priors».
- La fila excluida trae valor y error, no casos y muestra, y su nota lo dice.
- La narración de qué filas entran agrega «de esas medidas».
- La leyenda del bigote dice ahora «si trae error».
- Unidades «por 1000 personas-año».
- «Ponderado por la población».
- La leyenda de la toma 5 se aclaró para lo punteado.
- La narración final dice «el ancho del intervalo».
- El pie dice «la mitad de calentamiento».
- El YAML marca `# por defecto` en `datos_en_ajuste: []` y en `peso: 1`.
- El gráfico de ℓ(q) dice «eje vertical recortado».
- Los bigotes en R se calculan con η general: (val + η) e^(±1.96 s_log) − η.
- No apliqué uno: «hombres» en el título de cada panel. Ya lo dice la nota bajo los paneles.

**Diseñador**: 10 notas, todas aplicadas.

1. Las barras de los intervalos en 80 años quedan a la izquierda de la guía, sobre un recuadro, con «sin» y «con».
   Antes, unas cajas de fondo rompían la banda.
2. La media sin datos queda punteada al 60 % después del cambio.
3. El héroe cruza la media de punteada a sólida, sin salto a mitad, y el contorno sin datos entra 0.3 s antes.
4. En la toma λ, la lectura de λ = 1 baja al 40 % cuando entra la de 0.5, y el bloque baja 40–50 px.
5. Las tarjetas miden 540 px y la franja de la suma sube a y = 816, dentro de la escena.
6. La tabla de lectura se estira con EASE.mover y sus textos entran escalonados 60 ms.
7. Cada muestra de color de la leyenda de los paneles entra con su panel.
8. La caja excluida no se delata antes del tachado (14.7 s).
9. «altura en q_d: exp(ℓ_d)» va a 26 px, sin superíndice.
10. Las escalas de los tres paneles quedan parejas (tope a la misma altura).

**Hallazgos recurrentes que mandó la sesión coordinadora** (de las revisiones de las escenas 04, 05, 06 y 10), y
cómo los apliqué:
- «IC» se leía como intervalo de confianza: la leyenda y la tabla dicen ahora «intervalo del 95 %» y «media».
- La comprobación de λ era una constante (λ = 0.5 frente a 0.5) y no podía fallar. Ahora recalcula en JS
  L_ancla(λ = 0.5) a partir de L_ancla(λ = 1) y de la forma cuadrática z' Σ⁻¹ z. Compara el resultado con el valor de
  `.dl_lp_mvn_ar1()` a 1e-9, y así prueba que λ divide a Σ.
- Las demás observaciones ya se cumplían:
  - el ejemplo numérico va rotulado «en la media posterior»;
  - las unidades llevan denominador;
  - «sin datos en el ajuste» nunca se presenta como validación.

**Lo que encontré yo, antes de la revisión**:

- El cuadro 0 salía vacío salvo la cabecera: la primera toma entraba con un fundido desde t = 0. Lo vio la hoja de
  contactos, no la auditoría, que solo mira el título.
- La auditoría comparaba la media en la edad exacta de 80 con el estudio de 80 y más, y fallaba. La comparación
  correcta es el q_d del estudio (el promedio en 80-99), que ahora se exporta desde R.
- Al fusionar la rama base (narración a 42 px en una línea), todas las frases caben.

Las comprobaciones de la escena miden lo dibujado contra algo independiente:

- L_ancla con λ = 0.5 se recalcula desde λ = 1 y se compara con el del paquete;

- cada ℓ_d se recalcula en JS con la fórmula de la guía y se compara con el del paquete;
- la suma se compara con `.dl_lp_componentes()`;
- las cajas llenas se cuentan contra las filas que el paquete pone en el ajuste;
- las barras de 80 años se miden en px contra la escala del eje (480 px = 24 puntos).

## (b) Sugerencias para `lib/`

- `auditar.mjs`, «cuadro 0»: comprobar que haya más que la cabecera (por ejemplo, al menos un texto legible fuera de
  la cabecera y el pie, o un mínimo de elementos), porque un fundido desde t = 0 deja la composición vacía y pasa.
- `DL.mov.visible`: una opción `{ entrada: 0 }` automática cuando t0 = 0, o un aviso.
- `D.ejes`: aceptar `yTitulo` sin `$` en `parrafo` para que no choque con lo que haya encima, y un `xTitulo` en dos
  líneas.
- `D.texto` no interpreta `$…$`; un `D.etiqueta` que elija `texto` o `parrafo` según haya fórmula evitaría errores.
- Un ayudante `D.tabla(x, y, filas, columnas, op)` con entrada escalonada (lo armé a mano en la toma 5).
- Un ayudante para «dato con intervalo de edad y bigote» (`marcaDato` aquí); otras escenas (05, 09) lo van a
  necesitar.
- Exponer en `exportar.R` un `ctx$copia(cambios_yaml)` que devuelva insumos de una copia del ejemplo con claves
  cambiadas, para no repetir la escritura del YAML en cada escena.

## (c) Auditoría final

```text
Auditoría de «07-datos-locales» (2535 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (9/9)
  ! composición
      simultáneos: panel-titulo-2, panel-2-xtitulo, panel-2-ytitulo entran juntos en el cuadro 842
      simultáneos: yaml2-archivo, yaml2-ancla, yaml2-peso1, yaml2-datos entran juntos en el cuadro 1694
Todo en orden.
```

Los dos avisos son bloques que entran como una sola cosa: el título y los ejes de un panel, y la tarjeta YAML.

## (d) Tiempos

- Render completo (2535 cuadros, MP4 y hoja): 3 min 30 s en el contenedor.
- Auditoría: unos 15 s.
- `Rscript datos/exportar.R 07-datos-locales`: unos 100 s (los dos ajustes).
