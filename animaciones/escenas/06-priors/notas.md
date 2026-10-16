# 06 · Los priors — notas

## Decisiones tomadas sin consultar

- **Sexo:** hombres (sex_id 1), como pide el encargo. Solo el techo por defecto (0.234) usa los dos sexos, porque
  `.dl_techo_emr()` toma el máximo sobre sexos y bandas. El rótulo y el pie lo dicen.
- **Eje x de log i por nudo, no por edad.** D2_k se toma nudo a nudo sin dividir por la distancia, y los nudos (30,
  40, …, 80, 95) no están equiespaciados. Por eso el gráfico pone los siete nudos a la misma distancia y lo rotula.
  La inclinación de la demostración es «+0.25 por nudo» (lineal en el índice, girando en el nudo de 60). Una
  inclinación lineal en la edad cambiaría D2_5, porque el último tramo mide 15 años. La narración dice «pendiente
  por nudo».
- **Deformaciones ilustrativas** (+0.8; +0.25 por nudo; +0.6 en el nudo de 60) y **σ = 1**: el pie las rotula como
  ilustrativas. Todo lo demás sale del paquete.
- **El costo de doblar** se mide como ΔL_suavidad al mismo σ (4.29 con σ = 0.5; 1.07 con σ = 1). L_suavidad absoluta
  con σ = 1 no sirve para comparar: su constante de normalización cambia con σ.
- **Duración: 82.5 s.** Pasa del objetivo de 50-80 s y queda dentro del máximo de 90. Las narraciones cumplen la
  lectura mínima y no hay cómo meter dos priors y tres variantes en menos tiempo sin cortar contenido pedido.
- **Las D2 del ejemplo son pequeñas** (0.017 a 0.051, frente a σ = 0.5). El eje de las barras empieza acercado
  (±0.08) para que se lean y se abre a ±1.8 en una cámara declarada antes de que entre la franja ±σ. En el gráfico de
  log i, la separación de cada triplete mide 3 px: el barrido la muestra tal cual, sin exagerarla.

## (a) La revisión adversarial y qué se corrigió

**Metodólogo.** Rehízo en R todos los números (incluido el ajuste completo, semilla 1). Ninguna fórmula ni número en
pantalla estaba mal. Encontró cuatro rótulos engañosos y siete imprecisos:

- **Techo por defecto (0.234):** usa los dos sexos y solo aplica sin `mortalidad_exceso.techo`. Corregido: «f_max =
  0.25 por persona-año: mortalidad_exceso.techo del ejemplo» y «sin mortalidad_exceso.techo, 3 × la mayor csmr/p del
  ancla (dos sexos): 0.234». El pie aclara lo mismo.
- **«Nivel y pendiente los fija el ancla»:** invitaba a leer una inclinación en la edad como invariante. Ahora dice
  «pendiente por nudo».
- **La atenuación de «cero» seguía con «tabla»:** ahora se levanta al entrar «tabla», que vuelve a mostrar el prior
  por banda bajo el techo. La franja plana sale al entrar «cero».
- **Leyenda de las bandas:** decía «banda del ancla»; es el 95 % del prior de f. Ahora dice «prior de f por banda:
  centro y su 95 %».
- **factor_sd:** se nombraba sin decir qué hace. Ahora la nota dice «emr_prior.factor_sd, que multiplica sd_log».
- **Tarjeta «tabla»:** ahora dice que es por sexo, del tramo que contiene al nudo, sin extrapolar, con el mismo L_emr
  y el mismo techo.
- **Desfase de la curva posterior:** los puntos estaban 9 px al lado de los quiebres de su curva. Ahora están en la
  edad del nudo; el prior va dentro de su banda.
- **Posterior durante «plano»:** la media posterior es de un ajuste con desde_ancla, así que sale al entrar «plano».
- **Guion:** decía 1.08 con σ = 1; el valor es 1.07. El tiempo de la tarjeta «tabla» quedó alineado con el código.
- **Piso del prior plano:** el paquete usa max(cota inferior, f_max/1000). Con la configuración simple siempre es
  f_max/1000, así que no hizo falta cambiar nada.

**Diseñador de movimiento.** Diez notas, todas aplicadas:

1. Las D2 eran invisibles (barras de 2-3 px). Ahora hay una cámara de zoom sobre el eje de las barras: ±0.08 → ±1.8.
2. El mapeo nudo → banda se leía al revés. El bigote del prior va ahora dentro de la banda [a, a + 5) del nudo.
3. La invariancia no se veía. Durante subir e inclinar, los valores de D2 van en la marca como testigos, con «Las
   cinco D2_k y L_suavidad: sin cambio». Al doblar, las tres barras que cambian van en la marca.
4. Las guías de los nudos cruzaban textos. Ahora salen a 56.5 s, y la leyenda pasó a la columna derecha.
5. El momento de 42-48 s estaba sobrecargado:
   - los ejes de f vuelven a 43.0 s, como en el guion;
   - la nota se acortó y entra a 46.2 s, cuando ya llegaron las bandas.
6. El sostén final mostraba «tabla» sobre un gráfico apagado. Corregido junto con el punto del metodólogo.
7. La tarjeta «plano» repetía la narración. Ahora dice «Sin prior por banda: L_emr = 0. Piso 0.00025, techo 0.25 por
   persona-año».
8. Las lecturas no tenían jerarquía. El costo de doblar va a 28 px y en la marca; la lectura con σ = 0.5 pasa a
   tinta suave cuando σ cambia.
9. El rótulo del techo estaba sobre el rayado. Ahora va sobre una placa mate del color de la superficie, sin sombra.
10. Legibilidad en teléfono: los valores de D2 suben a 26 px, «±σ» va a 26 px en tinta y la leyenda ya no tiene un
    superíndice de 14 px.

Después de corregir: las marcas se volvieron a renderizar y mirar. Apareció un choque entre los rótulos ±0.05 y 0.0
al abrir el eje de D2 y se corrigió.

## (b) Cambios que sugeriría para la biblioteca compartida (no aplicados: `lib/` no se toca)

- **`D.ejes` con `yLog`:** no permite cambiar el dominio y los ticks con suavidad. Aquí se reconstruye el eje en cada
  cuadro (`ejesF(t)`, `ejesD2(t)`). Un `ejes.interpolar(op0, op1, u)` que filtre los ticks dentro del dominio
  ahorraría el código repetido en cada escena con cámara de ejes.
- **Patrones de rayado:** la escena define `<pattern id="rayado">` a mano. Un `D.rayado(id, color, opacidad)` en
  `dibujo.js` daría el mismo rayado a todas las escenas que muestren regiones prohibidas (techo, soporte).
- **`D.bigote(x, ylo, yhi, op)` y `D.rombo(x, y, r, op)`:** se definieron localmente. Son marcas de prior e intervalo
  que usarán otras escenas (05 ancla, 07 datos).
- **`D.parrafo` con placa (`op.placa`):** una placa mate opcional detrás del texto, para rótulos sobre rayados o
  franjas.
- **`DL.mov.lectura`:** cuenta «±», «×» o «=» sueltos como palabras. Conviene que la auditoría y el guion digan
  explícitamente que se cuenta así, porque al escribir la narración se calcula a mano y sale corto. Una herramienta
  `node herramientas/lectura.mjs escenas/<escena>`, que liste cada línea con su tiempo y el mínimo, habría ahorrado
  una vuelta.
- **`choques` en la auditoría:** solo avisa. Un choque entre dos rótulos con id debería fallar salvo que se marque
  como intencional.

## (c) Auditoría final

```text
Auditoría de «06-priors» (2475 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (10/10)
Todo en orden.
```

Las diez comprobaciones numéricas:

- las D2 dibujadas son las segundas diferencias del log i exportado y las de R (1e-12);
- L_suavidad = la de R;
- subir e inclinar no cambian las D2 ni L_suavidad;
- doblar cambia D2_2, D2_3 y D2_4 en +0.6, −1.2 y +0.6;
- σ llega a 1 y el costo baja;
- el prior de cada nudo calculado en la escena es el de `.dl_ctx_emr()`, y el nudo 30 lleva sd × 2;
- L_emr desde lo dibujado = la de R;
- techo 0.25;
- piso del prior plano = f_max/1000.

## (d) Tiempo de render

El render completo (`node herramientas/render.mjs escenas/06-priors`: 2475 cuadros, MP4 y hoja de contactos) tardó
2 min 55 s en el contenedor de la nube. Exportar los datos (`Rscript datos/exportar.R 06-priors`, con el ajuste de
4 cadenas) tarda unos 25 s.
