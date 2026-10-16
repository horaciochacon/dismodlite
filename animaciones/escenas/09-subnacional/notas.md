# 09 · De lo nacional a lo subnacional — notas

## Decisiones

- **Qué se sigue.** Mujeres (`sex_id` 2), simulación k = 1, Huancavelica (09) y la banda de población de 70 a 74 años
  (`age_group_id` 19). Huancavelica tiene un LDI y un HAQ claramente por debajo del país en esa simulación (y en la
  media) y un SEV algo por encima: se ven los dos canales (log i y log f) con efectos visibles. La guía de la
  estimación subnacional también la usa.
- **La llamada a la cascada.** `dl_cascada(ajuste, insumos, semilla = 1)` sobre `ctx$ajuste()`, como
  `.dl_correr_proyecto()` (R/herramientas.R). El ajuste del laboratorio pide 1000 simulaciones pero las cadenas guardan
  800 (el paquete avisa): en pantalla dice 800.
- **Los sorteos en pantalla.** `cascada$dX` guarda solo la diferencia; para mostrar X_nac, X_d y β_c de la simulación
  1, `datos/r/09-subnacional.R` repite los sorteos de `.dl_dX` y `.dl_draws_betas` en su orden (cabecera de
  R/cascada.R) con las funciones internas del paquete. Reproducen `cascada$dX` en las 440 000 filas con diferencia 0, y
  log i_d − log i_nac − Σ δ_c (y lo mismo para f, con el techo) da 0 en todas las edades.
- **Sin mapa.** El paquete no trae geometría: las 25 ubicaciones van en un gráfico de puntos ordenado por su
  prevalencia de 70-74 años antes de renormalizar.
- **El modo plano** sale de correr el paquete (una copia del ejemplo con `subnacional.modo: plano`, como en la guía),
  no de suponer que el factor es 1. **El modo razón** solo se muestra con sus fórmulas: no hay tabla `razones` en el
  ejemplo y no se inventan números.
- **Escala log del gráfico de tasas**, en «por 1000 personas-año»: el desplazamiento es aditivo en log, así que en esa
  escala es una traslación vertical; el techo se rotula en las dos unidades.

## (a) Revisión adversarial

Dos revisores independientes, en paralelo, sobre los fotogramas de cada marca, la hoja de contactos y el código.

**Metodólogo** (recalculó la cascada en R; nada incorrecto, 3 engañosos, 6 imprecisos):

1. «techo f_max = 0.25» sobre un eje en «por 1000»: ahora «250 por 1000 (0.25 por persona-año)». Además dice «aquí no
   se alcanza» (truncados = 0; f_d máxima 0.091).
2. La fila del SEV de la tabla del paso 2 no se podía cotejar con la dX del paso 1 (0.596 × 0.081 ≠ 0.062: δ usa la
   interpolación en 70, no la banda 70-74). La tabla lleva ahora la columna Σ_b dX_b P_ba a los 70 (0.103 / −0.762 /
   −10.47).
3. La leyenda «▼ sorteos» solo describía el triángulo: ahora «sorteo de X_nac» (triángulo) y «sorteo de X_d» (punto).
4. P_ba se mostraba bajo «1 · Diferencia»; la guía lo define en el paso 2. La fila de pasos cambia en 30.4 s y la
   narración dice «Segundo, por edad…».
5. La fila de pasos seguía en el paso 3 con el gráfico del paso 4 en pantalla: el paso 4 empieza en 55.5 s.
6. «un factor … para p y para i(1-p)» sugería un solo factor: «un factor para p y otro para i(1-p)».
7. La regla general de los canales no se decía: nota «efecto_sobre prevalencia o incidencia: log i; mortalidad_exceso:
   log f».
8. Modo razón: ahora dice que la cascada es la plana y que el reparto va después de los AVD.
9. Validación: agrega la condición «con una cascada por covariables».
10. (nit) La raya nacional del SEV es el valor estandarizado por edad (cierran en él todas las bandas): la etiqueta lo
    dice; LDI y HAQ dicen «todas las edades».
11. (nit) Título del gráfico de puntos: «Prevalencia media de 70 a 74 años».
12. (nit, no aplicado) «los casos cambian solo con la población» frente a «difieren solo por la población»: la línea
    del plano se reescribió para caber en 42 px («no hay gradiente») y la nota dice «difieren solo por su población».

**Diseñador** (10 notas, todas aplicadas):

1. El texto de la validación (33 palabras) duraba 4.5 s: se acortó y la toma empieza en 83.7 s (la auditoría no lo veía
   porque llega al final; queda anotado abajo como sugerencia).
2. La mitad derecha quedaba vacía 12 s: el gráfico de puntos baja al 40 % en el modo razón y vuelve al gradiente
   renormalizado para el final.
3. Cuadros vacíos entre tomas (salida que termina 0.2 s antes de la entrada): las entradas empiezan cuando termina la
   salida anterior.
4. Trece elementos en un segundo (18.1-19.2 s): los sorteos en tres tiempos (intervalo 18.1, ubicación 19.8, sorteos
   21.3).
5. Gráfico de 25 nombres a 22 px de paso: solo los extremos, Lima y Huancavelica; eje más ancho (el movimiento de la
   renormalización pasa de 20 a 30 px); el segmento de Huancavelica con un marco visible; sin la raya suelta bajo la
   barra.
6. El paso 3 daba p a los 70 años (5.86 %) y el 4 la banda 70-74 (6.27 %): el paso 3 marca la banda y da los mismos
   números.
7. Fila de pasos: dos barras a la vez en cada cambio y barras más largas que el texto: la que entra espera a que salga
   la otra; anchos ajustados.
8. log i_d y log f_d entraban a la vez que el movimiento principal: 0.15 s después y tras el arranque de f.
9. Rótulos de las franjas en el canal entre columnas y «dX = 0.081» pegado a la leyenda: franjas a x 1230, rótulos a
   1080.
10. Techo en otras unidades que el eje y dominio vacío arriba: unidades del eje y dominio hasta 0.3.

**Mensaje del coordinador** (base movida a 8098d7d): se fusionó; la narración, ahora a 42 px, se reescribió para caber
en una línea; los subrayados de KaTeX llevan `text-decoration-skip-ink:none`; la comprobación «la p dibujada es la del
arreglo» (que no podía fallar) se cambió por una independiente: la p dibujada cumple dp/da = i_d (1 − p) − f_d p (1 − p)
por diferencia central, fuera de los nudos (en los nudos las tasas tienen quiebres y la diferencia central se aleja
hasta 5 %).

## (b) Sugerencias para la biblioteca compartida

- `auditar.mjs`: la regla de lectura exime a todo texto que llega al final; debería eximir solo el tiempo que falta y
  exigir igual 0.3 s por palabra + 0.5 s desde su entrada (aquí lo encontró el diseñador, no la auditoría).
- `D.ejes`: permitir una leyenda nacional/ubicación (trazo de guiones frente a lleno) y rótulos al final de cada curva;
  aquí `leyenda()` está en index.html.
- Un helper de franja de puntos con apilado («beeswarm» determinista) y de gráfico de puntos ordenado (`FRANJAS`,
  `puntos()` en index.html): servirían a las escenas de datos locales y de corrida.
- Un helper de barra apilada con revelado y escala (`barras()`), útil para cualquier cierre «la suma es el total».
- `D.formula`: el subrayado de las tasas como opción (`tasas: true`) en lugar de repetir `SUBRAYADO` en cada escena,
  ya con `text-decoration-skip-ink:none`.
- `D.parrafo` con `ancla: 'end'` y un ancho mayor que el texto ocupa todo el ancho: un `ajustar: true` que mida el
  texto ayudaría a la auditoría de choques.
- `contrato.js`: un `c.dato` que acumule por marca, para comprobaciones que comparan varios cuadros.

## (c) Auditoría final

```
Auditoría de «09-subnacional» (2700 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (8/8)
Todo en orden.
```

Las ocho comprobaciones: los sorteos repetidos reproducen `cascada$dX` y la dX mostrada es la del paquete; la línea
Σ_b dX_b P_ba es la de `.dl_pesos_edad()` en cada edad (< 1e-12); κ en pantalla es el de la cascada; las curvas
desplazadas de i y f son las que resolvió el paquete (1e-12); la p dibujada cumple dp/da fuera de los nudos (< 1 %); el
factor recalculado es el del paquete (1e-12) y el mostrado su redondeo; después de renormalizar, Σ_d N_d q_d =
N_nac q_nac (1e-10) para p y para i(1 − p), y q_d es la de `cascada$draws_q`; en el modo plano el factor es 1 y los
puntos quedan en el valor nacional.

## (d) Tiempo de render

Render completo (2700 cuadros, MP4 y hoja de contactos): 205 s de reloj, 169.9 s de cuadros con 3 páginas. Fotogramas
de las marcas: unos 12 s. Auditoría: unos 21 s. Exportar los datos (`Rscript datos/exportar.R 09-subnacional`, con el
ajuste nacional, la cascada y el ajuste y la cascada del modo plano): unos 75 s.
