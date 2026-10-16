# 11 · Una corrida, de principio a fin — notas

Escena de síntesis: los tres comandos y los trece pasos de una corrida de `dl_correr()`, cada uno con la escena de la
serie que lo explica. 90 s, 2700 cuadros. Datos: `datos/r/11-corrida.R`, que corre
`dl_correr(dl_ejemplo(), causa = 9100, semilla = 1, rapido = TRUE, carpeta_salida = tempdir())` (unos 2.5 min) y lee
de la corrida los mensajes, `manifest.yaml`, `diagnostics/`, `etiquetas/` y `cause/`.

## Decisiones

- **El orden de los pasos sale del código, no del guion pedido.** `datos/r/11-corrida.R` busca cada llamada en el
  cuerpo de `.dl_correr_proyecto()` (`R/herramientas.R`) y se detiene si no están en orden; además comprueba que los
  mensajes que emitió la corrida aparezcan en el mismo orden. De ahí salen dos diferencias con el encargo:
  - la compuerta de convergencia va **justo después del ajuste nacional**, no después de los AVD (la corrida la
    comprueba antes de gastar tiempo en el resto);
  - el resumen es **`dl_resumir()`**, no `dl_estimaciones()`: `dl_correr()` no llama a `dl_estimaciones()` (la tabla
    por edad simple de un ajuste); las celdas por banda, ubicación y sexo salen de `dl_resumir()`.
- **Trece pasos «de esta corrida».** Se dibujan los que corrieron: también `dl_ajustar_solo_prior()` (la referencia de
  las etiquetas) y `dl_factor_comorbilidad()` (el ancla del ejemplo trae AVD). No se dibujan los condicionales que
  no corrieron (el reparto por razón, `dl_repartir_razon()`; la rama sin cascada) ni la comprobación de la severidad
  dentro de los insumos; la cabecera dice «13 pasos de esta corrida».
- **La llamada en pantalla es la que corre.** El ejemplo vive en la instalación del paquete y `dl_correr()` se niega a
  escribir ahí: la llamada lleva `carpeta_salida = tempdir()`, y el guion de datos corre exactamente esa.
- **Los diagnósticos y resultados son de la prueba.** R-hat máximo 2.31 y ESS mínimo 11.4 (no pasan la compuerta; la
  prueba la salta y el manifiesto lo declara). Una etiqueta fija «corrida de prueba» acompaña todos los paneles desde
  el primer paso, y el pie lo dice.
- **Los avisos de la revisión no se cuentan.** `dl_revisar_proyecto(dl_ejemplo(), causa = 9100)` da 16 comprobaciones,
  0 errores y 4 avisos, pero los 4 avisos son advertencias internas de data.table («Item n has 0 rows but longest item
  has 1; filled with NA»), no problemas del proyecto. La escena muestra «16 comprobaciones, 0 errores».
- **Fichas de escena.** Según la tabla de `LEEME.md`. La validación y la compuerta del ancla no tienen escena propia:
  llevan «esc. 11». Las etiquetas apuntan a la 07 (`datos_en_ajuste`, que decide si los datos pueden moverlas).
- El identificador de la corrida lleva la fecha del día en que se exportan los datos (`2026-10-16_..._v1`):
  re-exportar otro día cambia ese texto, no los números.

## (a) Revisión adversaria y lo que se corrigió

Dos revisores independientes en paralelo, sobre los fotogramas de cada marca, la hoja y el código.

**Metodólogo** (16 hallazgos, todos aplicados salvo donde se dice):
1. La llamada mostrada fallaba tal cual (sin `carpeta_salida`) → la llamada y la corrida usan `carpeta_salida = tempdir()`.
2. «9 ajustes y 2 cascadas» era falso: se corre una cascada por κ dentro de cada (λ, ρ) → «9 ajustes × 2 κ = 18
   cascadas», y la narración «otros λ y ρ en el ajuste, otros κ en la cascada». La auditoría comprueba que
   `sensibilidad.csv` tenga una fila por ajuste, κ, sexo y banda.
3. Faltaba `dl_factor_comorbilidad()`, que sí corrió → paso 8 con su panel (la fórmula de COMO_b de la guía y sus 24
   valores, 0.73-0.95); R verifica su posición en el código.
4. Fichas: la validación y la compuerta 2 apuntaban a la 05, que no las explica → «esc. 11».
5. Los 4 avisos de la revisión eran ruido del paquete → no se cuentan (arriba).
6. «Esta no se salta nunca» exageraba (el máximo se cambia en la configuración) → «ni rapido ni forzar la saltan;
   el máximo va en la configuración».
7. «25 filas fuera del ajuste» sugería que las otras entraban → «25 nacionales: no entran / 160 subnacionales: solo
   validan».
8. Las etiquetas se calculan en las 24 celdas nacionales y las subnacionales heredan → se dice en el panel; la
   narración habla de «por sexo y banda».
9. Los resultados de la prueba no estaban marcados → etiqueta fija «corrida de prueba».
10. Las claves del manifiesto se mostraban truncadas → rutas completas (`inputs.bundle_hash`,
    `configuracion.por_defecto`, `validacion.gates.force`, `limitaciones`, `decisiones`).
11. «con dl_proyecto() se repite la corrida» → «inputs/contrato/ es la carpeta de un proyecto: con ella se repite la
    corrida».
12. `dl_nuevo_proyecto()` crea plantillas, no las tablas → narración corregida; el panel dice «las 8 tablas que trae
    dl_ejemplo()».
13. «con la misma semilla, la misma corrida» (cada corrida tiene un `_v<n>` nuevo) → «lo corre todo, con una semilla».
14. La validación hace más que la prevalencia → «también valida la incidencia implícita y la amplitud subnacional».
15. La calibración COMO quedaba sin decir → ahora es un paso propio (8).
16. «28 parámetros» → «(14 por sexo)».

**Diseñador de movimiento** (10 notas, todas aplicadas):
1. En cada cambio de paso cuatro cosas a la vez → el marcador llega cuando empieza la frase, el panel entra 0.15 s
   después y el anterior sale 0.5 s antes.
2. La lista perdía jerarquía → futuras 40 %, activa 100 %, hechas 70 %; las compuertas en la marca solo activas;
   recapitulación final (los trece al 100 %, uno por uno, desde 86.6 s).
3. La mitad derecha vacía 6 s → la llamada se queda hasta que entra el primer paso.
4. Las lecturas (2.31, 11.4, 0.033) eran más chicas que sus rótulos → mono 40 px.
5. La función se repetía en el panel y en la lista → el subtítulo queda solo donde lleva la regla (las compuertas).
6. La cascada no tenía movimiento principal → los departamentos salen de la línea nacional hacia su valor.
7. La compuerta 2 conservaba el título de la validación → título propio.
8. Compuerta 1 apretada → el bloque de ESS baja 40 px y el veredicto 36 px.
9. Las fichas de escena ensuciaban la columna 65 s → solo la del paso activo queda encendida.
10. Final apretado y suelto → una línea une `manifest.yaml` con su tarjeta; campos a 22-24 px; la recapitulación.

Además, por el aviso de la sesión coordinadora: fusión de la base (narración a 42 px en una línea), subrayado sin
`skip-ink`, y comprobaciones de la auditoría contra fuentes independientes del dibujo (el orden de las llamadas en el
código y de los mensajes, el lado del umbral en cada compuerta, la tabla de ubicaciones, la configuración, el eje).

Por los hallazgos recurrentes de las revisiones de las escenas 04, 05, 06 y 10 (aviso de la sesión coordinadora):
«no pasa» lleva «lo esperable con cadenas cortas»; la validación dice «sin datos locales en el ajuste, se espera que
pase»; los AVD van «por 100 000 personas»; los valores se rotulan «media» y sus cuantiles 2.5/97.5, «intervalo del
95 %».

## (b) Cambios que sugeriría a la biblioteca compartida

- **`DL.dib.escala(x0, x1, y, dominio, marcas, formato)`**: un eje horizontal suelto (sin área de datos), para
  umbrales y lecturas de un solo número; aquí vive como `escala()` dentro de la escena.
- **`DL.dib.marcaVerificacion` / `cruz`**: dibujadas como trazo (sin depender de que la fuente tenga ✓ o ✕).
- **`D.cabecera` con un pie de fila opcional** («corrida de prueba») o una etiqueta de estado estándar arriba a la
  derecha del panel, para escenas que muestran resultados que no sirven para publicar.
- **Una lista de pasos con marcador** (`DL.dib.pasos({ filas, activo(t), ... })`): la columna con la fila activa, las
  hechas y las futuras, que otras escenas de flujo podrían reusar.
- **`M.lectura` y los «×», «·», «%» sueltos**: cuentan como palabras; una fórmula en línea cuenta como una. Convendría
  contar solo palabras con letras o cifras, o documentarlo.
- En `auditar.mjs`, la ventana de «simultáneos» (0.1 s) choca con el escalonado `ESCALONADO.fila` (0.12 s) cuando cada
  fila tiene dos textos; sugeriría 0.15 s como escalonado por defecto para filas de dos textos.

Fuera de la animación: los 4 avisos de `dl_revisar_proyecto()` sobre el ejemplo vienen de una advertencia de
data.table en la traducción del proyecto (`.dl_causas_proyecto`); valdría corregirlos en el paquete.

## (c) Auditoría final

```text
Auditoría de «11-corrida» (2700 cuadros, 30 por segundo)
  ✓ contrato
  ✓ pureza
  ✓ cuadro 0
  ✓ lectura
  ✓ escena (25/25)
Todo en orden.
```

## (d) Tiempo de render

Render completo (`node herramientas/render.mjs escenas/11-corrida`): 2700 cuadros en 183 s, 226 s con el MP4 y la
hoja. `--marcas`: unos 6 s; auditoría: unos 20 s. Exportar los datos (`Rscript datos/exportar.R 11-corrida`, que
corre la corrida de prueba con la sensibilidad): unos 150 s.
