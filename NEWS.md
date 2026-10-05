# dismodlite 2.3.0 (en desarrollo)

## Mortalidad en exceso fija en cero

* `mortalidad_exceso: {prior: cero}` (en el formato completo, `emr_prior.tipo: cero`) fija la mortalidad en exceso en
  0 y no la estima, para una causa sin muertes. Los parámetros son solo log i en los nudos (un bloque del
  muestreador), la ecuación se resuelve con f = 0 exacto, en R y en C++, y la log-posterior no tiene término de
  mortalidad en exceso ni techo. Sin esta opción, log f de una causa sin muertes no tiene información y sus cadenas no
  convergen.
* No pide la `mortalidad` en la tabla `ancla` (si la trae, no se usa). No admite `mortalidad_exceso.techo`,
  `mortalidad_exceso.fraccion_aguda`, `sensibilidad.fraccion_aguda` ni `mortalidad` en `datos_en_ajuste` (con f = 0
  el modelo predice 0 muertes): la revisión del proyecto lo dice con la clave.
* Vale en todo el recorrido de `dl_correr()`, con los dos motores: en la cascada, f es 0 en toda ubicación (una beta
  con `efecto_sobre: mortalidad_exceso` no tiene efecto); la validación con la mortalidad subnacional reservada se
  omite, con su motivo; `dl_estimaciones(x, "mortalidad_exceso")` da 0.
* El manifiesto de la corrida lo declara: `params.emr_prior: cero`, `params.emr_cota: [0, 0]`,
  `params.emr_cota_origen: cero` y una limitación; `diagnostics/mcmc.csv` no trae filas de log f.
* Los proyectos con los otros priores (`desde_ancla`, `plano`) dan los mismos números que en la versión 2.2.0.

## Reparto subnacional por razón

* Modo subnacional nuevo, `subnacional: {modo: razon}` (`cascada.modo: razon` en el formato completo), para una causa
  cuyo patrón entre ubicaciones sale de una razón ya calculada y no de covariables: cada ubicación recibe la tasa
  nacional por su razón, simulación a simulación y con cierre exacto en el valor nacional, en la prevalencia, la
  incidencia y los AVD. La razón es la misma en todas las edades y sexos, y el manifiesto lo declara como limitación.
  En un proyecto el modo se declara solo con `subnacional.modo`: `avanzado: {cascada: {modo: }}` no puede cambiarlo a
  `razon` ni desde `razon` (es un error de configuración).
* Tabla nueva del contrato de insumos, `razones` (la undécima; `?dl_tablas`): `ubicacion`, `anio`, `razon` y
  `error_log` (el error estándar del logaritmo de la razón), con `causa` y `fuente` opcionales. Una razón 0 deja la
  ubicación sin casos. `dl_plantilla("razones")` da su plantilla y `dl_nuevo_proyecto()` la escribe; su `LEEME.md`
  dice cuándo llenarla.
* `dl_proyecto()` y `dl_revisar_proyecto()` comprueban que, en el año que se estima, la tabla traiga la razón de cada
  ubicación subnacional de la población y de ninguna otra (tampoco de la nacional), con números finitos y alguna
  razón mayor que 0; el modo exige la tabla y no admite valores subnacionales de covariables (ni `proxies_crudos`).
  Con `dl_correr(anios = )`, un año sin razones detiene la corrida antes de correr el primero. Una tabla `razones`
  sin el modo es un aviso.
* `dl_repartir_razon()` es el paso nuevo, después de `dl_avd()`; su resultado es la pieza `reparto` de
  `dl_resumir()`. Con el modo `razon`, `dl_cascada()` da la cascada plana. `dl_correr()` hace todos los pasos y
  escribe una sola corrida, ya repartida: `cascada.modo: razon` y el bloque `razon` en el manifiesto (la semilla del
  sorteo, cuántas ubicaciones, el rango de las razones, cuántas son cero y sus fuentes) y `diagnostics/razon.csv`
  (la razón aplicada por ubicación). El nivel nacional, la validación contra el ancla, los factores de
  renormalización (`renorm.csv`), las etiquetas y la sensibilidad son los del ajuste nacional (la cascada plana).
* El reparto se detiene con un error que nombra el sexo y la banda de edad si en una celda las tasas repartidas no
  son finitas (por ejemplo, si las ubicaciones con razón mayor que 0 no tienen población en esa celda), y con otro,
  que nombra las ubicaciones, si una prevalencia repartida pasa de 1.
* La semilla del sorteo de las razones es la de la corrida; otra se fija con
  `avanzado: {cascada: {razon_semilla: <entero>}}`.

## Cambios de comportamiento

* El peso de un dato cuyo intervalo de edad cruza bandas de población con distinto número de edades reparte la
  población de cada banda entre sus edades. El caso típico es un dato de todas las edades, de 0 a 125 años, sobre una
  población por quinquenios con una banda abierta de 80 años y más: antes cada edad llevaba la población entera de su
  banda y, con las edades del modelo hasta los 99 años, la banda abierta pesaba 4 veces de más frente a un quinquenio.
  Cambian los resultados de los proyectos con datos así en el ajuste, y la validación de la mortalidad subnacional si
  sus intervalos cruzan bandas de ese modo. Dentro de una banda, o entre bandas con el mismo número de edades (varios
  quinquenios), los pesos son los de antes bit a bit: el ancla, los datos por banda y el arnés de compatibilidad no
  cambian.
* El ancla puede ir a peso completo (`ancla.peso` 1) aunque la tabla `fuentes_gbd` (en el formato completo, la
  evidencia) traiga fuentes locales no fatales de la causa, si ningún dato local entra al ajuste (`datos_en_ajuste`
  vacío): sin datos en el ajuste no hay nada que contar dos veces. Antes era un error. `dl_insumos()` lo dice con un
  mensaje, que `dl_revisar_proyecto()` muestra como aviso, y la corrida lo declara entre las limitaciones de su
  manifiesto («ancla a peso completo (lambda = 1) con N fuente(s) local(es) no fatal(es) en la evidencia…»), además
  de `params.fuentes_locales_no_fatales`. Con alguna medida en `datos_en_ajuste` sigue siendo un error, que ahora
  nombra las medidas; con mortalidad en el ajuste y causas de muerte en la evidencia, también.
* `dl_correr()` corre la sensibilidad con el `motor` de sus `opciones` y usa sus `nucleos` como procesos, que
  reparten las combinaciones de la grilla; las cadenas siguen siendo las propias de la sensibilidad (200
  simulaciones, 2 cadenas de 6000 iteraciones con 3000 de calentamiento). Antes la corría siempre con el motor
  `"mh"` en un proceso. Con las opciones por defecto (motor `"mh"`, 1 núcleo) nada cambia; con `motor = "rcpp"`,
  `diagnostics/sensibilidad.csv` cambia en las últimas cifras decimales, como el resto de la corrida, y la corrida
  de producción del ejemplo pasa de unos 2.5 minutos a alrededor de 1. Con `rapido = TRUE` todo sigue igual.
* `dl_etiquetas()` hace sus reajustes por rho con adelgazamiento 10, no con el del ajuste: 2 cadenas de 6000
  iteraciones con 3000 de calentamiento guardan 600 simulaciones. Con un ajuste de adelgazamiento 30 quedaban 200,
  pocas para la varianza de cada celda. Las etiquetas de `dl_correr()` y de `dl_etiquetas()` sin `opciones` cambian
  solo si el ajuste usa otro adelgazamiento que 10 (el valor por defecto) y la grilla de rho pide reajustes.

# dismodlite 2.2.0

Un proyecto se corre como se corre de verdad sin recetas propias: varios años con una sola configuración, el año que
todavía no tiene estimación de referencia, la causa que es la suma de sus subtipos, y como claves de la configuración
lo que antes pedía `avanzado`.

## Varios años con una configuración

* `dl_correr(..., anios = 2018:2023)` corre la causa una vez por año, en orden, y vuelve a leer el proyecto para cada
  uno. Devuelve una lista de corridas (clase `dl_corridas`, con los años como nombres: `corridas[["2023"]]`) y el
  nombre de cada corrida lleva el año (`causa-<causa>-<año>`). El proyecto de todos los años se lee antes de correr el
  primero: un año que no se puede leer (el ancla no lo trae ni trae el anterior, falta su población...) detiene
  `dl_correr()` con el año y el problema, sin haber escrito ninguna corrida. Si un año falla al correr, se detiene
  ahí con el error de ese año y dice qué años quedaron escritos. Los dos errores conservan la clase y los campos del
  original (como `problemas`) y llevan además `anio` y `escritas`. Un proyecto leído con tablas dadas como argumentos se vuelve a leer con esas mismas tablas,
  tenga carpeta o no. Sin `anios`, todo como antes.
* `dl_proyecto(..., anio = 2021)` lee el proyecto para otro año que el de su configuración, sin editarla (los
  proxies de `proxies_crudos` se calibran para ese año). Las lecturas de años distintos del mismo proyecto conviven
  en la sesión.
* El año del ancla sale de una sola regla. Sin `ancla.anio`, es el año que se estima si la tabla `ancla` lo trae; si
  no, el año anterior, y el nivel nacional se proyecta desde él: un mensaje lo anuncia («el ancla no trae 2024: se
  proyecta desde 2023»), `dl_revisar_proyecto()` lo muestra como aviso y queda en el manifiesto (la procedencia del
  año del ancla y las limitaciones). La proyección es de un año a lo sumo. Con `ancla.anio` declarado y un año pedido
  con `anio` o `anios`, el año del ancla es el menor de los dos: una configuración con `anio: 2024` y
  `ancla: {anio: 2023}` sirve para 2024 y para los años anteriores, cada uno con su propia ancla (y la procedencia
  del año del ancla lo dice).

## Escribir una corrida que aún no converge

* `dl_correr(..., forzar = TRUE)` escribe la corrida aunque las cadenas no pasen la compuerta de la convergencia: un
  mensaje da su R-hat y su ESS y el manifiesto lo declara (`validacion.gates.force`), como en
  `dl_exportar_corrida()`. No salta la compuerta del ancla. Sirve para mirar una corrida mientras se afinan las
  cadenas; sus números no sirven para publicar.

## Claves nuevas de la configuración

Cuatro claves que antes solo se podían declarar bajo `avanzado`, con los nombres del formato completo:

* `ancla.error_maximo` (un número entre 0 y 1; por defecto, 0.05): el error relativo mediano máximo entre la
  prevalencia ajustada y la del ancla con que la corrida se escribe. Los mensajes de la compuerta del ancla la
  nombran.
* `mortalidad_exceso.fraccion_aguda` (de 0 a menos de 1): la parte de las muertes de la causa que ocurre en la fase
  aguda y no entra al compartimento crónico.
* `sensibilidad.fraccion_aguda`: los valores de la anterior que recorre el análisis de sensibilidad.
* `subtipo_de`: la causa padre de un subtipo que vive en su propia carpeta. Sin betas propias, usa las de esa causa,
  y su corrida declara a la causa padre, que es lo que pide la suma. Si la configuración del padre, en el mismo
  proyecto, también lo declara en `subtipos`, las dos deben decir la misma causa.

```yaml
ancla:
  error_maximo: 0.08
mortalidad_exceso:
  fraccion_aguda: 0.3
subtipo_de: 1010
```

Una clave ausente no cambia nada: los proyectos de la versión 2.1.0 dan la misma configuración. Siguen en `avanzado`
`emr_prior.factor_techo`, `nsub` y `emr_prior.fraccion_aguda.cfr_30d`.

## Una causa que es la suma de sus subtipos

* Claves nuevas `suma_de_subtipos` y `subtipos_omitidos`: la causa padre no se ajusta, se reporta como la suma de las
  corridas de sus subtipos.

  ```yaml
  causa: 1010
  nombre: Enfermedad de ejemplo
  anio: 2023
  subtipos: [1011, 1012, 1013]
  suma_de_subtipos: sí
  subtipos_omitidos:                 # opcional: los que no se modelan, con su motivo
    - {causa: 1013, motivo: sin datos suficientes}
  ```

* El proyecto de una suma solo necesita `ubicaciones` y `poblacion`: no pide `edad_inicio`, ancla, severidad ni
  betas. `print()` dice qué suma, `dl_revisar_proyecto()` revisa su configuración, sus dos tablas y qué subtipos
  tienen configuración en el proyecto, y `dl_insumos()` la rechaza con un mensaje claro (no hay nada que ajustar).
* `dl_correr()` de esa causa busca en la carpeta de las corridas la más reciente de cada subtipo que entra, del año
  que se estima (las de prueba con `rapido = TRUE`, las de producción sin él), y las suma simulación a simulación.
  No pide `semilla`. Los mensajes dicen qué corrida tomó de cada subtipo y cuáles se escribieron con
  `forzar = TRUE`, que en una suma de producción quedan también entre las limitaciones de su manifiesto; si falta
  la de un subtipo, el error dice cuál, de qué año y dónde se buscó. Con `anios`, suma cada año con las corridas de
  ese año.

  ```r
  for (causa in c(1011, 1012)) dl_correr("mi_proyecto", causa = causa, semilla = 1)
  dl_correr("mi_proyecto", causa = 1010)
  ```

* Límites: una suma no puede ser subtipo de otra suma; y entre corridas de un subtipo escritas el mismo día con
  nombres distintos, la más reciente es la que se escribió última (por la fecha de su `manifest.yaml`: copiar la
  carpeta de las corridas sin conservar las fechas cambia ese desempate).
* `dl_sumar_hijas()` no cambia: queda para sumar corridas elegidas a mano.

## Covariables y ancla

* `valor_nacional_de` (tabla `betas`): la beta que lo declara ya no necesita la fila nacional de su propia
  covariable, copiada con otro nombre; basta la de la covariable que nombra. La exención vale para una covariable
  con valores subnacionales (en `covariables` o en `proxies_crudos`). Con `proxies_crudos`, la calibración cierra en
  el valor nacional de la covariable nombrada: `dl_proyecto()` lo toma de `betas`, `dl_calibrar_proxies()` tiene el
  argumento `valor_nacional_de`, y el manifiesto lo registra en `params.proxies`, solo en la covariable que toma
  su valor nacional de otra. Un proyecto que trae la fila copiada sigue dando lo mismo.
* Las filas de ambos sexos del ancla, que el modelo no usa, ya no piden `poblacion_detalle` de `ambos` cuando el
  ancla trae bandas más finas que la población: sin ese detalle, se dejan fuera.

## Cambios de comportamiento

* Un ancla que no trae el año que se estima ya no es un error si trae el año anterior: se proyecta desde él y se
  anuncia (arriba). Antes había que declarar `ancla: {anio: ...}`. Si no trae el año ni el anterior, sigue siendo
  un error, que dice qué años trae.
* Un subtipo declarado en `subtipos` por dos configuraciones del mismo proyecto es ahora un error: un subtipo tiene
  una sola causa padre.

## Revisión del proyecto

* `dl_revisar_proyecto()` avisa (`!`), en el paso de la configuración, cuando una clave y su equivalente del formato
  completo bajo `avanzado` están las dos declaradas con valores distintos (por ejemplo `ancla: {error_maximo: 0.1}`
  y `avanzado: {anchor: {gate_err_mediano: {valor: 0.2, ...}}}`): nombra las dos y dice que rige la de `avanzado`.
  Vale para las claves de números (`ancla.error_maximo`, `mortalidad_exceso.fraccion_aguda`,
  `sensibilidad.fraccion_aguda`, `subtipo_de`, `ancla.peso`, `remision`...). La lectura no cambia.

## Compatibilidad

* Un proyecto que corría con la versión 2.1.0 da los mismos insumos, las mismas salidas y los mismos manifiestos. El
  arnés de compatibilidad sigue reproduciendo la versión 0.2.2 bit a bit.
* Los argumentos nuevos (`anio`, `anios`, `forzar`, `valor_nacional_de`) van al final de las firmas.

## Guías

* «Corridas, versiones y tablas consolidadas»: varios años con `anios`, la regla del año del ancla y la proyección,
  `forzar` y cómo correr todas las causas de un proyecto, con las que son la suma de sus subtipos al final.
* «Subtipos y suma»: el flujo con `dl_correr()` para los subtipos y para la suma (`suma_de_subtipos`,
  `subtipos_omitidos`), con `dl_sumar_hijas()` como la forma manual.
* «Preparar tus datos»: las claves nuevas, la configuración de una suma, `valor_nacional_de` sin fila copiada y las
  filas de ambos sexos del ancla.

# dismodlite 2.1.0

Los valores subnacionales de una covariable pueden venir de una encuesta: el paquete los calibra al leer el proyecto.

## De la encuesta a la covariable

* Tabla nueva del contrato, `proxies_crudos` (la décima): un indicador de encuesta, u otra fuente con ediciones, ya
  agregado por ubicación subnacional y edición, con su error estándar (`ubicacion`, `anio`, `sexo`, edades,
  `covariable`, `indicador`, `valor`, `error_estandar` y `fuente`). Una fila sin edades es de todas las edades de la
  población, empiece donde empiece.
* `dl_calibrar_proxies()` convierte esos valores en las filas subnacionales de `covariables` del año que se estima:
  - el gradiente de cada ubicación en cada edición, frente al promedio de la edición ponderado por la población, en
    `cociente` (log de la razón) o en `diferencia` (puntos);
  - el gradiente del año: un paseo aleatorio observado con error, suavizado con todas las ediciones (filtro de Kalman
    y suavizador RTS), con su varianza por año q estimada por máxima verosimilitud para cada covariable; o la edición
    del año o la más cercana (`metodo = "edicion"`);
  - el cierre exacto en el valor nacional: el promedio ponderado por la población de las filas calibradas es el valor
    nacional de `covariables`.
  Devuelve también la calibración de cada covariable (método, transformación, q, ediciones usadas y excluidas, años
  de la población), las series observadas y suavizadas y las ediciones excluidas. Con `ubicacion_gbd` lee
  `covariables` desde una carpeta de descargas del GHDx con varias ubicaciones (como en `dl_tabla()`).
* `dl_proyecto()` y `dl_configuracion()` calibran `proxies_crudos` al leer el proyecto, para el año que se estima y
  con el valor nacional del año del ancla. Claves nuevas de la configuración: `proxies.metodo`,
  `proxies.transformacion` (por covariable; por defecto, `cociente`) y `proxies.excluir` (ediciones que no entran,
  cada una con su motivo). Las filas subnacionales de cada covariable vienen de una sola tabla: si vienen de
  `covariables` y de `proxies_crudos`, es un error.
* `print()` del proyecto y `dl_revisar_proyecto()` (paso «proxies») muestran, por covariable, el método, q y las
  ediciones, y dicen si q quedó en un borde de su intervalo de búsqueda (relativo al error de las ediciones, así que
  no depende de las unidades del indicador).
* `dl_correr()` congela `proxies_crudos` en `inputs/contrato/` (repetir la corrida desde ahí vuelve a calibrar
  igual), escribe las series en `diagnostics/proxies_series.csv` y registra la calibración en `params.proxies` del
  manifiesto.
* `dl_nuevo_proyecto()` escribe también la plantilla `proxies_crudos.csv`.
* Los proxies de un proyecto son los del año con que se tradujo: si el año de su configuración cambia después
  (`p$configuracion$years$ajuste`), `dl_insumos()` se detiene con un error que pide volver a llamar a
  `dl_proyecto()` con ese año (antes, la tabla `cov_proxy` quedaba vacía y la cascada fallaba más adelante).

## El ejemplo

* El proyecto de ejemplo (`dl_ejemplo()`) trae `proxies_crudos.csv`, una encuesta sintética con tres ediciones
  (2019, 2021 y 2023), en lugar de `covariables/proxies.csv`; su configuración declara el HAQ en `diferencia`. Sus
  proxies departamentales son ahora los de la calibración, así que las estimaciones departamentales del ejemplo
  cambian un poco. `acs_peru_completo` no cambia.
* `dl_rutas_ejemplo(anio = 2019)` y `dl_rutas_ejemplo(anio = 2024)` traen los proxies de ese año (en la versión
  2.0.0, la tabla `cov_proxy` del formato simple quedaba vacía en esos años).

## Compatibilidad

* Un proyecto sin `proxies_crudos` da exactamente los mismos insumos que con la versión 2.0.0. El arnés de
  compatibilidad sigue reproduciendo la versión 0.2.2 bit a bit (sus escenarios del contrato leen el ejemplo con
  los proxies ya calibrados de `acs_peru_completo`).

## Guías

* «Estimación subnacional» explica, en la sección «De la encuesta a la covariable», el gradiente, el paseo
  aleatorio, cómo leer q, el cierre, cuándo usar `cociente` o `diferencia` y cómo queda registrada la calibración.
* «Preparar tus datos» describe las diez tablas, con `proxies_crudos` y la regla de una sola fuente por covariable.

# dismodlite 2.0.0

Los insumos pasan a ser un contrato público y corto: unas pocas tablas con un eje común, que se preparan igual vengan
de donde vengan los datos. Es una versión mayor porque los proyectos de la versión 1.0.0 se rehacen (abajo).

## El contrato de insumos

* Nueve tablas como máximo: `ubicaciones`, `poblacion` y `ancla` (obligatorias), `covariables`, `betas`, `datos`,
  `severidad`, `fuentes_gbd` y `poblacion_detalle`. `?dl_tablas` documenta cada una, con sus columnas, unidades y
  valores, desde una sola definición (`inst/referencia/tablas.csv`) que usan también el validador, las plantillas y
  los mensajes.
* Todas comparten un **eje**: `causa`, `ubicacion`, `anio`, `sexo`, `edad_inicio` y `edad_fin`. Una columna del eje
  que no viene significa que la tabla no varía en esa dimensión (sin `ubicacion`, la nacional; sin `sexo`, ambos;
  sin edades, todas...). Las unidades son fijas: proporciones de 0 a 1 y tasas por persona-año.
* Las ubicaciones se declaran una vez en `ubicaciones`, con códigos propios: la nacional sin `padre` y las
  subnacionales con la nacional de padre. Salen `location_id`, `age_group_id`, `sex_id` y los ids inventados.
* Una sola tabla `covariables` para el valor nacional de cada covariable y sus valores subnacionales (los proxies).
  Las betas pasan de la configuración a la tabla `betas`; un subtipo sin filas propias usa las de su causa padre (la
  que lo declara en `subtipos` o, en su propia carpeta, la de `avanzado: extraction`).
* Las descargas de GBD Results y del GHDx se reconocen por sus columnas y se convierten solas (el *Rate* entre
  100 000, las edades de GBD a bandas, `measure_id` a `medida`). La clave `ubicacion_gbd` (antes
  `ubicacion_nacional`) dice qué `location_id` de GBD es el país; por defecto, el código nacional de `ubicaciones`
  si está en la descarga, o la única ubicación de la descarga (la misma regla en los tres lectores). La lista de
  fuentes del GHDx queda con el código nacional del proyecto, con cada fuente una vez (la lista del GHDx la repite en
  varias filas); sus fuentes de otras ubicaciones de GBD se descartan, con un aviso.
* La configuración guarda decisiones y las tablas, números con su fuente. Entran `severidad.particion`,
  `severidad.padre` y `componente.secuelas`, que antes solo existían en el formato completo.
* La corrida congela las tablas del contrato que usó y la configuración del proyecto tal como se leyó
  (`inputs/contrato/<tabla>.csv` y `inputs/contrato/config.yaml`) con su sha256 en el manifiesto: `inputs/contrato/`
  es la carpeta de un proyecto, y una corrida hecha con tablas o una configuración de R se repite sin ellas. La de un
  subtipo con las betas de su causa padre las congela a su nombre, con la causa padre en `avanzado: extraction`. La
  carpeta de `severidad.particion` se congela en `inputs/contrato/particion/<corrida>/`, con la ruta de la
  configuración congelada cambiada a ella (la tabla `severidad`, que sale de ella, no se congela aparte).
* La severidad que sale de la partición de una hija o de un componente puede tener límites mayores que 1 (los de la
  partición divididos por su cuota): se acepta con un aviso que nombra los estados, como en la versión 0.2.2. La tabla
  `severidad` escrita por el usuario sigue en $[0, 1]$.
* Los proxies de `covariables` pueden venir en bandas de edad propias, uniones de las de la población (por ejemplo
  45-59 con bandas de 5 años).
* Las bandas de la población van seguidas: un hueco entre ellas es un problema de la población. En `datos`,
  `anio_inicio` mayor que `anio_fin` es un problema de la tabla. Las filas de los mensajes se cuentan como las líneas
  del CSV (el encabezado es la 1) o, en un `data.frame`, por su número de fila.

## Dos puertas, una función

* `dl_proyecto()` recibe las tablas de la carpeta del proyecto (nombres fijos: `poblacion.csv`, `ancla/`...) o como
  argumentos, un `data.frame` o la ruta de un CSV o de una carpeta: `dl_proyecto(configuracion = list(...),
  ubicaciones = ..., poblacion = ..., ancla = ...)`. Las dos puertas se mezclan (lo que se pasa reemplaza a la tabla
  de la carpeta) y pasan por el mismo validador. `dl_proyecto()` se detiene con el problema de una tabla sola o, si
  todas están bien, con todos los problemas entre tablas juntos (una sola ubicación nacional, ubicaciones conocidas,
  población y ancla que cubren el modelo...), y avisa de sus sospechas.
* `dl_tabla()` lee, convierte y valida una tabla suelta; `dl_plantilla()` da la plantilla de una tabla (sus columnas
  y una fila de ejemplo). `dl_nuevo_proyecto()` crea la carpeta con las plantillas de todas.
* `dl_revisar_proyecto()` revisa cada tabla, la configuración, las reglas que cruzan tablas y los insumos, y acepta
  también un proyecto ya leído con `dl_proyecto()`. Cada mensaje dice la tabla, su origen (el archivo o el argumento),
  la columna, las filas y cómo corregirlo.

## Bandas de edad del ancla

* El ancla usa sus bandas de edad tal como vienen cuando la población tiene ese detalle: cada banda (80-84, 85-89,
  90-94, 95 y más...) es un término del ajuste y una celda de los resultados, también en los AVD, las etiquetas y el
  consolidado. La versión 1.0.0 agrupaba siempre el ancla de 80 años y más (y la de menos de 5) en una sola banda.
* **Los resultados de un proyecto rehecho cambian en las edades de 80 años y más**: hay una celda por banda del ancla
  y el ajuste usa la información de cada una. Para reproducir la agrupación de la 1.0.0, la configuración declara
  `avanzado: {anchor: {agrupar_bandas_finas: true}}`.
* Si la población es más gruesa que el ancla (por ejemplo, solo 80 y más, o los menores de 5 en una banda), las
  bandas del ancla se agrupan en las de la población con los pesos de la tabla nueva `poblacion_detalle` (la
  población nacional con más detalle de edad). Reemplaza a `pesos_80mas.csv`.
* `ancla.correlacion_edad` (ρ) cuenta la distancia en bandas: con más bandas en las edades altas, la misma ρ las
  correlaciona menos.

## Catálogos de GBD en el paquete

* Los catálogos de GBD 2023 de estados de salud (con sus pesos de discapacidad) y de secuelas vienen en
  `inst/referencia/`, con su cita a IHME y sus términos de uso. En la tabla `severidad`, un estado de salud de GBD
  (por nombre o id) toma sus pesos del catálogo; un estado propio trae los tres pesos. En cualquier estado van los tres
  o ninguno.

## Los proyectos 1.0.0 se rehacen

* El formato simple de la versión 1.0.0 (con `proxies.csv`, `pesos_80mas.csv`, `location_id` en las tablas y
  `covariables:` en la configuración) no se lee: esos proyectos se rehacen en el contrato. La revisión de la
  configuración señala las claves que ya no existen (por ejemplo, `ubicacion_nacional` ahora es `ubicacion_gbd`), y
  la guía «Preparar tus datos» explica cada tabla.
* Los proyectos de la versión 0.2.2 (el formato completo, con `schema: dismod_lite/v1`) se siguen leyendo igual y
  dan los mismos números; salen de las guías y quedan documentados en `?dl_proyecto` y `?dl_configuracion`.
* El proyecto de ejemplo (`dl_ejemplo()`) está en el contrato.

## Corrección: la prevalencia del ancla se lee en *Rate*

* El paquete leía la prevalencia de las estimaciones de GBD en la métrica *Percent*, como si fuera la proporción de la
  población. No lo es: GBD Results calcula ese *Percent* sobre las personas con alguna causa (la prevalencia de todas
  las causas), no sobre la población, y supera a *Rate* / 100 000 en la inversa de esa prevalencia. En el país del
  ejemplo, en 2023, la diferencia es nula desde los 75 años, menor que 1 % entre los 20 y los 60, de 1,5 % a 6 % entre
  los 2 y los 19 y de 22 % a 37 % antes de los 2. En las causas cardiovasculares, el total de todas las edades baja
  entre 0,02 % y 2,4 %. Ahora la prevalencia se lee en *Rate* / 100 000, como las demás medidas.
* Las descargas de `ancla/` deben traer la prevalencia en *Rate*; si solo la traen en *Percent*, el mensaje lo dice.
* `anchor.metrica_prevalencia: {valor: Percent, procedencia}` (en un proyecto, dentro de `avanzado:`) repite
  la lectura anterior, para reproducir corridas hechas hasta la versión 1.0.0.
* Los datos de ejemplo traen la prevalencia en las dos métricas.

# dismodlite 1.0.0

Primera versión como paquete independiente y en español. Los proyectos y los scripts de la versión 0.2.2 siguen
funcionando y, con los mismos insumos, la misma semilla y las mismas opciones, dan los mismos números.

## Formato simple de proyecto

* Un proyecto es una carpeta con una configuración corta (`config.yaml`, o `config/<causa>.yaml` con varias
  causas), las descargas de GBD Results (`ancla/`) y del GHDx (`covariables/`) tal como se descargan, y unas pocas
  tablas planas: `poblacion.csv`, `severidad.csv` y, si hacen falta, `proxies.csv`, `datos.csv` y
  `pesos_80mas.csv`. `?dl_proyecto` documenta cada archivo y cada columna, con sus unidades; `?dl_configuracion`,
  cada clave, su símbolo en el modelo y su valor por defecto.
* Lo mínimo de la configuración son tres claves: `causa`, `anio` y `edad_inicio`. Las demás tienen un valor por
  defecto, que `print()` del proyecto muestra y el manifiesto de cada corrida registra
  (`configuracion.por_defecto`). Las claves tienen nombres descriptivos, con el símbolo del modelo en la ayuda:
  `ancla.peso` (λ), `ancla.correlacion_edad` (ρ), `incidencia.suavidad` (σ), `subnacional.kappa` (κ)...
* El bloque `avanzado:` pasa claves del formato completo tal cual, para las opciones que no tienen clave simple.
* `dl_proyecto()` lee la carpeta y la traduce al formato completo que usa el resto del paquete; la traducción solo
  renombra columnas y agrega constantes, así que los números medidos pasan tal cual. `dl_insumos(dl_proyecto(...))`
  es el primer paso de una corrida, y con un proyecto simple los mensajes citan sus claves y sus archivos.
* Datos locales: entran al ajuste solo los tipos que declara `datos_en_ajuste` (por defecto ninguno: validan). Con
  datos en el ajuste y el ancla a peso completo, un aviso recuerda el posible doble conteo.
* Los proyectos de la versión 0.2.2 (el formato completo, con `schema: dismod_lite/v1`) se leen igual; es el
  formato de las opciones avanzadas (componentes de una causa, severidad desde una partición, tablas
  consolidadas).

## Ubicaciones de cualquier país

* El país es el de `ubicacion_nacional` (el `location_id` de GBD) y las ubicaciones subnacionales son las de
  `poblacion.csv`, con códigos libres: departamentos, regiones, provincias... El bloque de la configuración se llama
  `subnacional` (`departamentos`, su nombre anterior, sigue valiendo).
* Si `poblacion.csv` no trae las filas nacionales, el total nacional es la suma exacta de las subnacionales; los
  pesos de las bandas de 80 años y más salen de la población.
* El formato completo conserva las reglas de la versión 0.2.2 para las ubicaciones.

## Herramientas del proyecto

* `dl_nuevo_proyecto()` crea la carpeta de un proyecto con la configuración comentada (cada clave con su
  explicación y su valor por defecto), las plantillas de las tablas y un `LEEME.md` con los pasos. Nunca
  sobrescribe.
* `dl_revisar_proyecto()` revisa la configuración, cada archivo y los insumos completos sin detenerse en el primer
  problema, y dice cómo corregir cada uno.
* `dl_correr()` hace la corrida completa en una llamada (insumos, ajuste, estimación subnacional, validación, AVD,
  etiquetas, sensibilidad, resumen y carpeta de la corrida), con las cadenas de producción por defecto; con
  `rapido = TRUE`, una prueba corta que no sirve para publicar; con `registro`, la corrida queda además en el
  registro de corridas.

## Funciones y argumentos en español

* Todas las funciones y sus argumentos tienen nombres en español: `dl_ajustar()`, `dl_cascada()`, `dl_avd()`,
  `semilla`, `simulaciones`, `calentamiento`... Los nombres de la versión 0.2.2 siguen exportados, con sus
  argumentos, y dan el mismo resultado (ver `?dl_nombres_anteriores`). Los objetos, sus clases y sus campos no
  cambian.
* Nuevas: `dl_estimaciones()` (media e intervalo por edad, sexo y ubicación de un ajuste, una cascada o los AVD),
  `dl_ejemplo()` (con `copiar_en`, una copia del proyecto de ejemplo para editar), `dl_rutas_ejemplo()` y
  `dl_configuracion_ejemplo()`.
* Los mensajes están en español, nombran la función que se llamó y dicen el archivo, la columna y la corrección.
  Un CSV guardado desde Excel con separador `;`, coma decimal o códigos sin el cero inicial se reconoce y se explica.
* Datos de ejemplo sintéticos: una enfermedad ficticia con tres subtipos, con la geografía del Perú como ejemplo,
  en los dos formatos (`acs_peru` y `acs_peru_completo`), con las curvas verdaderas en `verdad.csv`.

## Tablas consolidadas con nombres neutros

* La función del consolidado se llama `dl_consolidar()` (su nombre anterior sigue funcionando). Escribe en
  `mod/consolidado/<id>/` la tabla canónica (`canonico/`), una tabla por medida con la forma de un perfil de
  columnas (`tablas/`) y el manifiesto, con las limitaciones armadas a partir de lo que se consolidó.
* Los perfiles de columnas vienen con el paquete, en `system.file("perfiles", package = "dismodlite")`, con las
  mismas columnas que en la versión 0.2.2.
* Una corrida se anota en el registro de corridas solo si se da el registro, y ese archivo debe existir.

## Paquete independiente

* El paquete ya no necesita el repositorio en el que se desarrolló: los esquemas, los perfiles y el núcleo en C++
  vienen con él, y la variable de entorno que apuntaba a ese repositorio ya no se usa. Del entorno solo se leen
  `DATA_ROOT` y `CATALOGOS_DIR`, como respaldo de las rutas.
* Funciona en rutas con espacios y tildes (por ejemplo `C:/Users/Ana María/Mis análisis/`) y en una sesión con
  locale C.

## Compatibilidad con la versión 0.2.2

* Las pruebas del paquete corren doce escenarios con la API nueva (cinco de ellos también con el proyecto de
  ejemplo en el formato simple) y comprueban que dan los mismos números que la versión 0.2.2, bit a bit en la misma
  máquina.
* Diferencias de comportamiento, todas para entradas que antes pasaban sin aviso: una ruta opcional que se da y no
  existe es un error; una tabla de severidad de otra causa detiene `dl_insumos()`; los datos locales con valores
  imposibles (una prevalencia fuera de 0 a 1, casos que superan la muestra, un error estándar no positivo) o un dato
  excluido con el motivo vacío lo detienen también. `?dl_nombres_anteriores` lista las diferencias de los nombres
  anteriores.

## Motor en C++

* `motor = "rcpp"` (en `dl_opciones_mcmc()`) sigue siendo opcional y evalúa la log-posterior varias veces más
  rápido; `"mh"`, en R, es el valor por defecto y la referencia, y el motor no se elige solo según lo que haya
  instalado (`?dl_opciones_mcmc` explica por qué). Sin Rcpp o sin compilador, un mensaje dice qué instalar o que se
  use `motor = "mh"`.

## Documentación

* Ayuda en español para cada función, con ejemplos que corren sobre el proyecto de ejemplo, y guías que explican
  el modelo, cómo preparar los datos y cada etapa de una corrida.
* Las guías están escritas en Quarto y el sitio tiene un diseño propio, con modo claro y oscuro.
* Logo del paquete: la prevalencia por edad del proyecto de ejemplo, con la curva nacional y las 25
  subnacionales (`data-raw/logo.R`).
