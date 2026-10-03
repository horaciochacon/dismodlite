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
  de la población), las series observadas y suavizadas y las ediciones excluidas.
* `dl_proyecto()` y `dl_configuracion()` calibran `proxies_crudos` al leer el proyecto, para el año que se estima y
  con el valor nacional del año del ancla. Claves nuevas de la configuración: `proxies.metodo`,
  `proxies.transformacion` (por covariable; por defecto, `cociente`) y `proxies.excluir` (ediciones que no entran,
  cada una con su motivo). Las filas subnacionales de cada covariable vienen de una sola tabla: si vienen de
  `covariables` y de `proxies_crudos`, es un error.
* `print()` del proyecto y `dl_revisar_proyecto()` (paso «proxies») muestran, por covariable, el método, q y las
  ediciones, y dicen si q quedó en un borde de su intervalo de búsqueda.
* `dl_correr()` congela `proxies_crudos` en `inputs/contrato/` (repetir la corrida desde ahí vuelve a calibrar
  igual), escribe las series en `diagnostics/proxies_series.csv` y registra la calibración en `params.proxies` del
  manifiesto.
* `dl_nuevo_proyecto()` escribe también la plantilla `proxies_crudos.csv`.

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
