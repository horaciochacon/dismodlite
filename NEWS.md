# dismodlite (en desarrollo)

## Corrección: la prevalencia del ancla se lee en *Rate*

* El paquete leía la prevalencia de las estimaciones de GBD en la métrica *Percent*, como si fuera la proporción de la
  población. No lo es: GBD Results calcula ese *Percent* sobre las personas con alguna causa (la prevalencia de todas
  las causas), no sobre la población, y supera a *Rate* / 100 000 en la inversa de esa prevalencia. En Perú 2023 la
  diferencia es nula desde los 75 años, menor que 1 % entre los 20 y los 60, de 1,5 % a 6 % entre los 2 y los 19 y de
  22 % a 37 % antes de los 2. En las causas cardiovasculares, el total de todas las edades baja entre 0,02 % y 2,4 %.
  Ahora la prevalencia se lee en *Rate* / 100 000, como las demás medidas.
* Las descargas de `ancla/` deben traer la prevalencia en *Rate*; si solo la traen en *Percent*, el mensaje lo dice.
* `anchor.metrica_prevalencia: {valor: Percent, procedencia}` (en el formato simple, dentro de `avanzado:`) repite
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
