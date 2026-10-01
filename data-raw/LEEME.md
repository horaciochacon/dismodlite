# data-raw: cómo se generan y verifican los datos de ejemplo

Carpeta de desarrollo (excluida del paquete por `.Rbuildignore`). Todo se ejecuta desde la raíz del repositorio.

- `generar_acs_peru.R`: genera los datos de ejemplo (100 % sintéticos) en sus dos formatos, con los mismos números:
  `inst/extdata/acs_peru/` (el formato simple: el proyecto que se copia para empezar, ver `?dl_proyecto`) e
  `inst/extdata/acs_peru_completo/` (el formato completo de la versión 0.2.2, el que usan el arnés de
  compatibilidad y las guías avanzadas); también las tablas de referencia del paquete (`inst/referencia/`: grupos de
  edad de GBD y etiquetas en español, las del catálogo del formato completo). Lo que el formato simple calcula al
  leer el proyecto (las filas nacionales de la población, suma de las departamentales, y los pesos de 80+, cuotas de
  la población nacional) sale en el formato completo de las mismas cuentas y del mismo escritor (`fwrite`), así que
  la traducción del simple da exactamente los mismos números (`tests/testthat/test-formato-simple.R` lo comprueba).
  `Rscript data-raw/generar_acs_peru.R`
- `legado.R`: lo común a los dos scripts siguientes: `cargar_legado()` carga el código de la etiqueta `v0.2.2` desde
  un worktree temporal de git y `cargar_arnes()` carga el arnés de compatibilidad (`tests/testthat/helper-arnes-*.R`),
  con `rutas_acs(api, raiz, causa, ...)`, las rutas del conjunto con las claves de `dl_paths()` (`causa` es
  obligatoria, restricción 1).
- `verificar_acs_legado.R`: con el código de la etiqueta `v0.2.2` arma el bundle de cada causa (9100-9103) en cada
  variante (nacional, datos y proxies, cascada plana, datos locales, componente, severidad desde la partición, 2019 y
  proyección 2024), comprueba que la variante se aplicó y revisa la plausibilidad.
  `Rscript data-raw/verificar_acs_legado.R`
- `referencia_legado.R`: con el código de la etiqueta `v0.2.2` corre los escenarios E1-E12 (E12 solo con Rcpp) del
  arnés de compatibilidad y escribe sus referencias en `tests/testthat/_referencia/` (sección «Arnés de
  compatibilidad»).
  `Rscript data-raw/referencia_legado.R [--salida <carpeta>] [--una-vez] [--escenarios E1,E5]`
- `logo.R`: dibuja el logo del paquete (`man/figures/logo.png` y `man/figures/logo.svg`) con una figura real del
  modelo: la prevalencia por edad del proyecto de ejemplo (curva nacional, su incertidumbre y las 25 curvas
  departamentales de la cascada). Usa pkgload, ragg, showtext y sysfonts, que no van en DESCRIPTION, y descarga la
  letra (IBM Plex Sans) de Google Fonts.
  `Rscript data-raw/logo.R`

## Reproducibilidad

Semilla `20260929` (un bloque por componente: `+1` proxies, `+2` ruido del ancla, `+3` datos locales), sin fechas ni
rutas dentro de los archivos, CSV con `data.table::fwrite(eol = "\n")` y texto en UTF-8 con `\n`. Dos corridas dan
archivos idénticos byte a byte (comprobado con `git status --porcelain` tras una segunda corrida). La EDO de la verdad
se resuelve con `dl_edo_resolver` (antes `dl_ode_solve`) del paquete: el código del árbol cargado con `pkgload` (o,
sin `pkgload`, el paquete instalado; mismas cuentas por la invariancia de coma flotante).

## Modelo de verdad

- Subtipo k, sexo s, año y, edad a: `i_k(a) = I_k exp(g_k (a - 60)) M_k^[s = 1] T_y`, `f_k(a) = F_k exp(h_k (a - 60))`,
  remisión 0, `p(30) = 0`; RK4 con `nsub = 5` sobre la malla h/2 de 30 a 99 años.
- Constantes: 9101 `I = 0.0016, g = 0.045, F = 0.020, h = 0.030, M = 1.20`; 9102 `I = 0.0006, g = 0.050, F = 0.030,
  h = 0.035, M = 1.30`; 9103 `I = 0.0003, g = 0.040, F = 0.050, h = 0.030, M = 1.10`; `T_2019 = 0.97`, `T_2023 = 1`.
  **Ajuste:** con `I = 0.0040, 0.0015, 0.0008` la prevalencia del padre a los 70-74 años era 17 % (hombres) y 14 %
  (mujeres), fuera del rango plausible 2-10 %; se multiplicaron por 0.4 (queda 6.8 % y 5.7 %).
- Padre 9100 = suma de subtipos: `p = sum p_k`, `i = sum i_k (1 - p_k) / (1 - p)`, `f = sum p_k f_k / p` (a los 30
  años, donde `p = 0`, `f` es el límite `sum i_k f_k / sum i_k`). Su ancla es la suma de las anclas de los subtipos.
- Bandas: promedio de la verdad anual ponderado con la población de la banda fina de cada edad (regla del paquete).
  Ancla = verdad x `exp(N(0, 0.05))`, intervalo `x exp(-/+ 1.96 x 0.15)`. AVD = prevalencia x `sum pi DW` x
  `COMO(a)`, `COMO(a) = 1 - 0.004 (a - 30)`; el AVD del ancla repite el error de la prevalencia de su celda más un
  término propio `N(0, 0.01)`, así el factor COMO empírico (`dl_como_factor`) recupera la curva: baja de ~0.95 a los
  40-44 años a ~0.77 en 80+ y queda por debajo de 1 en las cuatro causas. Con errores independientes (0.05 cada uno)
  el factor salía dentado y pasaba de 1 en alguna celda.
- Departamentos: `log i_d = log i + 0.62 dX_SEV,d(a) - 0.21 dX_LDI,d`, `log f_d = log f - 0.012 dX_HAQ,d`, con el dX
  del SEV por banda llevado a la malla como la cascada por defecto (lineal entre puntos medios, 0 antes de 40 años).
- Población: 15 millones de 30 años y más en 2023, 2019 = 2023 x 0.95, 2024 = 2023 x 1.01; cuotas departamentales
  aproximadas (Lima un tercio); nacional = suma exacta de los departamentos. Estructura por edad: razón 0.86 entre
  bandas quinquenales de 30-34 a 75-79 y, desde los 80, razones 0.70, 0.65, 0.55 y 0.40 (80+ ~6.5 % de los 30 y más;
  `pesos_80mas` ~0.47/0.30/0.16/0.06 en hombres). Con la razón 0.86 hasta 95+ el 80+ era 11 % y el 95+ un 19 % del
  80+. Hombres: 0.49 hasta 55-59 y luego baja hasta 0.40 en 95+ (48 % en conjunto). Cada departamento inclina la
  estructura nacional (multiplicador `exp(inclinación x t)`, `t` de -1 en 30-34 a +1 en 95+): costa urbana más
  envejecida (Lima, Callao, Arequipa, Moquegua, Tacna), Amazonía más joven (Loreto, Ucayali, Madre de Dios, San
  Martín, Amazonas); el 80+ va de ~4.8 % a ~7.2 % de los 30 y más según el departamento.
- Proxies: valor nacional x efecto departamental (el del SEV crece con la edad), desplazados para que el promedio
  ponderado por población cierre exactamente en el valor nacional; 2024 se ancla en el valor nacional de 2023. El
  efecto sigue un índice de desarrollo sintético (orden aproximado de los departamentos, correlación 0.9 con LDI, 0.85
  con HAQ y 0.5 con SEV) más ruido propio, para que Lima, Callao, Arequipa, Moquegua y Tacna queden arriba en LDI y
  HAQ y Huancavelica, Apurímac o Cajamarca abajo. No describe a los departamentos reales.
- Covariables nacionales (también Global y la región 120): valores inventados; los tres CSV llevan
  `acquisition_id = sintetico_cov_v1` (el paquete lee solo las columnas que necesita).
- Datos locales (`datos.csv`, solo 9100): csmr nacional del registro vital, estudio de prevalencia con conteos,
  cohorte de incidencia y un atípico (2023); held-out departamental de 2019 en 10 departamentos: prevalencia (conteos,
  n = 600) y csmr del registro vital (50-54 ... 75-79, mismo ruido y completitud que el nacional, `acquisition_id =
  sintetico_rv_dep_v1`). Definición de caso genérica (`tamizaje vascular estandarizado`), válida para los tres
  subtipos.

## Restricciones descubiertas al leer los datos con la versión 0.2.2

1. **Severidad, un archivo por causa** (`severidad/<causa>.csv`). El paquete usa todas las filas del archivo: `dl_yld`
   renormaliza las proporciones de todas las filas y `dl_como_factor` suma `pi x DW` de todas. Tampoco compara el
   `cause_id` de la tabla con el del config: un bundle de 9101 armado con la tabla de 9100 se arma sin error ni aviso
   y da AVD y factor COMO con la mezcla del padre. Por eso `rutas_acs()` exige `causa` (sin valor por defecto)
   y el verificador comprueba `all(b$severidad$cause_id == b$cfg$cause_id)` en cada bundle; el arnés y
   `dl_rutas_ejemplo()` deben hacer lo mismo. `anio` solo se valida: los demás archivos traen todos sus años y causas
   y el paquete filtra.
2. **`registro/sequela_rei.csv` sin `rei_id` ni `rei_id_severidad`.** Ninguna secuela de la ACS tiene deterioro; una
   columna vacía en todas las filas se lee como lógica y `dl_bundle` la rechaza (`sequela_map.rei_id: tipo esperado
   int`). Son columnas opcionales del contrato y pueden faltar enteras. Por lo mismo
   `registro/modelos_proporcion_deterioro.csv` lleva solo la cabecera: `dl_insumos()` ya no lo lee (solo sirve para
   validar tablas de atribución de deterioros, que la ACS no tiene), pero el `dl_bundle` de v0.2.2 lo exige y el
   verificador corre esa versión sobre estos mismos datos.
3. **Población con la banda 80+ (21) además de las finas** (30, 31, 32, 235). Los conteos de las tablas consolidadas
   buscan la población de cada celda de salida (40-44 ... 75-79 y 80+). El paquete descarta 21 como banda agregada al
   ponderar edades, salvo en un lugar: el peso de cada departamento en `amplitud_csmr` (`.dl_amplitud_csmr`, el
   promedio nacional de las observaciones departamentales) suma todas las bandas, así que cuenta dos veces el 80+.
   Con la inclinación etaria ese peso ya no es proporcional a la población de 30 y más; es el comportamiento de la
   versión 0.2.2 y el arnés lo reproduce tal cual.
4. **`pesos_80mas.csv` = cuotas de la población nacional de 2023** en 80-84, 85-89, 90-94 y 95+, por sexo: así el
   ancla de 80+ (agregada con esos pesos) coincide con el 80+ que calcula el modelo con la población.
5. **Partición de severidad:** `cause_sequela` trae las secuelas de los subtipos como parte de la prevalencia del padre
   (filas de 9100, para `severidad.padre`) y como parte de la del propio subtipo (filas de 9101-9103, para
   `anchor.componente`); `cause_health_state` trae las cuatro causas. Así `severidad.fuente: mod` funciona en todas.
6. **Evidencia:** una fila de registro vital (componente 4) en 123 por causa: con csmr en `medidas_entrada` exige
   `lambda < 1`. La cohorte global (componente 5) va en la ubicación 1 y solo en `rows.csv`; en 123 obligaría a
   `lambda < 1` siempre.
7. **`master_gbd.csv` declara 9100 como suma de 9101-9103** (lo exige `dl_sum_hijas`). En consecuencia la
   consolidación rechaza un ajuste nacional de 9100 como corrida vigente de (9100, año) (`dl_export_seleccionar`:
   «se reporta como suma_de_hijas ... y el run vigente ... es un fit nacional»). Para consolidar, el registro debe
   tener como corrida vigente de 9100 una suma: por ejemplo, consolidar solo 2023 con la suma de subtipos como última
   corrida de 9100/2023, o registrar solo las corridas de subtipos, suma, componente y severidad. Los nombres de los
   subtipos van en `causas_nivel4_es.csv`.
8. **Cambios (overrides) de `dl_config`:** `utils::modifyList` ignora las listas sin nombres; las claves que son
   secuencias (`medidas_entrada`, `decisiones`) se reemplazan con vectores atómicos (`c(...)`), no con `list(...)`.
9. **Cadenas cortas y la puerta del ancla:** con datos locales (`lambda = 0.5`, 9100, semilla 20260929, 40
   simulaciones, 2 cadenas) el error relativo mediano de `anchor_identity` fue 0.065 con 2000/1000 iteraciones
   (R-hat 2.7), 0.046 con 4000/2000 y 0.043 con 8000/4000, frente a la puerta 0.05, que `force` no salta. Es
   convergencia, no tensión entre datos y ancla. Con cadenas cortas hay que declarar la puerta en los cambios:
   `anchor = list(gate_err_mediano = list(valor = 0.10, procedencia = "cadenas cortas del ejemplo; ..."))`; la
   exportación pasa y el manifiesto lo registra como limitación. El verificador lo declara en la variante
   `datos_locales`. Los ajustes nacionales sin datos pasan la puerta con 2000/1000 (error 0.02-0.03).
10. **Held-out departamental y validación de amplitud:** `dl_validate_gbd(..., cascade =)` solo usa el csmr de nivel 1
    (`tipo_dato == "csmr"`); la prevalencia departamental cuenta en `n_heldout` del manifiesto pero no valida nada.
    Con el csmr de 2019 la amplitud da pendientes cercanas a 1 (estandarizadas ~0.96 y ~1.07 por sexo en el ajuste
    de 4000/2000 del verificador) y el manifiesto de la corrida con validación trae el bloque `amplitud_csmr`. La
    limitación «csmr departamental held-out de 2019 contra una cascada de 2023» del manifiesto la escribe la versión
    0.2.2 siempre que `cascada.heldout_anio` difiere del año de ajuste, haya o no cascada.
11. **Variantes que en los subtipos solo prueban la configuración:** `datos.csv` es solo de 9100, así que en 9101-9103
    `datos_y_proxies` y `datos_locales` comprueban proxies y configuración, no datos (el verificador lo anota).
12. **Rutas de 100 bytes como máximo dentro del tarball.** R CMD build avisa y R CMD check da una NOTE («portable
    file names») por cada ruta de más de 100 bytes contando desde `dismodlite/`. La partición de severidad repite el
    identificador de la corrida en la carpeta y en el CSV
    (`acs_peru_completo/particion/<corrida>/cause_health_state/proportion/<corrida>.csv`), así que la corrida se
    llama `acs_v1` y la carpeta `particion` (la ruta más larga mide 99 bytes; con `particion_severidad` medía 109 y
    con `sintetico_split_acs_v1`, 132). El lector
    toma el único CSV de `<entidad>/proportion/` y el identificador solo aparece en la columna `run_id` y en los
    mensajes; ninguna referencia del arnés lo guarda. `test-portabilidad.R` vigila el límite.

## Arnés de compatibilidad con la versión 0.2.2

`referencia_legado.R` carga el código de `v0.2.2` en un worktree temporal y corre los escenarios del arnés
(`tests/testthat/helper-arnes-*.R`: `api`, `escenarios`, `normalizar` y `referencia`) con `api_legado()`;
`tests/testthat/test-compatibilidad-legado.R` los vuelve a correr con el código actual y compara. Cadenas cortas
(40 simulaciones, 2 cadenas, 2000/1000 iteraciones, `mh`,
semilla 20260929) y `forzar = TRUE`. Lo que se guarda por escenario: medias posteriores de los parámetros
(`parametros.csv`), media e intervalo por celda (`celdas.csv`), manifiestos normalizados y la lista de archivos de
cada corrida (`manifiestos.yaml`) y tablas (`tablas/`: diagnósticos, etiquetas y sumas de control de cada corrida,
factor de comorbilidad, severidad derivada, sensibilidad, tablas consolidadas).

| Escenario | Qué ejercita además de lo básico |
|---|---|
| E1 | ajuste nacional sin datos ni proxies, ajuste solo con el prior, validación contra el ancla |
| E2 | cascada por proxies (25 departamentos, SEV por edad), held-out de 2019 y amplitud del csmr |
| E3 | cascada plana; remisión 0.02 y `remision.por_edad` 0.1 en [30, 50); estado de severidad sin Beta |
| E4 | datos por las tres rutas (binomial, Poisson, log-normal con offset); atípico; lambda 0.5; tres etiquetas |
| E5 | subtipos con cascada, su suma y la suma con una hija omitida |
| E6 | componente (`anchor.componente`) con la partición de severidad |
| E7 | severidad de 9101 desde la partición del padre y de 9102 directa (renormalizada) |
| E8 | 2019; proyección 2024 con el ancla de 2023 y la beta de HAQ en 0-1 (`escala` 0.01); consolidado de 2024 |
| E9 | re-resumen de la corrida de E2 (estructura, tablas y simulaciones enlazadas) |
| E10 | perfiles v1 y v2, gana la versión 2 de 9101; consolidado con una hija omitida |
| E11 | sensibilidad (2 lambda x 1 rho x 2 kappa) sobre E2; etiquetas con la rejilla rho = (0.5, 0.9) sobre E4 |
| E12 | E1 con el motor `rcpp` (se omite sin Rcpp, en la prueba y en el script) |

1. **Solo nacional y tres departamentos** (01 Amazonas, 15 Lima, 25 Ucayali) en celdas y tablas. Con los 25
   departamentos las tablas consolidadas de E10 solas pesarían unos 5 MB. Las otras 22 ubicaciones las vigilan las
   **sumas de control** (`<pieza>__sumas.csv`): por medida, ubicación y sexo, la suma sobre las edades de val, lower
   y upper de las celdas de cada corrida y de la media y los cuantiles 2.5 % y 97.5 % de sus simulaciones
   (`draws/*.csv.gz`, que la suma de hijas escribe y el re-resumen enlaza aparte de las celdas); en los consolidados,
   las mismas sumas de la tabla canónica por ubicación, sexo y métrica (`*__sumas_canonico.csv`, conteos incluidos).
   Todo pesa 1.5 MB (objetivo 2 MB).
2. **Dos corridas iguales.** El script corre todo dos veces (la segunda con una copia de los datos en otra carpeta,
   otra carpeta de salida y la cache de ajustes vacía) y exige números, tablas y manifiestos normalizados
   idénticos. Ninguna clave del manifiesto sin normalizar cambió entre las dos corridas: los manifiestos no llevan
   rutas. Se quitan por diseño la fecha (`generado`), la versión (`params.version_paquete`) y el sha256 de las
   particiones (el CSV lleva el run_id, que lleva la fecha); de la prosa (`limitaciones`) queda cuántas son
   (`n_limitaciones`); en todo texto la fecha de los identificadores de corrida pasa a `FECHA_`. La columna `fuente`
   se compara cuando es un valor cerrado (`tabla`, `mod`: la severidad) y se quita cuando es prosa (comorbilidad).
   Del consolidado se comparan solo las claves que no cambian de nombre en la versión 1.0.0 (bloques, parámetros,
   población, huecos, filas por medida y nombres de las tablas del perfil); su prosa se reescribe en la 1.0.0 y no se
   cuenta.
3. **Orden alfabético.** La versión 0.2.2 ordena con `split()` las claves de `cascada.dx_por_edad.bandas_por_proxy`
   del manifiesto, así que su orden depende de `LC_COLLATE` (`haqi` va antes o después de `LDI_pc`). testthat fija
   el orden «C»; `correr_escenario()` también, para que el script y la prueba coincidan.
4. **Re-resumen al 95 %.** `dl_validate_estimates` de la versión 0.2.2 exige `ui_level` 0.95, así que E9 (y la
   versión 2 de 9101 en E10) re-resumen con `nivel = 0.95`: prueban la lectura de las simulaciones y el re-armado de
   la corrida, no otro nivel.
5. **Registros.** Cada grupo de escenarios registra sus corridas en un registro temporal propio. E10 consolida desde
   el registro de E5 (subtipos y suma) más la versión 2 de 9101; el consolidado con omitidas usa un registro con 9101,
   9102 y la suma sin 9103. Con un ajuste nacional de 9100 como corrida vigente la consolidación se detiene
   (restricción 7): el consolidado de 2024 de E8 usa una copia de `registro/` con 9100 sin hijas.
6. **Variantes de los datos.** Algunas ramas no se activan con los datos de ejemplo tal cual; el arnés escribe
   copias modificadas de unos pocos archivos en la carpeta del escenario (`variantes/`) y la referencia se genera con
   las mismas copias: `datos.csv` con la cohorte de incidencia en conteos (20 000 personas-año) y dos estudios de
   prevalencia pequeños (E4); `severidad/9100.csv` con el estado moderado en 5 % [0, 90 %], cuya varianza no admite
   una Beta (E3); la partición con las filas del padre para la secuela 91013 x 1.02 y las de 9102 x (1 + 8e-7)
   (E7); la configuración y la extracción con la beta de HAQ en 0-1 (-1.2) y `escala: 0.01` (E8); `master_gbd.csv`
   sin hijas de 9100 (E8). Los cambios (`cambios`) de `dl_config` no alcanzan: `transformaciones` es una lista sin
   nombres (restricción 8).
7. **Perfiles.** Los perfiles de consolidación de la etiqueta (`compat/config/export_cdc/`) se copian a
   `tests/testthat/_referencia/perfiles_legado/` con el identificador y la descripción neutros (la consolidación no
   los usa en las tablas) y el script genera las referencias con esas copias: la prueba las usa con `api_legado()`
   aunque `compat/` desaparezca. Con `api_nueva()` la prueba usa `inst/perfiles/` y esta carpeta sobra.
8. **Modo exacto.** Con `DL_ARNES_EXACTO=1` la prueba compara con tolerancia 0: un cambio de código que reescribe una
   expresión aritmética (p. ej. `h / 6 * s` por `h * s / 6` en el RK4) cambia bits que la tolerancia 1e-8 deja pasar
   y el modo exacto lo detecta. Exige referencias generadas en la misma máquina. Las guardadas valen solo en su plataforma
   (`_referencia/plataforma.txt`, hoy macOS arm64): en otra, la prueba se salta y se regeneran ahí con
   `Rscript data-raw/referencia_legado.R --salida <carpeta>` (necesita la etiqueta git local v0.2.2) para leerlas con
   `DL_REFERENCIA_DIR`. El 2026-10-01 se comprobó así, en modo exacto, en Windows, Ubuntu y macOS. Para releer bit a bit, los
   números se escriben con 15-17 cifras y se leen con un conversor de redondeo correcto (el `strtod` de C vía
   `jsonlite`): en esta máquina `as.numeric` y `fread` dejan a 1 ulp uno de cada seis números de 17 cifras. El script
   comprueba al final, en modo exacto, que lo escrito se relee igual a lo corrido.
9. **Tiempos** (este equipo): cada corrida completa de E1-E12 tarda unos 75 s; el script, unos 2.5 min; la prueba,
   unos 80 s (los ajustes se memorizan entre escenarios).
