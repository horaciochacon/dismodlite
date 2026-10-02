# acs_peru: datos de ejemplo de dismodlite

**Todos los datos son sintéticos.** La enfermedad es ficticia: la *arteriopatía crónica
sintética* (ACS, causa 9100) con tres subtipos: 9101 miembros inferiores, 9102 carotídea y 9103
renovascular. La geografía es la real del Perú (nacional 123 y 25 departamentos, ubigeo 01-25);
ningún número proviene de datos reales. Los genera `data-raw/generar_acs_peru.R` con una semilla fija.

Es un proyecto con las tablas del contrato de insumos: cópialo para empezar el tuyo. Se lee con
`dl_proyecto(dl_ejemplo(), causa = 9100)`; ver `?dl_tablas` (las tablas y sus columnas), `?dl_proyecto` (la
carpeta) y `?dl_configuracion` (las claves de la configuración).

- `config/`: la configuración de cada causa (`9100.yaml` ... `9103.yaml`); 9100 es la suma de sus subtipos.
- `ubicaciones.csv`: el país (código 123) y sus 25 departamentos (01-25).
- `poblacion.csv`: población de los 25 departamentos (2019, 2023 y 2024) por sexo y grupo de edad; la
  nacional es su suma y la calcula el paquete.
- `ancla/`: la estimación de referencia, una descarga de GBD Results con «ID y nombre»: prevalencia,
  incidencia, mortalidad y AVD por edad y sexo de las cuatro causas, 2019 y 2023.
- `covariables/`: descargas del GHDx de SEV, LDI y HAQ (valores inventados) para Perú (123), Global (1) y la
  región (120), y `proxies.csv`: las tres covariables por departamento (2019, 2023 y 2024; el SEV por grupo
  de edad); su promedio ponderado por la población es el valor nacional. Siguen un índice sintético y no
  describen a los departamentos reales.
- `betas.csv`: las betas de las tres covariables (las de la causa 9100; los subtipos usan las de su padre).
- `datos.csv` (solo la causa 9100): mortalidad nacional, un estudio de prevalencia, una cohorte de incidencia y
  un valor atípico (2023); mortalidad y prevalencia departamentales de 2019 que sirven para validar.
- `severidad.csv`: proporciones y pesos de discapacidad por estado de salud de cada causa.
- `verdad.csv`: curvas verdaderas p, i y f por edad, nacionales y departamentales, de 2019 y 2023 (no es un
  insumo del modelo).

El mismo proyecto en el formato completo de la versión 0.2.2 está en `acs_peru_completo`.
