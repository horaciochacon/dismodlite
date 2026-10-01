# acs_peru_completo: datos de ejemplo de dismodlite en el formato completo

**Todos los datos son sintéticos.** La enfermedad es ficticia: la *arteriopatía crónica
sintética* (ACS, causa 9100) con tres subtipos: 9101 miembros inferiores, 9102 carotídea y 9103
renovascular. La geografía es la real del Perú (nacional 123 y 25 departamentos, ubigeo 01-25);
ningún número proviene de datos reales. Los genera `data-raw/generar_acs_peru.R` con una semilla fija.

Es el mismo proyecto que `acs_peru` (el formato simple), con los mismos números, en el formato completo: el
que usan las guías avanzadas y el arnés de compatibilidad con la versión 0.2.2.

- `config/`: configuración de cada causa (`9100.yaml` ... `9103.yaml`); los subtipos usan la
  extracción de 9100.
- `ancla/`: estimaciones de referencia (prevalencia, incidencia, mortalidad y AVD) por edad y sexo, 2019 y 2023.
- `covariables/`: SEV, LDI y HAQ inventados para Perú (123), Global (1) y la región (120), formato GHDx;
  `extraccion.yaml`: sus betas.
- `proxies_departamentales.csv`: proxies de 2019, 2023 y 2024 (el del SEV varía por banda de edad); sus
  diferencias entre departamentos siguen un índice sintético y no describen a los departamentos reales.
- `poblacion.csv` y `pesos_80mas.csv`: población nacional y departamental (2019, 2023, 2024) y pesos de 80+.
- `datos.csv` (solo la causa 9100): mortalidad nacional, estudio de prevalencia, cohorte de incidencia y un valor
  atípico (2023); mortalidad y prevalencia departamentales de 2019 que no entran al ajuste: sirven para
  validar.
- `severidad/<causa>.csv`: proporciones por estado de salud; `particion/`: una corrida de partición
  (proporción por estado de salud de las cuatro causas y por secuela).
- `evidencia/`, `registro/` y `catalogos/`: fuentes citadas por el ancla, registros (nombres, subtipos y
  secuelas; sin deterioros) y catálogos (Perú y sus 25 departamentos).

Las curvas verdaderas (`verdad.csv`) están en `acs_peru`.
