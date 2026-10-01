# Nombres de columnas que el código usa dentro de `[.data.table` (evaluación no estándar) y los símbolos especiales de
# data.table (.N, .SD, .BY, :=). Declararlos evita las notas «no visible binding» de R CMD check.
utils::globalVariables(c(
  ".BY", ".N", ".SD", ":=", "N", "a0", "a1", "acquisition_id", "age_end", "age_group_id", "age_group_name", "age_id",
  "age_name", "age_start", "agregacion", "anio", "banda", "beta_lower", "beta_upper", "canal", "causa", "causa_propia",
  "cause_id", "cause_name", "cause_name_es", "check", "cociente", "component_id", "contraccion", "covariable",
  "covariate_id", "covariate_id_proxy", "covariate_id_x", "covariate_name_short", "cubierto_ic95", "dato_id", "delta",
  "draw", "dw", "dw_lower", "dw_mean", "dw_upper", "dX", "edad", "err_rel", "escala", "estado", "etiqueta", "f",
  "fraccion_aguda", "health_state_id", "hijos", "i.factor", "id_estado", "lambda", "location_id", "location_level",
  "location_name", "lower", "measure_id", "med_post", "med_prior", "medida", "metric_id", "metric_name", "modo", "n",
  "n_ef", "n_efectivo", "nivel", "nombre", "nombre_ancla", "nombre_es", "obs", "obs_nac", "orden", "outlier", "p",
  "parametro_objetivo", "parent_id", "peso", "peso_discapacidad", "peso_inferior", "peso_superior", "poblacion", "pred",
  "pred_nac", "prev", "prop_lower", "prop_upper", "proporcion", "proporcion_inferior", "proporcion_superior",
  "proportion", "rho", "run_fuente", "run_id", "ruta", "s_log", "se", "sequela_id", "sex_id", "sigma_log_csmr",
  "sigma_log_prev", "simple", "subtipos", "sustituto", "tabla", "tipo_cod", "tipo_dato", "transformacion", "upper", "v",
  "v_post", "v_prior", "val", "val_csmr", "val_prev", "valor_gbd", "w", "x", "xs", "y", "year", "year_end", "year_id",
  "year_start", "yld", "ys"
))
