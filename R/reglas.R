# Reglas del contrato dismod_lite/v1: las que el esquema (inst/schema/dismod_lite.v1.yaml) declara en `reglas:` para
# cada tabla. Cada regla es function(dt, tabla, sch, ctx) y devuelve los problemas que encuentra (character(), o
# NULL si no hay ninguno), cada uno como «tabla: regla — descripción». dl_validar_tabla() (R/validar.R) las aplica
# después de comprobar columnas y tipos; `ctx` trae lo que necesitan las reglas que cruzan tablas (población, betas,
# configuración...).
.dl_reglas <- new.env(parent = emptyenv())   # nombre de la regla -> función
.dl_regla <- function(nombre, f) assign(nombre, f, envir = .dl_reglas)

# ---- Datos locales ----

.dl_regla("edad_valida", function(dt, tabla, sch, ctx) {
  mal <- dt$age_start >= dt$age_end
  if (any(mal)) sprintf("%s: edad_valida \u2014 la edad de inicio no es menor que la edad de fin: %s", tabla,
                        paste(dt$dato_id[mal], collapse = ", "))
})

.dl_regla("pareja_val_se_o_x_n", function(dt, tabla, sch, ctx) {
  ok <- (!is.na(dt$val) & !is.na(dt$se)) | (!is.na(dt$x) & !is.na(dt$n))
  if (any(!ok)) sprintf("%s: pareja_val_se_o_x_n \u2014 sin valor y error est\u00e1ndar ni casos y muestra: %s",
                        tabla, paste(dt$dato_id[!ok], collapse = ", "))
})

.dl_regla("crosswalk_si_alternativa", function(dt, tabla, sch, ctx) {
  mal <- !dt$es_referencia & is.na(dt$crosswalk_id)
  if (any(mal))
    sprintf("%s: crosswalk_si_alternativa \u2014 un dato con definici\u00f3n alternativa exige crosswalk_id", tabla)
})

# Un motivo vacío (una celda en blanco del CSV) es un motivo que falta.
.dl_regla("outlier_con_motivo", function(dt, tabla, sch, ctx) {
  mal <- dt$outlier & (is.na(dt$outlier_motivo) | !nzchar(trimws(dt$outlier_motivo)))
  if (any(mal)) sprintf("%s: outlier_con_motivo \u2014 un dato marcado para excluir exige su motivo: %s",
                        tabla, paste(dt$dato_id[mal], collapse = ", "))
})

# Con anchor.location_id (códigos subnacionales libres) no se exige el ubigeo de dos dígitos.
.dl_regla("nivel_0_o_1", function(dt, tabla, sch, ctx) {
  loc <- ctx$loc_ancla %||% .DL_LOC_ANCLA[["peru"]]
  ubigeo <- is.null(ctx$cfg) || .dl_codigos_ubigeo(ctx$cfg)
  mal <- !dt$location_level %in% c(0L, 1L) |
         (dt$location_level == 0L & dt$location_id != loc) |
         (ubigeo & dt$location_level == 1L & nchar(dt$location_id) != 2L)
  if (any(mal)) sprintf(paste0("%s: nivel_0_o_1 \u2014 solo se admiten filas nacionales (nivel 0, la ",
                               "ubicaci\u00f3n del ancla %s) o %s"), tabla, loc,
                        if (ubigeo) "departamentales (nivel 1, ubigeo de dos d\u00edgitos)"
                        else "subnacionales (nivel 1)")
})

# ---- Ancla y betas ----

.dl_regla("ui_ordenado", function(dt, tabla, sch, ctx) {
  mal <- !(dt$lower <= dt$val & dt$val <= dt$upper)
  if (any(mal)) sprintf("%s: ui_ordenado \u2014 se exige lower <= val <= upper", tabla)
})

.dl_regla("lambda_en_0_1", function(dt, tabla, sch, ctx)
  if (any(!.DL_DOMINIO_REJILLA$lambda$dentro(dt$lambda)))
    sprintf("%s: lambda_en_0_1 \u2014 lambda fuera de %s", tabla, .DL_DOMINIO_REJILLA$lambda$texto))

.dl_regla("location_es_123", function(dt, tabla, sch, ctx) {
  loc <- ctx$loc_ancla %||% .DL_LOC_ANCLA[["peru"]]
  if (any(dt$location_id != loc))
    sprintf("%s: location_es_123 \u2014 el ancla debe ser de la ubicaci\u00f3n de la configuraci\u00f3n (%s)",
            tabla, loc)
})

.dl_regla("modo_consistente_con_ic", function(dt, tabla, sch, ctx) {
  mal <- (dt$modo == "informativa" & (is.na(dt$beta_lower) | is.na(dt$beta_upper))) |
         (dt$modo == "fija" & !is.na(dt$beta_lower) & !is.na(dt$beta_upper))
  if (any(mal)) sprintf(paste0("%s: modo_consistente_con_ic \u2014 una beta informativa exige su intervalo ",
                               "(beta_lower, beta_upper) y una fija no debe traerlo"), tabla)
})

# El nombre publicado de la covariable (nombre_impreso) debe delatar la transformación declarada: «log-» para log,
# «logit» para logit. Las covariables con token_exento en la configuración (con su procedencia) no se revisan.
.dl_regla("token_transformacion", function(dt, tabla, sch, ctx) {
  tokens <- ctx$tokens %||% c(log = "log-", logit = "logit", lineal = "")
  exentos <- ctx$token_exentos %||% character()
  probs <- character()
  for (i in seq_len(nrow(dt))) {
    if (dt$covariate_name_short[i] %in% exentos) next
    tk <- tokens[[dt$transformacion[i]]]
    if (nzchar(tk) && !grepl(tk, tolower(dt$nombre_impreso[i]), fixed = TRUE))
      probs <- c(probs, sprintf(paste0("%s: token_transformacion \u2014 \u00ab%s\u00bb se declara %s pero su ",
                                       "nombre_impreso no contiene \u00ab%s\u00bb"),
                                tabla, dt$covariate_name_short[i], dt$transformacion[i], tk))
  }
  probs
})

.dl_regla("fija_con_var_default", function(dt, tabla, sch, ctx) {
  mal <- dt$modo == "fija" & is.na(dt$var_default)
  if (any(mal))
    sprintf("%s: fija_con_var_default \u2014 una correcci\u00f3n fija exige var_default (su varianza inflada)", tabla)
})

# ---- Proxies subnacionales ----

.dl_regla("proxy_acunado", function(dt, tabla, sch, ctx) {
  mal <- dt$covariate_id_proxy < 900101L | dt$covariate_id_proxy >= 900200L
  if (any(mal)) sprintf("%s: proxy_acunado \u2014 covariate_id_proxy debe estar en [900101, 900200)", tabla)
})

# El promedio de los departamentos, ponderado por población, reproduce el valor nacional del proxy (ancla_ghdx) en
# cada proxy, año, sexo y banda. Cada ubicación de los proxies debe estar en la población (con otro código, el
# promedio la dejaría fuera).
.dl_regla("promedio_cierra_ancla", function(dt, tabla, sch, ctx) {
  if (is.null(ctx$poblacion))
    return(sprintf("%s: promedio_cierra_ancla \u2014 falta la poblaci\u00f3n en el contexto (contexto$poblacion)",
                   tabla))
  if (!nrow(dt)) return(NULL)                                       # sin proxies (insumos sin cascada) no hay cierre
  pob <- ctx$poblacion
  sin_pob <- setdiff(dt$location_id, pob$location_id[pob$location_level %||% 1L == 1L])
  if (length(sin_pob))
    return(sprintf(paste0("%s: promedio_cierra_ancla \u2014 ubicaci\u00f3n(es) sin poblaci\u00f3n subnacional: %s; ",
                          "los proxies usan las ubicaciones de la poblaci\u00f3n"), tabla,
                   paste(utils::head(sin_pob, 5L), collapse = ", ")))
  dt <- .dl_proxy_con_banda(dt)                                       # tabla sin age_group_id: todas las edades (22)
  if ("sex_id" %in% names(pob)) {
    # tabla de población completa: el peso es la población departamental del sexo del proxy (3 = ambos) en la banda
    # del proxy, la suma de las bandas de la tabla contenidas en ella (límites del catálogo)
    if ("location_level" %in% names(pob)) pob <- pob[location_level == 1L]
    pl <- .dl_poblacion_en_bandas(pob, unique(dt$age_group_id), ctx$bandas_catalogo)
    p3 <- if (any(pl$sex_id == 3L)) pl[sex_id == 3L]
          else pl[, list(pob = sum(pob), sex_id = 3L), by = list(location_id, age_group_id)]
    pob <- rbind(pl[sex_id %in% c(1L, 2L)], p3[, list(location_id, sex_id, age_group_id, pob)])
    m <- merge(dt, pob, by = c("location_id", "sex_id", "age_group_id"))
  } else m <- merge(dt, pob[, list(location_id, pob = val)], by = "location_id")
  cierre <- m[, list(prom = sum(valor_calibrado * pob) / sum(pob), ancla = ancla_ghdx[1]),
              by = list(covariate_id_proxy, year, sex_id, age_group_id)]
  tol <- ctx$tol %||% 1e-8
  mal <- cierre[abs(prom - ancla) > tol]
  if (!nrow(mal)) return(NULL)
  cv <- Filter(function(x) identical(as.integer(x$proxy$covariate_id_proxy), as.integer(mal$covariate_id_proxy[1])),
               ctx$cfg$covariables)
  nombre <- if (length(cv)) cv[[1]]$covariate_name_short else ""
  sprintf(paste0("%s: promedio_cierra_ancla \u2014 el promedio ponderado por poblaci\u00f3n es %s y el valor ",
                 "nacional de la covariable es %s: difieren en %s y la tolerancia es %s (%s, %s, %s). El promedio de ",
                 "las ubicaciones, ponderado por su poblaci\u00f3n del mismo sexo y grupo de edad, debe ser el valor ",
                 "nacional de la covariable; desplaza o escala sus valores para que cierren (ver ?dl_proyecto)"),
          tabla, format(mal$prom[1], digits = 12), format(mal$ancla[1], digits = 12),
          format(mal$prom[1] - mal$ancla[1], digits = 3), format(tol),
          if (nzchar(nombre)) paste("covariable", nombre) else sprintf("proxy %d", mal$covariate_id_proxy[1]),
          .dl_nombres_sexo(mal$sex_id[1]), .dl_texto_banda(mal$age_group_id[1], ctx$bandas_catalogo))
})

# Un grupo de edad en los mensajes, por sus edades («20-24 años», «80 años y más», «todas las edades»); sin catálogo, o
# si dura menos de un año, por su id («grupo de edad 2»).
.dl_texto_banda <- function(id, catalogo) {
  k <- if (!is.null(catalogo)) match(id, catalogo$age_group_id) else NA_integer_
  a0 <- catalogo$age_start[k]; a1 <- catalogo$age_end[k]
  if (is.na(k) || a1 - a0 < 1) sprintf("grupo de edad %d", id)
  else if (a1 < 125) sprintf("%g-%g a\u00f1os", a0, a1 - 1)
  else if (a0 > 0) sprintf("%g a\u00f1os y m\u00e1s", a0)
  else "todas las edades"
}

# dX = T(valor_calibrado) - T(X_nac), con X_nac de cov_valores: el ancla con que se calibró el proxy tiene que ser ese
# mismo X_nac (año de ajuste; mismo sexo, o sexo 3 si la covariable no distingue sexo). Si difieren (otra adquisición
# GHDx, otra banda de edad, otra escala), todos los departamentos heredan un sesgo constante que ninguna otra regla ve.
.dl_regla("ancla_igual_cov_valores", function(dt, tabla, sch, ctx) {
  if (is.null(ctx$cov_valores) || is.null(ctx$cfg) || !nrow(dt)) return(NULL)
  anio <- .dl_anio_ajuste(ctx$cfg)
  cv <- ctx$cov_valores[year == anio & location_id == (ctx$loc_ancla %||% .DL_LOC_ANCLA[["peru"]])]
  a <- unique(dt[, list(covariate_id_proxy, covariate_id_gbd, sex_id, ancla_ghdx)])
  probs <- character()
  for (i in seq_len(nrow(a))) {
    x <- .dl_filas_del_sexo(cv[covariate_id == a$covariate_id_gbd[i]], a$sex_id[i])
    if (nrow(x) != 1L) {
      probs <- c(probs, sprintf(paste0("%s: ancla_igual_cov_valores \u2014 el proxy %d no tiene un valor ",
                                       "nacional \u00fanico en cov_valores (covariable %d, a\u00f1o %d, sexo %d: ",
                                       "%d filas)"),
                                tabla, a$covariate_id_proxy[i], a$covariate_id_gbd[i], anio, a$sex_id[i], nrow(x)))
    } else if (abs(x$val - a$ancla_ghdx[i]) > 1e-6 * max(abs(x$val), 1e-12)) {
      probs <- c(probs, sprintf(paste0("%s: ancla_igual_cov_valores \u2014 proxy %d, sexo %d: ancla_ghdx %.6g ",
                                       "no es el valor nacional %.6g de cov_valores (covariable %d, a\u00f1o %d)"),
                                tabla, a$covariate_id_proxy[i], a$sex_id[i], a$ancla_ghdx[i], x$val,
                                a$covariate_id_gbd[i], anio))
    }
  }
  probs
})

# ---- Severidad, deterioros y comorbilidad ----

.dl_regla("proporciones_suman_uno", function(dt, tabla, sch, ctx) {
  s <- dt[, list(s = sum(proportion)), by = cause_id]
  mal <- s[abs(s - 1) > (ctx$tol %||% 1e-8)]
  if (nrow(mal)) sprintf(paste0("%s: proporciones_suman_uno \u2014 la suma de las proporciones de los estados ",
                                "es %s en la causa %d (debe ser 1)"),
                         tabla, format(mal$s[1], digits = 12), mal$cause_id[1])
})

.dl_regla("dw_en_0_1", function(dt, tabla, sch, ctx) {
  mal <- dt$dw_mean < 0 | dt$dw_mean > 1 | dt$dw_lower < 0 | dt$dw_upper > 1
  if (any(mal)) sprintf("%s: dw_en_0_1 \u2014 un peso de discapacidad fuera de [0, 1]", tabla)
})

.dl_regla("poblacion_no_negativa", function(dt, tabla, sch, ctx)
  if (any(dt$val < 0)) sprintf("%s: poblacion_no_negativa \u2014 hay poblaci\u00f3n negativa", tabla))

.dl_regla("atribucion_suma_uno", function(dt, tabla, sch, ctx) {
  s <- dt[, list(s = sum(p_causa)), by = list(rei_id, location_id, year, age_group_id, sex_id)]
  mal <- s[abs(s - 1) > (ctx$tol %||% 1e-8)]
  if (nrow(mal)) sprintf("%s: atribucion_suma_uno \u2014 la suma de p_causa es %.6f (debe ser 1)", tabla, mal$s[1])
})

.dl_regla("modelo_padre_registrado", function(dt, tabla, sch, ctx) {
  # sin la tabla en el contexto no se revisa; la pasa quien llama a
  # dl_validar_tabla(contexto = list(modelos_proporcion = ...))
  if (is.null(ctx$modelos_proporcion)) return(NULL)
  m <- merge(dt[, list(rei_id, cause_id, proportion_model)], ctx$modelos_proporcion,
             by = c("rei_id", "cause_id"), suffixes = c("", "_reg"), all.x = TRUE)
  mal <- m[is.na(proportion_model_reg) | proportion_model != proportion_model_reg]
  if (nrow(mal)) sprintf(paste0("%s: modelo_padre_registrado \u2014 el modelo de proporci\u00f3n (%s) no coincide con ",
                                "modelos_proporcion_deterioro.csv de la carpeta `registro`"),
                         tabla, paste(unique(mal$proportion_model), collapse = ", "))
})

.dl_regla("split_suma_uno_con_asintomatico", function(dt, tabla, sch, ctx) {
  probs <- character()
  grupos <- dt[, list(s = sum(q_h), asint = any(nivel == "asymptomatic")), by = list(rei_id, cause_id_override)]
  if (any(!grupos$asint))
    probs <- c(probs, sprintf(paste0("%s: split_suma_uno_con_asintomatico \u2014 la partici\u00f3n no tiene el nivel ",
                                     "asintom\u00e1tico (DW 0): la suma real de q ser\u00eda menor que 1"), tabla))
  mal <- grupos[abs(s - 1) > (ctx$tol %||% 1e-8)]
  if (nrow(mal))
    probs <- c(probs, sprintf("%s: split_suma_uno \u2014 la suma de q_h es %.6f (debe ser 1)", tabla, mal$s[1]))
  probs
})

.dl_regla("rol_deterioro_con_rei", function(dt, tabla, sch, ctx) {
  mal <- dt$rol == "impairment_severity" & (is.na(dt$rei_id) | is.na(dt$rei_id_severidad))
  if (any(mal))
    sprintf("%s: rol_deterioro_con_rei \u2014 el rol impairment_severity exige rei_id y rei_id_severidad", tabla)
})

.dl_regla("residual_unico_por_causa", function(dt, tabla, sch, ctx) {
  n <- dt[rol == "residual_sin_deterioro", .N, by = cause_id][N > 1]
  if (nrow(n)) sprintf("%s: residual_unico_por_causa \u2014 la causa %d tiene %d secuelas residuales (se admite una)",
                       tabla, n$cause_id[1], n$N[1])
})

.dl_regla("factor_en_0_1", function(dt, tabla, sch, ctx)
  if (any(dt$factor <= 0 | dt$factor > 1))
    sprintf("%s: factor_en_0_1 \u2014 factor de comorbilidad fuera de (0, 1]", tabla))

# ---- Reglas que cruzan tablas: tipos declarados, doble uso, proxies de la configuración y población ----

# Las filas nacionales que no se excluyen entran al ajuste: su tipo debe estar en medidas_entrada. prev_admin (un
# registro administrativo) no puede estar: sus filas nacionales se excluyen.
.dl_regla("tipos_declarados", function(dt, tabla, sch, ctx) {
  if (is.null(ctx$cfg) || !nrow(dt)) return(NULL)
  tipos <- unique(dt$tipo_dato[dt$location_level == 0L & !dt$outlier])
  falta <- setdiff(tipos, unlist(ctx$cfg$medidas_entrada))
  otros <- setdiff(falta, "prev_admin")
  c(if (length(otros))
      sprintf(paste0("%s: tipos_declarados \u2014 tipo(s) de dato %s en filas nacionales que entran al ajuste sin ",
                     "estar en medidas_entrada de la configuraci\u00f3n: agr\u00e9galo(s) a medidas_entrada o marca ",
                     "esas filas para excluir, con su motivo"), tabla, paste(otros, collapse = ", ")),
    if ("prev_admin" %in% falta)
      sprintf(paste0("%s: tipos_declarados \u2014 las filas nacionales de prev_admin no pueden entrar al ajuste: ",
                     "m\u00e1rcalas para excluir, con su motivo"), tabla))
})

.dl_regla("doble_uso_acquisition", function(dt, tabla, sch, ctx) {
  if (is.null(ctx$acq_prior_csmr) || !nrow(dt)) return(NULL)
  mal <- dt$tipo_dato == "csmr" & dt$acquisition_id %in% ctx$acq_prior_csmr
  if (any(mal)) sprintf(paste("%s: doble_uso_acquisition \u2014 datos de mortalidad de la misma fuente (acquisition_id",
                              "%s) que el prior de la EMR: un dato no puede entrar al prior y al ajuste"),
                        tabla, paste(unique(dt$acquisition_id[mal]), collapse = ", "))
})

.dl_regla("proxy_mapea_config", function(dt, tabla, sch, ctx) {
  if (is.null(ctx$cfg) || !nrow(dt)) return(NULL)
  # covariables[].sustituye: el proxy se ancla en otra covariable GBD declarada (la versión estandarizada por edad de
  # una beta por edad); el par aceptado es entonces (proxy, sustituta) y no (proxy, beta): covariate_id_x.
  mapa <- .dl_mapa_proxies(ctx$cfg, ctx$betas[, list(covariate_name_short, covariate_id)])
  if (is.null(mapa))
    return(sprintf("%s: proxy_mapea_config \u2014 la configuraci\u00f3n no declara ning\u00fan proxy en covariables",
                   tabla))
  m <- merge(unique(dt[, list(covariate_id_proxy, covariate_id_gbd)]), mapa, by = "covariate_id_proxy", all.x = TRUE)
  mal <- m[is.na(covariate_id_x) | covariate_id_x != covariate_id_gbd]
  if (nrow(mal)) sprintf(paste("%s: proxy_mapea_config \u2014 el proxy %d no est\u00e1 declarado en covariables de la",
                               "configuraci\u00f3n o su covariate_id_gbd no es el de la beta (ni el de su",
                               "sustituye.covariate_id)"), tabla, mal$covariate_id_proxy[1])
})

.dl_regla("nivel1_suma_nivel0", function(dt, tabla, sch, ctx) {
  if (!any(dt$location_level == 1L)) return(NULL)
  s <- merge(dt[location_level == 1L, list(suma = sum(val)), by = list(year, sex_id, age_group_id)],
             dt[location_level == 0L, list(nac = val), by = list(year, sex_id, age_group_id)],
             by = c("year", "sex_id", "age_group_id"))
  mal <- s[abs(suma - nac) > 1e-6 * pmax(nac, 1)]
  if (nrow(mal)) sprintf(paste0("%s: nivel1_suma_nivel0 \u2014 la poblaci\u00f3n subnacional no suma la nacional ",
                                "(a\u00f1o %d, %s, %s): la nacional es %s y la suma de las subnacionales %s"), tabla,
                         mal$year[1], .dl_nombres_sexo(mal$sex_id[1]),
                         .dl_texto_banda(mal$age_group_id[1], ctx$bandas_catalogo),
                         format(mal$nac[1], digits = 12), format(mal$suma[1], digits = 12))
})
