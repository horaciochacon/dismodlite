# Compuerta de equivalencia del contrato de insumos: los escenarios S1-S5 del arnés (helper-arnes-simple.R) corren el
# ejemplo en el contrato (dl_ejemplo(), con la lectura de la versión 0.2.2 en `avanzado`) por dl_proyecto() y
# dl_insumos(), repiten los pasos de un escenario E y se comparan con su referencia de la versión 0.2.2 (la misma de
# test-compatibilidad-legado.R, generada sobre acs_peru_completo): parámetros, celdas y tablas con la tolerancia del
# arnés (0 con DL_ARNES_EXACTO=1, el modo que respalda que la traducción al formato completo no cambia ningún número)
# y manifiestos normalizados idénticos salvo las claves de .MANIFIESTO_FUERA_SIMPLE (la procedencia de los insumos).
# Cada escenario empieza con la cache de ajustes vacía: sus números salen de muestrear con los insumos del contrato,
# nunca de un ajuste de un escenario E.

# Lo que reproduce cada escenario (va en el nombre de su prueba).
.QUE_REPRODUCE <- c(
  S1 = "el ajuste nacional",
  S2 = "la cascada por proxies",
  S3 = "los datos locales en el ajuste (datos_en_ajuste, ancla.peso y avanzado)",
  S4 = "los subtipos y su suma",
  S5 = "el a\u00f1o 2019, la proyecci\u00f3n 2024 (ancla.anio, escala de betas.csv) y su consolidado")

for (id in names(.EQUIVALENTE_SIMPLE)) {
  ref <- .EQUIVALENTE_SIMPLE[[id]]
  test_that(sprintf("%s: el ejemplo en el contrato reproduce %s de %s (versi\u00f3n 0.2.2)", id,
                    .QUE_REPRODUCE[[id]], ref), {
    api <- api_nueva()
    api$limpiar_cache()
    saltar_si_otra_plataforma()
    res <- correr_escenario(id, api, raiz_datos = dl_ejemplo(), carpeta_salida = withr::local_tempdir())
    comparar_con_referencia(ref, res, fuera = .MANIFIESTO_FUERA_SIMPLE)
  })
}
