# Compatibilidad con la versión 0.2.2: cada escenario del arnés (helper-arnes-*.R) se corre con el código actual
# y se compara con las referencias que data-raw/referencia_legado.R generó con el código de la etiqueta v0.2.2 sobre
# los mismos datos (inst/extdata/acs_peru_completo): números con tolerancia relativa 1e-8, manifiestos normalizados idénticos.
# Los ajustes se memorizan entre escenarios dentro de la sesión (E9, E10 y E11 reutilizan los de E2, E4 y E5).
#
# Modo exacto: con DL_ARNES_EXACTO=1 la tolerancia es 0 y cualquier diferencia de un bit falla. Sirve para
# comprobar que un cambio de código no toca ninguna cuenta (reescribir una expresión aritmética, por ejemplo, cambia
# bits que la tolerancia 1e-8 deja pasar); exige referencias generadas en la misma máquina.
#
# La API de los escenarios son las funciones nuevas del paquete, sin adaptador (api_nueva()); api_legado() queda
# para los scripts de data-raw/, que corren los mismos escenarios con el código de la versión 0.2.2. La
# consolidación usa los perfiles del paquete (inst/perfiles); las referencias se generaron con las copias de los
# perfiles de la versión 0.2.2 (_referencia/perfiles_legado). Las columnas de los dos son las mismas y
# normalizar_consolidado() compara el contenido de las tablas, no las carpetas (entrega/ antes, tablas/ ahora) ni
# el identificador del perfil.

# Lo que ejercita cada escenario además de lo básico (va en el nombre de su prueba). E12 exige Rcpp.
.QUE_EJERCITA <- c(
  E1  = "nacional",
  E2  = "cascada por proxies (25 departamentos, held-out y amplitud)",
  E3  = "cascada plana, remisi\u00f3n por tramo de edad y severidad sin Beta",
  E4  = "datos locales (binomial, Poisson, log-normal con offset, lambda 0.5)",
  E5  = "subtipos y suma, con y sin una hija omitida",
  E6  = "componente con la partici\u00f3n de severidad",
  E7  = "severidad desde la partici\u00f3n (del padre y directa, renormalizada)",
  E8  = "a\u00f1o 2019, proyecci\u00f3n 2024 (HAQ en 0-1) y su consolidado",
  E9  = "re-resumen de la corrida de E2",
  E10 = "consolidado (perfiles v1 y v2, versi\u00f3n nueva, hija omitida)",
  E11 = "sensibilidad y etiquetas con la rejilla de rho",
  E12 = "motor rcpp (E1 con C++)")

for (id in c(.ESCENARIOS_BASE, .ESCENARIOS_RCPP)) {
  test_that(sprintf("%s %s reproduce la versi\u00f3n 0.2.2", id, .QUE_EJERCITA[[id]]), {
    if (id %in% .ESCENARIOS_RCPP) skip_if_not_installed("Rcpp")
    saltar_si_otra_plataforma()
    res <- correr_escenario(id, api_nueva(), raiz_datos = ruta_acs(), carpeta_salida = withr::local_tempdir())
    comparar_con_referencia(id, res)
  })
}
