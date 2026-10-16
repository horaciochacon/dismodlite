// Modelo enfermedad-muerte de dismodlite, portado a JavaScript para las animaciones metodológicas.
//
// Qué es: una copia fiel, función por función, de la matemática del paquete R, para que lo que dibuja una escena
// sea exactamente lo que el paquete calcula. La referencia es el código R; cada función de aquí cita a su gemela
// (archivo y función). Notación: la de la cabecera de R/edo.R y de vignettes/el-modelo.qmd:
//   a edad (años); p prevalencia (proporción en [0, 1)); i incidencia en S; r remisión; f mortalidad en exceso (EMR);
//   todas las tasas por persona-año. dp/da = i (1 - p) - r p - f p (1 - p).
//   theta = (log i en los nudos, log f en los nudos); con la EMR fija en cero, theta = (log i en los nudos) y f = 0.
//   Mallas (a0 = edad_inicio, aF = edad_fin, h = 1/nsub):
//     *_anual  edades enteras a0, a0 + 1, ..., aF               (n_anios + 1 puntos)
//     *_malla  malla h:   a0, a0 + h, ..., aF                    (n_anios nsub + 1 puntos)
//     *_media  malla h/2: a0, a0 + h/2, ..., aF                  (2 n_anios nsub + 1 puntos; «media» = medio paso)
//     *_nudos  en los nudos de theta
//   El paso n de RK4 (base 0 aquí) va del punto 2n al 2n + 2 de la malla h/2 y usa el 2n + 1 como punto medio.
//
// Qué se porta (gemela en R entre paréntesis):
//   mallas e interpolación     edadesMedia (.dl_edades_media), idxAnualMalla/idxAnualMedia, matrizW (.dl_base_interp),
//                              interpolar (W %*% valores)                                                    R/edo.R
//   EDO                        dpDa (.dl_dp_da), pasoRk4 y resolverEdo (dl_edo_resolver), edo (dl_edo),
//                              remisionMedia (.dl_r_media), remisionPorEdad (.dl_remision_por_edad),
//                              aEdadesEnteras (.dl_a_edades_enteras), prevalenciaExacta (la solución cerrada de
//                              vignettes/el-modelo.qmd, sección 3)                                           R/edo.R
//   bandas de edad             bandaDe (.dl_banda_de), bandasFinas (.dl_bandas_finas), pesosIntervalo
//                              (.dl_pesos_intervalo), qIntervalos (.dl_q_intervalos), integrando (.dl_integrando,
//                              R/esquema.R)                                                                  R/bandas.R
//   log-posterior              lpSuavidad, lpPriorEmr, lpEmr, fFueraDeCota, lpMvnAr1, lpAncla, lpLognormal,
//                              lpBinomial, lpPoisson, lpDatos, lpComponentes (.dl_lp_componentes),
//                              logPosterior (.dl_log_post), thetaInicial, edoCtx, prepararCtx (.dl_ctx)
//                                                                                              R/verosimilitud.R
//   prior de la EMR            priorEmrBanda (.dl_prior_emr_tabla), emrEnNudos (.dl_emr_en_nudos), emrPlano,
//                              emrCero, emrDadoEnNudos, techoEmr (.dl_techo_emr)                            R/emr.R
//   álgebra                    sigmaLog (columna sigma_log de R/insumos.R), correlacionAr1 y cholesky
//                              (.dl_chol_ar1), logMvn
//   muestreo y diagnóstico     rhat (dl_rhat), ess (dl_ess), autocorrelacion (stats::acf), sumaGeyer
//                              (.dl_suma_geyer), escalaInicial, escalaAdaptada, formaPropuesta, propuestaMh,
//                              aceptaMh, semillaDerivada                                                R/muestreador.R
//   subnacional                renormalizar y renormalizarBanda (.dl_renormalizar), pesosEdad (.dl_pesos_edad),
//                              desplazamientoLog (.dl_desplazamiento_log)                                  R/cascada.R
//                              transformar, sdTransformada (R/betas.R), sdDelta (.dl_sd_delta, R/cascada.R)
//   AVD                        betaMomentos (.dl_beta_momentos), renormalizarProporciones, avdBanda
//                              (.dl_avd_bandas)                                                                R/avd.R
//
// Orden de las operaciones. RK4 repite las expresiones de dl_edo_resolver() en el mismo orden (los operadores de JS
// y de R asocian igual): con las mismas tasas en la malla h/2, p sale idéntico bit a bit. W %*% theta se hace como
// la dgemv de la BLAS de referencia: una suma por nudo, en su orden, desde 0 (cada fila de W tiene a lo sumo dos
// términos no nulos). Lo que no puede ser idéntico bit a bit: Math.exp, Math.log y Math.log1p de V8 frente a los de
// glibc (pueden diferir en el último bit), sum() y mean() de R, que acumulan en long double (aquí, suma compensada
// de Neumaier, que da el mismo resultado salvo un redondeo excepcional) y chol() de LAPACK (otro orden de las sumas).
// Por eso p coincide con dl_edo() a ~1e-15 relativo y la log-posterior con la de R a ~1e-12 absoluto.
//
// Convenciones: funciones puras (sin Math.random ni Date), arreglos simples de números, índices en base 0 (el
// índice j de aquí es el j + 1 de R); un NA de R es NaN aquí, y «sin banda» (NA en .dl_banda_de) es -1. Las matrices
// son arreglos de filas: W[j][k], P[b][a], U[b][c]. Script clásico (no módulo ES): se carga con <script> desde
// file:// o con vm.runInThisContext() en node, y deja todo en globalThis.DL.modelo.
(function (DL) {
  'use strict';

  // ---- Constantes (las mismas de R) ----------------------------------------------------------------------------

  var NSUB = 5;                          // .DL_NSUB (R/edo.R): subpasos de RK4 por año, h = 0.2
  var EDAD_FIN = 99;                     // .DL_EDAD_FIN (R/edo.R)
  var RK4_LIMITE_PASO = 2.5;             // .DL_RK4_LIMITE_PASO (R/edo.R): cota de k h que exige dl_insumos()
  var ANCHO_IC95_EN_SD = 3.92;           // .DL_ANCHO_IC95_EN_SD (R/betas.R): 2 x 1.96
  var LN_SQRT_2PI = 0.918938533204672741780329736406;   // M_LN_SQRT_2PI de Rmath (log(sqrt(2 pi)))
  var LOG_I_INICIAL = -6;                // .DL_LOG_I_INICIAL (R/verosimilitud.R)
  var MARGEN_LOG_F_INICIAL = 0.5;        // .DL_MARGEN_LOG_F_INICIAL (R/verosimilitud.R)
  var RUTA_LOGNORMAL = 0;                // .DL_RUTA_LOGNORMAL: el dato trae val y se
  var RUTA_CONTEOS = 1;                  // .DL_RUTA_CONTEOS: x de n
  var EMR_TECHO_K = 3;                   // .DL_EMR_TECHO_K (R/emr.R)
  var EMR_PLANO_RANGO = 1000;            // .DL_EMR_PLANO_RANGO (R/emr.R)
  var EMR_PLANO_INICIO_BAJO_TECHO = 2;   // .DL_EMR_PLANO_INICIO_BAJO_TECHO (R/emr.R)
  var EMR_SD_FUERA_DE_BANDAS = 2;        // .DL_EMR_SD_FUERA_DE_BANDAS (R/emr.R)
  var MH_ESCALA_OPTIMA = 2.38;           // .DL_MH_ESCALA_OPTIMA (R/muestreador.R)
  var MH_ACEPTACION_OBJETIVO = 0.28;     // .DL_MH_ACEPTACION_OBJETIVO
  var MH_VENTANA = 100;                  // .DL_MH_VENTANA
  var MH_JITTER_COVARIANZA = 1e-8;       // .DL_MH_JITTER_COVARIANZA
  var MH_SD_INICIO = 0.1;                // .DL_MH_SD_INICIO
  var MH_FACTOR_SEMILLA = 1000, MH_DESPLAZAMIENTO_SEMILLA = 500, MH_MODULO_SEMILLA = 2147483647;
  var CASCADA_EDAD_CIERRE_BANDA_ABIERTA = 85;   // .DL_CASCADA_EDAD_CIERRE_BANDA_ABIERTA (R/cascada.R)
  var VARIANZA_MINIMA_BETA = 1e-12;      // .DL_VARIANZA_MINIMA_BETA (R/avd.R)

  // Tipos de dato local: las filas de .DL_TIPOS_DATO (R/esquema.R), en su orden (que es el de la suma de L_datos).
  var TIPOS_DATO = [
    { tipo: 'prev_estudio', codigo: 1, integrando: 'p', familia_conteos: 'binomial', measure_id: 5 },
    { tipo: 'incidencia', codigo: 2, integrando: 'ipop', familia_conteos: 'poisson', measure_id: 6 },
    { tipo: 'csmr', codigo: 3, integrando: 'pf', familia_conteos: 'poisson', measure_id: 900006 }
  ];

  // Nombres de los componentes de la log-posterior, en el orden de .DL_LP_NOMBRES (R/verosimilitud.R).
  var LP_NOMBRES = ['suavidad', 'emr', 'ancla', 'datos', 'total', 'datos_prev_estudio', 'datos_incidencia',
    'datos_csmr'];

  // ---- Utilidades numéricas ------------------------------------------------------------------------------------

  // sum() de R: acumula en long double. Aquí, suma compensada de Neumaier (el resultado es la suma exacta
  // redondeada a double salvo casos excepcionales, igual que el long double de R). Con algún término no finito, la
  // suma simple (Inf, -Inf o NaN, como en R).
  function suma(x) {
    var s = 0, c = 0, simple = 0;
    for (var k = 0; k < x.length; k++) {
      var v = x[k];
      simple += v;
      var t = s + v;
      if (Math.abs(s) >= Math.abs(v)) c += (s - t) + v; else c += (v - t) + s;
      s = t;
    }
    if (!isFinite(simple)) return simple;
    return s + c;
  }

  // mean() de R para un vector double (src/main/summary.c): s = sum / n en long double y una pasada de corrección,
  // s + sum(x - s) / n.
  function media(x) {
    var n = x.length;
    if (!n) return NaN;
    var s = suma(x) / n;
    if (!isFinite(s)) return s;
    var dif = new Array(n);
    for (var k = 0; k < n; k++) dif[k] = x[k] - s;
    return s + suma(dif) / n;
  }

  // var() de R para un vector (stats::var -> cov.c): media con corrección y sum((x - media)^2) / (n - 1).
  // Con n < 2, NaN (NA en R).
  function varianza(x) {
    var n = x.length;
    if (n < 2) return NaN;
    var m = media(x);
    var cuad = new Array(n);
    for (var k = 0; k < n; k++) cuad[k] = (x[k] - m) * (x[k] - m);
    return suma(cuad) / (n - 1);
  }

  // stats::cov(matriz): covarianza de las columnas de una matriz dada como arreglo de filas. Devuelve d x d.
  function covarianza(filas) {
    var n = filas.length, d = filas[0].length, medias = new Array(d), C = [];
    for (var a = 0; a < d; a++) medias[a] = media(columna(filas, a));
    for (a = 0; a < d; a++) C.push(new Array(d));
    for (a = 0; a < d; a++)
      for (var b = 0; b <= a; b++) {
        var prod = new Array(n);
        for (var k = 0; k < n; k++) prod[k] = (filas[k][a] - medias[a]) * (filas[k][b] - medias[b]);
        C[a][b] = C[b][a] = suma(prod) / (n - 1);
      }
    return C;
  }

  function columna(filas, j) {
    var out = new Array(filas.length);
    for (var k = 0; k < filas.length; k++) out[k] = filas[k][j];
    return out;
  }

  // dnorm(x, mu, sigma, log = TRUE) de R (src/nmath/dnorm.c), con sus casos especiales:
  //   -(log(sqrt(2 pi)) + 0.5 z^2 + log(sigma)),  z = |x - mu| / sigma.
  function dnormLog(x, mu, sigma) {
    if (isNaN(x) || isNaN(mu) || isNaN(sigma)) return x + mu + sigma;
    if (sigma < 0) return NaN;
    if (!isFinite(sigma)) return -Infinity;
    if (!isFinite(x) && mu === x) return NaN;
    if (sigma === 0) return (x === mu) ? Infinity : -Infinity;
    var z = (x - mu) / sigma;
    if (!isFinite(z)) return -Infinity;
    z = Math.abs(z);
    if (z >= 2 * Math.sqrt(Number.MAX_VALUE)) return -Infinity;
    return -(LN_SQRT_2PI + 0.5 * z * z + Math.log(sigma));
  }

  // findInterval(x, v) de R (v creciente): cuántos elementos de v son <= x (0 si ninguno); NaN si x es NaN.
  function findInterval(x, v) {
    if (isNaN(x)) return NaN;
    var lo = 0, hi = v.length;
    while (lo < hi) { var mid = (lo + hi) >> 1; if (v[mid] <= x) lo = mid + 1; else hi = mid; }
    return lo;
  }

  // seq(desde, hasta, by = paso) de R con desde, hasta y paso double (seq.default): n = (hasta - desde) / paso,
  // as.integer(n + 1e-10) puntos más uno, desde + k paso y el último recortado a `hasta`.
  function secuencia(desde, hasta, paso) {
    var n = Math.trunc((hasta - desde) / paso + 1e-10);
    var x = new Array(n + 1);
    for (var k = 0; k <= n; k++) x[k] = Math.min(desde + k * paso, hasta);
    return x;
  }

  // seq(desde, hasta) de enteros.
  function enteros(desde, hasta) {
    var x = [];
    for (var a = desde; a <= hasta; a++) x.push(a);
    return x;
  }

  // ---- Mallas (R/edo.R) ----------------------------------------------------------------------------------------

  // Edades de la malla h/2: a0, a0 + h/2, ..., aF. Gemela: .dl_edades_media().
  function edadesMedia(edad_inicio, edad_fin, nsub) { return secuencia(edad_inicio, edad_fin, 1 / (2 * nsub)); }

  // Edades de la malla h: los puntos pares (base 0) de la malla h/2 (el punto n de la malla h es el 2n de la h/2).
  function edadesMalla(edad_inicio, edad_fin, nsub) {
    var media_ = edadesMedia(edad_inicio, edad_fin, nsub), out = [];
    for (var j = 0; j < media_.length; j += 2) out.push(media_[j]);
    return out;
  }

  // Posiciones (base 0) de las edades enteras en la malla h: 0, nsub, 2 nsub, ... Gemela: .dl_idx_anual_malla().
  function idxAnualMalla(n_anios, nsub) {
    var out = [];
    for (var j = 0; j <= n_anios * nsub; j += nsub) out.push(j);
    return out;
  }

  // Posiciones (base 0) de las edades enteras en la malla h/2 de n_media puntos: 0, 2 nsub, ...
  // Gemela: .dl_idx_anual_media().
  function idxAnualMedia(n_media, nsub) {
    var out = [];
    for (var j = 0; j < n_media; j += 2 * nsub) out.push(j);
    return out;
  }

  // ---- Interpolación de los nudos (matriz W, R/edo.R) ------------------------------------------------------------

  // W (edades x nudos) tal que log tasa(edades) = W log tasa(nudos). Gemela: .dl_base_interp().
  //   a = min(max(edad, nudo_1), nudo_K) (constante fuera de los nudos);
  //   a en [nudo_k, nudo_k+1): W[j][k] = 1 - w, W[j][k+1] = w, w = (a - nudo_k) / (nudo_k+1 - nudo_k);
  //   a = nudo_K: W[j][K] = 1. Cada fila suma 1.
  function matrizW(nudos, edades) {
    var K = nudos.length, W = new Array(edades.length);
    for (var j = 0; j < edades.length; j++) {
      var fila = new Array(K);
      for (var k = 0; k < K; k++) fila[k] = 0;
      var a = Math.min(Math.max(edades[j], nudos[0]), nudos[K - 1]);
      var kk = findInterval(a, nudos);          // 1..K, como en R
      if (kk >= K) fila[K - 1] = 1;
      else {
        var w = (a - nudos[kk - 1]) / (nudos[kk] - nudos[kk - 1]);
        fila[kk - 1] = 1 - w; fila[kk] = w;
      }
      W[j] = fila;
    }
    return W;
  }

  // W %*% x como la dgemv de la BLAS de referencia: y_j = 0 + x_1 W[j][1] + x_2 W[j][2] + ... en el orden de los
  // nudos. Si x trae un valor no finito, R usa su producto interno (mismos términos): el resultado es igual.
  function productoW(W, x) {
    var y = new Array(W.length);
    for (var j = 0; j < W.length; j++) {
      var fila = W[j], s = 0;
      for (var k = 0; k < x.length; k++) s = s + x[k] * fila[k];
      y[j] = s;
    }
    return y;
  }

  // W %*% valores_nudos en las `edades`: log i(a) o log f(a) desde sus valores en los nudos.
  function interpolar(valores_nudos, nudos, edades) { return productoW(matrizW(nudos, edades), valores_nudos); }

  // ---- Remisión (R/edo.R) ----------------------------------------------------------------------------------------

  // r en la malla h/2: un número se repite n_media veces; un arreglo debe traer un valor por punto.
  // Gemela: .dl_r_media().
  function remisionMedia(remision, n_media) {
    if (typeof remision === 'number') { var r = new Array(n_media); for (var j = 0; j < n_media; j++) r[j] = remision; return r; }
    if (remision.length === 1) return remisionMedia(remision[0], n_media);
    if (remision.length !== n_media)
      throw new Error('`remision` debe ser un número o un vector de la longitud de `i_media` (' + n_media +
        '), no de longitud ' + remision.length);
    return remision.slice();
  }

  // r(a) en `edades` según la configuración {valor, por_edad: [{edad_inicio, edad_fin, valor}]}: valor (0 si
  // falta) en todas y, en cada tramo, su valor en [edad_inicio, edad_fin). Gemela: .dl_remision_por_edad().
  function remisionPorEdad(remision, edades) {
    remision = remision || {};
    var base = remision.valor == null ? 0 : Number(remision.valor), r = new Array(edades.length);
    for (var j = 0; j < edades.length; j++) r[j] = base;
    var tramos = remision.por_edad || [];
    for (var t = 0; t < tramos.length; t++)
      for (j = 0; j < edades.length; j++)
        if (edades[j] >= tramos[t].edad_inicio && edades[j] < tramos[t].edad_fin) r[j] = Number(tramos[t].valor);
    return r;
  }

  // ---- EDO y RK4 (R/edo.R) ---------------------------------------------------------------------------------------

  // dp/da = i (1 - p) - r p - f p (1 - p). Gemela: .dl_dp_da().
  function dpDa(i, r, f, p) { return i * (1 - p) - r * p - f * p * (1 - p); }

  function valorEn(x, j) { return typeof x === 'number' ? x : x[j]; }

  // Un paso de RK4 (para la escena de RK4): de la edad a (punto j, base 0, de la malla h/2) a a + h, con
  // p1 = p(a). Usa las tasas en j (a), j + 1 (a + h/2) y j + 2 (a + h); r_media puede ser un número.
  //   k1 = F(a, p1); p2 = p1 + h/2 k1; k2 = F(a + h/2, p2); p3 = p1 + h/2 k2; k3 = F(a + h/2, p3);
  //   p4 = p1 + h k3; k4 = F(a + h, p4); p(a + h) = p1 + h/6 (k1 + 2 k2 + 2 k3 + k4).
  // Mismas expresiones y mismo orden que el bucle de dl_edo_resolver(): p_siguiente es el mismo double.
  function pasoRk4(p1, j, i_media, f_media, r_media, nsub) {
    var h = 1 / nsub;
    var k1 = i_media[j] * (1 - p1) - valorEn(r_media, j) * p1 - f_media[j] * p1 * (1 - p1);
    var p2 = p1 + h / 2 * k1;
    var k2 = i_media[j + 1] * (1 - p2) - valorEn(r_media, j + 1) * p2 - f_media[j + 1] * p2 * (1 - p2);
    var p3 = p1 + h / 2 * k2;
    var k3 = i_media[j + 1] * (1 - p3) - valorEn(r_media, j + 1) * p3 - f_media[j + 1] * p3 * (1 - p3);
    var p4 = p1 + h * k3;
    var k4 = i_media[j + 2] * (1 - p4) - valorEn(r_media, j + 2) * p4 - f_media[j + 2] * p4 * (1 - p4);
    return { j: j, h: h, p1: p1, k1: k1, p2: p2, k2: k2, p3: p3, k3: k3, p4: p4, k4: k4,
      p_siguiente: p1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4) };
  }

  // RK4 sobre la malla h desde p(a0) = p0, con i, f y r en la malla h/2 (n_media = 2 n_pasos + 1 puntos).
  // Devuelve p en la malla h (n_pasos + 1 valores, el primero p0), sin truncar. Gemela: dl_edo_resolver() (misma
  // firma por posición: i_media, f_media, remision, p0, nsub).
  function resolverEdo(i_media, f_media, remision, p0, nsub) {
    if (remision == null) remision = 0;
    if (p0 == null) p0 = 0;
    if (nsub == null) nsub = NSUB;
    var h = 1 / nsub, n_media = i_media.length;
    if (f_media.length !== n_media)
      throw new Error('`f_media` debe tener la longitud de `i_media` (' + n_media + '), no ' + f_media.length);
    var r_media = remisionMedia(remision, n_media);
    var n_pasos = (n_media - 1) >> 1;
    var p_malla = new Array(n_pasos + 1);
    p_malla[0] = p0;
    for (var paso = 0; paso < n_pasos; paso++) {
      var j = 2 * paso, p1 = p_malla[paso];
      var k1 = i_media[j] * (1 - p1) - r_media[j] * p1 - f_media[j] * p1 * (1 - p1);
      var p2 = p1 + h / 2 * k1;
      var k2 = i_media[j + 1] * (1 - p2) - r_media[j + 1] * p2 - f_media[j + 1] * p2 * (1 - p2);
      var p3 = p1 + h / 2 * k2;
      var k3 = i_media[j + 1] * (1 - p3) - r_media[j + 1] * p3 - f_media[j + 1] * p3 * (1 - p3);
      var p4 = p1 + h * k3;
      var k4 = i_media[j + 2] * (1 - p4) - r_media[j + 2] * p4 - f_media[j + 2] * p4 * (1 - p4);
      p_malla[paso + 1] = p1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4);
    }
    return p_malla;
  }

  // p en la malla h e i, f en la malla h/2 -> p, i, f en las edades enteras. Gemela: .dl_a_edades_enteras().
  function aEdadesEnteras(p_malla, i_media, f_media, idx_malla, idx_media) {
    var n = idx_malla.length, p = new Array(n), i = new Array(n), f = new Array(n);
    for (var t = 0; t < n; t++) { p[t] = p_malla[idx_malla[t]]; i[t] = i_media[idx_media[t]]; f[t] = f_media[idx_media[t]]; }
    return { p: p, i: i, f: f };
  }

  function mapa(x, fn) { var out = new Array(x.length); for (var k = 0; k < x.length; k++) out[k] = fn(x[k], k); return out; }

  // theta en los nudos -> p, i, f en las edades enteras. Gemela: dl_edo().
  //   opciones: edad_inicio (obligatoria), edad_fin (99), remision (0: un número, un arreglo con un valor por punto
  //   de la malla h/2, o {valor, por_edad} como la configuración), p0 (0), nsub (5).
  //   log_f_nudos = null: EMR fija en cero (f = 0 exacto, como .dl_log_tasas_media() con log f = -Inf).
  // Devuelve {edades, p, i, f} (lo de dl_edo()) y, para las escenas, las mallas: edades_media, i_media, f_media,
  // r_media, edades_malla y p_malla.
  function edo(log_i_nudos, log_f_nudos, nudos, opciones) {
    opciones = opciones || {};
    var edad_inicio = opciones.edad_inicio;
    if (edad_inicio == null) throw new Error('falta `edad_inicio`');
    var edad_fin = opciones.edad_fin == null ? EDAD_FIN : opciones.edad_fin;
    var nsub = opciones.nsub == null ? NSUB : opciones.nsub;
    var p0 = opciones.p0 == null ? 0 : opciones.p0;
    if (log_i_nudos.length !== nudos.length || (log_f_nudos != null && log_f_nudos.length !== nudos.length))
      throw new Error('`log_i_nudos` y `log_f_nudos` deben traer un valor por nudo (' + nudos.length + ' nudos)');
    var edades_media = edadesMedia(edad_inicio, edad_fin, nsub);
    var W = matrizW(nudos, edades_media);
    var i_media = mapa(productoW(W, log_i_nudos), Math.exp);
    var f_media = log_f_nudos == null ? mapa(edades_media, function () { return 0; })
      : mapa(productoW(W, log_f_nudos), Math.exp);
    var rem = opciones.remision == null ? 0 : opciones.remision;
    var r_media = (typeof rem === 'object' && !Array.isArray(rem)) ? remisionPorEdad(rem, edades_media)
      : remisionMedia(rem, edades_media.length);
    var p_malla = resolverEdo(i_media, f_media, r_media, p0, nsub);
    var anuales = aEdadesEnteras(p_malla, i_media, f_media, idxAnualMalla(edad_fin - edad_inicio, nsub),
      idxAnualMedia(i_media.length, nsub));
    var edades_malla = [];
    for (var j = 0; j < edades_media.length; j += 2) edades_malla.push(edades_media[j]);
    return { edades: enteros(edad_inicio, edad_fin), p: anuales.p, i: anuales.i, f: anuales.f,
      edades_media: edades_media, i_media: i_media, f_media: f_media, r_media: r_media, edades_malla: edades_malla,
      p_malla: p_malla };
  }

  // Solución exacta con i y f constantes y r = 0 (vignettes/el-modelo.qmd, sección 3):
  //   p(t) = i (E - 1) / (i E - f),  E = exp((i - f) t),  t = a - a0.  Con i = f, el límite i t / (1 + i t).
  function prevalenciaExacta(i, f, t) {
    if (i === f) return i * t / (1 + i * t);
    var E = Math.exp((i - f) * t);
    return i * (E - 1) / (i * E - f);
  }

  // ---- Bandas de edad y promedios poblacionales (R/bandas.R) ----------------------------------------------------
  // Una banda es {inicio, fin} = [age_start, age_end) (con age_group_id opcional). `bandas`: bandas finas ordenadas
  // por inicio y sin solapes.

  // Banda de cada edad: índice (base 0) de la banda con inicio <= a < fin; -1 si ninguna (NA en R).
  // Gemela: .dl_banda_de().
  function bandaDe(edades, bandas) {
    var inicios = mapa(bandas, function (b) { return b.inicio; });
    return mapa(edades, function (a) {
      var k = findInterval(a, inicios);
      return (k >= 1 && a < bandas[k - 1].fin) ? k - 1 : -1;
    });
  }

  // Bandas finas: quita toda banda que contiene a otra (incluida otra con los mismos límites) y ordena por inicio.
  // Gemela: .dl_bandas_finas().
  function bandasFinas(limites) {
    var out = limites.filter(function (b, i) {
      return !limites.some(function (c, j) { return j !== i && c.inicio >= b.inicio && c.fin <= b.fin; });
    });
    return out.slice().sort(function (x, y) { return x.inicio - y.inicio; });
  }

  // Pesos w_a del promedio poblacional sobre [inicio, fin). Gemela: .dl_pesos_intervalo().
  //   edades_anual  edades de la malla anual, crecientes;
  //   bandas, N     bandas finas de la población y su población N_b (mismo orden).
  //   A = {a : inicio <= a < fin};  m_a = N_b(a) / n_b(a) (n_b: edades de TODA la malla en la banda b);
  //   w_a = m_a / sum m_a. Si todas las edades de A tienen el mismo n_b, no se divide (como en R).
  // Devuelve {idx (posiciones base 0 en edades_anual), w}. Error si el intervalo no tiene edades o alguna edad no
  // tiene banda o población.
  function pesosIntervalo(edades_anual, inicio, fin, bandas, N) {
    var idx = [];
    for (var k = 0; k < edades_anual.length; k++) if (edades_anual[k] >= inicio && edades_anual[k] < fin) idx.push(k);
    if (!idx.length) throw new Error('el intervalo de edad [' + inicio + ', ' + fin + ') no contiene ninguna edad de la malla anual');
    var banda = bandaDe(mapa(idx, function (k) { return edades_anual[k]; }), bandas);
    if (banda.indexOf(-1) >= 0) throw new Error('edad(es) del intervalo [' + inicio + ', ' + fin + ') sin banda en la población');
    var Nb = mapa(banda, function (b) { return N[b]; });
    if (Nb.some(function (v) { return v == null || isNaN(v); }))
      throw new Error('población sin alguna banda necesaria para el intervalo [' + inicio + ', ' + fin + ')');
    var conteo = mapa(bandas, function () { return 0; });
    bandaDe(edades_anual, bandas).forEach(function (b) { if (b >= 0) conteo[b]++; });
    var n_malla = mapa(banda, function (b) { return conteo[b]; });
    var distintos = n_malla.some(function (v) { return v !== n_malla[0]; });
    if (distintos) Nb = mapa(Nb, function (v, t) { return v / n_malla[t]; });
    var total = suma(Nb);
    return { idx: idx, w: mapa(Nb, function (v) { return v / total; }) };
  }

  // q_j = sum_{a en A_j} w_a x(a) en cada intervalo j (`pesos`: lista de {idx, w}). Gemela: .dl_q_intervalos().
  function qIntervalos(x_anual, pesos) {
    return mapa(pesos, function (pb) { return suma(mapa(pb.idx, function (k, t) { return x_anual[k] * pb.w[t]; })); });
  }

  // Integrando anual de una medida sobre una solución {p, i, f}: 'p' (prevalencia), 'ipop' = i (1 - p) (incidencia
  // por persona-año de toda la población) o 'pf' = p f (mortalidad por la causa). Gemela: .dl_integrando().
  function integrando(sol, nombre) {
    if (nombre === 'p') return sol.p.slice();
    if (nombre === 'ipop') return mapa(sol.i, function (i, a) { return i * (1 - sol.p[a]); });
    if (nombre === 'pf') return mapa(sol.p, function (p, a) { return p * sol.f[a]; });
    throw new Error('integrando desconocido «' + nombre + '»');
  }

  // Promedio poblacional de x_anual sobre [inicio, fin) (pesosIntervalo + qIntervalos de una banda).
  function promedioBanda(x_anual, edades_anual, inicio, fin, bandas, N) {
    return qIntervalos(x_anual, [pesosIntervalo(edades_anual, inicio, fin, bandas, N)])[0];
  }

  // ---- Álgebra del ancla -----------------------------------------------------------------------------------------

  // sd en escala log que implica un intervalo del 95 %: (log superior - log inferior) / 3.92 (columna sigma_log de
  // R/insumos.R; también sd_T de R/betas.R con T = log).
  function sigmaLog(inferior, superior) { return (Math.log(superior) - Math.log(inferior)) / ANCHO_IC95_EN_SD; }

  // Producto exacto a * b = p + e (Dekker, con la partición de Veltkamp; sin FMA).
  function productoExacto(a, b) {
    var p = a * b, c = 134217729 * a, ah = c - (c - a), al = a - ah;
    c = 134217729 * b;
    var bh = c - (c - b), bl = b - bh;
    return [p, ((ah * bh - p) + ah * bl + al * bh) + al * bl];
  }

  // x^k de R con k entero >= 0 (R_POW de arithmetic.c): x * x si k = 2, 1 si k = 0 o x = 1 y, si no, powl(x, k) en
  // long double redondeado a double. powl se imita con aritmética doble-doble (error ~k 2^-106): el resultado es el
  // double más cercano a x^k, el mismo de R salvo un doble redondeo excepcional.
  function potenciaR(x, k) {
    if (k === 2) return x * x;
    if (k === 0 || x === 1) return 1;
    if (k === 1) return x;
    if (!isFinite(x) || k < 0 || k !== Math.floor(k)) return Math.pow(x, k);
    var hi = x, lo = 0;
    for (var t = 1; t < k; t++) {
      var pe = productoExacto(hi, x), e = pe[1] + lo * x;
      hi = pe[0] + e; lo = e - (hi - pe[0]);
    }
    return hi + lo;
  }

  // Correlación AR(1) entre n bandas: R_bc = rho^|b - c| (la distancia en bandas, no en años).
  function correlacionAr1(n, rho) {
    var R = [];
    for (var b = 0; b < n; b++) { var fila = new Array(n); for (var c = 0; c < n; c++) fila[c] = potenciaR(rho, Math.abs(b - c)); R.push(fila); }
    return R;
  }

  // Cholesky: U triangular superior con A = U'U (chol() de R). Error si A no es definida positiva.
  function cholesky(A) {
    var n = A.length, U = [];
    for (var b = 0; b < n; b++) { var fila = new Array(n); for (var c = 0; c < n; c++) fila[c] = 0; U.push(fila); }
    for (var j = 0; j < n; j++) {
      var s = A[j][j];
      for (var k = 0; k < j; k++) s -= U[k][j] * U[k][j];
      if (!(s > 0)) throw new Error('la matriz no es definida positiva (cholesky, columna ' + (j + 1) + ')');
      var ujj = Math.sqrt(s);
      U[j][j] = ujj;
      for (c = j + 1; c < n; c++) {
        var t = A[j][c];
        for (k = 0; k < j; k++) t -= U[k][j] * U[k][c];
        U[j][c] = t / ujj;
      }
    }
    return U;
  }

  // chol(R) de la correlación AR(1). Gemela: .dl_chol_ar1().
  function cholAr1(n, rho) { return cholesky(correlacionAr1(n, rho)); }

  // Sustitución hacia adelante: u con U' u = b (backsolve(U, b, transpose = TRUE), como la dtrsm de referencia).
  function resolverTranspuesta(U, b) {
    var n = b.length, u = new Array(n);
    for (var i = 0; i < n; i++) {
      var t = b[i];
      for (var k = 0; k < i; k++) t = t - U[k][i] * u[k];
      u[i] = t / U[i][i];
    }
    return u;
  }

  // log MVN(z; 0, Sigma) con Sigma una matriz de covarianza cualquiera (Cholesky de Sigma):
  //   -1/2 [z' Sigma^-1 z + log det Sigma + n log(2 pi)].
  function logMvn(z, Sigma) {
    var U = cholesky(Sigma), u = resolverTranspuesta(U, z), n = z.length;
    var quad = suma(mapa(u, function (v) { return v * v; }));
    var logdet = 2 * suma(mapa(U, function (fila, b) { return Math.log(fila[b]); }));
    return -0.5 * (quad + logdet + n * Math.log(2 * Math.PI));
  }

  // log MVN(z; 0, Sigma / lambda), Sigma = diag(sigma_log) R diag(sigma_log), R = U'U (U = chol_R):
  //   -1/2 [lambda |u|^2 + log det Sigma - n log lambda + n log(2 pi)],  U' u = z / sigma_log,
  //   log det Sigma = 2 sum log diag(U) + 2 sum log sigma_log.  Gemela: .dl_lp_mvn_ar1().
  function lpMvnAr1(z, sigma_log, chol_R, lambda) {
    var n = z.length;
    var u = resolverTranspuesta(chol_R, mapa(z, function (v, b) { return v / sigma_log[b]; }));
    var quad = suma(mapa(u, function (v) { return v * v; }));
    var logdet = 2 * suma(mapa(chol_R, function (fila, b) { return Math.log(fila[b]); })) +
      2 * suma(mapa(sigma_log, Math.log));
    return -0.5 * (lambda * quad + logdet - n * Math.log(lambda) + n * Math.log(2 * Math.PI));
  }

  // ---- Prior de la EMR (R/emr.R) ---------------------------------------------------------------------------------

  // Prior de log f de una banda desde el ancla (f = csmr / p):
  //   mu_log = log(csmr) + log1p(-fraccion_aguda) - log(p);  sd_log = factor_sd sqrt(sigma_log_p^2 + sigma_log_csmr^2).
  // Gemela: una fila de .dl_prior_emr_tabla().
  function priorEmrBanda(csmr, prev, sigma_log_csmr, sigma_log_prev, fraccion_aguda, factor_sd) {
    if (fraccion_aguda == null) fraccion_aguda = 0;
    if (factor_sd == null) factor_sd = 1;
    return { mu_log: Math.log(csmr) + Math.log1p(-fraccion_aguda) - Math.log(prev),
      sd_log: factor_sd * Math.sqrt(sigma_log_prev * sigma_log_prev + sigma_log_csmr * sigma_log_csmr) };
  }

  // Techo de la EMR derivado del ancla: [0, K max(exp(mu_log))] sobre sexos y bandas. Gemela: .dl_techo_emr()
  // (rama «derivado»).
  function techoEmr(mu_log, k_techo) {
    if (k_techo == null) k_techo = EMR_TECHO_K;
    var emr_max = Math.max.apply(null, mapa(mu_log, Math.exp));
    if (!isFinite(emr_max) || emr_max <= 0) throw new Error('no se puede derivar el techo de la mortalidad en exceso');
    return { cota: [0, k_techo * emr_max], emr_max_ancla: emr_max, k: k_techo };
  }

  // Prior informativo en los nudos desde las bandas de un sexo ([{inicio, fin, mu_log, sd_log}]): cada nudo toma
  // la banda que lo contiene; uno fuera de la cobertura, la primera o la última con sd x 2. Gemela: .dl_emr_en_nudos().
  function emrEnNudos(tabla, nudos, cota) {
    var bandas = tabla.slice().sort(function (x, y) { return x.inicio - y.inicio; });
    var banda = bandaDe(nudos, bandas), nb = bandas.length;
    banda = mapa(banda, function (b, k) { return b >= 0 ? b : (nudos[k] < bandas[0].inicio ? 0 : nb - 1); });
    return {
      mu_log: mapa(banda, function (b) { return bandas[b].mu_log; }),
      sd_log: mapa(banda, function (b, k) {
        var fuera = nudos[k] < bandas[0].inicio || nudos[k] >= bandas[nb - 1].fin;
        return bandas[b].sd_log * (fuera ? EMR_SD_FUERA_DE_BANDAS : 1);
      }),
      cota: cota.slice(), plano: false, cero: false };
  }

  // Prior dado por tabla en los nudos (cada nudo, el tramo que lo contiene). Gemela: .dl_emr_dado_en_nudos().
  function emrDadoEnNudos(tabla, nudos, cota) {
    var tramos = tabla.slice().sort(function (x, y) { return x.inicio - y.inicio; });
    var tramo = bandaDe(nudos, tramos);
    return { mu_log: mapa(tramo, function (t) { return tramos[t].mu_log; }),
      sd_log: mapa(tramo, function (t) { return tramos[t].sd_log; }), cota: cota.slice(), plano: false, cero: false };
  }

  // Prior plano bajo el techo: f_min = max(cota[0], f_max / 1000); mu_log = log(f_max) - 2 (solo el punto de
  // partida); sin término en la log-posterior. Gemela: .dl_emr_plano().
  function emrPlano(nudos, cota) {
    var c = [Math.max(cota[0], cota[1] / EMR_PLANO_RANGO), cota[1]];
    return { mu_log: mapa(nudos, function () { return Math.log(c[1]) - EMR_PLANO_INICIO_BAJO_TECHO; }),
      sd_log: mapa(nudos, function () { return NaN; }), cota: c, plano: true, cero: false };
  }

  // EMR fija en cero: sin log f en theta, sin término ni cota. Gemela: .dl_emr_cero().
  function emrCero() { return { mu_log: [], sd_log: [], cota: [0, 0], plano: true, cero: true }; }

  // ---- Términos de la log-posterior (R/verosimilitud.R) ----------------------------------------------------------

  // TRUE si algún f = exp(log f) de los nudos cae fuera de [cota[0], cota[1]]. Gemela: .dl_f_fuera_de_cota().
  function fFueraDeCota(log_f_nudos, cota) {
    for (var k = 0; k < log_f_nudos.length; k++) {
      var f = Math.exp(log_f_nudos[k]);
      if (f < cota[0] || f > cota[1]) return true;
    }
    return false;
  }

  // Segundas diferencias de log i en los nudos: D2_k = (x_k+2 - x_k+1) - (x_k+1 - x_k) (diff(diff(x)) de R).
  function segundasDiferencias(x) {
    var d1 = [], d2 = [];
    for (var k = 0; k + 1 < x.length; k++) d1.push(x[k + 1] - x[k]);
    for (k = 0; k + 1 < d1.length; k++) d2.push(d1[k + 1] - d1[k]);
    return d2;
  }

  // L_suavidad = sum_k log N(D2_k; 0, sigma). Gemela: .dl_lp_suavidad().
  function lpSuavidad(log_i_nudos, sigma) {
    return suma(mapa(segundasDiferencias(log_i_nudos), function (d) { return dnormLog(d, 0, sigma); }));
  }

  // L_emr sin la cota: 0 con el prior plano; si no, sum_k log N(log f_k; mu_log_k, sd_log_k).
  // Gemela: .dl_lp_prior_emr().
  function lpPriorEmr(log_f_nudos, emr) {
    if (emr.plano) return 0;
    return suma(mapa(log_f_nudos, function (x, k) { return dnormLog(x, emr.mu_log[k], emr.sd_log[k]); }));
  }

  // L_emr con todo lo que la log-posterior hace con log f: 0 con la EMR fija en cero (log_f_nudos se ignora);
  // -Infinity si algún f de los nudos sale de la cota (el techo); si no, lpPriorEmr (0 con el prior plano).
  function lpEmr(log_f_nudos, emr) {
    if (emr.cero) return 0;
    if (fFueraDeCota(log_f_nudos, emr.cota)) return -Infinity;
    return lpPriorEmr(log_f_nudos, emr);
  }

  // L_ancla = log MVN(z; 0, Sigma / lambda), z_b = log q_b - log val_b. ancla: {val, sigma_log, chol_R, lambda}.
  // -Infinity si algún q no es finito o es <= 0 (el control de .dl_lp_componentes()). Gemela: .dl_lp_ancla().
  function lpAncla(q_ancla, ancla) {
    for (var b = 0; b < q_ancla.length; b++) if (!isFinite(q_ancla[b]) || q_ancla[b] <= 0) return -Infinity;
    var z = mapa(q_ancla, function (q, b) { return Math.log(q) - Math.log(ancla.val[b]); });
    return lpMvnAr1(z, ancla.sigma_log, ancla.chol_R, ancla.lambda);
  }

  // l_d de un dato. Gemelas: .dl_lp_lognormal(), .dl_lp_binomial() y .dl_lp_poisson().
  //   log-normal  log N(log(q + eta); log(val + eta), s_log)
  //   binomial    x log q + (n - x) log(1 - q)          (sin lchoose(n, x))
  //   Poisson     x log(n q) - n q                      (sin -lgamma(x + 1))
  function lpLognormal(q, val, s_log, eta) { return dnormLog(Math.log(q + eta), Math.log(val + eta), s_log); }
  function lpBinomial(q, x, n) { return x * Math.log(q) + (n - x) * Math.log1p(-q); }
  function lpPoisson(q, x, n) { return x * Math.log(n * q) - n * q; }

  // L_datos por tipo: {datos_prev_estudio, datos_incidencia, datos_csmr}, con la solución sol = {p, i, f} anual y
  // los datos del contexto preparado (ctx.datos_por_tipo). Algún q no finito o <= 0 deja el tipo en -Infinity.
  // Gemela: .dl_lp_datos().
  function lpDatos(sol, ctx) {
    var out = {};
    for (var t = 0; t < TIPOS_DATO.length; t++) {
      var nombre = 'datos_' + TIPOS_DATO[t].tipo, d = ctx.datos_por_tipo[t];
      out[nombre] = 0;
      if (!d) continue;
      var q = qIntervalos(integrando(sol, d.integrando), d.pesos);
      if (q.some(function (v) { return !isFinite(v) || v <= 0; })) { out[nombre] = -Infinity; continue; }
      var l = mapa(q, function (qd, k) {
        if (d.ruta[k] === RUTA_LOGNORMAL) return lpLognormal(qd, d.val[k], d.s_log[k], d.eta[k]);
        return d.familia_conteos === 'binomial' ? lpBinomial(qd, d.x[k], d.n_ef[k]) : lpPoisson(qd, d.x[k], d.n_ef[k]);
      });
      out[nombre] = suma(l);
    }
    return out;
  }

  // ---- Contexto de un sexo ---------------------------------------------------------------------------------------
  // El contexto JSON (lo que escribe un exportador en R desde .dl_ctx(); ver pruebas/fixtures.R) tiene:
  //   nudos, edad_inicio, edad_fin, nsub, sigma_suavidad
  //   remision    {valor, por_edad: [{edad_inicio, edad_fin, valor}]}     (opcional si viene r_media)
  //   r_media     r en la malla h/2 (opcional; si falta, sale de `remision`)
  //   emr         {cero, plano, mu_log, sd_log, cota: [f_min, f_max]}
  //   ancla       {inicio, fin, val, sigma_log, lambda, rho}               bandas del ancla de prevalencia, por edad
  //   poblacion   {inicio, fin, N}                                        bandas finas y su población (ubicación
  //                                                                       del ancla, el sexo)
  //   datos       {tipo, inicio, fin, ruta, val, s_log, eta, x, n}        datos locales en el ajuste (columnas;
  //                                                                       tipo prev_estudio | incidencia | csmr,
  //                                                                       n = n_ef, ruta 0 log-normal | 1 conteos)
  //   pesos_ancla, pesos_datos   [{idx (base 0), w}] (opcionales; si faltan, salen de `poblacion`)
  // prepararCtx() completa lo que R guarda en .dl_ctx(): edades_media, anual, W, idx_anual_malla, idx_anual_media,
  // r_media, ancla.chol_R, pesos y datos_por_tipo. logPosterior() acepta el contexto JSON o el preparado; con
  // muchas evaluaciones conviene preparar una vez.

  function columnasAFilas(cols, nombres) {
    var n = cols[nombres[0]] ? cols[nombres[0]].length : 0, filas = [];
    for (var k = 0; k < n; k++) { var f = {}; nombres.forEach(function (nm) { f[nm] = cols[nm] ? cols[nm][k] : undefined; }); filas.push(f); }
    return filas;
  }

  function prepararCtx(ctx) {
    if (ctx.preparado === true) return ctx;
    var nsub = ctx.nsub == null ? NSUB : ctx.nsub, edad_fin = ctx.edad_fin == null ? EDAD_FIN : ctx.edad_fin;
    var edades_media = edadesMedia(ctx.edad_inicio, edad_fin, nsub), anual = enteros(ctx.edad_inicio, edad_fin);
    var bandas_pobl = ctx.poblacion ? columnasAFilas(ctx.poblacion, ['inicio', 'fin']) : null;
    var N = ctx.poblacion ? ctx.poblacion.N : null;
    var pesos = function (inicio, fin) {
      return mapa(inicio, function (a, k) { return pesosIntervalo(anual, a, fin[k], bandas_pobl, N); });
    };
    var nb = ctx.ancla.val.length;
    var ancla = { inicio: ctx.ancla.inicio, fin: ctx.ancla.fin, val: ctx.ancla.val, sigma_log: ctx.ancla.sigma_log,
      lambda: ctx.ancla.lambda, rho: ctx.ancla.rho,
      chol_R: ctx.ancla.chol_R || cholAr1(nb, ctx.ancla.rho) };
    var datos = ctx.datos || { tipo: [] };
    var nd = datos.tipo ? datos.tipo.length : 0;
    var pesos_datos = ctx.pesos_datos || (nd ? pesos(datos.inicio, datos.fin) : []);
    var datos_por_tipo = mapa(TIPOS_DATO, function (td) {
      var idx = [];
      for (var k = 0; k < nd; k++) if (datos.tipo[k] === td.tipo) idx.push(k);
      if (!idx.length) return null;
      var tomar = function (col) { return mapa(idx, function (k) { return col ? col[k] : NaN; }); };
      return { integrando: td.integrando, familia_conteos: td.familia_conteos,
        pesos: mapa(idx, function (k) { return pesos_datos[k]; }), ruta: tomar(datos.ruta), val: tomar(datos.val),
        s_log: tomar(datos.s_log), eta: tomar(datos.eta), x: tomar(datos.x), n_ef: tomar(datos.n) };
    });
    var emr = ctx.emr;
    return {
      preparado: true, nudos: ctx.nudos, edad_inicio: ctx.edad_inicio, edad_fin: edad_fin, nsub: nsub,
      sigma_suavidad: ctx.sigma_suavidad, edades_media: edades_media, anual: anual,
      W: matrizW(ctx.nudos, edades_media), idx_anual_malla: idxAnualMalla(edad_fin - ctx.edad_inicio, nsub),
      idx_anual_media: idxAnualMedia(edades_media.length, nsub),
      r_media: ctx.r_media ? ctx.r_media.slice() : remisionPorEdad(ctx.remision, edades_media),
      emr: { cero: !!emr.cero, plano: !!emr.plano, mu_log: emr.mu_log || [], sd_log: emr.sd_log || [],
        cota: emr.cota },
      ancla: ancla, poblacion: ctx.poblacion, bandas_pobl: bandas_pobl,
      pesos_ancla: ctx.pesos_ancla || pesos(ctx.ancla.inicio, ctx.ancla.fin),
      datos: datos, pesos_datos: pesos_datos, datos_por_tipo: datos_por_tipo
    };
  }

  // theta -> log i, log f en la malla h/2 (W theta). Con la EMR fija en cero, log f = -Infinity (f = 0 exacto).
  // Gemela: .dl_log_tasas_media().
  function logTasasMedia(theta, ctx) {
    ctx = prepararCtx(ctx);
    var K = ctx.nudos.length;
    var log_i = productoW(ctx.W, theta.slice(0, K));
    if (ctx.emr.cero) return { log_i: log_i, log_f: mapa(log_i, function () { return -Infinity; }) };
    return { log_i: log_i, log_f: productoW(ctx.W, theta.slice(K, 2 * K)) };
  }

  // log i, log f en la malla h/2 -> p, i, f anuales con la malla y r del contexto; con techo_f, log f se recorta a
  // log(techo_f) (truncado = 1 si algún punto lo superaba). Gemela: .dl_edo_log_media().
  function edoLogMedia(log_i_media, log_f_media, ctx, techo_f) {
    ctx = prepararCtx(ctx);
    var truncado = 0;
    if (techo_f != null) {
      var lt = Math.log(techo_f);
      if (log_f_media.some(function (v) { return v > lt; })) {
        truncado = 1; log_f_media = mapa(log_f_media, function (v) { return Math.min(v, lt); });
      }
    }
    var i_media = mapa(log_i_media, Math.exp), f_media = mapa(log_f_media, Math.exp);
    var p_malla = resolverEdo(i_media, f_media, ctx.r_media, 0, ctx.nsub);
    var an = aEdadesEnteras(p_malla, i_media, f_media, ctx.idx_anual_malla, ctx.idx_anual_media);
    return { p: an.p, i: an.i, f: an.f, truncado: truncado, i_media: i_media, f_media: f_media, p_malla: p_malla };
  }

  // theta -> p, i, f en las edades enteras del contexto. Gemela: .dl_edo_ctx().
  function edoCtx(theta, ctx) {
    ctx = prepararCtx(ctx);
    var lm = logTasasMedia(theta, ctx);
    return edoLogMedia(lm.log_i, lm.log_f, ctx);
  }

  function salida(valores) { var o = {}; LP_NOMBRES.forEach(function (nm, k) { o[nm] = valores[k]; }); return o; }

  // log posterior(theta) por términos, con los nombres de LP_NOMBRES (suavidad, emr, ancla, datos, total y datos
  // por tipo). total = suavidad + emr + ancla + datos, en ese orden.
  //   Algún f de los nudos fuera de la cota: todos -Infinity. Algún q del ancla no finito o <= 0: ancla, datos,
  //   total y datos por tipo -Infinity. EMR fija en cero: theta solo trae log i, emr = 0 y no hay cota.
  // Gemela: .dl_lp_componentes().
  function lpComponentes(theta, ctx) {
    ctx = prepararCtx(ctx);
    var K = ctx.nudos.length, cero = ctx.emr.cero;
    var log_i_nudos = theta.slice(0, K), log_f_nudos = null;
    if (!cero) {
      log_f_nudos = theta.slice(K, 2 * K);
      if (fFueraDeCota(log_f_nudos, ctx.emr.cota)) return salida(mapa(LP_NOMBRES, function () { return -Infinity; }));
    }
    var sol = edoCtx(theta, ctx);
    var lp_suav = lpSuavidad(log_i_nudos, ctx.sigma_suavidad);
    var lp_emr = cero ? 0 : lpPriorEmr(log_f_nudos, ctx.emr);
    var q_ancla = qIntervalos(sol.p, ctx.pesos_ancla);
    if (q_ancla.some(function (q) { return !isFinite(q) || q <= 0; }))
      return salida([lp_suav, lp_emr, -Infinity, -Infinity, -Infinity, -Infinity, -Infinity, -Infinity]);
    var lp_ancla = lpAncla(q_ancla, ctx.ancla);
    var por_tipo = lpDatos(sol, ctx);
    var vp = [por_tipo.datos_prev_estudio, por_tipo.datos_incidencia, por_tipo.datos_csmr];
    var lp_datos = suma(vp);
    var lp_total = lp_suav + lp_emr + lp_ancla + lp_datos;
    return salida([lp_suav, lp_emr, lp_ancla, lp_datos, lp_total, vp[0], vp[1], vp[2]]);
  }

  // log posterior(theta) total (lo que evalúa el muestreador). Gemela: .dl_log_post().
  function logPosterior(theta, ctx) { return lpComponentes(theta, ctx).total; }

  // Punto inicial de las cadenas: log i = -6 en los nudos; log f = min(mu_log, log(techo) - 0.5).
  // Gemela: .dl_theta_inicial() (sin los nombres).
  function thetaInicial(ctx) {
    var K = ctx.nudos.length, theta = [];
    for (var k = 0; k < K; k++) theta.push(LOG_I_INICIAL);
    if (ctx.emr.cero) return theta;
    var tope = Math.log(ctx.emr.cota[1]) - MARGEN_LOG_F_INICIAL;
    for (k = 0; k < K; k++) theta.push(Math.min(ctx.emr.mu_log[k], tope));
    return theta;
  }

  // ---- Muestreo y diagnósticos (R/muestreador.R) -----------------------------------------------------------------
  // Una cadena es una matriz de simulaciones: arreglo de filas (una por simulación guardada), cada fila un arreglo
  // con un valor por parámetro. Una cadena de un solo parámetro puede darse como arreglo de números.

  function comoMatriz(cadena) { return typeof cadena[0] === 'number' ? mapa(cadena, function (v) { return [v]; }) : cadena; }

  // R-hat partido de varias cadenas (un valor por parámetro). Gemela: dl_rhat().
  //   n = floor(filas / 2) por mitad; var_entre = n Var(medias de las mitades); var_dentro = media de sus
  //   varianzas; R-hat = sqrt(((n - 1) / n var_dentro + var_entre / n) / var_dentro).
  function rhat(cadenas) {
    cadenas = mapa(cadenas, comoMatriz);
    var n = Math.floor(cadenas[0].length / 2), d = cadenas[0][0].length, out = [];
    var mitades = [];
    cadenas.forEach(function (c) { mitades.push(c.slice(0, n)); mitades.push(c.slice(n, 2 * n)); });
    for (var j = 0; j < d; j++) {
      var medias = mapa(mitades, function (m) { return media(columna(m, j)); });
      var varianzas = mapa(mitades, function (m) { return varianza(columna(m, j)); });
      var var_entre = n * varianza(medias), var_dentro = media(varianzas);
      out.push(Math.sqrt(((n - 1) / n * var_dentro + var_entre / n) / var_dentro));
    }
    return out;
  }

  // Autocorrelaciones de rezago 1..lag_max de una serie, como stats::acf(x, lag.max, plot = FALSE)$acf[-1]:
  // centrada en su media (colMeans), sumas simples en double con divisor n, y acotada a [-1, 1].
  function autocorrelacion(x, lag_max) {
    var n = x.length, m = suma(x) / n, xc = mapa(x, function (v) { return v - m; }), acf = [];
    for (var lag = 0; lag <= lag_max; lag++) {
      var s = 0;
      for (var i = 0; i < n - lag; i++) s += xc[i + lag] * xc[i];
      acf.push(s / n);
    }
    var se = Math.sqrt(acf[0]), out = [];
    for (lag = 1; lag <= lag_max; lag++) { var a = acf[lag] / (se * se); out.push(a > 1 ? 1 : (a < -1 ? -1 : a)); }
    return out;
  }

  // Suma truncada tipo Geyer de autocor_1..autocor_tmax: pares P_k = autocor_{2k-1} + autocor_{2k} hasta el primer
  // par negativo (que no se suma). Gemela: .dl_suma_geyer().
  function sumaGeyer(autocor) {
    var s = 0;
    for (var k = 0; k + 1 < autocor.length; k += 2) {
      var par = autocor[k] + autocor[k + 1];
      if (par < 0) break;
      s = s + par;
    }
    return s;
  }

  // Tamaño efectivo de muestra (un valor por parámetro): n_cadenas n_sim / (1 + 2 sumaGeyer(autocor)), con autocor
  // el promedio entre cadenas de la autocorrelación hasta t_max = min(max_lag, n_sim - 2). Gemela: dl_ess().
  function ess(cadenas, max_lag) {
    if (max_lag == null) max_lag = 200;
    cadenas = mapa(cadenas, comoMatriz);
    var nc = cadenas.length, n_sim = cadenas[0].length, d = cadenas[0][0].length, out = [];
    if (n_sim < 4) throw new Error('cada cadena necesita al menos 4 simulaciones');
    var t_max = Math.min(max_lag, n_sim - 2);
    for (var j = 0; j < d; j++) {
      var acfs = mapa(cadenas, function (c) { return autocorrelacion(columna(c, j), t_max); });
      var autocor = [];
      for (var t = 0; t < t_max; t++) autocor.push(suma(mapa(acfs, function (a) { return a[t]; })) / nc);
      out.push(nc * n_sim / (1 + 2 * sumaGeyer(autocor)));
    }
    return out;
  }

  // Escala inicial de la propuesta de un bloque de dimensión d: 2.38 / sqrt(d). Gemela: .dl_mh_escala_inicial().
  function escalaInicial(d) { return MH_ESCALA_OPTIMA / Math.sqrt(d); }

  // Paso de Robbins-Monro al cerrar la ventana k: s exp((a - 0.28) / sqrt(k)). Gemela: .dl_mh_escala_adaptada().
  function escalaAdaptada(escala, aceptacion, k) { return escala * Math.exp((aceptacion - MH_ACEPTACION_OBJETIVO) / Math.sqrt(k)); }

  // Forma de la propuesta: L triangular inferior con L L' = Cov + 1e-8 I (filas = iteraciones del calentamiento,
  // columnas = parámetros del bloque). Gemela: .dl_mh_forma_propuesta().
  function formaPropuesta(theta_b) {
    var C = covarianza(theta_b);
    for (var a = 0; a < C.length; a++) C[a][a] = C[a][a] + MH_JITTER_COVARIANZA;
    return transponer(cholesky(C));
  }

  function transponer(A) {
    var T = [];
    for (var c = 0; c < A[0].length; c++) { var fila = new Array(A.length); for (var r = 0; r < A.length; r++) fila[r] = A[r][c]; T.push(fila); }
    return T;
  }

  // Propuesta de un bloque: theta' = theta salvo theta'[idx] = theta[idx] + escala L z (z: normales estándar que
  // da la escena; L = null es la identidad). Como dl_mh(): escala * (L %*% z).
  function propuestaMh(theta, idx, escala, L, z) {
    var prop = theta.slice();
    for (var a = 0; a < idx.length; a++) {
      var s = 0;
      if (L) for (var b = 0; b < z.length; b++) s = s + L[a][b] * z[b]; else s = z[a];
      prop[idx[a]] = prop[idx[a]] + escala * s;
    }
    return prop;
  }

  // Regla de aceptación: log u < lp(theta') - lp(theta) (u uniforme en (0, 1) que da la escena). Una propuesta con
  // lp = -Infinity siempre se rechaza.
  function aceptaMh(lp_propuesta, lp_actual, u) { return Math.log(u) < lp_propuesta - lp_actual; }

  // Semilla derivada de (semilla, cadena): (s 1000 + c + desplazamiento) mod (2^31 - 1). Gemela:
  // .dl_semilla_derivada().
  function semillaDerivada(semilla, cadena, desplazamiento) {
    var v = (semilla * MH_FACTOR_SEMILLA + cadena + desplazamiento) % MH_MODULO_SEMILLA;
    if (v < 0) v += MH_MODULO_SEMILLA;
    return Math.trunc(v);
  }

  // ---- Estimación subnacional (R/cascada.R, R/betas.R) -----------------------------------------------------------

  // Factor de renormalización de una banda fina y una simulación: N_nac q_nac / sum_d N_d q_d (q por ubicación ya
  // promediada en la banda). Gemela: el cálculo de `factores` en .dl_renormalizar().
  function renormalizar(N_nac, q_nac, N_d, q_d) {
    return suma([q_nac * N_nac]) / suma(mapa(q_d, function (q, k) { return q * N_d[k]; }));
  }

  // Renormalización de una banda fina: q de cada ubicación = promedio simple (mean de R) de sus edades anuales en la
  // banda; factor = renormalizar(); q_d(a) <- q_d(a) factor en cada edad. q_nac_edades: valores anuales nacionales
  // en la banda; q_d_edades: uno de esos arreglos por ubicación subnacional (orden de N_d).
  // Devuelve {factor, q_nac, q_d, q_d_edades} (q_d_edades ya renormalizadas). Gemela: .dl_renormalizar().
  function renormalizarBanda(q_nac_edades, N_nac, q_d_edades, N_d) {
    var q_nac = media(q_nac_edades), q_d = mapa(q_d_edades, media);
    var factor = renormalizar(N_nac, q_nac, N_d, q_d);
    return { factor: factor, q_nac: q_nac, q_d: q_d,
      q_d_edades: mapa(q_d_edades, function (fila) { return mapa(fila, function (v) { return v * factor; }); }) };
  }

  // Pesos P (bandas x edades) de las bandas del proxy [{inicio, fin}] sobre `edades`: dX(a) = sum_b dX_b P[b][a].
  //   interp 'escalon': 1 en la banda que contiene la edad; 'lineal' (por defecto): interpolación lineal entre los
  //   puntos medios (inicio + min(fin, 85)) / 2, constante fuera de los medios extremos, solo en
  //   [inicio de la primera, fin de la última). fuera 'cero' (por defecto): 0 fuera de las bandas; 'vecina': la banda
  //   más próxima (en un empate, la más joven). Gemela: .dl_pesos_edad().
  function pesosEdad(edades, bandas, fuera, interp) {
    fuera = fuera || 'cero'; interp = interp || 'lineal';
    var lim = bandas.slice().sort(function (x, y) { return x.inicio - y.inicio; }), nb = lim.length;
    var idx = bandaDe(edades, lim);
    if (fuera === 'vecina')
      idx = mapa(idx, function (b, k) {
        if (b >= 0) return b;
        var mejor = -1, dmin = Infinity;
        for (var c = 0; c < nb; c++) {
          var dist = Math.max(lim[c].inicio - edades[k], edades[k] - (lim[c].fin - 1), 0);
          if (-dist > -dmin) { dmin = dist; mejor = c; }
        }
        return mejor;
      });
    var P = [];
    for (var b = 0; b < nb; b++) { var fila = new Array(edades.length); for (var a = 0; a < edades.length; a++) fila[a] = 0; P.push(fila); }
    idx.forEach(function (bb, a) { if (bb >= 0) P[bb][a] = 1; });
    if (interp !== 'escalon' && nb > 1) {
      var medio = mapa(lim, function (l) { return (l.inicio + Math.min(l.fin, CASCADA_EDAD_CIERRE_BANDA_ABIERTA)) / 2; });
      for (a = 0; a < edades.length; a++) {
        if (!(edades[a] >= lim[0].inicio && edades[a] < lim[nb - 1].fin)) continue;
        var x = Math.min(Math.max(edades[a], medio[0]), medio[nb - 1]);
        var izq = Math.min(findInterval(x, medio), nb - 1);          // 1..nb-1, como en R
        var w = (x - medio[izq - 1]) / (medio[izq] - medio[izq - 1]);
        for (b = 0; b < nb; b++) P[b][a] = 0;
        P[izq - 1][a] = 1 - w; P[izq][a] = w;
      }
    }
    return P;
  }

  // Desplazamiento en log de una covariable en una ubicación y una simulación, en cada edad de P:
  //   delta(a) = (kappa beta) sum_b dX_b P[b][a]  (el producto dX %*% P de R, en el orden de las bandas).
  // Gemela: .dl_desplazamiento_log() (una fila).
  function desplazamientoLog(dX_bandas, P, beta, kappa) {
    var nA = P[0].length, out = new Array(nA), kb = kappa * beta;
    for (var a = 0; a < nA; a++) {
      var s = 0;
      for (var b = 0; b < P.length; b++) s = s + P[b][a] * dX_bandas[b];
      out[a] = kb * s;
    }
    return out;
  }

  // T(x) de una covariable: 'log', 'logit' (log(x / (1 - x))) o 'lineal'. Gemela: .dl_transformar().
  function transformar(x, tipo) {
    if (tipo === 'log') return Math.log(x);
    if (tipo === 'logit') return Math.log(x / (1 - x));
    if (tipo === 'lineal') return x;
    throw new Error('transformación desconocida «' + tipo + '»');
  }

  // sd_T = (T(upper) - T(lower)) / 3.92; 0 sin intervalo o con upper <= lower. Gemela: .dl_sd_transformada().
  function sdTransformada(lower, upper, tipo) {
    if (lower == null || upper == null || isNaN(lower) || isNaN(upper) || upper <= lower) return 0;
    return (transformar(upper, tipo) - transformar(lower, tipo)) / ANCHO_IC95_EN_SD;
  }

  // sd de T(X) por el método delta desde el error estándar se de X. Gemela: .dl_sd_delta().
  function sdDelta(x, se, tipo) {
    if (tipo === 'log') return se / x;
    if (tipo === 'logit') return se / (x * (1 - x));
    if (tipo === 'lineal') return se;
    throw new Error('transformación desconocida «' + tipo + '»');
  }

  // ---- AVD (R/avd.R) ---------------------------------------------------------------------------------------------

  // Beta por momentos desde la media y el intervalo del 95 %: v = ((superior - inferior) / 3.92)^2,
  // forma1 = media (media (1 - media) / v - 1), forma2 = (1 - media) forma1 / media. Sin Beta (el elemento es
  // constante en su media): {constante, motivo} con motivo 'degenerado' (media <= 0, media >= 1 o v < 1e-12) o
  // 'infactible' (v >= media (1 - media); R avisa). alfa y beta son forma1 y forma2. Gemela: .dl_beta_momentos().
  function betaMomentos(media_, inferior, superior) {
    var r = (superior - inferior) / ANCHO_IC95_EN_SD, v = r * r;
    if (media_ <= 0 || media_ >= 1 || v < VARIANZA_MINIMA_BETA) return { constante: media_, motivo: 'degenerado', v: v };
    if (v >= media_ * (1 - media_)) return { constante: media_, motivo: 'infactible', v: v };
    var forma1 = media_ * (media_ * (1 - media_) / v - 1);
    var forma2 = (1 - media_) * forma1 / media_;
    return { forma1: forma1, forma2: forma2, alfa: forma1, beta: forma2, v: v };
  }

  // pi_e <- pi_e / sum_e pi_e (una simulación). Gemela: pi_muestra / rowSums(pi_muestra) en .dl_simular_pi_dw().
  function renormalizarProporciones(pi) { var s = suma(pi); return mapa(pi, function (v) { return v / s; }); }

  // AVD de una banda y una simulación: p_banda x sum_e pi_e DW_e x COMO (COMO = 1 si se omite).
  // Gemela: .dl_avd_bandas() con suma_pi_dw = rowSums(pi_ke * dw_ke).
  function avdBanda(p_banda, pi, dw, como) {
    var val = p_banda * suma(mapa(pi, function (v, e) { return v * dw[e]; }));
    return como == null ? val : val * como;
  }

  // ---- JSON escrito por R ----------------------------------------------------------------------------------------

  // Reviver para JSON.parse de un JSON de jsonlite con na = "string": "Inf", "-Inf", "NaN" y "NA" -> números.
  function reviverR(clave, valor) {
    if (valor === 'Inf') return Infinity;
    if (valor === '-Inf') return -Infinity;
    if (valor === 'NaN' || valor === 'NA') return NaN;
    return valor;
  }

  DL.modelo = {
    version: '1.0.0',
    constantes: { NSUB: NSUB, EDAD_FIN: EDAD_FIN, RK4_LIMITE_PASO: RK4_LIMITE_PASO, ANCHO_IC95_EN_SD: ANCHO_IC95_EN_SD,
      LOG_I_INICIAL: LOG_I_INICIAL, MARGEN_LOG_F_INICIAL: MARGEN_LOG_F_INICIAL, RUTA_LOGNORMAL: RUTA_LOGNORMAL,
      RUTA_CONTEOS: RUTA_CONTEOS, EMR_TECHO_K: EMR_TECHO_K, EMR_PLANO_RANGO: EMR_PLANO_RANGO,
      EMR_PLANO_INICIO_BAJO_TECHO: EMR_PLANO_INICIO_BAJO_TECHO, EMR_SD_FUERA_DE_BANDAS: EMR_SD_FUERA_DE_BANDAS,
      MH_ESCALA_OPTIMA: MH_ESCALA_OPTIMA, MH_ACEPTACION_OBJETIVO: MH_ACEPTACION_OBJETIVO, MH_VENTANA: MH_VENTANA,
      MH_JITTER_COVARIANZA: MH_JITTER_COVARIANZA, MH_SD_INICIO: MH_SD_INICIO,
      MH_DESPLAZAMIENTO_SEMILLA: MH_DESPLAZAMIENTO_SEMILLA,
      CASCADA_EDAD_CIERRE_BANDA_ABIERTA: CASCADA_EDAD_CIERRE_BANDA_ABIERTA,
      VARIANZA_MINIMA_BETA: VARIANZA_MINIMA_BETA, TIPOS_DATO: TIPOS_DATO, LP_NOMBRES: LP_NOMBRES },
    // utilidades
    suma: suma, media: media, varianza: varianza, covarianza: covarianza, dnormLog: dnormLog,
    findInterval: findInterval, secuencia: secuencia, reviverR: reviverR,
    // mallas, W y EDO
    edadesMedia: edadesMedia, edadesMalla: edadesMalla, idxAnualMalla: idxAnualMalla, idxAnualMedia: idxAnualMedia,
    matrizW: matrizW, productoW: productoW, interpolar: interpolar, remisionMedia: remisionMedia,
    remisionPorEdad: remisionPorEdad, dpDa: dpDa, pasoRk4: pasoRk4, resolverEdo: resolverEdo,
    aEdadesEnteras: aEdadesEnteras, edo: edo, prevalenciaExacta: prevalenciaExacta,
    // bandas
    bandaDe: bandaDe, bandasFinas: bandasFinas, pesosIntervalo: pesosIntervalo, qIntervalos: qIntervalos,
    integrando: integrando, promedioBanda: promedioBanda,
    // álgebra del ancla
    sigmaLog: sigmaLog, correlacionAr1: correlacionAr1, cholesky: cholesky, cholAr1: cholAr1,
    resolverTranspuesta: resolverTranspuesta, logMvn: logMvn, lpMvnAr1: lpMvnAr1,
    // prior de la EMR
    priorEmrBanda: priorEmrBanda, techoEmr: techoEmr, emrEnNudos: emrEnNudos, emrDadoEnNudos: emrDadoEnNudos,
    emrPlano: emrPlano, emrCero: emrCero,
    // log-posterior
    fFueraDeCota: fFueraDeCota, segundasDiferencias: segundasDiferencias, lpSuavidad: lpSuavidad,
    lpPriorEmr: lpPriorEmr, lpEmr: lpEmr, lpAncla: lpAncla, lpLognormal: lpLognormal, lpBinomial: lpBinomial,
    lpPoisson: lpPoisson, lpDatos: lpDatos, prepararCtx: prepararCtx, logTasasMedia: logTasasMedia,
    edoLogMedia: edoLogMedia, edoCtx: edoCtx, lpComponentes: lpComponentes, logPosterior: logPosterior,
    thetaInicial: thetaInicial,
    // muestreo
    rhat: rhat, ess: ess, autocorrelacion: autocorrelacion, sumaGeyer: sumaGeyer, escalaInicial: escalaInicial,
    escalaAdaptada: escalaAdaptada, formaPropuesta: formaPropuesta, propuestaMh: propuestaMh, aceptaMh: aceptaMh,
    semillaDerivada: semillaDerivada,
    // subnacional
    renormalizar: renormalizar, renormalizarBanda: renormalizarBanda, pesosEdad: pesosEdad,
    desplazamientoLog: desplazamientoLog, transformar: transformar, sdTransformada: sdTransformada, sdDelta: sdDelta,
    // AVD
    betaMomentos: betaMomentos, renormalizarProporciones: renormalizarProporciones, avdBanda: avdBanda
  };
})(globalThis.DL = globalThis.DL || {});
