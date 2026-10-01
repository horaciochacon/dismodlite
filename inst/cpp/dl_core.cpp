// Modelo enfermedad-muerte (DisMod-lite): núcleo numérico en C++ del motor "rcpp"
//
// Notación: ver R/edo.R (modelo, mallas, sufijos _anual, _malla, _media y _nudos) y R/verosimilitud.R (términos de
// la log-posterior). Aquí además:
//   Índices de base 0: el punto n de la malla h es el punto 2n de la malla h/2; el paso n de RK4 va del punto 2n al
//   2n + 2 de la malla h/2. Los índices de edades y pesos que llegan de R (.dl_ctx_cpp) ya vienen en base 0.
//   Índices: k nudo; j punto de la malla h/2; n paso de RK4 (en el código, `paso`); b, c bandas del ancla; d dato;
//   t peso de una banda.
//   Matrices por columnas, como en R: W[fila + columna * n_media] y chol_R[c + b * n_bandas].
//
// Gemelo de R/edo.R y R/verosimilitud.R (la referencia, motor "mh"). Cada función cita a su gemela y sigue la misma
// fórmula. El solver (dp_da, resolver_edo) hace además las mismas operaciones en el mismo orden: compilado sin FMA
// (-ffp-contract=off) es idéntico bit a bit a dl_edo_resolver() (test-rcpp.R). La log-posterior no es idéntica bit
// a bit: en R, W %*% theta pasa por BLAS, sum() acumula en long double (precisión extendida donde la plataforma la
// tiene) y .dl_lp_mvn_ar1 suma log det en dos sumas; aquí todo se acumula en double, término a término. Coinciden a
// 1e-9 (test-rcpp.R). Cambiar una gemela exige cambiar la otra.
//
// Uso: R/rcpp.R compila este archivo al vuelo con Rcpp::sourceCpp(). El contexto de un sexo (.dl_ctx() en R,
// aplanado por .dl_ctx_cpp()) se copia una sola vez a memoria C++ (dl_ctx_construir_cpp, puntero externo) y
// cada evaluación de la log-posterior solo recibe theta.
#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// ---- Constantes ----

// Tipos de dato local: las filas de .DL_TIPOS_DATO (R/esquema.R), con códigos 1 .. N_TIPOS_DATO (= su columna
// codigo); lp_tipo[tipo - 1] guarda el término de datos del tipo.
static const int TIPO_PREV_ESTUDIO = 1, TIPO_INCIDENCIA = 2, TIPO_CSMR = 3;
static const int N_TIPOS_DATO = 3;
// Ruta y familia de un dato (.dl_datos_verosimilitud en R/verosimilitud.R y .dl_ctx_cpp en R/rcpp.R): ruta
// RUTA_LOGNORMAL (= .DL_RUTA_LOGNORMAL) si el dato trae val y se, si no conteos x de n; familia de los conteos
// FAMILIA_BINOMIAL o Poisson (1).
static const int RUTA_LOGNORMAL = 0, FAMILIA_BINOMIAL = 0;
// Posiciones del vector de componentes de la log-posterior, en el orden de .DL_LP_NOMBRES (R/verosimilitud.R): los
// cuatro términos, el total y el término de datos de cada tipo.
enum Salida { SUAVIDAD, EMR, ANCLA, DATOS, TOTAL, DATOS_PREV_ESTUDIO, DATOS_INCIDENCIA, DATOS_CSMR };
static const int N_SALIDA = DATOS_CSMR + 1;

// Contexto de un sexo. Gemelo de .dl_ctx() (R/verosimilitud.R); lo llena dl_ctx_construir_cpp() con la lista
// de .dl_ctx_cpp() (R/rcpp.R), que ya trae los índices en base 0.
struct DlCtx {
  // Mallas
  int n_nudos, n_media, nsub, n_anual;
  std::vector<double> W;                 // n_media x n_nudos, por columnas
  std::vector<int> idx_anual_malla;      // posiciones de las edades enteras en la malla h
  std::vector<double> r_media;           // r en la malla h/2
  // Priors de suavidad y de EMR
  double sigma_suavidad;
  std::vector<double> cota;              // {mínimo, máximo} de f en los nudos
  std::vector<double> emr_mu_log, emr_sd_log;
  bool emr_plano;                        // prior plano dentro de la cota: sin término de EMR
  // Ancla: bandas, chol de su correlación AR(1) y pesos de población de cada banda (concatenados: los de la
  // banda b son ancla_idx/ancla_w[ancla_off[b] .. ancla_off[b + 1] - 1], con ancla_idx en las edades enteras)
  int n_bandas;
  double lambda;
  std::vector<double> ancla_val, ancla_sigma_log, chol_R;   // chol_R: n_bandas x n_bandas, por columnas
  std::vector<int> ancla_idx, ancla_off;
  std::vector<double> ancla_w;
  // Datos locales (.dl_datos_verosimilitud): tipo TIPO_PREV_ESTUDIO, TIPO_INCIDENCIA o TIPO_CSMR; ruta
  // RUTA_LOGNORMAL (log-normal con offset) o conteos; familia FAMILIA_BINOMIAL o Poisson. Pesos concatenados como
  // los del ancla.
  int n_datos;
  std::vector<int> dato_tipo, dato_ruta, dato_familia;
  std::vector<double> dato_val, dato_s_log, dato_x, dato_n, dato_eta;
  std::vector<int> dato_idx, dato_off;
  std::vector<double> dato_w;
  // Memoria de trabajo de cada evaluación
  std::vector<double> i_media, f_media, p_malla, p_anual, i_anual, f_anual, z, u;
};

// ---- EDO ----

// dp/da = i (1 - p) - r p - f p (1 - p). Gemela en R: .dl_dp_da() en R/edo.R.
static inline double dp_da(double i, double r, double f, double p) {
  return i * (1 - p) - r * p - f * p * (1 - p);
}

// RK4 sobre la malla h desde p(a0) = p0, con i, f y r en la malla h/2 (n_media puntos); deja p en p_malla.
// Paso n (base 0; edad a -> a + h), con j = 2n la posición de a en la malla h/2 (k1..k4 son las pendientes de las
// cuatro etapas, no un índice):
//   k1 = dp_da en (a, p1),  p2 = p1 + h/2 k1,  k2 = dp_da en (a + h/2, p2),  p3 = p1 + h/2 k2,
//   k3 = dp_da en (a + h/2, p3),  p4 = p1 + h k3,  k4 = dp_da en (a + h, p4),
//   p(a + h) = p1 + h/6 (k1 + 2 k2 + 2 k3 + k4).
// Gemela en R: el bucle de dl_edo_resolver() en R/edo.R (ahí las etapas llevan dp_da escrita en línea).
static inline void resolver_edo(const double* i_media, const double* f_media, const double* r_media, int n_media,
                                double p0, int nsub, std::vector<double>& p_malla) {
  const double h = 1.0 / nsub;
  const int n_pasos = (n_media - 1) / 2;
  p_malla.assign(n_pasos + 1, 0.0);
  p_malla[0] = p0;
  for (int paso = 0; paso < n_pasos; ++paso) {
    const int j = 2 * paso;
    const double p1 = p_malla[paso];
    const double k1 = dp_da(i_media[j], r_media[j], f_media[j], p1);
    const double p2 = p1 + h / 2 * k1;
    const double k2 = dp_da(i_media[j + 1], r_media[j + 1], f_media[j + 1], p2);
    const double p3 = p1 + h / 2 * k2;
    const double k3 = dp_da(i_media[j + 1], r_media[j + 1], f_media[j + 1], p3);
    const double p4 = p1 + h * k3;
    const double k4 = dp_da(i_media[j + 2], r_media[j + 2], f_media[j + 2], p4);
    p_malla[paso + 1] = p1 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4);
  }
}

// Gemela en R: dl_edo_resolver() (misma firma por posición). remision: un valor o uno por punto de la malla h/2.
// nsub = 5 es .DL_NSUB de R/edo.R.
// [[Rcpp::export]]
NumericVector dl_edo_resolver_cpp(NumericVector i_media, NumericVector f_media,
                                  NumericVector remision = NumericVector::create(0.0), double p0 = 0, int nsub = 5) {
  const int n_media = i_media.size();
  std::vector<double> r_media(n_media, 0.0);
  if (remision.size() == 1) std::fill(r_media.begin(), r_media.end(), remision[0]);
  else if (remision.size() == n_media) r_media.assign(remision.begin(), remision.end());
  else stop("`remision` debe tener un valor o uno por punto de la malla h/2");
  std::vector<double> p_malla;
  resolver_edo(i_media.begin(), f_media.begin(), r_media.data(), n_media, p0, nsub, p_malla);
  return wrap(p_malla);
}

// ---- Contexto ----

template <class V> static std::vector<typename V::stored_type> como_vector(SEXP s) {
  V v(s);
  return std::vector<typename V::stored_type>(v.begin(), v.end());
}

// cx: lista de .dl_ctx_cpp() (R/rcpp.R). Devuelve un puntero externo al contexto.
// [[Rcpp::export]]
SEXP dl_ctx_construir_cpp(List cx) {
  DlCtx* c = new DlCtx();
  NumericMatrix W = cx["W"];
  c->n_nudos = as<int>(cx["n_nudos"]); c->n_media = W.nrow(); c->nsub = as<int>(cx["nsub"]);
  c->W.assign(W.begin(), W.end());
  c->idx_anual_malla = como_vector<IntegerVector>(cx["idx_anual_malla"]); c->n_anual = c->idx_anual_malla.size();
  c->r_media = como_vector<NumericVector>(cx["r_media"]);   // un valor por punto de la malla h/2 (.dl_ctx)
  c->sigma_suavidad = as<double>(cx["sigma_suavidad"]);
  c->cota = como_vector<NumericVector>(cx["cota"]);
  c->emr_mu_log = como_vector<NumericVector>(cx["emr_mu_log"]);
  c->emr_sd_log = como_vector<NumericVector>(cx["emr_sd_log"]);
  c->emr_plano = as<bool>(cx["emr_plano"]);
  NumericMatrix U = cx["chol_R"]; c->chol_R.assign(U.begin(), U.end()); c->n_bandas = U.nrow();
  c->lambda = as<double>(cx["lambda"]);
  c->ancla_val = como_vector<NumericVector>(cx["ancla_val"]);
  c->ancla_sigma_log = como_vector<NumericVector>(cx["ancla_sigma_log"]);
  c->ancla_idx = como_vector<IntegerVector>(cx["ancla_idx"]);
  c->ancla_off = como_vector<IntegerVector>(cx["ancla_off"]);
  c->ancla_w = como_vector<NumericVector>(cx["ancla_w"]);
  c->dato_tipo = como_vector<IntegerVector>(cx["dato_tipo"]);
  c->dato_ruta = como_vector<IntegerVector>(cx["dato_ruta"]);
  c->dato_familia = como_vector<IntegerVector>(cx["dato_familia"]);
  c->dato_val = como_vector<NumericVector>(cx["dato_val"]);
  c->dato_s_log = como_vector<NumericVector>(cx["dato_s_log"]);
  c->dato_x = como_vector<NumericVector>(cx["dato_x"]); c->dato_n = como_vector<NumericVector>(cx["dato_n"]);
  c->dato_eta = como_vector<NumericVector>(cx["dato_eta"]);
  c->n_datos = c->dato_val.size();
  c->dato_idx = como_vector<IntegerVector>(cx["dato_idx"]); c->dato_off = como_vector<IntegerVector>(cx["dato_off"]);
  c->dato_w = como_vector<NumericVector>(cx["dato_w"]);
  c->i_media.resize(c->n_media); c->f_media.resize(c->n_media);
  c->p_anual.resize(c->n_anual); c->i_anual.resize(c->n_anual); c->f_anual.resize(c->n_anual);
  c->z.resize(c->n_bandas); c->u.resize(c->n_bandas);
  return XPtr<DlCtx>(c, true);
}

// ---- Solución de la EDO en el contexto ----

// log i = W theta_i, log f = W theta_f en la malla h/2; i = exp(log i), f = exp(log f).
// Gemela en R: .dl_log_tasas_media() y el exp de .dl_edo_log_media() (R/verosimilitud.R), sin techo de f.
static inline void tasas_media(const double* theta, DlCtx& c) {
  const int n_nudos = c.n_nudos, n_media = c.n_media;
  for (int fila = 0; fila < n_media; ++fila) {
    double log_i = 0.0, log_f = 0.0;
    for (int k = 0; k < n_nudos; ++k) {
      const double w = c.W[fila + k * n_media];
      log_i += w * theta[k]; log_f += w * theta[n_nudos + k];
    }
    c.i_media[fila] = std::exp(log_i); c.f_media[fila] = std::exp(log_f);
  }
}

// p, i, f en las edades enteras: p de la malla h en idx_anual_malla, i y f de la malla h/2 en 2 idx_anual_malla.
// pos_anual: posición (base 0) de la edad entera en la malla anual. Gemela en R: .dl_a_edades_enteras() (R/edo.R).
static inline void anuales(DlCtx& c) {
  for (int pos_anual = 0; pos_anual < c.n_anual; ++pos_anual) {
    const int j = c.idx_anual_malla[pos_anual];
    c.p_anual[pos_anual] = c.p_malla[j]; c.i_anual[pos_anual] = c.i_media[2 * j];
    c.f_anual[pos_anual] = c.f_media[2 * j];
  }
}

// ---- Términos de la log-posterior ----

// Algún f = exp(log f) de los nudos fuera de [cota[0], cota[1]]. Gemela en R: .dl_f_fuera_de_cota().
static inline bool f_fuera_de_cota(const double* log_f_nudos, const DlCtx& c) {
  for (int k = 0; k < c.n_nudos; ++k) {
    const double f = std::exp(log_f_nudos[k]);
    if (f < c.cota[0] || f > c.cota[1]) return true;
  }
  return false;
}

// sum_k log N(D2_k; 0, sigma), D2_k = (log i_k+2 - log i_k+1) - (log i_k+1 - log i_k).
// Gemela en R: .dl_lp_suavidad().
static inline double lp_suavidad(const double* log_i_nudos, const DlCtx& c) {
  double lp = 0.0;
  for (int k = 0; k + 2 < c.n_nudos; ++k)
    lp += R::dnorm((log_i_nudos[k + 2] - log_i_nudos[k + 1]) - (log_i_nudos[k + 1] - log_i_nudos[k]), 0.0,
                   c.sigma_suavidad, 1);
  return lp;
}

// sum_k log N(log f_k; mu_log_k, sd_log_k); 0 con el prior plano. Gemela en R: .dl_lp_prior_emr().
static inline double lp_prior_emr(const double* log_f_nudos, const DlCtx& c) {
  double lp = 0.0;
  if (!c.emr_plano)
    for (int k = 0; k < c.n_nudos; ++k) lp += R::dnorm(log_f_nudos[k], c.emr_mu_log[k], c.emr_sd_log[k], 1);
  return lp;
}

// q de una banda: sum_t cantidad_anual[idx_t] w_t sobre los pesos off[b] .. off[b + 1] - 1.
// Gemela en R: .dl_q_intervalos() (R/bandas.R) para una banda.
static inline double q_banda(const std::vector<double>& anual, const std::vector<int>& idx,
                             const std::vector<int>& off, const std::vector<double>& w, int b) {
  double q = 0.0;
  for (int t = off[b]; t < off[b + 1]; ++t) q += anual[idx[t]] * w[t];
  return q;
}

// log MVN(z; 0, Sigma / lambda), Sigma = diag(sigma_log) R diag(sigma_log), R = U'U:
//   -1/2 [lambda |u|^2 + log det Sigma - n log lambda + n log(2 pi)],  U' u = z / sigma_log.
// Gemela en R: .dl_lp_mvn_ar1().
// log det se acumula banda a banda (R: 2 sum log diag(U) + 2 sum log sigma_log).
static inline double lp_mvn_ar1(const double* z, const double* sigma_log, const double* U, double lambda, int n,
                                double* u) {
  double quad = 0.0, logdet = 0.0;
  // Sustitución hacia adelante, u = backsolve(U, z / sigma_log, transpose = TRUE). U está por columnas:
  // U[c + b n] = U_cb = (U')_bc, así que el bucle en c recorre la fila b de U' u = z / sigma_log.
  for (int b = 0; b < n; ++b) {
    double resto = z[b] / sigma_log[b];
    for (int c = 0; c < b; ++c) resto -= U[c + b * n] * u[c];
    u[b] = resto / U[b + b * n];
    quad += u[b] * u[b];
    logdet += 2.0 * std::log(U[b + b * n]) + 2.0 * std::log(sigma_log[b]);
  }
  return -0.5 * (lambda * quad + logdet - n * std::log(lambda) + n * std::log(2.0 * M_PI));
}

// L_ancla con z_b = log q_b - log val_b. Devuelve false si algún q_b no es finito o es <= 0 (sin calcular L_ancla).
// Gemela en R: el cálculo de q_ancla, su control y .dl_lp_ancla() en .dl_lp_componentes().
static inline bool lp_ancla(DlCtx& c, double& lp) {
  for (int b = 0; b < c.n_bandas; ++b) {
    const double q = q_banda(c.p_anual, c.ancla_idx, c.ancla_off, c.ancla_w, b);
    if (!std::isfinite(q) || q <= 0.0) return false;
    c.z[b] = std::log(q) - std::log(c.ancla_val[b]);
  }
  lp = lp_mvn_ar1(c.z.data(), c.ancla_sigma_log.data(), c.chol_R.data(), c.lambda, c.n_bandas, c.u.data());
  return true;
}

// l de un dato. Gemelas en R: .dl_lp_lognormal(), .dl_lp_binomial() y .dl_lp_poisson().
static inline double lp_lognormal(double q, double val, double s_log, double eta) {
  return R::dnorm(std::log(q + eta), std::log(val + eta), s_log, 1);
}
static inline double lp_binomial(double q, double x, double n) {
  return x * std::log(q) + (n - x) * std::log1p(-q);
}
static inline double lp_poisson(double q, double x, double n) {
  return x * std::log(n * q) - n * q;
}

// Integrando anual del tipo de dato en la edad entera de posición pos_anual (base 0): TIPO_PREV_ESTUDIO p,
// TIPO_INCIDENCIA i (1 - p), TIPO_CSMR p f. Gemela en R: .dl_integrando() (R/esquema.R).
static inline double integrando(int tipo, int pos_anual, const DlCtx& c) {
  return (tipo == TIPO_PREV_ESTUDIO) ? c.p_anual[pos_anual]
       : (tipo == TIPO_INCIDENCIA)   ? c.i_anual[pos_anual] * (1 - c.p_anual[pos_anual])
       :                               c.p_anual[pos_anual] * c.f_anual[pos_anual];
}

// L_datos por tipo (1 .. N_TIPOS_DATO) en lp_tipo[tipo - 1]: suma de l de los datos de cada tipo, en el orden de
// los datos; un q no finito o <= 0 deja su tipo en -Inf. Un tipo cuya suma ya no es finita no suma más datos.
// Gemela en R: .dl_lp_datos(). Misma cuenta que .dl_lp_datos() en otro orden de control: aquí se revisa q dato a
// dato y se corta en el primer lp no finito; R revisa todos los q del tipo antes de sumar. Solo difieren si un
// término NaN precede a un q <= 0 (aquí NaN, en R -Inf). q se calcula en línea (q_banda del integrando) para no
// materializar el integrando anual. q >= 1 en la binomial da NaN, como en R (ver .dl_lp_datos).
static inline void lp_datos(const DlCtx& c, double* lp_tipo) {
  for (int tipo = 1; tipo <= N_TIPOS_DATO; ++tipo) {
    double lp = 0.0;
    for (int d = 0; d < c.n_datos; ++d) {
      if (c.dato_tipo[d] != tipo) continue;
      if (!std::isfinite(lp)) break;
      double q = 0.0;
      for (int t = c.dato_off[d]; t < c.dato_off[d + 1]; ++t) {
        const double x_a = integrando(tipo, c.dato_idx[t], c);
        q += x_a * c.dato_w[t];
      }
      if (!std::isfinite(q) || q <= 0.0) { lp = R_NegInf; break; }
      double l;
      if (c.dato_ruta[d] == RUTA_LOGNORMAL) l = lp_lognormal(q, c.dato_val[d], c.dato_s_log[d], c.dato_eta[d]);
      else if (c.dato_familia[d] == FAMILIA_BINOMIAL) l = lp_binomial(q, c.dato_x[d], c.dato_n[d]);
      else l = lp_poisson(q, c.dato_x[d], c.dato_n[d]);
      lp += l;
    }
    lp_tipo[tipo - 1] = lp;
  }
}

// ---- Log-posterior ----

// out, en el orden de Salida (.DL_LP_NOMBRES): los dos primeros términos y el mismo valor en el resto, de ANCLA en
// adelante (las salidas tempranas de dl_lp_core).
static inline void llenar_salida(double* out, double lp_suav, double lp_emr, double resto) {
  out[SUAVIDAD] = lp_suav; out[EMR] = lp_emr; for (int k = ANCLA; k < N_SALIDA; ++k) out[k] = resto;
}

// log posterior(theta) por términos. Gemela en R: .dl_lp_componentes() (R/verosimilitud.R).
static void dl_lp_core(const double* theta, DlCtx& c, double* out) {
  const double* log_i_nudos = theta;
  const double* log_f_nudos = theta + c.n_nudos;
  if (f_fuera_de_cota(log_f_nudos, c)) { llenar_salida(out, R_NegInf, R_NegInf, R_NegInf); return; }
  tasas_media(theta, c);
  resolver_edo(c.i_media.data(), c.f_media.data(), c.r_media.data(), c.n_media, 0.0, c.nsub, c.p_malla);
  anuales(c);
  const double lp_suav = lp_suavidad(log_i_nudos, c);
  const double lp_emr = lp_prior_emr(log_f_nudos, c);
  double lp_anc;
  if (!lp_ancla(c, lp_anc)) { llenar_salida(out, lp_suav, lp_emr, R_NegInf); return; }
  double lp_tipo[N_TIPOS_DATO];
  lp_datos(c, lp_tipo);
  // Suma explícita de los tres tipos, en el orden de .DL_TIPOS_DATO: fija el orden del redondeo (en R, sum(por_tipo)).
  const double lp_dat = lp_tipo[TIPO_PREV_ESTUDIO - 1] + lp_tipo[TIPO_INCIDENCIA - 1] + lp_tipo[TIPO_CSMR - 1];
  out[SUAVIDAD] = lp_suav; out[EMR] = lp_emr; out[ANCLA] = lp_anc; out[DATOS] = lp_dat;
  out[TOTAL] = lp_suav + lp_emr + lp_anc + lp_dat;
  out[DATOS_PREV_ESTUDIO] = lp_tipo[TIPO_PREV_ESTUDIO - 1]; out[DATOS_INCIDENCIA] = lp_tipo[TIPO_INCIDENCIA - 1];
  out[DATOS_CSMR] = lp_tipo[TIPO_CSMR - 1];
}

// Gemela en R: .dl_lp_componentes().
// [[Rcpp::export]]
NumericVector dl_lp_componentes_cpp(NumericVector theta, SEXP ptr) {
  XPtr<DlCtx> c(ptr);
  double out[N_SALIDA];
  dl_lp_core(theta.begin(), *c, out);
  return NumericVector::create(_["suavidad"] = out[SUAVIDAD], _["emr"] = out[EMR], _["ancla"] = out[ANCLA],
                               _["datos"] = out[DATOS], _["total"] = out[TOTAL],
                               _["datos_prev_estudio"] = out[DATOS_PREV_ESTUDIO],
                               _["datos_incidencia"] = out[DATOS_INCIDENCIA], _["datos_csmr"] = out[DATOS_CSMR]);
}

// Gemela en R: .dl_log_post().
// [[Rcpp::export]]
double dl_lp_total_cpp(NumericVector theta, SEXP ptr) {
  XPtr<DlCtx> c(ptr);
  double out[N_SALIDA];
  dl_lp_core(theta.begin(), *c, out);
  return out[TOTAL];
}
