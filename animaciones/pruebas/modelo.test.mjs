// Pruebas de lib/modelo.js contra los valores de referencia que calcula dismodlite (pruebas/fixtures.R).
//
//   cd animaciones && Rscript pruebas/fixtures.R && node --test pruebas/
//
// Cada grupo compara una función de modelo.js con su gemela en R y registra la mayor diferencia observada; la
// última prueba las lista (diagnósticos «ℹ» del reporte de node --test). Tolerancias: EDO 1e-12 relativo (se espera
// ~1e-15: las mismas operaciones en el mismo orden; con las mismas tasas en la malla h/2, idéntico bit a bit),
// log-posterior 1e-9 absoluto, R-hat y ESS 1e-10 relativo.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import vm from 'node:vm';

const aqui = path.dirname(fileURLToPath(import.meta.url));
const fuente = readFileSync(path.join(aqui, '..', 'lib', 'modelo.js'), 'utf8');
vm.runInThisContext(fuente, { filename: 'lib/modelo.js' });
const M = globalThis.DL.modelo;
const fx = JSON.parse(readFileSync(path.join(aqui, 'fixtures.json'), 'utf8'), M.reviverR);

// ---- Comparación ------------------------------------------------------------------------------------------------

const maximos = new Map();   // grupo -> {dif, tipo}
function registrar(grupo, dif, tipo) {
  const previo = maximos.get(grupo);
  if (!previo || dif > previo.dif) maximos.set(grupo, { dif, tipo });
}

// Diferencia entre dos números: 0 si son iguales (también dos NaN o dos infinitos del mismo signo); relativa o
// absoluta según `tipo`; Infinity si uno es finito y el otro no.
function diferencia(a, b, tipo) {
  if (Object.is(a, b) || a === b) return 0;
  if (Number.isNaN(a) && Number.isNaN(b)) return 0;
  if (!Number.isFinite(a) || !Number.isFinite(b)) return Infinity;
  const d = Math.abs(a - b);
  return tipo === 'rel' ? d / Math.max(Math.abs(b), 1e-300) : d;
}

function comoArreglo(x) { return Array.isArray(x) || ArrayBuffer.isView(x) ? Array.from(x) : [x]; }

// Compara arreglos (o escalares) elemento a elemento; registra y exige la mayor diferencia <= tol.
function comparar(grupo, actual, esperado, tol, tipo = 'rel', etiqueta = '') {
  const a = comoArreglo(actual), e = comoArreglo(esperado);
  assert.equal(a.length, e.length, `${grupo} ${etiqueta}: largo ${a.length} frente a ${e.length}`);
  let max = 0, donde = -1;
  for (let k = 0; k < e.length; k++) {
    const d = diferencia(a[k], e[k], tipo);
    if (d > max) { max = d; donde = k; }
  }
  registrar(grupo, max, tipo);
  assert.ok(max <= tol, `${grupo} ${etiqueta}: diferencia ${tipo} ${max} > ${tol} en [${donde}] ` +
    `(JS ${a[donde]}, R ${e[donde]})`);
  return max;
}

function compararMatriz(grupo, actual, esperado, tol, tipo = 'abs', etiqueta = '') {
  assert.equal(actual.length, esperado.length, `${grupo} ${etiqueta}: filas`);
  actual.forEach((fila, k) => comparar(grupo, fila, esperado[k], tol, tipo, `${etiqueta} fila ${k}`));
}

// ---- Pureza ---------------------------------------------------------------------------------------------------

test('modelo.js es un script clásico puro: sin Math.random, Date, import ni export', () => {
  const codigo = fuente.split('\n').filter((l) => !l.trim().startsWith('//')).join('\n');
  assert.ok(!/Math\.random|\bDate\b|performance\.now/.test(codigo));
  assert.ok(!/^\s*(import|export)\s/m.test(codigo));
  assert.equal(typeof M.logPosterior, 'function');
});

// ---- (a) Viñeta: W, malla, EDO y RK4 ----------------------------------------------------------------------------

const v = fx.vineta;

test('edadesMedia reproduce .dl_edades_media()', () => {
  comparar('mallas (edades, índices)', M.edadesMedia(v.edad_inicio, v.edad_fin, v.nsub), v.edades_media, 0, 'abs');
});

test('matrizW reproduce .dl_base_interp() (malla h/2 y edades sonda, dentro y fuera de los nudos)', () => {
  const W = M.matrizW(v.nudos, v.edades_media);
  compararMatriz('matrizW', W, v.W, 0);
  compararMatriz('matrizW', M.matrizW(v.nudos, v.edades_sonda), v.W_sonda, 0);
  for (const fila of W) assert.ok(Math.abs(fila.reduce((s, x) => s + x, 0) - 1) < 1e-15);
  comparar('interpolar (W %*% x)', M.interpolar(v.log_i, v.nudos, v.edades_sonda), v.interpolado_sonda, 0, 'rel');
});

test('i y f en la malla h/2: exp(W log tasa) como dl_edo()', () => {
  const W = M.matrizW(v.nudos, v.edades_media);
  comparar('tasas en la malla h/2 (exp)', M.productoW(W, v.log_i).map(Math.exp), v.i_media, 1e-15);
  comparar('tasas en la malla h/2 (exp)', M.productoW(W, v.log_f).map(Math.exp), v.f_media, 1e-15);
});

test('resolverEdo con las tasas de R es idéntico bit a bit a dl_edo_resolver()', () => {
  const p = M.resolverEdo(v.i_media, v.f_media, 0, 0, v.nsub);
  comparar('resolverEdo (mismas tasas)', p, v.p_malla, 0, 'rel');
});

test('edo() reproduce dl_edo() en la viñeta', () => {
  const s = M.edo(v.log_i, v.log_f, v.nudos, { edad_inicio: v.edad_inicio });
  comparar('edo vs dl_edo (edades)', s.edades, v.edo.edades, 0, 'abs');
  comparar('edo vs dl_edo (p, i, f)', s.p, v.edo.p, 1e-12);
  comparar('edo vs dl_edo (p, i, f)', s.i, v.edo.i, 1e-12);
  comparar('edo vs dl_edo (p, i, f)', s.f, v.edo.f, 1e-12);
  comparar('edo vs dl_edo (p en la malla h)', s.p_malla, v.p_malla, 1e-12);
});

test('pasoRk4 reproduce las cuatro etapas de dl_edo_resolver()', () => {
  for (const r of v.pasos_rk4) {
    const e = M.pasoRk4(r.p1, r.j, v.i_media, v.f_media, 0, v.nsub);
    for (const nm of ['p1', 'k1', 'p2', 'k2', 'p3', 'k3', 'p4', 'k4', 'p_siguiente'])
      comparar('pasoRk4 (etapas)', e[nm], r[nm], 0, 'rel', `paso ${r.paso} ${nm}`);
  }
  comparar('dpDa', v.dp_da.valor.map((_, k) => M.dpDa(v.dp_da.i[k], v.dp_da.r[k], v.dp_da.f[k], v.dp_da.p[k])),
    v.dp_da.valor, 0, 'rel');
});

test('prevalenciaExacta y la precisión de RK4 (sección 3 de la viñeta)', () => {
  const pr = fx.precision;
  comparar('prevalenciaExacta', pr.t.map((t) => M.prevalenciaExacta(pr.i, pr.f, t)), pr.exacta, 1e-14);
  const s = M.edo([Math.log(pr.i), Math.log(pr.i)], [Math.log(pr.f), Math.log(pr.f)], [30, 95], { edad_inicio: 30 });
  comparar('edo vs dl_edo (p, i, f)', s.p, pr.p, 1e-12);
  const err = Math.max(...s.p.map((p, k) => Math.abs(p - pr.exacta[k]) / Math.max(pr.exacta[k], 1e-12)));
  assert.ok(err < 1e-6);
  comparar('precisión RK4 (error relativo máximo)', err, pr.error_relativo_max, 1e-9);
});

// ---- (c) EDO en configuraciones de borde ------------------------------------------------------------------------

test('edo() en los bordes: nsub 10, remisión, edad_inicio 0 y 20, edad_fin 90, p0, f = 0', () => {
  for (const c of fx.edo_bordes) {
    const s = M.edo(v.log_i, c.f_cero ? null : v.log_f, v.nudos,
      { edad_inicio: c.edad_inicio, edad_fin: c.edad_fin, nsub: c.nsub, remision: c.remision, p0: c.p0 });
    assert.equal(s.edades_media.length, c.n_media, c.nombre);
    if (c.r_media) comparar('remisionPorEdad', s.r_media, c.r_media, 0, 'abs', c.nombre);
    comparar('edo en los bordes (p, i, f, p_malla)', s.edades, c.edades, 0, 'abs', c.nombre);
    for (const nm of ['p', 'i', 'f', 'p_malla']) comparar('edo en los bordes (p, i, f, p_malla)', s[nm], c[nm], 1e-12, 'rel', `${c.nombre} ${nm}`);
  }
});

// ---- Funciones sueltas de la log-posterior ----------------------------------------------------------------------

const fn = fx.funciones;

test('dnormLog = dnorm(log = TRUE) de R, con sus casos especiales', () => {
  comparar('dnormLog', fn.dnorm.x.map((x, k) => M.dnormLog(x, fn.dnorm.mu[k], fn.dnorm.sigma[k])), fn.dnorm.valor, 1e-15);
});

test('lpSuavidad, lpPriorEmr, lpMvnAr1, logMvn y cholesky', () => {
  comparar('segundas diferencias', M.segundasDiferencias(fn.suavidad.log_i), fn.suavidad.d2, 0, 'abs');
  comparar('lpSuavidad', M.lpSuavidad(fn.suavidad.log_i, fn.suavidad.sigma), fn.suavidad.valor, 1e-12, 'abs');
  comparar('lpPriorEmr', M.lpPriorEmr(fn.prior_emr.log_f, { plano: false, mu_log: fn.prior_emr.mu_log, sd_log: fn.prior_emr.sd_log }),
    fn.prior_emr.valor, 1e-12, 'abs');
  const m = fn.mvn_ar1;
  comparar('lpMvnAr1', M.lpMvnAr1(m.z, m.sigma_log, m.chol_R, m.lambda), m.valor, 1e-12, 'abs');
  comparar('lpMvnAr1', M.lpMvnAr1(m.z, m.sigma_log, M.cholAr1(m.z.length, m.rho), m.lambda), m.valor, 1e-12, 'abs');
  // Sigma / lambda con la normal multivariada general: el mismo número
  const SigmaL = m.Sigma.map((fila) => fila.map((x) => x / m.lambda));
  comparar('logMvn', M.logMvn(m.z, SigmaL), m.valor, 1e-11, 'abs');
  comparar('logMvn', M.logMvn(fn.mvn.z, fn.mvn.Sigma), fn.mvn.valor, 1e-11, 'abs');
  for (const c of fn.cholesky) {
    compararMatriz('correlacionAr1', M.correlacionAr1(c.n, c.rho), c.R, 1e-16);
    compararMatriz('cholesky (chol de R)', M.cholAr1(c.n, c.rho), c.U, 0);   // DPOTRF2 de LAPACK repetido: idéntico bit a bit
  }
});

test('lpLognormal, lpBinomial, lpPoisson, fFueraDeCota y sigmaLog', () => {
  const l = fn.lognormal, bi = fn.binomial, po = fn.poisson;
  comparar('verosimilitud de un dato', l.q.map((q, k) => M.lpLognormal(q, l.val[k], l.s_log[k], l.eta[k])), l.valor, 1e-14);
  comparar('verosimilitud de un dato', bi.q.map((q, k) => M.lpBinomial(q, bi.x[k], bi.n[k])), bi.valor, 1e-14);
  comparar('verosimilitud de un dato', po.q.map((q, k) => M.lpPoisson(q, po.x[k], po.n[k])), po.valor, 1e-14);
  assert.equal(M.fFueraDeCota(fn.fuera_de_cota.log_f, fn.fuera_de_cota.cota), fn.fuera_de_cota.valor);
  // sigmaLog: Math.log de V8 y log de glibc pueden diferir en el último bit, y la resta log(sup) - log(inf) lo
  // amplifica (cancelación): se admite 1e-14 relativo.
  comparar('sigmaLog', fn.sigma_log.inferior.map((a, k) => M.sigmaLog(a, fn.sigma_log.superior[k])), fn.sigma_log.valor, 1e-14);
});

test('lpEmr: EMR cero, techo, prior plano y prior informativo', () => {
  const e = { mu_log: [-4, -3], sd_log: [0.3, 0.2], cota: [0, 0.25], plano: false, cero: false };
  assert.equal(M.lpEmr([Math.log(0.3), -3], e), -Infinity);
  assert.equal(M.lpEmr([Math.log(0.1), -3], { ...e, plano: true }), 0);
  assert.equal(M.lpEmr([5, 5], M.emrCero()), 0);
  assert.equal(M.lpEmr([-4.1, -3.2], e), M.lpPriorEmr([-4.1, -3.2], e));
  assert.equal(M.lpEmr([Math.log(1e-6)], M.emrPlano([30], [0, 0.25])), -Infinity);   // bajo el piso f_max / 1000
});

// ---- Bandas de edad ---------------------------------------------------------------------------------------------

const bd = fx.bandas;
const filasDe = (c) => c.inicio.map((a, k) => ({ inicio: a, fin: c.fin[k], age_group_id: c.age_group_id ? c.age_group_id[k] : undefined }));

test('bandasFinas, bandaDe y pesosIntervalo reproducen R/bandas.R (incluida la rama que divide por n_b)', () => {
  const finas = M.bandasFinas(filasDe(bd.limites));
  comparar('bandas (finas, banda de cada edad)', finas.map((b) => b.age_group_id), bd.finas.age_group_id, 0, 'abs');
  comparar('bandas (finas, banda de cada edad)', M.bandaDe(bd.banda_de.edades, filasDe(bd.banda_de.bandas)), bd.banda_de.valor, 0, 'abs');
  const bandas = filasDe(bd.pesos.bandas);
  for (const iv of bd.pesos.intervalos) {
    const pw = M.pesosIntervalo(bd.pesos.edades_anual, iv.inicio, iv.fin, bandas, bd.pesos.N);
    comparar('pesosIntervalo (idx)', pw.idx, iv.idx, 0, 'abs', `[${iv.inicio}, ${iv.fin})`);
    comparar('pesosIntervalo (w)', pw.w, iv.w, 1e-15, 'rel', `[${iv.inicio}, ${iv.fin})`);
    comparar('qIntervalos', M.qIntervalos(bd.pesos.x, [pw]), iv.q, 1e-15, 'rel', `[${iv.inicio}, ${iv.fin})`);
    assert.ok(Math.abs(pw.w.reduce((s, x) => s + x, 0) - 1) < 1e-14);
  }
  assert.throws(() => M.pesosIntervalo(bd.pesos.edades_anual, 10, 15, bandas, bd.pesos.N));
});

test('integrando: p, i (1 - p) y p f', () => {
  for (const nm of ['p', 'ipop', 'pf']) comparar('integrando', M.integrando(v.edo, nm), fx.integrandos[nm], 0, 'rel', nm);
});

// ---- (b) Contextos del proyecto de ejemplo y log-posterior -----------------------------------------------------

for (const [variante, cv] of Object.entries(fx.contextos)) {
  for (const s of cv.sexos) {
    const etiqueta = `${variante}, sexo ${s.sexo}`;

    test(`prepararCtx reproduce .dl_ctx() (${etiqueta})`, () => {
      const c = M.prepararCtx(s.ctx), r = s.r;
      assert.equal(c.edades_media.length, r.n_media);
      comparar('prepararCtx (mallas, W, r, pesos)', [c.edades_media[0], c.edades_media.at(-1)], r.edades_media_extremos, 0, 'abs');
      comparar('prepararCtx (mallas, W, r, pesos)', r.filas_W.map((j) => c.edades_media[j]), r.edades_media_filas, 0, 'abs');
      compararMatriz('prepararCtx (mallas, W, r, pesos)', r.filas_W.map((j) => c.W[j]), r.W_filas, 0, 'abs', 'W');
      comparar('prepararCtx (mallas, W, r, pesos)', c.r_media, r.r_media, 0, 'abs', 'r_media');
      comparar('mallas (edades, índices)', c.idx_anual_malla, r.idx_anual_malla, 0, 'abs');
      comparar('mallas (edades, índices)', c.idx_anual_media, r.idx_anual_media, 0, 'abs');
      comparar('mallas (edades, índices)', c.anual, r.anual, 0, 'abs');
      compararMatriz('cholesky (chol de R)', c.ancla.chol_R, r.chol_R, 0);
      const pesos = (a, e, nm) => {
        assert.equal(a.length, e.length, nm);
        a.forEach((p, k) => { comparar('mallas (edades, índices)', p.idx, e[k].idx, 0, 'abs'); comparar('pesosIntervalo (w)', p.w, e[k].w, 1e-15, 'rel', nm); });
      };
      pesos(c.pesos_ancla, r.pesos_ancla, 'ancla');
      pesos(c.pesos_datos, r.pesos_datos, 'datos');
      comparar('thetaInicial', M.thetaInicial(c), r.theta_inicial, 0, 'abs');
    });

    test(`edoCtx reproduce .dl_edo_ctx() (${etiqueta})`, () => {
      const sol = M.edoCtx(s.thetas[1].theta, s.ctx);
      for (const nm of ['p', 'i', 'f']) comparar('edoCtx vs .dl_edo_ctx (p, i, f)', sol[nm], s.r.edo_theta2[nm], 1e-12, 'rel', nm);
    });

    test(`lpComponentes y logPosterior reproducen .dl_lp_componentes() (${etiqueta})`, () => {
      const c = M.prepararCtx(s.ctx);
      for (const th of s.thetas) {
        const lp = M.lpComponentes(th.theta, c);
        for (const nm of M.constantes.LP_NOMBRES)
          comparar(`log-posterior (${nm === 'total' ? 'total' : 'términos'})`, lp[nm], th.lp[nm], 1e-9, 'abs', `${th.nombre} ${nm}`);
        assert.equal(M.logPosterior(th.theta, s.ctx), lp.total);   // sin preparar: el mismo número
      }
    });
  }
}

test('el ajuste corto: edoCtx con la simulación 1 reproduce draws_q de dl_ajustar()', () => {
  for (const a of fx.ajuste) {
    const ctx = fx.contextos.ejemplo.sexos.find((s) => s.sexo === a.sexo).ctx;
    const sol = M.edoCtx(a.theta, ctx);
    for (const nm of ['p', 'i', 'f']) comparar('edoCtx vs draws_q de dl_ajustar', sol[nm], a[nm], 1e-12, 'rel', nm);
    comparar('edoCtx vs draws_q de dl_ajustar', M.integrando(sol, 'ipop'), a.ipop, 1e-12, 'rel', 'ipop');
  }
});

test('logTasasMedia y edoLogMedia con el techo de la EMR reproducen .dl_log_tasas_media() y .dl_edo_log_media()', () => {
  const ctx = M.prepararCtx(fx.contextos.ejemplo.sexos[0].ctx);
  for (const c of fx.techo_cascada) {
    const lm = M.logTasasMedia(c.theta, ctx);
    comparar('logTasasMedia y techo de la cascada', lm.log_i, c.log_i_media, 0, 'rel');
    comparar('logTasasMedia y techo de la cascada', lm.log_f, c.log_f_media, 0, 'rel');
    const s = M.edoLogMedia(lm.log_i, lm.log_f.map((x) => x + c.desplazamiento), ctx, c.techo_f);
    assert.equal(s.truncado, c.truncado);
    for (const nm of ['p', 'i', 'f']) comparar('logTasasMedia y techo de la cascada', s[nm], c[nm], 1e-12, 'rel', nm);
  }
  assert.ok(fx.techo_cascada.some((c) => c.truncado === 1) && fx.techo_cascada.some((c) => c.truncado === 0));
});

test('edadesMalla, promedioBanda, remisionMedia y prepararCtx con los pesos y la r de R', () => {
  const em = M.edadesMedia(30, 99, 5), eh = M.edadesMalla(30, 99, 5);
  assert.equal(eh.length, (em.length - 1) / 2 + 1);
  eh.forEach((a, n) => assert.equal(a, em[2 * n]));
  const bandas = filasDe(bd.pesos.bandas), iv = bd.pesos.intervalos[0];
  assert.equal(M.promedioBanda(bd.pesos.x, bd.pesos.edades_anual, iv.inicio, iv.fin, bandas, bd.pesos.N), iv.q);
  assert.deepEqual(M.remisionMedia(0.1, 3), [0.1, 0.1, 0.1]);
  assert.throws(() => M.remisionMedia([0.1, 0.2], 3));
  // el contexto con los pesos y la r ya calculados en R da la misma log-posterior
  const s = fx.contextos.datos.sexos[0];
  const conR = { ...s.ctx, r_media: s.r.r_media, pesos_ancla: s.r.pesos_ancla, pesos_datos: s.r.pesos_datos };
  for (const th of s.thetas) comparar('log-posterior (total)', M.logPosterior(th.theta, conR), th.lp.total, 1e-9, 'abs', th.nombre);
});

// ---- Prior de la EMR --------------------------------------------------------------------------------------------

test('priorEmrBanda, techoEmr, emrEnNudos, emrPlano, emrDadoEnNudos y emrCero reproducen R/emr.R', () => {
  const e = fx.emr, an = e.ancla;
  const mu = an.prev.map((p, k) => M.priorEmrBanda(an.csmr[k], p, an.sigma_log_csmr[k], an.sigma_log_prev[k]));
  comparar('prior de la EMR', mu.map((x) => x.mu_log), e.tabla.mu_log, 1e-14);
  comparar('prior de la EMR', mu.map((x) => x.sd_log), e.tabla.sd_log, 1e-14);
  const fa = an.prev.map((p, k) => M.priorEmrBanda(an.csmr[k], p, an.sigma_log_csmr[k], an.sigma_log_prev[k],
    e.tabla_fraccion_aguda.fraccion_aguda, e.tabla_fraccion_aguda.factor_sd));
  comparar('prior de la EMR', fa.map((x) => x.mu_log), e.tabla_fraccion_aguda.mu_log, 1e-14);
  comparar('prior de la EMR', fa.map((x) => x.sd_log), e.tabla_fraccion_aguda.sd_log, 1e-14);
  comparar('sigmaLog', an.prev_lower.map((l, k) => M.sigmaLog(l, an.prev_upper[k])), an.sigma_log_prev, 1e-14);
  comparar('prior de la EMR', M.techoEmr(e.tabla.mu_log).cota, e.techo.cota, 1e-15);
  comparar('prior de la EMR', M.techoEmr(e.tabla.mu_log).emr_max_ancla, e.techo.emr_max, 1e-15);
  comparar('prior de la EMR', M.techoEmr(e.tabla.mu_log, 2).cota, e.techo_k2.cota, 1e-15);
  for (const en of e.en_nudos) {
    const tabla = e.tabla.sex_id.map((sx, k) => ({ sx, inicio: e.tabla.inicio[k], fin: e.tabla.fin[k], mu_log: e.tabla.mu_log[k], sd_log: e.tabla.sd_log[k] }))
      .filter((t) => t.sx === en.sexo);
    const r = M.emrEnNudos(tabla, en.nudos, [0, 0.25]);
    comparar('prior de la EMR', r.mu_log, en.mu_log, 0, 'abs');
    comparar('prior de la EMR', r.sd_log, en.sd_log, 0, 'abs');
  }
  const pl = M.emrPlano(e.plano.nudos, e.plano.cota_entrada);
  comparar('prior de la EMR', pl.mu_log, e.plano.mu_log, 1e-15);
  comparar('prior de la EMR', pl.cota, e.plano.cota, 0, 'abs');
  comparar('prior de la EMR', M.emrPlano([30], e.plano_piso.cota_entrada).cota, e.plano_piso.cota, 0, 'abs');
  const dado = e.dado.inicio.map((a, k) => ({ inicio: a, fin: e.dado.fin[k], mu_log: e.dado.mu_log_tabla[k], sd_log: e.dado.sd_log_tabla[k] }));
  const dr = M.emrDadoEnNudos(dado, e.dado.nudos, [0, 0.25]);
  comparar('prior de la EMR', dr.mu_log, e.dado.mu_log, 0, 'abs');
  comparar('prior de la EMR', dr.sd_log, e.dado.sd_log, 0, 'abs');
  // los contextos: el informativo del ejemplo, el plano y el cero
  const ctxE = fx.contextos.ejemplo.sexos[0].ctx;
  const tabla1 = e.tabla.sex_id.map((sx, k) => ({ sx, inicio: e.tabla.inicio[k], fin: e.tabla.fin[k], mu_log: e.tabla.mu_log[k], sd_log: e.tabla.sd_log[k] })).filter((t) => t.sx === 1);
  comparar('prior de la EMR', M.emrEnNudos(tabla1, ctxE.nudos, ctxE.emr.cota).mu_log, ctxE.emr.mu_log, 0, 'abs');
  comparar('prior de la EMR', M.emrEnNudos(tabla1, ctxE.nudos, ctxE.emr.cota).sd_log, ctxE.emr.sd_log, 0, 'abs');
  const ctxP = fx.contextos.plano.sexos[0].ctx;
  comparar('prior de la EMR', M.emrPlano(ctxP.nudos, [0, 0.25]).cota, ctxP.emr.cota, 0, 'abs');
  comparar('prior de la EMR', M.emrPlano(ctxP.nudos, [0, 0.25]).mu_log, ctxP.emr.mu_log, 1e-15);
  const cero = M.emrCero(), ctxC = fx.contextos.cero.sexos[0].ctx;
  assert.deepEqual([cero.cero, cero.plano, cero.cota], [ctxC.emr.cero, ctxC.emr.plano, ctxC.emr.cota]);
});

// ---- Muestreo y diagnósticos ------------------------------------------------------------------------------------

test('rhat y ess reproducen dl_rhat() y dl_ess(); autocorrelacion, stats::acf()', () => {
  const d = fx.diagnosticos;
  comparar('rhat', M.rhat(d.grandes.cadenas), d.grandes.rhat, 1e-10);
  comparar('ess', M.ess(d.grandes.cadenas), d.grandes.ess, 1e-10);
  comparar('ess', M.ess(d.grandes.cadenas, 10), d.grandes.ess_lag10, 1e-10);
  comparar('rhat', M.rhat(d.chicas.cadenas), d.chicas.rhat, 1e-10);
  comparar('ess', M.ess(d.chicas.cadenas), d.chicas.ess, 1e-10);
  comparar('rhat', M.rhat(d.una_columna.cadenas), d.una_columna.rhat, 1e-10);
  comparar('ess', M.ess(d.una_columna.cadenas, 5), d.una_columna.ess, 1e-10);
  // una cadena de un parámetro también puede darse como arreglo de números
  comparar('rhat', M.rhat(d.una_columna.cadenas.map((c) => c.map((f) => f[0]))), d.una_columna.rhat, 1e-10);
  const a1 = d.grandes.cadenas[0].map((f) => f[0]);
  comparar('autocorrelacion (acf)', M.autocorrelacion(a1, 30), d.grandes.acf_a1, 1e-12, 'abs');
  comparar('sumaGeyer', M.sumaGeyer(d.geyer.autocor), d.geyer.valor, 0, 'abs');
});

test('escalaInicial, escalaAdaptada, formaPropuesta, semillaDerivada y la regla de Metropolis', () => {
  const m = fx.mh;
  comparar('muestreador (escalas, forma, semillas)', m.escala_inicial.d.map(M.escalaInicial), m.escala_inicial.valor, 0, 'rel');
  comparar('muestreador (escalas, forma, semillas)', M.escalaAdaptada(m.escala_adaptada.escala, m.escala_adaptada.aceptacion, m.escala_adaptada.k),
    m.escala_adaptada.valor, 1e-15);
  compararMatriz('muestreador (forma de la propuesta)', M.formaPropuesta(m.forma.theta_b), m.forma.L, 1e-12, 'abs');
  comparar('muestreador (escalas, forma, semillas)',
    m.semillas.semilla.map((s, k) => M.semillaDerivada(s, m.semillas.cadena[k], m.semillas.desplazamiento[k])), m.semillas.valor, 0, 'abs');
  assert.deepEqual(M.propuestaMh([1, 2, 3], [1, 2], 0.5, [[1, 0], [0.5, 2]], [1, -1]), [1, 2.5, 2.25]);
  assert.equal(M.aceptaMh(-Infinity, -3, 0.999), false);
  assert.equal(M.aceptaMh(-3, -3.2, Math.exp(0.1)), true);
});

// ---- Estimación subnacional -------------------------------------------------------------------------------------

test('renormalizarBanda reproduce el factor y las q renormalizadas de dl_cascada()', () => {
  for (const c of fx.subnacional.renormalizacion) {
    const r = M.renormalizarBanda(c.q_nac_edades, c.N_nac, c.q_d_edades, c.N_d);
    comparar('renormalizar (factor)', r.factor, c.factor, 1e-14, 'rel', `${c.columna} sexo ${c.sexo} banda ${c.banda}`);
    r.q_d_edades.forEach((fila, k) => comparar('renormalizar (q renormalizadas)', fila, c.q_d_renorm[k], 1e-14));
    // la suma ponderada por población de las ubicaciones es la nacional
    const total = M.suma(r.q_d_edades.map((f, k) => M.media(f) * c.N_d[k]));
    assert.ok(Math.abs(total - r.q_nac * c.N_nac) / (r.q_nac * c.N_nac) < 1e-12);
  }
});

test('pesosEdad, desplazamientoLog, transformar, sdTransformada y sdDelta reproducen R/cascada.R y R/betas.R', () => {
  const sn = fx.subnacional;
  for (const c of sn.pesos_edad)
    compararMatriz('pesosEdad', M.pesosEdad(sn.edades_P, filasDe(c.bandas), c.fuera, c.interp), c.P, 1e-15, 'abs', `${c.fuera} ${c.interp}`);
  const P = M.pesosEdad(sn.edades_P, filasDe(sn.pesos_edad[0].bandas), 'cero', 'lineal');
  sn.desplazamiento.dX.forEach((dX, k) =>
    comparar('desplazamientoLog', M.desplazamientoLog(dX, P, sn.desplazamiento.beta[k], sn.desplazamiento.kappa), sn.desplazamiento.delta[k], 1e-15, 'abs'));
  const t = sn.transformaciones;
  t.tipos.forEach((tipo, k) => {
    comparar('transformaciones', t.x.map((x) => M.transformar(x, tipo)), t.T[k], 1e-15);
    comparar('transformaciones', M.sdTransformada(0.2, 0.6, tipo), t.sd_T[k], 1e-15);
    comparar('transformaciones', M.sdDelta(0.3, 0.02, tipo), t.sd_delta[k], 1e-15);
  });
  comparar('transformaciones', [M.sdTransformada(0.5, 0.5, 'log'), M.sdTransformada(NaN, 0.5, 'log')], t.sd_T_degenerada, 0, 'abs');
});

// ---- AVD --------------------------------------------------------------------------------------------------------

test('betaMomentos, renormalizarProporciones y avdBanda reproducen R/avd.R', () => {
  for (const c of fx.avd.beta_momentos) {
    const b = M.betaMomentos(c.media, c.inferior, c.superior);
    if ('constante' in c) { assert.ok('constante' in b, `media ${c.media}`); assert.equal(b.constante, c.constante); }
    else {
      comparar('betaMomentos', [b.forma1, b.forma2], [c.forma1, c.forma2], 1e-13);
      assert.equal(b.alfa, b.forma1); assert.equal(b.beta, b.forma2);
    }
  }
  assert.equal(M.betaMomentos(0.95, 0, 1).motivo, 'infactible');
  assert.equal(M.betaMomentos(0, 0, 0).motivo, 'degenerado');
  for (const pr of fx.avd.proporciones) comparar('renormalizarProporciones', M.renormalizarProporciones(pr.pi_muestra), pr.pi, 1e-15);
  for (const c of fx.avd.bandas) {
    const pr = fx.avd.proporciones[c.draw - 1];
    comparar('avdBanda', M.avdBanda(c.p_banda, pr.pi, pr.dw, c.como), c.avd, 1e-14);
  }
});

// ---- (d) Casos adversarios --------------------------------------------------------------------------------------

const adv = fx.adversario;

test('ctx.emr se rehace desde el ancla y la configuración en cada variante (prior, techo derivado, plano, tabla, cero)', () => {
  for (const [variante, cv] of Object.entries(fx.contextos)) {
    const e = cv.emr_fuente;
    // dl_prior_emr(): una fila por sexo y banda presentes en la prevalencia y en el csmr del ancla
    const tabla = [];
    e.prev.sex_id.forEach((sx, k) => {
      const j = e.csmr.sex_id.findIndex((s2, m) => s2 === sx && e.csmr.inicio[m] === e.prev.inicio[k] && e.csmr.fin[m] === e.prev.fin[k]);
      if (j < 0) return;
      tabla.push({ sx, inicio: e.prev.inicio[k], fin: e.prev.fin[k],
        ...M.priorEmrBanda(e.csmr.val[j], e.prev.val[k], e.csmr.sigma_log[j], e.prev.sigma_log[k], e.fraccion_aguda, e.factor_sd) });
    });
    let cota = e.cota_cfg;
    if (e.techo_origen === 'derivado') {   // .dl_techo_emr: K_techo por el máximo de csmr / p sobre los dos sexos
      const t = M.techoEmr(tabla.map((x) => x.mu_log), e.factor_techo);
      comparar('prior de la EMR (rehecho desde el ancla)', t.cota, e.cota_cfg, 1e-14, 'rel', `${variante} techo derivado`);
      cota = t.cota;
    }
    for (const s of cv.sexos) {
      let r;
      if (e.tipo === 'cero') r = M.emrCero();
      else if (e.tipo === 'plano_cota') r = M.emrPlano(s.ctx.nudos, e.cota_cfg);
      else if (e.tipo === 'tabla') {
        const dado = e.dado.sex_id.map((sx, k) => ({ sx, inicio: e.dado.inicio[k], fin: e.dado.fin[k], mu_log: e.dado.mu_log[k], sd_log: e.dado.sd_log[k] }));
        r = M.emrDadoEnNudos(dado.filter((x) => x.sx === s.sexo), s.ctx.nudos, cota);
      } else r = M.emrEnNudos(tabla.filter((x) => x.sx === s.sexo), s.ctx.nudos, cota);
      const et = `${variante} sexo ${s.sexo}`;
      comparar('prior de la EMR (rehecho desde el ancla)', r.mu_log, s.ctx.emr.mu_log, 1e-14, 'rel', `${et} mu_log`);
      comparar('prior de la EMR (rehecho desde el ancla)', r.sd_log, s.ctx.emr.sd_log, 1e-14, 'rel', `${et} sd_log`);
      comparar('prior de la EMR (rehecho desde el ancla)', r.cota, s.ctx.emr.cota, 1e-14, 'rel', `${et} cota`);
      assert.deepEqual([r.plano, r.cero], [s.ctx.emr.plano, s.ctx.emr.cero], et);
    }
  }
  // las variantes cubren los cinco caminos de .dl_ctx_emr()
  const tipos = new Set(Object.values(fx.contextos).map((cv) => `${cv.emr_fuente.tipo}/${cv.emr_fuente.techo_origen}`));
  for (const t of ['informativo_edad/impreso', 'informativo_edad/derivado', 'plano_cota/impreso', 'tabla/impreso', 'cero/cero'])
    assert.ok(tipos.has(t), `falta una variante con ${t}`);
});

test('la cota de la EMR es cerrada: f igual al techo o al piso queda dentro; a un ulp, fuera (como en R)', () => {
  const vistos = { techo_igual: 0, techo_fuera: 0, piso_igual: 0, piso_fuera: 0 };
  for (const cv of Object.values(fx.contextos)) for (const s of cv.sexos) {
    const c = M.prepararCtx(s.ctx), K = c.nudos.length;
    for (const th of s.thetas) {
      const m = /^(techo|piso)_([+-]\d)_ulp$/.exec(th.nombre);
      if (!m) continue;
      const k = m[1] === 'techo' ? K + 1 : K, borde = m[1] === 'techo' ? c.emr.cota[1] : c.emr.cota[0];
      const f = Math.exp(th.theta[k]), lp = M.lpComponentes(th.theta, c).total;
      assert.equal(Number.isFinite(lp), Number.isFinite(th.lp.total), `${cv.nombre} ${s.sexo} ${th.nombre}`);
      if (f === borde && Number.isFinite(th.lp.total)) vistos[`${m[1]}_igual`]++;
      if ((m[1] === 'techo' ? f > borde : f < borde) && th.lp.total === -Infinity) vistos[`${m[1]}_fuera`]++;
    }
  }
  for (const [caso, n] of Object.entries(vistos)) assert.ok(n > 0, `ningún theta probó ${caso}`);
  assert.equal(M.fFueraDeCota([Math.log(0.25)], [0, Math.exp(Math.log(0.25))]), false);
});

test('q = 0, RK4 inestable y desborde: los mismos -Inf que R en cada término', () => {
  for (const cv of Object.values(fx.contextos)) for (const s of cv.sexos) {
    const c = M.prepararCtx(s.ctx);
    for (const th of s.thetas.filter((t) => ['q_cero', 'rk4_inestable', 'desborde'].includes(t.nombre))) {
      const lp = M.lpComponentes(th.theta, c);
      for (const nm of M.constantes.LP_NOMBRES) {
        assert.equal(Number.isFinite(lp[nm]), Number.isFinite(th.lp[nm]), `${cv.nombre} ${s.sexo} ${th.nombre} ${nm}`);
        if (!Number.isFinite(th.lp[nm])) assert.ok(Object.is(lp[nm], th.lp[nm]), `${th.nombre} ${nm}: ${lp[nm]} frente a ${th.lp[nm]}`);
      }
    }
  }
});

test('cholesky = chol() de R bit a bit con rho cerca de 1, n = 1 y matrices cualesquiera; L_ancla con lambda 0.25', () => {
  for (const c of adv.cholesky) {
    compararMatriz('cholesky (chol de R)', M.cholAr1(c.n, c.rho), c.U, 0, 'abs', `n ${c.n} rho ${c.rho}`);
    comparar('lpMvnAr1', M.lpMvnAr1(c.z, c.sigma_log, M.cholAr1(c.n, c.rho), c.lambda), c.valor, 1e-12, 'abs', `n ${c.n} rho ${c.rho}`);
  }
  for (const c of adv.cholesky_general) compararMatriz('cholesky (chol de R)', M.cholesky(c.A), c.U, 0, 'abs', `n ${c.A.length}`);
  assert.throws(() => M.cholesky([[1, 2], [2, 1]]), /definida positiva/);
});

test('rhat, ess y sumaGeyer con cadenas cortas, impares y anticorrelacionadas; forma con un parámetro quieto', () => {
  for (const [nombre, d] of Object.entries(adv.diagnosticos)) {
    comparar('rhat', M.rhat(d.cadenas), d.rhat, 1e-12, 'rel', nombre);
    comparar('ess', M.ess(d.cadenas), d.ess, 1e-12, 'rel', nombre);
    comparar('ess', M.ess(d.cadenas, 2), d.ess_lag2, 1e-12, 'rel', `${nombre} max_lag 2`);
    comparar('ess', M.ess(d.cadenas, 3), d.ess_lag3, 1e-12, 'rel', `${nombre} max_lag 3`);
  }
  for (const g of adv.geyer) comparar('sumaGeyer', M.sumaGeyer(g.autocor), g.valor, 0, 'abs');
  compararMatriz('muestreador (forma de la propuesta)', M.formaPropuesta(adv.forma_quieta.theta_b), adv.forma_quieta.L, 1e-12, 'abs');
  // lo que dl_rhat() y dl_ess() rechazan
  assert.throws(() => M.ess(adv.diagnosticos.n4.cadenas, 1), /max_lag/);
  assert.throws(() => M.ess([[[1], [2], [3]]]), /al menos 4/);
  assert.throws(() => M.rhat([[[1], [2], [3], [4]], [[1], [2], [3]]]), /mismas filas/);
});

test('pesosEdad con bandas desordenadas, huecos, banda abierta y empates; desplazamientoLog en el orden de P', () => {
  const pe = adv.pesos_edad, bandas = filasDe(pe.bandas);
  const ids_ordenados = bandas.slice().sort((a, b) => a.inicio - b.inicio).map((b) => b.age_group_id);
  for (const c of pe.casos) {
    assert.deepEqual(ids_ordenados, c.filas_id);   // las filas de P van en el orden de las bandas por edad, como en R
    compararMatriz('pesosEdad', M.pesosEdad(pe.edades, bandas, c.fuera, c.interp), c.P, 0, 'abs', `${c.fuera} ${c.interp}`);
  }
  const P = M.pesosEdad(pe.edades, bandas, 'vecina', 'lineal'), d = adv.desplazamiento;
  d.dX.forEach((dX, k) => comparar('desplazamientoLog', M.desplazamientoLog(dX, P, d.beta[k], d.kappa), d.delta[k], 0, 'abs'));
});

test('betaMomentos en los bordes: v = media (1 - media) es infactible, v < 1e-12 degenerado, intervalo invertido', () => {
  const motivos = new Set();
  for (const c of adv.beta_bordes) {
    const b = M.betaMomentos(c.media, c.inferior, c.superior);
    assert.equal(b.v, c.v);
    assert.equal(b.motivo ?? 'beta', c.motivo, `media ${c.media} [${c.inferior}, ${c.superior}]`);
    motivos.add(c.motivo);
    if (c.motivo === 'beta') comparar('betaMomentos', [b.forma1, b.forma2], [c.forma1, c.forma2], 0, 'rel');
    else assert.equal(b.constante, c.constante);
  }
  assert.deepEqual([...motivos].sort(), ['beta', 'degenerado', 'infactible']);
});

test('el techo de la cascada: log f igual a log(techo) no trunca; a un ulp por encima, sí', () => {
  const ctx = M.prepararCtx(fx.contextos.ejemplo.sexos[0].ctx);
  for (const t of adv.techo_cascada) {
    // si Math.log de V8 y log de glibc difirieran aquí, la decisión podría cambiar: la prueba lo detectaría
    assert.equal(Math.log(t.techo_f), t.log_techo, `log(${t.techo_f})`);
    t.casos.forEach((c, k) => {
      const s = M.edoLogMedia(t.log_i_media, c.log_f_media, ctx, t.techo_f);
      assert.equal(s.truncado, c.truncado, `techo ${t.techo_f} caso ${k}`);
      comparar('logTasasMedia y techo de la cascada', s.p, c.p, 1e-12, 'rel');
      comparar('logTasasMedia y techo de la cascada', s.f, c.f, 1e-12, 'rel');
    });
    assert.deepEqual(t.casos.map((c) => c.truncado), [0, 1]);
  }
});

// ---- (d) Entradas de las escenas: Float64Array, formas de la remisión y errores que R también da -----------------

test('Float64Array vale donde vale un arreglo (theta, remisión, cadenas, tasas)', () => {
  const c = fx.edo_bordes.find((x) => x.nombre === 'nsub7_33_97_remision_vector');
  const op = { edad_inicio: c.edad_inicio, edad_fin: c.edad_fin, nsub: c.nsub };
  const conArreglo = M.edo(v.log_i, v.log_f, v.nudos, { ...op, remision: c.remision });
  const conF64 = M.edo(Float64Array.from(v.log_i), Float64Array.from(v.log_f), v.nudos, { ...op, remision: Float64Array.from(c.remision) });
  comparar('edo en los bordes (p, i, f, p_malla)', conF64.p, c.p, 1e-12, 'rel', 'Float64Array');
  assert.deepEqual(Array.from(conF64.p), Array.from(conArreglo.p));
  const s = fx.contextos.adversario.sexos[0], ctx = M.prepararCtx(s.ctx);
  for (const th of s.thetas) assert.ok(Object.is(M.logPosterior(Float64Array.from(th.theta), ctx), M.logPosterior(th.theta, ctx)), th.nombre);
  const cad = adv.diagnosticos.n7.cadenas;
  assert.deepEqual(M.rhat(cad.map((ch) => ch.map((f) => Float64Array.from(f)))), M.rhat(cad));
  assert.deepEqual(M.resolverEdo(Float64Array.from(v.i_media), Float64Array.from(v.f_media), 0, 0, 5), v.p_malla);
});

test('la remisión del contexto: número, arreglo por punto o {valor, por_edad} dan la misma r', () => {
  const s = fx.contextos.nsub10.sexos[0], n = s.r.r_media.length;
  const conCfg = M.prepararCtx(s.ctx);
  comparar('remisionPorEdad', conCfg.r_media, s.r.r_media, 0, 'abs', 'configuración');
  comparar('remisionPorEdad', M.prepararCtx({ ...s.ctx, remision: s.r.r_media }).r_media, s.r.r_media, 0, 'abs', 'arreglo');
  comparar('remisionPorEdad', M.prepararCtx({ ...s.ctx, remision: 0.05 }).r_media, new Array(n).fill(0.05), 0, 'abs', 'número');
  comparar('remisionPorEdad', M.prepararCtx({ ...s.ctx, remision: { valor: 0.05 } }).r_media, new Array(n).fill(0.05), 0, 'abs');
  comparar('remisionPorEdad', M.prepararCtx({ ...s.ctx, remision: undefined }).r_media, new Array(n).fill(0), 0, 'abs', 'sin remisión');
});

test('errores que R también da: theta corto, nudos o bandas sin ordenar, ancla desordenada', () => {
  const s = fx.contextos.ejemplo.sexos[0], ctx = M.prepararCtx(s.ctx), K = ctx.nudos.length, th = s.thetas[1].theta;
  assert.throws(() => M.logPosterior(th.slice(0, K), ctx), /hacen falta 14/);
  assert.throws(() => M.edoCtx(th.slice(0, 2 * K - 1), ctx), /theta trae/);
  assert.equal(M.logPosterior([...th, 99], ctx), M.logPosterior(th, ctx));   // valores de más se ignoran, como en R
  const cero = M.prepararCtx(fx.contextos.cero.sexos[0].ctx);   // con la EMR cero, theta = log i (los K valores bastan)
  assert.ok(Number.isFinite(M.logPosterior(fx.contextos.cero.sexos[0].thetas[1].theta, cero)));
  assert.throws(() => M.matrizW([40, 30, 50], [35]), /creciente/);
  assert.throws(() => M.bandaDe([42], [{ inicio: 45, fin: 50 }, { inicio: 40, fin: 45 }]), /creciente/);
  const perm = [2, 0, 1, ...s.ctx.ancla.inicio.slice(3).map((_, k) => k + 3)];
  const desordenada = { ...s.ctx, ancla: { ...s.ctx.ancla, ...Object.fromEntries(['age_group_id', 'inicio', 'fin', 'val', 'sigma_log'].map((c) => [c, perm.map((k) => s.ctx.ancla[c][k])])) } };
  assert.throws(() => M.prepararCtx(desordenada), /ancla.inicio debe ser creciente/);
});

// ---- Resumen ----------------------------------------------------------------------------------------------------

test('resumen: mayor diferencia observada por grupo (JS frente a R)', (t) => {
  for (const [grupo, { dif, tipo }] of maximos) t.diagnostic(`${grupo}: ${dif.toExponential(2)} (${tipo === 'rel' ? 'relativa' : 'absoluta'})`);
  assert.ok(maximos.size > 20);
});
