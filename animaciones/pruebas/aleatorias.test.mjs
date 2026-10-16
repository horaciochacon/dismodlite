// Prueba diferencial de lib/modelo.js con casos al azar (pruebas/fixtures_aleatorias.R, bloque fx.aleatorio de
// pruebas/fixtures.json): contextos sintéticos de la log-posterior pasados por .dl_ctx() y .dl_lp_componentes(),
// dl_edo() con entradas al azar, bandas, pesos del proxy, diagnósticos, chol de AR(1) y la Beta por momentos.
//
//   cd animaciones && Rscript pruebas/fixtures.R && node --test pruebas/
//
// Tolerancias: las mismas de pruebas/modelo.test.mjs; la log-posterior, 1e-9 por max(1, |lp|) (con theta lejos de
// la posterior lp llega a -1e7 y 1e-9 absoluto sería pedir 1e-16 relativo), y los no finitos deben coincidir
// exactamente (-Inf con -Inf, NaN con NaN).
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import vm from 'node:vm';

const aqui = path.dirname(fileURLToPath(import.meta.url));
vm.runInThisContext(readFileSync(path.join(aqui, '..', 'lib', 'modelo.js'), 'utf8'), { filename: 'lib/modelo.js' });
const M = globalThis.DL.modelo;
const fx = JSON.parse(readFileSync(path.join(aqui, 'fixtures.json'), 'utf8'), M.reviverR);
const az = fx.aleatorio;

const maximos = new Map();
function registrar(grupo, dif, tipo) {
  const previo = maximos.get(grupo);
  if (!previo || dif > previo.dif) maximos.set(grupo, { dif, tipo });
}
function diferencia(a, b, tipo) {
  if (Object.is(a, b) || a === b) return 0;
  if (Number.isNaN(a) && Number.isNaN(b)) return 0;
  if (!Number.isFinite(a) || !Number.isFinite(b)) return Infinity;
  const d = Math.abs(a - b);
  if (tipo === 'rel') return d / Math.max(Math.abs(b), 1e-300);
  if (tipo === 'lp') return d / Math.max(1, Math.abs(b));
  return d;
}
const comoArreglo = (x) => (Array.isArray(x) || ArrayBuffer.isView(x) ? Array.from(x) : [x]);
function comparar(grupo, actual, esperado, tol, tipo = 'rel', etiqueta = '') {
  const a = comoArreglo(actual), e = comoArreglo(esperado);
  assert.equal(a.length, e.length, `${grupo} ${etiqueta}: largo ${a.length} frente a ${e.length}`);
  let max = 0, donde = -1;
  for (let k = 0; k < e.length; k++) {
    const d = diferencia(a[k], e[k], tipo);
    if (d > max) { max = d; donde = k; }
  }
  registrar(grupo, max, tipo);
  assert.ok(max <= tol, `${grupo} ${etiqueta}: diferencia ${tipo} ${max} > ${tol} en [${donde}] (JS ${a[donde]}, R ${e[donde]})`);
}
const compararMatriz = (grupo, a, e, tol, tipo, etq = '') => {
  assert.equal(a.length, e.length, `${grupo} ${etq}: filas`);
  a.forEach((fila, k) => comparar(grupo, fila, e[k], tol, tipo, `${etq} fila ${k}`));
};
const deRle = (r) => r.valor.flatMap((v, k) => new Array(r.veces[k]).fill(v));

// ---- Contextos sintéticos ------------------------------------------------------------------------------------------

test(`contextos al azar: prepararCtx reproduce .dl_ctx() (${az.contextos.length} contextos)`, () => {
  az.contextos.forEach((cz, n) => {
    const c = M.prepararCtx(cz.ctx), r = cz.r, et = `contexto ${n}`;
    assert.equal(c.edades_media.length, r.n_media, et);
    comparar('azar: mallas, r, W, chol', c.r_media, deRle(r.r_media), 0, 'abs', `${et} r_media`);
    comparar('azar: mallas, r, W, chol', c.idx_anual_malla, r.idx_anual_malla, 0, 'abs', et);
    comparar('azar: mallas, r, W, chol', c.anual, r.anual, 0, 'abs', et);
    comparar('azar: mallas, r, W, chol', c.W[0], r.W_primera, 0, 'abs', et);
    comparar('azar: mallas, r, W, chol', c.W.at(-1), r.W_ultima, 0, 'abs', et);
    compararMatriz('azar: mallas, r, W, chol', c.ancla.chol_R, r.chol_R, 0, 'abs', et);
    for (const [nm, a, e] of [['ancla', c.pesos_ancla, r.pesos_ancla], ['datos', c.pesos_datos, r.pesos_datos]]) {
      assert.equal(a.length, e.length, `${et} pesos ${nm}`);
      a.forEach((p, k) => {
        comparar('azar: pesos (idx)', p.idx, e[k].idx, 0, 'abs', `${et} ${nm} ${k}`);
        comparar('azar: pesos (w)', p.w, e[k].w, 1e-15, 'rel', `${et} ${nm} ${k}`);
      });
    }
    comparar('azar: thetaInicial', M.thetaInicial(c), r.theta_inicial, 0, 'abs', et);
  });
});

test('contextos al azar: edoCtx reproduce .dl_edo_ctx()', () => {
  az.contextos.forEach((cz, n) => {
    const sol = M.edoCtx(cz.thetas[1].theta, cz.ctx);
    for (const nm of ['p', 'i', 'f']) comparar('azar: edoCtx (p, i, f)', sol[nm], cz.r.edo[nm], 1e-12, 'rel', `contexto ${n} ${nm}`);
  });
});

test('contextos al azar: lpComponentes reproduce .dl_lp_componentes() término a término', () => {
  const vistos = new Set();
  az.contextos.forEach((cz, n) => {
    const c = M.prepararCtx(cz.ctx);
    for (const th of cz.thetas) {
      const lp = M.lpComponentes(th.theta, c);
      for (const nm of M.constantes.LP_NOMBRES) {
        const et = `contexto ${n} ${th.nombre} ${nm}`, a = lp[nm], e = th.lp[nm];
        if (!Number.isFinite(e)) { assert.ok(Object.is(a, e), `${et}: JS ${a}, R ${e}`); vistos.add(`${nm}=${e}`); }
        else comparar(`azar: log-posterior (${nm === 'total' ? 'total' : 'términos'})`, a, e, 1e-9, 'lp', et);
      }
    }
  });
  // los casos al azar llegan a los no finitos de cada término
  for (const k of ['total=-Infinity', 'ancla=-Infinity', 'suavidad=-Infinity']) assert.ok(vistos.has(k), `ningún caso con ${k}`);
});

test('contextos al azar: ctx.emr se rehace desde el ancla (prior por banda, techo derivado, plano, tabla, cero)', () => {
  const tipos = new Set();
  az.contextos.forEach((cz, n) => {
    const e = cz.emr_fuente, et = `contexto ${n} (${e.tipo})`;
    tipos.add(e.tipo);
    const tabla = [];
    e.prev.sex_id.forEach((sx, k) => {
      const j = e.csmr.sex_id.findIndex((s2, m) => s2 === sx && e.csmr.inicio[m] === e.prev.inicio[k] && e.csmr.fin[m] === e.prev.fin[k]);
      if (j >= 0) tabla.push({ sx, inicio: e.prev.inicio[k], fin: e.prev.fin[k],
        ...M.priorEmrBanda(e.csmr.val[j], e.prev.val[k], e.csmr.sigma_log[j], e.prev.sigma_log[k], e.fraccion_aguda, e.factor_sd) });
    });
    let r;
    if (e.tipo === 'cero') r = M.emrCero();
    else if (e.tipo === 'plano_cota') r = M.emrPlano(cz.ctx.nudos, e.cota);
    else if (e.tipo === 'tabla') r = M.emrDadoEnNudos(e.dado.inicio.map((a, k) => ({ inicio: a, fin: e.dado.fin[k], mu_log: e.dado.mu_log[k], sd_log: e.dado.sd_log[k] })), cz.ctx.nudos, e.cota);
    else r = M.emrEnNudos(tabla.filter((x) => x.sx === 1), cz.ctx.nudos, e.cota);
    comparar('azar: prior de la EMR rehecho', r.mu_log, cz.r.emr.mu_log, 1e-14, 'rel', `${et} mu_log`);
    comparar('azar: prior de la EMR rehecho', r.sd_log, cz.r.emr.sd_log, 1e-14, 'rel', `${et} sd_log`);
    comparar('azar: prior de la EMR rehecho', r.cota, cz.r.emr.cota, 0, 'rel', `${et} cota`);
    if (e.techo_derivado) {
      const t = M.techoEmr(tabla.map((x) => x.mu_log), e.factor_techo);
      comparar('azar: prior de la EMR rehecho', t.cota, e.cota, 1e-14, 'rel', `${et} techo derivado`);
    }
  });
  assert.deepEqual([...tipos].sort(), ['cero', 'informativo_edad', 'plano_cota', 'tabla']);
});

// ---- EDO, bandas, proxy ----------------------------------------------------------------------------------------------

test('dl_edo() con entradas al azar', () => {
  az.edo.forEach((c, n) => {
    const s = M.edo(c.log_i, c.log_f, c.nudos, { edad_inicio: c.edad_inicio, edad_fin: c.edad_fin, nsub: c.nsub, remision: c.remision, p0: c.p0 });
    comparar('azar: edo (edades)', s.edades, c.edades, 0, 'abs', `caso ${n}`);
    for (const nm of ['p', 'i', 'f']) comparar('azar: edo (p, i, f)', s[nm], c[nm], 1e-12, 'rel', `caso ${n} ${nm}`);
  });
});

test('bandasFinas, bandaDe y pesosIntervalo con bandas al azar', () => {
  az.bandas.forEach((c, n) => {
    const lim = c.limites.inicio.map((a, k) => ({ inicio: a, fin: c.limites.fin[k], age_group_id: c.limites.age_group_id[k] }));
    const finas = M.bandasFinas(lim);
    comparar('azar: bandas', finas.map((b) => b.age_group_id), c.finas, 0, 'abs', `caso ${n} finas`);
    const fin = lim.filter((b) => b.age_group_id >= 100 && b.age_group_id < 500).sort((a, b) => a.inicio - b.inicio);
    comparar('azar: bandas', M.bandaDe(c.sonda, fin), c.banda_sonda, 0, 'abs', `caso ${n} bandaDe`);
    for (const iv of c.intervalos) {
      if (iv.error) { assert.throws(() => M.pesosIntervalo(c.edades, iv.inicio, iv.fin, fin, c.N), undefined, `caso ${n} [${iv.inicio}, ${iv.fin})`); continue; }
      const pw = M.pesosIntervalo(c.edades, iv.inicio, iv.fin, fin, c.N);
      comparar('azar: pesos (idx)', pw.idx, iv.idx, 0, 'abs', `caso ${n}`);
      comparar('azar: pesos (w)', pw.w, iv.w, 1e-15, 'rel', `caso ${n}`);
    }
  });
});

test('pesosEdad y desplazamientoLog con bandas del proxy al azar', () => {
  az.proxy.forEach((c, n) => {
    const bandas = c.bandas.inicio.map((a, k) => ({ inicio: a, fin: c.bandas.fin[k], age_group_id: c.bandas.age_group_id[k] }));
    const edades = M.edadesMedia(c.edad_inicio, c.edad_fin, c.nsub);
    const P = M.pesosEdad(edades, bandas, c.fuera, c.interp);
    assert.deepEqual(bandas.slice().sort((a, b) => a.inicio - b.inicio).map((b) => b.age_group_id), c.filas_id);
    const denso = c.filas_id.map(() => new Array(edades.length).fill(0));
    c.P.fila.forEach((f, k) => { denso[f][c.P.col[k]] = c.P.valor[k]; });
    compararMatriz('azar: pesosEdad', P, denso, 0, 'abs', `caso ${n} ${c.fuera} ${c.interp}`);
    c.dX.forEach((dX, k) => comparar('azar: desplazamientoLog', M.desplazamientoLog(dX, P, c.beta[k], c.kappa), c.delta[k], 0, 'abs', `caso ${n}`));
  });
});

// ---- Diagnósticos, chol, Beta --------------------------------------------------------------------------------------------

test('rhat, ess y formaPropuesta con cadenas al azar', () => {
  az.diagnosticos.forEach((c, n) => {
    comparar('azar: rhat', M.rhat(c.cadenas), c.rhat, 1e-12, 'rel', `caso ${n}`);
    if (c.ess_error) assert.throws(() => M.ess(c.cadenas, c.max_lag));
    else comparar('azar: ess', M.ess(c.cadenas, c.max_lag), c.ess, 1e-12, 'rel', `caso ${n}`);
    const filas = c.cadenas.flat();
    if (c.forma.flat().some(Number.isNaN)) assert.throws(() => M.formaPropuesta(filas));
    else compararMatriz('azar: formaPropuesta', M.formaPropuesta(filas), c.forma, 1e-12, 'rel', `caso ${n}`);
  });
});

test('cholAr1 y lpMvnAr1 con n y rho al azar', () => {
  for (const c of az.cholesky) {
    compararMatriz('azar: cholAr1', M.cholAr1(c.n, c.rho), c.U, 0, 'abs', `n ${c.n} rho ${c.rho}`);
    comparar('azar: lpMvnAr1', M.lpMvnAr1(c.z, c.sigma_log, M.cholAr1(c.n, c.rho), c.lambda), c.valor, 1e-12, 'lp', `n ${c.n} rho ${c.rho}`);
  }
});

test('betaMomentos con medias e intervalos al azar', () => {
  for (const c of az.beta) {
    const b = M.betaMomentos(c.media, c.inferior, c.superior);
    if ('constante' in c) assert.equal(b.constante, c.constante, `media ${c.media}`);
    else comparar('azar: betaMomentos', [b.forma1, b.forma2], [c.forma1, c.forma2], 0, 'rel', `media ${c.media}`);
  }
});

test('resumen de los casos al azar: mayor diferencia por grupo', (t) => {
  for (const [grupo, { dif, tipo }] of maximos)
    t.diagnostic(`${grupo}: ${dif.toExponential(2)} (${{ rel: 'relativa', abs: 'absoluta', lp: 'relativa a max(1, |lp|)' }[tipo]})`);
  assert.ok(maximos.size > 10);
});
