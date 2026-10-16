// auditar.mjs: comprueba que una escena cumple el contrato y las reglas de movimiento, sin mirarla.
//
// Uso (desde animaciones/):  node herramientas/auditar.mjs escenas/01-enfermedad-muerte [--paso 1]
//
// Comprobaciones (FALLA detiene; AVISO se revisa a ojo):
//   contrato     el código de la escena no usa relojes ni azar sin semilla, y solo carga bibliotecas fijadas a una
//                versión (KaTeX de jsDelivr) y fuentes de Google Fonts; la página carga sin errores
//   pureza       el mismo cuadro da el mismo SVG sin importar el orden en que se dibujan los cuadros, y la misma
//                imagen al volver a él
//   cuadro 0     el primer cuadro es una composición terminada: el título se lee desde el cuadro 0
//   lectura      cada texto legible (data-lee) queda quieto mientras se lee y dura al menos 0,3 s por palabra + 0,5 s
//   márgenes     ningún texto visible se sale del cuadro (8 px)
//   choques      dos textos legibles no se tapan (AVISO)
//   simultáneos  no entran tres o más textos a la vez (AVISO: una sola cosa principal por momento)
//   escena       las comprobaciones numéricas que declara la escena (auditoria), por ejemplo que la curva dibujada
//                sea la que calcula el paquete
// El informe queda en salida/<id>/auditoria.json.
import { readFileSync, readdirSync, writeFileSync, mkdirSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import { rutaHtml, abrirNavegador, abrirEscena, congelar } from './render.mjs';

const RAIZ = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

const PROHIBIDOS = [
  [/\bDate\.now\s*\(/, 'Date.now()'],
  [/\bnew\s+Date\s*\(/, 'new Date()'],
  [/\bsetTimeout\s*\(/, 'setTimeout'],
  [/\bsetInterval\s*\(/, 'setInterval'],
  [/\bMath\.random\s*\(/, 'Math.random() sin semilla (usa DL.azar o DL.hash01)'],
  [/\bperformance\.now\s*\(/, 'performance.now()'],
  [/\brequestAnimationFrame\s*\(/, 'requestAnimationFrame (la vista previa la pone contrato.js)'],
  [/@keyframes|\btransition\s*:/, 'animaciones o transiciones CSS (todo se mueve desde renderAt)'],
];

function lintar(html) {
  const dir = path.dirname(html);
  const fallas = [];
  const archivos = [html, ...readdirSync(dir).filter((f) => f.endsWith('.js')).map((f) => path.join(dir, f))];
  for (const a of archivos) {
    const src = readFileSync(a, 'utf8');
    for (const [re, nombre] of PROHIBIDOS) {
      const lineas = src.split('\n');
      lineas.forEach((l, k) => { if (re.test(l)) fallas.push(`${path.basename(a)}:${k + 1} usa ${nombre}`); });
    }
  }
  const src = readFileSync(html, 'utf8');
  for (const m of src.matchAll(/(?:src|href)\s*=\s*"(https?:\/\/[^"]+)"/g)) {
    const url = m[1];
    const fuente = /^https:\/\/fonts\.googleapis\.com\/css2\?/.test(url);
    const fijada = /^https:\/\/cdn\.jsdelivr\.net\/npm\/[a-z0-9@._-]+@\d+\.\d+\.\d+\//i.test(url);
    if (!fuente && !fijada) fallas.push(`carga ${url}: solo fuentes de Google Fonts y bibliotecas de jsDelivr con versión exacta`);
  }
  return fallas;
}

// Recorre los cuadros y mide los textos legibles. Corre dentro de la página.
function medirTextos(paso) {
  const E = window.ESCENA;
  const salida = [];
  function opacidad(el) {
    let o = 1;
    for (let n = el; n && n.nodeType === 1; n = n.parentNode) {
      const cs = getComputedStyle(n);
      o *= Number(cs.opacity);
      if (cs.display === 'none' || cs.visibility === 'hidden') return 0;
      if (n.id === 'escenario') break;
    }
    return o;
  }
  for (let i = 0; i < E.cuadros; i += paso) {
    window.renderAt(i / E.fps);
    const fila = [];
    document.querySelectorAll('[data-lee]').forEach((el) => {
      const r = el.getBoundingClientRect();
      fila.push({
        id: el.getAttribute('data-lee'), palabras: Number(el.getAttribute('data-palabras') || 0),
        seguir: el.hasAttribute('data-seguir'), o: opacidad(el),
        x: r.left, y: r.top, w: r.width, h: r.height,
      });
    });
    salida.push({ i, textos: fila });
  }
  return salida;
}

function analizarLectura(cuadros, E, paso) {
  const fallas = [];
  const avisos = [];
  const tolerancia = 1.5 * paso / E.fps;
  const porId = new Map();
  for (const { i, textos } of cuadros) {
    for (const x of textos) {
      if (!porId.has(x.id)) porId.set(x.id, []);
      porId.get(x.id).push({ i, ...x });
    }
  }
  for (const [id, filas] of porId) {
    // Tramos consecutivos de cuadros en que el texto es legible (opacidad ≥ 0,98).
    let tramo = [];
    const tramos = [];
    let previo = null;
    for (const f of filas) {
      const legible = f.o >= 0.98;
      if (legible && previo !== null && f.i === previo + paso) tramo.push(f);
      else if (legible) { if (tramo.length) tramos.push(tramo); tramo = [f]; }
      else if (tramo.length) { tramos.push(tramo); tramo = []; }
      previo = legible ? f.i : null;
    }
    if (tramo.length) tramos.push(tramo);
    for (const tr of tramos) {
      const a = tr[0], b = tr[tr.length - 1];
      const dura = (b.i - a.i + paso) / E.fps;
      const llegaAlFinal = b.i + paso >= E.cuadros;
      const necesita = 0.3 * a.palabras + 0.5;
      if (dura + tolerancia < necesita && !llegaAlFinal) {
        fallas.push(`«${id}» se lee ${dura.toFixed(2)} s (cuadros ${a.i}-${b.i}); con ${a.palabras} palabras necesita ${necesita.toFixed(2)} s`);
      }
      if (!a.seguir) {
        // Quieto mientras se lee, salvo dentro de un movimiento de cámara declarado (ESCENA.camaras): cada tramo
        // entre cámaras debe estar quieto por su cuenta.
        const enCamara = (i) => (E.camaras || []).some(([c0, c1]) => i / E.fps >= c0 && i / E.fps <= c1);
        let ref = null, mov = 0, donde = a.i;
        for (const f of tr) {
          if (enCamara(f.i)) { ref = null; continue; }
          if (!ref) { ref = f; continue; }
          const d = Math.max(Math.abs(f.x - ref.x), Math.abs(f.y - ref.y));
          if (d > mov) { mov = d; donde = f.i; }
        }
        if (mov > 0.75) fallas.push(`«${id}» se mueve ${mov.toFixed(1)} px mientras es legible (cuadros ${a.i}-${b.i}, cuadro ${donde})`);
      }
    }
  }
  // Márgenes, choques y entradas simultáneas.
  const choques = new Map();
  const entradas = [];
  let antes = new Set();
  for (const { i, textos } of cuadros) {
    const ahora = new Set();
    for (const x of textos) {
      if (x.o > 0.05 && (x.x < 8 || x.y < 8 || x.x + x.w > E.ancho - 8 || x.y + x.h > E.alto - 8)) {
        fallas.push(`«${x.id}» se sale del cuadro en el cuadro ${i} (${Math.round(x.x)}, ${Math.round(x.y)}, ${Math.round(x.w)} x ${Math.round(x.h)})`);
      }
      if (x.o >= 0.5) ahora.add(x.id);
    }
    const nuevos = [...ahora].filter((id) => !antes.has(id));
    if (nuevos.length) entradas.push({ i, ids: nuevos });
    antes = ahora;
    const vis = textos.filter((x) => x.o >= 0.5 && x.w > 0 && x.h > 0);
    for (let a = 0; a < vis.length; a++) {
      for (let b = a + 1; b < vis.length; b++) {
        const p = vis[a], q = vis[b];
        const sx = Math.min(p.x + p.w, q.x + q.w) - Math.max(p.x, q.x);
        const sy = Math.min(p.y + p.h, q.y + q.h) - Math.max(p.y, q.y);
        if (sx > 4 && sy > 4) {
          const clave = [p.id, q.id].sort().join(' y ');
          if (!choques.has(clave)) choques.set(clave, i);
        }
      }
    }
  }
  for (const [clave, i] of choques) avisos.push(`choque: ${clave} se tapan desde el cuadro ${i}`);
  // Más de dos textos que entran dentro de 0,1 s (excepto el cuadro 0, que es la composición inicial).
  const ventana = Math.max(1, Math.round(0.1 * E.fps));
  for (let k = 0; k < entradas.length; k++) {
    if (entradas[k].i === 0) continue;
    const ids = new Set(entradas[k].ids);
    for (let j = k + 1; j < entradas.length && entradas[j].i - entradas[k].i <= ventana; j++) entradas[j].ids.forEach((x) => ids.add(x));
    if (ids.size >= 3) avisos.push(`simultáneos: ${[...ids].join(', ')} entran juntos en el cuadro ${entradas[k].i}`);
  }
  return { fallas: [...new Set(fallas)], avisos: [...new Set(avisos)] };
}

async function principal() {
  const args = process.argv.slice(2);
  const escenaArg = args.find((a) => !a.startsWith('--'));
  const k = args.indexOf('--paso');
  const paso = k >= 0 ? Number(args[k + 1]) : 1;
  if (!escenaArg) { console.error('Uso: node herramientas/auditar.mjs escenas/<escena> [--paso 1]'); process.exit(2); }
  const html = rutaHtml(escenaArg);
  const informe = { escena: null, fallas: {}, avisos: {}, escenaChecks: [] };
  const falla = (grupo, msg) => { (informe.fallas[grupo] ||= []).push(msg); };
  const aviso = (grupo, msg) => { (informe.avisos[grupo] ||= []).push(msg); };

  for (const f of lintar(html)) falla('contrato', f);

  const navegador = await abrirNavegador();
  try {
    const { pagina, escena: E, errores } = await abrirEscena(navegador, html);
    informe.escena = E;

    // Pureza: el SVG de cada cuadro de una muestra, dibujado hacia adelante y hacia atrás, es el mismo.
    const muestra = Array.from({ length: 24 }, (_, j) => Math.floor(((j * 7919) % 997) / 997 * E.cuadros));
    const ida = await pagina.evaluate((m) => m.map((i) => { window.renderAt(i / window.ESCENA.fps); return document.getElementById('lienzo').innerHTML; }), muestra);
    const vuelta = await pagina.evaluate((m) => m.slice().reverse().map((i) => { window.renderAt(i / window.ESCENA.fps); return document.getElementById('lienzo').innerHTML; }), muestra);
    vuelta.reverse();
    muestra.forEach((i, j) => { if (ida[j] !== vuelta[j]) falla('pureza', `el cuadro ${i} cambia según los cuadros dibujados antes`); });
    const A = Math.floor(0.37 * E.cuadros), B = Math.floor(0.71 * E.cuadros);
    await congelar(pagina, A / E.fps); const a1 = await pagina.screenshot();
    await congelar(pagina, B / E.fps); await pagina.screenshot();
    await congelar(pagina, A / E.fps); const a2 = await pagina.screenshot();
    if (!a1.equals(a2)) falla('pureza', `el cuadro ${A} no da la misma imagen al volver a él`);

    // Cuadro 0: composición terminada, con el título legible.
    const c0 = await pagina.evaluate(() => {
      window.renderAt(0);
      const t = document.querySelector('[data-lee="titulo"]');
      return { elementos: document.querySelectorAll('#lienzo *').length, titulo: t ? Number(getComputedStyle(t).opacity) : 0 };
    });
    if (c0.elementos < 6) falla('cuadro 0', `el cuadro 0 tiene solo ${c0.elementos} elementos: debe ser una composición terminada`);
    if (c0.titulo < 0.98) falla('cuadro 0', 'el título (data-lee="titulo") no se lee en el cuadro 0');

    // Lectura, márgenes, choques y entradas simultáneas.
    const cuadros = await pagina.evaluate(medirTextos, paso);
    const lectura = analizarLectura(cuadros, E, paso);
    lectura.fallas.forEach((f) => falla('lectura', f));
    lectura.avisos.forEach((f) => aviso('composición', f));

    // Comprobaciones de la escena.
    const lista = await pagina.evaluate(() => window.__auditoria.lista);
    for (let j = 0; j < lista.length; j++) {
      const r = await pagina.evaluate((j) => window.__auditoria.correr(j), j);
      informe.escenaChecks.push(r);
      if (!r.ok) falla('escena', `${r.nombre} (t = ${r.t} s): ${r.detalle}`);
    }
    if (!lista.length) aviso('escena', 'la escena no declara comprobaciones numéricas (auditoria)');

    errores.forEach((e) => falla('contrato', e));
    await pagina.close();
  } finally {
    await navegador.close();
  }

  const id = informe.escena ? informe.escena.id : path.basename(path.dirname(html));
  const dir = path.join(RAIZ, 'salida', id);
  mkdirSync(dir, { recursive: true });
  writeFileSync(path.join(dir, 'auditoria.json'), JSON.stringify(informe, null, 2));

  const grupos = ['contrato', 'pureza', 'cuadro 0', 'lectura', 'escena'];
  console.log(`Auditoría de «${id}» (${informe.escena?.cuadros} cuadros, ${informe.escena?.fps} por segundo)`);
  for (const g of grupos) {
    const f = informe.fallas[g] || [];
    const extra = g === 'escena' ? ` (${informe.escenaChecks.filter((x) => x.ok).length}/${informe.escenaChecks.length})` : '';
    console.log(`  ${f.length ? '✗' : '✓'} ${g}${extra}`);
    f.forEach((x) => console.log(`      ${x}`));
  }
  for (const [g, lista] of Object.entries(informe.avisos)) {
    console.log(`  ! ${g}`);
    lista.forEach((x) => console.log(`      ${x}`));
  }
  const total = Object.values(informe.fallas).flat().length;
  console.log(total ? `${total} falla(s).` : 'Todo en orden.');
  process.exit(total ? 1 : 0);
}

principal().catch((e) => { console.error(e.message || e); process.exit(1); });
