// render.mjs: congela una escena en cada cuadro y guarda los PNG; con ffmpeg arma el MP4 (y el GIF si se pide); arma
// la hoja de contactos, los fotogramas de las marcas y la piel de cebolla.
//
// Uso (desde animaciones/):
//   node herramientas/render.mjs escenas/01-enfermedad-muerte              cuadros + MP4 + hoja de contactos
//   node herramientas/render.mjs escenas/01-enfermedad-muerte --gif        además, el GIF (960 px, 20 cuadros/s)
//   node herramientas/render.mjs escenas/01-enfermedad-muerte --marcas     solo un fotograma por marca
//   node herramientas/render.mjs escenas/01-enfermedad-muerte --cuadros 0,24,48
//   node herramientas/render.mjs escenas/01-enfermedad-muerte --t 2.5,7.25
//   node herramientas/render.mjs escenas/01-enfermedad-muerte --hoja       borrador: solo la hoja de contactos (12 cuadros)
//   node herramientas/render.mjs escenas/01-enfermedad-muerte --cebolla 90-119:3
//                                    los cuadros 90 a 119, uno de cada 3, superpuestos (espaciado de un movimiento)
//   Opciones: --trabajadores N (páginas en paralelo; por defecto, núcleos - 1, hasta 4), --salida DIR.
//
// Cada PNG es la página congelada en t = i / fps (renderAt), así que cada cuadro es exacto por lenta que sea la
// máquina. Se renderizan los cuadros 0 a N - 1. La salida va a salida/<id>/.
import { chromium } from 'playwright';
import { pathToFileURL, fileURLToPath } from 'node:url';
import { mkdirSync, rmSync, existsSync, statSync, writeFileSync, readdirSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { cpus } from 'node:os';
import path from 'node:path';

const RAIZ = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

function leerArgumentos(argv) {
  const op = { escena: null, banderas: new Set(), valores: {} };
  for (let k = 0; k < argv.length; k++) {
    const a = argv[k];
    if (!a.startsWith('--')) { op.escena = a; continue; }
    const nombre = a.slice(2);
    if (['cuadros', 't', 'trabajadores', 'salida', 'cebolla', 'fps-gif'].includes(nombre)) op.valores[nombre] = argv[++k];
    else op.banderas.add(nombre);
  }
  if (!op.escena) {
    console.error('Uso: node herramientas/render.mjs escenas/<escena> [--marcas | --cuadros 0,24 | --t 2.5 | --hoja | --gif]');
    process.exit(2);
  }
  return op;
}

export function rutaHtml(escena) {
  let p = path.resolve(process.cwd(), escena);
  if (!existsSync(p)) p = path.resolve(RAIZ, escena);
  if (existsSync(p) && statSync(p).isDirectory()) p = path.join(p, 'index.html');
  if (!existsSync(p)) throw new Error(`No encuentro la escena: ${escena}`);
  return p;
}

export async function abrirNavegador() {
  const proxy = process.env.HTTPS_PROXY || process.env.https_proxy;
  return chromium.launch({ proxy: proxy ? { server: proxy } : undefined });
}

// Abre la escena en modo render y espera a que esté lista. Devuelve { pagina, escena, errores }.
export async function abrirEscena(navegador, html, extra = '') {
  const pagina = await navegador.newPage({ viewport: { width: 1920, height: 1080 }, deviceScaleFactor: 1 });
  const errores = [];
  pagina.on('pageerror', (e) => errores.push(`error de la página: ${e.message}`));
  pagina.on('console', (m) => { if (m.type() === 'error') errores.push(`consola: ${m.text()}`); });
  pagina.on('requestfailed', (r) => errores.push(`no se pudo cargar ${r.url()}: ${r.failure()?.errorText}`));
  await pagina.goto(pathToFileURL(html).href + '?render=1' + extra, { waitUntil: 'networkidle' });
  try {
    await pagina.waitForFunction(() => window.__ready === true || window.__error, null, { timeout: 90000 });
  } catch (e) {
    throw new Error(`La escena no quedó lista en 90 s. ${errores.join(' | ')}`);
  }
  const fallo = await pagina.evaluate(() => window.__error || null);
  if (fallo) throw new Error(`La escena falló al prepararse: ${fallo}`);
  const escena = await pagina.evaluate(() => window.ESCENA);
  if (escena.ancho !== 1920 || escena.alto !== 1080) {
    await pagina.setViewportSize({ width: escena.ancho, height: escena.alto });
  }
  return { pagina, escena, errores };
}

export async function congelar(pagina, t) {
  await pagina.evaluate((t) => window.renderAt(t), t);
}

export async function fotografiar(pagina, escena, t, archivo) {
  await congelar(pagina, t);
  return pagina.screenshot({ path: archivo, clip: { x: 0, y: 0, width: escena.ancho, height: escena.alto } });
}

function ffmpeg(args) {
  execFileSync('ffmpeg', ['-hide_banner', '-loglevel', 'error', '-y', ...args], { stdio: 'inherit' });
}

// Compone imágenes en el navegador: una hoja de contactos (rejilla con el número de cada cuadro) o una piel de
// cebolla (los cuadros superpuestos con la misma opacidad).
async function componer(navegador, archivos, etiquetas, salida, modo) {
  const pagina = await navegador.newPage({ viewport: { width: 1920, height: 1080 } });
  const imgs = archivos.map((a) => pathToFileURL(a).href);
  let html;
  if (modo === 'hoja') {
    html = `<html><body style="margin:0;background:#16211E;font:20px 'IBM Plex Mono',monospace;color:#F5F7F6">
      <div style="display:grid;grid-template-columns:repeat(4,472px);gap:8px;padding:8px">${imgs.map((src, k) =>
        `<div><img src="${src}" style="width:472px;display:block"><div style="padding:4px 2px">${etiquetas[k]}</div></div>`).join('')}
      </div></body></html>`;
  } else {
    const o = (1 / imgs.length) * 1.6;
    html = `<html><body style="margin:0;background:#F5F7F6"><div style="position:relative;width:1920px;height:1080px">
      ${imgs.map((src) => `<img src="${src}" style="position:absolute;left:0;top:0;width:1920px;opacity:${Math.min(1, o).toFixed(3)};mix-blend-mode:multiply">`).join('')}
      <div style="position:absolute;right:24px;bottom:16px;font:22px monospace;color:#16211E">${etiquetas.join(' · ')}</div>
      </div></body></html>`;
  }
  const tmp = salida + '.html';
  writeFileSync(tmp, html);
  await pagina.goto(pathToFileURL(tmp).href, { waitUntil: 'load' });
  await pagina.screenshot({ path: salida, fullPage: true });
  await pagina.close();
  rmSync(tmp);
}

async function renderizarCuadros(navegador, html, lista, escena, carpeta, trabajadores) {
  mkdirSync(carpeta, { recursive: true });
  const trozos = Array.from({ length: trabajadores }, (_, k) => lista.filter((_, j) => j % trabajadores === k));
  const errores = [];
  await Promise.all(trozos.map(async (trozo) => {
    if (!trozo.length) return;
    const { pagina, errores: e } = await abrirEscena(navegador, html);
    for (const i of trozo) {
      await fotografiar(pagina, escena, i / escena.fps, path.join(carpeta, `${String(i).padStart(4, '0')}.png`));
    }
    errores.push(...e);
    await pagina.close();
  }));
  return errores;
}

async function principal() {
  const op = leerArgumentos(process.argv.slice(2));
  const html = rutaHtml(op.escena);
  const navegador = await abrirNavegador();
  const { pagina, escena, errores } = await abrirEscena(navegador, html);
  await pagina.close();
  const salida = path.resolve(op.valores.salida || path.join(RAIZ, 'salida', escena.id));
  mkdirSync(salida, { recursive: true });
  const N = escena.cuadros;
  const trabajadores = Math.max(1, Number(op.valores.trabajadores || Math.min(4, Math.max(1, cpus().length - 1))));
  const marcaDe = (i) => `cuadro ${String(i).padStart(4, '0')} · ${(i / escena.fps).toFixed(2)} s`;

  try {
    if (op.banderas.has('marcas') || op.valores.cuadros || op.valores.t) {
      // Fotogramas sueltos: las marcas, una lista de cuadros o una lista de segundos.
      const carpeta = path.join(salida, 'fotogramas');
      mkdirSync(carpeta, { recursive: true });
      const pedidos = [];
      if (op.banderas.has('marcas')) {
        for (const [nombre, t] of Object.entries(escena.marcas)) pedidos.push({ nombre, i: Math.round(t * escena.fps) });
      }
      for (const c of (op.valores.cuadros || '').split(',').filter(Boolean)) pedidos.push({ nombre: 'cuadro', i: Number(c) });
      for (const s of (op.valores.t || '').split(',').filter(Boolean)) pedidos.push({ nombre: 't', i: Math.round(Number(s) * escena.fps) });
      const { pagina: p } = await abrirEscena(navegador, html);
      for (const x of pedidos) {
        const i = Math.max(0, Math.min(N - 1, x.i));
        const archivo = path.join(carpeta, `${x.nombre}-${String(i).padStart(4, '0')}.png`);
        await fotografiar(p, escena, i / escena.fps, archivo);
        console.log(archivo);
      }
      await p.close();
    } else if (op.valores.cebolla) {
      const [rango, paso = '1'] = op.valores.cebolla.split(':');
      const [a, b] = rango.split('-').map(Number);
      const lista = [];
      for (let i = a; i <= Math.min(b, N - 1); i += Number(paso)) lista.push(i);
      const carpeta = path.join(salida, 'cebolla-cuadros');
      rmSync(carpeta, { recursive: true, force: true });
      await renderizarCuadros(navegador, html, lista, escena, carpeta, trabajadores);
      const archivo = path.join(salida, `cebolla-${a}-${b}.png`);
      await componer(navegador, lista.map((i) => path.join(carpeta, `${String(i).padStart(4, '0')}.png`)),
        [`cuadros ${a}-${b}, paso ${paso}`], archivo, 'cebolla');
      console.log(archivo);
    } else {
      // Todo, o solo la hoja de contactos (--hoja: 12 cuadros repartidos).
      const soloHoja = op.banderas.has('hoja');
      const hojaIdx = Array.from({ length: 12 }, (_, k) => Math.round(k * (N - 1) / 11));
      const lista = soloHoja ? hojaIdx : Array.from({ length: N }, (_, i) => i);
      const carpeta = path.join(salida, soloHoja ? 'hoja-cuadros' : 'cuadros');
      rmSync(carpeta, { recursive: true, force: true });
      const t0 = process.hrtime.bigint();
      errores.push(...await renderizarCuadros(navegador, html, lista, escena, carpeta, trabajadores));
      const seg = Number(process.hrtime.bigint() - t0) / 1e9;
      console.log(`${lista.length} cuadros en ${seg.toFixed(1)} s (${trabajadores} página(s)) → ${carpeta}`);
      const hoja = path.join(salida, 'hoja.png');
      await componer(navegador, hojaIdx.map((i) => path.join(carpeta, `${String(i).padStart(4, '0')}.png`)),
        hojaIdx.map(marcaDe), hoja, 'hoja');
      console.log(hoja);
      if (!soloHoja) {
        const patron = path.join(carpeta, '%04d.png');
        const mp4 = path.join(salida, `${escena.id}.mp4`);
        if (!op.banderas.has('sin-mp4')) {
          ffmpeg(['-framerate', String(escena.fps), '-i', patron, '-c:v', 'libx264', '-profile:v', 'high',
            '-pix_fmt', 'yuv420p', '-crf', '18', '-movflags', '+faststart', mp4]);
          console.log(mp4);
        }
        if (op.banderas.has('gif')) {
          // 20 cuadros/s: 50 ms por cuadro se guarda exacto en centésimas de segundo (a 15 o 24 cuadros/s, el GIF
          // redondea cada demora y la animación se adelanta o se atrasa).
          const fpsGif = Number(op.valores['fps-gif'] || 20);
          const gif = path.join(salida, `${escena.id}.gif`);
          ffmpeg(['-framerate', String(escena.fps), '-i', patron, '-vf',
            `fps=${fpsGif},scale=960:-1:flags=lanczos,split[a][b];[a]palettegen=stats_mode=diff[p];[b][p]paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle`,
            '-loop', '0', gif]);
          console.log(gif);
        }
      }
    }
  } finally {
    await navegador.close();
  }
  if (errores.length) {
    console.error('Errores de la página:\n  ' + [...new Set(errores)].join('\n  '));
    process.exit(1);
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  principal().catch((e) => { console.error(e.message || e); process.exit(1); });
}
