// todas.mjs: renderiza y audita todas las escenas, una tras otra, y arma la galería salida/index.html.
//
// Uso (desde animaciones/):
//   node herramientas/todas.mjs                 render completo + auditoría de cada escena + galería
//   node herramientas/todas.mjs --solo-galeria  solo la galería, con lo que ya haya en salida/
//   node herramientas/todas.mjs 02-nudos 03-rk4 solo esas escenas (y la galería con todas)
//
// La galería es una página local: cada escena con su video, su objetivo (del guion), su duración y el resultado de su
// auditoría. Ábrela con doble clic.
import { readdirSync, existsSync, readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const RAIZ = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const soloGaleria = args.includes('--solo-galeria');
const pedidas = args.filter((a) => !a.startsWith('--'));

const escenas = readdirSync(path.join(RAIZ, 'escenas'))
  .filter((d) => existsSync(path.join(RAIZ, 'escenas', d, 'index.html')))
  .sort();

const resultados = {};
if (!soloGaleria) {
  for (const e of escenas) {
    if (pedidas.length && !pedidas.includes(e)) continue;
    const dir = path.join('escenas', e);
    console.log(`\n== ${e}`);
    const r = { render: false, auditoria: false };
    try {
      execFileSync('node', ['herramientas/render.mjs', dir], { cwd: RAIZ, stdio: 'inherit' });
      r.render = true;
    } catch { console.error(`  el render de ${e} falló`); }
    try {
      execFileSync('node', ['herramientas/auditar.mjs', dir], { cwd: RAIZ, stdio: 'inherit' });
      r.auditoria = true;
    } catch { console.error(`  la auditoría de ${e} tiene fallas`); }
    resultados[e] = r;
  }
}

// ---- La galería ----

function objetivo(e) {
  const g = path.join(RAIZ, 'escenas', e, 'guion.md');
  if (!existsSync(g)) return '';
  const m = readFileSync(g, 'utf8').match(/OBJETIVO:\s*([\s\S]*?)\n\s*\n/);
  return m ? m[1].replace(/\s+/g, ' ').trim() : '';
}
function titulo(e) {
  const html = readFileSync(path.join(RAIZ, 'escenas', e, 'index.html'), 'utf8');
  const m = html.match(/<title>([^<]*)<\/title>/);
  return m ? m[1].replace(/\s*·\s*dismodlite\s*$/, '') : e;
}
function esc(s) { return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }

const tarjetas = escenas.map((e) => {
  const mp4 = path.join(RAIZ, 'salida', e, `${e}.mp4`);
  const hoja = path.join(RAIZ, 'salida', e, 'hoja.png');
  const aud = path.join(RAIZ, 'salida', e, 'auditoria.json');
  let estado = 'sin auditar';
  let duracion = '';
  if (existsSync(aud)) {
    const a = JSON.parse(readFileSync(aud, 'utf8'));
    const fallas = Object.values(a.fallas || {}).flat().length;
    const checks = (a.escenaChecks || []).length;
    estado = fallas ? `${fallas} falla(s) en la auditoría` : `auditoría en orden · ${checks} comprobaciones numéricas`;
    if (a.escena) duracion = `${a.escena.duracion} s · ${a.escena.cuadros} cuadros`;
  }
  const video = existsSync(mp4)
    ? `<video controls preload="metadata" src="${e}/${e}.mp4"${existsSync(hoja) ? ` poster="${e}/cuadros/0000.png"` : ''}></video>`
    : '<div class="falta">Sin render todavía: <code>node herramientas/render.mjs escenas/' + e + '</code></div>';
  return `<article>
    <h2>${esc(titulo(e))}</h2>
    ${video}
    <p class="obj">${esc(objetivo(e))}</p>
    <p class="meta">${esc(duracion)}${duracion ? ' · ' : ''}${esc(estado)} ·
      <a href="../escenas/${e}/index.html">escena</a> · <a href="../escenas/${e}/guion.md">guion</a>${existsSync(hoja) ? ` · <a href="${e}/hoja.png">hoja de contactos</a>` : ''}</p>
  </article>`;
});

const pagina = `<!doctype html>
<html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cómo estima dismodlite · animaciones</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600&family=Source+Serif+4:opsz,wght@8..60,600&display=swap">
<style>
  :root { --fondo: #F5F7F6; --tinta: #16211E; --suave: #55645F; --marca: #0E6B62; --borde: #DCE4E1; }
  body { margin: 0; background: var(--fondo); color: var(--tinta); font: 18px/1.5 "Source Sans 3", system-ui, sans-serif; }
  main { max-width: 1120px; margin: 0 auto; padding: 48px 16px 96px; }
  h1 { font: 600 40px/1.15 "Source Serif 4", Georgia, serif; margin: 0 0 8px; }
  .intro { color: var(--suave); max-width: 760px; margin: 0 0 40px; }
  article { margin: 0 0 56px; }
  h2 { font: 600 26px/1.2 "Source Serif 4", Georgia, serif; margin: 0 0 12px; }
  video, .falta { width: 100%; aspect-ratio: 16 / 9; border: 1px solid var(--borde); border-radius: 8px; background: #fff; display: block; }
  .falta { display: grid; place-items: center; color: var(--suave); }
  .obj { margin: 12px 0 4px; max-width: 900px; }
  .meta { margin: 0; color: var(--suave); font-size: 16px; }
  a { color: var(--marca); }
</style></head>
<body><main>
<h1>Cómo estima dismodlite</h1>
<p class="intro">Animaciones metodológicas, una por paso del modelo. Cada número sale del paquete o de la guía «El modelo»;
cada cuadro es una función pura del tiempo, y cada escena pasa una auditoría automática antes de renderizarse.</p>
${tarjetas.join('\n')}
</main></body></html>
`;
mkdirSync(path.join(RAIZ, 'salida'), { recursive: true });
const destino = path.join(RAIZ, 'salida', 'index.html');
writeFileSync(destino, pagina);
console.log(`\nGalería: ${destino}`);
const malas = Object.entries(resultados).filter(([, r]) => !r.render || !r.auditoria);
if (malas.length) {
  console.error(`Con problemas: ${malas.map(([e]) => e).join(', ')}`);
  process.exit(1);
}
