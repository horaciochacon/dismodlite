// Dibujo: la paleta, las tipografías, la retícula y las piezas con que se arma cada cuadro.
//
// Todas las funciones devuelven cadenas SVG (modo inmediato): dibujar(t, c) las junta con c.svg(...) y el contrato
// reemplaza el lienzo entero en cada cuadro, así que ningún estado pasa de un cuadro al siguiente.
//
// Color (validado con el validador de paletas de la guía de visualización, superficie #F5F7F6):
//   - Las tres cantidades del modelo llevan los tres matices de las figuras de la guía «El modelo»: incidencia i azul,
//     mortalidad en exceso f naranja y prevalencia p verde. Los tres pasan las comprobaciones de daltonismo entre
//     todos los pares; un cuarto matiz ya no, así que no hay cuarto matiz.
//   - Lo que viene de fuera del modelo (el ancla, los datos locales, la población) va en tinta, con forma propia:
//     cajas, bigotes, puntos huecos. Nunca con un color de serie.
//   - El texto va en tinta o tinta suave (contraste ≥ 4,5:1 sobre el fondo), nunca en el color de una serie: un trazo
//     o un punto de color al lado dice a qué serie pertenece.
//   - El verde azulado de la marca (#0E6B62) señala: la toma activa, el término que se explica, el nudo que se mueve.
//
// Texto legible: un texto con op.id lleva data-lee="id" y data-palabras="n". La auditoría mide esos elementos en
// cada cuadro: mientras su opacidad es ≥ 0,98 no pueden moverse, y deben quedar legibles al menos
// 0,3 s por palabra + 0,5 s. op.seguir = true exime del «no moverse» (una etiqueta de valor pegada a una marca que se
// mueve); op.id ausente = no se audita (marcas de eje).
(function (DL) {
  'use strict';

  var paleta = {
    fondo: '#F5F7F6',
    superficie: '#FFFFFF',
    tinta: '#16211E',
    suave: '#55645F',
    rejilla: '#DCE4E1',
    guia: '#B4C2BD',
    marca: '#0E6B62',
    marcaSuave: '#4FC2B3',
    ambar: '#E0A030',
    i: '#2a78d6',
    f: '#eb6834',
    p: '#1baf7a',
  };

  var fuentes = {
    titulo: '"Source Serif 4", Georgia, serif',
    texto: '"Source Sans 3", "Helvetica Neue", Arial, sans-serif',
    mono: '"IBM Plex Mono", ui-monospace, Menlo, monospace',
  };

  // Retícula de 1920 x 1080: márgenes de 96 px, cabecera arriba, rótulo de narración abajo.
  var marco = {
    ancho: 1920, alto: 1080, izq: 96, der: 1824,
    cabecera: { y: 72, alto: 132 },          // 72 a 204
    escena: { y: 236, alto: 660 },           // 236 a 896
    rotulo: { y: 924, alto: 84 },            // 924 a 1008
    pie: { y: 1046 },
  };

  // Tamaños de letra (px a 1080p): nada legible por debajo de 22.
  var tam = { titulo: 54, subtitulo: 30, rotulo: 36, cuerpo: 30, etiqueta: 26, eje: 22, nota: 22, kicker: 20 };

  // ---- Utilidades ----

  function esc(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }
  function r2(x) { return Math.round(x * 100) / 100; }
  function opa(o) { return (o === undefined || o >= 1) ? '' : ' opacity="' + r2(Math.max(0, o)) + '"'; }
  function lee(op, texto) {
    if (!op || !op.id) return '';
    var n = op.palabras !== undefined ? op.palabras : DL.mov.palabras(texto);
    return ' data-lee="' + esc(op.id) + '" data-palabras="' + n + '"' + (op.seguir ? ' data-seguir="1"' : '');
  }
  function nada(o) { return o !== undefined && o <= 0.001; }

  // Formato de números con punto decimal, como en la guía del paquete.
  function num(x, digitos) {
    if (!isFinite(x)) return String(x);
    return Number(x).toFixed(digitos === undefined ? 2 : digitos);
  }
  // Cifras significativas, sin notación científica para valores corrientes.
  function sig(x, cifras) {
    if (x === 0 || !isFinite(x)) return String(x);
    var c = cifras || 3;
    var d = Math.max(0, c - 1 - Math.floor(Math.log10(Math.abs(x))));
    return Number(x).toFixed(Math.min(d, 12));
  }
  function miles(n) { return String(Math.round(n)).replace(/\B(?=(\d{3})+(?!\d))/g, ' '); }

  // ---- Formas ----

  function grupo(contenido, op) {
    op = op || {};
    if (nada(op.o)) return '';
    var tr = [];
    if (op.tx || op.ty) tr.push('translate(' + r2(op.tx || 0) + ' ' + r2(op.ty || 0) + ')');
    if (op.escala !== undefined && op.escala !== 1) {
      var ox = op.origen ? op.origen[0] : 0, oy = op.origen ? op.origen[1] : 0;
      tr.push('translate(' + r2(ox) + ' ' + r2(oy) + ') scale(' + op.escala + ') translate(' + r2(-ox) + ' ' + r2(-oy) + ')');
    }
    if (op.rot) tr.push('rotate(' + op.rot + (op.origen ? ' ' + op.origen[0] + ' ' + op.origen[1] : '') + ')');
    var clip = op.recorte ? ' clip-path="url(#' + op.recorte + ')"' : '';
    return '<g' + (tr.length ? ' transform="' + tr.join(' ') + '"' : '') + opa(op.o) + clip + '>' +
      (Array.isArray(contenido) ? contenido.join('') : contenido) + '</g>';
  }

  function rect(x, y, w, h, op) {
    op = op || {};
    if (nada(op.o)) return '';
    return '<rect x="' + r2(x) + '" y="' + r2(y) + '" width="' + r2(Math.max(0, w)) + '" height="' + r2(Math.max(0, h)) +
      '" rx="' + (op.radio || 0) + '" fill="' + (op.relleno || 'none') + '"' +
      (op.borde ? ' stroke="' + op.borde + '" stroke-width="' + (op.grosor || 2) + '"' : '') +
      (op.guiones ? ' stroke-dasharray="' + op.guiones + '"' : '') + opa(op.o) + '/>';
  }

  function linea(x1, y1, x2, y2, op) {
    op = op || {};
    if (nada(op.o)) return '';
    return '<line x1="' + r2(x1) + '" y1="' + r2(y1) + '" x2="' + r2(x2) + '" y2="' + r2(y2) + '" stroke="' +
      (op.color || paleta.tinta) + '" stroke-width="' + (op.grosor || 2) + '" stroke-linecap="' + (op.extremo || 'round') + '"' +
      (op.guiones ? ' stroke-dasharray="' + op.guiones + '"' : '') + opa(op.o) + '/>';
  }

  function circulo(x, y, r, op) {
    op = op || {};
    if (nada(op.o) || r <= 0) return '';
    return '<circle cx="' + r2(x) + '" cy="' + r2(y) + '" r="' + r2(r) + '" fill="' + (op.relleno || 'none') + '"' +
      (op.borde ? ' stroke="' + op.borde + '" stroke-width="' + (op.grosor || 2) + '"' : '') + opa(op.o) + '/>';
  }

  // Corta una polilínea [[x, y], ...] al avance u en [0, 1] de su longitud.
  function cortar(puntos, u) {
    if (u >= 1 || puntos.length < 2) return puntos;
    if (u <= 0) return [puntos[0]];
    var largos = [0];
    for (var k = 1; k < puntos.length; k++) {
      largos.push(largos[k - 1] + Math.hypot(puntos[k][0] - puntos[k - 1][0], puntos[k][1] - puntos[k - 1][1]));
    }
    var meta = u * largos[largos.length - 1];
    var salida = [puntos[0]];
    for (var j = 1; j < puntos.length; j++) {
      if (largos[j] <= meta) { salida.push(puntos[j]); continue; }
      var w = (meta - largos[j - 1]) / (largos[j] - largos[j - 1] || 1);
      salida.push([puntos[j - 1][0] + w * (puntos[j][0] - puntos[j - 1][0]), puntos[j - 1][1] + w * (puntos[j][1] - puntos[j - 1][1])]);
      break;
    }
    return salida;
  }

  function camino(puntos) {
    var s = '';
    for (var k = 0; k < puntos.length; k++) s += (k ? 'L' : 'M') + r2(puntos[k][0]) + ' ' + r2(puntos[k][1]);
    return s;
  }

  // trazo(puntos, op): polilínea. op.progreso en [0, 1] la dibuja hasta esa fracción de su longitud.
  function trazo(puntos, op) {
    op = op || {};
    if (nada(op.o) || !puntos.length) return '';
    var p = op.progreso === undefined ? puntos : cortar(puntos, op.progreso);
    if (p.length < 2) return '';
    return '<path d="' + camino(p) + (op.cerrar ? 'Z' : '') + '" fill="' + (op.relleno || 'none') + '" stroke="' +
      (op.color || paleta.tinta) + '" stroke-width="' + (op.grosor === undefined ? 2 : op.grosor) +
      '" stroke-linejoin="round" stroke-linecap="round"' + (op.guiones ? ' stroke-dasharray="' + op.guiones + '"' : '') +
      opa(op.o) + '/>';
  }

  // area(arriba, abajo, op): región entre dos polilíneas con las mismas x (arriba de izquierda a derecha).
  function area(arriba, abajo, op) {
    op = op || {};
    if (nada(op.o) || arriba.length < 2) return '';
    var pts = arriba.concat(abajo.slice().reverse());
    return '<path d="' + camino(pts) + 'Z" fill="' + (op.relleno || paleta.guia) + '"' + opa(op.o) + '/>';
  }

  // flecha(x1, y1, x2, y2, op): flecha recta o curva (op.curva: desplazamiento perpendicular del punto de control, px).
  // op.progreso dibuja el tramo inicial; la cabeza aparece al final del tramo dibujado.
  function flecha(x1, y1, x2, y2, op) {
    op = op || {};
    if (nada(op.o)) return '';
    var color = op.color || paleta.tinta, grosor = op.grosor || 3, cabeza = op.cabeza || 14;
    var curva = op.curva || 0;
    var mx = (x1 + x2) / 2, my = (y1 + y2) / 2, dx = x2 - x1, dy = y2 - y1, L = Math.hypot(dx, dy) || 1;
    var cx = mx - curva * dy / L, cy = my + curva * dx / L;
    var pts = [];
    for (var k = 0; k <= 32; k++) {
      var s = k / 32, m = 1 - s;
      pts.push([m * m * x1 + 2 * m * s * cx + s * s * x2, m * m * y1 + 2 * m * s * cy + s * s * y2]);
    }
    var parte = op.progreso === undefined ? pts : cortar(pts, op.progreso);
    if (parte.length < 2) return '';
    var fin = parte[parte.length - 1], prev = parte[parte.length - 2];
    var ang = Math.atan2(fin[1] - prev[1], fin[0] - prev[0]);
    // El cuerpo termina donde empieza la cabeza, para que la punta quede limpia.
    var cuerpo = parte.slice(0, -1).concat([[fin[0] - Math.cos(ang) * cabeza * 0.6, fin[1] - Math.sin(ang) * cabeza * 0.6]]);
    var a1 = ang + Math.PI * 0.82, a2 = ang - Math.PI * 0.82;
    var punta = 'M' + r2(fin[0]) + ' ' + r2(fin[1]) + 'L' + r2(fin[0] + Math.cos(a1) * cabeza) + ' ' + r2(fin[1] + Math.sin(a1) * cabeza) +
      'L' + r2(fin[0] + Math.cos(a2) * cabeza) + ' ' + r2(fin[1] + Math.sin(a2) * cabeza) + 'Z';
    return '<g' + opa(op.o) + '>' + trazo(cuerpo, { color: color, grosor: grosor, guiones: op.guiones }) +
      '<path d="' + punta + '" fill="' + color + '"/></g>';
  }

  // ---- Texto ----

  // texto(x, y, cadena, op): una línea. op: tam, peso, color, fuente ('texto' | 'titulo' | 'mono'), ancla ('start' |
  // 'middle' | 'end'), base ('alphabetic' | 'middle' | 'hanging'), espaciado (em), italica, o, id, palabras, seguir.
  function texto(x, y, cadena, op) {
    op = op || {};
    if (nada(op.o)) return '';
    return '<text x="' + r2(x) + '" y="' + r2(y) + '" font-family=\'' + (fuentes[op.fuente || 'texto']) + '\' font-size="' +
      (op.tam || tam.cuerpo) + '" font-weight="' + (op.peso || 400) + '" fill="' + (op.color || paleta.tinta) + '"' +
      ' text-anchor="' + (op.ancla || 'start') + '" dominant-baseline="' + (op.base || 'alphabetic') + '"' +
      (op.espaciado ? ' letter-spacing="' + op.espaciado + 'em"' : '') + (op.italica ? ' font-style="italic"' : '') +
      ' style="font-variant-numeric: tabular-nums"' + opa(op.o) + lee(op, cadena) + '>' + esc(cadena) + '</text>';
  }

  // KaTeX: cada fórmula se compila una vez y se guarda.
  var cacheKatex = {};
  function katexHtml(tex, display) {
    var clave = (display ? 'D:' : 'I:') + tex;
    if (cacheKatex[clave] === undefined) {
      if (!globalThis.katex) throw new Error('KaTeX no está cargado: agrega su <script> fijado a la versión 0.16.11.');
      cacheKatex[clave] = globalThis.katex.renderToString(tex, {
        displayMode: !!display, throwOnError: true, trust: true, strict: 'ignore', output: 'html',
      });
    }
    return cacheKatex[clave];
  }

  // Texto con fórmulas en línea entre signos $ ... $, negrita **...**, cursiva *...* y saltos de línea \n.
  function enriquecer(cadena) {
    var partes = String(cadena).split('$');
    var html = '';
    for (var k = 0; k < partes.length; k++) {
      if (k % 2 === 1) { html += katexHtml(partes[k], false); continue; }
      html += esc(partes[k]).replace(/\*\*([^*]+)\*\*/g, '<b>$1</b>').replace(/\*([^*]+)\*/g, '<em>$1</em>')
        .replace(/\n/g, '<br>');
    }
    return html;
  }

  function cajaHtml(x, y, ancho, alto, interior, op, valign) {
    op = op || {};
    var ancla = op.ancla || 'start';
    var x0 = ancla === 'middle' ? x - ancho / 2 : (ancla === 'end' ? x - ancho : x);
    var y0 = valign === 'middle' ? y - alto / 2 : y;
    var just = ancla === 'middle' ? 'center' : (ancla === 'end' ? 'flex-end' : 'flex-start');
    return '<foreignObject x="' + r2(x0) + '" y="' + r2(y0) + '" width="' + r2(ancho) + '" height="' + r2(alto) + '"' + opa(op.o) + '>' +
      '<div xmlns="http://www.w3.org/1999/xhtml" style="width:' + r2(ancho) + 'px;height:' + r2(alto) + 'px;display:flex;' +
      'align-items:' + (valign === 'middle' ? 'center' : 'flex-start') + ';justify-content:' + just + ';">' + interior +
      '</div></foreignObject>';
  }

  // parrafo(x, y, ancho, cadena, op): texto que se ajusta al ancho, desde la esquina superior izquierda (x, y).
  // Admite $fórmulas$, **negrita**, *cursiva* y saltos de línea (\n). op: tam, color, fuente, peso, interlineado, alto, ancla, o, id.
  function parrafo(x, y, ancho, cadena, op) {
    op = op || {};
    if (nada(op.o)) return '';
    var alto = op.alto || 400;
    var t = op.tam || tam.cuerpo;
    var interior = '<div' + lee(op, cadena) + ' style="font-family:' + fuentes[op.fuente || 'texto'].replace(/"/g, '\'') +
      ';font-size:' + t + 'px;line-height:' + (op.interlineado || 1.32) + ';color:' + (op.color || paleta.tinta) +
      ';font-weight:' + (op.peso || 400) + ';text-align:' + ({ start: 'left', middle: 'center', end: 'right' })[op.ancla || 'start'] +
      ';font-variant-numeric:tabular-nums;max-width:' + r2(ancho) + 'px">' + enriquecer(cadena) + '</div>';
    return cajaHtml(x, y, ancho, alto, interior, op, 'top');
  }

  // formula(tex, x, y, op): fórmula de KaTeX centrada verticalmente en y. op: tam (px), color, ancla, o, id, palabras,
  // display (modo bloque), ancho y alto de la caja, estilos: { clase: 'css' } para los términos marcados en el tex con
  // \htmlClass{clase}{...} (por ejemplo, atenuar o colorear un término sin recompilar la fórmula).
  function formula(tex, x, y, op) {
    op = op || {};
    if (nada(op.o)) return '';
    var ancho = op.ancho || 1500, alto = op.alto || 220;
    var estilo = '';
    if (op.estilos) {
      var reglas = [];
      for (var clase in op.estilos) reglas.push('.' + clase + '{' + op.estilos[clase] + '}');
      estilo = '<style>' + reglas.join('') + '</style>';
    }
    var interior = estilo + '<span' + lee(op, op.palabras !== undefined ? '' : tex) +
      (op.id && op.palabras === undefined ? '' : '') + ' style="font-size:' + (op.tam || 44) + 'px;color:' +
      (op.color || paleta.tinta) + ';white-space:nowrap">' + katexHtml(tex, op.display) + '</span>';
    return cajaHtml(x, y, ancho, alto, interior, op, 'middle');
  }

  // ---- Piezas de la retícula ----

  // cabecera(numero, total, titulo, op): la franja superior, quieta desde el cuadro 0. op.funcion: la función del
  // paquete que hace lo que muestra la escena (por ejemplo 'dl_edo()').
  function cabecera(numero, total, titulo, op) {
    op = op || {};
    var y = marco.cabecera.y;
    var n = String(numero).padStart(2, '0') + ' / ' + String(total).padStart(2, '0');
    return [
      texto(marco.izq, y + 22, 'DISMODLITE · CÓMO ESTIMA', { fuente: 'mono', tam: tam.kicker, color: paleta.marca, espaciado: 0.12, peso: 500 }),
      texto(marco.der, y + 22, n, { fuente: 'mono', tam: tam.kicker, color: paleta.suave, ancla: 'end', espaciado: 0.08 }),
      texto(marco.izq, y + 86, titulo, { fuente: 'titulo', tam: tam.titulo, peso: 600, id: 'titulo' }),
      op.funcion ? texto(marco.der, y + 86, op.funcion, { fuente: 'mono', tam: 26, color: paleta.suave, ancla: 'end', id: 'funcion' }) : '',
      linea(marco.izq, y + 118, marco.der, y + 118, { color: paleta.rejilla, grosor: 2 }),
    ].join('');
  }

  // rotulo(t, lineas): la narración. lineas = [[t0, t1, 'texto'], ...]: cada línea aparece en t0 y se va en t1, quieta
  // en la franja inferior. La auditoría comprueba que cada una dure lo que pide su número de palabras.
  function rotulo(t, lineas) {
    var s = '';
    for (var k = 0; k < lineas.length; k++) {
      var l = lineas[k];
      var o = DL.mov.visible(t, l[0], l[1], { entrada: 0.3, salida: 0.24 });
      if (o <= 0) continue;
      s += parrafo(marco.izq + 28, marco.rotulo.y + 4, 1560, l[2], { tam: tam.rotulo, o: o, id: 'rotulo-' + k, alto: 90 });
      s += rect(marco.izq, marco.rotulo.y + 8, 6, 44, { relleno: paleta.marca, radio: 3, o: o });
    }
    return s;
  }

  // pie(cadena): la nota fija de abajo a la derecha (la fuente de los números).
  function pie(cadena) {
    return texto(marco.der, marco.pie.y, cadena, { tam: tam.nota, color: paleta.suave, ancla: 'end', id: 'pie' });
  }

  // ---- Ejes ----

  // ejes(op): un sistema de coordenadas. op: x, y, ancho, alto (px, esquina superior izquierda del área de datos),
  // xDom, yDom ([min, max]), yLog (escala logarítmica en y), xTicks, yTicks, xFormato, yFormato, xTitulo, yTitulo,
  // id (para el recorte). Devuelve { x(v), y(v), svg(op), curva(xs, ys, op), banda(xs, lo, hi, op), recorte }.
  function ejes(op) {
    var X0 = op.x, Y0 = op.y, W = op.ancho, H = op.alto;
    var xd = op.xDom, yd = op.yDom;
    var ylog = !!op.yLog, xlog = !!op.xLog;
    var idRecorte = 'recorte-' + (op.id || 'ejes');
    function tx(v) {
      var u = xlog ? (Math.log(v) - Math.log(xd[0])) / (Math.log(xd[1]) - Math.log(xd[0])) : (v - xd[0]) / (xd[1] - xd[0]);
      return X0 + u * W;
    }
    function ty(v) {
      var u = ylog ? (Math.log(v) - Math.log(yd[0])) / (Math.log(yd[1]) - Math.log(yd[0])) : (v - yd[0]) / (yd[1] - yd[0]);
      return Y0 + H - u * H;
    }
    function svg(o2) {
      o2 = o2 || {};
      if (nada(o2.o)) return '';
      var s = [];
      var xf = op.xFormato || function (v) { return String(v); };
      var yf = op.yFormato || function (v) { return String(v); };
      (op.yTicks || []).forEach(function (v) {
        var yy = ty(v);
        s.push(linea(X0, yy, X0 + W, yy, { color: paleta.rejilla, grosor: 1.5 }));
        s.push(texto(X0 - 14, yy, yf(v), { tam: tam.eje, color: paleta.suave, ancla: 'end', base: 'middle' }));
      });
      s.push(linea(X0, Y0 + H, X0 + W, Y0 + H, { color: paleta.guia, grosor: 2 }));
      (op.xTicks || []).forEach(function (v) {
        var xx = tx(v);
        s.push(linea(xx, Y0 + H, xx, Y0 + H + 8, { color: paleta.guia, grosor: 2 }));
        s.push(texto(xx, Y0 + H + 36, xf(v), { tam: tam.eje, color: paleta.suave, ancla: 'middle' }));
      });
      // Los títulos admiten $fórmulas$ en línea («Prevalencia $p$»).
      if (op.xTitulo) {
        s.push(op.xTitulo.indexOf('$') >= 0
          ? parrafo(X0 + W, Y0 + H + 50, 700, op.xTitulo, { tam: tam.eje + 2, color: paleta.suave, ancla: 'end', alto: 40, id: op.id ? op.id + '-xtitulo' : undefined })
          : texto(X0 + W, Y0 + H + 74, op.xTitulo, { tam: tam.eje + 2, color: paleta.suave, ancla: 'end', id: op.id ? op.id + '-xtitulo' : undefined }));
      }
      if (op.yTitulo) {
        s.push(op.yTitulo.indexOf('$') >= 0
          ? parrafo(X0 - 14, Y0 - 50, 700, op.yTitulo, { tam: tam.eje + 2, color: paleta.suave, alto: 40, id: op.id ? op.id + '-ytitulo' : undefined })
          : texto(X0 - 14, Y0 - 26, op.yTitulo, { tam: tam.eje + 2, color: paleta.suave, ancla: 'start', id: op.id ? op.id + '-ytitulo' : undefined }));
      }
      return grupo(s, { o: o2.o });
    }
    function recorte() {
      return '<clipPath id="' + idRecorte + '"><rect x="' + (X0 - 4) + '" y="' + (Y0 - 4) + '" width="' + (W + 8) + '" height="' + (H + 8) + '"/></clipPath>';
    }
    function puntos(xs, ys) {
      var p = [];
      for (var k = 0; k < xs.length; k++) if (isFinite(ys[k]) && (!ylog || ys[k] > 0)) p.push([tx(xs[k]), ty(ys[k])]);
      return p;
    }
    return {
      x: tx, y: ty, svg: svg, recorte: recorte, idRecorte: idRecorte, puntos: puntos,
      izq: X0, der: X0 + W, arriba: Y0, abajo: Y0 + H, ancho: W, alto: H,
      // curva(xs, ys, op): op como en trazo (color, grosor, progreso, o, guiones); op.recortar (por defecto true).
      curva: function (xs, ys, o2) {
        o2 = o2 || {};
        var s = trazo(puntos(xs, ys), o2);
        return o2.recortar === false ? s : '<g clip-path="url(#' + idRecorte + ')">' + s + '</g>';
      },
      banda: function (xs, lo, hi, o2) {
        o2 = o2 || {};
        return '<g clip-path="url(#' + idRecorte + ')">' + area(puntos(xs, hi), puntos(xs, lo), o2) + '</g>';
      },
    };
  }

  DL.dib = {
    paleta: paleta, fuentes: fuentes, marco: marco, tam: tam,
    esc: esc, num: num, sig: sig, miles: miles,
    grupo: grupo, rect: rect, linea: linea, circulo: circulo, trazo: trazo, area: area, flecha: flecha, cortar: cortar,
    texto: texto, parrafo: parrafo, formula: formula, katexHtml: katexHtml,
    cabecera: cabecera, rotulo: rotulo, pie: pie, ejes: ejes,
  };
})(globalThis.DL = globalThis.DL || {});
