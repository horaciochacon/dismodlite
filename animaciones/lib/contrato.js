// El contrato de render de las animaciones de dismodlite.
//
// Cada escena es un archivo HTML autocontenido. Esta biblioteca hace cumplir el contrato:
//
//   1. Cada cuadro es una función pura del tiempo t, en segundos: window.renderAt(t) dibuja exactamente ese cuadro,
//      sin importar qué cuadros se dibujaron antes ni cuánto tarda la máquina.
//   2. Nada de relojes en las escenas: ni Date.now, ni new Date(), ni setTimeout o setInterval, ni Math.random sin
//      semilla. El único reloj vive aquí, en la vista previa (requestAnimationFrame), y solo elige qué t se dibuja.
//   3. El azar es con semilla: DL.azar(semilla) para listas fijas que se arman una vez al cargar, y DL.hash01(...)
//      para un valor que depende solo de enteros (cuadro, índice), sin estado.
//   4. Con ?t=2.5 (o ?cuadro=75) la página queda congelada en ese cuadro; con ?render=1 no arranca la vista previa y
//      espera a que la herramienta pida cada cuadro (herramientas/render.mjs). window.__ready vale true cuando las
//      fuentes, KaTeX y los datos están listos.
//   5. Se renderizan los cuadros 0 a N - 1, con N = duración x fps. El cuadro 0 es una composición terminada.
//
// Además, cada cuadro deja dos registros para la auditoría (herramientas/auditar.mjs):
//   window.__textos  los textos legibles del cuadro (id, posición, opacidad, número de palabras), para comprobar que
//                    ningún texto se mueve mientras se lee y que cada uno queda en pantalla al menos
//                    0,3 s por palabra + 0,5 s;
//   window.__estado  los números que la escena declara (c.dato(nombre, valor)), para comparar lo dibujado con lo que
//                    calcula el paquete.
//
// Uso, en el HTML de una escena (después de cargar contrato.js, movimiento.js, dibujo.js y modelo.js):
//
//   DL.escena({
//     id: '01-enfermedad-muerte', titulo: 'El modelo enfermedad-muerte',
//     duracion: 30, fps: 30, ancho: 1920, alto: 1080,
//     marcas: { m_se_cancela: 14.2 },        // momentos con nombre: fotogramas de revisión y auditoría
//     camaras: [[39.4, 40.2]],               // movimientos de cámara: los únicos tramos en que un texto legible
//                                            // puede desplazarse (todo el bloque se mueve junto, entre dos frases)
//     dibujar: function (t, c) { c.svg(DL.dib.texto(...)); c.dato('p_95', 0.12); },
//     auditoria: function (A) { return [ A.en('m_se_cancela', 'm está tachada', function (e) { ... }) ]; },
//   });
(function (DL) {
  'use strict';

  // ---- Azar con semilla ----

  // mulberry32: generador de 32 bits, rápido y suficiente para decorar (posiciones, desfases). Nunca para inferencia:
  // los números del modelo salen del paquete (datos/) o de modelo.js.
  function mulberry32(semilla) {
    var a = semilla >>> 0;
    return function () {
      a = (a + 0x6D2B79F5) >>> 0;
      var x = Math.imul(a ^ (a >>> 15), 1 | a);
      x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x;
      return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
    };
  }

  // DL.azar(semilla): { u() en [0, 1), normal() N(0, 1), entre(a, b), entero(n) en 0..n-1 }. Úsalo solo al cargar la
  // escena (fuera de dibujar) para armar listas fijas; dentro de dibujar, usa DL.hash01.
  DL.azar = function (semilla) {
    var u = mulberry32(semilla === undefined ? 1 : semilla);
    var guardada = null;
    return {
      u: u,
      entre: function (a, b) { return a + (b - a) * u(); },
      entero: function (n) { return Math.floor(u() * n); },
      normal: function () {
        if (guardada !== null) { var g = guardada; guardada = null; return g; }
        var u1 = Math.max(u(), 1e-12), u2 = u();
        var radio = Math.sqrt(-2 * Math.log(u1));
        guardada = radio * Math.sin(2 * Math.PI * u2);
        return radio * Math.cos(2 * Math.PI * u2);
      },
    };
  };

  // DL.hash01(a, b, ...): un número en [0, 1) que depende solo de los enteros que recibe (sin estado). Para variar algo
  // por cuadro o por índice dentro de dibujar sin romper la pureza.
  DL.hash01 = function () {
    var h = 0x811C9DC5;
    for (var k = 0; k < arguments.length; k++) {
      h = Math.imul(h ^ (arguments[k] | 0), 0x01000193);
      h ^= h >>> 13;
      h = Math.imul(h, 0x5BD1E995);
      h ^= h >>> 15;
    }
    return (h >>> 0) / 4294967296;
  };

  // ---- El cuadro: lo que dibujar(t, c) va llenando ----

  function nuevoCuadro(t, cfg) {
    var partes = [];
    var defs = [];
    var textos = [];
    var estado = {};
    var c = {
      t: t,
      cuadro: Math.round(t * cfg.fps),
      ancho: cfg.ancho,
      alto: cfg.alto,
      // c.svg(cadena, cadena, ...): agrega elementos SVG en orden de pintado (lo último queda encima).
      svg: function () {
        for (var k = 0; k < arguments.length; k++) {
          var x = arguments[k];
          if (x === null || x === undefined || x === false || x === '') continue;
          if (Array.isArray(x)) { for (var j = 0; j < x.length; j++) c.svg(x[j]); } else { partes.push(String(x)); }
        }
        return c;
      },
      // c.defs(cadena): gradientes, marcadores o recortes (<defs>).
      defs: function (x) { defs.push(String(x)); return c; },
      // c.dato(nombre, valor): un número que la auditoría puede leer en window.__estado.
      dato: function (nombre, valor) { estado[nombre] = valor; return c; },
      // Lo usa dibujo.js: registra un texto legible.
      _texto: function (registro) { textos.push(registro); },
      _resultado: function () {
        return { svg: (defs.length ? '<defs>' + defs.join('') + '</defs>' : '') + partes.join(''), textos: textos,
                 estado: estado };
      },
    };
    return c;
  }

  // ---- Fuentes ----

  // Las familias que usan las escenas. Se piden todas antes de declarar la escena lista, para que el cuadro 0 nunca
  // salga con una fuente de respaldo.
  var FUENTES = [
    '400 32px "Source Sans 3"', '600 32px "Source Sans 3"', 'italic 400 32px "Source Sans 3"',
    '600 32px "Source Serif 4"', '400 32px "Source Serif 4"',
    '400 32px "IBM Plex Mono"', '500 32px "IBM Plex Mono"',
    '400 32px KaTeX_Main', 'italic 400 32px KaTeX_Math', 'bold 32px KaTeX_Main', '400 32px KaTeX_AMS',
    '400 32px KaTeX_Size1', '400 32px KaTeX_Size2', '400 32px KaTeX_Size3', '400 32px KaTeX_Size4',
  ];

  function cargarFuentes() {
    if (!document.fonts || !document.fonts.load) return Promise.resolve();
    return Promise.all(FUENTES.map(function (f) {
      return document.fonts.load(f, 'aáéíóúñpqfiλρσθκηΣ0123456789').catch(function () { return null; });
    })).then(function () { return document.fonts.ready; });
  }

  // ---- La escena ----

  DL.escena = function (cfg) {
    if (DL.__escena) throw new Error('Una página tiene una sola escena.');
    var fps = cfg.fps || 30;
    var c0 = {
      id: cfg.id, titulo: cfg.titulo || cfg.id, duracion: cfg.duracion, fps: fps,
      ancho: cfg.ancho || 1920, alto: cfg.alto || 1080, marcas: cfg.marcas || {}, camaras: cfg.camaras || [],
    };
    c0.cuadros = Math.round(c0.duracion * fps);
    if (!(c0.cuadros > 0)) throw new Error('La escena necesita una duración positiva.');
    for (var nombre in c0.marcas) {
      if (!(c0.marcas[nombre] >= 0 && c0.marcas[nombre] < c0.duracion)) {
        throw new Error('La marca «' + nombre + '» (' + c0.marcas[nombre] + ' s) cae fuera de la escena.');
      }
    }
    DL.__escena = c0;

    var q = new URLSearchParams(location.search);
    var modoRender = q.has('render');
    var tFijo = q.has('t') ? (parseFloat(q.get('t')) || 0) : (q.has('cuadro') ? (parseInt(q.get('cuadro'), 10) || 0) / fps : null);
    var depurar = q.has('depurar') || (!modoRender && tFijo === null);

    // El escenario: un SVG de ancho x alto. En la vista previa se escala para caber en la ventana; al renderizar,
    // la ventana mide exactamente ancho x alto y la escala es 1.
    document.documentElement.style.background = DL.dib ? DL.dib.paleta.fondo : '#F5F7F6';
    var escenario = document.createElement('div');
    escenario.id = 'escenario';
    escenario.style.width = c0.ancho + 'px';
    escenario.style.height = c0.alto + 'px';
    var lienzo = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    lienzo.id = 'lienzo';
    lienzo.setAttribute('width', c0.ancho);
    lienzo.setAttribute('height', c0.alto);
    lienzo.setAttribute('viewBox', '0 0 ' + c0.ancho + ' ' + c0.alto);
    escenario.appendChild(lienzo);
    var contador = document.createElement('div');
    contador.id = 'contador';
    escenario.appendChild(contador);
    document.body.appendChild(escenario);

    function ajustarEscala() {
      if (modoRender) { escenario.style.transform = ''; return; }
      var s = Math.min(window.innerWidth / c0.ancho, window.innerHeight / c0.alto, 1);
      escenario.style.transform = 'scale(' + s + ')';
    }

    var ultimoT = null;
    function renderAt(t) {
      t = Number(t);
      if (!isFinite(t)) t = 0;
      t = Math.max(0, Math.min(t, c0.duracion - 1 / (2 * fps)));
      var c = nuevoCuadro(t, c0);
      DL.__c = c;
      cfg.dibujar(t, c);
      DL.__c = null;
      var r = c._resultado();
      var fondo = '<rect x="0" y="0" width="' + c0.ancho + '" height="' + c0.alto + '" fill="' +
        (DL.dib ? DL.dib.paleta.fondo : '#F5F7F6') + '"/>';
      lienzo.innerHTML = fondo + r.svg;
      window.__textos = r.textos;
      window.__estado = r.estado;
      window.__t = t;
      ultimoT = t;
      var n = Math.round(t * fps);
      contador.textContent = 'cuadro ' + String(n).padStart(4, '0') + ' / ' + (c0.cuadros - 1) + ' · ' + t.toFixed(2) + ' s';
      contador.style.display = depurar ? 'block' : 'none';
    }
    window.renderAt = renderAt;
    window.ESCENA = c0;

    // ---- Auditoría declarada por la escena ----
    // A.en(marcaOSegundos, nombre, comprobar): comprobar(estado, textos) devuelve true, o una cadena con el problema.
    var comprobaciones = [];
    if (typeof cfg.auditoria === 'function') {
      var A = {
        en: function (cuando, nombre, comprobar) {
          var t = typeof cuando === 'string' ? c0.marcas[cuando] : cuando;
          if (t === undefined) throw new Error('Auditoría: no hay una marca «' + cuando + '».');
          return { t: t, nombre: nombre, comprobar: comprobar };
        },
        cerca: function (a, b, tol) {
          return Math.abs(a - b) <= (tol === undefined ? 1e-9 : tol) * Math.max(1, Math.abs(b));
        },
      };
      comprobaciones = cfg.auditoria(A) || [];
    }
    window.__auditoria = {
      lista: comprobaciones.map(function (x) { return { t: x.t, nombre: x.nombre }; }),
      correr: function (k) {
        var x = comprobaciones[k];
        renderAt(x.t);
        var r;
        try { r = x.comprobar(window.__estado, window.__textos); } catch (e) { r = 'error: ' + e.message; }
        return { nombre: x.nombre, t: x.t, ok: r === true, detalle: r === true ? '' : String(r) };
      },
    };

    // ---- Preparar, cargar fuentes y declararse lista ----
    window.__ready = false;
    Promise.resolve(typeof cfg.preparar === 'function' ? cfg.preparar() : null)
      .then(function () {
        // Dibujar el principio, el final y cada marca hace que el navegador pida las fuentes que se usan.
        renderAt(0);
        for (var m in c0.marcas) renderAt(c0.marcas[m]);
        renderAt(c0.duracion);
        return cargarFuentes();
      })
      .then(function () {
        ajustarEscala();
        if (tFijo !== null) {
          renderAt(tFijo);
        } else if (!modoRender) {
          iniciarVistaPrevia();
        } else {
          renderAt(0);
        }
        window.__ready = true;
      })
      .catch(function (e) {
        window.__error = e && e.message ? e.message : String(e);
        console.error('La escena no pudo prepararse: ' + window.__error);
      });

    // ---- Vista previa: el único reloj, y solo elige qué t se dibuja ----
    function iniciarVistaPrevia() {
      window.addEventListener('resize', ajustarEscala);
      var pausada = false;
      var tActual = 0;
      var antes = null;
      function paso(ahora) {
        if (antes === null) antes = ahora;
        if (!pausada) {
          tActual = (tActual + (ahora - antes) / 1000) % c0.duracion;
          renderAt(tActual);
        }
        antes = ahora;
        window.requestAnimationFrame(paso);
      }
      window.addEventListener('keydown', function (ev) {
        var cuadro = Math.round(tActual * fps);
        if (ev.code === 'Space') { pausada = !pausada; ev.preventDefault(); return; }
        if (ev.code === 'ArrowRight' || ev.code === 'ArrowLeft') {
          var salto = (ev.shiftKey ? 10 : 1) * (ev.code === 'ArrowRight' ? 1 : -1);
          cuadro = (cuadro + salto + c0.cuadros) % c0.cuadros;
        } else if (ev.code === 'Home') {
          cuadro = 0;
        } else {
          return;
        }
        pausada = true;
        tActual = cuadro / fps;
        renderAt(tActual);
        ev.preventDefault();
      });
      window.requestAnimationFrame(paso);
    }
  };
})(globalThis.DL = globalThis.DL || {});
