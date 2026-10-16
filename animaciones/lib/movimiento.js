// Movimiento: un solo archivo de constantes, que todas las escenas importan. Ninguna escena escribe números de
// movimiento sueltos: las curvas, los resortes, las duraciones y los escalonados salen de aquí.
//
// Las curvas son las de la referencia de easings de Andrey Sitnik (cubic-bezier). Las reglas:
//   - se entra con una curva de salida (EASE.entrar), se sale con una de entrada (EASE.salir) y la salida dura de 40 a
//     50 % menos que la entrada (MS.entrada 400, MS.salida 240);
//   - lo que cruza la pantalla usa EASE.mover; la cámara (zoom, desplazamiento del gráfico) la más suave, EASE.camara;
//   - lineal solo para lo que es lineal: el tiempo, la edad que avanza, una barra de progreso;
//   - los resortes se dan con su razón de amortiguamiento (zeta): 1 no rebota, 0.7-0.85 sobrio, 0.4-0.6 juguetón.
//     La posición y el tamaño pueden rebotar; la opacidad y el color, nunca.
//
// Todo es una función pura del tiempo: resorte(t) devuelve el avance en t sin simular los cuadros anteriores.
(function (DL) {
  'use strict';

  // ---- Curvas cubic-bezier como funciones ----

  // bezier(x1, y1, x2, y2) devuelve u -> y, con u en [0, 1]. Resuelve x(s) = u por Newton con respaldo de bisección,
  // como los navegadores.
  function bezier(x1, y1, x2, y2) {
    function coord(s, a, b) { var m = 1 - s; return 3 * m * m * s * a + 3 * m * s * s * b + s * s * s; }
    function deriv(s, a, b) { var m = 1 - s; return 3 * m * m * a + 6 * m * s * (b - a) + 3 * s * s * (1 - b); }
    return function (u) {
      if (u <= 0) return 0;
      if (u >= 1) return 1;
      var s = u;
      for (var k = 0; k < 8; k++) {
        var dx = coord(s, x1, x2) - u;
        if (Math.abs(dx) < 1e-7) return coord(s, y1, y2);
        var d = deriv(s, x1, x2);
        if (Math.abs(d) < 1e-6) break;
        s -= dx / d;
      }
      var lo = 0, hi = 1;
      s = u;
      for (var j = 0; j < 40; j++) {
        var x = coord(s, x1, x2);
        if (Math.abs(x - u) < 1e-7) break;
        if (x < u) lo = s; else hi = s;
        s = (lo + hi) / 2;
      }
      return coord(s, y1, y2);
    };
  }

  var EASE = {
    entrar: bezier(0.16, 1, 0.3, 1),        // ease-out-expo: textos y fórmulas que aparecen
    entrarSuave: bezier(0.22, 1, 0.36, 1),  // ease-out-quint: entradas sobrias
    mover: bezier(0.76, 0, 0.24, 1),        // ease-in-out-quart: lo que se desplaza por la pantalla
    camara: bezier(0.65, 0, 0.35, 1),       // ease-in-out-cubic: zoom y desplazamiento de un gráfico entero
    salir: bezier(0.7, 0, 0.84, 0),         // ease-in-expo: salidas rápidas
    pop: bezier(0.34, 1.56, 0.64, 1),       // ease-out-back: un acento con sobrepaso (solo tamaño, nunca opacidad)
    lineal: function (u) { return u <= 0 ? 0 : (u >= 1 ? 1 : u); },
  };

  // Duraciones en milisegundos (convertir con seg()).
  var MS = {
    retro: 150,      // respuesta inmediata: un resaltado, un punto que se enciende
    entrada: 400,    // un texto o una fórmula que aparece
    mover: 500,      // un elemento que cambia de lugar
    heroe: 750,      // el movimiento principal de una toma
    trazo: 1600,     // una curva que se dibuja de izquierda a derecha
    salida: 240,     // lo que se va (40 % menos que la entrada)
  };

  // Escalonado entre elementos de un grupo, en milisegundos. El siguiente arranca cuando el anterior va por el 30-50 %.
  var ESCALONADO = { letra: 30, palabra: 90, item: 60, fila: 120 };

  // ---- Ayudas de tiempo ----

  function seg(ms) { return ms / 1000; }
  function limitar(x, a, b) { return x < a ? a : (x > b ? b : x); }
  function lerp(a, b, u) { return a + (b - a) * u; }

  // tramo(t, t0, t1): avance lineal de 0 a 1 entre t0 y t1 (0 antes, 1 después).
  function tramo(t, t0, t1) { return t1 <= t0 ? (t >= t1 ? 1 : 0) : limitar((t - t0) / (t1 - t0), 0, 1); }

  // entra(t, t0, dur, curva): avance con curva, de 0 en t0 a 1 en t0 + dur. Por defecto, EASE.entrar y MS.entrada.
  function entra(t, t0, dur, curva) {
    return (curva || EASE.entrar)(tramo(t, t0, t0 + (dur === undefined ? seg(MS.entrada) : dur)));
  }

  // sale(t, t0, dur, curva): de 1 en t0 a 0 en t0 + dur. Por defecto, EASE.salir y MS.salida.
  function sale(t, t0, dur, curva) {
    return 1 - (curva || EASE.salir)(tramo(t, t0, t0 + (dur === undefined ? seg(MS.salida) : dur)));
  }

  // visible(t, t0, t1, op): opacidad de algo que entra en t0 y sale en t1 (1 entre ambos). op.entrada y op.salida en
  // segundos; t1 = Infinity para lo que se queda. Entra con una curva de salida y sale con una de entrada.
  function visible(t, t0, t1, op) {
    op = op || {};
    var de = op.entrada === undefined ? seg(MS.entrada) : op.entrada;
    var ds = op.salida === undefined ? seg(MS.salida) : op.salida;
    if (t < t0 || t >= t1 + ds) return 0;
    var a = de > 0 ? EASE.entrarSuave(tramo(t, t0, t0 + de)) : 1;
    var b = (t1 === Infinity || ds <= 0) ? (t < t1 ? 1 : 0) : 1 - EASE.salir(tramo(t, t1, t1 + ds));
    return Math.min(a, b);
  }

  // resorte(t, zeta, f): avance de 0 a 1 de un resorte que arranca en t = 0, como función pura de t.
  // zeta: razón de amortiguamiento (1 no rebota, 0.7 sobrio, 0.5 juguetón). f: frecuencia en Hz.
  function resorte(t, zeta, f) {
    if (zeta === undefined) zeta = 0.7;
    if (f === undefined) f = 2;
    if (t <= 0) return 0;
    var w = 2 * Math.PI * f;
    if (zeta >= 1) return 1 - Math.exp(-w * t) * (1 + w * t);
    var wd = w * Math.sqrt(1 - zeta * zeta);
    return 1 - Math.exp(-zeta * w * t) * (Math.cos(wd * t) + (zeta * w / wd) * Math.sin(wd * t));
  }

  // Razón de amortiguamiento a partir de rigidez k, amortiguamiento d y masa m de cualquier biblioteca.
  function razonAmortiguamiento(k, d, m) { return d / (2 * Math.sqrt(k * (m || 1))); }

  // lectura(texto): segundos que un texto debe quedar quieto y legible: 0,3 s por palabra + 0,5 s. Una fórmula en
  // línea ($...$) cuenta como una palabra.
  function palabras(texto) {
    var limpio = String(texto).replace(/\$[^$]*\$/g, 'x').replace(/<[^>]*>/g, ' ').replace(/\\[a-zA-Z]+/g, ' ').trim();
    return limpio ? limpio.split(/\s+/).length : 0;
  }
  function lectura(texto) { return 0.3 * palabras(texto) + 0.5; }

  // escalonado(t, t0, k, pasoMs, dur, curva): avance del elemento k de un grupo que entra escalonado.
  function escalonado(t, t0, k, pasoMs, dur, curva) { return entra(t, t0 + k * seg(pasoMs), dur, curva); }

  // ---- Guion: tomas en secuencia ----

  // guion([['titulo', 3], ['modelo', 8.5], ...]) arma tomas consecutivas desde t = 0 (duraciones en segundos), o
  // guion({titulo: [0, 3], modelo: [3, 11.5]}) con inicios y fines explícitos. Devuelve:
  //   g.ini(nombre), g.fin(nombre), g.dur(nombre)
  //   g.u(nombre, t)        avance lineal dentro de la toma
  //   g.activa(nombre, t)   t está en [ini, fin)
  //   g.total               fin de la última toma
  function guion(tomas) {
    var tabla = {};
    var orden = [];
    var total = 0;
    if (Array.isArray(tomas)) {
      var cursor = 0;
      tomas.forEach(function (x) {
        tabla[x[0]] = [cursor, cursor + x[1]];
        orden.push(x[0]);
        cursor += x[1];
      });
      total = cursor;
    } else {
      Object.keys(tomas).forEach(function (k) {
        tabla[k] = tomas[k].slice();
        orden.push(k);
        total = Math.max(total, tomas[k][1]);
      });
    }
    function rango(nombre) {
      if (!tabla[nombre]) throw new Error('El guion no tiene una toma «' + nombre + '».');
      return tabla[nombre];
    }
    return {
      tomas: orden,
      total: total,
      ini: function (n) { return rango(n)[0]; },
      fin: function (n) { return rango(n)[1]; },
      dur: function (n) { return rango(n)[1] - rango(n)[0]; },
      u: function (n, t) { return tramo(t, rango(n)[0], rango(n)[1]); },
      activa: function (n, t) { return t >= rango(n)[0] && t < rango(n)[1]; },
    };
  }

  DL.mov = {
    bezier: bezier, EASE: EASE, MS: MS, ESCALONADO: ESCALONADO,
    seg: seg, limitar: limitar, lerp: lerp, tramo: tramo, entra: entra, sale: sale, visible: visible,
    resorte: resorte, razonAmortiguamiento: razonAmortiguamiento, palabras: palabras, lectura: lectura,
    escalonado: escalonado, guion: guion,
  };
})(globalThis.DL = globalThis.DL || {});
