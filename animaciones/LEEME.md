# Animaciones metodológicas de dismodlite

Animaciones cortas que muestran, paso a paso, cómo estima dismodlite: el modelo enfermedad-muerte, las tasas por
edad, la ecuación y su solución numérica, las bandas de edad, el ancla, los priors, los datos locales, el muestreo,
la estimación subnacional y los años vividos con discapacidad. Cada número que aparece en pantalla sale del paquete
(o de la guía «El modelo», `vignettes/el-modelo.qmd`, cuando la guía usa valores inventados para ilustrar).

Cada escena es **un programa que dibuja cuadros**, no un video: un archivo HTML en el que cada cuadro es una función
pura del tiempo. Se abre en el navegador y se reproduce; una herramienta la congela cuadro por cuadro, guarda los PNG
y ffmpeg arma el MP4. Así cada cuadro es reproducible, cada cambio es un número en el código y la escena se puede
revisar sola (fotogramas, hoja de contactos y una auditoría que mide lo que se dibuja). El método es el del
«Motion Lab» de Florence (@flxrnc, X, 30 de septiembre de 2026): dirección con números, contrato de render y el
ciclo renderizar–mirar–corregir.

## Las escenas

| # | Escena | Qué muestra | Funciones del paquete |
|---|---|---|---|
| 01 | `01-enfermedad-muerte` | Los compartimentos S y C, las tasas i, r, f, m y por qué m se cancela en dp/da | `dl_edo()` |
| 02 | `02-nudos` | θ: log i y log f en los nudos; la interpolación W, lineal en escala log y constante fuera | `dl_edo()` |
| 03 | `03-rk4` | Cómo se resuelve dp/da: RK4 con paso h = 0.2 años en la malla h/2, frente a la solución exacta | `dl_edo_resolver()` |
| 04 | `04-bandas` | De las edades simples a las bandas: el promedio ponderado por la población; p, i(1-p) y p f | `dl_insumos()` |
| 05 | `05-ancla` | El ancla: su intervalo como log-normal (σ_log), la correlación AR(1) entre bandas (ρ) y su peso (λ) | `dl_ajustar()` |
| 06 | `06-priors` | La suavidad de log i (segundas diferencias, σ) y el prior de la mortalidad en exceso (csmr/p, techo) | `dl_prior_emr()` |
| 07 | `07-datos-locales` | Cómo entra cada dato: log-normal, binomial o Poisson; `datos_en_ajuste` y el doble conteo | `dl_ajustar()` |
| 08 | `08-mcmc` | Metropolis-Hastings adaptativo por bloques, el calentamiento, las simulaciones, R-hat y ESS | `dl_mh()`, `dl_ajustar()` |
| 09 | `09-subnacional` | De lo nacional a lo subnacional: covariables, β, κ, nueva solución y renormalización | `dl_cascada()` |
| 10 | `10-avd` | Los AVD: p × Σ π DW × COMO, con la severidad simulada | `dl_avd()` |
| 11 | `11-corrida` | Una corrida de principio a fin: insumos, ajuste, cascada, AVD, diagnósticos y manifiesto | `dl_correr()` |

## Ver una escena

Abre `escenas/<escena>/index.html` en Chrome (doble clic; no hace falta servidor). En la vista previa:
**espacio** pausa, **← →** avanzan un cuadro (con **mayúsculas**, diez), **Inicio** vuelve al cuadro 0, y un contador
abajo a la derecha dice el cuadro y el segundo. `index.html?t=12.5` congela el cuadro de 12.5 s; `?cuadro=375`, el
cuadro 375. Las fuentes (Google Fonts) y KaTeX (jsDelivr, versión fija) se cargan de internet.

## Renderizar

Hace falta Node 22, ffmpeg y, para regenerar los datos, R con dismodlite instalado.

```sh
cd animaciones
npm install                                    # Playwright (Chromium headless)
Rscript datos/exportar.R                       # los números de cada escena, desde el paquete (datos/*.js)
node herramientas/render.mjs escenas/01-enfermedad-muerte              # cuadros, MP4 y hoja de contactos
node herramientas/render.mjs escenas/01-enfermedad-muerte --gif        # además, el GIF
node herramientas/render.mjs escenas/01-enfermedad-muerte --marcas     # un fotograma por marca
node herramientas/render.mjs escenas/01-enfermedad-muerte --hoja       # borrador: 12 cuadros en una hoja
node herramientas/render.mjs escenas/01-enfermedad-muerte --cuadros 0,24,48
node herramientas/render.mjs escenas/01-enfermedad-muerte --cebolla 1200-1260:4   # piel de cebolla
node herramientas/auditar.mjs escenas/01-enfermedad-muerte             # la auditoría
node --test pruebas/                           # modelo.js frente al paquete (Rscript pruebas/fixtures.R antes)
```

Todo va a `salida/<escena>/` (fuera de git). Si la red pasa por un proxy, `render.mjs` toma `HTTPS_PROXY`.

## El contrato de render

1. Un archivo HTML autocontenido por escena. Solo las bibliotecas que se nombran, fijadas a una versión (KaTeX
   0.16.11 de jsDelivr), y las fuentes de Google Fonts. Nada más se pide a la red.
2. Cada cuadro es una función pura del tiempo t, en segundos: `window.renderAt(t)` dibuja exactamente ese cuadro.
3. Nada de `Date.now`, `new Date()`, `setTimeout`, `setInterval`, `performance.now`, `requestAnimationFrame` ni
   `Math.random` en la escena. El azar es con semilla (`DL.azar`, `DL.hash01`).
4. Nada de animaciones ni transiciones CSS: todo se mueve desde `renderAt`.
5. Se renderizan los cuadros 0 a N − 1. El cuadro 0 es una composición terminada: el título se lee desde el cuadro 0.
6. Ningún texto se mueve mientras se lee, salvo en un movimiento de cámara declarado (`camaras`), y cada texto queda
   legible al menos 0,3 s por palabra + 0,5 s (una fórmula en línea cuenta como una palabra).

`herramientas/auditar.mjs` comprueba todo eso, más los márgenes, los choques entre textos, las entradas simultáneas
y las comprobaciones numéricas que declara cada escena (por ejemplo, que la curva dibujada sea la que calcula el
paquete). Cuando una corrección funciona, se agrega al contrato: el contrato es la memoria del laboratorio.

## Cómo se hace una escena

Cada escena vive en `escenas/<NN-nombre>/` con dos archivos, y sus datos en `datos/r/<NN-nombre>.R`:

- `guion.md`: el guion dirigido, escrito **antes** del código, con la plantilla de abajo. Cada adjetivo es un número.
- `index.html`: la escena. Carga `lib/contrato.js`, `lib/movimiento.js`, `lib/dibujo.js`, `lib/modelo.js` (si la usa)
  y `datos/<NN-nombre>.js`, y llama a `DL.escena({...})`. La escena 01 es el ejemplo de referencia.
- `datos/r/<NN-nombre>.R`: define `exportar(ctx)` y devuelve una lista con los números de la escena, calculados con
  dismodlite. `Rscript datos/exportar.R <NN-nombre>` la escribe en `datos/<NN-nombre>.js`. `ctx` da, una sola vez,
  `ctx$proyecto()`, `ctx$insumos()` y `ctx$ajuste()` del proyecto de ejemplo (causa 9100, datos sintéticos).

La plantilla del guion:

```text
OBJETIVO: lo que quien mira debe entender al final, en una oración.
SALIDA: un HTML autocontenido, SVG + KaTeX 0.16.11, 1920 x 1080, con lib/contrato.js.
TIEMPO: segundos x 30 cuadros/s = cuadros. Sin bucle: el cuadro 0 es una composición terminada.
ASPECTO: colores (hex), tipografías y tamaños; qué lleva color de serie y qué va en tinta.
MOVIMIENTO: curvas (tokens de movimiento.js), resortes por razón de amortiguamiento, duraciones en segundos.
TOMAS: tramos de tiempo, un movimiento principal por toma y cómo pasa a la siguiente.
PROHIBIDO: los defectos que no quieres ver.
REVISIÓN: fotogramas en las marcas, hoja de contactos y las comprobaciones numéricas de la auditoría.
```

El ciclo de trabajo, dos a cuatro vueltas: escribir el guion → exportar los datos → programar la escena →
`render.mjs --marcas` y **mirar** cada fotograma → anotar con números de cuadro → corregir → `--hoja` →
`auditar.mjs` hasta «Todo en orden» → render completo. Para un problema que no se va: renderizar los cuadros a
cada lado, compararlos y corregir solo la línea que lo causa.

## Reglas de diseño

- **Retícula** de 1920 × 1080: márgenes de 96 px; cabecera arriba (`D.cabecera`), narración abajo (`D.rotulo`), la
  nota de la fuente abajo a la derecha (`D.pie`). Se elige la disposición antes del primer cuadro; si un bloque
  tiene que correrse para dejar sitio, es un movimiento de cámara declarado entre dos frases.
- **Color**: incidencia `i` #2a78d6, mortalidad en exceso `f` #eb6834 y prevalencia `p` #1baf7a (validados para
  daltonismo entre todos los pares; no hay cuarto matiz). Lo que viene de fuera del modelo (ancla, datos, población)
  va en tinta con forma propia: cajas, bigotes, puntos huecos. El verde azulado de la marca, #0E6B62, señala lo que
  se explica. El texto va en tinta (#16211E) o tinta suave (#55645F), nunca en el color de una serie: en las
  fórmulas, cada tasa lleva un subrayado de su color (`\htmlClass` + `op.estilos`).
- **Tipografía**: títulos Source Serif 4; texto Source Sans 3; números y código IBM Plex Mono; fórmulas KaTeX con
  la notación de la guía «El modelo». Nada legible por debajo de 22 px; la narración a 36 px.
- **Movimiento**: solo los tokens de `lib/movimiento.js` (curvas, duraciones, escalonado, resortes). Se entra con
  `EASE.entrar` o `entrarSuave`, se sale con `EASE.salir` y más rápido; lo que cruza la pantalla, `EASE.mover`; la
  cámara, `EASE.camara`; lineal solo para lo que avanza a ritmo constante (la edad, las iteraciones).
- **Una cosa principal por momento**. Lo secundario recorre el 50-70 % y arranca 100-200 ms después; lo demás espera,
  baja al 40 % o se queda quieto.
- **Prohibido**: degradados morados o azules, brillos, destellos, partículas decorativas, todo apareciendo a la vez,
  aparecer-y-subir en cada elemento, todo centrado, texto que se mueve mientras se lee, 3D, sombras, emojis, más de
  tres destellos por segundo, números que no salgan del paquete, notación distinta de la guía.
- **Honestidad**: los datos del ejemplo son sintéticos y el pie lo dice; un valor ilustrativo se rotula como tal.

## La biblioteca (`lib/`)

- `contrato.js`: `DL.escena({ id, titulo, duracion, fps, marcas, camaras, preparar, dibujar(t, c), auditoria(A) })`;
  dentro de `dibujar`, `c.svg(...)` agrega elementos, `c.defs(...)` definiciones y `c.dato(nombre, valor)` un número
  para la auditoría. `A.en(marca, nombre, comprobar(estado))` declara una comprobación; `A.cerca(a, b, tol)`.
  `DL.azar(semilla)` y `DL.hash01(...)` para el azar con semilla.
- `movimiento.js` (`DL.mov`): `EASE`, `MS`, `ESCALONADO`, `entra`, `sale`, `visible(t, t0, t1)`, `tramo`,
  `resorte(t, zeta, f)`, `escalonado`, `lectura(texto)`, `guion([...])`.
- `dibujo.js` (`DL.dib`): `paleta`, `fuentes`, `marco`, `tam`; `texto`, `parrafo` (con `$fórmulas$`, `**negrita**`,
  `\n`), `formula` (KaTeX, con `estilos` por término), `rect`, `linea`, `circulo`, `trazo` (con `progreso`), `area`,
  `flecha` (recta o curva, con `progreso`), `grupo` (traslación, escala, opacidad), `ejes` (escala lineal o log,
  `curva`, `banda`, `puntos`), `cabecera`, `rotulo`, `pie`; formatos `num`, `sig`, `miles`.
- `modelo.js` (`DL.modelo`): la matemática del paquete en JavaScript (matriz W, RK4 de `dl_edo_resolver()`,
  `dl_edo()`, bandas, log-posterior, R-hat, ESS, renormalización, momentos de la Beta), comprobada contra el paquete
  en `pruebas/` (`Rscript pruebas/fixtures.R && node --test pruebas/`). Sirve para lo que una escena calcula cuadro
  a cuadro (un nudo que se mueve y la curva que responde); lo demás viene de `datos/`.
