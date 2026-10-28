# Corrección de bindings de texto — racafeForms 0.3.1

La implementación anterior construía un textInput y después cambiaba su tipo HTML a
email o tel. Esto depende de los selectores que soporte el binding de cada versión de
Shiny; por ejemplo, Shiny 1.10.0 incluye email en su binding de texto, pero no tel.
No se asume que todas las versiones fallen igual. Para una ruta compatible y estable,
ambas opciones conservan type="text" y añaden inputmode/autocomplete apropiados.
Password sigue usando passwordInput. No se modifica ni reemplaza ningún binding JS.

Se conservan la firma pública, el namespace una sola vez, required, readonly, disabled,
help, error, los catálogos de sugerencias y el autocomplete explícito. Los cuatro tipos
se actualizan mediante shiny::updateTextInput(session, id_local, value = ...).
El correo ya no usa validación HTML typeMismatch: la validación de formato de correo
/teléfono corresponde al servidor. Los atributos required y aria-* siguen presentes.
El modo de teclado es una sugerencia; su presentación depende del navegador/dispositivo.

## Pruebas ejecutadas

- 285 comprobaciones testthat correctas, sin fallos, advertencias ni omisiones.
- 48 de ellas prueban los cuatro tipos, atributos, namespace, catálogo, autocomplete y moduleServer.
- 41 comprobaciones Chromium en Shiny y otras 41 en bs4Dash: valores iniciales registrados,
  escritura y salida reactiva real del servidor, aislamiento de dos módulos, binding nativo
  compartido por text/email/tel, password enmascarado, actualización de ida y vuelta desde R,
  required con valor vacío, readonly/disabled, ayuda y error asociados al ID local.
- Sin errores JS/servidor ni peticiones externas en ambas pruebas.
- R CMD build/install correctos; R CMD check --no-manual: Status OK, usando
  _R_CHECK_FORCE_SUGGESTS_=false porque devtools no está instalado.
- Verificador de documentación del monorepo correcto. No se modificó Compartido,
  ninguna aplicación consumidora ni FastAPI. No se añadieron dependencias.

Entorno: R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5, htmltools 0.5.8.1,
Playwright 1.62.1 y Chromium 154. Los datos y la contraseña de la prueba son ficticios.
No se usaron credenciales reales. No se afirma comprobación física del teclado móvil.

## Reproducir

```sh
Rscript -e 'testthat::test_local("racafeForms", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD build racafeForms
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual racafeForms_0.3.1.tar.gz
Rscript -e 'shiny::runApp("racafeForms/inst/examples/text-input-bindings.R", port=3890)'
# Repetir con TEXT_BINDINGS_BS4DASH=1 y otro puerto para bs4Dash.
```

Con el servidor iniciado, desde tests/browser, ejecutar npm ci e instalar Chromium de
Playwright, o indicar CHROMIUM_PATH a un ejecutable existente:

```sh
RACAFE_TEST_URL=http://127.0.0.1:3890 CI=1 CHROMIUM_PATH=/usr/bin/chromium node test-text-input-bindings.cjs
```

El ejemplo tiene moduleUI/moduleServer, dos instancias y racafeForm. Muestra los valores
reactivos, incluida la contraseña ficticia, únicamente para demostrar el binding; no
copie esa salida ni utilice datos sensibles en un entorno consumidor.

Base local de revisión: origin/main en 483180da53995ae5555d182e65f983a3edcdcac1.
La actualización remota del checkout no pudo ejecutarse por falta de autenticación GitHub;
se debe contrastar el PR con main remoto cuando se restablezca ese acceso.
