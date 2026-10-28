# Corrección de bindings de texto — racafeForms 0.4.1

La asignación de type mediante addAttrs sin eliminar el atributo anterior puede concatenar
valores. Ahora se elimina type antes de asignar email/tel, conservando el tipo HTML literal
y la clase shiny-input-text. inputmode y autocomplete mantienen la mejora de teclado.

Corregir el atributo no garantiza el registro reactivo: Shiny 1.10.0 reconoce email, pero
no tel. La dependencia base incluye un adaptador de prioridad inferior que hereda getValue,
subscribe, receiveMessage y los demás métodos del binding de texto. Solo busca campos
email/tel propios no reconocidos por el selector nativo, sin modificarlo ni afectar otros
paquetes. La inscripción es idempotente. Password sigue usando passwordInput.

Se conservan firma pública, namespace, required, readonly, disabled, help, error, catálogo
y autocomplete explícito. Los cuatro tipos se actualizan con updateTextInput. Email tiene
la validación nativa de formato del navegador; el servidor debe validar correo, teléfono
y permisos. No se requieren recursos de Compartido para estos bindings básicos.

## Pruebas ejecutadas

- 437 comprobaciones testthat correctas, sin fallos, advertencias ni omisiones.
- 52 de ellas prueban los cuatro tipos, atributos, namespace, catálogo, autocomplete y moduleServer.
- 42 comprobaciones Chromium en Shiny y otras 42 en bs4Dash: valores iniciales registrados,
  escritura y salida reactiva real del servidor, aislamiento de dos módulos, métodos nativos
  compartidos por text/email/tel, password enmascarado, actualización de ida y vuelta desde R,
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
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual racafeForms_0.4.1.tar.gz
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

Base de revisión actualizada: origin/main en b61080cbfc17fc85ed86c41f1c0ae66aa7643153,
con racafeForms 0.4.0. Se conserva la API de esa versión y la corrección se publica como 0.4.1.
