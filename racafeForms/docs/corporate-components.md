# Componentes corporativos y validación

Esta revisión de racafeForms 0.6.0 mantiene las APIs anteriores y añade presentación reutilizable.
No requiere cambios en aplicaciones consumidoras. No incluye persistencia ni servicios externos.

## API pública nueva

| Función | Uso |
| --- | --- |
| `racafeSummaryTable(data, caption = NULL, empty = "Sin registros", digits = 2L)` | Tabla escapada, numéricos con coma de miles y punto decimal. Desplazamiento local si es necesario. |
| `racafeMoneyTotal(label, value, currency = "COP", digits = 2L)` | Etiqueta y total monetario en una fila; no calcula valores. |
| `racafeStack(..., spacing = c("normal", "compact", "comfortable"))` | Separación vertical uniforme entre hijos directos. |
| `racafeNoticeList(items, type = c("info", "warning", "success"), title = NULL)` | Lista informativa; no bloquea formularios. |
| `racafeConfirmation(id, fields = list(), ..., title, confirm_label, cancel_label, ns, size, cancel_id = NULL)` | Modal con `racafeInfoGrid()` e `racafeInfoItem()`; omite campos NULL y admite textos, tablas y actividades. |
| `racafeResetValidation(validator)` | Reinicia mensajes e intento, sin destruir controles, valores ni observers. Equivale a `validator$reset()`. |

Las firmas completas, parámetros y ejemplos están en la ayuda de RStudio.

```r
ui <- function(ns) racafeForms::racafeForm(id = "form", ns = ns,
  racafeForms::racafeInputText("name", "Nombre", required = TRUE, ns = ns),
  racafeForms::racafeButton("save", "Revisar", ns = ns))

server <- function(id) shiny::moduleServer(id, function(input, output, session) {
  validation <- racafeForms::racafeFormValidation(input, function() ui(session$ns), session,
    warnings = list(name = function(value, values) {
      if (nchar(value) < 4) "Nombre corto; puede continuar." else TRUE
    }))
  shiny::observeEvent(input$save, {
    if (!validation$validate()) return()
    shiny::showModal(racafeForms::racafeConfirmation("confirm", list(Nombre = input$name, Opcional = NULL),
      racafeForms::racafeSummaryTable(data.frame(Producto = "Café", Importe = 1234.56)),
      racafeForms::racafeMoneyTotal("Total", 1234.56),
      racafeForms::racafeNoticeList(c("Revisar los datos", "Confirmar")), ns = session$ns))
  })
  shiny::observeEvent(input$confirm, {
    if (!validation$validate()) return()
    shiny::removeModal()
    # Guardado de demostración en memoria; no se escribe en servicios externos.
    racafeForms::racafeResetValidation(validation)
    racafeForms::racafeNotify(session, "Guardado en memoria", type = "success")
  })
})
```

## Validación

`racafeValidateForm()` y `racafeFormValidation()` aceptan `warnings` como último argumento.
Utiliza reglas nombradas con el mismo contrato de `rules`: TRUE o un mensaje.
Cada campo muestra un único mensaje, con prioridad del error sobre la advertencia.
`result$warnings` contiene avisos; `valid` y `pending` dependen exclusivamente de errores.
Los opcionales ausentes no se indexan como errores ni ejecutan reglas de campos vacíos.

El controlador sigue validando de forma diferida por defecto; después del primer intento revalida
al editar. `initial = TRUE` conserva la modalidad reactiva inmediata. Tras guardar, invoque
`racafeResetValidation(validation)`. Para borrar también los valores, llame primero a
`racafeResetForm(session, "form")`. El controlador conserva sus observers y puede reutilizarse.
Los esquemas reactivos admiten controles de `renderUI()` e `insertUI()`; los IDs deben ser únicos.
Cada controlador limpia exclusivamente sus propios campos, incluso en formularios anidados.

## Controlbar y compatibilidad

`bs4Dash` cierra al hacer clic fuera de un controlbar no fijado. `pinned = TRUE` evita ese cierre
nativamente, pero también deshabilita el botón de cierre hasta desfijarlo. Las demos conservan
`pinned = FALSE` para comprobar apertura, interacción y cierre normales.

Los popups corporativos reconocen el controlbar propietario. Durante su clic se utiliza
transitoriamente `data-pin` y se restaura su valor antes del siguiente turno de interacción.
Los listeners se instalan directamente en el controlbar y sus popups propios, una vez por nodo;
no detienen propagación ni impiden acciones de Bootstrap. El área desplazable descuenta la altura
de la barra nativa de fijación para mantener accesibles sus últimos controles. Incluye picker en portal, calendarios,
modales y controles dinámicos. Otros controles y páginas no reciben esta protección.

`racafeFormRow()` conserva sus modalidades y su valor predeterminado NULL. Las columnas explícitas
no requieren CSS de la aplicación. Los valores de los selectores conservan sus códigos originales;
`uppercase` cambia únicamente la presentación, incluidos Ninguno/Ninguna/Todos/Todas.

`racafeConfirmation()` es nuevo: su confirmación es un botón Shiny con contador.
`racafeConfirm()` mantiene su resultado lógico SweetAlert. No son intercambiables sin adaptar el handler.
`ListaDesplegableAgregable(confirmation = "summary")` utiliza el nuevo modal; el valor por defecto
`"alert"` conserva IDs y contrato anteriores. `confirm = FALSE` continúa incorporando sin confirmación.
El modo summary requiere racafeForms >= 0.6.0.

## Fuente de estilos y reglas consolidadas

Compartido/Styles/racafe-controls.css es la fuente canónica nueva. Se distribuye idéntica en
racafeForms/inst/www y en la instantánea de la galería; nunca se edita una copia a mano.

Se extrajeron de racafeForms/inst/www/racafe-forms.css:

- Menú del picker y acciones de selección, búsqueda, hover y selección simple/múltiple.
- Mayúsculas visuales del picker y su portal.
- Check rojo del menú múltiple alojado fuera del control.

Se eliminó el listener global de clic del controlbar que detenía la propagación. Los tamaños de
modal siguen en racafe-modal-sizes.css; no se duplican en el archivo nuevo. No se modifican
las reglas heredadas de columnas ni los estilos de navegación de AdminLTE.

La identidad usa tokens de Compartido y los componentes nuevos usan variables de separación,
tamaño, color y tipografía. Los campos conservan sus tokens de altura y radio. El rojo es #DA291C.
`Styles/style.css` sigue siendo autocontenido para includeCSS(URL): contiene fuentes consolidadas,
sin imports relativos. `components.css` sigue siendo el punto de entrada sin reglas generales.

Reconstrucción desde la raíz de cada repositorio:

```sh
# Compartido
Rscript utils/build_styles.R
Rscript utils/build_styles.R --check
Rscript tests/test-build-styles.R
# Libreria-Racafe
Rscript racafeForms/tools/sync_shared.R /ruta/Compartido
```

## Archivos por repositorio

### Libreria-Racafe

```text
racafeForms/DESCRIPTION
racafeForms/NAMESPACE
racafeForms/NEWS.md
racafeForms/R/01_layout.R
racafeForms/R/11_validation.R
racafeForms/R/13_summaries.R
racafeForms/R/gallery.R
racafeForms/README.md
racafeForms/docs/corporate-components.md
racafeForms/inst/gallery-assets/README.md
racafeForms/inst/gallery-assets/Styles/components.css
racafeForms/inst/gallery-assets/Styles/racafe-controls.css
racafeForms/inst/gallery-assets/Styles/tokens.css
racafeForms/inst/www/racafe-controls.css
racafeForms/inst/www/racafe-forms.css
racafeForms/inst/www/racafe-forms.js
racafeForms/man/racafeConfirmation.Rd
racafeForms/man/racafeFormValidation.Rd
racafeForms/man/racafeMoneyTotal.Rd
racafeForms/man/racafeNoticeList.Rd
racafeForms/man/racafeResetValidation.Rd
racafeForms/man/racafeStack.Rd
racafeForms/man/racafeSummaryTable.Rd
racafeForms/man/racafeValidateForm.Rd
racafeForms/tests/browser/test-corporate-workflow.cjs
racafeForms/tests/browser/test-gallery.cjs
racafeForms/tests/testthat/test-corporate-components.R
racafeForms/tools/sync_shared.R
```

### racafeModulos

```text
NEWS.md
R/ListaDesplegableAgregable.R
README.md
man/ListaDesplegableAgregable.Rd
tests/testthat/test-lista-agregable.R
```

### Compartido

```text
Styles/README.md
Styles/components.css
Styles/racafe-controls.css
Styles/style.css
Styles/tokens.css
tests/browser/test-styles.cjs
tests/styles-regression/app.R
tests/test-build-styles.R
utils/build_styles.R
```

## Pruebas ejecutables

`racafeFormsGallery()` contiene 66 ejemplos, incluidos los seis exports nuevos. Su controlbar
incluye validación, módulo anidado, renderUI, insertUI, selector dependiente, calendarios, tablas,
totales, avisos, confirmación y notificación, sin datos empresariales.
`racafeModulos::DemoListaDesplegableAgregable()` demuestra la confirmación con resumen en controlbar.

```r
testthat::test_local("racafeForms")
testthat::test_local("/ruta/racafeModulos")
shiny::runApp(racafeForms::racafeFormsGallery(), port = 4040)
```

Con las dependencias de tests/browser y Chromium disponibles, en otra terminal:

```sh
RACAFE_GALLERY_URL=http://127.0.0.1:4040 node racafeForms/tests/browser/test-corporate-workflow.cjs
RACAFE_TEST_URL=http://127.0.0.1:4040 node racafeForms/tests/browser/test-gallery.cjs
```

También se conservan las pruebas responsive-rows.cjs y test-modal-controlbar.cjs. Estas verifican
los anchos del contenedor, densidades, ventanas, binding, tamaños de modal antes de apertura,
selectores simples/múltiples, portales y cierre nativo. La suite de Compartido compara navegación
con el CSS anterior y comprueba la inclusión directa mediante includeCSS.

## Resultado de validación local

- racafeForms: 1.086 comprobaciones testthat; R CMD check sin errores, advertencias ni notas.
- racafeModulos: 63 comprobaciones testthat; R CMD check sin errores y con cuatro advertencias
  y una nota preexistentes (LICENSE, Unicode del código anterior, library/require y documentación heredada).
- Compartido: 28 comprobaciones de construcción; salida determinista y --check correcto.
- Navegador: 4.766 comprobaciones de columnas, 250 de galería, 58 de modal/controlbar,
  69 de CSS/bs4Dash y 26 del flujo de resúmenes/validación/agregable, incluida fijación nativa.
- Sidebar de Compartido idéntico píxel a píxel a la referencia; reglas anteriores preservadas en orden.
- Copias de racafe-controls.css idénticas byte a byte a su fuente canónica.

Entorno validado: R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5, shinyWidgets 0.9.1 y Chromium.
R CMD check se ejecutó con --no-manual, --no-build-vignettes y _R_CHECK_FORCE_SUGGESTS_=false.
No se realizaron despliegues, instalaciones productivas ni publicaciones.
