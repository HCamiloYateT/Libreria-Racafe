# Estandarización integral de formularios

Estado: cambios preparados para revisión mediante PR, validados el 3 de noviembre de 2026.
No se modifica CRM Nacional ni se publican tags o versiones. `Compartido` conserva sus recursos 1.1.0;
se revisaron y reutilizan sus variables sin necesitar cambios en ese repositorio.

## Instalación y demostración

Desde la raíz de Libreria-Racafe:

```r
remotes::install_local("racafeForms", dependencies = NA, upgrade = "never")
```

Reinicie R y recargue el navegador sin caché: la versión de los recursos no se incrementó.
En una sesión nueva:

```r
shiny::runApp(system.file("examples", "standardized-forms.R", package = "racafeForms"))
```

La demo no usa bases de datos ni servicios externos. Presenta una instancia en el cuerpo y
otra en el controlbar, campos dinámicos, numéricos, selecciones, switch lógico con
`conditionalPanel`, fechas/rangos/fecha-hora, validación, CSV y modal. El guardado se simula
en memoria. Opciones antes de ejecutarla:

```r
Sys.setenv(RACAFE_STANDARD_BS4DASH = "0") # Shiny sin bs4Dash; omitir para el dashboard
Sys.setenv(RACAFE_STANDARD_MODE = "both") # inline (predeterminado), alert o both
Sys.setenv(RACAFE_SHARED_PATH = "/ruta/a/Compartido") # opcional, sin dependencia fija
```

## API de validación

```r
racafeValidateForm(input, form, rules = list(), required = list(), ns = ...)
racafeShowValidation(session, result, mode = c("inline", "alert", "both"), focus = TRUE)
racafeFormValidation(input, form, session = shiny::getDefaultReactiveDomain(),
                     rules = list(), required = list(),
                     mode = c("inline", "alert", "both"), initial = FALSE)
```

`form` acepta la UI de un formulario o una función que devuelve la UI actual. Los controles
corporativos aportan ID, etiqueta, obligatoriedad, tipo y límites. También admite una lista
nombrada de esquemas (`label`, `required`, `kind`, `min`, `max`) para controles ajenos;
`kind` admite text/select/numeric/logical/date/range/datetime. No añade bindings a controles
ajenos. Los IDs de las reglas son los nombres locales de `input`, sin prefijo de módulo.

El resultado contiene `valid`, `errors`, `pending`, `labels`, `ids`, `values` y `form_id`.
`values` conserva tipos R; los valores no se envían al navegador por el canal de errores.
Valida NULL, NA, textos vacíos o con espacios, selectores vacíos, números finitos y límites,
Date, rangos ordenados y POSIXct. `FALSE` y `0` son valores válidos. Para exigir aceptación,
combine `required = TRUE` y una regla `isTRUE(value)`: así tampoco pasa un valor ausente.
Las reglas de negocio adicionales siguen siendo responsabilidad del servidor.

Cada regla es `function(value, values)` y devuelve `TRUE` o un mensaje. Un campo admite una
lista de reglas. Los opcionales vacíos omiten reglas; use `required` para obligatoriedad
condicional. Tanto `rules` como `required` admiten funciones que devuelven su lista actual.
Al ocultar un control por una condición, exclúyalo del esquema o haga que su regla devuelva
TRUE cuando esté inactivo; ocultarlo en el navegador no modifica las reglas del servidor.

`racafeFormValidation()` se registra **una vez** por sesión/formulario. Métodos:

- `validate(show = TRUE)`: valida y devuelve un lógico; debe bloquear cada acción de negocio.
- `check()`: resultado actual sin mostrar mensajes ni marcar un intento.
- `result()` y `attempted()`: reactivos para consultar el estado.
- `destroy()`: destruye su observador cuando el propietario elimina el formulario.

No muestra errores hasta `validate()`, salvo `initial = TRUE`. Después revalida al editar,
limpia errores corregidos y actualiza pendientes sin repetir alertas ni mover el foco.
`validate()` enfoca el primer pendiente; con alerta espera al cierre. Inline incluye resumen
y errores por campo con atributos accesibles. Los errores explícitos `error=` permanecen
bajo control de la aplicación y no se eliminan automáticamente.

### Migración de validaciones manuales

Antes, cada acción repetía `req()`/`validate(need())` y mensajes personalizados. Ahora
reutilice la misma función de UI y su namespace completo:

```r
form_ui <- function(ns) {
  racafeForms::racafeForm(id = "form", ns = ns, unstyled = TRUE,
    racafeForms::racafeFormGroup(title = "Contacto",
      racafeForms::racafeInputText("nombre", "Nombre", required = TRUE, ns = ns),
      racafeForms::racafeInputSwitch("contactar", "Requiere correo", ns = ns),
      shiny::conditionalPanel("input.contactar === true", ns = ns,
        racafeForms::racafeInputText("correo", "Correo", type = "email", ns = ns))),
    racafeForms::racafeFormFooter(
      racafeForms::racafeButton("guardar", "Guardar", ns = ns),
      racafeForms::racafeButtonDownload("csv", "Descargar", validate = TRUE, ns = ns)))
}
moduleUI <- function(id) form_ui(shiny::NS(id))
moduleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    validation <- racafeForms::racafeFormValidation(input,
      form = function() form_ui(session$ns), session = session,
      required = list(correo = function(values) isTRUE(values$contactar)),
      rules = list(correo = function(value, values) {
        if (!isTRUE(values$contactar) || grepl("^[^@ ]+@[^@ ]+[.][^@ ]+$", value)) TRUE
        else "Revise el correo."
      }))
    shiny::observeEvent(input$guardar, {
      if (!validation$validate()) return()
      # Guardar con las validaciones y permisos de negocio propios.
      racafeForms::racafeUpdateButton(session, "guardar", success = TRUE)
    })
    racafeForms::racafeDownloadHandler("csv", validation, "contacto.csv",
      content = function(file, values) {
        utils::write.csv(data.frame(nombre = values$nombre), file, row.names = FALSE)
      }, session = session)
  })
}
```

En módulos anidados use `session$ns` al reconstruir la UI del esquema; `NS(id)` por sí solo
puede perder el namespace del módulo padre. Cada formulario debe registrar únicamente sus
campos. Para campos dinámicos, la función `form` debe devolver el esquema actual y las
reglas deben contener únicamente IDs actuales. La demo muestra adición, eliminación y
recreación. Puede montar un formulario sin ID, pero un ID explícito facilita referenciarlo;
no registre varios validadores para el mismo formulario montado. Si elimina por completo
una instancia, destruya su validador y cree uno para la nueva instancia.

## Fecha-hora y botones

```r
racafeInputDateTime(id, label = NULL, value = Sys.time(), min = NULL, max = NULL,
                    tz = "America/Bogota", ns = NULL, width = "100%",
                    disabled = FALSE, required = FALSE, help = NULL, error = NULL)
racafeUpdateButton(session = shiny::getDefaultReactiveDomain(), id,
                   disabled = NULL, loading = NULL, success = NULL)
racafeDownloadHandler(id, validation, filename, content,
                      session = shiny::getDefaultReactiveDomain(), contentType = NULL)
```

Fecha-hora usa el mismo `shinyWidgets::airDatepickerInput()` que las otras fechas: español,
`dd/mm/yyyy`, hora de 24 horas y sin botón lateral. Interpreta texto en `tz`, acepta POSIXct
y devuelve POSIXct convertible a UTC. Los límites se comprueban también en R. La
representación local se envía como texto para evitar desplazamientos de zona del navegador
en shinyWidgets 0.9.1; no se activa la rama defectuosa `options$tz` de esa versión. No se
modifican calendarios de otros paquetes. Los actualizadores de fechas anteriores conservan
su contrato; esta ampliación no añade un actualizador específico de fecha-hora.

`racafeButton()` y `racafeButtonDownload()` añaden `disabled`/`loading` sin desplazar los
argumentos existentes. `racafeUpdateButton()` acepta IDs locales dentro del módulo; NULL
conserva el estado. La confirmación `success` persiste hasta actualizarla explícitamente.
Deshabilitar un botón es un estado de interfaz: la acción en R debe seguir validando.

Para descargar, sustituya el enlace oculto y su clic manual por el par botón/handler.
`validate = FALSE` conserva la descarga nativa anterior. Con TRUE, el clic solicita validar,
solo inicia si es válido y **content vuelve a validar**, incluso ante acceso directo a la
URL. `content(file, values)` recibe una instantánea validada y tipada; también acepta
`content(file)`. Registre una vez por ID/sesión y añada sus propios permisos de acceso.

## Cambios visuales y compatibilidad

Las firmas anteriores se conservan; los argumentos nuevos se añaden al final. Switches,
checkboxes y radios reciben estados comunes y etiquetas uniformes; el switch sigue
transmitiendo lógicos reales y admite navegación por teclado. Los readonly conservan el
binding nativo y el atributo readonly. Ni readonly ni disabled sustituyen permisos en R.

Los formularios mantienen el tamaño compacto predeterminado. Las etiquetas comparten
estilo; los controles de una línea conservan las variables existentes de altura y radio.
Textareas conservan sus filas y los controles binarios su forma compacta. Los encabezados
HTML conservan contenido y clases internas, normalizando h1–h6 para impedir títulos enormes.
El título de pestaña pertenece a la navegación; no lo duplique dentro del formulario.

`racafeFormGroup(divider = TRUE)` separa el grupo del siguiente grupo hermano; no dibuja un
borde final antes del footer. `divider = FALSE` lo desactiva. Elimine `hr()` manuales entre
grupos. Admite grupos anidados y sin título. Dentro de un panel use `racafeForm(unstyled =
TRUE)` para evitar tarjetas duplicadas. `racafeModalFooter()` se reutiliza sin crear otra API.
El controlbar reduce espacios y evita desbordar pantallas estrechas. Todo CSS nuevo se acota
a componentes del paquete y reutiliza variables corporativas; las aplicaciones no necesitan
copiar CSS ni depender de una ruta fija a Compartido.

## Archivos de la entrega

En Libreria-Racafe, bajo `racafeForms/`:

- `R/00_utils.R`, `01_layout.R`, `02_inputs.R`, `03_buttons_alerts.R`, `05_panels_cards.R`:
  metadatos, encabezados, grupos, estados y compatibilidad de controles existentes.
- `R/11_validation.R`, `R/12_datetime_download.R`: seis funciones públicas nuevas y helpers.
- `inst/www/racafe-forms.css` y `.js`: estilos, accesibilidad, errores y protocolo de descargas.
- `inst/examples/standardized-forms.R`: demostración autocontenida.
- `NAMESPACE`, `man/*.Rd`: exportaciones y ayuda de RStudio generadas con roxygen2.
- `tests/testthat/test-form-validation.R`, `tests/browser/test-standardized-forms.cjs`
  y `tests/browser/test-validation-modes.cjs`:
  regresiones de servidor y navegador.
- `README.md`, `NEWS.md` y esta guía: instalación, cambios y migración.

En PlantillaBs4Dash: `APP/global.R`, `APP_SINGLE/global.R`, ambos
`misc/modules/demo.R` y `README.md`. Los módulos reutilizan el esquema, validan Consultar y
Guardar, integran el footer y descargan CSV con valores numéricos. La carga comprueba que
estén disponibles las APIs de esta revisión. Sin cambios en Compartido o aplicaciones CRM.

## Validación reproducible

Desde Libreria-Racafe, con dependencias instaladas:

```sh
Rscript -e 'devtools::test("racafeForms")'
R CMD build --no-build-vignettes racafeForms
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual racafeForms_0.5.0.tar.gz
```

Inicie la demo en un puerto conocido y ejecute Playwright:

```sh
Rscript -e 'shiny::runApp("racafeForms/inst/examples/standardized-forms.R", port=3990)'
RACAFE_TEST_URL=http://127.0.0.1:3990 node racafeForms/tests/browser/test-standardized-forms.cjs
```

Requiere Playwright y Chromium; admite `NODE_PATH`, `CHROMIUM_PATH` y `CI=1`.
Repita con `RACAFE_STANDARD_BS4DASH=0` y con `RACAFE_SHARED_PATH` para las variantes.
En PlantillaBs4Dash: `Rscript tests/test-templates.R` y su prueba de navegador existente.

Resultados de esta revisión:

| Comprobación | Resultado |
| --- | --- |
| testthat completo de racafeForms | 557 verificaciones; 0 fallos, avisos u omisiones |
| R CMD check (sin manual PDF) | OK |
| Demo nueva: bs4Dash / Shiny / bs4Dash con Compartido | 43 / 41 / 43 comprobaciones de navegador |
| Alertas both y zonas de navegador UTC/Los Ángeles | 11 comprobaciones |
| Demo corporativa anterior | 74 comprobaciones de navegador |
| PlantillaBs4Dash: pruebas R / navegador | 56 / 60 comprobaciones |

Para reproducir las alertas y las zonas horarias, inicie otra instancia con
`RACAFE_STANDARD_MODE=both` y ejecute `tests/browser/test-validation-modes.cjs` con
`RACAFE_TEST_URL` apuntando a esa instancia. Estas pruebas usan Chromium; no sustituyen
una comprobación en otros navegadores ni prueban permisos o persistencia de aplicaciones
consumidoras. No se hicieron cambios en servicios externos.

Las comprobaciones cubren validación diferida, reglas condicionales, auto-limpieza, foco, namespaces, controles
dinámicos con y sin ID de formulario, estados, descarga real, DateTime y tamaño móvil.
En el entorno de pruebas se restauraron los assets oficiales de datepicker de Shiny que
habían sido sustituidos por enlaces del empaquetado del sistema; no se cambió código de
proveedores ni se distribuye ese ajuste dentro de racafeForms.
