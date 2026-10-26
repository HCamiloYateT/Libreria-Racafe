# racafeForms

Paquete corporativo de formularios Shiny independiente de `racafeShiny`.

## Instalación

Requiere **R >= 4.1.0**. Instale `remotes` una sola vez si no está disponible:

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
```

No requiere instalar otros paquetes Racafe.

### Desde GitHub

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeForms", dependencies = NA, upgrade = "never")
```

### Desde un clon local

Ejecute desde la raíz de `Libreria-Racafe`, donde están las carpetas de los paquetes:

```r
remotes::install_local("racafeForms", dependencies = NA, upgrade = "never")
```

`dependencies = NA` instala las dependencias obligatorias; `upgrade = "never"`
evita actualizar otros paquetes ya instalados. Para herramientas de desarrollo,
pruebas y acceso a repositorios privados, consulte la [guía general](../README.md#instalación).

Compruebe la instalación:

```r
library(racafeForms)
packageVersion("racafeForms")
```

Para ejecutar el catálogo de controles incluido:

```r
shiny::runApp(system.file("examples", "demo.R", package = "racafeForms"))
```

`racafeForms` y `racafeShiny` se instalan de forma independiente. Incorporar
formularios nuevos no requiere desinstalar ni migrar los componentes existentes.

## Uso

```r
library(shiny)
library(racafeForms)

ui <- fluidPage(
  racafeForm(
    racafeFormRow(
      racafeInputSelect("asesor", "Asesor", c("Laura", "Pedro")),
      racafeInputNumeric("presupuesto", "Presupuesto", 1500000, format = "cop")
    ),
    racafeFormRow(
      racafeInputDate("fecha", "Fecha"),
      racafeInputDateRange("periodo", "Periodo")
    ),
    racafeInputSwitch("activo", "Activo"),
    racafeFormFooter(
      racafeButtonSave("guardar"),
      help = "Las modificaciones quedan registradas."
    )
  )
)
server <- function(input, output, session) {
  observeEvent(input$guardar, {
    racafeConfirm(session, "confirmar", "Guardar cambios", "¿Confirma la operacion?")
  })
  observeEvent(input$confirmar, {
    if (isTRUE(input$confirmar)) racafeAlert(session, "Guardado", "Confirmacion recibida", "success")
  })
}
shinyApp(ui, server)
```

## Migracion

- `racafeShiny::ListaDesplegable()` pasa conceptualmente a `racafeInputSelect()`.
  En la nueva API `multiple = FALSE` y `empty = TRUE` por defecto.
- `racafeShiny::InputDinero()`, `InputNumero()`, `InputPorcentaje()` y
  `InputNumerico()` se unifican en `racafeInputNumeric(format = ...)`.
- `racafeShiny::Boton()` migra a `racafeButton()`; los botones de descarga
  usan `racafeButtonDownload()` y `downloadHandler`.
- `racafeShiny::InputFecha()` migra a `racafeInputDate()`; rangos con
  `racafeInputDateRange()`.
- Mantener las funciones originales de racafeShiny hasta migrar aplicaciones;
  no crear una dependencia inversa de racafeForms hacia racafeShiny.

## Notas

- En modulos: construir los controles con `ns = ns` y actualizar los IDs
  locales con `session`, sin aplicar `ns` de nuevo.
- `racafeConfirm()` requiere `ns = session$ns` dentro de un modulo y emite un
  booleano en `input$id` local.
- Validar permisos, valores y operaciones de base de datos en el servidor.
- El CSS se adjunta con `racafeForm()` o manualmente con `racafeFormsDependencies()`.
- No se incluyen credenciales ni accesos a datos del CRM.
