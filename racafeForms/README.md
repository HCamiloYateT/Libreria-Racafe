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

## Ayuda en RStudio

```r
help(package = "racafeForms")
help("racafeForms-package", package = "racafeForms")
```

El índice permite abrir la ayuda de cada función, con su sintaxis, argumentos y
valor de retorno. También puede usar `?racafeForms::nombre_de_funcion`, sustituyendo
`nombre_de_funcion` por el nombre real, o pulsar **F1** sobre una función en RStudio.
Si actualiza desde una versión sin ayuda, reinstale el paquete y reinicie R.
Consulte la [guía general de ayuda](../README.md#ayuda-de-las-funciones-en-rstudio).

## Selectores múltiples y rangos de fechas

`racafeInputSelect()` acepta `gender = "m"` (predeterminado) o `gender = "f"`.
Una selección muestra su etiqueta, varias muestran el conteo, todas muestran
`Todos`/`Todas` y ninguna muestra `Ninguno`/`Ninguna`. La búsqueda no altera el
conteo total y los valores que recibe Shiny permanecen intactos.

```r
racafeForms::racafeInputSelect("asesor", "Asesor", c(Ana = "a", Luis = "b", Marta = "c"), multiple = TRUE)
racafeForms::racafeInputSelect("categoria", "Categoría", c(Uno = "1", Dos = "2"), multiple = TRUE, gender = "f")
```

En un módulo, pase `ns = ns` al construir los controles y use el ID local en
`racafeUpdateSelect(session, "categoria", choices = nuevas_opciones)`. Al omitir
`selected`, se conservan las selecciones válidas de la sesión. Para vaciar una
selección múltiple, pase `selected = character(0)`; para seleccionar todas,
pase explícitamente sus valores. También se admite `shinyWidgets::updatePickerInput()`.

`racafeInputDateRange()` mantiene el mismo binding y devuelve un vector `Date`
de longitud dos. El separador visual pasa a `-`; el calendario conserva el idioma
español, los límites `min`/`max` y el formato `dd/mm/yyyy`. Sus estilos se aplican
solo al calendario asociado al control. `racafeUpdateDateRange()` conserva la
compatibilidad con `shiny::updateDateRangeInput()`.

Las dependencias visuales se adjuntan automáticamente a selectores y rangos,
incluso fuera de `racafeForm()`. Dentro del formulario se respetan las densidades
`compact`, `normal` y `comfortable`. Para CRM Nacional basta usar `gender = "f"`
en `FT_LinNeg` y `FT_Categoria`; no hace falta cambiar el ETL ni los valores filtrados.

### Cambios respecto a 0.1.3

- Las selecciones múltiples muestran un resumen en lugar de concatenar etiquetas.
- `gender` se agrega al final de la firma, conservando los argumentos posicionales anteriores.
- `empty_label` explícito sigue teniendo prioridad sobre la etiqueta predeterminada.
- Actualizar `choices` con `racafeUpdateSelect()` sin `selected` conserva la selección
  válida actual; las aplicaciones que quieran restablecerla deben indicar `selected`.
- Si una aplicación reemplaza el CSS del paquete, revise sus reglas de
  `.filter-option` y `.input-daterange` para evitar estilos contradictorios.
