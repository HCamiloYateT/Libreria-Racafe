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

### Selección visual y filtros sin selección

La selección única conserva el acento rojo. En selección múltiple las filas mantienen
el fondo neutro y un check rojo identifica cada opción seleccionada, sin cambiar los valores enviados a Shiny.

Para un filtro opcional, puede interpretar una selección vacía como **sin filtro**:
se incluyen todos los registros y también los que aparezcan al actualizar los datos.
Use una etiqueta explícita y aplique esa regla en el servidor:

```r
# UI (también dentro de un módulo con ns = ns).
racafeForms::racafeInputSelect("asesor", "Asesor", choices = asesores, multiple = TRUE,
                              empty_label = "Todos (sin filtro)", help = "Sin selección se incluyen todos.")

# Servidor: datos() devuelve el conjunto disponible antes de aplicar este filtro.
datos_filtrados <- shiny::reactive({
  seleccion <- input$asesor
  tabla <- datos()
  if (length(seleccion) == 0L) return(tabla)
  tabla[tabla$asesor %in% seleccion, , drop = FALSE]
})
```

Esta política es opcional: `input$asesor` continúa siendo `NULL` o `character(0)`
al vaciarlo. Cambiar solo `empty_label` no cambia el filtrado. Evite `req(input$asesor)`
en este caso, pues impediría mostrar todos. En un filtro obligatorio o donde vacío
signifique «ningún registro», conserve la interpretación habitual.
El botón «Ninguno» desmarca todo y, con esta política, quita el filtro; seleccionar
«Todos» marca únicamente las opciones disponibles en ese momento. Si los datos
contienen asesores `NA` o ausentes del catálogo, «sin filtro» también los incluye;
seleccionar todas las opciones del catálogo puede excluirlos.

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

## Calendarios con apariencia unificada

Los calendarios de `racafeInputDate()` y `racafeInputDateRange()` comparten colores,
fuente, borde, sombra, navegación y dimensiones. La vista diaria empieza en lunes
con seis semanas completas. Las medidas se adaptan a `compact`, `normal` y
`comfortable`, y los calendarios conservan el español y las restricciones de fechas.

Se mantienen sus componentes internos: Air Datepicker en fecha y Bootstrap
Datepicker en rango. Esto conserva `racafeUpdateDate()`, `racafeUpdateDateRange()`,
los IDs y los valores que recibe Shiny. La estructura HTML interna es diferente;
la distribución y el estilo visibles son equivalentes. Los controles externos al
paquete no reciben estos estilos. El campo de fecha sigue sin botón adicional.

## Componentes corporativos con Compartido (0.4.0)

Los componentes nuevos usan los estilos y scripts del repositorio **Compartido**,
no una copia dentro del paquete. Mantenga una copia local controlada, por ejemplo
`vendor/Compartido`, fijada a un commit revisado. La carga no realiza peticiones a GitHub:

```r
assets <- racafeForms::racafeSharedAssets("vendor/Compartido", version = "1.1.0")
ui <- shiny::fluidPage(
  assets,
  racafeForms::racafePanel("Resumen",
    racafeForms::racafeMetricGrid(
      racafeForms::racafeMetric(12, "Creadas"),
      racafeForms::racafeMetric(8, "Completadas"), columns = 2),
    racafeForms::racafeInfoGrid(
      racafeForms::racafeInfoItem("Referencia", "REF-001"),
      racafeForms::racafeInfoItem("Estado", "Disponible")),
    racafeForms::racafeInputReadOnly("referencia", "Referencia", "REF-001", copyable = TRUE))
)
```

También puede configurar `RACAFE_SHARED_PATH` o `options(racafeForms.shared_path = ...)`
y llamar `racafeSharedAssets(version = "1.1.0")`. Solo se publican Styles, Scripts e img;
no se expone la raíz ni `.git`. El cargador rechaza versiones declaradas distintas de
la solicitada y prefijos que ya apuntan a otra copia. Los recursos repetidos con la
misma configuración se deduplican mediante las dependencias HTML de Shiny.

Los parámetros `css`, `javascript` e `images` activan cada grupo de recursos de
**Compartido**; la dependencia base anterior `racafeFormsDependencies()` se conserva.
No incluya además los mismos archivos mediante tags link o script manuales.
Las imágenes se construyen con `paste0(attr(assets, "image_url"), "/logo.png")`.

| Función | Uso |
|---|---|
| `racafePanel()` | Contenedor con título, variante, padding, borde y footer opcionales; sin altura mínima por defecto. |
| `racafeCard()` | Tarjeta informativa; clic y teclado únicamente con clickable = TRUE e ID explícito. |
| `racafeMetric()` / `racafeMetricGrid()` | Indicadores y cuadrícula adaptable de una a cuatro columnas. |
| `racafeInfoItem()` / `racafeInfoGrid()` | Datos de etiqueta-valor, con sustituto para vacíos. |
| `racafeActionBar()` | Grupos de botones a izquierda, centro y derecha, con wrap configurable. |
| `racafeModal()` / `racafeModalFooter()` | Modales nativos con los siete tamaños existentes de Compartido. |
| `racafeCopyButton()` | Copia directa de texto, valor de otro control o PNG. |
| `racafeCopyText()` / `racafeCopyImage()` | Solicitudes de copia desde R, con resultado asíncrono real. |
| `racafeInputReadOnly()` | Referencias seleccionables y enfocables, con copia opcional. |
| `racafeResetForm()` | Restaurar o limpiar campos editables de un formulario con ID. |

### Composición, estados y catálogos

`racafeForm(unstyled = TRUE)` evita el borde/fondo redundante al colocarlo en un panel.
`racafeFormFooter()` conserva sus argumentos anteriores y admite `left`, `center` y
`right` mediante `racafeActionBar()`. Los botones de descarga siguen conectándose
al mismo `output$id <- shiny::downloadHandler(...)`, sin enlaces ocultos ni clics simulados.

Texto, textarea y numéricos admiten readonly; los selectores y fechas admiten
`disabled`, `required`, `help` y `error`. Los mensajes de error se asocian con
`aria-describedby` y `aria-invalid`. `racafeInputText(choices = ...)` ofrece sugerencias
con un datalist local. Para etiquetas y valores distintos o catálogos reactivos, use
`racafeInputSelect()` con búsqueda y `racafeUpdateSelect()`; no cambia su contrato.
Estas señales son de interfaz: los permisos y la validación real siguen en el servidor.

`racafeFormMessage()` y `racafeAlert()` añaden loading y empty a los estados anteriores.
Los detalles técnicos solo aparecen cuando la aplicación proporciona explícitamente
`details`, siempre como texto escapado; no se muestran excepciones automáticamente.
No pase secretos ni datos que no deban quedar disponibles en el navegador.

### Módulos, portapapeles y restablecimiento

Construya los controles con IDs locales y `ns = ns`. En servidor, pase la sesión del
módulo y el ID local; no aplique el namespace dos veces:

```r
server_modulo <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$copiar, {
      racafeForms::racafeCopyText(session, "resultado", "Texto autorizado por la aplicación")
    })
    shiny::observeEvent(input$resultado_copy_result, {
      if (isTRUE(input$resultado_copy_result$success)) {
        shiny::showNotification("Copia confirmada por el navegador")
      }
    })
    shiny::observeEvent(input$restablecer, racafeForms::racafeResetForm(session, "formulario"))
  })
}
```

Un botón `racafeCopyButton("copiar", ...)` informa en `input$copiar_copy_result`.
El campo readonly `id = "referencia"` con copia opcional informa en
`input$referencia_copy_copy_result`. El resultado incluye success, type, error y
request_id, sin el contenido copiado. La copia requiere HTTPS o localhost y puede
ser denegada por el navegador. PNG requiere ClipboardItem; URL externas requieren
CORS. Se informa el éxito después de completar la operación, nunca antes.

El reset necesita `racafeForm(id = "formulario", ns = ns)`. FALSE restaura la
instantánea inicial de cada control; `clear = TRUE` limpia sus valores editables.
No toca campos readonly, archivos ni botones. El resultado en
`input$formulario_reset_result` informa success, count, failures y request_id.
La UI dinámica conserva su propia instantánea al enlazarse con Shiny.

### Demostración y pruebas

```r
options(racafeForms.shared_path = "vendor/Compartido")
shiny::runApp(system.file("examples", "corporate-components.R", package = "racafeForms"))
```

Para la misma demo en bs4Dash, instale ese paquete opcional y defina
`RACAFE_DEMO_BS4DASH=1` antes de ejecutarla. El paquete no añade una dependencia
obligatoria a bs4Dash. La demo suprime los CDN opcionales ionicons/googlefonts de
bs4Dash y utiliza iconos locales y fuentes de sustitución para funcionar sin conexión externa. La demostración incluye dos módulos simultáneos, variantes,
cuadrículas de 1 a 4 columnas, los siete modales, UI dinámica, copia real, descargas y reset.

Desde la raíz del monorepo:

```r
testthat::test_local("racafeForms", stop_on_failure = TRUE)
```

Las pruebas de navegador están en `tests/browser`. Allí ejecute `npm ci` y
`npx playwright install chromium` una vez; arranque la demo y ejecute `npm test`.
`RACAFE_TEST_URL` define la dirección local de la demo; `CHROMIUM_PATH` permite
usar Chromium ya instalado. Playwright es solo una dependencia de desarrollo.
El runner bloquea peticiones externas y comprueba copia real, fallos de permisos,
modales, descargas, módulos, móvil y recursos deduplicados.

### Compatibilidad y migración

| API | Recursos | Resultado |
|---|---|---|
| Funciones anteriores de racafeForms 0.2.1 | Solo dependencia base | Conservan sus argumentos y comportamiento. |
| Funciones anteriores en 0.3.0 + Compartido 1.0.0 | Adopción explícita | Usan la identidad de Compartido. |
| Componentes nuevos de 0.3.0 | Compartido 1.0.0 | Estilos e interacciones nuevos centralizados. |
| Copia en navegador sin soporte o permisos | Compartido 1.0.0 | Resultado de fallo; no éxito simulado. |

No se añadieron dependencias R ni reglas de negocio de Firma Digital. Se conserva
la independencia respecto a racafeShiny. Consulte la [matriz de componentes](docs/component-matrix.md)
y la [migración representativa](docs/migration/README.md). El ejemplo representativo
pasa de 39 a 13 líneas de UI no vacías y sin comentarios (66,7 % menos); no es una
medición de los módulos reales de Firma Digital, que no se han recibido ni modificado.

## Tipos email y tel: binding reactivo (0.4.2)

`racafeInputText(type = "email")` y `type = "tel"` generan sus tipos HTML literales.
Se elimina primero el atributo `type` anterior con `removeAttrs("type")` y después
se asigna el nuevo, evitando concatenar `text email` o `text tel`. Conservan
`inputmode` y `autocomplete` adecuados; un autocomplete explícito tiene precedencia.

Cuando Shiny reconoce el tipo, usa su binding nativo. Cuando no lo reconoce (como tel
en Shiny 1.10.0), la dependencia base registra un adaptador que hereda los métodos de
texto y busca solo campos email/tel propios no cubiertos por el selector nativo.
No modifica ese selector ni captura campos de otros paquetes. Funciona sin Compartido.
Los cuatro tipos se actualizan con `shiny::updateTextInput()` usando el ID local del módulo.

Email conserva la validación HTML de formato del navegador; tel no impone un formato
telefónico. Valide siempre formato y permisos en el servidor. `required`, `readonly`,
`disabled`, `help` y `error` mantienen su comportamiento. No se modifican consumidores.

Prueba ejecutable con dos módulos, cuatro tipos y estados de campo (solo datos ficticios):

```r
shiny::runApp(system.file("examples", "text-input-bindings.R", package = "racafeForms"))
# TEXT_BINDINGS_BS4DASH=1 activa el modo bs4Dash antes de iniciar la app.
```

La demo muestra la contraseña ficticia para verificar el valor del servidor; no utilice
credenciales reales ni copie esa salida a una aplicación. La prueba automatizada está en
`tests/browser/test-text-input-bindings.cjs`. Vea el [informe de la corrección](docs/text-input-bindings.md).

## Componentes visuales 0.4.0

Use `racafeSharedAssets(version = "1.1.0")` una vez en la UI. La identidad visual,
las densidades y los badges viven en Compartido; las funciones R construyen la interfaz.
Los estilos base incluidos en el paquete se conservan para compatibilidad de controles
anteriores. No copie estilos, paletas ni scripts a cada módulo.

`racafePanel()` y `racafeCard()` admiten encabezado adicional (`header`), cuerpo y `footer`.
Las tarjetas e indicadores añaden `padding = "compact"`, `"normal"` o `"comfortable"`;
el valor normal conserva el espaciado anterior. Las tarjetas no se estiran para alcanzar
la altura de su vecina. `racafeMetric(value, label, detail)` usa label como título y detail
como nota; el icono sigue siendo opcional. `racafeMetricGrid(columns = 2/3/4)` se adapta
al móvil sin CSS en la aplicación.

```r
assets <- racafeForms::racafeSharedAssets("vendor/Compartido", version = "1.1.0")
shiny::fluidPage(assets,
  racafeForms::racafeMetricGrid(columns = 3,
    racafeForms::racafeMetric(42, "Total", "Actualizado hoy", padding = "compact"),
    racafeForms::racafeMetric(8, "Pendientes", icon = "clock", padding = "compact"),
    racafeForms::racafeCard("Información", "Contenido de altura variable",
      header = racafeForms::racafeStatusBadge("Disponible", "success"), padding = "compact")),
  racafeForms::racafeActionBar(
    left = racafeForms::racafeButton("volver", "Volver"),
    right = racafeForms::racafeButtonDownload("exportar", icon = NULL)))
```

`racafeStatusBadge(value, variant = "neutral", size = "sm")` devuelve un span de texto
escapado. Admite `success`, `warning`, `danger`, `info`, `neutral`, y tamaños `xs`, `sm`, `md`.
Es informativo: no registra clics ni convierte cada celda en una región viva. Mantenga
texto descriptivo para que el significado no dependa solo del color. La paleta corporativa
predeterminada tiene contraste de texto >= 4,5:1; vuelva a comprobarlo si personaliza colores.

```r
reactable::reactable(data.frame(estado = c("Disponible", "Pendiente")),
  columns = list(estado = reactable::colDef(cell = function(value) {
    racafeForms::racafeStatusBadge(value, if (value == "Disponible") "success" else "warning")
  })))
```

La selección de variante corresponde al consumidor, no a reglas de Firma Digital dentro
del badge. `reactable` y `bs4Dash` son dependencias sugeridas para ejemplos/pruebas; no
son necesarias para construir un badge. `TablaReactable2` permanece en `racafeModulos`.

Los campos de texto, listas y rangos conservan los bindings y sus valores Shiny. Con
Compartido 1.1.0 comparten altura, radio, borde, fuente y padding, incluso fuera de
`racafeForm()`. Las tres densidades siguen disponibles dentro del formulario.
`racafeButtonDownload(icon = NULL)` omite el icono y conserva `downloadHandler()`;
un icono explícito se muestra una sola vez. Las barras de acciones distribuyen los grupos
izquierdo/central/derecho y permiten envolver botones en móvil con `wrap = TRUE`.

La demo existente se amplía, en lugar de mantener otra implementación de los componentes:

```r
options(racafeForms.shared_path = "/ruta/Compartido")
shiny::runApp(system.file("examples", "corporate-components.R", package = "racafeForms"))
# Para bs4Dash, establezca RACAFE_DEMO_BS4DASH=1 antes de iniciarla.
```

Instale `reactable` para esta demo. Incluye filtros y tabla con badges, dos módulos con
namespace, descarga nativa con/sin icono, siete tamaños de modal, copia de texto e imágenes
y estados de formulario. No requiere CSS, HTML ni handlers JS propios para esos controles.
No se han modificado las aplicaciones. Vea [validación y compatibilidad 0.4.0](docs/visual-components-0.4.0.md).
