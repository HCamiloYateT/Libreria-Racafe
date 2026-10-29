# racafeShiny

Componentes de presentación para aplicaciones Shiny: formatos numéricos, estilos
de tablas `gt`, inputs, botones, sidebars, modales y un módulo de KPI. Depende de
`racafeCore` y `racafeGraph`.

## Instalación

Requiere **R >= 4.1.0**. Instale `remotes` una sola vez si no está disponible:

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
```

Instale primero las dependencias internas, en el orden del ejemplo.

### Desde GitHub

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeGraph", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeShiny", dependencies = NA, upgrade = "never")
```

### Desde un clon local

Ejecute desde la raíz de `Libreria-Racafe`, donde están las carpetas de los paquetes:

```r
remotes::install_local("racafeCore", dependencies = NA, upgrade = "never")
remotes::install_local("racafeGraph", dependencies = NA, upgrade = "never")
remotes::install_local("racafeShiny", dependencies = NA, upgrade = "never")
```

`dependencies = NA` instala las dependencias obligatorias; `upgrade = "never"`
evita actualizar otros paquetes ya instalados. Para herramientas de desarrollo,
pruebas y acceso a repositorios privados, consulte la [guía general](../README.md#instalación).

Compruebe la instalación:

```r
library(racafeShiny)
packageVersion("racafeShiny")
```

## Formatos y texto

```r
library(racafeShiny)

FormatearNumero(1250000, "dinero")
FormatearNumero(0.85, "porcentaje", meta = 0.80)
FormatearTexto("Meta alcanzada", color = "#28B78D", negrita = TRUE)

FormatoD3("dinero")
FormatoJS("porcentaje")
FormatoHOT("numero")

col_kpi(c(0.70, 0.92, 1.05))
chr_kpi(c(0.70, 0.92, 1.05))
col_num(1:5)
```

`DefinirFormato()` y `ObtenerFormato()` administran el registro de formatos del
paquete. `FormatrearTexto()` se mantiene como alias de compatibilidad para el
nombre correcto `FormatearTexto()`.

## Tablas `gt`

```r
ventas <- data.frame(
  region = c("Norte", "Sur"),
  cumplimiento = c(0.92, 1.04),
  variacion = c(-0.03, 0.08)
)

ventas |>
  gt::gt() |>
  gt_minimal_style() |>
  gt_pct_style(cumplimiento) |>
  gt_var_style(variacion) |>
  gt_sign_style(variacion)

gt_mensaje_vacio("Sin resultados para los filtros aplicados")
```

`gt_color_columns()` aplica un color fijo a columnas seleccionadas. Los helpers
devuelven objetos `gt_tbl`, por lo que se pueden encadenar con funciones de `gt`.

## Inputs y botones

```r
# Dentro de una UI de Shiny:
InputNumerico("meta", "Meta mensual", value = 1e6, dec = 0)

ListaDesplegable(
  "region", "Región",
  choices = c("Norte", "Sur", "Centro"),
  multiple = TRUE
)

BotonesRadiales(
  "periodo", "Periodo",
  choices = c("Mensual", "Trimestral", "Anual")
)

InputFecha("fecha", "Fecha")
InputMes("mes", "Mes")
BotonEstado("activar", "Activar")
Boton("guardar", label = "Guardar", icono = "floppy-disk")
BotonDescarga(
  "exportar", label = "Exportar", icono = "file-excel", title = "Exportar"
)
```

## Cajas de valor y módulo KPI

`CajaValor()` crea un componente estático. Para valores reactivos use el par
`cajaValor_ui()` / `cajaValor_server()`:

```r
ui <- shiny::fluidPage(
  cajaValor_ui("kpi_ventas", icono = "dollar-sign", texto = "Ventas")
)

server <- function(input, output, session) {
  ventas <- shiny::reactive(1250000)

  modulo <- cajaValor_server(
    id = "kpi_ventas",
    valor_r = ventas,
    formato = "dinero",
    meta = 1000000
  )

  shiny::observeEvent(modulo$click_detalle(), {
    mostrarModal(
      "Contenido del detalle",
      titulo = "Detalle de ventas",
      tamano = "subventana3"
    )
  })
}

shiny::shinyApp(ui, server)
```

## Otras utilidades

- `SidebarItemWrap()` adapta contenido para una barra lateral.
- `CajaIco()` crea una caja con icono.
- `DefinirColumnaHtml()`, `FormatearFila()` y `ObtenerReglaFila()` ayudan a
  construir tablas con HTML y reglas por fila.
- `mostrarModal()` acepta los tamaños corporativos `ventana`, `subventana1` a
  `subventana5` y `aviso`; las clases visuales deben existir en el CSS de la app.

Consulte `help(package = "racafeShiny")` y la ayuda de cada función para revisar
todos los argumentos disponibles.

## Ayuda en RStudio

```r
help(package = "racafeShiny")
help("racafeShiny-package", package = "racafeShiny")
```

El índice permite abrir la ayuda de cada función, con su sintaxis, argumentos y
valor de retorno. También puede usar `?racafeShiny::nombre_de_funcion`, sustituyendo
`nombre_de_funcion` por el nombre real, o pulsar **F1** sobre una función en RStudio.
Si actualiza desde una versión sin ayuda, reinstale el paquete y reinicie R.
Consulte la [guía general de ayuda](../README.md#ayuda-de-las-funciones-en-rstudio).

## Separadores numéricos

Los formatos numéricos predeterminados usan punto decimal y coma de miles: **1,234.56**.
Los valores reactivos siguen siendo numéricos; no convierta el texto formateado con
`as.numeric()` sin retirar antes los separadores de miles. Las fechas conservan su formato.

## Loaders corporativos (1.1.0)

Los tres loaders de `PlantillaBs4Dash` están disponibles como componentes del paquete:
`inicio` («Cargando…»), `actualizar` («Actualizando…») y `calculando` («Calculando…»).
Utilizan **waiter**, recursos CSS/JS locales y los colores corporativos. No necesitan
Compartido, Bootstrap ni logotipos remotos; `logo` permite indicar una imagen propia.
`waiter` se instala como dependencia obligatoria mediante las instrucciones anteriores.

### Mostrar antes de que R quede ocupado

```r
ui <- shiny::fluidPage(
  racafeShiny::racafeLoaderUI(
    triggers = c(actualizar = "recargar", calculando = "procesar")
  ),
  shiny::actionButton("recargar", "Actualizar"),
  shiny::actionButton("procesar", "Procesar")
)
```

Incluya `racafeLoaderUI()` una vez en el body. `startup = TRUE` cubre la carga inicial;
`busy = TRUE` responde a los eventos busy/idle de Shiny. Los `triggers` muestran el
loader **en el navegador al hacer clic**, antes de que la acción llegue al servidor.
No sustituyen el binding del botón. Use IDs con namespace o `ns = ns` para IDs locales;
el namespace se aplica una sola vez. Un vector sin nombres usa `calculando`.

El loader automático cierra en idle y al desconectarse. Sus recursos solo afectan a
clases `rsh-loader-*`; no retira los waiters explícitos de otros paneles. Respeta la
preferencia de movimiento reducido. Registre botones que efectivamente envíen acciones
al servidor; un botón puramente local no genera el ciclo busy/idle necesario para cerrar.

**Limitación de Shiny:** llamar a `$show()` desde R e iniciar inmediatamente una tarea
síncrona puede retrasar el envío hasta que termine. Por eso se ofrece la activación por
clic. Ningún loader vuelve asíncrona una consulta o cálculo: la animación puede continuar
en el navegador mientras R trabaja, pero la sesión no atenderá otras tareas y no podrá
recibir actualizaciones intermedias del servidor durante ese bloqueo. Para trabajo
asíncrono use mecanismos como `ExtendedTask` y controle el cierre explícitamente.

### Objetos waiter por sesión o panel

```r
shiny::moduleServer("modulo", function(input, output, session) {
  # La UI del módulo debe contener un div con id = ns("panel").
  loader <- racafeShiny::racafeLoader(session, "calculando", id = "panel")
  shiny::observeEvent(input$mostrar, loader$show())
  shiny::observeEvent(input$mensaje, {
    loader$update(racafeShiny::racafePreloader("actualizar", "Procesando datos…")$html)
  })
  shiny::observeEvent(input$ocultar, loader$hide())
})
```

Sin `id`, el objeto cubre la pantalla. El objeto encapsula `waiter::Waiter` y fija la
sesión al ejecutar cada método; no lo almacene globalmente ni lo comparta entre usuarios.
El cierre del objeto es explícito. Proteja su limpieza al terminar y ante errores:

```r
procesar <- function(loader) {
  loader$show()
  on.exit(loader$hide(), add = TRUE)
  # Operación y manejo de errores propios de la aplicación.
}
```

Este patrón garantiza limpieza; no garantiza que `$show()` se pinte antes de un cálculo
síncrono. En una promesa cierre en las ramas de resolución y rechazo, sin `on.exit()`
en la función que únicamente agenda el trabajo. Desactive `busy` si solo necesita el
objeto persistente y no quiere un segundo loader automático.

### Preloader nativo de bs4Dash

```r
# Argumento de bs4Dash::dashboardPage():
preloader <- racafeShiny::racafePreloader("inicio", text = "Preparando aplicación…")
```

La función devuelve la lista `html`/`color` que espera `preloader`. Si utiliza ese
preloader inicial, configure `racafeLoaderUI(startup = FALSE, ...)` para evitar duplicarlo.
Los textos se escapan; `loader$update()` recibe HTML de confianza, como el generado por
`racafePreloader()`. La plantilla original se mantiene sin cambios: la migración consiste
en reemplazar sus objetos locales y añadir la configuración UI de la librería.

### Demo y validación

```r
shiny::runApp(system.file("examples", "loaders.R", package = "racafeShiny"))
# Para la misma demo en bs4Dash, antes de ejecutarla:
Sys.setenv(RACAFE_LOADER_BS4DASH = "1")
```

Incluye dos módulos, carga inicial lenta, tareas que bloquean R intencionalmente,
error controlado y objetos locales con actualización/cierre. No ejecute `Sys.sleep()`
en producción; aquí permite comprobar el comportamiento durante un bloqueo real.
Consulte el [informe de validación](docs/loaders-1.1.0.md).
