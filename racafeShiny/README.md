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

## Loaders corporativos (1.2.0)

Los loaders usan `waiter`, recursos locales y el logo corporativo `logo2.png` incluido
en el paquete. Funcionan sin Compartido, CDN ni acceso a internet. El logo aparece
encima del mensaje, conserva sus proporciones y cabe en móvil. Todas las operaciones
comparten tarjeta blanca, texto oscuro y animación roja; se respeta movimiento reducido.

| `type` | Mensaje predeterminado |
| --- | --- |
| inicio | Cargando aplicación… |
| actualizar | Actualizando información… |
| consultar | Consultando información… |
| calculando | Procesando cálculos… |
| guardar | Guardando información… |
| enviar | Enviando información… |
| generar | Generando documento… |
| descargar | Preparando descarga… |
| validar | Validando información… |
| importar | Importando información… |
| eliminar | Eliminando registro… |
| sincronizar | Sincronizando información… |

### API pública

```r
racafePreloader(type = "inicio", text = NULL, logo = NULL, show_logo = TRUE)
racafeLoaderUI(startup = TRUE, busy = TRUE, triggers = NULL, ns = NULL,
               logo = NULL, show_logo = TRUE)
racafeLoader(session = shiny::getDefaultReactiveDomain(), type = "inicio", text = NULL,
             logo = NULL, id = NULL, ns = NULL, show_logo = TRUE)
```

`text` permite personalizar el mensaje; se muestra como texto escapado. `logo = NULL`
ahora utiliza el logo corporativo local. Para cambiarlo indique una ruta o URL; si la
imagen falla, se intenta el logo local y la operación sigue funcionando. Para ocultarlo
use `show_logo = FALSE`. Los argumentos anteriores mantienen su posición; los tipos
`inicio`, `actualizar` y `calculando` conservan su funcionamiento y actualizan sus textos.
`waiter` sigue siendo la única dependencia del motor de loaders.

### Activación inmediata y módulos

```r
# Dentro del body de Shiny o bs4Dash:
racafeShiny::racafeLoaderUI(
  startup = FALSE, busy = TRUE,
  triggers = c(consultar = "consultar_firmas-actualizar",
               guardar = "crear_firma-crear_confirmado",
               enviar = "crear_firma-envio_confirmado",
               validar = "crear_masiva-revisar",
               enviar = "crear_masiva-confirmar_envio")
)
```

Los nombres repetidos de `triggers` son válidos. Puede usar IDs completos como arriba,
o IDs locales con `ns = ns`; no se duplica el namespace. Los vectores sin nombres
usan `calculando`. Incluya la configuración una vez en el body; los recursos se deduplican.
Los botones deben producir una acción en el servidor para que haya un ciclo busy/idle.

El clic activa el loader **en el navegador antes de enviar la acción**. `startup` cubre
la carga inicial y `busy` detecta actividad de Shiny. El automático cierra en idle,
error controlado y desconexión, y cede si ya existe un waiter. No cierra modales ni
notificaciones; al terminar desaparece y deja visibles los avisos de la operación.
Los loaders corporativos explícitos se limpian también al desconectar.

### Objetos por sesión y contenedor

```r
loader <- racafeShiny::racafeLoader(session, type = "consultar", id = "tabla_solicitudes")
loader$show()
loader$update(racafeShiny::racafePreloader("consultar", "Consultando solicitudes…")$html)
loader$hide()
```

El ID debe corresponder a un contenedor HTML existente. Dentro de un módulo se aplica
`session$ns` una sola vez; sin `id` se cubre la pantalla. Los objetos encapsulan
`waiter::Waiter`, fijan la sesión por método y ofrecen `show()`, `update(html)` y `hide()`.
No los comparta globalmente entre usuarios. `update()` recibe HTML de confianza;
use `racafePreloader()` para generar mensajes escapados.

Proteja el cierre explícito con `on.exit(loader$hide(), add = TRUE)` dentro de la función
que ejecuta la operación, o con `tryCatch(..., finally = loader$hide())`. Para promesas
cierre tanto al resolver como al rechazar; no use `on.exit()` en una función que solo
agenda el trabajo. `busy = FALSE` permite trabajar únicamente con objetos explícitos.

**Límite de Shiny:** `$show()` seguido inmediatamente de un cálculo síncrono puede no
llegar al navegador a tiempo; use `triggers` para ese caso. La animación no convierte
la operación en asíncrona ni permite enviar avances mientras R está bloqueado.
No se implementa `setProgress()` en esta versión: no se simulan porcentajes por registro.
Para progreso real el trabajo debe proporcionar avances y devolver el control al bucle
de eventos; el loader actual permite actualizar mensajes en esas oportunidades.

### Integración con el preloader de bs4Dash

```r
# En bs4Dash::dashboardPage():
preloader <- racafeShiny::racafePreloader("inicio", "Cargando autorización de datos…")
# En dashboardBody(), para el resto de operaciones:
recursos <- racafeShiny::racafeLoaderUI(startup = FALSE, triggers = c(guardar = "guardar"))
```

El preloader devuelve la lista `html`/`color` que espera bs4Dash. Use `startup = FALSE`
cuando bs4Dash ya controla el inicio. También se detecta un waiter existente para evitar
superponer el automático incluso si una aplicación dejó `startup = TRUE`.

### Actualización y demo

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeShiny",
                        dependencies = NA, upgrade = "never")
# Mientras la ampliación esté pendiente de integrar, añada:
# ref = "codex/shiny-loader-operations"
```

Reinicie la sesión de R y la aplicación tras actualizar. El motor no añade dependencias
obligatorias nuevas. La demo admite `readxl` para importar Excel, `racafeForms` para
mostrar `racafeNotify()` al finalizar y `bs4Dash` para ese layout; son dependencias opcionales.
Sin `readxl` la importación XLSX informa el requisito y cierra el loader; CSV usa R base.

```r
shiny::runApp(system.file("examples", "loaders.R", package = "racafeShiny"))
# Para bs4Dash, antes de ejecutar:
Sys.setenv(RACAFE_LOADER_BS4DASH = "1")
# Para probar también su preloader inicial nativo:
Sys.setenv(RACAFE_LOADER_PRELOADER = "1")
```

La demo incluye dos módulos, tabla local, consulta, actualización, guardado en memoria,
validación/importación de archivos, correo simulado, PDF real y preparación de descarga,
error controlado y operaciones consecutivas. No envía correos ni modifica bases de datos.
Los bloqueos intencionales con `Sys.sleep()` sirven exclusivamente para la demostración.
Vea el [informe de validación de 1.2.0](docs/loaders-1.2.0.md).
