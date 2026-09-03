# racafeShiny

Componentes de presentación para aplicaciones Shiny: formatos numéricos, estilos
de tablas `gt`, inputs, botones, sidebars, modales y un módulo de KPI. Depende de
`racafeCore` y `racafeGraph`.

## Instalación

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore")
remotes::install_github(repo, subdir = "racafeGraph")
remotes::install_github(repo, subdir = "racafeShiny")
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

# Configurar los textos y la búsqueda de ListaDesplegable().
opciones_picker <- pick_opt(c("Norte", "Sur", "Centro"), fem = TRUE)

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
