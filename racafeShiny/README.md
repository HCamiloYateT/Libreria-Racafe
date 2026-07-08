# racafeShiny

Capa de presentación del ecosistema Racafe. Incluye registro de formatos, formateo HTML, estilos para tablas `gt`, inputs y botones Shiny, cajas KPI, módulos UI/Server y utilidades para sidebars y modales.

## Instalación

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeGraph")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeShiny")
```

## Funciones disponibles

- **Formatos y texto:** `DefinirFormato()`, `ObtenerFormato()`, `FormatoD3()`, `FormatoJS()`, `FormatoHOT()`, `FormatearNumero()`, `FormatearTexto()`, `FormatrearTexto()`.
- **Tablas `gt`:** `gt_minimal_style()`, `gt_mensaje_vacio()`, `gt_pct_style()`, `gt_var_style()`, `gt_sign_style()`, `gt_color_columns()`.
- **KPI y columnas:** `col_kpi()`, `chr_kpi()`, `col_num()`.
- **Inputs y botones:** `InputNumerico()`, `ListaDesplegable()`, `pick_opt()`, `BotonesRadiales()`, `BotonEstado()`, `Boton()`, `InputFecha()`, `InputMes()`.
- **Outputs, tablas y módulos:** `BotonDescarga()`, `CajaIco()`, `CajaValor()`, `DefinirColumnaHtml()`, `FormatearFila()`, `ObtenerReglaFila()`, `cajaValor_ui()`, `cajaValor_server()`.
- **Layout:** `SidebarItemWrap()`, `mostrarModal()`.

## Uso

```r
library(racafeShiny)

DefinirFormato("millones", prefijo = "$", sufijo = " M", decimales = 1, escala = 1e-6)
FormatearNumero(1250000, "millones")
FormatearTexto("Meta alcanzada", color = "#28B78D", negrita = TRUE)
col_kpi(c(0.70, 0.92, 1.05))
chr_kpi(c(0.70, 0.92, 1.05))

ventas |>
  gt::gt() |>
  gt_minimal_style() |>
  gt_pct_style(columns = cumplimiento) |>
  gt_sign_style(columns = variacion)
```

## Componentes Shiny

```r
# UI
InputNumerico("meta", "Meta mensual", value = 1e6, dec = 0)
ListaDesplegable("region", "Región", choices = c("Norte", "Sur", "Centro"))
BotonesRadiales("periodo", "Periodo", choices = c("Mensual", "Trimestral", "Anual"))
Boton("guardar", label = "Guardar cambios", align = "right")
BotonDescarga("export_excel", icono = "file-excel", size = "sm", title = "Exportar a Excel")
CajaValor(1250000, "dinero", "Ventas del mes", "dollar-sign", inputId = "ver_detalle")

# Módulo UI/Server
cajaValor_ui("kpi_ventas", icono = "dollar-sign", texto = "Ventas")
mod <- cajaValor_server(
  id = "kpi_ventas",
  valor_r = shiny::reactive(sum(datos()$ventas, na.rm = TRUE)),
  formato = "dinero",
  meta = 1000000
)

mostrarModal(shiny::p("Contenido"), titulo = "Detalle", tamano = "subventana3")
```

## Documentación

```r
?FormatearNumero
?Boton
?cajaValor_server
?mostrarModal
```
