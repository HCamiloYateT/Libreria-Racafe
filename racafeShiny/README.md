# racafeShiny

Capa de presentación del ecosistema Racafe. Inputs y outputs Shiny
con estilo corporativo, tablas `gt`, módulos UI/Server y utilidades HTML.

## Instalación

```r
remotes::install_github("HCamiloYateT/Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Racafe/racafeGraph")
remotes::install_github("HCamiloYateT/Racafe/racafeShiny")
```

## Formatos y estilos

`racafeShiny` re-exporta `DefinirFormato`, `ObtenerFormato` y `ListarFormatos`
desde `racafeCore`, por lo que no es necesario cargar `racafeCore`
explícitamente para usarlos.

```r
library(racafeShiny)

# Formateo con color condicional (HTML)
FormatearNumero(1250000, "dinero")
FormatearNumero(0.85, "porcentaje", meta = 0.80)    # verde: cumple
FormatearNumero(0.72, "porcentaje", meta = 0.80)    # rojo: no cumple
FormatearTexto("Meta alcanzada", color = "#28B78D", negrita = TRUE)

# KPI helpers
col_kpi(c(0.70, 0.92, 1.05))   # "#C0392B" "#F39C12" "#1A7A5E"
chr_kpi(c(0.70, 0.92, 1.05))   # "✖" "⚠" "✔"

# Tablas gt
ventas |>
  gt::gt() |>
  gt_minimal_style() |>
  gt_pct_style(columns = cumplimiento) |>
  gt_sign_style(columns = variacion) |>
  gt_color_columns(columns = meta, color = "#28B78D")

# Tabla vacia con mensaje
gt_mensaje_vacio("Sin resultados para los filtros aplicados")
```

## Inputs Shiny

```r
# En UI:
InputNumerico("meta", "Meta mensual", value = 1e6, dec = 0,
  label_col = 5, input_col = 7)

ListaDesplegable("region", "Región",
  choices  = c("Norte","Sur","Centro","Occidente"),
  multiple = TRUE, fem = FALSE)

BotonesRadiales("periodo", "Periodo",
  choices    = c("Mensual","Trimestral","Anual"),
  alineacion = "center")

BotonEstado("activar_comp", "Activar comparativo")
BotonGuardar("guardar",     label = "Guardar cambios", align = "right")
```

## Outputs y módulo CajaValor

```r
# Componente estatico (no modular)
CajaValor(1250000, "dinero", "Ventas del mes", "dollar-sign",
  inputId = "ver_detalle")

# Patron modular recomendado en apps productivas
# --- UI ---
cajaValor_ui("kpi_ventas", icono = "dollar-sign", texto = "Ventas")

# --- Server ---
mod <- cajaValor_server(
  id      = "kpi_ventas",
  valor_r = reactive(sum(datos()$ventas, na.rm = TRUE)),
  formato = "dinero",
  meta    = 1000000       # rojo si < 1M, verde si >= 1M
)

# Escuchar clic del boton de detalle desde el modulo padre
observeEvent(mod$click_detalle(), {
  showModal(modalDialog(title = "Detalle ventas", ...))
})

# Boton de descarga con color corporativo
BotonDescarga("export_excel",
  color = "#28B78D", title = "Exportar a Excel")
```
