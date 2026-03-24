# racafeGraph

Capa de visualización corporativa con plotly. Paletas, tema estándar,
gráficos de anillo, Sankey y distribución. La agregación de datos
siempre ocurre en funciones internas separadas del render.

## Instalación

```r
remotes::install_github("HCamiloYateT/Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Racafe/racafeGraph")
```

## Uso

```r
library(racafeGraph)

# ---- Paletas ----
ColoresRacafe(5)                                  # 5 colores corporativos
ColoresGreenBlue(seq(0, 1, length.out = 10))      # gradiente verde-azul

# ---- Tema corporativo ----
# Aplicar a cualquier grafico plotly con !!!
p <- plotly::plot_ly(df, x = ~mes, y = ~valor, type = "bar") |>
  plotly::layout(!!!tema_racafe_plotly("Ventas mensuales"))

# ---- Lineas de referencia ----
plotly::layout(p, shapes = list(
  vline(as.Date("2024-06-01"), color = "#28B78D"),
  hline(1000000, color = "#C0392B")
))

# ---- Grafico de anillo ----
ImprimirAnillo(
  data       = ventas,
  var_label  = "region",
  var_medida = "valor",
  funcion    = "sum"
)

# ---- Distribucion con densidad ----
ImprimirDensidad(
  datos   = transacciones,
  columna = "monto",
  titulo  = "Distribucion de transacciones",
  formato = "dinero"        # cualquier formato de racafeCore
)

# ---- Sankey de flujo ----
ImprimeSankey(
  data    = pipeline,
  vars    = c("fuente_captacion", "etapa_comercial", "resultado"),
  fun     = "sum",
  var     = "valor_oportunidad",
  colores = ColoresRacafe(6)
)
```
