# racafeGraph

Capa de visualización corporativa con Plotly. Provee paletas institucionales, un tema estándar, líneas de referencia y gráficos listos para análisis exploratorio o tableros.

## Instalación

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeGraph")
```

## Funciones disponibles

- `ColoresRacafe()`: paleta corporativa discreta.
- `ColoresGreenBlue()`: gradiente verde-azul.
- `tema_racafe_plotly()`: lista de configuración para `plotly::layout()`.
- `vline()` y `hline()`: líneas verticales/horizontales para `layout(shapes = ...)`.
- `ImprimirDensidad()`: histograma/densidad de una variable numérica.
- `ImprimirAnillo()`: gráfico de anillo agregado por categoría.
- `ImprimeSankey()`: diagrama Sankey para flujos entre etapas.

## Uso

```r
library(racafeGraph)

ColoresRacafe(5)
ColoresGreenBlue(seq(0, 1, length.out = 10))

p <- plotly::plot_ly(df, x = ~mes, y = ~valor, type = "bar") |>
  plotly::layout(!!!tema_racafe_plotly("Ventas mensuales"))

plotly::layout(p, shapes = list(
  vline(as.Date("2026-07-01"), color = "#28B78D"),
  hline(1000000, color = "#C0392B")
))

ImprimirAnillo(ventas, var_label = "region", var_medida = "valor", funcion = "sum")
ImprimirDensidad(transacciones, columna = "monto", titulo = "Distribución de transacciones")
ImprimeSankey(pipeline, vars = c("fuente", "etapa", "resultado"), fun = "sum", var = "valor")
```

## Documentación

```r
?tema_racafe_plotly
?ImprimirAnillo
?ImprimeSankey
```
