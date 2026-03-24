# racafeForecast

Motor de pronósticos de series de tiempo. Ajusta ETS, ARIMA, NNETAR,
TBATS y Tendencia lineal. Selecciona el mejor modelo por RMSE.
Resultado estandarizado como objeto S3 `pronostico_racafe`.

## Instalación

```r
remotes::install_github("HCamiloYateT/Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Racafe/racafeForecast")
```

## Flujo completo

```r
library(racafeForecast)

# 1. Preparar datos
df <- Consulta("
  SELECT fecha_mes AS fecha, SUM(valor) AS ventas, SUM(costo) AS costo
  FROM fact_ventas
  GROUP BY fecha_mes ORDER BY fecha_mes
")

# 2. Pronosticar (orquesta todo: imputacion, split, modelos, metricas)
pron <- Pronosticar(
  df                = df,
  fecha_col         = "fecha",
  valor_cols        = c("ventas", "costo"),  # multiples columnas
  nivel_confianza   = 0.95,
  prop_train        = 0.80,
  h_periods         = 12,
  metodo_imputacion = "interpolacion"
)

# 3. Ver metricas de todos los modelos por columna
metricas <- PronMetricas(pron)
metricas |> dplyr::arrange(columna, rmse)

# 4. Seleccionar el mejor modelo automaticamente (menor RMSE)
mejor <- PronSeleccionar(pron)

# 5. Extraer resultados
serie   <- PronSerie(mejor)      # fecha | pronostico | lower | upper | columna
mensual <- PronMensual(mejor)    # resumen por mes
patron  <- PronPatronMes(mejor)  # indice estacional por mes (1.0 = promedio)

# 6. Visualizar con racafeGraph
library(racafeGraph)

plotly::plot_ly(serie |> dplyr::filter(columna == "ventas")) |>
  plotly::add_lines(x = ~fecha, y = ~pronostico,
    name = "Pronostico",
    line = list(color = ColoresRacafe(1))) |>
  plotly::add_ribbons(x = ~fecha, ymin = ~lower, ymax = ~upper,
    name = "IC 95%",
    fillcolor = "rgba(40,183,141,0.15)",
    line      = list(color = "transparent")) |>
  plotly::layout(!!!tema_racafe_plotly("Pronostico ventas — 12 meses"))
```

## Imputación de NA

```r
# Metodos disponibles
aplicar_imputacion(serie, "promedio")
aplicar_imputacion(serie, "mediana")
aplicar_imputacion(serie, "interpolacion")   # requiere zoo
aplicar_imputacion(serie, "ultimo")           # last observation carried forward
aplicar_imputacion(serie, "constante", valor_constante = 0)
aplicar_imputacion(serie, "percentil", prob_percentil = 0.25)
```

## Objeto `pronostico_racafe`

```r
# Inspeccion del objeto
class(pron)         # "pronostico_racafe" "list"
pron$columnas       # columnas pronosticadas
pron$h_periods      # horizonte
pron$nivel_confianza

# Pipeline completo en una cadena
Pronosticar(df, "fecha", "ventas") |>
  PronSeleccionar() |>
  PronSerie()
```
