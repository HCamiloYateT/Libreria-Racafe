# racafeForecast

Motor de pronósticos para series de tiempo. Imputa valores faltantes, divide la
serie en entrenamiento y prueba, compara ETS, ARIMA, NNETAR, TBATS y tendencia,
y selecciona el modelo con menor RMSE. Los resultados usan la clase S3
`pronostico_racafe`.

## Instalación

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore")
remotes::install_github(repo, subdir = "racafeForecast")
```

## Flujo completo reproducible

```r
library(racafeForecast)

set.seed(123)
serie <- data.frame(
  fecha = seq(as.Date("2022-01-01"), by = "month", length.out = 48),
  ventas = 100 + 1:48 + 12 * sin(2 * pi * (1:48) / 12) + rnorm(48, 0, 3),
  costo = 70 + 0.7 * (1:48) + rnorm(48, 0, 2)
)
serie$ventas[c(8, 21)] <- NA

pron <- Pronosticar(
  df = serie,
  fecha_col = "fecha",
  valor_cols = c("ventas", "costo"),
  nivel_confianza = 0.95,
  prop_train = 0.80,
  h_periods = 12,
  metodo_imputacion = "interpolacion"
)

# Comparar y seleccionar el menor RMSE por columna.
metricas <- PronMetricas(pron)
mejor <- PronSeleccionar(pron)

# Extraer tablas normalizadas.
detalle <- PronSerie(mejor)
mensual <- PronMensual(mejor)
patron <- PronPatronMes(mejor)
```

`valor_cols = NULL` selecciona todas las columnas numéricas excepto la fecha.
Use una serie suficientemente larga para separar entrenamiento y prueba y para
que los modelos estacionales puedan ajustarse.

## Imputación

```r
x <- c(10, NA, 13, 15, NA, 18)

aplicar_imputacion(x, "promedio")
aplicar_imputacion(x, "mediana")
aplicar_imputacion(x, "interpolacion")
aplicar_imputacion(x, "ultimo")
aplicar_imputacion(x, "constante", valor_constante = 0)
aplicar_imputacion(x, "percentil", prob_percentil = 0.25)
```

## Inspección y filtrado

```r
class(pron)
pron$columnas
pron$h_periods
pron$nivel_confianza

# Trabajar con una sola variable.
PronMetricas(pron, columna = "ventas")
PronSeleccionar(pron, columna = "ventas") |>
  PronSerie(columna = "ventas")
```

## Visualización opcional

`racafeForecast` no genera gráficos. La tabla de `PronSerie()` se puede pasar a
Plotly directamente o combinar con `racafeGraph`:

```r
ventas_pron <- detalle[detalle$columna == "ventas", ]

plotly::plot_ly(ventas_pron, x = ~fecha) |>
  plotly::add_ribbons(
    ymin = ~lower, ymax = ~upper,
    name = "IC 95%",
    fillcolor = "rgba(40,183,141,0.15)",
    line = list(color = "transparent")
  ) |>
  plotly::add_lines(
    y = ~pronostico,
    name = "Pronóstico",
    line = list(color = "#28B78D")
  )
```

Las funciones `ejecutar_pronosticos()` y `extraer_intervalos()` exponen piezas de
nivel inferior para flujos personalizados; `Pronosticar()` es la entrada
recomendada para el flujo habitual.
