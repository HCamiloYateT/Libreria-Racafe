# racafeForecast

Motor de pronósticos de series de tiempo para Racafe. Orquesta imputación de valores faltantes, ajuste de modelos, evaluación por RMSE y extracción de resultados desde objetos `pronostico_racafe`.

## Instalación

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeForecast")
```

## Funciones disponibles

- **Preparación y ejecución:** `aplicar_imputacion()`, `extraer_intervalos()`, `ejecutar_pronosticos()`, `Pronosticar()`.
- **Análisis del resultado:** `PronMetricas()`, `PronSeleccionar()`, `PronSerie()`, `PronMensual()`, `PronPatronMes()`.

## Flujo recomendado

```r
library(racafeForecast)

pron <- Pronosticar(
  df = df,
  fecha_col = "fecha",
  valor_cols = c("ventas", "costo"),
  nivel_confianza = 0.95,
  prop_train = 0.80,
  h_periods = 12,
  metodo_imputacion = "interpolacion"
)

metricas <- PronMetricas(pron)
mejor <- PronSeleccionar(pron)
serie <- PronSerie(mejor)
mensual <- PronMensual(mejor)
patron <- PronPatronMes(mejor)
```

## Imputación

```r
aplicar_imputacion(serie, "promedio")
aplicar_imputacion(serie, "mediana")
aplicar_imputacion(serie, "interpolacion")
aplicar_imputacion(serie, "ultimo")
aplicar_imputacion(serie, "constante", valor_constante = 0)
aplicar_imputacion(serie, "percentil", prob_percentil = 0.25)
```

## Documentación

```r
?Pronosticar
?PronMetricas
?PronSerie
```
