# racafeGraph

Capa de visualización corporativa basada en Plotly. Incluye paletas, un tema de
layout, líneas de referencia y gráficos de densidad, anillo y Sankey.

## Instalación

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore")
remotes::install_github(repo, subdir = "racafeGraph")
```

## Paletas y tema

```r
library(racafeGraph)

ColoresRacafe(5)
ColoresGreenBlue(seq(0, 1, length.out = 10))

ventas_mes <- data.frame(
  mes = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
  valor = c(85, 110, 103)
)

p <- plotly::plot_ly(
  ventas_mes,
  x = ~mes,
  y = ~valor,
  type = "bar"
)

# El operador de inyección !!! requiere rlang.
p <- rlang::inject(
  plotly::layout(p, !!!tema_racafe_plotly("Ventas mensuales"))
)
```

También puede aplicar la lista sin sintaxis de inyección:

```r
p <- do.call(plotly::layout, c(list(p = p), tema_racafe_plotly("Ventas")))
```

## Líneas de referencia

```r
plotly::layout(
  p,
  shapes = list(
    vline(as.Date("2026-02-01"), color = "#28B78D"),
    hline(100, color = "#C0392B")
  )
)
```

## Gráficos incluidos

```r
set.seed(123)
transacciones <- data.frame(monto = rgamma(250, shape = 3, rate = 0.7))
ImprimirDensidad(
  transacciones,
  columna = "monto",
  titulo = "Distribución de transacciones"
)

ventas <- data.frame(
  region = c("Norte", "Sur", "Centro"),
  valor = c(40, 35, 25)
)
ImprimirAnillo(ventas, "region", "valor", funcion = "sum")

flujo <- data.frame(
  origen = c("Web", "Tienda"),
  etapa = c("Contacto", "Venta"),
  valor = c(70, 30)
)
ImprimeSankey(
  flujo,
  vars = c("origen", "etapa"),
  fun = "sum",
  var = "valor",
  colores = ColoresRacafe(4)
)
```

`ImprimirDensidad()` usa un eje X logarítmico, por lo que está orientada a
variables numéricas positivas y sesgadas. Consulte `?ImprimirAnillo` y
`?ImprimeSankey` para las opciones de agregación.
