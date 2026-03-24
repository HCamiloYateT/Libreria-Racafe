# Racafe

Repositorio centralizado con 6 paquetes R independientes.
Cada subcarpeta es un paquete instalable de forma autónoma.

## Paquetes

| Paquete | Descripcion | Dependencias |
|---|---|---|
| `racafeCore` | Texto, fechas, numericas, transformacion, operadores | base R, dplyr, stringi, lubridate, scales |
| `racafeBD` | Conexion y operaciones SQL Server | racafeCore, DBI, odbc, janitor |
| `racafeDrive` | Microsoft Graph / OneDrive / SharePoint | racafeCore, httr2, readxl, openxlsx2 |
| `racafeGraph` | Visualizacion con plotly | racafeCore, plotly |
| `racafeShiny` | Componentes Shiny, formatos gt, modulos UI/Server | racafeCore, racafeGraph, shiny, gt, shinyWidgets |
| `racafeForecast` | Pronosticos de series de tiempo | racafeCore, forecast, zoo |

## Instalacion

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeBD")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeDrive")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeGraph")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeShiny")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeForecast")
```

## Grafo de dependencias

```
racafeCore
    ├── racafeBD
    ├── racafeDrive
    ├── racafeGraph
    │       └── racafeShiny
    └── racafeForecast
```

## Desarrollo local

```r
# Instalar todos en orden correcto
pkgs <- c("racafeCore","racafeBD","racafeDrive","racafeGraph",
          "racafeShiny","racafeForecast")
for (p in pkgs) devtools::install(file.path(".", p))

# Test de un paquete especifico
devtools::test("racafeCore")
```
