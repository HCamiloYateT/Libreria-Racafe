# Racafe

Monorepo centralizado con 6 paquetes de R independientes.
Cada subcarpeta corresponde a un paquete instalable de forma autónoma.

## Compatibilidad y dependencias críticas

| Paquete | Versión mínima de R | Dependencias críticas | Instalación local recomendada (desde la raíz del monorepo) |
|---|---:|---|---|
| `racafeCore` | `>= 4.1.0` | `dplyr`, `stringi`, `lubridate`, `janitor` | `devtools::install("./racafeCore")` |
| `racafeBD` | `>= 4.1.0` | `racafeCore`, `DBI`, `odbc` | `devtools::install("./racafeBD")` |
| `racafeDrive` | `>= 4.1.0` | `racafeCore`, `httr2`, `readxl`, `openxlsx2` | `devtools::install("./racafeDrive")` |
| `racafeGraph` | `>= 4.1.0` | `racafeCore`, `plotly` | `devtools::install("./racafeGraph")` |
| `racafeShiny` | `>= 4.1.0` | `racafeCore`, `racafeGraph`, `shiny`, `gt` | `devtools::install("./racafeShiny")` |
| `racafeForecast` | `>= 4.1.0` | `racafeCore`, `forecast`, `zoo` | `devtools::install("./racafeForecast")` |

## Paquetes

| Paquete | Descripción |
|---|---|
| `racafeCore` | Utilidades base: limpieza de texto, validación, fechas, transformación y operadores utilitarios. |
| `racafeBD` | Conexión y operaciones con SQL Server. |
| `racafeDrive` | Integración con Microsoft Graph, OneDrive y SharePoint. |
| `racafeGraph` | Visualización corporativa con `plotly`. |
| `racafeShiny` | Componentes Shiny, formatos `gt` y módulos UI/Server. |
| `racafeForecast` | Pronósticos de series de tiempo. |

## Instalación desde GitHub

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeBD")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeDrive")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeGraph")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeShiny")
remotes::install_github("HCamiloYateT/Libreria-Racafe/racafeForecast")
```

## Versionado y changelog

- Cada paquete usa **SemVer** independiente (`MAJOR.MINOR.PATCH`) en su `DESCRIPTION`.
- La política completa está documentada en [`VERSIONING.md`](./VERSIONING.md).
- El historial de cambios se mantiene en `NEWS.md` dentro de cada paquete:
  - `racafeCore/NEWS.md`
  - `racafeBD/NEWS.md`
  - `racafeDrive/NEWS.md`
  - `racafeGraph/NEWS.md`
  - `racafeShiny/NEWS.md`
  - `racafeForecast/NEWS.md`

## Grafo de dependencias

```text
racafeCore
    ├── racafeBD
    ├── racafeDrive
    ├── racafeGraph
    │   └── racafeShiny
    └── racafeForecast
```

## Desarrollo local (ruta recomendada)

```r
# 1) Ubícate en la raíz del monorepo
setwd("/ruta/local/Libreria-Racafe")

# 2) Instala en orden de dependencia
pkgs <- c(
  "racafeCore",
  "racafeBD",
  "racafeDrive",
  "racafeGraph",
  "racafeShiny",
  "racafeForecast"
)
for (p in pkgs) devtools::install(file.path(".", p))

# 3) Ejecuta pruebas de un paquete específico
devtools::test("racafeCore")
```

## Quality gates y reproducibilidad

- CI bloquea cambios cuando falla `R CMD check` en cualquiera de los subpaquetes.
- CI ejecuta gate de cobertura con `covr` (umbral inicial `75%`) y valida no regresión contra la rama base en PR.
- CI ejecuta pruebas de integración E2E (`core -> bd/drive -> graph/shiny -> forecast`) con datasets de juguete y stubs para servicios externos.
- Política de dependencias y actualización mensual documentada en [`docs/dependency-policy.md`](./docs/dependency-policy.md).
- Lockfile de entorno de desarrollo en `renv.lock`.
