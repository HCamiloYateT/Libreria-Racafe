# Librería Racafe

Monorepo con seis paquetes de R instalables de forma independiente. `racafeCore`
contiene las utilidades compartidas; los otros paquetes separan acceso a datos,
Microsoft Graph, visualización, componentes Shiny y pronósticos.

## Estructura

| Paquete | Responsabilidad | Dependencias internas |
|---|---|---|
| [`racafeCore`](./racafeCore/) | Transformación, texto, validación, fechas, HTML y carga de módulos R. | Ninguna |
| [`racafeBD`](./racafeBD/) | Lectura y escritura mediante DBI; conexión principal MySQL y consultas heredadas a SQL Server. | `racafeCore` |
| [`racafeDrive`](./racafeDrive/) | Autenticación y acceso a OneDrive/SharePoint mediante Microsoft Graph. | `racafeCore` |
| [`racafeGraph`](./racafeGraph/) | Paletas, tema y gráficos corporativos con Plotly. | `racafeCore` |
| [`racafeShiny`](./racafeShiny/) | Inputs, outputs, módulos, tablas `gt` y estilos para aplicaciones Shiny. | `racafeCore`, `racafeGraph` |
| [`racafeForecast`](./racafeForecast/) | Imputación, comparación de modelos y pronósticos de series de tiempo. | `racafeCore` |

Todos los paquetes requieren **R >= 4.1.0**. Cada uno mantiene su propia versión
en `DESCRIPTION` y su historial en `NEWS.md`.

## Instalación

Desde GitHub, indique la subcarpeta del monorepo de forma explícita:

```r
repo <- "HCamiloYateT/Libreria-Racafe"

remotes::install_github(repo, subdir = "racafeCore")
remotes::install_github(repo, subdir = "racafeGraph")
remotes::install_github(repo, subdir = "racafeBD")
remotes::install_github(repo, subdir = "racafeDrive")
remotes::install_github(repo, subdir = "racafeForecast")
remotes::install_github(repo, subdir = "racafeShiny")
```

Instale solo los paquetes que necesite, respetando el orden de sus dependencias
internas. Para un repositorio privado, configure `GITHUB_PAT` antes de ejecutar
la instalación.

Para desarrollar desde un clon local:

```r
install.packages(c("devtools", "remotes"))

remotes::install_local("racafeCore", dependencies = TRUE)
remotes::install_local("racafeGraph", dependencies = TRUE)
remotes::install_local("racafeBD", dependencies = TRUE)
remotes::install_local("racafeDrive", dependencies = TRUE)
remotes::install_local("racafeForecast", dependencies = TRUE)
remotes::install_local("racafeShiny", dependencies = TRUE)
```

## Desarrollo y pruebas

Ejecute las pruebas de un paquete desde la raíz:

```r
devtools::test("racafeCore")
devtools::check("racafeCore", args = "--no-manual")
```

Las pruebas de integración usan datos de juguete y dobles para los servicios
externos:

```bash
Rscript scripts/run_integration_tests.R
```

Los flujos de GitHub Actions ejecutan `R CMD check`, pruebas unitarias, pruebas
E2E y un umbral de cobertura del 75 % para los seis paquetes. El entorno de
desarrollo se registra en [`renv.lock`](./renv.lock); la política de dependencias
está en [`docs/dependency-policy.md`](./docs/dependency-policy.md).

## Documentación

- Consulte el README de cada paquete para requisitos, configuración y ejemplos.
- Verifique que toda la API exportada figure en esos README y tenga documentación
  Roxygen con `python3 scripts/check_documentation.py`.
- Vea [`VERSIONING.md`](./VERSIONING.md) para la política de versiones.
- Tras instalar un paquete, use `help(package = "racafeCore")` y
  `?nombre_de_funcion` para consultar su API.
