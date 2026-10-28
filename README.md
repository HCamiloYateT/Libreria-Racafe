# Librería Racafe

Monorepo con ocho paquetes de R instalables de forma independiente. `racafeCore`
contiene las utilidades compartidas; los otros paquetes separan acceso a datos,
Microsoft Graph, visualización, componentes Shiny, correo corporativo y pronósticos.

## Estructura

| Paquete | Responsabilidad | Dependencias internas |
|---|---|---|
| [`racafeCore`](./racafeCore/) | Transformación, texto, validación, fechas, HTML y carga de módulos R. | Ninguna |
| [`racafeBD`](./racafeBD/) | Lectura y escritura mediante DBI; conexión principal MySQL y consultas heredadas a SQL Server. | `racafeCore` |
| [`racafeDrive`](./racafeDrive/) | Autenticación y acceso a OneDrive/SharePoint mediante Microsoft Graph. | `racafeCore` |
| [`racafeGraph`](./racafeGraph/) | Paletas, tema y gráficos corporativos con Plotly. | `racafeCore` |
| [`racafeShiny`](./racafeShiny/) | Inputs, outputs, módulos, tablas `gt` y estilos para aplicaciones Shiny. | `racafeCore`, `racafeGraph` |
| [`racafeForms`](./racafeForms/) | Formularios, entradas, botones y alertas corporativas para Shiny. | Ninguna |
| [`racafeMail`](./racafeMail/) | Configuración R, renderizado local y envío explícito de cuatro plantillas mediante blastula. | Ninguna |
| [`racafeForecast`](./racafeForecast/) | Imputación, comparación de modelos y pronósticos de series de tiempo. | `racafeCore` |

Todos los paquetes requieren **R >= 4.1.0**. Cada uno mantiene su propia versión
en `DESCRIPTION` y su historial en `NEWS.md`.

## Instalación

Requiere **R >= 4.1.0** y acceso a CRAN para descargar dependencias. Instale
`remotes` una sola vez si no está disponible:

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
```

### Desde GitHub

Indique la subcarpeta del monorepo de forma explícita. Para instalar los ocho
paquetes, ejecute en este orden:

```r
repo <- "HCamiloYateT/Libreria-Racafe"

remotes::install_github(repo, subdir = "racafeCore", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeGraph", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeBD", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeDrive", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeForecast", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeShiny", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeForms", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeMail", dependencies = NA, upgrade = "never")
```

Puede instalar solo los paquetes que necesite: `racafeCore`, `racafeForms` y `racafeMail` no
tienen dependencias internas; `racafeShiny` requiere primero `racafeCore` y
`racafeGraph`; los demás requieren `racafeCore`. `racafeForms` no requiere
`racafeShiny` ni sustituye automáticamente sus componentes.

Si el repositorio es privado, use la autenticación de GitHub disponible en su
entorno. En una sesión local que lo requiera, configure `GITHUB_PAT` de forma
segura antes de instalar; no escriba el token en scripts ni lo confirme en Git.

### Desde un clon local

Ejecute desde la raíz de `Libreria-Racafe`, donde están las ocho carpetas:

```r
remotes::install_local("racafeCore", dependencies = NA, upgrade = "never")
remotes::install_local("racafeGraph", dependencies = NA, upgrade = "never")
remotes::install_local("racafeBD", dependencies = NA, upgrade = "never")
remotes::install_local("racafeDrive", dependencies = NA, upgrade = "never")
remotes::install_local("racafeForecast", dependencies = NA, upgrade = "never")
remotes::install_local("racafeShiny", dependencies = NA, upgrade = "never")
remotes::install_local("racafeForms", dependencies = NA, upgrade = "never")
remotes::install_local("racafeMail", dependencies = NA, upgrade = "never")
```

`dependencies = NA` instala las dependencias obligatorias declaradas en cada
`DESCRIPTION`; `upgrade = "never"` evita actualizar otros paquetes ya instalados.
Para desarrollar y ejecutar pruebas, instale también las herramientas y las
dependencias sugeridas de los paquetes que vaya a trabajar:

```r
install.packages(c("devtools", "remotes"))
remotes::install_local("racafeForms", dependencies = TRUE, upgrade = "never")
```

Sustituya `racafeForms` por el paquete que necesite. Para instalar y probar todos
los paquetes desde la raíz, también puede usar `Rscript dev.R install` y
`Rscript dev.R test` en la terminal.

### Verificar la instalación

Por ejemplo, para formularios Shiny:

```r
library(racafeForms)
packageVersion("racafeForms")
shiny::runApp(system.file("examples", "demo.R", package = "racafeForms"))
```

Consulte el README de cada paquete para requisitos adicionales del sistema y
configuración de servicios externos, especialmente bases de datos y Microsoft Graph.

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

Los flujos de GitHub Actions ejecutan `R CMD check` y pruebas unitarias para los
ocho paquetes, además de pruebas E2E del monorepo. El umbral de cobertura del
75 % conserva su alcance sobre los seis paquetes originales. El entorno de
desarrollo se registra en [`renv.lock`](./renv.lock); la política de dependencias
está en [`docs/dependency-policy.md`](./docs/dependency-policy.md).

## Documentación

- Consulte el README de cada paquete para requisitos, configuración y ejemplos.
- Vea [`VERSIONING.md`](./VERSIONING.md) para la política de versiones.
- Tras instalar un paquete, use `help(package = "racafeCore")` y
  `?nombre_de_funcion` para consultar su API.

## Ayuda de las funciones en RStudio

La instalación de cada paquete incluye sus páginas de ayuda; no necesita ejecutar
roxygen2 ni instalar herramientas de desarrollo para consultarlas. En RStudio se
muestran en el panel **Help**. Por ejemplo:

```r
help(package = "racafeCore")
help("LimpiarNombres", package = "racafeCore")
?racafeCore::LimpiarNombres
help("%||%", package = "racafeCore")
help(package = "racafeForms")
?racafeForms::racafeInputNumeric
```

Puede colocar el cursor sobre una función y pulsar **F1**. Si tiene una versión
anterior instalada, reinstale el paquete desde el clon actualizado (o desde GitHub
cuando los cambios estén integrados) y reinicie la sesión R para cargar la versión nueva:

```r
remotes::install_local("racafeCore", dependencies = NA, upgrade = "never", force = TRUE)
```

Sustituya `racafeCore` por el paquete que quiera actualizar. Para mantener la ayuda
al modificar funciones, edite sus comentarios roxygen y regenere los archivos
`man/*.Rd` con `devtools::document("racafeCore")` desde la raíz. Incluya esos archivos
en Git. Compruebe los ocho paquetes con:

```bash
Rscript scripts/check_documentation.R
```

## Correo corporativo con racafeMail

`racafeMail` 0.2.0 incluye configuración R, cuatro plantillas, firma y logotipo en el paquete.
No requiere Compartido ni una carpeta email externa. Use `racafeMailConfig()` sin argumentos;
render y preview no hacen SMTP ni descargas. El envío explícito utiliza blastula y mantiene
las credenciales fuera de Git. Consulte [instalación, API y migración](racafeMail/README.md).
Para revisar esta versión antes de integrarla, use `ref = "codex/racafemail-self-contained"`
al instalar el subpaquete. Fije el SHA completo aprobado para una instalación reproducible.
