# Librería Racafe

Monorepo de paquetes R para el ecosistema Racafe. Cada subcarpeta es un paquete instalable de forma independiente, con documentación `help()` generada en `man/` y una responsabilidad clara dentro del flujo analítico: utilidades base, bases de datos, Microsoft Graph/OneDrive, visualización, componentes Shiny y pronósticos.

## Paquetes y funciones principales

| Paquete | Responsabilidad | Funciones exportadas |
|---|---|---|
| `racafeCore` | Núcleo compartido: dependencias, carga modular, transformación, validación, fechas y HTML básico. | `Loadpkg()`, `load_modules()`, `PushInicial()`, `RecodificarTop()`, `TopAbsoluto()`, `TopRelativo()`, `AdicionarBotones()`, `bind_rows_na()`, `left_join_all()`, `RevisarDuplicados()`, `%||%`, `LimpiarNombres()`, `LimpiarCadena()`, `UnirCadenas()`, `Unicos()`, `EsVacio()`, `EsEnteroPositivo()`, `EsNumero()`, `EsNumTelefono()`, `EsEmail()`, `buscar_cadena()`, `SiError_0()`, `Variacion()`, `Moda()`, `RedondearMultiplo()`, `PrimerDia()`, `FechaTexto()`, `EdadCumplida()`, `Saltos()`, `Espacios()`, `Obligatorio()` |
| `racafeBD` | Acceso a datos, lectura, escritura, reemplazo y consultas SQL. | `ConectarBD()`, `EscribirDatos()`, `AgregarDatos()`, `ReemplazarDatos()`, `CargarDatos()`, `Consulta()`, `ConsultaSistema()` |
| `racafeDrive` | Integración con Microsoft Graph, OneDrive y SharePoint. | `ObtenerTokenAcceso()`, `CabecerasGraph()`, `ObtenerIdSite()`, `ObtenerIdDriveSite()`, `ObtenerIdDrive()`, `CargarExcelDesdeOneDrive()`, `DescargarExcelDesdeOneDrive()`, `ListarCarpetas()`, `ObtenerIdCarpeta()`, `ListarContenidoCarpetaNombre()`, `ListarContenidoCarpetaId()`, `ListarContenidoCarpetaRecursivo()`, `ListarDriveRecursivo()`, `ListarTodoContenidoCarpeta()`, `DescargarArchivoId()`, `ListarHojasExcelOneDrive()`, `LeerExcelDesdeOneDrive()`, `CargarExcelSite()` |
| `racafeGraph` | Paletas, tema Plotly y gráficos corporativos. | `ColoresRacafe()`, `ColoresGreenBlue()`, `tema_racafe_plotly()`, `vline()`, `hline()`, `ImprimirDensidad()`, `ImprimirAnillo()`, `ImprimeSankey()` |
| `racafeShiny` | Formatos, estilos `gt`, inputs, botones, outputs y módulos Shiny. | `DefinirFormato()`, `ObtenerFormato()`, `FormatoD3()`, `FormatoJS()`, `FormatoHOT()`, `FormatearNumero()`, `FormatearTexto()`, `FormatrearTexto()`, `gt_minimal_style()`, `gt_mensaje_vacio()`, `col_kpi()`, `chr_kpi()`, `col_num()`, `gt_pct_style()`, `gt_var_style()`, `gt_sign_style()`, `gt_color_columns()`, `InputNumerico()`, `ListaDesplegable()`, `pick_opt()`, `BotonesRadiales()`, `BotonEstado()`, `Boton()`, `InputFecha()`, `InputMes()`, `SidebarItemWrap()`, `BotonDescarga()`, `CajaIco()`, `CajaValor()`, `DefinirColumnaHtml()`, `FormatearFila()`, `ObtenerReglaFila()`, `cajaValor_ui()`, `cajaValor_server()`, `mostrarModal()` |
| `racafeForecast` | Pronósticos con varios modelos, métricas y extractores S3. | `aplicar_imputacion()`, `extraer_intervalos()`, `ejecutar_pronosticos()`, `Pronosticar()`, `PronMetricas()`, `PronSeleccionar()`, `PronSerie()`, `PronMensual()`, `PronPatronMes()` |

## Instalación desde GitHub

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore")
remotes::install_github(repo, subdir = "racafeBD")
remotes::install_github(repo, subdir = "racafeDrive")
remotes::install_github(repo, subdir = "racafeGraph")
remotes::install_github(repo, subdir = "racafeShiny")
remotes::install_github(repo, subdir = "racafeForecast")
```

Para repositorios privados, define previamente `GITHUB_PAT`.

## Desarrollo local

```r
setwd("/ruta/local/Libreria-Racafe")

pkgs <- c("racafeCore", "racafeBD", "racafeDrive", "racafeGraph", "racafeShiny", "racafeForecast")
for (p in pkgs) devtools::install(file.path(".", p))

devtools::test("racafeCore")
```

## Documentación en R

Todos los paquetes incluyen archivos `.Rd` bajo `man/`, por lo que las funciones exportadas se pueden consultar con `help()` después de instalar o cargar el paquete:

```r
library(racafeCore)
help(load_modules)
?RecodificarTop

library(racafeForecast)
?Pronosticar
```

## Variables de entorno por módulo

- `racafeBD`: `DB_NAME`, `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `DB_ENCODING`; para `ConsultaSistema()`, también `SYS_UID` y `SYS_PWD` cuando no se pasan credenciales explícitas.
- `racafeDrive`: `MS_TENANT_ID`, `MS_CLIENT_ID`, `MS_CLIENT_SECRET` y opcionalmente `GRAPH_DOMAIN`.

## Grafo de dependencias

```text
racafeCore
    ├── racafeBD
    ├── racafeDrive
    ├── racafeGraph
    │   └── racafeShiny
    └── racafeForecast
```

## Calidad

- Pruebas unitarias con `testthat` por paquete.
- Pruebas de integración en `tests/integration`.
- Política de dependencias en `docs/dependency-policy.md`.
- Versionado SemVer independiente por paquete.
