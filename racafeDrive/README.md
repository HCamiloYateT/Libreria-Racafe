# racafeDrive

Integración con Microsoft Graph API, OneDrive y SharePoint. Incluye autenticación con cache de token, navegación de sitios/drives/carpetas, descarga de archivos y lectura de libros Excel.

## Instalación

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeDrive")
```

## Variables de entorno

```text
MS_TENANT_ID=<tenant-id>
MS_CLIENT_ID=<client-id>
MS_CLIENT_SECRET=<client-secret>
GRAPH_DOMAIN=racafe.com
```

## Funciones disponibles

- **Autenticación y cabeceras:** `ObtenerTokenAcceso()`, `CabecerasGraph()`.
- **SharePoint y drives:** `ObtenerIdSite()`, `ObtenerIdDriveSite()`, `ObtenerIdDrive()`.
- **Carpetas y contenido:** `ListarCarpetas()`, `ObtenerIdCarpeta()`, `ListarContenidoCarpetaNombre()`, `ListarContenidoCarpetaId()`, `ListarContenidoCarpetaRecursivo()`, `ListarDriveRecursivo()`, `ListarTodoContenidoCarpeta()`.
- **Archivos y Excel:** `DescargarArchivoId()`, `CargarExcelDesdeOneDrive()`, `DescargarExcelDesdeOneDrive()`, `ListarHojasExcelOneDrive()`, `LeerExcelDesdeOneDrive()`, `CargarExcelSite()`.

## Uso

```r
library(racafeDrive)

token <- ObtenerTokenAcceso()
headers <- CabecerasGraph()

drive_id <- ObtenerIdDrive("juan.perez")
carpetas <- ListarCarpetas("juan.perez")
carpeta_id <- ObtenerIdCarpeta("juan.perez", "Reportes/2026")
contenido <- ListarTodoContenidoCarpeta("juan.perez", carpeta_id)

hojas <- ListarHojasExcelOneDrive(archivo_id = contenido$id[[1]], usuario = "juan.perez")
df <- LeerExcelDesdeOneDrive(archivo_id = contenido$id[[1]], usuario = "juan.perez", sheet = hojas$name[[1]])

site_id <- ObtenerIdSite("racafe.sharepoint.com", "sites/analitica")
site_drive <- ObtenerIdDriveSite(site_id, "Documentos compartidos")
recientes <- ListarDriveRecursivo(site_drive, fecha_desde = Sys.Date() - 7)
```

## Documentación

```r
?ObtenerTokenAcceso
?ListarDriveRecursivo
?CargarExcelSite
```
