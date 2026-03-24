# racafeDrive

Integración completa con Microsoft Graph API, OneDrive y SharePoint.
Token con **cache en memoria** y renovación automática al expirar (3600s).

## Instalación

```r
remotes::install_github("HCamiloYateT/Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Racafe/racafeDrive")
```

## Variables de entorno requeridas

```
GRAPH_TOKEN_URL=https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token
GRAPH_CLIENT_ID=<client-id-azure-ad>
GRAPH_CLIENT_SECRET=<client-secret>
GRAPH_DOMAIN=racafe.com
```

## Uso

```r
library(racafeDrive)

# ---- OneDrive de usuario ----
drive_id <- ObtenerIdDrive("juan.perez")
carpetas  <- ListarCarpetas("juan.perez")

# Leer Excel directamente (sin guardar en disco)
df <- LeerExcelDesdeOneDrive(
  archivo_id = "ABC123def456",
  usuario    = "juan.perez",
  sheet      = "Datos", skip = 1
)

# Descargar y guardar localmente
DescargarExcelDesdeOneDrive(
  usuario      = "juan.perez",
  ruta         = "Reportes/2024",
  archivo      = "cierre_enero.xlsx",
  nombre_salida = "cierre_enero_local"
)

# Listar contenido recursivo de una carpeta
todos <- ListarTodoContenidoCarpeta("juan.perez", carpeta_id = "XYZ789")

# ---- SharePoint ----
site_id  <- ObtenerIdSite("racafe.sharepoint.com", "sites/analitica")
drive_id <- ObtenerIdDriveSite(site_id, "Documentos compartidos")

# Archivos modificados en los ultimos 7 dias
recientes <- ListarDriveRecursivo(drive_id, fecha_desde = Sys.Date() - 7)

# Leer Excel directamente desde SharePoint
df <- CargarExcelSite(
  drive_id = drive_id,
  item_id  = recientes$id[[1]],
  hoja     = "Presupuesto",
  skip     = 2
)
```

## Cache de token

El token se obtiene una sola vez por proceso R y se renueva
automáticamente 60 segundos antes de expirar. No es necesario
llamar `ObtenerTokenAcceso()` manualmente en flujos normales.

```r
# Forzar renovacion manual (util en procesos batch de larga duracion)
ObtenerTokenAcceso(force = TRUE)
```
