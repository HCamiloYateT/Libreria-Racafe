# racafeDrive

Cliente de Microsoft Graph para autenticación de aplicación, navegación de
OneDrive y SharePoint, y descarga o lectura de libros Excel. El token se conserva
en memoria durante el proceso R y se renueva 60 segundos antes de expirar.

## Instalación

Requiere **R >= 4.1.0**. Instale `remotes` una sola vez si no está disponible:

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
```

Instale primero las dependencias internas, en el orden del ejemplo.

### Desde GitHub

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore", dependencies = NA, upgrade = "never")
remotes::install_github(repo, subdir = "racafeDrive", dependencies = NA, upgrade = "never")
```

### Desde un clon local

Ejecute desde la raíz de `Libreria-Racafe`, donde están las carpetas de los paquetes:

```r
remotes::install_local("racafeCore", dependencies = NA, upgrade = "never")
remotes::install_local("racafeDrive", dependencies = NA, upgrade = "never")
```

`dependencies = NA` instala las dependencias obligatorias; `upgrade = "never"`
evita actualizar otros paquetes ya instalados. Para herramientas de desarrollo,
pruebas y acceso a repositorios privados, consulte la [guía general](../README.md#instalación).

Compruebe la instalación:

```r
library(racafeDrive)
packageVersion("racafeDrive")
```

## Configuración de Microsoft Graph

Defina las credenciales de una aplicación de Microsoft Entra ID:

```text
MS_TENANT_ID=<tenant-id>
MS_CLIENT_ID=<application-client-id>
MS_CLIENT_SECRET=<client-secret>
GRAPH_DOMAIN=ejemplo.com
```

`GRAPH_DOMAIN` solo se usa para completar alias sin `@`; su valor predeterminado
es `racafe.com`. La aplicación debe contar con permisos de Graph compatibles con
los usuarios, sitios y archivos consultados (por ejemplo, permisos de lectura de
archivos y sitios) y con el consentimiento administrativo correspondiente.

## OneDrive

```r
library(racafeDrive)

drive_id <- ObtenerIdDrive("juan.perez")
raiz <- ListarCarpetas("juan.perez")
carpeta_id <- ObtenerIdCarpeta("juan.perez", "Reportes")
contenido <- ListarContenidoCarpetaId("juan.perez", carpeta_id)
todo <- ListarContenidoCarpetaRecursivo("juan.perez", carpeta_id)

# Leer un libro directamente desde Graph.
hojas <- ListarHojasExcelOneDrive("ABC123", "juan.perez")
datos <- LeerExcelDesdeOneDrive(
  archivo_id = "ABC123",
  usuario = "juan.perez",
  sheet = "Datos",
  skip = 1
)

# Buscar por ruta y guardar una copia local.
DescargarExcelDesdeOneDrive(
  usuario = "juan.perez",
  ruta = "Reportes/2026",
  archivo = "cierre_enero.xlsx",
  nombre_salida = "cierre_enero_local"
)
```

## SharePoint

```r
site_id <- ObtenerIdSite("ejemplo.sharepoint.com", "sites/analitica")
drive_id <- ObtenerIdDriveSite(site_id, "Documentos compartidos")

recientes <- ListarDriveRecursivo(
  drive_id,
  fecha_desde = Sys.Date() - 7
)

datos <- CargarExcelSite(
  drive_id = drive_id,
  item_id = recientes$id[[1]],
  hoja = "Presupuesto",
  skip = 2
)
```

## Autenticación y diagnóstico

Normalmente no hace falta solicitar el token manualmente. Para renovarlo en un
proceso de larga duración:

```r
token <- ObtenerTokenAcceso(force = TRUE)
headers <- CabecerasGraph()
```

Las llamadas de red no se ejecutan al cargar el paquete. Los errores de Graph
incluyen contexto para revisar credenciales, identificadores y permisos.
