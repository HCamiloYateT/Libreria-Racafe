# racafeBD

`racafeBD` centraliza operaciones de lectura y escritura con `DBI`. La conexión
que usan `ConectarBD()`, `Consulta()`, `CargarDatos()` y las funciones de
escritura es **MySQL**. `ConsultaSistema()` conserva un acceso alternativo a
bases corporativas de **SQL Server** mediante ODBC.

## Instalación

```r
repo <- "HCamiloYateT/Libreria-Racafe"
remotes::install_github(repo, subdir = "racafeCore")
remotes::install_github(repo, subdir = "racafeBD")
```

Además de las dependencias declaradas por el paquete, la conexión principal
requiere el paquete `RMySQL` y un cliente/controlador MySQL disponible en el
sistema. El acceso heredado requiere un controlador ODBC para SQL Server.

## Configuración

Las funciones principales leen las credenciales desde el entorno. Por ejemplo,
en `.Renviron`:

```text
DB_HOST=db.ejemplo.local
DB_PORT=3306
DB_NAME=mi_base
DB_USER=mi_usuario
DB_PASSWORD=mi_contrasena
DB_ENCODING=utf8
```

`ConsultaSistema()` usa `SYS_UID` y `SYS_PWD`; admite los alias `syscafe`,
`cafesys` y `estad`. No almacene secretos en scripts ni los confirme en Git.

## Uso

```r
library(racafeBD)

# La conexión debe cerrarse cuando se administra manualmente.
con <- ConectarBD()
DBI::dbDisconnect(con)

# Lectura
ventas <- CargarDatos("fact_ventas")
ventas_2024 <- CargarDatos("fact_ventas", "anio = 2024")
top_10 <- Consulta(
  "SELECT * FROM fact_ventas ORDER BY valor DESC LIMIT 10"
)

# Escritura: reemplazar la tabla o agregar filas.
EscribirDatos(df_resumen, "reporte_diario")
AgregarDatos(df_nuevos, "reporte_diario")

# Elimina las filas que coinciden con las llaves y agrega el reemplazo
# dentro de una transacción.
ReemplazarDatos(
  df_corregido,
  "fact_ventas",
  llaves = list(id = 42, fecha = "2024-01-15")
)
```

Consulta a un sistema SQL Server alternativo:

```r
legacy <- ConsultaSistema(
  bd = "syscafe",
  query = "SELECT TOP 10 * FROM tabla_legacy",
  server = "sqlserver.ejemplo.local"
)
```

Las funciones que reciben texto SQL o nombres de tabla esperan entradas de
confianza. Parametrice y valide cualquier dato proveniente de usuarios antes de
construir consultas.

## API principal

| Función | Uso |
|---|---|
| `ConectarBD()` | Abrir la conexión MySQL configurada. |
| `CargarDatos()` | Leer una tabla con una condición opcional. |
| `Consulta()` | Ejecutar SQL y normalizar el resultado. |
| `EscribirDatos()` | Sobrescribir una tabla. |
| `AgregarDatos()` | Agregar filas a una tabla. |
| `ReemplazarDatos()` | Reemplazar filas por llaves dentro de una transacción. |
| `ConsultaSistema()` | Consultar las bases SQL Server heredadas. |
