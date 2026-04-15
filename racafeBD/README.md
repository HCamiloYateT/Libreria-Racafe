# racafeBD

Capa de acceso a datos del ecosistema Racafe. Conexiones a SQL Server
con credenciales exclusivamente desde variables de entorno.

## Instalación

```r
remotes::install_github("HCamiloYateT/Racafe/racafeCore")
remotes::install_github("HCamiloYateT/Racafe/racafeBD")
```

## Variables de entorno requeridas

En `.Renviron`:
```
DB_SERVER=172.16.19.21
DB_PORT=1433
DB_NAME=syscafe
DB_UID=mi_usuario
DB_PWD=mi_contrasena
```

## Uso

```r
library(racafeBD)

# Leer
ventas      <- CargarDatos("fact_ventas")
ventas_2024 <- CargarDatos("fact_ventas", "anio = 2024")
top10       <- Consulta("SELECT TOP 10 * FROM fact_ventas ORDER BY valor DESC")

# Escribir
EscribirDatos(df_resumen, "reporte_diario")      # sobrescribe
AgregarDatos(df_nuevos,   "reporte_diario")      # append

# Reemplazar registros especificos
ReemplazarDatos(df_corregido, "fact_ventas",
  llaves = list(id = 42, fecha = "2024-01-15"))

# Servidor alternativo (migracion / consultas historicas)
# Usa SYS_UID/SYS_PWD por defecto (si no se pasan uid/pwd)
df <- ConsultaSistema("syscafe", "SELECT * FROM tabla_legacy",
  server = "172.16.19.10")
```
