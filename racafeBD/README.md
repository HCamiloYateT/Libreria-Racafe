# racafeBD

Capa de acceso a datos del ecosistema Racafe. Centraliza conexiones, consultas y operaciones de escritura contra bases de datos usando credenciales desde variables de entorno.

## Instalación

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeCore")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeBD")
```

## Variables de entorno

```text
DB_NAME=syscafe
DB_HOST=servidor_o_ip
DB_PORT=3306
DB_USER=usuario
DB_PASSWORD=contrasena
DB_ENCODING=utf8
SYS_UID=usuario_sistema
SYS_PWD=contrasena_sistema
```

## Funciones disponibles

- `ConectarBD()`: abre una conexión con las credenciales configuradas.
- `CargarDatos()`: lee una tabla completa o filtrada con condición SQL.
- `Consulta()`: ejecuta una consulta SQL arbitraria y limpia nombres de columnas.
- `ConsultaSistema()`: consulta un servidor alternativo/sistema histórico.
- `EscribirDatos()`: sobrescribe una tabla con un `data.frame`.
- `AgregarDatos()`: agrega filas a una tabla existente.
- `ReemplazarDatos()`: elimina registros por llaves e inserta los datos nuevos en una transacción.

## Uso

```r
library(racafeBD)

ventas <- CargarDatos("fact_ventas")
ventas_2026 <- CargarDatos("fact_ventas", "anio = 2026")
top10 <- Consulta("SELECT * FROM fact_ventas ORDER BY valor DESC LIMIT 10")

EscribirDatos(df_resumen, "reporte_diario")
AgregarDatos(df_nuevos, "reporte_diario")
ReemplazarDatos(df_corregido, "fact_ventas", llaves = list(id = 42, fecha = "2026-07-08"))

df_legacy <- ConsultaSistema("syscafe", "SELECT * FROM tabla_legacy", server = "172.16.19.10")
```

## Documentación

```r
?ConectarBD
?ConsultaSistema
?ReemplazarDatos
```
