# ============================================================
# Seccion 01 — Base de datos
# Conexion, lectura y escritura sobre SQL Server via DBI/odbc
# ============================================================


# ---- Conexion a base de datos ----

#' Establecer conexion a la base de datos corporativa
#'
#' Lee credenciales exclusivamente desde variables de entorno.
#' Nunca pasar credenciales como argumentos.
#'
#' @param bd Nombre de la base de datos. Por defecto `Sys.getenv("DB_NAME")`.
#' @param server IP o hostname del servidor. Por defecto `Sys.getenv("DB_SERVER")`.
#' @param port Puerto TCP. Por defecto `1433`.
#' @return Objeto de conexion `DBIConnection`.
#' @export
#' @examples
#' \dontrun{
#'   con <- ConectarBD()
#'   DBI::dbDisconnect(con)
#' }
ConectarBD <- function(
    bd     = Sys.getenv("DB_NAME"),
    server = Sys.getenv("DB_SERVER"),
    port   = as.integer(Sys.getenv("DB_PORT", "1433"))) {

  .check_pkg("odbc", "Base de datos")

  uid <- Sys.getenv("DB_UID")
  pwd <- Sys.getenv("DB_PWD")

  if (nchar(uid) == 0 || nchar(pwd) == 0) {
    stop(
      "Variables de entorno DB_UID y DB_PWD no definidas.",
      call. = FALSE
    )
  }

  DBI::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = server,
    Database = bd,
    UID      = uid,
    PWD      = pwd,
    Port     = port
  )
}


# ---- Escritura de datos ----

#' Sobrescribir una tabla con el contenido de un data.frame
#'
#' @param df `data.frame` con los datos a escribir.
#' @param tabla Nombre de la tabla destino.
#' @param bd Nombre de la base de datos (env var por defecto).
#' @return Invisible `TRUE` si exitoso.
#' @export
#' @examples
#' \dontrun{
#'   df <- data.frame(x = 1:3, y = letters[1:3])
#'   EscribirDatos(df, "mi_tabla")
#' }
EscribirDatos <- function(df, tabla, bd = Sys.getenv("DB_NAME")) {
  stopifnot(is.data.frame(df), nchar(tabla) > 0)
  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, tabla, df, overwrite = TRUE, row.names = FALSE)
  invisible(TRUE)
}


#' Agregar filas a una tabla existente
#'
#' @param df `data.frame` con filas a agregar.
#' @param tabla Nombre de la tabla destino.
#' @param bd Nombre de la base de datos.
#' @return Invisible `TRUE` si exitoso.
#' @export
AgregarDatos <- function(df, tabla, bd = Sys.getenv("DB_NAME")) {
  stopifnot(is.data.frame(df), nchar(tabla) > 0)
  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, tabla, df, append = TRUE, row.names = FALSE)
  invisible(TRUE)
}


#' Reemplazar registros en una tabla segun llaves
#'
#' Elimina filas que coincidan con las llaves especificadas e inserta
#' los nuevos datos. Operacion atomica via transaccion.
#'
#' @param df `data.frame` con los nuevos datos.
#' @param tabla Nombre de la tabla.
#' @param llaves Lista nombrada con pares columna = valor para el DELETE.
#' @param bd Nombre de la base de datos.
#' @return Invisible `TRUE` si exitoso.
#' @export
#' @examples
#' \dontrun{
#'   df <- data.frame(id = 1, fecha = as.Date("2024-01-01"), valor = 100)
#'   ReemplazarDatos(df, "mi_tabla", list(id = 1, fecha = "2024-01-01"))
#' }
ReemplazarDatos <- function(df, tabla, llaves, bd = Sys.getenv("DB_NAME")) {
  stopifnot(is.data.frame(df), is.list(llaves), length(llaves) > 0)

  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))

  # Construir clausula WHERE
  condiciones <- mapply(
    function(col, val) sprintf("%s = '%s'", col, val),
    names(llaves), llaves
  )
  where_sql <- paste(condiciones, collapse = " AND ")
  delete_sql <- sprintf("DELETE FROM %s WHERE %s", tabla, where_sql)

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, delete_sql)
    DBI::dbWriteTable(con, tabla, df, append = TRUE, row.names = FALSE)
  })

  invisible(TRUE)
}


# ---- Lectura de datos ----

#' Recuperar datos de una tabla con filtro opcional
#'
#' @param tabla Nombre de la tabla.
#' @param condicion Cadena SQL para clausula WHERE (sin la palabra WHERE).
#' @param bd Nombre de la base de datos.
#' @return `data.frame` con los resultados.
#' @export
#' @examples
#' \dontrun{
#'   df <- CargarDatos("mi_tabla")
#'   df_filtrado <- CargarDatos("mi_tabla", "x > 10")
#' }
CargarDatos <- function(
    tabla,
    condicion = NULL,
    bd = Sys.getenv("DB_NAME")) {

  sql <- if (!is.null(condicion) && nchar(condicion) > 0) {
    sprintf("SELECT * FROM %s WHERE %s", tabla, condicion)
  } else {
    sprintf("SELECT * FROM %s", tabla)
  }

  Consulta(sql, bd = bd)
}


#' Ejecutar una consulta SQL arbitraria
#'
#' @param consulta Cadena SQL completa.
#' @param bd Nombre de la base de datos.
#' @return `data.frame` con los resultados, columnas limpias con janitor.
#' @export
#' @examples
#' \dontrun{
#'   resultado <- Consulta("SELECT COUNT(*) AS n FROM mi_tabla")
#' }
Consulta <- function(consulta, bd = Sys.getenv("DB_NAME")) {
  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))
  resultado <- DBI::dbGetQuery(con, consulta)
  janitor::clean_names(resultado)
}


#' Ejecutar consulta en SQL Server con credenciales explicitas
#'
#' Funcion de compatibilidad para conectarse a servidores alternos.
#' Preferir `Consulta()` con variables de entorno para nuevos desarrollos.
#'
#' @param bd Nombre de la base de datos.
#' @param query Consulta SQL.
#' @param uid Usuario. Por defecto `Sys.getenv("DB_UID")`.
#' @param pwd Contrasena. Por defecto `Sys.getenv("DB_PWD")`.
#' @param server IP del servidor.
#' @param port Puerto TCP.
#' @return `data.frame` con columnas limpias.
#' @export
ConsultaSistema <- function(
    bd,
    query,
    uid    = Sys.getenv("DB_UID"),
    pwd    = Sys.getenv("DB_PWD"),
    server = "172.16.19.21",
    port   = 1433) {

  .check_pkg("odbc", "Base de datos")

  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = server,
    Database = bd,
    UID      = uid,
    PWD      = pwd,
    Port     = port
  )
  on.exit(DBI::dbDisconnect(con))

  resultado <- DBI::dbGetQuery(con, query)
  janitor::clean_names(resultado)
}
