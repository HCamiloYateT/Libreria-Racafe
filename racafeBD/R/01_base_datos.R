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
ConectarBD <- function(bd = Sys.getenv("DB_NAME")) {
  con <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = bd,
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    DBMSencoding = Sys.getenv("DB_ENCODING")
  )
  DBI::dbGetQuery(con, "SET NAMES 'utf8'")
  return(con)
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
  if (!is.data.frame(df)) {
    .error_bd("EscribirDatos - `df` no es data.frame", "enviar un data.frame valido")
  }
  if (!is.character(tabla) || length(tabla) != 1 || nchar(tabla) == 0) {
    .error_bd("EscribirDatos - `tabla` no es un nombre valido", "usar un nombre de tabla no vacio")
  }
  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))
  tryCatch(
    DBI::dbWriteTable(con, tabla, df, overwrite = TRUE, row.names = FALSE),
    error = function(e) {
      .error_bd(
        sprintf("EscribirDatos - no se pudo sobrescribir la tabla '%s' (%s)", tabla, conditionMessage(e)),
        "revisar permisos de escritura y estructura de columnas"
      )
    }
  )
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
  if (!is.data.frame(df)) {
    .error_bd("AgregarDatos - `df` no es data.frame", "enviar un data.frame valido")
  }
  if (!is.character(tabla) || length(tabla) != 1 || nchar(tabla) == 0) {
    .error_bd("AgregarDatos - `tabla` no es un nombre valido", "usar un nombre de tabla no vacio")
  }
  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))
  tryCatch(
    DBI::dbWriteTable(con, tabla, df, append = TRUE, row.names = FALSE),
    error = function(e) {
      .error_bd(
        sprintf("AgregarDatos - no se pudieron insertar filas en '%s' (%s)", tabla, conditionMessage(e)),
        "verificar tipos de columnas y permisos de insercion"
      )
    }
  )
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
  if (!is.data.frame(df)) {
    .error_bd("ReemplazarDatos - `df` no es data.frame", "enviar un data.frame valido")
  }
  if (!is.list(llaves) || length(llaves) == 0) {
    .error_bd("ReemplazarDatos - `llaves` no contiene criterios de reemplazo", "enviar una lista nombrada con al menos una llave")
  }
  if (!is.character(tabla) || length(tabla) != 1 || nchar(tabla) == 0) {
    .error_bd("ReemplazarDatos - `tabla` no es un nombre valido", "usar un nombre de tabla no vacio")
  }

  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))

  # Construir clausula WHERE
  condiciones <- mapply(
    function(col, val) sprintf("%s = '%s'", col, val),
    names(llaves), llaves
  )
  where_sql <- paste(condiciones, collapse = " AND ")
  delete_sql <- sprintf("DELETE FROM %s WHERE %s", tabla, where_sql)

  tryCatch(
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, delete_sql)
      DBI::dbWriteTable(con, tabla, df, append = TRUE, row.names = FALSE)
    }),
    error = function(e) {
      .error_bd(
        sprintf("ReemplazarDatos - fallo transaccion en tabla '%s' (%s)", tabla, conditionMessage(e)),
        "validar llaves, permisos y consistencia de tipos en la tabla destino"
      )
    }
  )

  invisible(TRUE)
}


# ---- Lectura de datos ----

#' Recuperar datos de una tabla con filtro opcional
#'
#' @param tabla Nombre de la tabla.
#' @param condicion Cadena SQL para clausula WHERE (sin la palabra WHERE).
#' @return `data.frame` con los resultados.
#' @export
#' @examples
#' \dontrun{
#'   df <- CargarDatos("mi_tabla")
#'   df_filtrado <- CargarDatos("mi_tabla", "x > 10")
#' }
CargarDatos <- function(
    tabla,
    condicion = NULL) {
  con <- ConectarBD()
  on.exit(DBI::dbDisconnect(con))
  consulta <- paste("SELECT * FROM", tabla)
  if (!is.null(condicion)) {
    consulta <- paste(consulta, "WHERE", condicion)
  }
  resultado <- DBI::dbGetQuery(con, consulta)
  nombres <- names(resultado)
  if (length(nombres) > 0) {
    nombres_sin_acentos <- iconv(nombres, from = "UTF-8", to = "ASCII//TRANSLIT")
    nombres_sin_acentos[is.na(nombres_sin_acentos)] <- nombres[is.na(nombres_sin_acentos)]
    names(resultado) <- nombres_sin_acentos
  }
  resultado
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
  if (!is.character(consulta) || length(consulta) != 1 || nchar(consulta) == 0) {
    .error_bd("Consulta - `consulta` no es SQL valido", "enviar una cadena SQL no vacia")
  }
  con <- ConectarBD(bd = bd)
  on.exit(DBI::dbDisconnect(con))
  resultado <- tryCatch(
    DBI::dbGetQuery(con, consulta),
    error = function(e) {
      .error_bd(
        sprintf("Consulta - ejecucion SQL fallo (%s)", conditionMessage(e)),
        "revisar sintaxis SQL y permisos de lectura sobre los objetos consultados"
      )
    }
  )
  janitor::clean_names(resultado)
}


#' Ejecutar consulta en SQL Server con credenciales explicitas
#'
#' Funcion de compatibilidad para conectarse a servidores alternos.
#' Preferir `Consulta()` con variables de entorno para nuevos desarrollos.
#'
#' @param capitalizacion Modo de capitalizacion para columnas de texto:
#' `"ninguna"`, `"mayusculas"`, `"minusculas"` o `"titulo"`.
#' @param bd Nombre de la base de datos.
#' @param query Consulta SQL.
#' @param uid Usuario. Por defecto `Sys.getenv("SYS_UID")`.
#' @param pwd Contrasena. Por defecto `Sys.getenv("SYS_PWD")`.
#' @param server IP del servidor.
#' @param port Puerto TCP.
#' @return `data.frame` con los nombres de columnas originales.
#' @export
ConsultaSistema <- function(
    bd,
    query,
    capitalizacion = c("mayusculas", "ninguna", "minusculas", "titulo"),
    uid    = Sys.getenv("SYS_UID"),
    pwd    = Sys.getenv("SYS_PWD"),
    server = "172.16.19.21",
    port   = 1433) {

  .check_pkg("odbc", "Base de datos")
  capitalizacion <- match.arg(capitalizacion)

  base <- switch(
    bd,
    "syscafe" = "ContabRacafe",
    "cafesys" = "Cafesys",
    "estad"   = "EstadRacafe",
    NA_character_
  )

  if (is.na(base)) {
    .error_bd(
      "ConsultaSistema - base invalida",
      "las bases disponibles son: 'syscafe', 'cafesys' o 'estad'"
    )
  }

  if (!is.character(query) || length(query) != 1 || nchar(query) == 0) {
    .error_bd(
      "ConsultaSistema - `query` no es SQL valido",
      "enviar una cadena SQL no vacia"
    )
  }

  if (nchar(uid) == 0 || nchar(pwd) == 0) {
    .error_bd(
      "ConsultaSistema - faltan credenciales explicitas (uid/pwd)",
      "definir argumentos uid/pwd o variables SYS_UID/SYS_PWD"
    )
  }

  con <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(),
      Driver                 = "ODBC Driver 18 for SQL Server",
      Server                 = server,
      Database               = base,
      uid                    = uid,
      pwd                    = pwd,
      port                   = port,
      TrustServerCertificate = "yes"
    ),
    error = function(e) {
      .error_bd(
        sprintf("ConsultaSistema - no se pudo abrir conexion a '%s/%s:%s' (%s)", server, base, port, conditionMessage(e)),
        "revisar parametros de conexion y estado del servidor SQL"
      )
    }
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tryCatch(
    {
      resultado <- DBI::dbGetQuery(con, query)
      .postprocesar_consulta_sistema(resultado, capitalizacion = capitalizacion)
    },
    error = function(e) {
      .error_bd(
        sprintf("ConsultaSistema - fallo al ejecutar query (%s)", conditionMessage(e)),
        "validar sintaxis SQL y permisos sobre la base de datos solicitada"
      )
    }
  )
}


.postprocesar_consulta_sistema <- function(resultado, capitalizacion = "mayusculas") {
  .es_codigo_numerico <- function(x) {
    if (!is.character(x)) {
      return(FALSE)
    }

    valores_con_dato <- !is.na(x) & nzchar(trimws(x))
    if (!any(valores_con_dato)) {
      return(FALSE)
    }

    all(grepl("^[0-9]+$", trimws(x[valores_con_dato])))
  }

  columnas_char <- vapply(resultado, is.character, logical(1))
  if (any(columnas_char)) {
    resultado[columnas_char] <- lapply(resultado[columnas_char], function(x) {
      if (.es_codigo_numerico(x)) {
        return(trimws(x))
      }

      limpio <- racafeCore::LimpiarCadena(x)
      switch(
        capitalizacion,
        "mayusculas" = toupper(limpio),
        "minusculas" = tolower(limpio),
        "titulo" = tools::toTitleCase(tolower(limpio)),
        limpio
      )
    })
  }

  columnas_fecha <- grepl("fec|fecha", names(resultado), ignore.case = TRUE)
  if (any(columnas_fecha)) {
    resultado[columnas_fecha] <- lapply(resultado[columnas_fecha], function(x) {
      if (inherits(x, "Date")) {
        return(x)
      }
      if (inherits(x, c("POSIXct", "POSIXt"))) {
        return(as.Date(x))
      }

      if (!is.character(x)) {
        return(x)
      }

      convertido <- as.Date(
        x,
        tryFormats = c(
          "%Y-%m-%d",
          "%Y/%m/%d",
          "%d/%m/%Y",
          "%d-%m-%Y",
          "%m/%d/%Y",
          "%d.%m.%Y"
        )
      )
      valores_con_dato <- !is.na(x) & nzchar(trimws(x))
      if (!any(!is.na(convertido[valores_con_dato]))) {
        return(x)
      }
      convertido
    })
  }

  resultado
}
