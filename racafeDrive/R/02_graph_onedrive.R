# ============================================================
# Seccion 02 — Microsoft Graph / OneDrive / SharePoint
# Autenticacion, navegacion y descarga de archivos via Graph API
# ============================================================


# ---- Autenticacion ----

#' Obtener token de acceso para Microsoft Graph
#'
#' Implementa cache en memoria con verificacion de expiracion.
#' El token se renueva automaticamente al expirar.
#'
#' Variables de entorno requeridas:
#' - `GRAPH_TOKEN_URL`: endpoint de token del tenant
#' - `GRAPH_CLIENT_ID`: client ID de la aplicacion Azure AD
#' - `GRAPH_CLIENT_SECRET`: client secret
#'
#' @param force Logico. Si `TRUE`, fuerza renovacion aunque no haya expirado.
#' @return Cadena con el access token.
#' @export
ObtenerTokenAcceso <- function(force = FALSE) {
  ahora <- as.numeric(Sys.time())

  if (!force &&
      exists("token", envir = .token_cache, inherits = FALSE) &&
      exists("expira", envir = .token_cache, inherits = FALSE) &&
      .token_cache$expira > ahora) {
    return(.token_cache$token)
  }

  token_url <- Sys.getenv("GRAPH_TOKEN_URL")
  client_id <- Sys.getenv("GRAPH_CLIENT_ID")
  client_secret <- Sys.getenv("GRAPH_CLIENT_SECRET")

  if (any(nchar(c(token_url, client_id, client_secret)) == 0)) {
    stop(
      "Variables de entorno GRAPH_TOKEN_URL, GRAPH_CLIENT_ID ",
      "y GRAPH_CLIENT_SECRET deben estar definidas.",
      call. = FALSE
    )
  }

  resp <- httr2::request(token_url) |>
    httr2::req_body_form(
      client_id     = client_id,
      client_secret = client_secret,
      scope         = "https://graph.microsoft.com/.default",
      grant_type    = "client_credentials"
    ) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    stop("Error al obtener token: ", httr2::resp_body_string(resp), call. = FALSE)
  }

  datos <- httr2::resp_body_json(resp)
  .token_cache$token  <- datos$access_token
  .token_cache$expira <- ahora + datos$expires_in - 60

  .token_cache$token
}


#' Construir cabeceras HTTP con token Bearer
#'
#' @return Lista de cabeceras HTTP lista para usar con httr2.
#' @export
CabecerasGraph <- function() {
  list(Authorization = paste("Bearer", ObtenerTokenAcceso()))
}


# ---- Identificadores de sitios y drives ----

#' Obtener ID de sitio SharePoint
#'
#' @param hostname Hostname de SharePoint (ej. `"contoso.sharepoint.com"`).
#' @param site_path Ruta del sitio (ej. `"sites/mi-sitio"`).
#' @return Cadena con el site ID.
#' @export
ObtenerIdSite <- function(hostname, site_path) {
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/sites/%s:/%s",
    hostname, site_path
  )
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  resp$id
}


#' Obtener ID del drive de un sitio SharePoint
#'
#' @param site_id ID del sitio obtenido con `ObtenerIdSite()`.
#' @param nombre_drive Nombre del drive. `NULL` devuelve el drive principal.
#' @return Cadena con el drive ID.
#' @export
ObtenerIdDriveSite <- function(site_id, nombre_drive = NULL) {
  url <- sprintf("https://graph.microsoft.com/v1.0/sites/%s/drives", site_id)
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  drives <- resp$value

  if (is.null(nombre_drive)) {
    return(drives[[1]]$id)
  }

  match_drive <- Filter(function(d) d$name == nombre_drive, drives)
  if (length(match_drive) == 0) {
    stop(sprintf("Drive '%s' no encontrado en el sitio.", nombre_drive), call. = FALSE)
  }
  match_drive[[1]]$id
}


#' Obtener ID del OneDrive de un usuario
#'
#' @param usuario UPN o alias del usuario (ej. `"juan.perez"`).
#' @return Cadena con el drive ID.
#' @export
ObtenerIdDrive <- function(usuario) {
  dominio <- Sys.getenv("GRAPH_DOMAIN", "racafe.com")
  upn <- if (grepl("@", usuario)) usuario else paste0(usuario, "@", dominio)

  url <- sprintf("https://graph.microsoft.com/v1.0/users/%s/drive", upn)
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  resp$id
}


# ---- Listado de contenido ----

#' Listar carpetas en la raiz del OneDrive de un usuario
#'
#' @param usuario UPN o alias del usuario.
#' @return `data.frame` con nombre, id y tipo de cada elemento.
#' @export
ListarCarpetas <- function(usuario) {
  drive_id <- ObtenerIdDrive(usuario)
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root/children",
    drive_id
  )
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  .graph_items_a_df(resp$value)
}


#' Obtener ID de una carpeta por nombre en el OneDrive de un usuario
#'
#' @param usuario UPN o alias del usuario.
#' @param nombre_carpeta Nombre de la carpeta.
#' @return Cadena con el folder ID.
#' @export
ObtenerIdCarpeta <- function(usuario, nombre_carpeta) {
  carpetas <- ListarCarpetas(usuario)
  idx <- which(carpetas$nombre == nombre_carpeta)
  if (length(idx) == 0) {
    stop(sprintf("Carpeta '%s' no encontrada.", nombre_carpeta), call. = FALSE)
  }
  carpetas$id[[idx[[1]]]]
}


#' Listar contenido de una carpeta por nombre
#'
#' @param usuario UPN o alias del usuario.
#' @param nombre_carpeta Nombre de la carpeta.
#' @return `data.frame` con metadatos de los elementos.
#' @export
ListarContenidoCarpetaNombre <- function(usuario, nombre_carpeta) {
  carpeta_id <- ObtenerIdCarpeta(usuario, nombre_carpeta)
  ListarContenidoCarpetaId(usuario, carpeta_id)
}


#' Listar contenido de una carpeta por ID
#'
#' @param usuario UPN o alias del usuario.
#' @param carpeta_id ID de la carpeta.
#' @return `data.frame` con metadatos de los elementos.
#' @export
ListarContenidoCarpetaId <- function(usuario, carpeta_id) {
  drive_id <- ObtenerIdDrive(usuario)
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/items/%s/children",
    drive_id, carpeta_id
  )
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  .graph_items_a_df(resp$value)
}


#' Listar contenido de una carpeta de forma recursiva
#'
#' @param usuario UPN o alias del usuario.
#' @param carpeta_id ID de la carpeta raiz.
#' @return Lista anidada con elementos y subcarpetas.
#' @export
ListarContenidoCarpetaRecursivo <- function(usuario, carpeta_id) {
  elementos <- ListarContenidoCarpetaId(usuario, carpeta_id)

  subcarpetas <- elementos[!is.na(elementos$es_carpeta) & elementos$es_carpeta, ]

  resultado <- list(archivos = elementos)
  for (i in seq_len(nrow(subcarpetas))) {
    sub_id <- subcarpetas$id[[i]]
    sub_nombre <- subcarpetas$nombre[[i]]
    resultado[[sub_nombre]] <- ListarContenidoCarpetaRecursivo(usuario, sub_id)
  }
  resultado
}


#' Listar todos los archivos de una carpeta como tibble recursivo
#'
#' @param usuario UPN o alias del usuario.
#' @param carpeta_id ID de la carpeta raiz.
#' @return `tibble` con todos los archivos incluyendo subcarpetas.
#' @export
ListarTodoContenidoCarpeta <- function(usuario, carpeta_id) {
  .listar_recursivo_plano(usuario, carpeta_id, ruta_actual = "")
}


#' Listar archivos en un drive de SharePoint/OneDrive con metadatos
#'
#' @param drive_id ID del drive.
#' @param item_id ID del item raiz. Por defecto `"root"`.
#' @param ruta Ruta acumulada (uso interno recursivo).
#' @param fecha_desde Fecha minima de modificacion para filtrar. Opcional.
#' @return `tibble` con metadatos: nombre, id, ruta, fecha_modificacion, tamano.
#' @export
ListarDriveRecursivo <- function(
    drive_id,
    item_id     = "root",
    ruta        = "",
    fecha_desde = NULL) {

  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/items/%s/children",
    drive_id, item_id
  )
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  items <- resp$value
  if (length(items) == 0) return(tibble::tibble())

  resultado <- purrr::map_dfr(items, function(item) {
    es_carpeta <- !is.null(item$folder)
    ruta_item <- if (nchar(ruta) > 0) {
      paste0(ruta, "/", item$name)
    } else {
      item$name
    }

    if (es_carpeta) {
      ListarDriveRecursivo(drive_id, item$id, ruta_item, fecha_desde)
    } else {
      fecha_mod <- as.POSIXct(
        item$lastModifiedDateTime,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      )
      if (!is.null(fecha_desde) && fecha_mod < as.POSIXct(fecha_desde)) {
        return(tibble::tibble())
      }
      tibble::tibble(
        nombre            = item$name,
        id                = item$id,
        ruta              = ruta_item,
        fecha_modificacion = fecha_mod,
        tamano_bytes      = item$size %||% NA_integer_
      )
    }
  })

  resultado
}


# ---- Descarga y lectura de archivos ----

#' Descargar archivo de OneDrive a un archivo temporal
#'
#' @param archivo_id ID del archivo en OneDrive.
#' @param usuario UPN o alias del usuario.
#' @return Ruta al archivo temporal descargado.
#' @export
DescargarArchivoId <- function(archivo_id, usuario) {
  drive_id <- ObtenerIdDrive(usuario)
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/items/%s/content",
    drive_id, archivo_id
  )

  tmp <- tempfile()
  httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform(path = tmp)

  tmp
}


#' Listar hojas de un Excel alojado en OneDrive
#'
#' @param archivo_id ID del archivo Excel.
#' @param usuario UPN o alias del usuario.
#' @return Vector de caracteres con nombres de hojas.
#' @export
ListarHojasExcelOneDrive <- function(archivo_id, usuario) {
  tmp <- DescargarArchivoId(archivo_id, usuario)
  on.exit(unlink(tmp))
  readxl::excel_sheets(tmp)
}


#' Leer un Excel desde OneDrive con readxl
#'
#' @param archivo_id ID del archivo Excel.
#' @param usuario UPN o alias del usuario.
#' @param ... Argumentos adicionales pasados a `readxl::read_excel`.
#' @return `tibble` con el contenido del archivo.
#' @export
#' @examples
#' \dontrun{
#'   df <- LeerExcelDesdeOneDrive("ABC123", "juan.perez", sheet = "Datos", skip = 1)
#' }
LeerExcelDesdeOneDrive <- function(archivo_id, usuario, ...) {
  tmp <- DescargarArchivoId(archivo_id, usuario)
  on.exit(unlink(tmp))
  readxl::read_excel(tmp, ...)
}


#' Cargar y abrir Excel desde OneDrive como workbook openxlsx2
#'
#' @param usuario UPN o alias del usuario.
#' @param ruta Ruta relativa dentro del OneDrive.
#' @param archivo Nombre del archivo.
#' @return Objeto `wb_workbook` de openxlsx2.
#' @export
CargarExcelDesdeOneDrive <- function(usuario, ruta, archivo) {
  drive_id <- ObtenerIdDrive(usuario)
  ruta_completa <- paste0(ruta, "/", archivo)
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root:/%s:/content",
    drive_id, utils::URLencode(ruta_completa)
  )
  tmp <- tempfile(fileext = ".xlsx")
  httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform(path = tmp)
  openxlsx2::wb_load(tmp)
}


#' Guardar localmente un Excel desde OneDrive
#'
#' @param usuario UPN o alias del usuario.
#' @param ruta Ruta relativa dentro del OneDrive.
#' @param archivo Nombre del archivo origen.
#' @param nombre_salida Nombre del archivo destino (sin extension).
#' @return Invisible ruta al archivo guardado.
#' @export
DescargarExcelDesdeOneDrive <- function(usuario, ruta, archivo, nombre_salida) {
  drive_id <- ObtenerIdDrive(usuario)
  ruta_completa <- paste0(ruta, "/", archivo)
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root:/%s:/content",
    drive_id, utils::URLencode(ruta_completa)
  )
  destino <- paste0(nombre_salida, ".xlsx")
  httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform(path = destino)
  invisible(destino)
}


#' Descargar y leer un Excel desde un drive de SharePoint por ID
#'
#' @param drive_id ID del drive de SharePoint.
#' @param item_id ID del item (archivo) dentro del drive.
#' @param hoja Nombre o indice de hoja. `NULL` lee la primera.
#' @param ... Argumentos adicionales para `readxl::read_excel`.
#' @return `tibble` con el contenido del archivo.
#' @export
CargarExcelSite <- function(drive_id, item_id, hoja = NULL, ...) {
  url <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/items/%s/content",
    drive_id, item_id
  )
  tmp <- tempfile(fileext = ".xlsx")
  httr2::request(url) |>
    httr2::req_headers(!!!CabecerasGraph()) |>
    httr2::req_perform(path = tmp)
  on.exit(unlink(tmp))

  args <- list(path = tmp, ...)
  if (!is.null(hoja)) args$sheet <- hoja

  do.call(readxl::read_excel, args)
}


# ---- Utilidades internas Graph ----

# Convierte lista de items de Graph API a data.frame
.graph_items_a_df <- function(items) {
  if (length(items) == 0) {
    return(data.frame(
      nombre = character(),
      id     = character(),
      es_carpeta = logical(),
      tamano_bytes = integer(),
      stringsAsFactors = FALSE
    ))
  }

  purrr::map_dfr(items, function(item) {
    data.frame(
      nombre       = item$name %||% NA_character_,
      id           = item$id %||% NA_character_,
      es_carpeta   = !is.null(item$folder),
      tamano_bytes = item$size %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  })
}

# Listar recursivamente como tibble plano (uso interno)
.listar_recursivo_plano <- function(usuario, carpeta_id, ruta_actual) {
  elementos <- ListarContenidoCarpetaId(usuario, carpeta_id)
  if (nrow(elementos) == 0) return(tibble::tibble())

  archivos    <- elementos[!elementos$es_carpeta, ]
  subcarpetas <- elementos[elementos$es_carpeta, ]

  archivos$ruta <- ruta_actual

  sub_resultado <- purrr::map_dfr(seq_len(nrow(subcarpetas)), function(i) {
    nueva_ruta <- if (nchar(ruta_actual) > 0) {
      paste0(ruta_actual, "/", subcarpetas$nombre[[i]])
    } else {
      subcarpetas$nombre[[i]]
    }
    .listar_recursivo_plano(usuario, subcarpetas$id[[i]], nueva_ruta)
  })

  dplyr::bind_rows(archivos, sub_resultado)
}
