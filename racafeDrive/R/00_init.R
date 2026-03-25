# ============================================================
# racafeDrive — Inicializacion del paquete
# ============================================================

# Cache en memoria del token Microsoft Graph (por proceso, por sesion R)
.token_cache <- new.env(parent = emptyenv())

# Helper interno: verificar disponibilidad de paquete opcional
.check_pkg <- function(pkg, modulo = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "El paquete '%s' es requerido%s.\nInstalar con: install.packages('%s')",
      pkg,
      if (!is.null(modulo)) sprintf(" para '%s'", modulo) else "",
      pkg
    ), call. = FALSE)
  }
}

# Helper interno: error homologado para integraciones Graph/OneDrive
.error_graph <- function(causa, accion, funcion = NULL) {
  contexto <- if (!is.null(funcion)) sprintf("%s - %s", funcion, causa) else causa
  stop(
    sprintf("[racafeDrive] %s. Accion sugerida: %s.", contexto, accion),
    call. = FALSE
  )
}

# Helper interno: validar respuesta HTTP Graph y devolver JSON
.graph_resp_json <- function(resp, funcion, accion_default) {
  status <- httr2::resp_status(resp)
  if (status >= 300) {
    detalle <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    .error_graph(
      sprintf("HTTP %s en llamada a Microsoft Graph (%s)", status, detalle),
      accion_default,
      funcion = funcion
    )
  }
  httr2::resp_body_json(resp)
}

# Helper interno: validar respuesta HTTP Graph para descargas
.graph_resp_ok <- function(resp, funcion, accion_default) {
  status <- httr2::resp_status(resp)
  if (status >= 300) {
    detalle <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    .error_graph(
      sprintf("HTTP %s en descarga Graph (%s)", status, detalle),
      accion_default,
      funcion = funcion
    )
  }
  invisible(TRUE)
}
