# ============================================================
# Seccion 01 — Dependencias
# Carga e instalacion de paquetes de forma conveniente
# ============================================================

#' Cargar paquetes instalando los faltantes
#'
#' Verifica si los paquetes solicitados estan instalados; en caso
#' contrario, intenta instalarlos con dependencias y luego cargarlos.
#'
#' @param pkg Vector de caracteres con nombres de paquetes.
#' @return Vector logico nombrado con el resultado de `library()` por paquete.
#' @export
Loadpkg <- function(pkg) {
  if (!is.character(pkg) || length(pkg) == 0 || any(is.na(pkg)) || any(trimws(pkg) == "")) {
    stop("`pkg` debe ser un vector character no vacio, sin NA ni cadenas vacias.", call. = FALSE)
  }

  nuevos <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    install.packages(nuevos, dependencies = TRUE)
  }

  sapply(pkg, library, character.only = TRUE, logical.return = TRUE)
}
