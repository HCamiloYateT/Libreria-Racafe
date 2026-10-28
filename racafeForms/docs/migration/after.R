# Ejemplo representativo con Compartido cargado una vez en la UI principal.
crear_ui_compuesta <- function(id, datos) {
  ns <- shiny::NS(id)
  racafeForms::racafePanel("Información de la solicitud",
    racafeForms::racafeInfoGrid(columns = 2,
      racafeForms::racafeInfoItem("Nombre", datos$nombre),
      racafeForms::racafeInfoItem("Referencia", datos$referencia),
      racafeForms::racafeInfoItem("Vigencia", datos$vigencia),
      racafeForms::racafeInfoItem("Estado", datos$estado)),
    racafeForms::racafeInputReadOnly("enlace", "Enlace", datos$enlace, copyable = TRUE, ns = ns),
    footer = racafeForms::racafeActionBar(
      left = racafeForms::racafeButton("correo", "Preparar correo", ns = ns),
      right = racafeForms::racafeButtonDownload("archivo", "Descargar", ns = ns)))
}
