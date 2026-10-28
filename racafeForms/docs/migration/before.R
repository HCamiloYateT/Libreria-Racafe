# Ejemplo representativo: no procede de los módulos reales de Firma Digital.
crear_ui_manual <- function(id, datos) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(
      ".resumen-manual {border:1px solid #e8e8e8;border-radius:12px;padding:16px;}",
      ".datos-manual {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;}",
      ".acciones-manual {display:flex;justify-content:space-between;gap:8px;flex-wrap:wrap;}",
      ".referencia-manual {display:flex;gap:8px;align-items:center;}"
    ),
    shiny::tags$section(class = "resumen-manual",
      shiny::tags$h3("Información de la solicitud"),
      shiny::tags$div(class = "datos-manual",
        shiny::tags$dl(shiny::tags$dt("Nombre"), shiny::tags$dd(datos$nombre)),
        shiny::tags$dl(shiny::tags$dt("Referencia"), shiny::tags$dd(datos$referencia)),
        shiny::tags$dl(shiny::tags$dt("Vigencia"), shiny::tags$dd(datos$vigencia)),
        shiny::tags$dl(shiny::tags$dt("Estado"), shiny::tags$dd(datos$estado))
      ),
      shiny::tags$div(class = "referencia-manual",
        shiny::tags$label(`for` = ns("enlace"), "Enlace"),
        shiny::tags$input(id = ns("enlace"), value = datos$enlace, readonly = "readonly"),
        shiny::tags$button(id = ns("copiar"), type = "button", "Copiar")
      ),
      shiny::tags$div(class = "acciones-manual",
        shiny::actionButton(ns("correo"), "Preparar correo"),
        shiny::downloadButton(ns("archivo"), "Descargar")
      )
    ),
    shiny::tags$script(shiny::HTML(paste0(
      "document.getElementById('", ns("copiar"), "').addEventListener('click', async function () {",
      "try {",
      "await navigator.clipboard.writeText(document.getElementById('", ns("enlace"), "').value);",
      "Shiny.setInputValue('", ns("copia_resultado"), "', {success:true}, {priority:'event'});",
      "} catch (error) {",
      "Shiny.setInputValue('", ns("copia_resultado"), "', {success:false}, {priority:'event'});",
      "}",
      "});"
    )))
  )
}
