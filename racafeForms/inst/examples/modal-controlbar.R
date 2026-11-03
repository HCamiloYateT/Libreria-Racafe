# Prueba del primer modal, incluso sin otros componentes RF en la UI ----
.module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::selectInput(ns("size"), "Tamaño", c("aviso", "subventana3", "ventana")),
    shiny::actionButton(ns("open"), "Abrir modal"), shiny::textOutput(ns("result")))
}
.module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    count <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$open, {
      shiny::showModal(racafeForms::racafeModal("Contenido local", title = "Prueba de primera apertura",
        size = input$size,
        footer = racafeForms::racafeModalFooter(shiny::modalButton("Cancelar"),
          shiny::actionButton(session$ns("confirm"), "Confirmar"))))
    })
    shiny::observeEvent(input$confirm, { count(count() + 1L); shiny::removeModal() })
    output$result <- shiny::renderText(paste("Confirmaciones:", count()))
  })
}
ui <- function(request) {
  preload <- !identical(shiny::parseQueryString(request$QUERY_STRING)$preload, "0")
  shiny::fluidPage(if (preload) racafeForms::racafeFormsDependencies(), .module_ui("case"))
}
server <- function(input, output, session) .module_server("case")
shiny::shinyApp(ui, server)
