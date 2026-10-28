# Prueba ejecutable de bindings; solo datos ficticios, nunca contraseñas reales ----
# TEXT_BINDINGS_BS4DASH=1 activa bs4Dash. No requiere Compartido ni JavaScript propio.
moduleUI <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(
    shiny::h3(paste("Módulo", id)),
    lapply(c("text", "email", "tel", "password"), function(type) {
      racafeForms::racafeInputText(type, paste("Campo", type), value = paste0("inicial-", type),
        type = type, ns = ns, required = TRUE, help = "Dato ficticio de prueba", error = "Validación de ejemplo")
    }),
    racafeForms::racafeInputText("readonly", "Solo lectura", "Fijo", type = "email", readonly = TRUE, ns = ns),
    racafeForms::racafeInputText("disabled", "Deshabilitado", "Bloqueado", type = "tel", disabled = TRUE, ns = ns),
    racafeForms::racafeButton("actualizar", "Actualizar desde servidor", ns = ns),
    shiny::verbatimTextOutput(ns("resultado")))
}
moduleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    valores <- shiny::reactive({
      ids <- c("text", "email", "tel", "password", "readonly", "disabled")
      stats::setNames(lapply(ids, function(name) input[[name]]), ids)
    })
    output$resultado <- shiny::renderPrint(valores())
    shiny::observeEvent(input$actualizar, {
      for (type in c("text", "email", "tel", "password")) {
        shiny::updateTextInput(session, type, value = paste0("servidor-", type))
      }
    })
    valores
  })
}
content <- shiny::tagList(shiny::p("Solo datos ficticios: esta prueba muestra también la contraseña de prueba."),
                          moduleUI("uno"), moduleUI("dos"))
if (identical(Sys.getenv("TEXT_BINDINGS_BS4DASH"), "1")) {
  if (!requireNamespace("bs4Dash", quietly = TRUE)) stop("Instale bs4Dash para este modo.")
  ui <- shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"),
    bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(), sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
                           body = bs4Dash::dashboardBody(content)))
} else ui <- shiny::fluidPage(content)
server <- function(input, output, session) {
  moduleServer("uno")
  moduleServer("dos")
}
shiny::shinyApp(ui, server)
