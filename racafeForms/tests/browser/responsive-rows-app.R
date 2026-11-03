# Ejecutar desde la raíz del repositorio, con racafeForms instalado.
library(shiny)
form_matrix <- function(location) {
  tagList(lapply(c("compact", "normal", "comfortable"), function(density) {
    tagList(lapply(c("legacy", "auto", "stack", "fixed"), function(mode) {
      tagList(lapply(1:4, function(columns) {
        id <- paste(location, density, mode, columns, sep = "-")
        fields <- lapply(1:4, function(i) racafeForms::racafeInputText(paste0(id, "-", i), "Campo"))
        row <- do.call(racafeForms::racafeFormRow,
          c(fields, list(columns = columns, responsive = if (mode != "legacy") mode)))
        racafeForms::racafeForm(id = id, size = density, variant = "plain", row)
      }))
    }))
  }))
}
mixed_form <- function(id) {
  ns <- NS(id)
  racafeForms::racafeForm(id = paste0(id, "-mixed"), variant = "plain",
    racafeForms::racafeFormRow(columns = 4, responsive = "fixed",
      racafeForms::racafeInputSelect("select", "Catálogo", c("Café" = "cafe", "Cacao" = "cacao"),
                                    multiple = TRUE, ns = ns),
      racafeForms::racafeInputNumeric("number", "Cantidad", ns = ns),
      racafeForms::racafeInputDate("date", "Fecha", ns = ns),
      racafeForms::racafeButton("button", "Consultar", ns = ns)), verbatimTextOutput(ns("values")))
}
ui <- bs4Dash::dashboardPage(
  header = bs4Dash::dashboardHeader(), sidebar = bs4Dash::dashboardSidebar(),
  controlbar = bs4Dash::dashboardControlbar(form_matrix("bar"), mixed_form("bardemo"), id = "controlbar", width = 440),
  body = bs4Dash::dashboardBody(
    racafeForms::racafeFormsDependencies(),
    racafeForms::racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE),
    form_matrix("main"),
    mixed_form("demo")))
server <- function(input, output, session) {
  for (id in c("demo", "bardemo")) moduleServer(id, function(input, output, session) {
    output$values <- renderPrint(list(select = input$select, number = input$number, date = input$date))
  })
}
shinyApp(ui, server)
