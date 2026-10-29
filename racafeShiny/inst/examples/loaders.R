# Demo modular: tareas bloqueantes y objetos waiter ----
.loader_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::wellPanel(
    shiny::h4(paste("Módulo", id)),
    shiny::actionButton(ns("recargar"), "Actualizar (3 segundos)"),
    shiny::actionButton(ns("calcular"), "Calcular (3 segundos)"),
    shiny::actionButton(ns("fallar"), "Probar error controlado"),
    shiny::textOutput(ns("resultado")),
    shiny::hr(),
    shiny::actionButton(ns("mostrar"), "Mostrar loader local"),
    shiny::actionButton(ns("actualizar"), "Cambiar texto local"),
    shiny::actionButton(ns("ocultar"), "Ocultar loader local"),
    shiny::tags$div(id = ns("panel"), style = "position:relative;min-height:220px;padding:20px;",
      shiny::p("El objeto waiter solo cubre este panel.")))
}
.loader_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    resultado <- shiny::reactiveVal("Pendiente")
    loader <- racafeShiny::racafeLoader(session, "calculando", id = "panel")
    output$resultado <- shiny::renderText(resultado())
    shiny::observeEvent(input$recargar, {
      Sys.sleep(3) # Bloqueo intencional para comprobar que el navegador ya muestra el loader.
      resultado("Actualizado")
    })
    shiny::observeEvent(input$calcular, {
      Sys.sleep(3)
      resultado("Calculado")
    })
    shiny::observeEvent(input$fallar, {
      tryCatch({
        Sys.sleep(1)
        stop("Error de demostración")
      }, error = function(error) {
        resultado("Error controlado; sesión disponible")
      })
    })
    shiny::observeEvent(input$mostrar, loader$show())
    shiny::observeEvent(input$actualizar, {
      loader$update(racafeShiny::racafePreloader("actualizar", text = "Procesando datos…")$html)
    })
    shiny::observeEvent(input$ocultar, loader$hide())
  })
}
.body <- shiny::tagList(
  racafeShiny::racafeLoaderUI(triggers = c(
    actualizar = "uno-recargar", calculando = "uno-calcular", calculando = "uno-fallar",
    actualizar = "dos-recargar", calculando = "dos-calcular", calculando = "dos-fallar")),
  shiny::h3("Loaders corporativos"),
  shiny::textOutput("inicio"),
  shiny::textInput("editable", "Campo para verificar disponibilidad tras finalizar"),
  .loader_ui("uno"), .loader_ui("dos"))
if (identical(Sys.getenv("RACAFE_LOADER_BS4DASH"), "1")) {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "Loaders"),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    body = bs4Dash::dashboardBody(.body))
  ui <- shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"), ui)
} else {
  ui <- shiny::fluidPage(.body)
}
server <- function(input, output, session) {
  output$inicio <- shiny::renderText({
    Sys.sleep(2) # Carga inicial lenta intencional para probar startup.
    "Aplicación lista"
  })
  .loader_server("uno")
  .loader_server("dos")
}
shiny::shinyApp(ui, server)
