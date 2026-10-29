# Demo nativa: RACAFE_FILES_BS4DASH=0 usa Shiny; el valor predeterminado usa bs4Dash ----
.files_ui <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafeForm(
    shiny::h3(paste("Archivos y mensajes", id)),
    racafeForms::racafeFormRow(
      racafeForms::racafeInputFile("xlsx", "Archivo XLSX", ".xlsx", max_size = 1048576, ns = ns),
      racafeForms::racafeInputFile("pdf", "Archivo PDF", ".pdf", max_size = 1048576, ns = ns)),
    racafeForms::racafeFormRow(
      racafeForms::racafeInputFile("varios", "Varios archivos", c(".csv", ".xlsx", ".pdf"), multiple = TRUE,
                                   max_size = 1048576, help = "Validar contenido CSV, XLSX o PDF antes de procesar.",
                                   ns = ns),
      racafeForms::racafeInputFile("limitado", "CSV de hasta 32 bytes", ".csv", max_size = 32,
                                   size = "normal", ns = ns)),
    racafeForms::racafeInputFile("deshabilitado", "Campo deshabilitado", disabled = TRUE, ns = ns),
    racafeForms::racafeInputText("edicion", "La edición continúa durante las notificaciones", ns = ns),
    shiny::textOutput(ns("texto")), shiny::verbatimTextOutput(ns("archivos")),
    racafeForms::racafeActionBar(left = lapply(c("info", "success", "warning", "error"), function(type) {
      racafeForms::racafeButton(type, type, ns = ns)
    })),
    racafeForms::racafeActionBar(left = shiny::tagList(
      racafeForms::racafeButton("persistente", "Persistente", ns = ns),
      racafeForms::racafeButton("actualizar", "Actualizar persistente", ns = ns),
      racafeForms::racafeButton("retirar", "Retirar", ns = ns)),
      right = racafeForms::racafeButton("limpiar", "Limpiar mensajes", ns = ns)),
    racafeForms::racafeActionBar(left = shiny::tagList(
      racafeForms::racafeButton("temporal", "Un segundo", ns = ns),
      racafeForms::racafeButton("sin_cierre", "Sin cierre manual", ns = ns),
      racafeForms::racafeButton("inseguro", "Texto escapado", ns = ns)),
      right = shiny::tagList(racafeForms::racafeButton("confirmar", "Confirmar", ns = ns),
                            racafeForms::racafeButton("alerta", "Alerta", ns = ns),
                            racafeForms::racafeButton("modal", "Modal", ns = ns))),
    shiny::textOutput(ns("decisiones")), shiny::textOutput(ns("id_notificacion")))
}
.files_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    notices <- shiny::reactiveVal(character())
    persistent <- shiny::reactiveVal(NULL)
    decisions <- shiny::reactiveVal(0L)
    last_id <- shiny::reactiveVal("")
    rules <- list(xlsx = list(accept = ".xlsx", max_size = 1048576),
                  pdf = list(accept = ".pdf", max_size = 1048576),
                  varios = list(accept = c(".csv", ".xlsx", ".pdf"), max_size = 1048576),
                  limitado = list(accept = ".csv", max_size = 32))
    remember <- function(id) { notices(unique(c(notices(), id))); last_id(id); id }
    output$archivos <- shiny::renderText(paste(vapply(names(rules), function(name) {
      file <- input[[name]]
      if (is.null(file)) return(paste(name, "pendiente"))
      result <- do.call(racafeForms::racafeValidateInputFile, c(list(files = file), rules[[name]]))
      paste(name, "n:", nrow(file), "valid:", result$valid, "names:", paste(file$name, collapse = ","),
            "sizes:", paste(file$size, collapse = ","), "types:", paste(file$type, collapse = ","),
            "columns:", paste(names(file), collapse = ","), "readable:", all(file.exists(file$datapath)),
            "errors:", paste(result$errors, collapse = " "))
    }, character(1)), collapse = "\n"))
    for (type in c("info", "success", "warning", "error")) local({
      selected <- type
      shiny::observeEvent(input[[selected]], remember(racafeForms::racafeNotify(session,
        paste("Notificación", selected), "Mensaje no bloqueante", type = selected, id = selected, duration = NULL)))
    })
    shiny::observeEvent(input$persistente, persistent(remember(racafeForms::racafeNotify(session,
      "Persistente", "Permanece hasta retirarla", id = "persistente", duration = NULL))))
    shiny::observeEvent(input$actualizar, {
      id <- if (is.null(persistent())) "persistente" else persistent()
      persistent(remember(racafeForms::racafeNotify(session, "Actualizada", "Misma operación", type = "success",
                                                    id = id, duration = NULL)))
    })
    shiny::observeEvent(input$retirar, if (!is.null(persistent())) racafeForms::racafeRemoveNotify(persistent()))
    shiny::observeEvent(input$limpiar, lapply(notices(), racafeForms::racafeRemoveNotify))
    shiny::observeEvent(input$temporal, remember(racafeForms::racafeNotify(session, "Temporal", duration = 1)))
    shiny::observeEvent(input$sin_cierre, remember(racafeForms::racafeNotify(session, "Sin cierre",
                                                                           close_button = FALSE, duration = NULL)))
    shiny::observeEvent(input$inseguro, remember(racafeForms::racafeNotify(session,
      htmltools::HTML('<img src=x onerror="alert(1)">'), htmltools::HTML('<script>alert(1)</script>'),
      duration = NULL)))
    shiny::observeEvent(input$confirmar, racafeForms::racafeConfirm(session, "decision", "Confirmar acción",
      "Solo confirmar incrementa el contador.", ns = session$ns))
    shiny::observeEvent(input$decision, if (isTRUE(input$decision)) decisions(decisions() + 1L))
    shiny::observeEvent(input$alerta,
      racafeForms::racafeAlert(session, "Error", "Alerta de atención", "error"))
    shiny::observeEvent(input$modal, shiny::showModal(racafeForms::racafeModal("Modal nativo",
      title = "Detalle", easy_close = TRUE), session = session))
    output$texto <- shiny::renderText(paste("texto:", input$edicion))
    output$decisiones <- shiny::renderText(paste("confirmadas:", decisions()))
    output$id_notificacion <- shiny::renderText(last_id())
  })
}
content <- shiny::tagList(.files_ui("uno"), .files_ui("dos"), shiny::actionButton("nativa", "Notificación ajena"))
shared <- Sys.getenv("RACAFE_SHARED_PATH")
assets <- if (nzchar(shared)) racafeForms::racafeSharedAssets(shared) else NULL
if (!identical(Sys.getenv("RACAFE_FILES_BS4DASH"), "0")) {
  if (!requireNamespace("bs4Dash", quietly = TRUE)) stop("Instale bs4Dash para este modo.")
  ui <- shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"),
    bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(), sidebar = bs4Dash::dashboardSidebar(),
                           body = bs4Dash::dashboardBody(assets, content)))
} else ui <- shiny::fluidPage(assets, content)
server <- function(input, output, session) {
  .files_server("uno")
  .files_server("dos")
  shiny::observeEvent(input$nativa, shiny::showNotification("Notificación nativa sin cambios", duration = NULL,
                                                          id = "ajena", session = session))
}
shiny::shinyApp(ui, server)
