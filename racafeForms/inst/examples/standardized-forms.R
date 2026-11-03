# Formulario compartido: la UI también declara su esquema de validación ----
.form_ui <- function(id, extra = FALSE, ns = shiny::NS(id)) {
  racafeForms::racafeForm(id = "form", ns = ns, size = "compact", unstyled = TRUE,
    racafeForms::racafeFormGroup(title = shiny::tags$h2("Datos de la solicitud"),
      racafeForms::racafeFormRow(
        racafeForms::racafeInputText("nombre", shiny::tags$strong("Nombre"), required = TRUE, ns = ns),
        racafeForms::racafeInputNumeric("importe", "Importe", value = NA_real_, min = 0, max = 10000,
                                        required = TRUE, ns = ns)),
      racafeForms::racafeFormRow(
        racafeForms::racafeInputSelect("categorias", "Categorías", c("A", "B", "C"), multiple = TRUE,
                                      required = TRUE, ns = ns),
        racafeForms::racafeInputTextArea("nota", "Observación", rows = 2, ns = ns))),
    racafeForms::racafeFormGroup(title = "Fechas y preferencias",
      racafeForms::racafeFormRow(
        racafeForms::racafeInputDate("fecha", "Fecha", value = as.Date("2026-11-03"), required = TRUE, ns = ns),
        racafeForms::racafeInputDateRange("periodo", "Periodo", start = "2026-11-03", end = "2026-11-04", ns = ns)),
      racafeForms::racafeInputDateTime("cita", "Fecha y hora", value = "2026-11-03 10:30:00", ns = ns),
      racafeForms::racafeInputSwitch("contactar", "Requiere correo", ns = ns),
      shiny::conditionalPanel("input.contactar === true", ns = ns,
        racafeForms::racafeInputText("correo", "Correo", type = "email", ns = ns)),
      racafeForms::racafeInputRadio("prioridad", "Prioridad", c("Normal", "Urgente"), ns = ns),
      racafeForms::racafeInputCheckbox("aceptar", "Aceptar condiciones de prueba", required = TRUE, ns = ns),
      racafeForms::racafeInputCheckbox("ocupado", "Simular botón ocupado", ns = ns),
      racafeForms::racafeInputReadOnly("usuario", "Usuario", "Usuario de demostración", ns = ns),
      racafeForms::racafeFormGroup(divider = FALSE,
        racafeForms::racafeInputCheckbox("agregar", "Añadir campo dinámico", ns = ns),
        if (extra) racafeForms::racafeInputText("extra", "Referencia adicional", required = TRUE, ns = ns) else {
          shiny::uiOutput(ns("extra_ui"))
        })),
    racafeForms::racafeFormFooter(
      racafeForms::racafeButton("enviar", "Validar y guardar", ns = ns),
      racafeForms::racafeButtonDownload("archivo", "Descargar CSV", validate = TRUE, ns = ns),
      racafeForms::racafeButton("modal", "Abrir modal", variant = "secondary", ns = ns)))
}
.form_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    envios <- shiny::reactiveVal(0L)
    validation <- racafeForms::racafeFormValidation(input,
      form = function() .form_ui(id, extra = isTRUE(input$agregar), ns = session$ns), session = session,
      mode = Sys.getenv("RACAFE_STANDARD_MODE", "inline"),
      required = list(correo = function(values) isTRUE(values$contactar)),
      rules = list(aceptar = function(value, values) if (isTRUE(value)) TRUE else "Debe aceptar las condiciones.",
        correo = function(value, values) {
          if (!isTRUE(values$contactar) || grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", value)) {
            TRUE
          } else "Ingrese un correo válido."
        }))
    output$extra_ui <- shiny::renderUI({
      if (isTRUE(input$agregar)) racafeForms::racafeInputText("extra", "Referencia adicional", required = TRUE,
                                                          ns = session$ns)
    })
    shiny::observeEvent(input$ocupado, {
      racafeForms::racafeUpdateButton(session, "enviar", loading = isTRUE(input$ocupado))
    })
    shiny::observeEvent(input$enviar, {
      if (!validation$validate()) return()
      envios(envios() + 1L)
      racafeForms::racafeUpdateButton(session, "enviar", success = TRUE)
      racafeForms::racafeNotify(session, "Formulario válido", "Guardado solo en memoria.", type = "success")
    })
    racafeForms::racafeDownloadHandler("archivo", validation, filename = paste0(id, ".csv"),
      content = function(file, values) {
        utils::write.csv(data.frame(nombre = values$nombre, importe = values$importe,
                                   correo = if (isTRUE(values$contactar)) values$correo %||% "" else ""),
                         file, row.names = FALSE)
      }, session = session)
    shiny::observeEvent(input$modal, {
      shiny::showModal(shiny::modalDialog(title = "Confirmación de demostración",
        "El modal conserva el footer corporativo.",
        footer = racafeForms::racafeModalFooter()))
    })
    output$estado <- shiny::renderText({
      result <- validation$result()
      paste("Envíos:", envios(), "Intentado:", validation$attempted(),
            "Pendientes:", if (is.null(result)) "ninguno mostrado" else paste(result$pending, collapse = ","))
    })
    output$tipos <- shiny::renderText(paste("switch:", typeof(input$contactar), "importe:", typeof(input$importe),
                                           "fecha-hora:", paste(class(input$cita), collapse = "/"),
                                           "UTC:", if (!is.null(input$cita)) format(input$cita, tz = "UTC")))
  })
}
`%||%` <- function(x, y) if (is.null(x)) y else x
server <- function(input, output, session) {
  .form_server("uno")
  .form_server("dos")
}
.content <- shiny::tagList(
  if (nzchar(Sys.getenv("RACAFE_SHARED_PATH"))) {
    racafeForms::racafeSharedAssets(path = Sys.getenv("RACAFE_SHARED_PATH"))
  },
  .form_ui("uno"), shiny::textOutput("uno-estado"), shiny::textOutput("uno-tipos"))
if (!identical(Sys.getenv("RACAFE_STANDARD_BS4DASH"), "0")) {
  ui <- bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(title = "Formularios Racafé"),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    body = bs4Dash::dashboardBody(.content),
    controlbar = bs4Dash::dashboardControlbar(.form_ui("dos"), shiny::textOutput("dos-estado"),
                                           shiny::textOutput("dos-tipos"), id = "barra", width = 440))
  ui <- shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"), ui)
} else ui <- shiny::fluidPage(.content, shiny::h4("Segundo formulario"), .form_ui("dos"),
                              shiny::textOutput("dos-estado"), shiny::textOutput("dos-tipos"))
shiny::shinyApp(ui, server)
