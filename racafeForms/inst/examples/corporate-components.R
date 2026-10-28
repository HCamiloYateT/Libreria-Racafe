# Recursos e interfaces ----
# Configure RACAFE_SHARED_PATH o options(racafeForms.shared_path = "vendor/Compartido").
# RACAFE_DEMO_BS4DASH=1 permite probar la misma interfaz con bs4Dash, si está instalado.
if (!requireNamespace("reactable", quietly = TRUE)) stop("Instale reactable para ejecutar esta demo.")
assets <- racafeForms::racafeSharedAssets(version = "1.1.0")
logo <- paste0(attr(assets, "image_url"), "/logo.png")

.components_ui <- function(id) {
  ns <- shiny::NS(id)
  racafeForms::racafePanel(paste("Instancia", id),
    racafeForms::racafeForm(id = "form", ns = ns, unstyled = TRUE,
      racafeForms::racafeFormRow(
        racafeForms::racafeInputText("nombre", "Nombre", value = "Inicial", ns = ns,
          choices = c("Empresa A", "Empresa B"), autocomplete = "organization", help = "Catálogo opcional"),
        racafeForms::racafeInputSelect("catalogo", "Catálogo", c(Primero = "a", Segundo = "b", Tercero = "c"),
          selected = "a", multiple = TRUE, ns = ns)),
      racafeForms::racafeFormRow(
        racafeForms::racafeInputNumeric("monto", "Monto", 1200, format = "cop", ns = ns),
        racafeForms::racafeInputDate("fecha", "Fecha", value = "2026-10-15", ns = ns)),
      racafeForms::racafeInputDateRange("rango", "Rango", start = "2026-10-01", end = "2026-10-15", ns = ns),
      racafeForms::racafeInputText("requerido", "Campo requerido", required = TRUE,
                                  error = "Complete este campo", ns = ns),
      racafeForms::racafeInputText("bloqueado", "Deshabilitado", value = "No editable", disabled = TRUE, ns = ns),
      racafeForms::racafeInputReadOnly("referencia", "Referencia", "REF-001", copyable = TRUE, ns = ns),
      racafeForms::racafeInputReadOnly("notas", "Notas", "Primera línea\nSegunda línea", multiline = TRUE, ns = ns),
      racafeForms::racafeInputSwitch("activo", "Activo", value = TRUE, ns = ns),
      racafeForms::racafeFormFooter(
        left = racafeForms::racafeButton("reset", "Restablecer", ns = ns),
        right = shiny::tagList(racafeForms::racafeButton("clear", "Limpiar", ns = ns),
                              racafeForms::racafeButtonDownload("archivo", "Descargar", ns = ns),
                              racafeForms::racafeButtonDownload("sin_icono", "Sin icono", icon = NULL, ns = ns)))),
    racafeForms::racafeActionBar(
      left = racafeForms::racafeCopyButton("copiar", "Copiar texto", text = "Texto de ejemplo", ns = ns),
      center = racafeForms::racafeButton("copiar_servidor", "Copiar desde R", ns = ns),
      right = racafeForms::racafeCopyButton("copiar_imagen", "Copiar PNG", image = logo, ns = ns)),
    racafeForms::racafeActionBar(
      left = racafeForms::racafeButton("imagen_servidor", "Copiar PNG desde R", ns = ns),
      right = racafeForms::racafeButton("dinamico", "Añadir campo dinámico", ns = ns)),
    reactable::reactableOutput(ns("tabla")),
    shiny::uiOutput(ns("nuevo")), shiny::textOutput(ns("valores")), shiny::textOutput(ns("copia")),
    shiny::textOutput(ns("reset_result")))
}

.components_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive({
      tabla <- data.frame(codigo = c("a", "b", "c"), nombre = c("Primero", "Segundo", "Tercero"),
                          estado = c("Disponible", "Pendiente", "Revisar"),
                          variante = c("success", "warning", "danger"),
                          fecha = as.Date(c("2026-10-02", "2026-10-08", "2026-10-14")))
      if (length(input$catalogo)) tabla <- tabla[tabla$codigo %in% input$catalogo, , drop = FALSE]
      if (length(input$rango) == 2L && !anyNA(input$rango)) {
        tabla <- tabla[tabla$fecha >= input$rango[[1]] & tabla$fecha <= input$rango[[2]], , drop = FALSE]
      }
      tabla
    })
    output$tabla <- reactable::renderReactable({
      tabla <- datos()
      reactable::reactable(tabla, columns = list(codigo = reactable::colDef(show = FALSE),
        variante = reactable::colDef(show = FALSE), estado = reactable::colDef(cell = function(value, index) {
          racafeForms::racafeStatusBadge(value, variant = tabla$variante[[index]])
        })), compact = TRUE, pagination = FALSE)
    })
    output$sin_icono <- shiny::downloadHandler(filename = function() "sin-icono.txt",
      content = function(file) writeLines("Descarga sin icono", file))
    output$archivo <- shiny::downloadHandler(filename = function() "ejemplo.txt",
      content = function(file) writeLines("Descarga nativa", file))
    shiny::observeEvent(input$reset, racafeForms::racafeResetForm(session, "form"))
    shiny::observeEvent(input$clear, racafeForms::racafeResetForm(session, "form", clear = TRUE))
    shiny::observeEvent(input$copiar_servidor, racafeForms::racafeCopyText(session, "texto_r", "Texto desde R"))
    shiny::observeEvent(input$imagen_servidor, racafeForms::racafeCopyImage(session, "imagen_r", logo))
    shiny::observeEvent(input$dinamico, {
      output$nuevo <- shiny::renderUI(racafeForms::racafeInputReadOnly("extra", "Dinámico", "Nuevo valor",
                                                                    copyable = TRUE, ns = session$ns))
    })
    output$valores <- shiny::renderText(paste("nombre:", input$nombre,
      "catalogo:", paste(input$catalogo, collapse = ","),
      "monto:", input$monto, "fecha:", input$fecha, "activo:", input$activo))
    output$copia <- shiny::renderText({
      values <- list(boton = input$copiar_copy_result, readonly = input$referencia_copy_copy_result,
                     servidor = input$texto_r_copy_result, imagen = input$copiar_imagen_copy_result,
                     imagen_r = input$imagen_r_copy_result, dinamico = input$extra_copy_copy_result)
      paste(vapply(names(values), function(name) {
        result <- values[[name]]
        paste(name, if (is.null(result)) "pendiente" else paste(result$success, result$error, result$request_id))
      }, character(1)), collapse = " | ")
    })
    output$reset_result <- shiny::renderText({
      result <- input$form_reset_result
      if (!is.null(result)) paste("reset:", result$success, "count:", result$count, "failures:", result$failures)
    })
  })
}

# Catálogo de componentes ----
content <- shiny::tagList(
  shiny::tags$img(src = logo, alt = "Racafé", width = 180),
  shiny::h2("Componentes corporativos"),
  lapply(1:4, function(columns) racafeForms::racafeMetricGrid(columns = columns,
    lapply(seq_len(columns), function(value) racafeForms::racafeMetric(value, paste("Indicador", value))))),
  racafeForms::racafeInfoGrid(columns = 4,
    racafeForms::racafeInfoItem("Referencia", "Ejemplo"), racafeForms::racafeInfoItem("Vigencia", "30 días"),
    racafeForms::racafeInfoItem("Estado", "Disponible"), racafeForms::racafeInfoItem("Dato vacío", NULL)),
  racafeForms::racafeMetricGrid(columns = 3, lapply(c("default", "muted", "highlighted"), function(variant) {
    racafeForms::racafePanel(variant, "Panel sin altura mínima", variant = variant,
                             padding = switch(variant, default = "compact", muted = "normal", "comfortable"))
  })),
  racafeForms::racafeMetricGrid(columns = 3, lapply(c("default", "muted", "highlighted"), function(variant) {
    racafeForms::racafeCard(variant, "Tarjeta informativa sin eventos de clic.", variant = variant)
  })),
  racafeForms::racafeActionBar(left = lapply(c("xxs", "xs", "sm", "md", "lg"), function(size) {
    racafeForms::racafeButton(paste0("size_", size), size, size = size)
  })),
  racafeForms::racafeMetricGrid(columns = 3, lapply(c("compact", "normal", "comfortable"), function(size) {
    racafeForms::racafeForm(racafeForms::racafeFormRow(
      racafeForms::racafeInputText(paste0("density_", size), "Texto"),
      racafeForms::racafeInputSelect(paste0("density_select_", size), "Lista", c("Uno", "Dos")),
      racafeForms::racafeInputDateRange(paste0("density_range_", size), "Rango"), columns = 3), size = size)
  })),
  racafeForms::racafePanel("Campos independientes",
    racafeForms::racafeFormRow(
      racafeForms::racafeInputText("standalone_text", "Texto"),
      racafeForms::racafeInputSelect("standalone_select", "Lista", c("Uno", "Dos")),
      racafeForms::racafeInputDateRange("standalone_range", "Rango"), columns = 3)),
  racafeForms::racafePanel("Badges semánticos",
    lapply(c("success", "warning", "danger", "info", "neutral"), function(variant) {
      racafeForms::racafeStatusBadge(variant, variant = variant)
    })),
  racafeForms::racafeMetricGrid(columns = 3, lapply(c("compact", "normal", "comfortable"), function(padding) {
    racafeForms::racafeMetric(42, paste("Densidad", padding), detail = "Nota opcional", padding = padding)
  })),
  racafeForms::racafeMetricGrid(columns = 2,
    racafeForms::racafeCard("Tarjeta breve", "Contenido corto", padding = "compact",
      header = racafeForms::racafeStatusBadge("Disponible", "success"), footer = "Pie opcional"),
    racafeForms::racafeCard("Tarjeta extensa", lapply(seq_len(6), function(i) shiny::p(paste("Detalle", i))))),
  racafeForms::racafeCard("Tarjeta interactiva", "Activación explícita con clic, Enter o espacio.",
                         clickable = TRUE, id = "tarjeta"), shiny::textOutput("tarjeta_result"),
  lapply(c("info", "success", "warning", "error", "loading", "empty"), function(type) {
    racafeForms::racafeFormMessage(paste("Estado", type), type = type)
  }),
  racafeForms::racafeFormMessage("Mensaje para el usuario", "error",
                                details = "Detalle técnico explícito y controlado"),
  racafeForms::racafeActionBar(left = racafeForms::racafeButton("show_loading", "Alerta de carga"),
    center = racafeForms::racafeButton("show_empty", "Alerta vacía"),
    right = racafeForms::racafeButton("show_error", "Error con detalle controlado")),
  racafeForms::racafePanel("Modales",
    shiny::selectInput("modal_size", "Tamaño", c("ventana", paste0("subventana", 1:5), "aviso")),
    shiny::checkboxInput("modal_header", "Encabezado", TRUE),
    shiny::checkboxInput("modal_footer", "Pie", TRUE),
    shiny::checkboxInput("modal_close", "Cerrar con Escape o clic fuera", FALSE),
    racafeForms::racafeButton("modal", "Abrir modal"), shiny::textOutput("modal_selected")),
  racafeForms::racafeInfoGrid(.components_ui("uno"), .components_ui("dos"), columns = 2))

if (identical(Sys.getenv("RACAFE_DEMO_BS4DASH"), "1")) {
  if (!requireNamespace("bs4Dash", quietly = TRUE)) stop("Instale bs4Dash para este modo de demostración.")
  ui <- bs4Dash::dashboardPage(header = bs4Dash::dashboardHeader(title = "racafeForms"),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE), body = bs4Dash::dashboardBody(assets, content))
  ui <- shiny::tagList(htmltools::suppressDependencies("ionicons", "googlefonts"), ui)
} else ui <- shiny::fluidPage(assets, content)

server <- function(input, output, session) {
  shiny::observeEvent(input$show_loading, racafeForms::racafeAlert(session, "Estado", "Procesando", "loading"))
  shiny::observeEvent(input$show_empty, racafeForms::racafeAlert(session, "Estado", "Sin información", "empty"))
  shiny::observeEvent(input$show_error, racafeForms::racafeAlert(session, "Error", "Mensaje para el usuario", "error",
                                                              details = "Ejemplo técnico controlado"))
  .components_server("uno")
  .components_server("dos")
  output$modal_selected <- shiny::renderText(paste(input$modal_size, input$modal_header,
                                                   input$modal_footer, input$modal_close))
  output$tarjeta_result <- shiny::renderText(paste("clics:", input$tarjeta))
  shiny::observeEvent(input$modal, {
    shiny::showModal(racafeForms::racafeModal("Contenido genérico del modal",
      racafeForms::racafeStatusBadge("Informativo", "info"), size = input$modal_size,
      title = if (input$modal_header) "Detalle" else NULL,
      footer = if (input$modal_footer) racafeForms::racafeModalFooter() else NULL,
      easy_close = input$modal_close), session = session)
  })
}
shiny::shinyApp(ui, server)
