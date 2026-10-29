# Demo modular: tareas bloqueantes y objetos waiter ----
.loader_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::wellPanel(
    shiny::h4(paste("Módulo", id)),
    shiny::actionButton(ns("recargar"), "Actualizar (3 segundos)"),
    shiny::actionButton(ns("calcular"), "Calcular (3 segundos)"),
    shiny::actionButton(ns("fallar"), "Probar error controlado"),
    shiny::textOutput(ns("resultado")),
    shiny::tableOutput(ns("tabla")),
    shiny::fileInput(ns("archivo"), "Archivo de prueba (XLSX o CSV)", accept = c(".xlsx", ".csv")),
    lapply(c("consultar", "guardar", "importar", "validar", "enviar", "generar", "descargar",
      "eliminar", "sincronizar"), function(type) shiny::actionButton(ns(type), type)),
    shiny::downloadButton(ns("pdf"), "Descargar PDF preparado"),
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
    datos <- shiny::reactiveVal(data.frame(registro = 1:2, valor = c(10, 20)))
    documento <- shiny::reactiveVal(NULL)
    loader <- racafeShiny::racafeLoader(session, "calculando", id = "panel")
    output$resultado <- shiny::renderText(resultado())
    output$tabla <- shiny::renderTable(datos())
    notify <- function(text, type = "success") {
      if (requireNamespace("racafeForms", quietly = TRUE)) {
        racafeForms::racafeNotify(session, text, type = type, duration = 5)
      } else shiny::showNotification(text, type = if (type == "error") "error" else "message")
    }
    create_pdf <- function() {
      previous <- documento()
      if (!is.null(previous)) unlink(previous)
      path <- tempfile(fileext = ".pdf")
      grDevices::pdf(path)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::plot.new()
      graphics::text(.5, .5, "Documento de prueba Racafe")
      documento(path)
    }
    session$onSessionEnded(function() {
      path <- shiny::isolate(documento())
      if (!is.null(path)) unlink(path)
    })
    operations <- c("consultar", "guardar", "importar", "validar", "enviar", "generar", "descargar",
                    "eliminar", "sincronizar")
    lapply(operations, function(type) {
      shiny::observeEvent(input[[type]], {
        tryCatch({
          Sys.sleep(.5) # Simulación local; no se consultan servicios ni bases de datos.
          switch(type,
            consultar = resultado(paste("Consulta:", nrow(datos()), "filas")),
            guardar = {
              table <- datos()
              table[nrow(table) + 1L, ] <- c(nrow(table) + 1L, 30)
              datos(table)
              resultado("Registro guardado en memoria")
            },
            validar = {
              file <- input$archivo
              if (is.null(file) || file$size == 0 || !file.exists(file$datapath)) {
                stop("Seleccione un archivo no vacío")
              }
              if (!tolower(tools::file_ext(file$name)) %in% c("xlsx", "csv")) stop("Formato no permitido")
              resultado("Archivo validado; valide también su contenido antes de usarlo")
            },
            importar = {
              file <- input$archivo
              if (is.null(file)) stop("Seleccione Excel o CSV")
              extension <- tolower(tools::file_ext(file$name))
              if (extension == "xlsx") {
                if (!requireNamespace("readxl", quietly = TRUE)) stop("Instale readxl para importar Excel")
                table <- as.data.frame(readxl::read_xlsx(file$datapath))
              } else if (extension == "csv") {
                table <- utils::read.csv(file$datapath)
              } else stop("Formato no permitido")
              datos(table)
              resultado(paste("Archivo importado:", nrow(table), "filas"))
            },
            enviar = resultado("Correo simulado; no se envió ningún mensaje"),
            generar = { create_pdf(); resultado("PDF generado") },
            descargar = { create_pdf(); resultado("Descarga preparada; use el botón PDF") },
            eliminar = { datos(utils::head(datos(), -1L)); resultado("Fila eliminada solo en memoria") },
            sincronizar = resultado("Sincronización simulada; sin servicios externos")
          )
          notify(paste("Finalizó:", type))
        }, error = function(error) {
          resultado(paste("Error controlado:", conditionMessage(error)))
          notify("No se completó la operación", "error")
        })
      }, ignoreInit = TRUE)
    })
    output$pdf <- shiny::downloadHandler(filename = "racafe-prueba.pdf", content = function(file) {
      shiny::req(documento())
      file.copy(documento(), file, overwrite = TRUE)
    })
    shiny::observeEvent(input$recargar, {
      Sys.sleep(3) # Bloqueo intencional para comprobar que el navegador ya muestra el loader.
      table <- datos()
      if ("valor" %in% names(table) && is.numeric(table$valor)) table$valor <- table$valor + 1
      datos(table)
      resultado("Actualizado")
      notify("Tabla actualizada")
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
        notify("Error de demostración", "error")
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
  racafeShiny::racafeLoaderUI(startup = FALSE, busy = FALSE, triggers = stats::setNames(
    unlist(lapply(c("uno", "dos"), function(id) paste0(id, "-", c("consultar", "guardar", "importar",
      "validar", "enviar", "generar", "descargar", "eliminar", "sincronizar")))),
    rep(c("consultar", "guardar", "importar", "validar", "enviar", "generar", "descargar",
          "eliminar", "sincronizar"), 2))),
  .loader_ui("uno"), .loader_ui("dos"))
if (identical(Sys.getenv("RACAFE_LOADER_BS4DASH"), "1")) {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "Loaders"),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    body = bs4Dash::dashboardBody(.body),
    preloader = if (identical(Sys.getenv("RACAFE_LOADER_PRELOADER"), "1")) racafeShiny::racafePreloader() else NULL)
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
