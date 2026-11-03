# Galería autocontenida ----

#' Galería interactiva de componentes corporativos
#' @return Aplicación shiny.appobj creada mediante shinyAppDir. Al imprimirla se inicia la galería.
#' @details Requiere bs4Dash (Suggests). Incluye recursos versionados de Compartido,
#'   sin CRM, bases de datos ni servicios externos. El código se puede copiar y ejecutar
#'   en una sesión nueva; los ejemplos de servidor utilizan moduleServer y namespaces.
#' @export
#' @examples
#' \dontrun{
#' racafeFormsGallery()
#' }
racafeFormsGallery <- function() {
  if (!requireNamespace("bs4Dash", quietly = TRUE)) stop("Instale bs4Dash para ejecutar la galer\u00eda.")
  shiny::shinyAppDir(system.file("examples", "racafeFormsGallery", package = "racafeForms", mustWork = TRUE))
}

# Inventario explícito y comprobable contra las exportaciones ----
.rf_gallery_catalog <- function(include_modules = FALSE) {
  categories <- list(
    Formularios = c("racafeForm", "racafeFormGroup", "racafeFormRow", "racafeFormFooter"),
    Inputs = c("racafeInputText", "racafeInputTextArea", "racafeInputNumeric", "racafeInputSelect",
               "racafeInputDate", "racafeInputDateRange", "racafeInputDateTime", "racafeInputReadOnly",
               "racafeInputFile", "racafeValidateInputFile"),
    "Selecci\u00f3n" = c("racafeInputSwitch", "racafeInputRadio", "racafeInputCheckbox"),
    Botones = c("racafeButton", "racafeButtonSave", "racafeButtonCancel", "racafeButtonDelete", "racafeButtonDownload"),
    Contenedores = c("racafePanel", "racafeCard", "racafeMetric", "racafeMetricGrid"),
    "Informaci\u00f3n" = c("racafeInfoItem", "racafeInfoGrid", "racafeFormMessage", "racafeStatusBadge"),
    Modales = c("racafeModal", "racafeModalFooter"),
    Notificaciones = c("racafeAlert", "racafeConfirm", "racafeNotify", "racafeRemoveNotify"),
    Acciones = c("racafeActionBar", "racafeUpdateSelect", "racafeUpdateNumeric", "racafeUpdateDate",
      "racafeUpdateDateRange", "racafeUpdateSwitch", "racafeUpdateButton", "racafeEnable", "racafeDisable",
      "racafeToggle", "racafeUpdateEnabled", "racafeResetForm", "racafeValidateForm", "racafeShowValidation",
      "racafeFormValidation", "racafeDownloadHandler", "racafeCopyButton", "racafeCopyText", "racafeCopyImage"),
    Recursos = c("racafeFormsDependencies", "racafeSharedAssets", "racafeFormsGallery"),
    `Formulario completo` = "Formulario completo")
  if (include_modules) categories$Agregables <- "ListaDesplegableAgregable"
  data.frame(name = unlist(categories, use.names = FALSE),
             category = rep(names(categories), lengths(categories)), stringsAsFactors = FALSE)
}
.rf_gallery_defaults <- function() {
  list(size = "compact", variant = "soft", required = FALSE, readonly = FALSE, disabled = FALSE,
       loading = FALSE, format = "number", decimals = 2L, value = 1234.56, min = -10000, max = 10000,
       thousand = ",", decimal = ".", multiple = FALSE, search = TRUE, empty = TRUE,
       gender = "m", uppercase = TRUE, unique_choices = TRUE, list_size = 6L, message = "info", icon = FALSE)
}
.rf_gallery_literal <- function(value) paste(capture.output(dput(value)), collapse = "\n")

# Código de UI compartido por la previsualización y el ejemplo copiable ----
.rf_gallery_parts <- function(name, config) {
  q <- .rf_gallery_literal
  flag <- function(key) q(isTRUE(config[[key]]))
  field <- paste0('racafeForms::racafeInputText("value", "Nombre", required = ', flag("required"),
                  ', disabled = ', flag("disabled"), ', readonly = ', flag("readonly"), ', ns = ns)')
  control <- switch(name,
    racafeInputText = field,
    racafeInputTextArea = paste0('racafeForms::racafeInputTextArea("value", "Observaci\u00f3n", required = ',
                                 flag("required"), ', disabled = ', flag("disabled"), ', ns = ns)'),
    racafeInputNumeric = paste0('racafeForms::racafeInputNumeric("value", "Importe", value = ', q(config$value),
      ', format = ', q(config$format), ', decimals = ', q(config$decimals), ', min = ', q(config$min),
      ', max = ', q(config$max), ', thousand = ', q(config$thousand), ', decimal = ', q(config$decimal),
      ', required = ', flag("required"), ', readonly = ', flag("readonly"), ', disabled = ', flag("disabled"),
      ', ns = ns)'),
    racafeInputSelect = paste0('racafeForms::racafeInputSelect("value", "Categor\u00eda",\n',
      '  choices = c("Caf\u00e9" = "cafe", "Cacao" = "cacao", "Caf\u00e9 repetido" = "cafe"),\n',
      '  multiple = ', flag("multiple"), ', search = ', flag("search"), ', empty = ', flag("empty"),
      ', gender = ', q(config$gender), ', size = ', q(config$list_size), ', unique_choices = ',
      flag("unique_choices"), ', uppercase = ', flag("uppercase"), ', required = ', flag("required"),
      ', disabled = ', flag("disabled"), ', ns = ns)'),
    racafeInputDate = 'racafeForms::racafeInputDate("value", "Fecha", ns = ns)',
    racafeInputDateRange = 'racafeForms::racafeInputDateRange("value", "Periodo", ns = ns)',
    racafeInputDateTime = 'racafeForms::racafeInputDateTime("value", "Fecha y hora", tz = "America/Bogota", ns = ns)',
    racafeInputReadOnly = 'racafeForms::racafeInputReadOnly("value", "C\u00f3digo", "DEMO-001", ns = ns)',
    racafeInputSwitch = 'racafeForms::racafeInputSwitch("value", "Activo", ns = ns)',
    racafeInputCheckbox = 'racafeForms::racafeInputCheckbox("value", "Acepto", ns = ns)',
    racafeInputRadio = 'racafeForms::racafeInputRadio("value", "Prioridad", c("Normal", "Urgente"), ns = ns)',
    racafeInputFile = 'racafeForms::racafeInputFile("value", accept = ".csv", ns = ns)',
    racafeValidateInputFile = 'racafeForms::racafeInputFile("value", accept = ".csv", ns = ns)',
    racafeButton = 'racafeForms::racafeButton("value", "Acci\u00f3n principal", ns = ns)',
    racafeButtonSave = 'racafeForms::racafeButtonSave("value", ns = ns)',
    racafeButtonCancel = 'racafeForms::racafeButtonCancel("value", ns = ns)',
    racafeButtonDelete = 'racafeForms::racafeButtonDelete("value", ns = ns)',
    racafeButtonDownload = 'racafeForms::racafeButtonDownload("file", ns = ns)',
    racafePanel = 'racafeForms::racafePanel("Secci\u00f3n", "Contenido sin tarjeta anidada", padding = "compact")',
    racafeCard = 'racafeForms::racafeCard("Cliente de ejemplo", "Contenido", clickable = TRUE, id = "value", ns = ns)',
    racafeMetric = 'racafeForms::racafeMetric("1,234.56", "Ventas", detail = "Datos simulados", padding = "compact")',
    racafeMetricGrid = paste0('racafeForms::racafeMetricGrid(\n',
      '  racafeForms::racafeMetric(12, "Pedidos"), racafeForms::racafeMetric(4, "Clientes"), columns = 2)'),
    racafeInfoItem = 'racafeForms::racafeInfoItem("Cuenta", "Cliente de demostraci\u00f3n")',
    racafeInfoGrid = paste0('racafeForms::racafeInfoGrid(racafeForms::racafeInfoItem("Cuenta", "Demo"),\n',
                            '  racafeForms::racafeInfoItem("Regi\u00f3n", "Bogot\u00e1"))'),
    racafeFormMessage = paste0('racafeForms::racafeFormMessage("Mensaje de demostraci\u00f3n", type = ',
      q(config$message),
      ', size = ', q(config$size), ', icon = ', if (config$icon) '"circle-info"' else 'NULL', ')'),
    racafeStatusBadge = 'racafeForms::racafeStatusBadge("Activo", variant = "success")',
    NULL)
  if (name %in% c("racafeButton", "racafeButtonSave", "racafeButtonCancel", "racafeButtonDelete",
                  "racafeButtonDownload")) {
    control <- sub("ns = ns)", paste0("ns = ns, disabled = ", flag("disabled"),
                                     ", loading = ", flag("loading"), ")"), control, fixed = TRUE)
  }
  action_controls <- paste0('shiny::tagList(\n',
    '  racafeForms::racafeInputSelect("value", "Categor\u00eda", c("Caf\u00e9", "Cacao"), ns = ns),\n',
    '  racafeForms::racafeInputNumeric("number", "Importe", value = 1234.56, ns = ns),\n',
    '  racafeForms::racafeInputDate("date", "Fecha", ns = ns),\n',
    '  racafeForms::racafeInputDateRange("range", "Periodo", ns = ns),\n',
    '  racafeForms::racafeInputSwitch("flag", "Activo", ns = ns),\n',
    '  racafeForms::racafeButton("run", "Ejecutar / alternar", ns = ns),\n',
    '  shiny::uiOutput(ns("dynamic")))')
  server <- 'output$values <- shiny::renderPrint(shiny::reactiveValuesToList(input))'
  if (name == "racafeInputSelect") {
    control <- paste0('shiny::tagList(', control, ',\n',
      '  racafeForms::racafeButton("update", "Actualizar alternativas", ns = ns),\n',
      '  racafeForms::racafeButton("toggle", "Habilitar / deshabilitar", ns = ns))')
    server <- paste(server, 'shiny::observeEvent(input$update, {',
      '  racafeForms::racafeUpdateSelect(session, "value", choices = c("Cacao" = "cacao", "T\u00e9" = "te"))',
      '})', 'shiny::observeEvent(input$toggle, {',
      '  racafeForms::racafeUpdateEnabled(session, "value", enabled = input$toggle %% 2L == 0L)', '})', sep = "\n")
  }
  if (name == "racafeValidateInputFile") {
    server <- 'output$values <- shiny::renderPrint(racafeForms::racafeValidateInputFile(input$value, accept = ".csv"))'
  }
  if (name == "racafeButtonDownload") {
    server <- paste(server, 'output$file <- shiny::downloadHandler("demo.csv", function(file) {',
      '  utils::write.csv(data.frame(nombre = "Demo", importe = 1234.56), file, row.names = FALSE)', '})', sep = "\n")
  }
  forms <- c("racafeForm", "racafeFormGroup", "racafeFormRow", "racafeFormFooter", "Formulario completo",
             "racafeValidateForm", "racafeShowValidation", "racafeFormValidation", "racafeDownloadHandler")
  if (name %in% forms) {
    control <- paste0('shiny::tagList(\n',
      '  racafeForms::racafeFormGroup(title = "Informaci\u00f3n comercial", variant = ',
      q(if (name == "racafeFormGroup") config$variant else "plain"), ',\n',
      '    racafeForms::racafeFormRow(\n',
      '      racafeForms::racafeInputText("value", "Nombre", required = TRUE, ns = ns),\n',
      '      racafeForms::racafeInputNumeric("number", "Importe", min = 0, max = 10000, ns = ns))),\n',
      '  racafeForms::racafeFormGroup(title = "Contacto",\n',
      '    racafeForms::racafeInputSwitch("flag", "Habilitar correo", ns = ns),\n',
      '    racafeForms::racafeInputText("email", "Correo", type = "email", disabled = TRUE, ns = ns),\n',
      '    racafeForms::racafeInputDate("date", "Fecha", ns = ns)),\n',
      '  racafeForms::racafeFormFooter(\n',
      '    racafeForms::racafeButtonSave("save", ns = ns),\n',
      '    racafeForms::racafeButtonCancel("reset", "Restablecer", ns = ns),\n',
      '    racafeForms::racafeButtonDownload("file", validate = TRUE, ns = ns)),\n',
      '  shiny::uiOutput(ns("result")))')
    server <- paste(server,
      'validation <- racafeForms::racafeFormValidation(input, function() example_ui(session$ns), session,',
      '  required = list(email = function(values) isTRUE(values$flag)),',
      '  rules = list(email = function(value, values) {',
      '    if (!isTRUE(values$flag) || grepl("^[^@ ]+@[^@ ]+[.][^@ ]+$", value)) TRUE else "Revise el correo."',
      '  }))',
      'shiny::observeEvent(input$flag, { racafeForms::racafeUpdateEnabled(session, "email", isTRUE(input$flag)) })',
      'shiny::observeEvent(input$save, {',
      '  if (!validation$validate()) return()',
      '  racafeForms::racafeConfirm(session, "confirm", "Guardar",',
      '    "\u00bfConfirmar guardado en memoria?", ns = session$ns)',
      '})',
      'shiny::observeEvent(input$confirm, {',
      '  if (!isTRUE(input$confirm) || !validation$validate()) return()',
      '  output$result <- shiny::renderUI(racafeForms::racafeFormMessage("Guardado en memoria", "success"))',
      '}, ignoreInit = TRUE)',
      'shiny::observeEvent(input$reset, { racafeForms::racafeResetForm(session, "form") })',
      'racafeForms::racafeDownloadHandler("file", validation, "formulario.csv",',
      '  content = function(file, values) {',
      '    utils::write.csv(data.frame(nombre = values$value, importe = values$number), file, row.names = FALSE)',
      '  }, session = session)', sep = "\n")
    if (name == "racafeValidateForm") server <- paste(server,
      'shiny::observeEvent(input$number, { print(racafeForms::racafeValidateForm(input, example_ui(session$ns))) })',
      sep = "\n")
    if (name == "racafeShowValidation") server <- paste(server,
      'shiny::observeEvent(input$date, {',
      '  result <- racafeForms::racafeValidateForm(input, example_ui(session$ns))',
      '  if (isTRUE(validation$attempted())) racafeForms::racafeShowValidation(session, result)', '})', sep = "\n")
  }
  if (name %in% c("racafeModal", "racafeModalFooter", "racafeConfirm")) {
    control <- paste0('shiny::tagList(\n',
      '  racafeForms::racafeInputSelect("modal_size", "Tama\u00f1o",\n',
      '    c("ventana", "subventana1", "subventana2", "subventana3", "subventana4", "subventana5", "aviso"),\n',
      '    selected = "subventana3", empty = FALSE, search = FALSE, ns = ns),\n',
      '  racafeForms::racafeButton("open", "Abrir modal", ns = ns))')
    server <- paste(server, 'shiny::observeEvent(input$open, {',
      '  shiny::showModal(racafeForms::racafeModal("Contenido de demostraci\u00f3n", title = "Modal corporativo",',
      '    size = if (is.null(input$modal_size)) "subventana3" else input$modal_size,',
      '    easy_close = TRUE, footer = racafeForms::racafeModalFooter(',
      '      shiny::actionButton(session$ns("ask"), "Confirmar dentro del modal"),',
      '      shiny::modalButton("Cancelar"))))',
      '})', 'shiny::observeEvent(input$ask, {',
      '  racafeForms::racafeConfirm(session, "confirm", "Confirmar",',
      '    "Escape conserva el modal inferior", ns = session$ns)',
      '})', sep = "\n")
  }
  if (name %in% c("racafeAlert", "racafeNotify", "racafeRemoveNotify")) {
    control <- 'racafeForms::racafeButton("run", "Mostrar mensaje / retirar", ns = ns)'
    server <- if (name == "racafeAlert") paste(server,
      'shiny::observeEvent(input$run, {',
      '  racafeForms::racafeAlert(session, "Aviso", "Datos de demostraci\u00f3n")', '})', sep = "\n")
    else paste(server,
      'notification <- shiny::reactiveVal(NULL)',
      'shiny::observeEvent(input$run, {',
      '  if (!is.null(notification())) {',
      '    racafeForms::racafeRemoveNotify(notification(), session); notification(NULL)',
      '  } else notification(racafeForms::racafeNotify(session, "Aviso", "Datos simulados", duration = NULL))',
      '})', sep = "\n")
  }
  updates <- c(racafeUpdateSelect = 'racafeForms::racafeUpdateSelect(session, "value", choices = c("Nuevo", "Otro"))',
    racafeUpdateNumeric = 'racafeForms::racafeUpdateNumeric(session, "number", value = 2500.75)',
    racafeUpdateDate = 'racafeForms::racafeUpdateDate(session, "date", value = Sys.Date() + 1)',
    racafeUpdateDateRange = paste0('racafeForms::racafeUpdateDateRange(session, "range", ',
                                  'start = Sys.Date(), end = Sys.Date() + 7)'),
    racafeUpdateSwitch = 'racafeForms::racafeUpdateSwitch(session, "flag", !isTRUE(input$flag))',
    racafeUpdateButton = 'racafeForms::racafeUpdateButton(session, "run", success = input$run %% 2L == 1L)',
    racafeEnable = 'racafeForms::racafeEnable("value", session)',
    racafeDisable = 'racafeForms::racafeDisable("value", session)',
    racafeToggle = 'racafeForms::racafeToggle("value", condition = input$run %% 2L == 0L, session = session)',
    racafeUpdateEnabled = 'racafeForms::racafeUpdateEnabled(session, "value", enabled = input$run %% 2L == 0L)',
    racafeResetForm = 'racafeForms::racafeResetForm(session, "form")')
  if (name %in% names(updates)) {
    control <- action_controls
    if (name == "racafeEnable") {
      control <- sub('"Categor\u00eda",', '"Categor\u00eda", disabled = TRUE,', control, fixed = TRUE)
    }
    server <- paste(server, 'output$dynamic <- shiny::renderUI({',
      '  if (isTRUE(input$flag)) racafeForms::racafeInputText("extra", "Campo din\u00e1mico", ns = session$ns)', '})',
      'shiny::observeEvent(input$run, {', paste0('  ', updates[[name]]), '})', sep = "\n")
  }
  if (name == "racafeUpdateEnabled") server <- paste(server,
    'shiny::observeEvent(input$run, {',
    '  racafeForms::racafeUpdateEnabled(session, "extra", enabled = input$run %% 2L == 0L)', '})', sep = "\n")
  if (name == "racafeUpdateButton") server <- paste(server,
    'shiny::observeEvent(input$flag, {',
    '  racafeForms::racafeUpdateButton(session, "run", loading = isTRUE(input$flag))', '})', sep = "\n")
  if (name %in% c("racafeCopyButton", "racafeCopyText", "racafeCopyImage")) {
    control <- if (name == "racafeCopyButton")
      'racafeForms::racafeCopyButton("run", text = "Texto corporativo de demostraci\u00f3n", ns = ns)'
    else 'racafeForms::racafeButton("run", "Copiar desde servidor", ns = ns)'
    if (name != "racafeCopyButton") {
      call <- if (name == "racafeCopyText") 'racafeForms::racafeCopyText(session, "run", "Texto de demostraci\u00f3n")'
      else paste0('racafeForms::racafeCopyImage(session, "run", ',
                  'paste0(attr(assets, "image_url"), "/logo.png"))')
      server <- paste(server, 'shiny::observeEvent(input$run, {', paste0('  ', call), '})', sep = "\n")
    }
  }
  if (name == "racafeActionBar") control <- paste0('racafeForms::racafeActionBar(\n',
    '  left = racafeForms::racafeButtonCancel("cancel", ns = ns),\n',
    '  right = racafeForms::racafeButtonSave("value", ns = ns))')
  if (name == "ListaDesplegableAgregable") {
    available <- paste0('requireNamespace("racafeModulos", quietly = TRUE) && ',
      '\"uppercase\" %in% names(formals(racafeModulos::ListaDesplegableAgregable))')
    control <- paste0('if (', available, ') shiny::tagList(\n',
      '  racafeModulos::ListaDesplegableAgregableUI(ns("simple")),\n',
      '  racafeModulos::ListaDesplegableAgregableUI(ns("multiple"))) else\n',
      '  racafeForms::racafeFormMessage(\n',
      '    "Instale la revisi\u00f3n actualizada de racafeModulos para probar este ejemplo.")')
    server <- paste0('if (', available, ') {\n',
      '  simple <- racafeModulos::ListaDesplegableAgregable("simple", "Categor\u00eda",\n',
      '    c("Caf\u00e9" = "cafe", "T\u00e9" = "te"), selected = "cafe", multiple = FALSE, fem = TRUE)\n',
      '  multiple <- racafeModulos::ListaDesplegableAgregable("multiple", "Segmentos",\n',
      '    c("Caf\u00e9", "Premium"), selected = "Premium", multiple = TRUE, confirm = FALSE)\n',
      '  output$values <- shiny::renderPrint(list(simple = simple$seleccion(), multiple = multiple$seleccion(),\n',
      '    catalogo_simple = simple$choices(), catalogo_multiple = multiple$choices()))\n',
      '}')
  }
  if (is.null(control)) control <- switch(name,
    racafeFormsDependencies = paste0('shiny::tagList(racafeForms::racafeFormsDependencies(), ',
                                    'shiny::p("Recursos base cargados"))'),
    racafeSharedAssets = paste0('shiny::tags$img(src = paste0(attr(assets, "image_url"), "/logo.png"), ',
                               'alt = "Racaf\u00e9", width = 120)'),
    racafeFormsGallery = paste0('shiny::p("Esta aplicaci\u00f3n es racafeFormsGallery(). ',
                               'Ejecute la funci\u00f3n en una nueva sesi\u00f3n de R.")'),
    stop("Componente sin demostraci\u00f3n: ", name))
  ui <- paste0('racafeForms::racafeForm(id = "form", ns = ns, size = ', q(config$size),
    ', variant = ', q(if (name == "racafeFormGroup") "plain" else config$variant), ',\n', control,
    ',\n  shiny::verbatimTextOutput(ns("values")))')
  list(ui = ui, server = server)
}
.rf_gallery_ui <- function(name, config, ns, assets) {
  eval(parse(text = .rf_gallery_parts(name, config)$ui), envir = environment())
}
.rf_gallery_code <- function(name, config) {
  parts <- .rf_gallery_parts(name, config)
  code <- paste0('# Ejemplo autocontenido: ', name, '\n',
    'assets <- racafeForms::racafeSharedAssets(\n',
    '  system.file("gallery-assets", package = "racafeForms"), global_css = FALSE)\n',
    'example_ui <- function(ns) {\n', parts$ui, '\n}\n',
    'example_server <- function(id) {\n  shiny::moduleServer(id, function(input, output, session) {\n',
    parts$server, '\n  })\n}\n',
    'shiny::shinyApp(shiny::fluidPage(assets, example_ui(shiny::NS("demo"))),\n',
    '  function(input, output, session) example_server("demo"))\n')
  paste(unlist(lapply(strsplit(code, "\n", fixed = TRUE)[[1]], .rf_gallery_wrap)), collapse = "\n")
}

# Ayuda local y aplicación bs4Dash ----
.rf_gallery_help <- function(catalog) {
  directory <- tempfile("racafe-gallery-help-")
  dir.create(directory)
  man <- system.file("man", package = "racafeForms")
  docs <- if (dir.exists(man)) lapply(list.files(man, "[.]Rd$", full.names = TRUE), tools::parse_Rd) else {
    tools::Rd_db("racafeForms")
  }
  titles <- list()
  if ("ListaDesplegableAgregable" %in% catalog$name && requireNamespace("racafeModulos", quietly = TRUE)) {
    docs <- c(docs, tools::Rd_db("racafeModulos"))
  }
  for (name in setdiff(catalog$name, "Formulario completo")) {
    matches <- vapply(docs, function(rd) any(vapply(rd, function(tag) {
      identical(attr(tag, "Rd_tag"), "\\alias") && identical(paste(unlist(tag), collapse = ""), name)
    }, logical(1))), logical(1))
    if (any(matches)) {
      rd <- docs[[which(matches)[1]]]
      title <- Filter(function(tag) identical(attr(tag, "Rd_tag"), "\\title"), rd)
      titles[[name]] <- trimws(paste(unlist(title), collapse = ""))
      tools::Rd2HTML(rd, out = file.path(directory, paste0(name, ".html")), stylesheet = "")
    }
  }
  prefix <- paste0("rf-gallery-help-", basename(directory))
  shiny::addResourcePath(prefix, directory)
  list(prefix = prefix, directory = directory, titles = titles)
}
.rf_gallery_app <- function() {
  catalog <- .rf_gallery_catalog(include_modules = TRUE)
  defaults <- .rf_gallery_defaults()
  assets <- racafeSharedAssets(system.file("gallery-assets", package = "racafeForms", mustWork = TRUE),
                              global_css = FALSE)
  help <- .rf_gallery_help(catalog)
  categories <- unique(catalog$category)
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "Racaf\u00e9 \u00b7 Componentes"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(id = "category", .list = lapply(seq_along(categories), function(i) {
        bs4Dash::menuItem(categories[[i]], tabName = paste0("category", i), icon = shiny::icon("layer-group"))
      }))),
    body = bs4Dash::dashboardBody(racafeFormsDependencies(), assets,
      shiny::tags$link(rel = "stylesheet", href = "gallery.css"),
      shiny::tags$main(class = "rf-gallery",
        shiny::h1("Galer\u00eda de componentes"),
        shiny::p("Configure, pruebe y copie ejemplos locales. Todos los datos son simulados."),
        shiny::fluidRow(shiny::column(8,
          shiny::selectInput("component", "Componente", choices = catalog$name[catalog$category == categories[[1]]]),
          shiny::uiOutput("description"), shiny::uiOutput("preview"),
          shiny::tags$section(class = "rf-gallery-code",
            shiny::h2("C\u00f3digo ejecutable"),
            racafeCopyButton("copy", "Copiar c\u00f3digo", target = "code"),
            shiny::uiOutput("help"), shiny::verbatimTextOutput("code")),
          shiny::textOutput("copy_status")),
        shiny::column(4, shiny::tags$section(class = "rf-gallery-config",
          shiny::h2("Configuraci\u00f3n"),
          shiny::selectInput("density", "Densidad", c("compact", "normal", "comfortable")),
          shiny::selectInput("variant", "Superficie", c("plain", "soft", "bordered"), selected = "soft"),
          shiny::conditionalPanel("/^racafeInput(Text|TextArea|Numeric|Select)$/.test(input.component)",
            shiny::checkboxInput("required", "Obligatorio")),
          shiny::conditionalPanel("/^racafeInput(Text|Numeric)$/.test(input.component)",
            shiny::checkboxInput("readonly", "Solo lectura")),
          shiny::conditionalPanel("/^racafe(Input(Text|TextArea|Numeric|Select)|Button.*)$/.test(input.component)",
            shiny::checkboxInput("disabled", "Deshabilitado")),
          shiny::conditionalPanel("/^racafeButton/.test(input.component)",
                                  shiny::checkboxInput("loading", "Procesando")),
          shiny::conditionalPanel("input.component === 'racafeInputNumeric'",
            shiny::selectInput("format", "Formato", c("integer", "number", "currency", "percent"), selected = "number"),
            shiny::numericInput("value", "Valor inicial", 1234.56),
            shiny::numericInput("decimals", "Decimales", 2, 0, 6),
            shiny::numericInput("min", "M\u00ednimo", -10000), shiny::numericInput("max", "M\u00e1ximo", 10000),
            shiny::selectInput("separators", "Miles / decimal", c("1,234.56" = "standard", "1.234,56" = "alternate"))),
          shiny::conditionalPanel("input.component === 'racafeInputSelect'",
            shiny::checkboxInput("multiple", "Selecci\u00f3n m\u00faltiple"),
            shiny::checkboxInput("search", "B\u00fasqueda", TRUE),
            shiny::checkboxInput("empty", "Opci\u00f3n vac\u00eda", TRUE),
            shiny::selectInput("gender", "Etiqueta vac\u00eda", c("Ninguno" = "m", "Ninguna" = "f")),
            shiny::checkboxInput("uppercase", "Etiquetas en may\u00fasculas", TRUE),
            shiny::checkboxInput("unique_choices", "Eliminar duplicados", TRUE),
            shiny::numericInput("list_size", "Opciones visibles", 6, 1, 20)),
          shiny::conditionalPanel("input.component === 'racafeFormMessage'",
            shiny::selectInput("message", "Estado",
                               c("info", "success", "warning", "error", "muted", "loading", "empty")),
            shiny::checkboxInput("icon", "Mostrar icono")),
          racafeButton("apply", "Aplicar configuraci\u00f3n")))))),
    controlbar = bs4Dash::dashboardControlbar(.rf_gallery_bar_ui("controlbar_demo"),
      id = "gallery_controlbar", width = 440, pinned = FALSE))
  ui <- shiny::tagList(htmltools::suppressDependencies("googlefonts", "ionicons"), ui)
  server <- function(input, output, session) {
    .rf_gallery_bar_server("controlbar_demo")
    configs <- shiny::reactiveValues()
    active <- shiny::reactiveVal(catalog$name[[1]])
    config <- function(name = active()) configs[[name]] %||% defaults
    shiny::observeEvent(input$category, {
      category <- categories[[as.integer(sub("category", "", input$category, fixed = TRUE))]]
      choices <- catalog$name[catalog$category == category]
      shiny::updateSelectInput(session, "component", choices = choices, selected = choices[[1]])
    })
    shiny::observeEvent(input$component, {
      if (input$component %in% catalog$name) active(input$component)
    })
    shiny::observeEvent(input$apply, {
      if (is.null(input$min) || is.null(input$max) || input$min > input$max ||
          is.null(input$value) || input$value < input$min || input$value > input$max) {
        racafeNotify(session, "Revise los l\u00edmites",
                     "El valor inicial debe estar entre m\u00ednimo y m\u00e1ximo.", type = "warning")
        return()
      }
      current <- defaults
      for (key in intersect(names(current), names(shiny::reactiveValuesToList(input)))) current[[key]] <- input[[key]]
      current$size <- input$density
      current$thousand <- if (input$separators == "alternate") "." else ","
      current$decimal <- if (input$separators == "alternate") "," else "."
      configs[[active()]] <- current
    })
    shiny::observeEvent(active(), {
      current <- shiny::isolate(config())
      for (key in names(current)) {
        value <- current[[key]]
        target <- if (key == "size") "density" else key
        if (is.logical(value)) shiny::updateCheckboxInput(session, target, value = value)
        else if (is.numeric(value)) shiny::updateNumericInput(session, target, value = value)
        else if (!key %in% c("thousand", "decimal")) shiny::updateSelectInput(session, target, selected = value)
      }
      shiny::updateSelectInput(session, "separators",
                               selected = if (current$thousand == ",") "standard" else "alternate")
    })
    for (name in catalog$name) local({
      component <- name
      module_id <- paste0("example", match(component, catalog$name))
      shiny::moduleServer(module_id, function(input, output, session) {
        example_ui <- function(ns) .rf_gallery_ui(component, config(component), ns, assets)
        eval(parse(text = .rf_gallery_parts(component, defaults)$server), envir = environment())
      })
    })
    output$preview <- shiny::renderUI({
      name <- active()
      .rf_gallery_ui(name, config(), shiny::NS(paste0("example", match(name, catalog$name))), assets)
    })
    output$description <- shiny::renderUI({
      name <- active()
      category <- catalog$category[match(name, catalog$name)]
      description <- help$titles[[name]] %||% "Formulario comercial con validaci\u00f3n y confirmaci\u00f3n en memoria."
      racafeFormMessage(paste(category, "\u00b7", name, "\u00b7", description), "muted")
    })
    output$code <- shiny::renderText(.rf_gallery_code(active(), config()))
    output$help <- shiny::renderUI({
      name <- if (active() == "Formulario completo") "racafeFormValidation" else active()
      if (!file.exists(file.path(help$directory, paste0(name, ".html")))) name <- "racafeInputSelect"
      shiny::tags$a(href = paste0(help$prefix, "/", name, ".html"), target = "_blank", rel = "noopener",
                    "Abrir ayuda local")
    })
    output$copy_status <- shiny::renderText({
      result <- input$copy_copy_result
      if (is.null(result)) "" else if (isTRUE(result$success)) {
        "C\u00f3digo copiado."
      } else "No se pudo copiar: revise permisos."
    })
  }
  shiny::shinyApp(ui, server)
}

# Ajuste de ancho sin cambiar literales ni agregar dependencias de formato ----
.rf_gallery_wrap <- function(line) {
  result <- character()
  indent <- paste0(sub("[^ ].*$", "", line), "  ")
  while (nchar(line) > 120L) {
    chars <- strsplit(line, "", fixed = TRUE)[[1]]
    quote <- ""; escaped <- FALSE; candidates <- integer()
    for (i in seq_len(min(110L, length(chars)))) {
      char <- chars[[i]]
      if (escaped) { escaped <- FALSE; next }
      if (nzchar(quote)) {
        if (char == "\\") escaped <- TRUE
        else if (char == quote) quote <- ""
      } else if (char %in% c("'", '\"', "`")) quote <- char
      else if (char == ",") candidates <- c(candidates, i)
    }
    if (!length(candidates)) break
    split <- max(candidates)
    result <- c(result, substr(line, 1L, split))
    line <- paste0(indent, trimws(substring(line, split + 1L), which = "left"))
  }
  c(result, line)
}

# Selector normal y menú externo dentro de bs4DashControlbar ----
.rf_gallery_bar_ui <- function(id) {
  ns <- shiny::NS(id)
  racafeForm(id = "form", ns = ns, variant = "plain",
    racafeFormGroup(title = "Selectores en controlbar",
      racafeInputSelect("single", "Selecci\u00f3n simple", c("Caf\u00e9" = "cafe", "Cacao" = "cacao"), ns = ns),
      racafeInputSelect("multiple", "Selecci\u00f3n m\u00faltiple",
                        c("Caf\u00e9" = "cafe", "Cacao" = "cacao", "T\u00e9" = "te"),
                        multiple = TRUE, ns = ns),
      racafeInputCheckbox("portal", "Men\u00fa din\u00e1mico fuera del panel", ns = ns),
      racafeInputCheckbox("dynamic_multiple", "Din\u00e1mico con selecci\u00f3n m\u00faltiple", value = TRUE, ns = ns),
      shiny::uiOutput(ns("dynamic_ui")),
      racafeButton("renew", "Reconstruir selector din\u00e1mico", ns = ns),
      shiny::verbatimTextOutput(ns("state"))))
}
.rf_gallery_bar_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    revision <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$renew, { revision(revision() + 1L) })
    output$dynamic_ui <- shiny::renderUI({
      tag <- racafeInputSelect("dynamic", paste("Din\u00e1mico", revision()), c("A" = "a", "B" = "b", "C" = "c"),
                               multiple = isTRUE(input$dynamic_multiple), ns = session$ns)
      if (isTRUE(input$portal)) {
        tag <- htmltools::tagQuery(tag)$find("select")$addAttrs(`data-container` = "body")$allTags()
      }
      tag
    })
    output$state <- shiny::renderPrint(list(single = input$single, multiple = input$multiple,
                                           dynamic = input$dynamic, revision = revision()))
  })
}
