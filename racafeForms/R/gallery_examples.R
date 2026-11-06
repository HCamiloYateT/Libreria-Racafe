# Referencia de funciones auxiliares ----
.rf_gallery_details <- function(name) {
  descriptions <- c(
    racafeValidateInputFile = paste0("Valida extensión, tamaño y cantidad de archivos en el servidor; ",
      "devuelve el resultado."),
    racafeUpdateSelect = "Actualiza las opciones o la selección de un selector existente desde el servidor.",
    racafeUpdateNumeric = "Actualiza el valor y los límites de un campo numérico existente.",
    racafeUpdateDate = "Actualiza la fecha de un control existente desde el servidor.",
    racafeUpdateDateRange = "Actualiza el inicio y el final de un rango de fechas existente.",
    racafeUpdateSwitch = "Actualiza el estado lógico de un switch existente.",
    racafeUpdateButton = "Actualiza el texto y los estados de carga o éxito de un botón existente.",
    racafeEnable = "Habilita un control existente; no crea un campo nuevo ni cambia su valor.",
    racafeDisable = "Deshabilita un control existente; no sustituye la validación del servidor.",
    racafeToggle = "Habilita o deshabilita un control según una condición del servidor.",
    racafeUpdateEnabled = "Actualiza la habilitación de un control, incluso si su UI se inserta después.",
    racafeResetForm = "Restablece los campos de un formulario existente a sus valores iniciales.",
    racafeValidateForm = "Evalúa los valores contra el esquema del formulario y devuelve errores y validez.",
    racafeShowValidation = "Presenta junto a los campos el resultado obtenido con racafeValidateForm().",
    racafeFormValidation = "Crea un validador por formulario y sesión; validate() comprueba y presenta los errores.",
    racafeResetValidation = "Limpia los errores y el estado de intento del validador; no borra los valores.",
    racafeDownloadHandler = "Registra una descarga que valida los campos antes de generar el archivo.",
    racafeCopyText = "Solicita copiar texto desde el servidor; el resultado depende del permiso del navegador.",
    racafeCopyImage = "Solicita copiar una imagen accesible por URL; requiere soporte y permiso del navegador.",
    racafeRemoveNotify = "Retira una notificación usando el identificador devuelto por racafeNotify().",
    racafeAlert = "Abre un aviso desde el servidor; no añade un control permanente a la UI.",
    racafeConfirm = "Solicita confirmación desde el servidor y devuelve la respuesta mediante un input.",
    racafeNotify = "Muestra una notificación desde el servidor y devuelve su identificador.",
    racafeFormsDependencies = "Registra los estilos y scripts base. Se carga una vez en la UI de la aplicación.",
    racafeSharedAssets = "Registra los recursos corporativos locales de Compartido y sus rutas públicas.",
    racafeFormsGallery = "Abre esta galería en una sesión de R independiente de la aplicación en desarrollo.")
  resource <- name %in% c("racafeFormsDependencies", "racafeSharedAssets", "racafeFormsGallery")
  list(kind = if (resource) "Recurso auxiliar" else if (name %in% names(descriptions)) "Función de servidor" else
         if (name == "ListaDesplegableAgregable") "Módulo opcional" else "Componente de interfaz",
       description = unname(descriptions[name]), resource = resource)
}

# Variaciones aplicadas a la misma llamada en la vista previa y el código ----
.rf_gallery_control_options <- function(name, code, config) {
  options <- switch(name,
    racafeInputDate = list(view = config$date_view),
    racafeInputTextArea = list(rows = config$rows),
    racafeInputRadio = list(inline = config$inline),
    racafeInputSwitch = list(value = config$checked),
    racafeInputCheckbox = list(value = config$checked),
    racafeStatusBadge = list(variant = config$badge_variant, size = config$badge_size),
    NULL)
  if (name %in% c("racafePanel", "racafeCard", "racafeMetric")) {
    options <- list(variant = config$surface, padding = config$size)
  }
  if (name %in% c("racafeButton", "racafeButtonSave", "racafeButtonCancel", "racafeButtonDelete",
                  "racafeButtonDownload")) {
    options <- list(size = config$button_size)
    if (config$button_variant != "default") options$variant <- config$button_variant
  }
  if (startsWith(name, "racafeInput")) {
    parameters <- names(formals(get(name, envir = asNamespace("racafeForms"))))
    for (key in intersect(c("required", "readonly", "disabled"), parameters)) options[[key]] <- config[[key]]
  }
  if (!length(options)) return(code)
  update <- function(expr) {
    if (!is.call(expr)) return(expr)
    args <- lapply(as.list(expr), update)
    if (identical(expr[[1]], substitute(racafeForms::FUN, list(FUN = as.name(name))))) {
      for (key in names(options)) args[[key]] <- options[[key]]
    }
    as.call(args)
  }
  paste(deparse(update(parse(text = code)[[1]]), width.cutoff = 100L), collapse = "\n")
}

# Fragmentos mínimos para una aplicación existente ----
.rf_gallery_example <- function(name, config) {
  parts <- .rf_gallery_parts(name, config)
  ui <- parts$control
  server <- "# No requiere lógica adicional de servidor."
  button <- 'racafeForms::racafeButton("run", "Ejecutar")'
  observe <- function(code, id = "run") paste0('shiny::observeEvent(input$', id, ', {\n  ', code, '\n})')
  details <- .rf_gallery_details(name)
  if (details$resource) {
    return(list(ui = "# Los recursos de la aplicación ya están cargados.",
                server = paste0("# ", details$description)))
  }

  # Cada actualizador muestra únicamente el control al que se aplica ----
  updates <- list(
    racafeUpdateSelect = c('racafeForms::racafeInputSelect("value", "Categoría", c("Café", "Cacao"))',
      'racafeForms::racafeUpdateSelect(session, "value", choices = c("Cacao", "Té"))'),
    racafeUpdateNumeric = c('racafeForms::racafeInputNumeric("number", "Importe", value = 1234.56)',
      'racafeForms::racafeUpdateNumeric(session, "number", value = 2500.75)'),
    racafeUpdateDate = c('racafeForms::racafeInputDate("date", "Fecha")',
      'racafeForms::racafeUpdateDate(session, "date", value = Sys.Date() + 1)'),
    racafeUpdateDateRange = c('racafeForms::racafeInputDateRange("range", "Periodo")',
      'racafeForms::racafeUpdateDateRange(session, "range", start = Sys.Date(), end = Sys.Date() + 7)'),
    racafeUpdateSwitch = c('racafeForms::racafeInputSwitch("flag", "Activo")',
      'racafeForms::racafeUpdateSwitch(session, "flag", !isTRUE(input$flag))'),
    racafeUpdateButton = c(NULL, 'racafeForms::racafeUpdateButton(session, "run", success = TRUE)'),
    racafeEnable = c('racafeForms::racafeInputText("value", "Nombre", disabled = TRUE)',
      'racafeForms::racafeEnable("value", session)'),
    racafeDisable = c('racafeForms::racafeInputText("value", "Nombre")',
      'racafeForms::racafeDisable("value", session)'),
    racafeToggle = c('racafeForms::racafeInputText("value", "Nombre")',
      'racafeForms::racafeToggle("value", condition = input$run %% 2L == 0L, session = session)'),
    racafeUpdateEnabled = c('racafeForms::racafeInputText("value", "Nombre")',
      'racafeForms::racafeUpdateEnabled(session, "value", enabled = input$run %% 2L == 0L)'))
  if (name %in% names(updates)) {
    update <- updates[[name]]
    ui <- paste0("shiny::tagList(\n  ", paste(c(head(update, -1L), button), collapse = ",\n  "), ")")
    server <- observe(tail(update, 1L))
  }
  if (name == "racafeValidateInputFile") {
    ui <- paste0("shiny::tagList(\n  ", ui, ',\n  shiny::verbatimTextOutput("result"))')
    server <- paste0('output$result <- shiny::renderPrint({\n',
      '  racafeForms::racafeValidateInputFile(input$value, accept = ".csv")\n})')
  }
  if (name == "racafeButtonDownload") {
    server <- paste0('output$file <- shiny::downloadHandler(\n  filename = "demo.csv",\n',
      '  content = function(file) utils::write.csv(data.frame(nombre = "Demo"), file, row.names = FALSE))')
  }
  if (name == "racafeInputSelect") {
    ui <- paste(deparse(parse(text = ui)[[1]][[2]], width.cutoff = 100L), collapse = "\n")
  }

  # Formularios y validación con un único campo obligatorio ----
  field <- 'racafeForms::racafeInputText("value", "Nombre", required = TRUE)'
  form <- paste0('racafeForms::racafeForm(id = "form", variant = "plain",\n  ', field,
    ',\n  racafeForms::racafeButtonSave("save"))')
  schema <- 'schema <- list(value = list(label = "Nombre", required = TRUE))'
  validator <- paste0(schema, '\nvalidation <- racafeForms::racafeFormValidation(input, schema, session)')
  if (name == "racafeForm") {
    ui <- paste0('racafeForms::racafeForm(id = "form", size = ', .rf_gallery_literal(config$size),
      ', variant = ', .rf_gallery_literal(config$variant), ',\n  ', parts$control, ')')
  }
  if (name == "racafeFormGroup") {
    ui <- parts$control
  }
  if (name == "racafeFormFooter") {
    ui <- paste0('racafeForms::racafeFormFooter(\n',
      '  racafeForms::racafeButtonSave("save"), racafeForms::racafeButtonCancel("cancel"))')
  }
  if (name %in% c("racafeValidateForm", "racafeShowValidation", "racafeFormValidation", "racafeResetValidation")) {
    ui <- form
    server <- paste0(validator, '\n', observe('validation$validate()', "save"))
    if (name %in% c("racafeValidateForm", "racafeShowValidation")) {
      ui <- sub('))', '), shiny::verbatimTextOutput("result"))', ui, fixed = TRUE)
      server <- paste0(schema, '\n', observe(paste0(
        'result <- racafeForms::racafeValidateForm(input, schema)\n  ',
        if (name == "racafeShowValidation") 'racafeForms::racafeShowValidation(session, result)\n  ' else "",
        'output$result <- shiny::renderPrint(result)'), "save"))
    }
    if (name == "racafeResetValidation") {
      ui <- sub('))', '), racafeForms::racafeButtonCancel("reset"))', ui, fixed = TRUE)
      server <- paste0(server, '\n', observe('racafeForms::racafeResetValidation(validation)', "reset"))
    }
  }
  if (name == "racafeResetForm") {
    ui <- paste0('racafeForms::racafeForm(id = "form", variant = "plain",\n  ', field, ',\n  ', button, ')')
    server <- observe('racafeForms::racafeResetForm(session, "form")')
  }
  if (name == "racafeDownloadHandler") {
    ui <- paste0('racafeForms::racafeForm(id = "form", variant = "plain",\n  ', field,
      ',\n  racafeForms::racafeButtonDownload("file", validate = TRUE))')
    server <- paste0(validator, '\nracafeForms::racafeDownloadHandler("file", validation, "demo.csv",\n',
      '  content = function(file, values) {\n',
      '    utils::write.csv(data.frame(nombre = values$value), file, row.names = FALSE)\n',
      '  }, session = session)')
  }
  if (name == "Formulario completo") {
    ui <- paste0('racafeForms::racafeForm(id = "form", size = ', .rf_gallery_literal(config$size),
      ', variant = ', .rf_gallery_literal(config$variant), ',\n', parts$control, ')')
    server <- parts$server
    server <- gsub('function() example_ui(session$ns)',
      'list(value = list(required = TRUE), number = list(min = 0, max = 10000),\n    email = list(), flag = list())',
      server, fixed = TRUE)
    server <- sub('output$values <- shiny::renderPrint(shiny::reactiveValuesToList(input))\n', '', server, fixed = TRUE)
  }

  # Modales y mensajes: una acción y su respuesta ----
  if (name %in% c("racafeModal", "racafeModalFooter", "racafeModalDetail", "racafeConfirmation", "racafeConfirm")) {
    ui <- button
    call <- switch(name,
      racafeModal = 'shiny::showModal(racafeForms::racafeModal("Contenido", title = "Detalle"))',
      racafeModalFooter = paste0('shiny::showModal(racafeForms::racafeModal("Contenido",\n',
        '    footer = racafeForms::racafeModalFooter(shiny::modalButton("Cerrar"))))'),
      racafeModalDetail = paste0('shiny::showModal(racafeForms::racafeModalDetail("Detalle",\n',
        '    racafeForms::racafeInfoItem("Cliente", "Demo")))'),
      racafeConfirmation = paste0('shiny::showModal(racafeForms::racafeConfirmation("confirm",\n',
        '    list(Cliente = "Demo")))'),
      racafeConfirm = 'racafeForms::racafeConfirm(session, "confirm", "Guardar", "¿Confirmar guardado?")')
    server <- observe(call)
    if (name %in% c("racafeConfirmation", "racafeConfirm")) {
      server <- paste0(server, '\n', observe(paste0(
        if (name == "racafeConfirm") 'if (!isTRUE(input$confirm)) return()\n  ' else 'shiny::removeModal()\n  ',
        'racafeForms::racafeNotify(session, "Confirmado", type = "success")'), "confirm"))
    }
  }
  if (name %in% c("racafeAlert", "racafeNotify", "racafeRemoveNotify")) {
    ui <- button
    server <- observe(if (name == "racafeAlert") 'racafeForms::racafeAlert(session, "Aviso", "Datos de ejemplo")'
      else 'racafeForms::racafeNotify(session, "Aviso", "Datos de ejemplo")')
    if (name == "racafeRemoveNotify") {
      ui <- 'shiny::tagList(racafeForms::racafeButton("show", "Mostrar"), racafeForms::racafeButton("run", "Retirar"))'
      server <- paste0('notification <- shiny::reactiveVal(NULL)\n', observe(paste0(
        'notification(racafeForms::racafeNotify(session, "Aviso", duration = NULL))'), "show"), '\n',
        observe('if (!is.null(notification())) racafeForms::racafeRemoveNotify(notification(), session)'))
    }
  }
  if (name %in% c("racafeCopyText", "racafeCopyImage")) {
    ui <- button
    server <- observe(if (name == "racafeCopyText") 'racafeForms::racafeCopyText(session, "run", "Texto de ejemplo")'
      else 'racafeForms::racafeCopyImage(session, "run", "/img/logo.png")')
  }
  if (name == "ListaDesplegableAgregable") {
    ui <- 'racafeModulos::ListaDesplegableAgregableUI("category")'
    server <- paste0('category <- racafeModulos::ListaDesplegableAgregable("category", "Categoría",\n',
      '  c("Café" = "cafe", "Cacao" = "cacao"))\n# category$seleccion() devuelve la selección reactiva.')
  }

  # Los fragmentos usan IDs de la aplicación; el aislamiento de la galería queda interno ----
  # Retirar namespaces mediante el árbol evita alterar cierres de llamadas anidadas.
  simplify <- function(expr) {
    if (!is.call(expr)) return(expr)
    if (identical(expr[[1]], as.name("ns")) || identical(expr[[1]], quote(session$ns))) return(expr[[2]])
    args <- as.list(expr)
    if (!is.null(names(args))) args <- args[is.na(names(args)) | names(args) != "ns"]
    # Omitir parámetros opcionales que coinciden con el valor predeterminado.
    if (is.call(args[[1]]) && identical(args[[1]][[1]], as.name("::")) &&
        identical(args[[1]][[2]], as.name("racafeForms"))) {
      defaults <- formals(get(as.character(args[[1]][[3]]), envir = asNamespace("racafeForms")))
      for (key in intersect(names(args), names(defaults))) {
        if (is.atomic(args[[key]]) && is.atomic(defaults[[key]]) &&
            isTRUE(all.equal(args[[key]], defaults[[key]]))) args[[key]] <- NULL
      }
    }
    as.call(lapply(args, simplify))
  }
  ui <- paste(vapply(parse(text = ui), function(expr) paste(deparse(simplify(expr), width.cutoff = 100L),
    collapse = "\n"), character(1)), collapse = "\n")
  if (config$size != "compact" && !name %in% c("racafeForm", "Formulario completo")) {
    ui <- paste0('racafeForms::racafeForm(size = ', .rf_gallery_literal(config$size), ', variant = "plain",\n', ui, ')')
  }
  list(ui = ui, server = server)
}
