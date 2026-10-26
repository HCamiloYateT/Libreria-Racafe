library(shiny)
library(racafeForms)
ui <- fluidPage(
  titlePanel("racafeForms · Catalogo de controles"),
  racafeForm(
    racafeFormGroup(title = "Datos generales",
      racafeFormRow(
        racafeInputText("razon", "Razon social", placeholder = "Empresa"),
        racafeInputSelect("asesor", "Asesor", c("Ana", "Carlos", "Marta"))
      ),
      racafeFormRow(
        racafeInputNumeric("monto", "Presupuesto COP", 1234567, format = "cop"),
        racafeInputNumeric("margen", "Margen %", 22.5, format = "percent")
      )
    ),
    racafeFormGroup(title = "Fechas y ajustes",
      racafeFormRow(racafeInputDate("fecha", "Fecha"),
                    racafeInputDateRange("rango", "Rango")),
      racafeInputSwitch("estado", "Activo"),
      racafeInputTextArea("nota", "Observaciones")
    ),
    racafeFormFooter(
      help = "Formulario corporativo de demostracion",
      racafeButtonCancel("limpiar"), racafeButtonSave("guardar")
    )
  )
)
server <- function(input, output, session) {
  observeEvent(input$guardar, {
    racafeConfirm(session, "confirmar", "Confirmar guardado",
                  "¿Desea guardar los valores ingresados?")
  })
  observeEvent(input$confirmar, {
    if (isTRUE(input$confirmar)) racafeAlert(session, "Correcto", "Operacion confirmada", "success")
  })
  observeEvent(input$limpiar, {
    updateTextInput(session, "razon", value = "")
    racafeUpdateSelect(session, "asesor", selected = "")
  })
}
shinyApp(ui, server)
