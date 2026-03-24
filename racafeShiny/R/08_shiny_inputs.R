# ============================================================
# Seccion 08 — Componentes Shiny: Inputs
# Controles de entrada con estilo corporativo Racafe
# ============================================================


# ---- Inputs numericos ----

#' Input numerico personalizado con layout de columnas
#'
#' @param id ID del input Shiny.
#' @param label Etiqueta visible del input.
#' @param value Valor inicial.
#' @param dec Numero de decimales permitidos.
#' @param max Valor maximo permitido. `NULL` sin limite.
#' @param min Valor minimo permitido. `NULL` sin limite.
#' @param type Nombre del formato registrado para mostrar (solo visual).
#' @param label_col Ancho de la columna del label (sobre 12).
#' @param input_col Ancho de la columna del input (sobre 12).
#' @param width Ancho CSS del input.
#' @param ns Funcion namespace de modulo Shiny. `NULL` si no es modulo.
#' @return `tagList` de Shiny con el input numerico.
#' @export
#' @examples
#' \dontrun{
#'   InputNumerico("ventas", "Ventas", 1000, dec = 0)
#' }
InputNumerico <- function(
    id,
    label,
    value,
    dec       = 2,
    max       = NULL,
    min       = NULL,
    type      = "numero",
    label_col = 6,
    input_col = 6,
    width     = "100%",
    ns        = NULL) {

  input_id <- if (!is.null(ns)) ns(id) else id

  shiny::fluidRow(
    shiny::column(
      width = label_col,
      shiny::tags$label(
        `for` = input_id,
        class = "control-label racafe-label",
        label
      )
    ),
    shiny::column(
      width = input_col,
      shiny::numericInput(
        inputId = input_id,
        label   = NULL,
        value   = value,
        min     = min %||% NA,
        max     = max %||% NA,
        step    = 10^(-dec),
        width   = width
      )
    )
  )
}


# ---- Selectores ----

#' Lista desplegable con pickerInput estilizado
#'
#' @param inputId ID del input.
#' @param label Etiqueta. `NULL` sin etiqueta.
#' @param choices Opciones disponibles.
#' @param selected Opciones seleccionadas por defecto.
#' @param multiple Logico. Permitir seleccion multiple.
#' @param fem Logico. Usar articulos en femenino en el placeholder.
#' @param ns Funcion namespace de modulo Shiny.
#' @return Objeto `pickerInput` de shinyWidgets.
#' @export
#' @examples
#' \dontrun{
#'   ListaDesplegable("region", "Region", choices = c("Norte", "Sur"))
#' }
ListaDesplegable <- function(
    inputId,
    label    = NULL,
    choices,
    selected = choices,
    multiple = TRUE,
    fem      = FALSE,
    ns       = NULL) {

  .check_pkg("shinyWidgets", "Shiny inputs")

  input_id  <- if (!is.null(ns)) ns(inputId) else inputId
  articulo  <- if (fem) c("Todas", "Ninguna") else c("Todos", "Ninguno")

  shinyWidgets::pickerInput(
    inputId  = input_id,
    label    = label,
    choices  = choices,
    selected = selected,
    multiple = multiple,
    options  = shinyWidgets::pickerOptions(
      actionsBox        = TRUE,
      selectAllText     = articulo[1],
      deselectAllText   = articulo[2],
      selectedTextFormat = "count > 3",
      countSelectedText = "{0} seleccionados",
      liveSearch        = length(choices) > 10,
      size              = min(10, length(choices))
    )
  )
}


#' Construir opciones con genero para pickerInput
#'
#' @param cho Vector de opciones.
#' @param fem Logico. `TRUE` para opciones en femenino.
#' @return Lista de opciones para `shinyWidgets::pickerOptions`.
#' @export
pick_opt <- function(cho, fem = TRUE) {
  articulo <- if (fem) c("Todas", "Ninguna") else c("Todos", "Ninguno")
  shinyWidgets::pickerOptions(
    actionsBox        = TRUE,
    selectAllText     = articulo[1],
    deselectAllText   = articulo[2],
    selectedTextFormat = "count > 2",
    countSelectedText = "{0} seleccionados",
    liveSearch        = length(cho) > 8
  )
}


#' Botones radiales estilizados (radioGroupButtons)
#'
#' @param inputId ID del input.
#' @param label Etiqueta del grupo. `NULL` sin etiqueta.
#' @param choices Vector de opciones.
#' @param selected Opcion seleccionada por defecto.
#' @param alineacion Alineacion del grupo: `"left"`, `"center"`, `"right"`.
#' @param ... Argumentos adicionales para `shinyWidgets::radioGroupButtons`.
#' @return Componente Shiny.
#' @export
#' @examples
#' \dontrun{
#'   BotonesRadiales("estado", "Estado", choices = c("Activo", "Inactivo"))
#' }
BotonesRadiales <- function(
    inputId,
    label      = NULL,
    choices,
    selected   = NULL,
    alineacion = c("left", "center", "right"),
    ...) {

  .check_pkg("shinyWidgets", "Shiny inputs")
  alineacion <- match.arg(alineacion)

  clase_alineacion <- switch(
    alineacion,
    left   = "justify-content-start",
    center = "justify-content-center",
    right  = "justify-content-end"
  )

  shiny::div(
    class = paste("racafe-radio-group", clase_alineacion),
    shinyWidgets::radioGroupButtons(
      inputId  = inputId,
      label    = label,
      choices  = choices,
      selected = selected %||% choices[[1]],
      ...
    )
  )
}


#' Boton interruptor (toggle switch)
#'
#' @param ... Argumentos pasados a `shinyWidgets::materialSwitch`.
#' @return Componente Shiny.
#' @export
#' @examples
#' \dontrun{
#'   BotonEstado("toggle_filtro", "Activar filtro")
#' }
BotonEstado <- function(...) {
  .check_pkg("shinyWidgets", "Shiny inputs")
  shinyWidgets::materialSwitch(
    ...,
    status = "success",
    right  = TRUE
  )
}


#' Boton de accion con alineacion configurable
#'
#' @param id ID del boton.
#' @param label Texto del boton.
#' @param align Alineacion: `"left"`, `"center"`, `"right"`.
#' @param ... Argumentos adicionales para `shiny::actionButton`.
#' @return Componente Shiny.
#' @export
#' @examples
#' \dontrun{
#'   BotonGuardar("guardar_form", label = "Guardar cambios", align = "center")
#' }
BotonGuardar <- function(id, label = "Guardar", align = "right", ...) {
  align <- match.arg(align, c("left", "center", "right"))

  clase_align <- switch(
    align,
    left   = "text-left",
    center = "text-center",
    right  = "text-right"
  )

  shiny::div(
    class = clase_align,
    shiny::actionButton(
      inputId = id,
      label   = label,
      class   = "btn btn-success btn-sm racafe-btn-guardar",
      icon    = shiny::icon("floppy-disk"),
      ...
    )
  )
}
