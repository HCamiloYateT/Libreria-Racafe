#' MostrarModalConClase
#'
#' Muestra un `modalDialog` y le agrega una clase CSS al `.modal-dialog`
#' recien insertado, para permitir tamanos o estilos diferenciados (por
#' ejemplo, `"aviso"` para confirmaciones compactas). Usa
#' `shinyjs::delay(0, ...)` porque el modal se inserta en el DOM de forma
#' asincrona, despues de que `showModal()` retorna.
#'
#' @param modal_ui `shiny.tag`. Resultado de `modalDialog()` o similar.
#' @param clase String. Clase CSS a agregar al `.modal-dialog` mas reciente.
#'
#' @return Nada (efectos secundarios: `showModal()` y `runjs()`).
#'
#' @examples
#' \dontrun{
#' MostrarModalConClase(shiny::modalDialog("Contenido"), "aviso")
#' }
#'
#' @export
MostrarModalConClase <- function(modal_ui, clase) {
  shiny::showModal(modal_ui)
  shinyjs::delay(0, shinyjs::runjs(
    sprintf("$('.modal-dialog').not('.%s').last().addClass('%s');", clase, clase)
  ))
}

#' ModalConfirmacion
#'
#' Construye el `modalDialog` estandar de confirmacion del CRM: titulo,
#' cuerpo de texto y pie con boton Cancelar (estilo neutro) y boton de accion
#' (color e icono configurables). Centraliza el patron usado en los modulos
#' `Embudo*` y `Formulario*` para descarte, guardado y calificacion.
#'
#' No dispara `showModal()` por si solo; se usa junto a
#' `MostrarModalConClase()` o `shiny::showModal()`. Consulte
#' `MostrarModalConfirmacion()` para combinar ambos pasos.
#'
#' @param ns Funcion. Namespace del modulo (`session$ns`).
#' @param titulo String. Titulo del modal, por ejemplo, `"Confirmar descarte"`.
#' @param texto String. Cuerpo del mensaje (se envuelve en `tags$p()`).
#' @param id_cancelar String. Input id (sin `ns()`) del boton Cancelar.
#' @param id_confirmar String. Input id (sin `ns()`) del boton de accion.
#' @param label_confirmar String. Texto del boton de accion.
#' @param icono_confirmar String. Icono Font Awesome del boton de accion.
#' @param color_confirmar String. Color de fondo del boton de accion. Por
#'   defecto `"#198754"` (verde, usado para guardar o confirmar acciones
#'   positivas).
#' @param label_cancelar String. Texto del boton cancelar. Por defecto
#'   `"Cancelar"`.
#' @param icono_cancelar String. Icono del boton cancelar. Por defecto
#'   `"xmark"`.
#' @param easyClose Logical. Indica si el modal se cierra al hacer clic afuera.
#'   Por defecto `FALSE`.
#'
#' @return `shiny.tag` generado por `modalDialog()`.
#'
#' @examples
#' \dontrun{
#' ModalConfirmacion(
#'   ns = session$ns, titulo = "Confirmar descarte",
#'   texto = "¿Deseas descartar este lead?",
#'   id_cancelar = "DES_Cancelar", id_confirmar = "DES_Confirmar",
#'   label_confirmar = "Descartar Lead", icono_confirmar = "ban",
#'   color_confirmar = "#C11007"
#' )
#' }
#'
#' @export
ModalConfirmacion <- function(ns, titulo, texto, id_cancelar, id_confirmar,
                              label_confirmar, icono_confirmar,
                              color_confirmar = "#198754",
                              label_cancelar = "Cancelar",
                              icono_cancelar = "xmark",
                              easyClose = FALSE) {
  shiny::modalDialog(
    title = titulo,
    easyClose = easyClose,
    footer = shiny::tagList(
      racafeShiny::Boton(
        ns(id_cancelar),
        label = label_cancelar,
        icono = icono_cancelar,
        color_fondo = "transparent",
        color_fuente = "#6c757d"
      ),
      racafeShiny::Boton(
        ns(id_confirmar),
        label = label_confirmar,
        icono = icono_confirmar,
        color_fondo = color_confirmar
      )
    ),
    shiny::tags$p(texto)
  )
}

#' MostrarModalConfirmacion
#'
#' Combina `ModalConfirmacion()` y `MostrarModalConClase()` con la clase
#' `"aviso"`, el tamano compacto estandar para confirmaciones del CRM.
#'
#' @inheritParams ModalConfirmacion
#' @param clase String. Clase CSS del modal. Por defecto `"aviso"`.
#'
#' @return Nada (efecto secundario; consulte `MostrarModalConClase()`).
#'
#' @examples
#' \dontrun{
#' shiny::observeEvent(input$DES_Solicitar, {
#'   MostrarModalConfirmacion(
#'     ns = session$ns, titulo = "Confirmar descarte",
#'     texto = "¿Deseas descartar este lead?",
#'     id_cancelar = "DES_Cancelar", id_confirmar = "DES_Confirmar",
#'     label_confirmar = "Descartar Lead", icono_confirmar = "ban",
#'     color_confirmar = "#C11007"
#'   )
#' })
#' }
#'
#' @export
MostrarModalConfirmacion <- function(ns, titulo, texto, id_cancelar,
                                     id_confirmar, label_confirmar,
                                     icono_confirmar,
                                     color_confirmar = "#198754",
                                     label_cancelar = "Cancelar",
                                     icono_cancelar = "xmark",
                                     easyClose = FALSE, clase = "aviso") {
  MostrarModalConClase(
    ModalConfirmacion(
      ns = ns,
      titulo = titulo,
      texto = texto,
      id_cancelar = id_cancelar,
      id_confirmar = id_confirmar,
      label_confirmar = label_confirmar,
      icono_confirmar = icono_confirmar,
      color_confirmar = color_confirmar,
      label_cancelar = label_cancelar,
      icono_cancelar = icono_cancelar,
      easyClose = easyClose
    ),
    clase
  )
}
