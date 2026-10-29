# Loaders corporativos ----
.rsh_loader_scalar <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(name, " debe ser un texto no vacío.", call. = FALSE)
  }
  as.vector(value, mode = "character")
}

.rsh_loader_dependency <- function() {
  htmltools::htmlDependency("racafe-shiny-loaders", "1.1.0",
    src = c(file = system.file("loaders", package = "racafeShiny")),
    script = "loaders.js", stylesheet = "loaders.css")
}

#' Contenido corporativo para los preloaders de bs4Dash
#' @param type Variante: inicio, actualizar o calculando.
#' @param text Mensaje de texto plano; NULL usa el mensaje de la variante.
#' @param logo Ruta o URL del logotipo; NULL no carga imágenes externas.
#' @return Lista html/color compatible con el argumento preloader de bs4Dash.
#' @details No activa loaders automáticos. Use racafeLoaderUI para activar la detección
#'   en el navegador. Los recursos son locales y no requieren Compartido ni Bootstrap.
#' @examples
#' racafePreloader("inicio")
#' @export
racafePreloader <- function(type = c("inicio", "actualizar", "calculando"), text = NULL, logo = NULL) {
  type <- match.arg(type)
  if (is.null(text)) {
    text <- switch(type, inicio = "Cargando…", actualizar = "Actualizando…", calculando = "Calculando…")
  }
  text <- .rsh_loader_scalar(text, "text")
  # bs4Dash inserta este HTML en JavaScript: preserva también comillas y barras.
  text <- htmltools::htmlEscape(text)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text <- gsub(intToUtf8(92L), "&#92;", text, fixed = TRUE)
  text <- gsub(intToUtf8(13L), "&#13;", text, fixed = TRUE)

  if (!is.null(logo)) logo <- .rsh_loader_scalar(logo, "logo")
  card <- shiny::tags$div(class = paste("rsh-loader-card", paste0("rsh-loader--", type)),
    role = "status", `aria-live` = "polite", `aria-atomic` = "true",
    if (!is.null(logo)) shiny::tags$img(class = "rsh-loader-logo", src = logo, alt = ""),
    shiny::tags$p(class = "rsh-loader-text", htmltools::HTML(text)),
    shiny::tags$span(class = "rsh-loader-spinner", `aria-hidden` = "true"))
  list(html = htmltools::attachDependencies(card, .rsh_loader_dependency()),
       color = if (type == "inicio") "#e5e7e9" else "rgba(255,255,255,0.65)")
}

#' Instalar loaders de inicio y actividad en el navegador
#' @param startup Mostrar el loader hasta el primer evento idle de Shiny.
#' @param busy Mostrarlo durante los ciclos busy/idle de Shiny.
#' @param triggers Vector de IDs de botones que lo muestran inmediatamente al pulsar.
#'   Puede nombrar cada elemento inicio, actualizar o calculando para elegir la variante.
#' @param ns Función namespace opcional para IDs locales de triggers.
#' @param logo Ruta o URL opcional del logotipo.
#' @return Recursos y configuración UI; incluya una vez en el body de la aplicación.
#' @details La activación por clic ocurre sin esperar al servidor y conserva los bindings.
#'   El cierre automático ocurre en idle, error de conexión o desconexión. No convierte
#'   tareas síncronas en asíncronas ni permite progreso intermedio mientras R está bloqueado.
#'   No registre botones sin acción de servidor: no producirían un ciclo busy/idle.
#'   Para tareas asíncronas use el objeto racafeLoader y cierre al resolver o rechazar.
#'   No combine startup TRUE con el preloader inicial de bs4Dash: elija uno de los dos.
#' @examples
#' racafeLoaderUI(triggers = c(actualizar = "recargar", calculando = "procesar"))
#' @export
racafeLoaderUI <- function(startup = TRUE, busy = TRUE, triggers = NULL, ns = NULL, logo = NULL) {
  for (value in list(startup, busy)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) stop("startup y busy deben ser lógicos.")
  }
  if (!is.null(ns) && !is.function(ns)) stop("ns debe ser una función.", call. = FALSE)
  if (!is.null(triggers) && (!is.character(triggers) || anyNA(triggers) || any(!nzchar(triggers)))) {
    stop("triggers debe contener IDs de botones.", call. = FALSE)
  }
  types <- names(triggers)
  if (is.null(types)) types <- rep("calculando", length(triggers))
  types[!nzchar(types)] <- "calculando"
  if (any(!types %in% c("inicio", "actualizar", "calculando"))) stop("Variante de trigger no válida.")
  if (!is.null(ns)) triggers <- vapply(triggers, function(id) {
    if (startsWith(id, ns(""))) id else ns(id)
  }, character(1))
  config <- shiny::tags$div(class = "rsh-loader-config", hidden = "hidden",
    `data-startup` = tolower(as.character(startup)), `data-busy` = tolower(as.character(busy)),
    lapply(c("inicio", "actualizar", "calculando"), function(type) {
      shiny::tags$template(`data-rsh-type` = type, racafePreloader(type, logo = logo)$html)
    }),
    lapply(seq_along(triggers), function(i) {
      shiny::tags$span(`data-rsh-trigger` = triggers[[i]], `data-rsh-type` = types[[i]])
    }))
  shiny::tagList(waiter::use_waiter(),
                 htmltools::attachDependencies(config, .rsh_loader_dependency()))
}

#' Crear un objeto waiter con presentación corporativa
#' @inheritParams racafePreloader
#' @param session Sesión Shiny, incluida la sesión de un módulo.
#' @param id ID local opcional del contenedor a cubrir; NULL cubre la pantalla.
#' @param ns Namespace opcional; por defecto utiliza session$ns para el contenedor.
#' @return Objeto racafe_loader que encapsula waiter::Waiter con métodos show(), hide() y update(html).
#' @details Incluya racafeLoaderUI en la UI. En trabajo síncrono, show seguido de un
#'   cálculo bloqueante puede no enviarse a tiempo: utilice triggers o busy en la UI.
#'   Proteja el cierre con on.exit(loader$hide(), add = TRUE) en una función o un
#'   tryCatch con finally. En trabajo asíncrono cierre en ambas ramas de la promesa.
#'   No comparta el objeto entre sesiones. Para actualizar el texto use
#'   loader$update(racafePreloader("calculando", text = "Nuevo mensaje")$html).
#' @export
racafeLoader <- function(session = shiny::getDefaultReactiveDomain(), type = c("inicio", "actualizar", "calculando"),
                         text = NULL, logo = NULL, id = NULL, ns = NULL) {
  if (is.null(session)) stop("Se requiere una sesión Shiny activa.", call. = FALSE)
  preset <- racafePreloader(match.arg(type), text, logo)
  if (!is.null(id)) {
    id <- .rsh_loader_scalar(id, "id")
    if (is.null(ns)) ns <- session$ns
    if (!is.function(ns)) stop("ns debe ser una función.", call. = FALSE)
    if (!startsWith(id, ns(""))) id <- ns(id)
  }
  loader <- waiter::Waiter$new(id = id, html = preset$html, color = preset$color,
    hide_on_render = FALSE, hide_on_error = FALSE, hide_on_silent_error = FALSE)
  # Waiter consulta el dominio al invocar cada método; fijamos la sesión del objeto.
  structure(list(
    show = function() shiny::withReactiveDomain(session, loader$show()),
    hide = function() shiny::withReactiveDomain(session, loader$hide()),
    update = function(html) shiny::withReactiveDomain(session, loader$update(html))
  ), class = "racafe_loader")
}
