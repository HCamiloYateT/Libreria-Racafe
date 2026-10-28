# Recursos de Compartido ----

#' Cargar una copia local versionada de los recursos corporativos
#' @param path Ruta local a Compartido; también admite la opción `racafeForms.shared_path`
#'   o la variable no secreta `RACAFE_SHARED_PATH`.
#' @param version Versión requerida de ASSETS_VERSION; NULL acepta la versión local.
#' @param css,javascript,images Activar estilos, scripts e imágenes, respectivamente.
#' @param prefix Prefijo URL propio; no es una ruta del sistema.
#' @return Lista de etiquetas con dependencias deduplicables. El atributo `image_url`
#'   contiene la URL base para logo.png y logo2.png cuando `images = TRUE`.
#' @details Publica solo Styles, Scripts e img mediante addResourcePath, nunca la raíz,
#'   .git ni utils. Los componentes originales requieren Compartido >= 1.0.0; los
#'   badges y densidades ampliadas de 0.4.0 requieren Compartido >= 1.1.0. Fije también el
#'   commit de la copia desplegada para una actualización controlada. No descarga recursos.
#'   Use una única versión por página. Las fuentes usan las familias CSS corporativas
#'   y sus alternativas locales; no se descargan fuentes externas.
#' @export
#' @examples
#' \dontrun{
#' assets <- racafeSharedAssets("vendor/Compartido", version = "1.1.0")
#' shiny::fluidPage(assets, shiny::tags$img(src = paste0(attr(assets, "image_url"), "/logo.png")))
#' }
racafeSharedAssets <- function(path = getOption("racafeForms.shared_path", Sys.getenv("RACAFE_SHARED_PATH")),
                               version = NULL, css = TRUE, javascript = TRUE, images = TRUE,
                               prefix = "racafe-shared") {
  .rf_scalar(path, "path")
  .rf_flag(css, "css")
  .rf_flag(javascript, "javascript")
  .rf_flag(images, "images")
  .rf_scalar(prefix, "prefix")
  if (!grepl("^[A-Za-z][A-Za-z0-9-]*$", prefix)) stop("'prefix' no es un prefijo URL v\u00e1lido.", call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  manifest <- file.path(path, "ASSETS_VERSION")
  if (!file.exists(manifest)) stop("Compartido no contiene ASSETS_VERSION; actualice su copia local.", call. = FALSE)
  actual <- trimws(readLines(manifest, n = 1L, warn = FALSE))
  .rf_scalar(actual, "ASSETS_VERSION")
  if (!grepl("^[0-9]+[.][0-9]+[.][0-9]+$", actual)) stop("ASSETS_VERSION no es v\u00e1lido.", call. = FALSE)
  if (utils::compareVersion(actual, "1.0.0") < 0) stop("Compartido debe ser >= 1.0.0.", call. = FALSE)
  if (!is.null(version) && !identical(.rf_scalar(version, "version"), actual)) {
    stop(sprintf("Se requiere Compartido %s; la copia local declara %s.", version, actual), call. = FALSE)
  }
  enabled <- c(Styles = css, Scripts = javascript, img = images)
  required <- c(if (css) c("Styles/style.css", "Styles/racafe-forms.css"),
                if (javascript) c("Scripts/scripts", "Scripts/clipboard.js", "Scripts/forms.js"),
                if (images) c("img/logo.png", "img/logo2.png"))
  if (!all(file.exists(file.path(path, required)))) stop("La copia de Compartido est\u00e1 incompleta.", call. = FALSE)
  base <- paste0(prefix, "-", gsub("[.]", "-", actual))
  urls <- stats::setNames(paste0(base, "-", tolower(names(enabled))), names(enabled))
  existing <- shiny::resourcePaths()
  for (directory in names(enabled)[enabled]) {
    local <- normalizePath(file.path(path, directory), winslash = "/", mustWork = TRUE)
    if (!startsWith(local, paste0(path, "/"))) stop("El directorio de recursos sale de Compartido.", call. = FALSE)
    if (urls[[directory]] %in% names(existing) && !identical(unname(existing[[urls[[directory]]]]), local)) {
      stop("El prefijo de esta versi\u00f3n ya publica otra copia de Compartido.", call. = FALSE)
    }
  }
  for (directory in names(enabled)[enabled]) shiny::addResourcePath(urls[[directory]], file.path(path, directory))
  dependency <- htmltools::htmlDependency("racafeSharedAssets", actual, src = c(href = ""), all_files = FALSE,
    stylesheet = if (css) paste0(urls[["Styles"]], "/style.css"),
    script = if (javascript) paste0(urls[["Scripts"]], c("/scripts", "/clipboard.js", "/forms.js")))
  result <- shiny::tagList(racafeFormsDependencies(), dependency)
  attr(result, "image_url") <- if (images) urls[["img"]] else NULL
  attr(result, "shared_version") <- actual
  result
}
