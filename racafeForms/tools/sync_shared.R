# Sincronizar copias exactas de la fuente corporativa, sin editar los artefactos ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Uso: Rscript racafeForms/tools/sync_shared.R /ruta/Compartido")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
package <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
source <- normalizePath(args[[1]], mustWork = TRUE)
files <- c("ASSETS_VERSION", file.path("Styles", c("components.css", "tokens.css", "racafe-forms.css",
  "racafe-modal-sizes.css", "racafe-controls.css")), file.path("Scripts", c("scripts", "clipboard.js", "forms.js")),
  file.path("img", c("logo.png", "logo2.png")))
if (!all(file.exists(file.path(source, files)))) stop("La fuente corporativa está incompleta.")
for (file in files) {
  bytes <- readBin(file.path(source, file), "raw", n = file.info(file.path(source, file))$size)
  destination <- file.path(package, "inst", "gallery-assets", file)
  dir.create(dirname(destination), showWarnings = FALSE, recursive = TRUE)
  writeBin(bytes, destination)
}
for (file in c("racafe-modal-sizes.css", "racafe-controls.css")) {
  path <- file.path(source, "Styles", file)
  writeBin(readBin(path, "raw", n = file.info(path)$size), file.path(package, "inst", "www", file))
}
cat("Recursos sincronizados desde", source, "\n")
