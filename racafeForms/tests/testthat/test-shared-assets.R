# Recursos locales versionados ----
.shared_fixture <- function() {
  path <- tempfile("rf-shared-")
  dir.create(path)
  for (directory in c("Styles", "Scripts", "img", ".git", "utils")) dir.create(file.path(path, directory))
  writeLines("1.0.0", file.path(path, "ASSETS_VERSION"))
  for (file in c("Styles/style.css", "Styles/racafe-forms.css", "Scripts/scripts", "Scripts/clipboard.js",
                  "Scripts/forms.js", "img/logo.png", "img/logo2.png")) writeLines("", file.path(path, file))
  path
}

.shared_cleanup <- function(prefix) {
  for (name in names(shiny::resourcePaths())) {
    if (startsWith(name, prefix)) shiny::removeResourcePath(name)
  }
}

test_that("el cargador publica solo los tres directorios y deduplica las dependencias", {
  path <- .shared_fixture()
  prefix <- basename(path)
  on.exit({ .shared_cleanup(prefix); unlink(path, recursive = TRUE) })
  assets <- racafeSharedAssets(path, version = "1.0.0", prefix = prefix)
  again <- racafeSharedAssets(path, version = "1.0.0", prefix = prefix)
  paths <- shiny::resourcePaths()
  paths <- paths[startsWith(names(paths), prefix)]
  expect_length(paths, 3L)
  expect_setequal(basename(paths), c("Styles", "Scripts", "img"))
  expect_false(any(paths == normalizePath(path)))
  expect_match(attr(assets, "image_url"), "-img", fixed = TRUE)
  deps <- htmltools::resolveDependencies(htmltools::renderTags(shiny::tagList(assets, again,
    racafePanel("Panel", racafeMetric(1, "Uno"))))$dependencies)
  names <- vapply(deps, function(x) x$name, character(1))
  expect_equal(sum(names == "racafeForms"), 1)
  expect_equal(sum(names == "racafeSharedAssets"), 1)
  shared <- deps[[which(names == "racafeSharedAssets")]]
  expect_length(shared$script, 3L)
  expect_false(any(grepl("github|https:", shared$script)))
  expect_error(racafeSharedAssets(path, version = "2.0.0", prefix = prefix), "requiere")
  expect_error(racafeSharedAssets(path, prefix = "../privado"), "prefijo")
  expect_error(racafeSharedAssets(path, css = NA), "lógico")
  other <- .shared_fixture()
  on.exit(unlink(other, recursive = TRUE), add = TRUE)
  expect_error(racafeSharedAssets(other, prefix = prefix), "otra copia")
})

test_that("CSS y JavaScript compartidos pueden desactivarse sin exponer directorios innecesarios", {
  path <- .shared_fixture()
  prefix <- basename(path)
  on.exit({ .shared_cleanup(prefix); unlink(path, recursive = TRUE) })
  assets <- racafeSharedAssets(path, css = FALSE, javascript = FALSE, prefix = prefix)
  paths <- shiny::resourcePaths()
  paths <- paths[startsWith(names(paths), prefix)]
  expect_length(paths, 1L)
  expect_identical(basename(unname(paths)), "img")
  deps <- htmltools::resolveDependencies(htmltools::renderTags(assets)$dependencies)
  shared <- Filter(function(x) x$name == "racafeSharedAssets", deps)[[1]]
  expect_null(shared$stylesheet)
  expect_null(shared$script)
  expect_error(racafeSharedAssets(tempdir()), "ASSETS_VERSION")
  unlink(file.path(path, "Scripts/clipboard.js"))
  expect_error(racafeSharedAssets(path), "incompleta")
})

test_that("versiones antiguas y configuraciones vacías se validan antes de publicar recursos", {
  path <- .shared_fixture()
  prefix <- basename(path)
  on.exit({ .shared_cleanup(prefix); unlink(path, recursive = TRUE) })
  writeLines("0.9.0", file.path(path, "ASSETS_VERSION"))
  expect_error(racafeSharedAssets(path), ">= 1.0.0", fixed = TRUE)
  writeLines("1.0.0", file.path(path, "ASSETS_VERSION"))
  assets <- racafeSharedAssets(path, css = FALSE, javascript = FALSE, images = FALSE, prefix = prefix)
  expect_null(attr(assets, "image_url"))
  expect_false(any(startsWith(names(shiny::resourcePaths()), prefix)))
  expect_error(racafeInputReadOnly("r", "R", ns = "m"), "función")
})
