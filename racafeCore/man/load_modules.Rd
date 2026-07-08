% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{load_modules}
\alias{load_modules}
\title{Cargar módulos R desde un directorio}
\usage{
load_modules(...)
}
\arguments{
  \item{path}{Cadena de texto de longitud 1 con la ruta del directorio que contiene los módulos. Por defecto usa `"misc"`.}
  \item{verbose}{Valor lógico de longitud 1. Si es `TRUE`, imprime el detalle de los archivos encontrados, su profundidad y los reintentos realizados.}
  \item{progress}{Valor lógico de longitud 1. Si es `TRUE` y `verbose = FALSE`, muestra una barra de progreso de texto durante la carga. }
}
\value{Invisiblemente, una lista con tres elementos: `ok`, número de módulos cargados correctamente; `fallidos`, rutas de módulos que no pudieron cargarse; y `errores`, lista nombrada con el último mensaje de error por archivo fallido.}
\description{
Busca archivos `.R` dentro de un directorio, los ordena desde las rutas más
cercanas a la raíz hacia las más profundas y los carga en el entorno global.
Si un archivo falla por depender de objetos definidos en otro módulo, la
función reintenta únicamente los archivos pendientes hasta que todos carguen o
hasta detectar una pasada sin progreso.
}
\examples{
dir_modulos <- tempfile("modulos_")
dir.create(dir_modulos)
writeLines("valor_base <- 2", file.path(dir_modulos, "01_base.R"))
writeLines("valor_doble <- valor_base * 2", file.path(dir_modulos, "02_calc.R"))
resultado <- load_modules(dir_modulos, progress = FALSE)
resultado$ok
valor_doble
}
