# racafeCore

Paquete base del monorepo Racafe. Reúne utilidades reutilizables de
transformación de datos, texto, validación, fechas, HTML, Git y carga modular de
scripts. No depende de ningún otro paquete Racafe.

## Instalación

```r
remotes::install_github(
  "HCamiloYateT/Libreria-Racafe",
  subdir = "racafeCore"
)
```

## Transformación de datos

```r
library(racafeCore)

ventas <- data.frame(
  categoria = c("A", "A", "B", "B", "C", "D"),
  valor = c(10, 20, 5, 15, 2, 1)
)

# Conservar las dos categorías con mayor suma y agrupar el resto.
TopAbsoluto(
  ventas, categoria, valor, "sum",
  n = 2, nom_var = "categoria_top"
)

# Conservar categorías que alcancen una proporción mínima.
TopRelativo(
  ventas, categoria, valor, "sum",
  pct_min = 0.10, nom_var = "categoria_top"
)

bind_rows_na(ventas, NULL, data.frame())
left_join_all(base, list(dimension_1, dimension_2), by = "id")
RevisarDuplicados(tabla_a, tabla_b, by = c("id", "fecha"))

valor <- entrada %||% "valor_predeterminado"
```

`RecodificarTop()` expone ambas estrategias (`"absoluto"` y `"relativo"`) en
una sola función. `AdicionarBotones()` agrega columnas con botones HTML a una
tabla.

## Texto y validación

```r
LimpiarNombres("  Camilo   Yate  ")
LimpiarCadena("¡Hola, mundo 123!")
UnirCadenas("A", NA, "B", sep = "-", na.rm = TRUE)
Unicos(c("b", "a", "a", "c"))

EsVacio(c("", NA, "texto"))
EsEmail("usuario@racafe.com")
EsNumTelefono("3123456789")
EsNumero("12.5")
EsEnteroPositivo("7")
```

`buscar_cadena()` busca texto dentro de archivos de una ruta mediante la
utilidad `grep` del sistema.

## Números, fechas y HTML

```r
Variacion(100, 120)
Moda(c(1, 2, 2, 3))
RedondearMultiplo(17, 5)
SiError_0(log(-1))

PrimerDia("2024-10-15")
PrimerDia("2024-10-15", uni = "year")
FechaTexto(as.Date("2024-10-15"))
EdadCumplida(as.Date("1990-05-25"), Sys.Date())

Saltos(2)
Espacios(3)
Obligatorio("Nombre")
```

## Carga de paquetes y módulos

`Loadpkg()` instala los paquetes ausentes y luego intenta cargarlos. Debido a que
modifica el entorno, es preferible declarar dependencias en `DESCRIPTION` para
paquetes y usar `Loadpkg()` solo en scripts interactivos.

`load_modules()` carga los archivos `.R` de un directorio en el entorno global,
omite `global.R` y reintenta los módulos cuyas dependencias todavía no estaban
disponibles:

```r
dir_modulos <- tempfile("modulos_")
dir.create(dir_modulos)
writeLines("valor_base <- 2", file.path(dir_modulos, "01_base.R"))
writeLines("valor_doble <- valor_base * 2", file.path(dir_modulos, "02_calc.R"))

resultado <- load_modules(dir_modulos, progress = FALSE)
resultado$ok
valor_doble
```

La función devuelve invisiblemente `ok`, `fallidos` y `errores`. Para conocer
todos los argumentos y valores de retorno, use `?load_modules` o
`help(package = "racafeCore")`.
