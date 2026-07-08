# racafeCore

Núcleo del ecosistema Racafe. Reúne utilidades reutilizables para instalar/cargar dependencias, cargar módulos R, preparar data frames, validar texto/números, manejar fechas y construir fragmentos HTML simples.

## Instalación

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeCore")
```

## Funciones disponibles

- **Dependencias y módulos:** `Loadpkg()`, `load_modules()`.
- **Git:** `PushInicial()`.
- **Transformación:** `RecodificarTop()`, `TopAbsoluto()`, `TopRelativo()`, `AdicionarBotones()`, `bind_rows_na()`, `left_join_all()`, `RevisarDuplicados()`, `%||%`.
- **Texto y validación:** `LimpiarNombres()`, `LimpiarCadena()`, `UnirCadenas()`, `Unicos()`, `EsVacio()`, `EsEnteroPositivo()`, `EsNumero()`, `EsNumTelefono()`, `EsEmail()`, `buscar_cadena()`.
- **Numéricas y fechas:** `SiError_0()`, `Variacion()`, `Moda()`, `RedondearMultiplo()`, `PrimerDia()`, `FechaTexto()`, `EdadCumplida()`.
- **HTML:** `Saltos()`, `Espacios()`, `Obligatorio()`.

## Ejemplos rápidos

```r
library(racafeCore)

# Carga modular con reintentos por dependencias entre scripts.
dir_modulos <- tempfile("modulos_")
dir.create(dir_modulos)
writeLines("valor_base <- 2", file.path(dir_modulos, "01_base.R"))
writeLines("valor_doble <- valor_base * 2", file.path(dir_modulos, "02_calc.R"))
load_modules(dir_modulos, progress = FALSE)

# Top y recodificación de categorías.
RecodificarTop(df, categoria, valor, fun_Top = "sum", estrategia = "relativo", pct_min = 0.05, nom_var = "categoria_top")
TopAbsoluto(df, categoria, valor, fun_Top = "sum", n = 5, nom_var = "categoria_top")
TopRelativo(df, categoria, valor, fun_Top = "sum", pct_min = 0.03, nom_var = "categoria_top")

# Validación y texto.
LimpiarNombres("  camilo   yate  ")
LimpiarCadena("¡Hola, mundo 123!")
UnirCadenas("A", NA, "B", sep = "-", na.rm = TRUE)
EsEmail("usuario@racafe.com")
EsNumTelefono("3123456789")

# Fechas y números.
Variacion(100, 120)
Moda(c(1, 2, 2, 3))
PrimerDia("2026-07-08", uni = "month")
FechaTexto(Sys.Date())

# HTML simple.
Obligatorio("Nombre")
Saltos(2)
Espacios(3)
```

## Documentación

Después de instalar, consulta cualquier función con `help()`:

```r
help(load_modules)
?RecodificarTop
?FechaTexto
```
