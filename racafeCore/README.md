# racafeCore

<!-- badges: start -->
[![R-CMD-check](https://github.com/HCamiloYateT/Racafe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HCamiloYateT/Racafe/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Núcleo del ecosistema Racafe. Sin dependencias internas — base requerida
por todos los demás paquetes del ecosistema.

## Instalación

```r
remotes::install_github("HCamiloYateT/Racafe/racafeCore")
```

## Contenido

### Registro de formatos
```r
# Formatos disponibles por defecto: numero, decimal, dinero, porcentaje, variacion
ListarFormatos()
ObtenerFormato("dinero")(1250000)         # "$1.250.000"
ObtenerFormato("porcentaje")(0.853)       # "85,3%"

# Registrar formato propio
DefinirFormato("miles_usd", scales::dollar_format(big.mark = ",", prefix = "USD "))
```

### Transformación
```r
# Top N categorias (absoluto o relativo)
TopAbsoluto(df, categoria, valor, "sum", n = 5, nom_var = "cat_top")
TopRelativo(df, categoria, valor, "sum", pct_min = 0.03, nom_var = "cat_top")

# Funcion unificada con estrategia como parametro
RecodificarTop(df, categoria, valor, "sum", estrategia = "relativo",
  pct_min = 0.05, nom_var = "cat_top")

# Combinacion segura de data.frames
bind_rows_na(df1, NULL, df2, data.frame())    # ignora vacios y NULLs

# Joins encadenados
left_join_all(base, list(dim1, dim2, dim3), by = "id")

# Revisar duplicados antes de un join
RevisarDuplicados(tabla_a, tabla_b, by = c("id", "fecha"))

# Null-coalescing
valor <- entrada %||% "valor_por_defecto"
```

### Texto y validación
```r
LimpiarNombres("  camilo   yate  ")     # "CAMILO YATE"
LimpiarCadena("¡Hola, mundo 123!")      # "Hola mundo"
UnirCadenas("A", NA, "B", sep = "-", na.rm = TRUE)  # "A-B"
Unicos(c("b","a","a","c"))              # c("a","b","c")

EsVacio(c("", NA, "texto"))             # c(TRUE, TRUE, FALSE)
EsEmail("usuario@racafe.com")           # TRUE
EsNumTelefono("3123456789")             # TRUE
EsNumero("12.5")                        # TRUE
EsEnteroPositivo("7")                   # TRUE
```

### Numéricas y fechas
```r
Variacion(100, 120)                     # 0.20
Variacion(0, 50)                        # NA  (sin division por cero)
Moda(c(1, 2, 2, 3))                     # 2
RedondearMultiplo(17, 5)                # 15
SiError_0(log(-1))                      # 0  (captura warning)

PrimerDia("2024-10-15")                 # 2024-10-01
PrimerDia("2024-10-15", uni = "year")   # 2024-01-01
FechaTexto(Sys.Date())                  # "octubre de 2024"
EdadCumplida(as.Date("1990-05-25"), Sys.Date())
```

### HTML utils
```r
Saltos(2)              # <br><br>
Espacios(3)            # &nbsp;&nbsp;&nbsp;
Obligatorio("Nombre")  # "Nombre <span style='color:#C0392B;'>*</span>"
```
