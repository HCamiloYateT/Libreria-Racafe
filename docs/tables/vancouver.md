# racafeTableVancouver

Nueva API de racafeShiny 1.4.0. Usa racafeCompactTable como base gt (que reutiliza gt_minimal_style),
y conserva el mecanismo existente de IDs de encabezado únicos. No modifica los defaults de esas funciones
ni añade estilos globales a Bootstrap, AdminLTE o tablas de otros paquetes.

La presentación usa opciones nativas gt y estilos por celda. Solo el ancho mínimo del contenedor Shiny
requiere una regla CSS, generada con el ID único de la tabla. El resultado conserva gt_tbl y puede continuar
transformándose con gt. No calcula totales ni cambia datos, tipos, orden o número de filas.

| Parámetros | Uso |
| --- | --- |
| header_color, header_width | Línea inferior del encabezado, #DA291C y 1px por defecto |
| background | Fondo neutro de la tabla |
| total_background | Gris uniforme de fila/columna Total, incluida la intersección |
| total_border_color, total_border_width | Línea superior de fila Total, 1px gris claro por defecto |
| total_label, total_rows, total_columns | Reconocimiento de totales por etiqueta; puede desactivarse |
| align | Vector completo o nombrado; override de texto izquierda/números derecha |
| labels | Etiquetas visibles por nombre original de columna |
| font_size, row_padding, column_padding | Tipografía y separación uniforme |
| width, min_width, max_height | Ancho del contenedor, ancho mínimo y altura opcional |
| decimals | Formato con coma de miles y punto decimal; NULL no reformatea |
| show_header, title, subtitle, source_note | Encabezado y textos auxiliares |

Los totales se reconocen ignorando mayúsculas y espacios exteriores: la primera celda identifica una fila,
y el encabezado visible identifica una columna. Se aplica el mismo background-color en la
intersección, sin añadir capas de opacidad. Las demás celdas no tienen cuadrícula ni bordes verticales.

```r
datos <- data.frame(Producto = c("Café", "Cacao", "Total"), Cantidad = c(2, 3, 5), Total = c(1234.56, 200, 1434.56))
tabla <- racafeShiny::racafeTableVancouver(datos)
gt::tab_source_note(tabla, "Valores de ejemplo")

racafeShiny::racafeTableVancouver(datos, header_color = "#245A81", header_width = 3,
  total_background = "#EEF0F2", align = c(Cantidad = "center"), row_padding = 9,
  labels = c(Producto = "Artículo"), min_width = "800px", max_height = "400px")
```

Utilizar nombres de columnas originales en align, incluso con labels. Los campos ya formateados se mantienen
como texto; puede indicarse align explícito. La función no interpreta texto suministrado como HTML.

En CRM Nacional se aplica a productos, detalle de operaciones e historial. El modal pasa de 900 a 1200px
máximos y las seis cajas usan racafeModulos 0.4.0 con icono y borde rojo. Se conserva una reacción de datos
compartida y seis módulos estables, sin observers nuevos por cada apertura o cambio de cliente.

## Validación

R 4.5.0, gt 0.11.1, Shiny 1.10.0 y bs4Dash 2.3.5. La nueva tabla pasa 19 comprobaciones testthat;
la integración de navegador pasa 29 comprobaciones de estilos calculados y modal, más 15 de regresión CRM
y 48 de encabezados. racafeModulos pasa sus 98 pruebas; la suite completa de racafeShiny tiene 362 PASS y
los mismos 28 fallos anteriores de formatos/clases de modal. Esos fallos no corresponden a esta API.
R CMD check de racafeShiny --no-tests y de racafeModulos no presentan errores; conservan respectivamente
1 WARNING/2 NOTEs y 4 WARNINGs/1 NOTE históricas de licencia, NEWS, ASCII y documentación/código anteriores.
La suite de racafeShiny se ejecutó también completa por separado. No se realizaron operaciones BD.
