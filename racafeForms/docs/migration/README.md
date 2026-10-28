# Migración representativa de interfaces manuales

No se han recibido ni modificado `01_crear_firma.R` o `02_consultas_firmas.R` reales.
Los dos ejemplos R de esta carpeta son funciones completas y sintácticamente válidas
para un bloque representativo, con datos ficticios suministrados por la aplicación.

| Implementación equivalente del bloque de UI | Líneas no vacías, sin comentarios |
|---|---:|
| before.R: HTML, estilo y listener manuales | 39 |
| after.R: composición racafeForms | 13 |

Reducción en este ejemplo: 26 líneas (66.7 %). No es una medición del proyecto Firma Digital.
La carga única `racafeSharedAssets()` de la aplicación no se repite por módulo.
La versión compuesta añade estilos compartidos, resultado de copia verificado,
namespaces y recursos deduplicados que antes debían mantenerse manualmente.

## Aplicación al caso de referencia

1. En la UI principal, cargar una copia controlada de Compartido con `racafeSharedAssets(path, version = "1.0.0")`.
2. Reemplazar el bloque exterior de creación por `racafePanel()` y los pares de datos por `racafeInfoGrid()`.
3. Usar `racafeInputReadOnly(copyable = TRUE)` para el enlace ya producido por la aplicación.
4. Para el QR PNG ya producido por la aplicación, usar `racafeCopyImage(session, "qr", imagen_png)`;
   observar `input$qr_copy_result$success` antes de afirmar que se copió.
5. Mostrar correo o QR con `shiny::showModal(racafeModal(..., size = "subventana4"))`;
   el tamaño se define por parámetro, no se inspecciona el contenido.
6. Conservar `output$archivo <- shiny::downloadHandler(...)`; `racafeButtonDownload()` se conecta al mismo ID.
7. En consultas, componer indicadores con `racafeMetricGrid()` y las acciones con `racafeActionBar()`.

SMTP, FastAPI, SQL, evidencias, credenciales, tokens, firma y reglas jurídicas siguen
siendo responsabilidad de Firma Digital. Este cambio no modifica esa aplicación.
