# Reporte de validación — 2026-11-04

Entorno: R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5, gt 0.11.1, shinyWidgets 0.9.1,
Chromium con Playwright. No se ejecutaron consultas ni escrituras en bases de datos.

| Validación ejecutada | Resultado |
| --- | --- |
| testthat racafeForms completo | 1120 PASS, 0 FAIL, 0 WARN |
| testthat racafeModulos completo | 80 PASS, 0 FAIL, 0 WARN |
| testthat racafeShiny navbar/loaders/ui-components | 223 PASS, 0 FAIL, 0 WARN |
| testthat racafeShiny completo | 339 PASS, 28 FAIL, 0 WARN |
| Baseline racafeShiny origin/main 4906ca7 | 300 PASS, los mismos 28 FAIL, 0 WARN |
| Playwright galería navbar | 30 comprobaciones correctas |
| Playwright galería racafeForms | 265 comprobaciones, 71 ejemplos correctos |
| Verificación estática de los seis adjuntos | 40 comprobaciones correctas |
| Construcción UI de adjuntos migrados | 7 construcciones correctas |
| R CMD build de los tres paquetes | Correcto |
| R CMD check racafeForms --no-manual | OK, incluye ejemplos y tests |
| R CMD check racafeModulos --no-manual | Sin errores; 4 WARNINGs y 1 NOTE |
| R CMD check racafeShiny --no-manual --no-tests | Sin errores; 1 WARNING y 2 NOTEs |

Los 28 fallos de racafeShiny ya existen en main: expectativas anteriores sobre formatos numéricos y
la clase del modal (Shiny incorpora `fade`). Se ejecutó la suite completa por separado, comparándola
con un checkout de main; no se ocultaron ni corrigieron cambiando APIs ajenas a esta tarea.
Por ello el check de empaquetado de racafeShiny se ejecutó explícitamente con --no-tests.

Las advertencias de empaquetado pendientes corresponden a licencia, texto no ASCII, NEWS históricos,
uso histórico de library/require y documentación de objetos antiguos de racafeModulos. Los objetos
nuevos y MenuHeader disponen de ayuda Rd; no hay discrepancias entre código y documentación.

El navegador verificó selección con ratón/Enter/Espacio, disabled, escaping, checklist reactivo,
contadores, aislamiento de dos módulos, retención/cierre de menús, acceso directo a modal, footer único,
trigger de confirmación lógica, overlay limitado al panel, interacción fuera del overlay, cierre en éxito/error,
transparencia/bordes/espaciado gt, viewport móvil, ausencia de IDs duplicados y de errores JavaScript.
La galería utiliza callbacks diferidos; no se atribuye asincronía a consultas síncronas de producción.

## Integración pendiente del consumidor

Se prepararon copias y parches de los seis adjuntos. No se proporcionó un checkout del CRM para este trabajo;
no se publicaron sus scripts, SQL ni reglas comerciales en los paquetes. La equivalencia se verificó mediante
AST, comentarios, literales SQL, llamadas DBI/racafeBD, funciones protegidas y combinaciones de checklist.
Esto no sustituye una prueba de integración con el CRM y sus permisos en staging. No se modificaron
FastAPI, servicios, credenciales ni aplicaciones productivas.
