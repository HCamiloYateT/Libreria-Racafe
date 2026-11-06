# Integración de CRM Nacional y traslado de CajaValor

La implementación de cajaValor_ui y cajaValor_server se traslada a racafeModulos 0.3.0.
Los nombres, argumentos, HTML y retorno click_detalle se conservan. El servidor continúa utilizando
racafeShiny::FormatearNumero, que es responsabilidad de la capa de presentación.

racafeShiny 1.3.1 conserva ambos exports como alias que delegan en racafeModulos >= 0.3.0. Esa dependencia
es opcional (Suggests); no se añade un import inverso que forme un ciclo con racafeModulos.
Si falta el módulo o su versión es antigua, el alias explica qué paquete debe actualizarse.

Migración del consumidor:

```r
# UI del módulo padre
racafeModulos::cajaValor_ui(ns("ventas"), texto = "Ventas")
# Dentro de moduleServer
ventas <- racafeModulos::cajaValor_server("ventas", shiny::reactive(1234.56), "numero")
shiny::observeEvent(ventas$click_detalle(), shiny::showNotification("Detalle"))
```

El repositorio CRM_Nacional incorpora además las nuevas APIs de listas, tablas, detalle y overlays en
encabezado y controlbar, manteniendo sus funciones comerciales y consultas. El resumen ejecutivo cambia
únicamente las dos referencias de CajaValor al nuevo namespace. Filtros y formulario de persona ya utilizan
componentes compartidos y no requieren cambios.

Validación 2026-11-06: racafeModulos 91 PASS; racafeShiny componentes afectados 227 PASS; suite completa
racafeShiny 343 PASS y los mismos 28 fallos preexistentes. Construcción e instalación correctas. R CMD check
sin errores: racafeModulos mantiene 4 advertencias/1 nota y racafeShiny --no-tests 1 advertencia/2 notas
históricas de licencia, NEWS, ASCII y código/documentación anteriores.

En CRM se ejecutaron 33 comprobaciones estáticas/de construcción UI y 15 comprobaciones de navegador con
las interfaces reales y servidor de demostración aislado. El verificador anterior del encabezado mantiene
sus 40 comprobaciones correctas. No se cargó global.R ni se ejecutaron consultas, transacciones o escrituras.
Los flujos de negocio, permisos y PDF deben comprobarse en staging.

Instalar las librerías antes del cambio del consumidor. racafeModulos nuevo funciona con racafeShiny >= 1.3.0;
actualizar racafeShiny a 1.3.1 cuando racafeModulos 0.3.0 esté disponible para conservar las llamadas antiguas.
