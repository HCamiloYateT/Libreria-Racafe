# Propuesta para simplificar IndFormularioUI

El código real de IndFormularioUI no está en los repositorios disponibles. Esta propuesta
es un patrón adaptable, no un reemplazo verificado de la función productiva. Para conservar
IDs, permisos, jerarquías y persistencia se necesita comparar con esa implementación.
No se modificó CRM Nacional ni otra aplicación productiva.

## Sustituciones puntuales

| Código local actual | Sustitución propuesta |
| --- | --- |
| ind_picker / crm_picker | racafeInputSelect con empty, empty_label/gender, search, size y unique_choices |
| runjs para disabled | racafeUpdateEnabled(session, ID local, enabled) |
| Mensajes HTML con estilo inline | racafeFormMessage en renderUI |
| Captura local de Escape | Dependencia central de racafeForms |
| hr manual entre grupos | racafeFormGroup(divider = TRUE) |
| Tarjetas duplicadas | racafeForm(variant = "plain") dentro del panel |
| Bloque colapsable local | racafeShiny::racafeCollapsible, si ya está disponible en la aplicación |

Conserve literalmente los IDs del original al sustituir cada llamada. Los catálogos con
valores duplicados deben comprobarse antes de activar unique_choices: se conserva la primera
etiqueta. No cambie códigos comerciales ni interprete NULL como todos sin revisar la regla
original. Mantenga las restricciones de acceso también en el servidor.

## Fragmento de referencia sin IDs inventados

Los argumentos id_cuenta_padre e id_mensaje reciben los IDs locales ya existentes; ns debe
ser el namespace completo de la UI del módulo. choices y enabled proceden de las mismas
reglas de jerarquía y permisos que ya utiliza el original.

```r
ind_cuenta_padre_ui <- function(ns, id_cuenta_padre, id_mensaje, choices, selected, enabled) {
  racafeForms::racafeFormGroup(title = "Jerarquía comercial", variant = "soft",
    racafeForms::racafeInputSelect(id_cuenta_padre, "Cuenta padre", choices,
      selected = selected, empty = TRUE, empty_label = "Ninguna", gender = "f",
      search = TRUE, size = 6, disabled = !enabled, ns = ns),
    shiny::uiOutput(ns(id_mensaje)))
}
# Dentro del moduleServer existente, reutilizando su condición y observers actuales:
# racafeForms::racafeUpdateEnabled(session, id_cuenta_padre, enabled = permiso_y_jerarquia)
# output[[id_mensaje]] <- shiny::renderUI(racafeForms::racafeFormMessage(mensaje_actual, "info"))
```

Si el módulo ya dispone de racafeShiny y necesita una sección colapsable, el contenedor puede
ser `racafeShiny::racafeCollapsible(ns(id_colapsable_existente), "Jerarquía comercial", ...)`.
No introduzca racafeShiny como dependencia de racafeForms: esta composición pertenece a
la aplicación consumidora.

Aplique estos cambios únicamente a UI y presentación. Mantenga las condiciones de permisos,
validaciones comerciales, construcción de registros, callbacks, confirmaciones de escritura
real y funciones de persistencia originales. Compare los valores reactivos antes/después en
los escenarios de crear, editar, selección vacía, cambio de padre y permisos restringidos.
