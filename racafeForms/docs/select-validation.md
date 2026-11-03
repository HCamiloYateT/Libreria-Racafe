# Formularios y listas agregables: migración

Esta revisión no publica ni incrementa versiones. Instale primero la copia actualizada de
racafeForms y luego racafeModulos. Reinicie R y recargue el navegador sin caché, porque los
recursos conservan la versión del paquete. No requiere cambios en Compartido ni CRM Nacional.

## Validación central

racafeValidateForm conserva la validación de servidor (required, vacíos, tipos, límites,
fechas y reglas condicionales) y añade required al resultado, sin modificar values. Las
etiquetas de los obligatorios incorporan un asterisco rojo; el texto de la etiqueta usado
en R no incluye el asterisco. Los cambios de obligatoriedad condicional se reflejan también
en la etiqueta y aria-required.

racafeShowValidation y racafeFormValidation aceptan mode = inline, alert o both para conservar
las llamadas existentes. **Todos usan ahora solo errores inline**. No hay resumen adicional,
SweetAlert ni modal por campos inválidos. Las acciones de negocio válidas pueden seguir
usando racafeConfirm. Los fallos generales sin campo concreto pueden comunicarse con
racafeNotify; no se convierten automáticamente errores de configuración en notificaciones.

Los mensajes automáticos aparecen únicamente después de validate(), salvo initial = TRUE,
y desaparecen al corregir. Un error estático del constructor se reutiliza si el validador
necesita mostrar el error en ese campo, sin agregar un segundo mensaje. Los mensajes que
la aplicación declare explícitamente mediante error= siguen siendo explícitos, incluso
antes del primer intento; no use error= para duplicar las reglas automáticas.

Una sesión registra un único racafeFormValidation por formulario. Si se llama otra vez con
la misma identidad, devuelve el objeto existente y su resultado; no duplica observers ni
restablece attempted. La primera configuración tiene prioridad. Use reglas/esquemas reactivos
para cambios dinámicos o destroy() antes de sustituir la configuración del formulario.
Asigne un ID propio a cada formulario, con el namespace completo de la sesión. destroy()
libera su registro. Las acciones deben seguir comprobando validate() antes del negocio.

## Mayúsculas de presentación

```r
racafeForms::racafeInputSelect("categoria", "Categoría",
  choices = c("Café" = "codeMixedCase", "Azúcar" = "Sugar"), uppercase = TRUE)
```

Se muestran CAFÉ, AZÚCAR, NINGUNO/NINGUNA, TODOS/TODAS y las acciones en mayúsculas. El
servidor recibe exactamente codeMixedCase o Sugar. No se cambia choices, nombres o códigos;
CSS transforma opciones, resúmenes, búsqueda y botones de selección total. Las actualizaciones
con racafeUpdateSelect conservan ese comportamiento. uppercase = FALSE lo desactiva.
No convierta el catálogo con toupper antes de pasarlo al control: podría modificar sus valores.

## Fila de captura y acción

```r
racafeForms::racafeForm(variant = "plain",
  racafeForms::racafeFormRow(actions = TRUE,
    racafeForms::racafeInputText("nuevo", "Nueva opción", required = TRUE),
    racafeForms::racafeButton("guardar", "Guardar")))
```

actions requiere dos elementos y dos columnas. Mantiene la acción a la altura del input,
no de su etiqueta, y le da la misma altura del control. Los errores debajo del campo no
desplazan el botón. El CSS está acotado a la fila corporativa; no cambia sidebars ni navbar.
El valor predeterminado FALSE conserva las filas existentes.

## Integración opcional de la galería

Instale la revisión actualizada de racafeModulos para activar la categoría Agregables de
racafeFormsGallery(). Incluye una selección simple con confirmación y otra múltiple sin
confirmación, dentro del namespace de cada ejemplo. La galería no duplica el código del
módulo ni exige racafeModulos para los controles base. Si falta esa dependencia opcional,
muestra instrucciones y permite seguir usando el resto de la galería.

La política completa del catálogo, sus IDs y las reglas de duplicados se documentan en
HCamiloYateT/racafeModulos, docs/lista-agregable.md. El módulo no modifica persistencia.

## Archivos de esta ampliación

En racafeForms: R/00_utils.R, R/01_layout.R, R/02_inputs.R, R/11_validation.R, R/gallery.R,
inst/www/racafe-forms.css y .js, DESCRIPTION, man, README, NEWS, esta guía y pruebas de
validación/galería. Los tests anteriores se actualizan para la nueva política sin alertas.
En racafeModulos: R/ListaDesplegableAgregable.R, DESCRIPTION, documentación, demo y pruebas.
Compartido se revisa sin cambios: los nuevos estilos se limitan a clases rf- del paquete.

## Validaciones ejecutadas

- racafeForms: 843 comprobaciones testthat; R CMD check --no-manual con estado OK.
- racafeModulos: 56 comprobaciones testthat; 44 comprobaciones de integración en Chromium.
- Galería: 200 comprobaciones; formularios: 43; modos heredados y zonas horarias: 11.
- Los navegadores comprueban selección simple/múltiple, mayúsculas, Todos sin acción,
  incorporación única, captura limpia, notificación, alineación con errores y namespaces anidados.
- racafeModulos conserva cuatro advertencias y una nota de R CMD check presentes también
  en main: LICENSE, caracteres no ASCII, dependencias/llamadas heredadas y documentación
  de otros componentes; la nota corresponde al formato histórico de NEWS. Sus pruebas pasan.

La comparación con main se ejecutó por separado sobre una copia de origin/main. Estas
advertencias no se ocultan ni se corrigen alterando módulos ajenos a esta solicitud.
