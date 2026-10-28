# racafeForms 0.3.0

- Añade cargador local versionado de Compartido, con publicación acotada a Styles, Scripts e img.
- Añade paneles, tarjetas, métricas, cuadrículas informativas y barras de acciones reutilizables.
- Añade modales que reutilizan los siete tamaños CSS existentes de Compartido.
- Añade copia de texto y PNG con resultado real del navegador, sin registrar contenido sensible.
- Añade campos readonly con copia optativa y reset de formularios por bindings nativos.
- Amplía argumentos opcionales de campos para ayuda, errores, requerido, deshabilitado y readonly cuando corresponde.
- Añade catálogo de sugerencias de texto, estados loading/empty y detalles técnicos explícitos y escapados.
- Conserva la API anterior y corrige el icono duplicado del botón de descarga, manteniendo downloadHandler nativo.
- Los estilos y scripts nuevos residen solo en Compartido; los recursos base anteriores se conservan.
- Incluye demo Shiny/bs4Dash, pruebas de navegador, matriz de compatibilidad y migración representativa.

# racafeForms 0.2.1

- Unifica la apariencia de los calendarios de fecha y rango: colores, fuente, bordes, sombra, dimensiones y navegación.
- Ambas vistas diarias comienzan en lunes y muestran seis semanas completas.
- Conserva Air Datepicker para fecha, Bootstrap Datepicker para rango y sus actualizadores nativos.
- Limita los estilos a instancias racafeForms y respeta las tres densidades del formulario.

# racafeForms 0.2.0

- Centra el contenido de los selectores y trunca etiquetas extensas sin cambiar la altura del control.
- Agrega `gender` y resúmenes de selección: Ninguno/Ninguna, etiqueta individual, conteo y Todos/Todas.
- Conserva selecciones válidas y vacías al actualizar opciones desde la sesión Shiny.
- Sincroniza el estado deshabilitado del selector y su botón al usar shinyjs.
- Cambia el separador del rango a `-` y distribuye las fechas con el mismo ancho y altura.
- Aplica el calendario corporativo solo a rangos racafeForms, también dentro de bs4DashControlbar.
- Adjunta CSS y JavaScript a los selectores y rangos sin requerir un contenedor de formulario.

# racafeForms 0.1.3

- Compacta el switch dentro de los formularios: pista de 28 px y botón de 16 px.

# racafeForms 0.1.2

- Reduce proporcionalmente los iconos de SweetAlert en alertas y confirmaciones.

# racafeForms 0.1.1

- Unifica altura visual de pickerInput y autonumericInput en 36 px.
- Verifica que la demostración de presupuesto usa `format = "cop"` (separadores colombianos).

# racafeForms 0.1.0

- Primera version: formularios, selects, numericos, fechas, rangos, switches,
  radio, checkbox, botones, descargas, SweetAlerts, actualizadores y tema CSS.
