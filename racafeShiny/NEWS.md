# racafeShiny 1.2.0

- Amplía los loaders a doce operaciones sin cambiar sus métodos ni argumentos anteriores.
- Incluye logo2.png local por defecto; añade show_logo y respaldo para imágenes fallidas.
- Evita duplicar el automático con waiters existentes o el preloader nativo de bs4Dash.
- Limpia loaders corporativos al desconectar y conserva modales/notificaciones.
- Amplía demo, documentación y pruebas de variantes, recursos locales y operaciones reales de prueba.
- No simula progreso porcentual de tareas síncronas ni añade dependencias obligatorias.

# racafeShiny 1.1.0

- Añade racafePreloader con variantes inicio, actualizar y calculando basadas en PlantillaBs4Dash.
- Añade racafeLoaderUI para activación en el navegador antes de tareas bloqueantes y detección busy/idle.
- Añade objetos racafeLoader sobre waiter con show, update y hide, aislados por sesión y namespace.
- Incorpora recursos locales, movimiento reducido, cierre automático y demo modular Shiny/bs4Dash.

# Separadores numéricos

- Unifica los formatos predeterminados en punto decimal y coma de miles (1,234.56).
- Fija los separadores de controles y formatos para evitar diferencias regionales.

# Novedades

Este paquete sigue **Semantic Versioning (SemVer)**: `MAJOR.MINOR.PATCH`.

## Desarrollo

### Added
- `BotonDescarga()` acepta `label` para mostrar texto junto al icono del botón.
- `InputDinero()`, `InputPorcentaje()` e `InputNumero()` ofrecen controles
  `autoNumeric` compactos con etiqueta y formatos de moneda, porcentaje o
  numero simple.
- `ListaDesplegable()` acepta `size` para fijar el numero minimo de opciones
  visibles cuando el selector se repuebla dinamicamente.

## 1.0.0 - 2026-03-25

### Added
- Publicación inicial del paquete dentro del monorepo Racafe.
