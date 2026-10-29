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
