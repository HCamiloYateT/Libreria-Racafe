# Política de versionado y changelog

Este monorepo adopta **Semantic Versioning (SemVer)** por paquete (`MAJOR.MINOR.PATCH`).
Cada paquete publica su propia versión de forma independiente en su archivo `DESCRIPTION`.

## Reglas de SemVer

- **PATCH** (`x.y.Z`): correcciones de bugs, mejoras internas o ajustes sin cambios en la API pública.
- **MINOR** (`x.Y.z`): nuevas funcionalidades retrocompatibles.
- **MAJOR** (`X.y.z`): cambios incompatibles en la API, contratos o comportamiento esperado.

## Alcance por paquete

Cada módulo de negocio/versionado mantiene su propio ciclo de vida:

- `racafeCore`
- `racafeBD`
- `racafeDrive`
- `racafeGraph`
- `racafeShiny`
- `racafeForecast`

Un release en un paquete **no obliga** a incrementar la versión de todos los demás, salvo cuando exista dependencia directa afectada por un cambio incompatible.

## Changelog

Se adopta estrategia de **`NEWS.md` por paquete**. Cada archivo `NEWS.md` se encuentra en la raíz de cada paquete e incluye:

- Sección por versión publicada.
- Clasificación sugerida: `Added`, `Changed`, `Fixed`, `Deprecated`, `Removed`, `Security`.
- Fecha de publicación en formato ISO (`YYYY-MM-DD`).

## Flujo recomendado de release

1. Actualizar cambios en el código del paquete.
2. Definir incremento SemVer apropiado en `DESCRIPTION`.
3. Registrar cambios en `NEWS.md` del paquete.
4. Ejecutar pruebas del paquete.
5. Publicar release del paquete.
