# Separadores numéricos

Convención predeterminada: punto decimal y coma de miles (`1,234.56`).

- racafeForms: controles autonumeric y tamaños de archivos.
- racafeShiny: controles autonumeric, formatos scales y porcentajes/variaciones gt.
- FormatoD3 y FormatoHOT ya utilizaban patrones compatibles y se conservan.
- Se mantienen APIs, valores numéricos reactivos y sobrescrituras explícitas.

Validación del 29 de octubre de 2026:

- Suite racafeForms: 507 comprobaciones sin fallos, avisos ni omisiones.
- Chromium: valor inicial `1,234.56`, edición a `9,876.54` y recepción de `9876.54`
  como valor reactivo del servidor.
- Sintaxis R verificada en racafeShiny. Se añaden pruebas de formatos scales y gt.
- La suite completa de racafeShiny no pudo ejecutarse: faltan racafeGraph, gt y
  scales en el entorno, y el acceso a CRAN rechazó la descarga de dependencias.
