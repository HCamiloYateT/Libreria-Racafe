# Política de dependencias y reproducibilidad

## Objetivo
Asegurar que el monorepo tenga builds reproducibles y minimizar riesgo por cambios de terceros.

## Lockfile de desarrollo (`renv`)
- El repositorio mantiene `renv.lock` en la raíz para fijar versiones de paquetes del entorno de desarrollo/CI.
- Cada cambio en dependencias (alta, baja o actualización) debe ir acompañado por actualización explícita del lockfile.
- Comandos base:

```r
renv::restore()
renv::snapshot(prompt = FALSE)
```

## Política de actualización mensual
- Frecuencia: una ventana mensual de actualización (primera semana de cada mes).
- Flujo recomendado:
  1. `renv::update()` en rama dedicada.
  2. Ejecutar `R CMD check` + pruebas unitarias + pruebas de integración.
  3. Revisar changelogs de dependencias críticas.
  4. Publicar PR con resumen de impacto.

## Revisión de dependencias no usadas
- En cada PR, revisar que `Imports` y `Suggests` de cada `DESCRIPTION` correspondan a uso real.
- Dependencias que no se usan se deben remover para reducir superficie de mantenimiento y seguridad.
- Comando recomendado para auditoría manual por paquete:

```r
desc::desc_get_deps(file = "<paquete>/DESCRIPTION")
```

## Quality gates asociados
- `R CMD check` debe pasar para todos los subpaquetes.
- La cobertura no puede bajar respecto al baseline del PR y debe cumplir el umbral mínimo configurado.
