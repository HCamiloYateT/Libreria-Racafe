# Relevo del preloader

`loader-handoff.cjs` usa Playwright, jQuery y waiter reales. La conexión Shiny se
simula para enviar busy/idle/desconexión sin una base de datos.

```sh
JQUERY_JS=/ruta/jquery.min.js WAITER_JS=/ruta/waiter.js node racafeShiny/tests/browser/loader-handoff.cjs
```

Opcional: `CHROMIUM_EXECUTABLE` selecciona Chromium. `LOADER_JS` permite probar
otra versión del script. Verifica que un overlay inicial visible tenga prioridad,
que el global aparezca cuando el inicial se oculta sin requerir otro evento busy,
que no se duplique y que cierre en idle o desconexión. También prueba los triggers
que anticipan un cálculo antes del mensaje busy del servidor.
