# Instantánea de recursos corporativos

Fuente canónica: HCamiloYateT/Compartido, commit c31d326b937be59a1b35f9eaf428d25352934330.
ASSETS_VERSION se conserva en 1.1.0; esta revisión no publica una nueva versión.
Base de la instantánea. Esta revisión incorpora además los tamaños de modal y la entrada
de componentes de Compartido, commit 7b7c5536d071fce1e7a533acb02bcff41d2953c0
(rama codex/modal-controlbar).

Copie sin modificaciones Styles/components.css, Styles/tokens.css,
Styles/racafe-forms.css, Styles/racafe-modal-sizes.css, Scripts/scripts, Scripts/clipboard.js, Scripts/forms.js,
img/logo.png, img/logo2.png y ASSETS_VERSION desde la misma revisión de Compartido.
No copie style.css: contiene reglas generales para aplicaciones.
No edite aquí los scripts ni estilos: cambie la fuente y sincronice la instantánea.
La galería y sus ejemplos usan racafeSharedAssets(global_css = FALSE).

Styles/racafe-modal-sizes.css también se distribuye, idéntico, en www/ para que
racafeFormsDependencies lo precargue sin depender de rutas externas. No edite esa copia.

Esta revisión de trabajo parte de Compartido main f6baef8 y agrega la fuente canónica
Styles/racafe-controls.css para pickers y resúmenes. ASSETS_VERSION continúa en 1.1.0
hasta una publicación autorizada. La fuente canónica se mantiene en el repositorio Compartido.
Sincronización reproducible desde Libreria-Racafe:

    Rscript racafeForms/tools/sync_shared.R /ruta/Compartido

El script valida las fuentes y copia bytes idénticos, incluidos los recursos canónicos de www.
