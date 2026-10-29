(function ($) {
  "use strict";
  if (window.racafeShinyLoaders) return;
  window.racafeShinyLoaders = true;
  var host, active = false, startupDone = false;
  function configs() { return Array.from(document.querySelectorAll(".rsh-loader-config")); }
  function first(flag) { return configs().find(function (config) { return config.dataset[flag] === "true"; }); }
  function show(config, type) {
    if (!config || !window.waiter) return;
    if (!host) {
      host = document.createElement("div"); host.id = "rsh-loader-host";
      host.className = "rsh-loader-host"; host.hidden = true; document.body.appendChild(host);
    }
    if (active) return;
    var template = config.querySelector('template[data-rsh-type="' + type + '"]');
    if (!template) return;
    host.querySelectorAll(".waiter-overlay").forEach(function (overlay) { overlay.remove(); });
    host.hidden = false; host.setAttribute("aria-busy", "true"); active = true;
    window.waiter.show({ id: host.id, html: template.innerHTML, color: "rgba(255,255,255,0.65)",
      hideOnRender: false, hideOnError: false, hideOnSilentError: false, fadeOut: false });
  }
  function hide() {
    if (!host || !active) return;
    active = false; host.hidden = true; host.setAttribute("aria-busy", "false");
    window.waiter.hide(host.id);
  }
  // Captura el clic antes de que el binding de Shiny envíe la acción al servidor.
  document.addEventListener("click", function (event) {
    var target = event.target.closest && event.target.closest("[id]");
    if (!target) return;
    configs().some(function (config) {
      return Array.from(config.querySelectorAll("[data-rsh-trigger]")).some(function (trigger) {
        var button = document.getElementById(trigger.dataset.rshTrigger);
        if (!button || !button.contains(event.target) || button.disabled ||
            button.getAttribute("aria-disabled") === "true") return false;
        show(config, trigger.dataset.rshType); return true;
      });
    });
  }, true);
  $(document).on("shiny:busy.rshLoaders", function () { show(first("busy"), "calculando"); });
  $(document).on("shiny:idle.rshLoaders", function () { startupDone = true; hide(); });
  $(document).on("shiny:disconnected.rshLoaders shiny:connectionfailed.rshLoaders", hide);
  $(function () { if (!startupDone) show(first("startup"), "inicio"); });
}(window.jQuery));
