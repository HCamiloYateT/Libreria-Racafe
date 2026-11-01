(function ($, Shiny) {
  "use strict";
  if (!Shiny || window.racafeShinyUI) return;
  window.racafeShinyUI = true;
  function expanded(el) { el.querySelector("summary").setAttribute("aria-expanded", String(el.open)); }
  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) { return $(scope).find("details.rsh-collapsible"); },
    getValue: function (el) { return el.open; },
    subscribe: function (el, callback) {
      expanded(el);
      el.rshToggle = function () {
        expanded(el); callback(false);
        // Shiny vuelve a medir outputs y reactiva los que estaban suspendidos al ocultarse.
        $(el).trigger(el.open ? "shown" : "hidden");
      };
      el.addEventListener("toggle", el.rshToggle);
    },
    unsubscribe: function (el) { el.removeEventListener("toggle", el.rshToggle); delete el.rshToggle; },
    receiveMessage: function (el, data) {
      if (typeof data.open === "boolean") { el.open = data.open; expanded(el); }
    }
  });
  Shiny.inputBindings.register(binding, "racafeShiny.collapsible");
}(window.jQuery, window.Shiny));
