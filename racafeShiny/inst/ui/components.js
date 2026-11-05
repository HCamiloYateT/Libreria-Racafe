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

// LISTAS Y OVERLAYS ----
(function ($, Shiny) {
  "use strict";
  if (!Shiny || window.rshPresentation) return;
  window.rshPresentation = true;
  const overlays = new Map();
  function select_item(event) {
    if (!(event.target instanceof Element)) return;
    const item = event.target.closest('.rsh-list-item[data-rsh-input]');
    if (!item || item.getAttribute('aria-disabled') === 'true') return;
    if (event.type === 'keydown') {
      if (!['Enter', ' '].includes(event.key)) return;
      event.preventDefault();
    }
    const value = JSON.parse(item.dataset.rshValue), id = item.dataset.rshInput;
    document.querySelectorAll('.rsh-list-item[data-rsh-input]').forEach(function (other) {
      if (other.dataset.rshInput !== id) return;
      other.classList.toggle('rsh-list-item--selected', other === item);
      other.setAttribute('aria-pressed', String(other === item));
    });
    Shiny.setInputValue(id, value, {priority:'event'});
    item.dispatchEvent(new CustomEvent('rsh:list-select', {bubbles:true, detail:{id:id, value:value}}));
  }
  document.addEventListener('click', select_item);
  document.addEventListener('keydown', select_item);
  function overlay(id, show) {
    overlays.set(id, show);
    const host = document.getElementById(id);
    if (!host || !host.classList.contains('rsh-overlay')) return;
    host.setAttribute('aria-busy', String(show));
    host.querySelector('.rsh-overlay-cover').hidden = !show;
  }
  Shiny.addCustomMessageHandler('rsh-overlay', function (data) {
    overlay(data.id, data.show);
  });
  function triggered(id) {
    document.querySelectorAll('.rsh-overlay[data-rsh-triggers]').forEach(function (host) {
      const triggers = JSON.parse(host.dataset.rshTriggers);
      if (triggers.includes(id)) overlay(host.id, true);
    });
  }
  document.addEventListener('click', function (event) {
    if (!(event.target instanceof Element)) return;
    const button = event.target.closest('.action-button');
    if (button && !button.disabled) triggered(button.id);
  }, true);
  $(document).on('shiny:inputchanged.rshOverlay', function (event) {
    if (event.value === true || (typeof event.value === 'number' && event.value > 0)) triggered(event.name);
  });
  $(document).on('shiny:disconnected.rshOverlay', function () {
    overlays.forEach((show,id) => overlay(id,false));
    document.querySelectorAll('.rsh-overlay').forEach(host => overlay(host.id,false));
  });
  $(document).on('shiny:error.rshOverlay', function (event) {
    const host = event.target.closest && event.target.closest('.rsh-overlay');
    if (host) overlay(host.id,false);
  });
  $(function () {
    new MutationObserver(function (records) {
      if (records.some(record => Array.from(record.addedNodes).some(node => node.nodeType === 1 &&
        (node.matches('.rsh-overlay') || node.querySelector('.rsh-overlay'))))) overlays.forEach((show,id) => overlay(id,show));
    }).observe(document.body, {childList:true, subtree:true});
  });
}(window.jQuery, window.Shiny));
