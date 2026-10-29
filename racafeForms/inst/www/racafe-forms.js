(function ($) {
  "use strict";
  // Reutilizar el binding de texto solo donde Shiny no reconoce email/tel de racafeForms.
  function register_text_fallback() {
    if (!window.Shiny || !Shiny.inputBindings) return;
    var bindings = Shiny.inputBindings.getBindings();
    if (bindings.some(function (entry) { return entry.binding.name === "racafeForms.textInput"; })) return;
    var native = bindings.find(function (entry) { return entry.binding.name === "shiny.textInput"; });
    if (!native) return;
    var fallback = Object.create(native.binding);
    fallback.find = function (scope) {
      return $(scope).find('.rf-field input.shiny-input-text[type="email"], ' +
        '.rf-field input.shiny-input-text[type="tel"]').not(native.binding.find(scope));
    };
    Shiny.inputBindings.register(fallback, "racafeForms.textInput", -1);
  }
  register_text_fallback();
  // Bootstrap Select admite funciones de conteo, pero pickerInput serializa sus opciones como atributos HTML.
  function configure_select(event) {
    var select = event.target;
    var wrapper = select.closest(".rf-select");
    var picker = $(select).data("selectpicker");
    if (!wrapper || !picker || !select.multiple) return;
    var feminine = wrapper.dataset.rfGender === "f";
    picker.options.selectedTextFormat = "count > 1";
    picker.options.countSelectedText = function () {
      var options = Array.from(select.options).filter(function (option) {
        return option.value !== "" && !option.disabled && !$(option).parent("optgroup:disabled").length;
      });
      var selected = options.filter(function (option) { return option.selected; }).length;
      if (selected === options.length && selected > 0) return feminine ? "Todas" : "Todos";
      return selected + (feminine ? " seleccionadas" : " seleccionados");
    };
    // render no emite change ni modifica los valores del select.
    picker.render();
  }

  function apply_calendar_style(calendar, input) {
    $(calendar).addClass("rf-calendar");
    var form = input.closest(".rf-form") || input.closest(".rf-date, .rf-date-range");
    var styles = window.getComputedStyle(form);
    ["--rf-font", "--rf-red", "--rf-gray-300", "--rf-gray-700"].forEach(function (name) {
      calendar.style.setProperty(name, styles.getPropertyValue(name));
    });
  }

  function style_calendar(event) {
    if (!event.target.closest(".rf-date-range")) return;
    var datepicker = $(event.target).data("datepicker");
    if (datepicker && datepicker.picker) apply_calendar_style(datepicker.picker[0], event.target);
  }

  function configure_air_calendar(event) {
    var input = event.target;
    var binding = $(input).data("shiny-input-binding");
    var instance = binding && binding.store && binding.store[input.id];
    if (!instance || !instance.opts || typeof instance.update !== "function") return;
    if ($.data(input, "rfCalendarInstance") === instance) return;
    $.data(input, "rfCalendarInstance", instance);
    var original_show = instance.opts.onShow;
    instance.update({
      fixedHeight: true,
      navTitles: Object.assign({}, instance.opts.navTitles, { days: "MMMM yyyy" }),
      onShow: function () {
        apply_calendar_style(instance.$datepicker, input);
        if (typeof original_show === "function") original_show.apply(instance, arguments);
      }
    });
    if (instance.$datepicker) apply_calendar_style(instance.$datepicker, input);
  }

  // shinyjs cambia el select nativo; sincronizar su botón sin refrescar opciones ni emitir eventos reactivos.
  var previous_observer = $.data(document, "racafeFormsObserver");
  if (previous_observer) previous_observer.disconnect();
  var disabled_observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
      var select = mutation.target;
      if (select.tagName !== "SELECT" || !select.closest(".rf-select")) return;
      var picker = $(select).data("selectpicker");
      if (picker) picker.checkDisabled();
    });
  });
  disabled_observer.observe(document.documentElement, { subtree: true, attributes: true, attributeFilter: ["disabled"] });
  $.data(document, "racafeFormsObserver", disabled_observer);

  // Un único conjunto de manejadores delegados, también para módulos e interfaces dinámicas.
  $(document).off(".racafeForms");
  $(document).on("loaded.bs.select.racafeForms refreshed.bs.select.racafeForms", ".rf-select select", configure_select);
  $(document).on("show.racafeForms", ".rf-date-range input", style_calendar);
  $(document).on("shiny:bound.racafeForms", ".rf-date .sw-air-picker", configure_air_calendar);
  $(function () {
    $(".rf-date .sw-air-picker.shiny-bound-input").each(function () { configure_air_calendar({ target: this }); });
  });
}(window.jQuery));

// Archivos y notificaciones propios; no cambia bindings ni paneles de otros componentes.
(function ($) {
  "use strict";
  if ($.data(document, "racafeFeedback")) return;
  $.data(document, "racafeFeedback", true);
  var selections = new WeakMap(), notifications = new WeakMap();
  function file_error(input, messages) {
    var wrapper = input.closest(".rf-file"), error = wrapper.querySelector(".rf-file-error");
    var text = Array.from(new Set(messages)).join(" ");
    if (error.textContent !== text) error.textContent = text;
    input.setAttribute("aria-invalid", text ? "true" : "false");
    if (wrapper.classList.contains("rf-file--error") !== !!text) wrapper.classList.toggle("rf-file--error", !!text);
  }
  function bytes_label(bytes) {
    if (bytes < 1024) return bytes + " B";
    if (bytes < 1048576) return (bytes / 1024).toLocaleString("en-US", { maximumFractionDigits: 1 }) + " KB";
    return (bytes / 1048576).toLocaleString("en-US", { maximumFractionDigits: 1 }) + " MB";
  }
  function allowed(file, accept) {
    if (!accept.length) return true;
    return accept.some(function (rule) {
      if (rule[0] === ".") return file.name.toLowerCase().endsWith(rule);
      if (rule.endsWith("/*")) return file.type.toLowerCase().startsWith(rule.slice(0, -1));
      return file.type.toLowerCase() === rule;
    });
  }
  function inspect_files(input) {
    var wrapper = input.closest(".rf-file"), list = wrapper.querySelector(".rf-file-list");
    var files = Array.from(input.files || []), errors = [], token = {};
    selections.set(input, token);
    list.textContent = "";
    var accept = (input.getAttribute("accept") || "").toLowerCase().split(",").map(function (x) {
      return x.trim();
    }).filter(Boolean);
    var max = Number(input.dataset.rfMaxSize) || Infinity;
    files.forEach(function (file) {
      var item = document.createElement("li");
      item.textContent = file.name + " (" + bytes_label(file.size) + ")";
      list.appendChild(item);
      if (!allowed(file, accept)) errors.push("Extensión o tipo no permitido.");
      if (!file.size) errors.push("El archivo está vacío.");
      if (file.size > max) errors.push("El archivo supera el tamaño máximo.");
      if (file.size) {
        var reader = new FileReader();
        function failed() {
          if (selections.get(input) !== token) return;
          errors.push("No se pudo leer el archivo.");
          file_error(input, errors);
        }
        reader.onerror = failed;
        reader.onload = function () { if (!reader.result || !reader.result.byteLength) failed(); };
        try { reader.readAsArrayBuffer(file.slice(0, 1)); } catch (error) { failed(); }
      }
    });
    var display = wrapper.querySelector('input[type="text"]');
    if (files.length > 1) display.value = files.length + " archivos seleccionados";
    else if (!files.length) display.value = "";
    file_error(input, errors);
  }
  $(document).on("change.racafeFeedback", '.rf-file input[type="file"]', function () { inspect_files(this); });
  // Los drop nativos siguen activos excepto en un control explícitamente deshabilitado.
  document.addEventListener("drop", function (event) {
    var wrapper = event.target.closest && event.target.closest(".rf-file");
    if (wrapper && wrapper.querySelector('input[type="file"]').disabled) {
      event.preventDefault(); event.stopPropagation();
    }
  }, true);

  function reconcile() {
    document.querySelectorAll('.rf-file .progress-bar-danger').forEach(function (bar) {
      var input = bar.closest(".rf-file").querySelector('input[type="file"]');
      file_error(input, ["No se pudo leer o subir el archivo."]);
    });
    var nativePanel = document.getElementById("shiny-notification-panel");
    if (!nativePanel) return;
    var owned = Array.from(nativePanel.querySelectorAll(".shiny-notification")).filter(function (wrapper) {
      return !!wrapper.querySelector(".rf-notify");
    });
    if (!owned.length) return;
    var panel = nativePanel.querySelector(".rf-notifications");
    if (!panel) {
      panel = document.createElement("div"); panel.className = "rf-notifications";
      nativePanel.appendChild(panel);
    }
    owned.forEach(function (wrapper) {
      var content = wrapper.querySelector(".rf-notify");
      if (!wrapper.classList.contains("rf-notification")) wrapper.classList.add("rf-notification");
      wrapper.dataset.rfType = content.dataset.rfType;
      if (notifications.get(wrapper) !== content) {
        notifications.set(wrapper, content); panel.appendChild(wrapper);
      }
      var close = wrapper.querySelector(".shiny-notification-close");
      if (close) {
        close.setAttribute("role", "button"); close.setAttribute("tabindex", "0");
        close.setAttribute("aria-label", "Cerrar notificación");
      }
    });
    var items = Array.from(panel.querySelectorAll(".rf-notification"));
    items.forEach(function (item, index) { item.hidden = index < items.length - 3; });
    var bottom = 0;
    document.querySelectorAll(".main-header,.navbar").forEach(function (header) {
      var rect = header.getBoundingClientRect();
      if (rect.height && rect.top < innerHeight && rect.bottom > 0) bottom = Math.max(bottom, rect.bottom);
    });
    panel.style.setProperty("--rf-notify-top", Math.max(12, bottom + 12) + "px");
    panel.hidden = !!document.querySelector(".modal.show,.modal.in,.swal2-container") ||
      (innerWidth <= 767 && document.body.classList.contains("sidebar-open"));
  }
  var queued = false;
  function schedule() {
    if (queued) return;
    queued = true;
    requestAnimationFrame(function () { queued = false; reconcile(); });
  }
  $(function () {
    new MutationObserver(schedule).observe(document.body, { childList: true, subtree: true,
      attributes: true, attributeFilter: ["class"] });
    schedule();
  });
  window.addEventListener("resize", schedule);
  window.addEventListener("scroll", schedule, true);
  $(document).on("keydown.racafeFeedback", ".rf-notification .shiny-notification-close", function (event) {
    if (event.key === "Enter" || event.key === " ") { event.preventDefault(); $(this).trigger("click"); }
  });
}(window.jQuery));
