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
        return option.value !== "" && option.value !== wrapper.dataset.rfActionValue &&
          !option.disabled && !$(option).parent("optgroup:disabled").length;
      });
      var selected = options.filter(function (option) { return option.selected; }).length;
      if (selected === options.length && selected > 0) return feminine ? "Todas" : "Todos";
      return selected + (feminine ? " seleccionadas" : " seleccionados");
    };
    // render no emite change ni modifica los valores del select.
    picker.render();
  }

  // Las opciones de acción no participan en Todos ni en el conteo del catálogo.
  if (!$.data(document, "rfActionOptions")) {
    $.data(document, "rfActionOptions", true);
    document.addEventListener("click", function (event) {
      var button = event.target.closest && event.target.closest(".bs-select-all");
      var field = button && button.closest(".rf-select[data-rf-action-value]");
      if (!field && button) {
        var menu = button.closest(".rf-select-menu"), select = menu && document.getElementById(menu.dataset.rfSelectId);
        field = select && select.closest(".rf-select[data-rf-action-value]");
      }
      if (!field) return;
      var select = field.querySelector("select");
      if (!select || select.disabled) return;
      event.preventDefault(); event.stopImmediatePropagation();
      var values = Array.from(select.options).filter(function (option) {
        return option.value !== field.dataset.rfActionValue && option.value !== "" &&
          !option.disabled && !$(option).parent("optgroup:disabled").length;
      }).map(function (option) { return option.value; });
      $(select).selectpicker("val", values);
    }, true);
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

// Validación y descargas autorizadas por el servidor; no cambia bindings nativos.
(function ($, Shiny) {
  "use strict";
  if (!Shiny || window.racafeFormValidation) return;
  window.racafeFormValidation = true;
  var pendingFocus = null, approved = new WeakSet(), validations = new Map();
  function wrapper(id) {
    var input = document.getElementById(id);
    return input ? (input.closest(".rf-field") || input.closest(".shiny-input-container")) : Array.from(document.querySelectorAll(".rf-field[data-rf-id]")).find(
      function (field) { return field.dataset.rfId === id; });
  }
  function focusField(id) {
    var field = wrapper(id);
    if (!field) return;
    var parent = field.parentElement;
    while (parent) { if (parent.tagName === "DETAILS") parent.open = true; parent = parent.parentElement; }
    var target = field.querySelector(".bootstrap-select > button") || document.getElementById(id);
    if (!target || !target.matches("input,select,textarea,button")) target = field.querySelector("input,select,textarea,button");
    if (target && !target.getClientRects().length) {
      field.tabIndex = -1; target = field;
    }
    if (target) { target.focus(); target.scrollIntoView({ block: "center", behavior: "auto" }); }
  }
  function tryFocus() {
    if (pendingFocus && wrapper(pendingFocus) && !document.querySelector(".swal2-container")) {
      var id = pendingFocus; pendingFocus = null; focusField(id);
    }
  }
  $(function () {
    new MutationObserver(function (records) {
      tryFocus();
      var added = records.some(function (record) {
        return Array.from(record.addedNodes).some(function (node) {
          return node.nodeType === 1 && (node.matches(".rf-field") || node.querySelector(".rf-field"));
        });
      });
      if (added) validations.forEach(function (params, key) {
        if (key instanceof Element && !key.isConnected) { validations.delete(key); return; }
        renderValidation(Object.assign({}, params, { focus: false }));
      });
    }).observe(document.body, { childList: true, subtree: true });
  });
  function renderValidation(params) {
    var form = params.form && document.getElementById(params.form);
    if (!form && params.fields.length) {
      var first = wrapper(params.fields[0]); form = first && first.closest(".rf-form");
    }
    var fields = params.fields.map(wrapper).filter(Boolean);
    fields.forEach(function (field) {
      field.querySelectorAll(".rf-validation-error,.rf-validation-warning").forEach(function (error) {
        field.querySelectorAll("[aria-describedby]").forEach(function (control) {
          var ids = control.getAttribute("aria-describedby").split(/\s+/).filter(function (id) { return id !== error.id; });
          if (ids.length) control.setAttribute("aria-describedby", ids.join(" ")); else control.removeAttribute("aria-describedby");
        }); error.remove();
      });
      field.classList.remove("rf-validation-invalid");
      if (field.dataset.rfValidationOwned === "true") field.classList.remove("rf-field--error");
      delete field.dataset.rfValidationOwned;
      field.querySelectorAll("input,select,textarea,button.dropdown-toggle").forEach(function (control) {
        control.setAttribute("aria-invalid", field.querySelector(".rf-field-error") ? "true" : "false");
      });
    });
    if (form) form.querySelectorAll(".rf-validation-summary").forEach(function (summary) { summary.remove(); });
    (params.required || []).forEach(function (item) {
      var field = wrapper(item.id); if (!field) return;
      field.dataset.rfRequired = String(item.required);
      var label = field.querySelector("label"), star = label && label.querySelector(".rf-required");
      if (label && item.required && !star) {
        star = document.createElement("span"); star.className = "rf-required";
        star.setAttribute("aria-hidden", "true"); star.textContent = " *"; label.appendChild(star);
      } else if (star && !item.required) star.remove();
      field.querySelectorAll("input,select,textarea").forEach(function (control) {
        control.setAttribute("aria-required", String(item.required));
      });
    });
    if (params.inline) params.errors.forEach(function (item) {
      var field = wrapper(item.id);
      if (!field) return;
      var error = field.querySelector(".rf-field-error") || document.createElement("span");
      if (error.classList.contains("rf-field-error")) field.dataset.rfValidationOwned = "true";
      if (!error.id) error.id = item.id + "_validation";
      error.classList.add("rf-validation-error"); error.setAttribute("role", "alert"); error.textContent = item.text;
      field.classList.add("rf-validation-invalid"); field.appendChild(error);
      field.querySelectorAll("input,select,textarea,button.dropdown-toggle").forEach(function (control) {
        var ids = (control.getAttribute("aria-describedby") || "").split(/\s+/).filter(Boolean);
        ids.push(error.id); control.setAttribute("aria-describedby", Array.from(new Set(ids)).join(" "));
        control.setAttribute("aria-invalid", "true");
      });
    });
    if (params.inline) (params.warnings || []).forEach(function (item) {
      var field = wrapper(item.id);
      if (!field || field.querySelector(".rf-validation-error,.rf-field-error")) return;
      var notice = document.createElement("span");
      notice.id = item.id + "_warning"; notice.className = "rf-validation-warning";
      notice.setAttribute("role", "status"); notice.textContent = item.text; field.appendChild(notice);
      field.querySelectorAll("input,select,textarea,button.dropdown-toggle").forEach(function (control) {
        var ids = (control.getAttribute("aria-describedby") || "").split(/\s+/).filter(Boolean);
        control.setAttribute("aria-describedby", Array.from(new Set(ids.concat(notice.id))).join(" "));
      });
    });
    if (params.focus && params.errors.length) {
      pendingFocus = params.errors[0].id; requestAnimationFrame(tryFocus);
    }
  }
  Shiny.addCustomMessageHandler("racafe-validation", function (params) {
    var first = params.fields.length && wrapper(params.fields[0]);
    var form = (params.form && document.getElementById(params.form)) || (first && first.closest(".rf-form"));
    if (!params.inline && pendingFocus && params.fields.includes(pendingFocus)) pendingFocus = null;
    var key = form || params.fields.join("|"), previous = validations.get(key);
    if (previous) {
      var removed = previous.fields.filter(function (id) { return !params.fields.includes(id); });
      if (removed.length) renderValidation({ fields: removed, errors: [], inline: false });
    }
    validations.set(key, params); renderValidation(params);
  });
  function buttonState(el, data) {
    if (typeof data.disabled === "boolean") el.dataset.rfInactive = String(data.disabled);
    if (typeof data.loading === "boolean") el.classList.toggle("rf-btn--loading", data.loading);
    if (typeof data.success === "boolean") el.classList.toggle("rf-btn--success-feedback", data.success);
    var busy = el.classList.contains("rf-btn--loading"), disabled = busy || el.dataset.rfInactive === "true";
    el.setAttribute("aria-busy", String(busy)); el.setAttribute("aria-disabled", String(disabled));
    if (el.tagName === "BUTTON") el.disabled = disabled;
  }
  Shiny.addCustomMessageHandler("racafe-button-state", function (data) {
    var el = document.getElementById(data.id); if (el) buttonState(el, data);
  });
  document.addEventListener("click", function (event) {
    var el = event.target.closest && event.target.closest(".rf-btn");
    if (!el) return;
    if (el.getAttribute("aria-disabled") === "true" || el.disabled) {
      event.preventDefault(); event.stopImmediatePropagation(); return;
    }
    if (el.dataset.rfValidateDownload !== "true") return;
    if (approved.has(el)) { approved.delete(el); return; }
    event.preventDefault(); event.stopImmediatePropagation(); buttonState(el, { loading: true });
    Shiny.setInputValue(el.id + "_request", Date.now() + Math.random(), { priority: "event" });
  }, true);
  Shiny.addCustomMessageHandler("racafe-download-result", function (data) {
    var el = document.getElementById(data.id); if (!el) return;
    buttonState(el, { loading: false });
    if (data.allowed && el.getAttribute("aria-disabled") !== "true") { approved.add(el); el.click(); }
  });
}(window.jQuery, window.Shiny));

// Habilitación acotada y persistente para inputs dinámicos del paquete.
(function ($) {
  "use strict";
  if ($.data(document, "rfEnabled")) return;
  $.data(document, "rfEnabled", true);
  var states = new Map(), queued = false;
  function apply(id, enabled) {
    var el = document.getElementById(id);
    if (!el) return;
    var field = el.closest(".rf-field,.shiny-input-container");
    var controls = el.matches("input,select,textarea,button,a.rf-btn") ? [el] :
      Array.from(el.querySelectorAll("input,select,textarea,button"));
    controls.forEach(function (control) {
      var disabled = !enabled || control.classList.contains("rf-btn--loading");
      if (control.disabled !== disabled) control.disabled = disabled;
      control.setAttribute("aria-disabled", String(disabled));
      if (control.classList.contains("rf-btn")) control.dataset.rfInactive = String(!enabled);
      if (control.tagName === "SELECT") {
        var picker = $(control).data("selectpicker");
        if (picker) picker.checkDisabled();
      }
    });
    if (field) field.classList.toggle("rf-field--disabled", !enabled);
  }
  function register() {
    if (!window.Shiny || $.data(document, "rfEnabledRegistered")) return;
    $.data(document, "rfEnabledRegistered", true);
    Shiny.addCustomMessageHandler("racafe-enabled", function (params) {
      states.set(params.id, params.enabled); apply(params.id, params.enabled);
    });
  }
  $(register); $(document).on("shiny:connected.rfEnabled", register);
  $(document).on("shiny:bound.rfEnabled", function () { states.forEach(function (value, id) { apply(id, value); }); });
  new MutationObserver(function (changes) {
    if (!changes.some(function (change) { return change.addedNodes.length; }) || queued) return;
    queued = true;
    requestAnimationFrame(function () {
      queued = false; states.forEach(function (value, id) { apply(id, value); });
    });
  }).observe(document.documentElement, { childList: true, subtree: true });
}(window.jQuery));

// Escape pertenece primero a SweetAlert; Bootstrap conserva el modal que está debajo.
(function () {
  if (window.rfConfirmEscape) return;
  window.rfConfirmEscape = true;
  document.addEventListener("keydown", function (event) {
    if (event.key !== "Escape" && event.keyCode !== 27) return;
    var popup = document.querySelector(".swal2-container:not(.swal2-hidden) .swal2-popup");
    if (!popup) return;
    event.stopImmediatePropagation(); event.preventDefault();
    var cancel = popup.querySelector(".swal2-cancel");
    if (cancel && getComputedStyle(cancel).display !== "none") cancel.click();
    else {
      var confirm = popup.querySelector(".swal2-confirm");
      if (confirm && getComputedStyle(confirm).display !== "none") confirm.click();
    }
  }, true);
}());

// Conservar Bootstrap y evitar que bs4Dash confunda un picker con un clic externo.
(function ($) {
  "use strict";
  if ($.data(document, "rfControlbarPickers")) return;
  $.data(document, "rfControlbarPickers", true);
  var owned = new WeakSet(), bars = new WeakSet(), sized = new WeakSet(), pins = new WeakMap();
  var activeBar = null, activeAt = 0;
  // Respeta el cierre nativo: protección local durante el clic de un componente propio.
  function protect(event, sidebar) {
    if (!sidebar || !sidebar.isConnected || !document.body.classList.contains("control-sidebar-slide-open")) return;
    if (pins.has(sidebar)) return;
    var previous = sidebar.getAttribute("data-pin");
    pins.set(sidebar, previous);
    sidebar.setAttribute("data-pin", "true");
    setTimeout(function () {
      if (previous === null) sidebar.removeAttribute("data-pin"); else sidebar.setAttribute("data-pin", previous);
      pins.delete(sidebar);
    }, 0);
  }
  function own(node, sidebar) {
    if (!node || !sidebar || owned.has(node)) return;
    owned.add(node);
    node.addEventListener("click", function (event) { protect(event, sidebar); }, true);
  }
  function watchBar(sidebar) {
    if (!sized.has(sidebar) && sidebar.querySelector(".rf-form")) {
      sized.add(sidebar); sidebar.classList.add("rf-controlbar");
      var tools = Array.from(sidebar.children).filter(function (child) {
        return !child.classList.contains("control-sidebar-content");
      });
      var measure = function () {
        var height = tools.reduce(function (sum, child) { return sum + child.getBoundingClientRect().height; }, 0);
        sidebar.style.setProperty("--rf-controlbar-tools-height", height + "px");
      };
      measure();
      if (window.ResizeObserver) {
        var observer = new ResizeObserver(measure); tools.forEach(function (child) { observer.observe(child); });
      }
    }
    if (bars.has(sidebar)) return;
    bars.add(sidebar);
    sidebar.addEventListener("click", function (event) {
      activeBar = sidebar; activeAt = Date.now();
      if (event.target.closest(".rf-field,.rf-btn")) protect(event, sidebar);
    }, true);
    sidebar.addEventListener("focusin", function (event) {
      if (event.target.closest(".rf-field")) { activeBar = sidebar; activeAt = Date.now(); }
    });
  }
  function discover(node) {
    if (!(node instanceof Element)) return;
    var parentBar = node.closest("aside.control-sidebar");
    if (parentBar) watchBar(parentBar);
    if (node.matches("aside.control-sidebar")) watchBar(node);
    node.querySelectorAll("aside.control-sidebar").forEach(watchBar);
    var popups = node.matches(".air-datepicker,.datepicker-dropdown,.rf-modal") ? [node] :
      Array.from(node.querySelectorAll(".air-datepicker,.datepicker-dropdown,.rf-modal"));
    popups.forEach(function (popup) {
      var focused = document.activeElement, field = focused && focused.closest(".rf-field");
      var sidebar = field && field.closest("aside.control-sidebar");
      if (!sidebar && popup.matches(".rf-modal") && Date.now() - activeAt < 5000) sidebar = activeBar;
      own(popup, sidebar);
    });
  }
  $(function () {
    discover(document.body);
    new MutationObserver(function (records) {
      records.forEach(function (record) { record.addedNodes.forEach(discover); });
    }).observe(document.body, { childList: true, subtree: true });
  });
  function remember(event) {
    var select = event.target, field = select.closest && select.closest(".rf-select");
    var sidebar = field && field.closest("aside.control-sidebar"), picker = $(select).data("selectpicker");
    if (!field || !picker || !picker.$menu) return;
    var menu = picker.$menu[0];
    menu.classList.add("rf-select-menu");
    menu.classList.toggle("rf-select-menu--uppercase", field.classList.contains("rf-select--uppercase"));
    menu.dataset.rfMultiple = String(select.multiple);
    menu.dataset.rfSelectId = select.id;
    var style = getComputedStyle(field);
    ["--rf-font", "--rf-control-height", "--rf-input-radius", "--rf-red", "--rf-red-hover",
      "--rf-gray-300", "--rf-gray-900", "--rf-white"].forEach(function (name) {
      var value = style.getPropertyValue(name); if (value) menu.style.setProperty(name, value);
    });
    own(menu, sidebar);
    own(field, sidebar);
  }
  $(document).on("loaded.bs.select.rfControlbar shown.bs.select.rfControlbar refreshed.bs.select.rfControlbar",
    ".rf-select select", remember);
  $(document).on("shiny:bound.rfControlbar", ".rf-select select", remember);
  $(document).on("shiny:unbound.rfControlbar", ".rf-select select", function () {
    var picker = $(this).data("selectpicker");
    if (picker && picker.$bsContainer) picker.$bsContainer.remove();
  });
  $(function () { $(".control-sidebar .rf-select select").each(function () { remember({ target: this }); }); });
}(window.jQuery));
