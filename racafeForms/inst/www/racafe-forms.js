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
