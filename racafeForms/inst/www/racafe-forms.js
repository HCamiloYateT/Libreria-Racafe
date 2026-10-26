(function ($) {
  "use strict";
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

  function style_calendar(event) {
    if (!event.target.closest(".rf-date-range")) return;
    var datepicker = $(event.target).data("datepicker");
    if (!datepicker || !datepicker.picker) return;
    var calendar = datepicker.picker.addClass("rf-calendar")[0];
    var form = event.target.closest(".rf-form") || event.target.closest(".rf-date-range");
    var styles = window.getComputedStyle(form);
    ["--rf-font", "--rf-red", "--rf-gray-300", "--rf-gray-700"].forEach(function (name) {
      calendar.style.setProperty(name, styles.getPropertyValue(name));
    });
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
}(window.jQuery));
