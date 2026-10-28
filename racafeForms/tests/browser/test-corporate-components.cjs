// Arrancar inst/examples/corporate-components.R y configurar RACAFE_TEST_URL si no usa el puerto 3860.
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const { chromium } = require("playwright");

(async function () {
  const launch = { headless: true };
  if (process.env.CHROMIUM_PATH) launch.executablePath = process.env.CHROMIUM_PATH;
  if (process.env.CI) launch.args = ["--no-sandbox"];
  const browser = await chromium.launch(launch);
  const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3860/";
  const origin = new URL(url).origin;
  const context = await browser.newContext({ viewport: { width: 1440, height: 1100 } });
  await context.grantPermissions(["clipboard-read", "clipboard-write"], { origin });
  const page = await context.newPage();
  const errors = [], external = [];
  page.on("pageerror", error => errors.push(error.message));
  page.on("response", response => { if (response.status() >= 400) errors.push(String(response.status())); });
  await page.route("**/*", route => {
    if (new URL(route.request().url()).origin !== origin) {
      external.push(route.request().url());
      route.abort();
    } else route.continue();
  });
  let checks = 0;
  function check(value, message) { assert.ok(value, message); checks += 1; }
  async function wait_text(id, text) {
    await page.waitForFunction(({ id, text }) => document.getElementById(id).textContent.includes(text), { id, text });
  }
  async function result(id) {
    return page.evaluate(id => Shiny.shinyapp.$inputValues[id], id);
  }
  async function copy_result(button, id) {
    const previous = await result(id);
    await page.locator("#" + button).click();
    await page.waitForFunction(({ id, previous }) => {
      const value = Shiny.shinyapp.$inputValues[id];
      return value && (!previous || value.request_id !== previous.request_id);
    }, { id, previous });
    return result(id);
  }
  async function modal_ready() {
    await page.waitForFunction(() => {
      const element = document.querySelector(".rf-modal");
      const modal = element && window.jQuery(element).data("bs.modal");
      return modal && !modal._isTransitioning && getComputedStyle(element).opacity === "1" &&
        !element.getAnimations({ subtree: true }).some(animation => animation.playState === "running");
    });
  }
  async function set_select(id, value) {
    await page.evaluate(({ id, value }) => {
      const element = document.getElementById(id);
      if (element.selectize) element.selectize.setValue(value);
      else window.jQuery(element).val(value).trigger("change");
    }, { id, value });
  }
  try {
    await page.goto(url);
    await wait_text("uno-valores", "nombre: Inicial");
    await wait_text("dos-valores", "catalogo: a");
    await page.locator("#uno-tabla .rf-status-badge").first().waitFor();
    check(await page.locator("#uno-tabla .rf-status-badge").first().innerText() === "Disponible",
      "Badge serializado dentro de reactable con namespace");
    check(await page.locator("#dos-tabla .rf-status-badge").count() === 1, "Tabla aislada en el segundo módulo");
    for (const size of ["compact", "normal", "comfortable", "standalone"]) {
      const text = size === "standalone" ? "standalone_text" : "density_" + size;
      const select = size === "standalone" ? "standalone_select" : "density_select_" + size;
      const range = size === "standalone" ? "standalone_range" : "density_range_" + size;
      const styles = await page.evaluate(({ text, select, range }) => {
        const picker = document.getElementById(select).closest(".bootstrap-select").querySelector("button");
        return [document.getElementById(text), picker, document.querySelector("#" + range + " input")].map(el => {
          const s = getComputedStyle(el);
          return [s.height, s.borderRadius, s.borderColor, s.fontSize, s.fontFamily, s.padding];
        });
      }, { text, select, range });
      check(JSON.stringify(styles[0]) === JSON.stringify(styles[1]) &&
        JSON.stringify(styles[0]) === JSON.stringify(styles[2]), "Campos uniformes: " + size + " " + JSON.stringify(styles));
    }
    const heights = await page.locator(".rf-card").evaluateAll(cards => ["Tarjeta breve", "Tarjeta extensa"].map(title => {
      const card = cards.find(el => el.querySelector(".rf-card-title")?.textContent === title);
      return card.getBoundingClientRect().height;
    }));
    check(heights[0] < heights[1], "Las tarjetas vecinas conservan su altura por contenido");
    const padding = await page.locator(".rf-metric").evaluateAll(cards => ["compact", "normal", "comfortable"].map(size => {
      const card = cards.find(el => el.textContent.includes("Densidad " + size));
      return parseFloat(getComputedStyle(card).paddingTop);
    }));
    check(padding[0] < padding[1] && padding[1] < padding[2], "Densidades de indicador ordenadas");
    const ratios = await page.locator(".rf-status-badge").evaluateAll(badges => {
      function luminance(color) {
        const rgb = color.match(/[\d.]+/g).slice(0, 3).map(Number).map(x => {
          x /= 255; return x <= 0.04045 ? x / 12.92 : Math.pow((x + 0.055) / 1.055, 2.4);
        });
        return rgb[0] * 0.2126 + rgb[1] * 0.7152 + rgb[2] * 0.0722;
      }
      return badges.map(el => {
        const style = getComputedStyle(el), a = luminance(style.color), b = luminance(style.backgroundColor);
        return (Math.max(a, b) + 0.05) / (Math.min(a, b) + 0.05);
      });
    });
    check(ratios.every(ratio => ratio >= 4.5), "Contraste de badges >= 4.5:1 " + ratios.join(","));
    const themed = await page.evaluate(() => {
      document.documentElement.style.setProperty("--color-red-primary", "#7a1234");
      const el = document.querySelector(".rf-status-badge--danger");
      const background = getComputedStyle(el).backgroundColor;
      const rfRed = getComputedStyle(el).getPropertyValue("--rf-red").trim();
      document.documentElement.style.removeProperty("--color-red-primary");
      return { background, rfRed };
    });
    check(themed.background === "rgb(122, 18, 52)" && themed.rfRed === "#7a1234",
      "Color corporativo compartido como única fuente para badges y controles");
    for (const columns of [2, 3, 4]) {
      check(await page.locator(".rf-metric-grid.rf-grid--" + columns).first()
        .evaluate(el => getComputedStyle(el).gridTemplateColumns.split(" ").length) === columns,
      "Cuadrícula de escritorio: " + columns);
    }
    const unique_assets = await page.evaluate(() => {
      const urls = [...document.querySelectorAll("script[src],link[rel='stylesheet'][href]")]
        .map(el => el.src || el.href);
      return new Set(urls).size === urls.length;
    });
    check(unique_assets, "Sin scripts ni hojas CSS con URL duplicada, incluidas las dos tablas reactable");
    check(await page.locator('script[src$="/racafe-forms.js"]').count() === 1, "Script base único");
    check(await page.locator("#uno-archivo i").count() === 1, "Un icono por descarga");
    check(await page.locator("#uno-sin_icono i").count() === 0, "Descarga icon=NULL sin iconos");
    check(await page.locator(".shiny-output-error").count() === 0, "Sin errores de servidor ocultos por el CSS legado");
    check(await page.locator('link[href*="shared"][href$="style.css"]').count() === 1, "CSS compartido único");
    for (const file of ["clipboard.js", "forms.js"]) {
      check(await page.locator('script[src$="/' + file + '"]').count() === 1, "Script compartido único: " + file);
    }
    check(await page.locator(".rf-panel").first().evaluate(el =>
      !el.style.minHeight && ["0px", "auto"].includes(getComputedStyle(el).minHeight)),
      "Panel sin altura mínima impuesta");
    check(await page.locator(".rf-card:not([data-rf-action])").count() > 0, "Tarjetas informativas sin eventos");
    await page.locator("#tarjeta").click(); await wait_text("tarjeta_result", "clics: 1");
    await page.locator("#tarjeta").press("Enter"); await wait_text("tarjeta_result", "clics: 2");
    await page.locator("#tarjeta").press(" "); await wait_text("tarjeta_result", "clics: 3");
    check(true, "Tarjeta interactiva optativa con teclado");
    const text = await copy_result("uno-copiar", "uno-copiar_copy_result");
    check(text.success && text.type === "text", "Texto copiado con resultado real");
    check(await page.evaluate(() => navigator.clipboard.readText()) === "Texto de ejemplo", "Portapapeles real");
    check(!("text" in text), "El resultado no reenvía el contenido copiado");
    check(!await result("dos-copiar_copy_result"), "Sin contaminación entre módulos");
    check((await copy_result("uno-referencia_copy", "uno-referencia_copy_copy_result")).success,
      "Campo readonly copiable");
    check(await page.evaluate(() => navigator.clipboard.readText()) === "REF-001", "Texto del objetivo local");
    check((await copy_result("uno-copiar_servidor", "uno-texto_r_copy_result")).success, "Copia desde R");
    check(await page.evaluate(() => navigator.clipboard.readText()) === "Texto desde R", "Resultado de copia R");
    check((await copy_result("uno-copiar_imagen", "uno-copiar_imagen_copy_result")).success, "PNG copiado");
    check(await page.evaluate(async () => (await navigator.clipboard.read()).some(item => item.types.includes("image/png"))),
      "PNG presente en el portapapeles real");
    check((await copy_result("uno-imagen_servidor", "uno-imagen_r_copy_result")).success, "PNG desde R");
    await page.locator("#uno-dinamico").click();
    await page.locator("#uno-extra_copy").waitFor();
    check((await copy_result("uno-extra_copy", "uno-extra_copy_copy_result")).success, "Copia dinámica en módulo");
    check(await page.evaluate(() => navigator.clipboard.readText()) === "Nuevo valor", "Valor dinámico real");
    const clipboard_script = await page.locator('script[src$="/clipboard.js"]').getAttribute("src");
    const before = await copy_result("uno-copiar", "uno-copiar_copy_result");
    await page.addScriptTag({ url: new URL(clipboard_script, url).href });
    const after = await copy_result("uno-copiar", "uno-copiar_copy_result");
    check(after.request_id === before.request_id + 1, "Recargar script no duplica manejadores");
    await page.evaluate(() => {
      window.rf_test_write = navigator.clipboard.writeText.bind(navigator.clipboard);
      Object.defineProperty(navigator.clipboard, "writeText", { configurable: true,
        value: () => Promise.reject(new DOMException("", "NotAllowedError")) });
    });
    const failure = await copy_result("uno-copiar", "uno-copiar_copy_result");
    check(!failure.success && failure.error === "NotAllowedError", "Permiso denegado no produce éxito falso");
    await page.evaluate(() => Object.defineProperty(navigator.clipboard, "writeText", { configurable: true,
      value: () => new Promise(resolve => { window.rf_test_resolve = resolve; }) }));
    const pending = await result("uno-copiar_copy_result");
    await page.locator("#uno-copiar").click(); await page.waitForTimeout(100);
    check((await result("uno-copiar_copy_result")).request_id === pending.request_id, "Sin éxito anticipado");
    await page.evaluate(() => window.rf_test_resolve());
    await page.waitForFunction(id => Shiny.shinyapp.$inputValues[id].success, "uno-copiar_copy_result");
    await page.evaluate(() => Object.defineProperty(navigator.clipboard, "writeText", { configurable: true,
      value: window.rf_test_write }));
    for (const [size, width] of [["ventana", 1200], ["subventana1", 1050], ["subventana2", 900],
      ["subventana3", 760], ["subventana4", 620], ["subventana5", 500], ["aviso", 380]]) {
      await set_select("modal_size", size); await wait_text("modal_selected", size);
      await page.locator("#modal").click();
      const modal = page.locator(".rf-modal:visible"); await modal.waitFor(); await modal_ready(); await modal_ready();
      if (size === "ventana") check(await modal.locator(".rf-status-badge--info").count() === 1,
        "Badge en modal creado dinámicamente");
      const box = await modal.locator(".modal-dialog").boundingBox();
      check(Math.abs(box.width - width) < 1, "Ancho de modal " + size + ": " + box.width);
      await modal.press("Escape");
      check(await modal.isVisible(), "Cierre controlado " + size);
      await modal.locator('[data-dismiss="modal"]').click(); await modal.waitFor({ state: "hidden" });
    }
    await page.locator("#modal_header").uncheck(); await page.locator("#modal_footer").uncheck();
    await page.locator("#modal_close").check(); await wait_text("modal_selected", "FALSE FALSE TRUE");
    await page.locator("#modal").click();
    const modal = page.locator(".rf-modal:visible"); await modal.waitFor(); await modal_ready();
    check(await modal.locator(".modal-header,.modal-footer").count() === 0, "Encabezado y pie opcionales");
    await modal.press("Escape"); await modal.waitFor({ state: "hidden" });
    await page.locator("#uno-nombre").fill("Modificado");
    await page.evaluate(() => window.jQuery("#uno-catalogo").selectpicker("val", ["b", "c"]).trigger("change"));
    await wait_text("uno-valores", "catalogo: b,c");
    await page.locator("#uno-reset").click();
    await wait_text("uno-reset_result", "reset: TRUE");
    await wait_text("uno-valores", "nombre: Inicial catalogo: a monto: 1200");
    check(true, "Reset con valores iniciales y widgets nativos");
    const previous_reset = await result("uno-form_reset_result");
    await page.locator("#uno-clear").click();
    await page.waitForFunction(previous => Shiny.shinyapp.$inputValues["uno-form_reset_result"].request_id !== previous,
      previous_reset.request_id);
    const cleared = await result("uno-form_reset_result");
    check(cleared.success && cleared.failures === 0, "Limpiar completa todos los bindings sin errores");
    await page.waitForFunction(() => document.getElementById("uno-nombre").value === "");
    check(await page.locator("#uno-referencia").inputValue() === "REF-001", "Limpiar conserva readonly");
    check(await page.locator("#dos-nombre").inputValue() === "Inicial", "Reset aislado por módulo");
    const download_wait = page.waitForEvent("download"); await page.locator("#uno-archivo").click();
    const download = await download_wait;
    check(download.suggestedFilename() === "ejemplo.txt", "Descarga nativa con namespace");
    check((await fs.readFile(await download.path(), "utf8")).includes("Descarga nativa"), "Contenido de descarga");
    const no_icon_wait = page.waitForEvent("download"); await page.locator("#uno-sin_icono").click();
    const no_icon = await no_icon_wait;
    check(no_icon.suggestedFilename() === "sin-icono.txt" &&
      (await fs.readFile(await no_icon.path(), "utf8")).includes("Descarga sin icono"), "Descarga sin icono funcional");

    await page.setViewportSize({ width: 390, height: 844 });
    await page.waitForTimeout(100);
    check(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1),
      "Página y acciones sin desbordamiento horizontal en móvil");
    check(await page.locator(".rf-metric-grid.rf-grid--4").first()
      .evaluate(el => getComputedStyle(el).gridTemplateColumns.split(" ").length) === 1, "Cuadrícula móvil");
    await page.locator("#modal_header").check(); await page.locator("#modal_footer").check();
    await wait_text("modal_selected", "TRUE TRUE TRUE");
    await page.locator("#modal").click(); await page.locator(".rf-modal:visible").waitFor(); await modal_ready();
    const mobile = await page.locator(".rf-modal:visible .modal-dialog").boundingBox();
    check(mobile.width <= 390 && mobile.x >= 0 && mobile.x + mobile.width <= 390, "Modal sin desbordamiento móvil");
    await page.screenshot({ path: process.env.RACAFE_SCREENSHOT || "/tmp/racafe-corporate-mobile.png" });
    await page.locator(".rf-modal:visible").press("Escape");
    await page.locator(".rf-modal:visible").waitFor({ state: "hidden" });
    for (const type of ["loading", "empty", "error"]) {
      await page.locator("#show_" + type).click();
      const popup = page.locator(".swal2-popup:visible"); await popup.waitFor();
      await popup.locator(".rf-message--" + type).waitFor();
      check(true, "Alerta accesible renderizada como HTML real: " + type);
      await popup.locator(".swal2-confirm").click(); await popup.waitFor({ state: "hidden" });
    }
    await page.evaluate(() => {
      const button = document.createElement("button");
      button.id = "dynamic-tooltip"; button.title = "Ayuda dinámica"; button.textContent = "Ayuda";
      button.style.cssText = "position:fixed;top:20px;left:20px;z-index:30000";
      document.body.appendChild(button);
    });
    await page.locator("#dynamic-tooltip").hover();
    await page.locator(".tooltip-inner").filter({ hasText: "Ayuda dinámica" }).waitFor();
    check(true, "Tooltips dinámicos desde el script original de Compartido");
    check(await page.locator(".rf-metric-value").first().evaluate(el => parseFloat(getComputedStyle(el).fontSize)) >= 22,
      "Métricas legibles también en Bootstrap 3");
    check(errors.length === 0, "Sin errores JS ni recursos fallidos: " + errors.join(", "));
    check(external.length === 0, "Sin peticiones externas: " + external.join(", "));
    check(await page.locator(".shiny-output-error").count() === 0, "Sin errores de servidor al finalizar");
    console.log(checks + " comprobaciones de navegador superadas en " + origin + ".");
  } finally { await context.close(); await browser.close(); }
}()).catch(error => { console.error(error); process.exitCode = 1; });
