const assert = require("node:assert/strict");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  const errors = [];
  async function open(url, delayed = false) {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1100 } });
    page.on("pageerror", error => errors.push(error.message));
    await page.addInitScript(() => {
      window.modalFrames = [];
      const sample = () => {
        const modal = document.querySelector(".rf-modal"), dialog = modal && modal.querySelector(".modal-dialog");
        if (dialog && getComputedStyle(modal).display !== "none") window.modalFrames.push({
          width: dialog.getBoundingClientRect().width,
          ready: getComputedStyle(document.documentElement).getPropertyValue("--rf-modal-sizes-ready").trim()
        });
        requestAnimationFrame(sample);
      };
      requestAnimationFrame(sample);
    });
    await page.route("**/*", async route => {
      const request = new URL(route.request().url());
      if (request.origin !== new URL(url).origin) return route.abort();
      if (delayed && /racafe-(forms|modal-sizes)\.css/.test(request.pathname)) {
        await new Promise(resolve => setTimeout(resolve, request.pathname.includes("forms.css") ? 2200 : 900));
      }
      return route.continue();
    });
    await page.goto(url); return page;
  }
  async function footer(page) {
    const buttons = await page.locator(".rf-modal .modal-footer button").evaluateAll(nodes => nodes.map(el => {
      const rect = el.getBoundingClientRect();
      return { text: el.textContent.trim(), cls: el.className, y: rect.y, right: rect.right,
        color: getComputedStyle(el).backgroundColor };
    }));
    check(buttons.length === 2 && buttons.every(button => /(^| )rf-btn( |$)/.test(button.cls)), "Todos los botones son rf-btn");
    check(buttons[0].cls.includes("rf-btn--secondary"), "Cancelar secundario");
    check(buttons[1].cls.includes("rf-btn--primary") && buttons[1].color === "rgb(218, 41, 28)", "Confirmar principal rojo");
    check(Math.abs(buttons[0].y - buttons[1].y) < 1, "Footer horizontal");
    const right = await page.locator(".rf-modal .modal-footer").evaluate(el => el.getBoundingClientRect().right);
    check(Math.abs(right - 16 - buttons[1].right) <= 2, "Acciones alineadas a la derecha");
  }
  async function pickerClick(page, id, text) {
    const field = page.locator(".rf-select").filter({ has: page.locator(`[id='${id}']`) });
    await field.locator("button.dropdown-toggle").click();
    const menu = page.locator(`.rf-select-menu[data-rf-select-id='${id}']`);
    await menu.locator(".dropdown-item .text").filter({ hasText: text }).first().click();
    return menu;
  }
  async function controlbar(page, prefix, single, multiple, dynamic) {
    await page.locator("#controlbar-toggle").click();
    await page.waitForFunction(() => document.body.classList.contains('control-sidebar-slide-open'));
    await page.locator(`[id='${prefix}${single}']`).waitFor({ state: "attached" });
    await page.evaluate(() => {
      window.barCollapsed = 0;
      window.documentPickerClicks = 0;
      window.jQuery('#controlbar-toggle').on('collapsed.lte.controlsidebar.rfRegression', () => window.barCollapsed++);
      document.addEventListener('click', e => {
        if (e.composedPath().some(node => node instanceof Element && node.matches('.rf-select-menu'))) {
          window.documentPickerClicks++;
        }
      });
    });
    const singleId = prefix + single;
    const singleMenu = await pickerClick(page, singleId, /caf[eé]/i);
    check(!await singleMenu.isVisible(), "Bootstrap cierra el desplegable simple normalmente");
    check(await page.evaluate(() => document.body.classList.contains('control-sidebar-slide-open')), "Selección simple conserva controlbar");
    check(await page.evaluate(() => window.documentPickerClicks) === 1, "El evento sigue llegando a document/Bootstrap");
    const multiId = prefix + multiple;
    const menu = await pickerClick(page, multiId, /caf[eé]/i);
    check(await menu.isVisible(), "El desplegable múltiple permanece abierto");
    await menu.locator(".dropdown-item .text").filter({ hasText: /(?:cacao|premium)/i }).first().click();
    check(await menu.isVisible(), "Otra selección múltiple no cierra el desplegable");
    check(await page.evaluate(() => window.barCollapsed) === 0, "Seleccionar no dispara cierre del controlbar");
    await page.locator('.rf-select').filter({ has: page.locator(`[id='${multiId}']`) })
      .locator('button.dropdown-toggle').click();
    if (dynamic) {
      await page.locator(`[id='${prefix}portal']`).check();
      await page.waitForFunction(id => {
        const el = document.getElementById(id);
        return el && el.dataset.container === 'body' && el.classList.contains('shiny-bound-input');
      }, prefix + 'dynamic');
      for (let i = 0; i < 3; i++) {
        const portal = await pickerClick(page, prefix + 'dynamic', /^A$/);
        check(await portal.isVisible(), "El portal múltiple permanece abierto al seleccionar");
        check(await portal.evaluate(el => !el.closest('.control-sidebar')), "Menú portal realmente fuera del panel");
        check(await portal.locator('.text').first().evaluate(el => getComputedStyle(el).textTransform) === "uppercase",
          "Portal conserva mayúsculas corporativas");
        check(await page.evaluate(() => document.body.classList.contains('control-sidebar-slide-open')), "Portal no cierra controlbar");
        await page.locator('.rf-select').filter({ has: page.locator(`[id='${prefix}dynamic']`) })
          .locator('button.dropdown-toggle').click();
        await page.locator(`[id='${prefix}renew']`).click();
        await page.locator(`label[for='${prefix}dynamic']`).filter({ hasText: `Dinámico ${i + 1}` }).waitFor();
        await page.waitForFunction(id => document.getElementById(id).classList.contains('shiny-bound-input'), prefix + 'dynamic');
      }
    }
    if (dynamic) {
      await page.locator(`[id='${prefix}dynamic_multiple']`).uncheck();
      await page.waitForFunction(id => !document.getElementById(id).multiple, prefix + 'dynamic');
      const singlePortal = await pickerClick(page, prefix + 'dynamic', /^A$/);
      check(!await singlePortal.isVisible(), 'El portal simple conserva el cierre nativo del desplegable');
      check(await page.evaluate(() => document.body.classList.contains('control-sidebar-slide-open')),
        "Selección simple en portal conserva el controlbar y cierra solo el desplegable");
    }
    check(await page.evaluate(() => window.barCollapsed) === 0, "Reconstrucciones no acumulan cierres ni handlers");
    await page.locator("#controlbar-toggle").click();
    await page.waitForFunction(() => !document.body.classList.contains('control-sidebar-slide-open'));
    check(true, "Botón normal cierra el panel");
    await page.locator("#controlbar-toggle").click();
    await page.waitForFunction(() => document.body.classList.contains('control-sidebar-slide-open'));
    await page.mouse.click(350, 100);
    await page.waitForFunction(() => !document.body.classList.contains('control-sidebar-slide-open'));
    check(true, "Clic externo conserva el cierre normal");
  }
  try {
    const cold = await open((process.env.RACAFE_MODAL_URL || "http://127.0.0.1:4022") + "?preload=0", true);
    check(await cold.evaluate(() => getComputedStyle(document.documentElement).getPropertyValue('--rf-forms-ready')) === "",
      "Caso frío sin CSS RF en la UI inicial");
    await cold.locator('#case-open').click();
    await cold.waitForTimeout(400);
    check(await cold.locator('.rf-modal:visible').count() === 0, "No se visualiza antes de cargar CSS");
    await cold.locator('.rf-modal:visible').waitFor();
    await cold.waitForTimeout(100);
    const frames = await cold.evaluate(() => window.modalFrames);
    check(frames.length > 0 && frames.every(frame => frame.ready === "1" && frame.width <= 381),
      "Desde el primer frame mantiene el ancho aviso definitivo");
    await footer(cold);
    await cold.locator('#case-confirm').click();
    await cold.locator('#case-result').filter({ hasText: 'Confirmaciones: 1' }).waitFor();
    check(true, "Binding con namespace de botón normalizado");
    const preloaded = await open(process.env.RACAFE_MODAL_URL || "http://127.0.0.1:4022");
    await preloaded.waitForFunction(() => getComputedStyle(document.documentElement).getPropertyValue('--rf-modal-sizes-ready').trim() === '1');
    check(true, "Tamaños disponibles antes de abrir el primer modal");
    await preloaded.locator('#case-open').click(); await preloaded.locator('.rf-modal:visible').waitFor();
    await preloaded.setViewportSize({ width: 390, height: 844 });
    await footer(preloaded);
    await preloaded.locator('.rf-modal [data-dismiss=modal]').click();
    await preloaded.locator('.rf-modal:visible').waitFor({ state: 'hidden' });
    const gallery = await open(process.env.RACAFE_GALLERY_URL || "http://127.0.0.1:4020");
    await gallery.locator('.nav-link[data-value=category7]').click();
    const size = gallery.locator('#preview select[id$="-modal_size"]'); await size.waitFor({ state: 'attached' });
    const sizes = { ventana: 1200, subventana1: 1050, subventana2: 900, subventana3: 760,
      subventana4: 620, subventana5: 500, aviso: 380 };
    for (const [value, width] of Object.entries(sizes)) {
      await size.evaluate((el, value) => window.jQuery(el).selectpicker('val', value), value);
      await gallery.locator('#preview button[id$="-open"]').click();
      await gallery.locator('.rf-modal:visible').waitFor();
      check(Math.abs(await gallery.locator('.rf-modal .modal-dialog').evaluate(el => el.getBoundingClientRect().width) - width) < 1,
        `Tamaño ${value} aplicado antes de mostrarse`);
      await gallery.locator('.rf-modal [data-dismiss=modal]').click();
      await gallery.locator('.rf-modal:visible').waitFor({ state: 'hidden' });
    }
    const script = await gallery.locator('script[src*="racafe-forms.js"]').first().getAttribute('src');
    await gallery.evaluate(() => {
      window.rfAddedClicks = 0;
      window.rfAddOriginal = document.addEventListener;
      document.addEventListener = function (type, ...args) {
        if (type === "click") window.rfAddedClicks++;
        return window.rfAddOriginal.call(this, type, ...args);
      };
    });
    await gallery.addScriptTag({ url: new URL(script, gallery.url()).href });
    check(await gallery.evaluate(() => {
      document.addEventListener = window.rfAddOriginal;
      return window.rfAddedClicks === 0;
    }), "Recargar la dependencia no duplica listeners de clic");
    await controlbar(gallery, 'controlbar_demo-', 'single', 'multiple', true);
    if (process.env.RACAFE_MODULE_URL) {
      const modules = await open(process.env.RACAFE_MODULE_URL);
      await controlbar(modules, 'bar_', 'single-sel', 'multiple-sel', false);
      check(await modules.locator('#bar_single-sel option').first().getAttribute('value') === 'AGREGAR...', "Agregable primero en controlbar");
      check(await modules.locator('#bar_multiple-sel option').first().getAttribute('value') === 'AGREGAR...', "Múltiple primero en controlbar");
    }
    check(errors.length === 0, `Sin errores JavaScript: ${errors.join('; ')}`);
    console.log(`${checks} comprobaciones de modal/controlbar superadas.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
