const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  try {
    const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:4001";
    const context = await browser.newContext({ viewport: { width: 1500, height: 1000 },
      permissions: ["clipboard-read", "clipboard-write"] });
    const page = await context.newPage(), errors = [], external = [];
    page.on("pageerror", error => errors.push(error.message));
    await page.route("**/*", route => {
      if (new URL(route.request().url()).origin !== new URL(url).origin) {
        external.push(route.request().url()); return route.abort();
      }
      return route.continue();
    });
    await page.goto(url);
    await page.locator("#example1-value").waitFor();
    const categories = await page.locator(".nav-sidebar .nav-link[data-value]").evaluateAll(nodes =>
      nodes.map(node => ({ id: node.dataset.value, text: node.textContent.trim() })));
    const components = [];
    async function select(id, value) {
      await page.evaluate(({ id, value }) => {
        const el = document.getElementById(id);
        if (el.selectize) el.selectize.setValue(value); else window.jQuery(el).val(value).trigger("change");
      }, { id, value });
    }
    for (const category of categories) {
      await page.locator(`.nav-link[data-value="${category.id}"]`).click();
      await page.waitForFunction(text => document.querySelector('#description').textContent.includes(text), category.text);
      const names = await page.locator("#component").evaluate(el => Object.keys(el.selectize.options));
      for (const name of names) {
        await select("component", name);
        await page.waitForFunction(name => document.querySelector("#code").textContent.startsWith("# Ejemplo autocontenido: " + name + "\n"), name);
        await page.waitForFunction(() => !document.querySelector("#preview.recalculating"));
        check(name === "ListaDesplegableAgregable" ? await page.locator("#preview .rf-form").count() >= 1 :
          await page.locator("#preview .rf-form").count() === 1, `Demo montada: ${name}`);
        check(await page.locator(".shiny-output-error").count() === 0, `Sin error R: ${name}`);
        const href = await page.locator("#help a").getAttribute("href");
        check((await page.request.get(new URL(href, url).href)).ok(), `Ayuda local: ${name}`);
        components.push(name);
      }
    }
    check(components.length === 60, "58 funciones públicas, formulario completo y módulo agregable inventariados");
    async function navigate(categoryId, name) {
      await page.locator(`.nav-link[data-value="category${categoryId}"]`).click();
      await page.waitForFunction(name => !!document.querySelector('#component').selectize.options[name], name);
      await select("component", name);
      await page.waitForFunction(name => document.querySelector("#code").textContent.startsWith("# Ejemplo autocontenido: " + name + "\n"), name);
    }
    await navigate(2, "racafeInputNumeric");
    const numeric = page.locator("#preview input[id$='-value']");
    await select("format", "currency"); await page.locator("#decimals").fill("3");
    await select("density", "comfortable"); await page.locator("#readonly").check();
    await page.locator("#apply").click();
    await page.waitForFunction(() => document.querySelector('#code').textContent.includes('decimals = 3'));
    check(await numeric.getAttribute("readonly") !== null, "Solo lectura real");
    check(await numeric.evaluate(el => getComputedStyle(el).height) === "40px", "Densidad comfortable");
    check((await numeric.inputValue()).includes("1,234.560"), "Formato numérico configurable");
    await page.locator("#copy").click();
    await page.locator("#copy_status").filter({ hasText: "Código copiado" }).waitFor();
    check((await page.evaluate(() => navigator.clipboard.readText())) === await page.locator("#code").innerText(),
      "Portapapeles contiene el código mostrado");
    await navigate(2, "racafeInputSelect");
    await page.waitForFunction(() => !document.querySelector("#readonly").checked &&
      document.querySelector("#density").value === "compact");
    await page.locator("#multiple").check(); await page.locator("#apply").click();
    await page.waitForFunction(() => document.querySelector('#preview select')?.multiple);
    check(await page.locator("#preview select option[value='cafe']").count() === 1, "Valores duplicados opcionalmente eliminados");
    const toggle = page.locator("#preview button[id$='-toggle']"); await toggle.click();
    await page.waitForFunction(() => document.querySelector('#preview select')?.disabled);
    check(await page.locator("#preview .dropdown-toggle").isDisabled(), "Picker real y visual deshabilitado");
    await toggle.click();
    await page.waitForFunction(() => document.querySelector('#preview select') && !document.querySelector('#preview select').disabled);
    await page.locator("#preview button[id$='-update']").click();
    await page.locator("#preview option[value='te']").waitFor({ state: "attached" });
    check(true, "Actualización dinámica del catálogo");
    await navigate(9, "racafeUpdateEnabled");
    await page.locator("#preview button[id$='-run']").click();
    const dynamicFlag = page.locator("#preview input[id$='-flag']");
    await dynamicFlag.focus(); await dynamicFlag.press("Space");
    const extra = page.locator("#preview input[id$='-extra']");
    await extra.waitFor();
    await page.waitForFunction(() => document.querySelector('#preview input[id$="-extra"]').disabled);
    check(await extra.isDisabled(), "Estado enviado antes de renderUI se aplica al control nuevo");
    await page.locator("#preview button[id$='-run']").click();
    await page.waitForFunction(() => !document.querySelector('#preview input[id$="-extra"]').disabled);
    check(true, "Control dinámico vuelve a habilitarse sin eventos adicionales");
    await navigate(7, "racafeModal");
    for (let i = 0; i < 3; i++) {
      await page.locator("#preview button[id$='-open']").click();
      await page.locator(".modal button[id$='-ask']").click();
      await page.locator(".swal2-popup:visible").waitFor();
      await page.keyboard.press("Escape");
      await page.locator(".swal2-popup:visible").waitFor({ state: "hidden" });
      check(await page.locator(".modal:visible").count() === 1, "Escape conserva modal subyacente");
      await page.locator(".modal [data-dismiss='modal']").click();
      await page.locator(".modal:visible").waitFor({ state: "hidden" });
    }
    await navigate(11, "Formulario completo");
    await page.locator("#preview button[id$='-save']").click();
    await page.locator("#preview .rf-validation-error").waitFor();
    check(await page.locator("#preview .rf-validation-error").count() === 1, "Obligatorio en servidor");
    await page.locator("#preview input[id$='-value']").fill("Cliente demo");
    const flag = page.locator("#preview input[id$='-flag']");
    await flag.focus(); await flag.press("Space");
    await page.waitForFunction(() => !document.querySelector('#preview input[type=email]').disabled);
    check(true, "Switch habilita correo sin JavaScript de aplicación");
    await page.locator("#preview input[type=email]").fill("cliente@example.test");
    await page.locator("#preview button[id$='-save']").click();
    await page.locator(".swal2-confirm").click();
    await page.locator("#preview .rf-message--success").waitFor();
    const downloadPromise = page.waitForEvent("download");
    await page.locator("#preview a[id$='-file']").click();
    const download = await downloadPromise;
    const csv = await fs.readFile(await download.path(), "utf8");
    check(csv.includes("Cliente demo"), "Descarga real tras validación");
    await page.locator("#preview button[id$='-reset']").click();
    await page.waitForFunction(() => document.querySelector('#preview input[id$="-value"]').value === "");
    check(true, "Restablecimiento nativo de formulario");
    await navigate(1, "racafeFormRow");
    await page.locator('[data-widget="control-sidebar"]').first().click();
    for (const columns of ['1', '2', '3', '4']) for (const mode of ['auto', 'stack', 'fixed', 'legacy']) {
      await select('row_columns', columns);
      await select('row_responsive', mode);
      await page.locator('#apply').click();
      await page.waitForFunction(({columns, mode}) => {
        const rows = [...document.querySelectorAll('#preview .rf-row')];
        return rows.length === 2 && rows.every(row => row.classList.contains('rf-row--' + columns) &&
          (mode === 'legacy' ? !row.closest('.rf-row-container') : row.classList.contains('rf-row--' + mode)));
      }, {columns, mode});
      check(true, `Galería normal: ${columns} columnas, ${mode}`);
      await select('controlbar_demo-row_columns', columns);
      await select('controlbar_demo-row_responsive', mode);
      await page.waitForFunction(({columns, mode}) => {
        const rows = [...document.querySelectorAll('#controlbar_demo-row_ui .rf-row')];
        return rows.length === 2 && rows.every(row => row.classList.contains('rf-row--' + columns) &&
          (mode === 'legacy' ? !row.closest('.rf-row-container') : row.classList.contains('rf-row--' + mode)));
      }, {columns, mode});
      check(true, `Galería controlbar: ${columns} columnas, ${mode}`);
    }
    await page.setViewportSize({ width: 390, height: 844 });
    await page.waitForTimeout(400);
    check(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), "Sin desbordamiento móvil");
    check(errors.length === 0, `Sin errores JavaScript: ${errors.join("; ")}`);
    check(external.length === 0, `Sin recursos externos: ${external.join("; ")}`);
    console.log(`${checks} comprobaciones de galería superadas (${components.length} ejemplos).`);
    await context.close();
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
