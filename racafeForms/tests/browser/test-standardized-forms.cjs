const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  function check(value, message) { assert.ok(value, message); checks++; }
  try {
    const context = await browser.newContext({ viewport: { width: 1440, height: 1100 }, timezoneId: "UTC" });
    const page = await context.newPage(), errors = [], external = [], downloads = [];
    const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3956";
    page.on("pageerror", error => errors.push(error.message));
    page.on("download", download => downloads.push(download));
    await page.route("**/*", route => {
      if (new URL(route.request().url()).origin !== new URL(url).origin) { external.push(route.request().url()); route.abort(); }
      else route.continue();
    });
    await page.goto(url);
    await page.locator("#uno-tipos").filter({ hasText: "POSIXct" }).waitFor();
    check(await page.locator(".rf-validation-error").count() === 0, "No hay errores antes del primer intento");
    check((await page.locator("#uno-estado").innerText()).includes("Intentado: FALSE"), "Validación diferida");
    check(await page.locator("#uno-cita").inputValue() === "03/11/2026 10:30", "Fecha y hora integrada española");
    check((await page.locator("#uno-tipos").innerText()).includes("2026-11-03 15:30:00"), "Conversión UTC correcta");
    const labels = await page.locator("#uno-form .control-label").evaluateAll(nodes => nodes.map(node => {
      const style = getComputedStyle(node); return [style.fontSize, style.fontWeight, style.color, style.fontFamily].join("|");
    }));
    check(new Set(labels).size === 1, "Etiquetas uniformes entre tipos de control");
    check(await page.locator("#uno-form .rf-group-title").first().evaluate(el => getComputedStyle(el).fontSize) === "12px",
      "Encabezado HTML conserva contenido y densidad");
    check(await page.locator("#uno-form h1,#uno-form h2,#uno-form h3").count() === 0, "No se duplica título de navegación");
    check(await page.locator("#uno-form > .rf-form-group").nth(1).evaluate(el => getComputedStyle(el).borderTopWidth) === "1px",
      "Separador automático entre grupos");
    check(await page.locator("#uno-form > .rf-form-group").last().evaluate(el => getComputedStyle(el).borderBottomWidth) === "0px",
      "Sin doble borde junto al footer");
    await page.locator("#uno-archivo").click();
    await page.locator("#uno-estado").filter({ hasText: "Intentado: TRUE" }).waitFor();
    await page.waitForFunction(() => document.querySelector('#uno-archivo').getAttribute('aria-busy') === 'false');
    check(downloads.length === 0, "Descarga inválida no empieza");
    check(await page.locator("#uno-form .rf-validation-error").count() === 4, "Obligatorios y regla de casilla");
    check(await page.locator("#dos-form .rf-validation-error").count() === 0, "Formulario hermano sin errores cruzados");
    check(await page.evaluate(() => document.activeElement.id) === "uno-nombre", "Foco al primer pendiente");
    check(await page.locator("#uno-nombre").getAttribute("aria-invalid") === "true", "Error accesible");
    check((await page.locator("#uno-form .rf-validation-summary").innerText()).includes("Categorías"), "Resumen con pendientes");
    await page.locator("#uno-nombre").fill("Ana"); await page.locator("#uno-nombre").press("Tab");
    await page.locator("#uno-nombre_validation").waitFor({ state: "detached" }); check(true, "Texto corregido limpia error");
    const amount = page.locator("#uno-importe"); await amount.click(); await amount.press("ControlOrMeta+a");
    await amount.pressSequentially("1234.56"); await amount.press("Tab");
    await page.locator("#uno-importe_validation").waitFor({ state: "detached" });
    check(await amount.inputValue() === "1,234.56", "Numérico conserva formato corporativo");
    await page.locator("#uno-categorias").selectOption(["A"]);
    await page.locator("#uno-categorias_validation").waitFor({ state: "detached" });
    await page.locator("#uno-aceptar").check();
    await page.locator("#uno-aceptar_validation").waitFor({ state: "detached" });
    check(await page.locator("#uno-form .rf-validation-error").count() === 0, "Formulario corregido sin mensajes viejos");
    check(!(await page.locator("#uno-correo").isVisible()), "Correo oculto con switch FALSE");
    await page.locator("#uno-contactar").focus(); await page.locator("#uno-contactar").press("Space");
    await page.locator("#uno-correo").waitFor({ state: "visible" });
    await page.locator("#uno-correo_validation").waitFor();
    check((await page.locator("#uno-tipos").innerText()).includes("switch: logical"), "Switch booleano y conditionalPanel");
    await page.locator("#uno-correo").fill("sin-arroba"); await page.locator("#uno-correo").press("Tab");
    await page.locator("#uno-correo_validation").filter({ hasText: "correo válido" }).waitFor(); check(true, "Regla condicional de correo");
    await page.locator('.rf-field:has(#uno-contactar) label[for="uno-contactar"]').last().click();
    await page.locator("#uno-correo_validation").waitFor({ state: "detached" });
    check(!(await page.locator("#uno-correo").isVisible()), "Regla desactivada limpia el error del campo oculto");
    await page.locator('.rf-field:has(#uno-contactar) label[for="uno-contactar"]').last().click();
    await page.locator("#uno-correo_validation").waitFor(); check(true, "La regla se reactiva al habilitar el campo");
    await page.locator("#uno-correo").fill("ana@example.test"); await page.locator("#uno-correo").press("Tab");
    await page.locator("#uno-correo_validation").waitFor({ state: "detached" });
    await page.locator("#uno-agregar").check(); await page.locator("#uno-extra").waitFor();
    await page.locator("#uno-extra_validation").waitFor(); check(true, "Campo dinámico requerido se incorpora");
    await page.locator("#uno-agregar").uncheck(); await page.locator("#uno-extra").waitFor({ state: "detached" });
    await page.waitForFunction(() => !document.querySelector('#uno-form .rf-validation-error'));
    check(true, "Campo retirado limpia sus pendientes");
    await page.locator("#uno-agregar").check(); await page.locator("#uno-extra_validation").waitFor();
    await page.locator("#uno-extra").fill("Referencia"); await page.locator("#uno-extra").press("Tab");
    await page.locator("#uno-extra_validation").waitFor({ state: "detached" });
    check(true, "Campo recreado mantiene validación y limpieza");
    await page.locator("#uno-ocupado").check();
    await page.waitForFunction(() => document.querySelector('#uno-enviar').disabled);
    check(await page.locator("#uno-enviar").getAttribute("aria-busy") === "true", "Loading deshabilita y anuncia estado");
    await page.locator("#uno-ocupado").uncheck();
    await page.waitForFunction(() => !document.querySelector('#uno-enviar').disabled);
    check(true, "Salir de loading recupera el botón");
    await page.locator("#uno-enviar").click(); await page.locator("#uno-estado").filter({ hasText: "Envíos: 1" }).waitFor();
    check(await page.locator("#uno-enviar").evaluate(el => el.classList.contains("rf-btn--success-feedback")),
      "Acción ejecutada solo con datos válidos y confirmación visual");
    const event = page.waitForEvent("download"); await page.locator("#uno-archivo").click(); const download = await event;
    const csv = await fs.readFile(await download.path(), "utf8");
    check(csv.includes("Ana") && csv.includes("1234.56"), "CSV real con valores validados, sin miles en almacenamiento");
    check(await page.locator('a.shiny-download-link').count() === 2, "No hay anchors ocultos por aplicación");
    await page.locator("#uno-cita").click();
    const calendar = page.locator(".air-datepicker:visible"); await calendar.waitFor();
    await calendar.locator('[data-date="4"]:not(.-other-month-)').click();
    await page.locator("#uno-tipos").filter({ hasText: "2026-11-04 15:30:00" }).waitFor();
    check((await page.locator("#uno-tipos").innerText()).includes("POSIXct"), "Selección actualiza fecha-hora POSIXct");
    await page.locator("#uno-nombre").click();
    await page.locator("#uno-modal").click(); await page.locator("#shiny-modal").waitFor({ state: "visible" });
    check(await page.locator("#shiny-modal .rf-modal-footer .rf-btn--secondary").count() === 1, "Footer corporativo existente reutilizado");
    await page.locator("#shiny-modal button[data-dismiss=modal]").click();
    await page.locator("#shiny-modal").waitFor({ state: "detached" });
    const isBs4 = await page.locator(".control-sidebar").count() > 0;
    if (isBs4) {
      await page.locator('[data-widget="control-sidebar"]').click();
      await page.locator("#dos-enviar").waitFor({ state: "visible" });
      check(await page.locator("#dos-form").evaluate(el => getComputedStyle(el).getPropertyValue("--rf-font").trim()) === "11px",
        "Controlbar compacto por defecto");
    }
    await page.locator("#dos-enviar").click();
    await page.locator("#dos-estado").filter({ hasText: "Intentado: TRUE" }).waitFor();
    check(await page.locator("#dos-form .rf-validation-error").count() === 4, "Segunda instancia valida independientemente");
    check((await page.locator("#uno-estado").innerText()).includes("Envíos: 1"), "La instancia hermana conserva estado");
    if (isBs4) await page.locator('[data-widget="control-sidebar"]').click();
    await page.evaluate(() => document.getElementById("uno-form").removeAttribute("id"));
    await page.locator("#uno-agregar").uncheck();
    await page.locator("#uno-extra").waitFor({ state: "detached" });
    await page.locator("#uno-agregar").check();
    await page.locator("#uno-extra_validation").waitFor();
    check(true, "Formulario sin ID explícito conserva validación dinámica");
    await page.locator("#uno-extra").fill("Sin ID"); await page.locator("#uno-extra").press("Tab");
    await page.locator("#uno-extra_validation").waitFor({ state: "detached" });
    check(true, "No reaparecen mensajes antiguos al corregir un formulario sin ID");
    await page.setViewportSize({ width: 390, height: 844 });
    await page.waitForFunction(() => document.documentElement.scrollWidth <= innerWidth + 1);
    check(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), "Formulario móvil sin overflow");
    if (isBs4) {
      await page.locator('[data-widget="control-sidebar"]').click();
      await page.locator("#dos-enviar").waitFor({ state: "visible" });
      check((await page.locator(".control-sidebar").boundingBox()).width <= 390, "Controlbar cabe en móvil");
      await page.locator('[data-widget="control-sidebar"]').click();
    }
    check(await page.locator('script[src*="racafe-forms.js"]').count() === 1, "Recursos base deduplicados");
    check(errors.length === 0, errors.join("\n"));
    check(external.length === 0, "Sin peticiones externas");
    console.log(`${checks} comprobaciones de formularios superadas en ${url}`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
