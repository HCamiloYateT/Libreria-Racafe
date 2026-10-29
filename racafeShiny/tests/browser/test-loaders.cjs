// Iniciar inst/examples/loaders.R; repetir con RACAFE_LOADER_BS4DASH=1.
const assert = require("node:assert/strict");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  try {
    const page = await browser.newPage({ viewport: { width: 1366, height: 1000 } });
    const errors = [], external = [];
    const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3910/";
    const origin = new URL(url).origin;
    page.on("pageerror", error => errors.push(error.message));
    await page.route("**/*", route => {
      if (new URL(route.request().url()).origin !== origin) {
        external.push(route.request().url()); route.abort();
      } else route.continue();
    });
    await page.goto(url);
    const host = page.locator("#rsh-loader-host");
    await host.waitFor({ state: "visible" });
    check((await host.innerText()).includes("Cargando"), "Inicio visible mientras se prepara la sesión");
    await page.locator("#uno-resultado").filter({ hasText: "Pendiente" }).waitFor();
    await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
    check(true, "Inicio cierra al terminar la carga inicial");
    await page.locator("#uno-recargar").click();
    await host.waitFor({ state: "visible", timeout: 1000 });
    check((await host.innerText()).includes("Actualizando"), "Clic muestra la variante sin esperar respuesta");
    check((await page.locator("#uno-resultado").innerText()) === "Pendiente", "Servidor aún procesando");
    const spinner = host.locator(".rsh-loader-spinner");
    const first = await spinner.evaluate(el => getComputedStyle(el).transform);
    await page.waitForTimeout(150);
    const second = await spinner.evaluate(el => getComputedStyle(el).transform);
    check(first !== second, "Animación activa mientras R duerme");
    await page.locator("#uno-resultado").filter({ hasText: "Actualizado" }).waitFor();
    await host.waitFor({ state: "hidden" }); check(true, "Cierre automático al finalizar");
    await page.locator("#dos-calcular").click();
    await host.waitFor({ state: "visible", timeout: 1000 });
    check((await host.innerText()).includes("Calculando"), "Segundo namespace usa su variante");
    await page.locator("#dos-resultado").filter({ hasText: "Calculado" }).waitFor();
    await host.waitFor({ state: "hidden" });
    check((await page.locator("#uno-resultado").innerText()) === "Actualizado", "Módulos independientes");
    await page.locator("#uno-fallar").click(); await host.waitFor({ state: "visible" });
    await page.locator("#uno-resultado").filter({ hasText: "Error controlado" }).waitFor();
    await host.waitFor({ state: "hidden" }); check(true, "Error no deja overlay colgado");
    await page.locator("#uno-mostrar").click();
    await page.locator("#uno-panel .waiter-overlay").waitFor();
    check(await page.locator("#dos-panel .waiter-overlay").count() === 0, "Objeto local no cubre otro módulo");
    await page.locator("#uno-actualizar").click();
    await page.locator("#uno-panel .rsh-loader-text").filter({ hasText: "Procesando datos" }).waitFor();
    check(true, "Objeto permite actualizar el mensaje");
    // Un ciclo automático no debe retirar un waiter explícito del panel.
    check(await page.locator("#uno-panel .waiter-overlay").count() === 1, "Busy/idle respeta waiters explícitos");
    await page.locator("#uno-ocultar").click();
    await page.locator("#uno-panel .waiter-overlay").waitFor({ state: "detached" });
    check(true, "Objeto permite cierre explícito");
    // Busy sin trigger: omitir solo la captura local para comprobar el evento de Shiny.
    await page.evaluate(() => document.querySelectorAll('[data-rsh-trigger="uno-calcular"]').forEach(el => el.remove()));
    await page.locator("#uno-calcular").click(); await host.waitFor({ state: "visible" });
    check((await host.innerText()).includes("Calculando"), "Detección busy sin trigger");
    await page.locator("#uno-resultado").filter({ hasText: "Calculado" }).waitFor();
    await host.waitFor({ state: "hidden" });
    await page.setViewportSize({ width: 375, height: 812 });
    await page.locator("#dos-recargar").click(); await host.waitFor({ state: "visible" });
    const box = await host.locator(".rsh-loader-card").boundingBox();
    check(box.x >= 0 && box.x + box.width <= 375, "Tarjeta cabe en móvil");
    await page.emulateMedia({ reducedMotion: "reduce" });
    check(await spinner.evaluate(el => getComputedStyle(el).animationName) === "none", "Respeta movimiento reducido");
    await page.locator("#dos-resultado").filter({ hasText: "Actualizado" }).waitFor();
    await host.waitFor({ state: "hidden" });
    await page.locator("#editable").fill("Disponible"); check(true, "UI disponible al terminar");
    // Repetición rápida no acumula overlays.
    await page.evaluate(() => {
      for (let i = 0; i < 3; i++) {
        document.dispatchEvent(new CustomEvent("shiny:busy"));
        document.dispatchEvent(new CustomEvent("shiny:idle"));
      }
    });
    await page.waitForTimeout(350);
    check(await host.locator(".waiter-overlay").count() === 0, "No acumula overlays al repetir ciclos");
    await page.evaluate(() => window.jQuery(document).trigger("shiny:busy"));
    await host.waitFor({ state: "visible" });
    await page.evaluate(() => window.jQuery(document).trigger("shiny:disconnected"));
    await host.waitFor({ state: "hidden" }); check(true, "Desconexión cierra el loader automático");
    check(await page.locator('script[src*="loaders.js"]').count() === 1, "Un solo recurso JS");
    check(external.length === 0, "Sin solicitudes externas");
    check(errors.length === 0, errors.join("\n"));
    console.log(`${checks} comprobaciones de loaders superadas en ${url}`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
