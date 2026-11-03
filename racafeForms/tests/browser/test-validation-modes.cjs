const assert = require("node:assert/strict");
const { chromium } = require("playwright");
// Ejecutar la demo con RACAFE_STANDARD_MODE=both.
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  try {
    const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3993";
    for (const timezoneId of ["UTC", "America/Los_Angeles"]) {
      const context = await browser.newContext({ timezoneId });
      const page = await context.newPage(), errors = [];
      page.on("pageerror", error => errors.push(error.message));
      await page.goto(url);
      await page.locator("#uno-tipos").filter({ hasText: "POSIXct" }).waitFor();
      check(await page.locator("#uno-cita").inputValue() === "03/11/2026 10:30", `Hora local en ${timezoneId}`);
      check((await page.locator("#uno-tipos").innerText()).includes("2026-11-03 15:30:00"),
        `Instante UTC correcto en ${timezoneId}`);
      if (timezoneId === "UTC") {
        await page.locator("#uno-enviar").click();
        const popup = page.locator(".swal2-popup:visible");
        await popup.waitFor();
        check((await popup.innerText()).includes("Nombre"), "SweetAlert enumera campos pendientes");
        check(await page.locator("#uno-form .rf-validation-error").count() === 4, "Modo both muestra inline");
        await popup.locator(".swal2-confirm").click();
        await page.waitForFunction(() => document.activeElement.id === "uno-nombre");
        check(true, "Foco al primer campo tras cerrar alerta");
        await page.locator("#uno-nombre").fill("Ana"); await page.locator("#uno-nombre").press("Tab");
        await page.waitForFunction(() => !document.querySelector("#uno-nombre").hasAttribute("aria-invalid") ||
          document.querySelector("#uno-nombre").getAttribute("aria-invalid") === "false");
        check(await page.locator("#uno-form .rf-validation-error").count() === 3, "Limpieza automática en both");
        check(await popup.count() === 0, "Edición no repite alertas");
      }
      check(errors.length === 0, `Sin errores JavaScript en ${timezoneId}: ${errors.join("; ")}`);
      await context.close();
    }
    console.log(`${checks} comprobaciones de alertas y zonas horarias superadas.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
