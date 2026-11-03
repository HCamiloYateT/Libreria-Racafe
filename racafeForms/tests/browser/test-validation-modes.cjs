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
        await page.locator("#uno-nombre_validation").waitFor();
        check(await page.locator(".swal2-popup").count() === 0, "Modo heredado both no abre SweetAlert");
        check(await page.locator("#uno-form .rf-validation-error").count() === 4, "Solo errores inline");
        await page.waitForFunction(() => document.activeElement.id === "uno-nombre");
        check(true, "Foco al primer campo sin modal");
        await page.locator("#uno-nombre").fill("Ana"); await page.locator("#uno-nombre").press("Tab");
        await page.waitForFunction(() => !document.querySelector("#uno-nombre").hasAttribute("aria-invalid") ||
          document.querySelector("#uno-nombre").getAttribute("aria-invalid") === "false");
        check(await page.locator("#uno-form .rf-validation-error").count() === 3, "Limpieza automática en both");
        check(await page.locator(".swal2-popup").count() === 0, "Edición no abre alertas");
      }
      check(errors.length === 0, `Sin errores JavaScript en ${timezoneId}: ${errors.join("; ")}`);
      await context.close();
    }
    console.log(`${checks} comprobaciones de alertas y zonas horarias superadas.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
