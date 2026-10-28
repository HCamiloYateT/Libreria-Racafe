// Iniciar inst/examples/text-input-bindings.R; repetir con TEXT_BINDINGS_BS4DASH=1.
const assert = require("node:assert/strict");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (condition, message) => { assert.ok(condition, message); checks++; };
  try {
    const page = await browser.newPage();
    const errors = [], external = [];
    const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3890/";
    const origin = new URL(url).origin;
    page.on("pageerror", error => errors.push(error.message));
    await page.route("**/*", route => {
      if (new URL(route.request().url()).origin !== origin) {
        external.push(route.request().url()); route.abort();
      } else route.continue();
    });
    await page.goto(url);
    await page.locator("#uno-resultado").filter({ hasText: "inicial-password" }).waitFor();
    const serverText = () => page.locator("#uno-resultado").innerText();
    check((await serverText()).includes("Fijo") && (await serverText()).includes("Bloqueado"),
      "Readonly y disabled llegan inicialmente al servidor");
    for (const type of ["text", "email", "tel", "password"]) {
      const input = page.locator("#uno-" + type);
      check(await input.getAttribute("type") === type, "Tipo HTML " + type);
      check(await input.getAttribute("required") !== null, "Required " + type);
      check(await input.getAttribute("aria-invalid") === "true", "Validación " + type);
      check((await input.getAttribute("aria-describedby")) === "uno-" + type + "_help uno-" + type + "_error",
        "Ayuda y error con namespace " + type);
      check(await page.locator("#uno-" + type + "_error").isVisible(), "Mensaje visible " + type);
      const value = { text: "Ángela texto", email: "prueba@example.org", tel: "+57 3001234567",
        password: "clave-ficticia" }[type];
      await input.fill(value);
      await page.waitForFunction(value => document.getElementById("uno-resultado").textContent.includes(value), value);
      check((await serverText()).includes(value), "Escribir actualiza input local del servidor: " + type);
      if (["email", "tel"].includes(type)) {
        check(await input.getAttribute("inputmode") === type, "Teclado " + type);
        check(await input.getAttribute("autocomplete") === type, "Autocompletado " + type);
        check(await input.evaluate(el => {
          const binding = window.jQuery(el).data("shiny-input-binding");
          const native = window.jQuery("#uno-text").data("shiny-input-binding");
          return binding && binding.getValue === native.getValue && binding.subscribe === native.subscribe &&
            binding.receiveMessage === native.receiveMessage;
        }), "Métodos nativos de texto reutilizados: " + type);
      }
    }
    const isolated = await page.evaluate(() => {
      const fallback = Shiny.inputBindings.getBindings().find(entry => entry.binding.name === "racafeForms.textInput");
      const container = document.createElement("div");
      container.innerHTML = '<input type="tel" class="form-control"><input type="email" class="form-control">';
      return fallback.binding.find(container).length === 0;
    });
    check(isolated, "El adaptador no captura controles ajenos a racafeForms");
    check(!(await page.locator("#dos-resultado").innerText()).includes("prueba@example.org"), "Módulos aislados");
    check(await page.locator("#uno-readonly").getAttribute("readonly") !== null &&
      !await page.locator("#uno-readonly").isEditable(), "Readonly conserva bloqueo de edición");
    check(await page.locator("#uno-disabled").isDisabled(), "Disabled bloquea interacción");
    await page.locator("#uno-email").fill("");
    await page.waitForFunction(() => /\$email\s+\[1\] ""/.test(document.getElementById("uno-resultado").textContent));
    check(await page.locator("#uno-email").evaluate(el => el.validity.valueMissing),
      "Required conserva validación nativa y el vacío llega al servidor");
    await page.locator("#uno-actualizar").click();
    for (const type of ["text", "email", "tel", "password"]) {
      await page.waitForFunction(({ type }) => document.getElementById("uno-" + type).value === "servidor-" + type,
        { type });
      await page.waitForFunction(type => document.getElementById("uno-resultado").textContent.includes("servidor-" + type),
        type);
      check(true, "Actualizador nativo de ida y vuelta " + type);
    }
    check(errors.length === 0 && await page.locator(".shiny-output-error").count() === 0, "Sin errores JS/servidor");
    check(external.length === 0, "Sin peticiones externas");
    console.log(checks + " comprobaciones de bindings superadas en " + origin);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
