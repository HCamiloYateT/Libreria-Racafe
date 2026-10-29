// Iniciar inst/examples/files-notifications.R; probar Shiny y bs4Dash con el mismo script.
const assert = require("node:assert/strict");
const { chromium } = require("playwright");
const xlsx = Buffer.from(
  "UEsDBBQAAAAIAJGyXF0cdPw42AAAAPYBAAATAAAAW0NvbnRlbnRfVHlwZXNdLnhtbK2RvU4DMQzHX+WUtWpcGDqgXhfKCgy8gJvz" +
  "9aLLl2K3XN++uVAYUIGFyUr+Hz9L3rydE3EzeRe4VYNIegBgM5BH1jFRKEofs0cpz3yAhGbEA8H9arUGE4NQkKXMHWq72VGPRyfN" +
  "01S+2cbQqkyOVfP4YZxZrcKUnDUoRYdT6L5RlleCLsnq4cEmXhSDgpuEWfkZcM29nChn21Hzilme0RcXTA7eYx73MY7695IbW8a+" +
  "t4a6aI6+RDSnTNjxQCTe6Tq1RxsWf/OrmaGOu39e5Kv/cw+o595eAFBLAwQUAAAACACRslxd/luGcooAAADwAAAACwAAAF9yZWxz" +
  "Ly5yZWxzjc8xDsIwDAXQq1Q+QF0YGFDaiaUr4gImddqqTRw5QZTbk7EgBkbrf70vmyuvlGcJaZpjqja/htTClHM8IyY7sadUS+RQ" +
  "EifqKZdTR4xkFxoZj01zQt0b0Jm9WfVDC9oPB6hur8j/2OLcbPki9uE55B8TX40ik46cW9hWfIoud5GlLihgZ/Djwe4NUEsDBBQA" +
  "AAAIAJGyXF21MdMTkgAAAOQAAAAPAAAAeGwvd29ya2Jvb2sueG1sjY8xDoMwDEWvEvkANXTogICpC1uvEMA0ESSO7CB6/CIoeyf7" +
  "/y+/L9cby9wzz+YTlqgNuJxThaiDo2D1xoninkwsweZdyhs1CdlRHVEOC96L4oHB+ggnoZJ/GDxNfqAnD2ugmE+I0GKz56jOJ4W2" +
  "Phr0N020gRp4yUq9BXN43dhACUYqvy/SjSVgW+N1htdn7RdQSwMEFAAAAAgAkbJcXbZUqs+OAAAA8QAAABoAAAB4bC9fcmVscy93" +
  "b3JrYm9vay54bWwucmVsc43PPQ7CMAwF4KtUOUDdMjCgJhNLV8QFotRtojY/so2A2xMxoCIxMFl+lr4nDxfcrISc2IfCzSNuibXy" +
  "IuUEwM5jtNzmgqle5kzRSl1pgWLdaheEQ9cdgfaGMsPebMZJKxqnXjXXZ8F/7DzPweE5u1vEJD8q4J5pZY8oFbW0oGj1iRjeo2+r" +
  "qsAM8PWheQFQSwMEFAAAAAgAkbJcXeacBpyIAAAAswAAABgAAAB4bC93b3Jrc2hlZXRzL3NoZWV0MS54bWxNjk0KAjEMha8y9ABm" +
  "xoULaQuCBxA8QazRFvszpJHx+GZmIS4S3vvCe8QujV89EsnwKbl2Z6LIfAToIVLBvmszVb08GhcUtfyEPjPhfQuVDPtxPEDBVI23" +
  "GzujoLfcloGdmZSGVZwmM4gzqeZU6SqsPHVvxV/4TTe0IN7CSiDoaFr3Xx38/vRfUEsBAhQDFAAAAAgAkbJcXRx0/DjYAAAA9gEA" +
  "ABMAAAAAAAAAAAAAAIABAAAAAFtDb250ZW50X1R5cGVzXS54bWxQSwECFAMUAAAACACRslxd/luGcooAAADwAAAACwAAAAAAAAAA" +
  "AAAAgAEJAQAAX3JlbHMvLnJlbHNQSwECFAMUAAAACACRslxdtTHTE5IAAADkAAAADwAAAAAAAAAAAAAAgAG8AQAAeGwvd29ya2Jv" +
  "b2sueG1sUEsBAhQDFAAAAAgAkbJcXbZUqs+OAAAA8QAAABoAAAAAAAAAAAAAAIABewIAAHhsL19yZWxzL3dvcmtib29rLnhtbC5y" +
  "ZWxzUEsBAhQDFAAAAAgAkbJcXeacBpyIAAAAswAAABgAAAAAAAAAAAAAAIABQQMAAHhsL3dvcmtzaGVldHMvc2hlZXQxLnhtbFBL" +
  "BQYAAAAABQAFAEUBAAD/AwAAAAA=", "base64");
const pdf = Buffer.from(
  "JVBERi0xLjQKMSAwIG9iago8PCAvVHlwZSAvQ2F0YWxvZyAvUGFnZXMgMiAwIFIgPj4KZW5kb2JqCjIgMCBvYmoKPDwgL1R5cGUg" +
  "L1BhZ2VzIC9LaWRzIFszIDAgUl0gL0NvdW50IDEgPj4KZW5kb2JqCjMgMCBvYmoKPDwgL1R5cGUgL1BhZ2UgL1BhcmVudCAyIDAg" +
  "UiAvTWVkaWFCb3ggWzAgMCAzMDAgMzAwXSA+PgplbmRvYmoKeHJlZgowIDQKMDAwMDAwMDAwMCA2NTUzNSBmIAowMDAwMDAwMDA5" +
  "IDAwMDAwIG4gCjAwMDAwMDAwNTggMDAwMDAgbiAKMDAwMDAwMDExNSAwMDAwMCBuIAp0cmFpbGVyCjw8IC9TaXplIDQgL1Jvb3Qg" +
  "MSAwIFIgPj4Kc3RhcnR4cmVmCjE4NgolJUVPRgo=", "base64");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  function check(value, message) { assert.ok(value, message); checks++; }
  const page = await browser.newPage({ viewport: { width: 1440, height: 1100 } });
  const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3900/", origin = new URL(url).origin;
  const errors = [], external = [];
  page.on("pageerror", error => errors.push(error.message));
  await page.route("**/*", route => {
    if (new URL(route.request().url()).origin !== origin) { external.push(route.request().url()); route.abort(); }
    else route.continue();
  });
  async function wait_text(id, text) {
    await page.waitForFunction(({ id, text }) => document.getElementById(id).textContent.includes(text), { id, text });
  }
  async function clear() {
    await page.locator("#uno-limpiar").click(); await page.locator("#dos-limpiar").click();
    await page.waitForFunction(() => document.querySelectorAll(".rf-notification").length === 0);
  }
  async function click_notify(button, title) {
    await page.locator("#uno-" + button).click();
    await page.locator(".rf-notification:visible").filter({ hasText: title }).first().waitFor();
    return page.locator(".rf-notification").filter({ hasText: title }).first();
  }
  try {
    await page.goto(url); await wait_text("uno-archivos", "xlsx pendiente");
    check(await page.locator('.rf-file input[type="file"]').count() === 10, "Controles nativos en dos módulos");
    check(await page.locator(".rf-file").first().innerText().then(text => !text.includes("Browse")), "Sin Browse nativo");
    check(await page.locator("#uno-deshabilitado").isDisabled(), "Archivo deshabilitado");
    const chooser_wait = page.waitForEvent("filechooser");
    await page.locator("#uno-xlsx").locator("xpath=ancestor::div[contains(@class,'rf-file')]").locator(".btn-file").click();
    const chooser = await chooser_wait;
    await chooser.setFiles({ name: "prueba.xlsx", buffer: xlsx,
      mimeType: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
    await wait_text("uno-archivos", "xlsx n: 1 valid: TRUE");
    check((await page.locator("#uno-archivos").innerText()).includes("columns: name,size,type,datapath readable: TRUE"),
      "XLSX subido: dataframe y archivo temporal reales en servidor");
    await page.locator("#uno-pdf").setInputFiles({ name: "prueba.pdf", buffer: pdf, mimeType: "application/pdf" });
    await wait_text("uno-archivos", "pdf n: 1 valid: TRUE"); check(true, "PDF nativo con MIME y tamaño");
    await page.locator("#uno-varios").setInputFiles([
      { name: "área.csv", buffer: Buffer.from("a,b\n1,2\n"), mimeType: "text/csv" },
      { name: "b.pdf", buffer: pdf, mimeType: "application/pdf" }]);
    await wait_text("uno-archivos", "varios n: 2 valid: TRUE");
    check(await page.locator("#uno-varios_files li").count() === 2, "Nombre y tamaño de cada archivo múltiple");
    check((await page.locator("#uno-archivos").innerText()).includes("área.csv"), "Nombre Unicode llega al servidor");
    const display = page.locator("#uno-varios").locator("xpath=ancestor::div[contains(@class,'rf-file')]")
      .locator('input[type="text"]');
    check((await display.inputValue()).includes("2 archivos"), "Resumen múltiple en español");

    check((await page.locator("#dos-archivos").innerText()).includes("varios pendiente"), "Upload aislado por namespace");
    await page.locator("#uno-limitado").setInputFiles({ name: "grande.csv", buffer: Buffer.alloc(64, 65), mimeType: "text/csv" });
    await wait_text("uno-limitado_error", "máximo"); await wait_text("uno-archivos", "limitado n: 1 valid: FALSE");
    check(true, "Límite visual y comprobación independiente del servidor");
    await page.locator("#uno-limitado").setInputFiles({ name: "mal.txt", buffer: Buffer.from("abc"), mimeType: "text/plain" });
    await wait_text("uno-limitado_error", "no permitido"); check(true, "Extensión no permitida visible");
    await page.locator("#uno-limitado").setInputFiles({ name: "vacio.csv", buffer: Buffer.alloc(0), mimeType: "text/csv" });
    await wait_text("uno-limitado_error", "vacío"); check(true, "Archivo vacío visible");
    await page.locator("#uno-limitado").setInputFiles({ name: "<img onerror=alert(1)>.csv", buffer: Buffer.from("a"), mimeType: "text/csv" });
    await wait_text("uno-limitado_files", "<img");
    check(await page.locator("#uno-limitado_files img").count() === 0, "Nombre de archivo escapado");
    await page.evaluate(() => {
      window.rf_reader = window.FileReader;
      window.FileReader = class { readAsArrayBuffer() { queueMicrotask(() => this.onerror()); } };
    });
    await page.locator("#uno-limitado").setInputFiles({ name: "lectura.csv", buffer: Buffer.from("x"), mimeType: "text/csv" });
    await wait_text("uno-limitado_error", "leer");
    check(await page.locator("#uno-limitado").getAttribute("aria-invalid") === "true", "Error de lectura y aria-invalid");
    await page.evaluate(() => { window.FileReader = window.rf_reader; });
    await page.locator("#nativa").click(); await page.locator("#shiny-notification-ajena").waitFor();
    const native_before = await page.locator("#shiny-notification-ajena").evaluate(el => {
      const s = getComputedStyle(el); return [s.backgroundColor, s.position, el.parentElement.id];
    });
    for (const type of ["info", "success", "warning", "error"]) {
      const note = await click_notify(type, "Notificación " + type);
      check(await note.evaluate(el => getComputedStyle(el).backgroundColor) === "rgb(255, 255, 255)",
        "Fondo blanco: " + type);
    }
    check(await page.locator(".rf-notification:visible").count() === 3, "Máximo tres propias visibles");
    check(await page.locator("#shiny-notification-ajena").evaluate(el => !el.classList.contains("rf-notification")),
      "Notificación ajena no reclasificada");
    check(JSON.stringify(native_before) === JSON.stringify(await page.locator("#shiny-notification-ajena").evaluate(el => {
      const s = getComputedStyle(el); return [s.backgroundColor, s.position, el.parentElement.id];
    })), "Notificaciones nativas sin estilos ni posición alterados");
    await page.locator("#uno-edicion").fill("La interfaz sigue activa");
    await wait_text("uno-texto", "La interfaz sigue activa"); check(true, "Notificaciones no bloqueantes");
    await clear(); check(await page.locator("#shiny-notification-ajena").isVisible(), "Retirar propias conserva ajenas");

    await click_notify("info", "Notificación info");
    await page.locator("#dos-info").click();
    await page.locator("#shiny-notification-dos-rf-notify-key-info").waitFor();
    check(await page.locator("#shiny-notification-uno-rf-notify-key-info").count() === 1,
      "Mismo ID local en dos módulos no duplica namespaces ni reemplaza otro módulo");
    await clear();
    const persistent = await click_notify("persistente", "Persistente");
    const id = await persistent.getAttribute("id");
    await page.locator("#uno-actualizar").click();
    await page.locator("#" + id).filter({ hasText: "Actualizada" }).waitFor();
    check(await page.locator("#" + id).count() === 1, "Actualización con ID devuelto sin duplicados");
    await page.waitForTimeout(1200); check(await page.locator("#" + id).isVisible(), "duration NULL persiste");
    await page.locator("#uno-retirar").click(); await page.locator("#" + id).waitFor({ state: "detached" });
    check(true, "racafeRemoveNotify nativo funciona en módulo");
    const temporary = await click_notify("temporal", "Temporal");
    const start = Date.now(); await temporary.waitFor({ state: "detached" });
    check(Date.now() - start < 3000, "duration=1 se expresa en segundos");
    const no_close = await click_notify("sin_cierre", "Sin cierre");
    check(await no_close.locator(".shiny-notification-close").count() === 0, "close_button FALSE");
    await clear();
    const manual = await click_notify("persistente", "Persistente");
    await manual.locator(".shiny-notification-close").press("Enter");
    await manual.waitFor({ state: "detached" }); check(true, "Cierre manual accesible por teclado");
    const escaped = await click_notify("inseguro", "<img");
    check(await escaped.locator("img,script").count() === 0, "Texto externo nunca HTML, incluso HTML()");
    await clear();
    await click_notify("persistente", "Persistente");
    await page.locator("#uno-modal").click(); await page.locator(".modal:visible").waitFor();
    await page.waitForFunction(() => document.querySelector(".rf-notifications").hidden);
    check(await page.locator(".rf-notification:visible").count() === 0, "No cubre modales");
    await page.locator('[data-dismiss="modal"]').first().click(); await page.locator(".modal:visible").waitFor({ state: "hidden" });
    await page.locator(".rf-notification:visible").waitFor();
    await page.locator("#uno-confirmar").click(); await page.locator(".swal2-popup:visible").waitFor();
    await page.locator(".swal2-cancel").click(); await page.locator(".swal2-popup:visible").waitFor({ state: "hidden" });
    check((await page.locator("#uno-decisiones").innerText()).includes("0"), "Cancelar no ejecuta la acción");
    await page.locator("#uno-confirmar").click(); await page.locator(".swal2-popup:visible").waitFor();
    await page.keyboard.press("Escape");
    await page.waitForTimeout(150);
    check((await page.locator("#uno-decisiones").innerText()).includes("0"), "Escape no ejecuta la acción");
    if (await page.locator(".swal2-popup:visible").count()) await page.locator(".swal2-cancel").click();
    await page.locator(".swal2-popup:visible").waitFor({ state: "hidden" });
    await page.locator("#uno-confirmar").click(); await page.locator(".swal2-popup:visible").waitFor();
    await page.evaluate(() => window.Swal.close());
    await page.locator(".swal2-popup:visible").waitFor({ state: "hidden" });
    check((await page.locator("#uno-decisiones").innerText()).includes("0"), "Cerrar el diálogo no ejecuta la acción");
    await page.locator("#uno-confirmar").click(); await page.locator(".swal2-confirm").click();
    await wait_text("uno-decisiones", "confirmadas: 1"); check(true, "Confirmación sí ejecuta");
    await page.locator("#uno-alerta").click();
    await page.locator(".swal2-popup:visible").filter({ hasText: "Alerta de atención" }).waitFor();
    await page.locator(".swal2-confirm").click(); check(true, "Alerta conserva SweetAlert");
    const box = await page.locator(".rf-notification:visible").first().boundingBox();
    const navbar = await page.locator(".main-header").count() ? await page.locator(".main-header").boundingBox() : null;
    check(!navbar || box.y >= navbar.y + navbar.height, "Mensajes bajo el navbar");
    await page.setViewportSize({ width: 390, height: 844 });
    await page.locator(".rf-notification:visible").first().waitFor();
    const mobile = await page.locator(".rf-notification:visible").first().boundingBox();
    check(mobile.x >= 0 && mobile.x + mobile.width <= 390, "Mensaje dentro de móvil");
    check(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), "Sin desborde horizontal");
    if (navbar) {
      await page.locator('[data-widget="pushmenu"]').first().click();
      await page.waitForFunction(() => document.body.classList.contains("sidebar-open"));
      await page.waitForFunction(() => document.querySelector(".rf-notifications").hidden);
      check(await page.locator(".rf-notification:visible").count() === 0, "No cubre sidebar móvil");
    }
    check(await page.locator('script[src$="/racafe-forms.js"]').count() === 1, "Dependencia JS única");
    check(external.length === 0 && errors.length === 0, "Sin red externa ni errores JS: " + errors.join(","));
    check(await page.locator(".shiny-output-error").count() === 0, "Sin errores del servidor");
    console.log(checks + " comprobaciones de archivos/notificaciones superadas en " + origin);
  } finally { await page.close(); await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
