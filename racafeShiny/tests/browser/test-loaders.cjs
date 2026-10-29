// Iniciar inst/examples/loaders.R; repetir con RACAFE_LOADER_BS4DASH=1.
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
    const initial = page.locator(".waiter-overlay .rsh-loader--inicio");
    await initial.waitFor({ state: "visible" });
    check((await initial.innerText()).includes("Cargando"), "Inicio visible mientras se prepara la sesión");
    check(await page.locator(".waiter-overlay .rsh-loader-card").count() === 1, "Preloader inicial sin duplicados");
    const initialLogo = initial.locator("img");
    await initialLogo.evaluate(image => image.decode());
    check(await initialLogo.evaluate(image => image.naturalWidth > 0), "Logo inicial servido localmente");
    await page.locator("#uno-resultado").filter({ hasText: "Pendiente" }).waitFor();
    await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
    await initial.waitFor({ state: "detached" });
    check(true, "Inicio cierra al terminar la carga inicial");
    await page.locator("#uno-recargar").click();
    await host.waitFor({ state: "visible", timeout: 1000 });
    check((await host.innerText()).includes("Actualizando"), "Clic muestra la variante sin esperar respuesta");
    check((await page.locator("#uno-resultado").innerText()) === "Pendiente", "Servidor aún procesando");
    await host.locator("img").evaluate(image => image.decode());
    check(await host.locator("img").evaluate(image => image.naturalWidth > 0), "Logo local en actualizar");
    const spinner = host.locator(".rsh-loader-spinner");
    const first = await spinner.evaluate(el => getComputedStyle(el).transform);
    await page.waitForTimeout(150);
    const second = await spinner.evaluate(el => getComputedStyle(el).transform);
    check(first !== second, "Animación activa mientras R duerme");
    await page.locator("#uno-resultado").filter({ hasText: "Actualizado" }).waitFor();
    await host.waitFor({ state: "hidden" }); check(true, "Cierre automático al finalizar");
    check((await page.locator("#uno-tabla tbody tr").first().innerText()).includes("11"), "Tabla actualizada realmente");
    await page.locator("#dos-calcular").click();
    await host.waitFor({ state: "visible", timeout: 1000 });
    check((await host.innerText()).includes("Procesando cálculos"), "Segundo namespace usa su variante");
    await host.locator("img").evaluate(image => image.decode());
    check(await host.locator("img").evaluate(image => image.naturalWidth > 0), "Logo local en calculando");
    await page.locator("#dos-resultado").filter({ hasText: "Calculado" }).waitFor();
    await host.waitFor({ state: "hidden" });
    check((await page.locator("#uno-resultado").innerText()) === "Actualizado", "Módulos independientes");
    await page.locator("#uno-fallar").click(); await host.waitFor({ state: "visible" });
    await page.locator("#uno-resultado").filter({ hasText: "Error controlado" }).waitFor();
    await host.waitFor({ state: "hidden" }); check(true, "Error no deja overlay colgado");
    await page.locator("#uno-mostrar").click();
    await page.locator("#uno-panel .waiter-overlay").waitFor();
    check(await page.locator("#dos-panel .waiter-overlay").count() === 0, "Objeto local no cubre otro módulo");
    await page.locator("#uno-mostrar").click();
    await page.waitForTimeout(100);
    check(await page.locator("#uno-panel .waiter-overlay").count() === 1, "Mostrar dos veces no duplica el objeto");
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
    check((await host.innerText()).includes("Procesando cálculos"), "Detección busy sin trigger");
    await page.locator("#uno-resultado").filter({ hasText: "Calculado" }).waitFor();
    await host.waitFor({ state: "hidden" });
    for (const [operation, message] of Object.entries({ consultar: "Consultando información", guardar: "Guardando información",
      enviar: "Enviando información", generar: "Generando documento", descargar: "Preparando descarga",
      validar: "Validando información", importar: "Importando información", eliminar: "Eliminando registro",
      sincronizar: "Sincronizando información" })) {
      if (operation === "consultar") {
        await page.locator("#uno-consultar").focus();
        await page.locator("#uno-consultar").press("Enter");
      } else await page.locator("#uno-" + operation).click();
      await host.waitFor({ state: "visible", timeout: 1000 });
      check((await host.innerText()).includes(message), "Variante " + operation);
      const logo = host.locator("img"); await logo.evaluate(image => image.decode());
      check(await logo.evaluate(image => image.naturalWidth > 0), "Logo local en " + operation);
      await host.waitFor({ state: "hidden" });
    }
    await page.locator("#uno-archivo").setInputFiles({ name: "datos.xlsx", buffer: xlsx,
      mimeType: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
    await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
    await page.locator("#uno-importar").click();
    await page.waitForFunction(() => /Archivo importado|Instale readxl/.test(document.querySelector("#uno-resultado").textContent));
    await host.waitFor({ state: "hidden" }); check(true, "Excel real importado o requisito opcional informado sin bloqueo");
    await page.locator("#uno-archivo").setInputFiles({ name: "datos.csv", mimeType: "text/csv",
      buffer: Buffer.from("registro,valor\n1,50\n2,100\n") });
    await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
    await page.locator("#uno-importar").click();
    await page.locator("#uno-resultado").filter({ hasText: "Archivo importado: 2 filas" }).waitFor();
    await host.waitFor({ state: "hidden" }); check(true, "Importación local sin servicios externos");
    await page.locator("#uno-validar").click();
    await page.locator("#uno-resultado").filter({ hasText: "Archivo validado" }).waitFor();
    await host.waitFor({ state: "hidden" }); check(true, "Validación real del archivo cargado");
    await page.locator("#uno-descargar").click();
    await page.locator("#uno-resultado").filter({ hasText: "Descarga preparada" }).waitFor();
    await host.waitFor({ state: "hidden" });
    const downloadEvent = page.waitForEvent("download");
    await page.locator("#uno-pdf").click();
    const download = await downloadEvent;
    const fs = require("node:fs/promises");
    const pdf = await fs.readFile(await download.path());
    check(pdf.subarray(0, 5).toString() === "%PDF-", "Descarga de PDF real generado localmente");
    await page.waitForFunction(() => !document.querySelector(".rsh-loader-host:not([hidden])"));
    check(await page.locator(".rf-notification:visible").count() > 0, "racafeNotify visible después del cierre");
    // Los fallos de una imagen propia no bloquean el loader ni fuerzan red externa.
    await page.locator("#dos-calcular").click(); await host.waitFor({ state: "visible" });
    await host.locator("img").evaluate(image => { image.src = "logo-inexistente.png"; });
    await page.waitForFunction(() => {
      const image = document.querySelector(".rsh-loader-host img");
      return image && image.dataset.rshTriedFallback && image.complete && image.naturalWidth > 0;
    });
    check(true, "Logo personalizado fallido vuelve al corporativo local");
    await page.locator("#dos-resultado").filter({ hasText: "Calculado" }).waitFor();
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
    await page.locator("#uno-mostrar").click();
    await page.locator("#uno-panel .waiter-overlay").waitFor();
    await page.evaluate(() => {
      window.waiter.show({ id: null, html: '<span class="foreign-waiter">Ajeno</span>', color: "white" });
      window.jQuery(document).trigger("shiny:disconnected");
    });
    check(await page.locator("#uno-panel .waiter-overlay").count() === 0, "Desconexión limpia el objeto corporativo");
    check(await page.locator(".foreign-waiter").count() === 1, "Desconexión conserva waiters ajenos");
    check(await page.locator('script[src*="loaders.js"]').count() === 1, "Un solo recurso JS");
    check(await page.locator('link[href*="loaders.css"]').count() === 1, "Un solo recurso CSS");
    check(external.length === 0, "Sin solicitudes externas");
    check(errors.length === 0, errors.join("\n"));
    console.log(`${checks} comprobaciones de loaders superadas en ${url}`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
