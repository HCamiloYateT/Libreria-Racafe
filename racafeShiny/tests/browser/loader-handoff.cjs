// Ejecutar con Playwright y las rutas JQUERY_JS / WAITER_JS de los recursos instalados.
const { chromium } = require('playwright');
const path = require('path');
(async () => {
  const browser = await chromium.launch({headless:true,
    executablePath:process.env.CHROMIUM_EXECUTABLE || '/usr/bin/chromium', args:['--no-sandbox']});
  const page = await browser.newPage();
  await page.setContent(`<div id="startup" class="waiter-overlay" style="position:fixed;inset:0">Inicio</div>
    <div class="rsh-loader-config" hidden data-startup="false" data-busy="true">
      <template data-rsh-type="calculando"><div class="rsh-loader-card">Calculando</div></template>
      <span data-rsh-trigger="calcular" data-rsh-type="calculando"></span>
    </div><button id="calcular">Calcular</button>`);
  await page.addScriptTag({path:process.env.JQUERY_JS || '/usr/share/javascript/jquery/jquery.min.js'});
  await page.evaluate(() => { window.Shiny = {addCustomMessageHandler(){}, setInputValue(){}}; });
  await page.addScriptTag({path:process.env.WAITER_JS});
  await page.addScriptTag({path:process.env.LOADER_JS || path.resolve(__dirname,'../../inst/loaders/loaders.js')});
  await page.evaluate(() => new Promise(requestAnimationFrame));
  await page.evaluate(() => jQuery(document).trigger('shiny:busy'));
  if (await page.locator('#rsh-loader-host .waiter-overlay').count()) throw Error('Duplica el preloader inicial');
  // Al desaparecer el inicial, el cálculo continúa sin otro evento busy.
  await page.evaluate(() => { document.getElementById('startup').style.display = 'none'; });
  await page.waitForSelector('#rsh-loader-host .waiter-overlay .rsh-loader-card');
  await page.evaluate(() => jQuery(document).trigger('shiny:busy'));
  if (await page.locator('#rsh-loader-host .waiter-overlay').count() !== 1) throw Error('Loader duplicado');
  await page.evaluate(() => jQuery(document).trigger('shiny:idle'));
  await page.waitForFunction(() => document.getElementById('rsh-loader-host').hidden);
  // La intención de cálculo se muestra antes de recibir el evento busy del servidor.
  await page.locator('#calcular').click();
  await page.waitForFunction(() => !document.getElementById('rsh-loader-host').hidden);
  await page.evaluate(() => jQuery(document).trigger('shiny:disconnected'));
  await page.waitForFunction(() => document.getElementById('rsh-loader-host').hidden);
  console.log('OK: relevo inicio→cálculo, overlays ocultos, instancia única, triggers y desconexión.');
  await browser.close();
})().catch(error => { console.error(error); process.exit(1); });
