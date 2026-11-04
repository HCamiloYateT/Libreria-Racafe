const assert = require('node:assert/strict');
const { chromium } = require('playwright');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ['--no-sandbox'] : []});
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  try {
    const page = await browser.newPage({viewport: {width:1440, height:1000}}), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(process.env.RACAFE_GALLERY_URL || 'http://127.0.0.1:4040');
    await page.locator('#example1-value').waitFor();
    await page.locator('#controlbar-toggle').click();
    const form = page.locator('#corporate_demo-form');
    await form.scrollIntoViewIfNeeded();
    const open = () => page.evaluate(() => document.body.classList.contains('control-sidebar-slide-open'));
    await page.evaluate(() => {
      window.nativeClicks = 0;
      window.addEventListener('click', () => window.nativeClicks++);
    });
    await page.locator('#corporate_demo-save').click();
    await form.locator('.rf-validation-error').waitFor();
    check(await form.locator('.rf-validation-error').count() === 1, 'Un mensaje por campo obligatorio');
    await page.locator('#corporate_demo-name').fill('Ana');
    await form.locator('.rf-validation-warning').waitFor();
    check(await form.locator('.rf-validation-error').count() === 0, 'La advertencia no es un error');
    check(await page.locator('#corporate_demo-name').getAttribute('aria-invalid') === 'false', 'Aviso no invalida');
    await page.locator('#corporate_demo-dynamic').check();
    await page.locator('#corporate_demo-dynamic_text').waitFor();
    await form.locator('.rf-validation-error').waitFor();
    await page.locator('#corporate_demo-dynamic_text').fill('Dinámico');
    await page.locator('#corporate_demo-insert').click();
    await page.locator('#corporate_demo-inserted_text').waitFor();
    await page.locator('#corporate_demo-insert').click();
    check(await form.locator('#corporate_demo-inserted_text').count() === 1, 'insertUI no duplica controles');
    await page.locator('#corporate_demo-inserted_text').fill('Insertado');
    const picker = page.locator('.bootstrap-select:has(#corporate_demo-region)');
    await picker.locator('button.dropdown-toggle').click();
    await picker.locator('.dropdown-menu li').filter({hasText:'Caribe'}).last().click();
    await page.waitForFunction(() => [...document.querySelector('#corporate_demo-city').options]
      .some(option => option.value === 'cartagena'));
    check(await open(), 'Actualización dependiente conserva controlbar');
    check(await page.locator('#corporate_demo-region').inputValue() === 'caribe', 'Mayúsculas conservan código');
    await page.locator('#corporate_demo-range input').first().click();
    await page.locator('.datepicker-dropdown:visible .datepicker-days td.day:not(.old):not(.new)').nth(12).click();
    await page.locator('#corporate_demo-range input').last().click();
    await page.locator('.datepicker-dropdown:visible .datepicker-days td.day:not(.old):not(.new)').nth(16).click();
    check(await open(), 'Calendario de rango conserva controlbar');
    await page.locator('#corporate_demo-save').click();
    await page.locator('.rf-modal .rf-table').waitFor();
    check(await open(), 'Abrir resumen conserva controlbar');
    check(await page.locator('.rf-modal .rf-info-item').count() === 1, 'Campo NULL omitido');
    check(await page.locator('.rf-modal .rf-btn--secondary').count() === 1, 'Cancelar secundario');
    check(await page.locator('.rf-modal .rf-btn--primary').count() === 1, 'Confirmar rojo');
    await page.locator('.rf-modal .rf-btn--secondary').click();
    await page.waitForFunction(() => !document.querySelector('.modal.show'));
    check(await open(), 'Cancelar modal conserva controlbar');
    await page.locator('#corporate_demo-save').click();
    await page.locator('#corporate_demo-confirm').click();
    await page.waitForFunction(() => {
      const state = document.querySelector('#corporate_demo-state').textContent;
      return /\$attempted\s+\[1\] FALSE/.test(state) && /\$saved\s+\[1\] TRUE/.test(state) &&
        !document.querySelector('#corporate_demo-form .rf-validation-error,#corporate_demo-form .rf-validation-warning');
    });
    check(await form.locator('.rf-validation-error,.rf-validation-warning').count() === 0, 'Guardar limpia validación');
    await page.locator('#corporate_demo-name').fill('');
    check(await form.locator('.rf-validation-error').count() === 0, 'Edición tras guardar vuelve al modo diferido');
    await page.locator('#corporate_demo-save').click();
    await form.locator('.rf-validation-error').waitFor();
    await page.locator('#corporate_demo-reset').click();
    await page.waitForFunction(() => !document.querySelector('#corporate_demo-form .rf-validation-error'));
    check(await page.locator('#corporate_demo-name').inputValue() === '', 'Reset conserva el valor');
    check(await open(), 'Controles dinámicos conservan controlbar');
    await page.locator('#corporate_demo-nested-check').click();
    await page.locator('#corporate_demo-nested-form .rf-validation-error').waitFor();
    await page.locator('#corporate_demo-save').click();
    await page.locator('#corporate_demo-name_validation').waitFor();
    await page.locator('#corporate_demo-reset').click();
    await page.waitForFunction(() => !document.querySelector('#corporate_demo-name_validation'));
    check(await page.locator('#corporate_demo-nested-form .rf-validation-error').count() === 1,
      'El reset del padre conserva la validación del módulo anidado');
    await page.locator('#corporate_demo-nested-child').fill('Hijo');
    await page.waitForFunction(() => !document.querySelector('#corporate_demo-nested-form .rf-validation-error'));
    check(true, 'Binding y validación del namespace anidado funcionan');
    await page.waitForFunction(() => document.querySelector('.control-sidebar').getAttribute('data-pin') !== 'true');
    check(await page.locator('#corporate_demo-name').evaluate(el => {
      let count = 0;
      const listener = event => { if (event.target === el) count++; };
      window.addEventListener('click', listener);
      el.dispatchEvent(new MouseEvent('click', {bubbles:true}));
      window.removeEventListener('click', listener);
      return count === 1;
    }), 'El componente no intercepta la propagación hacia window y Bootstrap');
    await page.locator('#controlbarPin').click();
    await page.waitForFunction(() => document.querySelector('.control-sidebar').getAttribute('data-pin') === 'true');
    await page.locator('.rf-gallery h1').click();
    check(await open(), 'pinned nativo conserva el panel al hacer clic fuera');
    check(await page.locator('#controlbar-toggle').evaluate(el => el.classList.contains('disabled')),
      'La fijación conserva el estado nativo del botón');
    await page.locator('#controlbarPin').click();
    await page.waitForFunction(() => document.querySelector('.control-sidebar').getAttribute('data-pin') === 'false');
    await page.locator('#controlbar-toggle').click();
    await page.waitForFunction(() => !document.body.classList.contains('control-sidebar-slide-open'));
    check(true, 'Cierre nativo mediante botón conservado');
    if (process.env.RACAFE_MODULE_URL) {
      const modules = await browser.newPage({viewport:{width:1440, height:1000}});
      modules.on('pageerror', error => errors.push(error.message));
      await modules.goto(process.env.RACAFE_MODULE_URL);
      await modules.locator('#seg_multiple-sel').waitFor({state:'attached'});
      await modules.locator('#controlbar-toggle').click();
      const picker = modules.locator('.bootstrap-select:has(#bar_multiple-sel)');
      await picker.locator('button.dropdown-toggle').click();
      await picker.locator('.dropdown-menu li').filter({hasText:'AGREGAR...'}).last().click();
      await modules.locator('#bar_multiple-nuevo_valor').fill('Nueva demo');
      await modules.locator('#bar_multiple-guardar_nuevo').click();
      await modules.locator('.rf-modal .rf-info-item').waitFor();
      await modules.locator('#bar_multiple-CancelarNuevo').click();
      await modules.waitForFunction(() => !document.querySelector('.modal.show'));
      await modules.locator('#bar_multiple-guardar_nuevo').click();
      await modules.locator('#bar_multiple-ConfirmarNuevo').click();
      await modules.waitForFunction(() => [...document.querySelector('#bar_multiple-sel').options]
        .some(option => option.value === 'Nueva demo'));
      const state = await modules.locator('#bar_multiple-sel').evaluate(el => ({
        first:el.options[0].value, selected:[...el.selectedOptions].map(option => option.value),
        added:[...el.options].filter(option => option.value === 'Nueva demo').length
      }));
      check(state.first === 'AGREGAR...', 'AGREGAR sigue primero después de guardar');
      check(state.added === 1 && state.selected.includes('Premium') && state.selected.includes('Nueva demo'),
        'Resumen incorporado una vez y selección múltiple conservada');
      check(await modules.evaluate(() => document.body.classList.contains('control-sidebar-slide-open')),
        'Confirmación del agregable conserva controlbar');
    }
    check(errors.length === 0, errors.join('; '));
    console.log(`${checks} comprobaciones de resumen, validación, renderUI/insertUI y controlbar superadas.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
