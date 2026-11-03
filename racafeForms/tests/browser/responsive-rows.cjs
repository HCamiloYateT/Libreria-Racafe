// NODE_PATH=/opt/codex/cua_node/lib/node_modules node racafeForms/tests/browser/responsive-rows.cjs
const { chromium } = require('playwright');
const assert = require('node:assert/strict');
(async () => {
  const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
    headless: true, args: ['--no-sandbox']});
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', e => errors.push(e.message));
  await page.goto(process.env.ROW_TEST_URL || 'http://127.0.0.1:4031');
  await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket?.readyState === 1);
  await page.locator('[data-widget="control-sidebar"]').first().click();
  await page.waitForFunction(() => document.querySelector('.control-sidebar').getBoundingClientRect().width > 0);
  let checks = 0;
  for (const viewport of [1440, 700, 390]) {
    await page.setViewportSize({width: viewport, height: 900});
    for (const width of [740, 520, 360, 240, 170, 90]) {
      const rows = await page.evaluate(({width, viewport}) => {
        const result = [];
        document.querySelectorAll('.rf-form[id]').forEach(form => {
          if (!/^(main|bar)-/.test(form.id)) return;
          form.style.width = Math.min(width, viewport - 40) + 'px';
          form.style.maxWidth = 'none';
        });
        document.querySelectorAll('.rf-form[id]').forEach(form => {
          if (!/^(main|bar)-/.test(form.id)) return;
          const [location, density, mode, columns] = form.id.split('-');
          const row = form.querySelector('.rf-row');
          const style = getComputedStyle(row);
          result.push({id: form.id, location, density, mode, columns: +columns,
            width: row.getBoundingClientRect().width,
            tracks: style.gridTemplateColumns.split(' ').map(Number.parseFloat),
            gap: parseFloat(style.columnGap), overflow: row.scrollWidth - row.clientWidth,
            fields: [...row.children].map(el => el.getBoundingClientRect().width)});
        });
        return result;
      }, {width, viewport});
      for (const row of rows) {
        let expected;
        if (row.mode === 'legacy') {
          expected = viewport <= 700 || (row.location === 'bar' && row.density === 'compact') ? 1 : row.columns;
        } else if (row.mode === 'fixed') {
          expected = Math.min(row.columns, Math.max(1, Math.floor((row.width + row.gap + 0.1) / (80 + row.gap))));
        } else {
          expected = row.location === 'bar' && row.mode === 'stack' ? 1 :
            Math.min(row.columns, row.width >= 688 ? 4 : row.width >= 512 ? 3 : row.width >= 336 ? 2 : 1);
        }
        assert.equal(row.tracks.length, expected, JSON.stringify(row)); checks++;
        assert.ok(Math.max(...row.fields) - Math.min(...row.fields) < 1, 'Anchos desiguales: ' + row.id); checks++;
        if (row.mode !== 'legacy') {
          assert.ok(row.overflow <= 1, 'Desbordamiento: ' + JSON.stringify(row)); checks++;
        }
      }
    }
  }
  await page.setViewportSize({width:1440, height:900});
  for (const id of ['demo', 'bardemo']) {
    if (id === 'bardemo') {
      await page.locator('[data-widget="control-sidebar"]').first().click();
      await page.waitForFunction(() => document.querySelector('.control-sidebar').getBoundingClientRect().x < innerWidth);
    }
    for (const width of [740, 360, 170, 90]) {
      const overflow = await page.locator(`#${id}-mixed .rf-row`).evaluate((row, width) => {
        row.closest('.rf-form').style.width = width + 'px';
        return row.scrollWidth - row.clientWidth;
      }, width);
      assert.ok(overflow <= 1, `${id}: controles mixtos sin desbordar a ${width}px`); checks++;
    }
    await page.locator(`#${id}-mixed`).evaluate(el => el.style.width = '360px');
    await page.locator(`#${id}-number`).press('ControlOrMeta+A');
    await page.locator(`#${id}-number`).pressSequentially('123.45');
    await page.locator(`#${id}-number`).press('Tab');
    const picker = page.locator(`.bootstrap-select:has(#${id}-select)`);
    await picker.locator('button.dropdown-toggle').click();
    await picker.locator('.dropdown-menu li').filter({hasText: 'Café'}).last().click();
    await picker.locator('.dropdown-menu li').filter({hasText: 'Cacao'}).last().click();
    assert.ok(await picker.locator('button.dropdown-toggle').getAttribute('aria-expanded') === 'true'); checks++;
    await picker.locator('button.dropdown-toggle').click();
    await page.locator(`#${id}-date`).click();
    await page.waitForSelector('.air-datepicker.-active-');
    const day = page.locator('.air-datepicker.-active- .air-datepicker-cell.-day-:not(.-other-month-)').nth(14);
    const selectedDate = await day.evaluate(el => [el.dataset.year,
      String(+el.dataset.month + 1).padStart(2, '0'), el.dataset.date.padStart(2, '0')].join('-'));
    await day.click();
    await page.waitForFunction(({id, selectedDate}) => {
      const text = document.querySelector(`#${id}-values`).textContent;
      return text.includes('123.45') && text.includes('cacao') && text.includes(selectedDate);
    }, {id, selectedDate}); checks++;
    if (id === 'bardemo') {
      assert.ok(await page.locator('.control-sidebar').isVisible()); checks++;
    }
  }
  assert.deepEqual(errors, []); checks++;
  console.log(`${checks} comprobaciones de columnas, densidades, contenedores, compatibilidad y bindings: OK`);
  await browser.close();
})().catch(error => {console.error(error); process.exit(1);});
