const {chromium}=require('playwright');
const fs=require('node:fs/promises');
const assert=require('node:assert/strict');
const path=require('node:path');
const preview=process.env.RACAFE_MAIL_PREVIEW_DIR;
assert.ok(preview, 'Defina RACAFE_MAIL_PREVIEW_DIR');
(async()=>{
 const browser=await chromium.launch({executablePath:process.env.CHROMIUM_PATH,args:['--no-sandbox']});
 let checks=0;
 for(const width of [320,1440]){
  const page=await browser.newPage({viewport:{width,height:900}});
  const external=[];
  await page.route('**/*', route=>{external.push(route.request().url());return route.abort();});
  for(const name of ['invitacion','otp','notificacion','confirmacion']){
   const html=await fs.readFile(path.join(preview,name+'.html'),'utf8');
   await page.setContent(html,{waitUntil:'networkidle'});
   assert.equal(await page.locator('html').getAttribute('lang'),'es');checks++;
   assert.equal(await page.locator('script').count(),0);checks++;
   assert.equal(await page.locator('img').count(),2);checks++;
   const overflow=await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth);
   assert.equal(overflow,false,`Desborde ${name} ${width}`);checks++;
   const images=await page.locator('img').evaluateAll(elements=>elements.every(e=>e.complete&&e.naturalWidth>0));
   assert.ok(images, `Imagen de prueba: ${name} ${width}`);checks++;
  }
  assert.equal(external.length,0,'Preview sin solicitudes externas');checks++;
  await page.close();
 }
 console.log(`PASS ${checks}: cuatro previews a 320/1440 px; imágenes incluidas reales, sin red externa`);
 await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
