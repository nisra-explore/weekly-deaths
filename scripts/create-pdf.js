const { chromium } = require("playwright");
const path = require("path");

(async () => {
  const siteDir = path.resolve("_site");
  const htmlPath = "file://" + path.join(siteDir, "index.html").replace(/\\/g, "/");
  const pdfPath = path.join(siteDir, "weekly-deaths.pdf");

  const browser = await chromium.launch();
  const page = await browser.newPage({
    viewport: { width: 1440, height: 1200 }
  });

  await page.goto(htmlPath, { waitUntil: "networkidle" });

  // Open all accordion/detail sections before printing.
  await page.evaluate(() => {
    document.querySelectorAll("details").forEach((el) => {
      el.setAttribute("open", "");
    });
  });

  // Give plotly/leaflet/widgets a moment to settle after accordions open.
  await page.waitForTimeout(5000);

  await page.pdf({
    path: pdfPath,
    format: "A4",
    printBackground: true,
    preferCSSPageSize: true,
    margin: {
      top: "12mm",
      right: "12mm",
      bottom: "12mm",
      left: "12mm"
    }
  });

  await browser.close();

  console.log(`Created ${pdfPath}`);
})();