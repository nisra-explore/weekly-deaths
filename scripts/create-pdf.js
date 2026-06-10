const { chromium } = require("playwright");
const path = require("path");

(async () => {
  const siteDir = path.resolve("docs");
  const htmlPath = "file://" + path.join(siteDir, "index.html").replace(/\\/g, "/");
  const pdfPath = path.join(siteDir, "weekly-deaths.pdf");

  const browser = await chromium.launch();

  const page = await browser.newPage({
    viewport: {
      width: 1600,
      height: 1200
    },
    deviceScaleFactor: 1
  });

  await page.goto(htmlPath, { waitUntil: "networkidle" });

  // Open all accordions/details before printing
  await page.evaluate(() => {
    document.querySelectorAll("details").forEach((el) => {
      el.setAttribute("open", "");
    });
  });

  // Let plots/maps/layout settle
  await page.waitForTimeout(5000);

  // Make sure the page itself is not constrained/cropped during print
  await page.addStyleTag({
    content: `
      @media print {
        @page {
          size: 1600px auto;
          margin: 20px;
        }

        html, body {
          width: 1600px !important;
          max-width: none !important;
          overflow: visible !important;
        }

        body {
          zoom: 1 !important;
        }

        main,
        .content,
        .page-layout-full,
        .quarto-container,
        .quarto-layout-panel,
        .grid,
        .card {
          max-width: none !important;
          overflow: visible !important;
        }

        .publication-actions,
        #cookie-banner,
        .cookies-infobar {
          display: none !important;
        }

        details,
        .accordion {
          display: block !important;
        }
      }
    `
  });

  await page.pdf({
    path: pdfPath,
    printBackground: true,
    preferCSSPageSize: true,
    width: "2000px",
    height: "2200px",
    margin: {
      top: "20px",
      right: "20px",
      bottom: "20px",
      left: "20px"
    }
  });

  await browser.close();

  console.log(`Created ${pdfPath}`);
})();