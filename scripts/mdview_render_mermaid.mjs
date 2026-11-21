#!/usr/bin/env node
/**
 * Batched single-browser HiDPI Mermaid-to-PNG renderer using Playwright Core
 * and local mermaid@11.12.0 styled with a Dracula dark theme.
 *
 * Usage:
 *   MDVIEW_NODE_MODULES=~/modules/mermaid-internal/node_modules \
 *     node mdview_render_mermaid.mjs /path/to/manifest.json
 */

import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";

const manifestPath = process.argv[2];
if (!manifestPath) {
  process.stderr.write("Usage: mdview_render_mermaid.mjs <manifest.json>\n");
  process.exit(2);
}

const manifest = JSON.parse(fs.readFileSync(manifestPath, "utf-8"));
const nodeModulesDir =
  manifest.node_modules_dir ||
  process.env.MDVIEW_NODE_MODULES ||
  path.join(process.env.HOME || "", "modules/mermaid-internal/node_modules");

const requireFromModules = createRequire(
  path.join(nodeModulesDir, "package.json")
);
const { chromium } = requireFromModules("playwright-core");

const jobs = manifest.jobs || [];
if (jobs.length === 0) {
  process.stdout.write(JSON.stringify({ results: [] }) + "\n");
  process.exit(0);
}

const bgHex = manifest.background || "#282a36";
const scale = manifest.scale || 2;
const chromePath = manifest.chrome_path;
const mermaidJsPath = manifest.mermaid_js_path;

(async () => {
  const browser = await chromium.launch({
    executablePath: chromePath,
    args: [
      "--no-sandbox",
      "--disable-setuid-sandbox",
      "--disable-gpu",
      "--disable-dev-shm-usage",
      "--font-render-hinting=none",
    ],
  });

  try {
    const page = await browser.newPage({
      deviceScaleFactor: scale,
      viewport: { width: 3200, height: 2400 },
    });

    const containersHtml = jobs
      .map(
        (_, idx) =>
          `<div id="wrap-${idx}" class="diagram-wrap"><div id="target-${idx}"></div></div>`
      )
      .join("\n");

    await page.setContent(`<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<style>
  body {
    margin: 0;
    padding: 0;
    background: ${bgHex};
    color: #f8f8f2;
    font-family: "JetBrains Mono", "DejaVu Sans Mono", monospace;
  }
  .diagram-wrap {
    display: inline-block;
    background: ${bgHex};
    padding: 14px 18px;
    box-sizing: border-box;
  }
  svg {
    max-width: none !important;
    height: auto;
  }
</style>
</head>
<body>
${containersHtml}
</body>
</html>`);

    await page.addScriptTag({ path: mermaidJsPath });

    const evalStatuses = await page.evaluate(async (inputJobs) => {
      /* global mermaid */
      mermaid.initialize({
        startOnLoad: false,
        securityLevel: "strict",
        theme: "dark",
        fontSize: 16,
        fontFamily: '"JetBrains Mono", "DejaVu Sans Mono", monospace',
        flowchart: {
          htmlLabels: true,
          useMaxWidth: false,
          curve: "basis",
          padding: 14,
        },
        sequence: { useMaxWidth: false },
        state: { useMaxWidth: false },
        class: { useMaxWidth: false },
        er: { useMaxWidth: false },
        gantt: { useMaxWidth: false },
        journey: { useMaxWidth: false },
        gitGraph: { useMaxWidth: false },
        mindmap: { useMaxWidth: false },
        timeline: { useMaxWidth: false },
        quadrantChart: { useMaxWidth: false },
        requirement: { useMaxWidth: false },
        c4: { useMaxWidth: false },
        themeVariables: {
          darkMode: true,
          background: "#282a36",
          primaryColor: "#383a59",
          primaryTextColor: "#f8f8f2",
          primaryBorderColor: "#bd93f9",
          lineColor: "#8be9fd",
          secondaryColor: "#44475a",
          tertiaryColor: "#21222c",
          noteBkgColor: "#44475a",
          noteTextColor: "#f1fa8c",
          noteBorderColor: "#6272a4",
          actorBkg: "#383a59",
          actorBorder: "#bd93f9",
          actorTextColor: "#f8f8f2",
          signalColor: "#8be9fd",
          edgeLabelBackground: "#282a36",
        },
      });

      const statuses = [];
      for (let idx = 0; idx < inputJobs.length; idx++) {
        const job = inputJobs[idx];
        const target = document.getElementById(`target-${idx}`);
        try {
          const rendered = await mermaid.render(`mermaid_svg_${idx}`, job.code);
          target.innerHTML = rendered.svg;
          const svgEl = target.querySelector("svg");
          if (svgEl) {
            const vb = (svgEl.getAttribute("viewBox") || "")
              .trim()
              .split(/[\s,]+/)
              .map(Number);
            const attrW = svgEl.getAttribute("width") || "";
            if (
              vb.length === 4 &&
              vb[2] > 0 &&
              vb[3] > 0 &&
              (!attrW || attrW.includes("%"))
            ) {
              svgEl.setAttribute("width", String(Math.ceil(vb[2])));
              svgEl.setAttribute("height", String(Math.ceil(vb[3])));
              svgEl.style.width = `${Math.ceil(vb[2])}px`;
              svgEl.style.height = `${Math.ceil(vb[3])}px`;
            }
          }
          statuses.push({ id: job.id, ok: true, error: "" });
        } catch (err) {
          statuses.push({
            id: job.id,
            ok: false,
            error: err && err.message ? String(err.message) : String(err),
          });
        }
      }
      return statuses;
    }, jobs);

    const results = [];
    for (let idx = 0; idx < jobs.length; idx++) {
      const status = evalStatuses[idx];
      const job = jobs[idx];
      if (!status.ok) {
        results.push(status);
        continue;
      }
      const wrap = page.locator(`#wrap-${idx}`);
      const box = await wrap.boundingBox();
      await wrap.screenshot({
        path: job.out_png,
        omitBackground: false,
      });
      results.push({
        id: job.id,
        ok: true,
        width_px: box ? Math.round(box.width * scale) : 0,
        height_px: box ? Math.round(box.height * scale) : 0,
        error: "",
      });
    }

    process.stdout.write(JSON.stringify({ results }) + "\n");
  } finally {
    await browser.close();
  }
})().catch((err) => {
  process.stderr.write(
    `mdview_render_mermaid failed: ${err && err.stack ? err.stack : err}\n`
  );
  process.exit(1);
});
