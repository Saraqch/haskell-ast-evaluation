"""Adds load/run timing measurements to the upstream playground index.html."""
import sys

path = sys.argv[1]
html = open(path, encoding="utf-8").read()

replacements = [
    # timing helpers right after the imports
    ('import { DyLDBrowserHost, main } from "./dyld.mjs";',
     '''import { DyLDBrowserHost, main } from "./dyld.mjs";

      const timings = [];
      let last = performance.now();
      const mark = (label) => {
        const now = performance.now();
        timings.push({ fase: label, ms: Math.round(now - last), acumulado_ms: Math.round(now) });
        last = now;
        const el = document.getElementById("timings");
        if (el) el.textContent = timings.map((t) => `${t.fase}: ${t.ms} ms (total ${(t.acumulado_ms / 1000).toFixed(1)} s)`).join(String.fromCharCode(10));
        console.table(timings);
      };
      window.timings = timings;
      mark("pagina e imports (monaco, wasi shim)");'''),
    ('fetch("./rootfs.tar.zst").then((r) => r.bytes()),',
     'fetch("./rootfs.tar.zst", { cache: new URLSearchParams(location.search).has("nocache") ? "reload" : "default" }).then((r) => r.bytes()),'),
    ('bsdtar_wasi.fds[0] = new OpenFile(',
     'mark("descarga rootfs + bsdtar");\n      bsdtar_wasi.fds[0] = new OpenFile('),
    ('bsdtar_wasi.start(instance);',
     'bsdtar_wasi.start(instance);\n      mark("descompresion rootfs");'),
    ('const main_func = await dyld.exportFuncs.myMain("/tmp/hslib/lib");',
     'mark("enlazado de GHC (dyld)");\n      const main_func = await dyld.exportFuncs.myMain("/tmp/hslib/lib");\n      mark("sesion GHC lista");'),
    ('''          await main_func(
            document.getElementById("ghcArgs").value,
            editor.getValue()
          );''',
     '''          last = performance.now();
          await main_func(
            document.getElementById("ghcArgs").value,
            editor.getValue()
          );
          mark("ejecucion (compilar + correr)");'''),
    ('<div class="outputs">',
     '<pre id="timings" style="font-size:.8rem;opacity:.85;white-space:pre-wrap">Cargando GHC...</pre>\n        <div class="outputs">'),
]

for old, new in replacements:
    if html.count(old) != 1:
        sys.exit(f"patch-timings: expected exactly one match for: {old[:60]!r}")
    html = html.replace(old, new)

open(path, "w", encoding="utf-8").write(html)
print("patch-timings: index.html instrumented")
