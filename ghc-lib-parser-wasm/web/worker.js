// Runs esp.wasm (a WASI command) off the main thread and reports timings.
import { WASI, File, OpenFile, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";

let wasmModule = null;

async function load({ url, bypassCache }) {
  const t0 = performance.now();
  const response = await fetch(url, { cache: bypassCache ? "reload" : "default" });
  if (!response.ok) throw new Error(`No se pudo descargar ${url} (${response.status})`);
  const bytes = await response.arrayBuffer();
  const t1 = performance.now();
  wasmModule = await WebAssembly.compile(bytes);
  const t2 = performance.now();

  const entry = performance.getEntriesByName(new URL(url, self.location.href).href).at(-1);
  return {
    downloadMs: t1 - t0,
    compileMs: t2 - t1,
    bytes: bytes.byteLength,
    transferBytes: entry ? entry.transferSize : null,
  };
}

async function explain(source) {
  const encoder = new TextEncoder();
  const decoder = new TextDecoder();
  const stdout = [];
  const stderr = [];
  const fds = [
    new OpenFile(new File(encoder.encode(source))),
    new ConsoleStdout((chunk) => stdout.push(chunk.slice())),
    new ConsoleStdout((chunk) => stderr.push(chunk.slice())),
  ];
  const wasi = new WASI(["esp-wasm"], [], fds);

  const t0 = performance.now();
  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });
  const t1 = performance.now();
  const exitCode = wasi.start(instance);
  const t2 = performance.now();

  const text = (chunks) => chunks.map((c) => decoder.decode(c, { stream: true })).join("") + decoder.decode();
  return {
    instantiateMs: t1 - t0,
    runMs: t2 - t1,
    exitCode,
    stdout: text(stdout),
    stderr: text(stderr),
  };
}

self.onmessage = async ({ data }) => {
  try {
    const result = data.type === "load" ? await load(data) : await explain(data.source);
    self.postMessage({ id: data.id, ok: true, result });
  } catch (error) {
    self.postMessage({ id: data.id, ok: false, error: String(error && error.stack || error) });
  }
};
