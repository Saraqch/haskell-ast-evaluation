# Prueba: ghc-lib-parser (infraestructura de HLS)

## Información de la Biblioteca

| Campo | Detalle |
|---|---|
| **Nombre** | `ghc-lib-parser` |
| **Versión probada** | `9.6.7.20250325` |
| **Hackage** | https://hackage.haskell.org/package/ghc-lib-parser |
| **Repositorio fuente** | https://github.com/digital-asset/ghc-lib |
| **Última prueba** | Junio 2026 |
| **Estado** | Funcional |

## ¿Qué hace esta biblioteca?

`ghc-lib-parser` es el subconjunto del API de GHC necesario para parsear código Haskell.
Usa el **parser real de GHC** (no uno alternativo como `haskell-src-exts`), lo que
garantiza compatibilidad total con todas las extensiones del lenguaje.

Módulos utilizados en esta prueba:

- `GHC.Parser` — función `parseModule`, el punto de entrada del parser
- `GHC.Parser.Lexer` — mónada `P`, estado del parser y resultado (`ParseResult`)
- `GHC.Data.StringBuffer` — convierte un `String` al buffer que consume el parser
- `GHC.Types.SrcLoc` — ubicaciones en el código fuente
- `GHC.Hs` — tipos del AST de GHC en fase de parsing (`GhcPs`)
- `GHC.Utils.Outputable` — pretty-printing del AST mediante la clase `Outputable`

---

## Requisitos Previos

- **GHC** `9.6.7`
- **Cabal** `3.10.2.0` o superior

Si no los tenés instalados, usá [GHCup](https://www.haskell.org/ghcup/):

**Windows:** descargar el instalador desde https://www.haskell.org/ghcup/ y ejecutarlo.

Verificar instalación:
```bash
ghc --version    # debe mostrar 9.6.7
cabal --version  # debe mostrar 3.10.x o superior
```
## Cómo Ejecutar (paso a paso)

> Todos los comandos se ejecutan desde la carpeta `ghc-lib-parser/`.

**1. Moverse a la carpeta correcta**
```bash
cd ghc-lib-parser
```

**2. Actualizar el índice de paquetes** _(solo la primera vez)_
```bash
cabal update
```

**3. Compilar y ejecutar** _(la primera compilación tarda varios minutos: ghc-lib-parser es grande)_
```bash
cabal run ghc-lib-parser-prueba -- ./Ejemplo.hs
```

> **Nota:** `ghc-lib-parser` pesa ~150 MB de código fuente. La primera compilación
> puede tardar entre 5 y 15 minutos. Las siguientes son instantáneas.

> **Windows:** se recomienda tener el repositorio en una ruta sin espacios (ej. `C:\Projects\`).

---

## Estructura del Proyecto

```
ghc-lib-parser/
├── ghc-lib-parser-prueba.cabal   ← definición del proyecto y dependencias
├── Ejemplo.hs                    ← archivo Haskell de entrada para la prueba
└── src/
    └── Main.hs                   ← programa principal
```

---

 