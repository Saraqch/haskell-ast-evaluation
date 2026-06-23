# Prueba: haskell-src-exts

## Informacion de la Libreria

| Campo | Detalle |
|---|---|
| **Nombre** | `haskell-src-exts` |
| **Versión probada** | `1.23.1` |
| **Hackage** | https://hackage.haskell.org/package/haskell-src-exts |
| **Ultima actualizacion** | 21 de Junio del 2026 |
| **Estado** | Funcional |

## Que hace esta libreria?

haskell-src-exts provee soporte para manipular codigo fuente Haskell. Incluye un lexer, parser, pretty-printer y una definicion completa del AST de Haskell con extensiones.

Modulos principales utilizados:
- `Language.Haskell.Exts.Parser` - parsea codigo fuente en un AST
- `Language.Haskell.Exts.Syntax` - define los tipos del arbol sintactico
- `Language.Haskell.Exts.Pretty` - imprime el AST de vuelta a codigo

## Cómo Ejecutar

```bash
# Desde la carpeta raiz del repositorio:
cd haskell-src-exts

cabal update        # solo la primera vez
cabal run haskell-src-exts-prueba -- Ejemplo.hs
```

