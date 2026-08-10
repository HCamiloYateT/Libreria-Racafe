#!/usr/bin/env python3
"""Comprueba la cobertura documental de la API pública del monorepo."""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
EXPORT_RE = re.compile(r'^export\((?:"([^"]+)"|([^)]*))\)$')


def exports(namespace: Path) -> list[str]:
    """Devuelve los símbolos exportados por un archivo NAMESPACE."""
    result = []
    for line in namespace.read_text(encoding="utf-8").splitlines():
        match = EXPORT_RE.match(line.strip())
        if match:
            result.append((match.group(1) or match.group(2)).strip())
    return result


def roxygen_block(package: Path, symbol: str) -> list[str] | None:
    """Localiza el bloque Roxygen inmediatamente anterior a un símbolo."""
    escaped = re.escape(symbol)
    declaration = re.compile(
        rf"^(?:`{escaped}`|{escaped})\s*<-\s*(?:function\s*\(|[A-Za-z.])"
    )

    for source in sorted((package / "R").glob("*.R")):
        lines = source.read_text(encoding="utf-8").splitlines()
        for index, line in enumerate(lines):
            if not declaration.match(line):
                continue

            # Entre Roxygen y la declaración se toleran comentarios normales y
            # líneas vacías, pero nunca otra expresión de R.
            cursor = index - 1
            while cursor >= 0 and (
                not lines[cursor].strip()
                or (lines[cursor].lstrip().startswith("#")
                    and not lines[cursor].lstrip().startswith("#'"))
            ):
                cursor -= 1

            block = []
            while cursor >= 0 and lines[cursor].lstrip().startswith("#'"):
                block.append(lines[cursor].lstrip()[2:].lstrip())
                cursor -= 1
            return list(reversed(block)) or None
    return None


def main() -> int:
    failures: list[str] = []
    checked = 0

    for namespace in sorted(ROOT.glob("racafe*/NAMESPACE")):
        package = namespace.parent
        readme = (package / "README.md").read_text(encoding="utf-8")
        for symbol in exports(namespace):
            checked += 1
            label = f"{package.name}::{symbol}"
            if symbol not in readme:
                failures.append(f"{label}: no aparece en {package.name}/README.md")

            block = roxygen_block(package, symbol)
            if block is None:
                failures.append(f"{label}: no tiene bloque Roxygen")
                continue

            content = "\n".join(block)
            description = [
                line for line in block if line and not line.startswith("@")
            ]
            if not description:
                failures.append(f"{label}: no tiene título o descripción")
            if "@export" not in content:
                failures.append(f"{label}: no declara @export")
            if "@return" not in content and "@inherit " not in content:
                failures.append(f"{label}: no documenta el valor de retorno")

    if failures:
        print("Se encontraron problemas de documentación:", file=sys.stderr)
        for failure in failures:
            print(f"- {failure}", file=sys.stderr)
        return 1

    print(f"Documentación verificada: {checked} funciones exportadas.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
