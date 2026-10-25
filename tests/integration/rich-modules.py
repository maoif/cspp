"""Exercise rich module composition without loading the CSPP or rich facades."""
import json
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
SCHEME = ROOT / ".chezscheme-install" / "bin" / "scheme"

CASES = (
    ("panel", "rich-panel-render", "(make-rich-panel text)"),
    ("table", "rich-table-render", """(let ([table (make-rich-table)])
      (rich-table-add-column! table "A")
      (rich-table-add-row! table text)
      table)"""),
    ("tree", "rich-tree-render", "(make-rich-tree text)"),
    ("basic", "rich-padding-render", "(make-rich-padding text)"),
)


def main():
    with tempfile.TemporaryDirectory(prefix="cspp-rich-modules-") as directory:
        for module, render, value in CASES:
            # A fresh interpreter prevents earlier facade/console use from registering text.
            source = f'''(import (chezscheme))
(compile-imported-libraries #f)
(library-directories (list {json.dumps(str(ROOT / "src"), ensure_ascii=False)}))
(import (cspp rich text) (cspp rich segment) (cspp rich style)
        (cspp rich renderable) (cspp rich {module}))
(define style (rich-style 'red))
(define text (rich-text "hello" style))
(unless (rich-renderable? text)
  (error 'rich-modules "rich text depends on console initialization"))
(define lines ({render} {value}))
(define styled (filter (lambda (segment) (eq? style (rich-segment-style segment)))
                       (apply append lines)))
(unless (string=? "hello" (apply string-append (map rich-segment-text styled)))
  (error 'rich-modules "container did not preserve rich text and its style"))
'''
            probe = Path(directory) / (module + ".ss")
            probe.write_text(source)
            result = subprocess.run([str(SCHEME), "--script", str(probe)], cwd=ROOT,
                                    capture_output=True, timeout=10)
            assert result.returncode == 0 and not result.stdout and not result.stderr, result


if __name__ == "__main__":
    main()
