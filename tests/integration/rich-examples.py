"""Check the rich showcases' automatic sizing and redirected color policy."""
from concurrent.futures import ThreadPoolExecutor
import errno
import os
from pathlib import Path
import pty
import re
import selectors
import time
import subprocess
import tempfile
import unicodedata

from importlib.util import module_from_spec, spec_from_file_location

ROOT = Path(__file__).resolve().parents[2]
spec = spec_from_file_location("rich_terminal", Path(__file__).with_name("rich-terminal.py"))
terminal = module_from_spec(spec)
spec.loader.exec_module(terminal)
ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
SGR = re.compile(rb"\x1b\[[0-9;]*m")


def cell_width(text):
    # Independent fixture measurement: these familiar emoji occupy two cells each.
    for cluster in ("👩🏽‍💻", "🇨🇳", "❤️"):
        text = text.replace(cluster, "xx")
    return sum(0 if unicodedata.category(ch) in ("Mn", "Mc", "Me", "Cf")
               else 2 if unicodedata.east_asian_width(ch) in ("W", "F") else 1
               for ch in text)


def check(name, width):
    args = [str(ROOT / "cspp"), "--script=examples/" + name]
    if width is None:
        result = subprocess.run(args, cwd=ROOT, capture_output=True, timeout=30)
        assert result.returncode == 0 and not result.stderr, result.stderr
        output = result.stdout
        assert not SGR.search(output), f"{name}: SGR in redirected output"
        width = 80
    else:
        master, slave = pty.openpty()
        process = None
        try:
            terminal.resize(slave, width)
            # A file avoids stderr pipe deadlock and preserves errors for assertions.
            with tempfile.TemporaryFile() as errors:
                process = subprocess.Popen(args, cwd=ROOT, stdin=subprocess.DEVNULL,
                                           stdout=slave, stderr=errors)
                os.close(slave)
                slave = None
                output = b""
                deadline = time.monotonic() + 30
                with selectors.DefaultSelector() as selector:
                    selector.register(master, selectors.EVENT_READ)
                    while True:
                        remaining = deadline - time.monotonic()
                        assert remaining > 0 and selector.select(remaining), (
                            name, "terminal output timed out")
                        try:
                            data = os.read(master, 8192)
                        except OSError as error:
                            if error.errno == errno.EIO:
                                break
                            raise
                        if not data:
                            break
                        output += data
                assert process.wait(timeout=30) == 0, name
                errors.seek(0)
                assert not errors.read(), name
        finally:
            if process is not None and process.poll() is None:
                process.kill()
                process.wait()
            os.close(master)
            if slave is not None:
                os.close(slave)
    decoded = output.decode()
    export_label = "ansi value" if name == "rich.ss" else "ansi export"
    export_rows = [line for line in decoded.splitlines() if export_label in line]
    assert len(export_rows) == 1, (name, export_rows)
    assert not re.search(r'\\(?:x1[bB];|033|e)', export_rows[0]), (name, export_rows)
    if SGR.search(output):
        assert SGR.search(export_rows[0].encode()), (name, "unstyled ANSI preview")
    lines = ANSI.sub("", decoded).replace("\r", "\n").splitlines()
    title = "Styles, Themes, Text" if name == "rich.ss" else "Styled Signals"
    rules = [line for line in lines if title in line]
    progress = [line for line in lines if re.search(r"\[[#-]*\]", line)]
    assert len(rules) == 1 and cell_width(rules[0]) == width, (name, width, rules)
    assert progress and all(cell_width(line) == width for line in progress), (name, width, progress)
    unicode_rules = [line for line in lines if "Unicode Cells / 中文" in line]
    assert len(unicode_rules) == 1 and cell_width(unicode_rules[0]) == width, (
        name, width, unicode_rules)
    for sample in ("中文", "café", "👩🏽‍💻", "🇨🇳", "❤️"):
        assert sample in decoded, (name, "missing Unicode sample", sample)
    sample_rows = [line for line in lines if "│" in line
                   and any(sample in line for sample in ("中文", "café", "👩🏽‍💻", "🇨🇳", "❤️"))]
    assert len(sample_rows) == 5 and {cell_width(line) for line in sample_rows} == {16}, (
        name, sample_rows)



if __name__ == "__main__":
    with ThreadPoolExecutor(max_workers=4) as pool:
        list(pool.map(lambda pair: check(*pair),
                      [(name, width) for name in ("rich.ss", "rich-dashboard.ss")
                       for width in (None, 40, 120)]))
