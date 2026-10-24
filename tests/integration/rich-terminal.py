"""Exercise rich sizing against a real, resizable POSIX terminal."""
import errno
import fcntl
import os
from pathlib import Path
import pty
import re
import selectors
import struct
import subprocess
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parents[2]
END = b"\x1b]0;CSPP-END\x07"
ANSI = re.compile(rb"\x1b\[[0-?]*[ -/]*[@-~]")


def read_frame(fd):
    result = b""
    deadline = time.monotonic() + 10
    with selectors.DefaultSelector() as selector:
        selector.register(fd, selectors.EVENT_READ)
        while not result.endswith(END):
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not selector.select(remaining):
                raise AssertionError(f"terminal output timed out: {result!r}")
            try:
                data = os.read(fd, 8192)
            except OSError as error:
                if error.errno == errno.EIO:
                    data = b""
                else:
                    raise
            assert data, f"terminal closed early: {result!r}"
            result += data
    return result[:-len(END)]


def resize(fd, width):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, width, 0, 0))


def main():
    with tempfile.TemporaryDirectory(prefix="cspp-rich-terminal-") as directory:
        probe = Path(directory) / "probe.ss"
        probe.write_text('''(import (cspp))
(define console (make-rich-console))
(define rule (make-rich-rule))
(define progress (rich-progress :columns '(description bar percentage)))
(rich-progress-add-task! progress "build" 10)
(define live (make-rich-live progress console 0 #f))
(define table (rich-table :columns ("ID" "Payload")
                         :rows (("7" (make-string 120 #\\x)))))
(define panel (rich-panel :body (make-string 120 #\\z)))
(define mark
  (lambda () (display "\\033]0;CSPP-END\\007") (flush-output-port)))
(rich-print console rule "\\n" table "\\n" panel "\\n")
(rich-live-refresh! live)
(mark)
(get-line (current-input-port))
(rich-live-refresh! live)
(rich-print console "\\n" rule "\\n" table "\\n" panel)
(mark)
''')
        master, slave = pty.openpty()
        process = None
        try:
            resize(slave, 93)
            process = subprocess.Popen(
                [str(ROOT / "cspp"), "--script=" + str(probe)], cwd=ROOT,
                stdin=subprocess.PIPE, stdout=slave, stderr=subprocess.PIPE)
            initial = ANSI.sub(b"", read_frame(master)).split(b"\r\n")
            assert initial[0] == b"-" * 93, initial
            assert len(initial[-1]) == 93, initial
            assert all(len(line) <= 93 for line in initial), initial
            assert sum(line.count(b"x") for line in initial) == 120, initial
            assert sum(line.count(b"z") for line in initial) == 120, initial
            resize(slave, 47)
            process.stdin.write(b"\n")
            process.stdin.flush()
            updated = read_frame(master)
            # The 93-column old progress line reflows over two 47-column rows.
            assert b"\x1b[1A\r\x1b[2K" in updated, updated
            plain = ANSI.sub(b"", updated).split(b"\r\n")
            assert len(plain[0].replace(b"\r", b"")) == 47, plain
            assert plain[1] == b"-" * 47, plain
            assert all(len(line) <= 47 for line in plain[1:]), plain
            assert sum(line.count(b"x") for line in plain[2:]) == 120, plain
            assert sum(line.count(b"z") for line in plain[2:]) == 120, plain
            assert any(line.startswith(b"+") and len(line) == 47 for line in plain), plain
            _, error = process.communicate(timeout=10)
            assert process.returncode == 0 and not error, error
        finally:
            if process is not None:
                if process.poll() is None:
                    process.kill()
                process.communicate()
            os.close(master)
            os.close(slave)


def password_echo():
    # Hold the prompt's output callback open until input is already queued.
    # Echo must be disabled before that callback makes the prompt visible.
    with tempfile.TemporaryDirectory(prefix="cspp-rich-password-") as directory:
        release = Path(directory) / "release"
        os.mkfifo(release)
        probe = Path(directory) / "password.ss"
        probe.write_text('''(import (cspp))
(define destination (current-output-port))
(define first? #t)
(call-with-port
 (make-custom-textual-output-port "password-prompt"
  (lambda (text start count)
    (display (substring text start (+ start count)) destination)
    (flush-output-port destination)
    (when first?
      (set! first? #f)
      (call-with-input-file "RELEASE" get-line))
    count)
  #f #f #f)
 (lambda (output)
   (unless (string=? "secret" (rich-password (rich-console :output-port output) "Password"))
     (error 'password-test "queued password was lost"))
   (display "DONE" destination)
   (flush-output-port destination)))
'''.replace("RELEASE", str(release)))
        master, slave = pty.openpty()
        process = None
        try:
            original = termios.tcgetattr(slave)
            process = subprocess.Popen(
                [str(ROOT / "cspp"), "--script=" + str(probe)], cwd=ROOT,
                stdin=slave, stdout=slave, stderr=subprocess.PIPE)
            output = b""
            with selectors.DefaultSelector() as selector:
                selector.register(master, selectors.EVENT_READ)
                while b"Password: " not in output:
                    assert selector.select(10), output
                    output += os.read(master, 8192)
                assert not (termios.tcgetattr(slave)[3] & termios.ECHO), output
                os.write(master, b"secret\n")
                with release.open("w") as signal:
                    signal.write("continue\n")
                while b"DONE" not in output:
                    assert selector.select(10), output
                    output += os.read(master, 8192)
            _, error = process.communicate(timeout=10)
            assert process.returncode == 0 and not error, error
            assert b"secret" not in output, output
            assert termios.tcgetattr(slave) == original
        finally:
            if process is not None:
                if process.poll() is None:
                    process.kill()
                process.communicate()
            os.close(master)
            os.close(slave)


if __name__ == "__main__":
    main()
    password_echo()
