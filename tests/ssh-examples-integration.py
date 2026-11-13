"""Loopback SSH CLI integration; run --password with optional Paramiko installed.

The default run needs sshd, ssh-keygen, ssh-agent, ssh-add and a built cspp.
All host keys, authorized keys and known-hosts files belong to a temporary directory.
"""

import sys, os
sys.path = [path for path in sys.path if path != os.path.dirname(__file__)]
import contextlib, pathlib, pwd, select, shutil, socket, stat, subprocess, tempfile, time


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


@contextlib.contextmanager
def fixture(log_sessions=False, shell_command=None):
    with tempfile.TemporaryDirectory(prefix="cspp-ssh-examples-") as d:
        root = pathlib.Path(d)
        home = root / "home"
        ssh = home / ".ssh"
        ssh.mkdir(parents=True)
        key = ssh / "id_ed25519"
        host = root / "host"
        port = free_port()
        user = pwd.getpwuid(os.getuid()).pw_name
        for path in (key, host):
            subprocess.run(
                ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(path)],
                check=True,
                timeout=20,
            )
        (ssh / "known_hosts").write_text(
            f"[127.0.0.1]:{port} " + host.with_suffix(".pub").read_text()
        )
        config = root / "sshd_config"
        config.write_text(
            f"Port {port}\nListenAddress 127.0.0.1\nHostKey {host}\nPidFile {root}/sshd.pid\nAuthorizedKeysFile {key}.pub\nPasswordAuthentication no\nKbdInteractiveAuthentication no\nUsePAM no\nStrictModes no\nAllowUsers {user}\nSubsystem sftp internal-sftp\nLogLevel ERROR\n"
        )
        if log_sessions:
            config.write_text(config.read_text().replace("LogLevel ERROR", "LogLevel VERBOSE"))
        if shell_command:
            config.write_text(config.read_text() + f"ForceCommand {shell_command}\n")
        server = subprocess.Popen(
            [shutil.which("sshd") or "/usr/sbin/sshd", "-D", "-E", str(root / "sshd.log"), "-f", str(config)],
            stderr=subprocess.PIPE,
        )
        try:
            for _ in range(100):
                try:
                    with socket.create_connection(("127.0.0.1", port), 0.1):
                        break
                except OSError:
                    time.sleep(0.03)
            else:
                raise RuntimeError(
                    f"local sshd did not become ready (status {server.poll()})"
                )
            env = os.environ.copy()
            env["HOME"] = str(home)
            args = [
                "127.0.0.1",
                f"--port={port}",
            ]
            yield root, env, args
        finally:
            server.terminate()
            server.wait(timeout=5)


def run(script, env, args, *extra, input=None):
    return subprocess.run(
        ["./cspp", "--script=examples/net/" + script + ".ss", "--", *args, *extra],
        env=env,
        input=input,
        capture_output=True,
        timeout=20,
    )


def wait_for_log(process, fragment, timeout=10):
    """Wait for a readiness log without connecting to and consuming a relay channel."""
    deadline = time.monotonic() + timeout
    captured = bytearray()
    while fragment not in captured:
        remaining = deadline - time.monotonic()
        assert remaining > 0, f"readiness timeout: {bytes(captured)!r}"
        ready, _, _ = select.select([process.stderr], [], [], remaining)
        assert ready, f"readiness timeout: {bytes(captured)!r}"
        data = os.read(process.stderr.fileno(), 4096)
        assert data, f"process exited before readiness: {bytes(captured)!r}"
        captured.extend(data)


def verify():
    verify_scp_batch_timeouts()
    with fixture() as (root, env, args):
        # Defaults must use the local account, even if login environment names disagree.
        env["USER"] = "cspp-not-the-local-account"
        env["LOGNAME"] = "cspp-not-the-local-account"
        env.pop("SSH_AUTH_SOCK", None)
        p = run(
            "ssh-run-cmd",
            env,
            args,
            "--command=printf stdout; printf stderr >&2; exit 7",
        )
        assert p.returncode == 7 and p.stdout == b"stdout" and b"stderr" in p.stderr, p
        p = run("ssh-run-cmd", env, args, "--command=cat; printf eof-observed")
        assert p.returncode == 0 and p.stdout == b"eof-observed", p
        # Both output streams exceed typical SSH windows; drain them concurrently.
        p = run(
            "ssh-run-cmd",
            env,
            args,
            '--command=python3 -c "import os; os.write(1,bytes([97])*262144); os.write(2,bytes([98])*262144)"',
        )
        assert (
            p.returncode == 0
            and p.stdout == b"a" * 262144
            and b"b" * 262144 in p.stderr
        ), p.stderr
        # Options can follow HOST; command words after -- must remain literal.
        p = run("ssh-run-cmd", env, args, "--command=printf", "--", "positional")
        assert p.returncode == 0 and p.stdout == b"positional", p
        # HOST also works after options; an explicit user still overrides the default.
        p = run("ssh-run-cmd", env, [args[1], f"--user={pwd.getpwuid(os.getuid()).pw_name}"],
                "--command=printf host-last", args[0])
        assert p.returncode == 0 and p.stdout == b"host-last", p
        p = run("ssh-run-cmd", env, args, "--", "printf", "%s", "--literal")
        assert p.returncode == 0 and p.stdout == b"--literal", p
        keyargs = [x for x in args if x != "--auth=publickey"] + [
            "--auth=publickey",
            f"--private-key={root}/home/.ssh/id_ed25519",
        ]
        p = run("ssh-run-cmd", env, keyargs, "--command=printf explicit-key")
        assert p.returncode == 0 and p.stdout == b"explicit-key", p
        with tempfile.TemporaryDirectory(prefix="cspp-agent-") as agentdir:
            sock = str(pathlib.Path(agentdir) / "agent.sock")
            agent = subprocess.Popen(
                ["ssh-agent", "-D", "-a", sock],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            try:
                for _ in range(100):
                    if pathlib.Path(sock).exists():
                        break
                    time.sleep(0.01)
                agentenv = env.copy()
                agentenv["SSH_AUTH_SOCK"] = sock
                subprocess.run(
                    ["ssh-add", str(root / "home/.ssh/id_ed25519")],
                    env=agentenv,
                    check=True,
                    timeout=20,
                    capture_output=True,
                )
                p = run(
                    "ssh-run-cmd",
                    agentenv,
                    [*args, "--auth=agent"],
                    "--command=printf agent",
                )
                assert p.returncode == 0 and p.stdout == b"agent", p
            finally:
                agent.terminate()
                agent.wait(timeout=5)
        p = run("ssh-open-repl", env, args, "--pty=no", input=b"printf shell-eof\n")
        assert p.returncode == 0 and b"shell-eof" in p.stdout, p
        # Exercise a real local terminal as well as the pipe-based command checks.
        p = subprocess.run([sys.executable, "tests/ssh-shell-integration.py"],
                           capture_output=True, timeout=45)
        assert p.returncode == 0, p.stderr
        verify_scp_callback_cancellation(root, env, args)
        verify_scp_filter_cancellation(root, env, args)
        verify_scp_directories(root, env, args)
        verify_scp_symlinks(root, env, args)
        verify_scp_readonly_files(root, env, args)
        verify_scp_channel_reuse()
        source = root / "source"
        source.write_bytes(bytes(range(256)) * 1024)
        remote = root / "remote"
        destination = root / "destination"
        for direction in ("upload", "download"):
            p = run(
                "scp",
                env,
                keyargs,
                f"--direction={direction}",
                "--kind=file",
                f'--local={source if direction=="upload" else destination}',
                f"--remote={remote}",
            )
            assert p.returncode == 0, p.stderr
            assert b"100%" in p.stderr and b"262144/262144 bytes" in p.stderr, p.stderr
            assert not p.stdout, p.stdout
        assert source.read_bytes() == destination.read_bytes()
        # Error case: existing destinations must not be overwritten by default.
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
        )
        assert p.returncode != 0, p
        # Error case: invalid progress values fail before connecting.
        p = run("scp", env, args, "--direction=upload", "--kind=file",
                f"--local={source}", f"--remote={remote}", "--progress=maybe")
        assert p.returncode != 0 and b"--progress" in p.stderr, p
        source.write_bytes(b"replaced")
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
            "--overwrite=skip",
        )
        assert p.returncode == 0 and remote.read_bytes() != b"replaced", p
        assert b"bytes" not in p.stderr, p.stderr
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
            "--overwrite=replace",
            "--progress=no",
        )
        assert p.returncode == 0 and remote.read_bytes() == b"replaced", p
        assert b"bytes" not in p.stderr and b"100%" not in p.stderr, p.stderr
        tree = root / "tree"
        (tree / "nested").mkdir(parents=True)
        (tree / "nested/data").write_bytes(b"tree data")
        (tree / "empty").mkdir()
        (tree / "zero").touch()
        remote_tree = root / "remote-tree"
        copy = root / "copy"
        for direction in ("upload", "download"):
            p = run(
                "scp",
                env,
                args,
                f"--direction={direction}",
                "--kind=directory",
                f'--local={tree if direction=="upload" else copy}',
                f"--remote={remote_tree}",
            )
            assert p.returncode == 0, p.stderr
            assert b"9/9 bytes" in p.stderr and b"0/0 bytes" in p.stderr, p.stderr
        assert (copy / "nested/data").read_bytes() == b"tree data" and (
            copy / "empty"
        ).is_dir()
        (tree / "link").symlink_to("nested/data")
        # Error case: symlinks inside directory trees are rejected.
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=directory",
            f"--local={tree}",
            f"--remote={remote_tree}",
            "--overwrite=replace",
        )
        assert p.returncode != 0 and b"symbolic links" in p.stderr, p
        remote_link = root / "remote-link"
        remote_link.symlink_to("remote")
        # Error case: an existing remote source symlink must not be followed by file SCP.
        p = run(
            "scp",
            env,
            args,
            "--direction=download",
            "--kind=file",
            f"--local={root / 'link-output'}",
            f"--remote={remote_link}",
        )
        assert p.returncode != 0 and b"symlinks are rejected" in p.stderr, p
        verify_scp_path_links(root, env, args)
        verify_scp_interruption(root, env, args)
        verify_scp_interruption(root, env, args, "directory")
        verify_forwarding(env, args)
        verify_forwarding_recovery(env, args)
        verify_forwarding_cancellation(env, args)
        verify_forwarding_blocked_connect(env, args)
        verify_forwarding_open_lifecycle(root, env, args)
        verify_forwarding_connect_timeout(env, args)
        # Explicit encrypted private keys use a separate passphrase environment variable.
        key = root / "home/.ssh/id_ed25519"
        subprocess.run(
            [
                "ssh-keygen",
                "-q",
                "-p",
                "-P",
                "",
                "-N",
                "fixture-passphrase",
                "-f",
                str(key),
            ],
            check=True,
            timeout=20,
            capture_output=True,
        )
        keyenv = env.copy()
        keyenv["CSPP_SSH_PASSPHRASE"] = "fixture-passphrase"
        p = run("ssh-run-cmd", keyenv, keyargs, "--command=printf encrypted-key")
        assert p.returncode == 0 and p.stdout == b"encrypted-key", p
        for script in ("ssh-run-cmd", "ssh-open-repl", "ssh-port-forward", "scp"):
            for flag in ("-h", "--help"):
                p = run(script, env, [], flag)
                assert p.returncode == 0 and p.stdout, p
            # Error case: unrecognized options must fail before opening network resources.
            p = run(script, env, [], "--unknown-option=x")
            assert p.returncode != 0, p
        # Error case: SCP restart is rejected instead of silently starting over.
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
            "--resume=resume",
        )
        assert p.returncode != 0 and b"unsupported" in p.stderr, p
        # Error case: untrusted host keys must fail before authentication.
        badenv = env.copy()
        badenv["HOME"] = str(root / "untrusted")
        pathlib.Path(badenv["HOME"]).mkdir()
        p = run("ssh-run-cmd", badenv, args, "--command=true")
        assert p.returncode != 0 and b"host key is not trusted" in p.stderr, p


def verify_scp_batch_timeouts():
    # Error case: an SCP peer accepts exec but never sends its protocol acknowledgement.
    # SFTP stays responsive so the copy reaches the batched transfer, not an earlier stat.
    sftp_server = shutil.which('sftp-server') or next(
        (path for path in ('/usr/lib/ssh/sftp-server', '/usr/lib/openssh/sftp-server',
                           '/usr/libexec/sftp-server', '/usr/libexec/openssh/sftp-server')
         if pathlib.Path(path).is_file()), None)
    assert sftp_server, 'SCP timeout fixture requires the OpenSSH sftp-server executable'
    with tempfile.TemporaryDirectory(prefix='cspp-scp-timeout-') as directory:
        peer = pathlib.Path(directory) / 'peer.py'
        peer.write_text(f'''import os, sys
command = os.environ['SSH_ORIGINAL_COMMAND']
if command == 'internal-sftp':
    os.execv({sftp_server!r}, [{sftp_server!r}])
elif command.startswith('scp '):
    # Wait for client EOF without replying; cancellation must release this channel.
    sys.stdin.buffer.read()
else:
    os.execv('/bin/sh', ['sh', '-c', command])
''')
        with fixture(shell_command=f'{sys.executable} {peer}') as (root, env, args):
            user = pwd.getpwuid(os.getuid()).pw_name
            port = args[1].split('=')[1]
            for direction in ('upload', 'download'):
                source, target = root / f'{direction}-source', root / f'{direction}-target'
                source.mkdir()
                (source / 'file').write_bytes(b'payload')
                script = root / 'batch-timeout.ss'
                script.write_text(f'''(import (cspp) (cspp transducer) (cspp path) (cspp system) (cspp array) (cspp net))
(call-with-ssh-session "127.0.0.1" {port} "{user}"
 (lambda (ssh)
  (ssh-auth-publickey! ssh "{user}")
  (call-with-scp-session ssh
   (lambda (scp)
    (let* ([progress? #f]
           [policy (make-transfer-policy 'never 'replace 65536
                     (lambda arguments (set! progress? #t)))]
           [operation (scp-copy-directory/nonblocking scp '{direction}
                        "{source}" "{target}" 100 policy #f 'reject)])
     (guard (failure
             [(and (net-error? failure) (eq? (net-error-kind failure) 'timeout)) (void)]
             [else (raise failure)])
      (net-operation-wait operation)
      (error 'test "stalled SCP batch unexpectedly completed"))
     (unless (and progress? (eq? (net-operation-state operation) 'failed)
                  (scp-attributes? (scp-stat scp "{source}/file")))
      (error 'test "timeout did not release the batch and preserve its session")))))))
''')
                result = subprocess.run(['./cspp', f'--script={script}', '--'], env=env,
                                        capture_output=True, timeout=5)
                assert result.returncode == 0, (direction, result.stderr)
                if direction == 'download':
                    assert not (target / 'file').exists(), 'timed-out partial download remains'
                assert (source / 'file').read_bytes() == b'payload'


def verify_scp_callback_cancellation(root, env, args):
    user = pwd.getpwuid(os.getuid()).pw_name
    port = args[1].split('=')[1]
    for direction in ('upload', 'download'):
        for action in ('cancel', 'close', 'operation', 'close-owned'):
            source = root / f'callback-source-{direction}-{action}'
            target = root / f'callback-target-{direction}-{action}'
            source.mkdir()
            for name in ('a', 'b', 'c'):
                (source / name).write_bytes(b'x' * (4 * 1024 * 1024))
            script = root / 'callback-cancel.ss'
            cancel = {'cancel': '(scp-cancel-pending! scp)', 'close': '(scp-close scp)',
                      'operation': '(net-operation-cancel! operation)',
                      'close-owned': '(scp-close scp)'}[action]
            session_arg = (f"\"127.0.0.1\" {port} \"{user}\" 'publickey #f"
                           if action == 'close-owned' else 'ssh')
            script.write_text(f'''(import (cspp) (cspp transducer) (cspp path) (cspp system) (cspp array) (cspp net))
(call-with-ssh-session "127.0.0.1" {port} "{user}"
 (lambda (ssh)
  (ssh-auth-publickey! ssh "{user}")
  (call-with-scp-session {session_arg}
   (lambda (scp)
    (let* ([called? #f] [operation #f]
           [policy (make-transfer-policy 'never 'replace 65536
             (lambda (protocol direction path completed total)
              (unless called?
               (set! called? #t)
               {cancel})))])
     (set! operation (scp-copy-directory/nonblocking scp '{direction}
                      "{source}" "{target}" 30000 policy #f 'reject))
     (guard (failure [else (void)]) (net-operation-wait operation))
     (unless (and called? (eq? (net-operation-state operation) 'cancelled))
      (error 'test "copy ignored cancellation" (net-operation-state operation))))))))
''')
            result = subprocess.run(['./cspp', f'--script={script}', '--'], env=env,
                                    capture_output=True, timeout=15)
            assert result.returncode == 0, (direction, action, result.stderr)
            # Error case: cancellation must stop the current batch before later files.
            assert len(list(target.iterdir())) < 3, (direction, action)


def verify_scp_filter_cancellation(root, env, args):
    user = pwd.getpwuid(os.getuid()).pw_name
    port = args[1].split('=')[1]
    for direction in ('upload', 'download'):
        for action in ('cancel', 'close', 'operation', 'close-owned'):
            for selected in ('#t', '#f'):
                suffix = f'{direction}-{action}-{selected[-1]}'
                source = root / f'filter-source-{suffix}'
                target = root / f'filter-target-{suffix}'
                source.mkdir()
                target.mkdir()
                for name in ('a', 'b', 'c'):
                    (source / name).write_bytes(b'new contents')
                    (target / name).write_bytes(b'keep original')
                script = root / 'filter-cancel.ss'
                cancel = {'cancel': '(scp-cancel-pending! scp)', 'close': '(scp-close scp)',
                          'operation': '(net-operation-cancel! operation)',
                          'close-owned': '(scp-close scp)'}[action]
                session_arg = (f"\"127.0.0.1\" {port} \"{user}\" 'publickey #f"
                               if action == 'close-owned' else 'ssh')
                script.write_text(f'''(import (cspp) (cspp transducer) (cspp path) (cspp system) (cspp array) (cspp net))
(call-with-ssh-session "127.0.0.1" {port} "{user}"
 (lambda (ssh)
  (ssh-auth-publickey! ssh "{user}")
  (call-with-scp-session {session_arg}
   (lambda (scp)
    (let ([called? #f] [operation #f])
     (set! operation
      (scp-copy-directory/nonblocking scp '{direction} "{source}" "{target}" 30000
       (make-transfer-policy 'never 'replace 65536 #f)
       (lambda (path attributes)
        (unless called? (set! called? #t) {cancel})
        {selected})
       'reject))
     (guard (failure [else (void)]) (net-operation-wait operation))
     (unless (and called? (eq? (net-operation-state operation) 'cancelled))
      (error 'test "filter cancellation did not survive" (net-operation-state operation))))))))
''')
                result = subprocess.run(['./cspp', f'--script={script}', '--'], env=env,
                                        capture_output=True, timeout=15)
                assert result.returncode == 0, (suffix, result.stderr)
                # Error case: cancelling inside a filter must prevent all later writes,
                # even when the filter returns false and there is no active child yet.
                assert sorted(p.name for p in target.iterdir()) == ['a', 'b', 'c']
                assert all(p.read_bytes() == b'keep original' for p in target.iterdir()), suffix


def verify_scp_directories(root, env, args):
    # Regressions: UTF-8 names survive SFTP enumeration; nested/empty trees and
    # relative paths with trailing slashes work with either progress setting.
    source = root / "directory-source"
    (source / "nested" / "deeper" / "empty").mkdir(parents=True)
    files = {"root.txt": b"root", "nested/spaces and 'quotes'.txt": b"quoted",
             "nested/你好.txt": bytes(range(256)) * 257,
             "nested/deeper/zero": b"", ".hidden": b"hidden"}
    for name, data in files.items():
        (source / name).write_bytes(data)
    relative = os.path.relpath(source)
    for progress in ("yes", "no"):
        remote = root / ("directory-remote-" + progress)
        copied = root / ("directory-copy-" + progress)
        for direction in ("upload", "download"):
            local = relative + "/" if direction == "upload" else str(copied) + "/"
            result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                         f"--local={local}", f"--remote={remote}/", f"--progress={progress}")
            assert result.returncode == 0, (direction, progress, result.stderr)
            output = remote if direction == "upload" else copied
            for name, data in files.items():
                assert (output / name).read_bytes() == data, (direction, progress, name)
            assert (output / "nested/deeper/empty").is_dir()
    # Remote paths are relative to the SSH login directory, including overwrite checks.
    home = pathlib.Path(pwd.getpwuid(os.getuid()).pw_dir)
    with tempfile.TemporaryDirectory(prefix=".cspp-scp-relative-", dir=home) as directory:
        remote = pathlib.Path(directory) / "tree"
        relative_remote = str(remote.relative_to(home))
        copied = root / "relative-remote-copy"
        for direction in ("upload", "download"):
            result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                         f"--local={source if direction == 'upload' else copied}",
                         f"--remote={relative_remote}")
            assert result.returncode == 0, (direction, result.stderr)
        for name, data in files.items():
            assert (copied / name).read_bytes() == data, name
        (remote / "root.txt").write_bytes(b"keep existing")
        for policy in ("skip", "error"):
            result = run("scp", env, args, "--direction=upload", "--kind=directory",
                         f"--local={source}", f"--remote={relative_remote}",
                         f"--overwrite={policy}")
            assert (result.returncode == 0) == (policy == "skip"), (policy, result.stderr)
            assert (remote / "root.txt").read_bytes() == b"keep existing", policy
        # Error case: resolving the login directory must not hide symlinks before '..'.
        link = pathlib.Path(directory) / "link"
        link.symlink_to(remote, target_is_directory=True)
        result = run("scp", env, args, "--direction=upload", "--kind=directory",
                     f"--local={source}",
                     f"--remote={link.relative_to(home)}/../escaped", "--overwrite=replace")
        assert result.returncode != 0 and b"symlinks are rejected" in result.stderr, result
        assert not (pathlib.Path(directory) / "escaped").exists()
    # A missing source must fail rather than leaving a successful empty copy.
    result = run("scp", env, args, "--direction=download", "--kind=directory",
                 f"--local={root / 'missing-copy'}", f"--remote={root / 'missing-source'}")
    assert result.returncode != 0, result


def verify_scp_symlinks(root, env, args):
    # Preserve literal link targets, including dangling links, without traversing them.
    source = root / "links-source"
    (source / "nested").mkdir(parents=True)
    (source / "nested/file").write_bytes(b"original data")
    links = {"COPYING": "nested/file", "directory-link": "nested",
             "dangling": "missing", "nested/你好": "../missing"}
    for name, target in links.items():
        (source / name).symlink_to(target)
    remote, copied = root / "links-remote", root / "links-copy"
    for direction in ("upload", "download"):
        local = source if direction == "upload" else copied
        destination = remote if direction == "upload" else copied
        for overwrite in ("error", "skip", "replace"):
            if overwrite == "skip":
                (destination / "dangling").unlink()
                (destination / "dangling").symlink_to("keep missing target")
            result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                         f"--local={local}", f"--remote={remote}",
                         "--symlinks=preserve", f"--overwrite={overwrite}")
            assert result.returncode == 0, (direction, overwrite, result.stderr)
            for name, target in links.items():
                if overwrite == "skip" and name == "dangling":
                    target = "keep missing target"
                assert (destination / name).is_symlink(), (direction, name)
                assert os.readlink(destination / name) == target, (direction, name)
            assert (destination / "nested/file").read_bytes() == b"original data"
        # Error case: a pre-existing link is an existing destination, even if dangling.
        result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                     f"--local={local}", f"--remote={remote}", "--symlinks=preserve")
        assert result.returncode != 0, result
        # Error regression: replacing a leaf link to a directory must not write into its target.
        leaf_outside = root / ("leaf-outside-" + direction)
        leaf_outside.mkdir()
        (leaf_outside / "file").write_bytes(b"untouched leaf target")
        (destination / "COPYING").unlink()
        (destination / "COPYING").symlink_to(leaf_outside, target_is_directory=True)
        result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                     f"--local={local}", f"--remote={remote}",
                     "--symlinks=preserve", "--overwrite=replace")
        assert result.returncode == 0, result.stderr
        assert os.readlink(destination / "COPYING") == "nested/file", direction
        assert (leaf_outside / "file").read_bytes() == b"untouched leaf target"
        assert sorted(p.name for p in leaf_outside.iterdir()) == ["file"]
        # Error case: preserving source links must not follow a destination directory link.
        saved = root / ("saved-nested-" + direction)
        (destination / "nested").rename(saved)
        outside = root / ("outside-" + direction)
        outside.mkdir()
        (outside / "file").write_bytes(b"untouched")
        (destination / "nested").symlink_to(outside, target_is_directory=True)
        result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                     f"--local={local}", f"--remote={remote}",
                     "--symlinks=preserve", "--overwrite=replace")
        assert result.returncode != 0 and b"symlinks are rejected" in result.stderr, result
        assert (outside / "file").read_bytes() == b"untouched"
        (destination / "nested").unlink()
        saved.rename(destination / "nested")
        # Error case: source links still require explicit opt-in with the default policy.
        result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                     f"--local={source if direction == 'upload' else root / 'links-rejected'}",
                     f"--remote={root / 'links-reject-remote' if direction == 'upload' else remote}")
        assert result.returncode != 0 and b"reject" in result.stderr, result


def verify_scp_readonly_files(root, env, args):
    # Read-only Git objects must remain copyable on repeated replace transfers.
    source, remote, copied = (root / name for name in ("readonly-source", "readonly-remote",
                                                       "readonly-copy"))
    source.mkdir()
    (source / "object").write_bytes(b"read-only object")
    (source / "object").chmod(0o444)
    for direction in ("upload", "download"):
        local = source if direction == "upload" else copied
        destination = (remote if direction == "upload" else copied) / "object"
        for attempt in range(2):
            if attempt:
                destination.chmod(0o644)
                destination.write_bytes(b"old destination")
                destination.chmod(0o444)
                saved = root / ("readonly-hardlink-" + direction)
                os.link(destination, saved)
                for policy in ("skip", "error"):
                    # Error case: read-only destinations still obey overwrite selection.
                    result = run("scp", env, args, f"--direction={direction}",
                                 "--kind=directory", f"--local={local}", f"--remote={remote}",
                                 f"--overwrite={policy}")
                    assert (result.returncode == 0) == (policy == "skip"), result.stderr
                    assert destination.read_bytes() == b"old destination"
            result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                         f"--local={local}",
                         f"--remote={remote}", "--overwrite=replace")
            assert result.returncode == 0, (direction, attempt, result.stderr)
            assert destination.read_bytes() == b"read-only object"
            if attempt:
                assert saved.read_bytes() == b"old destination"
                assert stat.S_IMODE(saved.stat().st_mode) == 0o444
            destination.chmod(0o444)
    verify_scp_readonly_namespace(root, env, args)


def verify_scp_readonly_namespace(root, env, args):
    # Error case: raw SCP paths must not delete files in SFTP's normalized namespace.
    alias, physical, source = (root / name for name in ("alias", "physical", "raw-source"))
    (alias / "dest").mkdir(parents=True)
    (physical / "child").mkdir(parents=True)
    (physical / "dest").mkdir()
    source.mkdir()
    (source / "object").write_bytes(b"replacement")
    unrelated, target = alias / "dest/object", physical / "dest/object"
    unrelated.write_bytes(b"unrelated normalized path")
    target.write_bytes(b"existing read-only target")
    target.chmod(0o444)
    (alias / "link").symlink_to(physical / "child", target_is_directory=True)
    script = root / "raw-scp-path.ss"
    port = int(args[1].split("=", 1)[1])
    user = pwd.getpwuid(os.getuid()).pw_name
    script.write_text(f'''(import (cspp) (cspp transducer) (cspp path) (cspp system) (cspp array) (cspp net))
(let ([session (ssh-open "127.0.0.1" {port} "{user}")])
  (dynamic-wind
    void
    (lambda ()
      (ssh-auth-publickey! session "{user}")
      (call-with-scp-session session
        (lambda (scp)
          (scp-copy-directory scp 'upload "{source}" "{alias}/link/../dest"
            (make-transfer-policy 'never 'replace 65536 #f) #f 'reject))))
    (lambda () (ssh-close session))))
''')
    result = subprocess.run(["./cspp", f"--script={script}", "--"], env=env,
                            capture_output=True, timeout=20)
    assert result.returncode != 0, result
    assert unrelated.read_bytes() == b"unrelated normalized path"
    assert target.read_bytes() == b"existing read-only target"


def verify_scp_channel_reuse():
    # Real server sessions expose per-file setup overhead without timing assertions.
    with fixture(log_sessions=True) as (root, env, args):
        source = root / "many-files"
        (source / "nested").mkdir(parents=True)
        files = {f"file-{i:02d}": bytes([i]) * (4097 + i) for i in range(70)}
        files.update({"nested/你好 'quoted'": b"unicode", "empty": b"",
                      " leading space": b"space", "-dash": b"dash"})
        for name, data in files.items():
            (source / name).write_bytes(data)
        remote, copied = root / "many-remote", root / "many-copy"
        def session_count():
            return (root / "sshd.log").read_text().count("Starting session:")
        for direction in ("upload", "download"):
            before = session_count()
            result = run("scp", env, args, f"--direction={direction}", "--kind=directory",
                         f"--local={source if direction == 'upload' else copied}",
                         f"--remote={remote}")
            assert result.returncode == 0, result.stderr
            output = remote if direction == "upload" else copied
            for name, data in files.items():
                assert (output / name).read_bytes() == data, (direction, name)
            commands = session_count() - before
            assert commands <= 9, (direction, "per-file SSH session setup", commands)
            assert b"100%" in result.stderr, result.stderr
            for name in files:
                assert str(remote / name).encode() in result.stderr, name
        # Existing selected files retain skip/error semantics across a batch boundary.
        (remote / "file-00").write_bytes(b"keep remote")
        result = run("scp", env, args, "--direction=upload", "--kind=directory",
                     f"--local={source}", f"--remote={remote}", "--overwrite=skip")
        assert result.returncode == 0 and (remote / "file-00").read_bytes() == b"keep remote", result
        assert b"bytes" not in result.stderr, result.stderr
        result = run("scp", env, args, "--direction=upload", "--kind=directory",
                     f"--local={source}", f"--remote={remote}", "--overwrite=error")
        assert result.returncode != 0 and (remote / "file-00").read_bytes() == b"keep remote", result


def verify_scp_path_links(root, env, args):
    for direction in ("upload", "download"):
        for kind in ("file", "directory"):
            for scenario in ("source-parent", "source-dot-parent", "target-parent",
                             "target-missing-parent", "target-root", "target-child",
                             "target-dangling-child", "target-directory"):
                if kind == "file" and scenario in ("target-child", "target-dangling-child", "target-directory"):
                    continue
                case = root / f"links-{direction}-{kind}-{scenario}"
                source, target, outside = case / "source", case / "target", case / "outside"
                (source / "nested").mkdir(parents=True)
                (source / "nested/data").write_bytes(b"new content")
                outside.mkdir()
                (outside / "data").write_bytes(b"unrelated content")
                source_path = source / "nested/data" if kind == "file" else source
                target_path = target
                if scenario == "source-parent":
                    (case / "source-link").symlink_to(source, target_is_directory=True)
                    source_path = case / "source-link" / ("nested/data" if kind == "file" else "nested")
                elif scenario == "source-dot-parent":
                    (case / "source-link").symlink_to(source / "nested", target_is_directory=True)
                    source_path = case / "source-link/.." / ("nested/data" if kind == "file" else "nested")
                elif scenario in ("target-parent", "target-missing-parent"):
                    target.symlink_to(outside, target_is_directory=True)
                    target_path = target / ("data" if kind == "file" else "new-tree")
                    if scenario == "target-missing-parent":
                        target_path = case / "missing/../target" / target_path.name
                elif scenario == "target-root":
                    target.symlink_to(outside / "data" if kind == "file" else outside,
                                      target_is_directory=(kind == "directory"))
                elif scenario in ("target-child", "target-dangling-child"):
                    (target / "nested").mkdir(parents=True)
                    (target / "nested/data").symlink_to(
                        outside / ("data" if scenario == "target-child" else "missing"))
                else:
                    target.mkdir()
                    (target / "nested").symlink_to(outside, target_is_directory=True)
                local, remote = ((source_path, target_path) if direction == "upload"
                                 else (target_path, source_path))
                # Error case: symlinked ancestors and destination entries must never be followed.
                p = run("scp", env, args, f"--direction={direction}", f"--kind={kind}",
                        f"--local={local}", f"--remote={remote}", "--overwrite=replace")
                assert p.returncode != 0, (direction, kind, scenario, p)
                assert (outside / "data").read_bytes() == b"unrelated content", scenario
                assert not (outside / "new-tree").exists(), scenario
                assert not (outside / "missing").exists(), scenario


def verify_forwarding_recovery(env, args):
    import threading

    for direction in ("local", "remote"):
        with socket.socket() as target:
            target.bind(("127.0.0.1", 0))
            port = free_port()
            process = subprocess.Popen(
                ["./cspp", "--script=examples/net/ssh-port-forward.ss", "--", *args,
                 f"--direction={direction}", f"--listen-port={port}",
                 f"--target-port={target.getsockname()[1]}", "--duration=4000"],
                env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                wait_for_log(process, b"forwarding listening")
                # Error case: one refused target connection must leave the listener available.
                with socket.create_connection(("127.0.0.1", port), 5) as client:
                    client.settimeout(3)
                    try:
                        assert client.recv(1) == b""
                    except ConnectionResetError:
                        pass
                assert process.poll() is None, process.communicate(timeout=2)
                target.listen()
                target.settimeout(5)

                def echo():
                    conn, _ = target.accept()
                    with conn:
                        conn.sendall(conn.recv(32))

                thread = threading.Thread(target=echo, daemon=True)
                thread.start()
                with socket.create_connection(("127.0.0.1", port), 5) as client:
                    client.settimeout(5)
                    client.sendall(b"recovered")
                    assert client.recv(32) == b"recovered", direction
                thread.join(5)
                assert not thread.is_alive(), direction
                _, errors = process.communicate(timeout=8)
                assert process.returncode == 0, errors
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=5)


def verify_scp_interruption(root, env, args, kind="file"):
    import signal

    source = root / ("large-remote-" + kind)
    destination = root / ("interrupted-download-" + kind)
    if kind == "directory":
        source.mkdir()
    sources = [source] if kind == "file" else [source / "one", source / "two"]
    for path in sources:
        with path.open("wb") as output:
            output.truncate(256 * 1024 * 1024)
    process = subprocess.Popen(
        [
            "./cspp",
            "--script=examples/net/scp.ss",
            "--",
            *args,
            "--direction=download",
            f"--kind={kind}",
            f"--local={destination}",
            f"--remote={source}",
        ],
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    try:
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            outputs = ([destination] if kind == "file" else
                       list(destination.iterdir()) if destination.exists() else [])
            partial = next((path for path in outputs if path.exists() and path.stat().st_size > 0), None)
            if partial is not None:
                break
            assert process.poll() is None, process.communicate(timeout=2)
            time.sleep(0.002)
        else:
            raise AssertionError("SCP transfer never created its partial output")
        process.send_signal(signal.SIGINT)
        _, errors = process.communicate(timeout=10)
        assert process.returncode != 0 and b"closing SSH session" in errors, errors
        assert (
            not partial.exists()
        ), "interrupted SCP download leaked a partial output"
    finally:
        if process.poll() is None:
            process.kill()
            process.wait(timeout=5)
        shutil.rmtree(source) if kind == "directory" else source.unlink()


def verify_forwarding(env, args):
    import threading

    for direction in ("local", "remote"):
        target = socket.socket()
        target.bind(("127.0.0.1", 0))
        target.listen()

        def echo():
            conn, _ = target.accept()
            with conn:
                # Reply only after stdin EOF to verify end-to-end half-close.
                chunks = []
                while True:
                    chunk = conn.recv(32768)
                    if not chunk:
                        break
                    chunks.append(chunk)
                conn.sendall(b"".join(chunks) + b":eof")

        thread = threading.Thread(target=echo, daemon=True)
        thread.start()
        port = free_port()
        process = subprocess.Popen(
            [
                "./cspp",
                "--script=examples/net/ssh-port-forward.ss",
                "--",
                *args,
                f"--direction={direction}",
                f"--listen-port={port}",
                f"--target-port={target.getsockname()[1]}",
                "--duration=2500",
            ],
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        try:
            wait_for_log(process, b"forwarding listening")
            payload = bytes(range(256)) * 8192
            with socket.create_connection(("127.0.0.1", port), 5) as conn:
                conn.settimeout(10)
                conn.sendall(payload)
                conn.shutdown(socket.SHUT_WR)
                chunks = []
                while True:
                    chunk = conn.recv(65536)
                    if not chunk:
                        break
                    chunks.append(chunk)
                assert b"".join(chunks) == payload + b":eof", direction
            thread.join(5)
            assert not thread.is_alive()
            _, err = process.communicate(timeout=10)
            assert process.returncode == 0, err
            # Bounded shutdown releases the listening port.
            with socket.socket() as probe:
                probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                probe.bind(("127.0.0.1", port))
        finally:
            target.close()
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)


def verify_forwarding_open_lifecycle(root, env, args):
    user = pwd.getpwuid(os.getuid()).pw_name
    port = args[1].split('=')[1]
    with socket.socket() as target:
        target.bind(('127.0.0.1', 0))
        target.listen(1)
        held = [socket.create_connection(target.getsockname(), .2) for _ in range(2)]
        try:
            script = root / 'forward-open-lifecycle.ss'
            script.write_text(f'''(import (cspp) (cspp transducer) (cspp path) (cspp system) (cspp array) (cspp net))
(call-with-ssh-session "127.0.0.1" {port} "{user}"
 (lambda (ssh)
  (ssh-auth-publickey! ssh "{user}")
  (for-each
   (lambda (action)
    (let ([operation (ssh-open-local-forward/nonblocking
                      ssh "127.0.0.1" {target.getsockname()[1]} "127.0.0.1" 0 100)])
     (unless (eq? action 'unstarted)
      (net-operation-step! operation)
      (unless (and (eq? (net-operation-state operation) 'pending)
                   (pair? (net-operation-poll-targets operation)))
       (error 'test "expected pending forwarding setup")))
     ;; Error cases: timeout and cancellation must release only this channel.
     (if (eq? action 'timeout)
         (begin
          (guard (failure [else
                           (unless (and (net-error? failure)
                                        (eq? (net-error-kind failure) 'timeout))
                            (raise failure))])
           (net-operation-wait operation))
          (unless (eq? (net-operation-state operation) 'failed)
           (error 'test "expected forwarding timeout")))
         (begin
          (net-operation-cancel! operation)
          (net-operation-cancel! operation)
          (unless (eq? (net-operation-state operation) 'cancelled)
           (error 'test "expected forwarding cancellation"))))
     (call-with-ssh-channel ssh 1000
      (lambda (channel)
       (ssh-exec channel "printf alive" 1000)
       (ssh-send-eof! channel)
       (unless (equal? (ssh-read channel 5 1000) (string->utf8 "alive"))
        (error 'test "forwarding cleanup damaged shared SSH session"))))))
   '(timeout cancel unstarted))))
''')
            result = subprocess.run(['./cspp', f'--script={script}', '--'], env=env,
                                    capture_output=True, timeout=10)
            assert result.returncode == 0, result.stderr
        finally:
            for connection in held:
                connection.close()


def verify_forwarding_connect_timeout(env, args, script='examples/net/ssh-port-forward.ss'):
    import signal

    with socket.socket() as target:
        target.bind(('127.0.0.1', 0))
        target.listen(1)
        held = [socket.create_connection(target.getsockname(), .2) for _ in range(2)]
        port = free_port()
        process = subprocess.Popen(
            ['./cspp', f'--script={script}', '--', *args, '--direction=remote',
             f'--listen-port={port}', f'--target-port={target.getsockname()[1]}'],
            env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            wait_for_log(process, b'forwarding listening')
            with socket.create_connection(('127.0.0.1', port), 3):
                # Error case: a still-unconnected socket must retain its setup deadline.
                started = time.monotonic()
                wait_for_log(process, b'forwarded connection failed', timeout=33)
                assert 29 <= time.monotonic() - started <= 33, 'setup deadline was lost'
                assert process.poll() is None, 'one timeout stopped the shared listener'
                process.send_signal(signal.SIGINT)
                _, errors = process.communicate(timeout=3)
                assert process.returncode == 0 and b'forwarding stopped' in errors, errors
        finally:
            for connection in held:
                connection.close()
            if process.poll() is None:
                process.kill()
                process.communicate(timeout=5)


def verify_forwarding_blocked_connect(env, args):
    import signal

    for direction in ('local', 'remote'):
        for ending in ('duration', 'interrupt', 'recover'):
            with socket.socket() as target:
                target.bind(('127.0.0.1', 0))
                target.listen(1)
                target.settimeout(3)
                port = free_port()
                duration = '1500' if ending == 'duration' else '0'
                process = subprocess.Popen(
                    ['./cspp', '--script=examples/net/ssh-port-forward.ss', '--', *args,
                     f'--direction={direction}', f'--listen-port={port}',
                     f'--target-port={target.getsockname()[1]}', f'--duration={duration}'],
                    env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                held = []
                try:
                    wait_for_log(process, b'forwarding listening')
                    with socket.create_connection(('127.0.0.1', port), 3) as active:
                        peer, _ = target.accept()
                        with peer:
                            # Fill the target accept queue, then request another connection.
                            for _ in range(2):
                                held.append(socket.create_connection(target.getsockname(), .2))
                            with socket.create_connection(('127.0.0.1', port), 3) as blocked:
                                time.sleep(.15)
                                # Error case: stalled setup must not stall an existing relay.
                                peer.sendall(b'active-during-connect')
                                active.settimeout(1)
                                assert active.recv(100) == b'active-during-connect', direction
                                if ending == 'recover':
                                    # A pending connection must survive until backlog space opens.
                                    for _ in range(2):
                                        queued, _ = target.accept()
                                        queued.close()
                                    target.settimeout(5)
                                    recovered, _ = target.accept()
                                    with recovered:
                                        recovered.sendall(b'connected-after-backlog')
                                        blocked.settimeout(3)
                                        assert blocked.recv(100) == b'connected-after-backlog', direction
                                if ending in ('interrupt', 'recover'):
                                    process.send_signal(signal.SIGINT)
                                # Error case: pending setup must honor duration and Ctrl-C.
                                _, errors = process.communicate(timeout=3)
                                assert process.returncode == 0 and b'forwarding stopped' in errors, errors
                    with socket.socket() as probe:
                        probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                        probe.bind(('127.0.0.1', port))
                finally:
                    for connection in held:
                        connection.close()
                    if process.poll() is None:
                        process.kill()
                        process.communicate(timeout=5)


def verify_forwarding_cancellation(env, args):
    import signal

    for direction in ("local", "remote"):
        target = socket.socket()
        target.bind(("127.0.0.1", 0))
        target.listen()
        port = free_port()
        process = subprocess.Popen(
            [
                "./cspp",
                "--script=examples/net/ssh-port-forward.ss",
                "--",
                *args,
                f"--direction={direction}",
                f"--listen-port={port}",
                f"--target-port={target.getsockname()[1]}",
            ],
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        conn = None
        accepted = None
        try:
            wait_for_log(process, b"forwarding listening")
            conn = socket.create_connection(("127.0.0.1", port), 5)
            conn.sendall(b"active traffic")
            target.settimeout(5)
            accepted, _ = target.accept()
            accepted.settimeout(5)
            assert accepted.recv(100) == b"active traffic"
            # Ctrl-C must unwind an active relay and cancel both listener directions.
            process.send_signal(signal.SIGINT)
            _, err = process.communicate(timeout=10)
            assert process.returncode == 0 and b"forwarding stopped" in err, err
            assert accepted.recv(100) == b""
            with socket.socket() as probe:
                probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                probe.bind(("127.0.0.1", port))
        finally:
            if conn:
                conn.close()
            if accepted:
                accepted.close()
            target.close()
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)


def verify_password():
    """Optional isolated protocol server; never changes a system account password."""
    import paramiko, threading

    class Server(paramiko.ServerInterface):
        def __init__(self):
            self.command = threading.Event()

        def get_allowed_auths(self, user):
            return "password"

        def check_auth_password(self, user, password):
            return (
                paramiko.AUTH_SUCCESSFUL
                if user == "fixture" and password == "fixture-secret"
                else paramiko.AUTH_FAILED
            )

        def check_channel_request(self, kind, channel):
            return (
                paramiko.OPEN_SUCCEEDED
                if kind == "session"
                else paramiko.OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED
            )

        def check_channel_exec_request(self, channel, command):
            self.command.set()
            return True

    with tempfile.TemporaryDirectory(prefix="cspp-password-") as directory:
        home = pathlib.Path(directory)
        (home / ".ssh").mkdir()
        key = paramiko.RSAKey.generate(2048)
        listener = socket.socket()
        listener.bind(("127.0.0.1", 0))
        listener.listen()
        port = listener.getsockname()[1]
        (home / ".ssh/known_hosts").write_text(
            f"[127.0.0.1]:{port} {key.get_name()} {key.get_base64()}\n"
        )

        def serve():
            conn, _ = listener.accept()
            with conn:
                transport = paramiko.Transport(conn)
                try:
                    transport.add_server_key(key)
                    server = Server()
                    transport.start_server(server=server)
                    channel = transport.accept(10)
                    assert channel and server.command.wait(10)
                    channel.sendall(b"password-auth")
                    channel.send_stderr(b"password-stderr")
                    channel.send_exit_status(9)
                    channel.shutdown_write()
                    channel.close()
                    time.sleep(0.1)
                finally:
                    transport.close()

        thread = threading.Thread(target=serve, daemon=True)
        thread.start()
        try:
            env = os.environ.copy()
            env["HOME"] = str(home)
            env["CSPP_SSH_PASSWORD"] = "fixture-secret"
            p = run(
                "ssh-run-cmd",
                env,
                [
                    "127.0.0.1",
                    f"--port={port}",
                    "--user=fixture",
                    "--auth=password",
                ],
                "--command=fixture",
            )
            assert (
                p.returncode == 9
                and p.stdout == b"password-auth"
                and b"password-stderr" in p.stderr
            ), p
            thread.join(10)
            assert not thread.is_alive()
        finally:
            listener.close()


if __name__ == "__main__":
    import sys

    os.chdir(pathlib.Path(__file__).resolve().parent.parent)
    if "--batch-timeouts" in sys.argv:
        verify_scp_batch_timeouts()
    elif "--forwarding-timeout" in sys.argv:
        with fixture() as (root, env, args):
            verify_forwarding_connect_timeout(env, args)
    elif "--blocked-forwarding" in sys.argv:
        with fixture() as (root, env, args):
            verify_forwarding_blocked_connect(env, args)
            verify_forwarding_open_lifecycle(root, env, args)
    elif "--filter-cancellation" in sys.argv:
        with fixture() as (root, env, args):
            verify_scp_filter_cancellation(root, env, args)
    elif "--callback-cancellation" in sys.argv:
        with fixture() as (root, env, args):
            verify_scp_callback_cancellation(root, env, args)
    elif "--readonly-namespace" in sys.argv:
        with fixture() as (root, env, args):
            verify_scp_readonly_namespace(root, env, args)
    elif "--readonly" in sys.argv:
        with fixture() as (root, env, args):
            verify_scp_readonly_files(root, env, args)
    elif "--symlinks" in sys.argv:
        with fixture() as (root, env, args):
            verify_scp_symlinks(root, env, args)
    elif "--channels" in sys.argv:
        verify_scp_channel_reuse()
    elif "--password" in sys.argv:
        verify_password()
    else:
        verify()
