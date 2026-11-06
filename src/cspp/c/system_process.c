#define _GNU_SOURCE
#include "common.h"
#include <signal.h>
#include <pthread.h>
#if defined(__linux__)
#include <dirent.h>
#include <stddef.h>
#include <sys/syscall.h>
#endif

typedef struct {
  char *data;
  size_t len;
  size_t cap;
} byte_buffer;

static void buffer_free(byte_buffer *buf) {
  free(buf->data);
  buf->data = NULL;
  buf->len = 0;
  buf->cap = 0;
}

static int buffer_append(byte_buffer *buf, const char *data, size_t len) {
  if (len == 0) return 1;
  if (len > SIZE_MAX - buf->len) return 0;
  if (buf->len + len > buf->cap) {
    size_t next = buf->cap == 0 ? 4096 : buf->cap;
    while (next < buf->len + len) {
      if (next > SIZE_MAX / 2) { next = buf->len + len; break; }
      next *= 2;
    }
    char *new_data = (char *)realloc(buf->data, next);
    if (new_data == NULL) return 0;
    buf->data = new_data;
    buf->cap = next;
  }
  memcpy(buf->data + buf->len, data, len);
  buf->len += len;
  return 1;
}

static ptr buffer_to_bytes(byte_buffer *buffer) {
  ptr result = Smake_bytevector((iptr)buffer->len, 0);
  if (buffer->len) memcpy(Sbytevector_data(result), buffer->data, buffer->len);
  return result;
}

/* Encode Unicode scalar values without truncation at embedded NUL. */
static char *copy_scheme_string(ptr str) {
  if (!Sstringp(str)) { errno = EINVAL; return NULL; }
  iptr count = Sstring_length(str);
  if ((size_t)count > (SIZE_MAX - 1) / 4) { errno = ENOMEM; return NULL; }
  char *result = malloc((size_t)count * 4 + 1);
  if (!result) return NULL;
  size_t offset = 0;
  for (iptr i = 0; i < count; i++) {
    uint32_t ch = (uint32_t)Sstring_ref(str, i);
    if (!ch || ch > 0x10ffff || (ch >= 0xd800 && ch <= 0xdfff)) {
      free(result); errno = EINVAL; return NULL;
    }
    if (ch < 0x80) result[offset++] = (char)ch;
    else {
      if (ch < 0x800) result[offset++] = (char)(0xc0 | (ch >> 6));
      else {
        if (ch < 0x10000) result[offset++] = (char)(0xe0 | (ch >> 12));
        else {
          result[offset++] = (char)(0xf0 | (ch >> 18));
          result[offset++] = (char)(0x80 | ((ch >> 12) & 0x3f));
        }
        result[offset++] = (char)(0x80 | ((ch >> 6) & 0x3f));
      }
      result[offset++] = (char)(0x80 | (ch & 0x3f));
    }
  }
  result[offset] = 0;
  return result;
}

static int list_length(ptr ls) {
  int n = 0;
  while (ls != Snil) {
    if (!Spairp(ls)) return -1;
    n += 1;
    ls = Scdr(ls);
  }
  return n;
}

static void free_string_array(char **items) {
  if (items == NULL) return;
  for (int i = 0; items[i] != NULL; i += 1) free(items[i]);
  free(items);
}

static char **string_array_failure(char **items, int error) {
  free_string_array(items);
  errno = error;
  return NULL;
}

// Converts a nonempty Scheme list of strings into a NULL-terminated argv array.
// The caller owns the returned array and must release it with free_string_array.
static char **argv_from_list(ptr argv) {
  int argc = list_length(argv);
  if (argc <= 0) { errno = EINVAL; return NULL; }
  char **out = (char **)calloc((size_t)argc + 1, sizeof(char *));
  if (out == NULL) return NULL;
  ptr ls = argv;
  for (int i = 0; i < argc; i += 1) {
    out[i] = copy_scheme_string(Scar(ls));
    if (out[i] == NULL) {
      return string_array_failure(out, errno);
    }
    ls = Scdr(ls);
  }
  out[argc] = NULL;
  return out;
}

// Converts an option alist of string pairs into a NULL-terminated environment.
// False snapshots the inherited environment; an empty list creates an empty one.
static char **env_from_alist(ptr env) {
  if (env == Sfalse) return cspp_copy_environment();
  int count = list_length(env);
  if (count < 0) { errno = EINVAL; return NULL; }
  char **out = (char **)calloc((size_t)count + 1, sizeof(char *));
  if (out == NULL) return NULL;
  ptr ls = env;
  for (int i = 0; i < count; i += 1) {
    ptr entry = Scar(ls);
    if (!Spairp(entry) || !Sstringp(Scar(entry)) || !Sstringp(Scdr(entry))) {
      return string_array_failure(out, EINVAL);
    }
    char *key = copy_scheme_string(Scar(entry));
    if (!key) return string_array_failure(out, errno);
    char *value = copy_scheme_string(Scdr(entry));
    if (value == NULL) {
      int error = errno;
      free(key);
      return string_array_failure(out, error);
    }
    size_t key_len = strlen(key);
    size_t value_len = strlen(value);
    out[i] = (char *)malloc(key_len + value_len + 2);
    if (out[i] == NULL) {
      int error = errno;
      free(key);
      free(value);
      return string_array_failure(out, error);
    }
    memcpy(out[i], key, key_len);
    out[i][key_len] = '=';
    memcpy(out[i] + key_len + 1, value, value_len + 1);
    free(key);
    free(value);
    ls = Scdr(ls);
  }
  out[count] = NULL;
  return out;
}

/* Preserve the first conversion failure; a subsequent successful allocation
 * or malformed environment must not replace its errno. Callers free both arrays. */
static int copy_command(ptr argv, ptr env, char ***cargv, char ***cenv) {
  *cenv = NULL;
  *cargv = argv_from_list(argv);
  if (!*cargv) return errno;
  *cenv = env_from_alist(env);
  return *cenv ? 0 : errno;
}

#if defined(__unix__) || defined(__APPLE__)
static const char *environment_value(char **env, const char *name) {
  size_t name_len = strlen(name);
  for (char **entry = env; entry != NULL && *entry != NULL; entry += 1) {
    if (strncmp(*entry, name, name_len) == 0 && (*entry)[name_len] == '=') {
      return *entry + name_len + 1;
    }
  }
  return NULL;
}

// Prepare native-owned PATH candidates before fork. Relative candidates are
// interpreted after the child's chdir; only execve decides whether they work.
static int executable_candidates(const char *program, char **env, char ***result) {
  if (program[0] == '\0') return ENOENT;
  int explicit_path = strchr(program, '/') != NULL;
  const char *path = environment_value(env, "PATH");
  if (path == NULL) path = "/bin:/usr/bin";
  size_t count = 1;
  if (!explicit_path) {
    for (const char *p = path; *p; p++) {
      if (*p == ':') count++;
    }
  }
  if (count > SIZE_MAX / sizeof(char *) - 1) return ENOMEM;
  char **candidates = calloc(count + 1, sizeof(char *));
  if (!candidates) return ENOMEM;
  if (explicit_path) {
    candidates[0] = strdup(program);
    if (!candidates[0]) { free(candidates); return ENOMEM; }
    *result = candidates;
    return 0;
  }

  size_t program_len = strlen(program);
  const char *start = path;
  for (size_t i = 0; i < count; i++) {
    const char *end = strchr(start, ':');
    size_t directory_len = end == NULL ? strlen(start) : (size_t)(end - start);
    size_t separator_len = directory_len == 0 ? 0 : 1;
    if (program_len > SIZE_MAX - 2 || directory_len > SIZE_MAX - program_len - 2) {
      free_string_array(candidates);
      return ENOMEM;
    }
    char *candidate = (char *)malloc(directory_len + separator_len + program_len + 1);
    if (candidate == NULL) { free_string_array(candidates); return ENOMEM; }
    memcpy(candidate, start, directory_len);
    if (separator_len != 0) candidate[directory_len] = '/';
    memcpy(candidate + directory_len + separator_len, program, program_len + 1);
    candidates[i] = candidate;
    if (end == NULL) break;
    start = end + 1;
  }
  *result = candidates;
  return 0;
}

static int make_pipe(int fds[2]);

static void child_report_errno(int fd, int error) {
  const unsigned char *bytes = (const unsigned char *)&error;
  size_t written = 0;
  while (written < sizeof(error)) {
    ssize_t n = write(fd, bytes + written, sizeof(error) - written);
    if (n > 0) written += (size_t)n;
    else if (n < 0 && errno == EINTR) continue;
    else break;
  }
  _exit(127);
}

// source is a descriptor, -1 to inherit, or -2 to connect /dev/null.
static int child_redirect(int source, int target) {
  if (source == -1) return 0;
  int fd = source;
  if (source == -2) {
    fd = open("/dev/null", O_RDWR);
    if (fd < 0) return -1;
  }
  if (fd != target && dup2(fd, target) < 0) {
    if (source == -2) close(fd);
    return -1;
  }
  if (source == -2 && fd != target) close(fd);
  return 0;
}

static void wait_child_after_exec_error(pid_t pid) {
  int status;
  while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {
  }
}

/* Keep only standard streams and the exec-error pipe. No allocation or locks
 * are permitted after fork; Linux's procfs fallback uses a native stack buffer. */
static int child_close_descriptors(int preserve, long descriptor_limit) {
#if defined(__linux__)
  (void)descriptor_limit;
#ifdef SYS_close_range
  if ((preserve == 3 || syscall(SYS_close_range, 3U, (unsigned int)preserve - 1, 0) == 0)
      && syscall(SYS_close_range, (unsigned int)preserve + 1, ~0U, 0) == 0)
    return 0;
#endif

  int directory = open("/proc/self/fd", O_RDONLY | O_DIRECTORY | O_CLOEXEC);
  /* Current resource limits do not bound already-open descriptors: limits may
   * have been lowered after opening them. Refuse exec rather than leak a handle
   * when neither close_range nor complete procfs enumeration is available. */
  if (directory < 0) return ENOTSUP;
  int error = 0;
  for (;;) {
    char entries[4096];
    ssize_t size = syscall(SYS_getdents64, directory, entries, sizeof(entries));
    if (size < 0) {
      if (errno == EINTR) continue;
      error = errno;
      break;
    }
    if (size == 0) break;
    int closed = 0;
    for (size_t offset = 0; offset < (size_t)size;) {
      unsigned short length;
      memcpy(&length, entries + offset + offsetof(struct dirent64, d_reclen), sizeof(length));
      const char *name = entries + offset + offsetof(struct dirent64, d_name);
      offset += length;
      if (*name < '0' || *name > '9') continue;
      int fd = 0;
      for (; *name >= '0' && *name <= '9'; name++) fd = fd * 10 + (*name - '0');
      if (fd < 3 || fd == preserve || fd == directory) continue;
      close(fd);
      closed = 1;
    }
    /* Closing entries changes this directory; restart to avoid skipping any. */
    if (closed && lseek(directory, 0, SEEK_SET) < 0) { error = errno; break; }
  }
  close(directory);
  return error;
#else
  for (long fd = 3; fd < descriptor_limit; fd++) {
    if (fd != preserve) close((int)fd);
  }
  return 0;
#endif
}

static int spawn_native(pid_t *pid, char *const argv[], char *const env[],
                         char *const candidates[], const char *cwd,
                         int stdin_source, int stdout_source,
                         int stderr_source, int stderr_to_stdout) {
  long descriptor_limit = 0;
#if !defined(__linux__)
  descriptor_limit = sysconf(_SC_OPEN_MAX);
  if (descriptor_limit < 0) return errno ? errno : EINVAL;
#endif
  int error_pipe[2];
  if (make_pipe(error_pipe) != 0) return errno;
  int flags = fcntl(error_pipe[1], F_GETFD, 0);
  if (flags < 0 || fcntl(error_pipe[1], F_SETFD, flags | FD_CLOEXEC) < 0) {
    int error = errno;
    close(error_pipe[0]);
    close(error_pipe[1]);
    return error;
  }

  pid_t child = fork();
  if (child < 0) {
    int error = errno;
    close(error_pipe[0]);
    close(error_pipe[1]);
    return error;
  }
  if (child == 0) {
    close(error_pipe[0]);
    // Chez may be multithreaded.  After fork, the child must avoid Scheme,
    // allocation, stdio, and locks; only async-signal-safe calls run until execve.
    if (cwd != NULL && cwd[0] != '\0' && chdir(cwd) != 0)
      child_report_errno(error_pipe[1], errno);
    if (child_redirect(stdin_source, STDIN_FILENO) != 0)
      child_report_errno(error_pipe[1], errno);
    if (child_redirect(stdout_source, STDOUT_FILENO) != 0)
      child_report_errno(error_pipe[1], errno);
    if (stderr_to_stdout) {
      if (dup2(STDOUT_FILENO, STDERR_FILENO) < 0)
        child_report_errno(error_pipe[1], errno);
    } else if (child_redirect(stderr_source, STDERR_FILENO) != 0) {
      child_report_errno(error_pipe[1], errno);
    }
    int close_error = child_close_descriptors(error_pipe[1], descriptor_limit);
    if (close_error) child_report_errno(error_pipe[1], close_error);
    /* Chez ignores SIGPIPE for Scheme port I/O. External commands need the
     * default broken-pipe behavior; changing it here leaves the parent alone. */
    struct sigaction pipe_action = {0};
    pipe_action.sa_handler = SIG_DFL;
    sigemptyset(&pipe_action.sa_mask);
    if (sigaction(SIGPIPE, &pipe_action, NULL) != 0)
      child_report_errno(error_pipe[1], errno);
    int error = ENOENT, permission_denied = 0;
    for (size_t i = 0; candidates[i] != NULL; i++) {
      execve(candidates[i], argv, env);
      error = errno;
      switch (error) {
        case EACCES: permission_denied = 1; break;
        case ENOENT: case ENOTDIR: case ESTALE: case ENODEV: case ETIMEDOUT: break;
        // No implicit shell fallback for ENOEXEC. The kernel handles shebangs.
        default: child_report_errno(error_pipe[1], error);
      }
    }
    child_report_errno(error_pipe[1], permission_denied ? EACCES : error);
  }

  close(error_pipe[1]);
  int child_error = 0;
  size_t received = 0;
  while (received < sizeof(child_error)) {
    ssize_t n = read(error_pipe[0], (unsigned char *)&child_error + received,
                     sizeof(child_error) - received);
    if (n > 0) received += (size_t)n;
    else if (n == 0) break;
    else if (errno == EINTR) continue;
    else {
      int error = errno;
      close(error_pipe[0]);
      kill(child, SIGKILL);
      wait_child_after_exec_error(child);
      return error;
    }
  }
  close(error_pipe[0]);
  if (received != 0) {
    wait_child_after_exec_error(child);
    return received == sizeof(child_error) ? child_error : EIO;
  }
  *pid = child;
  return 0;
}
/* Fork/exec setup and its error-pipe handshake may block. Deactivate to permit
 * concurrent GC; argv, env, candidates, and directory are all native-owned.
 * The calling Scheme thread remains responsible for reactivation afterward. */
static int spawn_checked(pid_t *pid, char *const argv[], char *const env[],
                         char *const candidates[], const char *cwd,
                         int stdin_source, int stdout_source, int stderr_source,
                         int stderr_to_stdout) {
  char *directory = strdup(cwd ? cwd : "");
  if (!directory) return ENOMEM;
  Sdeactivate_thread();
  int error = spawn_native(pid, argv, env, candidates, directory, stdin_source,
                            stdout_source, stderr_source, stderr_to_stdout);
  Sactivate_thread(); /* Resume Scheme access before returning to the caller. */
  free(directory);
  return error;
}

#endif

static void close_fd(int *fd) {
  if (*fd >= 0) {
    close(*fd);
    *fd = -1;
  }
}

static int set_nonblock(int fd) {
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0) return -1;
  return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

static int make_pipe(int fds[2]) {
  #if defined(__linux__)
  if (pipe2(fds, O_CLOEXEC) != 0) return -1;
#else
  if (pipe(fds) != 0) return -1;
  if (fcntl(fds[0], F_SETFD, FD_CLOEXEC) < 0 ||
      fcntl(fds[1], F_SETFD, FD_CLOEXEC) < 0) {
    int error = errno; close(fds[0]); close(fds[1]); errno = error; return -1;
  }
#endif
  // Keep pipe descriptors above standard streams so spawn file actions cannot alias them.
  for (int i = 0; i < 2; i += 1) {
    if (fds[i] < 3) {
      int replacement = fcntl(fds[i], F_DUPFD_CLOEXEC, 3);
      if (replacement < 0) {
        close_fd(&fds[0]);
        close_fd(&fds[1]);
        return -1;
      }
      close(fds[i]);
      fds[i] = replacement;
    }
  }
  return 0;
}

static int64_t monotonic_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ((int64_t)ts.tv_sec * 1000) + ((int64_t)ts.tv_nsec / 1000000);
}

static void sleep_ms(int ms) {
  struct timespec ts;
  ts.tv_sec = ms / 1000;
  ts.tv_nsec = (long)(ms % 1000) * 1000000L;
  while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {
  }
}

static ptr pid_context(pid_t pid) {
  return Scons(Scons(Sstring("pid"), Sinteger((iptr)pid)), Snil);
}

static int wait_pid_block(pid_t pid, int *status) {
  for (;;) {
    pid_t r = waitpid(pid, status, 0);
    if (r == pid) return 0;
    if (r < 0 && errno == EINTR) continue;
    return -1;
  }
}

/* Called with an active Scheme thread. Reaping may block, so deactivate for GC;
 * only the native PID array and stack status cross this region. The caller
 * retains its original error separately from cleanup's errno. */
static void cleanup_pipeline_children(pid_t *pids, int count) {
  if (count == 0) return;
  Sdeactivate_thread();
  for (int i = 0; i < count; i += 1) kill(pids[i], SIGKILL);
  for (int i = 0; i < count; i += 1) {
    int status;
    (void)wait_pid_block(pids[i], &status);
  }
  Sactivate_thread(); /* Caller may now access/construct Scheme error results. */
}

/* Only native-owned memory crosses poll/wait. A single bounded read per stream
 * per iteration keeps a busy producer from starving stdin or the deadline. */
static int terminate_child(pid_t pid) {
  int status;
  kill(pid, SIGTERM);
  for (int i = 0; i < 20; i++) {
    pid_t result = waitpid(pid, &status, WNOHANG);
    if (result == pid || (result < 0 && errno == ECHILD)) return 0;
    if (result < 0 && errno != EINTR) return -1;
    sleep_ms(5);
  }
  kill(pid, SIGKILL);
  return wait_pid_block(pid, &status);
}

static ptr capture_child(pid_t pid, int input_fd, const unsigned char *input,
                         size_t input_size, int stdout_fd, int stderr_fd,
                         int retain_stdout, int retain_stderr,
                         int timeout_ms, const char *operation) {
  byte_buffer buffers[2] = {{0}, {0}};
  int output_fds[2] = {stdout_fd, stderr_fd};
  int retain[2] = {retain_stdout, retain_stderr};
  int status = 0, reaped = 0, error = 0, timed_out = 0, broken_pipe = 0;
  size_t sent = 0;
  int64_t start = monotonic_ms();
  sigset_t pipe_signal, old_mask, pending;
  sigemptyset(&pipe_signal);
  sigaddset(&pipe_signal, SIGPIPE);
  /* poll, I/O, reaping, and termination can block: release Chez's GC coordination.
   * input and output buffers are native-owned; only native descriptors, signal
   * sets, and stack values are accessed until Sactivate_thread below.
   * Writes can race a child's close. Block only this thread's SIGPIPE, then
   * consume only a newly generated pending instance before restoring its mask. */
  Sdeactivate_thread();
  int mask_error = pthread_sigmask(SIG_BLOCK, &pipe_signal, &old_mask);
  int had_pipe = sigpending(&pending) == 0 && sigismember(&pending, SIGPIPE);
  if (mask_error) error = mask_error;
  if (input_fd >= 0 && set_nonblock(input_fd) < 0) error = errno;
  for (int i = 0; i < 2; i++)
    if (output_fds[i] >= 0 && set_nonblock(output_fds[i]) < 0) error = errno;
  if (!input_size) close_fd(&input_fd);

  while (!error) {
    if (!reaped) {
      /* With no remaining I/O or deadline, block in waitpid instead of polling
       * an empty descriptor set. The Scheme thread is already inactive. */
      int wait_options = timeout_ms < 0 && input_fd < 0 &&
                         output_fds[0] < 0 && output_fds[1] < 0 ? 0 : WNOHANG;
      pid_t result = waitpid(pid, &status, wait_options);
      if (result == pid) { reaped = 1; close_fd(&input_fd); }
      else if (result < 0) {
        if (errno == EINTR) continue;
        error = errno;
        break;
      }
    }
    if (reaped && output_fds[0] < 0 && output_fds[1] < 0) break;
    int64_t remaining = timeout_ms < 0 ? 20 : timeout_ms - (monotonic_ms() - start);
    if (remaining <= 0) { timed_out = 1; break; }
    struct pollfd fds[3] = {
      {input_fd, POLLOUT, 0},
      {output_fds[0], POLLIN, 0},
      {output_fds[1], POLLIN, 0}
    };
    int ready = poll(fds, 3, remaining < 20 ? (int)remaining : 20);
    if (ready < 0) { if (errno == EINTR) continue; error = errno; break; }
    if (input_fd >= 0 && fds[0].revents) {
      size_t chunk = input_size - sent;
      if (chunk > 65536) chunk = 65536;
      ssize_t written = write(input_fd, input + sent, chunk);
      if (written > 0) {
        sent += (size_t)written;
        if (sent == input_size) close_fd(&input_fd);
      } else if (written < 0 && errno == EPIPE) {
        broken_pipe = 1;
        close_fd(&input_fd); /* A child may intentionally ignore remaining input. */
      } else if (written < 0 && errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
        error = errno;
      }
    }
    for (int i = 0; i < 2 && !error; i++) {
      if (output_fds[i] < 0 || !fds[i + 1].revents) continue;
      char chunk[65536];
      ssize_t length = read(output_fds[i], chunk, sizeof(chunk));
      if (length > 0) {
        if (retain[i] && !buffer_append(&buffers[i], chunk, (size_t)length)) error = ENOMEM;
      } else if (!length) close_fd(&output_fds[i]);
      else if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) error = errno;
    }
  }
  close_fd(&input_fd);
  close_fd(&output_fds[0]);
  close_fd(&output_fds[1]);
  if ((error || timed_out) && !reaped) (void)terminate_child(pid);
  if (!mask_error) {
    if (broken_pipe && !had_pipe) {
      struct timespec immediate = {0, 0};
      while (sigtimedwait(&pipe_signal, NULL, &immediate) < 0 && errno == EINTR) {}
    }
    (void)pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
  }
  Sactivate_thread(); /* Resume before creating Scheme captures and tagged results. */
  ptr result;
  if (error) { errno = error; result = cspp_errno_result(operation, pid_context(pid)); }
  else if (timed_out) result = cspp_timeout_result(operation, pid_context(pid));
  else {
    ptr value = Smake_vector(4, Sfalse);
    Svector_set(value, 0, Sinteger((iptr)pid));
    Svector_set(value, 1, Sinteger(status));
    Svector_set(value, 2, buffer_to_bytes(&buffers[0]));
    Svector_set(value, 3, buffer_to_bytes(&buffers[1]));
    result = cspp_ok(value);
  }
  buffer_free(&buffers[0]);
  buffer_free(&buffers[1]);
  return result;
}

// Spawns argv directly, optionally feeds stdin, captures requested output, and
// waits for completion. Shell parsing is intentionally left to Scheme wrappers.
ptr cspp_spawn_capture(ptr argv, ptr env, const char *cwd,
                         ptr stdin_payload, int capture_stdout, int capture_stderr,
                         int stdout_null, int stderr_null, int stderr_to_stdout,
                         int timeout_ms) {
#if defined(__unix__) || defined(__APPLE__)
  char **cargv, **cenv;
  int rc = copy_command(argv, env, &cargv, &cenv);
  if (rc != 0) {
    free_string_array(cargv);
    free_string_array(cenv);
    errno = rc;
    return cspp_errno_result("spawn-capture", Snil);
  }
  char **candidates = NULL;
  int resolve_error = executable_candidates(cargv[0], cenv, &candidates);
  if (resolve_error != 0) {
    free_string_array(cargv);
    free_string_array(cenv);
    errno = resolve_error;
    return cspp_errno_result("spawn-capture", Snil);
  }

  int stdin_pipe[2] = {-1, -1};
  int stdout_pipe[2] = {-1, -1};
  int stderr_pipe[2] = {-1, -1};
  if (Sbytevectorp(stdin_payload)) {
    if (make_pipe(stdin_pipe) != 0) rc = errno;
  }

  if (rc == 0 && capture_stdout) {
    if (make_pipe(stdout_pipe) != 0) rc = errno;
  }

  if (rc == 0 && !stderr_to_stdout && capture_stderr) {
    if (make_pipe(stderr_pipe) != 0) rc = errno;
  }

  if (rc != 0) {
    errno = rc;
    close_fd(&stdin_pipe[0]);
    close_fd(&stdin_pipe[1]);
    close_fd(&stdout_pipe[0]);
    close_fd(&stdout_pipe[1]);
    close_fd(&stderr_pipe[0]);
    close_fd(&stderr_pipe[1]);
    free_string_array(candidates);
    free_string_array(cargv);
    free_string_array(cenv);
    return cspp_errno_result("spawn-capture", Snil);
  }

  pid_t pid;
  Slock_object(stdin_payload);
  rc = spawn_checked(&pid, cargv, cenv, candidates, cwd,
                     Sbytevectorp(stdin_payload) ? stdin_pipe[0] : (Ssymbolp(stdin_payload) ? -1 : -2),
                     capture_stdout ? stdout_pipe[1] : (stdout_null ? -2 : -1),
                     capture_stderr ? stderr_pipe[1] : (stderr_null ? -2 : -1),
                     stderr_to_stdout);
  free_string_array(candidates);
  close_fd(&stdin_pipe[0]);
  close_fd(&stdout_pipe[1]);
  close_fd(&stderr_pipe[1]);
  free_string_array(cargv);
  free_string_array(cenv);

  if (rc != 0) {
    Sunlock_object(stdin_payload);
    errno = rc;
    close_fd(&stdin_pipe[1]);
    close_fd(&stdout_pipe[0]);
    close_fd(&stderr_pipe[0]);
    return cspp_errno_result("spawn-capture", Snil);
  }

  unsigned char *input = NULL;
  size_t input_size = 0;
  if (stdin_payload != Sfalse && !Ssymbolp(stdin_payload)) {
    if (!Sbytevectorp(stdin_payload)) {
      close_fd(&stdin_pipe[1]); close_fd(&stdout_pipe[0]); close_fd(&stderr_pipe[0]);
      /* Termination may block. Only pid crosses deactivation; stdin_payload
       * remains locked and is not accessed until activation resumes. */
      Sdeactivate_thread();
      terminate_child(pid);
      Sactivate_thread();
      Sunlock_object(stdin_payload);
      errno = EINVAL; return cspp_errno_result("spawn-capture", Snil);
    }
    input_size = (size_t)Sbytevector_length(stdin_payload);
    input = malloc(input_size ? input_size : 1);
    if (!input) {
      close_fd(&stdin_pipe[1]); close_fd(&stdout_pipe[0]); close_fd(&stderr_pipe[0]);
      /* Allow GC during cleanup using only the native pid; reactivate before
       * unlocking stdin_payload or constructing the allocation error. */
      Sdeactivate_thread();
      terminate_child(pid);
      Sactivate_thread();
      Sunlock_object(stdin_payload);
      errno = ENOMEM; return cspp_errno_result("spawn-capture", Snil);
    }
    if (input_size) memcpy(input, Sbytevector_data(stdin_payload), input_size);
  }
  Sunlock_object(stdin_payload);
  ptr result = capture_child(pid, stdin_pipe[1], input, input_size,
                             capture_stdout ? stdout_pipe[0] : -1,
                             capture_stderr ? stderr_pipe[0] : -1,
                             capture_stdout == 1, capture_stderr == 1,
                             timeout_ms, "spawn-capture");
  free(input);
  return result;
#else
  (void)argv; (void)env; (void)cwd; (void)stdin_payload;
  (void)capture_stdout; (void)capture_stderr; (void)stdout_null;
  (void)stderr_null; (void)stderr_to_stdout; (void)timeout_ms;
  return cspp_unsupported_result("spawn-capture");
#endif
}

// Spawns argv directly and returns the child pid without waiting. The process
// object in Scheme owns waiting; this helper only configures null stdio modes.
ptr cspp_spawn_process(ptr argv, ptr env, const char *cwd,
                         int stdin_mode, int stdout_mode, int stderr_mode) {
#if defined(__unix__) || defined(__APPLE__)
  const char *operation = "process-spawn";
  char **cargv, **cenv;
  char **candidates = NULL;
  int pipes[3][2] = {{-1, -1}, {-1, -1}, {-1, -1}};
  int modes[3] = {stdin_mode, stdout_mode, stderr_mode};
  int sources[3], error = copy_command(argv, env, &cargv, &cenv);
  pid_t pid = -1;
  if (error) goto done;
  error = executable_candidates(cargv[0], cenv, &candidates);
  if (error) goto done;
  for (int i = 0; i < 3; i++) {
    if (modes[i] < 0 || modes[i] > (i == 2 ? 3 : 2)) { error = EINVAL; goto done; }
    if (modes[i] == 2) {
      if (make_pipe(pipes[i]) < 0) { error = errno; goto done; }
      sources[i] = pipes[i][i == 0 ? 0 : 1];
    } else sources[i] = modes[i] == 1 ? -2 : -1;
  }
  error = spawn_checked(&pid, cargv, cenv, candidates, cwd,
                         sources[0], sources[1], sources[2], stderr_mode == 3);
 done:
  free_string_array(candidates);
  free_string_array(cargv);
  free_string_array(cenv);
  if (error) {
    for (int i = 0; i < 3; i++) { close_fd(&pipes[i][0]); close_fd(&pipes[i][1]); }
    errno = error;
    return cspp_errno_result(operation, Snil);
  }
  ptr value = Smake_vector(4, Sfalse);
  Svector_set(value, 0, Sinteger((iptr)pid));
  for (int i = 0; i < 3; i++) {
    int child_end = i == 0 ? 0 : 1;
    close_fd(&pipes[i][child_end]);
    Svector_set(value, i + 1, Sinteger(pipes[i][1 - child_end]));
  }
  return cspp_ok(value);
#else
  (void)argv; (void)env; (void)cwd; (void)stdin_mode; (void)stdout_mode; (void)stderr_mode;
  return cspp_unsupported_result("process-spawn");
#endif
}

ptr cspp_waitpid(int pid, int nohang) {
#if defined(__unix__) || defined(__APPLE__)
  int status;
  pid_t r;
  /* Blocking waitpid must not stall GC. Only scalar pid/nohang and native stack
   * status are used while inactive; preserve errno across reactivation. */
  Sdeactivate_thread();
  do {
    r = waitpid((pid_t)pid, &status, nohang ? WNOHANG : 0);
  } while (r < 0 && errno == EINTR);
  int saved_errno = errno;
  Sactivate_thread(); /* Resume before allocating status vectors or error context. */
  errno = saved_errno;
  if (r == 0) return cspp_ok(Sfalse);
  if (r < 0) return cspp_errno_result("waitpid", pid_context((pid_t)pid));
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sinteger((iptr)r));
  Svector_set(v, 1, Sinteger(status));
  return cspp_ok(v);
#else
  (void)pid; (void)nohang;
  return cspp_unsupported_result("waitpid");
#endif
}

/* Opened under the process mutex before another CSPP waiter can reap the PID.
 * Each wait invocation owns its descriptor. Any unavailable/resource-limited
 * pidfd path falls back to the existing nonblocking wait loop. */
int cspp_process_wait_open(int pid) {
#if defined(__linux__) && defined(SYS_pidfd_open)
  int fd = (int)syscall(SYS_pidfd_open, pid, 0);
  if (fd >= 0 && fd < 3) {
    int elevated = fcntl(fd, F_DUPFD_CLOEXEC, 3);
    close(fd);
    fd = elevated;
  }
  return fd;
#else
  (void)pid;
  return -1;
#endif
}

/* Bound with __collect_safe: Chez deactivates the thread before this blocking
 * call and reactivates it on return. Only scalar arguments and native stack
 * storage are used here; Scheme objects must never be accessed in this region.
 * Readiness does not reap: Scheme reacquires the process mutex to publish status. */
int cspp_process_wait_ready(int fd, int timeout_ms) {
  if (fd < 0 || timeout_ms < -1) return -EINVAL;
  struct pollfd event = {fd, POLLIN, 0};
  int ready = poll(&event, 1, timeout_ms);
  if (ready > 0) return event.revents & POLLNVAL ? -EBADF : 1;
  if (ready == 0) return 0;
  /* Distinguish interruption so Scheme can force an event check before another
   * blocking call. Merely returning may leave its event-trap counter unexpired.
   * Scheme retains the original monotonic deadline when it waits again. */
  return errno == EINTR ? 2 : -errno;
}

ptr cspp_make_pipe() {
#if defined(__unix__) || defined(__APPLE__)
  int fds[2];
  if (make_pipe(fds) != 0) {
    return cspp_errno_result("make-pipe", Snil);
  }
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sinteger(fds[0]));
  Svector_set(v, 1, Sinteger(fds[1]));
  return cspp_ok(v);
#else
  return cspp_unsupported_result("make-pipe");
#endif
}

#if defined(__unix__) || defined(__APPLE__)
typedef enum {
  PIPELINE_STDOUT_INHERIT,
  PIPELINE_STDOUT_CAPTURE,
  PIPELINE_STDOUT_DISCARD
} pipeline_stdout_mode;

typedef struct {
  int count;
  int spawned;
  char **env;
  char ***argvs;
  pid_t *pids;
  int input_fd;
  int capture_pipe[2];
} pipeline_launch;

static void close_pipeline_connections(pipeline_launch *launch) {
  close_fd(&launch->input_fd);
  close_fd(&launch->capture_pipe[1]);
}

/* On failure, reap launched children before releasing their native-owned arguments.
 * A successful caller owns the final capture read end until capture_child closes it. */
static void dispose_pipeline(pipeline_launch *launch, int terminate) {
  if (terminate) cleanup_pipeline_children(launch->pids, launch->spawned);
  close_pipeline_connections(launch);
  close_fd(&launch->capture_pipe[0]);
  if (launch->argvs != NULL) {
    for (int i = 0; i < launch->count; i += 1) free_string_array(launch->argvs[i]);
  }
  free(launch->argvs);
  free(launch->pids);
  free_string_array(launch->env);
}

/* Prepare and launch both live and synchronous pipelines. All resources belong
 * to launch on success; failure disposes them and returns the original error. */
static int start_pipeline(ptr specs, pipeline_stdout_mode stdout_mode,
                          pipeline_launch *launch) {
  memset(launch, 0, sizeof(*launch));
  launch->input_fd = -1;
  launch->capture_pipe[0] = -1;
  launch->capture_pipe[1] = -1;
  int error = 0;
  launch->count = list_length(specs);
  if (launch->count <= 0) return EINVAL;

  launch->env = env_from_alist(Sfalse);
  if (launch->env == NULL) { error = errno; goto failed; }
  launch->argvs = calloc((size_t)launch->count, sizeof(*launch->argvs));
  launch->pids = calloc((size_t)launch->count, sizeof(*launch->pids));
  if (launch->argvs == NULL || launch->pids == NULL) { error = ENOMEM; goto failed; }

  ptr remaining = specs;
  for (int i = 0; i < launch->count; i += 1) {
    launch->argvs[i] = argv_from_list(Scar(remaining));
    if (launch->argvs[i] == NULL) { error = errno; goto failed; }
    remaining = Scdr(remaining);
  }
  if (stdout_mode == PIPELINE_STDOUT_CAPTURE && make_pipe(launch->capture_pipe) != 0) {
    error = errno;
    goto failed;
  }

  for (int i = 0; i < launch->count; i += 1) {
    /* Retain only the preceding reader and the next connection. Closing each
     * writer after spawn also lets earlier stages observe EOF promptly. */
    int next_pipe[2] = {-1, -1};
    if (i < launch->count - 1 && make_pipe(next_pipe) != 0) { error = errno; goto failed; }
    char **candidates = NULL;
    error = executable_candidates(launch->argvs[i][0], launch->env, &candidates);
    if (error == 0) {
      error = spawn_checked(&launch->pids[i], launch->argvs[i], launch->env, candidates, "",
                            launch->input_fd,
                            i < launch->count - 1 ? next_pipe[1] :
                            (stdout_mode == PIPELINE_STDOUT_CAPTURE ? launch->capture_pipe[1] :
                             stdout_mode == PIPELINE_STDOUT_INHERIT ? -1 : -2),
                            -1, 0);
    }
    free_string_array(candidates);
    close_fd(&launch->input_fd);
    close_fd(&next_pipe[1]);
    if (error != 0) { close_fd(&next_pipe[0]); goto failed; }
    launch->input_fd = next_pipe[0];
    launch->spawned += 1;
  }
  return 0;

 failed:
  dispose_pipeline(launch, 1);
  return error;
}
#endif

// Starts every process in a pipeline and connects adjacent stdout/stdin pipes.
// The caller receives process ids and is responsible for waiting on them.
ptr cspp_spawn_pipeline(ptr specs) {
#if defined(__unix__) || defined(__APPLE__)
  pipeline_launch launch;
  int error = start_pipeline(specs, PIPELINE_STDOUT_INHERIT, &launch);
  if (error != 0) {
    errno = error;
    return cspp_errno_result("spawn-pipeline", Snil);
  }
  ptr pids = Smake_vector(launch.count, Sfalse);
  for (int i = 0; i < launch.count; i += 1)
    Svector_set(pids, i, Sinteger((iptr)launch.pids[i]));
  dispose_pipeline(&launch, 0);
  return cspp_ok(pids);
#else
  (void)specs;
  return cspp_unsupported_result("spawn-pipeline");
#endif
}

// Runs a pipeline to completion, optionally capturing the final process stdout.
// The returned status belongs to the final process, matching the Scheme API.
static ptr run_pipeline(ptr specs, int timeout_ms, int capture_stdout) {
#if defined(__unix__) || defined(__APPLE__)
  pipeline_launch launch;
  int error = start_pipeline(specs,
                             capture_stdout ? PIPELINE_STDOUT_CAPTURE : PIPELINE_STDOUT_DISCARD,
                             &launch);
  if (error != 0) {
    errno = error;
    return cspp_errno_result("spawn-pipeline", Snil);
  }
  close_pipeline_connections(&launch);
  int stdout_fd = launch.capture_pipe[0];
  launch.capture_pipe[0] = -1; /* capture_child owns and closes the read end. */
  ptr result = capture_child(launch.pids[launch.count - 1], -1, NULL, 0, stdout_fd, -1,
                             capture_stdout, 0, timeout_ms, "spawn-pipeline");
  Slock_object(result);
  if (Svector_ref(result, 0) != Sstring_to_symbol("ok")) {
    /* capture_child already cleaned up the final stage. Upstream stages may
     * never write to their output, so closing pipes alone cannot stop them. */
    cleanup_pipeline_children(launch.pids, launch.count - 1);
  } else {
    /* Earlier stages may still run. Release GC coordination while reaping;
     * launch owns native PIDs and result stays locked without being accessed. */
    Sdeactivate_thread();
    for (int i = 0; i < launch.count - 1; i += 1) {
      int status;
      (void)wait_pid_block(launch.pids[i], &status);
    }
    Sactivate_thread(); /* Resume before unlocking and returning the Scheme result. */
  }
  Sunlock_object(result);
  dispose_pipeline(&launch, 0);
  return result;
#else
  (void)specs; (void)timeout_ms; (void)capture_stdout;
  return cspp_unsupported_result("spawn-pipeline");
#endif
}

ptr cspp_spawn_pipeline_capture(ptr specs, int timeout_ms) {
  return run_pipeline(specs, timeout_ms, 1);
}

ptr cspp_run_pipeline(ptr specs) {
  return run_pipeline(specs, -1, 0);
}
