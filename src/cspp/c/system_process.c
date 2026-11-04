#define _GNU_SOURCE
#include "common.h"
#include <signal.h>
#include <pthread.h>

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
  if (!Sstringp(str)) return NULL;
  iptr count = Sstring_length(str);
  if ((size_t)count > (SIZE_MAX - 1) / 4) return NULL;
  char *result = malloc((size_t)count * 4 + 1);
  if (!result) return NULL;
  size_t offset = 0;
  for (iptr i = 0; i < count; i++) {
    uint32_t ch = (uint32_t)Sstring_ref(str, i);
    if (!ch || ch > 0x10ffff || (ch >= 0xd800 && ch <= 0xdfff)) { free(result); return NULL; }
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

// Converts a nonempty Scheme list of strings into a NULL-terminated argv array.
// The caller owns the returned array and must release it with free_string_array.
static char **argv_from_list(ptr argv) {
  int argc = list_length(argv);
  if (argc <= 0) return NULL;
  char **out = (char **)calloc((size_t)argc + 1, sizeof(char *));
  if (out == NULL) return NULL;
  ptr ls = argv;
  for (int i = 0; i < argc; i += 1) {
    out[i] = copy_scheme_string(Scar(ls));
    if (out[i] == NULL) {
      free_string_array(out);
      return NULL;
    }
    ls = Scdr(ls);
  }
  out[argc] = NULL;
  return out;
}

// Converts an option alist of string pairs into a NULL-terminated environment.
// A false or empty Scheme value means inherit the process environment.
static char **env_from_alist(ptr env) {
  if (env == Sfalse) return environ;
  int count = list_length(env);
  if (count < 0) return NULL;
  char **out = (char **)calloc((size_t)count + 1, sizeof(char *));
  if (out == NULL) return NULL;
  ptr ls = env;
  for (int i = 0; i < count; i += 1) {
    ptr entry = Scar(ls);
    if (!Spairp(entry) || !Sstringp(Scar(entry)) || !Sstringp(Scdr(entry))) {
      free_string_array(out);
      return NULL;
    }
    char *key = copy_scheme_string(Scar(entry));
    char *value = copy_scheme_string(Scdr(entry));
    if (key == NULL || value == NULL) {
      free(key);
      free(value);
      free_string_array(out);
      return NULL;
    }
    size_t key_len = strlen(key);
    size_t value_len = strlen(value);
    out[i] = (char *)malloc(key_len + value_len + 2);
    if (out[i] == NULL) {
      free(key);
      free(value);
      free_string_array(out);
      return NULL;
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

// Check the executable before spawning.  Some emulators report an exec failure
// as a child exit status instead of returning the POSIX spawn error to the parent.
static int executable_access(const char *path, const char *cwd) {
  if (cwd == NULL || cwd[0] == '\0' || path[0] == '/') {
    if (access(path, X_OK) == 0) return 0;
    return errno;
  }
  size_t cwd_len = strlen(cwd);
  size_t path_len = strlen(path);
  char *candidate = (char *)malloc(cwd_len + path_len + 2);
  if (candidate == NULL) return ENOMEM;
  memcpy(candidate, cwd, cwd_len);
  candidate[cwd_len] = '/';
  memcpy(candidate + cwd_len + 1, path, path_len + 1);
  int result = access(candidate, X_OK) == 0 ? 0 : errno;
  free(candidate);
  return result;
}

static char *copy_c_string(const char *value) {
  size_t length = strlen(value);
  char *copy = (char *)malloc(length + 1);
  if (copy == NULL) return NULL;
  memcpy(copy, value, length + 1);
  return copy;
}

// Resolve PATH in the parent, but leave the actual execve to the child.  The
// error pipe below reports exec-time failures without relying on preflight checks.
static int resolve_executable(const char *program, char **env, const char *cwd,
                              char **resolved) {
  if (strchr(program, '/') != NULL) {
    *resolved = copy_c_string(program);
    return *resolved == NULL ? ENOMEM : 0;
  }

  const char *path = environment_value(env, "PATH");
  if (path == NULL) path = "/bin:/usr/bin";
  int saved_errno = ENOENT;
  const char *start = path;
  for (;;) {
    const char *end = strchr(start, ':');
    size_t directory_len = end == NULL ? strlen(start) : (size_t)(end - start);
    size_t program_len = strlen(program);
    size_t separator_len = directory_len == 0 ? 0 : 1;
    char *candidate = (char *)malloc(directory_len + separator_len + program_len + 1);
    if (candidate == NULL) return ENOMEM;
    memcpy(candidate, start, directory_len);
    if (separator_len != 0) candidate[directory_len] = '/';
    memcpy(candidate + directory_len + separator_len, program, program_len + 1);
    int access_error = executable_access(candidate, cwd);
    if (access_error == 0) {
      *resolved = candidate;
      return 0;
    }
    if (access_error == EACCES) saved_errno = EACCES;
    free(candidate);
    if (end == NULL) break;
    start = end + 1;
  }
  return saved_errno;
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

static int spawn_native(pid_t *pid, char *const argv[], char *const env[],
                         const char *program, const char *cwd,
                         int stdin_source, int stdout_source,
                         int stderr_source, int stderr_to_stdout,
                         const int *child_fds, size_t child_fd_count) {
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
    for (size_t i = 0; i < child_fd_count; i += 1) {
      if (child_fds[i] >= 0) close(child_fds[i]);
    }
    execve(program, argv, env);
    child_report_errno(error_pipe[1], errno);
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
/* Caller strings used by fork/exec are copied before allowing Scheme GC. */
static int spawn_checked(pid_t *pid, char *const argv[], char *const env[],
                         const char *program, const char *cwd,
                         int stdin_source, int stdout_source, int stderr_source,
                         int stderr_to_stdout, const int *child_fds, size_t child_fd_count) {
  char *directory = strdup(cwd ? cwd : "");
  if (!directory) return ENOMEM;
  Sdeactivate_thread();
  int error = spawn_native(pid, argv, env, program, directory, stdin_source,
                            stdout_source, stderr_source, stderr_to_stdout,
                            child_fds, child_fd_count);
  Sactivate_thread();
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
                         int timeout_ms, const char *operation) {
  byte_buffer buffers[2] = {{0}, {0}};
  int output_fds[2] = {stdout_fd, stderr_fd};
  int status = 0, reaped = 0, error = 0, timed_out = 0;
  size_t sent = 0;
  int64_t start = monotonic_ms();
  sigset_t pipe_signal, old_mask, pending;
  sigemptyset(&pipe_signal);
  sigaddset(&pipe_signal, SIGPIPE);
  /* Writes can race a child's close. Block only this thread's SIGPIPE, then
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
      pid_t result = waitpid(pid, &status, WNOHANG);
      if (result == pid) { reaped = 1; close_fd(&input_fd); }
      else if (result < 0 && errno != EINTR) { error = errno; break; }
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
        if (!buffer_append(&buffers[i], chunk, (size_t)length)) error = ENOMEM;
      } else if (!length) close_fd(&output_fds[i]);
      else if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) error = errno;
    }
  }
  close_fd(&input_fd);
  close_fd(&output_fds[0]);
  close_fd(&output_fds[1]);
  if ((error || timed_out) && !reaped) (void)terminate_child(pid);
  if (!mask_error) {
    if (!had_pipe) {
      struct timespec immediate = {0, 0};
      while (sigtimedwait(&pipe_signal, NULL, &immediate) < 0 && errno == EINTR) {}
    }
    (void)pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
  }
  Sactivate_thread();
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
  char **cargv = argv_from_list(argv);
  char **cenv = env_from_alist(env);
  if (cargv == NULL || cenv == NULL) {
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    errno = EINVAL;
    return cspp_errno_result("spawn-capture", Snil);
  }
  char *resolved_program = NULL;
  int resolve_error = resolve_executable(cargv[0], cenv, cwd, &resolved_program);
  if (resolve_error != 0) {
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    errno = resolve_error;
    return cspp_errno_result("spawn-capture", Snil);
  }

  int stdin_pipe[2] = {-1, -1};
  int stdout_pipe[2] = {-1, -1};
  int stderr_pipe[2] = {-1, -1};
  int rc = 0;
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
    free(resolved_program);
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    return cspp_errno_result("spawn-capture", Snil);
  }

  pid_t pid;
  int child_fds[] = {stdin_pipe[0], stdin_pipe[1], stdout_pipe[0], stdout_pipe[1],
                     stderr_pipe[0], stderr_pipe[1]};
  Slock_object(stdin_payload);
  rc = spawn_checked(&pid, cargv, cenv, resolved_program, cwd,
                     Sbytevectorp(stdin_payload) ? stdin_pipe[0] : (Ssymbolp(stdin_payload) ? -1 : -2),
                     capture_stdout ? stdout_pipe[1] : (stdout_null ? -2 : -1),
                     capture_stderr ? stderr_pipe[1] : (stderr_null ? -2 : -1),
                     stderr_to_stdout, child_fds, sizeof(child_fds) / sizeof(child_fds[0]));
  free(resolved_program);
  close_fd(&stdin_pipe[0]);
  close_fd(&stdout_pipe[1]);
  close_fd(&stderr_pipe[1]);
  free_string_array(cargv);
  if (cenv != environ) free_string_array(cenv);

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
      Sdeactivate_thread(); terminate_child(pid); Sactivate_thread();
      Sunlock_object(stdin_payload);
      errno = EINVAL; return cspp_errno_result("spawn-capture", Snil);
    }
    input_size = (size_t)Sbytevector_length(stdin_payload);
    input = malloc(input_size ? input_size : 1);
    if (!input) {
      close_fd(&stdin_pipe[1]); close_fd(&stdout_pipe[0]); close_fd(&stderr_pipe[0]);
      Sdeactivate_thread(); terminate_child(pid); Sactivate_thread();
      Sunlock_object(stdin_payload);
      errno = ENOMEM; return cspp_errno_result("spawn-capture", Snil);
    }
    if (input_size) memcpy(input, Sbytevector_data(stdin_payload), input_size);
  }
  Sunlock_object(stdin_payload);
  ptr result = capture_child(pid, stdin_pipe[1], input, input_size,
                             capture_stdout ? stdout_pipe[0] : -1,
                             capture_stderr ? stderr_pipe[0] : -1,
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
                         int stdin_null, int stdout_null, int stderr_null) {
#if defined(__unix__) || defined(__APPLE__)
  char **cargv = argv_from_list(argv);
  char **cenv = env_from_alist(env);
  if (cargv == NULL || cenv == NULL) {
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    errno = EINVAL;
    return cspp_errno_result("spawn-process", Snil);
  }
  char *resolved_program = NULL;
  int resolve_error = resolve_executable(cargv[0], cenv, cwd, &resolved_program);
  if (resolve_error != 0) {
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    errno = resolve_error;
    return cspp_errno_result("spawn-process", Snil);
  }

  pid_t pid;
  int rc = spawn_checked(&pid, cargv, cenv, resolved_program, cwd,
                         stdin_null ? -2 : -1,
                         stdout_null ? -2 : -1,
                         stderr_null ? -2 : -1,
                         0, NULL, 0);
  free(resolved_program);
  free_string_array(cargv);
  if (cenv != environ) free_string_array(cenv);
  if (rc != 0) {
    errno = rc;
    return cspp_errno_result("spawn-process", Snil);
  }

  ptr v = Smake_vector(1, Sfalse);
  Svector_set(v, 0, Sinteger((iptr)pid));
  return cspp_ok(v);
#else
  (void)argv; (void)env; (void)cwd; (void)stdin_null; (void)stdout_null; (void)stderr_null;
  return cspp_unsupported_result("spawn-process");
#endif
}

ptr cspp_waitpid(int pid, int nohang) {
#if defined(__unix__) || defined(__APPLE__)
  int status;
  pid_t r;
  Sdeactivate_thread();
  do {
    r = waitpid((pid_t)pid, &status, nohang ? WNOHANG : 0);
  } while (r < 0 && errno == EINTR);
  int saved_errno = errno;
  Sactivate_thread();
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

// Starts every process in a pipeline and connects adjacent stdout/stdin pipes.
// The caller receives process ids and is responsible for waiting on them.
ptr cspp_spawn_pipeline(ptr specs) {
#if defined(__unix__) || defined(__APPLE__)
  int count = list_length(specs);
  if (count <= 0) {
    errno = EINVAL;
    return cspp_errno_result("spawn-pipeline", Snil);
  }

  char ***argvs = (char ***)calloc((size_t)count, sizeof(char **));
  pid_t *pids = (pid_t *)calloc((size_t)count, sizeof(pid_t));
  int (*pipes)[2] = NULL;
  if (argvs == NULL || pids == NULL) {
    free(argvs);
    free(pids);
    errno = ENOMEM;
    return cspp_errno_result("spawn-pipeline", Snil);
  }
  if (count > 1) {
    pipes = (int (*)[2])calloc((size_t)(count - 1), sizeof(int[2]));
    if (pipes == NULL) {
      free(argvs);
      free(pids);
      errno = ENOMEM;
      return cspp_errno_result("spawn-pipeline", Snil);
    }
    for (int i = 0; i < count - 1; i += 1) {
      pipes[i][0] = -1;
      pipes[i][1] = -1;
      if (make_pipe(pipes[i]) != 0) {
        for (int j = 0; j <= i; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
        free(argvs);
        free(pids);
        free(pipes);
        return cspp_errno_result("spawn-pipeline", Snil);
      }
    }
  }

  ptr ls = specs;
  for (int i = 0; i < count; i += 1) {
    argvs[i] = argv_from_list(Scar(ls));
    if (argvs[i] == NULL) {
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      if (pipes != NULL) {
        for (int j = 0; j < count - 1; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
      }
      free(argvs);
      free(pids);
      free(pipes);
      errno = EINVAL;
      return cspp_errno_result("spawn-pipeline", Snil);
    }
    ls = Scdr(ls);
  }

  int child_fd_count = count > 1 ? 2 * (count - 1) : 0;
  int *child_fds = child_fd_count == 0 ? NULL : (int *)malloc((size_t)child_fd_count * sizeof(int));
  if (child_fd_count != 0 && child_fds == NULL) {
    for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
    for (int j = 0; j < count - 1; j += 1) {
      close_fd(&pipes[j][0]);
      close_fd(&pipes[j][1]);
    }
    free(argvs);
    free(pids);
    free(pipes);
    errno = ENOMEM;
    return cspp_errno_result("spawn-pipeline", Snil);
  }
  for (int j = 0; j < count - 1; j += 1) {
    child_fds[2 * j] = pipes[j][0];
    child_fds[2 * j + 1] = pipes[j][1];
  }

  int spawned = 0;
  for (int i = 0; i < count; i += 1) {
    char *resolved_program = NULL;
    int rc = resolve_executable(argvs[i][0], environ, "", &resolved_program);
    if (rc == 0) {
      rc = spawn_checked(&pids[i], argvs[i], environ, resolved_program, "",
                         i > 0 ? pipes[i - 1][0] : -1,
                         i < count - 1 ? pipes[i][1] : -1,
                         -1, 0, child_fds, (size_t)child_fd_count);
    }
    free(resolved_program);
    if (rc != 0) {
      errno = rc;
      for (int j = 0; j < spawned; j += 1) kill(pids[j], SIGKILL);
      for (int j = 0; j < spawned; j += 1) {
        int status;
        (void)wait_pid_block(pids[j], &status);
      }
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      if (pipes != NULL) {
        for (int j = 0; j < count - 1; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
      }
      free(child_fds);
      free(argvs);
      free(pids);
      free(pipes);
      return cspp_errno_result("spawn-pipeline", Snil);
    }
    spawned += 1;
  }

  if (pipes != NULL) {
    for (int j = 0; j < count - 1; j += 1) {
      close_fd(&pipes[j][0]);
      close_fd(&pipes[j][1]);
    }
  }

  ptr v = Smake_vector(count, Sfalse);
  for (int i = 0; i < count; i += 1) {
    Svector_set(v, i, Sinteger((iptr)pids[i]));
  }
  for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
  free(child_fds);
  free(argvs);
  free(pids);
  free(pipes);
  return cspp_ok(v);
#else
  (void)specs;
  return cspp_unsupported_result("spawn-pipeline");
#endif
}

// Runs a pipeline to completion while capturing the final process stdout. The
// returned status is for the last process, matching the high-level Scheme API.
ptr cspp_spawn_pipeline_capture(ptr specs, int timeout_ms) {
#if defined(__unix__) || defined(__APPLE__)
  int count = list_length(specs);
  if (count <= 0) {
    errno = EINVAL;
    return cspp_errno_result("spawn-pipeline", Snil);
  }

  char ***argvs = (char ***)calloc((size_t)count, sizeof(char **));
  pid_t *pids = (pid_t *)calloc((size_t)count, sizeof(pid_t));
  int (*pipes)[2] = NULL;
  if (argvs == NULL || pids == NULL) {
    free(argvs);
    free(pids);
    errno = ENOMEM;
    return cspp_errno_result("spawn-pipeline", Snil);
  }
  if (count > 1) {
    pipes = (int (*)[2])calloc((size_t)(count - 1), sizeof(int[2]));
    if (pipes == NULL) {
      free(argvs);
      free(pids);
      errno = ENOMEM;
      return cspp_errno_result("spawn-pipeline", Snil);
    }
    for (int i = 0; i < count - 1; i += 1) {
      pipes[i][0] = -1;
      pipes[i][1] = -1;
      if (make_pipe(pipes[i]) != 0) {
        for (int j = 0; j <= i; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
        free(argvs);
        free(pids);
        free(pipes);
        return cspp_errno_result("spawn-pipeline", Snil);
      }
    }
  }

  ptr ls = specs;
  for (int i = 0; i < count; i += 1) {
    argvs[i] = argv_from_list(Scar(ls));
    if (argvs[i] == NULL) {
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      if (pipes != NULL) {
        for (int j = 0; j < count - 1; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
      }
      free(argvs);
      free(pids);
      free(pipes);
      errno = EINVAL;
      return cspp_errno_result("spawn-pipeline", Snil);
    }
    ls = Scdr(ls);
  }

  int capture_pipe[2] = {-1, -1};
  if (make_pipe(capture_pipe) != 0) {
    for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
    if (pipes != NULL) {
      for (int j = 0; j < count - 1; j += 1) {
        close_fd(&pipes[j][0]);
        close_fd(&pipes[j][1]);
      }
    }
    free(argvs);
    free(pids);
    free(pipes);
    return cspp_errno_result("spawn-pipeline", Snil);
  }

  int child_fd_count = 2 * (count - 1) + 2;
  int *child_fds = (int *)malloc((size_t)child_fd_count * sizeof(int));
  if (child_fds == NULL) {
    for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
    for (int j = 0; j < count - 1; j += 1) {
      close_fd(&pipes[j][0]);
      close_fd(&pipes[j][1]);
    }
    close_fd(&capture_pipe[0]);
    close_fd(&capture_pipe[1]);
    free(argvs);
    free(pids);
    free(pipes);
    errno = ENOMEM;
    return cspp_errno_result("spawn-pipeline", Snil);
  }
  for (int j = 0; j < count - 1; j += 1) {
    child_fds[2 * j] = pipes[j][0];
    child_fds[2 * j + 1] = pipes[j][1];
  }
  child_fds[2 * (count - 1)] = capture_pipe[0];
  child_fds[2 * (count - 1) + 1] = capture_pipe[1];

  int spawned = 0;
  for (int i = 0; i < count; i += 1) {
    char *resolved_program = NULL;
    int rc = resolve_executable(argvs[i][0], environ, "", &resolved_program);
    if (rc == 0) {
      rc = spawn_checked(&pids[i], argvs[i], environ, resolved_program, "",
                         i > 0 ? pipes[i - 1][0] : -1,
                         i < count - 1 ? pipes[i][1] : capture_pipe[1],
                         -1, 0, child_fds, (size_t)child_fd_count);
    }
    free(resolved_program);
    if (rc != 0) {
      errno = rc;
      for (int j = 0; j < spawned; j += 1) kill(pids[j], SIGKILL);
      for (int j = 0; j < spawned; j += 1) {
        int status;
        (void)wait_pid_block(pids[j], &status);
      }
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      for (int j = 0; j < count - 1; j += 1) {
        close_fd(&pipes[j][0]);
        close_fd(&pipes[j][1]);
      }
      close_fd(&capture_pipe[0]);
      close_fd(&capture_pipe[1]);
      free(child_fds);
      free(argvs);
      free(pids);
      free(pipes);
      return cspp_errno_result("spawn-pipeline", Snil);
    }
    spawned += 1;
  }

  for (int j = 0; j < count - 1; j += 1) {
    close_fd(&pipes[j][0]);
    close_fd(&pipes[j][1]);
  }
  close_fd(&capture_pipe[1]);

  ptr result = capture_child(pids[count - 1], -1, NULL, 0, capture_pipe[0], -1, timeout_ms, "spawn-pipeline");
  Slock_object(result);
  Sdeactivate_thread();
  for (int i = 0; i < count - 1; i += 1) {
    int status;
    (void)wait_pid_block(pids[i], &status);
  }
  for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
  free(child_fds);
  free(argvs);
  free(pids);
  free(pipes);
  Sactivate_thread();
  Sunlock_object(result);
  return result;
#else
  (void)specs; (void)timeout_ms;
  return cspp_unsupported_result("spawn-pipeline");
#endif
}


