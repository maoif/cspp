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

static int spawn_begin(pid_t *pid, int *error_fd, char *const argv[], char *const env[],
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
  int flags = fcntl(error_pipe[0], F_GETFL, 0);
  if (flags < 0 || fcntl(error_pipe[0], F_SETFL, flags | O_NONBLOCK) < 0) {
    int error = errno;
    close(error_pipe[0]);
    close(error_pipe[1]);
    return error;
  }

  /* A fork child must never enter inherited Chez handlers. Block delivery until
   * caught dispositions have been reset to exec's defaults. Preserve ignored
   * signals except SIGPIPE, and restore the spawning thread's original mask. */
  sigset_t blocked, original_mask;
  sigfillset(&blocked);
  int mask_error = pthread_sigmask(SIG_SETMASK, &blocked, &original_mask);
  if (mask_error) {
    close(error_pipe[0]);
    close(error_pipe[1]);
    return mask_error;
  }
  pid_t child = fork();
  int fork_error = errno;
  if (child != 0) (void)pthread_sigmask(SIG_SETMASK, &original_mask, NULL);
  if (child < 0) {
    close(error_pipe[0]);
    close(error_pipe[1]);
    return fork_error;
  }
  if (child == 0) {
    close(error_pipe[0]);
    // Chez may be multithreaded.  After fork, the child must avoid Scheme,
    // allocation, stdio, and locks; only async-signal-safe calls run until execve.
    struct sigaction action = {0};
    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    for (int number = 1; number < NSIG; number++) {
      struct sigaction inherited;
      if (number == SIGKILL || number == SIGSTOP) continue;
      if (sigaction(number, NULL, &inherited) != 0) {
        if (errno == EINVAL) continue; /* libc-reserved/unavailable signal. */
        child_report_errno(error_pipe[1], errno);
      }
      if ((number == SIGPIPE ||
           (inherited.sa_handler != SIG_DFL && inherited.sa_handler != SIG_IGN)) &&
          sigaction(number, &action, NULL) != 0)
        child_report_errno(error_pipe[1], errno);
    }
    if (sigprocmask(SIG_SETMASK, &original_mask, NULL) != 0)
      child_report_errno(error_pipe[1], errno);
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
  *pid = child;
  *error_fd = error_pipe[0];
  return 0;
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


/* One native-owned operation per Scheme dynamic-wind scope. No Scheme object
 * survives preparation. Steps and disposal use __collect_safe, so Chez supplies
 * Sdeactivate_thread()/Sactivate_thread() around them; neither touches Scheme.
 * A poll step sleeps at most 20ms and returns to Scheme for pending handlers. */
typedef struct {
  const char *operation;
  int live, pipeline, capture, error, timed_out;
  int count, launched, next, exec_fd, exec_error;
  size_t exec_received;
  char ***argvs, ***candidates, **env, *cwd;
  pid_t *pids, final_pid, wait_pid;
  int wait_fd;
  int stdin_mode, stdout_mode, stderr_mode, stderr_to_stdout;
  int live_fds[3], input_fd, output_fds[2], connection_fd, capture_pipe[2];
  unsigned char *input;
  size_t input_size, sent;
  int retain[2], timeout_ms, status;
  int64_t deadline;
  byte_buffer buffers[2];
} process_operation;

uptr cspp_process_operation_create(void) {
  process_operation *op = calloc(1, sizeof(*op));
  if (!op) return 0;
  op->exec_fd = op->input_fd = op->connection_fd = -1;
  op->wait_fd = -1;
  op->capture_pipe[0] = op->capture_pipe[1] = -1;
  op->output_fds[0] = op->output_fds[1] = -1;
  for (int i = 0; i < 3; i++) op->live_fds[i] = -1;
  op->timeout_ms = -1;
  return (uptr)op;
}

static int prepare_commands(process_operation *op, ptr specs, ptr env, const char *cwd) {
  op->count = list_length(specs);
  if (op->count <= 0) return EINVAL;
  op->argvs = calloc((size_t)op->count, sizeof(*op->argvs));
  op->candidates = calloc((size_t)op->count, sizeof(*op->candidates));
  op->pids = calloc((size_t)op->count, sizeof(*op->pids));
  op->cwd = strdup(cwd ? cwd : "");
  if (!op->argvs || !op->candidates || !op->pids || !op->cwd) return ENOMEM;
  op->env = env_from_alist(env);
  if (!op->env) return errno;
  ptr remaining = specs;
  for (int i = 0; i < op->count; i++, remaining = Scdr(remaining)) {
    op->argvs[i] = argv_from_list(Scar(remaining));
    if (!op->argvs[i]) return errno;
  }
  return 0;
}

void cspp_process_prepare_spawn(uptr handle, ptr argv, ptr env, const char *cwd,
                                int stdin_mode, int stdout_mode, int stderr_mode) {
  process_operation *op = (process_operation *)handle;
  op->operation = "process-spawn";
  op->live = 1;
  op->stdin_mode = stdin_mode;
  op->stdout_mode = stdout_mode;
  op->stderr_mode = stderr_mode;
  op->stderr_to_stdout = stderr_mode == 3;
  op->error = prepare_commands(op, Scons(argv, Snil), env, cwd);
}

void cspp_process_prepare_capture(uptr handle, ptr argv, ptr env, const char *cwd,
                                  ptr input, int stdout_mode, int stderr_mode,
                                  int stdout_null, int stderr_null, int stderr_to_stdout,
                                  int timeout_ms) {
  process_operation *op = (process_operation *)handle;
  op->operation = "spawn-capture";
  op->capture = 1;
  op->timeout_ms = timeout_ms;
  op->stdout_mode = stdout_mode ? 2 : stdout_null ? 1 : 0;
  op->stderr_mode = stderr_mode ? 2 : stderr_null ? 1 : 0;
  op->stderr_to_stdout = stderr_to_stdout;
  op->retain[0] = stdout_mode == 1;
  op->retain[1] = stderr_mode == 1;
  op->stdin_mode = Sbytevectorp(input) ? 2 : Ssymbolp(input) ? 0 : 1;
  op->error = prepare_commands(op, Scons(argv, Snil), env, cwd);
  if (op->error) return;
  if (Sbytevectorp(input)) {
    op->input_size = (size_t)Sbytevector_length(input);
    op->input = malloc(op->input_size ? op->input_size : 1);
    if (!op->input) { op->error = ENOMEM; return; }
    if (op->input_size) memcpy(op->input, Sbytevector_data(input), op->input_size);
  }
}

/* mode: 0 live pipeline, 1 captured stdout, 2 discarded stdout. */
void cspp_process_prepare_pipeline(uptr handle, ptr specs, int mode) {
  process_operation *op = (process_operation *)handle;
  op->operation = "spawn-pipeline";
  op->pipeline = 1;
  op->live = mode == 0;
  op->capture = mode == 1;
  op->retain[0] = mode == 1;
  op->stdout_mode = mode == 0 ? 0 : mode == 1 ? 2 : 1;
  op->error = prepare_commands(op, specs, Sfalse, "");
}

#if defined(__unix__) || defined(__APPLE__)
static int launch_stage(process_operation *op) {
  int pipes[3][2] = {{-1, -1}, {-1, -1}, {-1, -1}};
  int next_pipe[2] = {-1, -1};
  int sources[3] = {-1, -1, -1};
  int error = executable_candidates(op->argvs[op->next][0], op->env,
                                     &op->candidates[op->next]);
  if (error) return error;
  if (op->pipeline) {
    if (op->next < op->count - 1) {
      if (make_pipe(next_pipe) < 0) return errno;
      sources[1] = next_pipe[1];
    } else if (op->capture) {
      if (make_pipe(op->capture_pipe) < 0) return errno;
      sources[1] = op->capture_pipe[1];
    } else if (!op->live) sources[1] = -2;
    sources[0] = op->connection_fd;
  } else {
    int modes[3] = {op->stdin_mode, op->stdout_mode, op->stderr_mode};
    for (int i = 0; i < 3; i++) {
      if (i == 2 && op->stderr_to_stdout) continue;
      if (modes[i] == 2) {
        if (make_pipe(pipes[i]) < 0) { error = errno; goto done; }
        sources[i] = pipes[i][i == 0 ? 0 : 1];
      } else sources[i] = modes[i] == 1 ? -2 : -1;
    }
  }
  error = spawn_begin(&op->pids[op->next], &op->exec_fd,
                      op->argvs[op->next], op->env, op->candidates[op->next], op->cwd,
                      sources[0], sources[1], sources[2], op->stderr_to_stdout);
  if (error) goto done;
  op->launched++;
  op->final_pid = op->pids[op->next];
  if (op->pipeline) {
    close_fd(&op->connection_fd);
    close_fd(&next_pipe[1]);
    op->connection_fd = next_pipe[0];
    next_pipe[0] = -1;
    close_fd(&op->capture_pipe[1]);
  } else {
    for (int i = 0; i < 3; i++) {
      int child_end = i == 0 ? 0 : 1;
      close_fd(&pipes[i][child_end]);
      int parent_end = pipes[i][1 - child_end];
      pipes[i][1 - child_end] = -1;
      if (op->live) op->live_fds[i] = parent_end;
      else if (i == 0) op->input_fd = parent_end;
      else op->output_fds[i - 1] = parent_end;
    }
  }
 done:
  for (int i = 0; i < 3; i++) {
    close_fd(&pipes[i][0]);
    close_fd(&pipes[i][1]);
  }
  close_fd(&next_pipe[0]);
  close_fd(&next_pipe[1]);
  return error;
}

static int finish_exec_step(process_operation *op) {
  struct pollfd event = {op->exec_fd, POLLIN, 0};
  int ready = poll(&event, 1, 20);
  if (ready < 0) return errno == EINTR ? 0 : -errno;
  if (!ready) return 0;
  ssize_t received = read(op->exec_fd, (unsigned char *)&op->exec_error + op->exec_received,
                          sizeof(op->exec_error) - op->exec_received);
  if (received < 0)
    return errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK ? 0 : -errno;
  if (received > 0) {
    op->exec_received += (size_t)received;
    return op->exec_received == sizeof(op->exec_error) ? -op->exec_error : 0;
  }
  close_fd(&op->exec_fd);
  return op->exec_received ? -EIO : 1;
}

static int reap_step(process_operation *op, int index) {
  if (!op->pids[index]) return 1;
  int status;
  pid_t pid = waitpid(op->pids[index], &status, WNOHANG);
  if (pid > 0) {
    op->pids[index] = 0; /* No cleanup path may signal this PID after reaping. */
    if (pid == op->wait_pid) {
      close_fd(&op->wait_fd);
      op->wait_pid = 0;
    }
    if (index == op->count - 1) op->status = status;
    return 1;
  }
  return pid == 0 || errno == EINTR ? 0 : -errno;
}

/* Only one bounded read/write per descriptor per step, so an active producer
 * cannot starve stdin, the deadline, or the Scheme event checkpoint. */
static int capture_step(process_operation *op) {
  int reaped = reap_step(op, op->count - 1);
  if (reaped < 0) { op->error = -reaped; return 1; }
  if (reaped) close_fd(&op->input_fd);
  if (reaped && op->output_fds[0] < 0 && op->output_fds[1] < 0) {
    int finished = 1;
    for (int index = 0; index < op->count - 1; index++) {
      int result = reap_step(op, index);
      if (result < 0) { op->error = -result; return 1; }
      if (!result) finished = 0;
    }
    if (finished) return 1;
  }
  int64_t remaining = op->timeout_ms < 0 ? 20 : op->deadline - monotonic_ms();
  if (remaining <= 0) { op->timed_out = 1; return 1; }
  /* Once I/O is finished, pipe readiness cannot wake us for child exit. Own
   * one pidfd at a time, including earlier pipeline stages after the final
   * stage exits. Unavailable pidfds use short polls without busy waiting. */
  int no_io = op->input_fd < 0 && op->output_fds[0] < 0 && op->output_fds[1] < 0;
  if (no_io && !op->wait_pid) {
    /* Reaping checks the final stage first. Its notification must also be
     * selected first: an exited upstream stage would otherwise stay readable. */
    for (int index = op->count - 1; index >= 0; index--) {
      if (!op->pids[index]) continue;
      op->wait_pid = op->pids[index];
      op->wait_fd = cspp_process_wait_open((int)op->wait_pid);
      break;
    }
  }
  int interval = no_io && op->wait_fd < 0 ? 5 : 20;
  struct pollfd events[4] = {
    {op->input_fd, POLLOUT, 0}, {op->output_fds[0], POLLIN, 0},
    {op->output_fds[1], POLLIN, 0}, {op->wait_fd, POLLIN, 0}
  };
  int ready = poll(events, 4, remaining < interval ? (int)remaining : interval);
  if (ready < 0) { if (errno != EINTR) op->error = errno; return op->error != 0; }
  if (op->input_fd >= 0 && events[0].revents) {
    /* Block SIGPIPE only around the write, preserving any instance that was
     * already pending and restoring the thread's mask before returning to Scheme. */
    sigset_t pipe_signal, previous, pending;
    sigemptyset(&pipe_signal);
    sigaddset(&pipe_signal, SIGPIPE);
    int mask_error = pthread_sigmask(SIG_BLOCK, &pipe_signal, &previous);
    if (mask_error) { op->error = mask_error; return 1; }
    int had_pipe = sigpending(&pending) == 0 && sigismember(&pending, SIGPIPE);
    size_t length = op->input_size - op->sent;
    if (length > 65536) length = 65536;
    ssize_t written = write(op->input_fd, op->input + op->sent, length);
    int write_error = written < 0 ? errno : 0;
    if (write_error == EPIPE && !had_pipe) {
      struct timespec immediate = {0, 0};
      while (sigtimedwait(&pipe_signal, NULL, &immediate) < 0 && errno == EINTR) {}
    }
    (void)pthread_sigmask(SIG_SETMASK, &previous, NULL);
    if (written > 0) {
      op->sent += (size_t)written;
      if (op->sent == op->input_size) close_fd(&op->input_fd);
    } else if (write_error == EPIPE) close_fd(&op->input_fd);
    else if (write_error && write_error != EINTR && write_error != EAGAIN &&
             write_error != EWOULDBLOCK) op->error = write_error;
  }
  for (int i = 0; i < 2 && !op->error; i++) {
    if (op->output_fds[i] < 0 || !events[i + 1].revents) continue;
    char bytes[65536];
    ssize_t count = read(op->output_fds[i], bytes, sizeof(bytes));
    if (count > 0) {
      if (op->retain[i] && !buffer_append(&op->buffers[i], bytes, (size_t)count))
        op->error = ENOMEM;
    } else if (!count) close_fd(&op->output_fds[i]);
    else if (errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) op->error = errno;
  }
  return op->error != 0;
}
#endif

int cspp_process_operation_step(uptr handle) {
  process_operation *op = (process_operation *)handle;
#if defined(__unix__) || defined(__APPLE__)
  if (op->error || op->timed_out) return 1;
  if (op->next < op->count) {
    if (op->exec_fd < 0) {
      op->error = launch_stage(op);
      if (op->error) return 1;
    }
    int result = finish_exec_step(op);
    if (result < 0) { op->error = -result; return 1; }
    if (!result) return 0;
    op->next++;
    op->exec_received = 0;
    op->exec_error = 0;
    if (op->next < op->count) return 0;
    if (op->live) return 1;
    if (op->pipeline) {
      op->output_fds[0] = op->capture_pipe[0];
      op->capture_pipe[0] = -1;
    }
    op->deadline = monotonic_ms() + op->timeout_ms;
    if (!op->input_size) close_fd(&op->input_fd);
    if (op->input_fd >= 0 && set_nonblock(op->input_fd) < 0) op->error = errno;
    for (int i = 0; i < 2; i++)
      if (op->output_fds[i] >= 0 && set_nonblock(op->output_fds[i]) < 0) op->error = errno;
    return op->error != 0;
  }
  return op->live ? 1 : capture_step(op);
#else
  return 1;
#endif
}

ptr cspp_process_operation_result(uptr handle) {
  process_operation *op = (process_operation *)handle;
#if defined(__unix__) || defined(__APPLE__)
  if (op->error) {
    errno = op->error;
    return cspp_errno_result(op->operation, Snil);
  }
  if (op->timed_out)
    return cspp_timeout_result(op->operation,
      Scons(Scons(Sstring("pid"), Sinteger(op->final_pid)), Snil));
  ptr value;
  if (op->live && op->pipeline) {
    value = Smake_vector(op->count, Sfalse);
    for (int i = 0; i < op->count; i++) Svector_set(value, i, Sinteger(op->pids[i]));
  } else {
    value = Smake_vector(4, Sfalse);
    Svector_set(value, 0, Sinteger(op->final_pid));
    if (op->live) {
      for (int i = 0; i < 3; i++) Svector_set(value, i + 1, Sinteger(op->live_fds[i]));
    } else {
      Svector_set(value, 1, Sinteger(op->status));
      Svector_set(value, 2, buffer_to_bytes(&op->buffers[0]));
      Svector_set(value, 3, buffer_to_bytes(&op->buffers[1]));
    }
  }
  ptr result = cspp_ok(value);
  if (op->live) {
    /* The Scheme consumer receives this result with interrupts disabled and
     * publishes it inside an already-installed continuation-aware cleanup scope. */
    for (int i = 0; i < op->count; i++) op->pids[i] = 0;
    for (int i = 0; i < 3; i++) op->live_fds[i] = -1;
  }
  return result;
#else
  return cspp_unsupported_result(op->operation);
#endif
}

#if defined(__unix__) || defined(__APPLE__)
/* Retain the existing command-timeout grace period. Disposal is collect-safe
 * and may take longer than one normal step while terminating/reaping children. */
static void terminate_timed_out_child(pid_t pid) {
  kill(pid, SIGTERM);
  for (int attempt = 0; attempt < 20; attempt++) {
    int status;
    pid_t result = waitpid(pid, &status, WNOHANG);
    if (result == pid || (result < 0 && errno == ECHILD)) return;
    struct timespec interval = {0, 5000000};
    while (nanosleep(&interval, &interval) < 0 && errno == EINTR) {}
  }
  kill(pid, SIGKILL);
  int status;
  while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
}
#endif

void cspp_process_operation_dispose(uptr handle) {
  process_operation *op = (process_operation *)handle;
  close_fd(&op->wait_fd);
  close_fd(&op->exec_fd);
  close_fd(&op->input_fd);
  close_fd(&op->connection_fd);
  for (int i = 0; i < 3; i++) close_fd(&op->live_fds[i]);
  for (int i = 0; i < 2; i++) {
    close_fd(&op->output_fds[i]);
    close_fd(&op->capture_pipe[i]);
    buffer_free(&op->buffers[i]);
  }
#if defined(__unix__) || defined(__APPLE__)
  if (op->timed_out && op->pids[op->count - 1]) {
    terminate_timed_out_child(op->pids[op->count - 1]);
    op->pids[op->count - 1] = 0;
  }
  /* Only unreaped PIDs remain nonzero. Kill every owned child before waiting,
   * including stages that never write to their now-closed pipeline connection. */
  for (int i = 0; i < op->launched; i++) if (op->pids[i]) kill(op->pids[i], SIGKILL);
  for (int i = 0; i < op->launched; i++) {
    if (!op->pids[i]) continue;
    int status;
    while (waitpid(op->pids[i], &status, 0) < 0 && errno == EINTR) {}
  }
#endif
  for (int i = 0; i < op->count; i++) {
    if (op->argvs) free_string_array(op->argvs[i]);
    if (op->candidates) free_string_array(op->candidates[i]);
  }
  free(op->argvs);
  free(op->candidates);
  free(op->pids);
  free_string_array(op->env);
  free(op->cwd);
  free(op->input);
  free(op);
}
static ptr pid_context(pid_t pid) {
  return Scons(Scons(Sstring("pid"), Sinteger((iptr)pid)), Snil);
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
