#define _GNU_SOURCE

#include "common.h"

#include <grp.h>
#include <pwd.h>
#include <link.h>
#include <poll.h>
#include <spawn.h>
#if defined(__unix__) || defined(__APPLE__)
#include <signal.h>
#endif
#include <string.h>
#if defined(__unix__) || defined(__APPLE__)
#include <sys/stat.h>
#include <sys/statvfs.h>
#endif
#include <time.h>
#include <sys/utsname.h>

extern char **environ;


ptr cspp_getpwnam(const char *name);
ptr cspp_getpwuid(unsigned int uid);
ptr cspp_getgrnam(const char *name);
ptr cspp_getgrgid(unsigned int gid);

unsigned int cspp_getuid();
unsigned int cspp_getgid();
unsigned int cspp_geteuid();
unsigned int cspp_getegid();

ptr cspp_fork();
ptr cspp_vfork();
int cspp_getppid();
ptr cspp_shared_object_list();

ptr cspp_hostname();
ptr cspp_cpu_arch();
int cspp_cpu_count();


//=======================================================================
//
// credentials
//
//=======================================================================

//=======================================================================
//
// processes
//
//=======================================================================

int cspp_getppid() { return getppid(); }

ptr cspp_fork() {
  int res = fork();
  if (res == -1) {
    return errno_str();
  }

  return Sfixnum(res);
}

ptr cspp_vfork() {
  int res = vfork();
  if (res == -1) {
    return errno_str();
  }

  return Sfixnum(res);
}

static int shared_object_list_callback(struct dl_phdr_info *info, size_t size, void *data) {
  (void)size;
  ptr *objs_addr = (ptr *)data;
  ptr objects = *objs_addr;
  *objs_addr = Scons(Sstring(info->dlpi_name), objects);

  return 0;
}

ptr cspp_shared_object_list() {
  ptr objs = Snil;
  dl_iterate_phdr(shared_object_list_callback, &objs);
  
  return objs;
}

ptr cspp_send_signal(int pid, int sig) {
#if defined(__unix__) || defined(__APPLE__)
  if (kill(pid, sig) != 0) {
    return cspp_errno_result("send-signal", Snil);
  }

  return cspp_ok(Strue);
#else
  (void)pid;
  (void)sig;
  return cspp_unsupported_result("send-signal");
#endif
}

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
  if (buf->len + len > buf->cap) {
    size_t next = buf->cap == 0 ? 4096 : buf->cap;
    while (next < buf->len + len) next *= 2;
    char *new_data = (char *)realloc(buf->data, next);
    if (new_data == NULL) return 0;
    buf->data = new_data;
    buf->cap = next;
  }
  memcpy(buf->data + buf->len, data, len);
  buf->len += len;
  return 1;
}

static ptr buffer_to_string(byte_buffer *buf) {
  if (buf->len == 0) return Sstring("");
  return Sstring_utf8(buf->data, (iptr)buf->len);
}

static char *copy_scheme_string(ptr str) {
  if (!Sstringp(str)) return NULL;
  iptr n = Sstring_length(str);
  char *out = (char *)malloc((size_t)n + 1);
  if (out == NULL) return NULL;
  for (iptr i = 0; i < n; i += 1) {
    uint32_t ch = (uint32_t)Sstring_ref(str, i);
    if (ch > 255) {
      free(out);
      return NULL;
    }
    out[i] = (char)ch;
  }
  out[n] = '\0';
  return out;
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
  if (env == Sfalse || env == Snil) return environ;
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

static int spawn_checked(pid_t *pid, char *const argv[], char *const env[],
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
  if (pipe(fds) != 0) return -1;
  // Keep pipe descriptors above standard streams so spawn file actions cannot alias them.
  for (int i = 0; i < 2; i += 1) {
    if (fds[i] < 3) {
      int replacement = fcntl(fds[i], F_DUPFD, 3);
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

static int write_all_fd(int fd, const unsigned char *data, size_t len) {
  size_t done = 0;
  while (done < len) {
    ssize_t n = write(fd, data + done, len - done);
    if (n < 0) {
      if (errno == EINTR) continue;
      return -1;
    }
    done += (size_t)n;
  }
  return 0;
}

static int write_stdin_payload(int fd, ptr payload) {
  if (payload == Sfalse) return 0;
  if (!Sbytevectorp(payload)) {
    errno = EINVAL;
    return -1;
  }
  return write_all_fd(fd, Sbytevector_data(payload), (size_t)Sbytevector_length(payload));
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

static int wait_pid_timeout(pid_t pid, int *status, int timeout_ms, int64_t start_ms) {
  for (;;) {
    pid_t r = waitpid(pid, status, WNOHANG);
    if (r == pid) return 0;
    if (r < 0) {
      if (errno == EINTR) continue;
      return -1;
    }
    if (timeout_ms >= 0 && monotonic_ms() - start_ms >= timeout_ms) {
      return 1;
    }
    sleep_ms(5);
  }
}

static ptr reap_timeout(pid_t pid, const char *operation) {
  int status;
  kill(pid, SIGTERM);
  for (int i = 0; i < 20; i += 1) {
    pid_t r = waitpid(pid, &status, WNOHANG);
    if (r == pid) return cspp_timeout_result(operation, pid_context(pid));
    if (r < 0 && errno != EINTR) return cspp_errno_result(operation, pid_context(pid));
    sleep_ms(5);
  }
  kill(pid, SIGKILL);
  (void)wait_pid_block(pid, &status);
  return cspp_timeout_result(operation, pid_context(pid));
}

// Drains captured stdout/stderr while polling waitpid so a child cannot block
// on a full pipe. On timeout, the child is terminated and reaped before return.
static ptr capture_child(pid_t pid, int stdout_fd, int stderr_fd, int timeout_ms,
                         const char *operation) {
  byte_buffer out = {0};
  byte_buffer err = {0};
  int status = 0;
  int reaped = 0;
  int64_t start = monotonic_ms();
  char tmp[4096];

  if (stdout_fd >= 0) set_nonblock(stdout_fd);
  if (stderr_fd >= 0) set_nonblock(stderr_fd);

  while (stdout_fd >= 0 || stderr_fd >= 0) {
    if (timeout_ms >= 0 && monotonic_ms() - start >= timeout_ms) {
      close_fd(&stdout_fd);
      close_fd(&stderr_fd);
      buffer_free(&out);
      buffer_free(&err);
      return reap_timeout(pid, operation);
    }

    if (!reaped) {
      pid_t r = waitpid(pid, &status, WNOHANG);
      if (r == pid) reaped = 1;
      else if (r < 0 && errno != EINTR) {
        close_fd(&stdout_fd);
        close_fd(&stderr_fd);
        buffer_free(&out);
        buffer_free(&err);
        return cspp_errno_result(operation, pid_context(pid));
      }
    }

    struct pollfd pfds[2];
    int nfds = 0;
    int out_index = -1;
    int err_index = -1;
    if (stdout_fd >= 0) {
      out_index = nfds;
      pfds[nfds].fd = stdout_fd;
      pfds[nfds].events = POLLIN | POLLHUP;
      pfds[nfds].revents = 0;
      nfds += 1;
    }
    if (stderr_fd >= 0) {
      err_index = nfds;
      pfds[nfds].fd = stderr_fd;
      pfds[nfds].events = POLLIN | POLLHUP;
      pfds[nfds].revents = 0;
      nfds += 1;
    }

    int poll_timeout = 50;
    if (timeout_ms >= 0) {
      int64_t elapsed = monotonic_ms() - start;
      int64_t remaining = timeout_ms - elapsed;
      if (remaining < poll_timeout) poll_timeout = remaining < 0 ? 0 : (int)remaining;
    }

    int pr = poll(pfds, (nfds_t)nfds, poll_timeout);
    if (pr < 0) {
      if (errno == EINTR) continue;
      close_fd(&stdout_fd);
      close_fd(&stderr_fd);
      buffer_free(&out);
      buffer_free(&err);
      return cspp_errno_result(operation, pid_context(pid));
    }

    if (out_index >= 0 && (pfds[out_index].revents & (POLLIN | POLLHUP))) {
      for (;;) {
        ssize_t n = read(stdout_fd, tmp, sizeof(tmp));
        if (n > 0) {
          if (!buffer_append(&out, tmp, (size_t)n)) {
            errno = ENOMEM;
            close_fd(&stdout_fd);
            close_fd(&stderr_fd);
            buffer_free(&out);
            buffer_free(&err);
            return cspp_errno_result(operation, pid_context(pid));
          }
        } else if (n == 0) {
          close_fd(&stdout_fd);
          break;
        } else if (errno == EINTR) {
          continue;
        } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
          break;
        } else {
          close_fd(&stdout_fd);
          close_fd(&stderr_fd);
          buffer_free(&out);
          buffer_free(&err);
          return cspp_errno_result(operation, pid_context(pid));
        }
      }
    }

    if (err_index >= 0 && (pfds[err_index].revents & (POLLIN | POLLHUP))) {
      for (;;) {
        ssize_t n = read(stderr_fd, tmp, sizeof(tmp));
        if (n > 0) {
          if (!buffer_append(&err, tmp, (size_t)n)) {
            errno = ENOMEM;
            close_fd(&stdout_fd);
            close_fd(&stderr_fd);
            buffer_free(&out);
            buffer_free(&err);
            return cspp_errno_result(operation, pid_context(pid));
          }
        } else if (n == 0) {
          close_fd(&stderr_fd);
          break;
        } else if (errno == EINTR) {
          continue;
        } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
          break;
        } else {
          close_fd(&stdout_fd);
          close_fd(&stderr_fd);
          buffer_free(&out);
          buffer_free(&err);
          return cspp_errno_result(operation, pid_context(pid));
        }
      }
    }
  }

  if (!reaped) {
    int wr = timeout_ms >= 0
      ? wait_pid_timeout(pid, &status, timeout_ms, start)
      : wait_pid_block(pid, &status);
    if (wr == 1) {
      buffer_free(&out);
      buffer_free(&err);
      return reap_timeout(pid, operation);
    }
    if (wr < 0) {
      buffer_free(&out);
      buffer_free(&err);
      return cspp_errno_result(operation, pid_context(pid));
    }
  }

  ptr v = Smake_vector(4, Sfalse);
  Svector_set(v, 0, Sinteger((iptr)pid));
  Svector_set(v, 1, Sinteger(status));
  Svector_set(v, 2, buffer_to_string(&out));
  Svector_set(v, 3, buffer_to_string(&err));
  buffer_free(&out);
  buffer_free(&err);
  return cspp_ok(v);
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
  if (stdin_payload != Sfalse) {
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
  rc = spawn_checked(&pid, cargv, cenv, resolved_program, cwd,
                     stdin_payload != Sfalse ? stdin_pipe[0] : -2,
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
    errno = rc;
    close_fd(&stdin_pipe[1]);
    close_fd(&stdout_pipe[0]);
    close_fd(&stderr_pipe[0]);
    return cspp_errno_result("spawn-capture", Snil);
  }

  if (stdin_pipe[1] >= 0) {
    if (write_stdin_payload(stdin_pipe[1], stdin_payload) != 0) {
      close_fd(&stdin_pipe[1]);
      close_fd(&stdout_pipe[0]);
      close_fd(&stderr_pipe[0]);
      return reap_timeout(pid, "spawn-capture");
    }
    close_fd(&stdin_pipe[1]);
  }

  return capture_child(pid,
                       capture_stdout ? stdout_pipe[0] : -1,
                       capture_stderr ? stderr_pipe[0] : -1,
                       timeout_ms,
                       "spawn-capture");
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
  do {
    r = waitpid((pid_t)pid, &status, nohang ? WNOHANG : 0);
  } while (r < 0 && errno == EINTR);
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

  ptr result = capture_child(pids[count - 1], capture_pipe[0], -1, timeout_ms, "spawn-pipeline");
  for (int i = 0; i < count - 1; i += 1) {
    int status;
    (void)wait_pid_block(pids[i], &status);
  }
  for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
  free(child_fds);
  free(argvs);
  free(pids);
  free(pipes);
  return result;
#else
  (void)specs; (void)timeout_ms;
  return cspp_unsupported_result("spawn-pipeline");
#endif
}


//=======================================================================
//
// system info
//
//=======================================================================


ptr cspp_hostname() {
  struct utsname sysinfo;
  if (uname(&sysinfo) == 0) {
    return Sstring(sysinfo.nodename);
  }
  return errno_str_vector();
}


ptr cspp_cpu_arch() {
  struct utsname sysinfo;
  if (uname(&sysinfo) == 0) {
    return Sstring(sysinfo.machine);
  }
  return errno_str_vector();
}


int cspp_cpu_count() {
  long num_cores = sysconf(_SC_NPROCESSORS_ONLN); 
  if (num_cores == -1) {
    // just return a fallback value
    return 1;
  }

  return (int) num_cores;
}


//=======================================================================
//
// filesystem info
//
//=======================================================================


ptr cspp_filesystem_info(const char *path) {
#if defined(__unix__) || defined(__APPLE__)
  struct statvfs vfs;
  if (statvfs(path, &vfs) != 0) {
    return cspp_errno_result("filesystem-info", Snil);
  }

  struct stat st;
  if (stat(path, &st) != 0) {
    return cspp_errno_result("filesystem-info", Snil);
  }

  ptr v = Smake_vector(10, Sfalse);
  Svector_set(v, 0, Sstring(path));
  Svector_set(v, 1, Sunsigned64((Suint64_t)st.st_dev));
  Svector_set(v, 2, Sfalse);
  Svector_set(v, 3, Sunsigned64((Suint64_t)vfs.f_frsize));
  Svector_set(v, 4, Sunsigned64((Suint64_t)vfs.f_blocks));
  Svector_set(v, 5, Sunsigned64((Suint64_t)vfs.f_bfree));
  Svector_set(v, 6, Sunsigned64((Suint64_t)vfs.f_bavail));
  Svector_set(v, 7, Sunsigned64((Suint64_t)vfs.f_files));
  Svector_set(v, 8, Sunsigned64((Suint64_t)vfs.f_ffree));
#ifdef ST_RDONLY
  Svector_set(v, 9, (vfs.f_flag & ST_RDONLY) ? Strue : Sfalse);
#else
  Svector_set(v, 9, Sfalse);
#endif

  return cspp_ok(v);
#else
  (void)path;
  return cspp_unsupported_result("filesystem-info");
#endif
}
