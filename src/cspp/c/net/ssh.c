#include "../build-config.h"
#include "../common.h"
#include "../optional_library.h"

#include <dirent.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/stat.h>
#if CSPP_WITH_LIBSSH
#define SSH_SUPPRESS_DEPRECATED
#include <libssh/libssh.h>
#include <libssh/sftp.h>
#endif

typedef struct {
  ssh_session session;
} cspp_ssh_session;

typedef struct {
  ssh_channel channel;
  cspp_ssh_session *owner;
} cspp_ssh_channel;

typedef struct {
  sftp_session sftp;
  cspp_ssh_session *owner;
} cspp_sftp_session;

typedef struct {
  sftp_file file;
  cspp_sftp_session *owner;
#if LIBSSH_VERSION_INT >= SSH_VERSION_INT(0, 11, 0)
  sftp_aio pending_read;
  size_t pending_read_len;
  sftp_aio pending_write;
  size_t pending_write_len;
#endif
} cspp_sftp_file;

typedef struct {
  sftp_dir dir;
  cspp_sftp_session *owner;
} cspp_sftp_directory;

typedef struct {
  cspp_ssh_session *owner;
  ssh_channel channel;
  int fd;
  int direction;
  int phase;
  int initialized;
  int cancelled;
  int local_fd;
  int completed;
  int wait_events;
  size_t buffer_len;
  size_t buffer_pos;
  size_t header_len;
  size_t header_pos;
  uint64_t remaining;
  uint64_t offset;
  char *local_path;
  char *remote_name;
  char *command;
  /* Batch transfers share one channel for sibling files. No Scheme pointers persist. */
  char **batch_names;
  size_t batch_count;
  size_t batch_index;
  char *local_directory;
  char *remote_directory;
  unsigned char header[1024];
  unsigned char buffer[65536];
} cspp_scp_transfer;



static cspp_optional_library ssh_library =
    CSPP_OPTIONAL_LIBRARY_INIT("ssh");
static pthread_once_t ssh_once = PTHREAD_ONCE_INIT;
static int ssh_available_flag;
static int ssh_aio_available;

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr ssh_would_block_status(ssh_session session, int fallback_flags) {
  int flags = ssh_get_poll_flags(session);
  ptr events = Snil;
  if ((flags & SSH_WRITE_PENDING) != 0)
    events = Scons(Sstring_to_symbol("write"), events);
  if ((flags & SSH_READ_PENDING) != 0)
    events = Scons(Sstring_to_symbol("read"), events);
  if (Snullp(events)) {
    if ((fallback_flags & POLLOUT) != 0)
      events = Scons(Sstring_to_symbol("write"), events);
    if ((fallback_flags & POLLIN) != 0)
      events = Scons(Sstring_to_symbol("read"), events);
  }
  return make_status("would-block", events);
}

static ptr make_error_status_message(const char *msg) {
  return make_status("error", msg == NULL ? Sstring("SSH error") : Sstring(msg));
}

static ptr make_errno_status(void) { return make_status("error", errno_str()); }

static int configure_default_ssh_paths(ssh_session session) {
  char *home = cspp_environment_copy("HOME");
  char ssh_dir[PATH_MAX];
  char known_hosts[PATH_MAX];
  int n;

  if (home == NULL || *home == 0) { free(home); return 1; }

  n = snprintf(ssh_dir, sizeof(ssh_dir), "%s/.ssh", home);
  if (n <= 0 || (size_t)n >= sizeof(ssh_dir)) { free(home); return 1; }
  free(home);
  n = snprintf(known_hosts, sizeof(known_hosts), "%s/known_hosts", ssh_dir);
  if (n <= 0 || (size_t)n >= sizeof(known_hosts)) return 1;

  return ssh_options_set(session, SSH_OPTIONS_SSH_DIR, ssh_dir) == SSH_OK &&
         ssh_options_set(session, SSH_OPTIONS_KNOWNHOSTS, known_hosts) == SSH_OK;
}

static int add_default_identities(ssh_session session) {
  static const char *names[] = {"id_ed25519", "id_ecdsa", "id_rsa", "id_dsa", NULL};
  char *home = cspp_environment_copy("HOME");
  char path[PATH_MAX];
  int i;

  if (home == NULL || *home == 0) { free(home); return 1; }

  for (i = 0; names[i] != NULL; ++i) {
    int n = snprintf(path, sizeof(path), "%s/.ssh/%s", home, names[i]);
    if (n <= 0 || (size_t)n >= sizeof(path)) continue;
    if (access(path, R_OK) != 0) continue;
    if (ssh_options_set(session, SSH_OPTIONS_ADD_IDENTITY, path) != SSH_OK) { free(home); return 0; }
  }

  free(home);
  return 1;
}



static void initialize_ssh(void) {
  const char *version;
  unsigned major;
  unsigned minor;
  unsigned patch;

  version = ssh_version(SSH_VERSION_INT(0, 10, 0));
  if (version == NULL || sscanf(version, "%u.%u.%u", &major, &minor, &patch) != 3) {
    cspp_optional_library_fail(
        &ssh_library, "ssh: unable to parse runtime version %s",
        version == NULL ? "(null)" : version);
    return;
  }
  cspp_optional_library_set_version(&ssh_library, version);
  if (major == 0 && minor < 10) {
    cspp_optional_library_fail(
        &ssh_library, "ssh: runtime version %s requires libssh >= 0.10.0",
        version);
    return;
  }

#if LIBSSH_VERSION_INT >= SSH_VERSION_INT(0, 11, 0)
  ssh_aio_available = 1;
#endif
  ssh_available_flag = 1;
}

static int ssh_available(void) {
  pthread_once(&ssh_once, initialize_ssh);
  return ssh_available_flag;
}

const cspp_optional_library *cspp_net_ssh_library(void) {
  (void)ssh_available();
  return &ssh_library;
}

unsigned cspp_net_ssh_capabilities(void) {
  (void)ssh_available();
  return ssh_aio_available ? 1U : 0U;
}

static ptr ssh_error_status(ssh_session session, const char *fallback) {
  const char *msg = NULL;
  if (session != NULL) msg = ssh_get_error(session);
  return make_error_status_message(msg == NULL || *msg == 0 ? fallback : msg);
}

static ptr ssh_error_status_from_wrapper(cspp_ssh_session *wrapper, const char *fallback) {
  return ssh_error_status(wrapper == NULL ? NULL : wrapper->session, fallback);
}

static ptr ssh_channel_error_status(cspp_ssh_channel *wrapper, const char *fallback) {
  if (wrapper == NULL || wrapper->owner == NULL) return make_error_status_message(fallback);
  return ssh_error_status(wrapper->owner->session, fallback);
}

static ptr sftp_error_status(cspp_sftp_session *wrapper, const char *fallback) {
  char buffer[128];
  if (wrapper != NULL && wrapper->owner != NULL) {
    const char *msg = ssh_get_error(wrapper->owner->session);
    if (msg != NULL && *msg != 0) return make_error_status_message(msg);
  }
  if (wrapper != NULL && wrapper->sftp != NULL) {
    snprintf(buffer, sizeof(buffer), "%s (sftp code %d)", fallback, sftp_get_error(wrapper->sftp));
    return make_error_status_message(buffer);
  }
  return make_error_status_message(fallback);
}

static ptr sftp_file_error_status(cspp_sftp_file *wrapper, const char *fallback) {
  if (wrapper == NULL || wrapper->owner == NULL) return make_error_status_message(fallback);
  return sftp_error_status(wrapper->owner, fallback);
}

static ptr make_ssh_handle(uptr handle) { return Sunsigned(handle); }

static int64_t monotonic_ms(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return -1;
  return (int64_t)ts.tv_sec * 1000 + (int64_t)(ts.tv_nsec / 1000000);
}

static ptr wait_ssh_session_until(ssh_session session, short events, int64_t deadline,
                                  const char *timeout_msg) {
  struct pollfd pfd;

  if (session == NULL) return make_error_status_message("invalid ssh session");
  pfd.fd = (int)ssh_get_fd(session);
  if (pfd.fd < 0) return make_error_status_message("failed to query ssh session socket");
  pfd.events = events;
  pfd.revents = 0;

  for (;;) {
    int wait_ms = -1;
    int rc;
    if (deadline >= 0) {
      int64_t now = monotonic_ms();
      int64_t remaining;
      if (now < 0) return make_errno_status();
      remaining = deadline - now;
      if (remaining <= 0) return make_error_status_message(timeout_msg);
      wait_ms = remaining > INT_MAX ? INT_MAX : (int)remaining;
    }
    rc = poll(&pfd, 1, wait_ms);
    if (rc > 0) {
      if ((pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) != 0)
        return make_error_status_message("ssh session socket failed while waiting for readiness");
      if ((pfd.revents & events) != 0) return Strue;
      continue;
    }
    if (rc == 0) return make_error_status_message(timeout_msg);
    if (errno == EINTR) continue;
    return make_errno_status();
  }
}

static ptr sftp_attr_to_vector(sftp_attributes attr) {
  ptr v = Smake_vector(8, Sfalse);
  Svector_set(v, 0, attr->name == NULL ? Sfalse : Sstring_utf8(attr->name, (iptr)strlen(attr->name)));
  Svector_set(v, 1, Sfixnum((iptr)attr->type));
  Svector_set(v, 2, Sunsigned64(attr->size));
  Svector_set(v, 3, Sunsigned((uptr)attr->permissions));
  Svector_set(v, 4, Sunsigned((uptr)attr->uid));
  Svector_set(v, 5, Sunsigned((uptr)attr->gid));
  Svector_set(v, 6, Sunsigned64(attr->atime64 != 0 ? attr->atime64 : attr->atime));
  Svector_set(v, 7, Sunsigned64(attr->mtime64 != 0 ? attr->mtime64 : attr->mtime));
  return v;
}

#define CSPP_SCP_CHUNK_SIZE 65536

typedef struct {
  char **items;
  size_t len;
  size_t cap;
} cspp_path_stack;

static ptr make_path_error_status(const char *prefix, const char *path) {
  char buffer[PATH_MAX + 128];
  if (path == NULL) return make_error_status_message(prefix);
  snprintf(buffer, sizeof(buffer), "%s: %s", prefix, path);
  return make_error_status_message(buffer);
}

static char *dup_cstring(const char *s) {
  size_t len;
  char *out;
  if (s == NULL) return NULL;
  len = strlen(s);
  out = (char *)malloc(len + 1);
  if (out == NULL) return NULL;
  memcpy(out, s, len + 1);
  return out;
}

static char *dup_cstring_n(const char *s, size_t len) {
  char *out = (char *)malloc(len + 1);
  if (out == NULL) return NULL;
  memcpy(out, s, len);
  out[len] = 0;
  return out;
}

static int is_safe_scp_name(const char *name) {
  size_t i;
  if (name == NULL || *name == 0) return 0;
  if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) return 0;
  for (i = 0; name[i] != 0; ++i) {
    if (name[i] == '/') return 0;
  }
  return 1;
}

static ptr split_remote_path(const char *path, char **parent_out, char **base_out) {
  size_t len;
  ssize_t i;
  if (path == NULL || *path == 0) return make_error_status_message("remote path must not be empty");

  len = strlen(path);
  while (len > 1 && path[len - 1] == '/') --len;
  if (len == 0) return make_error_status_message("remote path must not be empty");

  for (i = (ssize_t)len - 1; i >= 0; --i) {
    if (path[i] == '/') {
      *parent_out = (i == 0) ? dup_cstring_n(path, 1) : dup_cstring_n(path, (size_t)i);
      *base_out = dup_cstring_n(path + i + 1, len - (size_t)i - 1);
      break;
    }
  }
  if (i < 0) {
    *parent_out = dup_cstring(".");
    *base_out = dup_cstring_n(path, len);
  }
  if (*parent_out == NULL || *base_out == NULL) {
    free(*parent_out);
    free(*base_out);
    *parent_out = NULL;
    *base_out = NULL;
    return make_errno_status();
  }
  if ((*base_out)[0] == 0) {
    free(*parent_out);
    free(*base_out);
    *parent_out = NULL;
    *base_out = NULL;
    return make_error_status_message("remote path must not end in '/'");
  }
  return Strue;
}

static char *parent_path_dup(const char *path) {
  size_t len;
  ssize_t i;

  if (path == NULL || *path == 0) return NULL;
  len = strlen(path);
  while (len > 1 && path[len - 1] == '/') --len;
  for (i = (ssize_t)len - 1; i >= 0; --i) {
    if (path[i] == '/') {
      if (i == 0) return dup_cstring_n(path, 1);
      return dup_cstring_n(path, (size_t)i);
    }
  }
  return NULL;
}

static ptr ensure_directory_recursive(const char *path, mode_t mode) {
  struct stat st;
  char *parent = NULL;

  if (path == NULL || *path == 0) return make_error_status_message("directory path must not be empty");
  if (stat(path, &st) == 0) {
    if (S_ISDIR(st.st_mode)) return Strue;
    return make_path_error_status("path exists and is not a directory", path);
  }
  if (errno != ENOENT) return make_errno_status();

  parent = parent_path_dup(path);
  if (parent != NULL && strcmp(parent, path) != 0) {
    ptr status = ensure_directory_recursive(parent, mode);
    free(parent);
    parent = NULL;
    if (status != Strue) return status;
  } else {
    free(parent);
    parent = NULL;
  }

  if (mkdir(path, mode) != 0 && errno != EEXIST) return make_errno_status();
  if (stat(path, &st) != 0) return make_errno_status();
  if (!S_ISDIR(st.st_mode)) return make_path_error_status("path exists and is not a directory", path);
  return Strue;
}

static ptr ensure_parent_directory_exists(const char *path) {
  struct stat st;
  char *parent = parent_path_dup(path);
  if (parent == NULL) return Strue;
  if (stat(parent, &st) != 0) {
    ptr status = (errno == ENOENT)
                   ? make_path_error_status("destination parent directory does not exist", parent)
                   : make_errno_status();
    free(parent);
    return status;
  }
  free(parent);
  if (!S_ISDIR(st.st_mode))
    return make_path_error_status("destination parent path is not a directory", path);
  return Strue;
}

static char *join_paths(const char *base, const char *name) {
  size_t base_len;
  size_t name_len;
  int need_sep;
  char *out;

  if (base == NULL || name == NULL) return NULL;
  base_len = strlen(base);
  name_len = strlen(name);
  need_sep = base_len > 0 && base[base_len - 1] != '/';
  out = (char *)malloc(base_len + (size_t)need_sep + name_len + 1);
  if (out == NULL) return NULL;
  memcpy(out, base, base_len);
  if (need_sep) out[base_len++] = '/';
  memcpy(out + base_len, name, name_len);
  out[base_len + name_len] = 0;
  return out;
}

static void path_stack_release(cspp_path_stack *stack) {
  size_t i;
  if (stack == NULL) return;
  for (i = 0; i < stack->len; ++i) free(stack->items[i]);
  free(stack->items);
  stack->items = NULL;
  stack->len = 0;
  stack->cap = 0;
}

static ptr path_stack_push(cspp_path_stack *stack, const char *path) {
  char *copy;
  if (stack->len == stack->cap) {
    size_t new_cap = stack->cap == 0 ? 4 : stack->cap * 2;
    char **items = (char **)realloc(stack->items, new_cap * sizeof(char *));
    if (items == NULL) return make_errno_status();
    stack->items = items;
    stack->cap = new_cap;
  }
  copy = dup_cstring(path);
  if (copy == NULL) return make_errno_status();
  stack->items[stack->len++] = copy;
  return Strue;
}

static void path_stack_pop(cspp_path_stack *stack) {
  if (stack == NULL || stack->len == 0) return;
  free(stack->items[stack->len - 1]);
  stack->items[--stack->len] = NULL;
}

static const char *path_stack_top(const cspp_path_stack *stack) {
  if (stack == NULL || stack->len == 0) return NULL;
  return stack->items[stack->len - 1];
}

static ptr scp_warning_status(ssh_scp scp) {
  const char *warning = 0 ? NULL : ssh_scp_request_get_warning(scp);
  return make_error_status_message(warning == NULL || *warning == 0 ? "scp warning" : warning);
}

static ptr wait_scp_until(cspp_ssh_session *wrapper, short events, int64_t deadline,
                          const char *timeout_msg) {
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  return wait_ssh_session_until(wrapper->session, events, deadline, timeout_msg);
}

static ptr scp_init_wait(cspp_ssh_session *wrapper, ssh_scp scp, int use_nonblocking,
                         int64_t deadline, const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_init(scp);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN | POLLOUT, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc != SSH_OK) return ssh_error_status_from_wrapper(wrapper, "failed to initialize scp session");
  return Strue;
}

static ptr scp_pull_request_wait(cspp_ssh_session *wrapper, ssh_scp scp, int use_nonblocking,
                                 int64_t deadline, int *request_out,
                                 const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_pull_request(scp);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc == SSH_ERROR) return ssh_error_status_from_wrapper(wrapper, "scp pull request failed");
  *request_out = rc;
  return Strue;
}

static ptr scp_accept_request_wait(cspp_ssh_session *wrapper, ssh_scp scp, int use_nonblocking,
                                   int64_t deadline, const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_accept_request(scp);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN | POLLOUT, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc != SSH_OK) return ssh_error_status_from_wrapper(wrapper, "failed to accept scp request");
  return Strue;
}

static ptr scp_push_directory_wait(cspp_ssh_session *wrapper, ssh_scp scp, const char *dirname,
                                   int mode, int use_nonblocking, int64_t deadline,
                                   const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_push_directory(scp, dirname, mode);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN | POLLOUT, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc != SSH_OK) return ssh_error_status_from_wrapper(wrapper, "failed to push scp directory");
  return Strue;
}

static ptr scp_leave_directory_wait(cspp_ssh_session *wrapper, ssh_scp scp, int use_nonblocking,
                                    int64_t deadline, const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_leave_directory(scp);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN | POLLOUT, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc != SSH_OK) return ssh_error_status_from_wrapper(wrapper, "failed to leave scp directory");
  return Strue;
}

static ptr scp_push_file_wait(cspp_ssh_session *wrapper, ssh_scp scp, const char *filename,
                              uint64_t size, int perms, int use_nonblocking, int64_t deadline,
                              const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_push_file64(scp, filename, size, perms);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN | POLLOUT, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc != SSH_OK) return ssh_error_status_from_wrapper(wrapper, "failed to push scp file");
  return Strue;
}

static ptr scp_read_wait(cspp_ssh_session *wrapper, ssh_scp scp, void *buffer, size_t size,
                         int use_nonblocking, int64_t deadline, const char *timeout_msg,
                         int *count_out) {
  int rc;
  for (;;) {
    rc = ssh_scp_read(scp, buffer, size);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLIN, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc == SSH_ERROR || rc < 0) return ssh_error_status_from_wrapper(wrapper, "scp read failed");
  *count_out = rc;
  return Strue;
}

static ptr scp_write_wait(cspp_ssh_session *wrapper, ssh_scp scp, const void *buffer, size_t size,
                          int use_nonblocking, int64_t deadline, const char *timeout_msg) {
  int rc;
  for (;;) {
    rc = ssh_scp_write(scp, buffer, size);
    if (rc != SSH_AGAIN || !use_nonblocking) break;
    {
      ptr wait_status = wait_scp_until(wrapper, POLLOUT, deadline, timeout_msg);
      if (wait_status != Strue) return wait_status;
    }
  }
  if (rc != SSH_OK) return ssh_error_status_from_wrapper(wrapper, "scp write failed");
  return Strue;
}

static ptr write_all_fd(int fd, const unsigned char *buf, size_t len) {
  size_t offset = 0;
  while (offset < len) {
    ssize_t rc = write(fd, buf + offset, len - offset);
    if (rc > 0) {
      offset += (size_t)rc;
      continue;
    }
    if (rc < 0 && errno == EINTR) continue;
    return make_errno_status();
  }
  return Strue;
}

static ptr scp_upload_file_contents(cspp_ssh_session *wrapper, ssh_scp scp, const char *local_path,
                                    const char *remote_name, int use_nonblocking,
                                    int64_t deadline) {
  struct stat st;
  unsigned char buffer[CSPP_SCP_CHUNK_SIZE];
  int fd = -1;
  ptr status;

  if (stat(local_path, &st) != 0) return make_errno_status();
  if (!S_ISREG(st.st_mode)) return make_path_error_status("local file expected", local_path);

  status = scp_push_file_wait(wrapper, scp, remote_name, (uint64_t)st.st_size,
                              (int)(st.st_mode & 07777), use_nonblocking, deadline,
                              "scp upload timed out");
  if (status != Strue) return status;

  fd = open(local_path, O_RDONLY);
  if (fd < 0) return make_errno_status();
  for (;;) {
    ssize_t nread = read(fd, buffer, sizeof(buffer));
    if (nread > 0) {
      status = scp_write_wait(wrapper, scp, buffer, (size_t)nread, use_nonblocking, deadline,
                              "scp upload timed out");
      if (status != Strue) {
        close(fd);
        return status;
      }
      continue;
    }
    if (nread == 0) break;
    if (errno == EINTR) continue;
    close(fd);
    return make_errno_status();
  }
  close(fd);
  return Strue;
}

static ptr scp_upload_directory_contents(cspp_ssh_session *wrapper, ssh_scp scp,
                                         const char *local_dir, const char *remote_name,
                                         int use_nonblocking, int64_t deadline) {
  struct stat st;
  DIR *dir = NULL;
  struct dirent *entry;
  ptr status;

  if (stat(local_dir, &st) != 0) return make_errno_status();
  if (!S_ISDIR(st.st_mode)) return make_path_error_status("local directory expected", local_dir);

  status = scp_push_directory_wait(wrapper, scp, remote_name, (int)(st.st_mode & 07777),
                                   use_nonblocking, deadline, "scp directory upload timed out");
  if (status != Strue) return status;

  dir = opendir(local_dir);
  if (dir == NULL) return make_errno_status();
  while ((entry = readdir(dir)) != NULL) {
    char *child_path;
    struct stat child_st;

    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
    child_path = join_paths(local_dir, entry->d_name);
    if (child_path == NULL) {
      closedir(dir);
      return make_errno_status();
    }
    if (stat(child_path, &child_st) != 0) {
      free(child_path);
      closedir(dir);
      return make_errno_status();
    }
    if (S_ISDIR(child_st.st_mode)) {
      status = scp_upload_directory_contents(wrapper, scp, child_path, entry->d_name,
                                             use_nonblocking, deadline);
    } else if (S_ISREG(child_st.st_mode)) {
      status = scp_upload_file_contents(wrapper, scp, child_path, entry->d_name,
                                        use_nonblocking, deadline);
    } else {
      status = make_path_error_status("unsupported local filesystem entry for scp upload",
                                      child_path);
    }
    free(child_path);
    if (status != Strue) {
      closedir(dir);
      return status;
    }
  }
  closedir(dir);
  return scp_leave_directory_wait(wrapper, scp, use_nonblocking, deadline,
                                  "scp directory upload timed out");
}

static ptr scp_download_file_to_path(cspp_ssh_session *wrapper, ssh_scp scp, const char *local_path,
                                     uint64_t size, int perms, int use_nonblocking,
                                     int64_t deadline) {
  unsigned char buffer[CSPP_SCP_CHUNK_SIZE];
  int fd = -1;
  uint64_t remaining = size;
  ptr status = ensure_parent_directory_exists(local_path);

  if (status != Strue) return status;
  fd = open(local_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd < 0) return make_errno_status();

  while (remaining > 0) {
    size_t want = remaining > sizeof(buffer) ? sizeof(buffer) : (size_t)remaining;
    int count = 0;
    status = scp_read_wait(wrapper, scp, buffer, want, use_nonblocking, deadline,
                           "scp download timed out", &count);
    if (status != Strue) {
      close(fd);
      unlink(local_path);
      return status;
    }
    if (count <= 0) {
      close(fd);
      unlink(local_path);
      return make_error_status_message("unexpected EOF while receiving scp file data");
    }
    status = write_all_fd(fd, buffer, (size_t)count);
    if (status != Strue) {
      close(fd);
      unlink(local_path);
      return status;
    }
    remaining -= (uint64_t)count;
  }

  close(fd);
  if (chmod(local_path, (mode_t)(perms & 07777)) != 0) {
    if (errno != EPERM) {
      unlink(local_path);
      return make_errno_status();
    }
  }
  return Strue;
}

static ptr scp_download_directory_to_path(cspp_ssh_session *wrapper, ssh_scp scp,
                                          const char *local_path, int use_nonblocking,
                                          int64_t deadline) {
  cspp_path_stack stack = {0};
  ptr status = Strue;
  int root_started = 0;

  for (;;) {
    int request = 0;
    status = scp_pull_request_wait(wrapper, scp, use_nonblocking, deadline, &request,
                                   "scp directory download timed out");
    if (status != Strue) break;

    if (request == SSH_SCP_REQUEST_WARNING) {
      status = scp_warning_status(scp);
      break;
    }
    if (request == SSH_SCP_REQUEST_EOF) break;
    if (request == SSH_SCP_REQUEST_ENDDIR) {
      if (stack.len == 0) {
        status = make_error_status_message("unexpected end-of-directory while receiving scp tree");
        break;
      }
      path_stack_pop(&stack);
      continue;
    }
    if (request == SSH_SCP_REQUEST_NEWDIR) {
      const char *name = ssh_scp_request_get_filename(scp);
      int perms = ssh_scp_request_get_permissions(scp);
      char *child_path = NULL;

      if (name == NULL) {
        status = make_error_status_message("scp directory entry is missing a name");
        break;
      }
      if (!root_started) {
        status = ensure_directory_recursive(local_path, (mode_t)(perms & 07777));
        if (status == Strue && chmod(local_path, (mode_t)(perms & 07777)) != 0 && errno != EPERM)
          status = make_errno_status();
        if (status != Strue) break;
        status = scp_accept_request_wait(wrapper, scp, use_nonblocking, deadline,
                                         "scp directory download timed out");
        if (status != Strue) break;
        status = path_stack_push(&stack, local_path);
        if (status != Strue) break;
        root_started = 1;
        continue;
      }
      if (!is_safe_scp_name(name)) {
        status = make_path_error_status("unsafe scp entry name", name);
        break;
      }
      child_path = join_paths(path_stack_top(&stack), name);
      if (child_path == NULL) {
        status = make_errno_status();
        break;
      }
      status = ensure_directory_recursive(child_path, (mode_t)(perms & 07777));
      if (status == Strue && chmod(child_path, (mode_t)(perms & 07777)) != 0 && errno != EPERM)
        status = make_errno_status();
      if (status == Strue)
        status = scp_accept_request_wait(wrapper, scp, use_nonblocking, deadline,
                                         "scp directory download timed out");
      if (status == Strue) status = path_stack_push(&stack, child_path);
      free(child_path);
      if (status != Strue) break;
      continue;
    }
    if (request == SSH_SCP_REQUEST_NEWFILE) {
      const char *name = ssh_scp_request_get_filename(scp);
      uint64_t size = ssh_scp_request_get_size64(scp);
      int perms = ssh_scp_request_get_permissions(scp);
      char *child_path;

      if (!root_started) {
        status = make_error_status_message("remote path is a file; use scp-download");
        break;
      }
      if (name == NULL) {
        status = make_error_status_message("scp file entry is missing a name");
        break;
      }
      if (!is_safe_scp_name(name)) {
        status = make_path_error_status("unsafe scp entry name", name);
        break;
      }
      child_path = join_paths(path_stack_top(&stack), name);
      if (child_path == NULL) {
        status = make_errno_status();
        break;
      }
      status = scp_accept_request_wait(wrapper, scp, use_nonblocking, deadline,
                                       "scp directory download timed out");
      if (status == Strue)
        status = scp_download_file_to_path(wrapper, scp, child_path, size, perms,
                                           use_nonblocking, deadline);
      free(child_path);
      if (status != Strue) break;
      continue;
    }
    status = make_error_status_message("unexpected scp request kind while receiving directory");
    break;
  }

  path_stack_release(&stack);
  return status;
}

ptr cspp_net_ssh_open(const char *host, int port, const char *user, int timeout_ms,
                        int hostkey_policy) {
  ssh_session session;
  cspp_ssh_session *wrapper;
  unsigned int uport;
  long timeout_sec;
  long timeout_usec;

  if (!ssh_available())
    return make_error_status_message(cspp_optional_library_error(&ssh_library));

  session = ssh_new();
  if (session == NULL) return make_error_status_message("failed to allocate ssh session");

  uport = port <= 0 ? 22u : (unsigned int)port;
  timeout_sec = (long)(timeout_ms / 1000);
  timeout_usec = (long)((timeout_ms % 1000) * 1000);

  if (ssh_options_set(session, SSH_OPTIONS_HOST, host) != SSH_OK ||
      ssh_options_set(session, SSH_OPTIONS_PORT, &uport) != SSH_OK ||
      (user != NULL && *user != 0 && ssh_options_set(session, SSH_OPTIONS_USER, user) != SSH_OK) ||
      ssh_options_set(session, SSH_OPTIONS_TIMEOUT, &timeout_sec) != SSH_OK ||
      ssh_options_set(session, SSH_OPTIONS_TIMEOUT_USEC, &timeout_usec) != SSH_OK) {
    ptr status = ssh_error_status(session, "failed to configure ssh session");
    ssh_free(session);
    return status;
  }

  if (!configure_default_ssh_paths(session)) {
    ptr status = ssh_error_status(session, "failed to configure ssh known-host paths");
    ssh_free(session);
    return status;
  }

  if (ssh_options_parse_config(session, NULL) != SSH_OK) {
    ptr status = ssh_error_status(session, "failed to parse ssh config");
    ssh_free(session);
    return status;
  }

  if (!add_default_identities(session)) {
    ptr status = ssh_error_status(session, "failed to configure ssh identities");
    ssh_free(session);
    return status;
  }

  if (ssh_connect(session) != SSH_OK) {
    ptr status = ssh_error_status(session, "failed to connect ssh session");
    ssh_disconnect(session);
    ssh_free(session);
    return status;
  }

  if (hostkey_policy != 2) {
    enum ssh_known_hosts_e state = ssh_session_is_known_server(session);
    switch (state) {
    case SSH_KNOWN_HOSTS_OK:
      break;
    case SSH_KNOWN_HOSTS_NOT_FOUND:
    case SSH_KNOWN_HOSTS_UNKNOWN:
      if (hostkey_policy == 1 && ssh_session_update_known_hosts(session) == SSH_OK)
        break;
      ssh_disconnect(session);
      ssh_free(session);
      return make_error_status_message("SSH host key is not trusted");
    case SSH_KNOWN_HOSTS_CHANGED:
    case SSH_KNOWN_HOSTS_OTHER:
      ssh_disconnect(session);
      ssh_free(session);
      return make_error_status_message("SSH host key mismatch");
    case SSH_KNOWN_HOSTS_ERROR:
    default:
      {
        ptr status = ssh_error_status(session, "failed to verify SSH host key");
        ssh_disconnect(session);
        ssh_free(session);
        return status;
      }
    }
  }

  wrapper = (cspp_ssh_session *)calloc(1, sizeof(cspp_ssh_session));
  if (wrapper == NULL) {
    ssh_disconnect(session);
    ssh_free(session);
    return make_errno_status();
  }
  wrapper->session = session;
  return make_ssh_handle((uptr)wrapper);
}

ptr cspp_net_ssh_close(uptr handle) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->session != NULL) {
    ssh_disconnect(wrapper->session);
    ssh_free(wrapper->session);
  }
  free(wrapper);
  return Strue;
}

ptr cspp_net_ssh_session_fd(uptr handle) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  socket_t fd;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  fd = ssh_get_fd(wrapper->session);
  if (fd < 0) return make_error_status_message("failed to query ssh session socket");
  return Sfixnum((iptr)fd);
}

ptr cspp_net_ssh_auth_password(uptr handle, const char *user, const char *password) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  rc = ssh_userauth_password(wrapper->session, user != NULL && *user != 0 ? user : NULL, password);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh password authentication failed");
}

ptr cspp_net_ssh_auth_publickey_auto(uptr handle, const char *user, const char *passphrase) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  rc = ssh_userauth_publickey_auto(wrapper->session,
                                     user != NULL && *user != 0 ? user : NULL,
                                     passphrase != NULL && *passphrase != 0 ? passphrase : NULL);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh publickey authentication failed");
}

ptr cspp_net_ssh_auth_publickey(uptr handle, const char *user, const char *public_path,
                                 const char *private_path, const char *passphrase) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  ssh_key public_key = NULL;
  ssh_key private_key = NULL;
  int rc;
  ptr status;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (public_path != NULL && *public_path != 0) {
    if (ssh_pki_import_pubkey_file(public_path, &public_key) != SSH_OK)
      return ssh_error_status_from_wrapper(wrapper, "failed to import SSH public key");
    rc = ssh_userauth_try_publickey(wrapper->session,
                                      user != NULL && *user != 0 ? user : NULL,
                                      public_key);
    ssh_key_free(public_key);
    if (rc != SSH_AUTH_SUCCESS)
      return ssh_error_status_from_wrapper(wrapper, "SSH public key was not accepted");
  }
  if (ssh_pki_import_privkey_file(private_path,
                                    passphrase != NULL && *passphrase != 0 ? passphrase : NULL,
                                    NULL, NULL, &private_key) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to import SSH private key");
  rc = ssh_userauth_publickey(wrapper->session,
                                user != NULL && *user != 0 ? user : NULL,
                                private_key);
  ssh_key_free(private_key);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  status = ssh_error_status_from_wrapper(wrapper, "SSH private-key authentication failed");
  return status;
}

ptr cspp_net_ssh_auth_keyboard_interactive_step(uptr handle, const char *user) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  int rc;
  int count;
  int i;
  ptr prompts;
  ptr echoes;
  ptr result;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  rc = ssh_userauth_kbdint(wrapper->session,
                             user != NULL && *user != 0 ? user : NULL, NULL);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  if (rc != SSH_AUTH_INFO)
    return ssh_error_status_from_wrapper(wrapper, "SSH keyboard-interactive authentication failed");
  count = ssh_userauth_kbdint_getnprompts(wrapper->session);
  if (count < 0)
    return ssh_error_status_from_wrapper(wrapper, "failed to read SSH authentication prompts");
  prompts = Smake_vector(count, Sfalse);
  echoes = Smake_vector(count, Sfalse);
  for (i = 0; i < count; i += 1) {
    char echo = 0;
    const char *prompt = ssh_userauth_kbdint_getprompt(wrapper->session, (unsigned int)i, &echo);
    if (prompt == NULL)
      return ssh_error_status_from_wrapper(wrapper, "failed to read SSH authentication prompt");
    Svector_set(prompts, i, Sstring(prompt));
    Svector_set(echoes, i, Sboolean(echo != 0));
  }
  result = Smake_vector(4, Sfalse);
  Svector_set(result, 0, Sstring(ssh_userauth_kbdint_getname(wrapper->session) == NULL
                                 ? "" : ssh_userauth_kbdint_getname(wrapper->session)));
  Svector_set(result, 1,
              Sstring(ssh_userauth_kbdint_getinstruction(wrapper->session) == NULL
                          ? "" : ssh_userauth_kbdint_getinstruction(wrapper->session)));
  Svector_set(result, 2, prompts);
  Svector_set(result, 3, echoes);
  return result;
}

ptr cspp_net_ssh_auth_keyboard_interactive_answer(uptr handle, int index,
                                                    const char *answer) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (ssh_userauth_kbdint_setanswer(wrapper->session, (unsigned int)index, answer) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to answer SSH authentication prompt");
  return Strue;
}

ptr cspp_net_ssh_auth_agent(uptr handle, const char *user) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  rc = ssh_userauth_agent(wrapper->session, user != NULL && *user != 0 ? user : NULL);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh agent authentication failed");
}

ptr cspp_net_ssh_auth_agent_identity(uptr handle, const char *user, const char *identity) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (identity != NULL && *identity != 0 &&
      ssh_options_set(wrapper->session, SSH_OPTIONS_IDENTITY, identity) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to select SSH agent identity");
  rc = ssh_userauth_agent(wrapper->session, user != NULL && *user != 0 ? user : NULL);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh agent authentication failed");
}

ptr cspp_net_ssh_known_host_check(uptr handle, const char *path) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  enum ssh_known_hosts_e state;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (path != NULL && *path != 0 &&
      ssh_options_set(wrapper->session, SSH_OPTIONS_KNOWNHOSTS, path) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to select SSH known-hosts file");
  state = ssh_session_is_known_server(wrapper->session);
  switch (state) {
  case SSH_KNOWN_HOSTS_OK: return Sstring_to_symbol("ok");
  case SSH_KNOWN_HOSTS_NOT_FOUND: return Sstring_to_symbol("not-found");
  case SSH_KNOWN_HOSTS_UNKNOWN: return Sstring_to_symbol("unknown");
  case SSH_KNOWN_HOSTS_CHANGED: return Sstring_to_symbol("changed");
  case SSH_KNOWN_HOSTS_OTHER: return Sstring_to_symbol("other");
  default: return ssh_error_status_from_wrapper(wrapper, "failed to check SSH known host");
  }
}

ptr cspp_net_ssh_known_host_update(uptr handle, const char *path) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (path != NULL && *path != 0 &&
      ssh_options_set(wrapper->session, SSH_OPTIONS_KNOWNHOSTS, path) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to select SSH known-hosts file");
  if (ssh_session_update_known_hosts(wrapper->session) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to update SSH known host");
  return Strue;
}

ptr cspp_net_ssh_known_host_export(uptr handle) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  char *entry = NULL;
  ptr result;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (ssh_session_export_known_hosts_entry(wrapper->session, &entry) != SSH_OK || entry == NULL)
    return ssh_error_status_from_wrapper(wrapper, "failed to export SSH known host");
  result = Sstring(entry);
  ssh_string_free_char(entry);
  return result;
}

ptr cspp_net_ssh_channel_open(uptr handle, int timeout_ms) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  cspp_ssh_channel *channel_wrapper;
  ssh_channel channel;
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }

  channel = ssh_channel_new(wrapper->session);
  if (channel == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate ssh channel");
  use_nonblocking = timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->session, 0);
  for (;;) {
    rc = ssh_channel_open_session(channel);
    if (rc != SSH_AGAIN || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->session, POLLIN | POLLOUT, deadline,
                                               "ssh channel open timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->session, 1);
        ssh_channel_free(channel);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->session, 1);
  if (rc != SSH_OK) {
    ptr status = ssh_error_status_from_wrapper(wrapper, "failed to open ssh channel");
    ssh_channel_free(channel);
    return status;
  }

  channel_wrapper = (cspp_ssh_channel *)calloc(1, sizeof(cspp_ssh_channel));
  if (channel_wrapper == NULL) {
    ssh_channel_close(channel);
    ssh_channel_free(channel);
    return make_errno_status();
  }
  channel_wrapper->channel = channel;
  channel_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)channel_wrapper);
}

ptr cspp_net_ssh_channel_open_forward(uptr handle, const char *remote_host, int remote_port,
                                        const char *source_host, int source_port,
                                        int timeout_ms) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  cspp_ssh_channel *channel_wrapper;
  ssh_channel channel;
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  channel = ssh_channel_new(wrapper->session);
  if (channel == NULL)
    return ssh_error_status_from_wrapper(wrapper, "failed to allocate SSH forwarding channel");
  use_nonblocking = timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->session, 0);
  for (;;) {
    rc = ssh_channel_open_forward(channel, remote_host, remote_port,
                                    source_host, source_port);
    if (rc != SSH_AGAIN || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->session, POLLIN | POLLOUT, deadline,
                                               "SSH forwarding channel open timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->session, 1);
        ssh_channel_free(channel);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->session, 1);
  if (rc != SSH_OK) {
    ptr status = ssh_error_status_from_wrapper(wrapper, "failed to open SSH forwarding channel");
    ssh_channel_free(channel);
    return status;
  }
  channel_wrapper = (cspp_ssh_channel *)calloc(1, sizeof(cspp_ssh_channel));
  if (channel_wrapper == NULL) {
    ssh_channel_close(channel);
    ssh_channel_free(channel);
    return make_errno_status();
  }
  channel_wrapper->channel = channel;
  channel_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)channel_wrapper);
}

/* A direct channel open must retain the same channel across SSH_AGAIN turns. */
ptr cspp_net_ssh_channel_forward_start(uptr handle) {
  cspp_ssh_session *owner = (cspp_ssh_session *)TO_VOIDP(handle);
  cspp_ssh_channel *channel;
  if (owner == NULL || owner->session == NULL)
    return make_error_status_message("invalid ssh session");
  channel = calloc(1, sizeof(*channel));
  if (channel == NULL) return make_errno_status();
  channel->owner = owner;
  channel->channel = ssh_channel_new(owner->session);
  if (channel->channel == NULL) {
    free(channel);
    return ssh_error_status_from_wrapper(owner, "failed to allocate SSH forwarding channel");
  }
  return make_ssh_handle((uptr)channel);
}

ptr cspp_net_ssh_channel_forward_step(uptr handle, const char *remote_host, int remote_port,
                                       const char *source_host, int source_port) {
  cspp_ssh_channel *channel = (cspp_ssh_channel *)TO_VOIDP(handle);
  int result, blocking;
  if (channel == NULL || channel->owner == NULL || channel->owner->session == NULL)
    return make_error_status_message("invalid ssh forwarding channel");
  blocking = ssh_is_blocking(channel->owner->session);
  ssh_set_blocking(channel->owner->session, 0);
  result = ssh_channel_open_forward(channel->channel, remote_host, remote_port,
                                    source_host, source_port);
  ssh_set_blocking(channel->owner->session, blocking);
  if (result == SSH_AGAIN)
    return ssh_would_block_status(channel->owner->session, POLLIN);
  if (result != SSH_OK)
    return ssh_channel_error_status(channel, "failed to open SSH forwarding channel");
  return Strue;
}

void cspp_net_ssh_channel_forward_discard(uptr handle) {
  cspp_ssh_channel *channel = (cspp_ssh_channel *)TO_VOIDP(handle);
  int blocking;
  if (channel == NULL) return;
  /* Cancellation must not wait for a remote open confirmation or close reply. */
  blocking = ssh_is_blocking(channel->owner->session);
  ssh_set_blocking(channel->owner->session, 0);
  ssh_channel_free(channel->channel);
  ssh_set_blocking(channel->owner->session, blocking);
  free(channel);
}

ptr cspp_net_ssh_remote_forward_listen(uptr handle, const char *address, int port) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  int bound_port = 0;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (ssh_channel_listen_forward(wrapper->session,
                                   address != NULL && *address != 0 ? address : NULL,
                                   port, &bound_port) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to request SSH remote forwarding");
  /* libssh fills bound_port only when the requested port was zero. */
  return Sfixnum(port == 0 ? bound_port : port);
}

ptr cspp_net_ssh_remote_forward_accept(uptr handle) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  cspp_ssh_channel *channel_wrapper;
  ssh_channel channel;
  int destination_port = 0;
  int originator_port = 0;
  char *originator = NULL;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  channel = ssh_channel_open_forward_port(wrapper->session, 0, &destination_port,
                                            &originator, &originator_port);
  if (originator != NULL) ssh_string_free_char(originator);
  if (channel == NULL) return ssh_would_block_status(wrapper->session, POLLIN);
  channel_wrapper = (cspp_ssh_channel *)calloc(1, sizeof(cspp_ssh_channel));
  if (channel_wrapper == NULL) {
    ssh_channel_close(channel);
    ssh_channel_free(channel);
    return make_errno_status();
  }
  channel_wrapper->channel = channel;
  channel_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)channel_wrapper);
}

ptr cspp_net_ssh_remote_forward_cancel(uptr handle, const char *address, int port) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (ssh_channel_cancel_forward(wrapper->session,
                                   address != NULL && *address != 0 ? address : NULL,
                                   port) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper, "failed to cancel SSH remote forwarding");
  return Strue;
}

ptr cspp_net_ssh_channel_send_eof(uptr handle) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL)
    return make_error_status_message("invalid ssh channel");
  if (ssh_channel_send_eof(wrapper->channel) != SSH_OK)
    return ssh_error_status_from_wrapper(wrapper->owner, "failed to send SSH channel EOF");
  return Strue;
}

ptr cspp_net_ssh_channel_close(uptr handle) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->channel != NULL) {
    ssh_channel_send_eof(wrapper->channel);
    ssh_channel_close(wrapper->channel);
    ssh_channel_free(wrapper->channel);
  }
  free(wrapper);
  return Strue;
}

ptr cspp_net_ssh_channel_request_exec(uptr handle, const char *cmd, int timeout_ms) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = ssh_channel_request_exec(wrapper->channel, cmd);
    if (rc != SSH_AGAIN || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN | POLLOUT, deadline,
                                               "ssh exec request timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
  if (rc != SSH_OK)
    return ssh_channel_error_status(wrapper, "failed to request ssh exec");
  return Strue;
}

ptr cspp_net_ssh_channel_request_shell(uptr handle, int timeout_ms) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = ssh_channel_request_shell(wrapper->channel);
    if (rc != SSH_AGAIN || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN | POLLOUT, deadline,
                                               "ssh shell request timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
  if (rc != SSH_OK)
    return ssh_channel_error_status(wrapper, "failed to request ssh shell");
  return Strue;
}

ptr cspp_net_ssh_channel_request_pty_size(uptr handle, const char *terminal,
                                          int columns, int rows, int resize, int timeout_ms) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = resize ? ssh_channel_change_pty_size(wrapper->channel, columns, rows) :
                  ssh_channel_request_pty_size(wrapper->channel, terminal, columns, rows);
    if (rc != SSH_AGAIN || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN | POLLOUT, deadline,
                                               "ssh pty request timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
  if (rc != SSH_OK)
    return ssh_channel_error_status(wrapper, "failed to request ssh pty");
  return Strue;
}

ptr cspp_net_ssh_channel_request_pty(uptr handle, int timeout_ms) {
  return cspp_net_ssh_channel_request_pty_size(handle, "xterm", 80, 24, 0, timeout_ms);
}

ptr cspp_net_ssh_channel_read(uptr handle, int size, int is_stderr, int nonblocking, int timeout_ms) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  ptr out;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->channel == NULL || wrapper->owner == NULL)
    return make_error_status_message("invalid ssh channel");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 0);
  out = Smake_bytevector((iptr)size, 0);
  for (;;) {
    rc = ssh_channel_read(wrapper->channel, Sbytevector_data(out), (uint32_t)size, is_stderr);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN, deadline,
                                               "ssh read timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);

  if (rc == SSH_AGAIN)
    return ssh_would_block_status(wrapper->owner->session, POLLIN);
  if (rc == SSH_ERROR) return ssh_channel_error_status(wrapper, "ssh read failed");
  if (rc == 0 && ssh_channel_is_eof(wrapper->channel)) return Seof_object;
  if (rc < 0) return ssh_channel_error_status(wrapper, "ssh read failed");
  if (rc == size) return out;

  {
    ptr clipped = Smake_bytevector((iptr)rc, 0);
    memcpy(Sbytevector_data(clipped), Sbytevector_data(out), (size_t)rc);
    return clipped;
  }
}

ptr cspp_net_ssh_channel_read_into(uptr handle, ptr bv, int start, int stop, int is_stderr,
                                     int nonblocking, int timeout_ms) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->channel == NULL || wrapper->owner == NULL)
    return make_error_status_message("invalid ssh channel");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = ssh_channel_read(wrapper->channel, Sbytevector_data(bv) + start,
                            (uint32_t)(stop - start), is_stderr);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN, deadline,
                                               "ssh read timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);

  if (rc == SSH_AGAIN)
    return ssh_would_block_status(wrapper->owner->session, POLLIN);
  if (rc == SSH_ERROR) return ssh_channel_error_status(wrapper, "ssh read failed");
  if (rc == 0 && ssh_channel_is_eof(wrapper->channel)) return Seof_object;
  if (rc < 0) return ssh_channel_error_status(wrapper, "ssh read failed");
  return Sfixnum((iptr)rc);
}

ptr cspp_net_ssh_channel_write(uptr handle, ptr bv, int start, int stop, int nonblocking,
                                 int timeout_ms) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->channel == NULL || wrapper->owner == NULL)
    return make_error_status_message("invalid ssh channel");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = ssh_channel_write(wrapper->channel, Sbytevector_data(bv) + start,
                             (uint32_t)(stop - start));
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLOUT, deadline,
                                               "ssh write timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) ssh_set_blocking(wrapper->owner->session, 1);

  if (rc == SSH_AGAIN)
    return ssh_would_block_status(wrapper->owner->session, POLLOUT);
  if (rc == SSH_ERROR || rc < 0) return ssh_channel_error_status(wrapper, "ssh write failed");
  return Sfixnum((iptr)rc);
}

ptr cspp_net_ssh_channel_exit_status(uptr handle) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  return Sfixnum((iptr)ssh_channel_get_exit_status(wrapper->channel));
}

ptr cspp_net_ssh_channel_request_environment(uptr handle, const char *name,
                                                const char *value) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL)
    return make_error_status_message("invalid ssh channel");
  if (ssh_channel_request_env(wrapper->channel, name, value) != SSH_OK)
    return ssh_channel_error_status(wrapper, "ssh environment request failed");
  return Strue;
}

ptr cspp_net_ssh_channel_request_subsystem(uptr handle, const char *subsystem) {
  cspp_ssh_channel *wrapper = (cspp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL)
    return make_error_status_message("invalid ssh channel");
  if (ssh_channel_request_subsystem(wrapper->channel, subsystem) != SSH_OK)
    return ssh_channel_error_status(wrapper, "ssh subsystem request failed");
  return Strue;
}

ptr cspp_net_sftp_open(uptr handle) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  cspp_sftp_session *sftp_wrapper;
  sftp_session sftp;

  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  sftp = sftp_new(wrapper->session);
  if (sftp == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate sftp session");
  if (sftp_init(sftp) != SSH_OK) {
    ptr status = ssh_error_status_from_wrapper(wrapper, "failed to initialize sftp session");
    sftp_free(sftp);
    return status;
  }

  sftp_wrapper = (cspp_sftp_session *)calloc(1, sizeof(cspp_sftp_session));
  if (sftp_wrapper == NULL) {
    sftp_free(sftp);
    return make_errno_status();
  }
  sftp_wrapper->sftp = sftp;
  sftp_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)sftp_wrapper);
}

ptr cspp_net_sftp_close(uptr handle) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->sftp != NULL) sftp_free(wrapper->sftp);
  free(wrapper);
  return Strue;
}

ptr cspp_net_sftp_list(uptr handle, const char *path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  sftp_dir dir;
  ptr out = Snil;

  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");

  dir = sftp_opendir(wrapper->sftp, path);
  if (dir == NULL) return sftp_error_status(wrapper, "failed to open sftp directory");

  while (!sftp_dir_eof(dir)) {
    sftp_attributes attr = sftp_readdir(wrapper->sftp, dir);
    if (attr == NULL) break;
    out = Scons(attr->name == NULL ? Sfalse : Sstring_utf8(attr->name, (iptr)strlen(attr->name)), out);
    sftp_attributes_free(attr);
  }

  if (sftp_closedir(dir) != SSH_OK) return sftp_error_status(wrapper, "failed to close sftp directory");
  return out;
}

ptr cspp_net_sftp_stat(uptr handle, const char *path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  sftp_attributes attr;
  ptr out;

  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");

  attr = sftp_stat(wrapper->sftp, path);
  if (attr == NULL) return sftp_error_status(wrapper, "failed to stat sftp path");
  out = sftp_attr_to_vector(attr);
  sftp_attributes_free(attr);
  return out;
}

ptr cspp_net_sftp_open_directory(uptr handle, const char *path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  cspp_sftp_directory *directory;
  sftp_dir dir;
  if (wrapper == NULL || wrapper->sftp == NULL)
    return make_error_status_message("invalid sftp session");
  dir = sftp_opendir(wrapper->sftp, path);
  if (dir == NULL) return sftp_error_status(wrapper, "failed to open sftp directory");
  directory = (cspp_sftp_directory *)calloc(1, sizeof(*directory));
  if (directory == NULL) {
    sftp_closedir(dir);
    return make_errno_status();
  }
  directory->dir = dir;
  directory->owner = wrapper;
  return make_ssh_handle((uptr)directory);
}

ptr cspp_net_sftp_read_directory(uptr handle) {
  cspp_sftp_directory *directory = (cspp_sftp_directory *)TO_VOIDP(handle);
  sftp_attributes attr;
  ptr out;
  if (directory == NULL || directory->dir == NULL || directory->owner == NULL)
    return make_error_status_message("invalid sftp directory");
  attr = sftp_readdir(directory->owner->sftp, directory->dir);
  if (attr == NULL) {
    if (sftp_dir_eof(directory->dir)) return Seof_object;
    return sftp_error_status(directory->owner, "failed to read sftp directory");
  }
  out = sftp_attr_to_vector(attr);
  sftp_attributes_free(attr);
  return out;
}

ptr cspp_net_sftp_close_directory(uptr handle) {
  cspp_sftp_directory *directory = (cspp_sftp_directory *)TO_VOIDP(handle);
  ptr out = Strue;
  if (directory == NULL) return out;
  if (directory->dir != NULL && sftp_closedir(directory->dir) != SSH_OK)
    out = sftp_error_status(directory->owner, "failed to close sftp directory");
  directory->dir = NULL;
  free(directory);
  return out;
}

ptr cspp_net_sftp_chmod(uptr handle, const char *path, unsigned mode) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL)
    return make_error_status_message("invalid sftp session");
  if (sftp_chmod(wrapper->sftp, path, (mode_t)mode) != SSH_OK)
    return sftp_error_status(wrapper, "failed to chmod sftp path");
  return Strue;
}

ptr cspp_net_sftp_chown(uptr handle, const char *path, unsigned uid, unsigned gid) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL)
    return make_error_status_message("invalid sftp session");
  if (sftp_chown(wrapper->sftp, path, (uid_t)uid, (gid_t)gid) != SSH_OK)
    return sftp_error_status(wrapper, "failed to chown sftp path");
  return Strue;
}

ptr cspp_net_sftp_utimes(uptr handle, const char *path, int64_t atime, int64_t mtime) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  struct timeval times[2];
  if (wrapper == NULL || wrapper->sftp == NULL || atime < 0 || mtime < 0)
    return make_error_status_message("invalid sftp utimes arguments");
  times[0].tv_sec = (time_t)atime; times[0].tv_usec = 0;
  times[1].tv_sec = (time_t)mtime; times[1].tv_usec = 0;
  if (sftp_utimes(wrapper->sftp, path, times) != SSH_OK)
    return sftp_error_status(wrapper, "failed to set sftp times");
  return Strue;
}

ptr cspp_net_sftp_symlink(uptr handle, const char *target, const char *dest) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL)
    return make_error_status_message("invalid sftp session");
  if (sftp_symlink(wrapper->sftp, target, dest) != SSH_OK)
    return sftp_error_status(wrapper, "failed to create sftp symlink");
  return Strue;
}

ptr cspp_net_sftp_readlink(uptr handle, const char *path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  char *target;
  ptr out;
  if (wrapper == NULL || wrapper->sftp == NULL)
    return make_error_status_message("invalid sftp session");
  target = sftp_readlink(wrapper->sftp, path);
  if (target == NULL) return sftp_error_status(wrapper, "failed to read sftp symlink");
  out = Sstring_utf8(target, (iptr)strlen(target));
  ssh_string_free_char(target);
  return out;
}

ptr cspp_net_sftp_seek(uptr handle, uint64_t offset) {
  cspp_sftp_file *wrapper = (cspp_sftp_file *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->file == NULL)
    return make_error_status_message("invalid sftp file");
  if (sftp_seek64(wrapper->file, offset) != SSH_OK)
    return sftp_file_error_status(wrapper, "failed to seek sftp file");
  return Strue;
}

ptr cspp_net_sftp_delete(uptr handle, const char *path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (sftp_unlink(wrapper->sftp, path) != SSH_OK) return sftp_error_status(wrapper, "failed to delete sftp file");
  return Strue;
}

ptr cspp_net_sftp_mkdir(uptr handle, const char *path, int mode) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (sftp_mkdir(wrapper->sftp, path, (mode_t)mode) != SSH_OK)
    return sftp_error_status(wrapper, "failed to create sftp directory");
  return Strue;
}

ptr cspp_net_sftp_rmdir(uptr handle, const char *path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (sftp_rmdir(wrapper->sftp, path) != SSH_OK)
    return sftp_error_status(wrapper, "failed to remove sftp directory");
  return Strue;
}

ptr cspp_net_sftp_rename(uptr handle, const char *from_path, const char *to_path) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (sftp_rename(wrapper->sftp, from_path, to_path) != SSH_OK)
    return sftp_error_status(wrapper, "failed to rename sftp path");
  return Strue;
}

ptr cspp_net_sftp_open_file(uptr handle, const char *path, int flags, int mode) {
  cspp_sftp_session *wrapper = (cspp_sftp_session *)TO_VOIDP(handle);
  cspp_sftp_file *file_wrapper;
  sftp_file file;

  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");

  file = sftp_open(wrapper->sftp, path, flags, (mode_t)mode);
  if (file == NULL) return sftp_error_status(wrapper, "failed to open sftp file");

  file_wrapper = (cspp_sftp_file *)calloc(1, sizeof(cspp_sftp_file));
  if (file_wrapper == NULL) {
    sftp_close(file);
    return make_errno_status();
  }
  file_wrapper->file = file;
  file_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)file_wrapper);
}

ptr cspp_net_sftp_close_file(uptr handle) {
  cspp_sftp_file *wrapper = (cspp_sftp_file *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
#if LIBSSH_VERSION_INT >= SSH_VERSION_INT(0, 11, 0)
  if (wrapper->pending_read != NULL)
    sftp_aio_free(wrapper->pending_read);
  if (wrapper->pending_write != NULL)
    sftp_aio_free(wrapper->pending_write);
#endif
  if (wrapper->file != NULL) sftp_close(wrapper->file);
  free(wrapper);
  return Strue;
}

ptr cspp_net_sftp_read(uptr handle, int size, int nonblocking, int timeout_ms) {
  cspp_sftp_file *wrapper = (cspp_sftp_file *)TO_VOIDP(handle);
  ssize_t rc;
  ptr out;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->file == NULL) return make_error_status_message("invalid sftp file");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;

  if (use_nonblocking && !ssh_aio_available)
    return make_error_status_message("libssh: sftp AIO capability unavailable");

  if (!use_nonblocking) {
    out = Smake_bytevector((iptr)size, 0);
    rc = sftp_read(wrapper->file, Sbytevector_data(out), (size_t)size);

    if (rc == SSH_AGAIN)
      return ssh_would_block_status(wrapper->owner->owner->session, POLLIN);
    if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
    if (rc == 0) return Seof_object;
    if (rc == size) return out;

    {
      ptr clipped = Smake_bytevector((iptr)rc, 0);
      memcpy(Sbytevector_data(clipped), Sbytevector_data(out), (size_t)rc);
      return clipped;
    }
  }

#if LIBSSH_VERSION_INT >= SSH_VERSION_INT(0, 11, 0)
  ssh_set_blocking(wrapper->owner->owner->session, 0);
  sftp_file_set_nonblocking(wrapper->file);
  for (;;) {
    if (wrapper->pending_read == NULL) {
      rc = sftp_aio_begin_read(wrapper->file, (size_t)size, &wrapper->pending_read);
      if (rc == SSH_ERROR || rc < 0) {
        sftp_file_set_blocking(wrapper->file);
        ssh_set_blocking(wrapper->owner->owner->session, 1);
        return sftp_file_error_status(wrapper, "sftp read failed");
      }
      wrapper->pending_read_len = (size_t)rc;
    }
    out = Smake_bytevector((iptr)wrapper->pending_read_len, 0);
    rc = sftp_aio_wait_read(&wrapper->pending_read, Sbytevector_data(out), wrapper->pending_read_len);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->owner->session, POLLIN, deadline,
                                               "sftp read timed out");
      if (wait_status != Strue) {
        sftp_file_set_blocking(wrapper->file);
        ssh_set_blocking(wrapper->owner->owner->session, 1);
        return wait_status;
      }
    }
  }
  sftp_file_set_blocking(wrapper->file);
  ssh_set_blocking(wrapper->owner->owner->session, 1);

  if (rc == SSH_AGAIN)
    return ssh_would_block_status(wrapper->owner->owner->session, POLLIN);
  wrapper->pending_read_len = 0;
  if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
  if (rc == 0) return Seof_object;
  if (rc == (ssize_t)Sbytevector_length(out)) return out;

  {
    ptr clipped = Smake_bytevector((iptr)rc, 0);
    memcpy(Sbytevector_data(clipped), Sbytevector_data(out), (size_t)rc);
    return clipped;
  }
#else
  return make_error_status_message("libssh: sftp AIO capability unavailable");
#endif
}

ptr cspp_net_sftp_read_into(uptr handle, ptr bv, int start, int stop, int nonblocking,
                              int timeout_ms) {
  cspp_sftp_file *wrapper = (cspp_sftp_file *)TO_VOIDP(handle);
  ssize_t rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->file == NULL) return make_error_status_message("invalid sftp file");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;

  if (use_nonblocking && !ssh_aio_available)
    return make_error_status_message("libssh: sftp AIO capability unavailable");

  if (!use_nonblocking) {
    rc = sftp_read(wrapper->file, Sbytevector_data(bv) + start, (size_t)(stop - start));

    if (rc == SSH_AGAIN)
      return ssh_would_block_status(wrapper->owner->owner->session, POLLIN);
    if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
    if (rc == 0) return Seof_object;
    return Sfixnum((iptr)rc);
  }

#if LIBSSH_VERSION_INT >= SSH_VERSION_INT(0, 11, 0)
  ssh_set_blocking(wrapper->owner->owner->session, 0);
  sftp_file_set_nonblocking(wrapper->file);
  for (;;) {
    if (wrapper->pending_read == NULL) {
      rc = sftp_aio_begin_read(wrapper->file, (size_t)(stop - start), &wrapper->pending_read);
      if (rc == SSH_ERROR || rc < 0) {
        sftp_file_set_blocking(wrapper->file);
        ssh_set_blocking(wrapper->owner->owner->session, 1);
        return sftp_file_error_status(wrapper, "sftp read failed");
      }
      wrapper->pending_read_len = (size_t)rc;
    }
    rc = sftp_aio_wait_read(&wrapper->pending_read,
                              Sbytevector_data(bv) + start,
                              (size_t)(stop - start));
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->owner->session, POLLIN, deadline,
                                               "sftp read timed out");
      if (wait_status != Strue) {
        sftp_file_set_blocking(wrapper->file);
        ssh_set_blocking(wrapper->owner->owner->session, 1);
        return wait_status;
      }
    }
  }
  sftp_file_set_blocking(wrapper->file);
  ssh_set_blocking(wrapper->owner->owner->session, 1);

  if (rc == SSH_AGAIN)
    return ssh_would_block_status(wrapper->owner->owner->session, POLLIN);
  wrapper->pending_read_len = 0;
  if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
  if (rc == 0) return Seof_object;
  return Sfixnum((iptr)rc);
#else
  return make_error_status_message("libssh: sftp AIO capability unavailable");
#endif
}

ptr cspp_net_sftp_write(uptr handle, ptr bv, int start, int stop, int nonblocking,
                          int timeout_ms) {
  cspp_sftp_file *wrapper = (cspp_sftp_file *)TO_VOIDP(handle);
  ssize_t rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->file == NULL) return make_error_status_message("invalid sftp file");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;

  if (use_nonblocking && !ssh_aio_available)
    return make_error_status_message("libssh: sftp AIO capability unavailable");

  if (!use_nonblocking) {
    rc = sftp_write(wrapper->file, Sbytevector_data(bv) + start, (size_t)(stop - start));

    if (rc == SSH_AGAIN)
      return ssh_would_block_status(wrapper->owner->owner->session, POLLOUT);
    if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp write failed");
    return Sfixnum((iptr)rc);
  }

#if LIBSSH_VERSION_INT >= SSH_VERSION_INT(0, 11, 0)
  ssh_set_blocking(wrapper->owner->owner->session, 0);
  sftp_file_set_nonblocking(wrapper->file);
  for (;;) {
    if (wrapper->pending_write == NULL) {
      rc = sftp_aio_begin_write(wrapper->file,
                                  Sbytevector_data(bv) + start,
                                  (size_t)(stop - start),
                                  &wrapper->pending_write);
      if (rc == SSH_ERROR || rc < 0) {
        sftp_file_set_blocking(wrapper->file);
        ssh_set_blocking(wrapper->owner->owner->session, 1);
        return sftp_file_error_status(wrapper, "sftp write failed");
      }
      wrapper->pending_write_len = (size_t)rc;
    }
    rc = sftp_aio_wait_write(&wrapper->pending_write);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->owner->session, POLLOUT, deadline,
                                               "sftp write timed out");
      if (wait_status != Strue) {
        sftp_file_set_blocking(wrapper->file);
        ssh_set_blocking(wrapper->owner->owner->session, 1);
        return wait_status;
      }
    }
  }
  sftp_file_set_blocking(wrapper->file);
  ssh_set_blocking(wrapper->owner->owner->session, 1);

  if (rc == SSH_AGAIN)
    return ssh_would_block_status(wrapper->owner->owner->session, POLLOUT);
  wrapper->pending_write_len = 0;
  if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp write failed");
  return Sfixnum((iptr)rc);
#else
  return make_error_status_message("libssh: sftp AIO capability unavailable");
#endif
}

ptr cspp_net_scp_upload_file(uptr handle, const char *local_path, const char *remote_path,
                               int timeout_ms) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  ssh_scp scp = NULL;
  char *remote_parent = NULL;
  char *remote_name = NULL;
  ptr status;
  int use_nonblocking;
  int64_t deadline = -1;
  int initialized = 0;

  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }

  status = split_remote_path(remote_path, &remote_parent, &remote_name);
  if (status != Strue) return status;

  scp = ssh_scp_new(wrapper->session, SSH_SCP_WRITE, remote_parent);
  if (scp == NULL) {
    free(remote_parent);
    free(remote_name);
    return ssh_error_status_from_wrapper(wrapper, "failed to allocate scp session");
  }

  use_nonblocking = 0;
  status = scp_init_wait(wrapper, scp, use_nonblocking, deadline, "scp upload timed out");
  if (status == Strue) {
    initialized = 1;
    status = scp_upload_file_contents(wrapper, scp, local_path, remote_name, use_nonblocking,
                                      deadline);
  }

  if (initialized) ssh_scp_close(scp);
  ssh_scp_free(scp);
  free(remote_parent);
  free(remote_name);
  return status;
}

ptr cspp_net_scp_stat(uptr handle, const char *path) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  sftp_session sftp;
  sftp_attributes attr;
  ptr out;
  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  sftp = sftp_new(wrapper->session);
  if (sftp == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate sftp stat");
  if (sftp_init(sftp) != SSH_OK) {
    out = ssh_error_status_from_wrapper(wrapper, "failed to initialize sftp stat");
    sftp_free(sftp);
    return out;
  }
  attr = sftp_stat(sftp, path);
  if (attr == NULL) {
    sftp_free(sftp);
    return Sfalse;
  }
  out = sftp_attr_to_vector(attr);
  sftp_attributes_free(attr);
  sftp_free(sftp);
  return out;
}

ptr cspp_net_scp_download_file(uptr handle, const char *remote_path, const char *local_path,
                                 int timeout_ms) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  ssh_scp scp = NULL;
  ptr status;
  int use_nonblocking;
  int64_t deadline = -1;
  int initialized = 0;
  int request = 0;

  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }

  scp = ssh_scp_new(wrapper->session, SSH_SCP_READ, remote_path);
  if (scp == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate scp session");

  use_nonblocking = 0;
  status = scp_init_wait(wrapper, scp, use_nonblocking, deadline, "scp download timed out");
  if (status == Strue) initialized = 1;
  if (status == Strue)
    status = scp_pull_request_wait(wrapper, scp, use_nonblocking, deadline, &request,
                                   "scp download timed out");
  if (status == Strue) {
    if (request == SSH_SCP_REQUEST_WARNING) {
      status = scp_warning_status(scp);
    } else if (request == SSH_SCP_REQUEST_NEWDIR) {
      ssh_scp_deny_request(scp, "directory not expected");
      status = make_error_status_message("remote path is a directory; use scp-copy-directory");
    } else if (request != SSH_SCP_REQUEST_NEWFILE) {
      status = make_error_status_message("unexpected scp request kind while receiving file");
    }
  }
  if (status == Strue)
    status = scp_accept_request_wait(wrapper, scp, use_nonblocking, deadline,
                                     "scp download timed out");
  if (status == Strue)
    status = scp_download_file_to_path(wrapper, scp, local_path, ssh_scp_request_get_size64(scp),
                                       ssh_scp_request_get_permissions(scp), use_nonblocking,
                                       deadline);

  if (initialized) ssh_scp_close(scp);
  ssh_scp_free(scp);
  return status;
}

ptr cspp_net_scp_upload_directory(uptr handle, const char *local_path, const char *remote_path,
                                    int timeout_ms) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  ssh_scp scp = NULL;
  char *remote_parent = NULL;
  char *remote_name = NULL;
  ptr status;
  int use_nonblocking;
  int64_t deadline = -1;
  int initialized = 0;

  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }

  status = split_remote_path(remote_path, &remote_parent, &remote_name);
  if (status != Strue) return status;

  scp = ssh_scp_new(wrapper->session, SSH_SCP_WRITE | SSH_SCP_RECURSIVE, remote_parent);
  if (scp == NULL) {
    free(remote_parent);
    free(remote_name);
    return ssh_error_status_from_wrapper(wrapper, "failed to allocate scp session");
  }

  use_nonblocking = 0;
  status = scp_init_wait(wrapper, scp, use_nonblocking, deadline,
                         "scp directory upload timed out");
  if (status == Strue) {
    initialized = 1;
    status = scp_upload_directory_contents(wrapper, scp, local_path, remote_name,
                                           use_nonblocking, deadline);
  }

  if (initialized) ssh_scp_close(scp);
  ssh_scp_free(scp);
  free(remote_parent);
  free(remote_name);
  return status;
}

ptr cspp_net_scp_download_directory(uptr handle, const char *remote_path, const char *local_path,
                                      int timeout_ms) {
  cspp_ssh_session *wrapper = (cspp_ssh_session *)TO_VOIDP(handle);
  ssh_scp scp = NULL;
  ptr status;
  int use_nonblocking;
  int64_t deadline = -1;
  int initialized = 0;

  if (wrapper == NULL || wrapper->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }

  scp = ssh_scp_new(wrapper->session, SSH_SCP_READ | SSH_SCP_RECURSIVE, remote_path);
  if (scp == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate scp session");

  use_nonblocking = 0;
  status = scp_init_wait(wrapper, scp, use_nonblocking, deadline,
                         "scp directory download timed out");
  if (status == Strue) {
    initialized = 1;
    status = scp_download_directory_to_path(wrapper, scp, local_path, use_nonblocking, deadline);
  }

  if (initialized) ssh_scp_close(scp);
  ssh_scp_free(scp);
  return status;
}

static ptr scp_transfer_pending(cspp_scp_transfer *t, int events);
static ptr scp_transfer_retry_pending(cspp_scp_transfer *t, int events);

static ptr scp_transfer_progress(cspp_scp_transfer *t) {
  /* POLLOUT normally returns immediately, allowing buffered protocol data to be consumed. */
  return scp_transfer_pending(t, POLLIN | POLLOUT);
}

static char *scp_quote_command(const char *mode, const char *path) {
  size_t path_len = strlen(path);
  size_t quote_count = 0;
  size_t i;
  size_t pos;
  char *command;
  for (i = 0; i < path_len; i++) {
    if (path[i] == '\'') quote_count++;
  }
  command = (char *)malloc(strlen(mode) + path_len + quote_count * 3 + 12);
  if (command == NULL) return NULL;
  pos = (size_t)sprintf(command, "scp %s -- '", mode);
  for (i = 0; i < path_len; i++) {
    if (path[i] == '\'') {
      memcpy(command + pos, "'\\''", 4);
      pos += 4;
    } else {
      command[pos++] = path[i];
    }
  }
  command[pos++] = '\'';
  command[pos] = '\0';
  return command;
}

static ptr scp_channel_error(cspp_scp_transfer *t, const char *fallback) {
  const char *message = ssh_get_error(t->owner->session);
  return make_error_status_message(message == NULL || *message == '\0' ? fallback : message);
}

static ptr scp_write_pending_buffer(cspp_scp_transfer *t, const unsigned char *buffer,
                                    size_t length, size_t *position) {
  int rc = ssh_channel_write(t->channel, buffer + *position,
                               (uint32_t)(length - *position));
  if (rc == SSH_AGAIN) return scp_transfer_retry_pending(t, POLLOUT);
  if (rc == 0) return scp_transfer_retry_pending(t, POLLOUT);
  if (rc < 0) return scp_channel_error(t, "scp channel write failed");
  *position += (size_t)rc;
  if (*position < length) {
    t->wait_events = POLLOUT;
    return Sfalse;
  }
  return Strue;
}

static ptr scp_read_ack(cspp_scp_transfer *t) {
  unsigned char ack;
  int rc = ssh_channel_read(t->channel, &ack, 1, 0);
  if (rc == SSH_AGAIN || rc == 0) return scp_transfer_retry_pending(t, POLLIN);
  if (rc < 0) return scp_channel_error(t, "scp acknowledgment read failed");
  if (ack != 0) return make_error_status_message("remote scp rejected the transfer");
  return Strue;
}

/* Incremental SCP file transfers; native steps use bounded, nonblocking batches. */
ptr cspp_net_scp_transfer_start(uptr handle, int direction, const char *source,
                                  const char *target) {
  cspp_ssh_session *owner = (cspp_ssh_session *)TO_VOIDP(handle);
  cspp_scp_transfer *t;
  char *parent = NULL;
  char *name = NULL;
  struct stat st;
  if (owner == NULL || owner->session == NULL)
    return make_error_status_message("invalid ssh session");
  if (direction != 0 && direction != 1)
    return make_error_status_message("invalid scp transfer direction");
  t = (cspp_scp_transfer *)calloc(1, sizeof(*t));
  if (t == NULL) return make_errno_status();
  t->local_fd = -1;
  t->owner = owner;
  t->direction = direction;
  t->local_path = dup_cstring(direction == 0 ? target : source);
  t->remote_name = dup_cstring(direction == 0 ? source : target);
  if (t->local_path == NULL || t->remote_name == NULL) goto oom;
  if (direction == 1) {
    if (stat(source, &st) != 0) goto errno_fail;
    if (!S_ISREG(st.st_mode)) {
      free(t->local_path); free(t->remote_name); free(t);
      return make_path_error_status("local file expected", source);
    }
    t->remaining = (uint64_t)st.st_size;
    t->local_fd = open(source, O_RDONLY);
    if (t->local_fd < 0) goto errno_fail;
    if (split_remote_path(target, &parent, &name) != Strue) goto fail;
    free(t->remote_name); t->remote_name = name; name = NULL;
    if (strchr(t->remote_name, '\n') != NULL || strchr(t->remote_name, '\r') != NULL) {
      close(t->local_fd);
      free(parent); free(t->local_path); free(t->remote_name); free(t);
      return make_path_error_status("invalid remote file name", target);
    }
    t->command = scp_quote_command("-t", parent);
  } else {
    t->local_fd = open(target, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (t->local_fd < 0) goto errno_fail;
    t->command = scp_quote_command("-f", source);
  }
  free(parent);
  if (t->command == NULL) goto oom;
  t->channel = ssh_channel_new(owner->session);
  if (t->channel == NULL) goto fail;
  t->fd = ssh_get_fd(owner->session);
  t->phase = 0;
  return make_status("ok", Sunsigned((uptr)t));
oom:
  if (t->local_fd >= 0) close(t->local_fd);
  if (t->channel != NULL) ssh_channel_free(t->channel);
  free(t->command); free(t->local_path); free(t->remote_name); free(t);
  return make_errno_status();
errno_fail:
  if (t->local_fd >= 0) close(t->local_fd);
  if (t->channel != NULL) ssh_channel_free(t->channel);
  free(parent); free(name); free(t->command); free(t->local_path); free(t->remote_name); free(t);
  return make_errno_status();
fail:
  if (t->local_fd >= 0) close(t->local_fd);
  if (t->channel != NULL) ssh_channel_free(t->channel);
  free(parent); free(name); free(t->command); free(t->local_path); free(t->remote_name); free(t);
  return ssh_error_status_from_wrapper(owner, "failed to allocate scp transfer");
}

void cspp_net_scp_transfer_close(uptr handle);

static void scp_free_names(char **names, size_t count) {
  size_t index;
  for (index = 0; index < count; index++) free(names[index]);
  free(names);
}

/* Names are UTF-8, NUL-delimited immediate children, bounded by the Scheme walker. */
ptr cspp_net_scp_transfer_start_many(uptr handle, int direction,
                                      const char *local_directory,
                                      const char *remote_directory, ptr encoded_names) {
  char **names = NULL;
  size_t count = 0, position = 0, length, index;
  char *local = NULL, *remote = NULL, *command = NULL;
  ptr result;
  cspp_scp_transfer *t;
  if (!Sbytevectorp(encoded_names)) return make_error_status_message("invalid SCP batch names");
  length = (size_t)Sbytevector_length(encoded_names);
  if (length == 0 || length > 32768) return make_error_status_message("invalid SCP batch size");
  names = calloc(32, sizeof(*names));
  if (names == NULL) return make_errno_status();
  while (position < length) {
    const char *name = (const char *)Sbytevector_data(encoded_names) + position;
    const char *end = memchr(name, 0, length - position);
    if (end == NULL || end == name || count == 32 || !is_safe_scp_name(name) ||
        strchr(name, '\n') != NULL || strchr(name, '\r') != NULL) {
      scp_free_names(names, count);
      return make_error_status_message("invalid SCP batch child name");
    }
    names[count] = dup_cstring(name);
    if (names[count] == NULL) goto oom;
    count++;
    position += (size_t)(end - name) + 1;
  }
  local = join_paths(local_directory, names[0]);
  remote = join_paths(remote_directory, names[0]);
  if (local == NULL || remote == NULL) goto oom;
  if (direction == 0) {
    size_t command_length = 0;
    for (index = 0; index < count; index++) {
      char *path = join_paths(remote_directory, names[index]);
      char *quoted = path == NULL ? NULL : scp_quote_command("-f", path);
      char *grown;
      const char *piece;
      size_t piece_length;
      free(path);
      if (quoted == NULL) goto oom;
      /* Keep the first command prefix; subsequent pieces are quoted operands. */
      piece = quoted + (index == 0 ? 0 : strlen("scp -f -- "));
      piece_length = strlen(piece);
      if (command_length + piece_length + 2 > 131072) {
        free(quoted);
        free(local); free(remote); free(command); scp_free_names(names, count);
        return make_error_status_message("SCP batch command is too long");
      }
      grown = realloc(command, command_length + piece_length + 2);
      if (grown == NULL) { free(quoted); goto oom; }
      command = grown;
      if (index != 0) command[command_length++] = ' ';
      memcpy(command + command_length, piece, piece_length + 1);
      command_length += piece_length;
      free(quoted);
    }
  }
  /* All Scheme data has been copied before start allocates Scheme status objects. */
  result = cspp_net_scp_transfer_start(handle, direction,
                                         direction == 0 ? remote : local,
                                         direction == 0 ? local : remote);
  free(local); free(remote);
  if (!Svectorp(result) || Svector_ref(result, 0) != Sstring_to_symbol("ok")) {
    free(command); scp_free_names(names, count);
    return result;
  }
  t = (cspp_scp_transfer *)TO_VOIDP(Sunsigned_value(Svector_ref(result, 1)));
  t->batch_names = names;
  t->batch_count = count;
  t->local_directory = dup_cstring(local_directory);
  t->remote_directory = dup_cstring(remote_directory);
  if (command != NULL) { free(t->command); t->command = command; }
  if (t->local_directory == NULL || t->remote_directory == NULL) {
    cspp_net_scp_transfer_close((uptr)t);
    return make_errno_status();
  }
  return result;
oom:
  free(local); free(remote); free(command); scp_free_names(names, count);
  return make_errno_status();
}

static ptr scp_batch_next_file(cspp_scp_transfer *t) {
  struct stat st;
  if (t->local_fd >= 0) close(t->local_fd);
  t->local_fd = -1;
  free(t->local_path); free(t->remote_name);
  t->local_path = NULL;
  t->remote_name = NULL;
  t->batch_index++;
  t->local_path = join_paths(t->local_directory, t->batch_names[t->batch_index]);
  t->remote_name = t->direction == 1 ? dup_cstring(t->batch_names[t->batch_index]) :
                    join_paths(t->remote_directory, t->batch_names[t->batch_index]);
  if (t->local_path == NULL || t->remote_name == NULL) return make_errno_status();
  t->offset = 0;
  t->remaining = 0;
  t->buffer_len = t->buffer_pos = t->header_len = t->header_pos = 0;
  t->completed = 0;
  if (t->direction == 1) {
    t->local_fd = open(t->local_path, O_RDONLY);
    if (t->local_fd < 0) return make_errno_status();
    if (fstat(t->local_fd, &st) != 0) return make_errno_status();
    if (!S_ISREG(st.st_mode)) return make_error_status_message("local batch file is not regular");
    t->remaining = (uint64_t)st.st_size;
    t->phase = 2;
  } else {
    t->local_fd = open(t->local_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (t->local_fd < 0) return make_errno_status();
    /* The previous file's final acknowledgement also requests the next header. */
    t->phase = 4;
  }
  return Strue;
}

static ptr scp_batch_status(cspp_scp_transfer *t, ptr status) {
  ptr out;
  if (t->batch_count == 0 || !Svectorp(status) || Svector_length(status) != 3) return status;
  out = Smake_vector(4, Sfalse);
  Svector_set(out, 0, Svector_ref(status, 0));
  Svector_set(out, 1, Svector_ref(status, 1));
  Svector_set(out, 2, Svector_ref(status, 2));
  /* Reporting uses the remote path in both directions, like individual transfers. */
  {
    char *path = join_paths(t->remote_directory, t->batch_names[t->batch_index]);
    if (path == NULL) return make_errno_status();
    Svector_set(out, 3, Sstring_utf8(path, (iptr)strlen(path)));
    free(path);
  }
  if (t->completed && t->batch_index + 1 < t->batch_count) {
    Svector_set(out, 0, Sstring_to_symbol("file-completed"));
    Svector_set(out, 1, Sfixnum(t->fd));
    t->phase = 9;
  }
  return out;
}

static ptr scp_transfer_pending(cspp_scp_transfer *t, int events) {
  ptr out = Smake_vector(3, Sfalse);
  ptr target = Smake_vector(2, Sfalse);
  ptr event_ls = Snil;
  if ((events & POLLOUT) != 0)
    event_ls = Scons(Sstring_to_symbol("write"), event_ls);
  if ((events & POLLIN) != 0)
    event_ls = Scons(Sstring_to_symbol("read"), event_ls);
  Svector_set(target, 0, Sinteger((iptr)t->fd));
  Svector_set(target, 1, event_ls);
  Svector_set(out, 0, Sstring_to_symbol("pending"));
  Svector_set(out, 1, target);
  /* Offset counts bytes accepted by SSH (upload) or written to disk (download).
     The remote download size is unknown until its file header has been parsed. */
  Svector_set(out, 2,
              Scons(Sunsigned64(t->offset),
                    (t->direction == 1 || t->phase >= 5) ?
                    Sunsigned64(t->offset + t->remaining) : Sfalse));
  return out;
}

static ptr scp_transfer_retry_pending(cspp_scp_transfer *t, int events) {
  int flags = ssh_get_poll_flags(t->owner->session);
  int requested = 0;
  if ((flags & SSH_WRITE_PENDING) != 0) requested |= POLLOUT;
  if ((flags & SSH_READ_PENDING) != 0) requested |= POLLIN;
  t->wait_events = requested == 0 ? events : requested;
  return Sfalse;
}

static ptr scp_transfer_step_once(cspp_scp_transfer *t) {
  int rc;
  if (t == NULL || t->cancelled || t->channel == NULL)
    return make_error_status_message("invalid or cancelled scp transfer");
  if (t->phase == 0) {
    rc = ssh_channel_open_session(t->channel);
    if (rc == SSH_AGAIN) return scp_transfer_retry_pending(t, POLLIN | POLLOUT);
    if (rc != SSH_OK) return scp_channel_error(t, "scp channel open failed");
    t->initialized = 1;
    t->phase = 1;
    return Strue;
  }
  if (t->phase == 1) {
    rc = ssh_channel_request_exec(t->channel, t->command);
    if (rc == SSH_AGAIN) return scp_transfer_retry_pending(t, POLLIN | POLLOUT);
    if (rc != SSH_OK) return scp_channel_error(t, "scp exec request failed");
    t->phase = 2;
    return Strue;
  }
  if (t->phase == 2) {
    if (t->direction == 1) {
      if (t->batch_index == 0) {
        ptr ans = scp_read_ack(t);
        if (ans != Strue) return ans;
      }
      {
        struct stat st;
        if (stat(t->local_path, &st) != 0) return make_errno_status();
        rc = snprintf((char *)t->header, sizeof(t->header), "C%04o %llu %s\n",
                      (unsigned)(st.st_mode & 07777),
                      (unsigned long long)st.st_size, t->remote_name);
        if (rc < 0 || (size_t)rc >= sizeof(t->header))
          return make_error_status_message("scp file header is too long");
        t->header_len = (size_t)rc;
      }
    } else {
      t->header[0] = 0;
      t->header_len = 1;
    }
    t->phase = 3;
    return Strue;
  }
  if (t->phase == 3) {
    if (t->direction == 1) {
      ptr ans = scp_write_pending_buffer(t, t->header, t->header_len, &t->header_pos);
      if (ans != Strue) return ans;
      t->phase = 4;
      return Strue;
    }
    {
      ptr ans = scp_write_pending_buffer(t, t->header, t->header_len, &t->header_pos);
      if (ans != Strue) return ans;
      t->header_len = 0;
      t->header_pos = 0;
      t->phase = 4;
      return Strue;
    }
  }
  if (t->phase == 4) {
    if (t->direction == 1) {
      ptr ans = scp_read_ack(t);
      if (ans != Strue) return ans;
      t->phase = 5;
      return Strue;
    }
    rc = ssh_channel_read(t->channel, t->header + t->header_len, 1, 0);
    if (rc == SSH_AGAIN || rc == 0) return scp_transfer_retry_pending(t, POLLIN);
    if (rc < 0) return scp_channel_error(t, "scp header read failed");
    t->header_len++;
    if (t->header_len >= sizeof(t->header))
      return make_error_status_message("scp file header is too long");
    if (t->header[t->header_len - 1] != '\n') return Strue;
    t->header[t->header_len - 1] = '\0';
    {
      unsigned mode;
      unsigned long long size;
      int name_start = 0;
      if (sscanf((char *)t->header, "C%o %llu%n", &mode, &size, &name_start) != 2 ||
          name_start <= 0 || t->header[name_start++] != ' ')
        return make_error_status_message("remote path is not a regular file");
      if (t->batch_count != 0 &&
          strcmp((char *)t->header + name_start, t->batch_names[t->batch_index]) != 0)
        return make_error_status_message("unexpected SCP batch filename");
      t->remaining = (uint64_t)size;
      (void)fchmod(t->local_fd, (mode_t)mode);
      t->header[0] = 0;
      t->header_len = 1;
      t->header_pos = 0;
      t->phase = 5;
      return Strue;
    }
  }
  if (t->phase == 5) {
    if (t->direction == 0 && t->header_pos < t->header_len) {
      ptr ans = scp_write_pending_buffer(t, t->header, t->header_len, &t->header_pos);
      if (ans != Strue) return ans;
      return Strue;
    }
    if (t->direction == 1) {
      ssize_t n;
      if (t->remaining == 0) {
        t->header[0] = 0;
        t->header_len = 1;
        t->header_pos = 0;
        t->phase = 6;
        return Strue;
      }
      if (t->buffer_pos == t->buffer_len) {
        size_t want = t->remaining > sizeof(t->buffer) ?
                      sizeof(t->buffer) : (size_t)t->remaining;
        n = read(t->local_fd, t->buffer, want);
        if (n < 0) {
          if (errno == EINTR) return scp_transfer_pending(t, POLLOUT);
          return make_errno_status();
        }
        if (n == 0) return make_error_status_message("local file became shorter during upload");
        t->buffer_len = (size_t)n;
        t->buffer_pos = 0;
      }
      {
        ptr ans = scp_write_pending_buffer(t, t->buffer, t->buffer_len, &t->buffer_pos);
        if (ans != Strue) return ans;
        t->offset += (uint64_t)t->buffer_len;
        t->remaining -= (uint64_t)t->buffer_len;
        return Strue;
      }
    }
    if (t->remaining == 0) {
      unsigned char end_marker;
      rc = ssh_channel_read(t->channel, &end_marker, 1, 0);
      if (rc == SSH_AGAIN || rc == 0) return scp_transfer_retry_pending(t, POLLIN);
      if (rc < 0) return scp_channel_error(t, "scp end marker read failed");
      if (end_marker != 0) return make_error_status_message("invalid scp end marker");
      t->header[0] = 0;
      t->header_len = 1;
      t->header_pos = 0;
      t->phase = 6;
      return Strue;
    }
    {
      size_t want = t->remaining > sizeof(t->buffer) ? sizeof(t->buffer) : (size_t)t->remaining;
      rc = ssh_channel_read(t->channel, t->buffer, (uint32_t)want, 0);
      if (rc == SSH_AGAIN || rc == 0) return scp_transfer_retry_pending(t, POLLIN);
      if (rc < 0) return scp_channel_error(t, "scp data read failed");
      {
        size_t written = 0;
        while (written < (size_t)rc) {
          ssize_t n = write(t->local_fd, t->buffer + written, (size_t)rc - written);
          if (n < 0) {
            if (errno == EINTR) continue;
            return make_errno_status();
          }
          if (n == 0) { errno = EIO; return make_errno_status(); }
          written += (size_t)n;
        }
      }
      t->offset += (uint64_t)rc;
      t->remaining -= (uint64_t)rc;
      return Strue;
    }
  }
  if (t->phase == 6) {
    ptr ans = scp_write_pending_buffer(t, t->header, t->header_len, &t->header_pos);
    if (ans != Strue) return ans;
    if (t->direction == 1) {
      t->phase = 7;
      return Strue;
    }
    t->completed = 1;
    t->phase = 8;
    {
      ptr out = Smake_vector(3, Sfalse);
      Svector_set(out, 0, Sstring_to_symbol("completed"));
      Svector_set(out, 1, Strue);
      Svector_set(out, 2, Scons(Sunsigned64(t->offset), Sunsigned64(t->offset)));
      return out;
    }
  }
  if (t->phase == 7) {
    ptr ans = scp_read_ack(t);
    if (ans != Strue) return ans;
    t->completed = 1;
    t->phase = 8;
    {
      ptr out = Smake_vector(3, Sfalse);
      Svector_set(out, 0, Sstring_to_symbol("completed"));
      Svector_set(out, 1, Strue);
      Svector_set(out, 2, Scons(Sunsigned64(t->offset), Sunsigned64(t->offset)));
      return out;
    }
  }
  return make_error_status_message("scp transfer is already complete");
}

static ptr scp_transfer_step(cspp_scp_transfer *t) {
  unsigned pass;
  uint64_t initial_offset;
  if (t == NULL || t->cancelled || t->channel == NULL)
    return make_error_status_message("invalid or cancelled scp transfer");
  if (t->phase == 9) {
    ptr result = scp_batch_next_file(t);
    if (result != Strue) return result;
  }
  initial_offset = t->offset;
  /* Drain buffered protocol input without allocating a Scheme update and polling
   * after every SSH read. Stop at backpressure, 16 operations, or 128 KiB so
   * callers still observe progress, deadlines, and cancellation regularly. */
  for (pass = 0; pass < 16; pass++) {
    ptr result = scp_transfer_step_once(t);
    if (result == Sfalse) {
      struct pollfd descriptor = {t->fd, (short)t->wait_events, 0};
      int ready;
      if (pass + 1 == 16) return scp_batch_status(t, scp_transfer_pending(t, t->wait_events));
      /* A nonblocking SSH read can consume only a transport fragment and still
       * return SSH_AGAIN. Retry ready input here instead of crossing the FFI
       * with an allocated pending update for every fragment. Never wait here. */
      ready = poll(&descriptor, 1, 0);
      if (ready < 0 && errno != EINTR) return make_errno_status();
      if (ready <= 0) return scp_batch_status(t, scp_transfer_pending(t, t->wait_events));
    } else if (result != Strue) {
      return scp_batch_status(t, result);
    }
    if (t->offset - initial_offset >= 131072) break;
  }
  return scp_batch_status(t, scp_transfer_progress(t));
}

/* Keep the session in its caller's mode between steps: progress callbacks can
 * open SFTP channels or inspect metadata on the same authenticated session. */
ptr cspp_net_scp_transfer_step(uptr handle) {
  cspp_scp_transfer *t = (cspp_scp_transfer *)TO_VOIDP(handle);
  int blocking;
  ptr result;
  if (t == NULL || t->owner == NULL || t->owner->session == NULL)
    return make_error_status_message("invalid scp transfer");
  blocking = ssh_is_blocking(t->owner->session);
  ssh_set_blocking(t->owner->session, 0);
  result = scp_transfer_step(t);
  ssh_set_blocking(t->owner->session, blocking);
  return result;
}

ptr cspp_net_scp_transfer_cancel(uptr handle) {
  cspp_scp_transfer *t = (cspp_scp_transfer *)TO_VOIDP(handle);
  if (t != NULL) t->cancelled = 1;
  return Strue;
}

void cspp_net_scp_transfer_close(uptr handle) {
  cspp_scp_transfer *t = (cspp_scp_transfer *)TO_VOIDP(handle);
  int remove_partial, blocking;
  if (t == NULL) return;
  /* A failed open must not unlink an existing destination we never opened. */
  remove_partial = t->direction == 0 && !t->completed && t->local_fd >= 0;
  if (t->local_fd >= 0) close(t->local_fd);
  if (t->channel != NULL) {
    /* Cancellation and close must not wait for remote channel replies. */
    blocking = ssh_is_blocking(t->owner->session);
    ssh_set_blocking(t->owner->session, 0);
    (void)ssh_channel_send_eof(t->channel);
    (void)ssh_channel_close(t->channel);
    ssh_channel_free(t->channel);
    ssh_set_blocking(t->owner->session, blocking);
  }
  if (remove_partial && t->local_path != NULL)
    (void)unlink(t->local_path);
  scp_free_names(t->batch_names, t->batch_count);
  free(t->local_directory); free(t->remote_directory);
  free(t->command); free(t->local_path); free(t->remote_name); free(t);
}

int cspp_net_sftp_flag_read(void) { return O_RDONLY; }
int cspp_net_sftp_flag_write(void) { return O_WRONLY; }
int cspp_net_sftp_flag_read_write(void) { return O_RDWR; }
int cspp_net_sftp_flag_append(void) { return O_APPEND; }
int cspp_net_sftp_flag_create(void) { return O_CREAT; }
int cspp_net_sftp_flag_truncate(void) { return O_TRUNC; }
int cspp_net_sftp_flag_exclusive(void) { return O_EXCL; }
#ifdef O_TEXT
int cspp_net_sftp_flag_text(void) { return O_TEXT; }
#else
int cspp_net_sftp_flag_text(void) { return 0; }
#endif
