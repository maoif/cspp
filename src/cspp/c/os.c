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
#if defined(__linux__)
#include <sched.h>
#endif

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
  /* A vfork child cannot return through this wrapper or Chez's FFI stack. */
  return cspp_unsupported_result("vfork");
}

typedef struct {
  char **names;
  size_t count;
  size_t capacity;
  int error;
} shared_object_snapshot;

/* dl_iterate_phdr holds the loader lock. Scheme allocation can acquire Chez's
 * runtime lock, which load-shared-object holds before entering dlopen. Keep
 * this callback entirely native to avoid that reversed lock order. */
static int shared_object_list_callback(struct dl_phdr_info *info, size_t size, void *data) {
  (void)size;
  shared_object_snapshot *snapshot = data;
  if (snapshot->count == snapshot->capacity) {
    size_t capacity = snapshot->capacity ? snapshot->capacity * 2 : 32;
    if (capacity < snapshot->capacity || capacity > SIZE_MAX / sizeof(char *)) {
      snapshot->error = ENOMEM;
      return 1;
    }
    char **names = realloc(snapshot->names, capacity * sizeof(char *));
    if (!names) { snapshot->error = ENOMEM; return 1; }
    snapshot->names = names;
    snapshot->capacity = capacity;
  }
  char *name = strdup(info->dlpi_name);
  if (!name) { snapshot->error = ENOMEM; return 1; }
  snapshot->names[snapshot->count++] = name;
  return 0;
}

ptr cspp_shared_object_list() {
  shared_object_snapshot snapshot = {0};
  /* Loader lock contention can block. Deactivation permits concurrent GC;
   * snapshot owns native strings and the callback never touches Scheme data. */
  Sdeactivate_thread();
  dl_iterate_phdr(shared_object_list_callback, &snapshot);
  /* Reactivate before constructing the Scheme list or any error condition. */
  Sactivate_thread();
  ptr objects = Snil;
  for (size_t i = 0; i < snapshot.count; i++) {
    if (!snapshot.error) objects = Scons(Sstring_utf8(snapshot.names[i], -1), objects);
    free(snapshot.names[i]);
  }
  free(snapshot.names);
  if (snapshot.error) {
    errno = snapshot.error;
    return cspp_errno_result("shared-object-list", Snil);
  }
  return cspp_ok(objects);
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

//=======================================================================
//
// system info
//
//=======================================================================

/* Chez converts sleep seconds through uint64_t before assigning time_t. */
Suint64_t cspp_sleep_max_seconds(void) {
  _Static_assert(sizeof(time_t) <= sizeof(uint64_t), "time_t exceeds Chez sleep range");
  unsigned int bits = sizeof(time_t) * CHAR_BIT;
  return UINT64_MAX >> (64 - bits + ((time_t)-1 < (time_t)0));
}


ptr cspp_hostname() {
  struct utsname sysinfo;
  if (uname(&sysinfo) == 0) {
    return cspp_ok(Sstring_utf8(sysinfo.nodename, -1));
  }
  return cspp_errno_result("hostname", Snil);
}


ptr cspp_cpu_arch() {
  struct utsname sysinfo;
  if (uname(&sysinfo) == 0) {
    return cspp_ok(Sstring_utf8(sysinfo.machine, -1));
  }
  return cspp_errno_result("cpu-arch", Snil);
}


#if defined(__linux__)
static int affinity_cpu_count(void) {
  cpu_set_t mask;
  CPU_ZERO(&mask);
  if (sched_getaffinity(0, sizeof(mask), &mask) == 0) return CPU_COUNT(&mask);
  if (errno != EINVAL) return 0;

  /* Large kernel CPU masks can exceed CPU_SETSIZE, even with few online CPUs. */
  for (size_t capacity = CPU_SETSIZE * 2; capacity <= INT_MAX; capacity *= 2) {
    size_t size = CPU_ALLOC_SIZE(capacity);
    cpu_set_t *large_mask = CPU_ALLOC(capacity);
    if (large_mask == NULL) return 0;
    CPU_ZERO_S(size, large_mask);
    int result = sched_getaffinity(0, size, large_mask);
    int error = errno;
    int count = result == 0 ? CPU_COUNT_S(size, large_mask) : 0;
    CPU_FREE(large_mask);
    if (result == 0) return count;
    if (error != EINVAL || capacity > INT_MAX / 2) break;
  }
  return 0;
}
#endif

int cspp_cpu_count() {
#if defined(__linux__)
  int available = affinity_cpu_count();
  if (available > 0) return available;
#endif
  long online = sysconf(_SC_NPROCESSORS_ONLN);
  return online > 0 ? (online > INT_MAX ? INT_MAX : (int)online) : 1;
}


//=======================================================================
//
// filesystem info
//
//=======================================================================


ptr cspp_filesystem_info(const char *path) {
#if defined(__unix__) || defined(__APPLE__)
  char *owned_path = strdup(path);
  if (owned_path == NULL) {
    errno = ENOMEM;
    return cspp_errno_result("filesystem-info", Snil);
  }
  struct statvfs vfs;
  struct stat st;
  int error = 0;
  /* Filesystems may block on remote or userspace services. Only native-owned
   * storage crosses deactivation; the FFI's encoded path can move during GC. */
  Sdeactivate_thread();
  /* Resolve once so a concurrent rename or symlink replacement cannot mix
   * capacity and device identity from different targets. O_PATH needs no read
   * permission and does not open devices or wait for FIFO peers. */
#ifdef O_PATH
  int fd = open(owned_path, O_PATH | O_CLOEXEC);
#elif defined(O_EVTONLY)
  int fd = open(owned_path, O_EVTONLY | O_NONBLOCK | O_CLOEXEC);
#else
  int fd = open(owned_path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
#endif
  if (fd < 0 || fstatvfs(fd, &vfs) != 0 || fstat(fd, &st) != 0) error = errno;
  if (fd >= 0) close(fd);
  /* No encoded FFI path or Scheme object was accessed while inactive. Resume
   * GC coordination before allocating the returned vector and strings. */
  Sactivate_thread();
  if (error != 0) {
    free(owned_path);
    errno = error;
    return cspp_errno_result("filesystem-info", Snil);
  }

  ptr v = Smake_vector(10, Sfalse);
  Svector_set(v, 0, Sstring_utf8(owned_path, -1));
  free(owned_path);
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

int cspp_linux_p(void) {
#if defined(__linux__)
  return 1;
#else
  return 0;
#endif
}
