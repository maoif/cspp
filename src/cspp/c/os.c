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

int cspp_linux_p(void) {
#if defined(__linux__)
  return 1;
#else
  return 0;
#endif
}
