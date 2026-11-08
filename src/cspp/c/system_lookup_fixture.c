#define _GNU_SOURCE
#include <dlfcn.h>
#include <pwd.h>
#include <stdatomic.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* A selected account lookup waits for the Scheme collector, with a bounded fallback.
 * This library is preloaded only in the isolated tilde-lookup regression process. */
static _Atomic int armed, entered, released, finished;

void cspp_lookup_fixture_arm(void) {
  entered = released = finished = 0;
  armed = 1;
}
int cspp_lookup_fixture_entered(void) { return entered; }
int cspp_lookup_fixture_finished(void) { return finished; }
void cspp_lookup_fixture_release(void) { released = 1; }

static int delayed_home(struct passwd *record, struct passwd **result) {
  struct timespec start, now, interval = {0, 1000000};
  clock_gettime(CLOCK_MONOTONIC, &start);
  entered = 1;
  do {
    nanosleep(&interval, NULL);
    clock_gettime(CLOCK_MONOTONIC, &now);
  } while (!released && now.tv_sec - start.tv_sec < 2);
  *record = (struct passwd){
    .pw_name = "__cspp_tilde_gc__", .pw_passwd = "x",
    .pw_uid = getuid(), .pw_gid = getgid(), .pw_gecos = "",
    .pw_dir = "/", .pw_shell = "/bin/sh"
  };
  *result = record;
  finished = 1;
  return 0;
}

int getpwnam_r(const char *name, struct passwd *record, char *buffer, size_t size,
               struct passwd **result) {
  if (strcmp(name, "__cspp_tilde_gc__") == 0 && atomic_exchange(&armed, 0))
    return delayed_home(record, result);
  int (*lookup)(const char *, struct passwd *, char *, size_t, struct passwd **) =
    dlsym(RTLD_NEXT, "getpwnam_r");
  return lookup(name, record, buffer, size, result);
}

int getpwuid_r(uid_t id, struct passwd *record, char *buffer, size_t size,
               struct passwd **result) {
  if (id == getuid() && atomic_exchange(&armed, 0)) return delayed_home(record, result);
  int (*lookup)(uid_t, struct passwd *, char *, size_t, struct passwd **) =
    dlsym(RTLD_NEXT, "getpwuid_r");
  return lookup(id, record, buffer, size, result);
}
