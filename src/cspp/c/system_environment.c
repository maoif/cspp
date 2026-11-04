#define _POSIX_C_SOURCE 200809L
#include "common.h"
#include <pthread.h>

extern char **environ;
static pthread_mutex_t environment_mutex = PTHREAD_MUTEX_INITIALIZER;

/* All CSPP environment readers, writers, and inherited spawn snapshots share
 * this lock. External libc environment mutation must be coordinated by callers. */
char **cspp_copy_environment(void) {
  pthread_mutex_lock(&environment_mutex);
  size_t count = 0;
  while (environ && environ[count]) count++;
  char **copy = calloc(count + 1, sizeof(char *));
  if (copy) {
    size_t i;
    for (i = 0; i < count; i++) {
      copy[i] = strdup(environ[i]);
      if (!copy[i]) break;
    }
    if (i != count) {
      while (i > 0) free(copy[--i]);
      free(copy);
      copy = NULL;
    }
  }
  pthread_mutex_unlock(&environment_mutex);
  return copy;
}

char *cspp_environment_copy(const char *name) {
  pthread_mutex_lock(&environment_mutex);
  const char *value = getenv(name);
  char *copy = value ? strdup(value) : NULL;
  int error = value && !copy ? ENOMEM : 0;
  pthread_mutex_unlock(&environment_mutex);
  errno = error;
  return copy;
}

ptr cspp_environment_get(const char *name) {
  char *copy = cspp_environment_copy(name);
  if (!copy && errno) return cspp_errno_result("getenv", Snil);
  ptr result = copy ? Sstring_utf8(copy, (iptr)strlen(copy)) : Sfalse;
  free(copy);
  return cspp_ok(result);
}

ptr cspp_environment_set(const char *name, const char *value) {
  pthread_mutex_lock(&environment_mutex);
  int result = setenv(name, value, 1);
  int error = errno;
  pthread_mutex_unlock(&environment_mutex);
  if (result) { errno = error; return cspp_errno_result("putenv", Snil); }
  return cspp_ok(Svoid);
}
