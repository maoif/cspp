#define _POSIX_C_SOURCE 200809L
#include "common.h"

/* NSS records and their strings remain in a per-call buffer until all fields
 * have been copied. No Scheme storage is accessed while the thread is inactive. */
static ptr user_lookup(const char *name, unsigned int id, int group) {
  const char *operation = group ? (name ? "getgrnam" : "getgrgid")
                                : (name ? "getpwnam" : "getpwuid");
  char *owned_name = name ? strdup(name) : NULL;
  if (name && !owned_name) { errno = ENOMEM; return cspp_errno_result(operation, Snil); }
  struct passwd pw, *pw_result = NULL;
  struct group gr, *gr_result = NULL;
  size_t capacity = 1024;
  char *buffer = NULL;
  int error = 0;
  Sdeactivate_thread();
  for (;;) {
    char *next = realloc(buffer, capacity);
    if (!next) { error = ENOMEM; break; }
    buffer = next;
    if (group)
      error = owned_name ? getgrnam_r(owned_name, &gr, buffer, capacity, &gr_result)
                         : getgrgid_r((gid_t)id, &gr, buffer, capacity, &gr_result);
    else
      error = owned_name ? getpwnam_r(owned_name, &pw, buffer, capacity, &pw_result)
                         : getpwuid_r((uid_t)id, &pw, buffer, capacity, &pw_result);
    if (error != ERANGE) break;
    if (capacity > SIZE_MAX / 2) { error = ENOMEM; break; }
    capacity *= 2;
  }
  Sactivate_thread();
  ptr context = Scons(Scons(Sstring(owned_name ? "name" : "id"),
                           owned_name ? Sstring_utf8(owned_name, -1) : Sunsigned(id)), Snil);
  ptr result;
  if (error) {
    errno = error;
    result = cspp_errno_result(operation, context);
  } else if (!(group ? (void *)gr_result : (void *)pw_result)) {
    result = cspp_not_found_result(operation, context);
  } else if (group) {
    result = Smake_vector(4, Sfalse);
    Svector_set(result, 0, Sstring_utf8(gr.gr_name, -1));
    Svector_set(result, 1, Sstring_utf8(gr.gr_passwd ? gr.gr_passwd : "", -1));
    Svector_set(result, 2, Sunsigned(gr.gr_gid));
    size_t count = 0;
    while (gr.gr_mem && gr.gr_mem[count]) count++;
    if (count) {
      ptr members = Smake_vector((iptr)count, Sfalse);
      for (size_t i = 0; i < count; i++) Svector_set(members, (iptr)i, Sstring_utf8(gr.gr_mem[i], -1));
      Svector_set(result, 3, members);
    }
  } else {
    result = Smake_vector(7, Sfalse);
    Svector_set(result, 0, Sstring_utf8(pw.pw_name, -1));
    Svector_set(result, 1, Sstring_utf8(pw.pw_passwd ? pw.pw_passwd : "", -1));
    Svector_set(result, 2, Sunsigned(pw.pw_uid));
    Svector_set(result, 3, Sunsigned(pw.pw_gid));
    Svector_set(result, 4, Sstring_utf8(pw.pw_gecos ? pw.pw_gecos : "", -1));
    Svector_set(result, 5, Sstring_utf8(pw.pw_dir, -1));
    Svector_set(result, 6, Sstring_utf8(pw.pw_shell, -1));
  }
  if (!error && (group ? gr_result != NULL : pw_result != NULL)) result = cspp_ok(result);
  free(buffer);
  free(owned_name);
  return result;
}

ptr cspp_getpwnam(const char *name) { return user_lookup(name, 0, 0); }
ptr cspp_getpwuid(unsigned int id) { return user_lookup(NULL, id, 0); }
ptr cspp_getgrnam(const char *name) { return user_lookup(name, 0, 1); }
ptr cspp_getgrgid(unsigned int id) { return user_lookup(NULL, id, 1); }
unsigned int cspp_getuid(void) { return getuid(); }
unsigned int cspp_getgid(void) { return getgid(); }
unsigned int cspp_geteuid(void) { return geteuid(); }
unsigned int cspp_getegid(void) { return getegid(); }

/* Tilde expansion needs a native-owned directory without shared NSS storage. */
char *cspp_user_home(const char *name) {
  size_t capacity = 1024;
  char *buffer = NULL, *home = NULL;
  struct passwd record, *result = NULL;
  int code;
  for (;;) {
    char *next = realloc(buffer, capacity);
    if (!next) break;
    buffer = next;
    code = name ? getpwnam_r(name, &record, buffer, capacity, &result)
                : getpwuid_r(getuid(), &record, buffer, capacity, &result);
    if (code == ERANGE && capacity <= SIZE_MAX / 2) { capacity *= 2; continue; }
    if (!code && result) home = strdup(result->pw_dir);
    break;
  }
  free(buffer);
  return home;
}
