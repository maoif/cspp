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
                           owned_name ? Sstring(owned_name) : Sunsigned(id)), Snil);
  ptr result;
  if (error) {
    errno = error;
    result = cspp_errno_result(operation, context);
  } else if (!(group ? (void *)gr_result : (void *)pw_result)) {
    result = cspp_not_found_result(operation, context);
  } else if (group) {
    result = Smake_vector(4, Sfalse);
    Svector_set(result, 0, Sstring(gr.gr_name));
    Svector_set(result, 1, Sstring(gr.gr_passwd ? gr.gr_passwd : ""));
    Svector_set(result, 2, Sunsigned(gr.gr_gid));
    size_t count = 0;
    while (gr.gr_mem && gr.gr_mem[count]) count++;
    if (count) {
      ptr members = Smake_vector((iptr)count, Sfalse);
      for (size_t i = 0; i < count; i++) Svector_set(members, (iptr)i, Sstring(gr.gr_mem[i]));
      Svector_set(result, 3, members);
    }
  } else {
    result = Smake_vector(7, Sfalse);
    Svector_set(result, 0, Sstring(pw.pw_name));
    Svector_set(result, 1, Sstring(pw.pw_passwd ? pw.pw_passwd : ""));
    Svector_set(result, 2, Sunsigned(pw.pw_uid));
    Svector_set(result, 3, Sunsigned(pw.pw_gid));
    Svector_set(result, 4, Sstring(pw.pw_gecos ? pw.pw_gecos : ""));
    Svector_set(result, 5, Sstring(pw.pw_dir));
    Svector_set(result, 6, Sstring(pw.pw_shell));
  }
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
