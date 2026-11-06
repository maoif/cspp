#include "common.h"


ptr errno_str() {
  if (errno == 0) {
    // e.g., getpwnam("bad_user")
    return Sstring("internal error");
  }
  return Sstring_utf8(strerror(errno), -1);
}

ptr errno_str_vector() {
  ptr v = Smake_vector(1, Sfalse);
  Svector_set(v, 0, errno_str());
  return v;
}

static ptr symbol_or_false(const char *name) {
  return name == NULL ? Sfalse : Sstring_to_symbol(name);
}

ptr cspp_ok(ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol("ok"));
  Svector_set(v, 1, value);
  return v;
}

ptr cspp_errno_result(const char *operation, ptr context) {
  int code = errno;
  ptr v = Smake_vector(5, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol("errno"));
  Svector_set(v, 1, symbol_or_false(operation));
  Svector_set(v, 2, Sfixnum(code));
  Svector_set(v, 3, code == 0 ? Sstring("internal error") : Sstring_utf8(strerror(code), -1));
  Svector_set(v, 4, context);
  return v;
}

ptr cspp_not_found_result(const char *operation, ptr context) {
  ptr v = Smake_vector(3, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol("not-found"));
  Svector_set(v, 1, symbol_or_false(operation));
  Svector_set(v, 2, context);
  return v;
}

ptr cspp_unsupported_result(const char *operation) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol("unsupported"));
  Svector_set(v, 1, symbol_or_false(operation));
  return v;
}

ptr cspp_timeout_result(const char *operation, ptr context) {
  ptr v = Smake_vector(3, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol("timeout"));
  Svector_set(v, 1, symbol_or_false(operation));
  Svector_set(v, 2, context);
  return v;
}

// from ChezScheme: io.c
// The caller has to free the returned pointer.
#define DIRMARKERP(c) ((c) == '/')
char *expand_pathname(const char *inpath) {
  char *outpath;
  const char *ip;

  if (*inpath == '~') {
    char *dir;
    size_t n1, n2;
    if (*(ip = inpath + 1) == 0 || DIRMARKERP(*ip)) {
      dir = cspp_environment_copy("HOME");
      if (dir == NULL) dir = cspp_user_home(NULL);
    } else {
      char *userbuf;
      const char *user_start = ip;
      do {
        ip += 1;
      } while (*ip != 0 && !DIRMARKERP(*ip));
      if ((userbuf = malloc(ip - user_start + 1)) == NULL)
        return NULL;
      memcpy(userbuf, user_start, ip - user_start);
      userbuf[ip - user_start] = 0;
      dir = cspp_user_home(userbuf);
      free(userbuf);
    }
    if (dir != NULL) {
      n1 = strlen(dir);
      n2 = strlen(ip) + 1;
      if ((outpath = malloc(n1 + n2)) == NULL) { free(dir); return NULL; }
      memcpy(outpath, dir, n1);
      memcpy(outpath + n1, ip, n2);
      free(dir);
      return outpath;
    }
  }

  /* if no ~ or tilde dir can't be found, copy inpath */
  {
    size_t n = strlen(inpath) + 1;
    if ((outpath = (char *)malloc(n)) == NULL)
      return NULL;
    memcpy(outpath, inpath, n);
    return outpath;
  }
}
