#define _GNU_SOURCE
#include "common.h"
#include <locale.h>
#include <wchar.h>

/* strerror uses the current locale's multibyte encoding, not necessarily UTF-8.
 * Snapshot the calling thread's locale and finish native conversion before any
 * Scheme allocation. uselocale changes only this thread, and is restored first. */
static ptr error_message(int code) {
  if (code == 0) return Sstring("internal error");
  locale_t locale = duplocale(uselocale((locale_t)0));
  wchar_t *characters = NULL;
  size_t count = (size_t)-1;
  if (locale) {
    locale_t previous = uselocale(locale);
    const char *message = strerror_l(code, locale);
    mbstate_t state = {0};
    const char *source = message;
    count = mbsrtowcs(NULL, &source, 0, &state);
    if (count != (size_t)-1 && count <= SIZE_MAX / sizeof(*characters) - 1) {
      characters = malloc((count + 1) * sizeof(*characters));
      if (characters) {
        memset(&state, 0, sizeof(state));
        source = message;
        if (mbsrtowcs(characters, &source, count + 1, &state) == (size_t)-1) {
          free(characters);
          characters = NULL;
        }
      }
    }
    uselocale(previous);
    freelocale(locale);
  }
  if (!characters) {
    /* Allocation/decoding failure must not replace the original error code or
     * return malformed text. The C locale's message is an ASCII fallback. */
    locale_t fallback = newlocale(LC_ALL_MASK, "C", (locale_t)0);
    ptr result = fallback ? Sstring(strerror_l(code, fallback)) : Sstring("system error");
    if (fallback) freelocale(fallback);
    return result;
  }
  ptr result = Smake_string((iptr)count, 0);
  for (size_t index = 0; index < count; index++)
    Sstring_set(result, (iptr)index, (uint32_t)characters[index]);
  free(characters);
  return result;
}

ptr errno_str() {
  return error_message(errno);
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
  Svector_set(v, 3, error_message(code));
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
