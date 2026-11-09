#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <stdatomic.h>
#include <stddef.h>

/* Preloaded only in the isolated native argument-copy regression. Fail one
 * allocation of the requested size without constraining Chez or the emulator. */
static _Atomic size_t failure_size;
static _Atomic int failed;

void cspp_allocation_fixture_arm(size_t size) {
  failed = 0;
  failure_size = size;
}

int cspp_allocation_fixture_failed(void) { return failed; }

void *malloc(size_t size) {
  size_t expected = size;
  if (size && atomic_compare_exchange_strong(&failure_size, &expected, 0)) {
    failed = 1;
    errno = ENOMEM;
    return NULL;
  }
  void *(*allocate)(size_t) = dlsym(RTLD_NEXT, "malloc");
  return allocate(size);
}
