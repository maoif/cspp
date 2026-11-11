#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <stdatomic.h>
#include <stddef.h>

/* Preloaded only in the isolated native argument-copy regression. Fail one
 * allocation of the requested size without constraining Chez or the emulator. */
static _Atomic size_t failure_size;
static _Atomic int failed;
static _Atomic(void *(*)(size_t)) real_malloc;

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
  /* Resolve once: repeated dlsym calls can consume a dependency's dlerror state
   * while that dependency is initializing during dlopen. */
  void *(*allocate)(size_t) = atomic_load(&real_malloc);
  if (allocate == NULL) {
    allocate = dlsym(RTLD_NEXT, "malloc");
    atomic_store(&real_malloc, allocate);
  }
  return allocate(size);
}
