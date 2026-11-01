/* Test-only OpenSSL allocator instrumentation, loaded before crypto initialization. */
#include <openssl/crypto.h>
#include <stdint.h>
#include <stdlib.h>

static uint64_t live_allocations;

static void *tracked_malloc(size_t size, const char *file, int line) {
  void *result;
  (void)file;
  (void)line;
  result = malloc(size == 0 ? 1 : size);
  if (result != NULL) ++live_allocations;
  return result;
}

static void tracked_free(void *address, const char *file, int line) {
  (void)file;
  (void)line;
  if (address != NULL) {
    --live_allocations;
    free(address);
  }
}

static void *tracked_realloc(void *address, size_t size, const char *file, int line) {
  if (address == NULL) return tracked_malloc(size, file, line);
  if (size == 0) {
    tracked_free(address, file, line);
    return NULL;
  }
  return realloc(address, size);
}

int crypto_test_track_memory(void) {
  return CRYPTO_set_mem_functions(tracked_malloc, tracked_realloc, tracked_free);
}

uint64_t crypto_test_live_allocations(void) {
  return live_allocations;
}
