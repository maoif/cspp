#include "build-config.h"
#include "net/unavailable.h"

#if CSPP_WITH_BLAKE3
#include <blake3.h>

static ptr blake3_result(const blake3_hasher *state) {
  uint8_t out[BLAKE3_OUT_LEN];
  blake3_hasher_finalize(state, out, sizeof(out));
  ptr result = Smake_bytevector(BLAKE3_OUT_LEN, 0);
  memcpy(Sbytevector_data(result), out, sizeof(out));
  return result;
}

ptr crypto_blake3_bytevector(ptr bytes, uint64_t start, uint64_t stop) {
  blake3_hasher state;
  blake3_hasher_init(&state);
  blake3_hasher_update(&state, Sbytevector_data(bytes) + (size_t)start,
                       (size_t)(stop - start));
  return blake3_result(&state);
}

ptr crypto_blake3_string(ptr text, uint64_t start, uint64_t stop) {
  blake3_hasher state;
  blake3_hasher_init(&state);
  for (uint64_t i = start; i < stop; ++i) {
    uint32_t character = (uint32_t)Sstring_ref(text, (iptr)i);
    blake3_hasher_update(&state, &character, sizeof(character));
  }
  return blake3_result(&state);
}

void *crypto_blake3_create(void) {
  blake3_hasher *state = malloc(sizeof(*state));
  if (state != NULL) blake3_hasher_init(state);
  return state;
}

void crypto_blake3_destroy(void *state) {
  free(state);
}

ptr crypto_blake3_get(void *state) {
  return state == NULL ? Sfalse : blake3_result(state);
}

int crypto_blake3_reset(void *state) {
  if (state == NULL) return 0;
  blake3_hasher_reset(state);
  return 1;
}

int crypto_blake3_update_bytevector(void *state, ptr bytes, uint64_t start,
                                   uint64_t stop) {
  if (state == NULL) return 0;
  blake3_hasher_update(state, Sbytevector_data(bytes) + (size_t)start,
                       (size_t)(stop - start));
  return 1;
}

int crypto_blake3_update_string(void *state, ptr text, uint64_t start, uint64_t stop) {
  if (state == NULL) return 0;
  for (uint64_t i = start; i < stop; ++i) {
    uint32_t character = (uint32_t)Sstring_ref(text, (iptr)i);
    blake3_hasher_update(state, &character, sizeof(character));
  }
  return 1;
}

#else
ptr crypto_blake3_bytevector(ptr bytes, uint64_t start, uint64_t stop) {
  (void)bytes; (void)start; (void)stop;
  return cspp_unavailable_status("blake3: disabled at build time");
}

ptr crypto_blake3_string(ptr text, uint64_t start, uint64_t stop) {
  (void)text; (void)start; (void)stop;
  return cspp_unavailable_status("blake3: disabled at build time");
}

void *crypto_blake3_create(void) { return NULL; }
void crypto_blake3_destroy(void *state) { (void)state; }

ptr crypto_blake3_get(void *state) {
  (void)state;
  return cspp_unavailable_status("blake3: disabled at build time");
}

int crypto_blake3_reset(void *state) { (void)state; return -1; }

int crypto_blake3_update_bytevector(void *state, ptr bytes, uint64_t start,
                                   uint64_t stop) {
  (void)state; (void)bytes; (void)start; (void)stop;
  return -1;
}

int crypto_blake3_update_string(void *state, ptr text, uint64_t start, uint64_t stop) {
  (void)state; (void)text; (void)start; (void)stop;
  return -1;
}
#endif
