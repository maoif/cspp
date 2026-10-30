#include "build-config.h"
#include "common.h"
#include "openssl_loader.h"

#include <arpa/inet.h>
#if CSPP_WITH_OPENSSL
#include <openssl/core_names.h>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/kdf.h>
#include <openssl/pem.h>
#include <openssl/params.h>
#include <openssl/rand.h>
#include <openssl/rsa.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/x509v3.h>
#endif
#include <limits.h>
#include <stdint.h>
#include <time.h>

typedef struct {
  EVP_MD *md;
  EVP_MD_CTX *ctx;
} cspp_digest_state;

typedef struct {
  EVP_MAC *mac;
  EVP_MAC_CTX *ctx;
  unsigned char *key;
  size_t key_len;
  const char *digest_name;
} cspp_hmac_state;

typedef struct {
  EVP_CIPHER *cipher;
  EVP_CIPHER_CTX *ctx;
  int encrypt;
  unsigned char *key;
  size_t key_len;
  unsigned char *iv;
  size_t iv_len;
} cspp_cipher_state;

typedef struct {
  X509_STORE *store;
  X509 *leaf;
  STACK_OF(X509) *chain;
  char *hostname;
} cspp_cert_verify_state;

ptr crypto_openssl_load_error(void) {
  const char *error;

  if (cspp_openssl_require()) return Sfalse;
  error = cspp_optional_library_error(
      (cspp_optional_library *)cspp_openssl_library());
  return error == NULL || error[0] == '\0' ? Sfalse : Sstring(error);
}

static ptr make_bytevector_copy(const unsigned char *buf, size_t len) {
  ptr bv = Smake_bytevector((iptr)len, 0);
  for (size_t i = 0; i < len; i++) {
    Sbytevector_u8_set(bv, (iptr)i, buf[i]);
  }
  return bv;
}

static size_t slice_len(uint64_t start, uint64_t stop) {
  return (size_t)(stop - start);
}

static int uint64_to_int(uint64_t n, int *out) {
  if (n > (uint64_t)INT_MAX) return 0;
  *out = (int)n;
  return 1;
}

static int rand_bytes_all(unsigned char *buf, uint64_t len) {
  uint64_t done = 0;
  while (done < len) {
    uint64_t remaining = len - done;
    int chunk = remaining > (uint64_t)INT_MAX ? INT_MAX : (int)remaining;
    if (RAND_bytes(buf + (size_t)done, chunk) != 1) return 0;
    done += (uint64_t)chunk;
  }
  return 1;
}

static int evp_cipher_update_all(EVP_CIPHER_CTX *ctx, unsigned char *out,
                                 size_t *out_len, const unsigned char *in,
                                 uint64_t in_len) {
  uint64_t done = 0;
  size_t written = 0;
  while (done < in_len) {
    uint64_t remaining = in_len - done;
    int chunk = remaining > (uint64_t)INT_MAX ? INT_MAX : (int)remaining;
    int chunk_out = 0;
    if (EVP_CipherUpdate(ctx, out == NULL ? NULL : out + written, &chunk_out,
                         in + (size_t)done, chunk) != 1) {
      return 0;
    }
    done += (uint64_t)chunk;
    written += (size_t)chunk_out;
  }
  *out_len = written;
  return 1;
}

static ptr make_string_utf32_copy(ptr str, uint64_t start, uint64_t stop) {
  ptr bv = Smake_bytevector((iptr)(slice_len(start, stop) * 4), 0);
  unsigned char *dst = Sbytevector_data(bv);
  for (uint64_t i = start; i < stop; i++) {
    uint32_t c = (uint32_t)Sstring_ref(str, (iptr)i);
    memcpy(dst + ((size_t)(i - start) * 4), &c, 4);
  }
  return bv;
}

static const char *digest_name(ptr which) {
  if (which == Sstring_to_symbol("md5")) return "MD5";
  if (which == Sstring_to_symbol("sha1")) return "SHA1";
  if (which == Sstring_to_symbol("sha224")) return "SHA224";
  if (which == Sstring_to_symbol("sha256")) return "SHA256";
  if (which == Sstring_to_symbol("sha384")) return "SHA384";
  if (which == Sstring_to_symbol("sha512")) return "SHA512";
  if (which == Sstring_to_symbol("sha512-224")) return "SHA512-224";
  if (which == Sstring_to_symbol("sha512-256")) return "SHA512-256";
  if (which == Sstring_to_symbol("sha3-224")) return "SHA3-224";
  if (which == Sstring_to_symbol("sha3-256")) return "SHA3-256";
  if (which == Sstring_to_symbol("sha3-384")) return "SHA3-384";
  if (which == Sstring_to_symbol("sha3-512")) return "SHA3-512";
  if (which == Sstring_to_symbol("blake2b-512")) return "BLAKE2B-512";
  if (which == Sstring_to_symbol("blake2s-256")) return "BLAKE2S-256";
  return NULL;
}

static EVP_MD *fetch_digest(ptr which) {
  const char *name = digest_name(which);
  if (name == NULL) return NULL;
  if (!cspp_openssl_require()) return NULL;
  return EVP_MD_fetch(NULL, name, NULL);
}

static const char *cipher_name(ptr which) {
  if (which == Sstring_to_symbol("aes-128-gcm")) return "AES-128-GCM";
  if (which == Sstring_to_symbol("aes-256-gcm")) return "AES-256-GCM";
  if (which == Sstring_to_symbol("aes-128-ctr")) return "AES-128-CTR";
  if (which == Sstring_to_symbol("aes-256-ctr")) return "AES-256-CTR";
  if (which == Sstring_to_symbol("chacha20")) return "CHACHA20";
  if (which == Sstring_to_symbol("chacha20-poly1305")) return "CHACHA20-POLY1305";
  return NULL;
}

static EVP_CIPHER *fetch_cipher(ptr which) {
  const char *name = cipher_name(which);
  if (name == NULL) return NULL;
  if (!cspp_openssl_require()) return NULL;
  return EVP_CIPHER_fetch(NULL, name, NULL);
}

static ptr digest_ctx_result(EVP_MD_CTX *ctx) {
  unsigned char out[EVP_MAX_MD_SIZE];
  unsigned int outlen = 0;
  if (EVP_DigestFinal_ex(ctx, out, &outlen) != 1) {
    return Sfalse;
  }
  return make_bytevector_copy(out, outlen);
}

static ptr make_symbol_string_pair(const char *tag, ptr value) {
  return Scons(Sstring_to_symbol(tag), value);
}

static ptr make_x509_name_string(X509_NAME *name) {
  BIO *bio = NULL;
  ptr ans = Sfalse;
  char *data = NULL;
  long len;

  if (name == NULL) return Sfalse;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) return Sfalse;
  if (X509_NAME_print_ex(bio, name, 0, XN_FLAG_RFC2253) < 0) goto done;
  len = BIO_ctrl(bio, BIO_CTRL_INFO, 0, &data);
  if (len < 0 || data == NULL) goto done;
  ans = Sstring_utf8(data, (iptr)len);

done:
  if (bio != NULL) BIO_free(bio);
  return ans;
}

static ptr make_iso_time_string(const ASN1_TIME *t) {
  struct tm tm_value;
  char buf[32];
  if (t == NULL) return Sfalse;
  if (ASN1_TIME_to_tm(t, &tm_value) != 1) return Sfalse;
  if (strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &tm_value) == 0) return Sfalse;
  return Sstring(buf);
}

int crypto_random_status() {
  if (!cspp_openssl_require()) return 0;
  return RAND_status();
}

ptr crypto_random_bytevector(uint64_t len) {
  if (!cspp_openssl_require()) return Sfalse;
  ptr bv = Smake_bytevector((iptr)len, 0);
  if (rand_bytes_all(Sbytevector_data(bv), len) != 1) {
    return Sfalse;
  }
  return bv;
}

int crypto_random_fill(ptr bv, uint64_t start, uint64_t stop) {
  if (!cspp_openssl_require()) return 0;
  return rand_bytes_all(Sbytevector_data(bv) + (size_t)start, stop - start);
}

int crypto_constant_time_eq(ptr bv1, uint64_t start1, uint64_t stop1, ptr bv2,
                            uint64_t start2, uint64_t stop2) {
  size_t len1 = slice_len(start1, stop1);
  size_t len2 = slice_len(start2, stop2);
  if (!cspp_openssl_require()) return 0;
  if (len1 != len2) return 0;
  return CRYPTO_memcmp(Sbytevector_data(bv1) + (size_t)start1,
                       Sbytevector_data(bv2) + (size_t)start2, len1) == 0;
}

ptr crypto_digest_bytevector(ptr which, ptr bv, uint64_t start, uint64_t stop) {
  EVP_MD *md = fetch_digest(which);
  EVP_MD_CTX *ctx;
  ptr ans = Sfalse;

  if (md == NULL) return Sfalse;
  ctx = EVP_MD_CTX_new();
  if (ctx == NULL) goto done;
  if (EVP_DigestInit_ex(ctx, md, NULL) != 1) goto done;
  if (EVP_DigestUpdate(ctx, Sbytevector_data(bv) + (size_t)start,
                       slice_len(start, stop)) != 1) {
    goto done;
  }
  ans = digest_ctx_result(ctx);

done:
  if (ctx != NULL) EVP_MD_CTX_free(ctx);
  EVP_MD_free(md);
  return ans;
}

ptr crypto_digest_string(ptr which, ptr str, uint64_t start, uint64_t stop) {
  ptr bv = make_string_utf32_copy(str, start, stop);
  return crypto_digest_bytevector(which, bv, 0, slice_len(start, stop) * 4);
}

int crypto_digest_output_size(ptr which) {
  EVP_MD *md = fetch_digest(which);
  int size = -1;
  if (md != NULL) {
    size = EVP_MD_get_size(md);
    EVP_MD_free(md);
  }
  return size;
}

int crypto_digest_block_size(ptr which) {
  EVP_MD *md = fetch_digest(which);
  int size = -1;
  if (md != NULL) {
    size = EVP_MD_get_block_size(md);
    EVP_MD_free(md);
  }
  return size;
}

void *crypto_digest_state_create(ptr which) {
  cspp_digest_state *st = malloc(sizeof(cspp_digest_state));
  if (st == NULL) return NULL;

  st->md = fetch_digest(which);
  st->ctx = NULL;
  if (st->md == NULL) goto fail;

  st->ctx = EVP_MD_CTX_new();
  if (st->ctx == NULL) goto fail;

  if (EVP_DigestInit_ex(st->ctx, st->md, NULL) != 1) goto fail;
  return st;

fail:
  if (st->ctx != NULL) EVP_MD_CTX_free(st->ctx);
  if (st->md != NULL) EVP_MD_free(st->md);
  free(st);
  return NULL;
}

void crypto_digest_state_destroy(void *ptr_st) {
  cspp_digest_state *st = (cspp_digest_state *)ptr_st;
  if (st == NULL) return;
  if (st->ctx != NULL) EVP_MD_CTX_free(st->ctx);
  if (st->md != NULL) EVP_MD_free(st->md);
  free(st);
}

ptr crypto_digest_state_get(void *ptr_st) {
  cspp_digest_state *st = (cspp_digest_state *)ptr_st;
  EVP_MD_CTX *dup;
  ptr ans;
  if (st == NULL || st->ctx == NULL) return Sfalse;
  dup = EVP_MD_CTX_new();
  if (dup == NULL) return Sfalse;
  if (EVP_MD_CTX_copy_ex(dup, st->ctx) != 1) {
    EVP_MD_CTX_free(dup);
    return Sfalse;
  }
  ans = digest_ctx_result(dup);
  EVP_MD_CTX_free(dup);
  return ans;
}

int crypto_digest_state_reset(void *ptr_st) {
  cspp_digest_state *st = (cspp_digest_state *)ptr_st;
  if (st == NULL || st->ctx == NULL || st->md == NULL) return 0;
  if (EVP_MD_CTX_reset(st->ctx) != 1) return 0;
  return EVP_DigestInit_ex(st->ctx, st->md, NULL);
}

int crypto_digest_state_update_bytevector(void *ptr_st, ptr bv, uint64_t start,
                                        uint64_t stop) {
  cspp_digest_state *st = (cspp_digest_state *)ptr_st;
  if (st == NULL || st->ctx == NULL) return 0;
  return EVP_DigestUpdate(st->ctx, Sbytevector_data(bv) + (size_t)start,
                          slice_len(start, stop));
}

int crypto_digest_state_update_string(void *ptr_st, ptr str, uint64_t start,
                                    uint64_t stop) {
  ptr bv = make_string_utf32_copy(str, start, stop);
  return crypto_digest_state_update_bytevector(ptr_st, bv, 0, slice_len(start, stop) * 4);
}

static int hmac_init_state(cspp_hmac_state *st) {
  OSSL_PARAM params[2];
  params[0] = OSSL_PARAM_construct_utf8_string(OSSL_MAC_PARAM_DIGEST,
                                               (char *)st->digest_name, 0);
  params[1] = OSSL_PARAM_construct_end();
  return EVP_MAC_init(st->ctx, st->key, st->key_len, params);
}

void *crypto_hmac_state_create(ptr which, ptr key, uint64_t start, uint64_t stop) {
  cspp_hmac_state *st = malloc(sizeof(cspp_hmac_state));
  size_t key_len = slice_len(start, stop);

  if (st == NULL) return NULL;
  st->mac = NULL;
  st->ctx = NULL;
  st->key = NULL;
  st->key_len = key_len;
  st->digest_name = digest_name(which);
  if (st->digest_name == NULL) goto fail;

  if (!cspp_openssl_require()) goto fail;
  st->mac = EVP_MAC_fetch(NULL, "HMAC", NULL);
  if (st->mac == NULL) goto fail;

  st->ctx = EVP_MAC_CTX_new(st->mac);
  if (st->ctx == NULL) goto fail;

  st->key = malloc(key_len == 0 ? 1 : key_len);
  if (st->key == NULL) goto fail;
  if (key_len != 0) {
    memcpy(st->key, Sbytevector_data(key) + (size_t)start, key_len);
  }

  if (hmac_init_state(st) != 1) goto fail;
  return st;

fail:
  if (st->key != NULL) {
    OPENSSL_cleanse(st->key, key_len);
    free(st->key);
  }
  if (st->ctx != NULL) EVP_MAC_CTX_free(st->ctx);
  if (st->mac != NULL) EVP_MAC_free(st->mac);
  free(st);
  return NULL;
}

void crypto_hmac_state_destroy(void *ptr_st) {
  cspp_hmac_state *st = (cspp_hmac_state *)ptr_st;
  if (st == NULL) return;
  if (st->key != NULL) {
    OPENSSL_cleanse(st->key, st->key_len);
    free(st->key);
  }
  if (st->ctx != NULL) EVP_MAC_CTX_free(st->ctx);
  if (st->mac != NULL) EVP_MAC_free(st->mac);
  free(st);
}

ptr crypto_hmac_state_get(void *ptr_st) {
  cspp_hmac_state *st = (cspp_hmac_state *)ptr_st;
  EVP_MAC_CTX *dup;
  size_t out_len;
  unsigned char *out;
  ptr ans;

  if (st == NULL || st->ctx == NULL) return Sfalse;
  dup = EVP_MAC_CTX_dup(st->ctx);
  if (dup == NULL) return Sfalse;
  out_len = EVP_MAC_CTX_get_mac_size(dup);
  out = malloc(out_len == 0 ? 1 : out_len);
  if (out == NULL) {
    EVP_MAC_CTX_free(dup);
    return Sfalse;
  }
  if (EVP_MAC_final(dup, out, &out_len, out_len) != 1) {
    EVP_MAC_CTX_free(dup);
    free(out);
    return Sfalse;
  }
  ans = make_bytevector_copy(out, out_len);
  EVP_MAC_CTX_free(dup);
  OPENSSL_cleanse(out, out_len);
  free(out);
  return ans;
}

ptr crypto_hmac_state_finalize(void *ptr_st) {
  cspp_hmac_state *st = (cspp_hmac_state *)ptr_st;
  size_t out_len;
  unsigned char *out;
  ptr ans;

  if (st == NULL || st->ctx == NULL) return Sfalse;
  out_len = EVP_MAC_CTX_get_mac_size(st->ctx);
  out = malloc(out_len == 0 ? 1 : out_len);
  if (out == NULL) return Sfalse;
  if (EVP_MAC_final(st->ctx, out, &out_len, out_len) != 1) {
    free(out);
    return Sfalse;
  }
  ans = make_bytevector_copy(out, out_len);
  OPENSSL_cleanse(out, out_len);
  free(out);
  return ans;
}

int crypto_hmac_state_reset(void *ptr_st) {
  cspp_hmac_state *st = (cspp_hmac_state *)ptr_st;
  if (st == NULL || st->ctx == NULL) return 0;
  return hmac_init_state(st);
}

int crypto_hmac_state_update_bytevector(void *ptr_st, ptr bv, uint64_t start,
                                        uint64_t stop) {
  cspp_hmac_state *st = (cspp_hmac_state *)ptr_st;
  if (st == NULL || st->ctx == NULL) return 0;
  return EVP_MAC_update(st->ctx, Sbytevector_data(bv) + (size_t)start,
                        slice_len(start, stop));
}

int crypto_hmac_state_update_string(void *ptr_st, ptr str, uint64_t start,
                                    uint64_t stop) {
  ptr bv = make_string_utf32_copy(str, start, stop);
  return crypto_hmac_state_update_bytevector(ptr_st, bv, 0, slice_len(start, stop) * 4);
}

static ptr hkdf_common(int mode, ptr which, ptr ikm, uint64_t ikm_start,
                       uint64_t ikm_stop, ptr salt, uint64_t salt_start,
                       uint64_t salt_stop, ptr info, uint64_t info_start,
                       uint64_t info_stop, int out_len) {
  EVP_KDF *kdf = NULL;
  EVP_KDF_CTX *ctx = NULL;
  OSSL_PARAM params[6];
  int idx = 0;
  int mode_copy = mode;
  unsigned char *out_buf = NULL;
  ptr ans = Sfalse;
  const char *md_name = digest_name(which);

  if (md_name == NULL || out_len < 0) return Sfalse;

  if (!cspp_openssl_require()) return Sfalse;
  kdf = EVP_KDF_fetch(NULL, "HKDF", NULL);
  if (kdf == NULL) goto done;
  ctx = EVP_KDF_CTX_new(kdf);
  if (ctx == NULL) goto done;

  params[idx++] =
      OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_DIGEST, (char *)md_name, 0);
  params[idx++] = OSSL_PARAM_construct_int(OSSL_KDF_PARAM_MODE, &mode_copy);
  params[idx++] = OSSL_PARAM_construct_octet_string(
      OSSL_KDF_PARAM_KEY, Sbytevector_data(ikm) + (size_t)ikm_start,
      slice_len(ikm_start, ikm_stop));
  params[idx++] = OSSL_PARAM_construct_octet_string(
      OSSL_KDF_PARAM_SALT, Sbytevector_data(salt) + (size_t)salt_start,
      slice_len(salt_start, salt_stop));
  params[idx++] = OSSL_PARAM_construct_octet_string(
      OSSL_KDF_PARAM_INFO, Sbytevector_data(info) + (size_t)info_start,
      slice_len(info_start, info_stop));
  params[idx++] = OSSL_PARAM_construct_end();

  out_buf = malloc(out_len == 0 ? 1 : (size_t)out_len);
  if (out_buf == NULL) goto done;
  if (EVP_KDF_derive(ctx, out_buf, (size_t)out_len, params) != 1) goto done;
  ans = make_bytevector_copy(out_buf, out_len);

done:
  if (out_buf != NULL) {
    OPENSSL_cleanse(out_buf, (size_t)out_len);
    free(out_buf);
  }
  if (ctx != NULL) EVP_KDF_CTX_free(ctx);
  if (kdf != NULL) EVP_KDF_free(kdf);
  return ans;
}

ptr crypto_hkdf(ptr which, ptr ikm, uint64_t ikm_start, uint64_t ikm_stop, ptr salt,
                uint64_t salt_start, uint64_t salt_stop, ptr info,
                uint64_t info_start, uint64_t info_stop, int out_len) {
  return hkdf_common(EVP_KDF_HKDF_MODE_EXTRACT_AND_EXPAND, which, ikm, ikm_start,
                     ikm_stop, salt, salt_start, salt_stop, info, info_start,
                     info_stop, out_len);
}

ptr crypto_hkdf_extract(ptr which, ptr ikm, uint64_t ikm_start, uint64_t ikm_stop,
                        ptr salt, uint64_t salt_start, uint64_t salt_stop) {
  EVP_MD *md = fetch_digest(which);
  int out_len;
  ptr ans;
  if (md == NULL) return Sfalse;
  out_len = EVP_MD_get_size(md);
  EVP_MD_free(md);
  ans = hkdf_common(EVP_KDF_HKDF_MODE_EXTRACT_ONLY, which, ikm, ikm_start, ikm_stop,
                    salt, salt_start, salt_stop, Smake_bytevector(0, 0), 0, 0,
                    out_len);
  return ans;
}

ptr crypto_hkdf_expand(ptr which, ptr prk, uint64_t prk_start, uint64_t prk_stop,
                       ptr info, uint64_t info_start, uint64_t info_stop, int out_len) {
  ptr empty = Smake_bytevector(0, 0);
  return hkdf_common(EVP_KDF_HKDF_MODE_EXPAND_ONLY, which, prk, prk_start, prk_stop,
                     empty, 0, 0, info, info_start, info_stop, out_len);
}

ptr crypto_pbkdf2(ptr which, ptr password, uint64_t pw_start, uint64_t pw_stop,
                  ptr salt, uint64_t salt_start, uint64_t salt_stop, int iterations,
                  int out_len) {
  EVP_MD *md = fetch_digest(which);
  unsigned char *out_buf = NULL;
  ptr ans = Sfalse;
  int pw_len, salt_len;

  if (md == NULL) return Sfalse;
  if (iterations <= 0 || out_len < 0) goto done;
  if (!uint64_to_int(pw_stop - pw_start, &pw_len)) goto done;
  if (!uint64_to_int(salt_stop - salt_start, &salt_len)) goto done;
  out_buf = malloc(out_len == 0 ? 1 : (size_t)out_len);
  if (out_buf == NULL) goto done;
  if (PKCS5_PBKDF2_HMAC((const char *)(Sbytevector_data(password) + (size_t)pw_start),
                        pw_len, Sbytevector_data(salt) + (size_t)salt_start,
                        salt_len, iterations, md, out_len, out_buf) != 1) {
    goto done;
  }
  ans = make_bytevector_copy(out_buf, out_len);

done:
  if (out_buf != NULL) {
    OPENSSL_cleanse(out_buf, (size_t)out_len);
    free(out_buf);
  }
  EVP_MD_free(md);
  return ans;
}

ptr crypto_scrypt(ptr password, uint64_t pw_start, uint64_t pw_stop, ptr salt,
                  uint64_t salt_start, uint64_t salt_stop, int n, int r, int p,
                  int out_len) {
  unsigned char *out_buf;
  ptr ans = Sfalse;

  if (!cspp_openssl_require()) return Sfalse;
  if (n < 2 || (n & (n - 1)) != 0 || r <= 0 || p <= 0 || out_len < 0)
    return Sfalse;
  out_buf = malloc(out_len == 0 ? 1 : (size_t)out_len);
  if (out_buf == NULL) return Sfalse;
  if (EVP_PBE_scrypt((const char *)(Sbytevector_data(password) + (size_t)pw_start),
                     slice_len(pw_start, pw_stop),
                     Sbytevector_data(salt) + (size_t)salt_start,
                     slice_len(salt_start, salt_stop), (uint64_t)n, (uint64_t)r,
                     (uint64_t)p, 0, out_buf, (size_t)out_len) != 1) {
    goto done;
  }
  ans = make_bytevector_copy(out_buf, out_len);

done:
  OPENSSL_cleanse(out_buf, (size_t)out_len);
  free(out_buf);
  return ans;
}

static ptr make_aead_pair(const unsigned char *ct, size_t ct_len,
                          const unsigned char *tag, size_t tag_len) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, make_bytevector_copy(ct, ct_len));
  Svector_set(v, 1, make_bytevector_copy(tag, tag_len));
  return v;
}

ptr crypto_aead_encrypt(ptr which, ptr key, uint64_t key_start, uint64_t key_stop,
                        ptr nonce, uint64_t nonce_start, uint64_t nonce_stop,
                        ptr aad, uint64_t aad_start, uint64_t aad_stop,
                        ptr plaintext, uint64_t pt_start, uint64_t pt_stop,
                        int tag_len) {
  EVP_CIPHER *cipher = fetch_cipher(which);
  EVP_CIPHER_CTX *ctx = NULL;
  unsigned char *out = NULL;
  unsigned char *tag = NULL;
  ptr ans = Sfalse;
  size_t out_len = 0;
  int out2 = 0;
  int nonce_len;
  size_t pt_len = slice_len(pt_start, pt_stop);

  (void)key_stop;
  if (cipher == NULL) return Sfalse;
  if (!uint64_to_int(nonce_stop - nonce_start, &nonce_len)) goto done;
  ctx = EVP_CIPHER_CTX_new();
  if (ctx == NULL) goto done;
  if (EVP_EncryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1) goto done;
  if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, nonce_len, NULL) != 1) goto done;
  if (EVP_EncryptInit_ex(ctx, NULL, NULL, Sbytevector_data(key) + (size_t)key_start,
                         Sbytevector_data(nonce) + (size_t)nonce_start) != 1) {
    goto done;
  }
  if (aad_stop > aad_start &&
      !evp_cipher_update_all(ctx, NULL, &out_len, Sbytevector_data(aad) + (size_t)aad_start,
                             aad_stop - aad_start)) {
    goto done;
  }
  out_len = 0;
  out = malloc(pt_len == 0 ? 1 : pt_len);
  tag = malloc(tag_len == 0 ? 1 : (size_t)tag_len);
  if (out == NULL || tag == NULL) goto done;
  if (pt_len != 0 &&
      !evp_cipher_update_all(ctx, out, &out_len,
                             Sbytevector_data(plaintext) + (size_t)pt_start,
                             pt_stop - pt_start)) {
    goto done;
  }
  if (EVP_EncryptFinal_ex(ctx, out + out_len, &out2) != 1) goto done;
  if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_GET_TAG, tag_len, tag) != 1) goto done;
  ans = make_aead_pair(out, out_len + (size_t)out2, tag, (size_t)tag_len);

done:
  if (out != NULL) {
    OPENSSL_cleanse(out, pt_len);
    free(out);
  }
  if (tag != NULL) {
    OPENSSL_cleanse(tag, (size_t)tag_len);
    free(tag);
  }
  if (ctx != NULL) EVP_CIPHER_CTX_free(ctx);
  if (cipher != NULL) EVP_CIPHER_free(cipher);
  return ans;
}

ptr crypto_aead_decrypt(ptr which, ptr key, uint64_t key_start, uint64_t key_stop,
                        ptr nonce, uint64_t nonce_start, uint64_t nonce_stop,
                        ptr aad, uint64_t aad_start, uint64_t aad_stop,
                        ptr ciphertext, uint64_t ct_start, uint64_t ct_stop,
                        ptr tag, uint64_t tag_start, uint64_t tag_stop) {
  EVP_CIPHER *cipher = fetch_cipher(which);
  EVP_CIPHER_CTX *ctx = NULL;
  unsigned char *out = NULL;
  ptr ans = Sfalse;
  size_t out_len = 0;
  int out2 = 0;
  int nonce_len, tag_len;
  size_t ct_len = slice_len(ct_start, ct_stop);

  (void)key_stop;
  if (cipher == NULL) return Sfalse;
  if (!uint64_to_int(nonce_stop - nonce_start, &nonce_len)) goto done;
  if (!uint64_to_int(tag_stop - tag_start, &tag_len)) goto done;
  /* CSPP AEAD algorithms always use full 128-bit authentication tags. */
  if (tag_len != 16) goto done;
  ctx = EVP_CIPHER_CTX_new();
  if (ctx == NULL) goto done;
  if (EVP_DecryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1) goto done;
  if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, nonce_len, NULL) != 1) goto done;
  if (EVP_DecryptInit_ex(ctx, NULL, NULL, Sbytevector_data(key) + (size_t)key_start,
                         Sbytevector_data(nonce) + (size_t)nonce_start) != 1) {
    goto done;
  }
  if (aad_stop > aad_start &&
      !evp_cipher_update_all(ctx, NULL, &out_len, Sbytevector_data(aad) + (size_t)aad_start,
                             aad_stop - aad_start)) {
    goto done;
  }
  out_len = 0;
  out = malloc(ct_len == 0 ? 1 : ct_len);
  if (out == NULL) goto done;
  if (ct_len != 0 &&
      !evp_cipher_update_all(ctx, out, &out_len,
                             Sbytevector_data(ciphertext) + (size_t)ct_start,
                             ct_stop - ct_start)) {
    goto done;
  }
  if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_TAG, tag_len,
                          Sbytevector_data(tag) + (size_t)tag_start) != 1) {
    goto done;
  }
  if (EVP_DecryptFinal_ex(ctx, out + out_len, &out2) != 1) goto done;
  ans = make_bytevector_copy(out, out_len + (size_t)out2);

done:
  if (out != NULL) {
    OPENSSL_cleanse(out, ct_len);
    free(out);
  }
  if (ctx != NULL) EVP_CIPHER_CTX_free(ctx);
  if (cipher != NULL) EVP_CIPHER_free(cipher);
  return ans;
}

static int cipher_state_init(cspp_cipher_state *st) {
  if (st == NULL || st->ctx == NULL || st->cipher == NULL) return 0;
  if (EVP_CipherInit_ex(st->ctx, st->cipher, NULL, NULL, NULL, st->encrypt) != 1)
    return 0;
  if (EVP_CIPHER_CTX_set_padding(st->ctx, 0) != 1) return 0;
  return EVP_CipherInit_ex(st->ctx, NULL, NULL, st->key, st->iv, st->encrypt);
}

int crypto_cipher_key_size(ptr which) {
  EVP_CIPHER *cipher = fetch_cipher(which);
  int ans = -1;
  if (cipher != NULL) {
    ans = EVP_CIPHER_get_key_length(cipher);
    EVP_CIPHER_free(cipher);
  }
  return ans;
}

int crypto_cipher_iv_size(ptr which) {
  EVP_CIPHER *cipher = fetch_cipher(which);
  int ans = -1;
  if (cipher != NULL) {
    ans = EVP_CIPHER_get_iv_length(cipher);
    EVP_CIPHER_free(cipher);
  }
  return ans;
}

int crypto_cipher_block_size(ptr which) {
  EVP_CIPHER *cipher = fetch_cipher(which);
  int ans = -1;
  if (cipher != NULL) {
    ans = EVP_CIPHER_get_block_size(cipher);
    EVP_CIPHER_free(cipher);
  }
  return ans;
}

void *crypto_cipher_state_create(ptr which, int encrypt, ptr key, uint64_t key_start,
                                 uint64_t key_stop, ptr iv, uint64_t iv_start,
                                 uint64_t iv_stop) {
  cspp_cipher_state *st = malloc(sizeof(cspp_cipher_state));

  if (st == NULL) return NULL;
  st->cipher = NULL;
  st->ctx = NULL;
  st->encrypt = encrypt;
  st->key = NULL;
  st->key_len = slice_len(key_start, key_stop);
  st->iv = NULL;
  st->iv_len = slice_len(iv_start, iv_stop);

  st->cipher = fetch_cipher(which);
  if (st->cipher == NULL) goto fail;

  st->ctx = EVP_CIPHER_CTX_new();
  if (st->ctx == NULL) goto fail;

  st->key = malloc(st->key_len == 0 ? 1 : st->key_len);
  st->iv = malloc(st->iv_len == 0 ? 1 : st->iv_len);
  if (st->key == NULL || st->iv == NULL) goto fail;
  if (st->key_len != 0) {
    memcpy(st->key, Sbytevector_data(key) + (size_t)key_start, st->key_len);
  }
  if (st->iv_len != 0) {
    memcpy(st->iv, Sbytevector_data(iv) + (size_t)iv_start, st->iv_len);
  }

  if (cipher_state_init(st) != 1) goto fail;
  return st;

fail:
  if (st->key != NULL) {
    OPENSSL_cleanse(st->key, st->key_len);
    free(st->key);
  }
  if (st->iv != NULL) {
    OPENSSL_cleanse(st->iv, st->iv_len);
    free(st->iv);
  }
  if (st->ctx != NULL) EVP_CIPHER_CTX_free(st->ctx);
  if (st->cipher != NULL) EVP_CIPHER_free(st->cipher);
  free(st);
  return NULL;
}

void crypto_cipher_state_destroy(void *ptr_st) {
  cspp_cipher_state *st = (cspp_cipher_state *)ptr_st;
  if (st == NULL) return;
  if (st->key != NULL) {
    OPENSSL_cleanse(st->key, st->key_len);
    free(st->key);
  }
  if (st->iv != NULL) {
    OPENSSL_cleanse(st->iv, st->iv_len);
    free(st->iv);
  }
  if (st->ctx != NULL) EVP_CIPHER_CTX_free(st->ctx);
  if (st->cipher != NULL) EVP_CIPHER_free(st->cipher);
  free(st);
}

ptr crypto_cipher_state_update(void *ptr_st, ptr bv, uint64_t start, uint64_t stop) {
  cspp_cipher_state *st = (cspp_cipher_state *)ptr_st;
  unsigned char *out = NULL;
  ptr ans = Sfalse;
  size_t out_len = 0;
  size_t in_len = slice_len(start, stop);
  size_t out_cap;

  if (st == NULL || st->ctx == NULL) return Sfalse;
  out_cap = in_len + (size_t)EVP_CIPHER_CTX_get_block_size(st->ctx);
  out = malloc(out_cap == 0 ? 1 : out_cap);
  if (out == NULL) return Sfalse;
  if (!evp_cipher_update_all(st->ctx, out, &out_len,
                             Sbytevector_data(bv) + (size_t)start,
                             stop - start)) {
    goto done;
  }
  ans = make_bytevector_copy(out, out_len);

done:
  if (out != NULL) {
    OPENSSL_cleanse(out, out_cap);
    free(out);
  }
  return ans;
}

ptr crypto_cipher_state_finalize(void *ptr_st) {
  cspp_cipher_state *st = (cspp_cipher_state *)ptr_st;
  unsigned char *out = NULL;
  ptr ans = Sfalse;
  int out_len = 0;
  size_t out_cap;

  if (st == NULL || st->ctx == NULL) return Sfalse;
  out_cap = (size_t)EVP_CIPHER_CTX_get_block_size(st->ctx);
  out = malloc(out_cap == 0 ? 1 : out_cap);
  if (out == NULL) return Sfalse;
  if (EVP_CipherFinal_ex(st->ctx, out, &out_len) != 1) goto done;
  ans = make_bytevector_copy(out, (size_t)out_len);

done:
  if (out != NULL) {
    OPENSSL_cleanse(out, out_cap);
    free(out);
  }
  return ans;
}

int crypto_cipher_state_reset(void *ptr_st) {
  cspp_cipher_state *st = (cspp_cipher_state *)ptr_st;
  if (st == NULL || st->ctx == NULL) return 0;
  if (EVP_CIPHER_CTX_reset(st->ctx) != 1) return 0;
  return cipher_state_init(st);
}

static const char *curve_name(ptr curve) {
  if (curve == Sstring_to_symbol("p-256")) return "prime256v1";
  if (curve == Sstring_to_symbol("prime256v1")) return "prime256v1";
  if (curve == Sstring_to_symbol("secp384r1")) return "secp384r1";
  if (curve == Sstring_to_symbol("secp521r1")) return "secp521r1";
  return NULL;
}

static const char *pkey_alg_name(ptr alg) {
  if (alg == Sstring_to_symbol("rsa")) return "RSA";
  if (alg == Sstring_to_symbol("ecdsa")) return "EC";
  if (alg == Sstring_to_symbol("ecdh")) return "EC";
  if (alg == Sstring_to_symbol("ed25519")) return "ED25519";
  if (alg == Sstring_to_symbol("x25519")) return "X25519";
  return NULL;
}

static ptr pkey_algorithm_symbol(EVP_PKEY *pkey) {
  int id = EVP_PKEY_get_base_id(pkey);
  switch (id) {
  case EVP_PKEY_RSA:
    return Sstring_to_symbol("rsa");
  case EVP_PKEY_EC:
    return Sstring_to_symbol("ec");
  case EVP_PKEY_ED25519:
    return Sstring_to_symbol("ed25519");
  case EVP_PKEY_X25519:
    return Sstring_to_symbol("x25519");
  default:
    return Sfalse;
  }
}

static int pkey_matches_algorithm(EVP_PKEY *key, ptr algorithm) {
  int id;
  if (key == NULL) return 0;
  id = EVP_PKEY_get_base_id(key);
  if (algorithm == Sstring_to_symbol("rsa-pss")) return id == EVP_PKEY_RSA;
  if (algorithm == Sstring_to_symbol("ecdsa") ||
      algorithm == Sstring_to_symbol("ecdh")) return id == EVP_PKEY_EC;
  if (algorithm == Sstring_to_symbol("ed25519")) return id == EVP_PKEY_ED25519;
  if (algorithm == Sstring_to_symbol("x25519")) return id == EVP_PKEY_X25519;
  return 0;
}

static ptr bio_to_bytevector(BIO *bio) {
  char *data = NULL;
  long len = BIO_ctrl(bio, BIO_CTRL_INFO, 0, &data);
  if (len < 0 || data == NULL) return Sfalse;
  return make_bytevector_copy((const unsigned char *)data, (size_t)len);
}

void *crypto_pkey_generate(ptr alg, int bits, ptr curve) {
  EVP_PKEY_CTX *ctx = NULL;
  EVP_PKEY *pkey = NULL;
  const char *name = pkey_alg_name(alg);
  OSSL_PARAM params[3];
  int idx = 0;
  char *curve_mut = NULL;
  int bits_copy = bits;

  if (name == NULL) return NULL;
  if (!cspp_openssl_require()) return NULL;
  ctx = EVP_PKEY_CTX_new_from_name(NULL, name, NULL);
  if (ctx == NULL) goto done;
  if (EVP_PKEY_keygen_init(ctx) != 1) goto done;

  if (alg == Sstring_to_symbol("rsa")) {
    params[idx++] = OSSL_PARAM_construct_int(OSSL_PKEY_PARAM_BITS, &bits_copy);
  } else if (alg == Sstring_to_symbol("ecdsa") || alg == Sstring_to_symbol("ecdh")) {
    curve_mut = (char *)curve_name(curve);
    if (curve_mut == NULL) goto done;
    params[idx++] =
        OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, curve_mut, 0);
  }
  if (idx != 0) {
    params[idx++] = OSSL_PARAM_construct_end();
    if (EVP_PKEY_CTX_set_params(ctx, params) != 1) goto done;
  }
  if (EVP_PKEY_generate(ctx, &pkey) != 1) goto done;

done:
  if (ctx != NULL) EVP_PKEY_CTX_free(ctx);
  return pkey;
}

void crypto_pkey_free(void *ptr_pkey) {
  if (ptr_pkey != NULL) EVP_PKEY_free((EVP_PKEY *)ptr_pkey);
}

ptr crypto_pkey_algorithm(void *ptr_pkey) {
  if (ptr_pkey == NULL) return Sfalse;
  return pkey_algorithm_symbol((EVP_PKEY *)ptr_pkey);
}

int crypto_pkey_bits(void *ptr_pkey) {
  if (ptr_pkey == NULL) return -1;
  return EVP_PKEY_get_bits((EVP_PKEY *)ptr_pkey);
}

void *crypto_pkey_public_from_private(void *ptr_pkey) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  BIO *bio = NULL;
  EVP_PKEY *pub = NULL;
  if (pkey == NULL) return NULL;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) goto done;
  if (PEM_write_bio_PUBKEY(bio, pkey) != 1) goto done;
  pub = PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL);
done:
  if (bio != NULL) BIO_free(bio);
  return pub;
}

ptr crypto_pkey_store_private_pem(void *ptr_pkey) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  BIO *bio = NULL;
  ptr ans = Sfalse;
  if (pkey == NULL) return Sfalse;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) goto done;
  if (PEM_write_bio_PrivateKey(bio, pkey, NULL, NULL, 0, NULL, NULL) != 1) goto done;
  ans = bio_to_bytevector(bio);
done:
  if (bio != NULL) BIO_free(bio);
  return ans;
}

ptr crypto_pkey_store_public_pem(void *ptr_pkey) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  BIO *bio = NULL;
  ptr ans = Sfalse;
  if (pkey == NULL) return Sfalse;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) goto done;
  if (PEM_write_bio_PUBKEY(bio, pkey) != 1) goto done;
  ans = bio_to_bytevector(bio);
done:
  if (bio != NULL) BIO_free(bio);
  return ans;
}

ptr crypto_pkey_store_private_der(void *ptr_pkey) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  BIO *bio = NULL;
  ptr ans = Sfalse;
  if (pkey == NULL) return Sfalse;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) goto done;
  if (i2d_PrivateKey_bio(bio, pkey) != 1) goto done;
  ans = bio_to_bytevector(bio);
done:
  if (bio != NULL) BIO_free(bio);
  return ans;
}

ptr crypto_pkey_store_public_der(void *ptr_pkey) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  BIO *bio = NULL;
  ptr ans = Sfalse;
  if (pkey == NULL) return Sfalse;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) goto done;
  if (i2d_PUBKEY_bio(bio, pkey) != 1) goto done;
  ans = bio_to_bytevector(bio);
done:
  if (bio != NULL) BIO_free(bio);
  return ans;
}

void *crypto_pkey_load_private_pem(ptr bv, uint64_t start, uint64_t stop) {
  int len;
  if (!cspp_openssl_require()) return NULL;
  if (!uint64_to_int(stop - start, &len)) return NULL;
  BIO *bio = BIO_new_mem_buf(Sbytevector_data(bv) + (size_t)start, len);
  EVP_PKEY *pkey = NULL;
  if (bio == NULL) return NULL;
  pkey = PEM_read_bio_PrivateKey(bio, NULL, NULL, NULL);
  BIO_free(bio);
  return pkey;
}

void *crypto_pkey_load_public_pem(ptr bv, uint64_t start, uint64_t stop) {
  int len;
  if (!cspp_openssl_require()) return NULL;
  if (!uint64_to_int(stop - start, &len)) return NULL;
  BIO *bio = BIO_new_mem_buf(Sbytevector_data(bv) + (size_t)start, len);
  EVP_PKEY *pkey = NULL;
  if (bio == NULL) return NULL;
  pkey = PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL);
  BIO_free(bio);
  return pkey;
}

void *crypto_pkey_load_private_der(ptr bv, uint64_t start, uint64_t stop) {
  int len;
  if (!cspp_openssl_require()) return NULL;
  if (!uint64_to_int(stop - start, &len)) return NULL;
  BIO *bio = BIO_new_mem_buf(Sbytevector_data(bv) + (size_t)start, len);
  EVP_PKEY *pkey = NULL;
  if (bio == NULL) return NULL;
  pkey = d2i_PrivateKey_bio(bio, NULL);
  BIO_free(bio);
  return pkey;
}

void *crypto_pkey_load_public_der(ptr bv, uint64_t start, uint64_t stop) {
  int len;
  if (!cspp_openssl_require()) return NULL;
  if (!uint64_to_int(stop - start, &len)) return NULL;
  BIO *bio = BIO_new_mem_buf(Sbytevector_data(bv) + (size_t)start, len);
  EVP_PKEY *pkey = NULL;
  if (bio == NULL) return NULL;
  pkey = d2i_PUBKEY_bio(bio, NULL);
  BIO_free(bio);
  return pkey;
}

ptr crypto_sign_message(ptr alg, ptr digest, void *ptr_pkey, ptr bv, uint64_t start,
                        uint64_t stop) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  EVP_MD_CTX *ctx = NULL;
  EVP_PKEY_CTX *pctx = NULL;
  EVP_MD *md = NULL;
  ptr ans = Sfalse;
  size_t sig_len = 0;
  unsigned char *sig = NULL;

  if (!pkey_matches_algorithm(pkey, alg)) return Sfalse;
  ctx = EVP_MD_CTX_new();
  if (ctx == NULL) goto done;

  if (alg == Sstring_to_symbol("ed25519")) {
    if (EVP_DigestSignInit(ctx, NULL, NULL, NULL, pkey) != 1) goto done;
    if (EVP_DigestSign(ctx, NULL, &sig_len, Sbytevector_data(bv) + (size_t)start,
                       slice_len(start, stop)) != 1) {
      goto done;
    }
    sig = malloc(sig_len == 0 ? 1 : sig_len);
    if (sig == NULL) goto done;
    if (EVP_DigestSign(ctx, sig, &sig_len, Sbytevector_data(bv) + (size_t)start,
                       slice_len(start, stop)) != 1) {
      goto done;
    }
  } else {
    md = fetch_digest(digest);
    if (md == NULL) goto done;
    if (EVP_DigestSignInit(ctx, &pctx, md, NULL, pkey) != 1) goto done;
    if (alg == Sstring_to_symbol("rsa-pss")) {
      if (EVP_PKEY_CTX_set_rsa_padding(pctx, RSA_PKCS1_PSS_PADDING) != 1) goto done;
      if (EVP_PKEY_CTX_set_rsa_pss_saltlen(pctx, -1) != 1) goto done;
    }
    if (EVP_DigestSignUpdate(ctx, Sbytevector_data(bv) + (size_t)start,
                             slice_len(start, stop)) != 1)
      goto done;
    if (EVP_DigestSignFinal(ctx, NULL, &sig_len) != 1) goto done;
    sig = malloc(sig_len == 0 ? 1 : sig_len);
    if (sig == NULL) goto done;
    if (EVP_DigestSignFinal(ctx, sig, &sig_len) != 1) goto done;
  }
  ans = make_bytevector_copy(sig, sig_len);

done:
  if (sig != NULL) {
    OPENSSL_cleanse(sig, sig_len);
    free(sig);
  }
  if (md != NULL) EVP_MD_free(md);
  if (ctx != NULL) EVP_MD_CTX_free(ctx);
  return ans;
}

int crypto_verify_message(ptr alg, ptr digest, void *ptr_pkey, ptr msg,
                          uint64_t msg_start, uint64_t msg_stop, ptr sig,
                          uint64_t sig_start, uint64_t sig_stop) {
  EVP_PKEY *pkey = (EVP_PKEY *)ptr_pkey;
  EVP_MD_CTX *ctx = NULL;
  EVP_PKEY_CTX *pctx = NULL;
  EVP_MD *md = NULL;
  int ok = 0;

  if (!pkey_matches_algorithm(pkey, alg)) return 0;
  ctx = EVP_MD_CTX_new();
  if (ctx == NULL) goto done;

  if (alg == Sstring_to_symbol("ed25519")) {
    if (EVP_DigestVerifyInit(ctx, NULL, NULL, NULL, pkey) != 1) goto done;
    ok = EVP_DigestVerify(ctx, Sbytevector_data(sig) + (size_t)sig_start,
                          slice_len(sig_start, sig_stop),
                          Sbytevector_data(msg) + (size_t)msg_start,
                          slice_len(msg_start, msg_stop)) == 1;
  } else {
    md = fetch_digest(digest);
    if (md == NULL) goto done;
    if (EVP_DigestVerifyInit(ctx, &pctx, md, NULL, pkey) != 1) goto done;
    if (alg == Sstring_to_symbol("rsa-pss")) {
      if (EVP_PKEY_CTX_set_rsa_padding(pctx, RSA_PKCS1_PSS_PADDING) != 1) goto done;
      if (EVP_PKEY_CTX_set_rsa_pss_saltlen(pctx, -1) != 1) goto done;
    }
    if (EVP_DigestVerifyUpdate(ctx, Sbytevector_data(msg) + (size_t)msg_start,
                               slice_len(msg_start, msg_stop)) != 1)
      goto done;
    ok = EVP_DigestVerifyFinal(ctx, Sbytevector_data(sig) + (size_t)sig_start,
                               slice_len(sig_start, sig_stop)) == 1;
  }

done:
  if (md != NULL) EVP_MD_free(md);
  if (ctx != NULL) EVP_MD_CTX_free(ctx);
  return ok;
}

ptr crypto_derive_shared_secret(ptr alg, void *ptr_priv, void *ptr_pub) {
  EVP_PKEY *priv = (EVP_PKEY *)ptr_priv;
  EVP_PKEY *pub = (EVP_PKEY *)ptr_pub;
  EVP_PKEY_CTX *ctx = NULL;
  unsigned char *out = NULL;
  size_t out_len = 0;
  ptr ans = Sfalse;

  if (priv == NULL || pub == NULL) return Sfalse;
  if (!(alg == Sstring_to_symbol("x25519") || alg == Sstring_to_symbol("ecdh")))
    return Sfalse;
  if (!pkey_matches_algorithm(priv, alg) || !pkey_matches_algorithm(pub, alg))
    return Sfalse;
  ctx = EVP_PKEY_CTX_new(priv, NULL);
  if (ctx == NULL) goto done;
  if (EVP_PKEY_derive_init(ctx) != 1) goto done;
  if (EVP_PKEY_derive_set_peer(ctx, pub) != 1) goto done;
  if (EVP_PKEY_derive(ctx, NULL, &out_len) != 1) goto done;
  out = malloc(out_len == 0 ? 1 : out_len);
  if (out == NULL) goto done;
  if (EVP_PKEY_derive(ctx, out, &out_len) != 1) goto done;
  ans = make_bytevector_copy(out, out_len);

done:
  if (out != NULL) {
    OPENSSL_cleanse(out, out_len);
    free(out);
  }
  if (ctx != NULL) EVP_PKEY_CTX_free(ctx);
  return ans;
}

static char *copy_hostname(ptr bv) {
  char *s;
  iptr len;
  if (bv == Sfalse) return NULL;
  len = Sbytevector_length(bv);
  /* Empty hostnames disable OpenSSL identity checking; NUL truncates the name. */
  if (len == 0 || memchr(Sbytevector_data(bv), '\0', (size_t)len) != NULL)
    return NULL;
  s = malloc((size_t)len + 1);
  if (s == NULL) return NULL;
  if (len != 0) {
    memcpy(s, Sbytevector_data(bv), (size_t)len);
  }
  s[len] = '\0';
  return s;
}

static X509 *load_x509_from_mem(const unsigned char *buf, uint64_t len, int pem) {
  BIO *bio = NULL;
  X509 *cert = NULL;
  int bio_len;
  if (!cspp_openssl_require() || !uint64_to_int(len, &bio_len)) return NULL;
  bio = BIO_new_mem_buf(buf, bio_len);
  if (bio == NULL) return NULL;
  cert = pem ? PEM_read_bio_X509(bio, NULL, NULL, NULL) : d2i_X509_bio(bio, NULL);
  BIO_free(bio);
  return cert;
}

static ptr make_general_name_entry(const GENERAL_NAME *name) {
  const ASN1_STRING *str;
  const unsigned char *data;
  int len;
  char ipbuf[INET6_ADDRSTRLEN];

  switch (name->type) {
  case GEN_DNS:
    str = name->d.dNSName;
    data = ASN1_STRING_get0_data(str);
    len = ASN1_STRING_length(str);
    return make_symbol_string_pair("dns", Sstring_utf8((const char *)data, (iptr)len));
  case GEN_URI:
    str = name->d.uniformResourceIdentifier;
    data = ASN1_STRING_get0_data(str);
    len = ASN1_STRING_length(str);
    return make_symbol_string_pair("uri", Sstring_utf8((const char *)data, (iptr)len));
  case GEN_EMAIL:
    str = name->d.rfc822Name;
    data = ASN1_STRING_get0_data(str);
    len = ASN1_STRING_length(str);
    return make_symbol_string_pair("email", Sstring_utf8((const char *)data, (iptr)len));
  case GEN_IPADD:
    str = name->d.iPAddress;
    data = ASN1_STRING_get0_data(str);
    len = ASN1_STRING_length(str);
    if (len == 4) {
      if (inet_ntop(AF_INET, data, ipbuf, sizeof(ipbuf)) == NULL) return Sfalse;
      return make_symbol_string_pair("ip", Sstring(ipbuf));
    }
    if (len == 16) {
      if (inet_ntop(AF_INET6, data, ipbuf, sizeof(ipbuf)) == NULL) return Sfalse;
      return make_symbol_string_pair("ip", Sstring(ipbuf));
    }
    return Sfalse;
  default:
    return Sfalse;
  }
}

void *crypto_cert_load_pem(ptr bv, uint64_t start, uint64_t stop) {
  return load_x509_from_mem(Sbytevector_data(bv) + (size_t)start, stop - start, 1);
}

void *crypto_cert_load_der(ptr bv, uint64_t start, uint64_t stop) {
  return load_x509_from_mem(Sbytevector_data(bv) + (size_t)start, stop - start, 0);
}

void crypto_cert_free(void *ptr_cert) {
  if (ptr_cert != NULL) X509_free((X509 *)ptr_cert);
}

ptr crypto_cert_subject(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  if (cert == NULL) return Sfalse;
  return make_x509_name_string(X509_get_subject_name(cert));
}

ptr crypto_cert_issuer(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  if (cert == NULL) return Sfalse;
  return make_x509_name_string(X509_get_issuer_name(cert));
}

ptr crypto_cert_not_before(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  if (cert == NULL) return Sfalse;
  return make_iso_time_string(X509_get0_notBefore(cert));
}

ptr crypto_cert_not_after(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  if (cert == NULL) return Sfalse;
  return make_iso_time_string(X509_get0_notAfter(cert));
}

ptr crypto_cert_subject_alt_names(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  GENERAL_NAMES *names;
  ptr head = Snil;
  ptr tail = Snil;

  if (cert == NULL) return Sfalse;
  names = X509_get_ext_d2i(cert, NID_subject_alt_name, NULL, NULL);
  if (names == NULL) return Snil;

  for (int i = 0;
       i < OPENSSL_sk_num((const OPENSSL_STACK *)names);
       i++) {
    ptr entry = make_general_name_entry(
        (GENERAL_NAME *)OPENSSL_sk_value(
            (const OPENSSL_STACK *)names, i));
    ptr cell;
    if (entry == Sfalse) continue;
    cell = Scons(entry, Snil);
    if (head == Snil) {
      head = cell;
      tail = cell;
    } else {
      Sset_cdr(tail, cell);
      tail = cell;
    }
  }
  GENERAL_NAMES_free(names);
  return head;
}

int crypto_cert_hostname_matches(void *ptr_cert, ptr hostname) {
  X509 *cert = (X509 *)ptr_cert;
  char *host = NULL;
  int ok = 0;
  struct in_addr addr4;
  struct in6_addr addr6;

  if (cert == NULL || hostname == Sfalse) return 0;
  host = copy_hostname(hostname);
  if (host == NULL) return 0;

  if (inet_pton(AF_INET, host, &addr4) == 1 || inet_pton(AF_INET6, host, &addr6) == 1) {
    ok = X509_check_ip_asc(cert, host, 0) == 1;
  } else {
    ok = X509_check_host(cert, host, 0, 0, NULL) == 1;
  }
  free(host);
  return ok;
}

ptr crypto_cert_public_key_der(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  EVP_PKEY *pkey = NULL;
  BIO *bio = NULL;
  ptr ans = Sfalse;

  if (cert == NULL) return Sfalse;
  pkey = X509_get_pubkey(cert);
  if (pkey == NULL) return Sfalse;
  bio = BIO_new(BIO_s_mem());
  if (bio == NULL) goto done;
  if (i2d_PUBKEY_bio(bio, pkey) != 1) goto done;
  ans = bio_to_bytevector(bio);

done:
  if (bio != NULL) BIO_free(bio);
  if (pkey != NULL) EVP_PKEY_free(pkey);
  return ans;
}

ptr crypto_cert_serial_number(void *ptr_cert) {
  X509 *cert = (X509 *)ptr_cert;
  const ASN1_INTEGER *serial;
  const unsigned char *data;
  int len;

  if (cert == NULL) return Sfalse;
  serial = X509_get0_serialNumber(cert);
  if (serial == NULL) return Sfalse;
  data = ASN1_STRING_get0_data((const ASN1_STRING *)serial);
  len = ASN1_STRING_length((const ASN1_STRING *)serial);
  return make_bytevector_copy(data, (size_t)len);
}

ptr crypto_cert_fingerprint(void *ptr_cert, ptr which) {
  X509 *cert = (X509 *)ptr_cert;
  EVP_MD *md = NULL;
  unsigned char out[EVP_MAX_MD_SIZE];
  unsigned int out_len = 0;
  ptr ans = Sfalse;

  if (cert == NULL) return Sfalse;
  md = fetch_digest(which);
  if (md == NULL) return Sfalse;
  if (X509_digest(cert, md, out, &out_len) != 1) goto done;
  ans = make_bytevector_copy(out, (size_t)out_len);

done:
  EVP_MD_free(md);
  return ans;
}

void *crypto_cert_store_create(int load_defaults) {
  X509_STORE *store;
  if (!cspp_openssl_require()) return NULL;
  store = X509_STORE_new();
  if (store == NULL) return NULL;
  if (load_defaults && X509_STORE_set_default_paths(store) != 1) {
    X509_STORE_free(store);
    return NULL;
  }
  return store;
}

void crypto_cert_store_destroy(void *ptr_store) {
  if (ptr_store != NULL) X509_STORE_free((X509_STORE *)ptr_store);
}

int crypto_cert_store_add(void *ptr_store, void *ptr_cert) {
  if (ptr_store == NULL || ptr_cert == NULL) return 0;
  return X509_STORE_add_cert((X509_STORE *)ptr_store, (X509 *)ptr_cert) == 1;
}

int crypto_cert_store_load_defaults(void *ptr_store) {
  if (ptr_store == NULL) return 0;
  return X509_STORE_set_default_paths((X509_STORE *)ptr_store) == 1;
}

void *crypto_cert_verify_state_create(void *ptr_cert, void *ptr_store, ptr hostname) {
  cspp_cert_verify_state *st = malloc(sizeof(cspp_cert_verify_state));
  if (st == NULL) return NULL;
  st->store = (X509_STORE *)ptr_store;
  st->leaf = (X509 *)ptr_cert;
  st->chain = (STACK_OF(X509) *)OPENSSL_sk_new_null();
  st->hostname = NULL;
  if (st->store == NULL || st->leaf == NULL || st->chain == NULL) goto fail;
  if (hostname != Sfalse) {
    st->hostname = copy_hostname(hostname);
    if (st->hostname == NULL) goto fail;
  }
  return st;

fail:
  if (st->chain != NULL)
    OPENSSL_sk_free((OPENSSL_STACK *)st->chain);
  free(st->hostname);
  free(st);
  return NULL;
}

int crypto_cert_verify_state_add_chain_cert(void *ptr_st, void *ptr_cert) {
  cspp_cert_verify_state *st = (cspp_cert_verify_state *)ptr_st;
  X509 *cert = (X509 *)ptr_cert;

  if (st == NULL || st->chain == NULL || cert == NULL) return 0;
  if (X509_up_ref(cert) != 1) return 0;
  if (OPENSSL_sk_push((OPENSSL_STACK *)st->chain, cert) == 0) {
    X509_free(cert);
    return 0;
  }
  return 1;
}

int crypto_cert_verify_state_verify(void *ptr_st) {
  cspp_cert_verify_state *st = (cspp_cert_verify_state *)ptr_st;
  X509_STORE_CTX *ctx = NULL;
  X509_VERIFY_PARAM *param;
  int ok = 0;
  struct in_addr addr4;
  struct in6_addr addr6;

  if (st == NULL || st->store == NULL || st->leaf == NULL) return 0;
  ctx = X509_STORE_CTX_new();
  if (ctx == NULL) goto done;
  if (X509_STORE_CTX_init(ctx, st->store, st->leaf,
                          OPENSSL_sk_num(
                              (const OPENSSL_STACK *)st->chain) == 0
                              ? NULL
                              : st->chain) != 1) {
    goto done;
  }
  param = X509_STORE_CTX_get0_param(ctx);
  if (st->hostname != NULL) {
    if (inet_pton(AF_INET, st->hostname, &addr4) == 1 ||
        inet_pton(AF_INET6, st->hostname, &addr6) == 1) {
      if (X509_VERIFY_PARAM_set1_ip_asc(param, st->hostname) != 1) goto done;
    } else {
      if (X509_VERIFY_PARAM_set1_host(param, st->hostname, 0) != 1) goto done;
    }
  }
  ok = X509_verify_cert(ctx) == 1;

done:
  if (ctx != NULL) X509_STORE_CTX_free(ctx);
  return ok;
}

void crypto_cert_verify_state_destroy(void *ptr_st) {
  cspp_cert_verify_state *st = (cspp_cert_verify_state *)ptr_st;
  if (st == NULL) return;
  if (st->chain != NULL)
    OPENSSL_sk_pop_free(
        (OPENSSL_STACK *)st->chain,
        (OPENSSL_sk_freefunc)X509_free);
  free(st->hostname);
  free(st);
}
