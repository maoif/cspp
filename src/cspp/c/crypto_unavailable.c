#include "net/unavailable.h"

int crypto_random_status(void) {
  return -1;
}

ptr crypto_random_bytevector(uint64_t len) {
  (void)len;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_random_fill(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return -1;
}

int crypto_constant_time_eq(ptr bv1, uint64_t start1, uint64_t stop1, ptr bv2,
                            uint64_t start2, uint64_t stop2) {
  (void)bv1;
  (void)start1;
  (void)stop1;
  (void)bv2;
  (void)start2;
  (void)stop2;
  return -1;
}

ptr crypto_digest_bytevector(ptr which, ptr bv, uint64_t start, uint64_t stop) {
  (void)which;
  (void)bv;
  (void)start;
  (void)stop;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_digest_string(ptr which, ptr str, uint64_t start, uint64_t stop) {
  (void)which;
  (void)str;
  (void)start;
  (void)stop;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_digest_output_size(ptr which) {
  (void)which;
  return -1;
}

int crypto_digest_block_size(ptr which) {
  (void)which;
  return -1;
}

void * crypto_digest_state_create(ptr which) {
  (void)which;
  return 0;
}

void crypto_digest_state_destroy(void *ptr_st) {
  (void)ptr_st;
}

ptr crypto_digest_state_get(void *ptr_st) {
  (void)ptr_st;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_digest_state_reset(void *ptr_st) {
  (void)ptr_st;
  return -1;
}

int crypto_digest_state_update_bytevector(void *ptr_st, ptr bv, uint64_t start,
                                        uint64_t stop) {
  (void)ptr_st;
  (void)bv;
  (void)start;
  (void)stop;
  return -1;
}

int crypto_digest_state_update_string(void *ptr_st, ptr str, uint64_t start,
                                    uint64_t stop) {
  (void)ptr_st;
  (void)str;
  (void)start;
  (void)stop;
  return -1;
}

void * crypto_hmac_state_create(ptr which, ptr key, uint64_t start, uint64_t stop) {
  (void)which;
  (void)key;
  (void)start;
  (void)stop;
  return 0;
}

void crypto_hmac_state_destroy(void *ptr_st) {
  (void)ptr_st;
}

ptr crypto_hmac_state_get(void *ptr_st) {
  (void)ptr_st;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_hmac_state_finalize(void *ptr_st) {
  (void)ptr_st;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_hmac_state_reset(void *ptr_st) {
  (void)ptr_st;
  return -1;
}

int crypto_hmac_state_update_bytevector(void *ptr_st, ptr bv, uint64_t start,
                                        uint64_t stop) {
  (void)ptr_st;
  (void)bv;
  (void)start;
  (void)stop;
  return -1;
}

int crypto_hmac_state_update_string(void *ptr_st, ptr str, uint64_t start,
                                    uint64_t stop) {
  (void)ptr_st;
  (void)str;
  (void)start;
  (void)stop;
  return -1;
}

ptr crypto_hkdf(ptr which, ptr ikm, uint64_t ikm_start, uint64_t ikm_stop, ptr salt,
                uint64_t salt_start, uint64_t salt_stop, ptr info,
                uint64_t info_start, uint64_t info_stop, int out_len) {
  (void)which;
  (void)ikm;
  (void)ikm_start;
  (void)ikm_stop;
  (void)salt;
  (void)salt_start;
  (void)salt_stop;
  (void)info;
  (void)info_start;
  (void)info_stop;
  (void)out_len;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_hkdf_extract(ptr which, ptr ikm, uint64_t ikm_start, uint64_t ikm_stop,
                        ptr salt, uint64_t salt_start, uint64_t salt_stop) {
  (void)which;
  (void)ikm;
  (void)ikm_start;
  (void)ikm_stop;
  (void)salt;
  (void)salt_start;
  (void)salt_stop;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_hkdf_expand(ptr which, ptr prk, uint64_t prk_start, uint64_t prk_stop,
                       ptr info, uint64_t info_start, uint64_t info_stop, int out_len) {
  (void)which;
  (void)prk;
  (void)prk_start;
  (void)prk_stop;
  (void)info;
  (void)info_start;
  (void)info_stop;
  (void)out_len;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_pbkdf2(ptr which, ptr password, uint64_t pw_start, uint64_t pw_stop,
                  ptr salt, uint64_t salt_start, uint64_t salt_stop, int iterations,
                  int out_len) {
  (void)which;
  (void)password;
  (void)pw_start;
  (void)pw_stop;
  (void)salt;
  (void)salt_start;
  (void)salt_stop;
  (void)iterations;
  (void)out_len;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_scrypt(ptr password, uint64_t pw_start, uint64_t pw_stop, ptr salt,
                  uint64_t salt_start, uint64_t salt_stop, int n, int r, int p,
                  int out_len) {
  (void)password;
  (void)pw_start;
  (void)pw_stop;
  (void)salt;
  (void)salt_start;
  (void)salt_stop;
  (void)n;
  (void)r;
  (void)p;
  (void)out_len;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_aead_encrypt(ptr which, ptr key, uint64_t key_start, uint64_t key_stop,
                        ptr nonce, uint64_t nonce_start, uint64_t nonce_stop,
                        ptr aad, uint64_t aad_start, uint64_t aad_stop,
                        ptr plaintext, uint64_t pt_start, uint64_t pt_stop,
                        int tag_len) {
  (void)which;
  (void)key;
  (void)key_start;
  (void)key_stop;
  (void)nonce;
  (void)nonce_start;
  (void)nonce_stop;
  (void)aad;
  (void)aad_start;
  (void)aad_stop;
  (void)plaintext;
  (void)pt_start;
  (void)pt_stop;
  (void)tag_len;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_aead_decrypt(ptr which, ptr key, uint64_t key_start, uint64_t key_stop,
                        ptr nonce, uint64_t nonce_start, uint64_t nonce_stop,
                        ptr aad, uint64_t aad_start, uint64_t aad_stop,
                        ptr ciphertext, uint64_t ct_start, uint64_t ct_stop,
                        ptr tag, uint64_t tag_start, uint64_t tag_stop) {
  (void)which;
  (void)key;
  (void)key_start;
  (void)key_stop;
  (void)nonce;
  (void)nonce_start;
  (void)nonce_stop;
  (void)aad;
  (void)aad_start;
  (void)aad_stop;
  (void)ciphertext;
  (void)ct_start;
  (void)ct_stop;
  (void)tag;
  (void)tag_start;
  (void)tag_stop;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_cipher_key_size(ptr which) {
  (void)which;
  return -1;
}

int crypto_cipher_iv_size(ptr which) {
  (void)which;
  return -1;
}

int crypto_cipher_block_size(ptr which) {
  (void)which;
  return -1;
}

void * crypto_cipher_state_create(ptr which, int encrypt, ptr key, uint64_t key_start,
                                 uint64_t key_stop, ptr iv, uint64_t iv_start,
                                 uint64_t iv_stop) {
  (void)which;
  (void)encrypt;
  (void)key;
  (void)key_start;
  (void)key_stop;
  (void)iv;
  (void)iv_start;
  (void)iv_stop;
  return 0;
}

void crypto_cipher_state_destroy(void *ptr_st) {
  (void)ptr_st;
}

ptr crypto_cipher_state_update(void *ptr_st, ptr bv, uint64_t start, uint64_t stop) {
  (void)ptr_st;
  (void)bv;
  (void)start;
  (void)stop;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cipher_state_finalize(void *ptr_st) {
  (void)ptr_st;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_cipher_state_reset(void *ptr_st) {
  (void)ptr_st;
  return -1;
}

void * crypto_pkey_generate(ptr alg, int bits, ptr curve) {
  (void)alg;
  (void)bits;
  (void)curve;
  return 0;
}

void crypto_pkey_free(void *ptr_pkey) {
  (void)ptr_pkey;
}

ptr crypto_pkey_algorithm(void *ptr_pkey) {
  (void)ptr_pkey;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_pkey_bits(void *ptr_pkey) {
  (void)ptr_pkey;
  return -1;
}

void * crypto_pkey_public_from_private(void *ptr_pkey) {
  (void)ptr_pkey;
  return 0;
}

ptr crypto_pkey_store_private_pem(void *ptr_pkey) {
  (void)ptr_pkey;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_pkey_store_public_pem(void *ptr_pkey) {
  (void)ptr_pkey;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_pkey_store_private_der(void *ptr_pkey) {
  (void)ptr_pkey;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_pkey_store_public_der(void *ptr_pkey) {
  (void)ptr_pkey;
  return cspp_unavailable_status("openssl: disabled at build time");
}

void * crypto_pkey_load_private_pem(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return 0;
}

void * crypto_pkey_load_public_pem(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return 0;
}

void * crypto_pkey_load_private_der(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return 0;
}

void * crypto_pkey_load_public_der(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return 0;
}

ptr crypto_sign_message(ptr alg, ptr digest, void *ptr_pkey, ptr bv, uint64_t start,
                        uint64_t stop) {
  (void)alg;
  (void)digest;
  (void)ptr_pkey;
  (void)bv;
  (void)start;
  (void)stop;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_verify_message(ptr alg, ptr digest, void *ptr_pkey, ptr msg,
                          uint64_t msg_start, uint64_t msg_stop, ptr sig,
                          uint64_t sig_start, uint64_t sig_stop) {
  (void)alg;
  (void)digest;
  (void)ptr_pkey;
  (void)msg;
  (void)msg_start;
  (void)msg_stop;
  (void)sig;
  (void)sig_start;
  (void)sig_stop;
  return -1;
}

ptr crypto_derive_shared_secret(ptr alg, void *ptr_priv, void *ptr_pub) {
  (void)alg;
  (void)ptr_priv;
  (void)ptr_pub;
  return cspp_unavailable_status("openssl: disabled at build time");
}

void * crypto_cert_load_pem(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return 0;
}

void * crypto_cert_load_der(ptr bv, uint64_t start, uint64_t stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return 0;
}

void crypto_cert_free(void *ptr_cert) {
  (void)ptr_cert;
}

ptr crypto_cert_subject(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cert_issuer(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cert_not_before(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cert_not_after(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cert_subject_alt_names(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

int crypto_cert_hostname_matches(void *ptr_cert, ptr hostname) {
  (void)ptr_cert;
  (void)hostname;
  return -1;
}

ptr crypto_cert_public_key_der(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cert_serial_number(void *ptr_cert) {
  (void)ptr_cert;
  return cspp_unavailable_status("openssl: disabled at build time");
}

ptr crypto_cert_fingerprint(void *ptr_cert, ptr which) {
  (void)ptr_cert;
  (void)which;
  return cspp_unavailable_status("openssl: disabled at build time");
}

void * crypto_cert_store_create(int load_defaults) {
  (void)load_defaults;
  return 0;
}

void crypto_cert_store_destroy(void *ptr_store) {
  (void)ptr_store;
}

int crypto_cert_store_add(void *ptr_store, void *ptr_cert) {
  (void)ptr_store;
  (void)ptr_cert;
  return -1;
}

int crypto_cert_store_load_defaults(void *ptr_store) {
  (void)ptr_store;
  return -1;
}

void * crypto_cert_verify_state_create(void *ptr_cert, void *ptr_store, ptr hostname) {
  (void)ptr_cert;
  (void)ptr_store;
  (void)hostname;
  return 0;
}

int crypto_cert_verify_state_add_chain_cert(void *ptr_st, void *ptr_cert) {
  (void)ptr_st;
  (void)ptr_cert;
  return -1;
}

int crypto_cert_verify_state_verify(void *ptr_st) {
  (void)ptr_st;
  return -1;
}

void crypto_cert_verify_state_destroy(void *ptr_st) {
  (void)ptr_st;
}
