# Native dependency resolution is shared by all root build targets.
PKG_CONFIG ?= pkg-config
NATIVE_OPTIONS_FILE := $(BUILD_ROOT)/.cspp-native-options.mk
OPTIONAL_DEPENDENCY_NAMES := CARES CURL GRPC IDN2 LIBSSH WEBSOCKETS ZLIB OPENSSL \
                             UUID XXHASH BLAKE3
OPTIONAL_DEPENDENCY_LABEL_CARES := c-ares
OPTIONAL_DEPENDENCY_LABEL_CURL := libcurl
OPTIONAL_DEPENDENCY_LABEL_GRPC := gRPC
OPTIONAL_DEPENDENCY_LABEL_IDN2 := libidn2
OPTIONAL_DEPENDENCY_LABEL_LIBSSH := libssh
OPTIONAL_DEPENDENCY_LABEL_WEBSOCKETS := libwebsockets
OPTIONAL_DEPENDENCY_LABEL_ZLIB := zlib
OPTIONAL_DEPENDENCY_LABEL_OPENSSL := OpenSSL
OPTIONAL_DEPENDENCY_LABEL_UUID := libuuid
OPTIONAL_DEPENDENCY_LABEL_XXHASH := xxHash
OPTIONAL_DEPENDENCY_LABEL_BLAKE3 := BLAKE3
OPTIONAL_DEPENDENCY_VERSION_REQ_CARES := >= 1.18.0 (SONAME 2)
OPTIONAL_DEPENDENCY_VERSION_MIN_CARES := 1.18.0
OPTIONAL_DEPENDENCY_VERSION_REQ_CURL := >= 8.0.0
OPTIONAL_DEPENDENCY_VERSION_MIN_CURL := 8.0.0
OPTIONAL_DEPENDENCY_VERSION_REQ_GRPC := ABI major 54.x or 56.x
OPTIONAL_DEPENDENCY_VERSION_MAJOR_SET_GRPC := 54 56
OPTIONAL_DEPENDENCY_VERSION_REQ_IDN2 := >= 2.3.0
OPTIONAL_DEPENDENCY_VERSION_MIN_IDN2 := 2.3.0
OPTIONAL_DEPENDENCY_VERSION_REQ_LIBSSH := >= 0.10.0
OPTIONAL_DEPENDENCY_VERSION_MIN_LIBSSH := 0.10.0
OPTIONAL_DEPENDENCY_VERSION_REQ_WEBSOCKETS := >= 4.3.0
OPTIONAL_DEPENDENCY_VERSION_MIN_WEBSOCKETS := 4.3.0
OPTIONAL_DEPENDENCY_VERSION_REQ_ZLIB := >= 1.2.11, < 2.0.0 (ABI 1)
OPTIONAL_DEPENDENCY_VERSION_MIN_ZLIB := 1.2.11
OPTIONAL_DEPENDENCY_VERSION_MAX_ZLIB := 1.99.99
OPTIONAL_DEPENDENCY_VERSION_REQ_OPENSSL := >= 3.0.0, < 4.0.0
OPTIONAL_DEPENDENCY_VERSION_MIN_OPENSSL := 3.0.0
OPTIONAL_DEPENDENCY_VERSION_MAX_OPENSSL := 3.99.99
OPTIONAL_DEPENDENCY_VERSION_REQ_UUID := no version API
OPTIONAL_DEPENDENCY_VERSION_REQ_XXHASH := >= 0.8.0, < 0.9.0
OPTIONAL_DEPENDENCY_VERSION_MIN_XXHASH := 0.8.0
OPTIONAL_DEPENDENCY_VERSION_MAX_XXHASH := 0.8.99
OPTIONAL_DEPENDENCY_VERSION_REQ_BLAKE3 := >= 1.8.0, < 1.9.0
OPTIONAL_DEPENDENCY_VERSION_MIN_BLAKE3 := 1.8.0
OPTIONAL_DEPENDENCY_VERSION_MAX_BLAKE3 := 1.8.99

# Reject empty, multiple, and unknown values before running any probes.
define validate-optional-choice
$(if $(and $(filter 1,$(words $(WITH_$(1)))),$(filter auto 0 1,$(WITH_$(1)))),,\
  $(error WITH_$(1) must be auto, 0, or 1; got '$(WITH_$(1))'))
endef
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(eval WITH_$(name) ?= auto))
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(call validate-optional-choice,$(name)))
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
  $(eval $(name)_CFLAGS_SUPPLIED := $(if $(filter undefined,$(origin $(name)_CFLAGS)),0,1))\
  $(eval $(name)_LIBS_SUPPLIED := $(if $(filter undefined,$(origin $(name)_LIBS)),0,1)))

# Cleaning must still work when an explicitly required dependency is unavailable.
ifeq ($(strip $(filter-out clean,$(MAKECMDGOALS))),)
ifneq ($(strip $(MAKECMDGOALS)),)
_SKIP_OPTIONAL_PROBES := 1
endif
endif
ifeq ($(_SKIP_OPTIONAL_PROBES),1)
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(eval RESOLVED_WITH_$(name) := 0))
else
_OPTIONAL_CONFIG_FILE := $(shell env \
  CC=$(call build-shell-quote,$(CC)) CPPFLAGS=$(call build-shell-quote,$(CPPFLAGS)) \
  CFLAGS=$(call build-shell-quote,$(CFLAGS)) LDFLAGS=$(call build-shell-quote,$(LDFLAGS)) \
  PKG_CONFIG=$(call build-shell-quote,$(PKG_CONFIG)) \
  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
    WITH_$(name)=$(call build-shell-quote,$(strip $(WITH_$(name)))) \
    OPTIONAL_VERSION_MIN_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_MIN_$(name))) \
    OPTIONAL_VERSION_MAX_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_MAX_$(name))) \
    OPTIONAL_VERSION_MAJOR_SET_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_MAJOR_SET_$(name))) \
    OPTIONAL_VERSION_REQ_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_REQ_$(name))) \
    $(name)_CFLAGS=$(call build-shell-quote,$($(name)_CFLAGS)) \
    $(name)_LIBS=$(call build-shell-quote,$($(name)_LIBS)) \
    $(name)_CFLAGS_SUPPLIED=$($(name)_CFLAGS_SUPPLIED) \
    $(name)_LIBS_SUPPLIED=$($(name)_LIBS_SUPPLIED)) \
  sh $(call build-shell-quote,$(BUILD_ROOT)/tools/probe-optional-libraries.sh))
ifeq ($(_OPTIONAL_CONFIG_FILE),)
$(error Optional dependency configuration failed; see diagnostic above)
endif
$(eval $(file <$(_OPTIONAL_CONFIG_FILE)))
_OPTIONAL_CONFIG_REMOVED := $(shell rm -f $(call build-shell-quote,$(_OPTIONAL_CONFIG_FILE)))
endif

OPTIONAL_CFLAGS := $(strip $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(RESOLVED_$(name)_CFLAGS)))
OPTIONAL_LIBS := $(strip $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(RESOLVED_$(name)_LIBS)))
BUILD_SIGNATURE_NAMES += CC CPPFLAGS CFLAGS LDFLAGS LDLIBS \
  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),WITH_$(name) RESOLVED_WITH_$(name) \
    $(name)_CFLAGS $(name)_LIBS $(name)_CFLAGS_SUPPLIED $(name)_LIBS_SUPPLIED \
    RESOLVED_$(name)_CFLAGS RESOLVED_$(name)_LIBS RESOLVED_PACKAGE_VERSION_$(name))

# Shared metadata helpers stay selected and must honor the generated feature macros.
_OPTIONAL_CARES_SOURCES := src/cspp/c/net/dns.c
_OPTIONAL_CURL_SOURCES := src/cspp/c/net/ftp.c
_OPTIONAL_GRPC_SOURCES := src/cspp/c/net/grpc.c
_OPTIONAL_IDN2_SOURCES := src/cspp/c/net/idna.c
_OPTIONAL_LIBSSH_SOURCES := src/cspp/c/net/ssh.c
_OPTIONAL_WEBSOCKETS_SOURCES := src/cspp/c/net/lws_http.c src/cspp/c/net/websocket.c
_OPTIONAL_ZLIB_SOURCES := src/cspp/c/zlib_loader.c
_OPTIONAL_OPENSSL_SOURCES := src/cspp/c/crypto.c src/cspp/c/net/tls.c
_OPTIONAL_UUID_SOURCES := src/cspp/c/uuid.c
_OPTIONAL_XXHASH_SOURCES := src/cspp/c/hash.c
# The BLAKE3 digest backend guards enabled and disabled bodies independently.
_OPTIONAL_BLAKE3_SOURCES :=
_OPTIONAL_ZLIB_FALLBACKS := src/cspp/c/zlib_unavailable.c
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
  $(if $(filter 0,$(RESOLVED_WITH_$(name))),\
    $(eval SRCS_C := $(filter-out $(_OPTIONAL_$(name)_SOURCES),$(SRCS_C)) \
      $(or $(_OPTIONAL_$(name)_FALLBACKS),$(patsubst %.c,%_unavailable.c,$(_OPTIONAL_$(name)_SOURCES))))))

.PHONY: force-build-config
src/cspp/c/build-config.h: force-build-config | prepare-build
	@sh tools/probe-optional-libraries.sh header $@ \
	  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(name)=$(RESOLVED_WITH_$(name)))

# Tests replay the requested settings, retaining empty versus absent manual overrides.
$(NATIVE_OPTIONS_FILE): cspp.lib Makefile tools/probe-optional-libraries.sh | prepare-build
	@env CC=$(call build-shell-quote,$(CC)) \
	  CPPFLAGS=$(call build-shell-quote,$(CPPFLAGS)) CFLAGS=$(call build-shell-quote,$(CFLAGS)) \
	  LDFLAGS=$(call build-shell-quote,$(LDFLAGS)) LDLIBS=$(call build-shell-quote,$(LDLIBS)) \
	  PKG_CONFIG=$(call build-shell-quote,$(PKG_CONFIG)) \
	  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
	    WITH_$(name)=$(call build-shell-quote,$(strip $(WITH_$(name)))) \
	    OPTIONAL_VERSION_MIN_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_MIN_$(name))) \
	    OPTIONAL_VERSION_MAX_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_MAX_$(name))) \
	    OPTIONAL_VERSION_MAJOR_SET_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_MAJOR_SET_$(name))) \
	    OPTIONAL_VERSION_REQ_$(name)=$(call build-shell-quote,$(OPTIONAL_DEPENDENCY_VERSION_REQ_$(name))) \
	    $(name)_CFLAGS=$(call build-shell-quote,$($(name)_CFLAGS)) \
	    $(name)_LIBS=$(call build-shell-quote,$($(name)_LIBS)) \
	    $(name)_CFLAGS_SUPPLIED=$($(name)_CFLAGS_SUPPLIED) \
	    $(name)_LIBS_SUPPLIED=$($(name)_LIBS_SUPPLIED) \
	    RESOLVED_WITH_$(name)=$(RESOLVED_WITH_$(name)) \
	    RESOLVED_PACKAGE_VERSION_$(name)=$(call build-shell-quote,$(RESOLVED_PACKAGE_VERSION_$(name))) \
	    RESOLVED_$(name)_CFLAGS=$(call build-shell-quote,$(RESOLVED_$(name)_CFLAGS)) \
	    RESOLVED_$(name)_LIBS=$(call build-shell-quote,$(RESOLVED_$(name)_LIBS))) \
	  sh tools/probe-optional-libraries.sh replay $@
