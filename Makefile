.DEFAULT_GOAL := all

CSPP_BUILD_SCRIPT_SIGNATURE := $(shell cksum tools/build-library.ss tools/compiler-options.ss tools/print-library-version.ss)

.PHONY: print-library-version
print-library-version: cspp.lib
	@$(CHEZ_SCHEME) --script tools/print-library-version.ss cspp.lib

ifneq ($(strip $(NATIVE_OPTIONS_REPLAY)),)
include $(NATIVE_OPTIONS_REPLAY)
endif

include build-options.mk

override CHEZ_SOURCE_DIR := vendor/ChezScheme
override CHEZ_BUILD_DIR := .chezscheme-build
override CHEZ_INSTALL_DIR := .chezscheme-install
override CHEZ_SCHEME := $(abspath $(CHEZ_INSTALL_DIR)/bin/scheme)
override CHEZ_INCLUDE_DIR = $(dir $(realpath $(CHEZ_SCHEME)))
override SCHEME_SCRIPT := $(CHEZ_SCHEME)
override CHEZ_TOOLCHAIN_SIGNATURE := $(CHEZ_BUILD_DIR)/.chezscheme-signature
PREFIX := /usr
PACKAGE_DIR ?= $(BUILD_ROOT)/dist
PACKAGE_COMMIT ?= $(shell git rev-parse HEAD)

SRCS_CSPP := $(shell find src/cspp/   -type f -name '*.ss')
SRCS_C      := $(shell find src/cspp/c/ -type f -name '*.c' ! -name 'lws_http2_fixture.c' ! -name 'crypto_memory_fixture.c' ! -name '*_unavailable.c')

include tests/test-files.mk

ifneq ($(filter test,$(MAKECMDGOALS)),)
TEST_FILE_GOALS := $(filter-out test,$(MAKECMDGOALS))
INVALID_TEST_FILES := $(filter-out $(SRCS_TEST),$(TEST_FILE_GOALS))
ifneq ($(strip $(INVALID_TEST_FILES)),)
$(error unsupported test file(s): $(INVALID_TEST_FILES))
endif
ifneq ($(strip $(TEST_FILE_GOALS)),)
.PHONY: $(TEST_FILE_GOALS)
$(TEST_FILE_GOALS): ;
endif
endif

ifeq ($(origin CC),default)
CC := gcc
endif
CC ?= gcc
CFLAGS ?= -fPIC -Wall -Wextra -O2 -pthread
LDLIBS ?=

include optional-libraries.mk

cspplibs = cspp.lib
csppwpos = src/cspp.wpo
csppdeps = ${cspplibs}

.PHONY: all cspp release debug coverage prepare-build print-build-options test test-non-net bundled-chez clean-all

all: cspp

release:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),release) all
debug:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),debug) all
coverage:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),coverage) all

print-build-options:
	$(print-build-options)

prepare-build: bundled-chez
	$(if $(filter 1,$(QUIET_BUILD_OPTIONS)),,$(call print-build-options))
	@if [ -f "$(BUILD_OPTIONS_SIGNATURE_FILE)" ]; then \
		old=$$(cat "$(BUILD_OPTIONS_SIGNATURE_FILE)"); \
		new=$(call build-shell-quote,$(BUILD_OPTIONS_SIGNATURE)); \
		if [ "$$old" != "$$new" ]; then \
			printf '%s\n' 'build options changed; running make clean'; \
			$(MAKE) --no-print-directory clean; \
		fi; \
	else \
		if [ -e cspp.lib ] || [ -e libcspp.so ] || find src/cspp tests -name '*.so' -print -quit | grep -q .; then \
			printf '%s\n' 'build options signature missing; running make clean'; \
			$(MAKE) --no-print-directory clean; \
		fi; \
	fi

bundled-chez:
	@set -eu; \
	source='$(abspath $(CHEZ_SOURCE_DIR))'; \
	build='$(abspath $(CHEZ_BUILD_DIR))'; \
	install='$(abspath $(CHEZ_INSTALL_DIR))'; \
	commit=unversioned; \
	if [ -e .git ]; then \
		submodules=$$(git submodule status --recursive -- "$(CHEZ_SOURCE_DIR)"); \
		if printf '%s\n' "$$submodules" | grep -Eq '^[-+U]'; then \
			git submodule update --init --depth 1 --filter=blob:none --recursive -- "$(CHEZ_SOURCE_DIR)"; \
		fi; \
		commit=$$(git -C "$$source" rev-parse HEAD); \
	fi; \
	signature="commit=$$commit configure=$$source/configure --installprefix=$$install"; \
	old_signature=$$(cat "$$build/.chezscheme-signature" 2>/dev/null || :); \
	if [ "$$old_signature" != "$$signature" ] || [ ! -f "$$build/Makefile" ] || \
		[ ! -x "$$install/bin/scheme" ]; then \
		$(MAKE) --no-print-directory clean; \
		rm -rf "$$build" "$$install"; \
		mkdir -p "$$build"; \
		( cd "$$build" && "$$source/configure" --installprefix="$$install" ); \
		$(MAKE) --no-print-directory -C "$$build"; \
		$(MAKE) --no-print-directory -C "$$build" install; \
		printf '%s\n' "$$signature" > "$$build/.chezscheme-signature"; \
	fi

$(CHEZ_TOOLCHAIN_SIGNATURE): | bundled-chez
	@test -f "$@"

test: cspp
	@$(MAKE) --no-print-directory -C tests test \
		BUILD_VARIANT='$(VARIANT)' \
		$(foreach option,$(BUILD_OPTION_NAMES),BUILD_$(option)='$($(option))') \
		QUIET_BUILD_OPTIONS=1 \
		$(TEST_FILE_GOALS)

test-non-net: cspp
	@test -n "$(SRCS_NON_NET_TEST)" || { echo 'non-net test selection is empty' >&2; exit 1; }
	@$(MAKE) --no-print-directory -C tests test \
		BUILD_VARIANT='$(VARIANT)' \
		$(foreach option,$(BUILD_OPTION_NAMES),BUILD_$(option)='$($(option))') \
		QUIET_BUILD_OPTIONS=1 \
		$(SRCS_NON_NET_TEST)

define generate_cspp_launcher
	@rm -f $(1)
	@sed \
	      -e 's|@SCHEME_SCRIPT@|$(if $(5),$(5),$(SCHEME_SCRIPT))|g' \
	      -e 's|@LIBCSPP@|$(2)|g' \
	      -e 's|@CSPP_LIB@|$(3)|g' \
	      -e 's|@FIBER_LIB@|$(4)|g' \
	      src/cspp-launcher.ss > $(1)
	@chmod +x $(1)
endef

.PHONY: run
run: cspp
	@./cspp

.PHONY: protobuf-generate
protobuf-generate: cspp
	@mkdir -p tests/generated
	@chmod +x tools/protoc-gen-cspp
	@protoc --plugin=protoc-gen-cspp=tools/protoc-gen-cspp \
	        --cspp_out=tests/generated --proto_path=tests/data \
	        tests/data/file-transfer.proto tests/data/codegen-features.proto

libcspp.so: ${SRCS_C} src/cspp/c/build-config.h $(CHEZ_TOOLCHAIN_SIGNATURE) | prepare-build
	$(CC) $(CPPFLAGS) -I$(CHEZ_INCLUDE_DIR) $(CFLAGS) $(OPTIONAL_CFLAGS) \
	  -include src/cspp/c/build-config.h \
	  -shared $(LDFLAGS) -o $@ $(SRCS_C) $(LDLIBS) $(OPTIONAL_LIBS)

cspp.lib: Makefile build-options.mk optional-libraries.mk \
	tools/probe-optional-libraries.sh tools/build-library.ss \
	tools/compiler-options.ss tools/print-library-version.ss src/cspp.ss ${SRCS_CSPP} \
	libcspp.so $(CHEZ_TOOLCHAIN_SIGNATURE) | prepare-build
	@CHEZSCHEMELIBDIRS='$(abspath src)' $(CHEZ_SCHEME) --script tools/build-library.ss \
		$(BUILD_SCRIPT_ARGUMENTS) --
	@$(CHEZ_SCHEME) --script tools/print-library-version.ss cspp.lib
	@rm -f src/cspp.so
	@if [ "$(wpo)" != t ]; then rm -f src/cspp.wpo; fi
	@tmp="$(BUILD_OPTIONS_SIGNATURE_FILE).tmp"; \
	printf '%s\n' $(call build-shell-quote,$(BUILD_OPTIONS_SIGNATURE)) > "$$tmp"; \
	mv "$$tmp" "$(BUILD_OPTIONS_SIGNATURE_FILE)"
cspp: bundled-chez prepare-build ${csppdeps} $(NATIVE_OPTIONS_FILE) src/cspp-launcher.ss Makefile
	$(call generate_cspp_launcher,cspp,$(abspath libcspp.so),$(abspath cspp.lib),)
	@rm -f scheme
	@ln -s "$(CHEZ_SCHEME)" scheme

.PHONY: cspp.exe
cspp.exe: cspp

installdeps: ${csppdeps}
	@case "$(PREFIX)" in /*) ;; *) printf '%s\n' 'error: PREFIX must be an absolute path' >&2; exit 2 ;; esac
	install -d $(PREFIX)/bin $(PREFIX)/lib
	install libcspp.so  $(PREFIX)/lib
	install ${cspplibs} $(PREFIX)/lib
	@if [ -f $(csppwpos) ]; then install $(csppwpos) $(PREFIX)/lib; fi
.PHONY: check-install-prefix install-bundled-chez

check-install-prefix:
	@case "$(PREFIX)" in /*) ;; *) printf '%s\n' 'error: PREFIX must be an absolute path' >&2; exit 2 ;; esac

install-bundled-chez: check-install-prefix bundled-chez
	@set -eu; \
	case "$(PREFIX)" in /*) ;; *) printf '%s\n' 'error: PREFIX must be an absolute path' >&2; exit 2 ;; esac; \
	install -d "$(PREFIX)/bin" "$(PREFIX)/lib"; \
	cp -a "$(CHEZ_INSTALL_DIR)/bin/." "$(PREFIX)/bin/"; \
	cp -a "$(CHEZ_INSTALL_DIR)/lib/." "$(PREFIX)/lib/"

.PHONY: install
install: check-install-prefix cspp installdeps install-bundled-chez
	rm -f $(PREFIX)/bin/cspp $(PREFIX)/lib/cspp.ss
	$(call generate_cspp_launcher,$(PREFIX)/bin/cspp,$(abspath $(PREFIX)/lib/libcspp.so),$(abspath $(PREFIX)/lib/cspp.lib),,$(abspath $(PREFIX)/bin/scheme))

.PHONY: package
package: cspp
	@set -eu; \
	case "$$(uname -s):$$(uname -m)" in \
		Linux:x86_64) architecture=x64 ;; \
		Linux:aarch64|Linux:arm64) architecture=arm64 ;; \
		Linux:riscv64) architecture=riscv64 ;; \
		Linux:loongarch64) architecture=loongarch64 ;; \
		*) echo 'error: package supports Linux x64, arm64, riscv64, and loongarch64' >&2; exit 1 ;; \
	esac; \
	for tool in readelf ldd patchelf tar sha256sum realpath readlink cp getconf cc; do \
		command -v "$$tool" >/dev/null || { echo "error: package requires $$tool" >&2; exit 1; }; \
	done; \
	workspace=$$(mktemp -d /tmp/cspp-package.XXXXXXXX); \
	trap 'rm -rf "$$workspace"' EXIT HUP INT TERM; \
	$(MAKE) --no-print-directory install PREFIX="$$workspace/installed" \
		NATIVE_OPTIONS_REPLAY=$(call build-shell-quote,$(abspath $(NATIVE_OPTIONS_FILE))); \
	version=$$("$(CHEZ_SCHEME)" --script tools/print-library-version.ss cspp.lib); \
	version=$${version#CSPP version: }; \
	./cspp --script=tools/package-release.ss "$$workspace/installed" \
		--output $(call build-shell-quote,$(PACKAGE_DIR)) \
		--arch "$$architecture" --version "$$version" \
		--commit $(call build-shell-quote,$(PACKAGE_COMMIT))

.PHONY: clean
clean:
	@rm -f cspp.lib cspp.wpo src/cspp.so src/cspp.wpo cspp bin/cspp libcspp.so \
		"$(BUILD_OPTIONS_SIGNATURE_FILE)" "$(BUILD_OPTIONS_SIGNATURE_FILE).tmp" \
		"$(NATIVE_OPTIONS_FILE)" "$(NATIVE_OPTIONS_FILE)".tmp.* \
		src/cspp/c/build-config.h src/cspp/c/build-config.h.tmp.*
	@find src/cspp/ -name '*.so'  -delete
	@find tests/  -name '*.so'  -delete
	@find src/cspp/ -name '*.wpo' -delete
	@find src/cspp/ tests/ \( -name '*.covin' -o -name '*.covout' \) -delete
	@rm -f *.covin *.covout

clean-all: clean
	@rm -rf "$(CHEZ_BUILD_DIR)" "$(CHEZ_INSTALL_DIR)" scheme

.PHONY: dump
dump:
	@echo ${PREFIX}
	@echo ${CHEZ_SCHEME}
	@echo ${SRCS_CSPP}
	@echo ${SRCS_C}
