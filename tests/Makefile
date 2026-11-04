.ONESHELL:
.DEFAULT_GOAL := test

ROOT_DIR := ..

ROOT_OPTIONS_FILE := $(ROOT_DIR)/.cspp-build-options
ifeq ($(wildcard $(ROOT_OPTIONS_FILE)),)
$(error root build options are missing; run make from the repository root first)
endif

ROOT_OPTIONS := $(shell cat "$(ROOT_OPTIONS_FILE)")
ROOT_NATIVE_OPTIONS_FILE := $(ROOT_DIR)/.cspp-native-options.mk
ifeq ($(wildcard $(ROOT_NATIVE_OPTIONS_FILE)),)
$(error root native build settings are missing; run make from the repository root first)
endif
include $(ROOT_NATIVE_OPTIONS_FILE)
define root_option
$(strip $(patsubst $(1)=%,%,$(filter $(1)=%,$(ROOT_OPTIONS))))
endef

BUILD_VARIANT := $(call root_option,VARIANT)
BUILD_o := $(call root_option,o)
BUILD_d := $(call root_option,d)
BUILD_c := $(call root_option,c)
BUILD_wpo := $(call root_option,wpo)

.PHONY: prepare-cspp

prepare-cspp:
	@$(MAKE) --no-print-directory -C $(ROOT_DIR) cspp \
		NATIVE_OPTIONS_REPLAY='$(abspath $(ROOT_NATIVE_OPTIONS_FILE))' \
		VARIANT='$(BUILD_VARIANT)' o='$(BUILD_o)' d='$(BUILD_d)' \
		i='t' psi='t' c='$(BUILD_c)' wpo='$(BUILD_wpo)' \
		QUIET_BUILD_OPTIONS=1

BUILD_OPTION_NAMES := o d cl i cp0 fc xf xl p xp bp xbp c loadspd dumpspd \
                      loadbpd dumpbpd compile pdhtml gac gic pps psi wpo
VARIANT := $(BUILD_VARIANT)
o := $(BUILD_o)
d := $(BUILD_d)
BUILD_TEST_OPTIMIZE_LEVEL := $(if $(filter 0,$(o)),0,2)
TEST_COMPILER_SCRIPTS := ../tools/compile-test.ss ../tools/compiler-options.ss
TEST_COMPILER_ARGUMENTS := 'o=$(BUILD_TEST_OPTIMIZE_LEVEL)' 'd=$(d)'
COVERAGE_ENABLED := $(if $(filter coverage,$(BUILD_VARIANT)),1,)

include test-files.mk

NET_TEST_SUPPORT := net-common.ss net-ftp-common.ss net-ftp-server-process.ss net-ssh-common.ss \
                    net-lws-fixture.ss

OBJS_TEST := $(SRCS_TEST:.ss=.so)
TEST_FILE_GOALS :=

ifneq ($(filter test,$(MAKECMDGOALS)),)
TEST_FILE_GOALS := $(filter-out test,$(MAKECMDGOALS))
INVALID_TEST_FILES := $(filter-out $(SRCS_TEST) $(STRESS_TESTS),$(TEST_FILE_GOALS))
ifneq ($(strip $(INVALID_TEST_FILES)),)
$(error unsupported test file(s): $(INVALID_TEST_FILES))
endif
ifneq ($(strip $(TEST_FILE_GOALS)),)
$(TEST_FILE_GOALS): ;
endif
endif
TEST_OBJS := $(if $(TEST_FILE_GOALS),$(TEST_FILE_GOALS:.ss=.so),$(OBJS_TEST))

HTTP_ERROR_SOAK_SECONDS ?= 300
HTTP_ERROR_SOAK_TESTS := net-http net-lws-http2 net-lws-server
HTTP_ERROR_SOAK_OBJS := $(addsuffix .so,$(HTTP_ERROR_SOAK_TESTS))

WEBSOCKETS_ENABLED := $(shell sed -n 's/^\#define CSPP_WITH_WEBSOCKETS //p' \
                                   $(ROOT_DIR)/src/cspp/c/build-config.h)
ifeq ($(WEBSOCKETS_ENABLED),1)
.SECONDARY: lws-http2-fixture
lws-http2-fixture: ../src/cspp/c/net/lws_http2_fixture.c
	$(CC) $(CPPFLAGS) $(RESOLVED_WEBSOCKETS_CFLAGS) -Wall -Wextra -O2 \
	  $< -o $@ $(LDFLAGS) $(RESOLVED_WEBSOCKETS_LIBS) -ldl

net-lws-http2.so net-http-fiber.so net-http-stress.so: lws-http2-fixture
endif

mat.so: mat.sls $(TEST_COMPILER_SCRIPTS) | prepare-cspp
	@stdout=$@.build.stdout; stderr=$@.build.stderr; \
	rm -f "$$stdout" "$$stderr"; \
	printf '%s\n' "== compiling mat.sls =="; \
	../cspp --script=../tools/compile-test.ss -- $(TEST_COMPILER_ARGUMENTS) -- mat.sls >"$$stdout" 2>"$$stderr"; \
	status=$$?; \
	if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi

$(OBJS_TEST) $(STRESS_TESTS:.ss=.so): %.so: %.ss mat.so $(NET_TEST_SUPPORT) ../cspp.lib \
	../cspp $(TEST_COMPILER_SCRIPTS) | prepare-cspp
	@stdout=$@.build.stdout; stderr=$@.build.stderr; \
	rm -f "$$stdout" "$$stderr"; \
	../cspp --script=../tools/compile-test.ss -- $(TEST_COMPILER_ARGUMENTS) -- \
	  "$(basename $@).ss" mat.so >"$$stdout" 2>"$$stderr"; \
	status=$$?; \
	if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi

digest.so: digest-process.py

ifeq ($(RESOLVED_WITH_OPENSSL),1)
crypto-memory-fixture.so: ../src/cspp/c/crypto_memory_fixture.c $(ROOT_NATIVE_OPTIONS_FILE)
	$(CC) $(CPPFLAGS) $(RESOLVED_OPENSSL_CFLAGS) -Wall -Wextra -O2 -fPIC \
	  -shared $< -o $@ $(LDFLAGS) $(RESOLVED_OPENSSL_LIBS)

crypto-hardening.so: crypto-memory-fixture.so
endif

crypto-hardening.so: crypto-envelope-memory.py crypto-envelope-memory.ss
crypto-hardening.so: crypto-thread-process.py crypto-thread-process.ss
crypto-hardening.so: crypto-continuation-process.py crypto-continuation-process.ss
crypto-hardening.so: crypto-input-process.py crypto-input-process.ss

system-runtime-suite.so: system-runtime.ss system-runtime-runner.ss system-lookup-fixture.so

system-lookup-fixture.so: ../src/cspp/c/system_lookup_fixture.c
	$(CC) $(CPPFLAGS) -Wall -Wextra -O2 -fPIC -shared $< -o $@ $(LDFLAGS) -ldl

system-process.so: system-process-cleanup-probe.ss system-process-handoff.ss system-native-cleanup.ss \
                   system-process-mutex.ss

system-user.so: system-user-thread.ss

file.so: fswatcher-blocking-process.ss file-copytree-process.ss

fiber.so: fiber-quantum-process.ss fiber-inherited-timer-process.ss

test.so: mat-requires-fixture.ss test-benchmark-close-error-process.ss

net-ftp.so: net-ftp-startup-process.ss

.PHONY: test test-all test-non-net

test-all: test

test-non-net:
	@test -n "$(SRCS_NON_NET_TEST)" || { echo 'non-net test selection is empty' >&2; exit 1; }
	@$(MAKE) --no-print-directory test $(SRCS_NON_NET_TEST)

.PHONY: test-release-package
test-release-package: prepare-cspp
	@cd .. && ./cspp --script=tests/release-package.ss

test: prepare-cspp mat.so $(TEST_OBJS) ../tools/run-test.ss
	@cleanup() { \
	  rm -f *.out *.stdout *.stderr *.build.stdout *.build.stderr *.covout coverage.covout; \
	  rm -f /tmp/cspp-net-test-cert.pem /tmp/cspp-net-test-key.pem \
	    /tmp/cspp-net-test-san-cert.pem /tmp/cspp-net-test-san-key.pem \
	    /tmp/cspp-net.sock /tmp/cspp-net-ftp-*.txt /tmp/cspp-net-ftp-*.bin \
	    /tmp/cspp-net-sftp-*.bin /tmp/cspp-net-scp-*.txt /tmp/cspp-net-ssh-*.txt; \
	  rm -rf /tmp/cspp-net-ftp-root /tmp/cspp-net-ftp-link-source \
	    /tmp/cspp-net-ftp-tree-source /tmp/cspp-net-ftp-tree-dest \
	    /tmp/cspp-example-sftp /tmp/explicit-cspp-sftp-state \
	    /tmp/upload-root; \
	}; \
	trap 'cleanup; rm -f lws-http2-fixture' EXIT HUP INT TERM; cleanup
	@status=0; \
	for item in $(TEST_OBJS); do \
	  stdout="$$item.stdout"; stderr="$$item.stderr"; \
	  rm -f "$$stdout" "$$stderr"; \
	  printf '%s\n' "== running $$item =="; \
	  if [ -n "$(COVERAGE_ENABLED)" ]; then \
	    covin_files=$$(find ../src/cspp -type f -name '*.covin' -print); \
	    ../cspp --script=coverage-init.ss "$$item" "$${item%.so}.covout" $$covin_files >"$$stdout" 2>"$$stderr"; \
	  else \
	    ../cspp --script=../tools/run-test.ss "$$item" >"$$stdout" 2>"$$stderr"; \
	  fi; \
	  code=$$?; \
	  if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	  if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	  if [ $$code -ne 0 ]; then status=1; fi; \
	  if grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then status=1; fi; \
	 done; \
	 if [ -n "$(COVERAGE_ENABLED)" ]; then \
	   covin_files=$$(find ../src/cspp -type f -name '*.covin' -print); \
	   covout_files=$$(find . -maxdepth 1 -type f -name '*.covout' -print); \
	   set -- $$covin_files; covin_count=$$#; \
	   ../cspp --script=coverage-report.ss "$$covin_count" coverage.covout $$covin_files $$covout_files; \
	   report_code=$$?; if [ $$report_code -ne 0 ]; then status=1; fi; \
	 fi; \
	 exit $$status

.PHONY: test-http-error-soak
test-http-error-soak: prepare-cspp mat.so $(HTTP_ERROR_SOAK_OBJS) ../tools/run-test.ss
	@case "$(HTTP_ERROR_SOAK_SECONDS)" in \
	  ''|*[!0-9]*) \
	    printf '%s\n' "HTTP_ERROR_SOAK_SECONDS must be a positive integer" >&2; exit 2 ;; \
	esac
	@if [ "$(HTTP_ERROR_SOAK_SECONDS)" -le 0 ]; then \
	  printf '%s\n' "HTTP_ERROR_SOAK_SECONDS must be a positive integer" >&2; exit 2; \
	fi
	@cleanup() { \
	  rm -f *.out *.stdout *.stderr *.build.stdout *.build.stderr; \
	  rm -f /tmp/cspp-net-test-cert.pem /tmp/cspp-net-test-key.pem \
	    /tmp/cspp-net-test-san-cert.pem /tmp/cspp-net-test-san-key.pem \
	    /tmp/cspp-net.sock /tmp/cspp-net-ftp-*.txt /tmp/cspp-net-ftp-*.bin \
	    /tmp/cspp-net-sftp-*.bin /tmp/cspp-net-scp-*.txt /tmp/cspp-net-ssh-*.txt; \
	  rm -rf /tmp/cspp-net-ftp-root /tmp/cspp-net-ftp-link-source \
	    /tmp/cspp-net-ftp-tree-source /tmp/cspp-net-ftp-tree-dest \
	    /tmp/cspp-example-sftp /tmp/explicit-cspp-sftp-state \
	    /tmp/upload-root; \
	}; \
	trap 'cleanup; rm -f lws-http2-fixture' EXIT HUP INT TERM; cleanup
	@start=$$(date +%s); deadline=$$((start + $(HTTP_ERROR_SOAK_SECONDS))); round=0; \
	while :; do \
	  round=$$((round + 1)); \
	  for base in $(HTTP_ERROR_SOAK_TESTS); do \
	    stdout="$$base.stdout"; stderr="$$base.stderr"; \
	    rm -f "$$stdout" "$$stderr"; \
	    printf '%s\n' "== soak round $$round: $$base.so =="; \
	    ../cspp --script=../tools/run-test.ss "$$base.so" >"$$stdout" 2>"$$stderr"; \
	    code=$$?; \
	    if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	    if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	    if [ $$code -ne 0 ] || grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then \
	      exit 1; \
	    fi; \
	  done; \
	  now=$$(date +%s); \
	  if [ $$now -ge $$deadline ]; then \
	    printf '%s\n' "completed $$round soak rounds in $$((now - start)) seconds"; \
	    break; \
	  fi; \
	done

.PHONY: test-cov
test-cov: $(SRCS_TESTS)
	echo todo

.PHONY: test-optional-hash-linkage
test-optional-hash-linkage:
	@./optional-hash-linkage.sh

.PHONY: test-optional-library-loader
test-optional-library-loader:
	@./optional-library-loader.sh

.PHONY: test-optional-library-linkage
test-optional-library-linkage:
	@./optional-library-linkage.sh

.PHONY: test-optional-library-build
test-optional-library-build:
	@./optional-library-build.sh

.PHONY: test-optional-native
test-optional-native:
	@python3 ../tools/test-optional-native.py --sentinels
	@../cspp --script=optional-library-runtime.ss

.PHONY: test-lws-runtime-lock
test-lws-runtime-lock: prepare-cspp
ifeq ($(WEBSOCKETS_ENABLED),1)
	@CC='$(CC)' CPPFLAGS='$(CPPFLAGS)' LDFLAGS='$(LDFLAGS)' \
	  LWS_CFLAGS='$(RESOLVED_WEBSOCKETS_CFLAGS) $(RESOLVED_OPENSSL_CFLAGS)' \
	  LWS_LIBS='$(RESOLVED_WEBSOCKETS_LIBS) $(RESOLVED_OPENSSL_LIBS)' \
	  CHEZ_INCLUDE_DIR='$(dir $(realpath ../.chezscheme-install/bin/scheme))' \
	  sh ./test-lws-runtime-lock.sh
endif

.PHONY: clean
clean:
	@rm -f *.so *.out *.stdout *.stderr *.build.stdout *.build.stderr lws-http2-fixture
	@rm -f /tmp/cspp-net-test-cert.pem /tmp/cspp-net-test-key.pem \
	  /tmp/cspp-net-test-san-cert.pem /tmp/cspp-net-test-san-key.pem \
	  /tmp/cspp-net.sock /tmp/cspp-net-ftp-*.txt /tmp/cspp-net-ftp-*.bin \
	  /tmp/cspp-net-sftp-*.bin /tmp/cspp-net-scp-*.txt /tmp/cspp-net-ssh-*.txt
	@rm -rf /tmp/cspp-net-ftp-root /tmp/cspp-net-ftp-link-source \
	  /tmp/cspp-net-ftp-tree-source /tmp/cspp-net-ftp-tree-dest \
	  /tmp/cspp-example-sftp /tmp/explicit-cspp-sftp-state /tmp/upload-root
