#!chezscheme
#|doc
Usage requirements are documented in `docs/test-and-benchmark.md`.
Standalone assertions and failure-value APIs may run in any thread or fiber. Assertions raise
in their caller; worker failures must be propagated to the controlling test body after joining.
Read-only record access requires stable payloads. Coordination APIs require synchronous execution
in one controlling OS thread, outside fibers; concurrent use and continuation reentry are undefined.
|#
(library (cspp test)
  (export chez-version
          chez-version<? chez-version<=? chez-version=? chez-version>=? chez-version>?
          test-descriptor? test-suite-descriptor? test-case-descriptor?
          test-expand-descriptor? test-compile-descriptor? test-concrete-case?
          test-result? test-summary? test-config? test-registry?
          make-test-case make-test-suite make-test-expand make-test-compile
          test-descriptor-name test-descriptor-phase test-descriptor-metadata
          test-descriptor-children test-descriptor-body
          make-test-config default-test-config test-config-with
          test-config-reporter test-config-output test-config-xfail-strict?
          test-config-stop-on-failure? test-config-color
          make-test-registry current-test-registry test-register!
          test-clear-registry! test-registry-descriptors
          make-test-concrete-case test-concrete-case-id test-concrete-case-name
          test-concrete-case-descriptor test-concrete-case-parameters
          test-concrete-case-metadata
          make-test-result test-result-id test-result-name test-result-status
          test-result-message test-result-condition test-result-stdout
          test-result-stderr test-result-parameters test-result-source
          make-test-summary test-summary-total test-summary-passed
          test-summary-failed test-summary-errored test-summary-skipped
          test-summary-xfail test-summary-xpass test-summary-results
          test-expand-parameters test-list test-select test-run test-run-registry
          test-run-files test-main
          test-failure? test-failure-message test-failure-expected
          test-failure-actual make-test-failure
          test-assert test-assert-equal test-assert-raises
          test-assert-not-raises
          test-true test-false test-eq test-eqv test-equal test-=
          test-pred test-raises test-not-raises test-fail
          test-capture-ports
          test-reporter? make-test-reporter test-text-reporter
          test-datum-reporter test-progress-reporter test-report test-summarize
          test-suite test-case test-expand test-compile test)
  (import (cspp chez)
          (cspp test private common)
          (cspp test assertion)
          (cspp test capture)
          (cspp test descriptor)
          (cspp test macros)
          (cspp test reporter)
          (cspp test runner)))
