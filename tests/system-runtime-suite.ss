(import (cspp))

(mat system-runtime-isolation
  ;; Error case: native stalls, crashes, inherited input, and child cleanup need isolation.
  (= 0 (system (if (file-exists? "system-runtime-runner.ss")
                   "../cspp --script=system-runtime-runner.ss boundaries capture gc inherit wait ports environment-race signals pipeline-contracts vfork-rejected"
                   "./cspp --script=tests/system-runtime-runner.ss boundaries capture gc inherit wait ports environment-race signals pipeline-contracts vfork-rejected"))))
