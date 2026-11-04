// #define _GNU_SOURCE

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>
#include <fcntl.h>
#include <poll.h>
#include <time.h>
#include <grp.h>
#include <pwd.h>


#include <scheme.h>

ptr errno_str();
ptr errno_str_vector();

ptr cspp_ok(ptr value);
ptr cspp_errno_result(const char *operation, ptr context);
ptr cspp_not_found_result(const char *operation, ptr context);
ptr cspp_unsupported_result(const char *operation);
ptr cspp_timeout_result(const char *operation, ptr context);

ptr cspp_filesystem_info(const char *path);
ptr cspp_send_signal(int pid, int sig);
ptr cspp_spawn_capture(ptr argv, ptr env, const char *cwd,
                         ptr stdin_payload, int capture_stdout, int capture_stderr,
                         int stdout_null, int stderr_null, int stderr_to_stdout,
                         int timeout_ms);
ptr cspp_spawn_process(ptr argv, ptr env, const char *cwd,
                         int stdin_mode, int stdout_mode, int stderr_mode);
ptr cspp_waitpid(int pid, int nohang);
ptr cspp_make_pipe();
ptr cspp_spawn_pipeline(ptr specs);
ptr cspp_spawn_pipeline_capture(ptr specs, int timeout_ms);

char *expand_pathname(const char *inpath);
