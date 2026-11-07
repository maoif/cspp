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
uptr cspp_process_operation_create(void);
void cspp_process_prepare_spawn(uptr handle, ptr argv, ptr env, const char *cwd,
                                int stdin_mode, int stdout_mode, int stderr_mode);
void cspp_process_prepare_capture(uptr handle, ptr argv, ptr env, const char *cwd,
                                  ptr input, int stdout_mode, int stderr_mode,
                                  int stdout_null, int stderr_null, int stderr_to_stdout,
                                  int timeout_ms);
void cspp_process_prepare_pipeline(uptr handle, ptr specs, int mode);
int cspp_process_operation_step(uptr handle);
ptr cspp_process_operation_result(uptr handle);
void cspp_process_operation_dispose(uptr handle);
ptr cspp_waitpid(int pid, int nohang);
int cspp_process_wait_open(int pid);
int cspp_process_wait_ready(int fd, int timeout_ms);
ptr cspp_make_pipe();

char *expand_pathname(const char *inpath);

char **cspp_copy_environment(void);
char *cspp_environment_copy(const char *name);
char *cspp_user_home(const char *name);
