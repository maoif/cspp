#define _POSIX_C_SOURCE 200809L
#include "common.h"

#if defined(__linux__)
#include <pthread.h>
#include <signal.h>

typedef struct {
  const char *name;
  int number;
} named_signal;

static const named_signal standard_signals[] = {
  {"hup", SIGHUP}, {"int", SIGINT}, {"quit", SIGQUIT}, {"ill", SIGILL},
  {"trap", SIGTRAP}, {"abrt", SIGABRT}, {"bus", SIGBUS}, {"fpe", SIGFPE},
  {"kill", SIGKILL}, {"usr1", SIGUSR1}, {"segv", SIGSEGV}, {"usr2", SIGUSR2},
  {"pipe", SIGPIPE}, {"alrm", SIGALRM}, {"term", SIGTERM}, {"stkflt", SIGSTKFLT},
  {"chld", SIGCHLD}, {"cont", SIGCONT}, {"stop", SIGSTOP}, {"tstp", SIGTSTP},
  {"ttin", SIGTTIN}, {"ttou", SIGTTOU}, {"urg", SIGURG}, {"xcpu", SIGXCPU},
  {"xfsz", SIGXFSZ}, {"vtalrm", SIGVTALRM}, {"prof", SIGPROF}, {"winch", SIGWINCH},
  {"io", SIGIO}, {"pwr", SIGPWR}, {"sys", SIGSYS}
};

static ptr signal_error(const char *operation, int code) {
  errno = code;
  return cspp_errno_result(operation, Snil);
}

/* Chez uses these synchronous signals to report native faults. Mask updates
 * preserve their original state and waits never consume them. */
static int runtime_signal(int number) {
  return number == SIGSEGV || number == SIGBUS || number == SIGILL || number == SIGFPE;
}

static int read_signal_set(ptr numbers, sigset_t *set, const sigset_t *current,
                           int allow_existing_runtime_signals) {
  if (!Svectorp(numbers)) return EINVAL;
  sigemptyset(set);
  for (iptr index = 0; index < Svector_length(numbers); index += 1) {
    ptr value = Svector_ref(numbers, index);
    if (!Sfixnump(value)) return EINVAL;
    iptr number = Sfixnum_value(value);
    if (number <= 0 || number > INT_MAX) return EINVAL;
    if (number == SIGKILL || number == SIGSTOP) return EINVAL;
    if (runtime_signal((int)number)
        && !(allow_existing_runtime_signals && sigismember(current, (int)number) == 1))
      return EINVAL;
    if (sigaddset(set, (int)number) != 0) return errno;
  }
  return 0;
}

static ptr signal_numbers(const sigset_t *set) {
  ptr result = Snil;
  for (int number = SIGRTMAX; number > 0; number -= 1) {
    if (sigismember(set, number) == 1) result = Scons(Sfixnum(number), result);
  }
  return result;
}
#endif

ptr cspp_signal_table(void) {
#if defined(__linux__)
  ptr result = Snil;
  int minimum = SIGRTMIN;
  int maximum = SIGRTMAX;
  for (int number = maximum; number >= minimum; number -= 1) {
    char name[32];
    if (number == minimum) snprintf(name, sizeof(name), "rtmin");
    else if (number == maximum) snprintf(name, sizeof(name), "rtmax");
    else snprintf(name, sizeof(name), "rtmin+%d", number - minimum);
    result = Scons(Scons(Sstring_to_symbol(name), Sfixnum(number)), result);
  }
  size_t count = sizeof(standard_signals) / sizeof(standard_signals[0]);
  while (count > 0) {
    count -= 1;
    result = Scons(Scons(Sstring_to_symbol(standard_signals[count].name),
                         Sfixnum(standard_signals[count].number)), result);
  }
  return cspp_ok(result);
#else
  return cspp_unsupported_result("signal-list");
#endif
}

ptr cspp_signal_mask(void) {
#if defined(__linux__)
  sigset_t current;
  int code = pthread_sigmask(SIG_BLOCK, NULL, &current);
  if (code != 0) return signal_error("signal-mask", code);
  return cspp_ok(signal_numbers(&current));
#else
  return cspp_unsupported_result("signal-mask");
#endif
}

/* action: 0 replaces the mask, 1 blocks, and 2 unblocks. */
ptr cspp_signal_mask_update(ptr numbers, int action) {
#if defined(__linux__)
  const char *operation = action == 0 ? "signal-mask-set!"
    : action == 1 ? "signal-block!" : "signal-unblock!";
  if (action < 0 || action > 2) return signal_error(operation, EINVAL);
  sigset_t current, requested;
  int code = pthread_sigmask(SIG_BLOCK, NULL, &current);
  if (code != 0) return signal_error(operation, code);
  code = read_signal_set(numbers, &requested, &current, action == 0);
  if (code != 0) return signal_error(operation, code);
  if (action == 0) {
    for (int number = 1; number <= SIGRTMAX; number += 1) {
      if (runtime_signal(number) && sigismember(&current, number) == 1)
        sigaddset(&requested, number);
    }
  }
  code = pthread_sigmask(action == 0 ? SIG_SETMASK : action == 1 ? SIG_BLOCK : SIG_UNBLOCK,
                         &requested, NULL);
  if (code != 0) return signal_error(operation, code);
  return cspp_ok(Svoid);
#else
  (void)numbers;
  (void)action;
  return cspp_unsupported_result("signal-mask-update");
#endif
}

typedef struct {
#if defined(__linux__)
  sigset_t requested;
#endif
  int error, received;
} signal_wait_operation;

uptr cspp_signal_wait_create(void) {
  return (uptr)calloc(1, sizeof(signal_wait_operation));
}

void cspp_signal_wait_prepare(uptr handle, ptr numbers) {
  signal_wait_operation *op = (signal_wait_operation *)handle;
#if defined(__linux__)
  sigset_t current;
  op->error = pthread_sigmask(SIG_BLOCK, NULL, &current);
  if (op->error) return;
  op->error = read_signal_set(numbers, &op->requested, &current, 0);
  if (op->error) return;
  if (Svector_length(numbers) == 0) { op->error = EINVAL; return; }
  for (iptr index = 0; index < Svector_length(numbers); index++) {
    if (sigismember(&current, (int)Sfixnum_value(Svector_ref(numbers, index))) != 1) {
      op->error = EINVAL;
      return;
    }
  }
#else
  (void)numbers;
#endif
}

/* Bound with __collect_safe: Chez supplies Sdeactivate_thread/Sactivate_thread.
 * The copied signal set is native-owned; no Scheme object is accessed here.
 * sigtimedwait returns on unrelated signals or after 20ms so Scheme can service
 * pending handlers, including signals delivered just before the native call. */
int cspp_signal_wait_step(uptr handle) {
  signal_wait_operation *op = (signal_wait_operation *)handle;
#if defined(__linux__)
  if (op->error) return 1;
  struct timespec interval = {0, 20000000};
  int received = sigtimedwait(&op->requested, NULL, &interval);
  if (received > 0) { op->received = received; return 1; }
  if (errno == EAGAIN || errno == EINTR) return 0;
  op->error = errno;
#else
  (void)op;
#endif
  return 1;
}

ptr cspp_signal_wait_result(uptr handle) {
  signal_wait_operation *op = (signal_wait_operation *)handle;
#if defined(__linux__)
  if (op->error) return signal_error("wait-signal", op->error);
  return cspp_ok(Sfixnum(op->received));
#else
  (void)op;
  return cspp_unsupported_result("wait-signal");
#endif
}

void cspp_signal_wait_dispose(uptr handle) {
  free((signal_wait_operation *)handle);
}
