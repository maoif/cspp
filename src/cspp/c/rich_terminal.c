#include <errno.h>

#ifdef _WIN32

#include <io.h>
#include <windows.h>

int cspp_rich_terminal_width(int fd) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo((HANDLE)_get_osfhandle(fd), &info)) return 0;
  return info.srWindow.Right - info.srWindow.Left + 1;
}

int cspp_rich_terminal_output_supported(int fd) { return _isatty(fd) ? 1 : 0; }

int cspp_rich_terminal_echo_supported(void) { return 0; }
int cspp_rich_terminal_disable_echo(void) { return ENOTSUP; }
int cspp_rich_terminal_restore_echo(void) { return ENOTSUP; }

#else

#include <limits.h>
#include <pthread.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>

int cspp_rich_terminal_width(int fd) {
  struct winsize size;
  if (!isatty(fd) || ioctl(fd, TIOCGWINSZ, &size) != 0) return 0;
  return size.ws_col;
}

int cspp_rich_terminal_output_supported(int fd) { return isatty(fd) ? 1 : 0; }

static struct termios saved_termios;
/* Nested and overlapping reads share the first successful disable's state. */
static unsigned int echo_depth = 0;
static pthread_mutex_t echo_lock = PTHREAD_MUTEX_INITIALIZER;

int cspp_rich_terminal_echo_supported(void) {
  return isatty(STDIN_FILENO) ? 1 : 0;
}

int cspp_rich_terminal_disable_echo(void) {
  struct termios current, hidden;
  int status = 0;

  pthread_mutex_lock(&echo_lock);
  if (echo_depth == UINT_MAX) {
    status = EOVERFLOW;
  } else if (echo_depth > 0) {
    ++echo_depth;
  } else if (!isatty(STDIN_FILENO)) {
    status = ENOTTY;
  } else if (tcgetattr(STDIN_FILENO, &current) != 0) {
    status = errno;
  } else {
    hidden = current;
    hidden.c_lflag &= ~ECHO;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &hidden) != 0) {
      status = errno;
    } else {
      saved_termios = current;
      echo_depth = 1;
    }
  }
  pthread_mutex_unlock(&echo_lock);
  return status;
}

int cspp_rich_terminal_restore_echo(void) {
  int status = 0;

  pthread_mutex_lock(&echo_lock);
  if (echo_depth > 1) {
    --echo_depth;
  } else if (echo_depth == 1) {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &saved_termios) != 0) {
      status = errno;
    } else {
      echo_depth = 0;
    }
  }
  pthread_mutex_unlock(&echo_lock);
  return status;
}

#endif
