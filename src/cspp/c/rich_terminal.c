#include <errno.h>

#ifdef _WIN32

#include <io.h>

int cspp_rich_terminal_output_supported(int fd) { return _isatty(fd) ? 1 : 0; }

int cspp_rich_terminal_echo_supported(void) { return 0; }
int cspp_rich_terminal_disable_echo(void) { return ENOTSUP; }
int cspp_rich_terminal_restore_echo(void) { return ENOTSUP; }

#else

#include <termios.h>
#include <unistd.h>

int cspp_rich_terminal_output_supported(int fd) { return isatty(fd) ? 1 : 0; }

static struct termios saved_termios;
static int saved_termios_valid = 0;

int cspp_rich_terminal_echo_supported(void) {
  return isatty(STDIN_FILENO) ? 1 : 0;
}

int cspp_rich_terminal_disable_echo(void) {
  struct termios current;

  if (!isatty(STDIN_FILENO)) {
    return ENOTTY;
  }

  if (tcgetattr(STDIN_FILENO, &current) != 0) {
    return errno;
  }

  saved_termios = current;
  saved_termios_valid = 1;
  current.c_lflag &= ~(ECHO);

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &current) != 0) {
    return errno;
  }

  return 0;
}

int cspp_rich_terminal_restore_echo(void) {
  if (!saved_termios_valid) {
    return 0;
  }

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &saved_termios) != 0) {
    return errno;
  }

  saved_termios_valid = 0;
  return 0;
}

#endif
