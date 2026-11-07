#define _GNU_SOURCE

#include "common.h"

#include <sys/stat.h>
#include <sys/inotify.h>
#include <fcntl.h>
#include <dirent.h>



// Various checks (e.g., file existence) should be performed on Scheme side.

// Error handling is context-dependent:
// If the function returns a string, it returns a vector with the error string as the sole element.
// If the function returns boolean, it can return the error string when error occurs.
// If the function returns other Scheme objects, ditto.

ptr cspp_statx(const char *path, int follow_link);
ptr cspp_readlink(const char *path);
ptr cspp_realpath(const char *path);
ptr cspp_link(const char *src, const char *dest);
ptr cspp_symlink(const char *src, const char *dest);
ptr cspp_touch(const char *path, ptr atime, ptr mtime, int follow_link);

ptr cspp_fswatcher_init(int block);
ptr cspp_fswatcher_close(int fsw);
ptr cspp_fswatcher_add(int fsw, const char *path, uint32_t mask);
ptr cspp_fswatcher_remove(int fsw, int id);
ptr cspp_fswatcher_next(int fsw, int timeout_ms);
uptr cspp_fs_open_directory(const char *path);
ptr cspp_fs_read_directory(uptr directory);
ptr cspp_fs_close_directory(uptr directory);

/* Path bytes identify entries. Never let Sstring_utf8 replace malformed bytes
 * with U+FFFD and thereby select a different file. Leave decoding to Chez. */
static int filesystem_utf8_valid(const char *text, size_t length) {
  const unsigned char *bytes = (const unsigned char *)text;
  size_t i = 0;
  while (i < length) {
    unsigned char first = bytes[i++];
    unsigned char low = 0x80, high = 0xbf;
    size_t count;
    if (first < 0x80) continue;
    if (first >= 0xc2 && first <= 0xdf) {
      count = 1;
    } else if (first >= 0xe0 && first <= 0xef) {
      count = 2;
      if (first == 0xe0) low = 0xa0;
      if (first == 0xed) high = 0x9f;
    } else if (first >= 0xf0 && first <= 0xf4) {
      count = 3;
      if (first == 0xf0) low = 0x90;
      if (first == 0xf4) high = 0x8f;
    } else {
      return 0;
    }
    if (length - i < count || bytes[i] < low || bytes[i] > high) return 0;
    i++;
    while (--count > 0) {
      if (bytes[i] < 0x80 || bytes[i] > 0xbf) return 0;
      i++;
    }
  }
  return 1;
}

static ptr filesystem_utf8_error(void) {
  return Sstring("filesystem name or link target is not valid UTF-8");
}

uptr cspp_fs_open_directory(const char *path) {
  char *p = expand_pathname(path);
  DIR *directory = opendir(p);
  free(p);
  if (directory == NULL) return 0;
  return (uptr)directory;
}

ptr cspp_fs_read_directory(uptr value) {
  DIR *directory = (DIR *)TO_VOIDP(value);
  struct dirent *entry;
  if (directory == NULL) return Sstring("directory is closed");
  errno = 0;
  entry = readdir(directory);
  if (entry == NULL) {
    if (errno != 0) return errno_str();
    return Sfalse;
  }
  size_t length = strlen(entry->d_name);
  /* Distinguish a consumed invalid name from a readdir failure, allowing
   * best-effort removal to skip that entry without retrying a failed read. */
  if (!filesystem_utf8_valid(entry->d_name, length)) return Sfixnum(-1);
  return Scons(Sstring_utf8(entry->d_name, (iptr)length), Sfixnum(entry->d_type));
}

ptr cspp_fs_close_directory(uptr value) {
  DIR *directory = (DIR *)TO_VOIDP(value);
  if (directory == NULL) return Strue;
  if (closedir(directory) == -1) return errno_str();
  return Strue;
}


ptr cspp_statx(const char *path, int follow_link) {
  char *p = expand_pathname(path);

  struct statx res;
  int op = follow_link ? 0 : AT_SYMLINK_NOFOLLOW;
  if (statx(AT_FDCWD, p, op, STATX_BASIC_STATS | STATX_BTIME, &res) == -1) {
    free(p);
    return errno_str();
  }

  free(p);

  ptr v = Smake_vector(14 + 4, Sfalse);
  __u32 mask = res.stx_mask;
  if (mask & STATX_TYPE) {
    Svector_set(v, 0, Sfixnum(res.stx_mode & S_IFMT));
  }
  if (mask & STATX_MODE) {
    Svector_set(v, 1, Sfixnum(res.stx_mode & ~S_IFMT));
  }
  if (mask & STATX_NLINK) {
    Svector_set(v, 2, Sunsigned32(res.stx_nlink));
  }
  if (mask & STATX_UID) {
    Svector_set(v, 3, Sunsigned32(res.stx_uid));
  }
  if (mask & STATX_GID) {
    Svector_set(v, 4, Sunsigned32(res.stx_gid));
  }
  if (mask & STATX_ATIME) {
    struct statx_timestamp t = res.stx_atime;
    Svector_set(v, 5, Sinteger(t.tv_sec));
    Svector_set(v, 6, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_CTIME) {
    struct statx_timestamp t = res.stx_ctime;
    Svector_set(v, 7, Sinteger(t.tv_sec));
    Svector_set(v, 8, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_MTIME) {
    struct statx_timestamp t = res.stx_mtime;
    Svector_set(v, 9, Sinteger(t.tv_sec));
    Svector_set(v, 10, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_BTIME) {
    struct statx_timestamp t = res.stx_btime;
    Svector_set(v, 11, Sinteger(t.tv_sec));
    Svector_set(v, 12, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_INO) {
    Svector_set(v, 13, Sunsigned64(res.stx_ino));
  }
  if (mask & STATX_SIZE) {
    Svector_set(v, 14, Sunsigned64(res.stx_size));
  }
  if (mask & STATX_BLOCKS) {
    Svector_set(v, 15, Sunsigned64(res.stx_blocks));
  }
  Svector_set(v, 16, Sfixnum(res.stx_dev_major));
  Svector_set(v, 17, Sfixnum(res.stx_dev_minor));

  return v;
}

ptr cspp_realpath(const char *path) {
  char *expanded = expand_pathname(path);
  char *resolved = realpath(expanded, NULL);
  int saved_errno = errno;
  free(expanded);
  if (resolved == NULL) {
    errno = saved_errno;
    return errno_str_vector();
  }
  size_t length = strlen(resolved);
  ptr result = filesystem_utf8_valid(resolved, length)
                 ? Sstring_utf8(resolved, (iptr)length)
                 : Smake_vector(1, filesystem_utf8_error());
  free(resolved);
  return result;
}

ptr cspp_readlink(const char *path) {
  char *p = expand_pathname(path);
  size_t capacity = 128;
  char *link;
  for (;;) {
    link = malloc(capacity);
    if (link == NULL) {
      free(p);
      errno = ENOMEM;
      return errno_str_vector();
    }

    ssize_t length = readlink(p, link, capacity);
    if (length == -1) {
      int saved_errno = errno;
      free(link);
      free(p);
      errno = saved_errno;
      return errno_str_vector();
    }
    if ((size_t)length < capacity) {
      ptr result = filesystem_utf8_valid(link, (size_t)length)
                     ? Sstring_utf8(link, length)
                     : Smake_vector(1, filesystem_utf8_error());
      free(link);
      free(p);
      return result;
    }

    free(link);
    if (capacity > SIZE_MAX / 2) {
      free(p);
      errno = ENAMETOOLONG;
      return errno_str_vector();
    }
    capacity *= 2;
  }
}

ptr cspp_link(const char *src, const char *dest) {
  /* Expanding src can permit GC, so snapshot the other FFI string first. */
  char *owned_dest = strdup(dest);
  if (owned_dest == NULL) return errno_str();
  char *s = expand_pathname(src);
  char *d = s == NULL ? NULL : expand_pathname(owned_dest);
  free(owned_dest);
  if (s == NULL || d == NULL) {
    free(s);
    free(d);
    return errno_str();
  }
  if (link(s, d) == -1) {
    free(s);
    free(d);
    return errno_str();
  }

  free(s);
  free(d);

  return Strue;
}

ptr cspp_symlink(const char *src, const char *dest) {
  /* Preserve literal link text across any collection during pathname expansion. */
  char *s = strdup(src);
  if (s == NULL) return errno_str();
  char *d = expand_pathname(dest);
  if (d == NULL) {
    free(s);
    return errno_str();
  }
  if (symlink(s, d) == -1) {
    free(s);
    free(d);
    return errno_str();
  }

  free(s);
  free(d);

  return Strue;
}

ptr cspp_touch(const char *path, ptr atime, ptr mtime, int follow_link) {
  /* Decode all Scheme objects before a tilde lookup can deactivate this thread. */
  struct timespec ts[2];

  // times are #f if left unchanged
  if (atime == Sfalse) {
    ts[0].tv_nsec = UTIME_OMIT;
  } else {
    ts[0].tv_nsec = Sinteger_value(Svector_ref(atime, 0));
    ts[0].tv_sec = Sinteger_value(Svector_ref(atime, 1));
  }

  if (mtime == Sfalse) {
    ts[1].tv_nsec = UTIME_OMIT;
  } else {
    ts[1].tv_nsec = Sinteger_value(Svector_ref(mtime, 0));
    ts[1].tv_sec = Sinteger_value(Svector_ref(mtime, 1));
  }

  char *p = expand_pathname(path);
  if (p == NULL) return errno_str();
  if (utimensat(AT_FDCWD, p, ts, follow_link ? 0 : AT_SYMLINK_NOFOLLOW)) {
    int saved_errno = errno;
    free(p);
    errno = saved_errno;
    return errno_str();
  }

  free(p);

  return Strue;
}

ptr cspp_fswatcher_init(int block) {
  /* Waiting is handled by poll, including for watchers with blocking defaults. */
  (void)block;
  int res = inotify_init1(IN_CLOEXEC | IN_NONBLOCK);
  if (res == -1) {
    return errno_str();
  }

  return Sfixnum(res);
}

ptr cspp_fswatcher_close(int fsw) {
  if (close(fsw) == -1) {
    return errno_str();
  }

  return Strue;
}

ptr cspp_fswatcher_add(int fsw, const char *path, uint32_t mask) {
  char *p = expand_pathname(path);
  int fid = inotify_add_watch(fsw, p, mask);
  if (fid == -1) {
    free(p);
    return errno_str();
  }

  free(p);

  return Sfixnum(fid);
}

ptr cspp_fswatcher_remove(int fsw, int fid) {
  /* Deletion and oneshot watches may already have queued IN_IGNORED. */
  if (inotify_rm_watch(fsw, fid) == -1 && errno != EINVAL) {
    return errno_str();
  }

  return Strue;
}

/* Use a monotonic deadline so EINTR does not restart the whole timeout. */
static int fswatcher_remaining_ms(const struct timespec *deadline) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) == -1) return -1;
  int64_t ns = ((int64_t)deadline->tv_sec - now.tv_sec) * INT64_C(1000000000)
    + deadline->tv_nsec - now.tv_nsec;
  if (ns <= 0) return 0;
  return (int)((ns + 999999) / 1000000);
}

ptr cspp_fswatcher_next(int fsw, int timeout_ms) {
  char buf[65536] __attribute__ ((aligned(__alignof__(struct inotify_event))));
  struct pollfd target = {fsw, POLLIN, 0};
  struct timespec deadline;
  ssize_t len = -1;
  int saved_errno = 0;
  int remaining = timeout_ms;

  if (timeout_ms > 0) {
    if (clock_gettime(CLOCK_MONOTONIC, &deadline) == -1) return errno_str();
    deadline.tv_sec += timeout_ms / 1000;
    deadline.tv_nsec += (timeout_ms % 1000) * 1000000L;
    if (deadline.tv_nsec >= 1000000000L) {
      deadline.tv_sec++;
      deadline.tv_nsec -= 1000000000L;
    }
  }

  /* Only native storage crosses the wait. An active foreign thread prevents
   * collection and can deadlock the Scheme producer that must wake this read. */
  Sdeactivate_thread();
  for (;;) {
    int ready = poll(&target, 1, remaining);
    if (ready == 0) { len = 0; break; }
    if (ready < 0) {
      if (errno != EINTR) { saved_errno = errno; break; }
    } else {
      len = read(fsw, buf, sizeof(buf));
      if (len >= 0) break;
      if (errno != EINTR && errno != EAGAIN) { saved_errno = errno; break; }
    }
    if (timeout_ms >= 0) {
      remaining = timeout_ms == 0 ? 0 : fswatcher_remaining_ms(&deadline);
      if (remaining < 0) { saved_errno = errno; break; }
      if (remaining == 0) { len = 0; break; }
    }
  }
  Sactivate_thread();
  if (saved_errno != 0) {
    errno = saved_errno;
    return errno_str();
  }
  if (len <= 0) return Sfalse;

  /* A read can contain many variable-length records. Return the entire batch
   * to Scheme, where unconsumed events remain attached to their watcher. */
  size_t count = 0;
  for (size_t offset = 0; offset < (size_t)len;) {
    const struct inotify_event *event = (const struct inotify_event *)(buf + offset);
    if (event->len > 0 && !filesystem_utf8_valid(event->name, strlen(event->name))) {
      return filesystem_utf8_error();
    }
    offset += sizeof(*event) + event->len;
    count++;
  }
  ptr batch = Smake_vector((iptr)count, Sfalse);
  size_t index = 0;
  for (size_t offset = 0; offset < (size_t)len;) {
    const struct inotify_event *event = (const struct inotify_event *)(buf + offset);
    ptr value = Smake_vector(3, Sfalse);
    Svector_set(value, 0, Sfixnum(event->wd));
    Svector_set(value, 1, Sunsigned32(event->mask));
    if (event->len > 0) {
      Svector_set(value, 2, Sstring_utf8(event->name, (iptr)strlen(event->name)));
    }
    Svector_set(batch, (iptr)index++, value);
    offset += sizeof(*event) + event->len;
  }
  return batch;
}
