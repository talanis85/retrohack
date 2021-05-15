#include <stdio.h>
#include <stdarg.h>

#include "libretro.h"

void default_retro_log_printf(enum retro_log_level level, const char *fmt, ...) {
  static const char * levelstr[] = { "dbg", "inf", "wrn", "err" };
  va_list va;

  fprintf(stderr, "[%s] ", levelstr[level]);

  va_start(va, fmt);
  vfprintf(stderr, fmt, va);
  va_end(va);
}
