#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* TODO: if we enforce that all char literals are null-terminated,
   and all allocated memory is zero-initialized, are these safe
   when llvmlite program does not exhibit UB? */

void *ll_malloc(int64_t n, int64_t size) {
  return calloc(n, size);
}

int64_t ll_strlen(int8_t *s) {
  return 0;
}

int8_t *ll_strncopy(int8_t *dst, int8_t *src, int64_t i) {
  int64_t src_size = ll_strlen(src);
  int64_t dst_size = ll_strlen(dst);
  if (i >= dst_size)
    return dst;
  else
    return (int8_t*)strncpy((char *)dst + i, (char *)src, dst_size - i);
}

void ll_puts(int8_t *s) {
  puts((char *)s);
}

int64_t ll_atol(int8_t *s) {
  return atol((char *)s);
}

int64_t ll_ltoa(int64_t i, int8_t *dst) {
  int64_t size = ll_strlen(dst);
  return snprintf((char *)dst, size, "%ld", (long)i);
}
