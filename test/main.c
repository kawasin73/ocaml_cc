#include <stdio.h>
#include <stdlib.h>

long foo(long, long, long, long, long, long,
         long, long, long, long, long, long);

enum { max_args = 12 };
int main(int argc, char ** argv) {
  long a[max_args];
  for (long i = 0; i < max_args; i++) {
    a[i] = 0;
  }
  for (long i = 1; i < argc; i++) {
    if (i - 1 < max_args) {
      a[i - 1] = atol(argv[i]);
    }
  }
  long y = foo(a[0], a[1], a[2], a[3], a[4], a[5],
               a[6], a[7], a[8], a[9], a[10], a[11]);
  printf("%ld\n", y);
  return 0;
}
