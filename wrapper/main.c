#include <stdio.h>

extern int my_code() asm("my_code");

int main(int argc, char** argv) {
  int result = my_code();
  printf("%d\n", result);
  return 0;
}
