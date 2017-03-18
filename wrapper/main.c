#include <stdio.h>

extern int my_code() asm("my_code");

const int BOOL_TAG   = 0x00000001;
const int BOOL_TRUE  = -2147483647 & BOOL_TAG;
const int BOOL_FALSE = 0x0 & BOOL_TAG;

int print(int val) {
  if ((val & BOOL_TAG) == 0) {
    printf("%d", val >> 1);
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#010x", val);
  }
  return val;
}

int main(int argc, char** argv) {
  int result = my_code();
  print(result);
  return 0;
}
