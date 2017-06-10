#include <stdio.h>
#include <stdlib.h>

extern int  my_code() asm("my_code");
extern void error()   asm("error");

const int BOOL_TAG   = 0x00000001;
const int BOOL_TRUE  = 0x80000001;
const int BOOL_FALSE = 0x0 | BOOL_TAG;

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

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
// other error codes here

void error(int errCode, int val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "*** Type Error: Expected number, but got %010x\n", val);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "*** Type Error: Expected boolean, but got %010x\n", val);
  } else {
    fprintf(stderr, "*** Error: Unexpected error code %d for value %010x\n", errCode, val);
  }

  exit(errCode);
}

int main(int argc, char** argv) {
  int result = my_code();
  print(result);
  puts("");
  return 0;
}
