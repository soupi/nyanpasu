#include <stdio.h>
#include <stdlib.h>

extern int  my_code() asm("my_code");
extern void error()   asm("error");
extern int print()    asm("print");

const int PAIR_TAG   = 7;
const int BOOL_TAG   = 0x00000001;
const int BOOL_TRUE  = 0x80000001;
const int BOOL_FALSE = 0x0 | BOOL_TAG;

int* heap;

int print(int val) {
  if ((val & PAIR_TAG) == PAIR_TAG) {
    int* p = (int*)(val - PAIR_TAG);
    printf("(");
    print(*p);
    printf(", ");
    print(*(p + 1));
    printf(")");
  } else if ((val & BOOL_TAG) == 0) {
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
const int ERR_NOT_PAIR = 3;
const int ERR_NO_MEM = 17;
// other error codes here

void error(int errCode, int val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "*** Type Error: Expected number, but got %010x\n", val);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "*** Type Error: Expected boolean, but got %010x\n", val);
  } else if (errCode == ERR_NOT_PAIR) {
    fprintf(stderr, "*** Type Error: Expected pair, but got %010x\n", val);
  } else if (errCode == ERR_NO_MEM) {
    fprintf(stderr, "*** Error: Not enough memory. Heap ran out of space. ESI: %p\n", (void*)val);
  } else {
    fprintf(stderr, "*** Error: Unexpected error code %d for value %010x\n", errCode, val);
  }

  exit(errCode);
}

int main(int argc, char** argv) {
  heap = malloc(1024*1024);
  if (heap == NULL) {
    fprintf(stderr, "*** Error: malloc() Failed. Cannot build heap.\n");
  }
  // printf("%p\n", heap);
  int result = my_code(heap);
  print(result);
  puts("");
  free(heap);
  return 0;
}
