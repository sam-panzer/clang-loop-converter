// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -- -I %S/Inputs \
// RUN:           && FileCheck -input-file=%t.cpp %s

#include "structures.h"
void f() {
  const int N = 10;
  int nums[N];
  int sum = 0;

  if (1) {
    int num = 0;
    for (int i = 0; i < N; ++i) {
      printf("Fibonacci number is %d\n", nums[i]);
      sum += nums[i] + 2 + num;
      (void) nums[i];
    }
    // CHECK: int num = 0;
    // CHECK-NEXT: for (auto & [[VAR:[a-z_]+]] : nums)
    // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
    // CHECK-NEXT: sum += [[VAR]] + 2 + num;
    // CHECK-NOT: (void) num;
    // CHECK: }
  }
}
