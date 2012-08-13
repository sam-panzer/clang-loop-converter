// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -- -I %S/Inputs \
// RUN:           && FileCheck -input-file=%t.cpp %s

#include "structures.h"
void f() {
  const int N = 10;
  Val Arr[N];
  for (int i = 0; i < N; ++i) {
    Val &t = Arr[i];
    int y = t.x;
  }
  // CHECK: for (auto & t : Arr)
  // The next line is left blank and not deleted, so we're not using NEXT.
  // CHECK: int y = t.x;

  int nums[N];
  int sum = 0;

  for (int i = 0; i < N; ++i) {
    printf("Fibonacci number is %d\n", nums[i]);
    sum += nums[i] + 2;
  }
  // CHECK: for (int & num : nums)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", num);
  // CHECK-NEXT: sum += num + 2;

  if (1) {
    int num = 0;
    for (int i = 0; i < N; ++i) {
      printf("Fibonacci number is %d\n", nums[i]);
      sum += nums[i] + 2 + num;
    }
    // CHECK: int num = 0;
    // CHECK-NOT: for (int & num : nums)
    // CHECK-NEXT: for (int & [[VAR:[a-z_]+]] : nums)
    // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
    // CHECK-NEXT: sum += [[VAR]] + 2 + num;
  }
}
