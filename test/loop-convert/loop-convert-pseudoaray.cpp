// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: c++migrate -for-range . %t.cpp -- -I %S/Inputs \
// RUN:           && FileCheck -input-file=%t.cpp %s
// RUN: rm -rf %t.cpp
#include "structures.h"

/////////////// This file focuses on array-like containers.
// All loops should be convertible.

void f() {
  const int N = 6;
  dependent<int> v;
  dependent<int> *pv;

  transparent<dependent<int> > cv;

  int sum = 0;
  for (int i = 0, e = v.size(); i < e; ++i) {
    printf("Fibonacci number is %d\n", v[i]);
    sum += v[i] + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : v)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
  // CHECK-NEXT: sum += [[VAR]] + 2;

  for (int i = 0, e = v.size(); i < e; ++i) {
    printf("Fibonacci number is %d\n", v.at(i));
    sum += v.at(i) + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : v)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
  // CHECK-NEXT: sum += [[VAR]] + 2;

  for (int i = 0, e = pv->size(); i < e; ++i) {
    printf("Fibonacci number is %d\n", pv->at(i));
    sum += pv->at(i) + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : *pv)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
  // CHECK-NEXT: sum += [[VAR]] + 2;

  for (int i = 0, e = cv->size(); i < e; ++i) {
    printf("Fibonacci number is %d\n", cv->at(i));
    sum += cv->at(i) + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : *cv)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
  // CHECK-NEXT: sum += [[VAR]] + 2;

}
