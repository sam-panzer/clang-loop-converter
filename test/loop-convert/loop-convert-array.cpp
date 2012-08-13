// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -- -I %S/Inputs \
// RUN:         && FileCheck -input-file=%t.cpp %s
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -A2 -- -I %S/Inputs \
// RUN:         && FileCheck -check-prefix=RISKY -input-file=%t.cpp %s

#include "structures.h"
void f() {
  const int N = 6;
  const int NMinusOne = N - 1;
  int arr[N] = {1, 2, 3, 4, 5, 6};
  int (*pArr)[N] = &arr;
  int sum = 0;

  for (int i = 0; i < N; ++i) {
    sum += arr[i];
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr) {
  // CHECK-NEXT: sum += [[VAR]];
  // CHECK-NEXT: }

  for (int i = 0; i < N; ++i) {
    printf("Fibonacci number is %d\n", arr[i]);
    sum += arr[i] + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);
  // CHECK-NEXT: sum += [[VAR]] + 2;

  for (int i = 0; i < N; ++i) {
    int x = arr[i];
    int y = arr[i] + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr)
  // CHECK-NEXT: int x = [[VAR]];
  // CHECK-NEXT: int y = [[VAR]] + 2;

  for (int i = 0; i < N; ++i) {
    int x = N;
    x = arr[i];
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr)
  // CHECK-NEXT: int x = N;
  // CHECK-NEXT: x = [[VAR]];

  for (int i = 0; i < N; ++i) {
    arr[i] += 1;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr) {
  // CHECK-NEXT: [[VAR]] += 1;
  // CHECK-NEXT: }

  for (int i = 0; i < N; ++i) {
    int x = arr[i] + 2;
    arr[i] ++;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr)
  // CHECK-NEXT: int x = [[VAR]] + 2;
  // CHECK-NEXT: [[VAR]] ++;

  for (int i = 0; i < N; ++i) {
    arr[i] = 4 + arr[i];
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr)
  // CHECK-NEXT: [[VAR]] = 4 + [[VAR]];

  for (int i = 0; i < NMinusOne + 1; ++i) {
    sum += arr[i];
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr) {
  // CHECK-NEXT: sum += [[VAR]];
  // CHECK-NEXT: }

  for (int i = 0; i < N; ++i) {
    printf("Fibonacci number %d has address %p\n", arr[i], &arr[i]);
    sum += arr[i] + 2;
  }
  // CHECK: for (int & [[VAR:[a-z_]+]] : arr)
  // CHECK-NEXT: printf("Fibonacci number %d has address %p\n", [[VAR]], &[[VAR]]);
  // CHECK-NEXT: sum += [[VAR]] + 2;

  for (int i = 0; i < N; ++i) {
    sum += (*pArr)[i];
  }
  // CHECK: for (int i = 0; i < N; ++i) {
  // CHECK: sum += (*pArr)[i];
  // RISKY: for (int & [[VAR:[a-z_]+]] : *pArr) {
  // RISKY-NEXT: sum += [[VAR]];
  // CHECK-NEXT: }

  Val teas[N];
  for (int i = 0; i < N; ++i) {
    teas[i].g();
  }
  // CHECK: for (auto & [[VAR:[a-z_]+]] : teas) {
  // CHECK-NEXT: [[VAR]].g();
  // CHECK-NEXT: }
}
