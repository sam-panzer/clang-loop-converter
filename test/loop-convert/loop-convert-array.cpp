// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -- && FileCheck -input-file=%t.cpp %s

struct T { void g(); };
void f() {
  const int N = 6;
  const int NMinusOne = N - 1;
  int arr[N] = {1, 2, 3, 4, 5, 6};
  int (*pArr)[N] = &arr;
  int sum = 0;

  for (int i = 0; i < N; ++i) {
    sum += arr[i];
  }
  // CHECK: for (auto & [[VAR:[a-z_]+]] : arr) {
  // CHECK-NEXT: sum += [[VAR]];
  // CHECK-NEXT: }

  for (int i = 0; i < N; ++i) {
    arr[i] += 1;
  }
  // CHECK: for (auto & [[VAR:[a-z_]+]] : arr) {
  // CHECK-NEXT: [[VAR]] += 1;
  // CHECK-NEXT: }

  for (int i = 0; i < NMinusOne + 1; ++i) {
    sum += arr[i];
  }
  // CHECK: for (auto & [[VAR:[a-z_]+]] : arr) {
  // CHECK-NEXT: sum += [[VAR]];
  // CHECK-NEXT: }

  for (int i = 0; i < N; ++i) {
    sum += (*pArr)[i];
  }
  // CHECK: for (auto & [[VAR:[a-z_]+]] : *pArr) {
  // CHECK-NEXT: sum += [[VAR]];
  // CHECK-NEXT: }

  T teas[N];
  for (int i = 0; i < N; ++i) {
    teas[i].g();
  }
  // CHECK: for (auto & [[VAR:[a-z_]+]] : teas) {
  // CHECK-NEXT: [[VAR]].g();
  // CHECK-NEXT: }
}
