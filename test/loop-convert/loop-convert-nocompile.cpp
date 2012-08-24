// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -- -I %S/Inputs \
// RUN:         || FileCheck -input-file=%t.cpp %s
// Note that this test expects the compilation to fail!

void valid() {
  const int arr[5];
  int sum = 0;
  for (int i = 0; i < 5; ++i) {
    sum += arr[i];
  }
}
void hasSyntaxError = 3;
// CHECK: void valid() {
// CHECK-NEXT: const int arr[5];
// CHECK-NEXT: int sum = 0;
// CHECK-NEXT: for (int i = 0; i < 5; ++i) {
// CHECK-NEXT: sum += arr[i];
// CHECK-NEXT: }
// CHECK-NEXT: }

// CHECK-NEXT: void hasSyntaxError = 3;
