// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . %t.cpp -- -I %S/Inputs \
// RUN:           && FileCheck -input-file=%t.cpp %s
// RUN: rm -rf %t.cpp
#include "structures.h"

struct RepeatedField {
  RepeatedField();
  int foo(int index) const;
  void set_foo(int index, int value) const;
  int foo_size() const;
  int blah_size() const;
  const dependent<int> foo() const;
  dependent<int> &mutable_foo();
};

void f() {
  RepeatedField rf;

  for (int i = 0; i < rf.foo_size(); ++i) {
    int k = 1 + rf.foo(i);
    int s = rf.blah_size();
  }
  // CHECK: for (auto & [[VAR:[A-Za-z_]+]] : rf.foo()) {
  // CHECK-NEXT:  int k = 1 + [[VAR]];
  // CHECK-NEXT:  int s = rf.blah_size();
  // CHECK-NEXT: }
}
