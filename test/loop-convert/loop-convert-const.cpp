// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: loop-convert . -prefer-auto -insert-const %t.cpp -- -I %S/Inputs \
// RUN:           && FileCheck -input-file=%t.cpp %s
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: rm -rf %t.cpp

#include "structures.h"

void f() {
  /// begin()/end() - based for loops here:
  T t;
  for (T::iterator it = t.begin(), e = t.end(); it != e; ++it) {
    printf("I found %d\n", *it);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : t)
  // CHECK-NEXT: printf("I found %d\n", [[VAR]]);

  T *pt;
  for (T::iterator it = pt->begin(), e = pt->end(); it != e; ++it) {
    printf("I found %d\n", *it);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : *pt)
  // CHECK-NEXT: printf("I found %d\n", [[VAR]]);

  S s;
  for (S::const_iterator it = s.begin(), e = s.end(); it != e; ++it) {
    printf("s has value %d\n", (*it).x);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: printf("s has value %d\n", [[VAR]].x);

  S *ps;
  for (S::const_iterator it = ps->begin(), e = ps->end(); it != e; ++it) {
    printf("s has value %d\n", (*it).x);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : *ps)
  // CHECK-NEXT: printf("s has value %d\n", [[VAR]].x);

  for (S::const_iterator it = s.begin(), e = s.end(); it != e; ++it) {
    printf("s has value %d\n", it->x);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: printf("s has value %d\n", [[VAR]].x);

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    it->x = 3;
  }
  // CHECK: for (auto{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].x = 3;

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    int & R = it->x;
  }
  // CHECK: for (auto{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: int & R = [[VAR]].x;

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    (*it).x = 3;
  }
  // CHECK: for (auto{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].x = 3;

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    it->nonConstFun(4, 5);
  }
  // CHECK: for (auto{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].nonConstFun(4, 5);


  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    it->constFun(3);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].constFun(3);

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    it->constFun(*it);
  }
  // CHECK: for (auto{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].constFun(elem);

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    it->constParamFun(*it);
  }
  // FIXME: make this const non-optional.
  // CHECK: for (auto{{( const)?}}{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].constParamFun(elem);

  for (S::iterator it = s.begin(), e = s.end(); it != e; ++it) {
    it->nonConstParamFun(*it);
  }
  // CHECK: for (auto{{ &? ?}}[[VAR:[a-z_]+]] : s)
  // CHECK-NEXT: [[VAR]].nonConstParamFun(elem);

  U u;
  for (U::iterator it = u.begin(), e = u.end(); it != e; ++it) {
    printf("s has value %d\n", it->x);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : u)
  // CHECK-NEXT: printf("s has value %d\n", [[VAR]].x);

  for (U::iterator it = u.begin(), e = u.end(); it != e; ++it) {
    printf("s has value %d\n", (*it).x);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : u)
  // CHECK-NEXT: printf("s has value %d\n", [[VAR]].x);

  dependent<int> v;
  for (dependent<int>::const_iterator it = v.begin(), e = v.end();
       it != e; ++it) {
    printf("Fibonacci number is %d\n", *it);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : v)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);

  for (dependent<int>::const_iterator it(v.begin()), e = v.end();
       it != e; ++it) {
    printf("Fibonacci number is %d\n", *it);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : v)
  // CHECK-NEXT: printf("Fibonacci number is %d\n", [[VAR]]);

  doublyDependent<int,int> intmap;
  for (doublyDependent<int,int>::iterator it = intmap.begin(), e = intmap.end();
       it != e; ++it) {
    printf("intmap[%d] = %d", it->first, it->second);
  }
  // CHECK: for (auto const{{ &? ?}}[[VAR:[a-z_]+]] : intmap)
  // CHECK-NEXT: printf("intmap[%d] = %d", [[VAR]].first, [[VAR]].second);

}
