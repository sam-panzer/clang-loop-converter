// RUN: rm -rf %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %s > %t.cpp
// RUN: grep -Ev "//\s*[A-Z-]+:" %S/Inputs/negative-header.h > \
// RUN:       %T/negative-header.h
// RUN: loop-convert . %t.cpp -- && FileCheck -input-file=%t.cpp %s \
// RUN:   && FileCheck -input-file=%T/negative-header.h \
// RUN:                %S/Inputs/negative-header.h

#include "negative-header.h"
struct T { void g(); };

// Single FileCheck line to make sure that no loops are converted.
// CHECK-NOT: for ({{.*[^:]:[^:].*}})

const int N = 6;
int arr[N] = {1, 2, 3, 4, 5, 6};
int (*pArr)[N] = &arr;
int sum = 0;

// Checks for the index start and end:
void indexStartAndEnd() {
  for (int i = 0; i < N + 1; ++i)
    sum += arr[i];

  for (int i = 0; i < N - 1; ++i)
    sum += arr[i];

  for (int i = 1; i < N; ++i)
    sum += arr[i];

  for (int i = 1; i < N; ++i)
    sum += arr[i];

  for (int i = 0; ; ++i)
    sum += (*pArr)[i];
}

// Checks for invalid increment steps:
void increment() {
  for (int i = 0; i < N; --i)
    sum += arr[i];

  for (int i = 0; i < N; i)
    sum += arr[i];

  for (int i = 0; i < N;)
    sum += arr[i];

  for (int i = 0; i < N; i += 2)
    sum ++;
}

// Checks to make sure that the index isn't used outside of the array:
void indexUse() {
  for (int i = 0; i < N; ++i)
    arr[i] += 1 + i;
}

// Check for loops that don't mention arrays
void noArray() {
  for (int i = 0; i < N; ++i)
    sum += i;

  for (int i = 0; i < N; ++i) { }

  for (int i = 0; i < N; ++i) ;
}

// Checks for incorrect loop variables.
void mixedVariables() {
  int badIndex;
  for (int i = 0; badIndex < N; ++i)
    sum += arr[i];

  for (int i = 0; i < N; ++badIndex)
    sum += arr[i];

  for (int i = 0; badIndex < N; ++badIndex)
    sum += arr[i];

  for (int i = 0; badIndex < N; ++badIndex)
    sum += arr[badIndex];
}

// Checks for multiple arrays indexed.
void multipleArrays() {
  int badArr[N];

  for (int i = 0; i < N; ++i)
    sum += arr[i] + badArr[i];
}
