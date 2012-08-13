#ifndef _LLVM_TOOLS_CLANG_TOOLS_TESTS_TOOLING_STRUCTURES_H_
#define _LLVM_TOOLS_CLANG_TOOLS_TESTS_TOOLING_STRUCTURES_H_

extern "C" {
extern int printf(const char *restrict, ...);
}

struct Val {int x; void g(); };
#endif  // _LLVM_TOOLS_CLANG_TOOLS_TESTS_TOOLING_STRUCTURES_H_
