//===-- loop-convert/LoopActions.h - C++11 For loop migration ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares matchers and callbacks for use in migrating C++ for loops.
//
//===----------------------------------------------------------------------===//
#ifndef _LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_LOOPACTIONS_H_
#define _LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_LOOPACTIONS_H_

#include "clang/Tooling/Refactoring.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

namespace clang {
namespace loop_migrate {
using clang::ast_matchers::MatchFinder;
using clang::ast_matchers::StatementMatcher;

class LoopFixer : public MatchFinder::MatchCallback {
 private:
  tooling::Replacements &Replace;

 public:
  explicit LoopFixer(tooling::Replacements &Replace) : Replace(Replace) { }
  virtual void run(const MatchFinder::MatchResult &Result);
};

extern StatementMatcher LoopMatcher;

} // namespace loop_migrate
} // namespace clang
#endif  // _LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_LOOPACTIONS_H_
