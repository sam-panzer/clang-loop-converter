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

#include "StmtAncestor.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

namespace clang {
namespace loop_migrate {
using clang::ast_matchers::MatchFinder;
using clang::ast_matchers::StatementMatcher;

/// \brief The level of safety to require of transformations.
enum TranslationConfidenceKind {
  TCK_Risky,
  TCK_Extra,
  TCK_Safe
};

enum LoopFixerKind {
  LFK_Array,
  LFK_Iterator,
  LFK_PseudoArray
};

/// \brief Argument pack for LoopFixer.
///
// Contains long-lived data structures that should be preserved across matcher
/// callback runs.
struct LoopFixerArgs {
  tooling::Replacements *Replace;
  StmtGeneratedVarNameMap *GeneratedDecls;
  ReplacedVarsMap *ReplacedVarRanges;
  unsigned AcceptedChanges;
  unsigned DeferredChanges;
  unsigned RejectedChanges;
  bool CountOnly;
  TranslationConfidenceKind ConfidenceLevel;
};

/// LoopFixer: The callback to be used for loop migration matchers.
///
/// The callback does extra checking not possible in matchers, and attempts to
/// convert the for loop, if possible.

class LoopFixer : public MatchFinder::MatchCallback {
 private:
  StmtAncestorASTVisitor *ParentFinder;
  LoopFixerArgs *Args;
  LoopFixerKind FixerKind;

 public:
  LoopFixer(LoopFixerArgs *Args, StmtAncestorASTVisitor *ParentFinder,
            LoopFixerKind FixerKind) :
  ParentFinder(ParentFinder), Args(Args) , FixerKind(FixerKind)  { }
  virtual void run(const MatchFinder::MatchResult &Result);
};

} // namespace loop_migrate
} // namespace clang
#endif  // _LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_LOOPACTIONS_H_
