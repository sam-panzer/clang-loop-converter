//===-- loop-convert/LoopConvert.cpp - C++11 For loop migration -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a tool that migrates for loops to take advantage of the
// range-basead syntax new to C++11.
//
// Usage:
// loop-convert <cmake-output-dir> <file1> <file2> ...
//
// Where <cmake-output-dir> is a CMake build directory containing a file named
// compile_commands.json.
//
// <file1>... specify the pahs of files in the CMake source tree, with the same
// requirements as other tools built on LibTooling.
//
//===----------------------------------------------------------------------===//

#include "LoopActions.h"
#include "LoopMatchers.h"

#include "clang/Basic/FileManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Refactoring.h"

using clang::ast_matchers::MatchFinder;
namespace cl = llvm::cl;
using namespace clang::tooling;
using namespace clang::loop_migrate;

static cl::opt<std::string> BuildPath(
    cl::Positional,
    cl::desc("<build-path>"));

static cl::list<std::string> SourcePaths(
    cl::Positional,
    cl::desc("<source0> [... <sourceN>]"),
    cl::OneOrMore);

// General options go here:
static cl::opt<bool> CountOnly(
    "count-only", cl::desc("Do not apply transformations; only count them."));

static cl::opt<TranslationConfidenceKind> TransformationLevel(
    cl::desc("Choose safety requirements for transformations:"),
    cl::values(clEnumValN(TCK_Safe, "A0", "Enable safe transformations"),
               clEnumValN(TCK_Extra, "A1",
                         "Enable transformations that might change semantics "
                         "(default)"),
               clEnumValN(TCK_Risky, "A2",
                          "Enable transformations that are likely "
                          "to change semantics"),
               clEnumValEnd),
    cl::init(TCK_Extra));

int main(int argc, const char **argv) {
  // Adjust arguments to force compilation in C++11 mode.
  int NumArgs = argc + 1;
  const char **Args = new const char *[NumArgs];
  for (int I = 0; I < argc; ++I)
    Args[I] = argv[I];
  Args[NumArgs - 1] = "-std=c++11";
  llvm::OwningPtr<CompilationDatabase> Compilations(
      FixedCompilationDatabase::loadFromCommandLine(NumArgs, Args));
  cl::ParseCommandLineOptions(NumArgs, Args);
  if (!Compilations) {
    std::string ErrorMessage;
    Compilations.reset(
        !BuildPath.empty() ?
        CompilationDatabase::autoDetectFromDirectory(BuildPath, ErrorMessage) :
        CompilationDatabase::autoDetectFromSource(SourcePaths[0],
                                                  ErrorMessage));
    if (!Compilations)
      llvm::report_fatal_error(ErrorMessage);
  }
  ClangTool SyntaxTool(*Compilations, SourcePaths);

  // First, let's check to make sure there were no errors.
  if (int result = SyntaxTool.run(
      newFrontendActionFactory<clang::SyntaxOnlyAction>())) {
    llvm::errs() << "Error compiling files.\n";
    return result;
  }

  RefactoringTool LoopTool(*Compilations, SourcePaths);
  StmtAncestorASTVisitor ParentFinder;
  StmtGeneratedVarNameMap GeneratedDecls;
  ReplacedVarsMap ReplacedVars;
  LoopFixerArgs FixerArgs = {&LoopTool.getReplacements(), &GeneratedDecls,
                             &ReplacedVars, /*AcceptedChanges=*/0,
                             /*DeferredChanges=*/0, /*RejectedChanges=*/0,
                             CountOnly, TransformationLevel};
  MatchFinder Finder;
  LoopFixer ArrayLoopFixer(&FixerArgs, &ParentFinder, LFK_Array);
  Finder.addMatcher(makeArrayLoopMatcher(), &ArrayLoopFixer);
  LoopFixer IteratorLoopFixer(&FixerArgs, &ParentFinder, LFK_Iterator);
  Finder.addMatcher(makeIteratorLoopMatcher(), &IteratorLoopFixer);
  if (int result = LoopTool.run(newFrontendActionFactory(&Finder))) {
    llvm::errs() << "Error encountered during translation.\n";
    return result;
  }

  llvm::outs() << "\nFor Loop Conversion:\n\t" << FixerArgs.AcceptedChanges
               << " converted loop(s)\n\t" << FixerArgs.DeferredChanges
               << " potentially conflicting change(s) deferred.\n\t"
               << FixerArgs.RejectedChanges << " change(s) rejected.\n";
  if (FixerArgs.DeferredChanges > 0)
     llvm::outs() << "Re-run this tool to attempt applying deferred changes.\n";
  if (FixerArgs.RejectedChanges > 0)
     llvm::outs() << "Re-run this tool with a lower required confidence level "
                     "to apply rejected changes.\n";

  if (FixerArgs.AcceptedChanges > 0) {
    // Check to see if the changes introduced any new errors.
    ClangTool EndSyntaxTool(*Compilations, SourcePaths);
    if (int result = EndSyntaxTool.run(
        newFrontendActionFactory<clang::SyntaxOnlyAction>())) {
      llvm::errs() << "Error compiling files after translation.\n";
      return result;
    }
  }

  delete[] Args;
  return 0;
}
