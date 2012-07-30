#include "LoopActions.h"

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

int main(int argc, const char **argv) {
  // OwningPtr is one of LLVM’s RAII smart pointers.
  llvm::OwningPtr<CompilationDatabase> Compilations(
      FixedCompilationDatabase::loadFromCommandLine(argc, argv));
  cl::ParseCommandLineOptions(argc, argv);
  if (!Compilations) {
    std::string ErrorMessage;
    Compilations.reset(CompilationDatabase::loadFromDirectory(BuildPath,
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
  Replacements &Replace = LoopTool.getReplacements();
  LoopFixer Fixer(Replace);
  MatchFinder Finder;
  Finder.addMatcher(LoopMatcher, &Fixer);
  if (int result = LoopTool.run(newFrontendActionFactory(&Finder))) {
    llvm::errs() << "Error encountered during translation.\n";
    return result;
  }

  return 0;
}
