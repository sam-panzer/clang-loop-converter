//===-- loop-convert/StmtAncestor.h - AST property visitors -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration several RecursiveASTVisitors used to build
// and check data structures used in loop migration.
//
//===----------------------------------------------------------------------===//
#ifndef _LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_STMT_ANCESTOR_H_
#define _LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_STMT_ANCESTOR_H_
#include "clang/AST/RecursiveASTVisitor.h"

namespace clang {
namespace loop_migrate {

typedef llvm::DenseMap<const Stmt*, const Stmt*> StmtMap;
typedef llvm::DenseMap<const VarDecl*, const DeclStmt*> DeclParentMap;
typedef llvm::DenseMap<const ForStmt*, const VarDecl *> ReplacedVarsMap;
typedef llvm::DenseMap<const Stmt*, std::string> DeclMap;
typedef llvm::SmallVector<const Expr *, 16> ComponentVector;

/// \brief Class used build the reverse AST properties needed to detect
/// name conflicts and free variables.
class StmtAncestorASTVisitor :
  public RecursiveASTVisitor<StmtAncestorASTVisitor> {
 private:
  StmtMap StmtAncestors;
  DeclParentMap DeclParents;
  llvm::SmallVector<const Stmt *, 16> StmtStack;

 public:
  StmtAncestorASTVisitor() {
    StmtStack.push_back(NULL);
  }

  /// \brief Run the analysis on the TranslationUnitDecl.
  ///
  /// In case we're running this analysis multiple times, don't repeat the
  /// work unless RunEvenIfNotEmpty is set to true.
  void gatherAncestors(const TranslationUnitDecl *TUD, bool RunEvenIfNotEmpty) {
    if (RunEvenIfNotEmpty || StmtAncestors.empty()) {
      TraverseDecl(const_cast<TranslationUnitDecl *>(TUD));
    }
  }

  /// Accessor for StmtAncestors.
  const StmtMap &getStmtMap() {
    return StmtAncestors;
  }

  /// Accessor for DeclParents.
  const DeclParentMap &getDeclMap() {
    return DeclParents;
  }

  friend class RecursiveASTVisitor<StmtAncestorASTVisitor>;
 private:
  bool TraverseStmt(Stmt *Statement);
  bool VisitDeclStmt(DeclStmt *Statement);
};

/// Class used to find the variables and member expressions on which an
/// arbitrary expression depends.
class ComponentFinderASTVisitor :
  public RecursiveASTVisitor<ComponentFinderASTVisitor> {
 private:
  ComponentVector Components;

 public:
  ComponentFinderASTVisitor() { }

  /// Find the components of an expression and place them in a ComponentVector.
  void findExprComponents(const Expr *SourceExpr) {
    Expr *E = const_cast<Expr *>(SourceExpr);
    RecursiveASTVisitor<ComponentFinderASTVisitor>::TraverseStmt(E);
  }

  /// Accessor for Components.
  const ComponentVector &getComponents() {
    return Components;
  }

  friend class RecursiveASTVisitor<ComponentFinderASTVisitor>;

 private:
  bool VisitDeclRefExpr(DeclRefExpr *E);
  bool VisitMemberExpr(MemberExpr *Member);
};

/// Class used to determine if an expression is dependent on a variable declared
/// inside of the loop where it would be used.
class DependencyFinderASTVisitor :
  public RecursiveASTVisitor<DependencyFinderASTVisitor> {
 private:
  const StmtMap *StmtParents;
  const DeclParentMap *DeclParents;
  const Stmt *ContainingLoop;
  const ReplacedVarsMap *ReplacedVars;
  bool ExternallyDependent;

 public:
  DependencyFinderASTVisitor(const StmtMap *StmtParents,
                             const DeclParentMap *DeclParents,
                             const ReplacedVarsMap *ReplacedVars,
                             const Stmt *ContainingLoop) :
    StmtParents(StmtParents), DeclParents(DeclParents),
    ContainingLoop(ContainingLoop), ReplacedVars(ReplacedVars) { }

  /// Run the analysis on Body, and return true iff the expression depends on
  /// some variable declared within ContainingLoop.
  bool isExternallyDependent(const Stmt *Body) {
    ExternallyDependent = false;
    TraverseStmt(const_cast<Stmt *>(Body));
    return ExternallyDependent;
  }

  friend class RecursiveASTVisitor<DependencyFinderASTVisitor>;

 private:
  bool VisitVarDecl(VarDecl *VD);
  bool VisitDeclRefExpr(DeclRefExpr *DRE);
};

/// Class used to determine if any declarations used in a Stmt would conflict
/// with a particular identifier.
class DeclFinderASTVisitor : public RecursiveASTVisitor<DeclFinderASTVisitor> {
 private:
  const std::string &Name;
  const DeclMap *GeneratedDecls;
  bool Found;

 public:
  DeclFinderASTVisitor(const std::string &Name,
                       const DeclMap *GeneratedDecls) :
    Name(Name), GeneratedDecls(GeneratedDecls), Found(false) { }

  /// Attempts to find any usages of variables name Name in Body, returning
  /// true when it is only used as an array index. A list of usages and
  /// the arrays TargetVar indexes are also tracked.
  bool findUsages(const Stmt *Body) {
    Found = false;
    TraverseStmt(const_cast<Stmt *>(Body));
    return Found;
  }

  friend class RecursiveASTVisitor<DeclFinderASTVisitor>;

 private:
  bool VisitForStmt(ForStmt *FS);
  bool VisitNamedDecl(NamedDecl *ND);
  bool VisitDeclRefExpr(DeclRefExpr *DRE);
};

} // namespace for_migrate
} // namespace clang
#endif //_LLVM_TOOLS_CLANG_TOOLS_LOOP_CONVERT_STMT_ANCESTOR_H_
