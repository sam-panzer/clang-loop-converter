#include "StmtAncestor.h"

namespace clang {
namespace loop_migrate {

//////////////////////StmtAncestorASTVisitor functions
// All this really does is inject push_back() before running
// RecursiveASTVisitor::TraverseStmt() and pop_back() afterwards. The Stmt atop
// the stack is the parent of the current statement (NULL for the topmost
// statement).
bool StmtAncestorASTVisitor::TraverseStmt(Stmt *Statement) {
  StmtAncestors.insert(std::make_pair(Statement, StmtStack.back()));
  StmtStack.push_back(Statement);
  RecursiveASTVisitor<StmtAncestorASTVisitor>::TraverseStmt(Statement);
  StmtStack.pop_back();
  return true;
}

// Keep track of the DeclStmt associated with each VarDecl. Combined with
// StmtAncestors, this provides roughly the same information as Scope, as we can
// map a VarDecl to its DeclStmt, then walk up the parent tree using
// StmtAncestors.
bool StmtAncestorASTVisitor::VisitDeclStmt(DeclStmt *Decls) {
  for (DeclStmt::const_decl_iterator I = Decls->decl_begin(),
                                     E = Decls->decl_end(); I != E; ++I)
    if (const VarDecl *VD = dyn_cast<VarDecl>(*I))
      DeclParents.insert(std::make_pair(VD, Decls));
  return true;
}

//////////////////////ComponentFinderASTVisitor functions
bool ComponentFinderASTVisitor::VisitDeclRefExpr(DeclRefExpr *E) {
  Components.push_back(E);
  return true;
}

bool ComponentFinderASTVisitor::VisitMemberExpr(MemberExpr *Member) {
  Components.push_back(Member);
  return true;
}

//////////////////////DependencyFinderASTVisitor functions
// Just forward any DeclRefExprs to a check on the actual variable declaration.
bool DependencyFinderASTVisitor::VisitDeclRefExpr(DeclRefExpr *DRE) {
  if (VarDecl *VD = dyn_cast_or_null<VarDecl>(DRE->getDecl()))
    return VisitVarDecl(VD);
  return true;
}

// Determine if any this variable is declared inside the ContainingLoop.
bool DependencyFinderASTVisitor::VisitVarDecl(VarDecl *VD) {
  const Stmt *Curr = DeclParents->lookup(VD);
  // First, see if the variable was declared within an inner scope of the loop.
  for (const Stmt *Prev = Curr; Curr != NULL;
       Prev = Curr, Curr = StmtParents->lookup(Prev))
    if (Curr == ContainingLoop) {
      ExternallyDependent = true;
      return false;
    }

  // Next, check if the variable was removed from existence by an earlier
  // iteration.
  for (ReplacedVarsMap::const_iterator I = ReplacedVars->begin(),
                                       E = ReplacedVars->end(); I != E; ++I)
    if ((*I).second == VD) {
      ExternallyDependent = true;
      return false;
    }
  return true;
}

///////////////////////DeclFinderASTVisitor Functions
// If we already created a variable for FS, check to make sure it has a
// different name.
bool DeclFinderASTVisitor::VisitForStmt(ForStmt *FS) {
  DeclMap::const_iterator I = GeneratedDecls->find(FS);
  if (I != GeneratedDecls->end() && I->second == Name) {
    Found = true;
    return false;
  }
  return true;
}

// If any named declaration has the same name, then consider Name already spoken
// for.
bool DeclFinderASTVisitor::VisitNamedDecl(NamedDecl *ND) {
  const IdentifierInfo *Ident = ND->getIdentifier();
  if (Ident && Ident->getName() == Name) {
    Found = true;
    return false;
  }
  return true;
}

// Forward any declaration references to the actual check on the referenced
// declaration.
bool DeclFinderASTVisitor::VisitDeclRefExpr(DeclRefExpr *DRE) {
  if (NamedDecl *ND = dyn_cast_or_null<NamedDecl>(DRE->getDecl()))
    return VisitNamedDecl(ND);
  return true;
}

} // namespace clang
} // namespace for_migrate
