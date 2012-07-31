#include "LoopActions.h"

namespace clang {
namespace loop_migrate {

using namespace clang::ast_matchers;

const char LoopName[] = "forLoop";
const char ConditionBoundName[] = "conditionBound";
const char ConditionVarName[] = "conditionVar";
const char IncrementVarName[] = "incrementVar";
const char InitVarName[] = "initVar";

// Returns true when two ValueDecls are the same variable.
static bool areSameVariable(const ValueDecl *First, const ValueDecl *Second) {
  return First && Second &&
         First->getCanonicalDecl() == Second->getCanonicalDecl();
}

void LoopPrinter::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Context = Result.Context;
  const ForStmt *FS = Result.Nodes.getStmtAs<ForStmt>(LoopName);

  if (!Context->getSourceManager().isFromMainFile(FS->getForLoc()))
    return;

  const VarDecl *LoopVar = Result.Nodes.getDeclAs<VarDecl>(IncrementVarName);
  const VarDecl *CondVar = Result.Nodes.getDeclAs<VarDecl>(ConditionVarName);
  const VarDecl *InitVar = Result.Nodes.getDeclAs<VarDecl>(InitVarName);

  if (!areSameVariable(LoopVar, CondVar) || !areSameVariable(LoopVar, InitVar))
    return;
  const Expr *BoundExpr= Result.Nodes.getStmtAs<Expr>(ConditionBoundName);

  llvm::outs() << "Discovered potentially translatable loop: variable is "
               << LoopVar->getNameAsString() << ".\n Bound expression is: ";
  BoundExpr->dump();
  llvm::outs() << "\n";
}

static StatementMatcher ArrayLHSMatcher =
  expression(ignoringImpCasts(declarationReference(to(
      variable(hasType(isInteger())).bind(ConditionVarName)))));
static StatementMatcher ArrayRHSMatcher =
  expression(hasType(isInteger())).bind(ConditionBoundName);

StatementMatcher LoopMatcher =
  id(LoopName, forStmt(
      hasLoopInit(declarationStatement(hasSingleDecl(variable(
          hasInitializer(integerLiteral(equals(0)))).bind(InitVarName)))),
      hasCondition(binaryOperator(hasOperatorName("<"),
                                  hasLHS(ArrayLHSMatcher),
                                  hasRHS(ArrayRHSMatcher))),
      hasIncrement(unaryOperator(
          hasOperatorName("++"),
          hasUnaryOperand(declarationReference(to(
              variable(hasType(isInteger())).bind(IncrementVarName))))))));


} // namespace loop_migrate
} // namespace clang
