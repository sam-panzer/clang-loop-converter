#include "LoopActions.h"

namespace clang {
namespace loop_migrate {

using namespace clang::ast_matchers;

void LoopPrinter::run(const MatchFinder::MatchResult &Result) {
  if (const ForStmt *FS = Result.Nodes.getStmtAs<ForStmt>("forLoop"))
    FS->dump();
}

StatementMatcher LoopMatcher =
  id("forLoop", forStmt(
      hasLoopInit(declarationStatement(hasSingleDecl(variable(
          hasInitializer(integerLiteral(equals(0))))))),
      hasIncrement(unaryOperator(
          hasOperatorName("++"),
          hasUnaryOperand(declarationReference(to(
              variable(hasType(isInteger())).bind("incrementVariable"))))))));

} // namespace loop_migrate
} // namespace clang
