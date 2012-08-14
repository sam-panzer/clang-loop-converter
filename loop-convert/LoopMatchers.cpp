#include "LoopMatchers.h"

namespace clang {
namespace loop_migrate {

using namespace clang::ast_matchers;
const char LoopName[] = "forLoop";
const char ConditionBoundName[] = "conditionBound";
const char ConditionVarName[] = "conditionVar";
const char IncrementVarName[] = "incrementVar";
const char InitVarName[] = "initVar";
const char EndCallName[] = "endCall";
const char EndVarName[] = "endVar";

static const TypeMatcher AnyType = anything();

// FIXME: How best to document complicated matcher expressions? They're fairly
// self-documenting...but there may be some unintuitive parts.

StatementMatcher makeArrayLoopMatcher() {
  static StatementMatcher LHSMatcher =
  expression(ignoringImpCasts(declarationReference(to(
      variable(hasType(isInteger())).bind(ConditionVarName)))));
  static StatementMatcher RHSMatcher =
  expression(hasType(isInteger())).bind(ConditionBoundName);

  return id(LoopName, forStmt(
      hasLoopInit(declarationStatement(hasSingleDecl(id(InitVarName, variable(
          hasInitializer(ignoringImpCasts(integerLiteral(equals(0))))))))),
      hasCondition(binaryOperator(hasOperatorName("<"),
                                  hasLHS(LHSMatcher),
                                  hasRHS(RHSMatcher))),
      hasIncrement(unaryOperator(
          hasOperatorName("++"),
          hasUnaryOperand(declarationReference(to(
              variable(hasType(isInteger())).bind(IncrementVarName))))))));
}

// The matcher used for for loops.
StatementMatcher makeIteratorLoopMatcher() {
  StatementMatcher BeginCallMatcher =
      memberCall(argumentCountIs(0), callee(method(hasName("begin"))));

  DeclarationMatcher InitDeclMatcher =
      variable(hasInitializer(anything())).bind(InitVarName);

  DeclarationMatcher EndDeclMatcher =
      variable(hasInitializer(anything())).bind(EndVarName);

  StatementMatcher EndCallMatcher =
      memberCall(argumentCountIs(0), callee(method(hasName("end"))));

  StatementMatcher IteratorBoundMatcher =
      expression(anyOf(ignoringParenImpCasts(declarationReference(to(
          variable().bind(EndCallName)))),
                       ignoringParenImpCasts(
                           expression(EndCallMatcher).bind(EndCallName)),
                       materializeTemporaryExpression(
                           ignoringParenImpCasts(
                               expression(EndCallMatcher).bind(EndCallName)))));

  StatementMatcher IteratorComparisonMatcher =
      expression(ignoringParenImpCasts(declarationReference(to(
          variable().bind(ConditionVarName)))));

  StatementMatcher OverloadedNEQMatcher = overloadedOperatorCall(
      hasOverloadedOperatorName("!="),
      argumentCountIs(2),
      hasArgument(0, IteratorComparisonMatcher),
      hasArgument(1, IteratorBoundMatcher));

  return id(LoopName, forStmt(
            hasLoopInit(
                declarationStatement(declCountIs(2),
                                     containsDeclaration(0, InitDeclMatcher),
                                     containsDeclaration(1, EndDeclMatcher))),
            hasCondition(anyOf(
                binaryOperator(hasOperatorName("!="),
                               hasLHS(IteratorComparisonMatcher),
                               hasRHS(IteratorBoundMatcher)),
                binaryOperator(hasOperatorName("!="),
                               hasLHS(IteratorBoundMatcher),
                               hasRHS(IteratorComparisonMatcher)),
                OverloadedNEQMatcher)),
            hasIncrement(anyOf(
                unaryOperator(hasOperatorName("++"),
                              hasUnaryOperand(declarationReference(to(
                                  variable(hasType(pointsTo(AnyType)))
                                     .bind(IncrementVarName))))),
                overloadedOperatorCall(
                    hasOverloadedOperatorName("++"),
                    hasArgument(0, declarationReference(to(
                        variable().bind(IncrementVarName)))))))));
}

} // namespace loop_migrate
} // namespace clang
