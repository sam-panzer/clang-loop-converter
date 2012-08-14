#include "LoopActions.h"
#include "VariableNaming.h"

#include "clang/Lex/Lexer.h"

namespace clang {
namespace loop_migrate {

using namespace clang::ast_matchers;
using namespace clang::tooling;

// Constants used for matcher name bindings
const char LoopName[] = "forLoop";
const char ConditionBoundName[] = "conditionBound";
const char ConditionVarName[] = "conditionVar";
const char IncrementVarName[] = "incrementVar";
const char InitVarName[] = "initVar";

typedef llvm::SmallPtrSet<const Expr *, 1> ContainerResult;
typedef const Expr* Usage;
typedef llvm::SmallVector<Usage, 8> UsageResult;

/// ForLoopIndexUseVisitor - discover usages of expressions indexing into
/// containers.
///
/// Given an index variable, recursively crawls a for loop to discover if the
/// index variable is used in a way consistent with range-based for loop access.
class ForLoopIndexUseVisitor
    : public RecursiveASTVisitor<ForLoopIndexUseVisitor> {
 public:
  ForLoopIndexUseVisitor(ASTContext *Context, const VarDecl *IndexVar) :
    OnlyUsedAsIndex(true), Context(Context), IndexVar(IndexVar),
  AliasDecl(NULL) { }

  /// Accessor for Usages.
  const UsageResult &getUsages() const { return Usages; }

  /// Accessor for ContainersIndexed.
  const ContainerResult &getContainersIndexed() const {
    return ContainersIndexed;
  }

  /// Accessor for ConfidenceLevel.
  TranslationConfidenceKind getConfidenceLevel() const {
    return ConfidenceLevel;
  }

  /// Returns the statement declaring the variable created as an alias for the
  /// loop element, if any.
  const DeclStmt *getAliasDecl() const { return AliasDecl; }

  // Finds all uses of IndexVar in Body, placing all usages in Usages, all
  // referenced arrays in ContainersIndexed, and returns true if IndexVar was
  // only used as an array index.
  //
  // The general strategy is to reject any DeclRefExprs referencing IndexVar,
  // with the exception of certain acceptable patterns.
  // For arrays, the DeclRefExpr for IndexVar must appear as the index of an
  // ArraySubscriptExpression.
  bool findUsages(const Stmt *Body) {
    ConfidenceLevel = TCK_Safe;
    TraverseStmt(const_cast<Stmt *>(Body));
    return OnlyUsedAsIndex;
  }

 private:
  bool OnlyUsedAsIndex;
  ASTContext *Context;
  // A container which holds all usages of IndexVar as the index of
  // ArraySubscriptExpressions.
  UsageResult Usages;
  /// A set which holds expressions containing the referenced arrays.
  ContainerResult ContainersIndexed;
  /// The index variable's VarDecl.
  const VarDecl *IndexVar;
  // The DeclStmt for an alias to the container element.
  const DeclStmt *AliasDecl;
  // The Expr which refers to the container.
  TranslationConfidenceKind ConfidenceLevel;

  // Typedef used in CRTP functions.
  typedef RecursiveASTVisitor<ForLoopIndexUseVisitor> VisitorBase;
  friend class RecursiveASTVisitor<ForLoopIndexUseVisitor>;
  // Overriden methods for RecursiveASTVisitor's traversal
  bool TraverseArraySubscriptExpr(ArraySubscriptExpr *ASE);
  bool VisitDeclRefExpr(DeclRefExpr *DRE);
  bool VisitDeclStmt(DeclStmt *DS);
};

// Obtain the original source code text from a SourceRange.
// FIXME: Maybe put this somewhere more generally accessible?
static StringRef getStringFromRange(SourceManager &SourceMgr,
                                    const LangOptions &LangOpts,
                                    SourceRange Range) {
  if (SourceMgr.getFileID(Range.getBegin()) !=
      SourceMgr.getFileID(Range.getEnd()))
    return NULL;

  CharSourceRange SourceChars(Range, true);
  return Lexer::getSourceText(SourceChars, SourceMgr, LangOpts);
}

// Returns the DeclRefExpr represented by E, or NULL if there isn't one.
static const DeclRefExpr *getDeclRef(const Expr *E) {
  return dyn_cast<DeclRefExpr>(E->IgnoreParenImpCasts());
}

// If the given expression is actually a DeclRefExpr, find and return the
// underlying VarDecl; otherwise, return NULL.
static const VarDecl *getReferencedVariable(const Expr *E) {
  if (const DeclRefExpr *DRE = getDeclRef(E))
    return dyn_cast<VarDecl>(DRE->getDecl());
  return NULL;
}

// Returns true when the given expression is a direct member expression
static bool isDirectMemberExpr(const Expr *E) {
  if (const MemberExpr *Member = dyn_cast<MemberExpr>(E->IgnoreParenImpCasts()))
    return isa<CXXThisExpr>(Member->getBase()->IgnoreParenImpCasts());
  return false;
}

// Returns true when two ValueDecls are the same variable.
static bool areSameVariable(const ValueDecl *First, const ValueDecl *Second) {
  return First && Second &&
         First->getCanonicalDecl() == Second->getCanonicalDecl();
}

// Returns true when two Exprs are equivalent.
static bool areSameExpr(ASTContext* Context, const Expr *First,
                        const Expr *Second) {
  if (!First || !Second)
    return false;

  llvm::FoldingSetNodeID FirstID, SecondID;
  First->Profile(FirstID, *Context, true);
  Second->Profile(SecondID, *Context, true);
  return FirstID == SecondID;
}

// Returns true when the ContainerResult contains an Expr equivalent to E.
static bool containsExpr(ASTContext *Context, const ContainerResult *Container,
                         const Expr *E) {
  for (ContainerResult::const_iterator I = Container->begin(),
       End = Container->end(); I != End; ++I)
    if (areSameExpr(Context, E, *I))
      return true;
  return false;
}

// Returns true when the index expression is a declaration reference to
// IndexVar and the array's base exists.
static bool isIndexInSubscriptExpr(const Expr *IndexExpr,
                                   const VarDecl *IndexVar,
                                   const Expr *Arr) {
  const DeclRefExpr *Idx = getDeclRef(IndexExpr);
  return Arr && Idx && Idx->getType()->isIntegerType()
             && areSameVariable(IndexVar, Idx->getDecl());
}

// Determines whether the given Decl defines an alias to the given variable.
static bool isAliasDecl(const Decl *TheDecl, const VarDecl *TargetDecl) {
  const VarDecl *VDecl = dyn_cast<VarDecl>(TheDecl);
  if (!VDecl)
    return false;

  if (!VDecl->hasInit())
    return false;
  const Expr *Init = VDecl->getInit()->IgnoreImpCasts();
  if (Init->getStmtClass() == Stmt::ArraySubscriptExprClass) {
    const ArraySubscriptExpr *ASE = cast<ArraySubscriptExpr>(Init);
    // We don't really care which array is used here. We check to make sure
    // it was the correct one later, since the AST will traverse it next.
    return isIndexInSubscriptExpr(ASE->getIdx(), TargetDecl, ASE->getBase());
  }
  return false;
}

// Determines whether the bound of a for loop condition expression is the same
// as the statically computable size of ArrayType.
static bool arrayMatchesBoundExpr(ASTContext *Context,
                                  const QualType &ArrayType,
                                  const Expr *ConditionExpr) {
  const Type *T = ArrayType.getCanonicalType().getTypePtr();
  if (const ConstantArrayType *CAT = dyn_cast<ConstantArrayType>(T)) {
    llvm::APSInt ConditionSize;
    if (!ConditionExpr->isIntegerConstantExpr(ConditionSize, *Context))
      return false;
    llvm::APSInt ArraySize(CAT->getSize());
    return llvm::APSInt::isSameValue(ConditionSize, ArraySize);
  }
  return false;
}

// If we encounter an array with IndexVar as the index, note it and prune the
// AST traversal so that VisitDeclRefExpr() doesn't discover the reference to
// the index and mark it as unconvertible.
// Otherwise, continue the traversal recursively.
bool ForLoopIndexUseVisitor::TraverseArraySubscriptExpr(
    ArraySubscriptExpr *ASE) {
  Expr *Arr = ASE->getBase();
  if (isIndexInSubscriptExpr(ASE->getIdx(), IndexVar, Arr)) {
    const Expr *ArrReduced = Arr->IgnoreParenCasts();
    if (!containsExpr(Context, &ContainersIndexed, ArrReduced))
      ContainersIndexed.insert(ArrReduced);
    Usages.push_back(ASE);
    return true;
  }
  return VisitorBase::TraverseArraySubscriptExpr(ASE);
}

// If we encounter a reference to IndexVar in an unpruned branch of the
// traversal, mark this loop as unconvertible.
bool ForLoopIndexUseVisitor::VisitDeclRefExpr(DeclRefExpr *DRE) {
  const ValueDecl *TheDecl = DRE->getDecl();
  if (areSameVariable(IndexVar, TheDecl))
    OnlyUsedAsIndex = false;
  return true;
}

// If we find that another variable is created just to refer to the loop
// element, we can reuse it as the loop variable.
bool ForLoopIndexUseVisitor::VisitDeclStmt(DeclStmt *DS) {
  if (!AliasDecl && DS->isSingleDecl() &&
      isAliasDecl(DS->getSingleDecl(), IndexVar))
      AliasDecl = DS;
  return true;
}

// Apply the source transformations necessary to migrate the loop!
static void doConversion(ASTContext *Context, LoopFixerArgs *Args,
                         const StmtParentMap *ParentMap,
                         const VarDecl *IndexVar, const Expr *ContainerExpr,
                         const UsageResult &Usages, const DeclStmt *AliasDecl,
                         const ForStmt *TheLoop) {
  const VarDecl *MaybeContainer = getReferencedVariable(ContainerExpr);
  std::string VarName;

  if (Usages.size() == 1 && AliasDecl) {
    const VarDecl *AliasVar = cast<VarDecl>(AliasDecl->getSingleDecl());
    VarName = AliasVar->getName().str();
    // We keep along the entire DeclStmt to keep the correct range here.
    const SourceRange &ReplaceRange = AliasDecl->getSourceRange();
    if (!Args->CountOnly)
      Args->Replace->insert(
          Replacement(Context->getSourceManager(),
                      CharSourceRange::getTokenRange(ReplaceRange), ""));
    // No further replacements are made to the loop, since the iterator or index
    // was used exactly once - in the initialization of AliasVar.
  } else {
    VariableNamer Namer(Context, Args->GeneratedDecls, ParentMap,
                        IndexVar->getDeclContext(), TheLoop, IndexVar,
                        MaybeContainer);
    VarName = Namer.createIndexName();
    // First, replace all usages of the array subscript expression with our new
    // variable.
    for (UsageResult::const_iterator I = Usages.begin(), E = Usages.end();
         I != E; ++I) {
      SourceRange ReplaceRange = (*I)->getSourceRange();
      std::string ReplaceText = VarName;
      Args->ReplacedVarRanges->insert(std::make_pair(TheLoop, IndexVar));
      if (!Args->CountOnly)
        Args->Replace->insert(
            Replacement(Context->getSourceManager(),
                        CharSourceRange::getTokenRange(ReplaceRange),
                        ReplaceText));
    }
  }

  // Now, we need to construct the new range expresion.
  SourceRange ParenRange(TheLoop->getLParenLoc(), TheLoop->getRParenLoc());
  StringRef ContainerString =
      getStringFromRange(Context->getSourceManager(), Context->getLangOpts(),
                         ContainerExpr->getSourceRange());

  QualType AutoRefType =
      Context->getLValueReferenceType(Context->getAutoDeductType());
  std::string TypeString = AutoRefType.getAsString();

  std::string Range = ("(" + TypeString + " " + VarName + " : "
                           + ContainerString + ")").str();
  if (!Args->CountOnly)
    Args->Replace->insert(Replacement(Context->getSourceManager(),
                                     CharSourceRange::getTokenRange(ParenRange),
                                     Range));
  Args->GeneratedDecls->insert(make_pair(TheLoop, VarName));
}

// The LoopFixer callback, which determines if loops discovered by the
// matchers are convertible, printing information about the loops if so.
void LoopFixer::run(const MatchFinder::MatchResult &Result) {
  TranslationConfidenceKind ConfidenceLevel = TCK_Safe;
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

  ForLoopIndexUseVisitor Finder(Context, LoopVar);
  if (!Finder.findUsages(FS->getBody()))
    return;

  // If we already modified the range of this for loop, don't do any further
  // updates on this iteration.
  // FIXME: Once Replacements can detect conflicting edits, replace this
  // implementation and rely on conflicting edit detection instead.
  if (Args->ReplacedVarRanges->count(FS)) {
    Args->DeferredChanges++;
    return;
  }

  ConfidenceLevel = std::min(ConfidenceLevel, Finder.getConfidenceLevel());

  // We require that a single array be indexed into by LoopVar.
  const ContainerResult &ContainersIndexed = Finder.getContainersIndexed();
  if (ContainersIndexed.size() != 1)
    return;

  const Expr *ContainerExpr = *(ContainersIndexed.begin());
  if (!arrayMatchesBoundExpr(Context, ContainerExpr->getType(), BoundExpr))
    return;

  // Very few loops are over expressions that generate arrays rather than
  // array variables. Consider loops over arrays that aren't just represented
  // by a variable to be risky conversions.
  if (!getReferencedVariable(ContainerExpr) &&
      !isDirectMemberExpr(ContainerExpr))
    ConfidenceLevel = std::min(ConfidenceLevel, TCK_Risky);

  ParentFinder->gatherAncestors(Context->getTranslationUnitDecl(),
                                /*RunEvenIfNotEmpty=*/false);

  // Ensure that we do not try to move an expression dependent on a local
  // variable declared inside the loop outside of it!
  DependencyFinderASTVisitor
      DependencyFinder(&ParentFinder->getStmtToParentStmtMap(),
                       &ParentFinder->getDeclToParentStmtMap(),
                       Args->ReplacedVarRanges, FS);
  // Not all of these are actually deferred changes.
  // FIXME: Determine when the external dependency isn't an expression converted
  // by another loop.
  if (DependencyFinder.dependsOnOutsideVariable(ContainerExpr)) {
    Args->DeferredChanges++;
    return;
  }

  if (ConfidenceLevel < Args->ConfidenceLevel) {
    Args->RejectedChanges++;
    return;
  }

  doConversion(Context, Args, &ParentFinder->getStmtToParentStmtMap(),
               LoopVar, ContainerExpr, Finder.getUsages(),
               Finder.getAliasDecl(), FS);

  Args->AcceptedChanges++;
}

static StatementMatcher ArrayLHSMatcher =
  expression(ignoringImpCasts(declarationReference(to(
      variable(hasType(isInteger())).bind(ConditionVarName)))));
static StatementMatcher ArrayRHSMatcher =
  expression(hasType(isInteger())).bind(ConditionBoundName);

StatementMatcher LoopMatcher =
  id(LoopName, forStmt(
      hasLoopInit(declarationStatement(hasSingleDecl(id(InitVarName, variable(
          hasInitializer(ignoringImpCasts(integerLiteral(equals(0))))))))),
      hasCondition(binaryOperator(hasOperatorName("<"),
                                  hasLHS(ArrayLHSMatcher),
                                  hasRHS(ArrayRHSMatcher))),
      hasIncrement(unaryOperator(
          hasOperatorName("++"),
          hasUnaryOperand(declarationReference(to(
              variable(hasType(isInteger())).bind(IncrementVarName))))))));


} // namespace loop_migrate
} // namespace clang
