#include "LoopActions.h"
#include "LoopMatchers.h"
#include "VariableNaming.h"

#include "clang/Lex/Lexer.h"

namespace clang {
namespace loop_migrate {

using namespace clang::ast_matchers;
using namespace clang::tooling;

typedef llvm::SmallVector<
  std::pair<const Expr *, llvm::FoldingSetNodeID>, 1> ContainerResult;

/// Usage - the information needed to describe a valid convertible usage
/// of an array index or iterator.
struct Usage {
  const Expr *E;
  bool IsArrow;
  /// Range is only used if IsArrow is set to true;
  /// otherwise, E->getSourceRange() should be used.
  /// This is needed to work around the range of operator-> not being exactly
  /// what needs to be replaced.
  SourceRange Range;
};

// The true return value of ForLoopIndexUseVisitor.
typedef llvm::SmallVector<Usage, 8> UsageResult;

/// ForLoopIndexUseVisitor - discover usages of expressions indexing into
/// containers.
///
/// Given an index variable, recursively crawls a for loop to discover if the
/// index variable is used in a way consistent with range-based for loop access.
class ForLoopIndexUseVisitor
    : public RecursiveASTVisitor<ForLoopIndexUseVisitor> {
 public:
  ForLoopIndexUseVisitor(ASTContext *Context, const VarDecl *IndexVar,
                    const VarDecl *EndVar, const Expr *ContainerExpr) :
    OnlyUsedAsIndex(true), Context(Context), IndexVar(IndexVar),
    EndVar(EndVar), AliasDecl(NULL), ContainerExpr(ContainerExpr)  {
      if (ContainerExpr)
        addComponent(ContainerExpr);
    }

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

  /// Add a set of components that we should consider relevant to the container.
  void addComponents(const ComponentVector &Components) {
    // FIXME: add sort(on ID)+unique to avoid extra work.
    for (ComponentVector::const_iterator I = Components.begin(),
                                         E = Components.end(); I != E; ++I)
      addComponent(*I);
  }

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
  // The loop's 'end' variable, which cannot be mentioned at all.
  const VarDecl *EndVar;
  // The DeclStmt for an alias to the container element.
  const DeclStmt *AliasDecl;
  // The Expr which refers to the container.
  const Expr *ContainerExpr;
  TranslationConfidenceKind ConfidenceLevel;
  llvm::SmallVector<
      std::pair<const Expr *, llvm::FoldingSetNodeID>, 16> DependentExprs;

  void addComponent(const Expr *E) {
    llvm::FoldingSetNodeID ID;
    const Expr *Node = E->IgnoreParenImpCasts();
    Node->Profile(ID, *Context, true);
    DependentExprs.push_back(std::make_pair(Node, ID));
  }

  /// Typedef used in CRTP functions.
  typedef RecursiveASTVisitor<ForLoopIndexUseVisitor> VisitorBase;
  friend class RecursiveASTVisitor<ForLoopIndexUseVisitor>;

  /// Overriden methods for RecursiveASTVisitor's traversal.
  bool TraverseArraySubscriptExpr(ArraySubscriptExpr *ASE);
  bool TraverseCXXMemberCallExpr(CXXMemberCallExpr *MemberCall);
  bool TraverseCXXOperatorCallExpr(CXXOperatorCallExpr *OpCall);
  bool TraverseMemberExpr(MemberExpr *Member);
  bool TraverseUnaryOperator(UnaryOperator *Uop);
  bool VisitDeclRefExpr(DeclRefExpr *DRE);
  bool VisitDeclStmt(DeclStmt *DS);
  // Used to call Traverse...Operator() correctly
  bool TraverseStmt(Stmt *S);
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

// Determines if an expression is a declaration reference to a particular
// variable.
static bool exprIsVariable(const ValueDecl *Target, const Expr *E) {
  if (!Target || !E)
    return false;
  const DeclRefExpr *DRE = getDeclRef(E);
  return DRE && areSameVariable(Target, DRE->getDecl());
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

/// Look through conversion/copy constructors to see the object being converted
/// from, returning it if such an object is found. The point is to look at
///   vector<int>::iterator it = v.begin()
/// and retrieve `v.begin()` as the expression used to initialize `it`.
static const Expr *digThroughConstructors(const Expr *E) {
  if (!E)
    return NULL;
  E = E->IgnoreParenImpCasts();
  if (const CXXConstructExpr *ConstructExpr = dyn_cast<CXXConstructExpr>(E)) {
    // The initial constructor must take exactly one parameter, but base class
    // and deferred constructors can take more.
    if (ConstructExpr->getNumArgs() != 1 ||
        ConstructExpr->getConstructionKind() != CXXConstructExpr::CK_Complete)
      return NULL;
    E = ConstructExpr->getArg(0);
    if (const MaterializeTemporaryExpr *MTE =
        dyn_cast<MaterializeTemporaryExpr>(E))
      E = MTE->GetTemporaryExpr();
    return digThroughConstructors(E);
  }
  return E;
}

// Returns true when the ContainerResult contains an Expr equivalent to E.
template<typename ContainerT>
static bool containsExpr(ASTContext *Context, const ContainerT *Container,
                         const Expr *E) {
  llvm::FoldingSetNodeID ID;
  E->Profile(ID, *Context, true);
  for (typename ContainerT::const_iterator I = Container->begin(),
       End = Container->end(); I != End; ++I)
    if (ID == I->second)
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

// Returns true when Opcall is a call a one-parameter operator on IndexVar.
// Note that this function assumes that the opcode is operator* or operator->.
static bool isValidDereference(const CXXOperatorCallExpr *OpCall,
                               const VarDecl *IndexVar) {
  return OpCall->getNumArgs() == 1 &&
         exprIsVariable(IndexVar, OpCall->getArg(0));
}

// Returns true when Uop is a dereference of IndexVar. Note that this is the
// only isValidXXX function that confirms that the opcode is correct, as there
// is only one way to trigger this case (namely, the builtin operator*).
static bool isValidUop(const UnaryOperator *Uop, const VarDecl *IndexVar) {
  return Uop->getOpcode() == UO_Deref &&
      exprIsVariable(IndexVar, Uop->getSubExpr());
}

// Determines whether the given Decl defines an alias to the given variable.
static bool isAliasDecl(const Decl *TheDecl, const VarDecl *TargetDecl) {
  const VarDecl *VDecl = dyn_cast<VarDecl>(TheDecl);
  if (!VDecl)
    return false;

  if (!VDecl->hasInit())
    return false;
  const Expr *Init =
      digThroughConstructors(VDecl->getInit()->IgnoreImpCasts());

  switch (Init->getStmtClass()) {
  case Stmt::ArraySubscriptExprClass: {
    const ArraySubscriptExpr *ASE = cast<ArraySubscriptExpr>(Init);
    // We don't really care which array is used here. We check to make sure
    // it was the correct one later, since the AST will traverse it next.
    return isIndexInSubscriptExpr(ASE->getIdx(), TargetDecl, ASE->getBase());
  }

  case Stmt::UnaryOperatorClass:
    return isValidUop(cast<UnaryOperator>(Init), TargetDecl);

  case Stmt::CXXOperatorCallExprClass: {
      const CXXOperatorCallExpr *OpCall = cast<CXXOperatorCallExpr>(Init);
      if (OpCall->getOperator() == OO_Star)
        return isValidDereference(OpCall, TargetDecl);
      break;
  }

  default:
    break;
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

// A workaround to allow a redefinition of Traverse...Operator.
bool ForLoopIndexUseVisitor::TraverseStmt(Stmt *S) {
  // Without this, an assert is triggered somewhere in RecursiveASTVisitor.
  if (!S)
    return true;

  switch (S->getStmtClass()) {
  case Stmt::UnaryOperatorClass:
   return TraverseUnaryOperator(cast<UnaryOperator>(S));
  default:
    return VisitorBase::TraverseStmt(S);
  }
}

// Traverses the subexpression of Uop. Permitted usages here are dereferences of
// pointer types.
bool ForLoopIndexUseVisitor::TraverseUnaryOperator(UnaryOperator *Uop) {
  // If we dereference an iterator that's actually a pointer, count the
  // occurrence.
  if (isValidUop(Uop, IndexVar)) {
    Usage U = {Uop, /*IsArrow=*/false, SourceRange()};
    Usages.push_back(U);
    return true;
  }

  // Otherwise, continue recursively.
  return TraverseStmt(Uop->getSubExpr());
}

/// Arrow expressions are okay in iterator loops, so we note them
/// for conversion.
bool ForLoopIndexUseVisitor::TraverseMemberExpr(MemberExpr *Member) {
  const Expr *Base = Member->getBase();
  const DeclRefExpr *Obj = getDeclRef(Base);
  const Expr *ResultExpr = Member;
  QualType ExprType;
  if (const CXXOperatorCallExpr *Call =
      dyn_cast<CXXOperatorCallExpr>(Base->IgnoreParenImpCasts())) {
    // If operator->() is a MemberExpr containing a CXXOperatorCallExpr, then
    // the MemberExpr does not have the expression we want. We therefore catch
    // that instance here.
    if(Call->getOperator() == OO_Arrow) {
      assert(Call->getNumArgs() == 1 &&
             "Operator-> takes more than one argument");
      Obj = getDeclRef(Call->getArg(0));
      ResultExpr = Obj;
      ExprType = Call->getCallReturnType();
    }
  }

  if (Member->isArrow() && Obj && exprIsVariable(IndexVar, Obj)) {
    if (ExprType.isNull())
      ExprType = Obj->getType();

    assert(ExprType->isPointerType() && "Operator-> returned non-pointer type");
    // FIXME: This works around not having the location of the arrow operator.
    // Consider adding OperatorLoc to MemberExpr?
    SourceLocation ArrowLoc =
        Lexer::getLocForEndOfToken(Base->getExprLoc(), 0,
                                   Context->getSourceManager(),
                                   Context->getLangOpts());
    // If something complicated is happening (i.e. the next token isn't an
    // arrow), give up on making this work.
    if (!ArrowLoc.isInvalid()) {
      Usage U = {ResultExpr, /*IsArrow=*/true,
                 SourceRange(Base->getExprLoc(), ArrowLoc)};
      Usages.push_back(U);
      return true;
    }
  }
  return TraverseStmt(Member->getBase());
}

// Calls on the iterator object are not permitted, unless done through
// operator->().
bool ForLoopIndexUseVisitor::TraverseCXXMemberCallExpr(
    CXXMemberCallExpr *MemberCall) {
  MemberExpr *Member = cast<MemberExpr>(MemberCall->getCallee());

  if (containsExpr(Context, &DependentExprs, Member->getBase()))
    ConfidenceLevel = std::min(ConfidenceLevel, TCK_Risky);

  bool BaseResult = TraverseMemberExpr(Member);
  for (unsigned I = 0; I < MemberCall->getNumArgs(); ++I)
    BaseResult = BaseResult && TraverseStmt(MemberCall->getArg(I));
  return BaseResult;
}

// Overloaded operator* and operator-> are permitted for iterator-based loops.
bool ForLoopIndexUseVisitor::TraverseCXXOperatorCallExpr(
    CXXOperatorCallExpr *OpCall) {
  switch (OpCall->getOperator()) {
  case OO_Star:
    if (isValidDereference(OpCall, IndexVar)) {
      Usage U = {OpCall, /*IsArrow=*/false, SourceRange()};
      Usages.push_back(U);
      return true;
    }
    break;

  case OO_Arrow:
    if (isValidDereference(OpCall, IndexVar)) {
      Usage U = {OpCall, /*IsArrow=*/true,
                 SourceRange(OpCall->getLocStart(), OpCall->getOperatorLoc())};
      Usages.push_back(U);
      return true;
    }

  default:
    break;
  }
  return VisitorBase::TraverseCXXOperatorCallExpr(OpCall);
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
    if (!containsExpr(Context, &ContainersIndexed, ArrReduced)) {
      llvm::FoldingSetNodeID ID;
      ArrReduced->Profile(ID, *Context, true);
      ContainersIndexed.push_back(std::make_pair(ArrReduced, ID));
    }
    Usage U = {ASE, /*IsArrow=*/false, SourceRange()};
    Usages.push_back(U);
    return true;
  }
  return VisitorBase::TraverseArraySubscriptExpr(ASE);
}

// If we encounter a reference to IndexVar in an unpruned branch of the
// traversal, mark this loop as unconvertible.
bool ForLoopIndexUseVisitor::VisitDeclRefExpr(DeclRefExpr *DRE) {
  const ValueDecl *TheDecl = DRE->getDecl();
  if (areSameVariable(IndexVar, TheDecl) || areSameVariable(EndVar, TheDecl))
    OnlyUsedAsIndex = false;
  if (containsExpr(Context, &DependentExprs, DRE))
    ConfidenceLevel = std::min(ConfidenceLevel, TCK_Risky);
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
                         const VarDecl *IndexVar, const VarDecl *EndVar,
                         const Expr *ContainerExpr,
                         const UsageResult &Usages, const DeclStmt *AliasDecl,
                         const ForStmt *TheLoop,
                         bool ContainerNeedsDereference) {
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
      std::string ReplaceText;
      SourceRange ReplaceRange;
      if (I->IsArrow) {
        ReplaceRange = I->Range;
        ReplaceText = VarName + ".";
      } else {
        ReplaceRange = I->E->getSourceRange();
        ReplaceText = VarName;
      }
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

  std::string MaybeDereference = ContainerNeedsDereference ? "*" : "";
  std::string TypeString = AutoRefType.getAsString();
  std::string Range = ("(" + TypeString + " " + VarName + " : "
                           + MaybeDereference + ContainerString + ")").str();
  if (!Args->CountOnly)
    Args->Replace->insert(Replacement(Context->getSourceManager(),
                                     CharSourceRange::getTokenRange(ParenRange),
                                     Range));
  Args->GeneratedDecls->insert(make_pair(TheLoop, VarName));
}

/// Determine whether VDecl appears to be an initializing an iterator.
/// If it is, returns the object whose begin() or end()
/// method is called, and the output parameter isArrow is set to indicate
/// whether the initialization is called via . or ->.
static const Expr *getContainerFromInitializer(const Expr* Init,
                                               bool IsBegin, bool *IsArrow) {
  // FIXME: Maybe allow declaration/initialization outside of the for loop?
  const CXXMemberCallExpr *TheCall =
      dyn_cast_or_null<CXXMemberCallExpr>(digThroughConstructors(Init));
  if (!TheCall || TheCall->getNumArgs() != 0)
      return NULL;

  const MemberExpr *Member = cast<MemberExpr>(TheCall->getCallee());
  const std::string Name = Member->getMemberDecl()->getName();
  const std::string TargetName = IsBegin ? "begin" : "end";
  if (Name != TargetName)
    return NULL;

  const Expr *SourceExpr = Member->getBase();
  if (!SourceExpr)
    return NULL;

  *IsArrow = Member->isArrow();
  return SourceExpr;
}

/// Determines the variable whose begin() and end() functions are called for an
/// iterator-based loop.
static const Expr *findContainer(ASTContext *Context, const VarDecl *BeginVar,
                                 const VarDecl *EndVar, const Expr *EndExpr,
                                 bool *ContainerNeedsDereference) {
  const Expr *BeginInitExpr = BeginVar->getInit();
  const Expr *EndInitExpr = EndVar ? EndVar->getInit() : EndExpr;

  // Now that we know the loop variable and test expression, make sure they are
  // valid.
  bool BeginIsArrow = false;
  bool EndIsArrow = false;
  const Expr *ContainerExpr = getContainerFromInitializer(BeginInitExpr,
                                                          /*IsBegin=*/true,
                                                          &BeginIsArrow);
  if (!ContainerExpr)
      return NULL;
  const Expr *EndSourceExpr = getContainerFromInitializer(EndInitExpr,
                                                          /*IsBegin=*/false,
                                                          &EndIsArrow);
  // Disallow loops that try evil things like this (note the dot and arrow):
  //  for (IteratorType It = Obj.begin(), E = Obj->end(); It != E; ++It) { }
  if (!EndSourceExpr || BeginIsArrow != EndIsArrow ||
      !areSameExpr(Context, EndSourceExpr, ContainerExpr))
    return NULL;

  *ContainerNeedsDereference = BeginIsArrow;
  return ContainerExpr;
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
  const VarDecl *EndVar = Result.Nodes.getDeclAs<VarDecl>(EndVarName);
  const Expr *ContainerExpr = NULL;

  // If the end comparison isn't a variable, we can try to work with the
  // expression the loop variable is being tested against instead.
  const CXXMemberCallExpr *EndCall =
      Result.Nodes.getStmtAs<CXXMemberCallExpr>(EndCallName);

  // If the loop calls end()/size() after each iteration, lower our confidence
  // level.
  if (FixerKind != LFK_Array && !EndVar) {
    if (!EndCall)
      return;
    ConfidenceLevel = std::min(ConfidenceLevel, TCK_Extra);
  }

  bool ContainerNeedsDereference = false;
  // FIXME: Try to put most of this logic inside a matcher. Currently, matchers
  // don't allow the right-recursive checks in digThroughConstructors.
  if (FixerKind == LFK_Iterator)
    ContainerExpr = findContainer(Context, LoopVar, EndVar, EndCall,
                                  &ContainerNeedsDereference);

  ForLoopIndexUseVisitor Finder(Context, LoopVar, EndVar, ContainerExpr);

  // Either a container or an integral upper bound must exist.
  if (ContainerExpr) {
    ComponentFinderASTVisitor ComponentFinder;
    ComponentFinder.findExprComponents(ContainerExpr->IgnoreParenImpCasts());
    Finder.addComponents(ComponentFinder.getComponents());
  } else if (!BoundExpr)
    return;

  // Either a container or an integral upper bound must exist.
  if (!Finder.findUsages(FS->getBody()))
    return;

  ConfidenceLevel = std::min(ConfidenceLevel, Finder.getConfidenceLevel());
  // We require that a single array/container be indexed into by LoopVar.
  // This check is done by ForLoopIndexUseVisitor for non-array loops, but we may not
  // know which array is being looped over until the end of the traversal.
  if (FixerKind == LFK_Array) {
    const ContainerResult &ContainersIndexed = Finder.getContainersIndexed();
    if (ContainersIndexed.size() != 1)
      return;

    ContainerExpr = ContainersIndexed.begin()->first;
    if (!arrayMatchesBoundExpr(Context, ContainerExpr->getType(), BoundExpr))
      return;
    // Very few loops are over expressions that generate arrays rather than
    // array variables. Consider loops over arrays that aren't just represented
    // by a variable to be risky conversions.
    if (!getReferencedVariable(ContainerExpr) &&
        !isDirectMemberExpr(ContainerExpr))
      ConfidenceLevel = std::min(ConfidenceLevel, TCK_Risky);
  }

  // If we already modified the range of this for loop, don't do any further
  // updates on this iteration.
  // FIXME: Once Replacements can detect conflicting edits, replace this
  // implementation and rely on conflicting edit detection instead.
  if (Args->ReplacedVarRanges->count(FS)) {
    Args->DeferredChanges++;
    return;
  }

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
               LoopVar, EndVar, ContainerExpr, Finder.getUsages(),
               Finder.getAliasDecl(), FS, ContainerNeedsDereference);

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
