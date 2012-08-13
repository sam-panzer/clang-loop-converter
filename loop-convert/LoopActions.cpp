#include "LoopActions.h"
#include "LoopMatchers.h"
#include "VariableNaming.h"

#include "clang/Lex/Lexer.h"

#include<algorithm>

namespace clang {
namespace loop_migrate {

using namespace clang::ast_matchers;
using namespace clang::tooling;

enum DereferenceUsageKind {
  DUK_Normal,
  DUK_Arrow
};

/// Usage - the information needed to describe a valid convertible usage
/// of an array index or iterator.
struct Usage {
  const Expr *E;
  const QualType T;
  DereferenceUsageKind UsageKind;
  SourceRange Range;
  bool IsNonConst;
};

typedef llvm::SmallVector<
  std::pair<const Expr *, FoldingSetNodeID>, 1> ContainerResult;
typedef llvm::SmallVector<Usage, 8> UsageResult;

/// ForLoopASTVisitor - discover usages of expressions indexing into containers.
///
/// Given an index variable, recursively crawls a for loop to discover if the
/// index variable is used in a way consistent with range-based for loop access.
class ForLoopASTVisitor : public RecursiveASTVisitor<ForLoopASTVisitor> {
 public:
  ForLoopASTVisitor(ASTContext *Context, const VarDecl *TargetVar,
                    const Expr *ContainerExpr) :
    OnlyUsedAsIndex(true), Context(Context), TargetVar(TargetVar),
    AliasVar(NULL), ContainerExpr(ContainerExpr) {
      if (ContainerExpr)
        addComponent(ContainerExpr);
    }

  // Accessor for Usages
  const UsageResult &getUsages() const { return Usages; }

  // Accessor for ContainersIndexed
  const ContainerResult &getContainersIndexed() const {
    return ContainersIndexed;
  }

  /// Accessor for ConfidenceLevel.
  TranslationConfidenceKind getConfidenceLevel() const {
    return ConfidenceLevel;
  }

  /// Add a set of components that we should consider relevant to the container.
  void addComponents(const ComponentVector &Components) {
    // FIXME: add sort(on ID)+unique to avoid extra work.
    for (ComponentVector::const_iterator I = Components.begin(),
                                         E = Components.end(); I != E; ++I)
      addComponent(*I);
  }

  /// Returns the statement declaring the variable created as an alias for the
  /// loop element, if any.
  const DeclStmt *getAliasVar() const { return AliasVar; }

  /// Finds all uses of TargetVar in Body, placing all usages in Usages, all
  /// referenced arrays in ContainersIndexed, and returns true if TargetVar was
  /// only used as an array index.
  bool findUsages(const Stmt *Body) {
    IsRValue = false;
    ConfidenceLevel = TCK_Safe;
    TraverseStmt(const_cast<Stmt *>(Body));
    return OnlyUsedAsIndex;
  }

 private:
  bool OnlyUsedAsIndex;
  ASTContext *Context;
  // A container which holds all allowed usages of TargetVar.
  UsageResult Usages;
  // A set which holds expressions containing the referenced arrays.
  ContainerResult ContainersIndexed;
  // The index variable's VarDecl.
  const VarDecl *TargetVar;
  // The DeclStmt for an alias to the container element.
  const DeclStmt *AliasVar;
  // The Expr which refers to the container.
  const Expr *ContainerExpr;
  bool IsRValue;
  TranslationConfidenceKind ConfidenceLevel;
  llvm::SmallVector<
      std::pair<const Expr *, FoldingSetNodeID>, 16> DependentExprs;

  void addComponent(const Expr *E) {
    FoldingSetNodeID ID;
    const Expr *Node = E->IgnoreParenImpCasts();
    Node->Profile(ID, *Context, true);
    DependentExprs.push_back(std::make_pair(Node, ID));
  }

  // Typedef used in CRTP functions.
  typedef RecursiveASTVisitor<ForLoopASTVisitor> VisitorBase;
  friend class RecursiveASTVisitor<ForLoopASTVisitor>;

  /// Overriden methods for RecursiveASTVisitor's traversal.
  bool TraverseArraySubscriptExpr(ArraySubscriptExpr *ASE);
  bool TraverseCXXMemberCallExpr(CXXMemberCallExpr *MemberCall);
  bool TraverseCXXOperatorCallExpr(CXXOperatorCallExpr *OpCall);
  bool TraverseImplicitCastExpr(ImplicitCastExpr *ICE);
  bool TraverseMemberExpr(MemberExpr *Member);
  bool TraverseUnaryOperator(UnaryOperator *Uop);
  bool VisitDeclRefExpr(DeclRefExpr *DRE);
  bool VisitDeclStmt(DeclStmt *DS);
  // Used to call Traverse...Operator() correctly
  bool TraverseStmt(Stmt *S);
};

// FIXME: This was stolen from SemaFixItUtils, but probably belongs elsewhere.
// Determine if an expression has unnecessary outer parentheses.
bool CouldShedParens(const Expr *E) {
  if (!E)
    return false;
  const Expr *Inner = E->IgnoreParenImpCasts();
  return isa<ArraySubscriptExpr>(Inner) ||
          isa<CallExpr>(Inner) ||
          isa<DeclRefExpr>(Inner) ||
          isa<CastExpr>(Inner) ||
          isa<CXXNewExpr>(Inner) ||
          isa<CXXConstructExpr>(Inner) ||
          isa<CXXDeleteExpr>(Inner) ||
          isa<CXXNoexceptExpr>(Inner) ||
          isa<CXXPseudoDestructorExpr>(Inner) ||
          isa<CXXScalarValueInitExpr>(Inner) ||
          isa<CXXThisExpr>(Inner) ||
          isa<CXXTypeidExpr>(Inner) ||
          isa<CXXUnresolvedConstructExpr>(Inner) ||
          isa<ObjCMessageExpr>(Inner) ||
          isa<ObjCPropertyRefExpr>(Inner) ||
          isa<ObjCProtocolExpr>(Inner) ||
          isa<MemberExpr>(Inner) ||
          isa<ParenListExpr>(Inner) ||
          isa<SizeOfPackExpr>(Inner) ||
          isa<UnaryOperator>(Inner);
}

// Find the parent expression, if it was a cast or ParenExpr.
static const Expr *getParentCastExpr(const StmtMap *ParentMap, const Expr *E) {
  const Expr *Prev = E;
  do {
    Prev = E;
    E = dyn_cast<Expr>(ParentMap->lookup(E));
  } while (CouldShedParens(E) &&
           (isa<ImplicitCastExpr>(E) || isa<ParenExpr>(E)));
  return Prev;
}

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

// Returns true when two Exprs are the same.
static bool areSameExpr(ASTContext* Context, const Expr *First,
                        const Expr *Second) {
  if (!First || !Second)
    return false;
  // Common case shortcut: Expr's of different classes cannot be the same.
  if (First->getStmtClass() != Second->getStmtClass())
    return false;

  llvm::FoldingSetNodeID FirstID, SecondID;
  First->Profile(FirstID, *Context, true);
  Second->Profile(SecondID, *Context, true);
  return FirstID == SecondID;
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
// TargetVar and the array's base exists.
static bool isValidSubscriptExpr(const Expr *IndexExpr,
                                 const VarDecl *TargetVar,
                                 const Expr *Arr) {
  const DeclRefExpr *Idx = getDeclRef(IndexExpr);
  return Arr && Idx && Idx->getType()->isIntegerType()
             && areSameVariable(TargetVar, Idx->getDecl());
}

// Returns true when the index expression is a declaration reference to
// TargetVar, Obj is the same expression as SourceExpr after all parens and
// implicit casts are stirpped off.
static bool isValidSubscriptExpr(ASTContext *Context, const Expr *IndexExpr,
                                 const VarDecl *TargetVar, const Expr *Obj,
                                 const Expr *SourceExpr) {
  return SourceExpr && Obj
      && isValidSubscriptExpr(IndexExpr, TargetVar, Obj)
      && areSameExpr(Context, SourceExpr->IgnoreParenImpCasts(),
                     Obj->IgnoreParenImpCasts());
}

// Returns true when Opcall is a call a one-parameter operator on TargetVar.
// Note that this function assumes that the opcode is operator* or operator->.
static bool isValidDereference(const CXXOperatorCallExpr *OpCall,
                               const VarDecl *TargetVar) {
  return OpCall->getNumArgs() == 1 &&
         exprIsVariable(TargetVar, OpCall->getArg(0));
}

// Returns true when Uop is a dereference of TargetVar. Note that this is the
// only isValidXXX function that confirms that the opcode is correct, as there
// is only one way to trigger this case (namely, the builtin operator*).
static bool isValidUop(const UnaryOperator *Uop, const VarDecl *TargetVar) {
  return Uop->getOpcode() == UO_Deref &&
      exprIsVariable(TargetVar, Uop->getSubExpr());
}

// Determines whether the given Decl defines an alias to the given variable.
static bool isAliasDecl(const Decl *TheDecl, const VarDecl *TargetDecl) {
  if (const VarDecl *VDecl = dyn_cast<VarDecl>(TheDecl)) {
    if (!VDecl->hasInit())
      return false;
    const Expr *Init = VDecl->getInit()->IgnoreImpCasts();
    switch (Init->getStmtClass()) {
    case Stmt::ArraySubscriptExprClass: {
      const ArraySubscriptExpr *ASE = cast<ArraySubscriptExpr>(Init);
      // We don't really care which array is used here. We check to make sure
      // it was the correct one later.
      return isValidSubscriptExpr(ASE->getIdx(), TargetDecl, ASE->getBase());
      }

    case Stmt::UnaryOperatorClass:
      return isValidUop(cast<UnaryOperator>(Init), TargetDecl);

    case Stmt::CXXOperatorCallExprClass: {
      const CXXOperatorCallExpr *OpCall = cast<CXXOperatorCallExpr>(Init);
      if (OpCall->getOperator() == OO_Star)
        return isValidDereference(OpCall, TargetDecl);
      else if (OpCall->getOperator() == OO_Subscript &&
               OpCall->getNumArgs() == 2)
        // As with ArraySubscriptExprClasses, we don't really care which
        // object this is called on, since we confirm that it's exactly one
        // variable later on.
        return isValidSubscriptExpr(OpCall->getArg(1), TargetDecl,
                                    OpCall->getArg(0));
      break;
    }

    case Stmt::CXXMemberCallExprClass: {
      const CXXMemberCallExpr *MemberCall = cast<CXXMemberCallExpr>(Init);
      const MemberExpr *Member = cast<MemberExpr>(MemberCall->getCallee());
      // We specifically allow an accessor named "at" to let STL in.
      if (Member->getMemberDecl()->getName() == "at" &&
          MemberCall->getNumArgs() == 2)
        return isValidSubscriptExpr(MemberCall->getArg(1), TargetDecl,
                                    MemberCall->getArg(0));
      break;
    }
    default:
      break;
    }
  }
  return false;
}

// Determines whether the bound of a for loop condition expression matches
// ArrayType.
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
bool ForLoopASTVisitor::TraverseStmt(Stmt *S) {
  if (!S)
    return true;
  switch (S->getStmtClass()) {
  case Stmt::UnaryOperatorClass:
   return TraverseUnaryOperator(cast<UnaryOperator>(S));
  default:
    return VisitorBase::TraverseStmt(S);
  }
}

// When we encounter an LValueToRValue implicit cast, immediate sub-nodes are
// considered used as an rvalue.
bool ForLoopASTVisitor::TraverseImplicitCastExpr(ImplicitCastExpr *ICE) {
  IsRValue = (ICE->getCastKind() == CK_LValueToRValue);
  return VisitorBase::TraverseImplicitCastExpr(ICE);
}

// Traverses the subexpression of Uop, noting when the operator would change
// the value of its operand. We include taking the address of the operand as
// such an operator to be unsafe, though a more sophisticated analysis (e.g.
// checking to see that this address is only used as a pointer-to-const)
// could be less conservative.
// FIXME: Do this more sophisticated analysis.
bool ForLoopASTVisitor::TraverseUnaryOperator(UnaryOperator *Uop) {
  // If we dereference an iterator that's actually a pointer, count the
  // occurrence.
  if (isValidUop(Uop, TargetVar)) {
    Usage U = {Uop, Uop->getType(), DUK_Normal, SourceRange(), IsRValue};
    Usages.push_back(U);
    return true;
  }

  // Otherwise, continue recursively.
  IsRValue = false;
  return TraverseStmt(Uop->getSubExpr());
}

// If we encounter an array with TargetVar as the index, note it and prune the
// AST traversal. Otherwise, continue the traversal recursively.
bool ForLoopASTVisitor::TraverseArraySubscriptExpr(ArraySubscriptExpr *ASE) {
  Expr *Arr = ASE->getBase();
  if (isValidSubscriptExpr(ASE->getIdx(), TargetVar, Arr)) {
    const Expr *ArrReduced = Arr->IgnoreParenCasts();
    if (!containsExpr(Context, &ContainersIndexed, ArrReduced)) {
      llvm::FoldingSetNodeID ID;
      ArrReduced->Profile(ID, *Context, true);
      ContainersIndexed.push_back(std::make_pair(ArrReduced, ID));
    }
    Usage U = {ASE, QualType(), DUK_Normal, SourceRange(), IsRValue};
    Usages.push_back(U);
    return true;
  }
  IsRValue = false;
  return TraverseStmt(Arr) && TraverseStmt(ASE->getIdx());
}

// If we encounter a reference to TargetVar in an unpruned branch of the
// traversal, mark this loop as unconvertible.
bool ForLoopASTVisitor::VisitDeclRefExpr(DeclRefExpr *DRE) {
  const ValueDecl *TheDecl = DRE->getDecl();
  if (areSameVariable(TargetVar, TheDecl))
    OnlyUsedAsIndex = false;
  if (containsExpr(Context, &DependentExprs, DRE) && !IsRValue)
    ConfidenceLevel = std::min(ConfidenceLevel, TCK_Risky);
  return true;
}

// If we find that another variable is created just to refer to the loop
bool ForLoopASTVisitor::VisitDeclStmt(DeclStmt *DS) {
  if (!AliasVar && DS->isSingleDecl() &&
      isAliasDecl(DS->getSingleDecl(), TargetVar))
      AliasVar = DS;
  return true;
}

/// Arrow expressions are okay in iterator loops, so we note them
/// for conversion.
bool ForLoopASTVisitor::TraverseMemberExpr(MemberExpr *Member) {
  const Expr *Base = Member->getBase();
  const DeclRefExpr *Obj = getDeclRef(Base);
  const Expr *ResultExpr = Member;
  QualType ExprType;
  if (const CXXOperatorCallExpr *Call =
      dyn_cast<CXXOperatorCallExpr>(Base->IgnoreParenImpCasts())) {
    // If operator-> is a MemberExpr containing a CXXOperatorCallExpr, then
    // the MemberExpr does not have the type we want. We therefore catch
    // that instance here.
    if(Call->getNumArgs() == 1 && Call->getOperator() == OO_Arrow) {
      Obj = getDeclRef(Call->getArg(0));
      ResultExpr = Obj;
      ExprType = Call->getCallReturnType();
    }
  }

  if (Member->isArrow() && Obj && exprIsVariable(TargetVar, Obj)) {
    if (ExprType.isNull())
      ExprType = Obj->getType();
    // Bail out if operator-> or operator* doesn't return a pointer.
    if (!ExprType->isPointerType())
      return true;
    const QualType T = ExprType->getAs<PointerType>()->getPointeeType();
    // FIXME: This works around not having the location of the arrow operator.
    // Consider adding OperatorLoc to MemberExpr?
    SourceLocation ArrowLoc =
        Lexer::getLocForEndOfToken(Base->getExprLoc(), 0,
                                   Context->getSourceManager(),
                                   Context->getLangOpts());
    // If something complicated is happening (i.e. the next token isn't an
    // arrow), give up on making this work.
    if (!ArrowLoc.isInvalid()) {
      Usage U = {ResultExpr, T, DUK_Arrow,
                 SourceRange(Base->getExprLoc(), ArrowLoc), IsRValue};
      Usages.push_back(U);
      return true;
    }
  }
  // MemberExpr's don't change the L- or R-value status.
  // FIXME: do they?
  return TraverseStmt(Member->getBase());
}

// Calls on the iterator object are not permitted, unless done through
// operator->. The one exception is allowing vector::at() for pseudoarrays.
// We also treat the base object as being an rvalue if the member function is
// const.
bool ForLoopASTVisitor::TraverseCXXMemberCallExpr(
    CXXMemberCallExpr *MemberCall) {
  MemberExpr *Member = cast<MemberExpr>(MemberCall->getCallee());
  // We specifically allow an accessor named "at" to let STL in, though
  // this is restricted to pseudo-arrays by requiring a single, integer
  // argument.
  if (Member->getMemberDecl()->getName() == "at" &&
      MemberCall->getNumArgs() == 1) {
    if (isValidSubscriptExpr(Context, MemberCall->getArg(0), TargetVar,
                             Member->getBase(), ContainerExpr)) {
      Usage U = {MemberCall, MemberCall->getCallReturnType(),
                 DUK_Normal, SourceRange(), IsRValue};
      Usages.push_back(U);
      return true;
    }
  }

  IsRValue = MemberCall->getMethodDecl()->isConst();
  if (containsExpr(Context, &DependentExprs, Member->getBase()) && !IsRValue)
    ConfidenceLevel = std::min(ConfidenceLevel, TCK_Risky);

  bool BaseResult = TraverseMemberExpr(Member);
  IsRValue = false;
  for (unsigned I = 0; I < MemberCall->getNumArgs(); ++I)
    BaseResult = BaseResult && TraverseStmt(MemberCall->getArg(I));
  return BaseResult;
}

// Overloaded operator* and operator-> are permitted for iterator-based loops,
// and operator[] is allowed for pseudo-array loops.
bool ForLoopASTVisitor::TraverseCXXOperatorCallExpr(
    CXXOperatorCallExpr *OpCall) {
  const QualType &T = OpCall->getCallReturnType();
  switch (OpCall->getOperator()) {
  case OO_Star:
    if (isValidDereference(OpCall, TargetVar)) {
      Usage U = {OpCall, T, DUK_Normal, SourceRange(), IsRValue};
      Usages.push_back(U);
      return true;
    }
    break;

  case OO_Arrow:
    if (isValidDereference(OpCall, TargetVar)) {
      Usage U = {OpCall, T, DUK_Arrow,
                 SourceRange(OpCall->getLocStart(), OpCall->getOperatorLoc()),
                 IsRValue};
      Usages.push_back(U);
      return true;
    }
  case OO_Subscript:
    if (OpCall->getNumArgs() != 2)
      break;
    if (isValidSubscriptExpr(Context, OpCall->getArg(1), TargetVar,
                             OpCall->getArg(0), ContainerExpr)) {
      Usage U = {OpCall, OpCall->getCallReturnType(),
                 DUK_Normal, SourceRange(), IsRValue};
      Usages.push_back(U);
      return true;
    }
    break;

  default:
    break;
  }
  IsRValue = false;
  return VisitorBase::TraverseCXXOperatorCallExpr(OpCall);
}

// Given a type and some printing properties, generate the string that should
// be printed out.
// FIXME: Find a good place to put the policy generation. It's a fast operation,
// but only needs to be done once.
static std::string getTypeString(ASTContext *Context, QualType ElementType,
                                 bool PreferAuto, bool InsertConst) {
  PrintingPolicy Policy(Context->getLangOpts());
  Policy.Bool = true;
  Policy.Dump = false;
  Policy.SuppressUnwrittenScope = true;
  bool IsRecordType = false;
  bool WasConst = false;
  bool WasReference = false;
  if (ElementType.isNull())
    ElementType = Context->getAutoDeductType();
  else {
    IsRecordType = ElementType->isRecordType();
    WasConst = ElementType.isConstQualified();
    WasReference = ElementType->isReferenceType();
    // Unfortunately, adding const and adding a reference are not commutative
    // operations. QualType::getAsString() gives diffrent output for each,
    // and we can end up with "const auto & const" as the type string.
    // We therefore remove the reference type here and add it back at the end.
    // FIXME: Fix QualType's printing logic rather than working around it here.
    ElementType = ElementType.getNonReferenceType();
    if (PreferAuto || ElementType->isCompoundType())
      ElementType = Context->getAutoDeductType();
  }

  if (InsertConst || WasConst)
    ElementType.addConst();
  ElementType = Context->getLValueReferenceType(ElementType);
  return ElementType.getAsString(Policy);
}

// Apply the source transformations necessary to migrate the loop!
static void doConversion(ASTContext *Context, LoopFixerArgs *Args,
                         const StmtMap *ParentMap,
                         const VarDecl *IndexVar, const VarDecl *EndVar,
                         const Expr *ContainerExpr,
                         const UsageResult &Usages, const DeclStmt *AliasVar,
                         const ForStmt *TheLoop, bool IteratorNeedsDereference,
                         bool IsArray) {
  // MaybeContainer points to the container variable if there is one, or NULL.
  const VarDecl *MaybeContainer = getReferencedVariable(ContainerExpr);
  std::string VarName;
  bool HaveConvincingType = false;
  QualType ElementType;
  bool CanBeConst = true;

  if (Usages.size() == 1 && AliasVar) {
    const VarDecl *AliasDecl = cast<VarDecl>(AliasVar->getSingleDecl());
    VarName = AliasDecl->getName().str();
    const SourceRange &ReplaceRange = AliasVar->getSourceRange();
    if (!Args->CountOnly)
      Args->Replace->insert(
          Replacement(Context->getSourceManager(),
                      CharSourceRange::getTokenRange(ReplaceRange), ""));
    // No further replacements are made to the loop, since the iterator or index
    // was used exactly once - in the initialization of AliasDecl.
    ElementType = AliasDecl->getType();
    HaveConvincingType = true;
    CanBeConst = ElementType.isConstQualified();
  } else {
    VariableNamer Namer(Context, Args->GeneratedDecls, ParentMap,
                        IndexVar->getDeclContext(), TheLoop, IndexVar,
                        MaybeContainer);
    VarName = Namer.createIndexName();
    // First, replace all usages of the element with our new variable.
    for (UsageResult::const_iterator I = Usages.begin(), E = Usages.end();
         I != E; ++I) {
      SourceRange ReplaceRange;
      std::string ReplaceText = VarName;
      if (ElementType.isNull())
        ElementType = I->T;
      if (I->UsageKind == DUK_Arrow) {
        ReplaceRange = I->Range;
        ReplaceText = VarName + ".";
      } else {
        // If we can remove parentheses, replace them too.
        const Expr *E = getParentCastExpr(ParentMap, I->E);
        ReplaceRange = E->getSourceRange();
        ReplaceText = VarName;
      }
      CanBeConst = CanBeConst && I->IsNonConst;

      Args->ReplacedVarRanges->insert(std::make_pair(TheLoop, IndexVar));
      if (!Args->CountOnly)
        Args->Replace->insert(
            Replacement(Context->getSourceManager(),
                        CharSourceRange::getTokenRange(ReplaceRange),
                        ReplaceText));
    }
  }

  // Now, we need to construct the new range expresion.
  // We know the type of both arays and iterators that are actually pointers.
  SourceRange ParenRange(TheLoop->getLParenLoc(), TheLoop->getRParenLoc());
  if (IsArray) {
    const Type *T = ContainerExpr->getType().getTypePtr();
    if (const ArrayType *AT = T->getAsArrayTypeUnsafe()) {
      ElementType = AT->getElementType();
      HaveConvincingType = true;
    }
  } else if (!HaveConvincingType) {
    const QualType &SourceType = IndexVar->getType();
    if (SourceType->isPointerType()) {
      ElementType = SourceType->getAs<PointerType>()->getPointeeType();
      HaveConvincingType = true;
    }
  }

  StringRef ContainerString =
      getStringFromRange(Context->getSourceManager(), Context->getLangOpts(),
                         ContainerExpr->getSourceRange());

  std::string MaybeDereference = IteratorNeedsDereference ? "*" : "";
  std::string TypeString = getTypeString(Context, ElementType,
                                    Args->PreferAuto || !HaveConvincingType,
                                    Args->InsertConst && CanBeConst);

  std::string Range = ("(" + TypeString + " " + VarName + " : " +
                       MaybeDereference + ContainerString + ")").str();
  if (!Args->CountOnly)
    Args->Replace->insert(Replacement(Context->getSourceManager(),
                                     CharSourceRange::getTokenRange(ParenRange),
                                     Range));
  Args->GeneratedDecls->insert(std::make_pair(TheLoop, VarName));
  Args->AcceptedChanges++;
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
                                 bool *IteratorNeedsDereference) {
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

  *IteratorNeedsDereference = BeginIsArrow;
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

  const VarDecl *EndVar = Result.Nodes.getDeclAs<VarDecl>(EndVarName);
  const Expr *BoundExpr= Result.Nodes.getStmtAs<Expr>(ConditionBoundName);
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

  bool IteratorNeedsDereference = false;
  // FIXME: Try to put most of this logic inside a matcher. Currently, matchers
  // don't allow the right-recursive checks in digThroughConstructors.
  if (FixerKind == LFK_Iterator)
    ContainerExpr = findContainer(Context, LoopVar, EndVar, EndCall,
                                  &IteratorNeedsDereference);
  else if (FixerKind == LFK_PseudoArray) {
    ContainerExpr = EndCall->getImplicitObjectArgument();
    IteratorNeedsDereference =
        cast<MemberExpr>(EndCall->getCallee())->isArrow();
  }

  /// Start to take action here
  ForLoopASTVisitor Finder(Context, LoopVar, ContainerExpr);

  // Either a container or an integral upper bound must exist.
  if (ContainerExpr) {
    ComponentFinderASTVisitor ComponentFinder;
    ComponentFinder.findExprComponents(ContainerExpr->IgnoreParenImpCasts());
    Finder.addComponents(ComponentFinder.getComponents());
  } else if (!BoundExpr)
    return;

  if (!Finder.findUsages(FS->getBody()))
    return;
  ConfidenceLevel = std::min(ConfidenceLevel, Finder.getConfidenceLevel());

  // We require that a single array/container be indexed into by LoopVar.
  // This check is done by ForLoopASTVisitor for non-array loops, but we may not
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
    if (!getReferencedVariable(ContainerExpr))
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
  DependencyFinderASTVisitor DependencyFinder(&ParentFinder->getStmtMap(),
                                              &ParentFinder->getDeclMap(),
                                              Args->ReplacedVarRanges,
                                              FS);
  // Not all of thsee are actually deferred changes.
  // FIXME: Determine when the external dependency isn't an expression converted
  // by another loop.
  if (DependencyFinder.isExternallyDependent(ContainerExpr)) {
    Args->DeferredChanges++;
    return;
  }

  if (ConfidenceLevel < Args->ConfidenceLevel) {
    Args->RejectedChanges++;
    return;
  }

  doConversion(Context, Args, &ParentFinder->getStmtMap(),
               LoopVar, EndVar, ContainerExpr, Finder.getUsages(),
               Finder.getAliasVar(), FS, IteratorNeedsDereference,
               FixerKind == LFK_Array);
}


} // namespace loop_migrate
} // namespace clang
