// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "AalCompiler.h"
#include "AalObject.h"
#include "Expressions.h"
#include <acdk/lang/System.h>
#include <acdk/lang/reflect/Modifier.h>


namespace acdk {
namespace aal {


using namespace acdk::aci;
using namespace acdk::lang::dmi;
//using acdk::lang::dmi::ClazzInfo;
using ::acdk::lang::reflect::Modifier;

void 
AdditiveExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RCode lhex = _subNodes[0];
  RCode rhex = _subNodes[2];
  RString op = _subNodes[1]->getCodeString();

  RDClazzInfo lhtype = lhex->getExpressionSemanticElement()->getType();
  RDClazzInfo rhtype = rhex->getExpressionSemanticElement()->getType();
  if (lhtype->isNumber() == true && rhtype->isNumber() == true)
  {
    setSemanticElement(&DClazzInfo::selectResultingAlgExpression(lhtype->getImplClazzInfo(), rhtype->getImplClazzInfo()));
    return;
  }
  createObjectCall(comp, Operator::operatorToFuncName(op), lhex, rhex);
}

void 
AdditiveExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  
  if (_subNodes->length() != 3)
    THROW2(CodeException, "operator + or - needs 2 arguments", this);
  RCode lhex = _subNodes[0];
  RCode rhex = _subNodes[2];
  RDClazzInfo lhtype = lhex->getExpressionSemanticElement()->getType();
  RDClazzInfo rhtype = rhex->getExpressionSemanticElement()->getType();
  RString op = _subNodes[1]->getCodeString();
  Code::emitOpCode(comp, oca);
  if (op->equals("+") == true)
    comp->addOpCode(oca, new OpCodeStm(OCO_ADD, this, Nil));
  else if (op->equals("-") == true)
    comp->addOpCode(oca, new OpCodeStm(OCO_SUB, this, Nil));
  else
    THROW2(CodeException, "Unknown additive operator: " + op, this);
}



void 
MultiplicativeExpr::postParse(IN(RCompiler) comp)
{
   Code::postParse(comp);
  RCode lhex = _subNodes[0];
  RCode rhex = _subNodes[2];
  RString op = _subNodes[1]->getCodeString();

  RDClazzInfo lhtype = lhex->getExpressionSemanticElement()->getType();
  RDClazzInfo rhtype = rhex->getExpressionSemanticElement()->getType();

  if (lhtype->isNumber() == true && rhtype->isNumber() == true)
  {
    if (rhtype->isIntegerNumber() == false || lhtype->isIntegerNumber() == false)
      THROW2(CodeException, "module arguments must be in integer type: " + lhtype->getName() + " % " + rhtype->getName(), this);
    setSemanticElement(&DClazzInfo::selectResultingAlgExpression(lhtype->getImplClazzInfo(), rhtype->getImplClazzInfo()));
    setExpressionSemanticElement(&lhtype);
    return;
  }
  createObjectCall(comp, Operator::operatorToFuncName(op), lhex, rhex);
}

void 
MultiplicativeExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode lhex = _subNodes[0];
  RCode rhex = _subNodes[2];
  RDClazzInfo lhtype = lhex->getExpressionSemanticElement()->getType();
  RDClazzInfo rhtype = rhex->getExpressionSemanticElement()->getType();
  RString op = _subNodes[1]->getCodeString();
  Code::emitOpCode(comp, oca);
  if (op->equals("*") == true)
    comp->addOpCode(oca, new OpCodeStm(OCO_MUL, this, Nil));
  else if (op->equals("/") == true)
    comp->addOpCode(oca, new OpCodeStm(OCO_DIV, this, Nil));
  else if (op->equals("%") == true)
    comp->addOpCode(oca, new OpCodeStm(OCO_MOD, this, Nil));
  else
    THROW2(CodeException, "Unknown multiplicative operator: " + op, this);
}

void 
AssignmentExpr::postParse(IN(RCompiler) comp)
{
  RCode lhc = _subNodes[0];
  RCode assop = _subNodes[1];
  RCode rhc = _subNodes[2];

  lhc->_flags |= IsLVarExpr;
  rhc->postParse(comp);
  lhc->postParse(comp);
  RSemanticElem rhexsem = rhc->getExpressionSemanticElement();
  RSemanticElem rhsem = rhc->getSemanticElement();
  RSemanticElem lhsem = lhc->getSemanticElement();
  RSemanticElem lhexsem = lhc->getExpressionSemanticElement();
  if (instanceof(lhsem, DClazzFieldInfo) == true &&
      RDClazzFieldInfo(lhsem)->getOwnerClazz()->flags & MiIvWeakBind)
  {
    setSemanticElement(rhsem); 
    setExpressionSemanticElement(rhsem); 
  }
  else if (rhexsem != Nil && instanceof(rhexsem, DClazzMethodInfo) == true)
  {
    // ## TODO check method assignment compatibility
    RDClazzInfo lhcls = lhsem->getType();
    RDClazzMethodInfo lhmeth = lhcls->getDefunMethod(0);
    if (lhmeth == Nil)
      THROW2(CodeException, "righthand Class is not a callable object: " + lhcls->getName(), this);
    RDClazzMethodInfo metinforg(rhexsem);
    RDClazzMethodInfo metinf = metinforg->getOwnerClass()->findCompatibleMethod(metinforg->getName(), lhmeth);
    if (metinf == Nil)
      THROW2(CodeException, "righthand method " + metinforg->toString() + " is not assignable to " + lhmeth->toString(), this);

    int tcounter = comp->getCounter();
    
    StringBuffer sb;
    RString ownerclassname = metinf->getOwnerClass()->getName();
    RString wrapperclassname = ownerclassname + "_method_caller" + tcounter;
    sb << "class " << wrapperclassname << "\n"
       << " implements " << lhsem->getType()->getName() << "\n{\n"
       << "  private " <<  ownerclassname << " _calltarget;\n"
       << "  public " << wrapperclassname << "(" << ownerclassname << " calltarget) : _calltarget(calltarget) {}\n"
       << "  ";
    RDClazzMethodInfo opmeth = (RDClazzMethodInfo)metinf->clone();
    opmeth->setName("operator_po_pc");
    opmeth->getMethodInfo()->toTypeString(sb, 0, acdk::lang::dmi::TpFtJavaType | acdk::lang::dmi::TpFtFqName | acdk::lang::dmi::TpFtTypeDef);
    sb << "{ ";
    if (metinf->getReturnType()->isVoid() == false)
      sb << "return ";
    sb << "_calltarget." << metinf->getName() << "(";
    acdk::lang::dmi::ClazzMethodArgInfo** args = metinf->getMethodInfo()->methodArgs;
    if (args != 0)
    {
      for (int i = 0; args[i] != 0; ++i)
      {
        if (i > 0)
          sb << ", ";
        sb << (char*)args[i]->name;
      }
    }
    sb << "); }\n"
       << "}\n"
       ;

    RString text = sb.toString();
    System::out->println("Subtext: [ " + text + "\n]\n");
    RCompiler subcomp = comp->createSubTextParser(text, Nil);
    RCode impldef = subcomp->parseComplete("CodeText");
    impldef->_parent = _parent;
    impldef->postParse(comp);
    /*
class AClass_method_caller
      implements StaticStringAdder
{
  AClass target;
  AClass_method_caller(AClass t) : target(t) {}
  public String operator(String s) { return target.adder(s); }
}
staticcallable = new AClass_method_caller(acls);
  
*/  
  }
  //Code::postParse(comp);
  setSemanticElement(lhc->getSemanticElement()); 
  setExpressionSemanticElement(lhc->getExpressionSemanticElement()); 
}



void 
AssignmentExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode lh = _subNodes[0];
  RCode op = _subNodes[1];
  RCode rh = _subNodes[2];
  rh->emitOpCode(comp, oca);
  lh->emitOpCode(comp, oca);
  RSemanticElem lhsem = lh->getSemanticElement();
  if (instanceof(lhsem, DClazzFieldInfo) == true &&
      RDClazzFieldInfo(lhsem)->getOwnerClazz()->flags & MiIvWeakBind)
  {
    // ### TODO implement MiIvWeakBind field access
  }
  if (instanceof(lhsem, DClazzFieldInfo) == false) // use OCO_POKE[_STATIC]
  {
    RDClazzInfo lhci = lh->getSemanticElement()->getType();
    comp->addOpCode(oca, new OpCodeStm(OCO_ASSIGN, this, Nil));
  }

}

RSemanticElem 
LogicalExpr::getExpressionSemanticElement()
{
  return (RSemanticElem)DClazzInfo::getInstance(acdk::lang::dmi::ClazzInfo::getBoolClazz());
}

void 
LogicalExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RCode lex = _subNodes[0];
  RString op = _subNodes[1]->getCodeString();
  RCode rex = _subNodes[2];
  if (lex->getExpressionSemanticElement()->getType()->isBoolean() == true)
  {
    if (rex->getExpressionSemanticElement()->getType()->isBoolean() == false)
      THROW2(CodeException, "Cannot use boolean operation with lefthand bool with righthand " 
                            + rex->getExpressionSemanticElement()->getType()->getName(), this);
    _sem = &DClazzInfo::getInstance(Boolean::getTYPE());
  }
  else
  {
    createObjectCall(comp, Operator::operatorToFuncName(op), lex,  rex);
  }
}

void 
LogicalExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode lex = _subNodes[0];
  RCode rex = _subNodes[2];
  RCode operatorcode = _subNodes[1];
  //RString op = operatorcode->getCodeString();
  if (rex->getExpressionSemanticElement()->getType()->isBoolean() == false ||
      lex->getExpressionSemanticElement()->getType()->isBoolean() == false)
    THROW2(CodeException, "Can only use expression in logical expressions" , this);
  int c = comp->getCounter();
  OpCodeOp branchop = OCO_NOP;
  RString codename = getCodeName();
  if (codename->equals("LogicalANDExpr") == true)
    branchop = OCO_BRFALSE;
  else if (codename->equals("LogicalORExpr") == true)
    branchop = OCO_BRTRUE;
  else
    THROW2(CodeException, "Unknown logical expression: " + codename, this);

  lex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_DUP, this, "safe lh expr result"));
  comp->addOpCode(oca, new BranchOp(branchop, this, "bool short-circuit", RString("logexprend") + c));
  comp->addOpCode(oca, new OpCodeStm(OCO_POP, this, "remove lh expr result"));
  rex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", RString("logexprend") + c));
  
}

void 
RelationalExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RCode lex = _subNodes[0];
  RString op = _subNodes[1]->getCodeString();
  RCode rex = _subNodes[2];
  if (lex->getExpressionSemanticElement()->getType()->isNumber() == true)
  {
    if (rex->getExpressionSemanticElement()->getType()->isNumber() == false)
      THROW2(CodeException, "Cannot compare lefthand number with righthand " 
                            + rex->getExpressionSemanticElement()->getType()->getName(), this);
    _sem = &DClazzInfo::getInstance(Boolean::getTYPE());
  }
  else
  {
    createObjectCall(comp, Operator::operatorToFuncName(op), lex,  rex);
  }
  
}

void 
RelationalExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RString op = _subNodes[1]->getCodeString();
  _subNodes[0]->emitOpCode(comp, oca);
  _subNodes[2]->emitOpCode(comp, oca);
  OpCodeOp opc = OCO_NOP;
  if (op->equals("<") == true)
    opc = OCO_LT;
  else if (op->equals(">") == true)
    opc = OCO_GT;
  else if (op->equals(">=") == true)
    opc = OCO_GTEQ;
  else if (op->equals("<=") == true)
    opc = OCO_LTEQ;
  else
    THROW2(CodeException, "Unknown relational operator: " + op, this);

  comp->addOpCode(oca, new OpCodeStm(opc, this, Nil));
}

void 
EqualityExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  
  RCode lex = _subNodes[0];
  RString op = _subNodes[1]->getCodeString();
  RCode rex = _subNodes[2];
  RDClazzInfo ltype = lex->getExpressionSemanticElement()->getType();
  RDClazzInfo rtype = rex->getExpressionSemanticElement()->getType();
  if (ltype->isNumber() == true)
  {
    if (rtype->isNumber() == false && ltype->isAny() == false)
      THROW2(CodeException, "Cannot compare equality lefthand number with righthand " 
                            + rtype->getName(), this);
    _sem = &DClazzInfo::getInstance(Boolean::getTYPE());
  } 
  else if (ltype->isBoolean() == true)
  {
    if (rtype->isBoolean() == false && ltype->isAny() == false)
      THROW2(CodeException, "Cannot compare equality lefthand boolean with righthand " 
                            + rtype->getName(), this);
    _sem = &DClazzInfo::getInstance(Boolean::getTYPE());
  }
  else if (ltype->isAny() == false)
  {
    if (rtype->isNumber() == true || rtype->isBoolean() == true)
      THROW2(CodeException, "Cannot compare equality lefthand object reference with righthand " 
                            + rtype->getName(), this);
    // ### check if object are same instance
    _sem = &DClazzInfo::getInstance(Boolean::getTYPE());
    //createObjectCall(comp, Operator::operatorToFuncName(op), lex,  rex);
  }
}

void 
EqualityExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RString op = _subNodes[1]->getCodeString();
  _subNodes[0]->emitOpCode(comp, oca);
  _subNodes[2]->emitOpCode(comp, oca);
  OpCodeOp opc = OCO_NOP;
  if (op->equals("==") == true)
    opc = OCO_EQ;
  else if (op->equals("!=") == true)
    opc = OCO_NE;
  else
    THROW2(CodeException, "Unknown Equality operator: " + op, this);

  comp->addOpCode(oca, new OpCodeStm(opc, this, Nil));
}

void 
EqualsExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RCode lex = _subNodes[0];
  RCode opcode = _subNodes[1];
  RString op = opcode->getCodeString();
  RCode rex = _subNodes[2];
  RDClazzInfo ltype = lex->getExpressionSemanticElement()->getType();
  RDClazzInfo rtype = rex->getExpressionSemanticElement()->getType();
  if (ltype->isNumber() == true || ltype->isBoolean() == true)
  {
    RCode newop;
    if (op->equals("===") == true)
      newop = Keyword::createCode(comp, "==");
    else
      newop = Keyword::createCode(comp, "!=");
    opcode->replaceThisWith(newop);
    // ## TODO replace this node with EqualityExpr
    EqualityExpr::postParse(comp);
    return;
  } 
  Code::postParse(comp);
  RDClazzInfo lhci = _subNodes[0]->getExpressionSemanticElement()->getType();
  RDClazzInfo rhci = _subNodes[2]->getExpressionSemanticElement()->getType();
  if (lhci->equals(rhci) == false)
    THROW2(CodeException, "Can equals operator can only operate on values with same type:" 
                          "left hand expression type: " + lhci->getName() +
                          ", right hand expression type: " + rhci->getName(), this);
  /*
  SubscribeExpr:
  VarName:
    s
  MemberSubscribeExpr:
    '.'
    SubscribeExpr:
      VarName:
        equals
      FuncSubscribeExpr:
        Arguments:
          Argument:
            Literal:
              : b
  */
  RCode subscrExpr = comp->createCode("SubscribeExpr");
  subscrExpr->addSubNode(lex);
  RCode memsc = comp->createCode("MemberSubscribeExpr");
  subscrExpr->addSubNode(memsc);
  memsc->addSubNode(Keyword::createCode(comp, "."));
  RCode fs = comp->createCode("SubscribeExpr");
  memsc->addSubNode(fs);
  fs->addSubNode(VarName::createCode(comp, "equals"));
  RCode funcs = comp->createCode("FuncSubscribeExpr");
  fs->addSubNode(funcs);
  RCode args = comp->createCode("Arguments");
  funcs->addSubNode(args);
  RCode arg = comp->createCode("Argument");
  args->addSubNode(arg);
  arg->addSubNode(rex);
  if (op->equals("===") == true)
  {
    replaceThisWith(subscrExpr);
    subscrExpr->postParse(comp);
  }
  else
  {
    RCode pref = comp->createCode("PrefixExpr");
    pref->addSubNode(Keyword::createCode(comp, "!"));
    pref->addSubNode(subscrExpr);
    replaceThisWith(pref);
    pref->postParse(comp);
  }
}

void 
EqualsExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{

  EqualityExpr::emitOpCode(comp, oca);
  /*
  RCode lex = _subNodes[0];
  RString op = _subNodes[1]->getCodeString();
  RCode rex = _subNodes[2];
  RDClazzInfo ltype = lex->getExpressionSemanticElement()->getType();
  RDClazzInfo rtype = rex->getExpressionSemanticElement()->getType();
  if (ltype->isNumber() == true || ltype->isBoolean() == true)
  {
    EqualityExpr::emitOpCode(comp, oca);
    return;
  }
  */
  // nothing
}


void 
PostfixExpr::postParse(IN(RCompiler) comp)
{
  RCode opc = _subNodes[1];
  RCode ex = _subNodes[0];
  _flags |= IsLVarExpr;
  ex->postParse(comp);
  if (ex->getExpressionSemanticElement()->getType()->isNumber() == true)
  {
    setSemanticElement(&ex->getExpressionSemanticElement()->getType());
    setExpressionSemanticElement(&ex->getExpressionSemanticElement()->getType());
    
  }
  else
  {
    // ### TODO implement createObjectCall(comp, Operator::operatorToFuncName(op), lex,  Nil);
  }
  for (int i = 2; i < _subNodes->length(); ++i)
    _subNodes[i]->postParse(comp);
}
void
PostfixExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode operatorcode = _subNodes[1];
  RString opc = _subNodes[1]->getCodeString();
  RCode ex = _subNodes[0];
  _flags |= IsLVarExpr;
  ex->emitOpCode(comp, oca);
  OpCodeOp op = OCO_INC;
  if (opc->equals("--") == true)
    op = OCO_DEC;
  else if (opc->equals("++") == true)
    op = OCO_INC;
  else
   THROW2(CodeException, "Unknown Prefix Expression: " + opc, this);
  RSymbolTable st = getSymbolTable();
  int tv1 = st->createTempVar(Nil); // ### figure out type of expression
  comp->addOpCode(oca, new OpCodeStm(OCO_DUP_VAL, this, "postfix expression temp"));
  comp->addOpCode(oca, new AfOp(OCO_CLVR, this, tv1, "postfix expression temp"));
  comp->addOpCode(oca, new AfOp(OCO_STORE, this, tv1, Nil));
  
  comp->addOpCode(oca, new OpCodeStm(op, this, "prefix inc/dec"));
  comp->addOpCode(oca, new OpCodeStm(OCO_POP, this, "postfix expression temp erg"));
  comp->addOpCode(oca, new AfOp(OCO_LOAD, this, tv1, Nil));

   for (int i = 2; i < _subNodes->length(); ++i)
    _subNodes[i]->emitOpCode(comp, oca);
}

void 
PrefixExpr::postParse(IN(RCompiler) comp)
{
  RCode opc = _subNodes[0];
  RString op = opc->getCodeString();
  RCode ex = _subNodes[1];
  _flags |= IsLVarExpr;
  ex->postParse(comp);
  if (op->equals("-") == true || op->equals("+") == true ||
      op->equals("--") == true || op->equals("++") == true)
  {
    if (ex->getExpressionSemanticElement()->getType()->isNumber() == true)
    {
      setSemanticElement(&ex->getExpressionSemanticElement()->getType());
      setExpressionSemanticElement(&ex->getExpressionSemanticElement()->getType());
    }
    else
    {
      // ### TODO implement createObjectCall(comp, Operator::operatorToFuncName(op), lex,  Nil);
    }
  }
  else if (op->equals("!") == true)
  {
    if (ex->getExpressionSemanticElement()->getType()->isBoolean() == true)
    {
      setSemanticElement(&ex->getExpressionSemanticElement()->getType());
      setExpressionSemanticElement(&ex->getExpressionSemanticElement()->getType());
    }
    else
    {
      // ### TODO implement createObjectCall(comp, Operator::operatorToFuncName(op), lex,  Nil);
    }
  }
  else if (op->equals("~") == true)
  {
    RDClazzInfo expr = ex->getExpressionSemanticElement()->getType();
    if (expr->isIntegerNumber() == false)
      THROW2(CodeException, "In bit operator only left hand integer types are allowed", this);
    setExpressionSemanticElement(&expr);
  }
  else
  {
    THROW2(CodeException, "Unknown prefix operator: " + op, this);
  }
}

void
PrefixExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RString opc = _subNodes[0]->getCodeString();
  RCode ex = _subNodes[1];
  _flags |= IsLVarExpr;
  ex->emitOpCode(comp, oca);
  OpCodeOp op = OCO_INC;
  if (opc->equals("--") == true)
    op = OCO_DEC;
  else if (opc->equals("++") == true)
    op = OCO_INC;
  else if (opc->equals("!") == true)
    op = OCO_NOT;
  else if (opc->equals("~") == true)
    op = OCO_BIN_NOT;
  else
   THROW2(CodeException, "Unknown Prefix Expression: " + opc, this);
  if (op == OCO_DEC || op == OCO_INC)
    comp->addOpCode(oca, new OpCodeStm(op, this, "prefix inc/dec"));
  else if (op == OCO_NOT || op == OCO_BIN_NOT)
    comp->addOpCode(oca, new OpCodeStm(op, this, "negation"));
}

void
EmptyExpression::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  // nothing
}

void
BitwiseInfixExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RString opc = _subNodes[1]->getCodeString();
  RDClazzInfo lhci = _subNodes[0]->getExpressionSemanticElement()->getType();
  RDClazzInfo rhci = _subNodes[2]->getExpressionSemanticElement()->getType();
  /*
    <<
    >>
    &
    |
    ^
  */

  if (lhci->isIntegerNumber() == false)
    THROW2(CodeException, "In bit operator only left hand integer types are allowed", this);

  // ## TODO test if integer types

  setExpressionSemanticElement(&lhci);
  
}


void
BitwiseInfixExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  RString opc = _subNodes[1]->getCodeString();
  OpCodeOp op = OCO_NOP;
  if (opc->equals(">>") == true)
    op = OCO_BIN_SHR;
  else if (opc->equals(">>>") == true)
    op = OCO_BIN_SHRUS;
  else if (opc->equals("<<") == true)
    op = OCO_BIN_SHL;
  else if (opc->equals("|") == true)
    op = OCO_BIN_OR;
  else if (opc->equals("&") == true)
    op = OCO_BIN_AND;
  else if (opc->equals("^") == true)
    op = OCO_BIN_XOR;
  else
    THROW2(CodeException, "Unknow BitwiseInfixExpr operator:" + opc, this);

  comp->addOpCode(oca, new OpCodeStm(op, this, "binary op: " + opc));
}
void
ConditionalExpr::postParse(IN(RCompiler) comp)
{
  RCode testex = _subNodes[0];
  RCode trueex = _subNodes[1];
  RCode falseex = _subNodes[2];
  Code::postParse(comp);

  if (testex->getExpressionSemanticElement()->getType()->isBoolean() == false)
    THROW2(CodeException, "In expression if (?:) test expression must be boolean: " + testex->getExpressionSemanticElement()->getName(), this);
  
  if (trueex->getExpressionSemanticElement()->getType()->equals(falseex->getExpressionSemanticElement()->getType()) == false)
    THROW2(CodeException, "In expression if (?:) both result expressions must have the same type: " +
          trueex->getExpressionSemanticElement()->getName() + " != " + falseex->getExpressionSemanticElement()->getName(), this);

}

void
ConditionalExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode testex = _subNodes[0];
  RCode trueex = _subNodes[1];
  RCode falseex = _subNodes[2];
  testex->emitOpCode(comp, oca);
  int c = comp->getCounter();
  RString falselabel = RString("__expriffalse") + c;
  RString truelabel = RString("__expriftrue") + c;
  comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "expr if false", falselabel));
  trueex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "expr if true", truelabel));
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", falselabel));
  falseex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", truelabel));
}


} // aal
} // acdk



