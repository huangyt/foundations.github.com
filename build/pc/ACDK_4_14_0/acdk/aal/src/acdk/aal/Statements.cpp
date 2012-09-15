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
#include "Statements.h"
#include "Expressions.h"
#include <acdk/lang/System.h>
#include <acdk/lang/reflect/Modifier.h>


namespace acdk {
namespace aal {


using namespace acdk::aci;
using namespace acdk::lang::dmi;


void 
FunctionBlock::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  if (oca->length() < 1 || oca[oca->length() - 1]->getOpCode() != OCO_RET)
  {
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, ScriptVar(), "void return"));
    comp->addOpCode(oca, new BranchOp(OCO_RET, this, "", Nil));
  }
}

void 
ReturnStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_RET, this, "return statement", Nil));
}


void 
BreakStatement::postParse(IN(RCompiler) comp)
{
  RString breakorcont = RKeyword(_subNodes[0])->getCodeString();
  RCode c = _parent;
  while (c != Nil)
  {
    if (instanceof(c, Breakable) == true)
    {
      if (breakorcont->equals("continue") == true)
        _targetLabel = RBreakable(c)->getNextLabel();
      else
        _targetLabel = RBreakable(c)->getLastLabel();
      if (_targetLabel != Nil)
        break;
    }
    c = c->_parent;
  }
  if (_targetLabel == Nil)
    THROW2(CodeException, "cannot find parent statment to break/continue to", this);
}

void 
BreakStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "break/continue", _targetLabel));
}


void
Block::postParse(IN(RCompiler) comp)
{
  setSymbolTable(new SymbolTable(getParentSymbolTable()));
  Code::postParse(comp);
}

void 
WhileStatement::postParse(IN(RCompiler) comp)
{
  int c = comp->getCounter();
  _continueLabel = RString("whilebegin") + c;
  _breakLabel =  RString("whileend") + c;
  Code::postParse(comp);
}

void 
WhileStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  
  /*
    startlabel:
      relex opcodes
      brfalse endlabel
      
      stm opcodes

      br startlabel
    endlabel:
 
  
   */
  
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "whilebegin", _continueLabel));
  RCode relex = _subNodes[0];
  RCode stm = _subNodes[1];
  relex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "break while loop", _breakLabel));
  stm->emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "next while loop", _continueLabel));
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", _breakLabel));
  
}


void 
DoStatement::postParse(IN(RCompiler) comp)
{
  int c = comp->getCounter();
  _continueLabel = RString("dobegin") + c;
  _breakLabel =  RString("doend") + c;
  Code::postParse(comp);
}

void 
DoStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  /*
    startlabel:
      
      stm opcodes
      
      relex opcodes
      brfalse endlabel
      
      br startlabel
    endlabel:
   */
  
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "dostart", _continueLabel));
  RCode stm = _subNodes[0];
  stm->emitOpCode(comp, oca);

  RCode relex = _subNodes[1];
  relex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "break do loop", _breakLabel));
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "next do loop", _continueLabel));
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", _breakLabel));
  
}

void
ForStatement::postParse(IN(RCompiler) comp)
{
  setSymbolTable(new SymbolTable(getParentSymbolTable()));
  RCode initcode = _subNodes[0];
  RCode testcode = _subNodes[1];
  RCode stepcode = _subNodes[2];
  RCode stmcode = _subNodes[3];
  int c = comp->getCounter();
  _continueLabel = RString("fornext") + c;
  _breakLabel =  RString("forlast") + c;
  _firstLabel = RString("forfirst") + c;
  Code::postParse(comp);
}

void
ForStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  /*
    initcode
    br forfirst
  fornext:
    stepcode
  forfirst:
    testcode
    brfalse forlast
    exprcode
    br fornext
  forlast:
*/
  RCode initcode = _subNodes[0];
  RCode testcode = _subNodes[1];
  RCode stepcode = _subNodes[2];
  RCode stmcode = _subNodes[3];
  initcode->emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "first for loop", _firstLabel));
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", _continueLabel));
  if (instanceof(stepcode, EmptyExpression) == false)
  {
    stepcode->emitOpCode(comp, oca);
    comp->addOpCode(oca, new OpCodeStm(OCO_POP, this, "remove step expression result"));
  }
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", _firstLabel));
  if (instanceof(testcode, EmptyExpression) == false)
  {
    testcode->emitOpCode(comp, oca);
    comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "break for loop", _breakLabel));
  }
  stmcode->emitOpCode(comp, oca);
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "next for loop", _continueLabel));
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", _breakLabel));
}

void
IfStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode relex = _subNodes[0];
  RCode trueblock = _subNodes[1];
  RCode falseblock;
  int c = comp->getCounter();
  if (_subNodes->length() > 2)
    falseblock = _subNodes[2];
  relex->emitOpCode(comp, oca);
  if (falseblock != Nil)
    comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "goto else block", RString("elseblock") + c));
  else
    comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "goto endif", RString("endif") + c));
  trueblock->emitOpCode(comp, oca);
  if (falseblock != Nil)
  {
    comp->addOpCode(oca, new BranchOp(OCO_BR, this, "goto endif", RString("endif") + c));
    comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", RString("elseblock") + c));
    falseblock->emitOpCode(comp, oca);
  }
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", RString("endif") + c));

}

void
GotoStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  // TODO: currently not checked if scope is scipped
  RString s = _subNodes[0]->getCodeString();
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "goto", s));
}

void
LabeledStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RString s = _subNodes[0]->getCodeString();
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "", s));
  for (int i = 1; i < _subNodes->length(); ++i)
    _subNodes[i]->emitOpCode(comp, oca);
}



void
ExprStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_POP, this, "discarge expression result for ExprStatement"));
}



void
ThrowStatement::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  if (_subNodes->length() > 0)
  {
    // ## TODO check if expression result is type of Throwable
  }
}

void
ThrowStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_THROW, this, "throws an exception"));
}


void
TryCatchStatement::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  
}

void
TryCatchStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  int c = comp->getCounter();
  RString label = RString("try") + c;
  comp->addOpCode(oca, new BranchOp(OCO_TRY, this, "try label", label));
  RCode triedst = _subNodes[0];
  triedst->emitOpCode(comp, oca);
  RString finallylabel = RString("finally") + comp->getCounter();
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "goto finally", finallylabel));
  

  RCode finalyc;
  bool hascatchblock = false;
  for (int i = 1; i < _subNodes->length(); ++i)
  {
    RCode c = _subNodes[i];
    if (instanceof(c, CatchBlock) == true)
    {
      hascatchblock = true;
      comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  label));
      RCatchBlock cb = (RCatchBlock)c;
      label = RString("nextcatch") + comp->getCounter();
      if (i + 1 > _subNodes->length())
        cb->_nextBreakLabel = label;
      cb->_finallyLabel = finallylabel;
      cb->emitOpCode(comp, oca);
    }
    else // finally
    {
      finalyc = c;
      if (hascatchblock == false)
        comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  label));
      comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  finallylabel));
      finallylabel = Nil;
      c->emitOpCode(comp, oca);
    }
  }
  if (finallylabel != Nil)
    comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  finallylabel));
  if (finalyc == Nil)
    comp->addOpCode(oca, new OpCodeStm(OCO_FINALLYEND, this, "goto to next level try-catch block or leafe method"));
}

void
CatchBlock::postParse(IN(RCompiler) comp)
{
  //Code::postParse(comp);
  RString extype = RFqTypeName(_subNodes[0])->getName();
  RString exvar = RVarName(_subNodes[1])->getName();
  
  RSymbolTable st = getSymbolTable();
  RDClazzInfo td = comp->findType(extype, this);
  if (td == Nil)
    THROW2(CodeException, "Cannot find Type: " + extype, this);
  // ## TODO: chec extype if is exception
  int flags = 0;
  st->newVarDecl(flags, td, exvar);
  
  RCode statement = _subNodes[2];
  statement->postParse(comp);
  
}

void
CatchBlock::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RString extype = RFqTypeName(_subNodes[0])->getName();
  RString exvar = RVarName(_subNodes[1])->getName();
  RCode statement = _subNodes[2];
  RDClazzInfo td = comp->findType(extype, this);
  comp->addOpCode(oca, new OpCodeStm(OCO_DUP, this, "safe exception"));
  comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(td->getName()), "exception name to test for catching"));  
  comp->addOpCode(oca, new OpCodeStm(OCO_INSTANCEOF, this, ""));
  if (_nextBreakLabel != Nil)
    comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "goto next catch block", _nextBreakLabel));
  else
    comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "goto next catch block", _finallyLabel));
  
  RSymbolTable st = getOwnerSymbolTable(exvar);
  RVarDefinition vd = st->getVar(exvar);
  
  if (vd == Nil)
    THROW2(CodeException, "Cannot find identifer: " + exvar, this);

  int varidx = st->getVarIndex(exvar);
  
  comp->addOpCode(oca, new AfOp(OCO_CLVR, this, varidx, exvar));
  comp->addOpCode(oca, new AfOp(OCO_STORE, this, varidx, Nil));
  statement->emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_CATCHEND, this, "clear exception"));
  comp->addOpCode(oca, new BranchOp(OCO_BR, this, "goto finally", _finallyLabel));
}

void
FinallyBlock::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
}

void
FinallyBlock::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(OCO_FINALLYEND, this, "goto to next level try-catch block or leafe method"));
}

void
SwitchStatement::postParse(IN(RCompiler) comp)
{
  RCode expr = _subNodes[0];
  expr->postParse(comp);
  int c = comp->getCounter();
  _breakLabel = RString("endswitch") + c;
  _tempTestVar = RString("__switchtestexpr") + c;
  RCode lvardecl = comp->createCode("LVarDecl");
  lvardecl->addSubNode(FqTypeName::createCode(comp, expr->getExpressionSemanticElement()->getType()->getName()));
  lvardecl->addSubNode(VarName::createCode(comp, _tempTestVar));
  RCode varinit = comp->createCode("VarInitializer");
  lvardecl->addSubNode(varinit);
  expr->replaceThisWith(lvardecl);
  varinit->addSubNode(expr);
  lvardecl->postParse(comp);
  
  setSemanticElement(expr->getExpressionSemanticElement());
  RString lastcaselabel =  RString("case") + c;
  RString nextcaselabel;
  for (int i = 1; i < _subNodes->length(); ++i)
  {
    RCaseClause cc = (RCaseClause)_subNodes[i];
    if (i + 1 < _subNodes->length())
      nextcaselabel = RString("case") + comp->getCounter();
    else
      nextcaselabel = _breakLabel;
    cc->_breakLabel = _breakLabel;
    cc->_continueLabel = nextcaselabel;
    cc->_thirdLabel = RString("case_stmt") + comp->getCounter();
    cc->postParse(comp);
    lastcaselabel = nextcaselabel;
  }
}

void
SwitchStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  _subNodes[0]->emitOpCode(comp, oca);
  RString lastswitchlabel;
  for (int i = 1; i < _subNodes->length(); ++i)
  {
    if (lastswitchlabel != Nil)
      comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  lastswitchlabel));
    RCaseClause cc = (RCaseClause)_subNodes[i];
    cc->emitOpCode(comp, oca);
    lastswitchlabel = cc->_continueLabel;
    if (i + 1 < _subNodes->length())
    {
      RCaseClause nextcc = (RCaseClause)_subNodes[i + 1];
      comp->addOpCode(oca, new BranchOp(OCO_BR, this, "goto next switch stm block", nextcc->_thirdLabel));
    }
    
  }
  comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  _breakLabel));
}

void
CaseClause::postParse(IN(RCompiler) comp)
{
  RCode kw = _subNodes[0];
  if (kw->getCodeString()->equals("default") == true)
  {
    for (int i = 1; i < _subNodes->length(); ++i)
      _subNodes[i]->postParse(comp);
  }
  else
  {
    RCode expr = _subNodes[1];
    RCode equalsexpr = comp->createCode("EqualsExpr");
    equalsexpr->addSubNode(VarName::createCode(comp, RSwitchStatement(_parent)->_tempTestVar));
    equalsexpr->addSubNode(Keyword::createCode(comp, "==="));
    expr->replaceThisWith(equalsexpr);
    equalsexpr->addSubNode(expr);
    
    equalsexpr->postParse(comp);
    for (int i = 2; i < _subNodes->length(); ++i)
      _subNodes[i]->postParse(comp);
  }

}

void
CaseClause::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode kw = _subNodes[0];
  if (kw->getCodeString()->equals("default") == true)
  {
    comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  _thirdLabel));
    for (int i = 1; i < _subNodes->length(); ++i)
      _subNodes[i]->emitOpCode(comp, oca);
  }
  else
  {

    RCode expr = _subNodes[1];
    expr->emitOpCode(comp, oca);
    comp->addOpCode(oca, new BranchOp(OCO_BRFALSE, this, "goto next switch", _continueLabel));
    comp->addOpCode(oca, new OpCodeStm(OCO_NOP, this, "",  _thirdLabel));
    for (int i = 2; i < _subNodes->length(); ++i)
      _subNodes[i]->emitOpCode(comp, oca);
  }
}




} // aal
} // acdk



