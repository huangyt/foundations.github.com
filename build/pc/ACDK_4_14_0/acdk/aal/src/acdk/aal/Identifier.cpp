// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "Identifier.h"
#include "Expressions.h"
#include "SubscribeExpressions.h"
#include "TypeName.h"
#include "VarName.h"
#include "AalCompiler.h"
#include "AalObject.h"

#include <acdk/lang/System.h>
#include <acdk/lang/reflect/Modifier.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Byte.h>

namespace acdk {
namespace aal {

using namespace acdk::aci;
using namespace acdk::lang::dmi;

void 
BooleanLiteral::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RString n = _subNodes[0]->getCodeString();
  if (n->equals("true") == true || n->equals("false") == true)
  {
    setSemanticElement(&DClazzInfo::getInstance(acdk::lang::dmi::ClazzInfo::getBoolClazz()));
    setExpressionSemanticElement(&DClazzInfo::getInstance(acdk::lang::dmi::ClazzInfo::getBoolClazz()));
  } 
  else if (n->equals("nil") == true)
  {
    setSemanticElement(&DClazzInfo::getInstance(Object::clazzInfo()));
    setExpressionSemanticElement(&DClazzInfo::getInstance(Object::clazzInfo()));
  }
  else
    THROW2(CodeException, "Unknown boolean/nil identifier: " + n, this);
}

void
BooleanLiteral::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RString n = _subNodes[0]->getCodeString();
  if (n->equals("true") == true)
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(true), "true"));
  else if (n->equals("false") == true)
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(false), "false"));
  else if (n->equals("nil") == true)
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(Nil), "Nil"));
  else
    THROW2(CodeException, "Unknown boolean/nil identifier: " + n, this);
}


RCode 
DecimalTerminalParseNode::parse(IN(RCompiler) comp)
{
  RTerminal t = (RTerminal)TerminalParseNode::parse(comp);
  if (t == Nil)
    return Nil;
  RString tstr = t->_sv.getStringVar();
  RString s = tstr;
  
  char postfix = 0;
  if (s->endsWith("L") == true || s->endsWith("I") == true || 
      s->endsWith("S") == true || s->endsWith("B") == true)
  {
    char postfix = s->charAt(s->length() - 1);
    s = s->substr(0, s->length() - 1);
  }
  jlong lvar = Long::parseLong(s);
  if (lvar > Integer::MAX_VALUE || lvar < Integer::MIN_VALUE || postfix == 'L')
  {
    if (postfix != 0 && postfix != 'L')
      THROW1(Exception, "Constant to big for declarated size: " + tstr);
    return new Terminal(this, inOf(lvar));
  }
  int ivar = (int)lvar;
  if (ivar > Short::MAX_VALUE || ivar < Short::MIN_VALUE || postfix == 'I')
  {
    if (postfix != 0 && postfix != 'I')
      THROW1(Exception, "Constant to big for declarated size: " + tstr);
    return new Terminal(this, inOf(ivar));
  }
  short svar = (short)ivar;
  if (svar > Byte::MAX_VALUE || svar < Byte::MIN_VALUE || postfix == 'S')
  {
    if (postfix != 0 && postfix != 'S')
      THROW1(Exception, "Constant to big for declarated size: " + tstr);
    return new Terminal(this, inOf(svar));
  }
  return new Terminal(this, inOf(byte(svar)));
}

RCode 
FloatTerminalParseNode::parse(IN(RCompiler) comp)
{
  RTerminal t = (RTerminal)TerminalParseNode::parse(comp);
  if (t == Nil)
    return Nil;
  double d = Double::parseDouble(t->_sv.getStringVar());
  return new Terminal(this, inOf(d));
}

RString
FqTypeName::getTypeName()
{
  if (_subNodes->length() < 2)
    return RTypeName(_subNodes[0])->getTypeName();
  if (instanceof(_subNodes[1], FqTypeName) == true)
    return RTypeName(_subNodes[0])->getTypeName() + "." + RFqTypeName(_subNodes[1])->getTypeName();
  if (_subNodes[1]->getCodeName()->equals("ArrayDims") == true)
  {
    StringBuffer sb;
    RCode dims = _subNodes[1];
    for (int i = 0; i < dims->_subNodes->length(); ++i)
      sb << "[]";
    return RTypeName(_subNodes[0])->getTypeName() + sb.toString();
  }
  else
    THROW2(CodeException, "Unknown FqTypeName specifier: " + _subNodes[1]->getCodeName(), this);
  return "";
}

RString 
FqTypeName::getLastElemName()
{
  if (_subNodes->length() < 2)
    return RTypeName(_subNodes[0])->getTypeName();
  if (instanceof(_subNodes[1], FqTypeName) == true)
    return RFqTypeName(_subNodes[1])->getLastElemName();
   if (_subNodes[1]->getCodeName()->equals("ArrayDims") == true)
  {
    StringBuffer sb;
    RCode dims = _subNodes[1];
    for (int i = 0; i < dims->_subNodes->length(); ++i)
      sb << "[]";
    return RTypeName(_subNodes[0])->getTypeName() + sb.toString();
  }
  else
    THROW2(CodeException, "Unknown FqTypeName specifier: " + _subNodes[1]->getCodeName(), this);
  return "";
}

RString 
FqTypeName::getNotLastElemName()
{
  RString t = getTypeName();
  int idx = t->lastIndexOf('.');
  if (idx == -1)
    return "";
  return t->substr(0, idx);
}

//static 
RCode 
FqTypeName::createCode(IN(RCompiler) comp, IN(RString) fqname, IN(RString) codename)
{
  /*
  acdk.lang.Exception becomes:
  FqTypeName:
   TypeName:
     acdk
   FqTypeName:
     TypeName:
       lang
     FqTypeName:
       TypeName:
         Exception
  */
  RCode fq = comp->createCode(codename);
  RCode lfq = fq;
  RString tpn = fqname;
  //do {
    int idx = tpn->indexOf('.');
    RString pt = tpn;
    if (idx != -1)
    {
      pt = tpn->substr(0, idx);
      tpn = tpn->substr(idx + 1);
    }
    RTypeName tp = (RTypeName)comp->createCode("TypeName");
    tp->setTypeName(pt);
    fq->addSubNode(&tp);
    if (idx == -1)
      return fq;
    fq->addSubNode(FqTypeName::createCode(comp, tpn, "FqTypeName"));
    return fq;
  //} while (true);
  //return (RFqTypeName)fq;
}

void 
Literal::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  setSemanticElement(_subNodes[0]->getSemanticElement());
  setExpressionSemanticElement(_subNodes[0]->getSemanticElement());
}

RCode 
IdentifierParseNode::parse(IN(RCompiler) comp)
{
  ScannerTokenStack ss(comp->scanner);
  ScannerToken* st = ss.fetch();
  if (st->token != AalCompiler::ATT_IDENTIFIER)
    return Nil;

  RString sval = st->sval;
  ss.commit();
  return new Identifier(this, st->sval);
}


RCode 
TypeNameParseNode::parse(IN(RCompiler) comp)
{
  ScannerTokenStack ss(comp->scanner);
  ScannerToken* st = ss.fetch();
  if (st->token != AalCompiler::ATT_IDENTIFIER)
    return Nil;
  ss.commit();
  return new TypeName(this, st->sval);
}
  

RCode 
VarNameParseNode::parse(IN(RCompiler) comp)
{
  ScannerTokenStack ss(comp->scanner);
  ScannerToken* st = ss.fetch();
  if (st->token != AalCompiler::ATT_IDENTIFIER)
    return Nil;
  if (comp->isKeyword(st->sval) == true)
    return Nil;
  ss.commit();
  return new VarName(this, st->sval);
}


void 
TypeName::execute_inner(IN(REvalEnv) env)
{
  Code::execute_inner(env);
}

//static 
RCode 
VarName::createCode(IN(RCompiler) comp, IN(RString) value, IN(RString) nodename)
{
  RVarName vn = (RVarName)comp->createCode(nodename);
  vn->setVarName(value);
  return &vn;
}

bool
isMethod(IN(RVarName) vn)
{
  RCode c = vn->getNextCode();
  if (c == Nil)
    return false;
  if (instanceof(c, FuncSubscribeExpr) == true)
    return true;
  return false;
}

void 
VarName::postParse(IN(RCompiler) comp)
{
  if (getSemanticElement() != Nil)
    return;
  if (_identifier->equals("__assert") == true)
  {
    //setSemanticElement(sem);
    return;
  }
  
  RSemanticElem psem = _parent->getSemanticElement();
  RSemanticElem sem;
  if (psem != Nil)
  {
    
    RDClazzInfo t = psem->getType();
    if (t != Nil && t->getFlags() & MiIvWeakBind) // need dynamic invokation
    {
      if (isMethod(this) == true)
        sem = new DClazzMethodInfo(0, _identifier, t->getImplClazzInfo());
      else
        sem = new DClazzFieldInfo(0, _identifier, DClazzInfo::getInstance(acdk::lang::dmi::DmiObject::clazzInfo()), t->getImplClazzInfo());
    }
    else if (t != Nil && t->getImplClazzInfo() == DmiObject::clazzInfo())
    {
      const ClazzInfo* owner = RDClazzFieldInfo(psem)->getOwnerClazz();
      sem = new DClazzMethodInfo(0, _identifier, owner);
    }
    else
      sem = psem->findSubSem(_identifier, _parent->getSubscribeOperator());
    
  }
  else
    sem = comp->findSubSem(_identifier, _parent->getSubscribeOperator(), this);
  RSymbolTable st = getSymbolTable();
  int vidx = st->getVarIndex(_identifier);

  // is local variable
  if (sem == Nil && vidx != -1) 
  {
    sem = st->findSubSem(_identifier, _parent->getSubscribeOperator());
  } 
  
  // test for implicite this. access
  if (sem == Nil || (instanceof(sem, VarDefinition) && vidx == -1))
  {
    RCode classdeclcode = findParentCode("ClassDeclDef");
    if (classdeclcode != Nil)
    {
      if (_identifier->equals("super") == true)
      {
        RDClazzInfo dci(classdeclcode->getSemanticElement());
        RCode c = FqTypeName::createCode(comp, dci->getSuperClass()->getName());
        replaceThisWith(c);
        c->postParse(comp);
        return;
      }
      sem = classdeclcode->getSemanticElement()->findSubSem(_identifier, "");
      
      if (sem != Nil)
      {
       
       
           /*
        Statement:
  ExprStatement:
    SubscribeExpr:
      VarName: print
      FuncSubscribeExpr:
        Arguments:

Statement:
  ExprStatement:
    SubscribeExpr:
      VarName: this
      MemberSubscribeExpr:
        '.'
        SubscribeExpr:
          VarName: print
          FuncSubscribeExpr:
            Arguments:
        */
        
        RCode se = comp->createCode("SubscribeExpr");
        se->setSemanticElement(sem);

        if (_flags & IsLVarExpr) 
          se->_flags |= IsLVarExpr;
        if (sem->getFlags() & MiStatic)
          se->addSubNode(VarName::createCode(comp, classdeclcode->getSemanticElement()->getName()));
        else
          se->addSubNode(VarName::createCode(comp, "this"));
        RCode mse = comp->createCode("MemberSubscribeExpr");
        mse->addSubNode(Keyword::createCode(comp, "."));
        mse->addSubNode(VarName::createCode(comp, _identifier));
        se->addSubNode(mse);
        replaceThisWith(se);
        
        se->postParse(comp);
        return;
        /* old implemenation

        RCode se = comp->createCode("SubscribeExpr");
        if (sem->getFlags() & MiStatic)
          se->addSubNode(VarName::createCode(comp, classdeclcode->getSemanticElement()->getName()));
        else
          se->addSubNode(VarName::createCode(comp, "this"));
        RCode mse = comp->createCode("MemberSubscribeExpr");
        mse->addSubNode(Keyword::createCode(comp, "."));
        RCode thiscode = this;
        se->addSubNode(mse);
        RCode p = _parent;
        p->replaceThisWith(se);
        mse->addSubNode(p);
        se->getRootCode()->printCodeTree(System::out, "");
        se->postParse(comp);
        return;
        */

        /*
        if (instanceof(sem, DClazzFieldInfo) == true)
        {
          replaceThisWith(se);
          se->addSubNode(thiscode);
          se->postParse(comp);
          return;
        }
        else if (instanceof(sem, DClazzMethodInfo) == true)
        {
          RCode p = _parent;
          p->replaceThisWith(se);
          se->addSubNode(p);
          se->postParse(comp);

        }
        else
          THROW2(CodeException, "Cannot access type via implicite this: " + sem->getName(), this);
          */
      }
    }
  }
  if (sem == Nil)
    THROW2(CodeException, "Cannot find semantic element: " + _identifier, this);

  setSemanticElement(sem);
  setExpressionSemanticElement(sem);
  
}

//virtual 
void 
VarName::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  if (_identifier->equals("__assert") == true)
  {
    return;
  }
  if (_sem == Nil)
    THROW2(CodeException, "VarName has no sem: " + _identifier, this);
  RCode leftexpr = findFlagInParents(IsLVarExpr);
  if (instanceof(_sem, VarDefinition) == true)
  {
    RSymbolTable st = getSymbolTable();
    int vidx = st->getVarIndex(_identifier);
    if (vidx != -1)
    {
      if (leftexpr != Nil)
        comp->addOpCode(oca, new AfOp(OCO_LOADREF, this, vidx, _identifier));
      else
        comp->addOpCode(oca, new AfOp(OCO_LOAD, this, vidx, _identifier));
    } 
    else 
    {
      THROW2(CodeException, "Cannot find VarName: " + _identifier, this);
    }
  }
  else if (instanceof(_sem, DClazzFieldInfo) == true)
  {
    RDClazzFieldInfo fi = (RDClazzFieldInfo)_sem;
    RString fieldname = fi->getName();
    if (fi->isStatic() == true)
    {
      //varname did this already comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(fi->getOwnerClassName()), "class name"));
      comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(fieldname), "fieldname"));
      comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, createInvokeFlags(0), "dmi flags")); // MiAiOut should not be needed
      if (leftexpr != Nil)
        comp->addOpCode(oca, new OpCodeStm(OCO_POKE_STATIC, this, Nil));
      else
        comp->addOpCode(oca, new OpCodeStm(OCO_PEEK_STATIC, this, Nil));
    }
    else
    {
      comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(fieldname), "fieldname"));
      comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, createInvokeFlags(0), "dmi flags"));
      if (leftexpr != Nil)
        comp->addOpCode(oca, new OpCodeStm(OCO_POKE, this, Nil));
      else
        comp->addOpCode(oca, new OpCodeStm(OCO_PEEK, this, Nil));
    }
  }
  else if (instanceof(_sem, NamespaceDefinition) == true)
  {
    // nothing
  }
  else if (instanceof(_sem, DClazzInfo) == true)
  {
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(_sem->getName()), "class name"));
  }
  else if (instanceof(_sem, DClazzMethodInfo) == true)
  {
    // nothing
  }
  else
    THROW2(CodeException, "VarName::emitOpCode: unknown semant type", this);
}

void 
VarName::execute_inner(IN(REvalEnv) env)
{
  Code::execute_inner(env);
}

RString 
Operator::operatorToFuncName(IN(RString) opstr)
{
  return acdk::lang::reflect::Method::encodeOperatorToFuncName(opstr);
}

RCode 
OperatorParseNode::parse(IN(RCompiler) comp)
{
  ROperator op = (ROperator)ParseNode::parse(comp);
  if (op == Nil)
    return Nil;
  StringBuffer opstr;
nextoptry:
  for (int i = 0; i < op->_subNodes->length(); ++i)
  {
    RCode sn = op->_subNodes[i];
    if (instanceof(sn, Keyword) == true)
      opstr.append(RKeyword(sn)->getCodeString());
    else if (instanceof(sn, Operator) == true)
    {
      op = (ROperator)sn;
      goto nextoptry;
    } //opstr.append(ROperator(sn)->getIdentifier());
    else
      THROW1(Exception, "Operator: Unknown subnode type:" + sn->getCodeName());
  }
  op->_subNodes = new CodeArray(0);
  op->setIdentifier(opstr.toString());
  return &op;
}

void 
Operator::postParse(IN(RCompiler) comp)
{
  _identifier = operatorToFuncName(_identifier);
  VarName::postParse(comp);
}

void 
Identifier::postParse(IN(RCompiler) comp)
{
  //setExpressionType(comp->findType(_identifier, this)->getType());
}

void 
Identifier::execute_inner(IN(REvalEnv) env)
{
  //Code::execute(env);
  //env->push(outOf(_identifier));
}

void 
Identifier::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  int vidx = getSymbolTable()->getVarIndex(_identifier);
  if (vidx != -1)
  {
    comp->addOpCode(oca, new AfOp(OCO_LOAD, this, vidx, _identifier));
  } 
  else 
  {
    // nothing?
  }
  
}


void 
LVarDecl::postParse(IN(RCompiler) comp)
{
  /** 
    ### TODO if initialer break this statemet
    LVarDecl
    -> to
    Statements
      Statement
        LVarDecl
        AssignExpr
  */
  RString tn = RFqTypeName(findCode("FqTypeName", 2))->getTypeName();
  RVarName vn = (RVarName)findCode("VarName");

  int initidx = getChildIndex("VarInitializer");
  if (initidx != -1)
  {
    RCode initializer = _subNodes[initidx];
    RCode exprstm = comp->createCode("ExprStatement");
    RCode ass = comp->createCode("AssignmentExpr");
    exprstm->addSubNode(ass);
    ass->addSubNode(VarName::createCode(comp, vn->getIdentifier()));
    ass->addSubNode(Keyword::createCode(comp, "="));
    ass->addSubNode(initializer->_subNodes[0]);
    initializer->replaceThisWith(exprstm);
  }
   
  
  RSymbolTable st = getParentSymbolTable();
  RDClazzInfo td = comp->findType(tn, this);
  if (td == Nil)
    THROW2(CodeException, "Cannot find Type: " + tn, this);
  int flags = 0;
  st->newVarDecl(flags, td, vn->getVarName());
  if (initidx != -1)
    _subNodes[initidx]->postParse(comp);
  
}

void 
LVarDecl::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
    

  RCode type = _subNodes[0];
  RCode var = _subNodes[1];
  RVarName vn = (RVarName)var->findCode("VarName");
  RString s = vn->getVarName();
  RSymbolTable st = getOwnerSymbolTable(s);
  RVarDefinition vd = st->getVar(s);
  
  if (vd == Nil)
    THROW2(CodeException, "Cannot find identifer: " + s, this);

  int varidx = st->getVarIndex(s);
  
  comp->addOpCode(oca, new AfOp(OCO_CLVR, this, varidx, s));
  comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, vd->getType()->getInitializeValue(), "initialize local value"));
  comp->addOpCode(oca, new AfOp(OCO_STORE, this, varidx, "initialize local value"));

  int initidx = getChildIndex("ExprStatement");
  if (initidx != -1)
  {
    _subNodes[initidx]->emitOpCode(comp, oca);
  }
  /*
  if (_subNodes->length() >= 3)
  {
    RCode init = _subNodes[2];
    init->emitOpCode(comp, oca);
    comp->addOpCode(oca, new AfOp(OCO_STORE, this, varidx, s));
  }*/
}



void 
Variable::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RIdentifier ident = (RIdentifier)findCode("Identifier");
  RString s = ident->getVarName();
  RSymbolTable st = getOwnerSymbolTable(s);
  if (st == Nil)
    THROW2(CodeException, "Cannot find identifer: " + s, this);
  if (st->getFlag(SymbolTable::StTypeMask) == SymbolTable::IsLocal)
  {
    comp->addOpCode(oca, new AfOp(OCO_LOAD, this, st->getVarIndex(s), s));
  }
  else
    THROW2(CodeException, "Variable::emitOpCode only implemented for local variables", this);
}

void 
LeftHandVariable::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RIdentifier ident = (RIdentifier)findCode("Identifier");
  RString s = ident->getVarName();
  RSymbolTable st = getOwnerSymbolTable(s);
  if (st == Nil)
    THROW2(CodeException, "Cannot find identifer: " + s, this);
  if (st->getFlag(SymbolTable::StTypeMask) == SymbolTable::IsLocal)
  {
    comp->addOpCode(oca, new AfOp(OCO_STORE, this, st->getVarIndex(s), s));
  }
  else
    THROW2(CodeException, "LeftHandVariable::emitOpCode only implemented for local variables", this);
}


RCode 
LabelParseNode::parse(IN(RCompiler) comp)
{
  ScannerTokenStack ss(comp->scanner);
  ScannerToken* st = ss.fetch();
  if (st->token != AalCompiler::ATT_IDENTIFIER)
    return Nil;
  ss.commit();
  return new Label(this, st->sval);
}

} // aal
} // acdk
