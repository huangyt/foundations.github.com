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
#include "SubscribeExpressions.h"
#include <acdk/lang/System.h>
#include <acdk/lang/reflect/Modifier.h>


namespace acdk {
namespace aal {


using namespace acdk::aci;
using namespace acdk::lang::dmi;

void 
SubscribeExpr::postParse(IN(RCompiler) comp)
{
  if (_sem == Nil)
    setSemanticElement(_parent->getSemanticElement());
  Code::postParse(comp);
  if (_sem == Nil)
    setSemanticElement(_subNodes[_subNodes->length() - 1]->getSemanticElement());
  setExpressionSemanticElement(_subNodes[_subNodes->length() - 1]->getExpressionSemanticElement());
  RSemanticElem thissem = getSemanticElement();
  RCode sub0 = _subNodes[0];
  RSemanticElem sem0 = sub0->getSemanticElement();
  if (instanceof(thissem, DClazzMethodInfo) == true && 
      instanceof(sem0, DClazzInfo) == true &&
      (instanceof(sub0, VarName) == true || instanceof(sub0, FqTypeName) == true))
  {
    RDClazzMethodInfo minf(thissem);
    /*
      really necessary? break code like acdk.lang.System.out.println("asdf");
    if (minf->isStatic() == false)
    {
      RCode castex = comp->createCode("CastExpr");
      castex->addSubNode(FqTypeName::createCode(comp, sub0->getCodeString()));
      castex->addSubNode(VarName::createCode(comp, "this"));
      sub0->replaceThisWith(castex);

      _sem = Nil;
      postParse(comp);
    }
    */
  }
}



void 
SubscribeExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
}

RString 
MemberSubscribeExpr::getName()
{
  return _subNodes[0]->getName();
}

void 
MemberSubscribeExpr::postParse(IN(RCompiler) comp)
{
  /*
    lh -> VarName
    
    rh -> VarName | SubscribeExpr
  */
  setSemanticElement(_parent->getSemanticElement());
  RString n;
  RCode lh = getPreviousCode();
  if (lh == Nil) // ###????
  {
    System::out->println("\nMemberSubscribeExpr has no prev code:");
    printParentCodeTree(System::out, "");
    printCodeTree(System::out, "");
    return;
  }
  RCode op = _subNodes[0];
  RString opcode = op->getCodeString();
  if (opcode->equals("->") == true)
    dflags |= MiIvWeakBind;

  RSemanticElem se = lh->getSemanticElement();
  setSemanticElement(se);
  for (int i = 0; i < _subNodes->length(); ++i)
    _subNodes[i]->postParse(comp);
  setSemanticElement(_subNodes[_subNodes->length() - 1]->getSemanticElement());
  setExpressionSemanticElement(_subNodes[_subNodes->length() - 1]->getExpressionSemanticElement());
}

void 
MemberSubscribeExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
}


void 
Expression::createObjectCall(IN(RCompiler) comp, IN(RString) opcallname, IN(RCode) lhex,  IN(RCode) rhex)
{
  /*
Statement:
  ExprStatement:
    AdditiveExpr:
      VarName: acls
      '+'
      Literal:
        : 40

Statement:
  ExprStatement:
    SubscribeExpr:
      VarName: acls
      MemberSubscribeExpr:
        '.'
        SubscribeExpr:
          VarName: operator_pl
          FuncSubscribeExpr:
            Arguments:
              Argument:
                Literal:
                  : 40

        */
  RSubscribeExpr  subscr = (RSubscribeExpr)comp->createCode("SubscribeExpr");
  subscr->addSubNode(lhex);
  RMemberSubscribeExpr membersubscr = (RMemberSubscribeExpr)comp->createCode("MemberSubscribeExpr");
  subscr->addSubNode(&membersubscr);
  
  membersubscr->addSubNode(Keyword::createCode(comp, "."));
  RCode sub2ex = comp->createCode("SubscribeExpr");
  membersubscr->addSubNode(sub2ex);
  sub2ex->addSubNode(VarName::createCode(comp, opcallname));
  RFuncSubscribeExpr func = (RFuncSubscribeExpr)comp->createCode("FuncSubscribeExpr"); 
  sub2ex->addSubNode(&func);

  RCode args = comp->createCode("Arguments"); 
  RCode arg = comp->createCode("Argument"); 
  arg->addSubNode(rhex);
  args->addSubNode(&arg);
  func->addSubNode(&args);
  subscr->_parent = _parent;
  subscr->postParse(comp);
  replaceThisWith(&subscr);
}


RString 
FuncSubscribeExpr::getName()
{
  if (_sem != Nil)
    return _sem->getName();
  return _subNodes[0]->getName();
}

void 
Arguments::getArgumentTypes(ArgumentExprTypes& args)
{
  RCodeArray argcodes = getArgumentCodes();
  for (int i = 0; i < argcodes->length(); ++i)
  {
    RCode arg = argcodes[i];
    RString argname = Nil;
    RCode label = arg->findCode("Label", 2);
    if (label != Nil)
      argname = RLabel(label)->getIdentifier();
    RSemanticElem exprsem = arg->getExpressionSemanticElement();
    RString asc;
    if (argname != Nil)
      asc = argname->convert(CCUtf8);
    args.push_back(ArgumentExprType(exprsem->getType()->getImplClazzInfo(), asc != Nil ? asc->c_str() : 0));
  }
}

void 
Arguments::reorgArgs(const ArgumentExprTypes& argtypes)
{
  int i;
  bool hastoreorg = false;
  for (i = 0; i < argtypes.size(); ++i)
  {
    if (argtypes[i]._position != i)
    {
      hastoreorg = true;
      break;
    }
  }
  if (hastoreorg == false)
    return;
  RCodeArray argcodes = getArgumentCodes();
  RCodeArray nargs = new CodeArray(argcodes->length());
  
  for (i = 0; i < argcodes->length(); ++i)
  {
    nargs[argtypes[i]._position] = argcodes[i];
    RCode arg = nargs[argtypes[i]._position];
    RCode label = arg->findCode("Label", 2);
    if (label != Nil)
      arg->removeChild(label);
  }
  setArgumentCodes(nargs);
}

void 
Arguments::setArgumentCodes(IN(RCodeArray) args)
{
  // ### dead code?!
  _args = args;
  RCode arglist = findCode("ArgumentList", 2);
  arglist->_subNodes = args;
}

void
FuncSubscribeExpr::postParse(IN(RCompiler) comp)
{
  setSemanticElement(_parent->getSemanticElement());

  ROperator op = (ROperator)_parent->findCode("Operator", 2);
  if (op != Nil)
  {
    //RCode c = VarName::createCode(comp, op->operatorToFuncName());
    RCode c = VarName::createCode(comp, op->getIdentifier());
    op->replaceThisWith(c);
    c->postParse(comp);
  }

  RCode vn = getPreviousCode();//_parent->findCode("VarName", 2);
  RSemanticElem prevsem = vn->getSemanticElement();
  RSemanticElem parsem = _parent->getSemanticElement();

  RString vname = vn->getName();
  if (vname->equals("__assert") == true)
  {
    _subNodes[0]->postParse(comp);
    setExpressionSemanticElement(&DClazzInfo::getInstance(acdk::lang::dmi::ClazzInfo::getBoolClazz()));
    return;
  }
  //Code::postParse(comp);
  RCode funcnamecode = vn;
  RArguments args = (RArguments)findCode("Arguments", 2);
  args->postParse(comp);
  
  RDClazzInfo ci;
  if (instanceof(prevsem, DClazzMethodInfo) == true)
  {
    RMemberSubscribeExpr parentmembersubscr = (RMemberSubscribeExpr)findParentCode("MemberSubscribeExpr", 3);
    if (parentmembersubscr != Nil)
      dflags |= parentmembersubscr->dflags;
    
    ci = RDClazzMethodInfo(prevsem)->getOwnerClass();
    if (dflags & MiIvWeakBind || ci->getFlags() & MiIvWeakBind) // dynamic DMI
    {
      dflags |= MiIvWeakBind;
      setSemanticElement(&prevsem);
      setExpressionSemanticElement(&DClazzInfo::getInstance(DmiObject::clazzInfo()));
      return;
    }
    ArgumentExprTypes argtypes(0);
    args->getArgumentTypes(argtypes);
    RDClazzMethodInfo mi = ci->findFunction(funcnamecode->getName(), argtypes, 0);
    if (mi == Nil)
      THROW2(CodeException, "Cannot find matching function " + funcnamecode->getName() + " in " + ci->getName(), this);
    args->reorgArgs(argtypes);
    // super method invocation ?!
    if (instanceof(parsem, DClazzInfo) == true && mi->isStatic() == false)
    {

    }
    setSemanticElement(&mi);
    setExpressionSemanticElement(&mi->getReturnType());
    return;
  }
  else
  {
    // Object aobj() instance or Class AClass()
      /*
      SubscribeExpr:
        VarName: acls
        FuncSubscribeExpr:
          Arguments:
            Argument:
              Literal:
                : 42
to:
SubscribeExpr:
  VarName: acls
  MemberSubscribeExpr:
    '.'
    SubscribeExpr:
      VarName: operator_po_pc
      FuncSubscribeExpr:
        Arguments:
          Argument:
            Literal:
              : 42

*/
      //System::out->println("\nBefore operator():");
      //_parent->printCodeTree(System::out, "");
    RCode mems = comp->createCode("MemberSubscribeExpr");
    mems->addSubNode(Keyword::createCode(comp, "."));
    
    RCode sbs = comp->createCode("SubscribeExpr");
    mems->addSubNode(sbs);
    sbs->addSubNode(Operator::createCode(comp, "()"));
    RCode funcsub = comp->createCode("FuncSubscribeExpr");
    funcsub->addSubNode(&args);
    sbs->addSubNode(funcsub);
    for (int i = _parent->_subNodes->length() - 1; i > 1 ; --i)
    {
      RCode n = _parent->_subNodes[i];
      _parent->removeChild(n);
      sbs->addSubNode(n);
    }
    RCode thissic = this;
    replaceThisWith(mems);
    //System::out->println("\nAfter reorged operator():");
    //_parent->printCodeTree(System::out, "");
    mems->postParse(comp);
    return;
    
  }
}

//virtual 
void 
FuncSubscribeExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode vn = getPreviousCode(); //findCode("VarName", 2);
  if (vn->getName()->equals("__assert") == true)
  {
     RArguments arguments = (RArguments)findCode("Arguments");
     RCodeArray args = arguments->getArgumentCodes();
     if (args->length() != 2)
       THROW2(CodeException, "__assert must have 2 arguments", this);
     if (arguments != Nil)
      arguments->emitOpCode(comp, oca);
    comp->addOpCode(oca, new OpCodeStm(OCO_ASSERT, this, Nil));
    return;
  }
  RArguments arguments = (RArguments)findCode("Arguments");
  RCodeArray args = arguments->getArgumentCodes();
  RSymbolTable st = getSymbolTable();
  int tv1 = st->createTempVar(Nil); // ### figure out type of expression
  comp->addOpCode(oca, new AfOp(OCO_CLVR, this, tv1, "ClassName or object instance"));
  comp->addOpCode(oca, new AfOp(OCO_STORE, this, tv1, Nil));
  // check static
  
  if (arguments != Nil)
    arguments->emitOpCode(comp, oca);
  RDClazzMethodInfo cmi(_sem);
  comp->addOpCode(oca, new AfOp(OCO_LOAD, this, tv1, "ClassName or object instance"));
  RString callname = cmi->getCallName();
  
  if (dflags & MiIvWeakBind)
  {
    callname = cmi->getName();
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(callname), "function name"));
  }
  else
  {
    dflags |= MiIvViaHash;
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(cmi->getMethodInfo()->getMethodSignatureHashValue()), "function name: " + callname));
  }
  comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, 
    createInvokeFlags(MiPublic | dflags, args->length()), RString("dmi flags = PUBLIC, argcount = ") + args->length()));
  if (dflags & MiIvWeakBind)
  {
    if (cmi->getFlags() & MiStatic)
      comp->addOpCode(oca, new OpCodeStm(OCO_INVOKE_STATIC, this, Nil));
    else
      comp->addOpCode(oca, new OpCodeStm(OCO_INVOKE, this, Nil));
  }
  else
  {
    if (cmi->getFlags() & MiStatic)
      comp->addOpCode(oca, new OpCodeStm(OCO_HASH_INVOKE_STATIC, this, Nil));
    else
      comp->addOpCode(oca, new OpCodeStm(OCO_HASH_INVOKE, this, Nil));
  }
}


void 
Argument::postParse(IN(RCompiler) comp)
{
  if (_flags & CodeParsed)
    return;
  _sem = _parent->getSemanticElement();
  Code::postParse(comp);
  RArguments arguments = (RArguments)findParentCode("Arguments", 2);
  arguments->addArgument(this);
  //printParentCodeTree(System::out, "");
  RCode valcode = _subNodes[_subNodes->length() - 1];
  setSemanticElement(valcode->getSemanticElement());
}



void 
NewExpr::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  RString type = RFqTypeName(findCode("FqTypeName", 2))->getTypeName();

  RDClazzInfo ci = (RDClazzInfo)comp->findType(type, this);
  if (ci == Nil)
    THROW2(CodeException, "Cannot find class: " + type, this);
  if (ci->getFlags() & MiCiAbstract)
    THROW2(CodeException, "Cannot create object from abstract class: " + ci->getName(), this);
  setSemanticElement(&ci);
}


void
NewExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RDClazzInfo dci = (RDClazzInfo)getSemanticElement();
  RString type = dci->getName();
  _subNodes[1]->emitOpCode(comp, oca);

  //RString type = RFqTypeName(findCode("FqTypeName", 2))->getTypeName();
  RArguments arguments = (RArguments)findCode("Arguments");
  RCodeArray args = arguments->getArgumentCodes();
  if (dci->getImplClazzInfo()->isArray() == true)
  {
    const ClazzInfo* el = dci->getImplClazzInfo()->getArrayElementType();
    int dims = dci->getImplClazzInfo()->getArrayDims();
    RString elcn = DClazzInfo::getFqClassName(el);
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(elcn), "array element class name"));
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, createInvokeFlags(0, args->length(), dims), RString("dmi flags = 0, argcount = ") + args->length()));
    comp->addOpCode(oca, new OpCodeStm(OCO_NEWARR, this, Nil));
  }
  else
  {
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(type), "class name"));
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, createInvokeFlags(0, args->length()), RString("dmi flags = 0, argcount = ") + args->length()));
    comp->addOpCode(oca, new OpCodeStm(OCO_NEW, this, Nil));
  }
}

void 
CastExpr::postParse(IN(RCompiler) comp)
{
  RFqTypeName tp = (RFqTypeName)_subNodes[0];
  RString tpname = tp->getTypeName();
  RCode expr = _subNodes[1];
  expr->postParse(comp);
  RDClazzInfo ci = comp->findType(tpname, this);
  // ### check rh value for castable (Object, etc)
  //const ClazzInfo* ci = ClazzInfo::findClazzInfo(tpname);
  if (ci == Nil)
    THROW2(CodeException, "Cannot find type for casting: " + tpname, this);
  setSemanticElement(&ci);
}

void
CastExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode expr = _subNodes[1];
  expr->emitOpCode(comp, oca);
  RDClazzInfo dci = (RDClazzInfo)getSemanticElement();
  comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(dci->getName()), "class name to cast to"));
  comp->addOpCode(oca, new OpCodeStm(OCO_CLASSCAST, this, ""));
}


void 
ArraySubscribeExpr::postParse(IN(RCompiler) comp)
{
  RCode lex = getPreviousCode();
  RCode subex = _subNodes[0];
  subex->postParse(comp);

  RDClazzInfo dci = lex->getExpressionSemanticElement()->getType();
  if (dci->isArray() == true)
  {
    setSemanticElement(&dci->getArrayElementType());
  }
  else
  {
    // find operator[](T t);
    THROW2(CodeException, "user defined operator[] not implemented", this);
  }
  
  for (int i = 2; i < _subNodes->length(); ++i)
    _subNodes[i]->postParse(comp);
}

void
ArraySubscribeExpr::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode lex = getPreviousCode();
  RCode subex = _subNodes[0];
  
  OpCodeOp op = OCO_LDAREL;
  if (findFlagInParents(IsLVarExpr) != Nil)
    op = OCO_LDARELREF;
  //lex->emitOpCode(comp, oca);
  subex->emitOpCode(comp, oca);
  comp->addOpCode(oca, new OpCodeStm(op, this, "array element access"));
  for (int i = 2; i < _subNodes->length(); ++i)
    _subNodes[i]->emitOpCode(comp, oca);
}


} // aal
} // acdk


