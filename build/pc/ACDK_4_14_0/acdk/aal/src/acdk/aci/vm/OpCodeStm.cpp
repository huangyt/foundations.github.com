// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/vm/OpCodeStm.cpp,v 1.5 2005/04/18 20:40:35 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include "OpCodeStm.h"
#include "EvalEnv.h"
#include "../Compiler.h"
#include "vm.h"
#include "../ast/Terminal.h"


namespace acdk {
namespace aci {
namespace vm {

using namespace acdk::lang::dmi;

void 
Executable::execute(IN(REvalEnv) env)
{
  //RCode code = getCode();
  env->dumpStack(acdk::lang::System::out);
  
  System::out->print(RString("") + env->pc() + ": ");
  ACDK_NLOG("acdk.aci.vm", Debug, SBSTR("(" << env->pc() <<  ") " << RObject(this)->toString()));
  //printOpCode(&System::out);
  execute_inner(env);
}



OpCodeOpDescription*
OpCodeStm::getOpCodeDescriptionTable()
{
  static OpCodeOpDescription _table[OCO_MAXOPCODE] =
  {
    { OCO_NOP, "nop", 0, OpCodeStm::createOpCodeFnc },
    { OCO_PUSH, "push", 1, (OpCodeCreatorFnc)OpCodeStm1::createOpCodeFnc },
    { OCO_POP, "pop", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_ADD, "add", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_SUB, "sub", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_LT, "lt", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_GT, "gt", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_LTEQ, "lteq", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_GTEQ, "gteq", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_EQ, "eq", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_NE, "ne", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_INC, "inc", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_DEC, "dec", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_NOT, "not", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_INVALID_OP, 0, 0 }
  };
  static OpCodeOpDescription _unSorted[] = 
  {
    { OCO_LOAD, "load", 1, (OpCodeCreatorFnc)AfOp::createOpCodeFnc },
    { OCO_STORE, "store", 1, (OpCodeCreatorFnc)AfOp::createOpCodeFnc },
    { OCO_CLVR, "clvr", 1, (OpCodeCreatorFnc)AfOp::createOpCodeFnc },
    { OCO_LOADREF, "loadref", 1, (OpCodeCreatorFnc)AfOp::createOpCodeFnc },
    { OCO_DUP, "dup", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_LDAREL, "ldarel", 0, (OpCodeCreatorFnc)AfOp::createOpCodeFnc },
    { OCO_LDARELREF, "ldarelref", 0, (OpCodeCreatorFnc)AfOp::createOpCodeFnc },

    { OCO_RET, "ret", 0, (OpCodeCreatorFnc)BranchOp::createOpCodeFnc },
    { OCO_BR, "br", 0, (OpCodeCreatorFnc)BranchOp::createOpCodeFnc },
    { OCO_BRFALSE, "brfalse", 0, (OpCodeCreatorFnc)BranchOp::createOpCodeFnc },
    { OCO_BRTRUE, "brtrue", 0, (OpCodeCreatorFnc)BranchOp::createOpCodeFnc },

    { OCO_ASSIGN, "assign", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_MUL, "mul", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_DIV, "div", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_MOD, "mod", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_AND, "bin_and", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_OR, "bin_or", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_NOT, "bin_not", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_XOR, "bin_xor", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_SHR, "bin_shr", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_SHRUS, "bin_shrus", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },
    { OCO_BIN_SHL, "bin_shl", 0, (OpCodeCreatorFnc)OpCodeStm::createOpCodeFnc },

    { OCO_LOADGLOB, "loadglob", 0, (OpCodeCreatorFnc)GlobVarOp::createOpCodeFnc },
    { OCO_LOADGLOBREF, "loadglobref", 0, (OpCodeCreatorFnc)GlobVarOp::createOpCodeFnc },
    { OCO_STOREGLOB, "storeglob", 0, (OpCodeCreatorFnc)GlobVarOp::createOpCodeFnc },
    { OCO_CREATEGLOB, "createglob", 0, (OpCodeCreatorFnc)GlobVarOp::createOpCodeFnc },

    { OCO_INVALID_OP, 0, 0 }
  };
    
  static bool initialized = false;
  if (initialized == true)
    return _table;
  int i;
  for (i = 0; i < OCO_MAXOPCODE; ++i)
  {
    if (_table[i].opCodeOp == OCO_INVALID_OP)
      break;
    if (_table[i].opCodeOp != i)
    {
      THROW1(Exception, "OpCodeOp doesn't match in OpCodeDescriptionTable");
    }
  }
  
  for (; i < OCO_MAXOPCODE; ++i)
  {
    _table[i].opCodeOp = OCO_INVALID_OP;
    _table[i].name = 0;
    _table[i].creator = 0;
  }
  for (i = 0; _unSorted[i].opCodeOp != OCO_INVALID_OP; ++i)
  {
    _table[_unSorted[i].opCodeOp] = _unSorted[i];
  }

  /*
  case OCO_SUB: return "sub";
  case OCO_LOAD: return "load";
  case OCO_STORE: return "store";
  case OCO_CLVR: return "clvr";
  case OCO_LOADREF: return "loadref";
 
  case OCO_PUSH: return "push";
  case OCO_POP: return "pop";
  case OCO_DUP: return "dup";
  case OCO_DUP_VAL: return "dupval";
  case OCO_DUP_REF: return "dupref";
  case OCO_LDAREL: return "ldarel";
  case OCO_LDARELREF: return "ldarelref";
  case OCO_LOADGLOB : return "loadglob";
  case OCO_LOADGLOBREF : return "loadglobref";
  case OCO_ASSIGN: return "assign";
  case OCO_RET: return "ret";
  case OCO_BR: return "br";
  case OCO_BRFALSE: return "brfalse";
  case OCO_BRTRUE: return "brtrue";
  case OCO_BIN_AND: return "band";
  case OCO_BIN_OR: return "bor";
  case OCO_BIN_NOT: return "bnot";
  case OCO_BIN_XOR: return "bxor";
  case OCO_BIN_SHR: return "bshr";
  case OCO_BIN_SHRUS: return "bshrus";
  case OCO_BIN_SHL: return "bshl";
  case OCO_MUL: return "mul";
  case OCO_DIV: return "div";
  case OCO_MOD: return "mod";
  case OCO_NEW : return "new";
  case OCO_NEWARR : return "newarr";
  case OCO_INSTANCEOF : return "instanceof";
  case OCO_TRY : return "try";
  case OCO_CATCHEND: return "catchend";
  case OCO_FINALLYEND: return "finallyend";
  case OCO_THROW: return "throw";
  case OCO_INVOKE : return "invoke";
  case OCO_INVOKE_STATIC : return "invoke_static";
  case OCO_HASH_INVOKE: return "hash_invoke";
  case OCO_HASH_INVOKE_STATIC: return "hash_invoke_static";
  case OCO_PEEK: return "peek";
  case OCO_PEEK_STATIC : return "peek_static";
  case OCO_POKE : return "poke";
  case OCO_POKE_STATIC : return "poke_static";
  case OCO_CLASSCAST: return "classcast";
  case OCO_INIT_CLAZZ : return "init_clazz";
  case OCO_ASSERT: return "assert";
  case OCO_DUMPSTACK: return "dumpstack";
  */
  initialized = true;
  return _table;
}

//static 
RString 
OpCodeStm::opToString(OpCodeOp op)
{
  OpCodeOpDescription* table =  getOpCodeDescriptionTable();
  if (table[op].opCodeOp == OCO_INVALID_OP)
    return "<invalide op>";
  return table[op].name;
}


void 
OpCodeStm::execute_inner(IN(REvalEnv) env)
{
  switch (_op)
  {
  case OCO_NOP:
    break;
  case OCO_POP:
    env->pop();
    break;
  case OCO_DUP:
    env->push(env->top());
    break;
  case OCO_DUP_VAL:
    env->push(env->top().inOf());
    break;
  case OCO_DUP_REF:
    env->push(env->top().inoutOf());
    break;
  case OCO_ADD:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    env->push(sv1.addition(sv2));
    break;
  }
  case OCO_SUB:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    env->push(sv1.subtraction(sv2));
    break;
  }
  
  case OCO_LT:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    env->push(sv2.less_than(sv1));
    break;
  }
  case OCO_GT:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    env->push(sv2.greater_than(sv1));
    break;
  }
  case OCO_LTEQ:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    env->push(sv2.less_or_equal(sv1));
    break;
  }
  case OCO_GTEQ:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    env->push(sv2.greater_or_equal(sv1));
    break;
  }
  case OCO_EQ:
  case OCO_NE:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    if (sv1.isObjectType() == true)
    {
      if (_op == OCO_EQ)
        env->push(inOf(sv1.getObjectVar() == sv2.getObjectVar()));
      else
        env->push(inOf(sv1.getObjectVar() != sv2.getObjectVar()));
    }
    else
    {
      if (_op == OCO_EQ)
        env->push(sv2.equal(sv1));
      else
        env->push(sv2.not_equal(sv1));
    }
    break;
  }
  case OCO_NOT:
  {
    ScriptVar sv = env->pop();
    env->push(inOf(sv.getBoolVar() == false));
    break;
  }
  case OCO_INC:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar e = sv1.addition(inOf(1));
    sv1 = e;
    env->push(e);
    break;
  }
  case OCO_DEC:
  {
    ScriptVar sv1 = env->pop();
    ScriptVar e = sv1.subtraction(inOf(1));
    sv1 = e;
    env->push(e);
    break;
  }
  case OCO_BIN_AND: 
  {
    ScriptVar lh = env->pop();
    ScriptVar rh = env->pop();
    env->push(rh.binary_and(lh));
    break;
  }
  case OCO_BIN_OR: 
  {
    ScriptVar lh = env->pop();
    ScriptVar rh = env->pop();
    env->push(rh.binary_or(lh));
    break;
  }
  case OCO_BIN_NOT: 
  {
    ScriptVar rh = env->pop();
    env->push(rh.binary_not());
    break;
  }
  case OCO_BIN_XOR: 
  {
    ScriptVar lh = env->pop();
    ScriptVar rh = env->pop();
    env->push(rh.binary_xor(lh));
    break;
  }
  case OCO_BIN_SHR: 
  {
    ScriptVar rh = env->pop();
    ScriptVar lh = env->pop();
    env->push(lh.binary_rightshift(rh));
    break;
  }
  case OCO_BIN_SHRUS:
  {
    ScriptVar rh = env->pop();
    ScriptVar lh = env->pop();
    env->push(lh.binary_rightshift_unsigned(rh));
    break;
  }
  case OCO_BIN_SHL: 
  {
    ScriptVar lh = env->pop();
    ScriptVar rh = env->pop();
    env->push(lh.binary_leftshift(rh));
    break;
  }
  case OCO_MUL: 
  {
    ScriptVar lh = env->pop();
    ScriptVar rh = env->pop();
    env->push(lh.multiply(rh));
    break;
  }
  case OCO_DIV: 
  {
    ScriptVar rh = env->pop();
    ScriptVar lh = env->pop();
    env->push(lh.divide(rh));
    break;
  }
  case OCO_MOD: 
  {
    ScriptVar rh = env->pop();
    ScriptVar lh = env->pop();
    env->push(lh.modulo(rh));
    break;
  }
  case OCO_ASSIGN :
  {
    ScriptVar sv1 = env->pop();
    ScriptVar sv2 = env->pop();
    // ## TODO check sv1 may for example intref and sv2 object
    sv1 = sv2;
    env->push(sv1.inOf());
    break;
  }
  case OCO_NEW:
  {
    env->compiler()->compNewObject(env);
    break;
  }
  case OCO_NEWARR:
  {
    env->compiler()->compNewArray(env);
    break;
  }
  case OCO_PEEK:
  {
    env->compiler()->compPeek(env);
    break;
  }
  case OCO_POKE:
  {
    env->compiler()->compPoke(env);
    break;
  }
  case OCO_PEEK_STATIC:
  {
    env->compiler()->compPeekStatic(env);
    break;
  }
  case OCO_POKE_STATIC:
  {
    env->compiler()->compPokeStatic(env);
    break;
  }
  case OCO_INVOKE:
  {
    env->compiler()->compInvokeMethod(env);
    break;
  }
  case OCO_INVOKE_STATIC:
  {
    env->compiler()->compInvokeStaticMethod(env);
    break;
  }
  case OCO_HASH_INVOKE:
  {
    env->compiler()->compInvokeHashMethod(env);
    break;
  }
  case OCO_HASH_INVOKE_STATIC:
  {
    env->compiler()->compInvokeStaticHashMethod(env);
    break;
  }
  case OCO_LDAREL:
  {
    int idx = env->pop().getIntVar();
    RObject obj = env->pop().getObjectVar();
    if (instanceof(obj, RObjectArray) == true)
      env->push(inOf(RObjectArray(obj)[idx]));
    else
      env->push(obj->invoke("get", idx));
    break;
  }
  case OCO_LDARELREF:
  {
    int idx = env->pop().getIntVar();
    RObject obj = env->pop().getObjectVar();
    if (instanceof(obj, ObjectArray) == true)
    {
      RObjectArray oarr = (RObjectArray)env->pop().getObjectVar();
      env->push(outOf(oarr[idx]));
    }
    else
    {
      env->push(obj->invoke("getref", idx));
    }
    break;
  }
  case OCO_CLASSCAST:
  {
    RString classname = env->pop().getStringVar();
    const ClazzInfo* toci = ClazzInfo::findClazzInfo(classname);
    if (toci == 0)
      THROW1(Exception, "Cannot find Class: " + classname);

    RObject obj = env->pop().getObjectVar();
    if (obj == Nil)
    {
      env->push(obj);
      break;
    }
    const ClazzInfo* fromci = obj->getClazzInfo();
    if (fromci == toci)
    {
      env->push(obj);
      break;
    }
    if (toci->assignableFrom(fromci) == true)
    {
      RCastedObject cow = new CastedObject(obj, toci);
      env->push(&cow);
      break;
    }
    THROW1(Exception, "Cannot cast Class from type " + DClazzInfo::getFqClassName(fromci) + " to " + DClazzInfo::getFqClassName(toci));
    break;
  }
  case OCO_INSTANCEOF:
  {
    RString classname = env->pop().getStringVar();
    RObject obj = env->pop().getObjectVar();
    RClass cls = Class::forName(classname);
    if (cls->isAssignableFrom(obj->getClass()) == true)
      env->push(inOf(true));
    else
      env->push(inOf(false));
    break;
  }
  case OCO_INIT_CLAZZ:
  {
    //RDClazzInfo ci = _code->getSymbolTable()->getMethodClazzInfo();
    RString classname = env->pop().getStringVar();
    const ClazzInfo* ci = ClazzInfo::findClazzInfo(classname, false);
    if (ci == 0)
      THROW1(Exception, "Cannot locate class: " + classname);
    ci->callClassInitializer();
    break;
  }
  case OCO_THROW:
  {
    RThrowable ex = (RThrowable)env->pop().getObjectVar();
    env->af().throwException(env, ex, false);
    break;
  }
  case OCO_CATCHEND:
  {
    env->af().clearException();
    break;
  }
  case OCO_FINALLYEND:
  {
    env->af().finallyEnd(env);
    break;
  }
  case OCO_ASSERT:
  {
    RString msg = env->pop().getStringVar();
    bool cond = env->pop().getBoolVar();
    if (cond == false)
      THROW1(Exception, "Assertion failed: " + msg); // ### better ex
    env->push(inOf(true));
    break;
  }
  case OCO_DUMPSTACK:
  {
    env->dumpStack(System::out);
    break;
  }
  default:
    THROW1(Exception, RString("Unknows OpCodeStm: ") + (int)_op);
  }
}

OpCodeStm::OpCodeStm(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(RString) label, IN(RString) comment)
: AstNode(parent)
, _op(op)
, _label(label)
, _comment(comment)
{}

RString 
OpCodeStm::toString() 
{ 
  StringBuffer sb;
  if (_label != Nil)
    sb << _label << ": ";
  sb << opToString(_op);
  if (_comment != Nil && _comment->length() > 0)
    sb << " // " << _comment;
  return sb.toString();
}

void 
OpCodeStm::printOpCode(IN(acdk::io::RPrintWriter) out)
{
  
  out->println(getLabelToPrint() + opToString(_op) + getCommentToPrint());
}

//static 
ROpCodeStm 
OpCodeStm::createOpCode(IN(RString) ident, const acdk::lang::dmi::ScriptVar& sv, IN(RString) label)
{
  OpCodeOpDescription* table = getOpCodeDescriptionTable();
  for (int i = 0; i < OCO_MAXOPCODE; ++i)
  {
    if (table[i].name == 0)
      continue;
    if (ident->equals(table[i].name) == true)
    {
      if (table[i].creator == 0)
        return Nil;
      return table[i].creator(table[i].opCodeOp, ident, label, sv);
    }
  }
  return Nil;
}

RString 
AfOp::toString()
{
  StringBuffer sb;
  if (_label != Nil)
    sb << _label << ": ";
  sb << opToString(_op) << " " << _index;
  if (_comment != Nil && _comment->length() > 0)
    sb << " // " << _comment;
  return sb.toString();
}


void 
AfOp::execute_inner(IN(REvalEnv) env)
{
  switch (_op)
  {
  case OCO_CLVR:
    env->af().crlv(_index);
    break;
  case OCO_LOAD:
    env->load(_index);
    break;
  case OCO_LOADREF:
    env->loadRef(_index);
    break;
  case OCO_STORE:
    env->store(_index);
    break;
  default:
    THROW1(Exception, RString("Unknows AfOp: ") + (int)_op);
  }
}

void 
AfOp::printOpCode(IN(acdk::io::RPrintWriter) out)
{
  out->println(getLabelToPrint() + opToString(_op) + " " + _index + getCommentToPrint());
}

RString 
GlobVarOp::toString()
{
  StringBuffer sb;
  if (_label != Nil)
    sb << _label << ": ";
  sb << opToString(_op) << " " << _varName << " " << _varName;
  if (_comment != Nil && _comment->length() > 0)
    sb << " // " << _comment;
  return sb.toString();
}

//virtual 
void 
GlobVarOp::execute_inner(IN(REvalEnv) env)
{
  switch (_op)
  {
  case OCO_LOADGLOB:
    env->loadGlob(_varName);
    break;
  case OCO_LOADGLOBREF:
    env->loadRefGlob(_varName);
    break;
  case OCO_STOREGLOB:
    env->storeGlob(_varName);
    break;
  case OCO_CREATEGLOB:
    if (_type == 0)
    {
      RDClazzInfo cls = env->compiler()->findType(_typeName, this);
      _type = cls->getImplClazzInfo();
    }
    env->createGlob(_varName, _type);
    break;
  default:
    THROW1(Exception, RString("Unknows GlobVarOp: ") + (int)_op);
  }
}

//virtual 
void 
GlobVarOp::printOpCode(IN(acdk::io::RPrintWriter) out)
{
  out->println(getLabelToPrint() + opToString(_op) + " " + _varName + getCommentToPrint());
}

void 
OpCodeStm1::execute_inner(IN(REvalEnv) env)
{
  switch (_op)
  {
  case OCO_PUSH:
    env->push(_val);
    break;
  default:
    THROW1(Exception, RString("Unknows OpCodeOp: ") + (int)_op);
  }
}

//virtual 
void 
OpCodeStm1::printOpCode(IN(acdk::io::RPrintWriter) out)
{
  out->println(getLabelToPrint() + opToString(_op) + " " + _val.toCode() + getCommentToPrint());
}

RString 
BranchOp::toString()
{
  StringBuffer sb;
  if (_label != Nil)
    sb << _label << ": ";
  sb << opToString(_op) << " " << _label << " (" << _pc << ")";
  if (_comment != Nil && _comment->length() > 0)
    sb << " // " << _comment;
  return sb.toString();
}

void 
BranchOp::execute_inner(IN(REvalEnv) env)
{
  switch (_op)
  {
  case OCO_BR:
    env->pc(_pc - 1);
    break;
  case OCO_BRFALSE:
    if (env->pop().getBoolVar() == false)
      env->pc(_pc - 1);
    break;
  case OCO_BRTRUE:
    if (env->pop().getBoolVar() == true)
      env->pc(_pc - 1);
    break;
  case OCO_RET:
    env->pc(-2);
    break;
  case OCO_TRY:
    env->af().enterTry(_pc);
    break;
  default:
    THROW1(Exception, RString("Unknows OpCodeOp: ") + (int)_op);
  }
}

void 
BranchOp::printOpCode(IN(acdk::io::RPrintWriter) out)
{
  out->println(getLabelToPrint() + opToString(_op) + " " + _label + getCommentToPrint());
}

RString 
OpCodeStm1::toString()
{
  StringBuffer sb;
  if (_label != Nil)
    sb << _label << ": ";
  sb << opToString(_op) << " " << _val.toCode();
  if (_comment != Nil && _comment->length() > 0)
    sb << " // " << _comment;
  return sb.toString();
}

void 
ObjectOp::execute_inner(IN(REvalEnv) env) // ### ObjectOp is superflous
{
  switch (_op)
  {
  default:
    THROW1(Exception, RString("Unknows AfOp: ") + (int)_op);
  }
}

void 
ObjectOp::printOpCode(IN(acdk::io::RPrintWriter) out)
{
  out->println(getLabelToPrint() + opToString(_op) + " " + _label + getCommentToPrint());
}

} // vm
} // aci
} // acdk




