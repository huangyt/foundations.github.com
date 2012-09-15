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
#if !defined(DOXYGENONLY)



#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/sys/core_value_scope.h>

#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/lang/dmi/MetaInfoChildsArray.h>
#include <acdk/lang/reflect/Method.h>
#include <acdk/lang/reflect/Enumeration.h>
#include <acdk/lang/dmi/DmiNamedArg.h>

#include "ScriptObject.h"
#include "Script.h"
#include "ScriptDebug.h"
#include "ScriptException.h"
#include "ScriptEval.h"
#include "ScriptExpr.h"


namespace acdk {
namespace cfgscript {



USING_CLASS(::acdk::io::, StreamTokenizer);
using namespace acdk::lang::reflect;
using namespace acdk::lang::dmi;


RObject getObjectWrapper(PEStack& stack, const acdk::lang::dmi::ScriptVar& sv)
{
  if (sv.isObjectType())
    return sv.getObjectVar();
  return new DmiObject(sv);
}


// helper functions
namespace {


using namespace acdk::lang::dmi;



void getArgs(PEStack& stack, ScriptVarArray& args, OUT(RStringArray) na)
{
  int paramsnum = stack.pop().val.getIntVar();
  int pnum = 0;
  int nnum = 0;
  int i;
  int j = 0;
  for (i = paramsnum - 1; i >= 0; --i, ++j)
  {
    PEVal v = stack.peek(j);
    if (v.tk == TT_ARGLABEL)
      ++nnum;
    else
      ++pnum;
  }
  args.resize(pnum);
  if (nnum != 0)
    na = new StringArray(nnum);
  //pnum = 0;
  //nnum = 0;
  for (i = paramsnum - 1; i >= 0; --i)
  {
    stack.resolveVarOnTop(ResolveToRef);
    PEVal v = stack.pop();
    if (v.tk == TT_ARGLABEL)
      na[--nnum] = v.val.getStringVar();
    else
      args[--pnum] = v.val;

  }
}



const ScriptVar& unwrapObject(const ScriptVar& sv)
{
  if (sv.isObjectType() && instanceof(sv.getObjectVar(), DmiObject) == true)
    return *RDmiObject(sv.getObjectVar());
  return sv;
}

RString ScriptVarArray_toString(ScriptVarArray& args)
{
  StringBuffer sb;
  sb.append("[");
  for (int i = 0; i < args.size(); ++i)
  {
    sb.append(args[i].getClazzInfo()->name);
    sb.append("=");
    sb.append(args[i].toCode());
    sb.append(" | ");
  }
  sb.append("]");
  return sb.toString();
}

} // anon namespace


void setScriptCastFlag(PEStack& stack, short flag, bool onOf)
{
  if (onOf == true)
  {
    stack.script->setCastFlags(stack.script->getCastFlags() | flag);
    stack.props->setCastFlags(stack.props->getCastFlags() | flag);
  }
  else
  {
    stack.script->setCastFlags(stack.script->getCastFlags() & ~flag);
    stack.props->setCastFlags(stack.props->getCastFlags() & ~flag);
  }

}

bool
PragmaParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  RString s = stack.curTokenAsString();
  if (s->equals("strict") == true)
  {
    stack.script->useStrict(true);
    return true;
  }
  if (s->equals("weak") == true)
  {
    stack.script->useStrict(false);
    return true;
  }
  if (s->equals("cast") == true)
  {

  /*
    #pragma cast c2i2c false
    #pragma cast f2i2f true
    #pragma cast b2n false
    #pragma cast n2b true
    #pragma cast o2b true
    #pragma cast autobox true
    #pragma cast checkoverflow false
    #pragma cast a2s false
    #pragma cast s2n false
  */
    tk = stack.nextToken();
    s = stack.curTokenAsString();
    tk = stack.nextToken();
    RString trueFalse = stack.curTokenAsString();
    bool on = false;
    if (trueFalse->equalsIgnoreCase("true") == true)
      on = true;
    else if (trueFalse->equalsIgnoreCase("false") == true)
      on = false;
    else
      ELOG("expect 'true' or 'false' after pragma cast <casttype>");

    if (s->equals("c2i2c") == true)
      setScriptCastFlag(stack, SVCastSVCastChar2Int, on);
    else if (s->equals("f2i2f") == true)
      setScriptCastFlag(stack, SVCastInt2Float, on);
    else if (s->equals("n2b") == true)
      setScriptCastFlag(stack, SVCastNum2Bool, on);
    else if (s->equals("b2n") == true)
      setScriptCastFlag(stack, SVCastBool2Number, on);
    else if (s->equals("o2b") == true)
      setScriptCastFlag(stack, SVCastObject2Bool, on);
    else if (s->equals("autobox") == true)
      setScriptCastFlag(stack, SVCastAutobox, on);
    else if (s->equals("checkoverflow") == true)
      setScriptCastFlag(stack, SVCastCheckOvervflow, on);
    else if (s->equals("a2s") == true)
      setScriptCastFlag(stack, SVCastEncodeString, on);
    else if (s->equals("s2n") == true)
      setScriptCastFlag(stack, SVCastDecodeString, on);
    else
      ELOG("unknown #pragma cast option: " + s);
  }
  else
    ELOG("Unknown pragma directive: " + stack.curTokenAsString());
  return true;
}

bool
PreProcParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (tk == StreamTokenizer::TT_OPERATOR)
  {
     RString cts = stack.curTokenAsString();
     if (cts->equals("!") == true || cts->equals("!/") == true)
     {
       tk = stack.nextToken();
       if (tk == StreamTokenizer::TT_WORD && stack.curTokenAsString()->equals("ignorenextline") == true)
       {
         stack.skipLine();
         stack.skipLine();
       }
       //else
       stack.pushBack();
       return stack.skipLine();
     }
  }
  if (tk != StreamTokenizer::TT_WORD)
    ELOG("expect identifier after '#'");
  if (stack.curTokenAsString()->equals("include") == true)
  {
    tk = stack.nextToken();
    if (tk != StreamTokenizer::TT_STRING)
      ELOG("expect string after '#include'");
    RString incl = stack.curTokenValue().getStringVar();
    if (stack.script->currentProps == Nil)
      stack.script->currentProps = stack.props;
    if (stack.script->include(incl, true) == false)
      ELOG("Including file failed: " + incl);
  }
  else if (stack.curTokenAsString()->equals("pragma") == true)
  {
   PARSEEXECUTE(PragmaParseNode().);
  }
  else
  {
    ELOG("Unknown preprocessor token: " + stack.curTokenAsString());
  }
  return true;
}



/*
  Stack enter
  -

  stack leave
  0 value parameter count
  1 - n parameter values
*/
bool
ParametersParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();

  if (tk != '(')
    ELOG("Expect '('");
  acdk::lang::sys::core_value_scope<RString> _stackOp(stack.leftOp, Nil);
  int topidx = stack.vs.size();
  while ((tk = stack.nextToken()) != ')')
  {
    RString paramname;
    if (tk == ':')
    {
      tk = stack.nextToken();
      paramname = stack.curTokenAsString();
      tk = stack.nextToken();
      if (stack.curTokenAsString()->equals("="))
        tk = stack.nextToken();
      stack.pushBack();
    }
    else
    {
      RString label = stack.curTokenAsString();
      tk = stack.nextToken();
      if (tk == ':')
      {
        paramname = label;
      }
      else
      {
        stack.pushBack();
        stack.pushBack();
      }
    }
    {
      acdk::lang::sys::core_value_scope<bool> evalOnStack(stack.evalAsRef, true);
      PARSEEXECUTE(ExpressionParseNode().);
    }
    if (paramname != Nil)
      stack.push(TT_ARGLABEL, inOf(paramname));
    stack.makeTopInOf();
    tk = stack.nextToken();
    if (tk == ')')
      break;
    if (tk != ',')
    {
      ELOG("Expect ',' for next Parameter");
    }
  }
  int newtop = stack.vs.size();
  stack.push(TT_VALUE, newtop - topidx);
  return true;
}



bool
NewExprParseNode::parseExecute(PEStack& stack)
{
  RString constructor = stack.parseIdentifier("");
  PARSEEXECUTE(ParametersParseNode().);
  ScriptVarArray args;
  RStringArray namedargs;
  getArgs(stack, args, namedargs);
  ASCLITERAL(atom);
  ASCLITERAL(Atom);
  if (constructor->equals(lit_atom) || constructor->equals(lit_Atom))
    constructor = "acdk.lang.dmi.DmiObject";
  RString classname = constructor->replace('.', '/');
  RString funcname = constructor;
  if (funcname->indexOf('/') != -1)
    funcname = funcname->substr(funcname->indexOf('/') + 1);
  ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient(stack.props->getCastFlags());
  DOUT("New: " << classname << "()");
  //classname = stack.getAliasOrUsingType(classname, false);
  ScriptVar erg = Object::New(stack.getAliasOrUsingTypeName(classname), args, namedargs, dmiclient);
  RObject tobj = erg.getObjectVar();
  stack.push(TT_VALUE, erg);
  return true;
}


bool
FuncOrMemberParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  RString m = stack.curTokenAsString();
  tk = stack.nextToken();
  if (tk == '(')
  {
    stack.pushBack();
    PEVal v = stack.pop();
    PARSEEXECUTE(ParametersParseNode().);
    ScriptVarArray args;
    RStringArray namedargs;
    getArgs(stack, args, namedargs);

    RObject o = getObjectWrapper(stack, v.val);
    DOUT("invoke: " << o->getClass()->getName() << "." << m << "()");
    ScriptVar erg;
      CFGSCRIPT_WRAPP_EXT_INVOKE(
        erg = unwrapObject(o->invokeMethod(m, args, namedargs));
      )
    stack.push(TT_VALUE, erg);
    return true;
  }
  else if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true)
  {

    //stack.pushBack();
    PEVal v = stack.pop();
    ScriptVar sv = v.val;
    if (v.tk == TT_IDENT)
      sv = inOf(stack.props->get(sv.getStringVar()));
    RObject o = getObjectWrapper(stack, sv);
    PARSEEXECUTE(ExpressionParseNode().);
    DOUT("Poke: " << o->getClass()->getName() << "." <<  m << " = " << stack.top().val.toCode());
    o->poke(m, stack.top().val);

  }
  else
  {
    stack.pushBack();
    PEVal v = stack.pop();
    ScriptVar sv = v.val;
    if (v.tk == TT_IDENT)
      sv = inOf(stack.props->get(sv.getStringVar()));
    RObject o = getObjectWrapper(stack, sv);
    DOUT("Peek: " << o->getClass()->getName() << "." <<  m);
    stack.push(TT_VALUE, unwrapObject(o->peek(m, MiAiIn)));

    return true;
  }
  return true;
}


bool
OperatorCall::parseExecute(PEStack& stack)
{
  RString op = stack.pop().val.getStringVar();
  op = acdk::lang::reflect::Method::encodeOperatorToFuncName(op);
  PEVal objv = stack.pop();
  PARSEEXECUTE(ExpressionParseNode().);
  ScriptVarArray args(1);
  stack.resolveVarOnTop();
  args[0] = stack.pop().val;
  RObject o = getObjectWrapper(stack, objv.val);
  DOUT("invoke op: " << o->getClass()->getName() << "." << op << "()");
  ScriptVar erg;
  CFGSCRIPT_WRAPP_EXT_INVOKE(
  erg = unwrapObject(o->invokeMethod(op, args));
  )
  stack.push(TT_VALUE, erg);
  return true;
}



bool
StaticFuncOrMemberParseNode::parseExecute(PEStack& stack)
{
  RString s = stack.pop().val.getStringVar();
  s = stack.getAliasOrUsingTypeName(s); // ### todo seach internal for MetaInfo here and a few lines later to
  const ClazzInfo* ci = 0;
  int tk;
  do {
    const MetaInfo* mi = MetaInfo::findMetaInfo(s);
    if (mi != 0)
    {
      if (mi->isEnumValInfo() == true)
      {
        const ClazzEnumValueInfo* evi = reinterpret_cast<const ClazzEnumValueInfo*>(mi);
        stack.push(TT_VALUE, inOf(evi->value));
        return true;
      }
      else if (mi->isClazzInfo() == true)
      {
        ci = reinterpret_cast<const ClazzInfo*>(mi);
        break;
      }
      else if (mi->isUnitInfo() == true)
      {
        ; //nothing
      }
      else {
        ;
      }
    }
    tk = stack.nextToken();
    if (tk == StreamTokenizer::TT_OPERATOR)
    {
      if (stack.curTokenAsString()->equals(".") == true)
      {
        tk = stack.nextToken();
        if (tk != StreamTokenizer::TT_WORD)
          ELOG("Expect identifier after " + s + ".");
        s = s + "/" + stack.curTokenAsString();
        continue;
      }
      else
      {
        ELOG("Unknown type: " + s);
      }
    }
    else
    {
      ELOG("Unknown type: " + s);
    }
  } while (tk != StreamTokenizer::TT_EOF);
  if (ci != 0)
  {
    int tk = stack.nextToken();
    if (stack.curTokenAsString()->equals(".") != true)
      ELOG("Expect '.' after class name " + s + ".");
    tk = stack.nextToken();
    if (tk != StreamTokenizer::TT_WORD)
      ELOG("Expect identifier after " + s + ".");
    RString m = stack.curTokenAsString();
    tk = stack.nextToken();
    if (tk == '(')
    {
      stack.pushBack();
      PARSEEXECUTE(ParametersParseNode().);
      ScriptVarArray args;
      RStringArray namedargs;
      getArgs(stack, args, namedargs);
      ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient(stack.props->getCastFlags());
      DOUT("invoke_static: " << s << "." <<  m << "()");
      CFGSCRIPT_WRAPP_EXT_INVOKE(
      stack.push(TT_VALUE, unwrapObject(StdDispatch::invokeStaticMethod(s, m, args, dmiclient, namedargs)));
      )
      return true;
    }

    else if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true)
    {
      PARSEEXECUTE(ExpressionParseNode().);
      stack.resolveVarOnTop();
      DOUT("Poke_static: " << s << "." <<  m << " = " << stack.top().val.toCode());
      StdDispatch::poke_static(s, m, stack.top().val);

      return true;
    }
    else //if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals(".") == true)
    {
      stack.pushBack();
      DOUT("Peek_static: " << s << "." <<  m);
      ScriptVar erg = StdDispatch::peek_static(s, m);
      stack.push(TT_VALUE, unwrapObject(erg));
      return true;
    }
  }
  return false;
}


bool
pushCurToken(PEStack& stack)
{
  int tk = stack.curToken();
  if (tk == StreamTokenizer::TT_NUMBER || tk == StreamTokenizer::TT_STRING || tk == StreamTokenizer::TT_QCHAR)
    stack.push(TT_VALUE, stack.curTokenValue());
  else if (tk == StreamTokenizer::TT_WORD)
    stack.push(TT_IDENT, stack.curTokenValue());
  else if (tk == StreamTokenizer::TT_OPERATOR)
    stack.push(TT_OPERATOR, stack.curTokenValue());
  else
  {
    ELOG("unknown token type to push");
  }
  return true;
}

bool
tryResolveImportedMethods(PEStack& stack)
{
  RString methodName = stack.pop().val.getStringVar();
  const MetaInfo* mi = 0;
  RUsingEntry ue = stack.findUsingChild(methodName, MiMethodInfo, &mi);
  if (ue != Nil)
  {
    if (ue->isVar == true)
    {
      stack.push(TT_IDENT, inOf(ue->usingName));
      stack.resolveVarOnTop();
      RObject obj = getObjectWrapper(stack, stack.pop().val);
      PEVal dotop(TT_DOTOP, PEVal(TT_VALUE, inOf(obj)), PEVal(TT_IDENT, inOf(methodName)));
      stack.push(dotop);
      PARSEEXECUTE(ExecuteMethodParseNode().);
      return true;
    }
    else
    {
      
      PEVal dotop(TT_DOTOP, PEVal(TT_IDENT, inOf(ue->usingName)), PEVal(TT_IDENT, inOf(methodName)));
      stack.push(dotop);
      PARSEEXECUTE(ExecuteMethodParseNode().);
      return true;
    }
  }
  ELOG("Cannot find object or class for method name: " + methodName);
  return false;
}


/**
  Stack enter:
  0 Method Name
  1 Argnum
  2 - n args
  n + 1 identifier | value

  or
  0 ns.class.method
  1 Argnum
  2 - n args

  stack leave
  0 method return value
*/

bool
ExecuteMethodParseNode::parseExecute(PEStack& stack)
{
  PEVal op = stack.pop();
  if (op.tk != TT_DOTOP)
  {
    RString methodName = op.toCode();
    if (methodName->equals("synchronize") == true)
    {
      ScriptVarArray args;
      RStringArray namedargs;
      getArgs(stack, args, namedargs);
      RObject obj = args[0].getObjectVar();
      SYNCOBJECT(obj);
      PARSEEXECUTE(StatementParseNode().);
      return true;
    }

    stack.push(TT_IDENT, inOf(methodName));
    tryResolveImportedMethods(stack);
    return true;

  }
  RString methodName = op.args->right.val.getStringVar();

  ScriptVarArray args;
  RStringArray namedargs;
  getArgs(stack, args, namedargs);


  RObject object;
  RString className;
  stack.push(op.args->left);
  //className = stack.top().val.getStringVar();
  bool viaObject = false;
  bool superCall = false;
  RObject superObj;
  const acdk::lang::dmi::ClazzInfo* ci = 0;
  ScriptVar orgObjectVar;
  if (stack.resolveVarOnTop(ResolveNothing) == true)
  {
    orgObjectVar = stack.pop().val;
    object = orgObjectVar.getObjectVar(stack.props->getCastFlags());
    viaObject = true;
    if (stack.props->hasValue(lit_this) == true)
    {
      RScriptObject thisO = (RScriptObject)stack.props->getObjectVal(lit_this);
      if (thisO->getSuperObject() == object && thisO != object)
      {
        superObj = thisO->getSuperObject();
        superCall = true;
        ci = superObj->clazzInfo();
      }
    }
  }
  else
  {
    className = stack.pop().val.getStringVar();
    if (stack.props->hasValue(lit_this) == true)
    {
      RScriptObject thisO = (RScriptObject)stack.props->getObjectVal(lit_this);
      if (className->equals(lit_super) == true)
      {
        superObj = thisO->getSuperObject();
        superCall = true;
      }
      else
      {
        ci = Class::forName(className)->objectClazzInfo();
        if (ci->assignableFrom(thisO->getClazzInfo()) == true)
        {
          superCall = true;
          superObj = &thisO;//->getSuperObject();
          if (ci != thisO->getClazzInfo())
            superObj = thisO->getSuperObject();
        }
      }
    }
  }

  ScriptVar erg;
  int callflags = MiPublic;
  if (viaObject == true || superCall == true)
  {

    if (viaObject == false)
    {
      object = superObj;
      if (className->equals(lit_super) == true)
        ci = superObj->getClazzInfo();
      else
        ci = Class::forName(className)->objectClazzInfo();
    }
    else
      ci = object->getClazzInfo();
    if (superCall == true)
    {
      callflags &= ~MiPublic;
      callflags = MiIvTransientCall;
      if (RString(ci->name)->endsWith("_DmiProxy") == true)
      {
        ci = ci->interfaces[0]->type;
        //callflags |= MiIvNoWeakBind;
      }
      //else if ((ci->flags & MiCiWeakBind) == false)
      callflags |= MiIvNoWeakBind;
    }
    const ClazzInfo* orgCi = orgObjectVar.getClazzInfo();
    ASCLITERAL(GetClass); ASCLITERAL(getClass);

    if (orgCi != 0 && orgCi->isBasicClazz() == true && (methodName->equals(lit_GetClass) == true || methodName->equals(lit_getClass) == true))
    {
      // ### todo this if branch is only handled here. but there are much more invokeMethod calls in this source
      erg = inOf(Class::getSingeltonClass(orgCi));
    }
    else
    {
      DOUT("invoke : " << object->getClass()->getName() << "." << methodName << "(...)");
      ScriptVar ret;
      CFGSCRIPT_WRAPP_EXT_INVOKE(
        object->standardDispatch(methodName, ret, args, getDmiClient(), namedargs, callflags, ci, 0);
      )
      erg = unwrapObject(ret);
    }
  }
  else
  {
    //const ClazzInfo* ci = ClazzInfo::findClazzInfo(className);
    const ClazzInfo* ci = stack.findType(className, false);
    if (ci == 0)
    {
      RString tstr = className;
      int idx = 0;
      while (idx != -1)
      {
        idx = tstr->lastIndexOf('.');
        if (idx != -1)
        {
          RString rs = tstr->substr(idx + 1);
          tstr = tstr->substr(0, idx);
          //ci = ClazzInfo::findClazzInfo(tstr);
          ci = stack.findType(tstr);
          RString rp = className->substr(tstr->length() + 1);
          stack.push(TT_IDENT, inOf(rp));
          stack.push(TT_IDENT, inOf(tstr));
          PARSEEXECUTE(ExecStaticPeek().);
          stack.push(TT_IDENT, inOf(methodName));
          PARSEEXECUTE(ExecuteMethodParseNode().);
          return true;
        }
      }
      ELOG("Cannot resolve classname or variable: " + className);
    }

    DOUT("invoke_static : " << className << "." << methodName << "(...)");
    ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient(stack.props->getCastFlags());
    className = stack.getAliasOrUsingTypeName(className);
    ASCLITERAL(GetClass);
    if (methodName->equals(lit_GetClass) == true)
    {
      if (ci == ClazzInfo::getCharClazz() || 
          ci == ClazzInfo::getUcCharClazz() ||
          ci == ClazzInfo::getBoolClazz() ||
          ci == ClazzInfo::getByteClazz() ||
          ci == ClazzInfo::getShortClazz() ||
          ci == ClazzInfo::getIntClazz() ||
          ci == ClazzInfo::getLongClazz() ||
          ci == ClazzInfo::getFloatClazz() ||
          ci == ClazzInfo::getDoubleClazz()
          )
      {
        stack.push(TT_VALUE, inOf(Class::getSingeltonClass(ci)));
        return true;
      }
      
    }
    CFGSCRIPT_WRAPP_EXT_INVOKE(
    erg = unwrapObject(invokeStaticMethod(className, methodName, args, dmiclient, namedargs));
    )
  }
  stack.push(TT_VALUE, erg);
  return true;

}

/**
  stack before
  0 left val or identifier
  1 member name
*/

bool
ExecPeek::parseExecute(PEStack& stack)
{
  if (stack.resolveVarOnTop(ResolveNothing | ResolveNoCompIdent | ResolveNotInUsings) == true)
  {
    PEVal objval = stack.pop();
    if (stack.top().tk == TT_DOTOP)
      stack.resolveVarOnTop(ResolveNothing);
    PEVal member = stack.pop();
    if (member.tk == TT_DOTOP)
    {
      ELOG("Unimplemented dotop in peek");
    }
    RString memberName = member.val.getStringVar();
    int midx = memberName->indexOf('.');
    if (midx != -1)
    {
      RString lm = memberName->substr(0, midx);
      RString rm = memberName->substr(midx + 1);
      stack.push(TT_IDENT, inOf(lm));
      stack.push(objval);
      PARSEEXECUTE(ExecPeek().);
      objval = stack.pop();
      stack.push(TT_IDENT, inOf(rm));
      stack.push(objval);
      PARSEEXECUTE(ExecPeek().);
      return true;
    }
    RObject obj = getObjectWrapper(stack, objval.val);
    // ### todo try ...
    DOUT("Peek: " << obj->getClass()->getName() << "." <<  memberName );
    stack.push(TT_VALUE, unwrapObject(obj->peek(memberName)));
    return true;
  }
  PARSEEXECUTE(ExecStaticPeek().);
  return true;
}

/**
  stack before
  0 classname
  1 membername
*/
bool
ExecStaticPeek::parseExecute(PEStack& stack)
{
  RString className = stack.pop().val.getStringVar();
  className = stack.getAliasOrUsingTypeName(className);

  RString memberName = stack.pop().val.getStringVar();
  DOUT("Peek_static: " << className << "." <<  memberName);
  // ### @todo try
  stack.push(TT_VALUE, unwrapObject(StdDispatch::peek_static(className, memberName)));
  return true;
}

/**
  0 right value
  1 member name
  2 left identifier or value
*/
bool
ExecPoke::parseExecute(PEStack& stack)
{
  PEVal val = stack.pop();
  RString memberName = stack.pop().val.getStringVar();
  PEVal objOrClassVal = stack.pop();
  int lps = -1;
  RString objOrClass;
  if (objOrClassVal.tk == TT_IDENT)
  {
    objOrClass = objOrClassVal.val.getStringVar();
    lps = objOrClass->indexOf('.');
  }
  if (lps == -1)
  {
    stack.push(objOrClassVal);
    RObject obj;
    if (stack.resolveVarOnTop(ResolveNothing) == true)
    {
      obj = getObjectWrapper(stack, stack.pop().val);
    }
    if (obj != Nil)
    {
      // ### todo try ...
      DOUT("Poke: " << obj->getClass()->getName() << "." <<  memberName << " = " << val.toCode());
      obj->poke(memberName, val.val);
      stack.push(val);
      return true;
    }
    RString className = stack.pop().val.getStringVar();
    className = stack.getAliasOrUsingTypeName(className);
    StdDispatch::poke_static(className, memberName, val.val);
    stack.push(val);
    return true;
  }
  RString lp = objOrClass->substr(0, lps);
  RString rst = objOrClass->substr(lps + 1);
  stack.push(TT_IDENT, inOf(lp));
  if (stack.resolveVarOnTop(ResolveNothing) == true)
  {
    PEVal tval = stack.pop();
    stack.push(TT_IDENT, inOf(rst));
    stack.push(tval);
    PARSEEXECUTE(ExecPeek().);
    PEVal nval = stack.pop();
    stack.push(nval);
    stack.push(TT_IDENT, inOf(memberName));
    stack.push(val);
    PARSEEXECUTE(ExecPoke().);
    return true;
  }
  else
  {
    stack.pop();
    RString className = objOrClass;
    className = stack.getAliasOrUsingTypeName(className);
    StdDispatch::poke_static(className, memberName, val.val);
    return true;
  }
  return false;
}

/*


  i = i.var + j.var * k.var;
  = .
    -> right
    . +
    -> left
    + .
      -> right
    . *
      -> left
    * .
      -> right

  i = i - j * k | 3
  = -
    -> right
    - *
      -> right
      * |
      -> left
    - |
      left

 */

RString seekNextOperator(PEStack& stack)
{
  int ctokenIdx = stack.getCurTokenIndex();
  int tk;
  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (isEndExpr(stack.curSourceToken()) == true)
    {
      stack.setCurTokenIndex(ctokenIdx);
      return Nil;
    }
    if (isOperator(stack.curSourceToken()) == true)
    {
      RString ret = stack.curTokenAsString();
      stack.setCurTokenIndex(ctokenIdx);
      return ret;
    }
  }
  stack.setCurTokenIndex(ctokenIdx);
  return Nil;
}

const OpDescription& getOperatorDesc(INP(RString) op);

bool
shallExecuteRight(IN(RString) left, IN(RString) right)
{
  OpDescription opd1 = getOperatorDesc(left);
  OpDescription opd2 = getOperatorDesc(right);
  DOUT("Compare Ops: " << opd1 << " <=> " << opd2);
  if ((opd1.prio - opd2.prio) > 0)
    return true;
  return false;
}


bool
shallExecuteRight(IN(RString) op1, PEStack& stack)
{
  RString op2 = seekNextOperator(stack);
  if (op2 == Nil)
    return false;
  return shallExecuteRight(op1, op2);
  /*
  OpDescription opd1 = getOperatorDesc(op1);
  OpDescription opd2 = getOperatorDesc(op2);
  DOUT("Compare Ops: " << opd1 << " <=> " << opd2);
  if ((opd1.prio - opd2.prio) > 0)
    return true;
  return false;
  */
}

bool handleBuildUnaryOp(PEStack& stack, IN(RString) op, ScriptVar& sv1, OUT(bool) berg)
{
  if (sv1.isObjectType() == true)
    return false;
  berg = true;
  int cf = stack.props->getCastFlags();
  
  if (op->length() == 1)
  {
    switch(op->charAt(0))
    {
    case '!':
      stack.push(TT_VALUE, sv1.logical_not(cf));
      return true;
    case '~':
      stack.push(TT_VALUE, sv1.binary_not(cf));
      return true;
    }
  }
  else if (op->length() == 2)
  {
    int hash = op->hashCode();
    switch(hash)
    {
    case StringHash<'+','+'>::hash:
      sv1 = sv1.addition(1, cf);
      stack.push(TT_VALUE, sv1);
      return true;
    case StringHash<'-','-'>::hash:
      sv1 = sv1.subtraction(1, cf);
      stack.push(TT_VALUE, sv1);
      return true;
    }
  }
  return false;
}

bool handleBuildBinaryOp(PEStack& stack, IN(RString) op, ScriptVar& sv1, const ScriptVar& sv2, bool& ret)
{
  if (sv1.isObjectType() == true)
    return false;
  int cf = stack.props->getCastFlags();
  ret = true;
  if (op->length() == 1)
  {
    switch(op->charAt(0))
    {
    case '+':
      stack.push(TT_VALUE, sv1.addition(sv2, cf));
      return true;
    case '-':
      stack.push(TT_VALUE, sv1.subtraction(sv2, cf));
      return true;
    case '*':
      stack.push(TT_VALUE, sv1.multiply(sv2, cf));
      return true;
    case '/':
      stack.push(TT_VALUE, sv1.divide(sv2, cf));
      return true;
    case '%':
      stack.push(TT_VALUE, sv1.modulo(sv2, cf));
      return true;
    case '<':
      stack.push(TT_VALUE, sv1.less_than(sv2, cf));
      return true;
    case '>':
      stack.push(TT_VALUE, sv1.greater_than(sv2, cf));
      return true;
     case '&':
      stack.push(TT_VALUE, sv1.binary_and(sv2, cf));
      return true;
    case '|':
      stack.push(TT_VALUE, sv1.binary_or(sv2, cf));
      return true;
    case '^':
      stack.push(TT_VALUE, sv1.binary_xor(sv2, cf));
      return true;
     
    }
  }
  else if (op->length() == 2)
  {
    int hash = op->hashCode();
    switch(hash)
    {
    case StringHash<'>','='>::hash:
      stack.push(TT_VALUE, sv1.greater_or_equal(sv2, cf));
      return true;
    case StringHash<'<','='>::hash:
      stack.push(TT_VALUE, sv1.less_or_equal(sv2, cf));
      return true;
    case StringHash<'=','='>::hash:
      stack.push(TT_VALUE, sv1.equal(sv2, cf));
      return true;
    case StringHash<'!','='>::hash:
      stack.push(TT_VALUE, sv1.not_equal(sv2, cf));
      return true;
    case StringHash<'&','&'>::hash:
      stack.push(TT_VALUE, sv1.logical_and(sv2, cf));
      return true;
    case StringHash<'|','|'>::hash:
      stack.push(TT_VALUE, sv1.logical_or(sv2, cf));
      return true;
    case StringHash<'<','<'>::hash:
      stack.push(TT_VALUE, sv1.binary_leftshift(sv2, cf));
      return true;
    case StringHash<'>','>'>::hash:
      stack.push(TT_VALUE, sv1.binary_rightshift(sv2, cf));
      return true;
    case StringHash<'+','+'>::hash:
    {
      ScriptVar sv = sv1;
      sv1 = sv1.addition(1, cf);
      stack.push(TT_VALUE, sv);
      return true;
    }
    case StringHash<'-','-'>::hash:
    {
      ScriptVar sv = sv1;
      sv1 = sv1.subtraction(1, cf);
      stack.push(TT_VALUE, sv);
      return true;
    }
    }
  }
  return false;
}


/**
  Stack on enter
  0 = op
  1 = right arg
  2 = left arg
  stack on leave
  0 = value
*/
bool
ExecBinaryOp::parseExecute(PEStack& stack)
{
  PEVal op = stack.pop();

  RString opstr = op.val.getStringVar();



  // assignment
  if (opstr->equals("=") == true)
  {
    stack.resolveVarOnTop();
    PEVal val = stack.pop();
    PEVal name = stack.pop();
    if (name.tk == TT_IDENT)
    {
      RString n = name.val.getStringVar();
      int nidx = n->lastIndexOf('.');
      if (nidx != -1)
      {
        RString ls = n->substr(0, nidx);
        RString rs = n->substr(nidx + 1);
        stack.push(TT_IDENT, inOf(ls));
        stack.push(TT_IDENT, inOf(rs));
        stack.push(val);
        PARSEEXECUTE(ExecPoke().);
        return true;
      }
      DOUT("Assign " << n << "=" << val.val.toCode());
      if (stack.script->useStrict() == true)
        stack.props->assign(n, new DmiObject(val.val.inOf()));
      else
      {
        if (stack.props->hasValue(n) == true)
          stack.props->assign(n, new DmiObject(val.val.inOf()));
        else
          stack.props->set(n, new DmiObject(val.val.inOf()));
      }

    }
    else if (name.tk == TT_REFVALUE)
    {
      name.val = val.val;
    }
    else if (name.tk == TT_DOTOP)
    {
      PEVal rval = name.args->left;
      PEVal namev = name.args->right;
      RString name = namev.val.getStringVar();
      stack.push(rval);
      if (stack.resolveVarOnTop(ResolveNothing) == false)
      {
        // static
        PEVal rval = stack.pop();
        stack.push(rval);
        stack.push(TT_IDENT, inOf(name));
        stack.push(val);
        PARSEEXECUTE(ExecPoke().);

      }
      else
      {
        // 0 right value
        // 1 member name
        // 2 left identifier or value
        PEVal rval = stack.pop();
        stack.push(rval);
        stack.push(TT_IDENT, inOf(name));
        stack.push(val);
        PARSEEXECUTE(ExecPoke().);
        return true;
      }
    }
    else
    {
      ELOG("Cannot assign");
    }
    stack.push(val);
    return true;
  }
  else if (opstr->equals(".") == true)
  {
    RString nameOrMethod = stack.pop().val.getStringVar();
    if (stack.top().tk == TT_IDENT)
    {
      stack.resolveVarOnTop(ResolveNothing);
    }
    stack.push(PEVal(TT_DOTOP, stack.pop(), PEVal(TT_IDENT, inOf(nameOrMethod))));
    return true;

  }
  else if (opstr->equals("instanceof") == true)
  {
    PEVal cl =  stack.pop();
    RString clsname = cl.toCode();
    clsname = stack.parseIdentifier(clsname);
    stack.resolveVarOnTop();
    ScriptVar sv = stack.pop().val;
    const ClazzInfo* to = stack.findType(clsname);
    if (sv.isObjectType() == true)
    {
      if (sv.getObjectVar(stack.props->getCastFlags())->_cast(to) != 0)
        stack.push(TT_VALUE, inOf(true));
      else
        stack.push(TT_VALUE, inOf(false));
      return true;
    }
    stack.push(TT_VALUE, inOf(to->assignableFrom(sv.getClazzInfo())));
    return true;
  }
  stack.resolveVarOnTop();
  PEVal arg = stack.pop();
  stack.resolveVarOnTop();
  PEVal objv = stack.pop();

  if (opstr->equals("==") == true || opstr->equals("!=") == true ||
      opstr->equals("operator_eq_eq") == true || opstr->equals("operator_nt_eq") == true)
  {
    if (objv.val.isObjectType() == true && arg.val.isObjectType())
    {
      RObject first = objv.val.getObjectVar();
      RObject sec = arg.val.getObjectVar();
      if (opstr->equals("==") == true)
        stack.push(TT_VALUE, inOf(bool(first == sec)));
      else
        stack.push(TT_VALUE, inOf(bool(first != sec)));
      return true;
    }
  }
  bool berg = true;
  if (handleBuildBinaryOp(stack, opstr, objv.val, arg.val, berg) == true)
    return berg;

  opstr = acdk::lang::reflect::Method::encodeOperatorToFuncName(opstr);

  ScriptVarArray args(1);
  args[0] = arg.val;
  RObject o = getObjectWrapper(stack, objv.val);
  DOUT("invoke op: " << o->getClass()->getName() << "." << opstr << "(" << arg.val.toCode() << ")");
  ScriptVar erg;
  CFGSCRIPT_WRAPP_EXT_INVOKE(
  erg = unwrapObject(o->invokeMethod(opstr, args));
  )
  stack.push(TT_VALUE, erg);
  return true;

}

/*
  +a
  (a + 1)
  stack enter
  0 op

  stack leave
  value
*/
bool
ExecUnaryPrefixOp::parseExecute(PEStack& stack)
{
  // handle . as .with statement
  PEVal op = stack.pop();
  RString opstr = op.toCode();
  ASCLITERAL(new);
  if (opstr->equals(lit_new) == true)
  {
    PARSEEXECUTE(NewExprParseNode().);
    return true;
  }
  else if (opstr->equals(".") == true)
  {
    stack.pop();
    ScriptVar sv = *stack.props->get(".with");
    stack.push(TT_VALUE, sv);
    stack.pushBack(); // member
    stack.pushBack(); // .
    //PARSEEXECUTE(ExpressionParseNode2().);
    return true;
  }

  //PEVal valOrId = stack.pop();
  stack.resolveVarOnTop(ResolveToRef);
  PEVal objval = stack.pop();

  bool berg = true;
  if (handleBuildUnaryOp(stack, opstr, objval.val, berg) == true)
    return berg;

  opstr = acdk::lang::reflect::Method::encodeOperatorToFuncName(opstr);
  
  RObject o = getObjectWrapper(stack, objval.val);
  ScriptVar erg;
  ScriptVarArray args;
  CFGSCRIPT_WRAPP_EXT_INVOKE(
    erg = unwrapObject(o->invokeMethod(opstr, args));
  )

  stack.push(TT_VALUE, erg);

  return true;
}

/*
  a--
  a(x)
  Stack enter:
  0 op
  1 left token
  Stack leave
  0 value
*/
bool
ExecUnarySuffixOp::parseExecute(PEStack& stack)
{
  RString op = stack.pop().toCode();
  PEVal lefttk = stack.pop();
  ScriptVar opArg;
  if (op->equals("(") == true)
  {


    stack.pushBack();
    PARSEEXECUTE(ParametersParseNode().);
    RString s;
    int idx = 0;
    if (lefttk.tk == TT_IDENT && (s = lefttk.val.getStringVar()) != Nil && (idx = s->lastIndexOf('.')) != -1)
    {
      RString lp = s->substr(0, idx);
      RString rp = s->substr(idx + 1);
      stack.push(TT_IDENT, inOf(lp));
      stack.push(TT_IDENT, inOf(rp));
    }
    else
      stack.push(lefttk);
    PARSEEXECUTE(ExecuteMethodParseNode().);
    return true;
    // arguments
  }
  else if (op->equals("[") == true)
  {
    //stack.pushBack();
    PARSEEXECUTE(ExpressionParseNode().);
    int tk = stack.nextToken();
    if (tk != ']')
      ELOG("Except ']'");
    stack.resolveVarOnTop();
    opArg = stack.pop().val;
    op = op + "]";
  }
  RString opstr = acdk::lang::reflect::Method::encodeOperatorToFuncName(op);
  stack.push(lefttk);
  stack.resolveVarOnTop(ResolveToRef);
  PEVal objval = stack.pop();
  RObject o = getObjectWrapper(stack, objval.val);
  ScriptVar erg;
  ScriptVarArray args;
  if (op->equals("[]") == true)
    args.push_back(opArg);
  else
    args.push_back(inOf(int(1)));
  CFGSCRIPT_WRAPP_EXT_INVOKE(
  erg = unwrapObject(o->invokeMethod(opstr, args));
  )
  stack.push(TT_VALUE, erg);
  return true;
}


/**
  stack before:
  0 Value;
*/
bool
OptionalSuffixOp::parseExecute(PEStack& stack)
{
  int tokenIndex = stack.getCurTokenIndex();
  int otk = stack.nextToken();
  if (isOperator(stack.curSourceToken()) == true)
  {
    if (otk == '(')
    {
      // noting Calling
      //DOUT("Calling");

    }
    else if (otk == '[')
    {
      // nothing
    }
    else
    {
      RString op1 = stack.curTokenAsString();
      int opflags = getOperatorFlags(op1);
      if ((opflags & OpSuffix) == OpSuffix)
      {
        stack.push(TT_OPERATOR, inOf(op1));
        PARSEEXECUTE(ExecUnarySuffixOp().);
        PARSEEXECUTE(OptionalSuffixOp().);
        return true;
      }
    }
    stack.setCurTokenIndex(tokenIndex);
  }
  else
  {
    stack.setCurTokenIndex(tokenIndex);
  }
  return true;
}

bool
ArrayLiteralParseNode::parseExecute(PEStack& stack)
{
  RDmiObjectArray oa;
  RObject dmiOa;
  if (stack.curLeftDeclType == 0)
    oa = new DmiObjectArray(0);
  else
  {
    RClass cls = Class::getSingeltonClass(stack.curLeftDeclType);
    dmiOa = (RObject)Object::New(cls->getName(), inOf(0));
  }
  stack.curLeftDeclType = 0; // to enable type safe nested arrays
  int tk = 0;
  tk = stack.nextToken();
  if (tk == ']')
  {
  	ScriptVar sv = oa != Nil ? inOf(oa) : inOf(dmiOa);
    stack.push(TT_VALUE, sv);
    return true;
  }
  stack.pushBack();
  do {
    PARSEEXECUTE(ExpressionParseNode().);
    if (oa != Nil)
      oa->append(new DmiObject(stack.pop().val.inOf()));
    else
      dmiOa->invoke("append", stack.pop().val.inOf());
    tk = stack.nextToken();

    if (tk == ',')
      continue;
    if (tk == ']')
    {
    	ScriptVar sv = oa != Nil ? inOf(oa) : inOf(dmiOa);
      stack.push(TT_VALUE, sv);
      return true;
    }
    ELOG("In ArrayLiteral: Expecting ',' or ']'");
  } while (true);
  return false;
}

bool
BacktickParseNode::parseExecute(PEStack& stack)
{
  int otk = stack.nextToken();
  if (otk != StreamTokenizer::TT_STRING)
    ELOG("expect string literal after operator `");

  RString evalstr = stack.curTokenValue().getStringVar();
  RString evaluated = stack.props->eval(evalstr);
  otk = stack.nextToken();
  if (otk != '`')
    ELOG("expect ` after `<stringliteral>");
    //return new ScriptExpr(OpVarOrIdent, 
  stack.push(TT_VALUE, inOf(evaluated));
  return true;
}

bool
DictLiteralParseNode::parseExecute(PEStack& stack)
{
  RProps props = new Props();
  props->setCastFlags(stack.script->getCastFlags());
  int tk = 0;
  tk = stack.nextToken();
  if (tk == '}')
  {
    stack.push(TT_VALUE, inOf(props));
    return true;
  }
  stack.pushBack();
  stack.curLeftDeclType = 0;
  do {
    tk = stack.nextToken();
    if (tk != StreamTokenizer::TT_WORD)
      ELOG("Props litaral: expect key identifier");

    RString ident = stack.curTokenAsString();
    tk = stack.nextToken();
    if (tk != ':')
      ELOG("Props litaral: expect ':' after key identifier");

    PARSEEXECUTE(ExpressionParseNode().);
    props->set(ident, new DmiObject(stack.pop().val.inOf()));
    tk = stack.nextToken();
    RString op1 = stack.curTokenAsString();
    if (tk == ',')
      continue;
    if (tk == '}')
    {
      stack.push(TT_VALUE, inOf(props));
      return true;
    }
    ELOG("In Props: Expecting ',' or '}'");
  } while (true);
  return false;
}




/**
  tokenizer before suffix op
  stack before:
  0 left alue


  stack after
  0 value
*/
bool
SuffixOp::parseExecute(PEStack& stack)
{
  int otk = stack.nextToken();
  if (isOperator(stack.curSourceToken()) == false)
    ELOG("Expect suffix operator");
  RString op1 = stack.curTokenAsString();
  int opflags = getOperatorFlags(op1);
  if ((opflags & OpSuffix) != OpSuffix)
    ELOG("Expect suffix operator");
  stack.push(TT_OPERATOR, inOf(op1));
  PARSEEXECUTE(ExecUnarySuffixOp().);
  PARSEEXECUTE(OptionalSuffixOp().);
  return true;
}

/**
  reads atom with pre and/or postfixes
  Scanner will be before next op/variable
  stack before:
  -
  stack after
  0 Variable
*/
bool
AtomPreOrSuffixOp::parseExecute(PEStack& stack)
{
  int otk = stack.nextToken();
  if (otk == -1)
    ELOG("Unexpected EOF");
  RString op1 = stack.curTokenAsString();
  ASCLITERAL(delegate);
  ASCLITERAL(lambda);
  
  if (op1->equals(lit_delegate) == true)
  {
    PARSEEXECUTE(DelegateParseNode().);
    return true;
  }
  if (op1->equals(lit_lambda) == true)
  {
    PARSEEXECUTE(LambdaParseNode().);
    return true;
  }
  if (isOperator(stack.curSourceToken()) == true)
  {
    int opflags = getOperatorFlags(op1);
    if ((opflags & OpOutfix) == OpOutfix)
    {
      if (otk == '(')
      {
        acdk::lang::sys::core_value_scope<RString> _stackOp(stack.leftOp, Nil);
        PARSEEXECUTE(ExpressionParseNode().);
        otk = stack.nextToken();
        if (otk != ')')
          ELOG("Expect ')'");
        PARSEEXECUTE(ExpressionParseNode2().);
        return true;
      }
      else if (otk == '`')
      {
        PARSEEXECUTE(BacktickParseNode().);
        return true;
      }
      else if (op1->equals("[") == true)
      {
        PARSEEXECUTE(ArrayLiteralParseNode().);
        return true;
      }
      else if (op1->equals("{") == true)
      {
        PARSEEXECUTE(DictLiteralParseNode().);
        return true;
      }
      
    }
    if ((opflags & OpPrefix) != OpPrefix)
      ELOG("Unexpected Operator (no prefix or outfix operator)");
    ASCLITERAL(new);
    if (op1->equals(lit_new) == false)
    {
      PARSEEXECUTE(AtomPreOrSuffixOp().); // ### @todo only parse additionally optional Prefixes
    }
    stack.push(TT_OPERATOR, inOf(op1));
    PARSEEXECUTE(ExecUnaryPrefixOp().);
    return true;
  }
  else if (valOrIdent(stack.curSourceToken()) == true)
  {
    stack.pushValOrIdent(stack.curSourceToken()); // #### @todo will not removed from stack
    //stack.resolveVarOnTop(ResolveNothing | ResolveToRef);
    PARSEEXECUTE(OptionalSuffixOp().);
    return true;
  }
  else
    ELOG("Unexpected token at start of expression");
}

/*
  (1 + 2).toString();
*/
bool parseExpression(PEStack& stack, OUTP(RScriptExpr) expr);

/*
  stack before
  0 label or value

  stack after
  0 value
*/
bool
ExpressionParseNode2::parseExecute(PEStack& stack)
{
restartExpr2:
  //stack.resolveVarOnTop(ResolveNothing);
  PEVal leftexpr = stack.pop();
  int otk = stack.nextToken();

  if (isEndExpr(stack.curSourceToken()) == true)
  {
    stack.pushBack();
    stack.push(leftexpr);
    stack.resolveVarOnTop();
    return true;
  }
  RString op1 = stack.curTokenAsString();

  if (isOperator(stack.curSourceToken()) == false)
    ELOG("Expected Operator");
  bool execRight = shallExecuteRight(op1, stack);

  int opflags = getOperatorFlags(op1);

  if (opflags & OpSuffix)
  {
    stack.pushBack();
    stack.push(leftexpr);
    PARSEEXECUTE(SuffixOp().);
    //goto restartExpr2;
    return true;
  }
  else if (opflags & OpInfix)
  {
    if (op1->equals("||") == true || op1->equals("&&") == true)
    {
      stack.push(leftexpr);
      stack.resolveVarOnTop();
      stack.pushBack();
      return true;
    }
    if (opflags & OpRightAssoc || execRight == true)
    {
      acdk::lang::sys::core_value_scope<RString> _stackOp(stack.leftOp, op1);
      PARSEEXECUTE(ExpressionParseNode().);
      //PARSEEXECUTE(ExpressionParseNodeOne().);

      // ### @todo check if empty
      PEVal rightExpr = stack.pop();
      bool execRight = shallExecuteRight(op1, stack);
      if (execRight == true)
      {
        while (execRight == true)
        {
          stack.push(rightExpr);
          PARSEEXECUTE(ExpressionParseNode2().);
          rightExpr = stack.pop();
          execRight = shallExecuteRight(op1, stack);
          if (execRight == true)
            continue;
          stack.push(leftexpr);
          stack.push(rightExpr);
          stack.push(TT_OPERATOR, inOf(op1));
          PARSEEXECUTE(ExecBinaryOp().);
          return true;
        }
      }
      else
      {
        stack.push(leftexpr);
        stack.push(rightExpr);
        stack.push(TT_OPERATOR, inOf(op1));
        PARSEEXECUTE(ExecBinaryOp().);
        PARSEEXECUTE(ExpressionParseNode2().);
      }
      return true;
    }
    else // left assoc
    {
      //int otk = stack.nextToken();
      PARSEEXECUTE(AtomPreOrSuffixOp().);
      PEVal rightExpr = stack.pop();
      stack.push(leftexpr);
      stack.push(rightExpr);
      stack.push(TT_OPERATOR, inOf(op1));
      PARSEEXECUTE(ExecBinaryOp().);
      /*
      bool execRight = shallExecuteRight(op1, stack);
      PARSEEXECUTE(ExpressionParseNode2().);
      */
      //goto restartExpr2;
      return true;
    }
  }
  else if (getOperatorFlags(op1) & OpOutfix)
  {
    stack.pushBack();
    stack.push(leftexpr);
    stack.push(TT_OPERATOR, inOf(op1));
    PARSEEXECUTE(ExecUnarySuffixOp().);
    int rtk = stack.nextToken();
    if (isEndExpr(stack.curSourceToken()) == true)
    {
      stack.pushBack();
      return true;
    }
    stack.pushBack();
    PARSEEXECUTE(ExpressionParseNode2().);
    //goto restartExpr2;
    return true;
  }
  return true;
}

bool
ExpressionParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (isEndExpr(stack.curSourceToken()) == true)
  {
    stack.push(TT_VALUE, inOf(Nil));
    return true;
  }
  stack.pushBack();
  PARSEEXECUTE(AtomPreOrSuffixOp().);

  // from here to ExpressionParseNode1
  PARSEEXECUTE(ExpressionParseNode1().);
  //dont' do that, because some expression doesn't have evaluatable expr: stack.resolveVarOnTop();
  return true;
  /*
nextRightExpr:
  PARSEEXECUTE(ExpressionParseNode2().);
  tk = stack.nextToken();
  if (isOperator(stack.curSourceToken()) == true)
  {
    //stack.resolveVarOnTop(ResolveNothing);
    stack.pushBack();
    goto nextRightExpr;
  }
  stack.pushBack();
  return true;
  */
}
/* dead
bool
ExpressionParseNodeOne::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (isEndExpr(stack.curSourceToken()) == true)
  {
    stack.push(TT_VALUE, inOf(Nil));
    return true;
  }
  stack.pushBack();
  PARSEEXECUTE(AtomPreOrSuffixOp().);
  PARSEEXECUTE(ExpressionParseNode2().);
  PEVal val = stack.top();
  if (val.tk != TT_DOTOP)
    return true;
  tk = stack.nextToken();
  while (tk == '(')
  {
    if (tk == '(')
    {
      stack.push(TT_OPERATOR, inOf(new String("(")));
      PARSEEXECUTE(ExecUnarySuffixOp().);
      val = stack.top();
      tk = stack.nextToken();
    }
    else
    {
      stack.pushBack();
      return true;
    }
  }
  stack.pushBack();
  return true;
}
*/
bool
ExpressionParseNode1::parseExecute(PEStack& stack)
{

nextRightExpr:
  PARSEEXECUTE(ExpressionParseNode2().);
  int tk = stack.nextToken();
  if (isOperator(stack.curSourceToken()) == true)
  {
    RString rop = stack.curTokenAsCode();
    if (rop->equals("||") == true || rop->equals("&&") == true)
    {
      PEVal& val = stack.top();
      if ((val.val.isTrue(stack.props->getCastFlags()) == true && rop->equals("||") == true) ||
          val.val.isTrue(stack.props->getCastFlags()) == false && rop->equals("&&") == true)
      {
        stack.skipExpressionUntilOpLowerOrEqual(getOperatorPredence(rop));
        goto nextRightExpr;
      }
      else
      {
        stack.pop();
        PARSEEXECUTE(ExpressionParseNode().);
        return true;
      }
    }
    if (stack.leftOp == Nil || shallExecuteRight(stack.leftOp, rop) == true)
    {
      //stack.resolveVarOnTop(ResolveNothing);
      stack.pushBack();
      goto nextRightExpr;
    }
  }
  
  if (stack.top().tk == TT_DOTOP) // for (int i in a.b)
    stack.resolveVarOnTop(ResolveNothing); // may be also a typename
  stack.pushBack();
  return true;
}



bool
StatementParseNode::parseExecute(PEStack& stack)
{
  DebugBreakPoints::doBreak(DRANextStatement, stack);
  int debugFlags = ExecutionStack::getDebugFlags();
  if (debugFlags & DbgExitAll)
  {
    stack.executionFlags |= EFActiveReturn;
    return true;
  }

  if (debugFlags & DbgStepOverNextStmt)
  {
     Script:ExecutionStack::setDebugFlags(debugFlags & ~DbgStepOverNextStmt);
     debugFlags &= ~DbgBreakStatements;
     ScopedDbgFlags scopeDbg(debugFlags);
     return StatementParseNode2().parseExecute(stack);
  }
  return StatementParseNode2().parseExecute(stack);
}

bool
ReturnParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
    if (tk == ';')
    {
      stack.executionFlags |= EFActiveReturn;
      stack.push(TT_VALUE, ScriptVar());
      return true;
    }
    stack.pushBack();

    PARSEEXECUTE(ExpressionParseNode().);
    tk = stack.nextToken();
    if (tk != ';')
      ELOG("Expecting ';'");
    stack.resolveVarOnTop();
    stack.executionFlags |= EFActiveReturn;
    return true;
}



bool
StatementParseNode2::parseExecute(PEStack& stack)
{
  int ctokenIdx = stack.getCurTokenIndex();
  
  //stack.script->onBreak(stack);
  int tk = stack.nextToken();
  if (tk == -1)
    return false;
  RString cts = stack.curTokenAsString();
  int ctsHc = cts->hashCode();

  ASCLITERAL(if);
  ASCLITERAL(with);
  ASCLITERAL(while);
  ASCLITERAL(for);
  ASCLITERAL(foreach);
  ASCLITERAL(do);
  ASCLITERAL(using);
  ASCLITERAL(class);
  static StaticAsciiLiteral lit_interface("interface");
  ASCLITERAL(enum);
  ASCLITERAL(ACDK_LOG);
  ASCLITERAL(ACDK_NLOG);
  ASCLITERAL(return);
  ASCLITERAL(break);
  ASCLITERAL(continue);
  ASCLITERAL(try);
  ASCLITERAL(throw);
  ASCLITERAL(catch);
  ASCLITERAL(switch);
  static StaticAsciiLiteral lit_synchronized("synchronized");
  ASCLITERAL(typealias);

  switch (ctsHc)
  {
  case StringHash<'i','f'>::hash:
    if (cts->equals(lit_if) == false)
      break;
    PARSEEXECUTE(IfStatementParseNode().);
    return true;
  case StringHash<'w','i','t','h'>::hash:
    if (cts->equals(lit_with) == false)
      break;
    PARSEEXECUTE(WithParseNode().);
    return true;
  case StringHash<'w','h', 'i','l','e'>::hash:
    if (cts->equals(lit_while) == false)
      break;
    PARSEEXECUTE(WhileParseNode().);
    return true;
  case StringHash<'f','o', 'r'>::hash:
    if (cts->equals(lit_for) == false)
      break;
    PARSEEXECUTE(ForParseNode().);
    return true;
  case StringHash<'f','o', 'r','e', 'a', 'c', 'h'>::hash:
    if (cts->equals(lit_foreach) == false)
      break;
    PARSEEXECUTE(ForEachParseNode().);
    return true;
  case StringHash<'d','o'>::hash:
    if (cts->equals(lit_do) == false)
      break;
    PARSEEXECUTE(DoParseNode().);
    return true;
  case StringHash<'u','s', 'i','n', 'g'>::hash:
    if (cts->equals(lit_using) == false)
      break;
    PARSEEXECUTE(UsingParseNode().);
    return true;
  case StringHash<'c','l', 'a','s', 's'>::hash:
    if (cts->equals(lit_class) == false)
      break;
    PARSEEXECUTE(ClassParseNode().);
    return true;
  case StringHash<'i','n', 't','e', 'r', 'f', 'a', 'c', 'e'>::hash:
    if (cts->equals(lit_interface) == false)
      break;
    PARSEEXECUTE(InterfaceParseNode().);
    return true;
  case StringHash<'e','n', 'u','m'>::hash:
    if (cts->equals(lit_enum) == false)
      break;
    PARSEEXECUTE(EnumParseNode().);
    return true;
   case StringHash<'A','C', 'D','K', '_', 'L', 'O', 'G'>::hash:
   case StringHash<'A','C', 'D','K', '_', 'N', 'L', 'O', 'G'>::hash:
     if (cts->equals(lit_ACDK_LOG) == false && cts->equals(lit_ACDK_NLOG) == false)
      break;
    stack.pushBack();
    PARSEEXECUTE(LogParseNode().);
    return true;
  case StringHash<'r','e', 't','u', 'r', 'n'>::hash:
    if (cts->equals(lit_return) == false)
      break;
    PARSEEXECUTE(ReturnParseNode().);
    return true;
  case StringHash<'b','r', 'e','a', 'k'>::hash:
    if (cts->equals(lit_break) == false)
      break;
    PARSEEXECUTE(BreakParseNode().);
    return true;
  case StringHash<'c','o', 'n','t', 'i', 'n', 'u', 'e'>::hash:
    if (cts->equals(lit_continue) == false)
      break;
    PARSEEXECUTE(ContinueParseNode().);
    return true;
  case StringHash<'t','r', 'y'>::hash:
    if (cts->equals(lit_try) == false)
      break;
    PARSEEXECUTE(TryCatchParseNode().);
    return true;
  case StringHash<'t','h', 'r','o', 'w'>::hash:
    if (cts->equals(lit_throw) == false)
      break;
    PARSEEXECUTE(ThrowParseNode().);
    return true;
  case StringHash<'s','w', 'i','t', 'c', 'h'>::hash:
    if (cts->equals(lit_switch) == false)
      break;
    PARSEEXECUTE(SwitchParseNode().);
    return true;
  case StringHash<'s','y', 'n','c', 'h', 'r', 'o', 'n', 'i', 'z', 'e', 'd'>::hash:
    if (cts->equals(lit_synchronized) == false)
      break;
    PARSEEXECUTE(SynchronizeParseNode().);
    return true;
  case StringHash<'t','y', 'p','e', 'a', 'l', 'i', 'a', 's'>::hash:
    if (cts->equals(lit_typealias) == false)
      break;
    PARSEEXECUTE(TypeAliasParseNode().);
    return true;
  default: 
    break;
  }
  if (tk == '{')
  {
    tk = stack.nextToken();
    if (tk == '}')
      return true;
    
    stack.pushBack();
    //  create new scope
    PropsScope propsscope(stack.props);
    while (tk != -1)
    {
      PARSEEXECUTE(StatementParseNode().);

      if ((stack.executionFlags & EFActiveContinue) || stack.executionFlags & EFActiveBreak)
        return true;
      tk = stack.nextToken();
      if (tk == '}')
        return true;
      stack.pushBack();
    }
    ELOG("Missing closing '}'");
  }
  else if (tk == ';') // empty statement
  {
    return true;
  }
  else if (tk == StreamTokenizer::TT_WORD)
  {
    RString type  = stack.curTokenAsString();
    ASCLITERAL(new);
    if (type->equals(lit_new) == true)
    {
      PARSEEXECUTE(NewExprParseNode().);
      RObject obj = stack.pop().val.getObjectVar();
      return true;
    }
    type = stack.parseIdentifier(type);

    tk = stack.nextToken();
    /*
      int var;
      acdk.lang.String var = 0;
      acdk.lang.String.toString("asdf");
    */
    if (tk == StreamTokenizer::TT_WORD)  // typedeclaration int i
    {
      RString varname = stack.curTokenAsString();
      const ClazzInfo* ci = stack.findType(type);
      stack.props->create(varname, ci);
      acdk::lang::sys::core_value_scope<const ClazzInfo*> leftType(stack.curLeftDeclType, ci);
      tk = stack.nextToken();
      if (tk == ';')
        return true;
      if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true)
      {
        PARSEEXECUTE(ExpressionParseNode().);
        stack.resolveVarOnTop();
         PEVal var = stack.pop();
        stack.props->assign(varname,  new DmiObject(var.val));
        tk = stack.nextToken();
        if (tk == ';')
          return true;
        ELOG("Expecting ';'");
      }
      else if (tk == '(')
      {
        ELOG("free function definitions not implemented yet");
      }
      ELOG("Expecting ';'");
    }
    else
    {
      stack.setCurTokenIndex(ctokenIdx);
      PARSEEXECUTE(ExpressionParseNode().);
      tk = stack.nextToken();
      if (tk != ';')
        ELOG("Expecting ';'");
    }
    stack.pop();
  }
  else if (tk == STkTemplateComment)
  {
    // simply do nothing
  }
  else if (tk == STkTemplateText)
  {
    acdk::io::RCharWriter cout = (acdk::io::RCharWriter)stack.props->getObjectVal("out");
    acdk::io::RWriter out = cout->getWriter();
    RbyteArray bytes = (RbyteArray)stack.curSourceToken().value.getObjectVar();
    out->write(bytes);
  }
  else if (tk == STkTemplateExpression)
  {
    PARSEEXECUTE(ExpressionParseNode().);
    acdk::io::RPrintWriter cout = (acdk::io::RPrintWriter)stack.props->getObjectVal("out");
    cout->print(stack.pop().val.toString());
  }
  else if (tk == '#')
  {
    PARSEEXECUTE(PreProcParseNode().);
  }
  else
  {
    stack.setCurTokenIndex(ctokenIdx);
    PARSEEXECUTE(ExpressionParseNode().);
    tk = stack.nextToken();
    if (tk != ';')
      ELOG("Expecting ';'");
    stack.pop();
  }
  return true;
}


bool
StatementOrPreProc::parseExecute(PEStack& stack)
{
  int tk;
  while ((tk = stack.nextToken()) != -1)
  {

    if (tk == '#')
    {
      PARSEEXECUTE(PreProcParseNode().);
    }
    else
    {
      stack.pushBack();
      PARSEEXECUTE(StatementParseNode().);
    }
    if (stack.curToken() == -1)
      return true;
  }
  return true;
}


bool
IfStatementParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (tk != '(')
    ELOG("Expecting '(' after 'if' ");
  PARSEEXECUTE(ExpressionParseNode().);

  stack.resolveVarOnTop();
  ScriptVar sv = stack.pop().val;
  tk = stack.nextToken();
  if (tk != ')')
    ELOG("Expecting ')' after 'if (expr'");
  if (sv.isTrue(stack.props->getCastFlags()) == true)
  {
    PARSEEXECUTE(StatementParseNode().);
  }
  else
  {
    tk = stack.skipStatement();
  }
  if (stack.curToken() == -1)
    return true;
  tk = stack.nextToken();
  RString ctstr = stack.curTokenAsString();
  ASCLITERAL(else);
  if (tk == StreamTokenizer::TT_WORD && ctstr->equals(lit_else) == true)
  {
    if (sv.isTrue(stack.props->getCastFlags()) == true)
      tk = stack.skipStatement();
    else
    {
      PARSEEXECUTE(StatementParseNode().);
    }
  }
  else
    stack.pushBack();
  return true;
}

void createStaticMember(const ClazzInfo* ci, ClazzFieldInfo* fi, PEStack& stack);
bool parseArgumentDecl(PEStack& stack, acdk::lang::dmi::ClazzInfo* ci, acdk::lang::dmi::ClazzMethodInfo* mi, IN(acdk::lang::dmi::RDmiNamedArgArray) defaultArgValues);

bool
ClassMemberOrFieldParseNode::parseExecute(PEStack& stack)
{
  ClazzInfo* ci = const_cast<ClazzInfo*>(RClass(stack.top().val.getObjectVar())->objectClazzInfo());
  int tk;
  int nextmemberflags = 0;
  while (true)
  {
    tk = stack.nextToken();
    if (tk == StreamTokenizer::TT_EOF)
      ELOG("Unexpected EOF while parsing class definition");
    RString stk = stack.curTokenAsString();
    if (stk->equals("public") == true)
      nextmemberflags |= MiPublic;
    else if (stk->equals("private") == true)
      nextmemberflags |= MiPrivate;
    else if (stk->equals("protected") == true)
      nextmemberflags |= MiProtected;
    else if (stk->equals("static") == true)
      nextmemberflags |= MiStatic;
    else
      break;
  }
  RString first = stack.curTokenAsString();
  first = stack.parseIdentifier(first);
  tk = stack.nextToken();
  RString second;
  if (tk == StreamTokenizer::TT_WORD)
  {
    second = stack.curTokenAsString();
    tk = stack.nextToken();
  }
  else if (tk == '(') // constructor
  {
    if (first->equals(ci->name) == false)
      ELOG("missing return type or wrong constructor name");
    nextmemberflags |= MiMethodInfo | MiMiConstructor;
    second = first;
    first = Nil;
  } else
    tk = stack.nextToken();
  if (second->equals("operator") == true)
  {
    RString op = stack.curTokenAsString();
    second = acdk::lang::reflect::Method::encodeOperatorToFuncName(op);
    tk = stack.nextToken();
  }
  if (tk == ';' || (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true))
  {
    nextmemberflags |= MiFieldInfo;
    ClazzFieldInfo* fi = createMetaInfo<ClazzFieldInfo>();
    fi->flags = nextmemberflags | MiDelete;
    fi->name = MetaInfo::strdup(second->c_str());
    fi->nameHashCode = -1;
    fi->type = stack.findType(first);
    fi->_scopeParent = (const NamedScopedMetaInfo*)ci;
    ci->addField(fi);
    if (tk == ';')
      return true;

    int codebegin = stack.getCurTokenIndex();
    tk = stack.skipExpression();
    tk = stack.nextToken();
    if (tk != ';')
      ELOG("Expect ';' after <[attributes]> <[type]> <membername> = <expression>");

    int codeend = stack.getCurTokenIndex();
    RScriptMethodInfo methodsource = new ScriptMethodInfo(stack.script, codebegin, codeend);
    Field(ci, fi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    if (fi->flags & MiStatic)
    {
      createStaticMember(ci, fi, stack);
    }
    return true;
  }
  else if (tk != '(')
    ELOG("expecting ';' or '(' for class field or member");

  ClazzMethodInfo* mi = createMetaInfo<ClazzMethodInfo>();
  mi->flags = nextmemberflags | MiDelete | MiMethodInfo | MiMiDmiImpl;
  mi->name = MetaInfo::strdup(second->c_str());
  mi->nameHashCode = -1;
  if (nextmemberflags & MiMiConstructor)
    mi->returnType = ci;
  else if (nextmemberflags & MiMiDestructor)
    mi->returnType = ClazzInfo::getVoidClazz();
  else
  {
    mi->returnType = stack.findType(first);
  }
  RDmiNamedArgArray defaultArgValues = new DmiNamedArgArray(0);
  if (parseArgumentDecl(stack, ci, mi, defaultArgValues) == false)
    return false;
  /*
  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == ')')
    {
      break;
    }
    if (tk == ',')
      continue;

    // ### TODO if (stack.curTokenAsString()->equals("in") out
    RString t = stack.curTokenAsString();

    t = stack.parseIdentifier(t);
    int argumentFlags = 0;
    if (t->equals("in") == true || t->equals("inout") == true || t->equals("out") == true)
    {
      if (t->equals("in") == true) argumentFlags |= MiAiIn;
      if (t->equals("out") == true) argumentFlags |= MiAiOut;
      if (t->equals("inout") == true) argumentFlags |= MiAiInOut;
      t = stack.parseIdentifier("");
    }
    tk = stack.nextToken();
    if (tk != StreamTokenizer::TT_WORD)
      ELOG("expecting word for argument identifier");
    RString n = stack.curTokenAsString();
    ClazzMethodArgInfo* ma = createMetaInfo<ClazzMethodArgInfo>();
    ma->flags = argumentFlags | MiDelete | MiMethodArgInfo;
    ma->name = MetaInfo::strdup(stack.curTokenAsString()->c_str());
    ma->type = stack.findType(t);
    mi->addArgument(ma);
    tk = stack.nextToken();
    if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true)
    {
      PARSEEXECUTE(ExpressionParseNode().);
      defaultArgValues->append(new DmiNamedArg(ma->name, new DmiObject(stack.pop().val.inOf())));
      // initializer
      tk = stack.nextToken();
    }
    if (tk == ')')
      break;
    if (tk == ',')
      continue;
    ELOG("expect ',' or ')' in parameter declaration");
  }

  // parse parameter
  mi->_scopeParent = (const NamedScopedMetaInfo*)ci;
  ci->addMethod(mi);

  int codebegin = stack.getCurTokenIndex();
  tk = stack.skipStatement();
  int codeend = stack.getCurTokenIndex();
  RScriptMethodInfo methodsource = new ScriptMethodInfo(stack.script, codebegin, codeend);
  Method(ci, mi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
  mi->dispatch = ScriptObject::dispatch;
  ClazzMethodInfo* defMi = mi;

  int defaultArgsLength = defaultArgValues->length();
  int argsLength = mi->getArgumentCount();
  int reqArgLength  = argsLength - defaultArgsLength;
  int k = 1;
  for (int i = argsLength - 1; i >= reqArgLength; --i, ++k)
  {
    // (int a, int b = 0, int c = 0) => 2

    defMi = defMi->clone();
    defMi->attributeRes = 0;
    Method(ci, defMi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    defMi->dispatch = mi->dispatch;
    defMi->_scopeParent = mi->_scopeParent;

    MetaInfoChildsArray<ClazzMethodArgInfo>(defMi->methodArgs).pop_back();
    //RDmiNamedArgArray defArgs = new DmiNamedArgArray(defaultArgsLength - i);
    RDmiNamedArgArray defArgs = new DmiNamedArgArray(k);
    int tdefargslength = defArgs->length();
    for (int j = tdefargslength - 1; j >= 0; --j)
    {
      //int davidx = defaultArgsLength - i + j;
      int davidx = defaultArgsLength - j - 1;
      defArgs[j] = defaultArgValues[davidx];
    }
    Method(ci, defMi).setMetaAttribute("_cfgscript_defaultArgs", (RObject)defArgs);

    ci->addMethod(defMi);
  }

  if (Method(ci, mi).hasMetaAttribute("_cfgscript_defaultArgs") == true)
  {
    RDmiNamedArgArray defaultArgs = (RDmiNamedArgArray)Method(ci, mi).getMetaAttribute("_cfgscript_defaultArgs")->value;
  }
  */
  return true;
}

bool
ClassBodyParseNode::parseExecute(PEStack& stack)
{
  int nextmemberflags = 0;
  int tk;
  RStringArray fields = new StringArray(0);

  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == '}')
      return true;
    else
    {
      stack.pushBack();
      PARSEEXECUTE(ClassMemberOrFieldParseNode().);
    }
  }
  ELOG("Unexpected EOF while parsing class definition");
  return false;
}

ClazzInfo* ClazzInfo_create(IN(RString) name, const UnitInfo* parent = 0)
{
  int idx = name->lastIndexOf('.');
  RString unit = "";
  RString classname = name;
  UnitInfo* ui = 0;
  if (idx != -1)
  {
    unit = name->substr(0, idx);
    classname = name->substr(idx + 1);
    ui = UnitInfo::findCreateUnit(unit->replace('.', '/')->replace("::", "/")->c_str());
  }
  ClazzInfo* ev = createMetaInfo<ClazzInfo>();
  ev->flags = MiClazzInfo | MiPublic | MiDelete | MiCiWeakBind | MiResolved;
  ev->name = MetaInfo::strdup(classname->c_str());
  ev->nameHashCode = -1;
  ev->ns = MetaInfo::strdup(unit->replace('.', '/')->c_str());
  ev->_scopeParent = ui->getMetaInfo();
  ev->static_dispatch = ScriptObject::static_dispatch;
  return ev;
}

struct RegisterTempClass
{
  ClazzInfo* _ci;
  RegisterTempClass(ClazzInfo* ci)
    : _ci(ci)
  {
    _ci->registerClazzInfo();
  }
  ~RegisterTempClass()
  {
    if (_ci == 0)
      return;
    ClazzInfo::unregisterClazzInfo(_ci);
    _ci->dispose();
  }
  void commit() { _ci = 0; }
};

bool
ClassParseNode_parseExecute(PEStack& stack, bool isInterface)
{
  RString classname = stack.parseIdentifier("");
  //int tk = stack.nextToken();
  //RString classname = stack.curTokenAsString();
  ClazzInfo* ci = ClazzInfo_create(classname);
  if (isInterface == true)
    ci->flags |= MiCiInterface;
  // have to register because in class definition may used the class
  RegisterTempClass _rtc(ci);
  RClass thisclass = Class::getSingeltonClass(ci);
  thisclass->setMetaAttribute("_cfgscript_script", (RObject)&stack.script);
  thisclass->setMetaAttribute("_cfgscript_module_props", (RObject)&stack.props);
  // ### @todo stack controlled unregister
  int tk = 0;
  acdk::lang::sys::core_vector<ClazzSuperInfo*> _supers;
  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == StreamTokenizer::TT_WORD || tk == ',')
    {
      RString deriveType = stack.curTokenAsString();
      if (deriveType->equals("implements") == true ||
          deriveType->equals("extends") == true ||
          tk == ',')
      {
        if (tk == ',')
          deriveType = "implements";

        RString super = stack.parseIdentifier("")->replace('.', '/');
        if (ClazzInfo::findClazzInfo(super + "_DmiProxy") != 0)
          super = super + "_DmiProxy";
        const ClazzInfo* sci = stack.findType(super);
        ClazzSuperInfo* ev = createMetaInfo<ClazzSuperInfo>();
        ev->flags = MiPublic | MiDelete | MiSuperInfo;
        ev->type = sci;
        _supers.push_back(ev);
        //ci->addSuper(ev);
      }
    }
    else if (tk == '{')
    {
      bool hasSuper = false;
      int i;
      for (i = 0; i < _supers.size(); ++i)
      {
        if (_supers[i] != 0 && ScriptObject::_isDmiProxyInterface(_supers[i]->type) == false)
        {
          hasSuper = true;
          break;
        }
      }
      if (hasSuper == false && isInterface == false)
      {
        ClazzSuperInfo* ev = createMetaInfo<ClazzSuperInfo>();
        ev->flags = MiPublic | MiDelete | MiSuperInfo;

        if (ClazzInfo::findClazzInfo(RString("acdk/lang/Object_DmiProxy")) != 0)
          ev->type = stack.findType("acdk/lang/Object_DmiProxy");
        else
          ev->type = Object::clazzInfo();
        ci->addSuper(ev);
      }
      for (i = 0; i < _supers.size(); ++i)
      {
        ci->addSuper(_supers[i]);
      }
      stack.push(TT_VALUE, inOf(thisclass));
      PARSEEXECUTE(ClassBodyParseNode().);
      stack.pop();
      _rtc.commit();
      // ### @todo register in Script and deregister in ~Script
      return true;
    }
    else
      ELOG("Unknown token in parsing class declaration");
  }
  return true;
}

bool
ClassParseNode::parseExecute(PEStack& stack)
{
  return ClassParseNode_parseExecute(stack, false);
/*
  RString classname = stack.parseIdentifier("");
  //int tk = stack.nextToken();
  //RString classname = stack.curTokenAsString();
  ClazzInfo* ci = ClazzInfo_create(classname);

  // have to register because in class definition may used the class
  RegisterTempClass _rtc(ci);
  RClass thisclass = Class::getSingeltonClass(ci);
  thisclass->setMetaAttribute("_cfgscript_script", (RObject)&stack.script);
  thisclass->setMetaAttribute("_cfgscript_module_props", (RObject)&stack.props);
  // ### @todo stack controlled unregister
  int tk = 0;
  acdk::lang::sys::core_vector<ClazzSuperInfo*> _supers;
  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == StreamTokenizer::TT_WORD || tk == ',')
    {
      RString deriveType = stack.curTokenAsString();
      if (deriveType->equals("implements") == true ||
          deriveType->equals("extends") == true ||
          tk == ',')
      {
        if (tk == ',')
          deriveType = "implements";

        RString super = stack.parseIdentifier("")->replace('.', '/');
        if (ClazzInfo::findClazzInfo(super + "_DmiProxy") != 0)
          super = super + "_DmiProxy";
        const ClazzInfo* sci = stack.findType(super);
        ClazzSuperInfo* ev = createMetaInfo<ClazzSuperInfo>();
        ev->flags = MiPublic | MiDelete | MiSuperInfo;
        ev->type = sci;
        _supers.push_back(ev);
        //ci->addSuper(ev);
      }
    }
    else if (tk == '{')
    {
      bool hasSuper = false;
      int i;
      for (i = 0; i < _supers.size(); ++i)
      {
        if (ScriptObject::_isDmiProxyInterface(_supers[i]->type) == false)
        {
          hasSuper = true;
          break;
        }
      }
      if (hasSuper == false)
      {
        ClazzSuperInfo* ev = createMetaInfo<ClazzSuperInfo>();
        ev->flags = MiPublic | MiDelete | MiSuperInfo;

        if (ClazzInfo::findClazzInfo(RString("acdk/lang/Object_DmiProxy")) != 0)
          ev->type = stack.findType("acdk/lang/Object_DmiProxy");
        else
          ev->type = Object::clazzInfo();
        ci->addSuper(ev);
      }
      for (i = 0; i < _supers.size(); ++i)
      {
        ci->addSuper(_supers[i]);
      }
      stack.push(TT_VALUE, inOf(thisclass));
      PARSEEXECUTE(ClassBodyParseNode().);
      stack.pop();
      _rtc.commit();
      // ### @todo register in Script and deregister in ~Script
      return true;
    }
    else
      ELOG("Unknown token in parsing class declaration");
  }
  return true;
  */
}

bool
InterfaceParseNode::parseExecute(PEStack& stack)
{
  return ClassParseNode_parseExecute(stack, true);
}

void 
createSuper(PEStack& stack, const ClazzSuperInfo* si, ScriptVarArray& args, IN(RStringArray) namedargs)
{
  RString classname = si->type->toTypeString(TpFtLoadableClass);
  RString funcname = si->type->name;

  ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient(stack.props->getCastFlags());

  DOUT("New Super: " << classname << "()");
  if (ClazzInfo::findClazzInfo(classname + "_DmiProxy") != 0)
    classname = classname + "_DmiProxy";
  ScriptVar superobj = Object::New(classname, args, namedargs, dmiclient);
  RScriptObject so = (RScriptObject)stack.props->getObjectVal(lit_this);
  RObject superObj = superobj.getObjectVar();
  so->setSuperObject(superObj);
  stack.props->setObjectVal(lit_super, superObj, PropsNoParentWrite);
  stack.addUsingVar(lit_super, superObj);
}

bool
ConstructorMethodParseNode::parseExecute(PEStack& stack)
{
  int sp = stack.getCurTokenIndex();

  int tk = stack.nextToken(); // consume '{'
  if (tk != '{')
    ELOG("Expect '{' after method header");
  tk = stack.nextToken();
  if (stack.curTokenAsString()->equals(lit_super) == true)
  {
    // ### TODO does't work when static!
    const ClazzSuperInfo* si = ExecutionStack::getTop()->_currentClazzInfo->getSuper();
    if (si == 0)
      ELOG("Class has no super classes to initialize");
    PARSEEXECUTE(ParametersParseNode().);
    ScriptVarArray args;
    RStringArray namedargs;
    getArgs(stack, args, namedargs);
    createSuper(stack, si, args, namedargs);

    while (tk != -1)
    {
      tk = stack.nextToken();
      if (tk == StreamTokenizer::TT_EOF)
        ELOG("Unexpected EOF");
      if (tk == '}')
        break;
      stack.pushBack();
      PARSEEXECUTE(StatementParseNode().);
    }
    return true;
  }
  else
  {
    const ClazzSuperInfo* si = ExecutionStack::getTop()->_currentClazzInfo->getSuper();
    if (si != 0)
    {
      ScriptVarArray args;
      RStringArray namedargs;
      createSuper(stack, si, args, namedargs);
    }
    stack.setCurTokenIndex(sp);
    PARSEEXECUTE(StatementOrPreProc().);
    return true;
  }
}

ClazzEnumInfo* EnumInfo_create(IN(RString) name, const UnitInfo* parent = 0)
{
  int idx = name->lastIndexOf('.');
  RString unit = "";
  RString classname = name;
  const UnitInfo* ui = 0;
  if (idx != -1)
  {
    unit = name->substr(0, idx);
    classname = name->substr(idx + 1);
    ui = UnitInfo::findCreateUnit(unit->c_str());
  }
  ClazzEnumInfo* ev = createMetaInfo<ClazzEnumInfo>();
  ev->flags = MiEnumInfo | MiPublic | MiDelete | MiResolved;
  ev->name = MetaInfo::strdup(classname->c_str());
  ev->nameHashCode = -1;
  ev->ns = MetaInfo::strdup(unit->replace('.', '/')->c_str());
  ev->_scopeParent = ui->getMetaInfo();
  return ev;
}


bool
EnumParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  RString enumName = stack.curTokenAsString();
  tk = stack.nextToken();
  if (tk != '{')
    ELOG("Excpet '{' after enum <identifier>");

  ClazzEnumInfo* ev = EnumInfo_create(enumName);
  acdk::lang::reflect::Enumeration(ev).setMetaAttribute("_cfgscript_script", (RObject)&stack.script);
  ClazzEnumInfo::registerEnumInfo(ev);
  int lastValue = -1;
  while (true)
  {
    tk = stack.nextToken();
    if (tk != StreamTokenizer::TT_WORD)
      ELOG("Expecting identifier as enum value identifier");
    RString name = stack.curTokenAsString();
    tk = stack.nextToken();
    if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true)
    {
      PARSEEXECUTE(ExpressionParseNode().);
      int value = stack.pop().val.getIntVar();
      lastValue = value;
      ClazzEnumValueInfo* evi = ClazzEnumValueInfo::create(ev, name, value);
      tk = stack.nextToken();
    }
    else
    {
      ClazzEnumValueInfo* evi = ClazzEnumValueInfo::create(ev, name, ++lastValue);
    }

    if (tk == '}')
      break;
    else if (tk == ',')
      continue;
    else
      ELOG("Unknown Token while parsing enum definition");
  }

  return true;
}


int
Script::_readEval2(IN(RProps) props, bool inplace)
{
  //ScriptSource ss("unknown", in);
  //StreamTokenizer tin(&ss);

  RProps module =  props;
  if (inplace == false)
  {
    module = new Props("module", PropsParentWrite | PropsParentRead, props);
    module->setCastFlags(getCastFlags());
  }
  PEStack stack(this, module);
  ExecutionStack::getTop()->setScopeProps(module);
  ExecutionStack::getTop()->setFrameProps(module);
  
  bool asExpr = props->hasValue("__evalScriptAsExpr");
  if (asExpr == false)
    return StatementOrPreProc().parseExecute(stack) == true ? 0 : 1;
  
  bool erg = ExpressionParseNode().parseExecute(stack);
  if (erg == true)
  {
    RDmiObject erg = new DmiObject(stack.pop().val.inOf());
    props->set("__evalScriptResult", erg);
    return 0;
  }
  return -1;
}


} // namespace cfgscript
} // namespace acdk


#endif //!defined(DOXYGENONLY)

