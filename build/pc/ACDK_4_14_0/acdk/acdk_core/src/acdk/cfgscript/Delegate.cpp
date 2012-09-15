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





#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/lang/dmi/MetaInfoChildsArray.h>
#include <acdk/lang/reflect/Method.h>

#include "ScriptObject.h"
#include "Script.h"
#include "ScriptException.h"
#include "ScriptEval.h"
#include "ScriptExpr.h"

namespace acdk {
namespace cfgscript {

//using namespace ::acdk::lang::dmi;
USING_CLASS(::acdk::io::, StreamTokenizer);
using namespace acdk::lang::reflect;
//using namespace acdk::lang::dmi;


bool
DelegateParseNode::parseExecute(PEStack& stack)
{
  //using namespace ::acdk::lang::dmi;
  int tk = stack.nextToken();
  if (tk == '(')
  {
    PARSEEXECUTE(ExpressionParseNode().);

    tk = stack.nextToken();
    
    if (tk != ')')
      ELOG("Expect ')' after 'delegate ( <Expression>'");
    tk = stack.nextToken();
    if (stack.curTokenAsString()->equals(".") == false)
      ELOG("Expect '.' after 'delegate ( <Expression> )'");
    tk = stack.nextToken();
    if (tk != acdk::io::StreamTokenizer::TT_WORD)
      ELOG("Expect Identifier after 'delegate ( <Expression> ).'");
    
    RString methodName = stack.curTokenAsString();
    RObject obj = stack.pop().val.getObjectVar(stack.props->getCastFlags());
    RDmiDelegate del = new StdDmiDelegate(obj, methodName);
    stack.push(TT_VALUE, inOf(del));
    return true;
  } 
  else
    stack.pushBack();

  RString comptype = stack.parseIdentifier("");
  int lidx = comptype->lastIndexOf('.');
  if (lidx != -1)
  {
    RString clsOrObject = comptype->substr(0, lidx);
    RString method = comptype->substr(lidx + 1);
    stack.push(TT_IDENT, inOf(clsOrObject));
    if (stack.resolveVarOnTop(ResolveNothing) == false)
    {
      stack.pop();
      const ClazzInfo* ci = stack.findType(clsOrObject);
      RClass cls = Class::getSingeltonClass(ci);
      acdk::lang::dmi::RDmiDelegate del = new StdDmiDelegate(cls, method);
      stack.push(TT_VALUE, inOf(del));
      return true;
    }
    else
    {
      RObject obj = stack.pop().val.getObjectVar(stack.props->getCastFlags());
      acdk::lang::dmi::RDmiDelegate del = new StdDmiDelegate(obj, method);
      stack.push(TT_VALUE, inOf(del));
      return true;
    }
  }
  else
  {
    const ClazzMethodInfo* mi = ExecutionStack::getTop()->_currentClazzMethodInfo;
    if (mi == 0)
      ELOG("'delegate <MethodName>' can only called inside a method");

    if (mi->flags & MiStatic)
    {
      RClass cls = Class::getSingeltonClass((const ClazzInfo*)mi->_scopeParent);
      acdk::lang::dmi::RDmiDelegate del = new StdDmiDelegate(cls, comptype);
      stack.push(TT_VALUE, inOf(del));
      return true;
    }
    else
    {
      RObject obj = stack.props->getObjectVal(lit_this);
      acdk::lang::dmi::RDmiDelegate del = new StdDmiDelegate(obj, comptype);
      stack.push(TT_VALUE, inOf(del));
      return true;
    }
  }
  return false;
}

// maybe used for callback in ClazzMethodArgInfo::getDefaultArgValueFunc
ScriptVar GetDefaultArgValueFromScriptVarFunc(const ClazzMethodArgInfo* ai)
{
  RDmiNamedArgArray na = (RDmiNamedArgArray )
    acdk::lang::reflect::Method((const ClazzInfo*)ai->_scopeParent->_scopeParent, 
      (const ClazzMethodInfo*)ai->_scopeParent).getMetaAttribute("_cfgscript_defaultArgs");
  if (na == Nil)
    return ScriptVar();
  for (int i = 0; na->length(); ++i)
    if (ai->equalsName(na[i]->name) == true)
      na[i]->value;
  return ScriptVar();
}

bool
parseArgumentDecl(PEStack& stack, acdk::lang::dmi::ClazzInfo* ci, acdk::lang::dmi::ClazzMethodInfo* mi, IN(acdk::lang::dmi::RDmiNamedArgArray) defaultArgValues)
{
  //using namespace ::acdk::lang::dmi;

  int tk;
  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == ')')
    {
      break;
    }
    if (tk == ',')
      continue;
    int argumentFlags = 0;
nextAttribute:
    RString t = stack.curTokenAsString();
    t = stack.parseIdentifier(t);
    if (t->equals("in") == true || t->equals("inout") == true || t->equals("out") == true ||
        t->equals("byRef") == true || t->equals("byval") == true)
    {
      if (t->equals("in") == true) argumentFlags |= MiAiIn;
      if (t->equals("out") == true) argumentFlags |= MiAiOut;
      if (t->equals("inout") == true) argumentFlags |= MiAiInOut;
      if (t->equals("byref") == true) argumentFlags |= MiAiByref;
      if (t->equals("byval") == true) argumentFlags |= MiAiByval;
      
      tk = stack.nextToken();
      goto nextAttribute;
      //t = stack.parseIdentifier("");
    } 
    else  if (t->equals("IN") == true || t->equals("INOUT") == true || t->equals("OUT") == true ||
              t->equals("BYVAL") == true || t->equals("BYVALIN") == true || t->equals("BYVALOUT") == true ||
              t->equals("BYVALINOUT") == true || t->equals("BYREF") == true || t->equals("BYREFIN") == true ||
              t->equals("BYREFOUT") == true || t->equals("BYREFINOUT") == true)
    {
      if (t->equals("IN") == true) argumentFlags |= MiAiIn;
      else if (t->equals("OUT") == true) argumentFlags |= MiAiOut;
      else if (t->equals("INOUT") == true) argumentFlags |= MiAiInOut;
      else if (t->equals("BYVAL") == true || t->equals("BYVALIN") == true) argumentFlags |= MiAiByval | MiAiIn;
      else if (t->equals("BYVALOUT") == true) argumentFlags |= MiAiByval | MiAiOut;
      else if (t->equals("BYVALINOUT") == true) argumentFlags |= MiAiByval | MiAiOut | MiAiInOut;
      else if (t->equals("BYREF") == true || t->equals("BYREFIN") == true) argumentFlags |= MiAiByref | MiAiIn;
      else if (t->equals("BYREFOUT") == true) argumentFlags |= MiAiByref | MiAiOut;
      else if (t->equals("BYREFINOUT") == true) argumentFlags |= MiAiByref | MiAiOut | MiAiInOut;
      tk = stack.nextToken();
      if (tk != '(')
        ELOG("Expect '(' after 'IN'");
      t = stack.parseIdentifier("");
      tk = stack.nextToken();
      if (tk != ')')
        ELOG("Expect ')' after 'IN(TypeName'");
      
    }
    tk = stack.nextToken();
    if (tk != StreamTokenizer::TT_WORD)
      ELOG("expecting word for argument identifier");
    RString n = stack.curTokenAsString();
    ClazzMethodArgInfo* ma = createMetaInfo<ClazzMethodArgInfo>();
    ma->flags = argumentFlags | MiDelete | MiMethodArgInfo;
    ma->name = MetaInfo::strdup(stack.curTokenAsString()->c_str());
    ma->nameHashCode = -1;
    ma->type = stack.findType(t);
    mi->addArgument(ma);
    ma->_scopeParent = mi->getMetaInfo();
    ma->getDefaultArgValueFunc = GetDefaultArgValueFromScriptVarFunc;
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
  tk = stack.nextToken();
  RScriptMethodInfo methodsource;
  // will be set if method implementation is a methodDelegate
  RDmiDelegate methodDelegate;
  if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("=") == true)
  {
    tk = stack.nextToken();
    if (tk == StreamTokenizer::TT_NUMBER && stack.curTokenAsString()->equals("0"))
    {
      tk = stack.nextToken();
      if (tk != ';')
        ELOG("expect ';' after type method(args) = 0");
    } 
    else 
    {
      ELOG("Lamda expression as method implementation currently not supported");
      /*
      stack.pushBack();
      PARSEEXECUTE(ExpressionParseNode().);
      ScriptVar var = stack.pop().val;
      tk = stack.nextToken();
      if (tk != ';')
        ELOG("expect ';' after type method(args) = delegateexpr");
      if (var.isObjectType() == true && instanceof(var.getObjectVar(), DmiDelegate) == true)
      {
        methodDelegate = (RDmiDelegate)var.getObjectVar();
        acdk::lang::reflect::Method(ci, mi).setMetaAttribute("_cfgscript_delegate", (RObject)methodDelegate);
        mi->dispatch = ScriptObject::delegate_dispatch;
      }
      else
        ELOG("in 'type method(args) = delegateexpr;' delegateexpr is not a delegate");
        */
    }
  }
  else if (tk == ';')
  {
    mi->flags |= MiMiAbstract;
    mi->dispatch = ScriptObject::abstract_method_dispatch; 
  }
  else
  {
    stack.pushBack();
    stack.skipStatement();
    int codeend = stack.getCurTokenIndex();
    methodsource = new ScriptMethodInfo(stack.script, codebegin, codeend);
    methodsource->setStaticRef(true);
    acdk::lang::reflect::Method(ci, mi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    mi->dispatch = ScriptObject::dispatch;
  }
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
    if (methodsource != Nil)
      acdk::lang::reflect::Method(ci, defMi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    if (methodDelegate != Nil)
      acdk::lang::reflect::Method(ci, defMi).setMetaAttribute("_cfgscript_delegate", (RObject)methodDelegate);

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
    acdk::lang::reflect::Method(ci, defMi).setMetaAttribute("_cfgscript_defaultArgs", (RObject)defArgs);
    
    ci->addMethod(defMi);
  }

  if (acdk::lang::reflect::Method(ci, mi).hasMetaAttribute("_cfgscript_defaultArgs") == true)
  {
    RDmiNamedArgArray defaultArgs = (RDmiNamedArgArray)acdk::lang::reflect::Method(ci, mi).getMetaAttribute("_cfgscript_defaultArgs")->value;
  }
  return true;
}

ClazzInfo* ClazzInfo_create(IN(RString) name, const UnitInfo* parent = 0);

ClazzInfo* getCreateGlobalLamdas(PEStack& stack, ClazzInfo* super = 0)
{
  static int lambdaClazzCounter = 0;
  const MetaInfo* mi = MetaInfo::findMetaInfo(SBSTR("__lamdas" << lambdaClazzCounter), MiClazzInfo);
  if (mi != 0)
  {
    ClazzInfo* ci = const_cast<ClazzInfo*>(reinterpret_cast<const ClazzInfo*>(mi));
    RClass thisclass = Class::getSingeltonClass(ci);
    RObject o = thisclass->getObjectMetaAttribute("_cfgscript_script");
    if (o == (RObject)&stack.script)
    {
      if (super == 0)
        return ci;
      if (super->assignableFrom(ci) == true)
        return ci;
    }
    ++lambdaClazzCounter;
  }
  ClazzInfo* ci = ClazzInfo_create(SBSTR("__lamdas" << lambdaClazzCounter));
  ci->registerClazzInfo();
  RClass thisclass = Class::getSingeltonClass(ci);
  thisclass->setMetaAttribute("_cfgscript_script", (RObject)&stack.script);
  thisclass->setMetaAttribute("_cfgscript_module_props", (RObject)&stack.props);
  return ci;
}
  
bool
LambdaParseNode::parseExecute(PEStack& stack)
{
  ClazzInfo* ci = getCreateGlobalLamdas(stack);
  int tk = stack.nextToken();
  bool isThisCall = false;
  
  RDmiNamedArgArray defaultArgs;
  if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("[") == true)
  {
    do {
      tk = stack.nextToken();
      if (tk == ']')
        break;
      RString s = stack.curTokenAsString();
      if (s->equals(lit_this) == true)
      {
        ci = const_cast<ClazzInfo*>(ExecutionStack::getTop()->_currentClazzInfo);
        isThisCall = true;
      }
      else
      {
        if (defaultArgs == Nil)
          defaultArgs = new DmiNamedArgArray(0);
        tk = stack.nextToken();
        if (tk == ':')
        {
          PARSEEXECUTE(ExpressionParseNode().);
          defaultArgs->append(new DmiNamedArg(s, new DmiObject(stack.pop().val.inOf())));
        }
        else
        {
          stack.pushBack();
          stack.push(TT_IDENT, inOf(s));
          stack.resolveVarOnTop();
          defaultArgs->append(new DmiNamedArg(s, new DmiObject(stack.pop().val.inOf())));
        }
      }
      tk = stack.nextToken();
      if (tk == ',')
        continue;
      if (tk == ']')
        break;
      ELOG("Expect ',' or ']' in lambda expression");
    } while (true);
    // local imports
  }
  else
    stack.pushBack();
  //tk = stack.nextToken();
  //ci = const_cast<ClazzInfo*>(stack.script->_currentClazz);
  ClazzMethodInfo* mi = createMetaInfo<ClazzMethodInfo>();
  ClazzMethodInfo* namedmi = createMetaInfo<ClazzMethodInfo>();
  mi->_scopeParent = ci->getMetaInfo();
  namedmi->_scopeParent = ci->getMetaInfo();
  
  mi->flags = MiPublic | MiDelete | MiMethodInfo | MiMiDmiImpl;
  namedmi->flags = MiPublic | MiDelete | MiMethodInfo | MiMiDmiImpl;
  if (isThisCall == false)
  {
    mi->flags |= MiStatic;
    namedmi->flags  |= MiStatic;
  }
  static int lexprCounter = 0;
  ++lexprCounter;
  RString methodName = SBSTR("__lambdaexpr" << lexprCounter);
  mi->name = MetaInfo::strdup(methodName->c_str());
  namedmi->name = MetaInfo::strdup(methodName->c_str());
  mi->nameHashCode = -1;
  namedmi->nameHashCode = -1;
  if (defaultArgs != Nil)
  {
    acdk::lang::reflect::Method(ci, mi).setMetaAttribute("_cfgscript_defaultArgs", (RObject)defaultArgs);
    acdk::lang::reflect::Method(ci, namedmi).setMetaAttribute("_cfgscript_defaultArgs", (RObject)defaultArgs);
  }

  tk = stack.nextToken();
  RString retType;
  RDmiNamedArgArray defaultArgValues = new DmiNamedArgArray(0);

  if (tk == '{')
  {
    ClazzMethodArgInfo* ma = createMetaInfo<ClazzMethodArgInfo>();
    ma->flags =  MiDelete | MiMethodArgInfo;
    ma->name = MetaInfo::strdup("rest");
    ma->nameHashCode = -1;
    ma->type = Class::forName("acdk::lang::dmi::DmiObjectArray")->objectClazzInfo();
    mi->addArgument(ma);
    mi->returnType = acdk::lang::dmi::DmiObject::clazzInfo();
    stack.pushBack();
    int codebegin = stack.getCurTokenIndex();
    tk = stack.skipStatement();
    int codeend = stack.getCurTokenIndex();
    RScriptMethodInfo methodsource = new ScriptMethodInfo(stack.script, codebegin, codeend);
    acdk::lang::reflect::Method(ci, mi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    mi->dispatch = ScriptObject::dispatch;
    ci->addMethod(mi);

    ma = createMetaInfo<ClazzMethodArgInfo>();
    ma->flags =  MiDelete | MiMethodArgInfo;
    ma->name = MetaInfo::strdup("rest");
    ma->nameHashCode = -1;
    ma->type = Class::forName("acdk::lang::dmi::DmiNamedArgArray")->objectClazzInfo();
    namedmi->addArgument(ma);
    namedmi->returnType = acdk::lang::dmi::DmiObject::clazzInfo();
    acdk::lang::reflect::Method(ci, namedmi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    namedmi->dispatch = ScriptObject::dispatch;
    ci->addMethod(namedmi);
    goto finished;
  }
  else
  {
    stack.pushBack();
    retType = stack.parseIdentifier("");
    mi->returnType = stack.findType(retType);
  }

  tk = stack.nextToken();
  
  if (tk != '(')
  {
    ELOG("expecting '(' after 'lambda [<localImport>] <ReturnType>");
  } 
  else
  {
    if (parseArgumentDecl(stack, ci, mi, defaultArgValues) == false)
      return false;
  }
finished:
  RClass cls = Class::getSingeltonClass((const ClazzInfo*)mi->_scopeParent);
  acdk::lang::dmi::RDmiDelegate del;
  if (isThisCall == true)
  {
    //RScriptObject so = (RScriptObject)stack.props->getObjectVal("this");
    RString sthis = lit_this;
    stack.push(TT_IDENT, &sthis);
    stack.resolveVarOnTop();
    RObject obj = stack.pop().val.getObjectVar(stack.props->getCastFlags());
    del = new StdDmiDelegate(obj, methodName);
  }
  else
  {
    del = new StdDmiDelegate(cls, methodName);
  }
  stack.push(TT_VALUE, inOf(del));
  return true;
}

bool
DmiLambdaParseNode_parseExecute(const ScriptVarArray& args,PEStack& stack, INOUT(acdk::lang::dmi::RDmiDelegate) del)
{
  ClazzInfo* thisCi = 0;
  int tk = stack.nextToken();
  bool isThisCall = false;
  int curArg = 0;
  RDmiNamedArgArray defaultArgs;
  RObject obj;
  if (tk == StreamTokenizer::TT_OPERATOR && stack.curTokenAsString()->equals("[") == true)
  {
    do {
      tk = stack.nextToken();
      if (tk == ']')
        break;
      RString s = stack.curTokenAsString();

      if (s->equals(lit_this) == true)
      {
        thisCi = const_cast<acdk::lang::dmi::ClazzInfo*>(args[curArg].getClazzInfo());
        isThisCall = true;
        obj = args[curArg].getObjectVar(stack.props->getCastFlags());
      }
      else
      {
        if (defaultArgs == Nil)
          defaultArgs = new DmiNamedArgArray(0);
        //tk = stack.nextToken();
        /*if (tk == ':')
        {
          defaultArgs->append(new DmiNamedArg(s, new DmiObject(args[curArg])));
        }
        else*/
        {
          defaultArgs->append(new DmiNamedArg(s, new DmiObject(args[curArg])));
        }
      }
      tk = stack.nextToken();
      if (tk == ',')
        continue;
      if (tk == ']')
        break;
      ELOG("Expect ',' or ']' in lambda expression");
    } while (true);
    // local imports
  }
  else
    stack.pushBack();

  ClazzInfo* ci = getCreateGlobalLamdas(stack, thisCi);
  if (thisCi != 0)
  {
    ClazzSuperInfo* ev = createMetaInfo<ClazzSuperInfo>();
     ev->flags = MiPublic | MiDelete | MiSuperInfo;
     ev->type = thisCi;
     ci->addSuper(ev);
  }
  //tk = stack.nextToken();
  //ci = const_cast<ClazzInfo*>(stack.script->_currentClazz);
  ClazzMethodInfo* mi = createMetaInfo<ClazzMethodInfo>();
  mi->_scopeParent = ci->getMetaInfo();
  mi->flags = MiPublic | MiDelete | MiMethodInfo | MiMiDmiImpl;
  if (isThisCall == false)
    mi->flags |= MiStatic;
  static int lexprCounter = 0;
  ++lexprCounter;
  RString methodName = SBSTR("__lambdaexpr" << lexprCounter);
  mi->name = MetaInfo::strdup(methodName->c_str());
  mi->nameHashCode = -1;
  if (defaultArgs != Nil)
  {
    acdk::lang::reflect::Method(ci, mi).setMetaAttribute("_cfgscript_defaultArgs", (RObject)defaultArgs);
  }

  tk = stack.nextToken();
  RString retType;
  RDmiNamedArgArray defaultArgValues = new DmiNamedArgArray(0);

  if (tk == '{')
  {
    ClazzMethodArgInfo* ma = createMetaInfo<ClazzMethodArgInfo>();
    ma->flags =  MiDelete | MiMethodArgInfo;
    ma->name = MetaInfo::strdup("rest");
    ma->nameHashCode = -1;
    ma->nameHashCode = -1;
    ma->type = Class::forName("acdk::lang::dmi::DmiObjectArray")->objectClazzInfo();
    mi->addArgument(ma);
    mi->returnType = acdk::lang::dmi::DmiObject::clazzInfo();
    stack.pushBack();
    int codebegin = stack.getCurTokenIndex();
    tk = stack.skipStatement();
    int codeend = stack.getCurTokenIndex();
    RScriptMethodInfo methodsource = new ScriptMethodInfo(stack.script, codebegin, codeend);
    acdk::lang::reflect::Method(ci, mi).setMetaAttribute("_cfgscript_source", (RObject)methodsource);
    mi->dispatch = ScriptObject::dispatch;
    ci->addMethod(mi);
    goto finished;
  }
  else
  {
    stack.pushBack();
    retType = stack.parseIdentifier("");
    mi->returnType = stack.findType(retType);
  }

  tk = stack.nextToken();
  
  if (tk != '(')
  {
    ELOG("expecting '(' after 'lambda [<localImport>] <ReturnType>");
  } 
  else
  {
    if (parseArgumentDecl(stack, ci, mi, defaultArgValues) == false)
      return false;
  }
finished:
  RClass cls = Class::getSingeltonClass((const ClazzInfo*)mi->_scopeParent);
  /*
  if (isThisCall == true)
  {
    //RScriptObject so = (RScriptObject)stack.props->getObjectVal("this");
    stack.push(TT_IDENT, "this");
    stack.resolveVarOnTop();
    RObject obj = stack.pop().val.getObjectVar(stack.props->getCastFlags());
    del = new StdDmiDelegate(obj, methodName);
  }
  else*/
  if (isThisCall == true)
  {
    del = new StdDmiDelegate(obj, methodName);
  }
  else
  {
    del = new StdDmiDelegate(cls, methodName);
  }
  stack.push(TT_VALUE, inOf(del));
  return true;
}


//static 
acdk::lang::dmi::RDmiDelegate 
Script::createScriptDelegate(const ScriptVarArray& args, IN(RString) code)
{
  RScript script = new Script("<mem>");
  script->_tokenized = new TokenizedSource("<text>", code);
  script->_tokenized->parseAll();
  int flags = PropsParentRead | PropsNoParentWrite; 
  ExecutionStack::get()->startTransMetaInfo(flags);
  int stackIndex = ExecutionStack::get()->push(new ExecutionStackFrame(script, 0, 0));

  RProps env = new Props();
  script->initAsEnvProps(env);
  PEStack stack(script, env);
  acdk::lang::dmi::RDmiDelegate del;
  DmiLambdaParseNode_parseExecute(args, stack, del);

  ExecutionStack::get()->pop(stackIndex);
  return del;
}


} // namespace cfgscript
} // namespace acdk


