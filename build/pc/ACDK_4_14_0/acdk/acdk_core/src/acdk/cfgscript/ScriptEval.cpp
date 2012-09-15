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

GLOBAL_ASCLITERAL(this);
GLOBAL_ASCLITERAL(super);

using acdk::io::StreamTokenizer;

int 
scanEndBlock(PEStack& stack)
{
  int tk = 0;
  int blockCount = 0;
  while ((tk = stack.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == '}')
    {
      if (blockCount > 0)
        --blockCount;
      else
        return tk;
    }
    else if (tk == '{')
      ++blockCount;
  }
  return tk;
}
  
bool 
splitClassToVar(PEStack& stack, INP(RString) label, OUTP(RString) classOrObjName, OUTP(RString) memberName)
{
  RString tstr = label;
  StringBuffer rpart;
  int idx = 0;
  while (idx != -1)
  {
    idx = tstr->indexOf('.');
    if (idx == -1)
    {
    }
    RString rs = tstr->substr(0, idx);
    RString ls = tstr->substr(idx + 1);
    if (rpart.length() > 0)
      rpart << ".";
    rpart << rs;
    // check if can be resolved
    if (rpart.toString()->indexOf('.') == -1 && stack.props->hasValue(rpart.toString()) == true)
    {
      classOrObjName = rpart.toString();
      memberName = ls;
      return true;
    }
    const ClazzInfo* ci = ClazzInfo::findClazzInfo(rpart.toString());
    if (ci != 0)
    {
      classOrObjName = rpart.toString();
      memberName = ls;
      return true;
    }
    tstr = ls;
  }
  return false;
}

RObject getObjectWrapper(PEStack& stack, const ScriptVar& sv);

bool
resolveInUsings(PEStack& stack, IN(RString) label, int flags)
{
  const MetaInfo* mi = 0;
  RUsingEntry ue = stack.findUsingChild(label, 0, &mi);
  if (ue != Nil)
  {
    if (mi->isFieldInfo() == true)
    {
      /*
      if (mi->isStatic() == true)
      {
        if (ue->isVar == true)
          continue;

      }
      else // not static
      {
        if (ue->isVar == false)
          continue;
        //PEVal dotop(TT_DOTOP, PEVal(TT_VALUE, inOf(ue->usingName)), PEVal(TT_IDENT, inOf(label)));
        //stack.push(dotop);
      }
      */
      PEVal dotop(TT_DOTOP, PEVal(TT_IDENT, inOf(ue->usingName)), PEVal(TT_IDENT, inOf(label)));
      stack.push(dotop);
      if (stack.resolveDopOp(ResolveNotInUsings | ResolveNothing) == true)
      //if (stack.resolveVarOnTop(ResolveNotInUsings) == true)
      {
        return true;
        /*
        RObject obj = getObjectWrapper(stack, stack.pop().val);
        PEVal dotop(TT_DOTOP, PEVal(TT_VALUE, inOf(obj)), PEVal(TT_IDENT, inOf(label)));
        stack.push(dotop);
        return stack.resolveDopOp(flags);
        */
      }
      //return stack.resolveDopOp(flags);
    }
  }
  return false;
  /* old code
  for (int i = usings->length() - 1; i >= 0; --i)
  {
    RString t = usings[i];
    stack.push(TT_IDENT, inOf(t));
    if (stack.resolveVarOnTop(ResolveNotInUsings) == true)
    {
      RObject obj = getObjectWrapper(stack, stack.pop().val);
      const MetaInfo* mi = obj->getClazzInfo()->findMetaInfo(label, 0);
      if (mi != 0 && mi->isFieldInfo() == true)
      {
        PEVal dotop(TT_DOTOP, PEVal(TT_VALUE, inOf(obj)), PEVal(TT_IDENT, inOf(label)));
        stack.push(dotop);
        return stack.resolveDopOp(flags);
      }
    }
    else
    {
      stack.pop();
    }
  }
  return false;
  */
}

void 
PEStack::_init()
{
}

bool 
PEStack::resolveVarOnTop(int flags/* = ResolveFail*/)
{
  PEVal val = top();
  if (val.tk == TT_DOTOP)
    return resolveDopOp(flags);
  if (val.tk == TT_METHOD || val.tk == TT_CLASS || val.tk == TT_UNIT)
    return false;

  if (val.tk != TT_IDENT)
    return true;
  
  RString label = val.val.getStringVar();
  if (props->hasValue(label) == true)
  {
    pop();
    if (evalAsRef == true || (flags & ResolveToRef))
      push(TT_REFVALUE, outOf(*props->get(label)));
    else
      push(TT_VALUE, inOf(*props->get(label)));
    return true;
  }
  
  if ((flags & ResolveNotInUsings) == 0)
  {
    pop();
    if (resolveInUsings(*this, label, flags) == true)
      return true;
    push(val);
  }
  int subscridx; 
  const MetaInfo* mi = findElement(label);
  if ((mi == 0 || mi->isUnitInfo() == true)
    && 
    ((ResolveNoCompIdent & flags) != ResolveNoCompIdent) && 
    ((subscridx = label->lastIndexOf('.')) != -1))
  {
    PEVal val = pop();
    RString classOrObject;
    RString member;
    if (splitClassToVar(*this, label, classOrObject, member) == true)
    {
      push(TT_IDENT, inOf(member));
      push(TT_IDENT, inOf(classOrObject));
      PARSEEXECUTE_INSTACK(ExecPeek().);
      return true;
    }
    else
      push(val);
  }
  if (mi != 0 && mi->isUnitInfo() == false)
  {
    if (mi->isEnumValInfo() == true) 
    {
      const ClazzEnumValueInfo* evi = reinterpret_cast<const ClazzEnumValueInfo*>(mi);
      pop();
      push(TT_VALUE, inOf(evi->value));
      return true;
    }
  }
  if (flags & ResolveFail)
  {
    ELOG_INSTACK("Cannot resolve label: " + label);
  }
  if (flags & ResolvePushNil)
  {
    pop();
    push(TT_VALUE, ScriptVar(Nil));
    return true;
  }
  return false;
}

bool
PEStack::resolveDopOp(int flags)
{
  
  PEVal op = pop();
  PEVal lv = op.args->left;
  push(op.args->left);
  PEVal rv = op.args->right;
  if (resolveVarOnTop(flags & ~ResolveFail)  == false)
  {
    // has to be class or namespace
    PEVal lv = pop();
    if (ClazzInfo::findClazzInfo(lv.val.getStringVar()) != 0)
    {
      push(rv);
      push(lv);
      PARSEEXECUTE_INSTACK(ExecStaticPeek().);
    }
    else
    {
      RString ns = SBSTR(lv.val.getStringVar() << "." << rv.val.getStringVar());
      const ClazzEnumValueInfo* enunval = ClazzEnumInfo::findEnumValue(ns);
      if (enunval != 0)
      {
        push(TT_VALUE, inOf(enunval->value));
        return true;
      }
      if ((flags & ResolveFail) == ResolveFail)
        ELOG_INSTACK("Cannot resolve label: " + ns);
      push(TT_IDENT, inOf(ns));
      return false;
    }
  }
  else
  {
    PEVal lv = pop();
    push(rv);
    push(lv);
    
    PARSEEXECUTE_INSTACK(ExecPeek().);
  }
  return true;
}



bool 
PEStack::skipStatement()
{
  int tk = nextToken();
  if (tk == -1)
    ELOG_INSTACK("EOF while parsing statement");
  if (tk == ';')
    return true;
  if (tk == '{')
  {
    tk = scanEndBlock(*this);
    return true;
  }
  if (tk == StreamTokenizer::TT_WORD)
  {
    RString ctstr = curTokenAsString();
    ASCLITERAL(if);
    ASCLITERAL(for);
    if (ctstr->equals(lit_if) == true)
    {
      skipExpression();
      skipStatement();
      tk = nextToken();
      ctstr = curTokenAsString();
      if (tk == StreamTokenizer::TT_WORD && ctstr->equals("else") == true)
        skipStatement();
      else
        pushBack();
      return true;
    }
    else if (ctstr->equals(lit_for) == true)
    {
      skipExpression();
      return skipStatement();
    }
    // while, do, switch as standard format
    /*
    else if (cstr->equals("switch") == true)
    {
    }*/
  }
  while (tk != -1)
  {
    tk = nextToken();
    if (tk == ';')
      return true;
  }
  ELOG_INSTACK("EOF while parsing statement");
  return false;
}

int 
PEStack::getBlockEndPos()
{
  int sp = getCurTokenIndex();
  if (skipStatement() == false)
    return -1;
  int erg = getCurTokenIndex();
  setCurTokenIndex(sp);
  return erg;
}

bool 
PEStack::skipLine()
{
  int cl = curSourceToken().sourcePos.linePos;

   do {
    int tk = nextToken();
    if (tk == -1)
      return false;
    if (curSourceToken().sourcePos.linePos > cl)
    {
      pushBack();
      return true;
    }
  } while(true);
  return false;
}

bool 
PEStack::skipExpression(int stopTk)
{
  
  int parcount = 0;
  do {
    int tk = nextToken();
    if (tk == -1)
      return false;
    if (tk == '(')
      ++parcount;
    else if (tk == ';' && parcount == 0)
    {
      pushBack();
      return true;
    }
    else if (tk == ')')
    {
      --parcount;
      if (parcount == -1)
      {
        pushBack();
        return true;
      }
      if (parcount == 0)
        return true;
    }
    else if (stopTk == tk && parcount <= 0)
    {
      pushBack();
      return true;
    }

  } while (true);
  return false;
}

bool 
PEStack::skipExpressionUntilOpLowerOrEqual(int opPrio)
{
  
  int parcount = 0;
  do {
    int tk = nextToken();
    if (tk == -1)
      return false;
    if (parcount <= 0 && isOperator(curSourceToken()) == true)
    {
      int prec = getOperatorPredence(curTokenAsString());
      if (opPrio <= prec)
      {
        pushBack();
        return true;
      }
    }
    if (tk == '(')
      ++parcount;
    else if (tk == ';')
    {
      pushBack();
      return true;
    }
    else if (tk == ')')
    {
      --parcount;
      if (parcount == -1)
      {
        pushBack();
        return true;
      }
    }
  } while (true);
  return false;
}

RString
PEStack::parseIdentifier(IN(RString) begin)
{
  int tk = 0;
  int blockCount = 0;
  bool expectDot = true;
  StringBuffer sb(begin);
  if (begin->length() == 0)
    expectDot = false;
  while ((tk = nextToken()) != StreamTokenizer::TT_EOF)
  {
    RString sval = curTokenAsCode();
    if (sval->equals(".") == true)
    {
      if (expectDot == true)
      {
        sb.append('.');
        expectDot = false;
        continue;
      } 
      
    } 
    else if (tk == StreamTokenizer::TT_WORD)
    {
      if (expectDot == false)
      {
        sb.append(sval);
        expectDot = true;
        continue;
      }
    }
    pushBack();
    break;
  }
  return sb.toString();
}



RString 
PEStack_getTypeAlias(IN(RProps) props, IN(RString) alias)
{
  if (props->hasValue("._cfgscript_typealias", PropsNoParentRead) == true)
  {
    RStringArray sa = props->getStringArrayVal("._cfgscript_typealias", PropsNoParentRead | PropsNoWarnRead);
    for (int i = 0; i + 1 < sa->length(); i += 2)
    {
      if (sa[i]->equals(alias) == true)
        return sa[i + 1];
    }
  }
  RPropsArray parr = props->getParentsProps();
  for (int i = 0; i < parr->length(); ++i)
  {
    RString ret = PEStack_getTypeAlias(parr[i], alias);
    if (ret != Nil)
      return ret;
  }
  return Nil;
}

RString 
PEStack::getTypeAlias(IN(RString) alias)
{
  return PEStack_getTypeAlias(props, alias);
}

void
splitNamespaceClassName(IN(RString) name, OUT(RString) ns, OUT(RString) cn)
{
  int idx;
  if ((idx = name->lastIndexOf('.')) == -1)
  {
    ns = Nil;
    cn = name;
    return;
  }
  ns = name->substr(0, idx);
  cn = name->substr(idx + 1);
}

const NamedScopedParentMetaInfo* 
PEStack::resolveNewUsing(IN(RString) use, OUT(bool) isVariable)
{
  if (props->hasValue(use) == true)
  {
    RObject o = props->getObjectVal(use);
    if (o == Nil)
      THROW1(Exception, "Cannot 'using' Nil object");
    isVariable = true;
    return (const NamedScopedParentMetaInfo*)o->getClazzInfo()->getMetaInfo();
  }
  isVariable = false;
  const MetaInfo* mi = MetaInfo::findMetaInfo(use, 0, true);
  if (mi == 0)
    THROW1(Exception, "Cannot resolve using '" + use + "'");
  if (mi->isClazzInfo() == false && mi->isUnitInfo() == false)
    THROW1(Exception, "Cannot use this type of such a type: " + use);
  return (const NamedScopedParentMetaInfo*)mi;
}

void 
PEStack::_appendUsing(IN(RUsingEntry) ue)
{
  RUsingEntryArray usings = (RUsingEntryArray)props->getObjectVal("._cfgscript_using", PropsNoParentRead | PropsNoWarnRead);
  if (usings == Nil)
  {
    usings = new UsingEntryArray(0);
    props->setObjectVal("._cfgscript_using", &usings, PropsNoParentWrite);
  }
  usings->append(ue);
}

void 
PEStack::addUsingVar(IN(RString) s, IN(RObject) var)
{
  if (var == Nil)
    THROW1(Exception, "Cannot 'using' Nil object");
  const NamedScopedParentMetaInfo* mi = (const NamedScopedParentMetaInfo*)var->getClazzInfo()->getMetaInfo();
  _appendUsing(new UsingEntry(s, mi, true));
}

void 
PEStack::addUsing(IN(RString) s)
{
  bool isVar = false;
  const NamedScopedParentMetaInfo* mi = resolveNewUsing(s, isVar);
  _appendUsing(new UsingEntry(s, mi, isVar));
}

void 
PEStack::addUsingType(const acdk::lang::dmi::ClazzInfo* metaInfo)
{
  bool isVar = false;
  RString tpName = metaInfo->toTypeString(TpFtJavaType);
  _appendUsing(new UsingEntry(tpName, metaInfo->getMetaInfo(), false));
}

RString 
PEStack::getAlias(IN(RString) typname)
{
  int tpHash = typname->hashCode();
  switch (tpHash)
  {
  case StringHash<'A','n', 'y'>::hash:
    return "acdk.lang.dmi.DmiObject";
  case StringHash<'R','e', 's', 't'>::hash:
    return "acdk.lang.dmi.DmiObjectArray";
  case StringHash<'N', 'a', 'm', 'e', 'd', 'R','e', 's', 't'>::hash:
     return "acdk.lang.dmi.DmiNamedArgArray";
  case StringHash<'D', 'e', 'l', 'e', 'g', 'a','t', 'e'>::hash:
     return "acdk.lang.dmi.DmiDelegate";
  case StringHash<'D', 'e', 'l', 'e', 'g', 'a','t', 'e', 'A', 'r', 'r', 'a', 'y'>::hash:
     return "acdk.lang.dmi.DmiDelegateArray";
  default:
    break;
  }
  return getTypeAlias(typname);
}

OUT(RUsingEntryArray)
getGlobalUsings()
{
  static RUsingEntryArray globalUsings = Nil;
  if (globalUsings != Nil)
    return globalUsings;
  globalUsings = new UsingEntryArray(0);
  System::registerStaticReference(globalUsings);
  globalUsings->append(new UsingEntry("acdk/lang", UnitInfo::findCreateUnit("acdk/lang")->getMetaInfo(), false));
  return globalUsings;
}

RUsingEntry 
_findUsingChild2(IN(RProps) p, IN(RString) tpName, int dmiFlags, const MetaInfo** foundPtr = 0)
{
  RUsingEntryArray usings = (RUsingEntryArray)p->getObjectVal("._cfgscript_using", PropsNoParentRead | PropsNoWarnRead);
  if (usings != Nil)
  {
    for (int i = 0; i < usings->length(); ++i)
    {
      RUsingEntry ue = usings[i];
      const MetaInfo* mi = ue->_metaInfo->findMetaInfo(tpName, dmiFlags); //??
      if (mi != 0)
      {
        if (mi->isFieldInfo() == true || mi->isMethodInfo() == true)
        {
          if (ue->isVar == false && mi->isStatic() == false)
            continue;
        }
        if (foundPtr != 0)
          *foundPtr = mi;
        return ue;
      }        
    }
  }
  RPropsArray parents  = p->getParentsProps();
  if (parents == Nil)
    return Nil;
  for (int i = 0; i < parents->length(); ++i)
  {
    RUsingEntry ue = _findUsingChild2(parents[i], tpName, dmiFlags, foundPtr);
    if (ue != Nil)
      return ue;
  }
  return Nil;
}

RUsingEntry 
_findUsingChild(IN(RProps) p, IN(RString) tpName, int dmiFlags, const MetaInfo** foundPtr = 0)
{
  RUsingEntry ue = _findUsingChild2(p, tpName, dmiFlags, foundPtr);
  if (ue != Nil)
    return ue;
  OUT(RUsingEntryArray) globals = getGlobalUsings();
  for (int i = 0; i < globals->length(); ++i)
  {
    RUsingEntry ue = globals[i];
    const MetaInfo* mi = ue->_metaInfo->findMetaInfo(tpName, dmiFlags); //??
    if (mi != 0)
    {
      if (ue->isVar == false && mi->isStatic() == false)
          continue;
      if (foundPtr != 0)
        *foundPtr = mi;
      return ue;
    }        
  }
  return Nil;
}

RUsingEntry 
PEStack::findUsingChild(IN(RString) tpName, int dmiFlags, const MetaInfo** foundPtr)
{
  return _findUsingChild(props, tpName, dmiFlags, foundPtr);
}


RString 
PEStack::getAliasOrUsingTypeName(IN(RString) typname, bool tryLoad)
{
  RString orgType = getAlias(typname);
  if (orgType != Nil)
    return orgType;
  const MetaInfo* mi = 0;
  if (_findUsingChild(props, typname, 0, &mi) != Nil)
  {
    if (mi->isEnumInfo() == true)
      return "int";
    if (mi->isClazzInfo() == true)
    {
      const ClazzInfo* ci = (const ClazzInfo*)mi;
      return ci->toTypeString(TpFtLoadableClass);
    }
  }
  return typname;
}




const ClazzInfo* 
PEStack::findType(IN(RString) str, bool throwOnFail)
{
  RString ns;
  RString cn;
  RString typname = str;
  splitNamespaceClassName(typname, ns, cn);
  if (cn->startsWith("R") && cn->length() > 1 && Character::isUpperCase(cn->charAt(1)) == true)
    cn = cn->substr(1);
  RString clsname;
  if (ns != Nil)
    typname = ns + "." + cn;
  else
    typname = cn;

  RString alias = getAlias(typname);
  if (alias != Nil)
    typname = alias;

  const MetaInfo* mi = MetaInfo::findMetaInfo(typname, false);
  if (mi == 0)
    _findUsingChild(props, typname, 0, &mi);

  if (mi != 0)
  {
    if (mi->isClazzInfo() == true)
      return (const ClazzInfo*)mi;
    if (mi->isEnumInfo() == true)
      return Class::forName("int")->objectClazzInfo(); 
  }
  if (throwOnFail == true)
    return Class::forName(typname)->objectClazzInfo(); // throws Ex
  
  try {
    return Class::forName(typname)->objectClazzInfo(); // throws Ex
  } catch(RClassNotFoundException ex) {
    return 0;
  }
}


const MetaInfo*
PEStack::getAliasOrUsingInfo(IN(RString) typeName, int dmiFlags, bool tryLoad)
{
  RString orgType = getAlias(typeName);
  if (orgType != Nil)
    return MetaInfo::findMetaInfo(orgType, dmiFlags, tryLoad);
  
  const MetaInfo* mi = 0;
  _findUsingChild(props, typeName, dmiFlags, &mi);
  if (mi != 0)
    return mi;
  return MetaInfo::findMetaInfo(typeName, dmiFlags, tryLoad);
}

const MetaInfo* 
PEStack::findElement(IN(RString) str, int dmiFlags)
{
  RString ns;
  RString cn;
  RString typname = str;
  splitNamespaceClassName(typname, ns, cn);
  if (cn->startsWith("R") && cn->length() > 1 && Character::isUpperCase(cn->charAt(1)) == true)
    cn = cn->substr(1);
  RString clsname;
  if (ns != Nil)
    typname = ns + "." + cn;
  else
    typname = cn;

  const MetaInfo* mi = getAliasOrUsingInfo(typname, dmiFlags, false);
  if (mi != 0)
    return mi;
  mi = getAliasOrUsingInfo(typname, dmiFlags, true);
  return mi;
}

} // namespace cfgscript
} // namespace acdk 
  

