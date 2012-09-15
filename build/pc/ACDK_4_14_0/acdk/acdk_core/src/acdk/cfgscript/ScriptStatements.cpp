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
#include <acdk/lang/sys/core_value_scope.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/MetaInfoChildsArray.h>
#include <acdk/lang/reflect/Method.h>

#include "ScriptObject.h"
#include "Script.h"
#include "ScriptException.h"
#include "ScriptEval.h"
#include "ScriptExpr.h"


namespace acdk {
namespace cfgscript {

using acdk::io::StreamTokenizer;
  
bool 
WithParseNode::parseExecute(PEStack& stack)
{
   int tk = stack.nextToken();
   if (tk != '(')
    ELOG("Expecting '(' after 'with' ");
  PARSEEXECUTE(ExpressionParseNode().);
  tk = stack.nextToken();
   if (tk != ')')
    ELOG("Expecting ')' after 'with (expression' ");
  stack.resolveVarOnTop();
  ScriptVar withvar = stack.pop().val;

  stack.props->set(".with", new DmiObject(withvar), PropsNoParentWrite);
  PARSEEXECUTE(StatementParseNode().);
  stack.props->unset(".with");
  return true;

}

bool 
WhileParseNode::parseExecute(PEStack& stack)
{
  //acdk::lang::sys::core_value_scope<int> _exeFlags(stack.executionFlags, stack.executionFlags | EFBreakableStatement | EFContinuableStatement);
  acdk::lang::sys::core_setbit_scope<int> _exeFlags(stack.executionFlags, EFBreakableStatement | EFContinuableStatement);
  int beginPos = stack.getCurTokenIndex();
  int tk = stack.nextToken();
    if (tk != '(')
      ELOG("Expecting '(' after 'while' ");
  beginPos = stack.getCurTokenIndex();
  stack.pushBack();
  stack.skipExpression();
  int beginBlock = stack.getCurTokenIndex();
  stack.skipStatement();
  int endBlock = stack.getCurTokenIndex();
  while (true)
  {
    stack.setCurTokenIndex(beginPos);
    
    PARSEEXECUTE(ExpressionParseNode().);
    tk = stack.nextToken();
    stack.resolveVarOnTop();
    ScriptVar e = stack.pop().val;
    if (e.isTrue(stack.props->getCastFlags()) == false)
    {
      stack.setCurTokenIndex(endBlock);
      return true;
    }
    PARSEEXECUTE(StatementParseNode().);
     if (stack.executionFlags & EFActiveContinue)
    {
      stack.executionFlags &= ~EFActiveContinue;
    }
    else if (stack.executionFlags & EFActiveBreak)
    {
      stack.executionFlags &= ~EFActiveBreak;
      stack.setCurTokenIndex(endBlock);
      break;
    }
  }
  return true;
}

bool 
ForParseNode::parseExecute(PEStack& stack)
{
   acdk::lang::sys::core_setbit_scope<int> _exeFlags(stack.executionFlags, EFBreakableStatement | EFContinuableStatement);
  int tk = stack.nextToken();
  if (tk != '(')
    ELOG("except '(' after 'for'");
  int beginit = stack.getCurTokenIndex();
  stack.skipStatement();
  int begintest = stack.getCurTokenIndex();
  stack.skipStatement();
  int begininc = stack.getCurTokenIndex();
  stack.skipExpression();
  tk = stack.nextToken();
  if (tk != ')')
    ELOG("except ')' after 'for (<expr> ; <expr> ; <expr>'");
  int beginblock = stack.getCurTokenIndex();
  stack.setCurTokenIndex(beginit);
  PropsScope propsscope(stack.props);
  PARSEEXECUTE(StatementParseNode().);
  while (true)
  {
    stack.setCurTokenIndex(begintest);
    PARSEEXECUTE(ExpressionParseNode().);
    stack.resolveVarOnTop();
    bool test = stack.pop().val.isTrue(stack.props->getCastFlags());
    if (test == false)
      break;
    stack.setCurTokenIndex(beginblock);
    PARSEEXECUTE(StatementParseNode().);

    if (stack.executionFlags & EFActiveContinue)
    {
      stack.executionFlags &= ~EFActiveContinue;
    }
    else if (stack.executionFlags & EFActiveBreak)
    {
      stack.executionFlags &= ~EFActiveBreak;
      break;
    }

    stack.setCurTokenIndex(begininc);
    PARSEEXECUTE(ExpressionParseNode().);
    stack.pop();
  }
  stack.setCurTokenIndex(beginblock);
  stack.skipStatement();
  return true;
}


ACDK_DECL_INTERFACE(DmiObjectIterator);

class DmiObjectIterator
      ACDK_INTERFACEBASE
{
public:
  /**
    returns Nil if End of iterator
  */
  virtual RDmiObject next() = 0;
};


/**
  Iterator which only returns one element
*/
class SingleObjectIterator
: extends acdk::lang::Object
, implements DmiObjectIterator
{
  RDmiObject _obj;
  bool _returned;
public:
  SingleObjectIterator(INP(RDmiObject) obj)
  : _obj(obj)
  , _returned(false)
  {
  }
  RDmiObject next()
  {
    if (_returned == true)
      return Nil;
    _returned =  true;
    return _obj;
  }
};

/**
  Iterator which iterates over Collections
*/
class CollectionIterator
: extends acdk::lang::Object
, implements DmiObjectIterator
{
  acdk::util::RIterator _it;
public:
  CollectionIterator(IN(acdk::util::RCollection) collection)
  : _it(collection->iterator())
  {
  }
  CollectionIterator(IN(acdk::util::RIterator) iterator)
  : _it(iterator)
  {
  }
  RDmiObject next()
  {
    if (_it->hasNext() == false)
      return Nil;
    return new DmiObject(inOf(_it->next()));
  }
};

/* not needed
class  ArrayIterator
: extends acdk::lang::Object
, implements DmiObjectIterator
{
  RObjectArrayBaseImpl _array;
  int _curIdx;
public:
  ArrayIterator(IN(RObjectArrayBaseImpl) array)
  : _array(array)
  , _curIdx(0)
  {
  }
  RDmiObject next()
  {
    if (_curIdx >= _array->length())
      return Nil;
    return new DmiObject(_array[_curIdx++]);
  }
};
*/

class  DmiArrayIterator
: extends acdk::lang::Object
, implements DmiObjectIterator
{
  RObject _array;
  int _curIdx;
public:
  DmiArrayIterator(IN(RObject) array)
  : _array(array)
  , _curIdx(0)
  {
  }
  RDmiObject next()
  {
    if (_curIdx >= (int)_array->invoke("length"))
      return Nil;
    return new DmiObject(_array->invoke("get", _curIdx++));
  }
};

class StringCharIterator
: extends acdk::lang::Object
, implements DmiObjectIterator
{
  RString _str;
  String::iterator it;
  String::iterator end;
public:
  StringCharIterator(IN(RString) s)
  : _str(s)
  , it(_str->begin())
  , end(_str->end())
  {
  }
  RDmiObject next()
  {
    if (it == end)
      return Nil;
    RDmiObject ret = new DmiObject(*it);
    ++it;
    return ret;
  }
};

RDmiObjectIterator
getIterator(INP(RDmiObject) dmiObj)
{
  if (dmiObj->isObjectType() == false)
    return new SingleObjectIterator(dmiObj);
  RObject obj = dmiObj->getObjectVar();
  
  if (instanceof(obj, acdk::util::Collection) == true)
    return new CollectionIterator(acdk::util::RCollection(obj));
  if (instanceof(obj, acdk::util::Iterator) == true)
    return new CollectionIterator(acdk::util::RIterator(obj));

  if (instanceof(obj, ObjectArrayBaseImpl) == true ||
      instanceof(obj, boolArray) || instanceof(obj, charArray) ||
      instanceof(obj, uccharArray) || instanceof(obj, byteArray) ||
      instanceof(obj, shortArray) || instanceof(obj, intArray) ||
      instanceof(obj, longArray) || instanceof(obj, floatArray) ||
      instanceof(obj, doubleArray))
      return new DmiArrayIterator(obj);
  if (instanceof(obj, String) == true)
    return new StringCharIterator(RString(obj));
  if (instanceof(obj, StringBuffer) == true)
    return new StringCharIterator(RStringBuffer(obj)->toString());
  return new SingleObjectIterator(dmiObj);
}


/**
  foreach (VarOrDecl in Expression)
  {
  }
*/
bool
ForEachParseNode::parseExecute(PEStack& stack)
{
  acdk::lang::sys::core_setbit_scope<int> _exeFlags(stack.executionFlags, EFBreakableStatement | EFContinuableStatement);
  int beginit = stack.getCurTokenIndex();
  stack.skipExpression();
  int beginblock = stack.getCurTokenIndex();
  stack.skipStatement();
  int endblock = stack.getCurTokenIndex();
  stack.setCurTokenIndex(beginit);
  int tk = stack.nextToken();
  if (tk != '(')
    ELOG("except '(' after 'foreach'");
  PropsScope propsscope(stack.props);
  RString ident = stack.parseIdentifier("");
  tk = stack.nextToken();
  if (tk != StreamTokenizer::TT_WORD)
    ELOG("Expect word after 'foreach (TypeOrVar)");
  RString s = stack.curTokenAsString();
  RString typeName;
  RString varName;
  if (s->equals("in") == false)
  {
    typeName = ident;
    varName = s;
    tk = stack.nextToken();
    if (stack.curTokenAsString()->equals("in") == false)
      ELOG("Expect 'in' after 'foreach (Type varname'");
    //RClass cls = Class::forName(typeName);
    const ClazzInfo* ci = stack.findType(typeName);
    stack.props->create(varName, ci);
  }
  else
  {
    varName = ident;
    stack.props->set(varName, new DmiObject(ScriptVar()));
  }
  PARSEEXECUTE(ExpressionParseNode().);
  //stack.resolveVarOnTop();
  RDmiObject exprObj = new DmiObject(stack.pop().val);
  RDmiObjectIterator it = getIterator(exprObj);

  RDmiObject obj = it->next();
beginLoop:
  while (obj != Nil)
  {
    stack.props->assign(varName, obj);
    stack.setCurTokenIndex(beginblock);
    PARSEEXECUTE(StatementParseNode().);
    if (stack.executionFlags & EFActiveContinue)
    {
      stack.executionFlags &= ~EFActiveContinue;
      obj = it->next();
      goto beginLoop;
    }
    else if (stack.executionFlags & EFActiveBreak)
    {
      stack.executionFlags &= ~EFActiveBreak;
      break;
    }

    obj = it->next();
  }
  stack.setCurTokenIndex(endblock);
  return true;
}

bool
DoParseNode::parseExecute(PEStack& stack)
{
  acdk::lang::sys::core_setbit_scope<int> _exeFlags(stack.executionFlags, EFBreakableStatement | EFContinuableStatement);

  int beginloop = stack.getCurTokenIndex();
  stack.skipStatement();
  int tk = stack.nextToken();
  ASCLITERAL(while);
  if (stack.curTokenAsString()->equals(lit_while) == false)
    ELOG("Expecting 'while' after 'do <expression> ");
  int begintest = stack.getCurTokenIndex();
  stack.skipExpression();
  tk = stack.nextToken();
  if (tk != ';')
    ELOG("Expecting ';' after 'do <statement> while (<expression>)'");
  int enddowhile = stack.getCurTokenIndex();
  

startLoop:
  stack.setCurTokenIndex(beginloop);
  PARSEEXECUTE(StatementParseNode().);
  if (stack.executionFlags & EFActiveContinue)
  {
    stack.executionFlags &= ~EFActiveContinue;
    goto startLoop;
  }
  else if (stack.executionFlags & EFActiveBreak)
  {
    stack.executionFlags &= ~EFActiveBreak;
    stack.setCurTokenIndex(enddowhile);
    return true;
  }
  stack.setCurTokenIndex(begintest);
  PARSEEXECUTE(ExpressionParseNode().);
  stack.resolveVarOnTop();
  bool test = stack.pop().val.isTrue(stack.props->getCastFlags());
  if (test == true)
      goto startLoop;
  return true;
}

bool
BreakParseNode::parseExecute(PEStack& stack)
{
  if ((stack.executionFlags & EFBreakableStatement) == 0)
    ELOG("No 'break'able context");
  stack.executionFlags |= EFActiveBreak;
  int tk = stack.nextToken();
  if (tk != ';')
    ELOG("Expection ';' after 'break'");
  return true;
}

bool
ContinueParseNode::parseExecute(PEStack& stack)
{
  if ((stack.executionFlags & EFContinuableStatement) == 0)
    ELOG("No 'continue'able context");
  stack.executionFlags |= EFActiveContinue;
  int tk = stack.nextToken();
  if (tk != ';')
    ELOG("Expection ';' after 'break'");
  return true;
}

bool
UsingParseNode::parseExecute(PEStack& stack)
{
  /*
    using acdk.lang;
    using acdk.lang.System;
    using acdk.lang.System.out;
  */
  RString s = stack.parseIdentifier("");
  stack.addUsing(s);
  int tk = stack.nextToken();
  if (tk != ';')
    ELOG("Expecting ';' after 'using <component>'");
  return true;
  /*
  int tk = stack.nextToken();
  RString ut = stack.curTokenAsString();
  if (tk != StreamTokenizer::TT_WORD || 
      (ut->equals("class") == false && ut->equals("namespace") == false))
    ELOG("Expecting 'class' or 'namespace' after 'using'");
  RString s = stack.parseIdentifier("");
  if (ut->equals("class") == true)
    stack.props->appendStringArrayVal("._using_class", s, PropsNoWarnRead);
  else
    stack.props->appendStringArrayVal("._using_namespace", s, PropsNoWarnRead);
  return true;
  */
}


bool
ThrowParseNode::parseExecute(PEStack& stack)
{
  PARSEEXECUTE(ExpressionParseNode().);
  stack.resolveVarOnTop();
  RThrowable ex = (RThrowable)stack.pop().val.getObjectVar();
  CFGSCRIPT_WRAPP_EXT_INVOKE(
    THROW_INSTANCE(ex);
  )
  return true;
}


bool
TryCatchParseNode::parseExecute(PEStack& stack)
{
  int ep = stack.getBlockEndPos();
  ASCLITERAL(catch);
  ASCLITERAL(finally);
  try {
    PARSEEXECUTE(StatementParseNode().);
  /*} catch (RScriptException ex) {
    throw;
    */
  } catch (RThrowable ex) {
    stack.setCurTokenIndex(ep);
    int tk;
nextCatchBlock:
    tk = stack.nextToken();
    if (stack.curTokenAsString()->equals(lit_catch) == true)
    {
      tk = stack.nextToken();
      if (tk != '(')
        ELOG("Expect '(' after 'catch'");
      //RClass cl = Class::forName(stack.parseIdentifier(""));
      const ClazzInfo* ci = stack.findType(stack.parseIdentifier(""));
      RString exvarname;
      tk = stack.nextToken();
      if (tk != ')')
      {
        if (tk != StreamTokenizer::TT_WORD)
          ELOG("Expect variable identifier after 'catch (<exception_type>'");
        exvarname = stack.curTokenAsString();
        tk = stack.nextToken();
        if (tk != ')')
          ELOG("Expect ')' after 'catch (<exception_type> [<varname>]'");
      }
      if (ci->assignableFrom(ex->getClazzInfo()) == true)
      {
        PropsScope propsscope(stack.props);
        if (exvarname != Nil)
          stack.props->create(exvarname, new DmiObject(&ex));
        PARSEEXECUTE(StatementParseNode().);
nextSkip:
        tk = stack.nextToken();
        if (stack.curTokenAsString()->equals(lit_catch) == true)
        {
          if (stack.skipExpression() == false)
            return false;
          if (stack.skipStatement() == false)
            return false;
          goto nextSkip;
        }
        else if (stack.curTokenAsString()->equals(lit_finally) == true)
        {
          PARSEEXECUTE(StatementParseNode().);
          return true;
        }
        else
        {
          stack.pushBack();
          return true;
        }
      }
      else
      {
        if (stack.skipStatement() == false)
          return false;
        goto nextCatchBlock;
      }

    }
    else if (stack.curTokenAsString()->equals(lit_finally) == true)
    {
      PARSEEXECUTE(StatementParseNode().);
    }
    // unhandled
    THROW_INSTANCE(ex);
  }
  
  while (true)
  {
    int tk = stack.nextToken();
    if (tk == StreamTokenizer::TT_EOF)
      return true;
    if (stack.curTokenAsString()->equals(lit_catch) == true)
    {
      if (stack.skipExpression() == false)
        return false;
      if (stack.skipStatement() == false)
        return false;
    }
    else if (stack.curTokenAsString()->equals(lit_finally) == true)
    {
      PARSEEXECUTE(StatementParseNode().);
      return true;
    }
    else
    {
      stack.pushBack();
      return true;
    }
  }
  return true;
}

/**
  void foo()  
  {
    synchronized(this) 
    {

    }
  }
*/
bool
SynchronizeParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (tk != '(')
    ELOG("Expecting '(' after 'synchronized'");
  PARSEEXECUTE(ExpressionParseNode().);
  stack.resolveVarOnTop();
  RObject obj = stack.pop().val.getObjectVar();
  tk = stack.nextToken();
  if (tk != ')')
    ELOG("Expecting ')' after 'synchronized (<expression>'");
  SYNCOBJECT(obj);
  PARSEEXECUTE(StatementParseNode().);
  return true;
}

/**
  switch (<expression>) 
  {
  case <expression>: 
    break;
  case <expression>:
  default:
    
  }
*/
bool
SwitchParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (tk != '(')
    ELOG("Expect '(' after 'switch'");
  PARSEEXECUTE(ExpressionParseNode().);
  tk = stack.nextToken();
  if (tk != ')')
    ELOG("Expect ')' after 'switch (<Expression>");

  PEVal switchExprVal = stack.pop();
  int endSwitch = stack.getBlockEndPos();
  
  tk = stack.nextToken();
  if (tk != '{')
    ELOG("Expect '{' after 'switch (<Expression>)'");
  
  acdk::lang::sys::core_setbit_scope<int> _exeFlags(stack.executionFlags, EFBreakableStatement);
  enum Expect 
  { 
    SeekCase,
    ExecCase,
    SkipCase,
    ExecDefault,
    SkipCaseExpr, // will continue with ExecCase after skip
  };
  
  tk = stack.nextToken();
  if (tk == '}')
    return true;
  int defaultPos = -1;
  Expect expect = SeekCase;
  bool caseExecuted = false;
  ASCLITERAL(case);
  ASCLITERAL(default);
  ASCLITERAL(break);
  while (true)
  {
    switch (expect)
    {
    case SeekCase:
      {
        RString caseOrDefault = stack.curTokenAsString();
        if (caseOrDefault->equals(lit_default) == true)
        {
          tk = stack.nextToken();
          if (tk != ':')
            ELOG("Expect ':' after 'default' in switch");
          defaultPos = stack.getCurTokenIndex();
          expect = SkipCase;
          break;
        }
        else if (caseOrDefault->equals(lit_case) == true)
        {

          do { 
            bool wasOperatorCall = false;
            tk = stack.nextToken();
            RString stk = stack.curTokenAsString();
            if (isOperator(stack.curSourceToken()) == true && (getOperatorFlags(stk) & OpSuffix) != OpSuffix)
            {
              stack.pushBack();
              stack.push(switchExprVal);
              PARSEEXECUTE(ExpressionParseNode1().);
              wasOperatorCall = true;
            }
            else
            {
              stack.pushBack();
              PARSEEXECUTE(ExpressionParseNode().);
            }
            PEVal testVal = stack.pop();
            if ((wasOperatorCall == true && testVal.val.isTrue(stack.props->getCastFlags()) == true) || 
                (wasOperatorCall == false &&  bool(testVal.val.equal(switchExprVal.val)) == true))
            {
                expect = SkipCaseExpr;
              break;
            }
            tk = stack.nextToken();
            if (tk == ',')
              continue;
            if (tk == ':')
            {
               expect = SkipCase;
              break;
            }
            ELOG("Expect ':' or ',' after 'case <Expression>' in switch");
          } while (expect == SeekCase);
         
        }
        else
          ELOG("Expect 'case' or 'default' in switch");
        break;
      }
    case SkipCase:
      {
        tk = stack.nextToken();
        if (tk == '}')
        {
          expect = ExecDefault;
          break;
        }
        RString caseOrDefault = stack.curTokenAsString();
        if (caseOrDefault->equals(lit_case) == true || caseOrDefault->equals(lit_default) == true)
        {
          expect = SeekCase;
          break;
        }
        stack.pushBack();
        stack.skipStatement();
        break;
      }
    case ExecCase:
      {
        caseExecuted = true;
        tk = stack.nextToken();
        if (tk == '}')
          return true;
        RString cts = stack.curTokenAsString();
        if (cts->equals(lit_break) == true)
        {
          tk = stack.nextToken();
          if (tk != ';')
            ELOG("Expect ';' after 'break'");
          stack.setCurTokenIndex(endSwitch);
          return true;
        }
        else if (cts->equals(lit_case) == true || cts->equals(lit_default) == true)
        {
          expect = SkipCaseExpr;
          break;
        }
        else
        {
          stack.pushBack();
          PARSEEXECUTE(StatementParseNode().);
          if (stack.executionFlags & EFActiveBreak)
          {
            stack.executionFlags &= ~EFActiveBreak;
            stack.setCurTokenIndex(endSwitch);
            return true;
          }
        }
        break;
      }
    case ExecDefault:
      {
        if (defaultPos == -1)
          return true;
        stack.setCurTokenIndex(defaultPos);
        expect = ExecCase;
        break;
      }
    case SkipCaseExpr:
      {
        tk = stack.nextToken();
        //RString cts = stack.curTokenAsString();
        if (tk == ':')
        {
          expect = ExecCase;
          break;
        }
        stack.pushBack();
        stack.skipExpression(':');
        break;
      }
    }
  }
  
  return true;
}

bool
TypeAliasParseNode::parseExecute(PEStack& stack)
{
  RString orgtype = stack.parseIdentifier("");
  RString aliastype = stack.parseIdentifier("");
  int tk = stack.nextToken();
  if (tk != ';')
    ELOG("Expect ';' after 'typealias TypeName TypeName'");
  stack.addTypeAlias(aliastype, orgtype);
  return true;
}

bool
LogParseNode::parseExecute(PEStack& stack)
{
  int tk = stack.nextToken();
  if (tk == -1)
    return false;
  RString cts = stack.curTokenAsString();
  bool isNamed = false;
  ASCLITERAL(ACDK_NLOG);
  if (cts->equals(lit_ACDK_NLOG) == true)
    isNamed = true;
  tk = stack.nextToken();
  if (tk != '(')
    ELOG("Except '(' after ACDK_xLOG");
  RString logname;;
  if (isNamed == true)
  {
    tk = stack.nextToken();
    logname = (RString)stack.curTokenValue().getObjectVar();
    tk = stack.nextToken(); // ,
  }
  RString ll = stack.parseIdentifier("");
  const ClazzEnumValueInfo* ev = ClazzEnumInfo::findEnumValue(ll, "acdk/util/logging");
  if (ev == 0)
    ELOG("Cannot find LogLevel: " + ll);
  int ill = ev->value;
  ::acdk::util::logging::RLogger logger;
  tk = stack.nextToken(); // ,
  if (logname == Nil)
    logger = ::acdk::util::logging::LogManager::getRootLogger();
  else
    logger = ::acdk::util::logging::LogManager::getLogger(logname);
  if (logger->doLog(ill) == false)
  {
    stack.skipStatement();
    return true;
  }
  
  PARSEEXECUTE(ExpressionParseNode().);
  RString msg = (RString)stack.pop().val.getObjectVar();
  RString f = stack.script->getFileName();
  int lineNo = stack.curCharStreamPos().linePos;
  logger->log(ill, logname, msg, f->c_str(), lineNo);
  stack.skipStatement();
  return true;
}

} // namespace cfgscript
} // namespace acdk 
  

