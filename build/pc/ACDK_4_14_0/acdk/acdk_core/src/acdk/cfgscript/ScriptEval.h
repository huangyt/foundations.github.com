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

#ifndef acdk_cfgscript_ScriptEval_h
#define acdk_cfgscript_ScriptEval_h
#if !defined(DOXYGENONLY)

#include "ScriptSource.h"
#include "ScriptException.h"
#include "Script.h"

#include "SourceTokenizer.h"
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>

namespace acdk {
namespace cfgscript {

using namespace acdk::lang::dmi;


//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)

RString spaces(int count);

#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << ThreadID::getCurrentThreadID().getId() << ": "; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)
#else
#define DOUT(strexpr) do { } while(false)
#endif

#define ELOG(msg) \
do { \
  ACDK_NLOG("acdk.cfgscript", Error, stack.getPositionAsString() + ": " + msg); \
  if (ExecutionStack::throwOnFail() == true) \
    THROW1(ScriptException, stack.getPositionAsString() + ": " + msg); \
  return false; \
} while(false)


#define ELOG_INSTACK(msg) \
do { \
  ACDK_NLOG("acdk.cfgscript", Error, getPositionAsString() + ": " + msg); \
  if (ExecutionStack::throwOnFail() == true) \
    THROW1(ScriptException, getPositionAsString() + ": " + msg); \
  return false; \
} while(false)

#define PARSEEXECUTE(object) \
do { \
  DOUT("enter: " #object); \
  bool erg = object parseExecute(stack); \
  DOUT("leave: " #object); \
  if (erg == false) return false; \
  if (stack.executionFlags & EFActiveReturn) \
    return true; \
} while (false)


#define PARSEEXECUTE_INSTACK(object) \
do { \
  DOUT("enter: " #object); \
  bool erg = object parseExecute(*this); \
  DOUT("leave: " #object); \
  if (erg == false) return false; \
  if (executionFlags & EFActiveReturn) \
    return true; \
} while (false)

#define PARSEEXECUTE_EX(object) \
do { \
try { \
  if (object parseExecute(stack) == false) \
    return false; \
  if (stack.executionFlags & EFActiveReturn) \
    return true; \
} catch (RScriptException ex) { \
  throw; \
} catch (RThrowable ex) { \
  stack.activeException = ex; \
  stack.executionFlags |= EFActiveException; \
  return true; \
} \
} while (false)


/* old
#define CFGSCRIPT_WRAPP_EXT_INVOKE( Code ) \
  stack.script->pushExecutionStack(ExecutionStack(stack.script, stack.curCharStreamPos(), stack.script->_currentClazzMethod)); \
  Code \
  stack.script->popExecutionStack();
*/
#define CFGSCRIPT_WRAPP_EXT_INVOKE( Code ) \
  Code

foreign
enum Token
{
  TT_VALUE    = 1000,
  TT_REFVALUE = 1001,
  TT_IDENT  =   1002,
  TT_ARGLABEL = 1003,
  TT_OPERATOR = 1004,
  TT_METHOD   = 1005,
  TT_CLASS    = 1006,
  TT_UNIT     = 1007,
  TT_FIELD    = 1008,
  TT_DOTOP    = 1009,

};

foreign
enum ResolveFlags
{
  ResolveNothing = 0x0000,
  /** 
    throws ex if cannot be resolved
  */
  ResolveFail  = 0x0001,
  /**
    Push Nil if cannot be resolveld
  */
  ResolvePushNil      = 0x0002,
  /**
    Pushes a reference to stack
  */
  ResolveToRef      = 0x0004,

  ResolveNoCompIdent = 0x0008,
  ResolveNotInUsings = 0x0010
};


ACDK_DECL_CLASS(LeftRightArgs);


struct PEVal
{
  Token tk;
  ScriptVar val;
  RLeftRightArgs args;
  PEVal(Token t, const ScriptVar& v) : tk(t), val(v) {}
  PEVal(Token t, IN(PEVal) left, IN(PEVal) right);
  static RString tokenToString(int tk);
  RString toCode();
  RString toString();
};

class LeftRightArgs
: extends acdk::lang::Object
{
public:
  PEVal left;
  PEVal right;
  LeftRightArgs(IN(PEVal) l, IN(PEVal) r) : left(l), right(r) {}
};



ACDK_DECL_CLASS(ScriptMethodInfo);

foreign
class ScriptMethodInfo
: extends acdk::lang::Object
{
public:
  int begintokenIndex;
  int endtokenIndex;
  RScript script;
  ScriptMethodInfo(IN(RScript) scrpt, int idxBegin, int idxEnd)
  : begintokenIndex(idxBegin)
  , endtokenIndex(idxEnd)
  , script(scrpt)
  {
  }
  virtual void getCollectableFields(FieldReferences& fields)
  {
    fields.push_back((RObject*)script._ref_this());
  }
};

ACDK_DECL_CLASS(UsingEntry);
foreign 
class UsingEntry
: extends acdk::lang::Object
, virtual public MetaInfoChangeListener
{
public:
  RString usingName;
  const acdk::lang::dmi::NamedScopedParentMetaInfo* _metaInfo;
  bool isVar;
  UsingEntry(IN(RString) use, const acdk::lang::dmi::NamedScopedParentMetaInfo* mi, bool isvar)
  : usingName(use)
  , _metaInfo(mi)
  , isVar(isvar)
  {
    MetaInfo::registerMetaInfoListener(this);
  }
  ~UsingEntry()
  {
    MetaInfo::unRegisterMetaInfoListener(this);
  }
  virtual void onReplaceMetaInfo(MetaInfo* oldMetaInfo, MetaInfo* newMetaInfo) 
  {
    if (_metaInfo == (const acdk::lang::dmi::NamedScopedParentMetaInfo*)oldMetaInfo)
      _metaInfo = (const acdk::lang::dmi::NamedScopedParentMetaInfo*)newMetaInfo;
  }
  virtual void getCollectableFields(FieldReferences& fields)
  {
    fields.push_back((RObject*)usingName._ref_this());
  }
};

struct PEStack
{
  typedef acdk::lang::sys::core_vector<PEVal> ValStack;
  RScript script;
  
  RProps props;
  ValStack vs;
  /**
    @see ExecutionFlags
  */
  int executionFlags;
  RThrowable activeException;
  SourceTokenizer tokenizer;
  bool evalAsRef;
  /// execute until higher than:
  RString leftOp;
  const ClazzInfo* curLeftDeclType;
  PEStack(IN(RScript) scr, IN(RProps) p)  
  : script(scr)
  , props(p)
  , vs()
  , executionFlags(0) 
  , tokenizer(scr->getTokenized())
  , evalAsRef(false)
  , curLeftDeclType(0)
  {
    _init();
  }
  PEStack(IN(RScript) scr, IN(RProps) p, int beginTkIdx, int endTkIdx)  
  : script(scr)
  , props(p)
  , vs()
  , executionFlags(0) 
  , tokenizer(scr->getTokenized())
  , evalAsRef(false)
  , curLeftDeclType(0)
  {
    tokenizer._tokenIdx = beginTkIdx;
    tokenizer._endTokenIdx = endTkIdx;
    _init();
  }
  void _init();
  void push(Token tk, const ScriptVar& sv) 
  { 
    push(PEVal(tk, sv)); 
  }
  void push(const PEVal& v)  
  { 
    vs.push_back(v); 
    DOUT("Push: " << getStackAsString());
  }
  void pushValOrIdent(const SourceToken& stk)
  {
    if (stk.tk == acdk::io::StreamTokenizer::TT_WORD)
      push(TT_IDENT, stk.value);
    else 
      push(TT_VALUE, stk.value);
  }
  PEVal& top() 
  {
    if (vs.empty() == true)
      THROW1(ScriptException, "Eval Stack underflow");
    return vs.back();
  }
  bool isStackEmtpy() { return vs.empty(); }
  PEVal pop() 
  {
    PEVal ret = top();
    DOUT("Pop: " << getStackAsString());
    vs.pop_back();
    return ret;
  }
  /**
    idx == 0 is top idx = 1 top -1 
    */
  PEVal& peek(int idx)
  {
    return vs[vs.size() - idx - 1];
  }
  int nextToken()
  {
    int tk = tokenizer.nextToken();
    ExecutionStack::setCurrentTokenIndex(tokenizer.getCurrentTokenIndex());
    //script->setCurrentTokenIndex(tokenizer.getCurrentTokenIndex());
    DOUT(">> [" << tokenizer.getCurrentTokenIndex() << "], " << String::valueOf(tk) << ": " << (tk != -1 ? tokenizer.getCurTokenAsCode() : String::emptyString()));
    return tk;
  }
  void pushBack() 
  {
    DOUT("<< [" << tokenizer.getCurrentTokenIndex() << "], " << tokenizer.getCurTokenAsCode());
    tokenizer.pushBack();
  }
  RString curTokenAsCode() 
  {
    SourceToken& stk = tokenizer.curSourceToken();
    return acdk::io::StreamTokenizer::toCode(stk.tk, stk.value);
  }
  RString curTokenAsString() 
  {
    return curTokenAsCode();
  }
  acdk::lang::dmi::ScriptVar& curTokenValue() { return tokenizer.curSourceToken().value; }
  int curToken() { return tokenizer.curSourceToken().tk; }
  INOUT(SourceToken) curSourceToken() { return tokenizer.curSourceToken(); }
  int getCurTokenIndex() { return tokenizer.getCurrentTokenIndex(); }
  void setCurTokenIndex(int tokenIndex) { tokenizer.setCurrentTokenIndex(tokenIndex); }
  
  RString getStackAsString()
  {
    StringBuffer sb;
    for (ValStack::iterator it = vs.begin(); it != vs.end(); ++it)
    {
      sb.append(" | ");
      sb.append(it->toString());
    }
    return sb.toString();
  }
  bool hasActiveEx() { return (executionFlags & EFActiveException) == EFActiveException; }
  RString getPositionAsString()
  {
    acdk::io::CharStreamPos csp = tokenizer.curSourceToken().sourcePos;
    return SBSTR(script->getFileName() << "(" << csp.linePos + 1 << "," << csp.columnPos << "): ");
  }
  acdk::io::CharStreamPos curCharStreamPos() { return tokenizer.curSourceToken().sourcePos; }
  //INOUT(RSourceTokenizer) getTokenizer() { return script->getTokenizer(); }
  /**
    @see ResolveFlags
  */
  bool resolveVarOnTop(int flags = ResolveFail);
  /**
    read until '}' or ';' end statement
  */
  bool skipStatement();
  /** skip all token until next line */
  bool skipLine();
  /**
    return end of block token position
  */
  int getBlockEndPos();

  /**
    skip expression
    optional token where also to stop (only if expression is not wrapped with ()
  */
  bool skipExpression(int stopTk = 0);
  /**
    skip expression until an operator with equal or lower prio than given
  */
  bool  skipExpressionUntilOpLowerOrEqual(int opPrio);

  /**
    read in a identifier a.b.c
  */
  RString parseIdentifier(IN(RString) begin);
  /**
    resolve TT_DOTOP 
    @see ResolveFlags
  */
  bool resolveDopOp(int flags);

  void addUsing(IN(RString) s);
  void addUsingVar(IN(RString) s, IN(RObject) var);
  void addUsingType(const acdk::lang::dmi::ClazzInfo* metaInfo);
  void _appendUsing(IN(RUsingEntry) ue);
  //RStringArray getUsings() { return props->getAllStringArrayVal("._cfgscript_using", PropsParentRead | PropsNoWarnRead); }
  
  void addTypeAlias(IN(RString) aliasType, IN(RString) realType)
  {
    props->appendStringArrayVal("._cfgscript_typealias", aliasType, PropsNoParentWrite | PropsNoParentRead);
    props->appendStringArrayVal("._cfgscript_typealias", realType, PropsNoParentWrite | PropsNoParentRead);
  }
  /**
    return Nil if no alias
  */
  RString getTypeAlias(IN(RString) alias);
  /**
    return the alias of param or orignal name
  */
  RString getAlias(IN(RString) alias);
  /**
    search for type name in aliases and usings
    if no alias or using is found, return orginal typname
  */
  RString getAliasOrUsingTypeName(IN(RString) typname, bool tryLoad = true);
  void makeTopInOf()
  {
    PEVal& val = top();
    if (val.val.flags & MiAiIn && ((val.val.flags & MiAiOut) == 0))
      return;
    val.val = val.val.inOf();
  }
  /**
    find a type with given name
    @throw if throwOnFail = true (default) ClassNotFoundException if type cannot be found
  */
  const ClazzInfo* findType(IN(RString) str, bool throwOnFail = true);
  /**
    search for element
    @param dmiFlags requested type acdk::lang::dmi::MetaInfoFlags
  */

  const MetaInfo* findElement(IN(RString) str, int dmiFlags = 0);
  const MetaInfo* getAliasOrUsingInfo(IN(RString) typname, int dmiFlags, bool tryLoad);
  /**
    @return Nil if tpName cannot be found
  */
  RUsingEntry findUsingChild(IN(RString) tpName, int dmiFlags = 0, const MetaInfo** foundPtr = 0);
  const NamedScopedParentMetaInfo* resolveNewUsing(IN(RString) use, OUT(bool) isVariable);
  
};

foreign
class PropsScope
{
public:
  RProps& _props;
  int _oldPropsFlags;
  PropsScope(RProps& props, int propsflags = PropsParentRead | PropsParentWrite) 
  : _props(props) 
  , _oldPropsFlags(0)
  {
    _props = new Props("block", propsflags, props);
    
    _props->set("__props", new DmiObject((RObject)props), PropsNoParentWrite);
    ExecutionStack::getTop()->setScopeProps(_props);
  }
   PropsScope(RProps& props, RProps& newRoot, int propsflags = PropsParentRead | PropsParentWrite) 
  : _props(props) 
  {
    _props = newRoot;
    _oldPropsFlags = newRoot->getDefaultFlags();
    newRoot->setDefaultFlags(propsflags);
    ExecutionStack::getTop()->setScopeProps(_props);

  }
  ~PropsScope() 
  { 
    _props->unset("__props", PropsNoParentWrite);
    _props->setDefaultFlags(_oldPropsFlags);
    _props = _props->getParentProps(); 
    ExecutionStack::getTop()->setScopeProps(_props);
  }
  
};



foreign
class ParseNode
: extends acdk::lang::Object
{
public:
  virtual bool parseExecute(PEStack& stack) = 0;
};

#define STD_PARSENODE_DECL(Name) \
ACDK_DECL_CLASS(Name); \
class Name \
: extends ParseNode \
{ \
public: \
  virtual bool parseExecute(PEStack& stack); \
}

#define STD_SKIPABLE_PARSENODE_DECL(Name) \
ACDK_DECL_CLASS(Name); \
class Name \
: extends ParseNode \
{ \
public: \
  virtual bool parseExecute(PEStack& stack); \
}

#define EXTERN_ASCLITERAL(strlit) extern acdk::lang::StaticAsciiLiteral lit_##strlit
#define GLOBAL_ASCLITERAL(strlit) acdk::lang::StaticAsciiLiteral lit_##strlit(#strlit)

EXTERN_ASCLITERAL(this);
EXTERN_ASCLITERAL(super);


} // namespace cfgscript
} // namespace acdk 

#endif //!defined(DOXYGENONLY)

#endif //acdk_cfgscript_ScriptEval_h
