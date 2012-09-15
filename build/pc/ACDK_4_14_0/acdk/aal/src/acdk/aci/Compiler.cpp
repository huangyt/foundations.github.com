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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/Compiler.cpp,v 1.21 2005/02/05 10:44:51 kommer Exp $



#include "Compiler.h"
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>
#include "parser/SyntaxParseNode.h"
#include "parser/KeywordParseNode.h"
#include "vm/OpCodeStm.h"
#include "ast/Terminal.h"

bool operator<(IN(RString) f, IN(RString) s)
{
  return f->compareTo(s) < 0;
}



namespace acdk {
namespace aci {

using namespace acdk::lang::dmi;

Compiler::Compiler(IN(RScanner) sc)
//: _codeParserTable(new acdk::util::HashMap())
//: _notReduceRules(new acdk::util::HashSet())
//, _keyWords(new acdk::util::HashSet())
//, _terminals(new acdk::util::HashMap())
//: _parseEnv(new ParseEnv(sc))
: scanner(sc)
, _genLabelCounter(0)
, _opCodes(new ExecutableArray(0))
//, _asLeftHandExpr(false)
{
  if (scanner == Nil)
  {
    scanner = new Scanner("");
    _parseEnv = scanner->getParseEnv();
  }
  _codeStack.push(Nil);
}

RAstNode 
Compiler::parseComplete(IN(RString) ruleName)
{
  RAstNode code = parseRule(ruleName, Nil);
  if (code == Nil)
    return Nil;
  /* ### @todo fixme
  ScannerTokenStack ss(scanner);
  ScannerToken* st = ss.peek(1);
  if (st->token != Scanner::TT_EOF)
  {
    
    System::out->println("! Unreaded Token in Stream: " + st->sval);
    //THROW1(Exception, "! Unreaded Token in Stream: " + st->sval);
    return code;
  }
  */
  return code;
}


RAstNode 
Compiler::parseRule(IN(RString) ruleName, IN(RParseNode) parent)
{
  RParseNodeArray pa = _parseEnv->getParseNodes(ruleName);
  if (pa == Nil)
  {
    //RParseNode pn = tryLoadParseNode(ruleName);
    // ### @todo use ClassLoader to resolve ParseNode
    StringBuffer sb;
    sb << "Cannot find CodeParser for rule: '" + ruleName;
    if (parent != Nil)
      sb << "' in parent '" + parent->getNodeName() + "' syntax: " + parent->getSyntax();
    THROW1(Exception, sb.toString());
  }
  ACDK_NLOG("acdk.aci.Parser", Trace, 
        "Compiler::parse:" + getDoutIndent() + "-> " + ruleName /* + "[" + scanner->peek()->getSourceTextFragment() + "]" */);

  //DOUT("-> " << codetype->c_str() << " [" << scanner->peek()->sval->c_str() << "]");
  ++dout_indent;
  for (int i = 0; i < pa->length(); ++i)
  {
    CodeStackScope css(codeStack());
    RParseNode pnode = pa[i];
    RAstNode code = pnode->parse(this);
    if (code != Nil)
    {
      css.commit();
      ACDK_NLOG("acdk.aci.Parser", Trace, "Compiler::parse:" + getDoutIndent() + "<= " + ruleName);
      --dout_indent;
      return code;
    }
    else
    {
      --dout_indent;
      ACDK_NLOG("acdk.aci.Parser", Trace, "Compiler::parse:" + getDoutIndent() + "/= " + ruleName);
      ++dout_indent;
    }
  }
  --dout_indent;
  return Nil;
}

RParseNode 
Compiler::getParseNode(IN(RString) rulename)
{
  RParseNodeArray pa = _parseEnv->getParseNodes(rulename);
  if (pa == Nil || pa->length() == 0)
    return Nil;
  return pa[0];
}

void 
Compiler::registerSyntaxRule(IN(RSyntaxParseNode) node, bool notreduce)
{
  node->onRegister(this);
  /*
  RString syntax = node->getSyntax();
  if (syntax != Nil)
  {
    if (syntax->startsWith("/") == true && syntax->endsWith("/") == true)
    {
      // contains regular expr 
    }
    else
    {
    RString matchex = "('(?:(?:\\\\')|[^'])+?')";
    acdk::text::RegExp regex(matchex);
    int slots[4];
    while (regex.match(syntax, slots, 4) >= 0)
    {
      if (slots[0] != -1 && slots[1] != -1)
      {
        RString kw = syntax->substr(slots[0] + 1, slots[1] - 1);
        kw = kw->replace("\\'", "'");
        registerKeyword(kw);
      }
      syntax = syntax->substr(slots[1]);
    }
    }
  }
  */
  RString nodename = node->getNodeName();
  _parseEnv->addParseNode(nodename, &node);
  
  if (notreduce == false)
    return;
  _parseEnv->addNotReducedRule(nodename);
}

/*
int 
ParseEnv::addKeyword(IN(RString) kw, IN(RString) qkw, byte prio, int index = -1, int bindToSubEx = 0) 
{ 
  _currentParseFrame->_keyWords->add(kw); 
  return _scanner->registerTerminal(new KeywordParseNode("Keyword", kw));
    //return _scanner->registerTerminal(qkw, prio, index, bindToSubEx);
}
*/
int 
Compiler::registerKeyword(IN(RString) kw) 
{ 
  RTerminalParseNode pn = _parseEnv->getTerminal(kw);
  if (pn != Nil)
    return pn->getScannerTokenId();
  
  pn = new KeywordParseNode(kw, kw);
  registerTerminal(&pn, 5);
  return pn->getScannerTokenId();
}

int 
Compiler::registerTerminal(IN(RTerminalParseNode) node, byte prio)
{
  node->onRegister(this);
  _parseEnv->addTerminal(node->getNodeName(), &node);

  /*
  RString cd = node->getSyntax();
  RString matchex = "(\\<[A-Z_]+?\\>)"; // ALL_CAPS_IDENTIFIER has to be terminal
  acdk::text::RegExp regex(matchex);
  int slots[4];
  StringBuffer sb;
  while (regex.match(cd, slots, 4) > 0)
  {
    sb.append(cd->substr(0, slots[0]));
    RString kw = cd->substr(slots[0] + 1, slots[1] - 1);
    RParseNode n = _parseEnv->getTerminal(kw);
    if (n == Nil)
      THROW1(Exception, "Cannot find terminal definition for: " + kw);
    sb.append(n->getSyntax());
    cd = cd->substr(slots[1]);
  }
  sb.append(cd);
  node->setSyntax(sb.toString());

  //### todo? node->_tk = scanner->registerTerminal(node->getSyntax(), prio, -1, 0);
  //_terminals.put(node->getNodeName(), &node);
  //node->setScannerTokenId(_parseEnv->addKeyword(node->getNodeName(), node->getSyntax(), prio, -1, 0));
  // registerSyntaxRule(&node, true);
  */
  return node->getScannerTokenId();
}

/*
int 
Compiler::registerScannerTerminal(IN(RString) symbol, IN(RScannerTerminal) scannerterminal)
{
  RTerminalParseNode node = new TerminalParseNode(symbol, "");
  
  node->_tk = scanner->registerTerminal(scannerterminal);
  _parseEnv->addTerminal(symbol, &node);
  registerRule(&node, true);
  return node->_tk;
}
*/

void 
Compiler::printSyntax(IN(acdk::io::RPrintWriter) out)
{
  RAstNodeParserTable pt = _parseEnv->_currentParseFrame->_codeParserTable;
  CodeParserTable::RKeyArrayType keys = pt->getKeys();
  for (int i = 0; i < keys->length(); ++i)
  {
    RParseNodeArray ca = pt->get(keys[i]);
    for (int j = 0; j < ca->length(); ++j)
      ca[j]->printSyntax(out);
  }
}

acdk::cfgscript::RProps 
Compiler::getSyntaxParserEnv()
{
  acdk::cfgscript::RProps props = _parseEnv->getSyntaxScannerProps();
  if (props->hasValue("compiler") == true)
    return props;

  props->create("compiler", new DmiObject((RObject)this)); // #### @todo recursive reference
  props->create("curParseNode", ParseNode::clazzInfo());
  props->create("curTopAstNode", AstNode::clazzInfo());
  return props;
}

RDClazzInfo 
Compiler::findType(IN(RString) name, IN(RAstNode) code)
{
  RString corename = name;
  int dims = 0;
  int idx = corename->indexOf("[]");
  if (idx != -1)
  {
    dims = corename->substr(idx)->length() / 2;
    corename = corename->substr(0, idx);
  }

  RDClazzInfo td = code->findType(corename);
  if (td == Nil)
  {
    if (_globals == Nil)
      return Nil;
    td = _globals->getType(name);
  }
  if (dims == 0 || td == Nil)
    return td;
  const acdk::lang::dmi::ClazzInfo* ci = td->getImplClazzInfo()->createArrayClazzInfo(dims);
  /*
  const acdk::lang::dmi::ClazzInfo* ci = td->getImplClazzInfo();
  for (; dims > 0; --dims)
  {
    const acdk::lang::dmi::ClazzInfo* ar = acdk::lang::dmi::ClazzInfo::findArrayClazzInfo(ci);
    if (ar == 0)
      break;
    ci = ar;
  }
  for (; dims > 0; --dims)
  {
    ci = ::acdk::lang::Class::getUnInitializedSingeltonArrayClazz(ci); 
  }*/
  return DClazzInfo::getInstance(ci);
}

RSemanticElem 
Compiler::findSubSem(IN(RString) name, IN(RString) op, IN(RAstNode) code)
{
  RSemanticElem td = code->findSubSem(name, op);
  if (td != Nil)
    return td;
  /*
  RAstNode classcode = code->findParentCode("ClassDeclDef");
  if (classcode != Nil)
  {
    RSemanticElem csem = classcode->getSemanticElement();
    if (csem != Nil)
    {
      td = csem->findSubSem(name, op);
      if (td != Nil)
        return td;
    }
  }*/
  if (_globals == Nil)
    return Nil;
  return _globals->findSubSem(name, op);
}

/*
>=x returns >=
x returns x

*/
RString 
Compiler::parseOpOrIdent()
{
  /*
  ScannerTokenStack ss(scanner);
  ss.wantWs(true);

  ScannerToken* st = ss.fetch();
  while (st->token == Scanner::TT_WS)
    st = ss.fetch();

  if (st->token == Scanner::TT_EOF)
    return "";

  if (st->token == Scanner::TT_WORD)
  {
    ss.commit();
    return st->sval;
  }

  
  StringBuffer sb;
  if (st->token == Scanner::TT_WORD ||
        st->token == Scanner::TT_NUMBER ||
        st->token == Scanner::TT_STRING)
      sb.append(st->sval);
    else
      sb.append((char)st->token);
  
  while ((st = ss.peek()) && 
          st->token != Scanner::TT_WS && 
          st->token != Scanner::TT_EOF &&
          st->token != Scanner::TT_WORD &&
          st->token != Scanner::TT_NUMBER && 
          st->token != Scanner::TT_STRING
          )
  {
    st = ss.fetch();
    sb.append((char)st->token);
  }
  ss.commit();
  return sb.toString();
  */
  return Nil;
}

RString 
Compiler::genLabel(IN(RString) prefix)
{
  return prefix + (++_genLabelCounter);
}

// static
void 
Compiler::resolveLabels(IN(RExecutableArray) oca)
{
  std::map<RString, int> _labels;
  int i;
  for (i = 0; i < oca->length(); ++i)
  {
    RString l = oca[i]->getLabel();
    if (l != Nil && l->length() > 0)
      _labels[l] = i;
  }
  for (i = 0; i < oca->length(); ++i)
  {
    if (instanceof(oca[i], BranchOp) == true)
    {
      RBranchOp bop(oca[i]);
      if (bop->_label != Nil)
      {
        std::map<RString, int>::iterator it = _labels.find(bop->_label);
        if (it == _labels.end())
          THROW1(Exception, "Cannot find label definition: " + bop->_label);
        bop->_pc = it->second;
      }
    }
  }
}

void 
Compiler::execute(IN(RExecutableArray) oca)
{
  resolveLabels(oca);
  EvalEnv env(this);
  env.execute(oca);
}

void 
Compiler::execute(IN(RExecutableArray) oca, IN(REvalEnv) env)
{
  resolveLabels(oca);
  env->execute(oca);
  RObject ex = env->af().activeException();
  if (ex != Nil)
    ex->invoke("throwException");
}


void 
Compiler::addOpCode(IN(RExecutableArray) oca, IN(RExecutable) ex)
{
  ex->printOpCode(System::out);
  oca->append(ex);
}

RAstNode 
Compiler::createCode(IN(RString) name)
{
  return getParseNode(name)->createStandardAstNode();
}

RTerminalParseNode 
Compiler::getTerminal(IN(RString) rn)
{
  return _parseEnv->getTerminal(rn);
}


RCompiler 
Compiler::createSubTextParser(IN(RString) codetext, IN(RAstNode) topCode)
{
  RCompiler comp = new Compiler();
  comp->_parseEnv = _parseEnv;
  comp->scanner = scanner->createSubTextScanner(codetext);
  return comp;
}

REvalEnv 
Compiler::getCompilerEnv()
{
  if (_compilerEnv != Nil)
    return _compilerEnv;

  _compilerEnv = new EvalEnv(this);
  int idx = _globals->newVarDecl(0, "acdk.aci.Compiler", "compiler");
  _compilerEnv->af().crlv(idx);
  _compilerEnv->push(inOf(RCompiler(this)));
  _compilerEnv->store(idx);
  
  return _compilerEnv;

}

void 
Compiler::compInvokeMethod(IN(REvalEnv) env)
{
  ActivationFrame& af = env->af();
  ScriptVar svflag = env->pop();
  RString funcname = env->pop().getStringVar();
  RObject obj = env->pop().getObjectVar();
    
  int argcount = EvalEnv::getArgCount(svflag);
  int dmiflags = EvalEnv::getDmiFlags(svflag);
  ScriptVarArray args(argcount);
  for (int i = argcount - 1; i >= 0; --i)
  {
    args[i] = env->pop();
  }
  AciDmiClient dmiclient(env);
  ScriptVar erg = compInvokeMethod(env, obj, funcname, args, Nil, dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(erg);
}

ScriptVar 
Compiler::compInvokeMethod(IN(REvalEnv) env, IN(RObject) target, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  try 
  {
    int dmiflags = EvalEnv::getDmiFlags(invokeflags);
    return target->invokeMethod(funcname, args, dc, Nil, dmiflags);
  } 
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
  return ScriptVar();
}

void 
Compiler::compInvokeStaticMethod(IN(REvalEnv) env)
{
  ScriptVar svflag = env->pop();
  RString funcname = env->pop().getStringVar();
  RString classname= env->pop().getStringVar();
    
  int argcount = EvalEnv::getArgCount(svflag);
  int dmiflags = EvalEnv::getDmiFlags(svflag);
  ScriptVarArray args(argcount);
  for (int i = argcount - 1; i >= 0; --i)
  {
    args[i] = env->pop();
  }
  AciDmiClient dmiclient(env);
  ScriptVar retobj = compInvokeStaticMethod(env, classname, funcname, args, Nil, dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(retobj);
}

//virtual 
ScriptVar 
Compiler::compInvokeStaticMethod(IN(REvalEnv) env, IN(RString) classname, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  
  try {
    int dmiflags = EvalEnv::getDmiFlags(invokeflags);
    return StdDispatch::invokeStaticMethod(classname, funcname, args, dc, namedArgs, dmiflags);
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
    //af.checkReturnedException(env);
  }
  return ScriptVar();
  
}

void 
Compiler::compInvokeHashMethod(IN(REvalEnv) env)
{
  ActivationFrame& af = env->af();
  ScriptVar svflag = env->pop();
  int funchash = env->pop().getIntVar();
  RObject obj = env->pop().getObjectVar();
    
  int argcount = EvalEnv::getArgCount(svflag);
  int dmiflags = EvalEnv::getDmiFlags(svflag);
  ScriptVarArray args(argcount);
  for (int i = argcount - 1; i >= 0; --i)
  {
    args[i] = env->pop();
  }
  AciDmiClient dmiclient(env);
  ScriptVar erg = compInvokeHashMethod(env, obj, funchash, args, Nil, dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(erg); 
}

ScriptVar 
Compiler::compInvokeHashMethod(IN(REvalEnv) env, IN(RObject) target, int methodhash, ScriptVarArray& args,
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  try 
  {
    int dmiflags = EvalEnv::getDmiFlags(invokeflags);
    ScriptVar erg;
    target->standardDispatch(Nil, erg, args, dc, namedArgs, dmiflags, target->getClazzInfo(), (const ClazzMethodInfo*)methodhash);
    return erg;
  } 
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
  return ScriptVar();
}
void 
Compiler::compInvokeStaticHashMethod(IN(REvalEnv) env)
{
  ScriptVar svflag = env->pop();
  int methodhash = env->pop().getIntVar();
  RString classname= env->pop().getStringVar();
    
  int argcount = EvalEnv::getArgCount(svflag);
  int dmiflags = EvalEnv::getDmiFlags(svflag);
  ScriptVarArray args(argcount);
  for (int i = argcount - 1; i >= 0; --i)
  {
    args[i] = env->pop();
  }
  AciDmiClient dmiclient(env);
  ScriptVar retobj = compInvokeStaticHashMethod(env, classname, methodhash, args, Nil, dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(retobj);
}

ScriptVar 
Compiler::compInvokeStaticHashMethod(IN(REvalEnv) env, IN(RString) classname, int methodhash, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  try {
    int dmiflags = EvalEnv::getDmiFlags(invokeflags);
    ScriptVar ret;
    StdDispatch::StandardDispatch(classname, "", ret, args, dc, namedArgs, dmiflags, 0, (const ClazzMethodInfo*)(void*)methodhash);
    return ret;    
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
    //af.checkReturnedException(env);
  }
  return ScriptVar();
}

void 
Compiler::compNewObject(IN(REvalEnv) env)
{
  jlong invokeflags = env->pop();
  RString funcname = env->pop().getStringVar();
  int argnum = EvalEnv::getArgCount(invokeflags);
  ScriptVarArray args(argnum);
  for (int i = argnum - 1; i >= 0; --i)
  {
    args[i] = env->pop();
  }
  AciDmiClient dmiclient(env);
  ScriptVar nobj = compNewObject(env, funcname, args, Nil, dmiclient, invokeflags);
  if (env->af().checkReturnedException(env) == false)
    env->push(nobj);
}

ScriptVar 
Compiler::compNewObject(IN(REvalEnv) env, IN(RString) funcname, ScriptVarArray& args, 
                                 IN(::acdk::lang::RStringArray) namedArgs, DmiClient& dc, jlong invokeflags)
{
  try {
    int dmiflags = EvalEnv::getDmiFlags(invokeflags);
    return StdDispatch::New(funcname, args, dc);
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
  return ScriptVar();
}

void 
Compiler::compPeek(IN(REvalEnv) env)
{
  ScriptVar svflag = env->pop();
  ScriptVar svfieldname = env->pop();
  ScriptVar svobj = env->pop();
  AciDmiClient dmiclient(env);
  ScriptVar ret = compPeek(env, svobj.getObjectVar(), svfieldname.getStringVar(), dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(ret);
}

ScriptVar 
Compiler::compPeek(IN(REvalEnv) env, IN(RObject) target, IN(RString) fieldname, DmiClient& dc, jlong invokeflags)
{
  try
  {
    return target->peek(fieldname, EvalEnv::getDmiFlags(invokeflags));
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
  return ScriptVar();
}

void 
Compiler::compPoke(IN(REvalEnv) env)
{
  ScriptVar svflag = env->pop();
  ScriptVar svfieldname = env->pop();
  ScriptVar svobj = env->pop();
  ScriptVar newvar = env->pop();
  AciDmiClient dmiclient(env);
  compPoke(env, svobj.getObjectVar(), svfieldname.getStringVar(), newvar, dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(newvar);
    
}

void 
Compiler::compPoke(IN(REvalEnv) env, IN(RObject) target, IN(RString) fieldname, IN(ScriptVar) value, DmiClient& dc, jlong invokeflags)
{
  try
  {
    target->poke(fieldname, value, EvalEnv::getDmiFlags(invokeflags));
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
}

void 
Compiler::compPeekStatic(IN(REvalEnv) env)
{
  ScriptVar svflag = env->pop();
  ScriptVar svfieldname = env->pop();
  ScriptVar svobj = env->pop();
  AciDmiClient dmiclient(env);
  ScriptVar erg = compPeekStatic(env, svobj.getStringVar(), svfieldname.getStringVar(), dmiclient, svflag);
  if (env->af().checkReturnedException(env) == false)
    env->push(erg);
    
}

ScriptVar 
Compiler::compPeekStatic(IN(REvalEnv) env, IN(RString) classname, IN(RString) fieldname, DmiClient& dc, jlong invokeflags)
{
  try
  {
    return StdDispatch::peek_static(classname, fieldname, EvalEnv::getDmiFlags(invokeflags));
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
  return ScriptVar();
}

void 
Compiler::compPokeStatic(IN(REvalEnv) env)
{
  ScriptVar svflag = env->pop();
  ScriptVar svfieldname = env->pop();
  ScriptVar clsname = env->pop();
  ScriptVar newvar = env->pop();
  AciDmiClient dmiclient(env);
  compPokeStatic(env, clsname.getStringVar(), svfieldname.getStringVar(), newvar, dmiclient, svflag);
  if (env->af().checkReturnedException(env) == Nil)
    env->push(newvar);
}

void 
Compiler::compPokeStatic(IN(REvalEnv) env, IN(RString) classname, IN(RString) fieldname, IN(ScriptVar) value, DmiClient& dc, jlong invokeflags)
{
  try
  {
    poke_static(classname, fieldname, value, EvalEnv::getDmiFlags(invokeflags));
  }
  catch (RThrowable ex) 
  {
    ActivationFrame& af = env->af();
    af.throwException(env, ex, true);
  }
}

RObject 
ObjectArray_create(int dims, int argnum, int* args) 
{
  if (dims == 0)
    return Nil;
  int size  = 0;
  if (argnum > 0)
    size = args[0];
  RObjectArray oa = new ObjectArray(size);
  for (int i = 0; i < size; ++i)
  {
    oa[i] = ObjectArray_create(dims - 1, argnum - 1, args + 1);
  }
  return &oa;
}


void 
Compiler::compNewArray(IN(REvalEnv) env)
{
  jlong invokeflags = env->pop();
  RString classname = env->pop().getStringVar();
  int argnum = EvalEnv::getArgCount(invokeflags);
  int dimcount = EvalEnv::getNamedArgCount(invokeflags);
  const ClazzInfo* ci =  ClazzInfo::findClazzInfo(classname);
  // ## TODO check ci == 0
  int argsizes[256]; memset(argsizes, 0, sizeof(argsizes));
  for (int j = 0, i = argnum - 1; i >= 0; --i, ++j)
  {
    argsizes[j] = env->pop().getIntVar();
  }
  RObject obj;
  if (dimcount == 1 && ci->array_creator != 0)
  {
    obj = ci->array_creator(argsizes[0]);
  } 
  else
  {
    obj = new CastedObject(ObjectArray_create(dimcount, argnum, argsizes),
                             ci->createArrayClazzInfo(dimcount));
  }
  env->push(inOf(obj));
}

} // aci
} // acdk

