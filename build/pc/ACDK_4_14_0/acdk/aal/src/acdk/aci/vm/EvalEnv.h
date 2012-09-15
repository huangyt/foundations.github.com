// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/vm/EvalEnv.h,v 1.3 2004/11/21 21:55:57 kommer Exp $

#ifndef acdk_aci_vm_EvalEnv_h
#define acdk_aci_vm_EvalEnv_h

#include <acdk.h>
#include "../Config.h"

#include "../SymbolTable.h"
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/util/TreeMap.h>
#include <acdk/cfgscript/Props.h>

#include <map>



namespace acdk {
namespace aci {
namespace vm {

ACDK_DECL_CLASS(EvalEnv);


#define DBG_EVSC_DOUT(msg) DBG_OUT_IMPL(msg)
//#define DBG_EVSC_DOUT(msg) DBG_OUT_NOIMPL(msg)

class ACDK_ACI_PUBLIC EvalScope
: extends acdk::lang::Object
{
  typedef acdk::lang::sys::core_vector<acdk::lang::dmi::ScriptVar> Stack;
  Stack _stack;
public:
  void push(const acdk::lang::dmi::ScriptVar& sv)
  {
    _stack.push_back(sv);
  }
  acdk::lang::dmi::ScriptVar& top() 
  {
    if (_stack.size() == 0)
      THROW1(Exception, "EvalStack underflow");
    return _stack.back();
  }
  acdk::lang::dmi::ScriptVar pop()
  {
    if (_stack.size() == 0)
      THROW1(Exception, "EvalStack underflow");
     acdk::lang::dmi::ScriptVar ret = _stack.back();
     _stack.erase(_stack.end() - 1, _stack.end());
     return ret;
  }
  acdk::lang::dmi::ScriptVar& get(int idx) 
  {
    return _stack[idx];
  }
  int size() { return _stack.size(); }
};


class ACDK_ACI_PUBLIC VarScope
: extends acdk::lang::Object
{
  /// RVarDefinition -> RDmiObject
  
  typedef std::map<RVarDefinition, acdk::lang::dmi::ScriptVar> VarHeap;
  VarHeap _map;
  static acdk::lang::dmi::ScriptVar _nilScriptVar;
public:
  VarScope()
  {
  }
  /*
  void set(IN(RString) name, IN(RType) tp)
  {
    _map[Var(name, tp)] = tp->_sv;
  }
  */
  acdk::lang::dmi::ScriptVar& get(IN(RString) name) 
  {
    VarDefinition vd(0, name, Nil);
    VarHeap::iterator it = _map.find(&vd);
    if (it == _map.end())
      return _nilScriptVar;
    return (*it).second;
  }
  static bool isNil(const acdk::lang::dmi::ScriptVar& sv)
  {
    return &sv == &_nilScriptVar;
  }
  void newVar(IN(RVarDefinition) var, const acdk::lang::dmi::ScriptVar& initval)
  {
    _map[var] = initval;
  }
  bool assignVar(IN(RVarDefinition) var, const acdk::lang::dmi::ScriptVar& sv);
  
};

class ACDK_ACI_PUBLIC TypeScope
: extends acdk::lang::Object
{
  // RString -> RType
  acdk::util::TreeMap _map;
public:
  TypeScope()
  {
  }
  /*
  void set(IN(RString) name, IN(RType) typ)
  {
    _map.put(&name, &typ);
  }
  RType get(IN(RString) name)
  {
    return (RType)_map.get(&name);
  }*/
};

class ACDK_ACI_PUBLIC ActivationFrame
: extends acdk::lang::Object
{
protected:
  typedef acdk::lang::sys::core_vector<acdk::lang::dmi::ScriptVar> FrameData;
  FrameData _frameData;
  int _retPc;
  int _stackTop;
  /// will be set true if invoke, etc set an exception
  bool _exceptionReturned;
  /// current active exception
  RObject _activeException;
  /**
    stores the stacked local catch handlers as pcs
  */
  acdk::lang::sys::core_stack<int> _catchHandler;
public:

  ActivationFrame(IN(REvalEnv) env);
  int normalizeIdx(int idx)
  {
    if (idx >= 0)
    {
      if (idx >= _frameData.size())
        THROW1(Exception, "ActivationFrame out of index");
      return idx;
    }
    idx = _frameData.size() + idx;
    if (idx < 0 || idx >= _frameData.size())
      THROW1(Exception, "ActivationFrame out of index");
    return idx;
  }
  void crlv(int idx)
  {
    _frameData.ensureSize(idx + 1);
  }
  int crlv()
  {
    _frameData.ensureSize(_frameData.size() + 1);
    return _frameData.size();
  }

  acdk::lang::dmi::ScriptVar load(int idx);
  acdk::lang::dmi::ScriptVar loadRef(int idx);
  void store(int idx, const acdk::lang::dmi::ScriptVar& sv)
  {
    _frameData[normalizeIdx(idx)] = sv;
  }
  void push(const acdk::lang::dmi::ScriptVar& sv = acdk::lang::dmi::ScriptVar())
  {
    _frameData.push_back(sv);
  }
  /**
    Prepare calling a function
    - set argument
    - safe pc
  */
  void prepareCallee(IN(REvalEnv) env, IN(RExecutableArray) oca);
  /**
    - restore pc
  */
  void afterCallee(IN(REvalEnv) env);
  void enterTry(int catchpc) { _catchHandler.push(catchpc); }
  /**
    throws in current AF an exception
    @param ex the exception to be thrown
    @param returnedfromCall if the exception will be thrown by an external call
  */
  void throwException(IN(REvalEnv) env, IN(RThrowable) ex, bool returnedfromCall);
  RObject activeException() { return _activeException; }
  void clearException() { _activeException = Nil; }
  /**
    returns true if an active Exception returned
    invoke methods should not push the return values
    onto stack if this methods returns true
  */
  bool checkReturnedException(IN(REvalEnv) env);
  bool exceptionReturned() { return _exceptionReturned; }
  /**
    leaf method (with no return type) if an active exception 
    exists.
  */
  void finallyEnd(IN(REvalEnv) env);
  bool hasThrowCatch() { return _catchHandler.size() > 0; }

};



class ACDK_ACI_PUBLIC EvalEnv
: extends acdk::lang::Object
{
  typedef acdk::lang::sys::core_vector<ActivationFrame> AfStack;
  AfStack _afStack;

  typedef acdk::lang::sys::core_vector<acdk::lang::dmi::ScriptVar> VarStack;
  VarStack _stack;
  RCompiler _compiler;
  int _pc;
  foreign static acdk::lang::sys::core_vector<EvalEnv*> _envStack;
  acdk::cfgscript::RProps _globals;
public:
  
  EvalEnv(IN(RCompiler) compiler);
  ~EvalEnv();
  static REvalEnv getEvalEnv();
  
  IN(RCompiler) compiler() { return _compiler; }

  void push(const acdk::lang::dmi::ScriptVar& sv)
  {
    _stack.push_back(sv);
  }
  acdk::lang::dmi::ScriptVar pop()
  {
    if (_stack.size() == 0)
      THROW1(Exception, "EvalEnv::pop: Stack underflow");
    acdk::lang::dmi::ScriptVar ret = _stack.back();
    _stack.erase(_stack.end() - 1, _stack.end());
    return ret;
  }
  int stackTopIdx() { return _stack.size(); }
  acdk::lang::dmi::ScriptVar& get(int idx) { return _stack[idx]; }
  acdk::lang::dmi::ScriptVar& top() { return _stack.back(); }
  
  ActivationFrame& af() 
  { 
    if (_afStack.size() == 0)
      THROW1(Exception, "No ActivationFrame available");
    return _afStack.back(); 
  }
  int activationFrameCount() { return _afStack.size(); }
  /**
    idx = 0 current af
    idx = -1 caller
  */
  ActivationFrame& af(int idx) 
  { 
    if (_afStack.size() < -idx + 1)
      THROW1(Exception, "No ActivationFrame available");
    return *(_afStack.end() - (-idx + 1)); 
  }
  ActivationFrame& pushaf() 
  { 
    _afStack.push_back(ActivationFrame(this));
    return _afStack.back(); 
  }
  void popaf() { _afStack.erase(_afStack.end() - 1, _afStack.end()); }
  void load(int index) 
  { 
    push(af().load(index)); 
  }
  void loadRef(int index) 
  { 
    push(af().loadRef(index)); 
  }
  void store(int index)
  {
    af().store(index, pop()); 
  }
  int pc() { return _pc; }
  void pc(int npc) { _pc = npc; }
  int incpc() { return ++_pc; }
  void execute(IN(RExecutableArray) oca);
  void dumpStack(IN(acdk::io::RPrintWriter) out);
  void loadGlob(IN(RString) s) 
  {
    acdk::lang::dmi::RDmiObject obj = _globals->get(s);
    if (obj == Nil)
      THROW1(Exception, "No global object with name '" + s + "' found");
    push(*obj);
  }
  void loadRefGlob(IN(RString) s)
  {
    acdk::lang::dmi::RDmiObject obj = _globals->get(s);
    if (obj == Nil)
      THROW1(Exception, "No global object with name '" + s + "' found");
    push(obj->outOf());
  }
  void storeGlob(IN(RString) s)
  {
    _globals->assign(s, new acdk::lang::dmi::DmiObject(pop()));
  }
  void createGlob(IN(RString) s, const acdk::lang::dmi::ClazzInfo* ci)
  {
    _globals->create(s, ci);
  }
  static inline int getDmiFlags(jlong fl)
  {
    return int(fl);
  }
  static inline byte getArgCount(jlong fl)
  {
    return byte(fl >> 32);
  }
  static inline byte getNamedArgCount(jlong jl)
  {
    return byte(jl >> 40);
  }
};


/**
  DmiClient, which holds current EvalEnv
*/
class ACDK_ACI_PUBLIC AciDmiClient
: public acdk::lang::dmi::AcdkDmiClient
{
public:
  REvalEnv _env;
  AciDmiClient(IN(REvalEnv) env)
    : _env(env)
  {}
  virtual ~AciDmiClient() {}
};


} // vm
} // aci
} // acdk

#endif //acdk_aci_EvalEnv_h

