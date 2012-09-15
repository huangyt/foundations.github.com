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



#include "ScriptObject.h"
#include "Script.h"
#include "ScriptSource.h"
#include "ScriptException.h"
#include "ScriptEval.h"
#include "ScriptExpr.h"

#include <acdk/lang/dmi/ClazzAttributesRes.h>
#include <acdk/lang/dmi/DmiNamedArg.h>
#include <acdk/lang/dmi/DmiDelegate.h>
#include <acdk/lang/sys/LocalGcHeap.h>

#include <acdk/lang/sys/core_value_scope.h>
#include <acdk/lang/sys/core_system.h>

#undef DOUT
//#define LOCAL_DEBUG
#if defined(LOCAL_DEBUG)

RString spaces(int count);
#define DOUT(exp) acdk::lang::sys::coreout << exp << acdk::lang::sys::eofl
#else
#define DOUT(strexpr) do { } while(false)
#endif


namespace acdk {
namespace cfgscript {


using namespace acdk::io;
using namespace acdk::lang::reflect;
using namespace acdk::lang::dmi;


typedef acdk::lang::sys::core_value_scope<const acdk::lang::dmi::ClazzInfo*> StackedClazzInfo;
typedef acdk::lang::sys::core_value_scope<const acdk::lang::dmi::ClazzMethodInfo*> StackedClazzMethodInfo;

void executeFieldInitializer(const acdk::lang::dmi::ClazzInfo* ci, const acdk::lang::dmi::ClazzFieldInfo* fi, INOUTP(RProps) target, PEStack& stack)
{
  target->set(fi->name, new DmiObject(fi->type), PropsNoParentWrite);
  Field field(ci, fi);
  if (field.hasMetaAttribute("_cfgscript_source") == false)
    return;

  RScriptMethodInfo methodsource =  (RScriptMethodInfo)field.getMetaAttribute("_cfgscript_source")->value;
  //RScript script = Script::getCurrentScript();
  RScript script = (RScript)Class::getSingeltonClass(ci)->getMetaAttribute("_cfgscript_script")->value;
  
  int execIdx = ExecutionStack::get()->push(new ExecutionStackFrame(script, methodsource->begintokenIndex, ci, 0));

  //StackedClazzInfo _savedci(script->_currentClazz, ci);
  //stack.props->dump();
  //System::out->println("====");
  target->addParentProps(stack.props);
  PropsScope propsscope(stack.props, target, PropsNoParentWrite | PropsParentRead);
  
  acdk::lang::sys::core_value_scope<int> tokenIdxscope(stack.tokenizer._tokenIdx, methodsource->begintokenIndex);
  acdk::lang::sys::core_value_scope<int> tokenEndIdscope(stack.tokenizer._endTokenIdx, methodsource->endtokenIndex);

  
  //stack.props->dump();
  bool erg = ExpressionParseNode().parseExecute(stack);
  if (erg == false)
    return;
  target->assign(fi->name, new DmiObject(stack.pop().val));
  ExecutionStack::get()->pop(execIdx);
}

void executeFieldInitializer(const acdk::lang::dmi::ClazzInfo* ci, const acdk::lang::dmi::ClazzFieldInfo* fi, INOUTP(RProps) target)
{
  target->set(fi->name, new DmiObject(fi->type), PropsNoParentWrite);
  Field field(ci, fi);
  if (field.hasMetaAttribute("_cfgscript_source") == false)
    return;

  RScriptMethodInfo methodsource =  (RScriptMethodInfo)field.getMetaAttribute("_cfgscript_source")->value;
  //RScript script = Script::getCurrentScript();
  RScript script = (RScript)Class::getSingeltonClass(ci)->getMetaAttribute("_cfgscript_script")->value;
  //StackedClazzInfo _savedci(script->_currentClazz, ci);

  PropsScope propsscope(script->currentProps, target, PropsNoParentWrite | PropsParentRead);
  
  PEStack stack(script, target, methodsource->begintokenIndex, methodsource->endtokenIndex);
  
  //StackedClazzMethodInfo _savemi(script->_currentClazzMethod, methinf);
  
  int execIdx = ExecutionStack::get()->push(new ExecutionStackFrame(script, methodsource->begintokenIndex, ci, 0));
  bool erg = ExpressionParseNode().parseExecute(stack);
  if (erg == false)
    return;
  target->assign(fi->name, new DmiObject(stack.pop().val));
  ExecutionStack::get()->pop(execIdx);
  
}

using namespace acdk::lang::dmi;
void
createStaticMember(const ClazzInfo* ci, ClazzFieldInfo* fi, PEStack& stack)
{
  RClass cls = Class::getSingeltonClass(ci);
  if (cls->hasMetaAttribute("_cfgscript_static_members") == false)
  {
    cls->setMetaAttribute("_cfgscript_static_members", (RObject)new Props("static_members"));
  }
  
  RProps statics =  (RProps)cls->getMetaAttribute("_cfgscript_static_members")->value;
  executeFieldInitializer(ci, fi, statics, stack);

}


ScriptObject::ScriptObject(const acdk::lang::dmi::ClazzInfo* ci)
: _castRecGuard(false)
, _locals(new Props("dynamic_members"))
, _dmiProxies(new ObjectArray(0))
, _superObject(Nil)
, _finalizeCalled(false)
, _scriptClazzInfo(ci)
, _derivedObject(Nil)
//, _proxyCount(0)
{
  ACDK_SAFE_CONSTRUCTOR();
  //setLocalGc(true);
  //RScript script = (RScript)Class::getSingeltonClass(ci)->getMetaAttribute("_cfgscript_script")->value;
  DOUT((void*)this << "; ScriptObject::ScriptObject: " << ci->name);
  /*
  RScript script = ExecutionStack::getTop()->_script;

  if (script != Nil && script->currentProps != Nil)
  {
    _locals->addParentProps(script->currentProps);
  }*/

  RClass cls = Class::getSingeltonClass(ci);
  if (cls->hasMetaAttribute("_cfgscript_static_members") == true)
  {
    RProps statics =  (RProps)cls->getMetaAttribute("_cfgscript_static_members")->value;
    _locals->addParentProps(statics);
  }
  if (cls->hasMetaAttribute("_cfgscript_module_props") == true)
  {
    RProps module =  (RProps)cls->getMetaAttribute("_cfgscript_module_props")->value;
    _locals->addParentProps(module);
  }
  int i;
  //PEStack stack(script, props, methodsource->begintokenIndex, methodsource->endtokenIndex);
  for (i = 0; i < ci->getFieldsCount(); ++i)
  {
    executeFieldInitializer(ci, ci->fields[i], _locals);
  }
  
  for (i = 0; i < ci->getInterfacesCount(); ++i)
  {
    if (_isDmiProxyInterface(ci->interfaces[i]->type) == true)
    {
      RClass cls = Class::getSingeltonClass(ci->interfaces[i]->type);
      cls->getDmiProxies(_dmiProxies, this, 0);
    }
  }
  for (i = 0; i < _dmiProxies.length(); ++i)
  {
    DmiProxyBase* pb;
    if ((pb = dynamic_cast<DmiProxyBase*>(_dmiProxies[i].impl())) != 0)
    {
      RObject* ro = (RObject*)_dmiProxies[i]._ref_this();
      //_sharedRefs.registerSharedObjectRefs(((DmiProxyBase*)_dmiProxies[i].impl())->_dmiTarget._ref_this(), (RObject*)_dmiProxies[i]._ref_this());
    }
  } 
  _setObjectRefFlag(true, ObjectBase::ObjectHasLocalGc);
}

void 
ScriptObject::setImplementation(IN(RScriptObject) impl)
{
  _derivedObject = impl;
}

void 
ScriptObject::setSuperObject(IN(RObject) obj)
{
  _superObject = obj;

  DOUT((void*)this << ";setSuperObect(" <<  (void*)_superObject.impl() << ")");
  DmiProxyBase* dmiproxy;
  if (_superObject != Nil && (dmiproxy = dynamic_cast<DmiProxyBase*>(_superObject.impl())) != 0)
  {
    dmiproxy->setDmiTarget(this);
    //_sharedRefs.registerSharedObjectRefs(dmiproxy->_dmiTarget._ref_this(), _superObject._ref_this());
  }
  else if (instanceof(_superObject, ScriptObject) == true)
  {
    RScriptObject superSo(_superObject);
    superSo->setImplementation(this);
    //_sharedRefs.registerSharedObjectRefs((RObject*)superSo->_derivedObject._ref_this(), _superObject._ref_this());
  }
}

ScriptObject::~ScriptObject()
{
  DOUT((void*)this << " ScriptObject::~ScriptObject(): " << _scriptClazzInfo->name);
  //_sharedRefs.unregisterAll();
  if (instanceof(_superObject, ScriptObject) == true)
    RScriptObject(_superObject)->_derivedObject = Nil;
  _superObject = Nil;
}


void 
ScriptObject::_setFinalized()
{
  _finalizeCalled = true;
  if (instanceof(_superObject, ScriptObject) == true)
    RScriptObject(_superObject)->_setFinalized();
  for (int i = 0; i < _dmiProxies.length(); ++i)
  {
    if (instanceof(_dmiProxies[i], ScriptObject) == true)
    {
      RScriptObject so(_dmiProxies[i]);
      so->_setFinalized();
    }
  }
}

void 
ScriptObject::finalize()
{
  getMostDerived()->_invokeFinalize();
}

void ScriptObject::_invokeFinalize()
{
  if (_finalizeCalled == true)
    return;
  _setFinalized();;
  {
  acdk::lang::dmi::ScriptVar ret;
  acdk::lang::dmi::ScriptVarArray args;
  ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient(_locals->getCastFlags());

  const acdk::lang::dmi::ClazzMethodInfo* methinf = lookupMethod(_scriptClazzInfo, "finalize", args, Nil, dmiclient, MiIvDeclared | MiIvNoThrowIfNotFound);
  if (methinf == 0)
    return;
  //_setObjectRefFlag(true, NoRefCounting);
  lockMem(true);
  standardDispatchFromTopMost("finalize", ret, args, dmiclient, Nil,
                                MiIvDeclared,
                                _scriptClazzInfo,
                                methinf);
  }
}


void releaseDmiProxy(INOUT(RObject) obj)
{
  if (obj == Nil)
    return;
  DmiProxyBase* proxy = dynamic_cast<DmiProxyBase*>(obj.impl()) ;
  if (proxy != 0)
    proxy->setDmiTarget(Nil);
  obj = Nil;
}

bool canReleased(IN(RObject) obj)
{
  if (obj == Nil)
    return true;
  DmiProxyBase* proxy = dynamic_cast<DmiProxyBase*>(obj.impl()) ;
  if (proxy == 0)
    return true;
  if (proxy->_dmiProxyGetTarget()->refCount() <= 2)
    return true;
  return false;
}

void 
ScriptObject::getCollectableFields(FieldReferences& fields)
{
  fields.push_back((RObject*)_derivedObject._ref_this());
  fields.push_back((RObject*)_superObject._ref_this());
  fields.push_back((RObject*)_dmiProxies._ref_this());
}

bool 
ScriptObject::_gc_releaseRef(bool force) const
{
  if (_releaseRefCount() == true) 
    return true;
  if (isWeakRef() == true  ||
      isMemLocked() == true  ||
      isStackRef() == true ||
      (const_cast<Object*>(static_cast<const Object*>(this))->allocator()->flags & acdk::lang::sys::NoRefCountAllocatorType) != 0) 
    return false;

  int refC = refCount();
  int potRefC = 0;
  if (_derivedObject != Nil)
    ++potRefC;
  if (_superObject != Nil)
    ++potRefC;
  potRefC += _dmiProxies->length();
  if (refC <= potRefC)
    return sys::LocalGcHeap::gcObject((Object*)this);
  return false;
}

//virtual 
::acdk::lang::dmi::ClazzInfo* 
ScriptObject::getClazzInfo()  
{ 
  if (_derivedObject != Nil)
    return _derivedObject->getClazzInfo();
  return const_cast<acdk::lang::dmi::ClazzInfo*>(_scriptClazzInfo); 
}
 
bool 
ScriptObject::isDirectSuper(const ::acdk::lang::dmi::ClazzInfo* ci)
{
  if (_scriptClazzInfo == ci)
    return true;
  if (_superObject != Nil && instanceof(_superObject, ScriptObject) == true)
  {
    RScriptObject sobj(_superObject);
    return sobj->isDirectSuper(ci);
  }
  return false;
}

RScriptObject 
ScriptObject::getMostDerived()
{
  RScriptObject o = this;
  while (o->_derivedObject != Nil)
    o = o->_derivedObject;
  return o;
}

//static 
bool 
ScriptObject::_isScriptInterface(const acdk::lang::dmi::ClazzInfo* ci)
{
  RMetaAttribute ma = Class::getSingeltonClass(ci)->getMetaAttribute("_cfgscript_script");
  if (ma != Nil)
    return true;
  return false;
}

Object* 
ScriptObject::_cast(const ::acdk::lang::dmi::ClazzInfo* ci)
{
  DOUT((void*)this << "; ScriptObject::_cast from " << _scriptClazzInfo->name << " to " << ci->name);
  if (ci == _scriptClazzInfo)
    return this;
  return getMostDerived()->_castFromTopMost(ci);
}

Object* 
ScriptObject::_castFromObject(IN(RObject) o, const ::acdk::lang::dmi::ClazzInfo* ci)
{
  if (o == Nil)
    return 0;
  if (dynamic_cast<DmiProxyBase*>(o.impl()) != 0)
  {
    if (ci->assignableFrom(o->getClazzInfo()) == true)
      return o;
  }
  else if (instanceof(o, ScriptObject) == true)
  {
    RScriptObject so(o);
    Object* o = so->_castFromTopMost(ci);
    if (o != 0)
      return o;
  }
  else if (ci->assignableFrom(_scriptClazzInfo) == true && _isScriptInterface(ci) == false)
  {
    // no dmi object available
    return o;
  }
  if (ci->assignableFrom(_scriptClazzInfo) == true)
  {
    if (_isScriptInterface(ci) == true)
      return this;
  }
  return 0;
}

Object* 
ScriptObject::_castFromTopMost(const ::acdk::lang::dmi::ClazzInfo* ci)
{
  if (ci == _scriptClazzInfo)
    return this;
  if (ci == Object::clazzInfo())
    return this;
  Object* ret = _castFromObject(_superObject, ci);
  if (ret != 0)
    return ret;

  for (int i = 0; i < _dmiProxies->length(); ++i)
  {
    ret = _castFromObject(_dmiProxies[i], ci);
    if (ret != 0)
      return ret;
  }
  return 0;
}



using namespace acdk::lang::dmi;


void ajustFlags(IN(RExecutionStackFrame) esf, const ClazzInfo* ci, int& flags)
{
  if (esf == Nil)
    return;
  if (esf->_currentClazzInfo != 0)
  {
    if (esf->_currentClazzInfo == ci)
    {
      flags &= ~MiPublic & ~MiProtected; flags |= MiPrivate;
    }
    else if (esf->_currentClazzInfo->assignableFrom(ci) == true)
    {
      flags &= ~MiPublic; flags |= MiProtected;
    }
  }
}

const acdk::lang::dmi::ClazzMethodInfo* 
ScriptObject::standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/)
{
  
  //static bool recursionsGuard = false;
  DOUT((void*)this << "; ScriptObject::standardDispatch: " << _scriptClazzInfo->name);
  if (flags & MiIvNoWeakBind) // called via super.call()
    return standardDispatchFromTopMost(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  return getMostDerived()->standardDispatchFromTopMost(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}
  
RObject 
ScriptObject::_findSuperByClazz(const acdk::lang::dmi::ClazzInfo* clazzinfo)
{
  if (clazzinfo == _scriptClazzInfo)
    return this;
  if (_superObject == Nil)
    ; // ooop;
  if (instanceof(_superObject, ScriptObject) == true)
    return RScriptObject(_superObject)->_findSuperByClazz(clazzinfo);
  return _superObject;
}

const acdk::lang::dmi::ClazzMethodInfo* 
ScriptObject::standardDispatchFromTopMost(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/)
{

  const ClazzInfo* ci = clazzinfo;

  if (methinf == 0 || methinf->flags & MiMiVirtual) // virtual methods may be overwriten
  {
    //RScript script = Script::getCurrentScript();
    RMetaAttribute ma = Class::getSingeltonClass(_scriptClazzInfo)->getMetaAttribute("_cfgscript_script");
    RScript script;
    if (ma != Nil)
      script = (RScript)ma->value;
    RExecutionStackFrame esf = ExecutionStack::getTop();

    ci = _scriptClazzInfo;

    ajustFlags(esf, ci, flags);
    ASCLITERAL(poke);
    ASCLITERAL(peek);

    if (fname->equals(lit_poke) == true || fname->equals(lit_peek) == true)
    {
      RString name = args[0].getStringVar();
      const ClazzInfo* sicci = ci;
      const ClazzFieldInfo* fi = ci->findField(sicci, name, (flags | MiPrivate) & ~MiPublic & ~MiProtected);
      
      if ((void*)fi->_scopeParent != (void*)ci && _superObject != Nil)
      {
        RObject superObject = _findSuperByClazz((const acdk::lang::dmi::ClazzInfo*)fi->_scopeParent);

        ajustFlags(esf, (const ClazzInfo*)fi->_scopeParent, flags); // ajust flags with new ci
        fi = ci->findField(sicci, name, flags); // find again
        int memberAccessFlag = 0;
        if (instanceof(_superObject, ScriptObject) == true)
        {
          RScriptObject(superObject)->standardDispatchFromTopMost(fname, ret, args, dc, namedArgs, memberAccessFlag, (const ClazzInfo*)fi->_scopeParent, 0);
        } 
        else 
        {
          memberAccessFlag |= MiIvNoWeakBind;
          if (fname->equals(lit_poke) == true)
          {
            superObject->setMember(name, args[1], dc, flags  | memberAccessFlag);
            ret = args[1];
          }
          else
          {
            ret = superObject->getMember(name, dc, flags | memberAccessFlag);
          }
        }
        return (methinf == 0 ? (const ClazzMethodInfo*)1 : methinf);
      }
      if (_locals->hasValue(name) == false)
        THROW1(DmiException, RString("Class ") + ci->name + " has no field named " + name);
      if (fname->equals(lit_poke) == true)
      {
        _locals->assign(name, new DmiObject(args[1]));
        ret = args[1];
      } 
      else // peek
      {
        ret= *_locals->get(name);
      }
      return methinf;
    }
    
    methinf = lookupMethod(ci, fname, args, namedArgs, dc, flags);
    //methinf = lookupMethodNoPolymorph(ci, fname, args, namedArgs, dc, flags);
    if (methinf == 0)
      ClazzMethodInfo::throwMethodNotFound(clazzinfo, fname, flags, dc._formatFlags);
  }
  if (methinf->dispatch != 0)
  {
    if (ScriptObject::dispatch != methinf->dispatch)
    {
      // call native implemented, but with dmiproxy if virtual
      
      return methinf->dispatch(this, fname, ret, args, dc, namedArgs, flags | ::acdk::lang::dmi::MiIvNoWeakBind, ci, methinf);
      

      
    }
    else
      return methinf->dispatch(this, fname, ret, args, dc, namedArgs, flags, ci, methinf);
  }
  return 0;
}


//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
ScriptObject::static_dispatch(IN(acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, 
                                               ::acdk::lang::dmi::ScriptVarArray& args, 
                                               ::acdk::lang::dmi::DmiClient& dc,
                                               IN(::acdk::lang::RStringArray) namedArgs,
                                                int flags,
                                                const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  const ClazzInfo* ci = clazzinfo;
  RClass cls = Class::getSingeltonClass(ci);
  
  if (methinf == 0)
  {
    RExecutionStackFrame esf = ExecutionStack::getTop();
    RScript script = (RScript)cls->getMetaAttribute("_cfgscript_script")->value;
    ajustFlags(esf, ci, flags);
    ASCLITERAL(peek_static);
    ASCLITERAL(poke_static);
    ASCLITERAL(GetClass);
    if (fname->equals(lit_peek_static) == true || fname->equals(lit_poke_static) == true)
    {
      if (cls->hasMetaAttribute("_cfgscript_static_members") == false)
        THROW1(DmiException, "in peek_static or poke_static: Class has no static members: " + cls->getName());
      if (fname->equals(lit_peek_static) == true)
      {
        RString fieldName = args[0].getStringVar();
        RProps statics =  (RProps)cls->getMetaAttribute("_cfgscript_static_members")->value;
        ret = *statics->get(fieldName);
        return (::acdk::lang::dmi::ClazzMethodInfo* )1; 
      }
      else if (fname->equals(lit_poke_static) == true)
      {
        RString fieldName = args[0].getStringVar();
        RClass cls = Class::getSingeltonClass(ci);
        RProps statics =  (RProps)cls->getMetaAttribute("_cfgscript_static_members")->value;
        statics->set(fieldName, new DmiObject(args[1]));
        ret = args[1];
        return (::acdk::lang::dmi::ClazzMethodInfo* )1; 
      }
    }
    else if (fname->equals(lit_GetClass) == true && args.size() == 0)
    {
      ret = &Class::getSingeltonClass(clazzinfo);
      return (::acdk::lang::dmi::ClazzMethodInfo* )1;
    }
    methinf = lookupMethod(ci, fname, args, namedArgs, dc, flags);
    
    if (methinf == 0)
      ClazzMethodInfo::throwMethodNotFound(clazzinfo, fname, flags, dc._formatFlags);
  }
  if (methinf->dispatch != 0)
    return methinf->dispatch(0, fname, ret, args, dc, namedArgs, flags, ci, methinf);
  return (::acdk::lang::dmi::ClazzMethodInfo* )0;
}




//virtual 
bool 
ScriptObject::isDmiOverLoaded(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) funcname, const acdk::lang::dmi::ClazzMethodInfo* mi, acdk::lang::dmi::ClazzMethodArgInfo** const args)
{
  if (mi != 0)
  {
    if (reinterpret_cast<const acdk::lang::dmi::ClazzInfo*>(mi->_scopeParent) == getClazzInfo())
      return true;
  }
  const ClazzInfo* nci = _scriptClazzInfo;
  ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
  const ClazzMethodInfo* nmi = StdDispatch::lookupMethod(nci, funcname, args, dmiclient, 0);
  if (nmi->flags & MiMiDmiImpl)
    return true;
  //const acdk::lang::dmi::ClazzMethodInfo* methinf = lookupMethodNoPolymorph(ci, funcname, flags);
  return false;
}



//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
ScriptObject::dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  RScriptObject thisobj;
  const ClazzInfo* ci = clazzinfo;
  if (flags & MiIvConstructor)
  {
    RScriptObject s = new ScriptObject(clazzinfo);
    ret = inOf(s);
    thisobj = s;
    This = &thisobj;
  }
  else if (This != 0)
  {
    thisobj = RScriptObject(This);
  }
  

  //RScript script = Script::getCurrentScript();
  RClass cls = Class::getSingeltonClass(ci);
  RScript script = (RScript)cls->getMetaAttribute("_cfgscript_script")->value;
  RProps moduleProps = (RProps)cls->getMetaAttribute("_cfgscript_module_props")->value;
  
  
  Method met(ci, methinf);
  RScriptMethodInfo methodsource =  (RScriptMethodInfo)met.getObjectMetaAttribute("_cfgscript_source");
  
  int esfIdx = ExecutionStack::get()->push(new ExecutionStackFrame(script, methodsource->begintokenIndex, methinf));
  
  RExecutionStackFrame esf = ExecutionStack::getTop();
  
  RProps locals;
  if (thisobj != Nil)
    locals = thisobj->_locals;
  else
    locals = script->currentProps;

  
  RProps props = new Props("method_locals", PropsParentWrite | PropsParentRead, locals);
  //props->addParentProps(moduleProps);
  ScopedCfgVar _localProps(props, "__props", new DmiObject(inOf(props)), PropsNoParentWrite);
  
  bool hasScriptVar = props->hasValue("__script");
  ScopedCfgVar _localScript(props, "__script");
  if (hasScriptVar == false)
    _localScript.set(new DmiObject(inOf(script)), PropsNoParentWrite);

  esf->_executionFlags |= ESFFirstStatement;
  //if (ci->getInterfacesCount() > 0 && (ci->interfaces[0]->type->flags & MiCiInterface) == 0)

  int pcount = methinf->getArgumentCount();
  int argssize = args.size();
  if (pcount != argssize)
    THROW1(DmiException, "method called with not matching parameter count");
  int i;

  for (i = 0; i < argssize; ++i)
  {
    int aflags = methinf->methodArgs[i]->flags;
    if (aflags & MiAiOut)
      props->set(methinf->methodArgs[i]->name, new DmiObject(args[i].outOf()), PropsNoParentWrite);
    else
      props->set(methinf->methodArgs[i]->name, new DmiObject(args[i].inOf()), PropsNoParentWrite);
  }
  
  if (Method(ci, methinf).hasMetaAttribute("_cfgscript_defaultArgs") == true)
  {
    RDmiNamedArgArray defaultArgs = (RDmiNamedArgArray)Method(ci, methinf).getMetaAttribute("_cfgscript_defaultArgs")->value;
    for (i = 0; defaultArgs != Nil && i < defaultArgs->length(); ++i)
    {
      props->set(defaultArgs[i]->name, defaultArgs[i]->value, PropsNoParentWrite);
    }
  }
  ASCLITERAL(rest);
  static const ClazzInfo* dminamedClazzInfo = Class::forName("[acdk/lang/dmi/DmiNamedArg")->objectClazzInfo();
  if (argssize > 0 && methinf->methodArgs[argssize - 1]->equalsName(lit_rest) && methinf->methodArgs[argssize - 1]->type == dminamedClazzInfo)
  {
    RDmiNamedArgArray namedRestArgs =  (RDmiNamedArgArray)args[argssize - 1].getObjectVar();
    if (namedRestArgs != Nil)
    {
      for (int i = 0; i < namedRestArgs->length(); ++i)
      {
        props->set(namedRestArgs[i]->name, namedRestArgs[i]->value);
      }
    }

  }
  PEStack stack(script, props, methodsource->begintokenIndex, methodsource->endtokenIndex);
  esf->setScopeProps(props);
  esf->setFrameProps(props);
  //StackedClazzMethodInfo _savemi(script->_currentClazzMethod, methinf);
  if (This != 0)
  {
    props->setObjectVal(lit_this, This, PropsNoParentWrite);
    stack.addUsingVar(lit_this, This);
    
    if (thisobj != Nil && thisobj->_superObject != Nil)
    {
      props->setObjectVal(lit_super, thisobj->_superObject, PropsNoParentWrite);
      stack.addUsingVar(lit_super, thisobj->_superObject);
    }
  }
  stack.addUsingType(clazzinfo); 

  //if (ExecutionStack::get()->calledByConstructor() == true)
  if (flags & MiIvConstructor)
  {
    bool erg = ConstructorMethodParseNode().parseExecute(stack);
    ret = inOf(thisobj);
    if (thisobj->_superObject == Nil && (clazzinfo->flags & MiCiInterface) == 0)
    {
      ScriptVarArray args;
      RStringArray namedargs;
      RString classname = "acdk/lang/Object";
      ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient(stack.props->getCastFlags());
      if (ClazzInfo::findClazzInfo(RString("acdk/lang/Object_DmiProxy")) != 0)
        classname = classname + "_DmiProxy";
      ScriptVar superobj = Object::New(classname, args, namedargs, dmiclient);
      thisobj->setSuperObject(superobj.getObjectVar());
    }
  } 
  else
  {
    //RScript sicScript = Script::getCurrentScript();
    //Script::setCurrentScript(script);
    bool erg = StatementOrPreProc().parseExecute(stack);
    stack.executionFlags &= ~(EFActiveReturn | EFActiveBreak | EFActiveContinue);
    if (stack.isStackEmtpy() == false)
      ret = stack.pop().val.inOf();
    //Script::setCurrentScript(sicScript);
  }
  ExecutionStack::get()->pop(esfIdx);
  return methinf;
}


//static 
bool 
ScriptObject::_isDmiProxyInterface(const ClazzInfo* ci)
{
  if (ci == 0)
    return false;
  if ((ci->flags & MiCiInterface) == MiCiInterface)
    return true;
  if (RString(ci->name)->endsWith("_DmiProxy") == false)
    return false;
  if (ci->getInterfacesCount() < 1)
    return false;
  if ((ci->interfaces[0]->type->flags & MiCiInterface) == MiCiInterface)
    return true;
  return false;
}

/* not working
const ::acdk::lang::dmi::ClazzMethodInfo* 
ScriptObject::delegate_dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  const ClazzInfo* ci = clazzinfo;
  Method met(ci, methinf);
  RDmiDelegate delegate =  (RDmiDelegate)met.getObjectMetaAttribute("_cfgscript_delegate");

  return 0;
}
*/

const ::acdk::lang::dmi::ClazzMethodInfo* 
ScriptObject::abstract_method_dispatch(
                                                        ::acdk::lang::Object* This, 
                                                         IN(acdk::lang::RString) fname, 
                                                         ::acdk::lang::dmi::ScriptVar& ret, 
                                                         ::acdk::lang::dmi::ScriptVarArray& args, 
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  RString className = "";
  if (clazzinfo != 0)
    className = clazzinfo->toTypeString();
  else if (This != 0)
    className = This->getClass()->toString();

  THROW1(NoSuchMethodException, "Call abstract method: " + className + "::" + fname);
  return 0;
}

} // namespace cfgscript
} // namespace acdk 
  

