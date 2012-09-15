// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "AalObject.h"

namespace acdk {
namespace aal {

using namespace acdk::lang::dmi;
using namespace acdk::aci;






AalObject::AalObject(IN(RDClazzInfo) dclazzInfo)
: _dclazzInfo(dclazzInfo)
{
  _initFields();
}

void 
AalObject::_initFields(const ClazzInfo* ci)
{
  if (ci->interfaces != 0)
  {
    for (int i = 0; ci->interfaces[i] != 0; ++i)
      _initFields(ci->interfaces[i]->type);
  }
  if (ci->fields == 0)
    return;
  for (int i = 0; ci->fields[i] != 0; ++i)
  {
    _fields.push_back(AalObjectField(ci->fields[i], ScriptVar::getInitialized(ci->fields[i]->type)));
  }
}

void 
AalObject::_initFields()
{
  if (_dclazzInfo == Nil)
    return;
  _initFields(_dclazzInfo->getImplClazzInfo());
}

::acdk::lang::dmi::SysFields 
AalObject::getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz)
{
  ::acdk::lang::dmi::SysFields _sys_fields;
  
  
  for (FieldsMap::iterator it = _fields.begin(); it < _fields.end(); ++it)
  {
    ScriptVar sv = it->_val.inoutOf();
    _sys_fields.push_back(SysField::getField(it->_fi, sv));
  }    
  return _sys_fields;
}

const ClazzMethodInfo* 
AalObject::StandardDispatch(IN(RString) fname, ScriptVar& ret, ScriptVarArray& args, 
                            DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ClazzInfo* clazzinfo,
                                                         const ClazzMethodInfo* methinf)
{
  if (methinf == 0 || flags & MiIvViaHash)
  {
    /*
    if (flags & MiMiConstructor)
    {
      RString initf = "__init_"; initf = initf + fname;
      methinf = ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, initf->c_str(), args, namedArgs, dc, flags);
    }
    else*/
      methinf = ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, ACDK_STACK_STR(fname), args, namedArgs, dc, flags, methinf);
      flags &= ~MiIvViaHash;
  }
  acdk::lang::dmi::MetaObjectImpl mo((acdk::lang::dmi::MetaInfo*)methinf);
  acdk::lang::dmi::RMetaAttribute ma = mo.getMetaAttribute("__aci_code");
  if (ma == Nil)
  {
    if (clazzinfo->static_dispatch == AalObject::StandardDispatch)
      THROW1(Exception, "cannot find static method handler in AalObject::StandardDispatch");
    return clazzinfo->static_dispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }

  acdk::aci::RAciCodeAttributeData cad = (acdk::aci::RAciCodeAttributeData)ma->value;
  acdk::aci::AciDmiClient* adc = dynamic_cast<acdk::aci::AciDmiClient*>(&dc);
  REvalEnv env;
  if (adc != 0)
    env = adc->_env;
  else
    env = EvalEnv::getEvalEnv();
  for (int i = args.size() - 1; i >= 0; --i)
    env->push(args[i]);
  
  RAalObject newobj;
  if (flags & MiMiConstructor)
  {
    newobj = new AalObject(DClazzInfo::getInstance(clazzinfo));
    //af.push(&newobj);
    env->push(inOf(newobj));
    env->push(inOf(RString(fname)));
    env->push(createInvokeFlags(flags, args.size() + 1));
  }
  else
  {
    int thisargcount = 1;
    if (methinf->flags & MiStatic)
      thisargcount = 0;
    env->push(inOf(RString(fname)));
    env->push(createInvokeFlags(flags, args.size() + thisargcount));
  }
  if (cad->_code == Nil)
    return methinf;

  executeFunctionCall(env, cad->_code, adc == 0);
  if (env->af().exceptionReturned() == false)
  {
    if (newobj != Nil)
      ret = &newobj;
    else
      ret = env->pop();
    if (flags & MiMiConstructor)
      env->pop();
  }
  return methinf;
}

//virtual  
const ClazzMethodInfo* 
AalObject::standardDispatch(IN(RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  if (clazzinfo == 0)
    clazzinfo = _dclazzInfo->getImplClazzInfo();
  const ClazzInfo* orgclazzinfo = clazzinfo;
  if (methinf == 0 || flags & MiIvViaHash)
  {
    methinf = ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags, methinf);
    flags &= ~MiIvViaHash;
  }
  
  acdk::lang::dmi::MetaObjectImpl mo((acdk::lang::dmi::MetaInfo*)methinf);
  acdk::lang::dmi::RMetaAttribute ma = mo.getMetaAttribute("__aci_code");
  if (ma == Nil)  // call native method
  {
    if (methinf->flags & MiMiConstructor) // call derived AcdkClass
    {
      clazzinfo->static_dispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
      _baseObject = ret.getObjectVar();
      return methinf;
    }
    else
    {
      if (_baseObject == Nil)
        THROW1(Exception, "AalObject: Unitialized base class object.");

      return _baseObject->standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
    }
  }
  acdk::aci::RAciCodeAttributeData cad = (acdk::aci::RAciCodeAttributeData)ma->value;

  acdk::aci::AciDmiClient* adc = dynamic_cast<acdk::aci::AciDmiClient*>(&dc);
  REvalEnv env;
  if (adc != 0)
    env = adc->_env;
  else
    env = EvalEnv::getEvalEnv();

  for (int i = args.size() - 1; i >= 0; --i)
    env->push(args[i]);

  env->push(inOf(RObject(this)));
  if (flags & MiIvViaHash)
    env->push(inOf((int)methinf));
  else
    env->push(inOf(RString(fname)));
  env->push(createInvokeFlags(flags, args.size() + 1));
  executeFunctionCall(env, cad->_code, adc == 0);
  if (env->af().checkReturnedException(env) == false)
	ret = env->pop();
  
  return methinf;
}


void 
AalObject::setGetField(IN(acdk::lang::RString) fname, 
                                      ::acdk::lang::dmi::ScriptVar& var, 
                                      ::acdk::lang::dmi::DmiClient& dc,
                                      int flags,
                                      const ::acdk::lang::dmi::ClazzInfo* ci,
                                      const ::acdk::lang::dmi::ClazzFieldInfo* fi)
{
  for (int i = 0; i < _fields.size(); ++i)
  {
    if (_fields[i]._fi == fi)
    {
      // ## check assignable
      if (flags & MiReadOnly)
        var = _fields[i]._val;
      else
        _fields[i]._val = var;
      return;
    }
  }
}

//static 
const ::acdk::lang::dmi::ClazzFieldInfo* 
AalObject::FieldAccessor(
                                    ::acdk::lang::Object* This, 
                                      IN(acdk::lang::RString) fname, 
                                      ::acdk::lang::dmi::ScriptVar& var, 
                                      ::acdk::lang::dmi::DmiClient& dc,
                                      int flags,
                                      const ::acdk::lang::dmi::ClazzInfo* ci,
                                      const ::acdk::lang::dmi::ClazzFieldInfo* fi)
{
  if (fi->flags & MiStatic)
  {
    acdk::lang::dmi::MetaObjectImpl moi((acdk::lang::dmi::MetaInfo*)fi);
    acdk::lang::dmi::RMetaAttribute ma = (acdk::lang::dmi::RMetaAttribute)moi.getMetaAttribute("__aal_field");
    acdk::lang::dmi::RDmiObject obj = (acdk::lang::dmi::RDmiObject)ma->value;
    // ??? check assignable
    if (flags & MiReadOnly)
      var = *obj;
    else
      *obj = var;
    return fi;
  }
  RAalObject aobj = (RAalObject)This;
  aobj->setGetField(fname, var, dc, flags, ci, fi);
  return fi;
}

void 
AalObject::executeFunctionCall(IN(REvalEnv) env, IN(RExecutableArray) oca, bool throwExceptions)
{
  acdk::aci::ActivationFrame& af = env->pushaf();
  af.prepareCallee(env, oca);
  env->execute(oca);
  af.afterCallee(env);
  if (throwExceptions == true && af.activeException() != Nil)
  {
    RObject ex = af.activeException();
    ex->invoke("throwException");
  }
  env->popaf();
}

//static 
bool 
AalObject::isAalClazz(const ClazzInfo* ci)
{
  if (ci->static_dispatch == AalObject::StandardDispatch)
    return true;
  return false;
}




} // aal
} // acdk

