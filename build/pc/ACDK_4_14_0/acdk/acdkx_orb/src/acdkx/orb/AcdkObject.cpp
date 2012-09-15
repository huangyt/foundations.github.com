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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AcdkObject.cpp,v 1.21 2005/04/21 08:28:51 kommer Exp $

#include "AcdkObject.h"
#include "CorObject.h"
#include <acdk/lang/DmiTypeConversionException.h>
#include <org/omg/CORBA/TypeCode.h>
#include <org/omg/CORBA/Any.h>
#include <acdk/lang/ParamsMismatchException.h>
#include <acdk/util/logging/Log.h>

#include "acdk2orb.h"

namespace acdkx {
namespace orb {

using namespace acdk::lang::dmi;

using ::org::omg::CORBA::TCKind;
using ::acdk::lang::dmi::ScriptVar;
using ::org::omg::CORBA::portable::CallerWrite;
using ::org::omg::CORBA::portable::CalleeRead;
using ::org::omg::CORBA::portable::CalleeWrite;
using ::org::omg::CORBA::portable::CallerRead;

USING_CLASS(::org::omg::CORBA::, Any);
USING_CLASS(::org::omg::CORBA::, TypeCode);



AcdkObject::AcdkObject(IN(::acdk::lang::RObject) localObject)
: ServerDelegate()
, _acdkObject(localObject)
, _factoryClazz(0)
{
  if (_acdkObject != Nil)
    _factoryClazz = _acdkObject->getClazzInfo();
  AORB::getAORB().impl_is_ready(this);
}

AcdkObject::AcdkObject(const ::acdk::lang::dmi::ClazzInfo* factoryClazz)
: ServerDelegate()
, _factoryClazz(factoryClazz)
{
  AORB::getAORB().impl_is_ready(this);
}

//static 
::acdk::lang::RObject 
AcdkObject::create_instance()
{
  return new AcdkObject(Nil); 
}


//virtual
::org::omg::CORBA::portable::ROutputStream
AcdkObject::_invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, 
                            ::org::omg::CORBA::portable::ResponseHandler& handler) 
                                                                    THROWS1(::org::omg::CORBA::RSystemException)
{
  return standard_invoke(method, input, handler);
}


::org::omg::CORBA::portable::ROutputStream
AcdkObject::dii_invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, 
                            ::org::omg::CORBA::portable::ResponseHandler& handler) 
{
  RObject This = _acdkObject;
  int flags = 0;
  if (This == Nil)
    flags |= MiStatic;
  if (method->equals("peek") == true || method->equals("poke") == true)
  {
    RString membername = input.read_string();
    const ClazzInfo* ci = _factoryClazz;
    const acdk::lang::dmi::ClazzFieldInfo* cfi = ClazzInfo::findField(ci, membername, 0);
    
    if (method->equals("peek") == true)
    {
      ScriptVar ret;
      if (flags & MiStatic)
        ret = getStaticMember(_factoryClazz, membername,  AcdkDmiClient::getDmiClient(), 0, 0);
      else
        ret = This->getMember(membername,  AcdkDmiClient::getDmiClient(), 0, 0);
      ::org::omg::CORBA::portable::ROutputStream out = handler.createReply();
      writeValueParam(*out, ret, cfi->type, 0, 0);
      return out; 
    }
    else
    {
      ScriptVar val;
      readValueParam(input, val, cfi->type, 0, 0);
      if (flags & MiStatic)
        setStaticMember(_factoryClazz, membername, val, AcdkDmiClient::getDmiClient(), 0);
      else
        This->setMember(membername, val, AcdkDmiClient::getDmiClient(), 0);
      return  handler.createReply();
    }
  }
  ScriptVarArray targs(0);
  RString methodname = method;
  if (methodname->startsWith("createCor") == true)
    methodname = methodname->substr(strlen("createCor"));
  const ClazzMethodInfo* cmi = lookupMethodNoPolymorph( _factoryClazz
                                                          , methodname
                                                          , targs
                                                          , Nil
                                                          , AcdkDmiClient::getDmiClient()
                                                          , 0
                                                          );
  if (cmi == 0)
      THROW1(InvalidName, "Method unknown: " + method);
  int argnum = cmi->getArgumentCount();
  ScriptVarArray args(argnum);
  int i;
  for (i = 0; cmi->methodArgs[i] != 0; ++i)
  {
    readValueParam(input, args[i], cmi->methodArgs[i]->type, cmi->methodArgs[i]->flags, MiAiIn);
  }
  ::org::omg::CORBA::portable::ROutputStream out = handler.createReply();
  
  ScriptVar retval;
  if (flags & MiStatic)
    _factoryClazz->static_dispatch(method, retval, args, AcdkDmiClient::getDmiClient(), Nil, flags, _factoryClazz, cmi);
  else
    This->standardDispatch( method, retval, args, AcdkDmiClient::getDmiClient(), Nil, 0, This->getClazzInfo(), cmi);

  writeValueReturn(*out, cmi, retval);
  for (i = 0; cmi->methodArgs[i] != 0; ++i)
  {
    writeValueParam(*out, args[i], cmi->methodArgs[i]->type, cmi->methodArgs[i]->flags, MiAiOut);
  }
  return out;
}


::org::omg::CORBA::portable::ROutputStream
AcdkObject::standard_invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, 
                            ::org::omg::CORBA::portable::ResponseHandler& handler) 
                                                                    THROWS1(::org::omg::CORBA::RSystemException)
{
  RObject This = _acdkObject; 
  try {
    if (method->equals("dyn_new") == true)
    {
    /*
    called from RObject CorObject::New(IN(RString) classname, IN(RString) constructor, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
      */
      RString classname = input.read_string();
      RString constructor = input.read_string();
      ScriptVarArray args;
      
      RClass cls = Class::forName(classname);
      const ClazzInfo* ci = cls->objectClazzInfo();
      const ClazzMethodInfo* cmi = lookupMethodNoPolymorph(ci, constructor, args, Nil, 
        AcdkDmiClient::getDmiClient(), MiMiConstructor);
      readAnyParams(input, cmi, args, MiAiIn);
      ScriptVar ret;
      cls->objectClazzInfo()->static_dispatch(constructor, ret, args, AcdkDmiClient::getDmiClient(), Nil, MiMiConstructor, cls->objectClazzInfo(), cmi);
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      out->write_fq_object(ret.getObjectVar(), DmiGiopIsReference);
      writeAnyParams(*out, cmi, args, MiAiOut);
      return out;
    } 
    else if (method->equals("dyn_invoke") == true)
    {
      RString methodname = input.read_string();
      ScriptVarArray args;
      const ClazzInfo* ci = This->getClazzInfo();
      const ClazzMethodInfo* cmi = lookupMethodNoPolymorph(ci, methodname, args, Nil, 
        AcdkDmiClient::getDmiClient(), 0);

      readAnyParams(input, cmi, args, MiAiIn);
      ScriptVar ret;
      This->standardDispatch(methodname, ret, args, AcdkDmiClient::getDmiClient(), Nil, 0, This->getClazzInfo(), cmi); 
      
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      /*
      if (ret.equal(This).isTrue() == true)
        writeAnyParam(*out, byRef(This));
      else
      */
      writeAnyParam(*out, ret);
      writeAnyParams(*out, cmi, args, MiAiOut);
      return out;
    }
    else if (method->equals("dyn_invoke_static") == true)
    {
      RString classname = input.read_string();
      RString methodname = input.read_string();
      RClass cls = Class::forName(classname);
      ScriptVarArray args;
      const ClazzInfo* ci = cls->objectClazzInfo();
      const ClazzMethodInfo* cmi = lookupMethodNoPolymorph(ci, methodname, args, Nil, 
        AcdkDmiClient::getDmiClient(), MiStatic);
      // cmi == 0
      readAnyParams(input, cmi, args, MiAiIn);
      ScriptVar ret;
      cls->objectClazzInfo()->static_dispatch(methodname, ret, args, AcdkDmiClient::getDmiClient(), Nil, 0, cls->objectClazzInfo(), cmi);
      
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      writeAnyParam(*out, ret);
      writeAnyParams(*out, cmi, args, MiAiOut);
      return out;
    }
    else if (method->equals("dyn_peek") == true)
    {
      RString membername = input.read_string();
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      ScriptVar erg = This->getMember(membername, AcdkDmiClient::getDmiClient(), 0, 0);
      writeAnyParam(*out, erg);
      return out;
    }
    else if (method->equals("dyn_peek_static") == true)
    {
      RString classname = input.read_string();
      RString membername = input.read_string();
      RClass cls = Class::forName(classname);
      ScriptVar ret = getStaticMember(cls->objectClazzInfo(), membername, AcdkDmiClient::getDmiClient(), 0, 0);
      
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      writeAnyParam(*out, ret);
      return out;
    }
    else if (method->equals("dyn_poke") == true)
    {
      RString membername = input.read_string();
      ScriptVar nval = readAnyParam(input);
      This->setMember(membername, nval, AcdkDmiClient::getDmiClient(), 0);
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      return out;
    }
    else if (method->equals("dyn_poke_static") == true)
    {
      RString classname = input.read_string();
      RString membername = input.read_string();
      ScriptVar nval = readAnyParam(input);
      RClass cls = Class::forName(classname);
      setStaticMember(cls->objectClazzInfo(), membername, nval, AcdkDmiClient::getDmiClient(), 0);
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      return out;
    }
    // get factory class of given name
    else if (method->equals("get_cor_factory") == true)
    {
      RString classname = input.read_string();
      RClass cls = Class::forName(classname);
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      out->write_Object(new AcdkObject(cls->objectClazzInfo()));
      return out;
    }
    // factory class
    else if (method->equals("dii_create") == true) 
    {
      RString methodname = input.read_string();
    }
    else 
    {
      return dii_invoke(method, input, handler);
      
    }
  } catch (RThrowable ex) {
    ::org::omg::CORBA::portable::ROutputStream out =  handler.createExceptionReply();
    out->write_exception(ex);
    return out;
  }

/*

  // if (method->equals("_is_a") == true) {


  } else if (method->startsWith("__acdk_dmi_") == true) {
    
    
    //  This is ACDK DMI over IIOP

    int methodflags = input.read_long();
    int argcount = input.read_long();
    ScriptVarArray args(argcount);
    int i = 0;
    for (i = 0; i < argcount; ++i)
    {
      input.read_scriptVar(args[i], CalleeRead);
    }
    RString nmethod = method->substr(11);
    ScriptVar ret; 
    if (nmethod->equals("New") == true)
    {
      StandardDispatch(nmethod->c_str(), ret, args, AcdkDmiClient::getDmiClient(), Nil, methodflags, 0);
    } 
    else
    {
      This->standardDispatch(nmethod->c_str(), ret, args, AcdkDmiClient::getDmiClient(), Nil, methodflags, 0);
    }
    ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();

    ret.flags |= MiAiOut;
    out->write_scriptVar(ret, CalleeWrite);
    for (i = 0; i < argcount; ++i)
    {
      out->write_scriptVar(args[i], CalleeWrite);
    }
    return out;
  } else {
    ::acdk::lang::sys::core_vector<const ClazzMethodInfo*> vec;
    if (This != Nil)
      StdDispatch::findFunctions(This->clazzInfo(), method->c_str(), MiPublic, vec);

    if (vec.size() == 0)
    {
      if (method->equals("New") == true) 
      {
        
        RString classname = input.read_string();
        ScriptVarArray args(0);
        readAnyParams(input, args);
        RString constructorname = classname;
        if (constructorname->indexOf('/') != -1)
          constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
        RObject newobj = StdDispatch::invokeStaticMethod(classname, constructorname, args);
        RAcdkObject acdkobj = new AcdkObject(newobj);
        ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
        out->write_Object(&acdkobj);
        return out;
      }
      
          
      } else if (method->equals("_dyn_invoke_static") == true) {

      } else if (method->startsWith("_dyn_") == true) {
         RString funcname = method->substr(strlen("_dyn_"));
          ScriptVarArray args(0);
          readAnyParams(input, args);
          ScriptVar erg = This->StdDispatch::invokeMethod(funcname, args);
          ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
          writeArgsFromScriptVar(*out, erg, args);
          
      } else {
        // ### error
      }
    }
    if (vec.size() == 1)
    {
      ScriptVarArray args(vec[0]->getArgumentCount());
      corba2ScriptVar(input, args, vec[0]);
      ScriptVar ret;
      This->standardDispatch(method->c_str(), ret, args, AcdkDmiClient::getDmiClient(), Nil, MiPublic, 0);
      ::org::omg::CORBA::portable::ROutputStream out =  handler.createReply();
      
      scriptVar2Corba(*out, ret);
      return out;
    } else { // more than one possibility

    }
  }*/
  return Nil; // ###
}


::acdk::lang::RObject 
AcdkObject::get_cor_factory(IN(RString) classname)
{
  return Nil;
}

RObject 
AcdkObject::dyn_new(IN(RString) classname, IN(RString) constructor, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
{
  return Nil;
}

RDmiObject 
AcdkObject::dyn_invoke(IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
{
  return Nil;
}

RDmiObject 
AcdkObject::dyn_invoke_static(IN(RString) classname, IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
{
  return Nil;
}

RDmiObject 
AcdkObject::dyn_peek(IN(RString) membername)
{
  return Nil;
}

RDmiObject 
AcdkObject::dyn_peek_static(IN(RString) classname, IN(RString) membername)
{
  return Nil;
}

void 
AcdkObject::dyn_poke(IN(RString) membername, IN(RDmiObject) value)
{
}

void 
AcdkObject::dyn_poke_static(IN(RString) classname, IN(RString) membername, IN(RDmiObject) value)
{
}





} // namespace orb 
} // namespace acdkx 


