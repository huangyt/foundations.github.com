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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CorObject.cpp,v 1.26 2005/02/05 10:45:39 kommer Exp $

#include "CorObject.h"
#include <acdk/lang/ParamsMismatchException.h>
#include <org/omg/CORBA/Any.h>

#include "acdk2orb.h"

namespace acdkx {
namespace orb {

using namespace ::acdk::lang::dmi;

using ::acdk::lang::dmi::ClazzInfo;
using ::acdk::lang::dmi::ClazzMethodInfo;
using ::org::omg::CORBA::portable::CallerWrite;
using ::org::omg::CORBA::portable::CalleeRead;
using ::org::omg::CORBA::portable::CalleeWrite;
using ::org::omg::CORBA::portable::CallerRead;

/* old code
const ClazzMethodInfo* 
LookupMethodNoPolymorph( const ClazzInfo*& clazz // ### in StandardDispatch.h //??
                                                          , IN(RString) fname
                                                          , ScriptVarArray& args
                                                          , IN(RStringArray) namedArgs
                                                          , DmiClient& dc
                                                          , int flags
                                                          )
{
  int i;
  for (i = 0; clazz->methods[i]; ++i)
  {
    const ClazzMethodInfo* cmi = clazz->methods[i];
    if (cmi->altlabel == 0)
    {
      if (fname->equals(cmi->name) == true)
        return cmi;
    }
    else
    {
      if (fname->equals(cmi->altlabel) == true)
        return cmi;
    }
  }
  if (flags & IsDeclared)
    THROW1(NoSuchMethodException, fname);
  

  for (i = 0; clazz->interfaces[i]; i++) 
  {
    const ClazzMethodInfo* ret = LookupMethodNoPolymorph(clazz->interfaces[i]->type, fname, args, Nil, dc, flags);
    if (ret != 0)
      return ret;
  }
  THROW1(NoSuchMethodException, fname);
  return 0;
}*/

::acdk::lang::dmi::AcdkDmiClient CorObject::_dmiClient;

bool 
CorObject::isDmiOverLoaded(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) funcname, const acdk::lang::dmi::ClazzMethodInfo* mi, acdk::lang::dmi::ClazzMethodArgInfo** const args)
{
  return true;
}

CorObject::CorObject()
: ServerDelegate()
, _objectClazzInfo(0)
{
  _dmiClient._lookupFunc = ::acdk::lang::dmi::StdDispatch::lookupMethodNoPolymorph;
}

CorObject::CorObject(IN(RString) stringifiedref)
: ServerDelegate(stringifiedref)
, _objectClazzInfo(0)
{
  _dmiClient._lookupFunc = ::acdk::lang::dmi::StdDispatch::lookupMethodNoPolymorph;
}

CorObject::CorObject(IN(RObjectKey) objectkey, IN(RORB) orb)
: ServerDelegate(orb, objectkey)
, _objectClazzInfo(0)
{
  _dmiClient._lookupFunc = ::acdk::lang::dmi::StdDispatch::lookupMethodNoPolymorph;
}


//virtual 
const ::acdk::lang::dmi::ClazzMethodInfo* 
CorObject::standardDispatch(IN(RString) fname, 
                 ::acdk::lang::dmi::ScriptVar& ret, 
                 ::acdk::lang::dmi::ScriptVarArray& args, 
                 ::acdk::lang::dmi::DmiClient& dc,  IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                  const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                 const ::acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/)

{
  while (true) 
  {
    if (AORB::getAORB().ignoreLocal == false && is_local() == true) 
    {
      return localObject()->standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
    } 
    else // remote
    { 
      
      // not local
      ::org::omg::CORBA::portable::ROutputStream out = Nil;
      ::org::omg::CORBA::portable::RInputStream in = Nil;
      if (_objectClazzInfo == 0)
      {
        THROW1(DmiException, "Not typeinfo for this object available. (use setRemoteClass() to set type info");
      }
      if (fname->equals("peek") == true)
      {
        RString fieldname = args[0].getStringVar();
        int flags = 0;
        const ClazzInfo* ci = _objectClazzInfo;
        const acdk::lang::dmi::ClazzFieldInfo* cfi = ClazzInfo::findField(ci, fieldname, 0);
        if (cfi == 0)
          THROW1(DmiException, "Field not found: " + fieldname);

        out = _request(fname, true);
        out->write_string(fieldname);
        in = ServerDelegate::_invoke(out);
        readValueParam(*in, ret, cfi->type, cfi->flags, 0);
        return (const ::acdk::lang::dmi::ClazzMethodInfo* )1;
      }
      if (methinf == 0)
      {
        RString funcname = fname;
        if (funcname->startsWith("createCor") == true)
          funcname = funcname->substr(strlen("createCor"));
        methinf = dc._lookupFunc(_objectClazzInfo, funcname, args, namedArgs, dc, flags, 0);
      }
      try {
        
        if (methinf != 0) 
        {
          if (args.size() != methinf->getArgumentCount())
            THROW1(ParamsMismatchException, RString("Wrong count of arguments in method: ") + fname);
          out = _request(methinf->altlabel, true);
          int i;
          for (i = 0; methinf->methodArgs[i] != 0; ++i)
          {
            writeValueParam(*out, args[i], methinf->methodArgs[i]->type, methinf->methodArgs[i]->flags, MiAiIn);
          }
          in = ServerDelegate::_invoke(out);

          readValueParam(*in, ret, methinf->returnType, 0, 0);
          for (i = 0; methinf->methodArgs[i] != 0; ++i)
          {
            readValueParam(*in, args[i], methinf->methodArgs[i]->type, methinf->methodArgs[i]->flags, MiAiOut);
          }
          return methinf;
        }
        /*
        else if (strcmp(fname, "NewAcdkObject") == 0)
        {
          if (args.size() < 2)
            THROW1(ParamsMismatchException, "AcdkObject::NewAcdkObject needs name of class as first parameters and name of constructor as second parameter");  
          ScriptVarArray sargs(args.size() - 1);
          for (int i = 0; i < 

        }
        else if (AORB::getAORB().ignoreDmiOverGIOP == false && isAcdkObject() == true)
        {
          StringBuffer sb(strlen("__acdk_dmi_") + strlen(fname) + 1);
          sb.append((const char*)"__acdk_dmi_"); sb.append(fname);
          out = _request(sb.toString()->c_str(), true);
          out->write_long(flags);
          int argcount = args.size();
          out->write_long(argcount);
          int i = 0;
          for (i = 0; i < argcount; ++i)
          {
            out->write_scriptVar(args[i], CallerWrite);
          }
          in = ServerDelegate::_invoke(out);
          ret.flags |= acdk::lang::reflect::MiAiOut;
          in->read_scriptVar(ret, CallerRead);
          for (i = 0; i < argcount; ++i)
          {
            in->read_scriptVar(args[i], CallerRead);
          }
          return  (::acdk::lang::dmi::ClazzMethodInfo*)1;
        } else {
          return 0;
        }
        */
      } catch (::org::omg::CORBA::portable::RRemarshalException ) {
        continue;
      } catch (::org::omg::CORBA::portable::RApplicationException ex) {
        ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
        in = ex->getInputStream();
        THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
      } 
    } // not local
  } // while (true);
  
  
  return (::acdk::lang::dmi::ClazzMethodInfo*)0;
}


using ::acdk::lang::dmi::StdDispatch;

//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
CorObject::StandardDispatch(IN(RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, 
                           ::acdk::lang::dmi::DmiClient& dc,  IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                           const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                           const ::acdk::lang::dmi::ClazzMethodInfo* methinf
)
{
  if (methinf == 0)
    methinf = StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);
  return ::acdk::lang::Object::StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}


bool 
CorObject::isAcdkObject()
{
  return objectKey()->_isAcdkObject == true;
}

void 
CorObject::setAcdkTypeLibrary(IN(RString) classname)
{
  RClass cls = Class::forName(classname);
  _objectClazzInfo = cls->objectClazzInfo();
}


RObject 
CorObject::get_cor_factory(IN(RString) classname)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("get_cor_factory", true);
      out->write_string(classname);
      in = ServerDelegate::_invoke(out);
      RObject ret = in->read_abstract_interface(0);
      return ret;
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
}

RObject 
CorObject::createDmiProxy(IN(RClass) cls)
{
  RObject obj = cls->createDmiProxy(this);
  _objectClazzInfo = cls->objectClazzInfo();
  return obj;
}

RObject 
CorObject::dyn_new(IN(RString) classname, IN(RString) constructor, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_new", true);
      out->write_string(classname);
      out->write_string(constructor);
      writeAnyParams(*out, inp);
      in = ServerDelegate::_invoke(out);
      
      RObject ret = in->read_fq_object(0); //readAny(*in);
      
      readAnyParams(*in, outp);
      return ret;
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
}

RDmiObject 
CorObject::dyn_invoke(IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_invoke", true);
      out->write_string(methodname);
      writeAnyParams(*out, inp);
      in = ServerDelegate::_invoke(out);
      return new DmiObject(readAny(*in));
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
  return Nil;
}

RDmiObject 
CorObject::dyn_invoke_static(IN(RString) classname, IN(RString) methodname, IN(RDmiObjectArray) inp, OUT(RDmiObjectArray) outp)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_invoke_static", true);
      out->write_string(classname);
      out->write_string(methodname);
      writeAnyParams(*out, inp);
      in = ServerDelegate::_invoke(out);
      
      RDmiObject ret;
      readAnyParam(*in, ret); 
      
      readAnyParams(*in, outp);
      return ret;
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
}

RDmiObject 
CorObject::dyn_peek(IN(RString) membername)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_peek", true);
      out->write_string(membername);
      in = ServerDelegate::_invoke(out);
      return new DmiObject(readAny(*in));
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
  return Nil;
}

RDmiObject 
CorObject::dyn_peek_static(IN(RString) classname, IN(RString) membername)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_peek_static", true);
      out->write_string(classname);
      out->write_string(membername);
      in = ServerDelegate::_invoke(out);
      return new DmiObject(readAny(*in));
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
  return Nil;
}

void 
CorObject::dyn_poke(IN(RString) membername, IN(RDmiObject) value)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_poke", true);
      out->write_string(membername);
      writeAnyParam(*out, value);
      in = ServerDelegate::_invoke(out);
      return ;
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
}

void 
CorObject::dyn_poke_static(IN(RString) classname, IN(RString) membername, IN(RDmiObject) value)
{
  while (true) 
  {
    ::org::omg::CORBA::portable::ROutputStream out = Nil;
    ::org::omg::CORBA::portable::RInputStream in = Nil;
    try {
      out = _request("dyn_poke_static", true);
      out->write_string(classname);
      out->write_string(membername);
      writeAnyParam(*out, value);
      in = ServerDelegate::_invoke(out);
      return ;
    } catch (::org::omg::CORBA::portable::RRemarshalException ) {
      continue;
    } catch (::org::omg::CORBA::portable::RApplicationException ex) {
      ::acdk::lang::RClass exClass = ex->getUserExceptionClass();
      in = ex->getInputStream();
      THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString("Unexpected User Exception: ") + exClass->getName()); // ### dispatching
    } 
  } // while (true);
}

} // namespace orb 
} // namespace acdkx 


