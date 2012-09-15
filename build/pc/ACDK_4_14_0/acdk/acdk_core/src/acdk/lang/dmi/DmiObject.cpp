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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiObject.cpp,v 1.18 2005/04/18 14:22:27 kommer Exp $

#include "DmiObject.h"

namespace acdk {
namespace lang {
namespace dmi {


::acdk::lang::Object* 
DmiObject::_cast(const ::acdk::lang::dmi::ClazzInfo* ci)
{
  return Object::_cast(ci);
}

void 
DmiObject::getCollectableFields(FieldReferences& fields)
{
  if (isObjectType() == true)
    fields.push_back((::acdk::lang::RObject*)_getrobject()._ref_this());
}

const acdk::lang::dmi::ClazzMethodInfo* 
DmiObject::standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/)
{
  if (isObjectType() == true)
  {
    RObject obj = getObjectVar();
    const ClazzInfo* ci = obj->getClazzInfo();
    ASCLITERAL(peek);
    ASCLITERAL(poke);
    if (fname->equals(lit_peek) || fname->equals(lit_poke))
    {
      if (ci->flags & MiCiWeakBind)
        return obj->standardDispatch(fname, ret, args, dc, namedArgs, flags, ci, methinf);
      RString mname = args[0].getStringVar();
      if (fname->equals(lit_peek) == true)
      {
        ret = obj->getMember(mname, dc, flags);
      } 
      else
      {
        ScriptVar nval = args[1];
        obj->setMember(mname, nval, dc, flags);
      }
      return (const acdk::lang::dmi::ClazzMethodInfo* )1;
    }
    else
    {
      return obj->standardDispatch(fname, ret, args, dc, namedArgs, flags, ci, methinf);
    }
  }
  return StdDispatch::standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}


DmiObject::DmiObject(const ClazzInfo* clazz)
: ScriptVar(ScriptVar::getInitialized(clazz))
{
  
}

DmiObject::DmiObject() 
: ScriptVar()
{
  
}

RDmiObject 
DmiObject::increment()
{
  *this = ScriptVar::addition(1);
  return this;
}

RDmiObject 
DmiObject::decrement(short castFlags)
{
  *this = ScriptVar::subtraction(ScriptVar(1), castFlags);
  return this;
}


RDmiObject 
DmiObject::increment(int)
{
  RDmiObject ret = new DmiObject(inOf());
  *this = ScriptVar::addition(ScriptVar(1));
  return ret;
}

RDmiObject 
DmiObject::decrement(int)
{
  RDmiObject ret = new DmiObject(inOf());
  *this = ScriptVar::subtraction(ScriptVar(1));
  return ret;
}

::acdk::lang::Object* 
DmiObjectArray::_cast(const ::acdk::lang::dmi::ClazzInfo* ci)
{
  if (ci->isArray() == false)
    return DmiObjectArraySuper::_cast(ci);
  if (ci->userInfo == ClazzInfo::getBoolClazz())
  {
    RboolArray ia = new boolArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (bool)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getByteClazz())
  {
    RbyteArray ia = new byteArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (byte)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getCharClazz())
  {
    RcharArray ia = new charArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (char)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getUcCharClazz())
  {
    RuccharArray ia = new uccharArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (ucchar)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getShortClazz())
  {
    RshortArray ia = new shortArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (short)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getIntClazz())
  {
    RintArray ia = new intArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (int)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getLongClazz())
  {
    RlongArray ia = new longArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (jlong)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getFloatClazz())
  {
    RfloatArray ia = new floatArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (float)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  if (ci->userInfo == ClazzInfo::getDoubleClazz())
  {
    RdoubleArray ia = new doubleArray(length());
    for (int i = 0; i < ia->length(); ++i)
      ia[i] = (double)*(*this)[i];
    _casted = &ia;
    return ia;
  }
  RClass cls = Class::getSingeltonClass(ci);
  RefHolder<ObjectArrayBase> dmiOa = (RefHolder<ObjectArrayBase>)Object::New(cls->getName(), inOf(length()));
  for (int i = 0; i < length(); ++i)
  {
    dmiOa->setElement(i, ((ScriptVar*)(*this)[i])->getObjectVar(SVCastStdFlags, dmiOa->getElementClazzInfo()));
  }
  _casted = &dmiOa;
  return dmiOa;
  //return DmiObjectArraySuper::_cast(ci);
}

} // namespace dmi
} // namespace lang
} // namespace acdk


