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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Field.cpp,v 1.16 2005/04/13 13:28:34 kommer Exp $



#include <acdk.h>

#include "../Boolean.h"
#include "../Integer.h"
#include "../Byte.h"
#include "../Character.h"
#include "../Long.h"
#include "../Short.h"
#include "../Float.h"
#include "../Double.h"

#include "../IllegalArgumentException.h"
#include "../UnsupportedOperationException.h"
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>


namespace acdk {
namespace lang {
namespace reflect {

using ::acdk::lang::IllegalArgumentException;
using ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient;

Field::Field(const acdk::lang::dmi::ClazzInfo* clazz, const acdk::lang::dmi::ClazzFieldInfo* field) 
: AccessibleObject(),
  _clazz(clazz),
  _field(field)
{
  setAccessible(field->flags & dmi::MiPublic);
}

//virtual 
bool 
Field::equals(IN(RObject) obj)
{
  if (instanceof(obj, Field) == false)
    return false;
  RField o = RField(obj);
  return toString()->compareTo(o->toString()) == 0;
}


bool 
Field::_isStatic()
{
  return dmi::MetaInfo::isStatic(_field->flags);
}



//virtual 
RObject 
Field::get(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return get();
  AcdkStdWeakTypeDmiClient dmiclient;
  return (RObject)obj->getMember(ACDK_STACK_STR(_field->name), dmiclient, accessFlags);
}

RObject 
Field::get(int accessFlags)
{
  AcdkStdWeakTypeDmiClient dmiclient;
  return (RObject)getStaticMember(_clazz, ACDK_STACK_STR(_field->name), dmiclient, accessFlags);
}


//virtual 
bool 
Field::getBoolean(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getBoolClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getBoolClazz());
}

//virtual 
byte 
Field::getByte(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getByteClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getByteClazz());
}

//virtual 
char 
Field::getChar(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getCharClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getCharClazz());
}

//virtual 
ucchar 
Field::getUcChar(IN(RObject) obj, int accessFlags)
{
	if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getUcCharClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getUcCharClazz());
}

//virtual 
RClass 
Field::getDeclaringClass()
{
  return Class::getSingeltonClass(_clazz);
}

//virtual 
double 
Field::getDouble(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getDoubleClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getDoubleClazz());
}

//virtual 
float 
Field::getFloat(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getFloatClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getFloatClazz());
}

//virtual 
int 
Field::getInt(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getIntClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getIntClazz());
  
}

//virtual 
jlong 
Field::getLong(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getLongClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getLongClazz());
}

//virtual 
int 
Field::getModifiers()
{
  return _field->flags;
}

//virtual 
RString 
Field::getName()
{
  return new String(_field->name);
}
 
//virtual 
short 
Field::getShort(IN(RObject) obj, int accessFlags)
{
  if (obj == Nil)
    return getStaticMember(_clazz, ACDK_STACK_STR(_field->name), 
                                       ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getShortClazz());
  return obj->getMember(ACDK_STACK_STR(_field->name), ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), 
                                       accessFlags,
                                       ::acdk::lang::dmi::ClazzInfo::getShortClazz());
}


//virtual 
RClass 
Field::getType()
{
  return Class::getSingeltonClass(_field->type);
}
 
//virtual 
int 
Field::hashCode()
{
  return getType()->hashCode() ^ getName()->hashCode();
}

//virtual 
void 
Field::set(IN(RObject) obj, IN(RObject) value, int accessFlags)
{
  AcdkStdWeakTypeDmiClient dmiclient;
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field->name, value, dmiclient, accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), value, dmiclient, accessFlags);
}

//virtual 
void 
Field::setBoolean(IN(RObject) obj, bool v, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, v, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags); // ### @todo accessFlags will be ignored
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), v, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);

  
}

//virtual 
void 
Field::setByte(IN(RObject) obj, byte c, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, c, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), c, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}


//virtual 
void 
Field::setChar(IN(RObject) obj, char c, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, c, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), c, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}

//virtual 
void 
Field::setUcChar(IN(RObject) obj, ucchar c, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, c, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), c, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}


//virtual 
void 
Field::setDouble(IN(RObject) obj, double d, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, d, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), d, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}

//virtual 
void 
Field::setFloat(IN(RObject) obj, float v, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, v, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), v, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}

//virtual 
void 
Field::setInt(IN(RObject) obj, int i, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, i, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), i, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}

//virtual 
void 
Field::setLong(IN(RObject) obj, jlong l, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, l, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), l, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}

//virtual 
void 
Field::setShort(IN(RObject) obj, short s, int accessFlags)
{
  if (obj == Nil)
  {
    setStaticMember(_clazz, _field, s, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
    return;
  }
  obj->setMember(ACDK_STACK_STR(_field->name), s, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), accessFlags);
}

//virtual 
RString 
Field::toString()
{
  return const_cast<Field*>(this)->getType()->toString() + " " + getName();
}



} // reflect
} // lang
} // acdk


