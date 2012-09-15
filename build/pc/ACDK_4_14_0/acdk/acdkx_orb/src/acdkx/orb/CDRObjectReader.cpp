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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CDRObjectReader.cpp,v 1.30 2005/04/21 08:28:52 kommer Exp $

#include "CDRObjectReader.h"
#include "ObjectKey.h"
#include "AORB.h"
#include "ServerDelegate.h"
#include "acdk2orb.h"
#include <acdk/util/logging/Log.h>
#include <acdk/text/Format.h>
#include <acdk/io/EOFException.h>
#include <org/omg/CORBA/OrbExceptions.h>
#include <org/omg/CORBA/portable/InvokeHandler.h>

namespace acdkx {
namespace orb {

USING_CLASS(::acdk::io::, EOFException);

using namespace acdk::lang::dmi;

CDRObjectReader::CDRObjectReader(IN(RReader) in, IN(org::omg::CORBA::RORB) orb)
: ACDK_FQ_SUPER_QUALIFIER(::acdk::io::, AbstractFilterReader)(in),
  _index(0)
{
}
 
//virtual 
int 
CDRObjectReader::read()
{
  int erg = ACDK_FQ_SUPER_QUALIFIER(::acdk::io::, AbstractFilterReader)::read();
  if (erg != -1)
    ++_index;
  return erg;
}

//virtual 
int 
CDRObjectReader::read(IN(RbyteArray) buffer, int offset/* = 0*/, int len/* = -1*/)
{
  int erg = ACDK_FQ_SUPER_QUALIFIER(::acdk::io::, AbstractFilterReader)::read(buffer, offset, len);
  if (erg != -1)
    _index += erg;
  return erg;
}


//virtual 
int 
CDRObjectReader::read(byte* buffer, int offset, int len)
{
  int erg = ACDK_FQ_SUPER_QUALIFIER(::acdk::io::, AbstractFilterReader)::read(buffer, offset, len);
  if (erg != -1)
    _index += erg;
  return erg;
}


//virtual 
bool 
CDRObjectReader::readBoolean()
{
  char c = readChar();
  //ACDK_LOG(Debug, RString("bool<[") +  c + "]");
  if (c == 1)
    return true;
  return false;
}

//virtual 
char 
CDRObjectReader::readChar()
{
  char c = (unsigned char)read();
  //ACDK_LOG(Debug, RString("char<[") +  (short)c + "]");
  return c;
}

uc2char 
CDRObjectReader::readUcChar()
{
  return readShort();
}
  
#define READBUFFER(size) \
  _checkAlignment(size); \
  byte buffer[size]; \
  if (read(buffer, 0, size) != size) \
    throw REOFException(new EOFException())

//virtual 
double 
CDRObjectReader::readDouble()
{
  READBUFFER(8);
  double retval = *(double*)buffer;
  CDRSWAP8(retval);
  //ACDK_LOG(Debug, RString("double<[") +  retval + "]");
  return retval;
}

//virtual 
float 
CDRObjectReader::readFloat()
{
  READBUFFER(4);
  float retval = *(float*)buffer;
  CDRSWAP4(retval);
  //ACDK_LOG(Debug, RString("float<[") +  retval + "]");
  return retval;
}

//virtual 
int 
CDRObjectReader::readInt()
{
  READBUFFER(4);
  int retval = *(int*)buffer;
  CDRSWAP4(retval);
  //ACDK_LOG(Debug, RString("int<[") +  retval + "]");
  return retval;
}

//virtual 
jlong 
CDRObjectReader::readLong()
{
  READBUFFER(8);
  jlong retval = *(jlong*)buffer;
  CDRSWAP8(retval);
  //ACDK_LOG(Debug, RString("jlong<[") +  retval + "]");
  return retval;
}

//virtual 
short 
CDRObjectReader::readShort()
{
  READBUFFER(2);
  short retval = *(short*)buffer;
  CDRSWAP2(retval);
  //ACDK_LOG(Debug, RString("short<[") +  retval + "]");
  return retval;
}

//virtual 
RString 
CDRObjectReader::readString()
{
  int size = read_long();
  if (size == 0)
    THROW1(Exception, RString("Buffer must be greater as 0"));
  if (size > 1024 * 1024)
    THROW1(Exception, RString("Buffer probably too big: ") + size);
  
  RString str = new String(size + 1); // ### FIXME unicode
  char* ptr = const_cast<char*>(str->c_str());
  read_char_array(ptr, 0, size);
  ptr[size - 1] = 0;
  str->_setEnd((byte*)ptr + size - 1);
  
  //ACDK_LOG(Debug, RString("string<[") +  str + "]");
  return str;  
}

//virtual 
RObject 
CDRObjectReader::readObject()
{
  THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  return Nil;
}

using ::acdk::lang::reflect::Modifier;
using ::acdk::lang::dmi::SysField;


#define CDR_READ_ARRAY(Char, char, octet) \
if (elci == ClazzInfo::get##Char##Clazz()) \
{ \
  R##char##Array arr = new char##Array(count); \
  for (int i = 0; i < count; ++i) \
    arr[i] = read_##octet(); \
  return &arr; \
}


//virtual 
::acdk::lang::RObject 
CDRObjectReader::readObject(IN(::acdk::lang::RClass) theClass)
{
  RClass theRealClass = theClass; //Class::forName(theRealClassname);
  
  if (theRealClass == String::GetClass())
    return &read_string();
  const ClazzInfo* ci = theRealClass->objectClazzInfo();
  
  if (theRealClass->isArray() == true) 
  {
    const ClazzInfo* elci = ci->getElementClazzInfo();
    RClass elcls = Class::getSingeltonClass(elci);
    int count = read_long();
    if (elci->isBasicClazz() == true)
    {
      CDR_READ_ARRAY(Bool, bool, boolean)
      CDR_READ_ARRAY(Char, char, octet)
      CDR_READ_ARRAY(Byte, byte, octet)
      CDR_READ_ARRAY(Short, short, short)
      CDR_READ_ARRAY(Int, int, long)
      CDR_READ_ARRAY(Long, long, longlong)
      CDR_READ_ARRAY(Float, float, float)
      CDR_READ_ARRAY(Double, double, float)
      // ### TODO handle wchar
      // ### TODO THROW unknonw basic type
    }
    else
    {
      RObjectArray tobj;
      if (elci->array_creator != 0)
        tobj = (RObjectArray)elci->array_creator(count);
      else
        tobj = (ObjectArray*)new ObjectArrayBase(elci, count);
      ObjectArray* ta = reinterpret_cast<ObjectArray*>(&tobj);
      for (int i = 0; i < count; ++i)
      {
        if (elci == String::clazzInfo())
          ta->set(i, &read_string());
        else
          ta->set(i, read_acdk_object(elcls));
      }
      return ta;
    }
  }
  RObject newObj = theRealClass->newInstance();
  
  ::acdk::lang::dmi::SysFields members = newObj->getInternalFields(MiNonStatic);
  /*bool hasChild = members.size() > 0; */
  for (int i = 0; i < members.size(); i++) 
  {
    ::acdk::lang::dmi::SysField& f = members[i];
    if (f.fieldInfo->flags & MiStatic)
      continue;
    switch (f.type) {
    case SysField::FT_Void: break;
    case SysField::FT_Bool: *f.cont.bval = read_boolean(); break;
    case SysField::FT_Char : *f.cont.cval = read_octet();  break;
    case SysField::FT_UcChar: *f.cont.ucval = read_short();  break;
    case SysField::FT_Byte : *f.cont.cval= read_octet();  break;
    case SysField::FT_Short : *f.cont.sval = read_short();  break;
   
    case SysField::FT_Int : *f.cont.ival = read_long();  break;
    case SysField::FT_JLong : *f.cont.jlval = read_longlong();  break;
    case SysField::FT_Float : *f.cont.fval = read_float();  break;
    case SysField::FT_Double : *f.cont.dval = read_float();  break;
    case SysField::FT_Object : {
      RClass member = Class::getSingeltonClass(f.fieldInfo->type);
      if (member == String::GetClass())
        f.set((RObject)read_string());
      else
        f.set(readObject(member));
      break;
    }
    }
  }
  return newObj;
}

//foreign virtual 
acdk::lang::dmi::ScriptVar 
CDRObjectReader::readScriptVar(bool withTypeInfo, bool withFlags)
{
  // ### implement me
  return acdk::lang::dmi::ScriptVar();
}

void 
CDRObjectReader::_checkAlignment(int align)
{
  int remainder = align - (_index % align);
  char data;
  if (remainder != align) {
    for (int i = remainder; i > 0; i--) {
      int erg = _in->read();
      if (erg == -1)
        return;
    }
    _index += remainder;
  }
  
}

//virtual 
void 
CDRObjectReader::read_char_array(char* value, int offset, int length) 
{ 
  int erg = read((byte*)value, offset, length);
  //ACDK_LOG(Debug, "charray<[" + acdk::text::Format::hexToString(value + offset, length) + "]");
}


//virtual 
::acdk::lang::RObject 
CDRObjectReader::read_acdk_object(IN(::acdk::lang::RClass) clz)
{
  if (clz == String::GetClass())
    return &read_string();
  if (::org::omg::CORBA::Object::GetClass()->isAssignableFrom(clz) == true ||
      ::org::omg::CORBA::portable::InvokeHandler::GetClass()->isAssignableFrom(clz) == true )
    return (::acdk::lang::RObject)read_Object();
  if (clz->hasMetaAttribute("acdkx_orb_StructType") == true)
  {
    RObject tobj = clz->newInstance();
    read_struct(tobj);
    return tobj;
  }
  if (clz->isArray() == true)
    return readObject(clz);
  return (::acdk::lang::RObject)read_Object();
  
  //THROW3_FQ(::org::omg::CORBA::, MARSHAL, clz->getName(), 5, ::org::omg::CORBA::COMPLETED_NO);
  //return Nil;
  
  //return (::acdk::lang::RObject)read_Object();
   /*
  if (::acdk::io::Serializable::GetClass()->isAssignableFrom(clz) == true ||
      ::acdk::lang::Throwable::GetClass()->isAssignableFrom(clz) == true ||
      clz->isArray() == true)
    return readObject(clz);
  
  
  */
  /* not used
  RObjectKey objectkey = new ObjectKey();
  objectkey->ior.read(*this);
  objectkey->ior_inited = true;
  objectkey->fromIOR();
  if (objectkey->isLocal() == true) {
    RServerDelegate serverobj(objectkey->localObject);
    return &serverobj;
  }
  return &AORB::createProxy(objectkey);
  */
  /*THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  return Nil;*/
}

//virtual 
::org::omg::CORBA::RObject 
CDRObjectReader::read_Object()
{
  return &ServerDelegate::read(*this, Nil);
  
}

//virtual 
::org::omg::CORBA::RObject 
CDRObjectReader::read_Object(IN(::acdk::lang::RClass) clz)
{
  return &ServerDelegate::read(*this, clz);
}

//foreign virtual 
::acdk::lang::RObject 
CDRObjectReader::read_abstract_interface() 
{
  return (::acdk::lang::RObject)read_Object();
}

//foreign virtual 
::acdk::lang::RObject 
CDRObjectReader::read_abstract_interface(IN(::acdk::lang::RClass) clz) 
{
  if (clz == Nil || clz->isAssignableFrom(::org::omg::CORBA::Object::GetClass()) == true)
    return (::acdk::lang::RObject)read_Object();
  THROW3_FQ(::org::omg::CORBA::, MARSHAL, clz->getName(), 5, ::org::omg::CORBA::COMPLETED_NO);
  return Nil;
}

using ::acdk::lang::dmi::ScriptVar;


#define READ_VALUES() ((dir != CalleeRead) || ((flags & MiAiIn) == MiAiIn))

//virtual 
void 
CDRObjectReader::read_scriptVar(OUT(::acdk::lang::dmi::ScriptVar) sv, ParamCallDirection dir)
{
  if (dir == CallerRead) {
    if ((sv.flags & MiAiOut) != MiAiOut)
      return;
  }
  int type = read_long();
  int flags = read_long();
  switch (type)
  {
  case ScriptVar::UnknownType:
    // Nothing
    sv = ScriptVar();
    break;
  case ScriptVar::BoolRefType:
  case ScriptVar::BoolType:
    if (READ_VALUES())
      sv = read_boolean();
    else
      sv = false;
    break;
  case ScriptVar::CharRefType:
  case ScriptVar::CharType:
    if (READ_VALUES())
      sv = read_char();
    else
      sv = (char)0;
    break;
  case ScriptVar::ByteRefType:
  case ScriptVar::ByteType:
    if (READ_VALUES())
      sv = read_octet(); 
    else
      sv = (byte)0;
    break;
  case ScriptVar::ShortRefType:
  case ScriptVar::ShortType:
    if (READ_VALUES())
      sv = read_short();
    else
      sv = (short)0;
    break;
  case ScriptVar::IntRefType:
  case ScriptVar::IntType:
    if (READ_VALUES())
      sv = read_long();
    else
      sv = (int)0;
    break;
  case ScriptVar::LongRefType:
  case ScriptVar::LongType: // i64
    if (READ_VALUES())
      sv = read_longlong();
    else
      sv = (jlong)0;
    break;
  case ScriptVar::FloatRefType:  
  case ScriptVar::FloatType:
    if (READ_VALUES())
      sv = read_float();
    else
      sv = (float)0.0;
    break;
  case ScriptVar::DoubleRefType:
  case ScriptVar::DoubleType:
    if (READ_VALUES())
      sv = read_double(); 
    else
      sv = (double)0.0;
    break;
  case ScriptVar::ObjectRefType:
    if (READ_VALUES())
      sv.setOwnObjectReference(read_fq_object(flags));
    else
      sv.setOwnObjectReference();
    break;
  case ScriptVar::ObjectType:
  {
    if (READ_VALUES())
    {
      sv = read_fq_object(flags);
    } else
      sv = RObject();
    break;
  }
  default:
    break;
  }
  sv.flags = flags;
}

RObject 
CDRObjectReader::read_fq_object(int flags)
{
  int sflags = read_long();
  
  if ((sflags & DmiGiopIsNil) == DmiGiopIsNil)
    return Nil;

  if ((sflags & DmiGiopIsString) == DmiGiopIsString)
    return &readString();
  if ((sflags & DmiGiopIsSerialized) == DmiGiopIsSerialized)
  {
    RString cname = read_string();
    RClass cls = Class::forName(cname);
    return readObject(cls);
  } 
  if ((sflags & DmiGiopIsReference) == DmiGiopIsReference)
  {
    return (RObject)read_Object();
  }
  // ### ex
  return Nil;
}


void 
CDRObjectReader::read_struct(IN(::acdk::lang::RObject) obj, const ::acdk::lang::dmi::ClazzInfo* ci, bool withParents)
{
  if (withParents == true && 
      ci != Throwable::clazzInfo()  &&
      ci != ::org::omg::CORBA::SystemException::clazzInfo() &&  
      ci != ::org::omg::CORBA::OrbException::clazzInfo() &&  
      ci->interfaces[0] != 0)
    read_struct(obj, ci->interfaces[0]->type, withParents);
  ci = ci->loadFullClazzInfo();
  bool isEx = ci == Throwable::clazzInfo();
  
  for (int i = 0; i < ci->getFieldsCount(); ++i)
  {
    const acdk::lang::dmi::ClazzFieldInfo* fi = ci->fields[i];
    if (MetaInfo::isStatic(fi->flags) == true ||
          fi->flags & MiFiTransient)
      continue;
    if (isEx == true &&  fi->equalsName("_what") == false)
      continue;
    ScriptVar sv;
    readValueParam(*this, sv, fi->type, 0, 0);
    obj->setMember(fi->name, sv, CorObject::_dmiClient, 0);
    
  }
}

void 
CDRObjectReader::read_struct(IN(RObject) obj, bool withParents)
{
  read_struct(obj, obj->getClazzInfo(), withParents);
}

RThrowable 
CDRObjectReader::read_exception()
{
  try {
    RString exname = read_string();
    if (exname->startsWith("IDL:") == true)
    {
      RString clsname = exname->substr(4);
      clsname = clsname->substr(0, clsname->lastIndexOf(":"));
      RClass cls = Class::forName(ObjectKey::classNameFromRepId(clsname));

      RThrowable ex = (RThrowable)cls->newInstance();
      read_struct(&ex, true);
      return ex;
    } 
    else
    {
      RClass cls = Class::forName(exname);
      int minor = read_long();
      int completed = read_long();
      ::org::omg::CORBA::RSystemException ex = (::org::omg::CORBA::RSystemException)cls->newInstance();
      ex->minor(minor);
      ex->completed(completed);
      return (RThrowable)&ex;
    }
  } 
  catch (RThrowable ex)
  {
    return new ::org::omg::CORBA::UNKNOWN();
  }
  return Nil; // ### implement
}

} // namespace orb 
} // namespace acdkx 


