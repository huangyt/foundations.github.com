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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CDRObjectWriter.cpp,v 1.26 2005/04/21 08:28:52 kommer Exp $


#include "CDRObjectWriter.h"
#include "ServerDelegate.h"
#include "CorObject.h"
#include "acdk2orb.h"

#include <org/omg/CORBA/portable/Delegate.h>
#include <org/omg/CORBA/portable/ObjectImpl.h>
#include <acdkx/orb/AcdkObject.h>
#include <acdk/util/logging/Log.h>
#include <acdk/text/Format.h>

namespace acdkx {
namespace orb {

using ::acdk::io::AbstractFilterWriter;

using namespace ::acdk::lang::dmi;


CDRObjectWriter::CDRObjectWriter(IN(RWriter) out, IN(org::omg::CORBA::RORB) orb)
: AbstractFilterWriter(out)
, _index(0)
, _orb(orb)
{

}



RInputStream 
CDRObjectWriter::create_input_stream() //### implement
{
  _throwNotImplementedYet("CDRObjectWriter::create_input_stream");
  return Nil;
}

//virtual 
void
CDRObjectWriter::writeBoolean(bool b)
{
  //ACDK_LOG(Debug, RString("bool=[") +  b + "]");
  if (b == true)
    writeChar(1);
  else
    writeChar(0);
}

//virtual 
void
CDRObjectWriter::writeChar(char b)
{
  //ACDK_LOG(Debug, RString("char=[") +  (short)b + "]");
  write(b);
}
 
void 
CDRObjectWriter::writeUcChar(uc2char b)
{
  writeShort(b);
}

//virtual 
void
CDRObjectWriter::writeShort(short value)
{
  //ACDK_LOG(Debug, RString("short=[") +  value + "]");
  _checkAlignment(2);
  byte buffer[3];
  buffer[2] = 0;
  short* sp = (short*)buffer;
  CDRSWAP2(value);
  *sp = value;
  write(buffer, 0, 2);
}
  
//virtual 
void
CDRObjectWriter::writeInt(int value)
{
  //ACDK_LOG(Debug, RString("int=[") +  value + "]");
  _checkAlignment(4);
  byte buffer[5];
  buffer[4] = 0;
  int* sp = (int*)buffer;
  CDRSWAP4(value);
  *sp = value;
  write(buffer, 0, 4);
}

//virtual 
void
CDRObjectWriter::writeLong(jlong value)
{
  //ACDK_LOG(Debug, RString("jlong=[") +  value + "]");
  _checkAlignment(8);
  byte buffer[9];
  buffer[8] = 0;
  jlong* sp = (jlong*)buffer;
  CDRSWAP8(value);
  *sp = value;
  write(buffer, 0, 8);
}
 
//virtual 
void
CDRObjectWriter::writeFloat(float value)
{
  //ACDK_LOG(Debug, RString("float=[") +  value + "]");
  _checkAlignment(4);
  byte buffer[5];
  buffer[4] = 0;
  float* sp = (float*)buffer;
  CDRSWAP4(value);
  *sp = value;
  write(buffer, 0, 4);
}
 
//virtual 
void
CDRObjectWriter::writeDouble(double value)
{
  //ACDK_LOG(Debug, RString("double=[") +  value + "]");
  _checkAlignment(8);
  byte buffer[9];
  buffer[8] = 0;
  double* sp = (double*)buffer;
  CDRSWAP8(value);
  *sp = value;
  write(buffer, 0, 8);
}

  

//virtual 
void
CDRObjectWriter::writeString(IN(RString) str)
{
  write_long(str->length() + 1);
  //ACDK_LOG(Debug, RString("string=[") +  str + "]");
  write((const byte*)str->c_str(), 0, str->length());
  write_octet(0); // 0-terminierung
}

//foreign virtual 
void 
CDRObjectWriter::write_char_array(const char* value, int offset, int length)
{
  //ACDK_LOG(Debug, "charray=[" + acdk::text::Format::hexToString(value + offset, length) + "]");
  write((const byte*)value, offset, length);
}



// virtual
void
CDRObjectWriter::write(const byte* cstr, int offset, int len)
{
  _index += len;
  _out->write(cstr, offset, len);
}

void 
CDRObjectWriter::write(byte c)
{
  _index += 1;
  _out->write(c);
}

//virtual 
void 
CDRObjectWriter::write(IN(RbyteArray) ch, int offset/* = 0*/, int len/* = -1*/)
{
  if (len == -1)
    _index += ch->length() - offset;
  else
    _index += len;
  _out->write(ch, offset, len);
}




void 
CDRObjectWriter::_checkAlignment(int align)
{
  int remainder = align - (_index % align);
  byte data = 0;
  if (remainder != align) {
    for (int i = remainder; i > 0; i--)
      _out->write(data);
    _index += remainder;
  }
}

void 
CDRObjectWriter::write_repid(IN(::acdk::lang::RObject) obj)
{
  write_string("IDL:" + obj->getClass()->getName() + ":1.0");
}

void 
CDRObjectWriter::write_struct(IN(::acdk::lang::RObject) obj, const ::acdk::lang::dmi::ClazzInfo* ci, bool withParents)
{
  if (withParents == true && 
      ci != Throwable::clazzInfo() && 
      ci != ::org::omg::CORBA::SystemException::clazzInfo()  && 
      ci != ::org::omg::CORBA::OrbException::clazzInfo() &&  
      ci->interfaces[0] != 0)
    write_struct(obj, ci->interfaces[0]->type, withParents);
  ci = ci->loadFullClazzInfo();
  bool isEx = ci == Throwable::clazzInfo();
  for (int i = 0; i < ci->getFieldsCount(); ++i)
  {
    const acdk::lang::dmi::ClazzFieldInfo* fi = ci->fields[i];
    if ((fi->flags & MiStatic) || (fi->flags & MiFiTransient))
      continue;
    if (isEx == true &&  fi->equalsName("_what") == false)
      continue;
    ScriptVar sv = obj->getMember(fi->name, CorObject::_dmiClient, 0);
    writeValueParam(*this, sv, fi->type, 0, 0);
  }
}

void 
CDRObjectWriter::write_struct(IN(::acdk::lang::RObject) obj, bool withParents)
{
  write_struct(obj, obj->getClazzInfo(), withParents);
}

void 
CDRObjectWriter::write_exception(IN(RThrowable) ex)
{
  
  if (instanceof(ex, ::org::omg::CORBA::SystemException) == true) 
  {
    ::org::omg::CORBA::RSystemException sysex(ex);
    write_string(ex->getClass()->getName());
    write_long(sysex->minor());
    write_long(sysex->completed());
  } 
  else 
  { 
    write_repid(ex);
    write_struct(ex, true);
  }
}


//virtual 
void 
CDRObjectWriter::write_acdk_object(IN(::acdk::lang::RObject) value)
{
  //ACDK_LOG(Debug, RString("obj=[") +  value + "]");
  if (value == Nil) {
    write_value((::acdk::io::RSerializable)value);
    return;
  }
  if (instanceof(value, ::org::omg::CORBA::Object) == true) 
  {
    write_Object(::org::omg::CORBA::RObject(value));
    return;
  }
  if (instanceof(value, String) == true) 
  {
    write_string(RString(value));
    return;
  }
  if (value->getClass()->hasMetaAttribute("acdkx_orb_StructType") == true)
  {
    //write_repid(value);
    write_struct(value);
    return;
  }
  if (instanceof(value, ::acdk::io::Serializable) == true ||
      instanceof(value, ::acdk::lang::Throwable) ||
      value->getClass()->isArray() == true) 
  {
    writeObject(value);
    return;
  }
  
  
  write_Object(new AcdkObject(value));
  //write_fq_object(value, 0);
  //THROW3_FQ(::org::omg::CORBA::, MARSHAL, value->getClass()->getName(), 5, ::org::omg::CORBA::COMPLETED_NO);
}

//virtual 
void 
CDRObjectWriter::write_Object(IN(::org::omg::CORBA::RObject) value) 
{
  ::org::omg::CORBA::portable::RDelegate d = Nil;
  if (instanceof(value, ::org::omg::CORBA::portable::ObjectImpl) == true) {
    RServerDelegate(::org::omg::CORBA::portable::RObjectImpl(value)->_get_delegate())->write(*this);
    return;
  }
  THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
}

//foreign virtual 
void 
CDRObjectWriter::write_abstract_interface(IN(::acdk::lang::RObject) obj)
{
  if (instanceof(obj, ::org::omg::CORBA::portable::ObjectImpl) == true)
    write_Object(::org::omg::CORBA::RObject(obj));
  else
    write_Object(new AcdkObject(obj));
}

  // 2.3
//virtual 
void 
CDRObjectWriter::write_value(IN(::acdk::io::RSerializable) value) 
{
  writeObject((RObject)value);
}


//virtual 
void 
CDRObjectWriter::writeObject(IN(RClass) cls, IN(RObject) obj)
{
  Object::_throwNotImplementedYet("CDRObjectWriter::writeObject(RClass cls, RObject obj))"); // ## implement 
}

void
CDRObjectWriter::writeObject(IN(RObject) obj)
{
  if (obj == Nil) {
    write_string("Nil");
    return;
  }
  RClass theClass = obj->getClass();
  //doen't, because reader doesn't expect meta info write_string(theClass->getName());
  ::acdk::lang::dmi::SysFields members = obj->getInternalFields(MiNonStatic);
  for (int i = 0; i < members.size(); i++) {
    ::acdk::lang::dmi::SysField& f = members[i];
    if (f.fieldInfo->flags & MiStatic)
      continue;
    switch (f.type) {
    case SysField::FT_Void: 
      continue;
    case SysField::FT_Bool: write_boolean(*f.cont.bval); break;
    case SysField::FT_Char : write_octet(*f.cont.cval);  break;
    case SysField::FT_UcChar : write_short(*f.cont.ucval);  break;
    case SysField::FT_Byte : write_octet(*f.cont.cval);  break;
    case SysField::FT_Short : write_short(*f.cont.sval);  break;
    case SysField::FT_Int : write_long((int)*f.cont.ival);  break;
    case SysField::FT_JLong : write_longlong(*f.cont.jlval);  break;
    case SysField::FT_Float : write_float(*f.cont.fval);  break;
    case SysField::FT_Double : write_float(*f.cont.dval);  break;
    case SysField::FT_Object : {
      RObject cobj = f.cont.oval->impl();
      if (instanceof(cobj, String) == true)
        write_string((RString)cobj);
      else  
        write_acdk_object(cobj);
      break;
    }
    }
  }
}

//foreign virtual 
void 
CDRObjectWriter::writeScriptVar(acdk::lang::dmi::ScriptVar& sv, bool withTypeInfo, bool withFlags)
{
  // ####

}

using ::acdk::lang::dmi::ScriptVar;
using ::org::omg::CORBA::portable::CallerWrite;
using ::org::omg::CORBA::portable::CalleeRead;
using ::org::omg::CORBA::portable::CalleeWrite;
using ::org::omg::CORBA::portable::CallerRead;

int scriptVarFlags2GiopFlags(int flags)
{
  int giopflags = 0;
  if (flags & MiAiByref)
    giopflags |= DmiGiopIsReference;
  if (flags & MiAiByval)
    giopflags |= DmiGiopIsSerialized;
  return giopflags;
}


//virtual 
void 
CDRObjectWriter::write_scriptVar(IN(ScriptVar) sv, ParamCallDirection dir)
{
  //ACDK_LOG(Debug, RString("scriptVar=[") +  sv.toCode() + "]");
  if (dir == CalleeWrite) {
    if ((sv.flags & MiAiOut) != MiAiOut)
      return;
  }
  write_long(sv.type);
  write_long(sv.flags);
  
  if (dir == CallerWrite) {
    if ((sv.flags & MiAiIn) != MiAiIn)
      return;
  }

  switch (sv.type)
  {
  case ScriptVar::UnknownType:

    break;
  case ScriptVar::BoolRefType:
  case ScriptVar::BoolType:
    write_boolean(sv.getBoolVar());
    break;
  case ScriptVar::CharRefType:
  case ScriptVar::CharType:
    write_char(sv.getCharVar());
    break;
  case ScriptVar::ByteRefType:
  case ScriptVar::ByteType:
    write_octet(sv.getCharVar());
    break;
  case ScriptVar::ShortRefType:
  case ScriptVar::ShortType:
    write_short(sv.getShortVar());
    break;
  case ScriptVar::IntRefType:
  case ScriptVar::IntType:
    write_long(sv.getIntVar());
    break;
  case ScriptVar::LongRefType:
  case ScriptVar::LongType: // i64
    write_longlong(sv.getLongVar());
    break;
  case ScriptVar::FloatRefType:  
  case ScriptVar::FloatType:
    write_float(sv.getFloatVar());
    break;
  case ScriptVar::DoubleRefType:
  case ScriptVar::DoubleType:
    write_double(sv.getDoubleVar());
    break;
  case ScriptVar::ObjectRefType:
  case ScriptVar::ObjectType:
  {
    write_fq_object(sv.getObjectVar(), scriptVarFlags2GiopFlags(sv.flags));
    break;
  }
  default:
    break;
  }
}

void 
CDRObjectWriter::write_fq_object(IN(RObject) obj, int flags)
{
  //ACDK_LOG(Debug, RString("fq_object=[") +  obj->toString() + "]");
  if (obj == Nil)
  {
    write_long(DmiGiopIsNil);
    return;
  }
  if (instanceof(obj, ::acdk::lang::String) == true)
  {
    write_long(DmiGiopIsString);
    write_string((RString)obj);
    return;
  }
  // ### wrong, using hint in flags
  if ((flags & DmiGiopIsReference) == 0 && 
      (flags & DmiGiopIsSerialized ||
      instanceof(obj, ::acdk::io::Serializable) == true || 
       instanceof(obj, ::acdk::lang::Throwable) ||
       obj->getClass()->isArray() == true)
      ) 
  {
    write_long(DmiGiopIsSerialized);
    write_string(obj->getClass()->getName());
    writeObject(obj);
    return;
  } 
  else 
  {
    write_long(DmiGiopIsReference);
    if (instanceof(obj, ::org::omg::CORBA::Object) == true) 
    {
      write_Object(::org::omg::CORBA::RObject(obj));
    } else {
      write_Object(new AcdkObject(obj));
    }
  }
}



} // namespace orb 
} // namespace acdkx 


