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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/Any.cpp,v 1.10 2005/02/05 10:45:40 kommer Exp $

#include "CORBA.h"
#include "OrbExceptions.h"

#include "Any.h"

namespace org {
namespace omg {
namespace CORBA {


Any::Any(const ScriptVar& v)
: _type(scriptVarType2TCKind(v))
, _value(v)
{
}

//static 
TCKind 
Any::scriptVarType2TCKind(const ScriptVar& sv)
{
  switch (sv.type)
  {
  case ScriptVar::UnknownType:
    return tk_void;
  case ScriptVar::BoolRefType:
  case ScriptVar::BoolType:
    return tk_boolean;
  case ScriptVar::CharRefType:
  case ScriptVar::CharType:
    return tk_char;
  case ScriptVar::ByteRefType:
  case ScriptVar::ByteType:
    return tk_octet;
  case ScriptVar::ShortRefType:
  case ScriptVar::ShortType:
    return tk_short;
  case ScriptVar::IntRefType:
  case ScriptVar::IntType:
    return tk_long;
  case ScriptVar::LongRefType:
  case ScriptVar::LongType:
    return tk_longlong;
  case ScriptVar::FloatRefType:
  case ScriptVar::FloatType:
    return tk_float;
  case ScriptVar::DoubleRefType:
  case ScriptVar::DoubleType:
    return tk_double;
  case ScriptVar::ObjectRefType:
  case ScriptVar::ObjectType:
  {
    ::acdk::lang::RObject obj = sv.getObjectVar();
    if (obj == Nil)
      return tk_null;
    if (instanceof(obj, String) == true)
      return tk_string;
    return tk_abstract_interface;
  }
  default:
    return tk_void;
  }
}

bool 
Any::equal(IN(RAny) a)
{
  if (a->type() != type())
    return false;
  return _value.equal(a->scriptVar()).getBoolVar();
}

RAny 
Any::extract_any()
{
  if (_type != tk_any)
    THROW0(BAD_OPERATION);
  ::acdk::lang::RObject ret = _value.getObjectVar();
  if (instanceof(ret, Any) == false)
    THROW0(BAD_OPERATION);
  return (RAny)ret;
}

bool 
Any::extract_boolean()
{
  if (_type != tk_boolean)
    THROW0(BAD_OPERATION);
  return _value.getBoolVar();
}

char 
Any::extract_char()
{
  if (_type != tk_char)
    THROW0(BAD_OPERATION);
  return _value.getCharVar();
}
double 
Any::extract_double() 
{
  if (_type != tk_double)
    THROW0(BAD_OPERATION);
  return _value.getDoubleVar();
}

  
float 
Any::extract_float()
{
  if (_type != tk_float)
    THROW0(BAD_OPERATION);
  return _value.getFloatVar();
}

int 
Any::extract_long() 
{
  if (_type != tk_long)
    THROW0(BAD_OPERATION);
  return _value.getIntVar();
}

jlong 
Any::extract_longlong() 
{
  if (_type != tk_longlong)
    THROW0(BAD_OPERATION);
  return _value.getLongVar();
}

org::omg::CORBA::RObject 
Any::extract_Object() 
{
  if (_type != tk_long)
    THROW0(BAD_OPERATION);
  ::acdk::lang::RObject obj = _value.getObjectVar();
  if (instanceof(obj, ::org::omg::CORBA::Object) == false)
    THROW0(BAD_OPERATION);
  return org::omg::CORBA::RObject(obj);
}

byte 
Any::extract_octet() 
{
  if (_type != tk_octet)
    THROW0(BAD_OPERATION);
  return _value.getByteVar();
}

short 
Any::extract_short() 
{
  if (_type != tk_short)
    THROW0(BAD_OPERATION);
  return _value.getShortVar();
}

RString 
Any::extract_string() 
{
  if (_type != tk_string)
    THROW0(BAD_OPERATION);
  ::acdk::lang::RObject obj = _value.getObjectVar();
  if (instanceof(obj, String) == false)
    THROW0(BAD_OPERATION);
  return RString(obj);
}

RTypeCode 
Any::extract_TypeCode() 
{
  if (_type != tk_TypeCode)
    THROW0(BAD_OPERATION);
  ::acdk::lang::RObject obj = _value.getObjectVar();
  if (instanceof(obj, RTypeCode) == false)
    THROW0(BAD_OPERATION);
  return RTypeCode(obj);
}

int 
Any::extract_ulong() 
{
  if (_type != tk_ulong)
    THROW0(BAD_OPERATION);
  return _value.getIntVar();
}

jlong 
Any::extract_ulonglong() 
{
  if (_type != tk_ulonglong)
    THROW0(BAD_OPERATION);
  return _value.getLongVar();
}

short 
Any::extract_ushort() 
{
  if (_type != tk_ushort)
    THROW0(BAD_OPERATION);
  return _value.getShortVar();
}

::acdk::io::RSerializable 
Any::extract_Value() 
{
  if (_type != tk_value)
    THROW0(BAD_OPERATION);
  ::acdk::lang::RObject obj = _value.getObjectVar();
  if (instanceof(obj, ::acdk::io::Serializable) == false)
    THROW0(BAD_OPERATION);
  return ::acdk::io::RSerializable(obj);
}

void 
Any::insert_any(IN(RAny) a) 
{
  _type = tk_any;
  _value = (::acdk::lang::RObject)a;
}

void 
Any::insert_boolean(bool b) 
{
  _type = tk_boolean;
  _value = b;
}

void 
Any::insert_char(char c) 
{
  _type = tk_char;
  _value = c;
}
void 
Any::insert_double(double d) 
{
  _type = tk_double;
  _value = d;
}

void 
Any::insert_float(float f) 
{
  _type = tk_float;
  _value = f;
}

void 
Any::insert_long(int l) 
{
  _type = tk_long;
  _value = l;
}

void 
Any::insert_longlong(jlong l) 
{
  _type = tk_longlong;
  _value = l;
}

void 
Any::insert_Object(IN(::org::omg::CORBA::RObject) o) 
{
  _type = tk_objref;
  _value = ::acdk::lang::RObject(o);
}

void 
Any::insert_Object(IN(::org::omg::CORBA::RObject) o, IN(RTypeCode) t)
{
  _type = t->kind();
  _value = ::acdk::lang::RObject(o);
}

void 
Any::insert_octet(byte b) 
{
  _type = tk_octet;
  _value = b;
}

void 
Any::insert_short(short s) 
{
  _type = tk_short;
  _value = s;
}
/*
void 
Any::insert_Streamable(RStreamable s) 
{
  _type = tk_streamable;
  _value = ::acdk::lang::RObject(s);
}*/
  
void 
Any::insert_string(IN(RString) s) 
{
  _type = tk_string;
  _value = ::acdk::lang::RObject(s);
}


void 
Any::insert_TypeCode(IN(RTypeCode) t) 
{
  _type = t->kind();
  _value = 0;
}

void 
Any::insert_ulong(int l) 
{
  _type = tk_ulong;
  _value = l;
}

void 
Any::insert_ulonglong(jlong l) 
{
  _type = tk_ulonglong;
  _value = l;
}

void 
Any::insert_ushort(short s) 
{
  _type = tk_ushort;
  _value = s;
}

void 
Any::insert_Value(IN(::acdk::io::RSerializable) v) 
{
  _type = tk_value;
  _value = (::acdk::lang::RObject)v;
}

void 
Any::insert_Value(IN(::acdk::io::RSerializable) v, IN(RTypeCode) t) 
{
  _type = t->kind();
  _value = (::acdk::lang::RObject)v;
}

void 
Any::read_value(IN(::org::omg::CORBA::portable::RInputStream) is, IN(RTypeCode) t) 
{
  _type = t->kind();
  switch(_type) 
  {
  case tk_null:
    _value = Nil;
    break;
  case tk_void :
    _value = Nil;
    break;
  case tk_ushort:
  case tk_short:
    _value = is->read_short();
    break;
  case tk_ulong:
  case tk_long:
    _value = is->read_long();
    break;
  case tk_float:
    _value = is->read_float();
    break;
  case tk_double:
    _value = is->read_double();
    break;
  case tk_boolean:
    _value = is->read_boolean();
    break;
  case tk_char:
    _value = is->read_char();
    break;
  case tk_octet:
    _value = is->read_octet();
    break;
  case tk_longlong:
  case tk_ulonglong:
    _value = is->read_longlong();
    break;
  case tk_any: 
  {
    RAny sany = (RAny)is->read_acdk_object(GetClass());
    _value = (::acdk::lang::RObject)sany;
    break;
  }
  case tk_native:
  case tk_value:
  case tk_union:
  case tk_enum:
  case tk_struct:
  case tk_sequence:
    THROW0(MARSHAL);
    break;
  case tk_string:
    _value = (::acdk::lang::RObject)is->read_string();
    break;
  case tk_except:
    _value = (::acdk::lang::RObject)is->read_acdk_object(::org::omg::CORBA::SystemException::GetClass());
    break;
  case tk_abstract_interface:
  case tk_objref:
    _value = (::acdk::lang::RObject)is->read_abstract_interface();
    break;
  case tk_TypeCode :
  case tk_Principal :
  case tk_wchar :
  case tk_wstring :
  case tk_array:
  case tk_alias :
  case tk_longdouble:
  case tk_fixed :
  case tk_value_box :
    THROW0(MARSHAL);
    break;
  
  }
}

RTypeCode 
Any::type() 
{
  return new TypeCode(_type);
}

void 
Any::type(IN(RTypeCode) t) 
{
  _type = t->kind();
  _value = 0;
}
  
void 
Any::write_value(IN(::org::omg::CORBA::portable::ROutputStream) os) 
{
  os->write_long(_type);
  switch (_type) 
  {
  case tk_null:
    os->write_value(Nil);
    break;
  case tk_void :
    break;
  case tk_ushort:
  case tk_short:
    os->write_short(_value.getShortVar());
    break;
  case tk_ulong:
  case tk_long:
    os->write_long(_value.getIntVar());
    break;
  case tk_float:
    os->write_float(_value.getFloatVar());
    break;
  case tk_double:
    os->write_double(_value.getDoubleVar());
    break;
  case tk_boolean:
    os->write_boolean(_value.getBoolVar());
    break;
  case tk_char:
    os->write_char(_value.getCharVar());
    break;
  case tk_octet:
    os->write_octet(_value.getByteVar());
    break;
  case tk_longlong:
  case tk_ulonglong:
    os->write_longlong(_value.getLongVar());
    break;
  case tk_any: 
  {
    RAny sany = (RAny)_value.getObjectVar();
    sany->write_value(os);
    break;
  }
  case tk_string:
    os->write_string((RString)_value.getObjectVar());
    break;
  case tk_native:
  case tk_value:
  case tk_union:
  case tk_enum:
  case tk_struct:
  case tk_sequence:
    THROW0(MARSHAL);
  case tk_except:
  case tk_objref:
  case tk_abstract_interface:
    os->write_abstract_interface(_value.getObjectVar());
    break;
  case tk_TypeCode :
  case tk_Principal :
  case tk_wchar :
  case tk_wstring :
  case tk_array:
  case tk_alias :
  case tk_longdouble:
  case tk_fixed :
  case tk_value_box :
    THROW0(MARSHAL);
    break;
  
  }
}
 


} // namespace CORBA
} // namespace omg
} // namespace org 


