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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/acdk2orb.cpp,v 1.13 2005/04/13 16:54:08 kommer Exp $

#include "acdk2orb.h"
#include <org/omg/CORBA/Any.h>

namespace acdkx {
namespace orb {

USING_CLASS(::org::omg::CORBA::, Any);
USING_CLASS(::org::omg::CORBA::, TypeCode);

using ::org::omg::CORBA::TCKind;

using namespace acdk::lang::dmi;

bool hasFlags(int isFlags, int wantFlags)
{
  return wantFlags == 0 ||isFlags & wantFlags || (wantFlags == MiAiIn && (isFlags & MiAiOut) == 0);
}


void 
readScriptVarValue(::org::omg::CORBA::portable::InputStream& input, const ClazzMethodArgInfo* cmai, ScriptVar& arg)
{
  if (hasFlags(cmai->flags, MiAiIn) == false)
    return;
  if ((cmai->flags & MiAiByval) == 0)
  {
    return;
  } else if (cmai->type == ClazzInfo::getCharClazz()) {
    arg = input.read_char();
  } else if (cmai->type == ClazzInfo::getByteClazz()) {
    arg = input.read_char();
  } else if (cmai->type == ClazzInfo::getShortClazz()) {
    arg = input.read_short();
  } else if (cmai->type == ClazzInfo::getIntClazz()) {
    arg = input.read_long();
  } else if (cmai->type == ClazzInfo::getLongClazz()) {
    arg = input.read_longlong();
  } else if (cmai->type == ClazzInfo::getFloatClazz()) {
    arg = input.read_float();
  } else if (cmai->type == ClazzInfo::getDoubleClazz()) {
    arg = input.read_double();
  } else if (cmai->type == ClazzInfo::getBoolClazz()) {
    arg = input.read_boolean();
  } else if (cmai->type->isArray() == true) {
    // ### implement/test    
  } else if (cmai->type == String::clazzInfo()) {
    arg = (RObject)input.read_string();
  } else {
    arg = input.read_acdk_object(Class::getSingeltonClass(cmai->type));
  }
}

void 
readArgValues(::org::omg::CORBA::portable::InputStream& input, ScriptVarArray& args, const ClazzMethodInfo* cmi)
{
  for (int i = 0; cmi->methodArgs[i] != 0; ++i)
    readScriptVarValue(input, cmi->methodArgs[i], args[i]);
}


void writeValue(::org::omg::CORBA::portable::OutputStream& out, ScriptVar& erg)
{
  switch (erg.type)
  {
    
  case ScriptVar::BoolType: 
    out.write_boolean(erg.getBoolVar()); 
    break;
  case ScriptVar::CharType :
    out.write_char(erg.getCharVar());
    break;
  case ScriptVar::ByteType: 
    out.write_char(erg.getCharVar());
    break;
  case ScriptVar::ShortType:
    out.write_short(erg.getShortVar());
    break;
  case ScriptVar::IntType:
    out.write_long(erg.getIntVar());
    break;
  case ScriptVar::LongType:
    out.write_longlong(erg.getLongVar());
    break;
  case ScriptVar::FloatType:
    out.write_float(erg.getFloatVar());
    break;
  case ScriptVar::DoubleType:
    out.write_double(erg.getDoubleVar());
    break;
  case ScriptVar::ObjectType :
  {
    RObject obj = erg.getObjectVar();
    if (instanceof(obj, String) == true)
    {
      out.write_string((RString)obj);
    } else {
      out.write_acdk_object(obj);
    }
    break;
  }
  case ScriptVar::UnknownType : // void return type?
  default:
      return;
  }
}

/*

void 
readAny(::org::omg::CORBA::portable::InputStream& in, ScriptVar& arg)
{
  org::omg::CORBA::TCKind tckind = (org::omg::CORBA::TCKind)in.read_long();
  switch(tckind)
  {
  case org::omg::CORBA::tk_null:
    arg = RObject(Nil);
    break;
  case org::omg::CORBA::tk_void:
    break;
  case org::omg::CORBA::tk_short:
    arg = in.read_short();
    break;
  case org::omg::CORBA::tk_long:
    arg = (int)in.read_long();
    break;
  case org::omg::CORBA::tk_ushort:
    arg = (short)in.read_ushort();
    break;
  case org::omg::CORBA::tk_ulong:
    arg = (int)in.read_ulong();
    break;
  case org::omg::CORBA::tk_float:
    arg = in.read_float();
    break;
  case org::omg::CORBA::tk_double:
    arg = in.read_double();
    break;
  case org::omg::CORBA::tk_boolean:
    arg = in.read_boolean();
    break;
  case org::omg::CORBA::tk_char:
    arg = in.read_char();
    break;
  case org::omg::CORBA::tk_octet:
    arg = (byte)in.read_octet();
    break;
  case org::omg::CORBA::tk_any:
    readArgFromAny(in, arg);
    break;
  case org::omg::CORBA::tk_TypeCode:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_TypeCode");
    break;
  case org::omg::CORBA::tk_Principal:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_Principal");
    break;
  case org::omg::CORBA::tk_objref:
    // ### implement
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_objref");
  case org::omg::CORBA::tk_struct:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_struct");
  case org::omg::CORBA::tk_union:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_union");
  case org::omg::CORBA::tk_enum:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_enum");
  case org::omg::CORBA::tk_string:
    arg = (RObject)in.read_string();
    break;
  case org::omg::CORBA::tk_sequence:
    // ### implement
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_sequence");
  case org::omg::CORBA::tk_array:
    // ### implement
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_array");
  case org::omg::CORBA::tk_alias:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_alias");
  case org::omg::CORBA::tk_except:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_except");
  case org::omg::CORBA::tk_longlong:
    arg = (jlong) in.read_longlong();
    break;
  case org::omg::CORBA::tk_ulonglong:
    arg = (jlong) in.read_ulonglong();
    break;
  case org::omg::CORBA::tk_longdouble:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_longdouble");
  case org::omg::CORBA::tk_wchar:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_wchar");
  case org::omg::CORBA::tk_wstring:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_wstring");
  case org::omg::CORBA::tk_fixed:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_fixed");
  case org::omg::CORBA::tk_value:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_value");
  case org::omg::CORBA::tk_value_box:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_value_box");
  case org::omg::CORBA::tk_native:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_native");
  case org::omg::CORBA::tk_abstract_interface:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::tk_abstract_interface");
  default:
    THROW1(DmiTypeConversionException, "Don't know how to convert ::org::omg::CORBA::TCKind::<unknown>");
    break;
 }
}

*/

ScriptVar readAny(::org::omg::CORBA::portable::InputStream& in)
{
  int type = in.read_long();
  Any any;
  TypeCode tc((TCKind)type);
  any.read_value(&in, &tc);
  return any.scriptVar();
}

void readAnys(::org::omg::CORBA::portable::InputStream& input, ScriptVarArray& args)
{
  for (int i = 0; i < args.size(); ++i)
  {
    args[i] = readAny(input);
  }
}
void writeScriptVar(::org::omg::CORBA::portable::OutputStream& out, const ScriptVar& arg)
{
  Any(arg).write_value(&out);
}



ScriptVar readAnyParam(::org::omg::CORBA::portable::InputStream& in, const ClazzMethodArgInfo* ai, int flags)
{
  
  if (hasFlags(ai->flags, flags ) == true)
  {
    return readAny(in);
  } else {
    return ScriptVar();
  }
}


void readAnyParams(::org::omg::CORBA::portable::InputStream& in, const ClazzMethodInfo* cmi, ScriptVarArray& args, int flags)
{

  int length = in.read_long();
  
  int i;
  for (i = 0; cmi->methodArgs[i] != 0; ++i)
  {
    args.push_back(readAnyParam(in, cmi->methodArgs[i], flags));
  }
  if (i > length)
    ; // ## error
}


void writeAnyParam(::org::omg::CORBA::portable::OutputStream& out, const ScriptVar& arg, const ClazzMethodArgInfo* ai, int flags)
{
  if (hasFlags(ai->flags, flags) == true)
  {
    writeScriptVar(out, arg);
  }
}


void writeAnyParams(::org::omg::CORBA::portable::OutputStream& out,  const ClazzMethodInfo* cmi, const ScriptVarArray& args, int flags)
{
  int count = 0;
  int i = 0;
  for (i = 0; cmi->methodArgs[i] != 0; ++i)
  {
    if (flags == 0 || cmi->methodArgs[i]->flags & flags)
      ++count;
  }
  out.write_long(count);
  for (i = 0; cmi->methodArgs[i] != 0; ++i)
  {
    writeAnyParam(out, args[i], cmi->methodArgs[i], flags);
  }
}

void writeValueReturn(::org::omg::CORBA::portable::OutputStream& out, const ClazzMethodInfo* cmi, ScriptVar& arg)
{
  if (cmi->returnType == ClazzInfo::getVoidClazz())
    return;
  if (arg.type == ScriptVar::UnknownType)
    return;
  writeValueParam(out, arg, cmi->returnType, 0, 0);
}

bool readValueParam(::org::omg::CORBA::portable::InputStream& input, ScriptVar& arg, 
                const acdk::lang::dmi::ClazzInfo* type, int isflags, int wantflags)
{
  if (hasFlags(isflags, wantflags) == false)
    return false;
  
  if (type == ClazzInfo::getVoidClazz()) 
    return false;
  if (type == ClazzInfo::getCharClazz()) {
    arg = input.read_char();
  } else if (type == ClazzInfo::getByteClazz()) {
    arg = input.read_char();
  } else if (type == ClazzInfo::getShortClazz()) {
    arg = input.read_short();
  } else if (type == ClazzInfo::getIntClazz()) {
    arg = input.read_long();
  } else if (type == ClazzInfo::getLongClazz()) {
    arg = input.read_longlong();
  } else if (type == ClazzInfo::getFloatClazz()) {
    arg = input.read_float();
  } else if (type == ClazzInfo::getDoubleClazz()) {
    arg = input.read_double();
  } else if (type == ClazzInfo::getBoolClazz()) {
    arg = input.read_boolean();
    
  } else if (type == String::clazzInfo()) {
    arg = (RObject)input.read_string();
  } else {
    arg = input.read_acdk_object(Class::getSingeltonClass(type));
  }
  return true;
}

void writeValueParam(::org::omg::CORBA::portable::OutputStream& out, ScriptVar& arg, 
                const acdk::lang::dmi::ClazzInfo* type, int isflags, int wantflags)
{
  if (hasFlags(isflags, wantflags) == false)
    return;
  
  if (type == ClazzInfo::getVoidClazz()) 
    return;
  if (type == ClazzInfo::getCharClazz()) {
    out.write_char(arg);
  } else if (type == ClazzInfo::getByteClazz()) {
    out.write_char(arg);
  } else if (type == ClazzInfo::getShortClazz()) {
    out.write_short(arg);
  } else if (type == ClazzInfo::getIntClazz()) {
    out.write_long(arg);
  } else if (type == ClazzInfo::getLongClazz()) {
    out.write_longlong(arg);
  } else if (type == ClazzInfo::getFloatClazz()) {
    out.write_float(arg);
  } else if (type == ClazzInfo::getDoubleClazz()) {
    out.write_double(arg);
  } else if (type == ClazzInfo::getBoolClazz()) {
    out.write_boolean(arg);
  
  } else if (type == String::clazzInfo()) {
     out.write_string((RString)arg);
  } else {
    out.write_acdk_object(arg.getObjectVar());
  }
}

void writeAnyParam(::org::omg::CORBA::portable::OutputStream& out, const ScriptVar& sv)
{
  ::org::omg::CORBA::Any(sv).write_value(&out);
}
void writeAnyParam(::org::omg::CORBA::portable::OutputStream& out, IN(RDmiObject) obj)
{
  writeAnyParam(out, *obj);
  //::org::omg::CORBA::Any(*obj).write_value(out);
}

void writeAnyParams(::org::omg::CORBA::portable::OutputStream& out, IN(RDmiObjectArray) inp)
{
  out.write_long(inp->length());
  for (int i = 0; i < inp->length(); ++i)
  {
    writeAnyParam(out, inp[i]);
  }
}


void readAnyParam(::org::omg::CORBA::portable::InputStream& in, OUT(RDmiObject) dmiobj)
{
  dmiobj = new DmiObject(readAny(in));
}

void readAnyParams(::org::omg::CORBA::portable::InputStream& in, OUT(RDmiObjectArray) outp)
{
  int len = in.read_long();
  outp = new DmiObjectArray(len);
  for (int i = 0; i < len; ++i)
  {
    RDmiObject dmiobj;
    readAnyParam(in, dmiobj);
    outp[i] = dmiobj;
  }
}

void readAnyParams(::org::omg::CORBA::portable::InputStream& in, ScriptVarArray& args)
{
  int argnum = in.read_long();
  for (int i = 0; i < argnum; ++i)
  {
    args.push_back(readAny(in));
  }
}


} // namespace acdkx
} // namespace orb



