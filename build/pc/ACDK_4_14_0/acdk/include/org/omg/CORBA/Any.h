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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/Any.h,v 1.10 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_Any_h
#define org_omg_CORBA_Any_h

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/io/Serializable.h>

#include "CORBA.h"
#include "Object.h"
#include "TypeCode.h"
#include <org/omg/CORBA/portable/InputStream.h>
#include <org/omg/CORBA/portable/OutputStream.h>

namespace org {
namespace omg {
namespace CORBA {

enum TCKind;

using ::acdk::lang::dmi::ScriptVar;

ACDK_DECL_CLASS(Any);

class ACDKX_ORB_PUBLIC Any
: extends ::acdk::lang::Object,
  implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Any)
private:
  TCKind _type;
  ScriptVar _value;
public:
  Any() : _type(tk_null) { }
  Any(const ScriptVar& v);
  
  ScriptVar& scriptVar() { return _value; }
  TCKind tckind() { return _type; }
  //RInputStream create_input_stream() 
  //OutputStream create_output_stream() 
  
  bool equal(IN(RAny) a);
  RAny extract_any();
  bool extract_boolean();
  char extract_char();
  

  double extract_double();
  
  float extract_float();
  int extract_long();
  jlong extract_longlong();
  ::org::omg::CORBA::RObject extract_Object();
  byte extract_octet();
  short extract_short();
  RString extract_string();
  RTypeCode extract_TypeCode();
  int extract_ulong();
  jlong extract_ulonglong();
  short extract_ushort();
  ::acdk::io::RSerializable extract_Value();
  //char extract_wchar() 
  //String extract_wstring() 
  void insert_any(IN(RAny) a);
  void insert_boolean(bool b);
  void insert_char(char c);
  void insert_double(double d);
  //void insert_fixed(BigDecimal value) 
  //void insert_fixed(BigDecimal value, TypeCode type) 
  void insert_float(float f);
  void insert_long(int l);
  void insert_longlong(jlong l);
  void insert_Object(IN(::org::omg::CORBA::RObject) o);
  void insert_Object(IN(::org::omg::CORBA::RObject) o, IN(RTypeCode) t);
  void insert_octet(byte b);
  void insert_short(short s);
  //void insert_Streamable(RStreamable s);
  void insert_string(IN(RString) s);
  void insert_TypeCode(IN(RTypeCode) t);
  void insert_ulong(int l);
  void insert_ulonglong(jlong l);
  void insert_ushort(short s);
  void insert_Value(IN(::acdk::io::RSerializable) v);
  void insert_Value(IN(::acdk::io::RSerializable) v, IN(RTypeCode) t);
  //void insert_wchar(char c) 
  //void insert_wstring(String s) 
  void read_value(IN(::org::omg::CORBA::portable::RInputStream) is, IN(RTypeCode) t);
  RTypeCode type();
  void type(IN(RTypeCode) t);
  void write_value(IN(::org::omg::CORBA::portable::ROutputStream) os);

  static TCKind scriptVarType2TCKind(const ScriptVar& sv);
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_Any_h
