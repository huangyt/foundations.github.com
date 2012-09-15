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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/OutputStream.h,v 1.12 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_OutputStream_h
#define org_omg_CORBA_portable_OutputStream_h


#include "../ORB.h"
// needed for Endian
#include "InputStream.h" 

namespace org {
namespace omg {
namespace CORBA {
namespace portable {

ACDK_DECL_INTERFACE(OutputStream);
ACDK_DECL_INTERFACE(InputStream);

class ACDKX_ORB_PUBLIC OutputStream
      ACDK_INTERFACEBASE

{
  ACDK_WITH_METAINFO(OutputStream)
protected:
  Endian _endian;
public:
  OutputStream(Endian endianess = naturualEndian) 
    : _endian(endianess)
  {
  }
  Endian endian() { return _endian; }
  void setEndian(Endian en) { _endian = en; }
  
  virtual ::org::omg::CORBA::RORB orb() = 0;
  virtual RInputStream create_input_stream() = 0;
  virtual void write_boolean (bool value) = 0;
  virtual void write_char (char value) = 0;
  virtual void write_wchar(char value) = 0;
  virtual void write_octet (byte value) = 0;
  virtual void write_short (short value) = 0;
  virtual void write_ushort (short value) = 0;
  virtual void write_long (int value) = 0;
  virtual void write_ulong (int value) = 0;
  virtual void write_longlong (jlong value) = 0;
  virtual void write_ulonglong(jlong value) = 0;
  virtual void write_float (float value) = 0;
  virtual void write_double (double value) = 0;
  virtual void write_string (IN(RString) value) = 0;
  virtual void write_wstring (IN(RString) value) = 0;
  virtual void write_char_array (const char* value, int offset, int length) = 0;

  /**
    acdkx_orb Extended
    try figure out themselves, which method applies
     If instanceOf ::acdk::io::Serializable -> write_value
    If instanceOf ::org::omg::CORBA::Object -> write_Object
  */
  virtual void write_acdk_object(IN(::acdk::lang::RObject) value) = 0;
  

  /** acdkx_orb Extended write object as value  */
  virtual void write_Object(IN(::org::omg::CORBA::RObject) value) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  /** writes an exception (Application and SystemExceptions); */
  virtual void write_exception(IN(RThrowable) ex)
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  // 2.3
  virtual void write_value(IN(::acdk::io::RSerializable) value) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  virtual void write_value(IN(::acdk::io::RSerializable) value, ::acdk::lang::RString rep_id) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  virtual void write_value(IN(::acdk::io::RSerializable) value, ::acdk::lang::RClass clz) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  /*
  virtual void write_value(::acdk::io::RSerializable value, org.omg.CORBA.BoxedValueHelper factory) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  */
  virtual void write_abstract_interface(IN(::acdk::lang::RObject) obj) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  /**
    writes a ScriptVar for the DMI over IIOP mapping.
    The format should include:<br>
    <li> ScriptVar::flags (see acdk::lang::reflect::Modifier
    <li> ScriptVar::type 
    <li> Value of ScriptVar
  */
  virtual void write_scriptVar(IN(::acdk::lang::dmi::ScriptVar) sv, ParamCallDirection dir)
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  /** 
    writes a serialized, string or reference 
  */
  virtual void write_fq_object(IN(::acdk::lang::RObject) obj, int flags)
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  /*
    these are not supported yet
  virtual void write_boolean_array(boolean[] value, int offset, int length);
  virtual void write_wchar_array (char[] value, int offset, int length);
  virtual void write_octet_array (byte[] value, int offset, int length);
  virtual void write_short_array (short[] value, int offset, int length);
  virtual void write_ushort_array (short[] value, int offset, int length);
  virtual void write_long_array (int[] value, int offset, int length);
  virtual void write_ulong_array (int[] value, int offset, int length);
  virtual void write_longlong_array (long[] value, int offset, int length);
  virtual void write_ulonglong_array(long[] value, int offset, int length); virtual void write_float_array (float[] value, int offset, int length);
  virtual void write_double_array(double[] value, int offset, int length);
  virtual void write_Object(org.omg.CORBA.Object value);
  virtual void write_TypeCode(org.omg.CORBA.TypeCode value);
  virtual void write_any (org.omg.CORBA.Any value);
  virtual void write_Context(org.omg.CORBA.Context ctx, org.omg.CORBA.ContextLists contexts);

  virtual void write_fixed(java.math.BigDecimal value)
  */


};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_portable_OutputStream_h
