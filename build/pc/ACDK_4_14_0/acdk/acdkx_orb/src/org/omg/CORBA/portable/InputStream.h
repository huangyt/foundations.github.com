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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/InputStream.h,v 1.15 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_InputStream_h
#define org_omg_CORBA_portable_InputStream_h

#include "../CORBA.h"
#include "../ORB.h"

namespace org {
namespace omg {
namespace CORBA {
namespace portable {

ACDK_DECL_INTERFACE(InputStream);

enum Endian
{
  LittleEndian,
  BigEndian
};

enum ParamCallDirection
{
	CallerWrite,
	CalleeRead,
	CalleeWrite,
	CallerRead
};


#if defined(__SPARC__)
const Endian naturualEndian = BigEndian;
#else 
const Endian naturualEndian = LittleEndian;
#endif 

class ACDKX_ORB_PUBLIC InputStream
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(InputStream)

  
protected:
  Endian _endian;
public:
  InputStream() 
  : _endian(LittleEndian) 
  {
  }
  Endian endian() { return _endian; }
  void setEndian(Endian en) { _endian = en; }
  virtual org::omg::CORBA::RORB orb() = 0;
  virtual bool read_boolean() = 0;
  virtual char read_char() = 0;
  virtual char read_wchar() = 0;
  virtual byte read_octet() = 0;
  virtual short read_short() = 0;
  virtual short read_ushort() = 0;
  virtual int read_long() = 0;
  virtual int read_ulong() = 0;
  virtual jlong read_longlong() = 0;
  virtual long read_ulonglong() = 0;
  virtual float read_float() = 0;
  virtual double read_double() = 0;
  virtual RString read_string() = 0;
  virtual void read_char_array(char* value, int offset, int length) = 0;
  
  /**
    try figure out themselves, which method applies.
    If instanceOf ::acdk::io::Serializable -> read_value
    If instanceOf ::org::omg::CORBA::Object -> read_Object

  */
  virtual ::acdk::lang::RObject read_acdk_object(IN(::acdk::lang::RClass) clz) = 0;

  virtual ::org::omg::CORBA::RObject read_Object() = 0;
  virtual ::org::omg::CORBA::RObject read_Object(IN(::acdk::lang::RClass) clz)
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }

  virtual ::acdk::lang::RObject read_abstract_interface() 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }
  virtual ::acdk::lang::RObject read_abstract_interface(IN(::acdk::lang::RClass) clz) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }
  virtual ::acdk::lang::RObject read_value() 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }
  virtual ::acdk::lang::RObject read_value(IN(::acdk::lang::RString) rep_id) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }
  virtual ::acdk::lang::RObject read_value(IN(::acdk::lang::RClass) clz) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }
  /*
  virtual ::acdk::lang::RObject read_value(org.omg.CORBA.BoxedValueHelper factory) {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  */
  virtual ::acdk::lang::RObject read_value(IN(::acdk::lang::RObject) value) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil; // not reachable
  }
  virtual void read_scriptVar(OUT(::acdk::lang::dmi::ScriptVar) sv, ParamCallDirection dir)
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  /**
    reads DmiGiopArgFlags and string, serialized or referenced object
    @param flags see Modifier
                 May contain ByVal
  */
  virtual ::acdk::lang::RObject read_fq_object(int flags)
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
     return Nil; // not reachable
  }
  /*
  virtual String read_wstring();
  
  virtual void read_boolean_array(boolean[] value, int offset, int length);
  
  virtual voidread_wchar_array(char[] value, int offset, int length);
  virtual voidread_octet_array(byte[] value,int offset, int length);
  virtual voidread_short_array(short[] value, int offset, int length);
  virtual voidread_ushort_array(short[] value, int offset, int length);
  virtual voidread_long_array(int[] value, int offset, int length);
  virtual voidread_ulong_array(int[] value, int offset, int length);
  virtual voidread_longlong_array(long[] value, int offset, int length);
  virtual voidread_ulonglong_array(long[] value, int offset, int length);
  virtual voidread_float_array(float[] value, int offset, int length);
  virtual voidread_double_array(double[] value, int offset, int length);
  
  virtual org.omg.CORBA.TypeCode read_TypeCode();
  virtual org.omg.CORBA.Any read_any();
  virtual org.omg.CORBA.Context read_Context() {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  virtual java.math.BigDecimal read_fixed() {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  */
};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

// 
#define CDRSWAP2(value) \
do { \
  if (_endian != org::omg::CORBA::portable::naturualEndian) { \
    short& v = *(short*)&value; \
    v = (v << 8) | (v >> 8); \
  } \
} while (false) 

#define CDRSWAP4(value) \
do { \
  if (_endian != org::omg::CORBA::portable::naturualEndian) { \
    int& v = *(int*)&value; \
    v = ((v << 24) | ((v & 0xff00) << 8) | ((v >> 8) & 0xff00) | (v >> 24)); \
  } \
} while (false)

#define CDRSWAP8(value) \
do { \
  if (_endian != org::omg::CORBA::portable::naturualEndian) { \
  	jlong& v = *(jlong*)&value; \
    int v0 = ((int*)&v)[0]; \
	  int v1 = ((int*)&v)[1]; \
    v0 = ((v0 << 24) | ((v0 & 0xff00) << 8) | ((v0 >> 8) & 0xff00) | (v0 >> 24)); \
    v1 = ((v1 << 24) | ((v1 & 0xff00) << 8) | ((v1 >> 8) & 0xff00) | (v1 >> 24)); \
	  ((int*)&v)[0] = v1; \
	  ((int*)&v)[1] = v0; \
  } \
} while (false)

#endif //org_omg_CORBA_portable_InputStream_h
