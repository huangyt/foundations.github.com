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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CDRObjectWriter.h,v 1.18 2005/03/14 17:59:13 kommer Exp $

#ifndef acdkx_orb_CDRObjectWriter_h
#define acdkx_orb_CDRObjectWriter_h

#include <acdk.h>
#include <acdk/io/ObjectWriter.h>
#include <acdk/io/AbstractFilterWriter.h>
#include "orb.h"
#include <org/omg/CORBA/portable/OutputStream.h>
#include <org/omg/CORBA/portable/InputStream.h>

namespace acdkx {
namespace orb {

using ::org::omg::CORBA::portable::ParamCallDirection;

ACDK_DECL_CLASS(CDRObjectWriter);

USING_CLASS(::acdk::io::, Writer);
USING_CLASS(::acdk::io::, DataWriter);

USING_CLASS(::org::omg::CORBA::portable::, InputStream);
USING_CLASS(::org::omg::CORBA::portable::, OutputStream);

class ACDKX_ORB_PUBLIC CDRObjectWriter
: extends ::acdk::io::AbstractFilterWriter,
  implements ::acdk::io::ObjectWriter,
  implements ::org::omg::CORBA::portable::OutputStream
{
  ACDK_WITH_METAINFO(CDRObjectWriter)
private:
  int _index;
protected:
  org::omg::CORBA::RORB _orb;
public:
  CDRObjectWriter(IN(RWriter) out, IN(org::omg::CORBA::RORB) orb);
  foreign virtual void flush() {   ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterWriter)::flush(); }
  foreign virtual void setOut(IN(RWriter) writer) { ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterWriter)::setOut(writer); }
  foreign virtual ::acdk::io::RStorage getStorage() { return ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterWriter)::getStorage(); }
  foreign virtual ::acdk::io::RWriter getStorageWriter() { return ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterWriter)::getStorageWriter(); }
  
  // acdk::io::DataWriter
  foreign virtual void write(const byte* cstr, int offset, int len);
  foreign virtual void write(byte c);
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);

  // acdk::io::DataWriter
  foreign virtual void writeBoolean(bool b);
  foreign virtual void writeChar(char b);
  foreign virtual void writeUcChar(uc2char b);
  foreign virtual void writeShort(short b);
  foreign virtual void writeInt(int b);
  foreign virtual void writeLong(jlong b);
  foreign virtual void writeFloat(float b);
  foreign virtual void writeDouble(double b);
  //virtual void write(RbyteArray array, int offset = 0, int len = -1);
  foreign virtual void writeString(IN(RString) str);
  // acdk::io::ObjectWriter
  foreign virtual void writeObject(IN(RObject) obj);
  foreign virtual void writeObject(IN(RClass) cls, IN(RObject) obj);
  /** not supported */
  foreign virtual void defaultWriteObject(IN(RClass) cls, IN(RObject) obj)
  {
    THROW0(UnsupportedOperationException);
  }
  /** not supported */
  virtual void writeUnshared(IN(RClass) cls, IN(RObject) obj) 
  {
     THROW0(UnsupportedOperationException);
  }
  /** not supported */
  virtual void writeClassDescriptor(IN(RClass) cls, IN(RObject) obj)
  {
     THROW0(UnsupportedOperationException);
  }


  foreign virtual void writeScriptVar(acdk::lang::dmi::ScriptVar& sv, bool withTypeInfo = true, bool withFlags = true);
  // org::omg::CORBA::portable::OutputStream
  virtual org::omg::CORBA::RORB orb() { return _orb; }
  RInputStream create_input_stream();

  foreign virtual void write_boolean(bool value) { writeBoolean(value); }
  foreign virtual void write_char(char value) { writeChar(value); }
  foreign virtual void write_wchar(char value) { writeChar(value); }
  foreign virtual void write_octet(byte value) { writeChar(value); }
  foreign virtual void write_short(short value) { writeShort(value); }
  foreign virtual void write_ushort(short value) { writeShort(value); }
  foreign virtual void write_long(int value) { writeInt(value); }
  foreign virtual void write_ulong(int value) { writeInt(value); }
  foreign virtual void write_longlong (jlong value) { writeLong(value); }
  foreign virtual void write_ulonglong(jlong value) { writeLong(value); }
  foreign virtual void write_float(float value) { writeFloat(value); }
  foreign virtual void write_double(double value) { writeDouble(value); }
  foreign virtual void write_string(IN(RString) value) { writeString(value); }
  foreign virtual void write_wstring(IN(RString) value) { writeString(value); }
  foreign virtual void write_char_array(const char* value, int offset, int length);

  virtual void write_acdk_object(IN(::acdk::lang::RObject) value);

  foreign virtual void write_Object(IN(::org::omg::CORBA::RObject) value);
  // 2.3
  foreign virtual void write_value(IN(::acdk::io::RSerializable) value);
  foreign virtual void write_value(IN(::acdk::io::RSerializable) value, IN(::acdk::lang::RString) rep_id) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  foreign virtual void write_value(IN(::acdk::io::RSerializable) value, IN(::acdk::lang::RClass) clz) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  foreign virtual void write_abstract_interface(IN(::acdk::lang::RObject) obj);
  void write_exception(IN(RThrowable) ex);
  /**
    writes "IDL: + classname + ":1.0"
  */
  void write_repid(IN(::acdk::lang::RObject) obj);
  /**
    write given Object as struct
  */
  void write_struct(IN(::acdk::lang::RObject) obj, bool withParents = true);

  /** used for DMI over GIOP */
  virtual void write_scriptVar(IN(::acdk::lang::dmi::ScriptVar) sv, ParamCallDirection dir);
  /**
    write Object with meta infor used for DMI over GIOP 
    @param obj Object to write
    @param flags see Modifier
                 May contain ByVal
    @see enum DmiGiopArgFlags for serialization format
  */
  virtual void write_fq_object(IN(RObject) obj, int flags);
  
protected:
  void _checkAlignment(int align);
  void write_struct(IN(::acdk::lang::RObject) obj, const ::acdk::lang::dmi::ClazzInfo* ci, bool withParents);
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_CDRObjectWriter_h
