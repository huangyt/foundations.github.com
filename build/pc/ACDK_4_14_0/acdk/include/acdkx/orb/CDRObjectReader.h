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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CDRObjectReader.h,v 1.19 2005/03/14 17:59:13 kommer Exp $

#ifndef acdkx_orb_CDRObjectReader_h
#define acdkx_orb_CDRObjectReader_h

#include <acdk.h>
#include <acdk/io/ObjectReader.h>
#include <acdk/io/AbstractFilterReader.h>
#include "orb.h"

#include <org/omg/CORBA/portable/OutputStream.h>
#include <org/omg/CORBA/portable/InputStream.h>


namespace acdkx {
namespace orb {

using ::org::omg::CORBA::portable::ParamCallDirection;
using ::org::omg::CORBA::portable::CallerWrite;
using ::org::omg::CORBA::portable::CalleeRead;
using ::org::omg::CORBA::portable::CalleeWrite;
using ::org::omg::CORBA::portable::CallerRead;

ACDK_DECL_CLASS(CDRObjectReader);

USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, DataReader);

USING_CLASS(::org::omg::CORBA::portable::, InputStream);
USING_CLASS(::org::omg::CORBA::portable::, OutputStream);

class ACDKX_ORB_PUBLIC CDRObjectReader
: extends ::acdk::io::AbstractFilterReader,
  implements ::acdk::io::ObjectReader,
  implements ::org::omg::CORBA::portable::InputStream
{
  ACDK_WITH_METAINFO(CDRObjectReader)
private:
  int _index;
protected:
  org::omg::CORBA::RORB _orb;
public:
  CDRObjectReader(IN(RReader) out, IN(org::omg::CORBA::RORB) orb);
  // Reader
  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = 1);
  foreign virtual int read(byte* buffer, int offset, int len);
  foreign RReader getStorageReader()  { return ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterReader)::getStorageReader(); }
  overwrite jlong skip(jlong n) { return ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterReader)::skip(n); }
  foreign virtual void reset() { ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterReader)::reset(); }
  foreign virtual void setIn(IN(RReader) reader) { ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterReader)::setIn(reader); }
  overwrite acdk::io::RStorage getStorage() { return ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterReader)::getStorage(); }
  foreign virtual jlong seek(acdk::io::SeekPos seekrel, jlong seekpos) { return ACDK_FQ_SUPER_QUALIFIER(acdk::io::, AbstractFilterReader)::seek(seekrel, seekpos); }
  
  // DataReader
  foreign virtual bool readBoolean();
  foreign virtual char readChar();
  foreign virtual uc2char readUcChar();
  foreign virtual double readDouble();
  foreign virtual float readFloat();
  foreign virtual int readInt();
  foreign virtual jlong readLong();
  foreign virtual short readShort();
  foreign virtual RString readString();

  // ObjectReader
  foreign virtual ::acdk::lang::RObject readObject();
  foreign virtual ::acdk::lang::RObject readObject(IN(::acdk::lang::RClass) clz);
  /** not supported */
  foreign virtual RClass readClassDescriptor(IN(RClass) cls)
  {
    THROW0(UnsupportedOperationException);
    return Nil;
  }
  /** not supported */
  foreign virtual void defaultReadObject(IN(RClass) cls, IN(RObject) obj)
  {
    THROW0(UnsupportedOperationException);
  }
  /**
    @param withTypeInfo the the is encoded in the stream
    @param withFlags  writes also the flags 
  */
  foreign virtual acdk::lang::dmi::ScriptVar readScriptVar(bool withTypeInfo = true, bool withFlags = true);

  // org::omg::CORBA::portable::InputStream
  foreign virtual org::omg::CORBA::RORB orb() { return _orb; }

  foreign virtual bool read_boolean() { return readBoolean(); }
  foreign virtual char read_char() { return readChar(); }
  foreign virtual char read_wchar() { return readChar(); }
  foreign virtual byte read_octet() { return readChar(); }
  foreign virtual short read_short() { return readShort(); }
  foreign virtual short read_ushort() { return readShort(); }
  foreign virtual int read_long() { return readInt(); }
  foreign virtual int read_ulong() { return readInt(); }
  foreign virtual jlong read_longlong() { return readLong(); }
  foreign virtual long read_ulonglong() { return readLong(); }
  foreign virtual float read_float() { return readFloat(); }
  foreign virtual double read_double() { return readDouble(); }
  foreign virtual RString read_string() { return readString(); }
  foreign virtual void read_char_array(char* value, int offset, int length);
  /**
    try figure out themselves, which method applies.
    If instanceOf ::acdk::io::Serializable -> readObject
    If instanceOf ::acdk::lang::Throwable -> readObject
    If instanceOf ::org::omg::CORBA::Object -> read_Object

  */
  virtual ::acdk::lang::RObject read_acdk_object(IN(::acdk::lang::RClass) clz);
  /** reads an IOR */
  foreign virtual ::org::omg::CORBA::RObject read_Object();
  /** reads an IOR */
  foreign virtual ::org::omg::CORBA::RObject read_Object(IN(::acdk::lang::RClass) clz);

  foreign virtual ::acdk::lang::RObject read_abstract_interface();
  foreign virtual ::acdk::lang::RObject read_abstract_interface(IN(::acdk::lang::RClass) clz);
  foreign virtual ::acdk::lang::RObject read_value() 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil;
  }
  foreign virtual ::acdk::lang::RObject read_value(IN(::acdk::lang::RString) rep_id) 
  {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
    return Nil;
  }
  foreign virtual ::acdk::lang::RObject read_value(IN(::acdk::lang::RClass) clz)
  {
    return readObject(clz);
  }
  /*
  virtual ::acdk::lang::RObject read_value(org.omg.CORBA.BoxedValueHelper factory) {
    THROW0_FQ(::org::omg::CORBA::, NO_IMPLEMENT);
  }
  */
  foreign virtual ::acdk::lang::RObject read_value(IN(::acdk::lang::RObject) value) 
  {
    return read_value(value->getClass());
  }

  /**
    used for DMI over GIOP 
  */
  virtual void read_scriptVar(OUT(::acdk::lang::dmi::ScriptVar) sv, ParamCallDirection dir);
  
  /**
    read fq Object from Stream used for DMI over GIOP
    @param flags see Modifier
                 May contain ByVal
    @see enum DmiGiopArgFlags for serialization format
  */
  ::acdk::lang::RObject read_fq_object(int flags);
  void read_struct(IN(RObject) obj, bool withParent = true);
  void read_struct(IN(::acdk::lang::RObject) obj, const ::acdk::lang::dmi::ClazzInfo* ci, bool withParents);
  RThrowable read_exception();
protected:
  void _checkAlignment(int align);
};

} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_CDRObjectReader_h
