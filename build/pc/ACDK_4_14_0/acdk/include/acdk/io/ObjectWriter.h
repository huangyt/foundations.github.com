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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ObjectWriter.h,v 1.16 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_ObjectWriter_h
#define acdk_io_ObjectWriter_h

#include "DataWriter.h"


namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(ObjectWriter);


/**
  Interface to write Objects.
  
  API: ACDK<br/>
  See also: gw_ref[acdk_hb_mi_serialization].
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:45 $

*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC ObjectWriter
: implements ::acdk::io::DataWriter
{
  ACDK_WITH_METAINFO(ObjectWriter)
public:

  // Writer:
  overwrite void flush() = 0;
  foreign void write(const byte* cstr, int offset, int len) { Writer::write(cstr, offset, len); }
  overwrite void write(byte c) = 0;

  // FilterWriter
  overwrite void setOut(IN(RWriter) writer) = 0;
  overwrite RStorage getStorage() = 0;
  overwrite RWriter getStorageWriter() = 0;
  // DataWriter
  overwrite void writeBoolean(bool b) = 0;
  overwrite void writeChar(char b) = 0;
  overwrite void writeUcChar(uc2char b) = 0;
  overwrite void writeShort(short b) = 0;
  overwrite void writeInt(int b) = 0;
  overwrite void writeLong(jlong b) = 0;
  overwrite void writeFloat(float b) = 0;
  overwrite void writeDouble(double b) = 0;
  overwrite void write(IN(RbyteArray) array, int offset = 0, int len = -1) = 0;
  overwrite void writeString(IN(RString) str) = 0;

  /**
      This version write Object to a Writer.

      In normal case, it also write a description (tagged)
      of the Class (for ClassLoader) at the beginning.
      @param obj the Object to writer
  */
  virtual void writeObject(IN(RObject) obj)
  {
    if (obj == Nil)
      writeObject(Nil, Nil);
    else
      writeObject(obj->getClass(), obj);
  }

  /**
    This version writes an Object to a Writer using
    the Class.

    In normal case, thit version writes the data
    untagged (i.e. no meta info) into the writer.
  */
  virtual void writeObject(IN(RClass) cls, IN(RObject) obj) = 0;
  /**
    @param hasTypeInfo the the is encoded in the stream
    @param withFlags writes also the flags
  */
  foreign virtual void writeScriptVar(acdk::lang::dmi::ScriptVar& sv, bool withTypeInfo = true, bool withFlags = true)
  {
    ObjectBase::_throwNotImplementedYet();
  }
  /**
    if a class implements the writeObject method it can call 
    defaultWriteObject for default object serialization
    This method only writes the Object information (the fields)
    of this class
  */
  virtual void defaultWriteObject(IN(RClass) cls, IN(RObject) obj) = 0;
  /**
    write a class unshared without any back references to previous written objects
  */
  virtual void writeUnshared(IN(RClass) cls, IN(RObject) obj) = 0;
  /**
    writes a class descriptor of current object.
    This method will only be called once for the most derived class
    The main purpose of the class descriptor is to identify the class
    to enable restruction out of the stream
  */
    
  virtual void writeClassDescriptor(IN(RClass) cls, IN(RObject) obj) = 0;

};


typedef ObjectWriter ObjectOutput;
typedef RObjectWriter RObjectOutput;


} // io
} // acdk

#endif //acdk_io_ObjectWriter_h

