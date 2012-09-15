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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/DataWriter.h,v 1.13 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_DataWriter_h
#define acdk_io_DataWriter_h

#include "FilterWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

class DataWriter;
ACDK_DECL_INTERFACE(DataWriter);

/**
  Interface for writing basic data types.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:44 $

*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC DataWriter
: implements ::acdk::io::FilterWriter
{
  ACDK_WITH_METAINFO(DataWriter)
public:
  // Writer:
  overwrite void flush() = 0;
  overwrite void write(byte c) = 0;

  // FilterWriter
  overwrite void setOut(IN(RWriter) writer) = 0;
  overwrite RStorage getStorage() = 0;
  overwrite RWriter getStorageWriter() = 0;

  virtual void writeBoolean(bool b) = 0;
  virtual void writeChar(char b) = 0;
  virtual void writeUcChar(uc2char b) = 0;
  virtual void writeShort(short b) = 0;
  virtual void writeInt(int b) = 0;
  virtual void writeLong(jlong b) = 0;
  virtual void writeFloat(float b) = 0;
  virtual void writeDouble(double b) = 0;
  foreign void write(const byte* cstr, int offset, int len) { Writer::write(cstr, offset, len); }
  /**
    writes to the unterlying data stream
  */
  virtual void write(IN(RbyteArray) array, int offset = 0, int len = -1) = 0;
  virtual void writeString(IN(RString) str) = 0;
  /**
    writes the data as opaque data.
    DataWriter may apply some encoding
  */
  virtual void writeOpaque(IN(RbyteArray) array)
  {
    writeInt(array->length());
    write(array);
  }
};

typedef DataWriter DataOutput;
typedef RDataWriter RDataOutput;

} // io
} // acdk

#endif //acdk_io_DataWriter_h

