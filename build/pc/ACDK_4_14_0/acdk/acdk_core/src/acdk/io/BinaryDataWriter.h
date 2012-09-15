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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryDataWriter.h,v 1.18 2005/03/01 10:12:47 kommer Exp $

#ifndef acdk_io_BinaryDataWriter_h
#define acdk_io_BinaryDataWriter_h

#include "AbstractFilterWriter.h"
#include "DataWriter.h"
#include <acdk/lang/Number.h>

namespace acdk {
namespace io {

ACDK_DECL_ENUM_FQ(acdk::lang, Endian)


ACDK_DECL_CLASS(BinaryDataWriter);

/**
  Filter to writes Data in Binary format.
  Overtakes the role of java.io.DataOutputStream.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.18 $
  @date $Date: 2005/03/01 10:12:47 $
  
*/

class ACDK_CORE_PUBLIC BinaryDataWriter
: extends AbstractFilterWriter,
  implements DataWriter
{
  ACDK_WITH_METAINFO(BinaryDataWriter)
  acdk::lang::Endian _endian;
public:
  BinaryDataWriter(IN(RWriter) out, acdk::lang::Endian en = acdk::lang::BigEndian) 
  : AbstractFilterWriter(out) 
  , _endian(en)
  { 
  }
  virtual ~BinaryDataWriter() { }
  // Writer:
  overwrite void flush() { AbstractFilterWriter::flush(); }
  overwrite void write(byte c) { AbstractFilterWriter::write(c); }

  
  foreign virtual void write(const byte* cstr, int offset, int len)  
  { 
     AbstractFilterWriter::write(cstr, offset, len); 
  }
  
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  // FilterWriter
  foreign void setOut(IN(RWriter) writer) { AbstractFilterWriter::setOut(writer); }
  foreign RStorage getStorage() { return AbstractFilterWriter::getStorage(); }
  foreign RWriter getStorageWriter() { return AbstractFilterWriter::getStorageWriter(); }

  // DataWriter
  virtual void writeBoolean(bool b);
  virtual void writeChar(char b);
  virtual void writeUcChar(uc2char b);
  virtual void writeShort(short b);
  virtual void writeInt(int b);
  virtual void writeLong(jlong b);
  virtual void writeFloat(float b);
  virtual void writeDouble(double b);
  virtual void writeString(IN(RString) str);
  virtual void writeOpaque(IN(RbyteArray) array);
  acdk::lang::Endian endian() { return _endian; }
  void endian(acdk::lang::Endian end) { _endian = end; }
};

} // io
} // acdk

#endif //acdk_io_BinaryDataWriter_h

