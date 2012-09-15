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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryDataReader.h,v 1.19 2005/03/01 10:12:47 kommer Exp $

#ifndef acdk_io_BinaryDataReader_h
#define acdk_io_BinaryDataReader_h

#include "AbstractFilterReader.h"
#include "DataReader.h"
#include <acdk/lang/Number.h>

namespace acdk {
namespace lang {
enum Endian;
} // lang

namespace io {
 
ACDK_DECL_ENUM_FQ(acdk::lang, Endian)
ACDK_DECL_ENUM(SeekPos)
enum SeekPos;

ACDK_DECL_CLASS(BinaryDataReader);

/**
  Filter to writes Data in Binary format.
  Overtakes the role of java.io.DataOutputStream
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.19 $
  @date $Date: 2005/03/01 10:12:47 $
  
*/

class ACDK_CORE_PUBLIC BinaryDataReader
: extends AbstractFilterReader,
  implements DataReader
{
  ACDK_WITH_METAINFO(BinaryDataReader)
public:
 
private:
  acdk::lang::Endian _endian;
public:
  BinaryDataReader(IN(RReader) in, acdk::lang::Endian end = acdk::lang::BigEndian) 
  : AbstractFilterReader(in)
  , _endian(end)
  { 
  }
  virtual ~BinaryDataReader() { }
  // Reader
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos) { return AbstractFilterReader::seek(seekrel, seekpos); }
  foreign virtual jlong skip(jlong n) { return AbstractFilterReader::skip(n); }
  foreign virtual void reset() {  AbstractFilterReader::reset(); }
  using AbstractFilterReader::read;
  using AbstractFilterReader::ready;
  
  // FilterReader 
  foreign virtual void setIn(IN(RReader) reader) { AbstractFilterReader::setIn(reader); }
  foreign virtual RStorage getStorage() { return AbstractFilterReader::getStorage(); }
  foreign virtual RReader getStorageReader() { return AbstractFilterReader::getStorageReader(); }

  // DataReader
  virtual bool readBoolean();
  virtual char readChar();
  virtual uc2char readUcChar();
  virtual double readDouble();
  virtual float readFloat();
  virtual int readInt();
  virtual jlong readLong();
  virtual short readShort();
  virtual RString readString();
  virtual RbyteArray readOpaque();

  acdk::lang::Endian endian() { return _endian; }
  void endian(acdk::lang::Endian end) { _endian = end; }
};

} // io
} // acdk

#endif //acdk_io_BinaryDataReader_h

