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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ASCIIDataReader.h,v 1.17 2005/03/01 10:12:43 kommer Exp $

#ifndef acdk_io_ASCIIDataReader_h
#define acdk_io_ASCIIDataReader_h


#include "AbstractFilterReader.h"
#include "DataReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(ASCIIDataReader);

enum SeekPos;

/**
  Filter to writes Data in Binary format.
  Overtakes the role of java.io.DataOutputStream.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/03/01 10:12:43 $
*/

class ACDK_CORE_PUBLIC ASCIIDataReader
: extends AbstractFilterReader,
  implements DataReader
{
  ACDK_WITH_METAINFO(ASCIIDataReader)
public:
  ASCIIDataReader(IN(RReader) in) : AbstractFilterReader(in) { }
  virtual ~ASCIIDataReader() { }
  // Reader
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos) { return AbstractFilterReader::seek(seekrel, seekpos); }
  foreign virtual jlong skip(jlong n) { return AbstractFilterReader::skip(n); }
  foreign virtual void reset() { AbstractFilterReader::reset(); }
  foreign virtual void setIn(IN(RReader) reader) { AbstractFilterReader::setIn(reader); }

  foreign virtual RStorage getStorage() { return AbstractFilterReader::getStorage(); }
  foreign virtual RReader getStorageReader() { return AbstractFilterReader::getStorageReader(); }

  using AbstractFilterReader::read;
  using AbstractFilterReader::ready;
  

  /*foreign virtual jlong seek(SeekPos rel, jlong pos) { return AbstractFilterReader::seek(rel, pos); }
  foreign virtual jlong skip(jlong n) { return AbstractFilterReader::skip(n); }
  foreign virtual int read() { return AbstractFilterReader::read(); }
  foreign virtual int read(RbyteArray buffer, int offset = 0, int len = -1) { return AbstractFilterReader::read(buffer, offset, len); }
  foreign virtual int read(byte* buffer, int offset, int len)  { return AbstractFilterReader::read(buffer, offset, len); }
  foreign virtual void reset() { AbstractFilterReader::reset(); }
  foreign virtual bool ready() { return AbstractFilterReader::ready(); }
  // FilterReader 
  foreign virtual void setIn(RReader reader) { AbstractFilterReader::setIn(reader); }
  foreign virtual RStorage getStorage() { return AbstractFilterReader::getStorage(); }
  */
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
};

} // io
} // acdk

#endif //acdk_io_ASCIIDataReader_h

