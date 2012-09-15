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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/DataReader.h,v 1.11 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_DataReader_h
#define acdk_io_DataReader_h

#include "Reader.h"
#include "FilterReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;


ACDK_DECL_INTERFACE(DataReader);

/** 
  Interface for reading basic data.

  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC DataReader 
: implements FilterReader
{
  ACDK_WITH_METAINFO(DataReader)
public:
   // from Reader
  overwrite jlong seek(SeekPos seekrel, jlong seekpos) = 0;
  overwrite void reset() = 0;
  overwrite jlong skip(jlong n) = 0;
  
  // from FilterReader
  overwrite void setIn(IN(RReader) reader) = 0;
  overwrite RStorage getStorage() = 0;
  overwrite RReader getStorageReader() = 0;

  virtual bool readBoolean() = 0;
  virtual char readChar() = 0;
  virtual uc2char readUcChar() = 0;
  virtual double readDouble() = 0;
  virtual float readFloat() = 0;
  virtual int readInt() = 0;
  virtual jlong readLong() = 0;
  virtual short readShort() = 0;
  virtual RString readString() = 0;
  /**
    writes the oktet array as opaque data with optional 
    encoding
  */
  virtual RbyteArray readOpaque()
  {
    int size = readInt();
    RbyteArray erg = new byteArray(size);
    read(erg);
    return erg;
  }
};

typedef DataReader DataInput;
typedef RDataReader RDataInput;


} // io
} // acdk

#endif //acdk_io_DataReader_h

