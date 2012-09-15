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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryObjectReader.h,v 1.17 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_BinaryObjectReader_h
#define acdk_io_BinaryObjectReader_h


#include "AbstractObjectReader.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(BinaryObjectReader);


/**
  Implementation to read Objects and basic type in binary format.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:44 $

*/
class ACDK_CORE_PUBLIC BinaryObjectReader
: extends ::acdk::io::AbstractObjectReader
{
private:
  BinaryDataReader _binin;
public:
  BinaryObjectReader(RReader in, int flags = SerializeDefaultFlags)
  : AbstractObjectReader(in, flags)
  , _binin(in)
  {
  }
  virtual bool readBoolean()
  {
    return _binin.readBoolean();
  }
  virtual char readChar()
  {
    return _binin.readChar();
  }
  virtual uc2char readUcChar()
  {
    return _binin.readUcChar();
  }
  virtual short readShort()
  {
    return _binin.readShort();
  }
  virtual int readInt()
  {
    return _binin.readInt();
  }
  virtual jlong readLong()
  {
    return _binin.readLong();
  }
  virtual float readFloat()
  {
    return _binin.readFloat();
  }
  virtual double readDouble()
  {
    return _binin.readDouble();
  }
  RString readStringImpl()
  {
    return _binin.readString();
  }
  int read()
  {
    return _binin.read();
  }
  int read(IN(RbyteArray) array, int offset = 0, int len = -1)
  {
    return _binin.read(array, offset, len);
  }
  foreign virtual int read(byte* buffer, int offset, int len)
  {
    return _binin.read(buffer, offset, len);
  }
  virtual RString readTagStart(IN(RString) key, IN(RString) value = Nil);
  virtual RString readTagEnd(IN(RString) key, IN(RString) value = Nil);
  virtual RClass readClassId();
};




} // io
} // acdk

#endif //acdk_io_BinaryObjectReader_h

