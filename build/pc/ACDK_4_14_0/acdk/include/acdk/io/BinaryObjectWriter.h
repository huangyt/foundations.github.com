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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryObjectWriter.h,v 1.16 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_BinaryObjectWriter_h
#define acdk_io_BinaryObjectWriter_h


#include "AbstractObjectWriter.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(BinaryObjectWriter);


/**
  Implementation to write Objects and basic type in binary format.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:44 $

*/
class ACDK_CORE_PUBLIC BinaryObjectWriter
: extends ::acdk::io::AbstractObjectWriter
{
private:
  BinaryDataWriter _binout;
public:
  BinaryObjectWriter(IN(RWriter) out, int flags = SerializeDefaultFlags)
  : AbstractObjectWriter(out, flags)
  , _binout(out)
  {
  }
  virtual void writeBoolean(bool b)
  {
    _binout.writeBoolean(b);
  }
  virtual void writeChar(char b)
  {
    _binout.writeChar(b);
  }
  virtual void writeUcChar(uc2char b)
  {
    _binout.writeUcChar(b);
  }
  virtual void writeShort(short b)
  {
    _binout.writeShort(b);
  }
  virtual void writeInt(int b)
  {
    _binout.writeInt(b);
  }
  virtual void writeLong(jlong b)
  {
    _binout.writeLong(b);
  }
  virtual void writeFloat(float b)
  {
    _binout.writeFloat(b);
  }
  virtual void writeDouble(double b)
  {
    _binout.writeDouble(b);
  }
  void writeStringImpl(IN(RString) str)
  {
    _binout.writeString(str);
  }
  void write(IN(RbyteArray) array, int offset = 0, int len = -1)
  {
    _binout.write(array, offset, len);
  }
  void write(byte b) { _binout.write(b); }
  foreign void write(const byte* buffer, int offset, int len) { _binout.write(buffer, offset, len); }

  /// implement form AbstractObjectWriter
  virtual void writeTagStart(IN(RString) key, IN(RString) value = Nil);
  /// implement form AbstractObjectWriter
  virtual void writeTagEnd(IN(RString) key, IN(RString) value = Nil);
  /// implement form AbstractObjectWriter
  virtual void writeClassId(IN(::acdk::lang::RClass) cls);

};




} // io
} // acdk

#endif //acdk_io_BinaryObjectWriter_h

