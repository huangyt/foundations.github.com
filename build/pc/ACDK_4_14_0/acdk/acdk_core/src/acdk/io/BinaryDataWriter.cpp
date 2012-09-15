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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryDataWriter.cpp,v 1.21 2005/03/14 12:20:44 kommer Exp $




#include <acdk.h>
#include "BinaryDataWriter.h"

#include <acdk/lang/System.h>
#include <acdk/lang/Number.h>

namespace acdk {
namespace io {

//#define LOCAL_DEBUG 

#ifdef LOCAL_DEBUG

RPrintWriter getDout();


#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)

#else
# define DOUT(str) do { } while(false)
#endif

template <class T> T convertBasicType(T t, acdk::lang::Endian end)
{
  if (end == acdk::lang::BigEndian)
    return Number::toBigEndian(t);
  else
    return Number::toLittleEndian(t);
}


//virtual 
void 
BinaryDataWriter::writeChar(char b)
{
  DOUT("BinaryDataWriter::writeChar: " << b);
  AbstractFilterWriter::write(b);
}

//virtual 
void 
BinaryDataWriter::writeUcChar(uc2char b)
{
  DOUT("BinaryDataWriter::writeChar: " << b);
  writeShort((short)b);
}


//virtual 
void 
BinaryDataWriter::writeBoolean(bool b)
{
  DOUT("BinaryDataWriter::writeBoolean: " << b);
  if (b == true)
    writeChar(char(1));
  else
    writeChar(char(0));
}

//virtual 
void 
BinaryDataWriter::writeShort(short b)
{
  DOUT("BinaryDataWriter::writeShort: " << b);
  b = convertBasicType(b, _endian);
  AbstractFilterWriter::write((const byte*)&b, 0, sizeof(b));
}

//virtual 
void 
BinaryDataWriter::writeInt(int b)
{
  DOUT("BinaryDataWriter::writeInt: " << b);
  b = convertBasicType(b, _endian);
  AbstractFilterWriter::write((const byte*)&b, 0, sizeof(b));
}

//virtual 
void 
BinaryDataWriter::writeLong(jlong b)
{
  DOUT("BinaryDataWriter::writeLong: " << b);
  b = convertBasicType(b, _endian);
  AbstractFilterWriter::write((const byte*)&b, 0, sizeof(b));
}

//virtual 
void 
BinaryDataWriter::writeFloat(float b)
{
  DOUT("BinaryDataWriter::writeFloat: " << b);
  b = convertBasicType(b, _endian);
  AbstractFilterWriter::write((const byte*)&b, 0, sizeof(b));
}

//virtual 
void 
BinaryDataWriter::writeDouble(double b)
{
  DOUT("BinaryDataWriter::writeDouble: " << b);
  b = convertBasicType(b, _endian);
  AbstractFilterWriter::write((const byte*)&b, 0, sizeof(b));
}

//virtual 
void
BinaryDataWriter::write(IN(RbyteArray) array, int offset, int len)
{
  
  if (len == -1)
    len = array->length() - offset;
  DOUT("BinaryDataWriter::write ba length: " << len);
  AbstractFilterWriter::write(array->data(), offset, len);
}



//virtual 
void 
BinaryDataWriter::writeString(IN(RString) str)
{
  RString nstr = str->convert(CCUtf8);
  int bl = nstr->byte_end() - nstr->byte_begin();
  writeInt(bl); // write byte length
  //acdk::locale::UTF8Encoder utf8enc;
  //utf8enc.encode(this, str);
  
  AbstractFilterWriter::write((const byte*)nstr->c_str(), 0, bl); 
  //writeChar('\0');
}

//virtual 
void 
BinaryDataWriter::writeOpaque(IN(RbyteArray) array)
{
  DOUT("BinaryDataWriter::writeOpaque ba length: " << array->length());
  writeInt(array->length());
  write(array);
}

static 
void __dummyInstance()
{
  RBinaryDataWriter br = new BinaryDataWriter((RWriter)System::out);
}


} // io
} // acdk
