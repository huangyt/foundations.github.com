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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryDataReader.cpp,v 1.19 2005/03/14 12:20:44 kommer Exp $




#include <acdk.h>
#include "BinaryDataReader.h"

#include "EOFException.h"

#include <acdk/lang/System.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/Number.h>
#include <acdk/locale/UTF8Encoding.h>
#include <acdk/lang/sys/core_alloca.h>


namespace acdk {
namespace io {

//#define LOCAL_DEBUG 

#ifdef LOCAL_DEBUG 

#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)

#else
# define DOUT(str) do { } while(false)
#endif


template <class T> T readBasicType(RReader& in, acdk::lang::Endian end)
{
  T b;
  if (in->read((byte*)&b, 0, sizeof(T)) < long(sizeof(T)))
    THROW1(EOFException, "Unexpected end of stream");
  if (end == acdk::lang::BigEndian)
    return Number::fromBigEndian(b);
  else
    return Number::fromLittleEndian(b);

}


//virtual 
bool 
BinaryDataReader::readBoolean()
{
  
  bool b = readBasicType<char>(_in, _endian) != '\0' ? true : false;
  DOUT("BinaryDataReader::readBoolean: " << b);
  return b;
}

//virtual 
char 
BinaryDataReader::readChar()
{
  int c = _in->read();
  if (c == -1)
    THROW1(EOFException, "Unexpected end of stream");
  DOUT("BinaryDataReader::readChar: " << c);
  return c;
}

//virtual 
uc2char 
BinaryDataReader::readUcChar()
{
  return (uc2char)readShort();
}


//virtual 
double 
BinaryDataReader::readDouble()
{
  double d = readBasicType<double>(_in, _endian);
  DOUT("BinaryDataReader::readDouble: " << d);
  return d;
}

//virtual 
float 
BinaryDataReader::readFloat()
{
  float d = readBasicType<float>(_in, _endian);
  DOUT("BinaryDataReader::readFloat: " << d);
  return d;
}

//virtual 
int 
BinaryDataReader::readInt()
{
  int d =  readBasicType<int>(_in, _endian);
  DOUT("BinaryDataReader::readInt: " << d);
  return d;
}

//virtual 
jlong 
BinaryDataReader::readLong()
{
  jlong d = readBasicType<jlong>(_in, _endian);
  DOUT("BinaryDataReader::readLong: " << d);
  return d;
}

//virtual 
short 
BinaryDataReader::readShort()
{
  short d = readBasicType<short>(_in, _endian);
  DOUT("BinaryDataReader::readShort: " << d);
  return d;
}

//virtual 
RString 
BinaryDataReader::readString()
{
  int length = readInt(); // 
  DOUT("BinaryDataReader::readString, length: " << length);
  if (length < 0)
    THROW1(IOException, RString("BinaryDataReader::readString: nagative string length") + length);
  byte* ptr = (byte*)core_alloca(length + 1);
  
  //byte* ptr = (byte*)allocate(length + 1, acdk::lang::sys::DumpBufferMem);
  int erg = read(ptr, 0, length);
  if (erg != length)
    THROW1(EOFException, "BinaryDataReader::readString: read buffer to small: requested: " + String::valueOf(length) + " readed: " + erg);
  ptr[length] = 0;
  RString serg = new String((byte*)ptr, acdk::locale::UTF8Encoding::getUTF8Encoding()->getDecoder(), 0, length);
  core_freea(ptr);
  return serg;

}

//virtual 
RbyteArray 
BinaryDataReader::readOpaque()
{
  int len = readInt();
  DOUT("BinaryDataReader::readOpaque, length: " << len);
  RbyteArray ba = new byteArray(len);
  read(ba, 0, len);
  return ba;
}

static 
void __dummyInstance()
{
  RBinaryDataReader br = new BinaryDataReader((RReader)System::in);
}

} // io
} // acdk
