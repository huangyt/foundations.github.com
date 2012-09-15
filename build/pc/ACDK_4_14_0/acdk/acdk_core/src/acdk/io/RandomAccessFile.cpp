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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/RandomAccessFile.cpp,v 1.16 2005/03/08 12:45:36 kommer Exp $





#include <acdk.h>
#include "RandomAccessFile.h"
#include "BinaryDataReader.h"
#include "BinaryDataWriter.h"

#include <stdio.h>
#include <errno.h>

#ifdef HAS_UNISTD_H
#  include <unistd.h>
#endif

#include <fcntl.h>
#include <sys/types.h> 
#include <sys/stat.h>


#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
#  include <direct.h>
#  include <io.h>
#  include <windows.h>
#endif
#ifdef ACDK_OS_UNIX
#  include <dirent.h>
#endif

#include "EOFException.h"
#include "IOException.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace io {

using namespace acdk::lang;



RandomAccessFile::RandomAccessFile(IN(RFile) file, IN(RString) mode,
                                   IN(RDataReader) dataReader,
                                   IN(RDataWriter) dataWriter)
: _inOut(file, mode),
  _file(file),
  _dataReader(dataReader),
  _dataWriter(dataWriter)
{
  if (_dataReader == Nil) 
    _dataReader = new (allocator()) BinaryDataReader(this);
  if (_dataWriter == Nil)
    _dataWriter = new (allocator()) BinaryDataWriter(this);
}
  
RandomAccessFile::RandomAccessFile(IN(RString) name, IN(RString) mode,
           IN(RDataReader) dataReader,
           IN(RDataWriter) dataWriter)
: _inOut(new (allocator()) File(name), mode),
  _file(new (allocator()) File(name)),
  _dataReader(dataReader),
  _dataWriter(dataWriter)
{
  ACDK_SAFE_CONSTRUCTOR();
  if (_dataReader == Nil)
    _dataReader = new (allocator()) BinaryDataReader(this);
  if (_dataWriter == Nil)
    _dataWriter = new (allocator()) BinaryDataWriter(this);
}

//virtual 
RandomAccessFile::~RandomAccessFile()
{
  close();
}

//virtual
void
RandomAccessFile::setIn(IN(RReader) reader)
{
  _dataReader = RDataReader(reader);
}
//virtual
void
RandomAccessFile::setOut(IN(RWriter) writer)
{
  _dataWriter = RDataWriter(writer);
}

//virtual 
void 
RandomAccessFile::write(const byte* cstr, int offset, int len)
{
  _inOut.write(cstr, offset, len);
}

//virtual 
void
RandomAccessFile::write(byte b)
{
  _inOut.write(b);
}

//virtual 
void
RandomAccessFile::write(IN(RbyteArray) b, int offset, int len)
{
  if (isWriteable() == false)
    THROW1(IOException, "File is not open for writing");
  _dataWriter->write(b, int(offset), int(len));
}


//virtual 
void 
RandomAccessFile::flush()
{
  _inOut.flush();
}

//virtual 
void 
RandomAccessFile::close()
{
  //_inOut.flush();
  _inOut.close();
}

//virtual 
RFileDescriptor 
RandomAccessFile::getFD()
{
  return _inOut.getFD();
}

//virtual 
jlong 
RandomAccessFile::getFilePointer()
{
  return _inOut.curSeekPos();
}

//virtual 
jlong 
RandomAccessFile::length()
{
  return _file->length();
}

//virtual 
int 
RandomAccessFile::read()
{
  if (isReadable() == false)
    THROW1(IOException, "File is not open for reading");
  return _inOut.read();
}

//virtual 
int
RandomAccessFile::read(IN(RbyteArray) b, int offset, int len)
{
  if (len == -1)
    len = b->length() - offset;
  if (isReadable() == false)
    THROW1(IOException, "File is not open for reading");
  return _inOut.read(b, offset, len);
}

//virtual 
int
RandomAccessFile::read(byte* buffer, int offset, int len)
{
  if (isReadable() == false)
    THROW1(IOException, "File is not open for reading");
  return _inOut.read(buffer, offset, len);
}
 
//virtual 
void
RandomAccessFile::seek(jlong pos)
{
  _inOut.seek(SeekSet, pos);  
}

//virtual 
jlong 
RandomAccessFile::seek(acdk::io::SeekPos seekrel, jlong seekpos)
{
  return _inOut.seek(seekrel, seekpos);
}

//virtual 
void 
RandomAccessFile::reset()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual 
bool 
RandomAccessFile::ready()
{
  return _inOut.available() > 0;
}

//virtual 
void 
RandomAccessFile::setLength(jlong newLength)
{
  if (isWriteable() == false)
    THROW1(IOException, "File is not open for writing");
  _inOut.setLength(newLength);  
}

//virtual 
int 
RandomAccessFile::skipBytes(int n)
{
  return _inOut.skip(n);
}

/*
static short byte2short(byte* buf)
{
  return (short)(((buf[0] & 0xFF) << 8) | (buf[1] & 0xFF));
}


static int byte2int(byte* buf)
{
  return ((buf[0] & 0xFF) << 24) | ((buf[1] & 0xFF) << 16) |  ((buf[2] & 0xFF) << 8) | (buf[3] & 0xFF);
}

static jlong byte2long(byte* buf)
{
  return (((jlong)buf[0] & 0xFF) << 56) | (((jlong)buf[1] & 0xFF) << 48) | 
                (((jlong)buf[2] & 0xFF) << 40) | (((jlong)buf[3] & 0xFF) << 32) | 
                (((jlong)buf[4] & 0xFF) << 24) | (((jlong)buf[5] & 0xFF) << 16) |
                (((jlong)buf[6] & 0xFF) << 8) | ((jlong)buf[7] & 0xFF);
}


static RString uft2String(RbyteArray buf)
{
  RStringBuffer sb = new StringBuffer("");
  for (int i = 0; i < buf->length(); i++) {
    int byte_read = buf[i];
    if ((byte_read & 0xE0) == 0xE0) { // 224
      int val = (byte_read & 0x0F) << 12;
      ++i;
      if (i == buf->length())
        THROW1(EOFException, "Unexpected end of stream");
      byte_read = buf[i];
      if ((byte_read & 0x80) != 0x80)
        THROW1(UTFDataFormatException, RString("Bad byte in input: ") + byte_read);
      val |= (byte_read & 0x3F) << 6;
      ++i;
      if (i == buf->length())
        THROW1(EOFException, "Unexpected end of stream");
      byte_read = buf[i];
      if ((byte_read & 0x80) != 0x80)
        THROW1(UTFDataFormatException, RString("Bad byte in input: ") + byte_read);
      val |= (byte_read & 0x3F);
      sb.append((byte)val);
    } else if ((byte_read & 0xC0) == 0xC0) { // 192 // Two byte encoding case
      int val = (byte_read & 0x1F) << 6;
      ++i;
      if (i == buf->length())
        THROW1(EOFException, "Unexpected end of stream");
      byte_read = buf[i];
      if ((byte_read & 0x80) != 0x80)
        THROW1(UTFDataFormatException. (RString("Bad byte in input: ") + byte_read);
      val |= (byte_read & 0x3F);
      sb.append((byte)val);
    }  else if (byte_read < 128) { // // One byte encoding case
      sb.append((byte)byte_read);
    } else {
      THROW1(UTFDataFormatException, RString("Bad byte in input: ") + byte_read);
    }      
  }
  return sb->toString();
}
*/

//virtual 
bool 
RandomAccessFile::readBoolean()
{
  return _dataReader->readBoolean();
}

//virtual 
char 
RandomAccessFile::readChar()
{
  return _dataReader->readChar();
}

//virtual 
uc2char 
RandomAccessFile::readUcChar()
{
  return _dataReader->readUcChar();
}


//virtual 
double 
RandomAccessFile::readDouble()
{
  return _dataReader->readDouble();
}

//virtual 
float 
RandomAccessFile::readFloat()
{
  return _dataReader->readFloat();
}

//virtual 
int 
RandomAccessFile::readInt()
{
  return _dataReader->readInt();
}

//virtual 
jlong 
RandomAccessFile::readLong()
{
  return _dataReader->readLong();
}

//virtual 
short 
RandomAccessFile::readShort()
{
  return _dataReader->readShort();
}

//virtual 
RString 
RandomAccessFile::readString()
{
  return _dataReader->readString();
}

//virtual
RString 
RandomAccessFile::readUTF()
{
  return Nil;
}


//virtual 
void
RandomAccessFile::writeBoolean(bool b)
{
  _dataWriter->writeBoolean(b);
}

//virtual 
void 
RandomAccessFile::writeChar(char b)
{
  _dataWriter->writeChar(b);
}

//virtual 
void 
RandomAccessFile::writeUcChar(uc2char b)
{
  _dataWriter->writeUcChar(b);
}


//virtual 
void 
RandomAccessFile::writeShort(short b)
{
  _dataWriter->writeShort(b);
}

//virtual 
void 
RandomAccessFile::writeInt(int b)
{
  _dataWriter->writeInt(b);
}

//virtual 
void 
RandomAccessFile::writeLong(jlong b)
{
  _dataWriter->writeLong(b);
}

//virtual 
void 
RandomAccessFile::writeFloat(float b)
{
  _dataWriter->writeFloat(b);
}

//virtual 
void 
RandomAccessFile::writeDouble(double b)
{
  _dataWriter->writeDouble(b);
}

//virtual
void 
RandomAccessFile::writeString(IN(RString) str)
{
  _dataWriter->writeString(str);
}

//virtual
void
RandomAccessFile::writeUTF(IN(RString) str)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual 
RString 
RandomAccessFile::getDeviceName()
{
  return _file->getCanonicalPath();
}

//virtual 
RStorage 
RandomAccessFile::getStorage()
{
  return this;
}

} // io
} // acdk


