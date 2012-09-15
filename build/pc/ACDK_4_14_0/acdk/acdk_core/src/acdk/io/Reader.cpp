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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/Reader.cpp,v 1.22 2005/02/05 10:44:54 kommer Exp $




#include <acdk.h>
#include "Reader.h"
#include "Writer.h"
#include "MemWriter.h"
#include "FilterReader.h"

#include "IOException.h"
#include "CharArrayWriter.h"
#include "Storage.h"
#include "StringReader.h"
#include "ByteToCharReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

/*
//virtual 
RReader 
Reader::read(RbyteArray buffer)
{
  return read(buffer->data(), 0, buffer->length());
}
*/

/* no default implementation, because derivition hides polymorph
//virtual 
int 
Reader::read(RbyteArray buffer, int offset, int len)
{
  if (len == -1)
    len = buffer->length() - offset;
  buffer->ensureCapacity(offset + len);
  return read(buffer->data(), offset, len);
}

//virtual 
int 
Reader::read(byte* buffer, int offset, int len)
{
  byte* ptr = buffer;
  int readed = 0;
  for (; (readed = read()) != -1 && len > 0; len--)
    *ptr++ = byte(readed);
  return readed;
}
*/

void 
Reader::trans(IN(RWriter) out)
{
  Writer& tout = *out;
  const int BufferSize = 4096;
  byte buffer[BufferSize];
  int readed = 0;
  do {
    readed = read(buffer, 0, BufferSize);
    if (readed == -1)
      break;
    tout.write(buffer, 0, readed);
  } while (readed == BufferSize);
}

RbyteArray 
Reader::readAll()
{
  MemWriter min;
  trans(&min);
  return min.getBuffer();
}

//virtual 
int 
Reader::read(byte* buffer, int offset, int len) 
{ 
  int count = 0;
  for (int i = offset; i < len; ++i, ++count)
  {
    int ch = read();
    if (ch == -1)
      return count;
    buffer[i] = (byte)ch;
  } 
  return count;
}

/*
RString 
Reader::readIntoString()
{
  CharArrayWriter caw;
  trans(&caw);
  return caw.toString();
}
*/

//virtual 
RStorage 
Reader::getReaderStorage()
{
  RReader tr(this);
  if (instanceof(tr, Storage) == true)
    return RStorage(tr);
  else if (instanceof(tr, FilterReader) == true)
    return RStorage(RFilterReader(tr)->getStorageReader());
  return Nil;
}

RString 
Reader::readAllAsString()
{
  return getCharReader()->readString();
}

//virtual 
RCharReader 
Reader::getCharReader(IN(acdk::locale::RDecoder) decoder)
{
  return new ByteToCharReader(this, decoder);
}

} // io
} // acdk
