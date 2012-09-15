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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/Writer.cpp,v 1.12 2005/02/05 10:44:54 kommer Exp $





#include <acdk.h>

#include "FilterWriter.h"
#include "Storage.h"
#include "CharToByteWriter.h"

namespace acdk {
namespace io {


//virtual 
RStorage 
Writer::getWriterStorage()
{
  RWriter tw(this);
  if (instanceof(tw, Storage) == true)
    return RStorage(tw);
  else if (instanceof(tw, FilterWriter) == true)
    return RStorage(RFilterWriter(tw)->getStorageWriter());
  return Nil;
}

void 
Writer::write(IN(RbyteArray) ch, int offset, int len)
{
  if (len == -1)
    len = ch->length() - offset;
  for (int i = offset; i < len + offset; ++i)
    write(ch[i]);
}

void 
Writer::write(const byte* cstr, int offset, int len)
{
  for (int i = offset; i < len; ++i)
    write(cstr[i]);
}
/*
void 
Writer::write_string(IN(RString) str)
{
  getCharWriter()->writeString(str);
  //write(str->byte_begin(), 0, str->byte_end() - str->byte_begin());
}
void 
Writer::write_string(const char* str)
{
  getCharWriter()->writeString(str);
  //write((const byte*)str, 0, strlen(str));
}
*/

RCharWriter 
Writer::getCharWriter(IN(acdk::locale::REncoder) encoder)
{
  return new CharToByteWriter(this, encoder);
}

} // io
} // acdk
