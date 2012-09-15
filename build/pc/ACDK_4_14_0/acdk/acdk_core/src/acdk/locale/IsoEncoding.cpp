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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/IsoEncoding.cpp,v 1.7 2005/03/07 14:02:13 kommer Exp $


#include "IsoEncoding.h"
#include "CEscapeEncoding.h"
#include "IllegalCharsetNameException.h"

#include <acdk/lang/Short.h>
#include <acdk/io/MemWriter.h>

namespace acdk {
namespace locale {

void 
IsoEncoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  /*
  {
    for (int i = 0; i < 255; ++i)
    {
      uc2char uc = _mapping->iso2uc(i);
      sys::coreout << i << " -> " << uc  << " <- " << _mapping->uc2iso(uc) << sys::eofl;
    }
  }*/
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (int i = 0; stopOn != 0 && it < end; ++it, ++i)
  {
    uc2char ch = *it;
    if (ch == 0 && stopOn == -2)
      return;

    uc2char c =  _mapping->iso2uc(ch);
    if (c == 0xFFFF)
      handleUnmappable(out, ch);
    else
    {
      out->write((byte)ch);
      ++_bytesWritten;
    }
    if (stopOn > 0)
      --stopOn;
  }
}



void 
IsoEncoder::encode(IN(acdk::io::RWriter) out, ucchar ch)
{
  uc2char c = _mapping->uc2iso(ch);
  if (c == 0xFFFF)
    handleUnmappable(out, ch);
  else
  {
    byte b = (char)c;
    out->write(&b, 0, 1);
    ++_bytesWritten;
  }
}

RString 
IsoEncoder::encode(IN(RString) str)
{
  // ### TODO
  return str;
}




int 
IsoDecoder::decodeToChar(IN(acdk::io::RReader) in)
{
  int c = in->read();
  if (c == -1)
    return c;
  ++_bytesReaded;
  c = _mapping->iso2uc(c);
  if (c == 0xffff)
  {
    RString s = handleUnmappable(c);
    if (s->length() > 0)
      return s->charAt(0);
  }
  return c;
}

RString 
IsoDecoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  
  StringBuffer sb;
  int i;
  while (stopOn != 0 && (i = in->read()) != -1)
  {
    ++_bytesReaded;
    uc2char uc = _mapping->iso2uc((char)i);
    if (stopOn == -2 && uc == 0)
      return sb.toString();
    sb.append(uc);
    if (stopOn > 0)
      --stopOn;
  }
  return sb.toString();
}

RString 
IsoDecoder::decode(IN(RString) str)
{
  // ### TODO
  return str;
}

extern IsoUnicodeMapping mappings[];

IsoEncoding::IsoEncoding(IN(RString) name)
: Encoding(name)
{
  for (int i = 0; mappings[i].name != 0; ++i)
  {
    if (name->equalsIgnoreCase(mappings[i].name) == true)
    {
      _mapping = &mappings[i];
      return;
    }
  }
  THROW1(IllegalCharsetNameException, "Unknown Iso Encoding name: " + name);
}

//static 
RStringArray 
IsoEncoding::getAvailableEncodings()
{
  RStringArray erg = new StringArray(0);
  for (int i = 0; mappings[i].name != 0; ++i)
  {
    erg->append(new String(mappings[i].name));
  }
  return erg;
}

//static 
REncoding 
IsoEncoding::getIsoEncoding(IN(RString) str)
{
  return new IsoEncoding(str);
}

REncoder 
IsoEncoding::getEncoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new IsoEncoder(this, _mapping, onMalformed, onUnmappable);
}

RDecoder 
IsoEncoding::getDecoder(CodingErrorAction onMalformed, CodingErrorAction onUnmappable)
{
  return new IsoDecoder(this, _mapping, onMalformed, onUnmappable);
}

namespace {
struct RegisterEncs
{
  RegisterEncs()
  {
    RStringArray sa = IsoEncoding::getAvailableEncodings();
    for (int i = 0; i < sa->length(); ++i)
    {
      Encoding::registerEncoding(sa[i], IsoEncoding::getIsoEncoding);
    }
  }
};
RegisterEncs _registerEncs;
} // anon namespace

} // locale
} // acdk

