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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/Format.cpp,v 1.13 2005/03/08 18:49:55 kommer Exp $





#include "Format.h"
#include "FieldPosition.h"
#include "ParsePosition.h"
#include <acdk/lang/StringBuffer.h>

#include <acdk/lang/UnsupportedOperationException.h>
#include <stdio.h>

namespace acdk {
namespace text {

Format::Format() 
{
}

Format::~Format() 
{
}

RString
Format::format(IN(RObject) obj)
{
  RStringBuffer sb = new StringBuffer("");
  RFieldPosition fp = new FieldPosition(0);
  format(obj,sb,fp);
  return RString(sb->toString());
}

RObject
Format::parseObject(IN(RString) str)
{
  RParsePosition pp = new ParsePosition(0);
  return parseObject(str, pp);
}

RObject
Format::clone()
{
  THROW0(UnsupportedOperationException);
  return Nil;
  //RFormat other = RFormat(Object::clone());
  //return other;
  
}

//static 
RString 
Format::dos2unix(IN(RString) str)
{
  return str->replace("\r\n", "\n");
}

//static 
RString 
Format::unix2dos(IN(RString) str)
{
  StringBuffer sb(str->length());
  String::iterator it = str->begin();
  String::iterator end = str->end();
  for (; it != end; ++it)
  {
    if (*it == '\r') {
      sb.append(*it);
      ++it;
      sb.append(*it);
    } else if (*it == '\n') {
      sb.append("\r\n");
    } else
      sb.append(*it);
  }
  return sb.toString();
}

//static 
RString 
Format::dumpbin(IN(RbyteArray) data, int wide)
{
  StringBuffer sb(data->length() * 5);
  int upto = 0;
  byte* it = data->data();
  byte* end = it + data->length();
  charArray linebuf(wide);
  while (it < end)
  {
    char buffer[200];
    sprintf(buffer, "%06lx ",(long)upto);
    sb.append(buffer);
    int j = 0;
    int i = 0;
    for (i = 0; i < wide; i++) 
    {
      int ch = *it++;
      if (it >= end) 
      {
        sb.append("   ");
      } else {
        linebuf[i] = ch;
        
        sprintf(buffer, "%02x ", ch);
        sb.append(buffer);
        j = i + 1;
      }
    }
    for (i = 0; i < j; i++) 
    {
      if (isprint((unsigned char)linebuf[i]))
      {
        sb.append(linebuf[i]);
      } 
      else 
      {
        sb.append('.');
      }
    }
    sb.append("\n");
    upto += wide;
  }
  return sb.toString();
}

//static 
RString 
Format::hexToString(IN(RcharArray) ch, int offset, int length)
{
  if (length == -1)
    length = ch->length() - offset;
  return hexToString(ch->data() + offset, length);
}

//static
RString 
Format::hexToString(const char* buffer, int length)
{
  StringBuffer sb(length * 3);
  for (int i = 0; i < length; ++i)
  {
    if (isprint(buffer[i]))
    {
      sb.append(" ");
      sb.append(buffer[i]);
      sb.append(" ");
    } 
    else
    {
      char cbuffer[5];
      sprintf(cbuffer, "%02x ", buffer[i]);
      sb.append(cbuffer);
    }
  }
  return sb.toString();
}

} // text
} // acdk

