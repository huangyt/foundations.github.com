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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLDecoder.cpp,v 1.8 2005/02/05 10:45:29 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Exception.h>

#include "URLDecoder.h"
#include <acdk/locale/CharacterCodingException.h>

#include <acdk/io/Reader.h>
#include <acdk/io/BytePtrReader.h>

namespace acdk {
namespace net {

int 
URLDecoder::decodeToChar(IN(acdk::io::RReader) in)
{
  int i = in->read();
  if (i == -1)
    return -1;
  if (i == '+')
    return ' ';
  if (i == '%')
  {
    i = in->read();
    if (i == -1)
      THROW1_FQ(acdk::locale::, CharacterCodingException, "Reached EOF while decoding character");
    int i2 = in->read();
    if (i == -1)
      THROW1_FQ(acdk::locale::, CharacterCodingException, "Reached EOF while decoding character");
    return (unhex(i) * 16 + unhex(i2));
  }
  return i;
}

RString 
URLDecoder::decodeToString(IN(acdk::io::RReader) in, int stopOn)
{
  int i = 0;
  StringBuffer sb;
  while ((i = decodeToChar(in)) != -1)
  {
    if ((stopOn == -2 && i == 0) || stopOn == 0)
      return sb.toString();
    if (stopOn > 0)
      --stopOn;
    sb.append(uc2char(i));
  }
  return sb.toString();
}


RString 
URLDecoder::decode(IN(RString) str)
{
  acdk::io::BytePtrReader in(str->byte_begin(), str->byte_end() - str->byte_begin());
  return decodeToString(&in, -1);
}
  
/*
//  static 
RString
URLDecoder::decode(IN(RString) source)
{

  StringBuffer result("");
  for (int i = 0; i < source->length(); i++)
  {
    char c = source->charAt(i);
    
    if ((c >= 'A') && (c <= 'Z'))
    {
      result.append(c);
      continue;
    }
    
    if ((c >= 'a') && (c <= 'z'))
    {
      result.append(c);
      continue;
    }
    
    if (c == '+')
    {
      result.append(' ');
      continue;
    }
    
    if (c == '%')
    {
      if ((i + 2) > (source->length() - 1))
        THROW1(Exception, RString("Invalid encoded URL: " + source));
      
      ++i;
      char c1 = source->charAt(i);
      ++i;
      char c2 = source->charAt(i);
      
      char r = (char)(unhex(c1)*16 + unhex(c2));
      result.append(r);
    }
    
    THROW1(Exception, "Unexpected character in encoded URL");
  }
  return result.toString();
  
}
*/

//  static 
int
URLDecoder::unhex(char c)
{

  c = Character::toUpperCase(c);
  int i;

  if ((c >= '0') && (c <= '9'))
    i = c - '0';
  else if ((c >= 'A') && (c <= 'F'))
    i = 10 + (c - 'A');
  else
    i = 0;
   
  return(i);

}

//  virtual 

} // namespace acdk
} // namespace net
