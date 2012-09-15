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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/URLEncoder.cpp,v 1.10 2005/04/08 10:53:20 kommer Exp $

#include <acdk.h>
#include "URLEncoder.h"
#include <acdk/io/Writer.h>

namespace acdk {
namespace net {


namespace {

/** @internal */
char hexchars[] = { '0', '1', '2', '3', '4', '5', '6', 
                                         '7', '8', '9', 'A', 'B', 'C', 'D', 
                                         'E', 'F' };
} // anon ns

void 
URLEncoder::encode(IN(acdk::io::RWriter) out, IN(RString) str, int stopOn)
{
  for (String::iterator it = str->begin(); it != str->end(); ++it)
  {
    if (stopOn == -2 && *it == 0)
      break;
    if (stopOn == 0)
      break;
    if (stopOn > 0)
      --stopOn;
    encode(out, *it);
  }
}

void 
URLEncoder::encode(IN(acdk::io::RWriter) out, uc2char ch)
{
  //c &= 0xFF;
  if (((ch >= 'A') && (ch <= 'Z')) || ((ch >= 'a') && (ch <= 'z')) || ((ch >= '0') && (ch <= '9'))) 
  {
    out->write((byte*)&ch, 0, 1);
  }
  else if (ch == ' ')
  {
    out->write((byte*)"+", 0, 1);
  } 
  else  
  {
    byte buffer[4];
    buffer[0] = '%';
    buffer[1] = hexchars[ ch / 16 ];
    buffer[2] = hexchars[ ch % 16 ];
    out->write(buffer, 0, 3);
  }
}

  
//  static 
RString
URLEncoder::encode(IN(RString) source)
{
  int nlen = static_cast<int>(float(source->length()) * 1.3);
  StringBuffer result(nlen);
  for (int i = 0; i < source->length(); i++)
    {
    char c = source->charAt(i);
    c &= 0xFF;
    if ((c >= 'A') && (c <= 'Z')) {
      result.append(c);
      continue;
    }
    
    if ((c >= 'a') && (c <= 'z')) {
      result.append(c);
      continue;
    }
    
    if (c == ' '){
      result.append('+');
      continue;
    }
    
    result.append('%');
    result.append(hexchars[ c / 16 ]);
    result.append(hexchars[ c % 16 ]);
  }

  return result.toString();
}

// protected:
// private:
//  static 


//  virtual 


} // namespace acdk
} // namespace net
