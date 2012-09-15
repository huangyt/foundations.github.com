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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/Base64.cpp,v 1.10 2005/04/08 10:53:21 kommer Exp $



#include "Base64.h"
#include <acdk/lang/Byte.h>

namespace acdk {
namespace text {

namespace {
/** @internal */
  char valid_chars[] = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/** @internal */
void
_decode(byte* decodeMap, //128
        byte* tempbuf, 
        byteArray& retbuffer, int& written)
{
  int towrite = 3;
  if (tempbuf[3] == '=')  
    towrite = 2;
  if (tempbuf[2] == '=')  
    towrite = 1;
  int b0 = decodeMap[tempbuf[0]];
  int b1 = decodeMap[tempbuf[1]];
  int b2 = decodeMap[tempbuf[2]];
  int b3 = decodeMap[tempbuf[3]];
  switch (towrite) {
  case 1:
    retbuffer[written++] = (byte)(b0 << 2 & 0xfc | b1 >> 4 & 0x3);
    break;
  case 2:
    retbuffer[written++] = (byte)(b0 << 2 & 0xfc | b1 >> 4 & 0x3);
    retbuffer[written++] = (byte)(b1 << 4 & 0xf0 | b2 >> 2 & 0xf);
    return;
  case 3:
    retbuffer[written++] = (byte)(b0 << 2 & 0xfc | b1 >> 4 & 0x3);
    retbuffer[written++] = (byte)(b1 << 4 & 0xf0 | b2 >> 2 & 0xf);
    retbuffer[written++] = (byte)(b2 << 6 & 0xc0 | b3 & 0x3f);
  default:
    THROW1(RuntimeException, "Unexpected Stream format in Base64");
  }
}


} // annon namespace

//static 
RbyteArray 
Base64::encode(IN(RbyteArray) in)
{
  if (in == Nil)
    return Nil;
  int len = in->length();
  if (len <= 0)  
    return new byteArray(0);
  
  RbyteArray out = new byteArray(len / 3 * 4 + 4);
  int rindex = 0;
  int cidx = 0;
  int rest = len;
  while (rest >= 3) {
    int i = ((in[rindex] & 0xff) << 16) 
            + ((in[rindex + 1] & 0xff) << 8)
            + (in[rindex+2] & 0xff);
    out[cidx++] = valid_chars[i >> 18];
    out[cidx++] = valid_chars[(i >> 12) & 0x3f];
    out[cidx++] = valid_chars[(i >> 6) & 0x3f];
    out[cidx++] = valid_chars[i & 0x3f];
    rindex += 3;
    rest -= 3;
  }
  if (rest == 1) {
    int i = in[rindex] & 0xff;
    out[cidx++] = valid_chars[i>>2];
    out[cidx++] = valid_chars[(i<<4)&0x3f];
    out[cidx++] = '=';
    out[cidx++] = '=';
  } else if (rest == 2) {
    int i = ((in[rindex]&0xff)<<8)+(in[rindex+1]&0xff);
    out[cidx++] = valid_chars[i>>10];
    out[cidx++] = valid_chars[(i>>4)&0x3f];
    out[cidx++] = valid_chars[(i<<2)&0x3f];
    out[cidx++] = '=';
  }
  out->resize(cidx);
  return out;
}


  
//static 
RbyteArray 
Base64::decode(IN(RbyteArray) data)
{
  if (data == Nil)  
    return Nil;
  int len = data->length();
  if (len == 0)
    return new byteArray(0);

  byte decodeMap[128];
  int i;
	for (i = 0; i < 128; i++)
	  decodeMap[int(valid_chars[i])] = (byte)i;
  
  byte tempbuf[4];

  
  int tempbufpos = 0;
  RbyteArray retbuffer = new byteArray(len / 4 * 3 + 3);
  int written = 0;
  for (i = 0; i < len; i ++) 
  {
    byte ch = data[i];
    if (ch == '=' || (ch < 64 && decodeMap[ch] != acdk::lang::Byte::MAX_VALUE))
    {
      tempbuf[tempbufpos++] = ch;
      if (tempbufpos == 4) {
        tempbufpos = 0;
        _decode(decodeMap, tempbuf, *retbuffer, written);
      }
    }
  }
  retbuffer->resize(written);
  return retbuffer;
}


} // text
} // acdk



