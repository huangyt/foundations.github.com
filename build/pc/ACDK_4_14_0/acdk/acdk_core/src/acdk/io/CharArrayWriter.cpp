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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharArrayWriter.cpp,v 1.10 2005/02/05 10:44:53 kommer Exp $

#include <acdk.h>
#include "CharArrayWriter.h"


namespace acdk {
namespace io {
using namespace acdk::lang;
/*
//virtual 
void
CharArrayWriter::write(const byte* cstr, int offset, int len)
{
  if (cstr == 0)
    return;
  if (int(len) == -1)
    len = strlen((const char*)cstr) - offset;

  cstr += offset;
  
  ensureCapacity(_writePos + len);
  for (int i = 0; i < len; i++)
    _sb[_writePos++] = cstr[i];

}
  */
} // io
} // acdk
