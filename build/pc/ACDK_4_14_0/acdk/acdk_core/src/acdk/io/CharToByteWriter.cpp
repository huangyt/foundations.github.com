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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharToByteWriter.cpp,v 1.7 2005/02/05 10:44:53 kommer Exp $


#include "CharToByteWriter.h"
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace  io {

CharToByteWriter::CharToByteWriter(IN(RWriter) out, IN(acdk::locale::REncoder) encoder, IN(RObject) iolock)
: AbstractCharWriter(iolock)
, _encoder(encoder)
, _out(out)
{
  if (_encoder == Nil)
    _encoder = acdk::locale::Encoding::getEncoding()->getEncoder();
}

void 
CharToByteWriter::writeChar(char c)
{
  _encoder->encode(_out, (ucchar)c);
}

void 
CharToByteWriter::writeChar(ucchar c)
{
  _encoder->encode(_out, c);
}

void 
CharToByteWriter::writeString(const char* cstr)
{
  String str(cstr);
  _encoder->encode(_out, &str);
}

void 
CharToByteWriter::writeString(const ucchar* cstr)
{
  String str(cstr);
  _encoder->encode(_out, &str);
}

void 
CharToByteWriter::writeString(IN(RString) str)
{
  _encoder->encode(_out, str);
}


} // io
} // acdk


