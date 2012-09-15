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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ByteToCharReader.cpp,v 1.6 2005/02/05 10:44:53 kommer Exp $

#include "ByteToCharReader.h"
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace io {


ByteToCharReader::ByteToCharReader(IN(RReader) in, IN(acdk::locale::RDecoder) decoder, IN(RObject) iolock)
: AbstractCharReader(iolock)
, _decoder(decoder)
, _in(in)
{
  if (_decoder == Nil) 
    _decoder = acdk::locale::Encoding::getEncoding()->getDecoder();
}

int 
ByteToCharReader::readChar()
{
  return _decoder->decodeToChar(_in);
}

RString 
ByteToCharReader::readString()
{
  return _decoder->decodeToString(_in);
}



} // io
} // acdk



