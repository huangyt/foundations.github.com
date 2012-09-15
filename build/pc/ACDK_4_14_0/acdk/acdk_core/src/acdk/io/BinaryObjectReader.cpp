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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryObjectReader.cpp,v 1.8 2005/03/07 13:53:22 kommer Exp $

#include "BinaryObjectReader.h"
#include "ObjectStreamException.h"

namespace acdk {
namespace io {

//virtual 
RString 
BinaryObjectReader::readTagStart(IN(RString) , IN(RString) value /*= Nil*/)
{
  // ignore key
  RString str = readString();
  if (value && value->equals(str) == false) {
      THROW1(StreamCorruptedException, 
        "Class in Stream does not match expected: Readed=[" + str + "]; Expected=[" + value + "];");
  }
  return str;
}

//virtual 
RString 
BinaryObjectReader::readTagEnd(IN(RString) , IN(RString) val/*= Nil*/)
{
    // intentionally left blank 
  return val;
}

//virtual 
RClass 
BinaryObjectReader::readClassId()
{
  RString cl = readString();
  if (cl->equals("Nil") == true)
    return Nil;
  RClass cls = Class::forName(cl);
  if (withSerialVersionUID() == true)
  {
    jlong isVersion = cls->getSerialVersionUID();
    jlong shallVersion = readLong();
    if (isVersion != shallVersion)
      THROW0(InvalidClassException);
  }
  return cls;
}



} // io
} // acdk


