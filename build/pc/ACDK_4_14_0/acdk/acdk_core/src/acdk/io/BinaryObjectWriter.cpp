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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BinaryObjectWriter.cpp,v 1.9 2005/03/07 13:53:22 kommer Exp $


#include <acdk.h>
#include "BinaryObjectWriter.h"


namespace acdk {
namespace io {


//virtual 
void 
BinaryObjectWriter::writeTagStart(IN(RString) key, IN(RString) value /*= Nil*/)
{
  writeString(key);
  if (value != Nil)
    writeString(value);
}

//virtual 
void 
BinaryObjectWriter::writeTagEnd(IN(RString) , IN(RString) /*= Nil*/)
{
  // intentionally left blank  
}

//virtual 
void 
BinaryObjectWriter::writeClassId(IN(::acdk::lang::RClass) cls)
{
  if (cls == Nil)
    writeString("Nil");
  else
  {
    writeString(cls->getName());
    if (withSerialVersionUID() == true)
    {
      jlong version = cls->getSerialVersionUID();
      writeLong(version);
    }
  }
}


} // io
} // acdk



