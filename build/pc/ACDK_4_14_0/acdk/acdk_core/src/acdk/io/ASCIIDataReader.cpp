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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ASCIIDataReader.cpp,v 1.8 2005/03/08 12:45:35 kommer Exp $




#include <acdk.h>
#include "ASCIIDataReader.h"

#include <acdk/lang/System.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace io {

//virtual 
bool 
ASCIIDataReader::readBoolean() // ### to implement, see also ASCIIDataWriter
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return false;
}

//virtual 
char 
ASCIIDataReader::readChar()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}

//virtual 
uc2char 
ASCIIDataReader::readUcChar()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}


//virtual 
double 
ASCIIDataReader::readDouble()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}

//virtual 
float 
ASCIIDataReader::readFloat()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}

//virtual 
int 
ASCIIDataReader::readInt()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}

//virtual 
jlong 
ASCIIDataReader::readLong()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}

//virtual 
short 
ASCIIDataReader::readShort()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return 0;
}

//virtual 
RString 
ASCIIDataReader::readString()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
  return Nil;
}

static 
void __dummyInstance()
{
  RASCIIDataReader br = new ASCIIDataReader((RReader)System::in);
}


} // io
} // acdk
