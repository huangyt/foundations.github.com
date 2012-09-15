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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/ASCIIDataWriter.cpp,v 1.12 2005/03/08 12:45:35 kommer Exp $




#include <acdk.h>
#include "ASCIIDataWriter.h"

#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/locale/AsciiUtfEncoding.h>

namespace acdk {
namespace io {


//virtual 
void 
ASCIIDataWriter::writeBoolean(bool b)
{
  char* truestr = "true";
  char* falsestr = "false";
  if (b == true)
    _out->write((const byte*)truestr, 0, strlen(truestr));
  else
    _out->write((const byte*)falsestr, 0, strlen(falsestr));
}

//virtual 
void 
ASCIIDataWriter::writeChar(char b)
{
  _out->write((const byte*)&b, 0, 1); 
}

//virtual 
void 
ASCIIDataWriter::writeUcChar(ucchar b)
{
  writeShort((short)b);
}


//virtual 
void 
ASCIIDataWriter::writeShort(short b)
{
  RString tstr = Short::toString(b);
  _out->write((const byte*)tstr->byte_begin(), 0, tstr->length()); // ### unicode
}

//virtual 
void 
ASCIIDataWriter::writeInt(int b)
{
  RString tstr = Integer::toString(b);
  _out->write((const byte*)tstr->byte_begin(), 0, tstr->length());
}

//virtual 
void 
ASCIIDataWriter::writeLong(jlong b) 
{
  RString tstr = Long::toString(b);
  _out->write((const byte*)tstr->byte_begin(), 0, tstr->length());
}

//virtual 
void 
ASCIIDataWriter::writeFloat(float b)
{
  RString tstr = Float::toString(b);
  _out->write((const byte*)tstr->byte_begin(), 0, tstr->length());
}

//virtual 
void 
ASCIIDataWriter::writeDouble(double b)
{
  RString tstr = Double::toString(b);
  _out->write((const byte*)tstr->byte_begin(), 0, tstr->length());
}

//virtual 
void
ASCIIDataWriter::write(IN(RbyteArray) array, int offset, int len)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual 
void 
ASCIIDataWriter::writeString(IN(RString) str)
{
  acdk::locale::REncoder enc = acdk::locale::AsciiUtfEncoding::getAsciiUtfEncoding()->getEncoder();
  enc->encode(this, str);

}

static 
void __dummyInstance()
{
  RASCIIDataWriter br = new ASCIIDataWriter((RWriter)System::out);
}


} // io
} // acdk






