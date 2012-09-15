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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_ReaderWriter_Test.cpp,v 1.7 2005/04/25 23:25:19 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/ConsoleWriter.h>
#include <acdk/io/ConsoleReader.h>
#include <acdk/io/StringWriter.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/BufferedWriter.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/locale/Encoding.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( ReaderWriter_Test )
  DECLARE_TEST( writerToCharWriter )
  DECLARE_TEST( charWriterToWriter )
END_DECLARE_TEST( ReaderWriter_Test  )

BEGIN_DEFINE_TEST( ReaderWriter_Test )
  ADD_TEST( ReaderWriter_Test, writerToCharWriter ) 
  ADD_TEST( ReaderWriter_Test, charWriterToWriter ) 
END_DEFINE_TEST( ReaderWriter_Test )


  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::locale;

void readWriteTextViaWriter(IN(RString) text)
{
  MemWriter mout;
  RCharWriter cout = mout.getCharWriter();
  cout->writeString(text);
  RbyteArray buf = mout.getBuffer();
  MemReader memin(buf);
  RCharReader cin = memin.getCharReader();
  RString erg = cin->readString();
  if (text->equals(erg) == false)
  {
    System::out->println(SBSTR("text!=erg: text=[\n" << text << "]; erg=[\n" << erg << "];"));
  }
  testAssert(text->equals(erg) == true);

}

void 
ReaderWriter_Test::writerToCharWriter()
{
  RString asciiText = "Hallo this is a 7bit text";
  readWriteTextViaWriter(asciiText);
  REncoding stdencoding = Encoding::getEncoding();
  Encoding::setEncoding(Encoding::getEncoding("latin-1"));
  RString ucodeText = _US("Hallo This is a 8bit Unicode Stra\\u00dfen\\u00e4berasschung \\u00e9");
  readWriteTextViaWriter(ucodeText);
  Encoding::setEncoding(stdencoding);
}



void readWriteTextViaCharWriter(const char* text)
{

  RStringWriter sout = new StringWriter();
  RWriter bout = sout->getWriter();
  int textlen = strlen(text);
  bout->write((const byte*)text, 0, textlen);
  MemReader min((RbyteArray)new byteArray((const byte*)text, textlen));
  RCharReader cin = min.getCharReader();
  RString s = cin->readString();
  
  testAssert(s->equals(sout->getString()) == true);
  StringReader sin(s);
  RReader bin = sin.getReader();
  byteArray ba(textlen);
  bin->read(&ba);
  testAssert(ba.equals(min.getBuffer()) == true);
}
void
ReaderWriter_Test::charWriterToWriter()
{
  /* does not make sense any more
  ::acdk::lang::System::out->println("to out");
  ::acdk::lang::System::err->println("to err");
  const char* asciiText = "Hallo this is a 7bit text";
  readWriteTextViaCharWriter(asciiText);
  ::acdk::lang::System::setProperty("ENCODING", "latin-1");

  const char* latin1text = "Hallo This is a 8bit Unicode Stra\\u00dfen\\u00e4berasschung \\u00e9";
  readWriteTextViaCharWriter(latin1text);
  */

}

} // namespace io
} // namespace acdk
} // namespace tests

