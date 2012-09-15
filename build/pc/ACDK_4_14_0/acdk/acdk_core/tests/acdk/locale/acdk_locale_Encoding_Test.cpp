
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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/locale/acdk_locale_Encoding_Test.cpp,v 1.7 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/locale/Encoding.h>
#include <acdk/locale/UTF8Encoding.h>
#include <acdk/locale/IsoEncoding.h>

namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( Encoding_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( utf8Encoding )
END_DECLARE_TEST( Encoding_Test )

BEGIN_DEFINE_TEST( Encoding_Test )
  ADD_TEST( Encoding_Test, standard ) 
  ADD_TEST( Encoding_Test, utf8Encoding ) 
  
END_DEFINE_TEST( Encoding_Test )

using namespace ::acdk::locale;

void
Encoding_Test::standard()
{
  RStringArray encs = Encoding::getAvailableEncodings();
  System::out->println("Available Encodings: " + encs->toString());
}

RString encodeDecode(IN(RString) s, IN(REncoding) enc)
{
  ::acdk::io::MemWriter mout;
   enc->getEncoder()->encode(&mout, s);
  ::acdk::io::MemReader min(&mout);
  return enc->getDecoder()->decodeToString(&min);
}

void 
Encoding_Test::utf8Encoding()
{
  RString s = "Hallo ACDK";
  RString erg;
  /*
  erg = encodeDecode(s);
  testAssert(erg->equals(s) == true);
  */
  REncoding utf8enc = UTF8Encoding::getUTF8Encoding();
  s = _US("Stra\\u00f6en\\u00fcberasschung \\u00e9");
  erg = encodeDecode(s, utf8enc);
  testAssert(erg->equals(s) == true);

  REncoding isoenc = Encoding::getEncoding("8859-1");
  erg = encodeDecode(s, isoenc);
  testAssert(erg->equals(s) == true);
  isoenc = Encoding::getEncoding("8859-2");
  erg = encodeDecode(s, isoenc);
  testAssert(erg->equals(s) == true);
  RStringArray encodings = IsoEncoding::getAvailableEncodings();
  
}


} // namespace io
} // namespace acdk
} // namespace tests
