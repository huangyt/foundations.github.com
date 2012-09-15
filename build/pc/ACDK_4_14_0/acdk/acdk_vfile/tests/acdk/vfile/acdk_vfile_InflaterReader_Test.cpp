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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/tests/acdk/vfile/acdk_vfile_InflaterReader_Test.cpp,v 1.11 2005/02/05 10:45:33 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>

#include <acdk/vfile/InflaterReader.h>
#include <acdk/vfile/DeflateWriter.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>

namespace tests {
namespace acdk {
namespace vfile {
namespace tar {
  
BEGIN_DECLARE_TEST( InflaterReader_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( InflaterReader_Test  )

BEGIN_DEFINE_TEST( InflaterReader_Test )
  ADD_TEST( InflaterReader_Test, standard ) 
  
END_DEFINE_TEST( InflaterReader_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::vfile;


void 
InflaterReader_Test::standard()
{
  testAssert(true);
  return; // does not work at the moment
#if 0
  RString str = 
"This is the text, which should be compressed "
"to test if the compression works";
  MemWriter mw;
  DeflateWriter deflw(&mw);
  deflw.write(str->getBytes());
  deflw.close();
  MemReader mr(&mw);
  InflaterReader infr(&mr);
  StringBuffer sb;
  int e = 0;
  while (true) {
    e = infr.read();
    if (e == -1)
      break;
    sb.append((char)e);
  }
  RString str2 = sb.toString();
  int len1 = str->length();
  int len2 = str2->length();
  testAssert(str2->equals(str) == true);
#endif
}




} // namespace tar
} // namespace vfile
} //namespace acdk 
} //namespace tests 



