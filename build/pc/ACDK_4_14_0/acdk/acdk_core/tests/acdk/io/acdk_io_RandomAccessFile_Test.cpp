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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_RandomAccessFile_Test.cpp,v 1.9 2005/02/05 10:45:08 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Byte.h>

#include <acdk/io/File.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/ConsoleWriter.h>
#include <acdk/io/ConsoleReader.h>
#include <acdk/io/RandomAccessFile.h>
#include <acdk/io/EOFException.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( RandomAccessFile_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( fileReader )
END_DECLARE_TEST( RandomAccessFile_Test  )

BEGIN_DEFINE_TEST( RandomAccessFile_Test )
  ADD_TEST( RandomAccessFile_Test, standard ) 
  ADD_TEST( RandomAccessFile_Test, fileReader ) 
END_DEFINE_TEST( RandomAccessFile_Test )


using namespace ::acdk::lang;
using namespace ::acdk::io;

void 
RandomAccessFile_Test::standard()
{
  RString fname = "./RandomAccessFile.file";
  {
    RandomAccessFile rw(fname, "w");
    int i;
    for (i = Byte::MIN_VALUE; i < Byte::MAX_VALUE; ++i)
    {
      rw.write((byte)i);
    }
  }
  {
    bool catched = false;
    RandomAccessFile rw(fname, "r");
    for (int i = Byte::MIN_VALUE; i < Byte::MAX_VALUE; ++i)
    {
      int readed = rw.read();
      testAssert(readed == i);
    }
    int readed = rw.read();
    testAssert(readed == -1);
    try {
      rw.read();
      testAssert(0 == "Exception expected");
    } catch (REOFException ex) {
      catched = true;
    } 
  }
  {
    int numbytes = Byte::MAX_VALUE - Byte::MIN_VALUE;
    byteArray ba(numbytes);
    RandomAccessFile rw(fname, "r");
    int readed = rw.read(&ba);
    testAssert(numbytes == readed);

    for (int i = 0; i < numbytes; ++i)
    {
      testAssert(ba[i] == Byte::MIN_VALUE + i);
    }
  }
  File(fname).deleteFile();
}

void
RandomAccessFile_Test::fileReader()
{
  RString fname = "./RandomAccessFile.file";
  {
    // create file
    RandomAccessFile rw(fname, "w");
    int i;
    for (i = Byte::MIN_VALUE; i < Byte::MAX_VALUE; ++i)
    {
      rw.write((byte)i);
    }
  }
  File file(fname);
  testAssert(file.setReadOnly(true) == true);
  testAssert(file.canWrite() == false);
  {
    FileReader fin(fname);
    RbyteArray ba = fin.readAll();
  }
  testAssert(file.setReadOnly(false) == true);
  testAssert(file.deleteFile() == true);
}

} // namespace io
} // namespace acdk
} // namespace tests

