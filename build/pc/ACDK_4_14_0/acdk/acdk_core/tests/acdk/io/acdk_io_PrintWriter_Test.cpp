
// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This PrintWriter is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_PrintWriter_Test.cpp,v 1.10 2004/11/21 21:56:32 kommer Exp $

#include <acdk.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>

namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( PrintWriter_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( quoted )
  DECLARE_TEST( inputReader )
END_DECLARE_TEST( PrintWriter_Test )

BEGIN_DEFINE_TEST( PrintWriter_Test )
  ADD_TEST( PrintWriter_Test, standard ) 
  ADD_TEST( PrintWriter_Test, quoted ) 
  ADD_TEST( PrintWriter_Test, inputReader ) 
  
END_DEFINE_TEST( PrintWriter_Test )

using namespace ::acdk::io;

void
PrintWriter_Test::quoted()
{
  RString str = "This is a quoted\n\"Text\".";
  MemWriter mout;
  PrintWriter pout((RWriter)&mout);

  pout.printQuoted(str);
  pout.println("garbage");
  MemReader min(mout.getBuffer());
  InputReader in((RReader)&min);
  RString ret = in.readQuoted();
  testAssert(ret->equals(str) == true);
}

void
PrintWriter_Test::standard()
{
  RString sval = "asdf";
  RObject obj;

  {
    MemWriter mem;
    PrintWriter pw((RWriter)&mem);
    pw << " ";
    pw << sval;
    pw << int(42);
    pw << new String("asdf") << endln;
  }
}

void PrintWriter_Test::inputReader()
{
  /*
  int i;
  do { 
    int i = ::getchar();
    ::acdk::lang::System::out->println(SBSTR("<<: " << i << "," << (char)i));
  } while (i > 0);
  */


  //RString s = ::acdk::lang::System::in->readLine();
  //::acdk::lang::System::out->println(s);
}

} // namespace io
} // namespace acdk
} // namespace tests
