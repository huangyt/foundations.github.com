
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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_StreamTokenizer_Test.cpp,v 1.5 2005/02/05 10:45:08 kommer Exp $

#include <acdk.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/StreamTokenizer.h>

namespace tests {
namespace acdk {
namespace io {

using namespace ::acdk::io;

BEGIN_DECLARE_TEST( StreamTokenizer_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( cmdline )
END_DECLARE_TEST( StreamTokenizer_Test )

BEGIN_DEFINE_TEST( StreamTokenizer_Test )
  ADD_TEST( StreamTokenizer_Test, standard ) 
  ADD_TEST( StreamTokenizer_Test, cmdline ) 
  
END_DEFINE_TEST( StreamTokenizer_Test )


void
StreamTokenizer_Test::standard()
{
  RString text = "+-i";
  StringReader rin(text);
  StreamTokenizer tin(&rin);
  int c = tin.nextToken();
  testAssert(c == '+');
  c = tin.nextToken();
  testAssert(c == '-');
  c = tin.nextToken();

}



RStringArray 
parseCommand(IN(RString) command, IN(RStringArray) _args)
{
  acdk::io::StringReader cin(command);
  acdk::io::StreamTokenizer tin(&cin);
  tin.readCxxIdentifier(false);
  tin.readOperator(false);
  tin.readNumberAsString(false);
  tin.wantWhiteSpace(true);
  tin.parseCCComments(false);
  int tk;
  StringBuffer sb;
  while ((tk = tin.nextToken()) != acdk::io::StreamTokenizer::TT_EOF)
  {
    if (tk == acdk::io::StreamTokenizer::TT_WS)
    {
      _args->append(sb.toString());
      sb.reset();
    }
    else
    {
      sb.append(tin.sval);
    }
  }
  if (sb.toString()->length() > 0)
    _args->append(sb.toString());
  return _args;
}

void
StreamTokenizer_Test::cmdline()
{
  RString cmd = "a /ab/c";
  RStringArray sa = new StringArray(0);
  parseCommand(cmd, sa);
}

} // namespace io
} // namespace acdk
} // namespace tests
