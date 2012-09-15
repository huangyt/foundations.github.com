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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/io/acdk_io_BufferedWriter_Test.cpp,v 1.7 2005/02/05 10:45:08 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/ConsoleWriter.h>
#include <acdk/io/ConsoleReader.h>
#include <acdk/io/BufferedWriter.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace io {

BEGIN_DECLARE_TEST( BufferedWriter_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( BufferedWriter_Test  )

BEGIN_DEFINE_TEST( BufferedWriter_Test )
  ADD_TEST( BufferedWriter_Test, standard ) 
END_DEFINE_TEST( BufferedWriter_Test )


  using namespace ::acdk::lang;
  using namespace ::acdk::io;

void 
BufferedWriter_Test::standard()
{
  /* ### rewrite with senseful code
  RString filename = "none";
  RWriter tout = new BufferedWriter(new NullWriter());
  RbyteArray cont = File(args[1]).loadBinary();
  tout->write(cont);
  */
}

} // namespace io
} // namespace acdk
} // namespace tests

