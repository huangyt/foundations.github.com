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
// $Header: /cvsroot/acdk/acdk/acdk_make/tests/acdk/make/acdk_make_CppSourceDependTask.cpp,v 1.7 2005/03/31 21:24:34 kommer Exp $



#include <acdk/make/CppSourceDependTask.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/lang/System.h>
#include <acdk/cfgscript/Script.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace make {
  
BEGIN_DECLARE_TEST( CppSourceDependTask_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( CppSourceDependTask_Test  )

BEGIN_DEFINE_TEST( CppSourceDependTask_Test )
  ADD_TEST( CppSourceDependTask_Test, standard ) 
  
END_DEFINE_TEST( CppSourceDependTask_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::make;
using namespace ::acdk::cfgscript;

void 
CppSourceDependTask_Test::standard()
{
  RLogger acdk_make_logger = new Logger("acdk.make");
  acdk_make_logger->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::Threshold = LogManager::MinLevel = Info;
  RString s = System::getAcdkHome() + ::acdk::io::File::separator() + "include/acdk/make/CppSourceDependTask.h";
  CppSourceDependTask cspt(s, s, true, false);
  cspt.addIncludeDir(System::getAcdkHome() + ::acdk::io::File::separator() + "include");

  Props noprops;
  bool berg = cspt.execute("", &noprops);

}

} // namespace make
} // namespace acdk
} // namespace tests



