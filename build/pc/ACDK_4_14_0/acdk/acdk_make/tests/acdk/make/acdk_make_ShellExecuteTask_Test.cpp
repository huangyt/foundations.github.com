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
// $Header: /cvsroot/acdk/acdk/acdk_make/tests/acdk/make/acdk_make_ShellExecuteTask_Test.cpp,v 1.6 2005/02/05 10:45:29 kommer Exp $


#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/make/ShellExecuteTask.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/lang/System.h>
#include <acdk/cfgscript/Script.h>
#include <acdk/io/File.h>


namespace tests {
namespace acdk {
namespace make {
  
BEGIN_DECLARE_TEST( ShellExecuteTask_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( viaShell )
  DECLARE_TEST( viaShellAndRedirect )
END_DECLARE_TEST( ShellExecuteTask_Test  )

BEGIN_DEFINE_TEST( ShellExecuteTask_Test )
  ADD_TEST( ShellExecuteTask_Test, standard ) 
  ADD_TEST( ShellExecuteTask_Test, viaShell ) 
  ADD_TEST( ShellExecuteTask_Test, viaShellAndRedirect ) 
  
END_DEFINE_TEST( ShellExecuteTask_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::make;
using namespace ::acdk::cfgscript;
using namespace ::acdk::io;

void 
ShellExecuteTask_Test::standard()
{
  RString acdkmc;
  RString acdkhome = System::getAcdkToolsHome() + File::separator() + "bin";
#if defined(ACDK_OS_WIN32)
  RString ts;
  GlobFilenameFilter filter("acdkmc*.exe");
  RStringArray files = File(acdkhome).list((RFilenameFilter)&filter);
  if (files->length() < 1)
  {
    System::out->println("ShellExecuteTask_Test::standard: cannot find acdkm*.exe");
    return;
  }
  acdkmc = acdkhome + File::separator() + files[0];
#else
  acdkmc = acdkhome + File::separator() + "acdkmc";
#endif
  ShellExecuteTask shexec("acdmc", "Unit Test ShellExecuteTask", acdkmc + " -help", 0);
  RProps props = new Props();
  shexec.execute("", props);
}

void
ShellExecuteTask_Test::viaShell()
{
#if defined(ACDK_OS_WIN32)
  RString cmd = "dir";
#else
  RString cmd = "ls";
#endif

  ShellExecuteTask shexec("acdmc", "Unit Test ShellExecuteTask", cmd, SExecUseShell);
  RProps props = new Props(PropsNoFlags, Nil, false);
  shexec.execute("", props);
}

void
ShellExecuteTask_Test::viaShellAndRedirect()
{
  #if defined(ACDK_OS_WIN32)
  RString cmd = "dir";
#else
  RString cmd = "ls";
#endif

  ShellExecuteTask shexec("acdmc", "Unit Test ShellExecuteTask", cmd, SExecUseShell | SExecUseFileRedirect);
  RProps props = new Props(PropsNoFlags, Nil, false);
  shexec.execute("", props);
}

} // namespace make
} // namespace acdk
} // namespace tests



