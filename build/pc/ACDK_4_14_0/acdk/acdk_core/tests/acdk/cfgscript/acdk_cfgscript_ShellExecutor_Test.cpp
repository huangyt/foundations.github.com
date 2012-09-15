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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/cfgscript/acdk_cfgscript_ShellExecutor_Test.cpp,v 1.3 2005/04/26 22:08:11 kommer Exp $





#include <acdk/io/CharArrayReader.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/lang/System.h>
#include <acdk/cfgscript/ShellExecutor.h>

#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace cfgscript {

BEGIN_DECLARE_TEST( ShellExecutor_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( viaProps )
END_DECLARE_TEST( ShellExecutor_Test  )

BEGIN_DEFINE_TEST( ShellExecutor_Test )
  ADD_TEST( ShellExecutor_Test, standard )
  ADD_TEST( ShellExecutor_Test, viaProps )
END_DEFINE_TEST( ShellExecutor_Test )


using namespace ::acdk::util::logging;

using namespace ::acdk::cfgscript;

void
ShellExecutor_Test::standard()
{
  RString cmd;
  RString acdkhome = System::getAcdkHome();
  if (System::getPlatformFlags() & PfWin32)
    cmd = "dir " + acdkhome;
  else
    cmd = "ls " + acdkhome;
  ShellExecutor exec(cmd, 0);
  RProps props = new Props();
  exec.execute(props);
  RString out = exec.getOutString();
  RString err = exec.getErrString();
  System::out->println("executed " + cmd + ":\n" + out);
}

void
ShellExecutor_Test::viaProps()
{
  RString cmd;
  RString acdkhome = System::getAcdkHome();
  if (System::getPlatformFlags() & PfWin32)
    cmd = "dir " + acdkhome;
  else
    cmd = "ls " + acdkhome;
  RProps p = new Props();
  cmd = "`" + cmd + "`";
  RString out = p->eval(cmd);
  System::out->println("executed: " + cmd + ":\n" + out);
}

} // namespace cfgscript
} // namespace acdk
} // namespace tests



