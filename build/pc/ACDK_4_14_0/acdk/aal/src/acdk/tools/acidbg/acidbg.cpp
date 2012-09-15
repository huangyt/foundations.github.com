// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/tools/acidbg/acidbg.cpp,v 1.5 2005/02/05 10:44:51 kommer Exp $

#include <acdk/aci/StdAci.h>
#include "AciDbgFrame.h"
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/text/RegExp.h>

namespace acdk {
namespace tools {
namespace acidbg {




class AciDbg
: extends acdk::wx::App
{
public:
  void setupLogger()
  {


  using namespace ::acdk::util::logging;
  static RLogger scannerlog = Nil;
  static RLogger parserlog = Nil;
  if (scannerlog != Nil)
    return;
  scannerlog = LogManager::getCreateLogger("acdk.aci");
  scannerlog->addConsumer(new ConsoleConsumer(new SimpleFormatter()));
  LogManager::MinLevel = acdk::util::logging::All;//Debug; //Trace;
  LogManager::Threshold = acdk::util::logging::All; //Trace;
  
}

  bool onInit()
  {
    setupLogger();
    acdk::wx::RFrame win = new AciDbgFrame("ACI/AAL Debugger");
    win->show(true);
    return true;
  }
  static int main(RStringArray args)
  {
    return acdk::wx::App::createGui(new AciDbg(), args);
  }
};

} // namespace acidbg
} // namespace tools 
} // namespace acdk 


#if defined(ACDK_OS_WIN32) && defined(THIS_IS_NOT_DEFINED)

int PASCAL WinMain(HINSTANCE hInstance,
                   HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine,
                   int nCmdShow)
{            
  return acdk::lang::System::main(::acdk::tools::acidbg::AciDbg::main, hInstance, hPrevInstance, lpCmdLine, nCmdShow);
  return -1;
}        
#else // defined(ACDK_OS_WIN32)

int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(::acdk::tools::acidbg::AciDbg::main, argc, argv, envptr);
}

#endif //defined(ACDK_OS_WIN32)


