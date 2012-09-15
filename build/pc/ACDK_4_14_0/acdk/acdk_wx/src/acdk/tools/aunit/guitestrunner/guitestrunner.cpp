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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/tools/aunit/guitestrunner/guitestrunner.cpp,v 1.6 2005/02/06 16:35:34 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/wx/App.h>
#include <acdk/wx/Frame.h>
#include "TestRunnerFrame.h"


namespace acdk {
namespace tools {
namespace aunit {
namespace guitestrunner {




class GuiTestRunner
: extends acdk::wx::App
{
public:
  bool onInit()
  {
    acdk::wx::RFrame win = new TestRunnerFrame("ACDK Test Runner");
    win->show(true);
    return true;
  }
  static int acdkMain(RStringArray args)
  {
    return acdk::wx::App::createGui(new GuiTestRunner(), args);
  }
};

} // namespace guitestrunner 
} // namespace aunit
} // namespace tools 
} // namespace acdk 


#if defined(ACDK_OS_WIN32) && defined(THIS_IS_NOT_DEFINED)

int PASCAL WinMain(HINSTANCE hInstance,
                   HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine,
                   int nCmdShow)
{            
  return acdk::lang::System::main(::acdk::tools::aunit::guitestrunner::GuiTestRunner::main, hInstance, hPrevInstance, lpCmdLine, nCmdShow);
  return -1;
}        
#else // defined(ACDK_OS_WIN32)

int
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(::acdk::tools::aunit::guitestrunner::GuiTestRunner::acdkMain, argc, argv, envptr);
}

#endif //defined(ACDK_OS_WIN32)


