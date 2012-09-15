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
// $Header: /cvsroot/acdk/acdk/acdk_wx/tests/acdk/wx/ide/acdk_wx_ide_StyledTextCtrl_Test.cpp,v 1.3 2005/03/07 23:46:25 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>

#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/Panel.h>
#include <acdk/wx/ide/StyledTextCtrl.h>


namespace tests {
namespace acdk {
namespace wx {
namespace ide {

  
BEGIN_DECLARE_TEST( StyledTextCtrl_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( StyledTextCtrl_Test  )

BEGIN_DEFINE_TEST( StyledTextCtrl_Test )
  ADD_TEST( StyledTextCtrl_Test, standard ) 
END_DEFINE_TEST( StyledTextCtrl_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::wx;
using namespace ::acdk::wx::ide;

namespace {

ACDK_DECL_CLASS(MyFrame);
class MyFrame
: extends Frame
{
  RStyledTextCtrl _edit;
public:
  MyFrame()
  {
    ACDK_SAFE_CONSTRUCTOR();
    
    RPanel panel = new Panel(this);
    _edit = new StyledTextCtrl(&panel, -1);
  }
};

class MyApp
: extends App
{
public:
  bool onInit()
  {
    RMyFrame win = new MyFrame();
    win->show(true);
    return true;
  }
};

} // anon namespace

void 
StyledTextCtrl_Test::standard()
{
  RStringArray args = new StringArray(0);
  int ret = App::createGui(new MyApp(), args);
  sys::coreout << "ret: " << ret << sys::eofl;
}



} // namespace ide
} // namespace wx
} //namespace acdk 
} //namespace tests 

