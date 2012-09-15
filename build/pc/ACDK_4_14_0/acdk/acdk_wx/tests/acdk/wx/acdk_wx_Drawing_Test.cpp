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
// $Header: /cvsroot/acdk/acdk/acdk_wx/tests/acdk/wx/acdk_wx_Drawing_Test.cpp,v 1.6 2005/03/08 18:56:26 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>
#include <acdk/wx/App.h>
#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/Panel.h>
#include <acdk/wx/FileDropTarget.h>
#include <acdk/wx/TextDropTarget.h>
#include <acdk/wx/DropSource.h>
#include <acdk/wx/SplitterWindow.h>
#include <acdk/wx/Button.h>
#include <acdk/wx/PaintDC.h>
#include <acdk/wx/Timer.h>
#include "TestFrame.h"

namespace tests {
namespace acdk {
namespace wx {

  
BEGIN_DECLARE_TEST( Drawing_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( Drawing_Test  )

BEGIN_DEFINE_TEST( Drawing_Test )
  ADD_TEST( Drawing_Test, standard ) 
END_DEFINE_TEST( Drawing_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::wx;

namespace {

ACDK_DECL_CLASS(MyFrame);
class MyFrame
: extends TestFrame
{
public:
  MyFrame()
  {
    ACDK_SAFE_CONSTRUCTOR();
    connect(PaintEvent::EvtPaint, -1, (ObjectEventFunction)&MyFrame::onPaint);

  }
  void onPaint(IN(RPaintEvent) event)
  {
    PaintDC dc(this);
    dc.drawText("Hallo", 20, 20);
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
Drawing_Test::standard()
{
  RStringArray args = new StringArray(0);
  int ret = App::createGui(new MyApp(), args);
  sys::coreout << "ret: " << ret << sys::eofl;
}






} // namespace wx
} //namespace acdk 
} //namespace tests 

