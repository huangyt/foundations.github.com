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
// $Header: /cvsroot/acdk/acdk/acdk_wx/tests/acdk/wx/acdk_wx_TextCtrl_Test.cpp,v 1.10 2005/04/21 09:52:30 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>

#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/Panel.h>
#include "TestFrame.h"

namespace tests {
namespace acdk {
namespace wx {

  
BEGIN_DECLARE_TEST( TextCtrl_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( nativeWx )
END_DECLARE_TEST( TextCtrl_Test  )

BEGIN_DEFINE_TEST( TextCtrl_Test )
  ADD_TEST( TextCtrl_Test, standard ) 
  ADD_TEST( TextCtrl_Test, nativeWx ) 
  
  
END_DEFINE_TEST( TextCtrl_Test )

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
    
    RPanel panel = new Panel(this);
    //RTextCtrl text = new TextCtrl(&panel, -1, "Hello");
    //RTextCtrl text = new TextCtrl(this, -1, "Hello");
    //wxTextCtrl* text = new wxTextCtrl(this->getWx(), -1, "Hello");
    wxTextCtrl* text = new wxTextCtrl(panel->getWx(), -1, _T("Hello"));
    //text->show(true);
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
TextCtrl_Test::standard()
{
  RStringArray args = new StringArray(0);
  int ret = App::createGui(new MyApp(), args);
  sys::coreout << "ret: " << ret << sys::eofl;
}


namespace {

class MyWxFrame
  :   public wxFrame
{
public:
  MyWxFrame()
    : wxFrame(0, -1, _T("Hello"))
  {
    //wxTextCtrl* text = new wxTextCtrl(this, 1001, _T("Hello"));
    wxTextCtrl* text = new wxTextCtrl(this, 1001, _T("Hallo"));
    //wxButton* button = new wxButton(this, -1, _T("Hello"));

  }
};



}

class MyWxApp
: extends wxApp
{
public:
  bool OnInit()
  {
    MyWxFrame* win = new MyWxFrame();
    win->Show(true);
    return true;
  }
};

#if defined(ACDK_OS_WIN32) 
wxApp* wxCreateApp()
#else
wxObject* wxCreateApp()
#endif
{
#if ACDK_CHECK_WX_VERSION(2, 6)
  wxAppConsole::CheckBuildOptions(WX_BUILD_OPTIONS_SIGNATURE, "acdk_wx");
#else
  wxApp::CheckBuildOptions(wxBuildOptions());
#endif
  return new tests::acdk::wx::MyWxApp();
}


void
TextCtrl_Test::nativeWx()
{
#if !ACDK_CHECK_WX_VERSION(2, 5)
  if (::acdk::tools::aunit::TestCase::TestInBatchMode == true)
      return;
  wxApp::SetInitializerFunction(wxCreateApp);
  
#if defined(ACDK_OS_WIN32) !ACDK_CHECK_WX_VERSION(2, 5)
  int ret = wxEntry((WXHINSTANCE) GetModuleHandle(0), (WXHINSTANCE) 0, "", SW_SHOW);
#else
  int argc = System::getArgc();
  char** argv = System::getArgv();
  int ret = wxEntry(System::getArgc(),  System::getArgv());
#endif
#endif // !ACDK_CHECK_WX_VERSION(2, 5)
}



} // namespace wx
} //namespace acdk 
} //namespace tests 

