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
// $Header: /cvsroot/acdk/acdk/acdk_wx/tests/acdk/wx/acdk_wx_DragNDrop_Test.cpp,v 1.6 2005/03/08 18:56:26 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>
#include "TestFrame.h"

#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/Panel.h>
#include <acdk/wx/FileDropTarget.h>
#include <acdk/wx/TextDropTarget.h>
#include <acdk/wx/DropSource.h>
#include <acdk/wx/SplitterWindow.h>
#include <acdk/wx/Button.h>

namespace tests {
namespace acdk {
namespace wx {

  
BEGIN_DECLARE_TEST( DragNDrop_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( DragNDrop_Test  )

BEGIN_DEFINE_TEST( DragNDrop_Test )
  ADD_TEST( DragNDrop_Test, standard ) 
END_DEFINE_TEST( DragNDrop_Test )

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::wx;

namespace {

ACDK_DECL_CLASS(MyFileDropTarget);

class MyFileDropTarget
: extends FileDropTarget
{
public:
  MyFileDropTarget() {}
  ::acdk::wx::DragResult onEnter(int x, int y, ::acdk::wx::DragResult def) 
  { 
    ::acdk::lang::System::out->println("onEnter");
    //super.onEnter(x, y, def);
    return DragCopy;
    //return def;  
  }
  
  ::acdk::wx::DragResult onDragOver(int x, int y, ::acdk::wx::DragResult def) 
  { 
    ::acdk::lang::System::out->println("onDragOver");
    //super.onDragOver(x, y, def);
    return DragCopy;
    //return def;
  }
  void onLeave() 
  { 
    ::acdk::lang::System::out->println("onLeave"); 
  }
  /*bool onDrop(int x, int y) 
  { 
     ::acdk::lang::System::out->println("onDrop"); 
  }
 
  bool onDropText(int x, int y, String text)
  {
    ::acdk::lang::System::out->println("onDropText: " + text); 
    return true;
  }
   */
  bool onDropFiles(int x, int y, IN(RStringArray) filenames) 
  {
    ::acdk::lang::System::out->println("onDropFiles: " + filenames->toString());
    return true;
  }
};

ACDK_DECL_CLASS(MyTextDropTarget);
class MyTextDropTarget
: extends TextDropTarget
{
public:
  MyTextDropTarget() {}
  ::acdk::wx::DragResult onEnter(int x, int y, ::acdk::wx::DragResult def) 
  { 
    ::acdk::lang::System::out->println("onEnter");
    return DragCopy;
  }
  
  ::acdk::wx::DragResult onDragOver(int x, int y, ::acdk::wx::DragResult def) 
  { 
    ::acdk::lang::System::out->println("onDragOver");
    return DragCopy;
  }
  void onLeave() 
  { 
    ::acdk::lang::System::out->println("onLeave"); 
  }
  bool onDropText(int x, int y, IN(RString) text)
  {
    ::acdk::lang::System::out->println("onDropText: " + text); 
    return true;
  }
};

ACDK_DECL_CLASS(MyFrame);
class MyFrame
: extends TestFrame
{
public:
  RMyFileDropTarget mfdt;
  RMyTextDropTarget mtdt;
  MyFrame()
  {
    ACDK_SAFE_CONSTRUCTOR();
    
    RSplitterWindow splitter = new SplitterWindow(this);
    RButton btn1 = new Button(&splitter, -1, "Drop Files here");
    RButton btn2 = new Button(&splitter, -1, "Drop Text here");
    splitter->splitVertically(&btn1, &btn2);
    mfdt = new MyFileDropTarget();
    //mdt->setDataObject(new TextDataObject());
    btn1->setDropTarget(&mfdt);
    mtdt = new MyTextDropTarget();
    btn2->setDropTarget(&mtdt);
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
DragNDrop_Test::standard()
{
  RStringArray args = new StringArray(0);
  int ret = App::createGui(new MyApp(), args);
  sys::coreout << "ret: " << ret << sys::eofl;
}






} // namespace wx
} //namespace acdk 
} //namespace tests 

