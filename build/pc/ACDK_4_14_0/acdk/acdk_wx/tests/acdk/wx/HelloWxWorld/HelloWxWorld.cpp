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
// $Header: /cvsroot/acdk/acdk/acdk_wx/tests/acdk/wx/HelloWxWorld/HelloWxWorld.cpp,v 1.10 2005/03/08 18:56:26 kommer Exp $

#include <acdk/wx/wx.h>

#include <acdk/wx/Frame.h>
#include <acdk/wx/Dialog.h>
#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/StaticText.h>
#include <acdk/wx/LayoutConstraints.h>
#include <acdk/wx/Sizer.h>
#include <acdk/wx/Button.h>
#include <acdk/wx/TreeCtrl.h>
#include <acdk/lang/System.h>

#include "ObjectExplorer.h"

namespace tests {
namespace acdk {
namespace wx {
namespace HelloWorld {

using namespace ::acdk::wx;

// IDs for the controls and the menu commands
enum
{
    // menu items
    Event_Quit = 1,
    Event_About,
    Event_Connect,
    Event_Dynamic,
    Event_Push,
    Event_Pop,
    Event_Custom,
    Event_Test,
    Event_ObjectExplorer
};

ACDK_DECL_CLASS(MyButton);

class MyButton
: extends Button
{
public:
  MyButton(IN(RWindow) parent, IN(RString) str) 
  : Button(parent, -2, str) 
  {
    connect(CommandEvent::EvtCommandButtonClicked, -1, (ObjectEventFunction)&MyButton::onClicked);
  }
  ~MyButton()
  {
  }
  void onClicked(IN(REvent) event)
  {
    getParent()->close(true);
  }
};

ACDK_DECL_CLASS(AboutDialog);
class AboutDialog
: extends Dialog
{
  RButton _okButton;
  RStaticText _label;
  //wxFileDialog
public:
  AboutDialog(IN(RWindow) parent)
    : Dialog(parent, -1, "About", Point::defaultPosition(), Size::defaultSize()/*, DefaultFrameStyle*/)
  {
    ACDK_SAFE_CONSTRUCTOR();

    setAutoLayout(true);
    RLayoutConstraints layout = new LayoutConstraints();
    layout->top()->sameAs(this, Top, 10);
    layout->centreX()->sameAs(this, CentreX);
    layout->width()->asIs();
    layout->height()->asIs();
    _label = new StaticText(this, -1, "Hello");
    _label->setConstraints(layout);

    layout = new LayoutConstraints();
    layout->top()->below(&_label);
    layout->centreX()->sameAs(this, CentreX);
    layout->width()->asIs();
    layout->height()->asIs();
    _okButton = new Button(this, wxID_OK, "Close");
    _okButton->setConstraints(layout);

    RTreeCtrl treectrl = new TreeCtrl(this);
    RTreeItemId rid = treectrl->addRoot("RootItem");
    treectrl->appendItem(rid, "First");
    
    layout = new LayoutConstraints();
    layout->top()->below(&_okButton);
    layout->centreX()->sameAs(this, CentreX);
    layout->width()->sameAs(this, Width);
    layout->bottom()->sameAs(this, Bottom);
    treectrl->setConstraints(layout);
  }
  
};

ACDK_DECL_CLASS(MyFrame);
class MyFrame
: public Frame
{
  RTextCtrl textControl;
  //RMyButton buttonControl;
public:
  MyFrame(IN(RString) str) 
  : Frame(str) 
  {
    ACDK_SAFE_CONSTRUCTOR();
    
    RMenu menuFile = new Menu();
    menuFile->append(Event_About, "&About...\tCtrl-A", "Show about dialog");
    menuFile->append(Event_ObjectExplorer, "&Object Explorer...\tCtrl-O", "Show Object Explorer");
    menuFile->appendSeparator();
    menuFile->append(Event_Quit, "E&xit\tAlt-X", "Quit this program");
    RMenuBar menuBar = new MenuBar();
    menuBar->append(menuFile, "&File");
    setMenuBar(menuBar);
    /*
    connect(CommandEvent::EvtCommandMenuSelected, Event_Quit, (ObjectEventFunction)&MyFrame::onClose);
    connect(CommandEvent::EvtCommandMenuSelected, Event_About, (ObjectEventFunction)&MyFrame::onAbout);
    connect(CommandEvent::EvtCommandMenuSelected, Event_ObjectExplorer, (ObjectEventFunction)&MyFrame::onObjectExplorer);
    */
    //textControl = new TextCtrl(this, -1);
    //RMyButton buttonControl = new MyButton(this, "hello");
    setTitle("Test");
    
  }
  virtual void setTitle(IN(RString) title) 
  { 
    sys::coreout << "set title" << sys::eofl;
    Window::setTitle(title);
  }
  void onClose(IN(REvent) )
  {
    close(true);
    //messageBox("Closing");
    //disconnectAll();
  }
  void onAbout(IN(RCommandEvent) event)
  {
    RAboutDialog about = new AboutDialog(this);
    about->showModal();
    
  }
  void onObjectExplorer(IN(RCommandEvent) event)
  {
    RObjectExplorer oe = new ObjectExplorer(this);
    oe->showModal();
    //oe->disconnectAll(); // ### TODO do it into DialogFwd

  }
  
  void onAnEvent(IN(REvent) clsevent)
  {
    sys::coreout << "onAnEvent" << sys::eofl;
  }
  void onMove(IN(RMoveEvent) event)
  {
    RPoint pt = event->getPosition();
    sys::coreout << "Move: " << pt->x() << ":" << pt->y() << sys::eofl;
  }
};

ACDK_DECL_CLASS(Main);


class Main 
: extends App
{
public:
  bool onInit()
  {
    /*
    RDialog dlg = new Dialog(Nil, -1, "Hello, empty Dialog", Point::defaultPosition(), new Size(200, 200));
    dlg->showModal();
    return false;
    */
    RMyFrame win = new MyFrame("Hello Wx");
    //win->connect(wxEVT_CLOSE_WINDOW, wxID_ANY, (ObjectEventFunction)win->onClose);
    //win->connect(EvtMove, -1, (ObjectEventFunction)&MyFrame::onMove);
    //win->connect(EvtSize, -1, (ObjectEventFunction)&MyFrame::onMove);
    //win->connect(-1, wxEVT_IDLE, (ObjectEventFunction)&MyFrame::onIdle);
    win->show(true);
    //win->addRef();
    return true;
  }
};



/*
// status bar fields
enum
{
    Status_Main = 0,
    Status_Dynamic,
    Status_Push
};
class MyWxFrame
: public wxFrame
{
  //DECLARE_EVENT_TABLE()
  
public:
  //DECLARE_DYNAMIC_CLASS(MyWxFrame)

  MyWxFrame() : wxFrame(0, -1, wxString(_T("")))  {}
  MyWxFrame(const wxString& str) 
    : wxFrame(0, -1, str) 
  {
    wxMenu *menuFile = new wxMenu;
    menuFile->Append(Event_About, _T("&About...\tCtrl-A"), _T("Show about dialog"));
    menuFile->AppendSeparator();
    menuFile->Append(Event_Quit, _T("E&xit\tAlt-X"), _T("Quit this program"));
    wxMenuBar *menuBar = new wxMenuBar();
    menuBar->Append(menuFile, _T("&File"));
    SetMenuBar(menuBar);
    CreateStatusBar(3);
    //SetStatusText(_T("Welcome to wxWindows event sample"));
    //SetStatusText(_T("Dynamic: off"), Status_Dynamic);
    //SetStatusText(_T("Push count: 0"), Status_Push);
    Connect(-1, wxEVT_SIZE, (wxObjectEventFunction) &MyWxFrame::anEvent);
  }

  void anEvent(wxEvent& evt)
  {
    sys::coreout << "MyWxFrame::onAnEvent" << sys::eofl;
  }
  void OnMove(wxMoveEvent& evt)
  {
    sys::coreout << "MyWxFrame::OnMove" << sys::eofl;
  }
  void OnCose(wxCloseEvent& evt)
  {
    sys::coreout << "MyWxFrame::OnCose" << sys::eofl;
  }
  void OnQuit(wxCommandEvent& WXUNUSED(event))
  {
    // TRUE is to force the frame to close
    Close(TRUE);
  }
  void OnAbout(wxCommandEvent& WXUNUSED(event))
  {
    wxMessageBox( wxT("Event sample shows different ways of using events\n")
                  wxT("© 2001 Vadim Zeitlin"),
                  wxT("About Event Sample"), wxOK | wxICON_INFORMATION, this );
  }
   
};

//IMPLEMENT_DYNAMIC_CLASS(MyWxFrame, wxFrame)
class WxMain
: public wxApp
{
public:
  bool OnInit()
  {
    MyWxFrame* frame = new MyWxFrame(wxString(_T("hello wx")));
    //frame->Connect(-1, wxEVT_MOVE, (wxObjectEventFunction) frame->anEvent);
    //frame->Connect(-1, wxEVT_SIZE, (wxObjectEventFunction) frame->anEvent);
    //frame->Connect(-1, wxEVT_IDLE, (wxObjectEventFunction) frame->anEvent);
    
    frame->Show(true);
    return true;
  }
 

};
*/

wxApp *createApp()
{
  static RMain m = new Main();
  wxApp::CheckBuildOptions(wxBuildOptions());
  return m->getWx();
 }
/*
wxApp *createWxApp()
{
  wxApp::CheckBuildOptions(wxBuildOptions());
  return new WxMain();
}
*/
wxAppInitializer wxTheAppInitializer((wxAppInitializerFunction) createApp); 
//wxAppInitializer wxTheAppInitializer((wxAppInitializerFunction) createWxApp); 
//IMPLEMENT_APP(WxMain)

} // HelloWorld
} // wx
} // acdk
} // tests



int
main(int argc, char* argv[], char** envptr)
{
  return acdk::wx::App::main(argc, argv, envptr);
} 
