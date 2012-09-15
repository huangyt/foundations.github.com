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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/App.cpp,v 1.6 2005/04/21 09:52:28 kommer Exp $

#include "wx.h"

#include "App.h"

namespace acdk {
namespace wx {



App::App()
: EvtHandler(new AppFwd(this))
{
  ::wxInitAllImageHandlers();
}

int 
App::onRun() 
{  
  try {
    return getWx()->wxApp::OnRun(); 
  } catch (RThrowable ex) {
    handleException(ex);
  } catch (...) {
    handleException(Nil);
  }
  return -1;
}

bool 
App::onInit() 
{ 
  try {
    return getWx()->wxApp::OnInit(); 
  } catch (RThrowable ex) {
    handleException(ex);
  } catch (...) {
    handleException(Nil);
  }
  return false;
}

int 
App::onExit() 
{ 
  try {
    return getWx()->wxApp::OnExit(); 
   } catch (RThrowable ex) {
    handleException(ex);
  } catch (...) {
    handleException(Nil);
  }
  return -1;
}

void 
App::handleException(IN(RThrowable) ex)
{
  if (ex == Nil)
    handleUnknownException();
  sys::coreout << "Caught Throwable in System::main(): "
        << ex->getMessage()->c_str() << sys::eofl;
   if (System::err != Nil)
    ex->printStackTrace();
  throw ex;
}

void 
App::handleUnknownException()
{
  if (System::err != Nil)
    System::printStackTrace();
  throw;
}

//static
int
App::main2(RStringArray args)
{
  StringBuffer sb("");
  for (int i = 0; i < args->length(); ++i)
  {
    sb.append(args[i]);
    sb.append(" ");
  }
  char* cmdline = (char*)sb.toString()->c_str();
#if defined(ACDK_OS_WIN32) //&& !ACDK_CHECK_WX_VERSION(2, 5) 

  return wxEntry((HINSTANCE) GetModuleHandle(0),
                           (HINSTANCE) 0, cmdline, SW_SHOW);
#else
  return wxEntry(System::getArgc(),  System::getArgv());
#endif
}

//static
int
App::main(int argc, char* argv[], char** envptr
                                   , const char** addargs)
{
  return System::main(App::main2, argc, argv, envptr, addargs);
}

RApp _globalApp;

RString _appClassName;



//RApp globalApplication;
#if defined(ACDK_OS_UNIX)
wxObject *createAppFromApplication()
#else
wxApp *createAppFromApplication()
#endif
{
#if ACDK_CHECK_WX_VERSION(2, 6)
wxAppConsole::CheckBuildOptions(WX_BUILD_OPTIONS_SIGNATURE, "acdk_wx");
#else
  wxApp::CheckBuildOptions(wxBuildOptions());
#endif
  IMPLEMENT_WX_THEME_SUPPORT
  return _globalApp->getWx();
}

#if defined(ACDK_OS_UNIX)
wxObject *createAppFromClazz()
#else
wxApp *createAppFromClazz()
#endif
{
  RClass cls = Class::forName(_appClassName);

  RObject obj = cls->newInstance();
  _globalApp = (RApp)obj;
  return createAppFromApplication();
  //return _globalApp->getWx();
}

#if !defined(wxUSE_GUI)
# error wxUSE_GUI has to be defined
#endif
//static
int
App::createGui(IN(RApp) application, IN(RStringArray) args)
{
  _globalApp = application;
#if ACDK_CHECK_WX_VERSION(2, 5) && 0 // does not work
  wxAppInitializer wxTheAppInitializer((wxAppInitializerFunction) createAppFromApplication);
#else
  wxApp::SetInitializerFunction((wxAppInitializerFunction)createAppFromApplication);
#endif
  //int t;
  //std::cin >> t;

  StringBuffer sb("");
  if (args->length() > 0)
    sb.append(args[0]);
  /*
  for (int i = 0; i < args->length(); ++i)
  {
    sb.append(args[i]);
    if (i > 0)
      break; 
    sb.append(" ");
  }*/
  char* cmdline = (char*)sb.toString()->c_str();
  int ret;
#if defined(ACDK_OS_WIN32) //&& !ACDK_CHECK_WX_VERSION(2, 5)
  ret = wxEntry((HINSTANCE) GetModuleHandle(0),
                           (HINSTANCE) 0, (wxCmdLineArgType)cmdline, SW_SHOW);
#else
  ret = wxEntry(System::getArgc(),  System::getArgv());
#endif
  _globalApp = Nil;
  return ret;
}


//static
int
App::createGui(IN(RString) appClassName, IN(RStringArray) args)
{
  _appClassName = appClassName;
  wxApp::SetInitializerFunction((wxAppInitializerFunction)createAppFromClazz);
  /*
#if ACDK_CHECK_WX_VERSION(2, 5)
  wxAppInitializer wxTheAppInitializer((wxAppInitializerFunction) createAppFromClazz);
#else
  wxApp::SetInitializerFunction(createAppFromClazz);
#endif
  */
  //int t;
  //std::cin >> t;

  StringBuffer sb("");
  if (args->length() > 0)
    sb.append(args[0]);
  // only acceppts -verbose
  /*
  for (int i = 0; i < args->length(); ++i)
  {
    sb.append(args[i]);
    sb.append(" ");
  }
  */
  char* cmdline = (char*)sb.toString()->c_str();
  int ret;
#if defined(ACDK_OS_WIN32) //&& !ACDK_CHECK_WX_VERSION(2, 5)
  ret = wxEntry((HINSTANCE) GetModuleHandle(0),
                           (HINSTANCE) 0, cmdline, SW_SHOW);
#else
  ret = wxEntry(System::getArgc(),  System::getArgv());
#endif
  _globalApp = Nil;
  return ret;
}


bool 
AppFwd::OnInit() 
{ 
  try {
    return _app->onInit(); 
  } catch (RThrowable ex) {
    releaseForward();
    _app = 0;
    System::out->println("Uncaught exception in App::onInit()");
    ex->printStackTrace(System::out);
    System::out->println(ex->getMessage());
  }
  return false;
}

} // wx
} // acdk

