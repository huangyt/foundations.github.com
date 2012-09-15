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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/App.h,v 1.8 2005/03/31 16:30:50 kommer Exp $

#ifndef acdk_wx_App_h
#define acdk_wx_App_h

#include "Window.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(App);
/**
  see wxApp
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/03/31 16:30:50 $
*/
class ACDK_WX_PUBLIC App
: extends EvtHandler
{
  ACDK_WITH_METAINFO(App)
public:
  ACDK_WX_STD_MEMBERS(App, EvtHandler)

  App();
  virtual bool onInit();
  virtual int onExit();
  virtual int onRun();
  static int main2(RStringArray args);
  static int createGui(IN(RString) appClassName, IN(RStringArray) args);
  static int createGui(IN(RApp) application, IN(RStringArray) args);
  static int main(int argc, char* argv[], char** envptr = 0
                                   , const char** addargs = 0);
  
  /** 
    overwrite this if wanted.
    by default it just print the exception and stacktrace to System::err
    an rethrow the exception
    @param ex may be Nil if an exception not derived from Throwable occours.
  */
  virtual void handleException(IN(RThrowable) ex);
  void handleUnknownException();
  
};


foreign
class ACDK_WX_PUBLIC AppFwd
: public wxApp
, protected WxForward
{
  App* _app;
public:
  AppFwd(App* app) 
  : WxForward(true)
  , _app(app) 
  {
  }
  Object* getObject() { return _app; }
  bool OnInit();
  int OnExit() { return _app->onExit(); }
  // only for console
  int OnRun() { return _app->onRun(); }
};


} // wx
} // acdk

#endif //acdk_wx_App_h
