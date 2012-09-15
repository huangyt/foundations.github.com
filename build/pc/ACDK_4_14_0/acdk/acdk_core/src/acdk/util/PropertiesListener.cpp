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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/PropertiesListener.cpp,v 1.12 2005/04/09 19:26:57 kommer Exp $


#include <acdk.h>
#include "PropertiesListener.h"
#include <acdk/lang/Runtime.h>
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/FileReader.h>

namespace acdk {
namespace util {

//static 
bool PropertiesListener::reconfigure = false;

class SignalEventHandlerImpl
: extends ::acdk::lang::Object
, implements ::acdk::lang::SignalEventHandler
{
public:
  virtual bool handleEvent(SignalEventType event)
  {
    if (event == CtrlC_Event)
      return false;
    PropertiesListener::reconfigure = true;
    return true;
  }
  virtual bool equals(IN(RSignalEventHandler) other) { return other->_getObjectPtr() == this; }
  virtual int listenToEvents() { return CtrlBreak_Event; }
};

static 
acdk::lang::RSignalEventHandler proptertyListener;


PropertiesListener::PropertiesListener(IN(RString) cfgFile, IN(RProperties) props)
: Thread()
, _configFile(cfgFile)
, _properties(props)
{
  Thread::setDaemon(true);
  if (proptertyListener == Nil)
  {
    proptertyListener  = new SignalEventHandlerImpl();
    acdk::lang::Runtime::registerSignalEventHandler(proptertyListener);
  }
}

void 
PropertiesListener::reload()
{
  ::acdk::io::File f(_configFile);
  ::acdk::io::FileReader fin((acdk::io::RFile)&f);
  _properties->clear();
  _properties->load((::acdk::io::RReader)SR(FileReader, fin));
  ::acdk::lang::System::out->println("Config file reloaded: File=[" + _configFile + "]; ");
}

void 
PropertiesListener::run()
{
  while (true)
  {
    if (PropertiesListener::reconfigure == true)
    {
      reload();
      PropertiesListener::reconfigure = false;
    }
    Thread::sleep(1000);
  }
}



} // util
} // acdk




