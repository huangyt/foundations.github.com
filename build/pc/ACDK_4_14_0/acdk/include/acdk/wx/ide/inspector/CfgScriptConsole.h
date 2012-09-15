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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ide/inspector/CfgScriptConsole.h,v 1.3 2005/02/06 13:12:12 kommer Exp $

#ifndef acdk_wx_inspector_CfgScriptConsole_h
#define acdk_wx_inspector_CfgScriptConsole_h

#include "inspector.h"
#include <acdk/wx/TextCtrl.h>

namespace acdk {
namespace wx {
namespace ide {
namespace inspector {



ACDK_DECL_CLASS(CfgScriptConsole);
/**
  Implements a CfgScript interpreter console.
  The CfgScript interpreter itself (acdk_cfgscript.dll/so) is not
  neccessary linked to the application, but will loaded dynamically
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/06 13:12:12 $
*/
class ACDK_WX_IDE_PUBLIC CfgScriptConsole
: extends TextCtrl
{
  ACDK_WITH_METAINFO(CfgScriptConsole)
protected:
  int _consoleState;
  int _scriptBeginPos;
  RObject _script;
  RObject _props;
public:
  CfgScriptConsole(IN(RWindow) parent, int id, IN(RPoint) point = Point::defaultPosition(), IN(RSize) size = Size::defaultSize());
  void onTextUpdated(IN(RCommandEvent) event);
  void callGc(bool all);
  void _prompt();
  void _processCommand(IN(RString) text, IN(RString) line);
  void printText(IN(RString) text);
  void _eval(IN(RString) text);
};


} // inspector
} // ide
} // wx
} // acdk

#endif //acdk_wx_inspector_CfgScriptConsole_h
