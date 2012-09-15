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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/cfgscript/acdkcfgscript/Main.h,v 1.1 2005/04/10 15:57:10 kommer Exp $

#ifndef acdk_cfgscript_acdkcfgscript_Main_h
#define acdk_cfgscript_acdkcfgscript_Main_h

#include <acdk.h>

#include <acdk/cfgscript/Script.h>

namespace acdk {
namespace cfgscript {
/**
  CfgScript interpreter executable
  @see gw_ref[acdk_cfgscript_interpreter]
*/
namespace acdkcfgscript {


class Main
: extends acdk::lang::Object
{
public:
  static int start(RStringArray args);
  static void help();
  static int interActive(IN(RProps) props);
};

} 
}
}

#endif //acdk_cfgscript_acdkcfgscript_Main_h
