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

#ifndef acdk_cfgscript_ScriptException_h
#define acdk_cfgscript_ScriptException_h


#include "Props.h"
#include <acdk/lang/DmiException.h>


namespace acdk {
namespace cfgscript {


ACDK_DECL_THROWABLE(ScriptException, DmiException);


class ACDK_CFGSCRIPT_LIB_PUBLIC ScriptException
: extends ::acdk::lang::DmiException
{
  ACDK_WITH_METAINFO(ScriptException)
public:
  ScriptException(IN(RString) msg)
  : DmiException(msg)
  {
  }
};




} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_ScriptException_h
