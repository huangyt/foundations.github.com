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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/cfgscript/ScriptGlobals.h,v 1.4 2005/03/25 12:11:31 kommer Exp $

#ifndef acdk_cfgscript_ScriptGlobals_h
#define acdk_cfgscript_ScriptGlobals_h

#include "Config.h"
#include <acdk.h>

namespace acdk {
namespace cfgscript {


ACDK_DECL_CLASS(ScriptGlobals);
/**
  represents global functions.
  The inOf, outOf, byRefAs etc are hints for acdkx_rdmi
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC ScriptGlobals
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ScriptGlobals)
public:  
  /**
    enables following casting options: 
    SVCastSVCastChar2Int | SVCastInt2Float | SVCastNum2Bool | SVCastBool2Number | SVCastObject2Bool | SVCastAutobox
    @see acdk::lang::dmi::ScriptVarCastFlags
  */
  static acdk::lang::dmi::RDmiObject castTo(IN(RDmiObject) val, IN(acdk::lang::RClass) cls);
  static acdk::lang::dmi::RDmiObject inOf(IN(RDmiObject) val)
  {
    return new acdk::lang::dmi::DmiObject(val, (val->flags & ~acdk::lang::dmi::MiAiOut) | acdk::lang::dmi::MiAiIn);
  }
  static acdk::lang::dmi::RDmiObject outOf(IN(RDmiObject) val)
  {
    return new acdk::lang::dmi::DmiObject(val, (val->flags & ~acdk::lang::dmi::MiAiIn) | acdk::lang::dmi::MiAiOut);
  }
  static acdk::lang::dmi::RDmiObject inoutOf(IN(RDmiObject) val)
  {
    return new acdk::lang::dmi::DmiObject(val, val->flags  | acdk::lang::dmi::MiAiIn | acdk::lang::dmi::MiAiOut);
  }
  static acdk::lang::dmi::RDmiObject byVal(IN(RDmiObject) val)
  {
    return new acdk::lang::dmi::DmiObject(val, (val->flags | MiAiByval) & ~MiAiByref);
  }
  static acdk::lang::dmi::RDmiObject byRef(IN(RDmiObject) val)
  {
    return new acdk::lang::dmi::DmiObject(val, (val->flags | MiAiByref) & ~MiAiByval);
  }
  static acdk::lang::dmi::RDmiObject byRefAs(IN(RDmiObject) val, IN(acdk::lang::RClass) cls)
  {
    return new acdk::lang::dmi::DmiObject(val, (val->flags | MiAiByref) & ~MiAiByval, cls->objectClazzInfo());
  }
  static acdk::lang::dmi::RDmiObject byValAs(IN(RDmiObject) val, IN(acdk::lang::RClass) cls)
  {
    return new acdk::lang::dmi::DmiObject(val, (val->flags | MiAiByval) & ~MiAiByref, cls->objectClazzInfo());
  }
};

} 
}
#endif //acdk_cfgscript_ScriptGlobals_h

