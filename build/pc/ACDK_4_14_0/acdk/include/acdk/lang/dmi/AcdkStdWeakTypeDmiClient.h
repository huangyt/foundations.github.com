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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h,v 1.19 2005/04/18 14:53:16 kommer Exp $

#ifndef acdk_lang_dmi_AcdkStdWeakTypeDmiClient_h
#define acdk_lang_dmi_AcdkStdWeakTypeDmiClient_h

#include "AcdkDmiClient.h"

namespace acdk {
namespace lang {
namespace dmi {


/**
  This DmiClient uses a more weak type casting scheme.
  If a type is requested from a string, it tries to 
  parse this string.
*/
foreign
class ACDK_CORE_PUBLIC AcdkStdWeakTypeDmiClient
: public AcdkDmiClient
{
public:
  /**
    A combination of ScriptVarCastFlags
  */
  int _scriptVarCastFlags;
  
  AcdkStdWeakTypeDmiClient(int scriptVarCastFlags = SVCastStdFlags)
  : AcdkDmiClient(TpFtFqName | TpFtJavaType)
  , _scriptVarCastFlags(scriptVarCastFlags)
  {
  }

   /**
    @see DmiClient::typeDistance
  */
  virtual int typeDistance(const ScriptVar& arg, const ClazzInfo* toType);
  virtual int typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType)
  {
    return AcdkDmiClient::typeDistance(fromType, toType);
  }
  /**
    @see DmiClient::castTo
  */
  virtual void castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType);
  virtual int getScriptVarCastFlags() const { return _scriptVarCastFlags; }

  static AcdkStdWeakTypeDmiClient _client;
  static DmiClient& getDmiClient() { return _client; }
protected:

  /**
    @see AcdkDmiClient::typeDistance
  */
  int weakTypeDistance(const ScriptVar& arg, const ClazzInfo* toType);
  
  /** 
    This call cast the arg to given toType
    @return true if value was modified
  */
  bool weakTypeCast(ScriptVar& arg, const ClazzInfo* toType);
public:

  static int typeDistance(const ScriptVar& arg, const ClazzInfo* toType, int svcastflags);
  
  /** 
    This call cast the arg to given toType
    @return true if value was modified
  */
  static bool typeCast(ScriptVar& arg, const ClazzInfo* toType, int svcastflags);
};

} // dmi
} // lang
} // acdk



#endif //acdk_lang_dmi_AcdkStdWeakTypeDmiClient_h

