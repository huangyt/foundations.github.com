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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/AcdkDmiClient.h,v 1.15 2005/04/18 14:53:15 kommer Exp $

#ifndef acdk_lang_dmi_AcdkDmiClient_h
#define acdk_lang_dmi_AcdkDmiClient_h

#include "DmiClient.h"

namespace acdk {
namespace lang {
namespace dmi {


/**
  Standard DmiClient for most usages
  @see DmiClient
*/
foreign
class ACDK_CORE_PUBLIC AcdkDmiClient 
: public DmiClient
{
public:
  AcdkDmiClient(int formatFlags = TpFtAcdkType | TpFtFqName) 
    : DmiClient(formatFlags)
  {
  }
  /**
    @see DmiClient::typeDistance
  */
  virtual int typeDistance(const ScriptVar& arg, const ClazzInfo* toType)
  {
    return getTypeDistance(arg, toType);
  }
  virtual int typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType)
  {
    return getTypeDistance(fromType, toType);
  }

  /**
    @see DmiClient::castTo
  */
  virtual void castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType);
  virtual int getScriptVarCastFlags() const
  {
    return SVCastStdFlags;
  }

  static AcdkDmiClient _client;
  static DmiClient& getDmiClient() { return _client; }

  static int getTypeDistance(const ScriptVar& arg, const ClazzInfo* toType);
  static int getTypeDistance(const ClazzInfo* fromType, const ClazzInfo* toType);
};




} // dmi
} // lang
} // acdk



#endif //acdk_lang_dmi_AcdkDmiClient_h

