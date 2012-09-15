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


#ifndef acdk_lisp_LispDmiClient_h
#define acdk_lisp_LispDmiClient_h

#include "LispVar.h"

namespace acdk {
namespace lisp {


/**
  Lisp Parameter converter
*/
class ACDK_ACDK_LISP_PUBLIC LispDmiClient
: public ::acdk::lang::dmi::AcdkDmiClient
{
public:
  typedef ::acdk::lang::dmi::AcdkDmiClient Super;
   /**
    @see DmiClient::typeDistance
  */
  virtual int typeDistance(const ::acdk::lang::dmi::ScriptVar& arg, const ::acdk::lang::dmi::ClazzInfo* toType);
  virtual int typeDistance(const ::acdk::lang::dmi::ClazzInfo* fromType, const ::acdk::lang::dmi::ClazzInfo* toType);

  /**
    @see DmiClient::castTo
  */
  virtual void castTo(::acdk::lang::dmi::ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType);
};

} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispDmiClient_h
