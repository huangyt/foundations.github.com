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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/arb/ADelegate.h,v 1.9 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_arb_ADelegate_h
#define acdkx_arb_ADelegate_h

#include <acdk.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>

#include "arb.h"
#include "AObjectImpl.h"

namespace acdkx {
namespace arb {

ACDK_DECL_INTERFACE(ADelegate);

class ACDKX_ARB_PUBLIC ADelegate
      ACDK_INTERFACEBASE
{
public:
  /**
    invoke a method through ARB/ORB.
    @param arb the used arb
    @param objid the remote object
    @param cmi information of the method
    @param args The out paramter
    @param ergs the in parameter. If the method has return type, ergs[0] contains the value
  */
  virtual void invoke(IN(RARB) arb, IN(RObjectID) objid, const dmi::ClazzMethodInfo* cmi, ::acdk::lang::dmi::ScriptVarArray& args, 
                                                                       ::acdk::lang::dmi::ScriptVarArray& ergs, 
                                                                       ::acdk::lang::dmi::ScriptVar& _theEx) = 0;
  virtual void dispatch(IN(RARB) arb, IN(::acdk::io::RReader) in, IN(::acdk::io::RWriter) out) = 0;
};



} // namespace arb 
} // namespace acdkx 

#endif //acdkx_arb_ADelegate_h

