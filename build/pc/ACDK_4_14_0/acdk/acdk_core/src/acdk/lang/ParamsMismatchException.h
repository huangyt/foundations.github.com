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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ParamsMismatchException.h,v 1.14 2005/02/05 10:44:56 kommer Exp $

#ifndef acdk_lang_ParamsMismatchException_h
#define acdk_lang_ParamsMismatchException_h


#include "DmiException.h"


namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(ParamsMismatchException, DmiException);

/** 
  While processing a dynamic Method Invokation (like New, invoke, invoke_static, peek, poke, peek_static, poke_static)
  and the element cannot be found, this exception will be thrown.
  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/02/05 10:44:56 $
*/  

class ACDK_CORE_PUBLIC ParamsMismatchException
: extends DmiException
{
  ACDK_WITH_METAINFO(ParamsMismatchException)
private:
  foreign const dmi::ClazzInfo* _memberClazzInfo;
  foreign dmi::ClazzMethodInfoVec _availableFunctions;
public:
  ParamsMismatchException() : DmiException() { }
  ParamsMismatchException(IN(RString) what) : DmiException(what) { }
  foreign ParamsMismatchException(IN(RString) what, const dmi::ClazzInfo* clzInfo, 
                                  const dmi::ClazzMethodInfoVec& functionsavail) 
  : DmiException(what) 
  , _memberClazzInfo(clzInfo)
  , _availableFunctions(functionsavail)
  { 
  }
  foreign dmi::ClazzMethodInfoVec& availableFunctions() { return _availableFunctions; }

  //virtual RString toString();
};


} // lang
} // acdk

#endif //acdk_lang_ParamsMismatchException_h

