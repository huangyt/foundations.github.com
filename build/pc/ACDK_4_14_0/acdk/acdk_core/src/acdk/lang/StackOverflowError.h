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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StackOverflowError.h,v 1.1 2005/03/31 11:12:01 kommer Exp $

#ifndef acdk_lang_StackOverflowError_h
#define acdk_lang_StackOverflowError_h

#include "Error.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(StackOverflowError, Error);

/**
  The StackOverflowError will only thrown on 
  platforms (OS/compiler), which supports structured 
  handling of these type of exceptions (f.e. VC on Windows)
  @author Roger Rene Kommer
  @version $Revision: 1.1 $
  @date $Date: 2005/03/31 11:12:01 $
*/  
class ACDK_CORE_PUBLIC StackOverflowError 
: extends Error
{
  //ACDK_WITH_METAINFO(StackOverflowError)
public:
  StackOverflowError() : Error() { }
  StackOverflowError(IN(RString) what) : Error(what) {}
};

} // lang
} // acdk

#endif //acdk_lang_StackOverflowError_h

