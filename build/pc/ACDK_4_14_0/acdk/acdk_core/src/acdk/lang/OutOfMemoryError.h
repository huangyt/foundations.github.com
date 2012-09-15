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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/OutOfMemoryError.h,v 1.4 2005/04/09 19:26:49 kommer Exp $

#ifndef acdk_lang_OutOfMemoryError_h
#define acdk_lang_OutOfMemoryError_h

#include "Error.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(OutOfMemoryError, Error);

/**
  This exception will be thrown in case no more memory cannot be allocated
  because either the memory was limited by the appliation or no more memory
  can be allocated from the underlying operation system.
  @author Roger Rene Kommer
  @version $Revision: 1.4 $
  @date $Date: 2005/04/09 19:26:49 $
*/  
class ACDK_CORE_PUBLIC OutOfMemoryError 
: extends Error
{
  //ACDK_WITH_METAINFO(OutOfMemoryError)
public:
  OutOfMemoryError() : Error() { }
  OutOfMemoryError(IN(RString) what) : Error(what) {}
};

} // lang
} // acdk

#endif //acdk_lang_OutOfMemoryError_h

