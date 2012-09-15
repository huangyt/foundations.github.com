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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/StringIndexOutOfBoundsException.h,v 1.10 2005/04/09 19:26:50 kommer Exp $

#ifndef acdk_lang_StringIndexOutOfBoundsException_h
#define acdk_lang_StringIndexOutOfBoundsException_h
#include <acdk.h>

#include "IndexOutOfBoundsException.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(StringIndexOutOfBoundsException, IndexOutOfBoundsException);

/** 
  May be thrown by acdk::lang::String or acdk::lang::StringBuffer
  if index based method calls the index is out of range.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:50 $
*/  
class ACDK_CORE_PUBLIC StringIndexOutOfBoundsException
: extends IndexOutOfBoundsException
{
  ACDK_WITH_METAINFO(StringIndexOutOfBoundsException)
public:
  // the constructors
  StringIndexOutOfBoundsException() : IndexOutOfBoundsException() { }
  StringIndexOutOfBoundsException(IN(RString) what) : IndexOutOfBoundsException(what) { }
  StringIndexOutOfBoundsException(IN(RString) what, int idx, int length) : IndexOutOfBoundsException(what, idx, length) {}
  StringIndexOutOfBoundsException(int idx) : IndexOutOfBoundsException("StringIndexOutOfBoundsException", idx, -1) {}
};

} // lang
} // acdk

#endif //acdk_lang_StringIndexOutOfBoundsException_h

