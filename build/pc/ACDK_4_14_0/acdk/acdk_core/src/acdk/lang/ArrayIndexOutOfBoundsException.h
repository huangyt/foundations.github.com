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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ArrayIndexOutOfBoundsException.h,v 1.9 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_lang_ArrayIndexOutOfBoundsException_h
#define acdk_lang_ArrayIndexOutOfBoundsException_h

#include "IndexOutOfBoundsException.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(ArrayIndexOutOfBoundsException, IndexOutOfBoundsException);

/** 
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:44:54 $
  
*/


class ACDK_CORE_PUBLIC ArrayIndexOutOfBoundsException
: extends IndexOutOfBoundsException
{
  ACDK_WITH_METAINFO(ArrayIndexOutOfBoundsException)
public:
  // the constructor
  ArrayIndexOutOfBoundsException() : IndexOutOfBoundsException() { }
  ArrayIndexOutOfBoundsException(RString what) : IndexOutOfBoundsException(what) { }
  ArrayIndexOutOfBoundsException(RString what, int idx, int length) : IndexOutOfBoundsException(what, idx, length) {}
  ArrayIndexOutOfBoundsException(int idx) : IndexOutOfBoundsException("ArrayIndexOutOfBoundsException", idx, -1) {}
};

} // Lang
} // acdk

#endif //acdk_lang_ArrayIndexOutOfBoundsException_h

