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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/IndexOutOfBoundsException.h,v 1.12 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_IndexOutOfBoundsException_h
#define acdk_lang_IndexOutOfBoundsException_h

#include "RuntimeException.h"

namespace acdk {
namespace lang {


ACDK_DECL_THROWABLE(IndexOutOfBoundsException, RuntimeException);

/**
  Objects providing index based access to elements may throw this exception
  if index is out of range.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC IndexOutOfBoundsException
: extends RuntimeException
{
  ACDK_WITH_METAINFO(IndexOutOfBoundsException)
private:
  int _accessIndex;
  int _length;
public:
  // the constructor
  IndexOutOfBoundsException() : RuntimeException(), _accessIndex(-1), _length(-1)  { }
  IndexOutOfBoundsException(IN(RString) what) : RuntimeException(what), _accessIndex(-1), _length(-1) { }
  IndexOutOfBoundsException(IN(RString) what, int idx, int length);

  int accessIndex() { return _accessIndex; }
  int maxIndex() { return _length; }
};


} // Lang
} // acdk

#endif //acdk_lang_IndexOutOfBoundsException_h

