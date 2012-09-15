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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ArithmeticException.h,v 1.1 2005/03/23 18:02:51 kommer Exp $

#ifndef acdk_lang_ArithmeticException_h
#define acdk_lang_ArithmeticException_h

#include "RuntimeException.h"

namespace acdk {
namespace lang {


ACDK_DECL_THROWABLE(ArithmeticException, RuntimeException);
/**
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.1 $
  @date $Date: 2005/03/23 18:02:51 $
*/  
class ACDK_CORE_PUBLIC ArithmeticException
: extends RuntimeException
{
  ACDK_WITH_METAINFO(ArithmeticException)
public:
  ArithmeticException() : RuntimeException() { }
  ArithmeticException(IN(RString) what) : RuntimeException(what) { }
};

ACDK_DECL_THROWABLE(ArithmeticOverflowException, ArithmeticException);

/**
  Signal arithmetic an overflow condition
  @author Roger Rene Kommer
  @version $Revision: 1.1 $
  @date $Date: 2005/03/23 18:02:51 $
*/  
class ACDK_CORE_PUBLIC ArithmeticOverflowException
: extends ArithmeticException
{
  ACDK_WITH_METAINFO(ArithmeticOverflowException)
public:
  ArithmeticOverflowException() : ArithmeticException() { }
  ArithmeticOverflowException(IN(RString) what) : ArithmeticException(what) { }
};


} // lang
} // acdk

#endif //acdk_lang_ArithmeticException_h

