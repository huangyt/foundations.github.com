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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Exception.h,v 1.10 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Exception_h
#define acdk_lang_Exception_h

/***
do not include this header directly.
Will be included with acdk_lang_Object.h
*/

#include "Throwable.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(Exception, Throwable);

/**
  Base class for most exception classes.
  @see acdk::lang::Error
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Exception 
: extends Throwable
{
  ACDK_WITH_METAINFO(Exception)
public:
  Exception()
  : Throwable()
  {
  }
  Exception(IN(RString) what, IN(RThrowable) cause = Nil)
  : Throwable(what, cause)
  {
  }
  Exception(IN(RThrowable) cause)
  : Throwable(cause)
  {
  }
};

} // lang
} // acdk

#endif //acdk_lang_Exception_h

