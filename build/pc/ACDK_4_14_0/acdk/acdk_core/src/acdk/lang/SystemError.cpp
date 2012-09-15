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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/SystemError.cpp,v 1.10 2005/04/18 19:47:49 kommer Exp $


#include <acdk.h>
#include "SystemError.h"
#include <stdlib.h>
#include <errno.h>

namespace acdk {
namespace lang {

SystemError::SystemError() 
: Error() 
{ 
  StringBuffer errmsg;
  int en = errno;
  if (en != 0)
    errmsg << "errno: " << en << "; " << strerror(en);
  _what = errmsg.toString();
  sys::coreout << "*** SystemError::SystemError()" << _what << sys::eofl;
  /*
  abort();
  char* ptr = 0;
  *ptr = 0;
  */
}
SystemError::SystemError(IN(RString) what) 
    : Error(what) 
  {
    StringBuffer errmsg;
    if (what != Nil)
      errmsg << what;
    
    int en = errno;
    if (en != 0)
      errmsg << "; errno: " << en << "; " << strerror(en);
    _what = errmsg.toString();
    
    sys::coreout << "*** SystemError::SystemError(what): " << errmsg.toString()->c_str() << sys::eofl;
    /*    
    char* ptr = 0;
    *ptr = 0;
    */
  }

} // lang
} // acdk
