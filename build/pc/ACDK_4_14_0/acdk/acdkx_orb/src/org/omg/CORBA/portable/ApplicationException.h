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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/portable/ApplicationException.h,v 1.9 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_portable_ApplicationException_h
#define org_omg_CORBA_portable_ApplicationException_h

#include <acdk.h>

#include "../CORBA.h"
#include "../ORB.h"
#include "InputStream.h"

namespace org {
namespace omg {
namespace CORBA {
namespace portable {


ACDK_DECL_THROWABLE(ApplicationException, Exception);

class ACDKX_ORB_PUBLIC ApplicationException
: extends ::acdk::lang::Exception
{
  ACDK_WITH_METAINFO(ApplicationException)
private:
  RInputStream _in;
  RString _userExceptionClassName;
  RClass _userclazz;
public:
  /*
  ApplicationException(RString what) : Exception(what) { }*/
  ApplicationException() : Exception() { }
  ApplicationException(IN(RInputStream) in, IN(RString) exname) 
  : Exception(),
    _in(in),
    _userExceptionClassName(exname)
  { 
  }
  /**
  Returns the CORBA repository ID of the exception without removing it from the exceptions input stream. 
  */
  RString getId() { return _userExceptionClassName; }
          
  RInputStream getInputStream() { return _in; }
  RClass getUserExceptionClass() 
  { 
    if (_userclazz != Nil)
      return _userclazz;
    _userclazz = Class::forName(_userExceptionClassName);
    return _userclazz;
  }
};

} // namespace portable
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_portable_ApplicationException_h
