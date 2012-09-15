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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/DmiTestException.h,v 1.8 2005/02/07 11:14:07 kommer Exp $

#ifndef acdk_tools_testunit_DmiTestException_h
#define acdk_tools_testunit_DmiTestException_h


#include <acdk.h>
#include "Config.h"

#include <acdk/lang/RuntimeException.h>
#include <acdk/io/PrintWriter.h>


namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_THROWABLE_FQ(DmiTestException, ::acdk::lang::, RuntimeException);


/** 
  This exception is for testing purpose.

  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/07 11:14:07 $
*/

class ACDK_TOOLS_AUNIT_PUBLIC DmiTestException
: extends ::acdk::lang::RuntimeException
{
  ACDK_WITH_METAINFO(DmiTestException)
public:
  int _code;
  RObject _causedObject;
  DmiTestException() 
  : RuntimeException()
  , _code(0) 
  {
  }
  DmiTestException(IN(RString) what) 
  : RuntimeException(what)
  , _code(0)
  {}
  DmiTestException(IN(RString) what, int code, IN(RObject) obj) 
  : RuntimeException(what) 
  , _code(code)
  , _causedObject(obj)
  {
  }
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_DmiTestException_h
