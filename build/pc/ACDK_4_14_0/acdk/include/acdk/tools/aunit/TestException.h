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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestException.h,v 1.12 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestException_h
#define acdk_tools_testunit_TestException_h


#include <acdk.h>
#include "Config.h"

#include <acdk/lang/RuntimeException.h>


namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(TestExpression);

/**
  internal helper class used by TestException
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestExpression
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(TestExpression)
public:
  RString expression;
  RString fileName;
  int lineNo;

  TestExpression(INP(RString) expr, INP(RString) file = "", int line = -1)
  : expression(expr)
  , fileName(file)
  , lineNo(line)
  {
  }
  RString toString()
  {
    return SBSTR(fileName << "(" << lineNo << "): " << expression);
  }
};
  
ACDK_DECL_THROWABLE_FQ(TestException, ::acdk::lang::, Throwable);

/**
  This exception is used to indicate failed tests.
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestException
: extends ::acdk::lang::Throwable
{
  ACDK_WITH_METAINFO(TestException)
private:
  RTestExpression _testExpr;
public:
  TestException(IN(RString) msg, IN(RString) filename = "", int lineNo = -1)
  : Throwable(msg)
  , _testExpr(new TestExpression(msg, filename, lineNo))
  {
  }
  int getLineNumber() { return _testExpr->lineNo; }
  RString getFileName() { return _testExpr->fileName; }
  RString getMessage() 
  {
     return _testExpr->toString() + ": " + Throwable::getMessage();
  }
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestException_h
