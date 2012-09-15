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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/tests/acdk/lisp/acdk_lisp_BinaryLisp.cpp,v 1.6 2005/02/05 10:45:12 kommer Exp $


#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/io/File.h>
#include <acdk/lisp/LispInterpreter.h>
#include <acdk/lisp/LispObject.h>


namespace tests {
namespace acdk {
namespace lisp {
  
BEGIN_DECLARE_TEST( Binary_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( Binary_Test  )

BEGIN_DEFINE_TEST( Binary_Test )
  ADD_TEST( Binary_Test, standard ) 
END_DEFINE_TEST( Binary_Test )


using namespace ::acdk::lisp;

void 
Binary_Test::standard()
{
  ::acdk::io::RFile tfile = ::acdk::io::File::createTempFile("lispenv", ".blisp");
  {
    LispInterpreter lip;
    const char* code = 
    {
      "(defun testmethod (a b) (+ a b))"
    };
    lip.eval(code);
    lip.lispEnvironment()->storeCompiled(tfile->getWriter());
  }
  {
    LispInterpreter lip;
    lip.lispEnvironment()->loadCompiled(tfile->getReader(), false);
    const char* code = 
    {
      "(testmethod 1 2)"
    };
    RLispAtom ret = (RLispAtom)lip.eval(code);
    int ival = ret->val();
    testAssert(ival == 3);

  }
  tfile->deleteFile();
}


} // namespace lisp
} // namespace acdk
} // namespace tests



