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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/core_test.h,v 1.15 2005/04/26 22:14:53 kommer Exp $

#ifndef acdk_tools_testunit_core_test_h
#define acdk_tools_testunit_core_test_h

ACDK_NO_METAINFO_HEADER // has no metainfo

#if !defined(DOXYGENONLY)

#ifndef __GNUG__
#include "Config.h"
#include <exception>
#if defined(__BORLANDC__)
#include <stdexcep.h>
#endif
#include <string>

foreign
class ACDK_TOOLS_AUNIT_PUBLIC core_test_exception
: public std::runtime_error
{
public:
  core_test_exception(const std::string& msg);
};

#define testAssert(expr) \
do { \
if (!(expr)) { \
  ::acdk::lang::RString s__ = SBSTR("Test Failed: ["  << #expr << "] in " << __FILE__ << ":" << __LINE__)->c_str(); \
  sys::coreout << s__->c_str() << sys::eofl; \
  throw core_test_exception(s__->c_str()); \
}  \
} while(false)

#else //__GNUG__

#include <stdio.h>

#define testAssert(expr) \
do { \
if (!(expr)) { \
  printf("Test Failed: ["  #expr "] in " __FILE__ ":%i\n", __LINE__); \
  exit(1); \
}  \
} while(false)


#endif //ACDK_MINGW
#endif //!defined(DOXYGENONLY)

#endif //acdk_tools_testunit_core_test_h



