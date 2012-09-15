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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_core_hashmap_Test.cpp,v 1.7 2005/04/26 22:15:49 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include "../../../../src/acdk/lang/sys/core_hashmap.h"
//#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>
/*
#include <vector>
#include <exception>
*/
int hash(int i, int maxh)
{
  return i % maxh;
}

namespace tests {
namespace acdk {
namespace lang {
namespace sys {


using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;

BEGIN_DECLARE_TEST( core_hashmap_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( core_hashmap_Test  )

BEGIN_DEFINE_TEST( core_hashmap_Test )
  ADD_TEST( core_hashmap_Test, standard )

END_DEFINE_TEST( core_hashmap_Test )



void
core_hashmap_Test::standard()
{
  core_flathashmap<int, int> cont;
  int i;
  for (i = 0; i < 100; ++i)
    cont.put(i, i);
  for (i = 0; i < 100; ++i)
    testAssert(cont.get(i).value() == i);
}

} // namespace sys
} //namespace lang
} //namespace acdk
} // namespace tests
