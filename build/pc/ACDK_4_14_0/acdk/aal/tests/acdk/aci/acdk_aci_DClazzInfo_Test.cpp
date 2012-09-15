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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aci/acdk_aci_DClazzInfo_Test.cpp,v 1.6 2005/02/05 10:44:52 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aci/DClazzInfo.h>
#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdk {
namespace aal {



// Declare test cases
BEGIN_DECLARE_TEST( DClazzInfo_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( persistence )
END_DECLARE_TEST( DClazzInfo_Test  )

BEGIN_DEFINE_TEST( DClazzInfo_Test )
  ADD_TEST( DClazzInfo_Test, standard ) 
  ADD_TEST( DClazzInfo_Test, persistence ) 
END_DEFINE_TEST( DClazzInfo_Test )

using namespace ::acdk::aci;

void
DClazzInfo_Test::standard()
{
}

void
DClazzInfo_Test::persistence()
{
  RDClazzInfo dinf = DClazzInfo::getInstance(Integer::clazzInfo());

  dinf->writeObject(Nil);
}

} // namespace aal
} // namespace acdk
} // namespace tests

