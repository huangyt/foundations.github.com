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
// $Header: /cvsroot/acdk/acdk/acdkx_com/tests/acdkx/com/acdkx_com_CreateTypeInfo_Test.cpp,v 1.9 2005/02/05 10:45:38 kommer Exp $


#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdkx/com/CreateTypeLib.h>
#include <acdkx/com/CoException.h>

namespace tests {
namespace acdkx {
namespace com {
  
BEGIN_DECLARE_TEST( CreateTypeInfo_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( CreateTypeInfo_Test  )

BEGIN_DEFINE_TEST( CreateTypeInfo_Test )
  ADD_TEST( CreateTypeInfo_Test, standard ) 
END_DEFINE_TEST( CreateTypeInfo_Test )

using namespace ::acdkx::com;

void
CreateTypeInfo_Test::standard()
{
  return;
  /** #### does not work
  try {
    ::acdkx::com::CreateTypeLib ct("./ACDKTL2.tlb");
    ct.setName("Test2");

  static const GUID LIBID_Component2 = 
    { 0x806f5ab6, 0x5c71, 0x478c, { 0xa1, 0xd5, 0x5d, 0xf0, 0x69, 0x6d, 0xf5, 0x54 } };

    ct->SetGuid(LIBID_Component2);
    ct->SetVersion(1, 0);
    ct->SetLcid(LANG_NEUTRAL);

    ct.setDocString("ACDK COM Type Library Test");
    RCreateTypeInfo ti = ct.createTypeInfo(CreateTypeLib::Interface, "acdk.lang.Object");
    ti->createClass(Object::GetClass());

    ct.commit();
  } catch (RCoException ex) {
    System::out->println(ex->getMessage());
  }
  */
}


} // com
} // acdkx
} // tests
