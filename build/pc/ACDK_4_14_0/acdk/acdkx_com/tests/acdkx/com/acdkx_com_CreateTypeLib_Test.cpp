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
// $Header: /cvsroot/acdk/acdk/acdkx_com/tests/acdkx/com/acdkx_com_CreateTypeLib_Test.cpp,v 1.8 2005/02/05 10:45:38 kommer Exp $

#include <acdkx/com/CreateTypeLib.h>
#include <acdkx/com/CoException.h>

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>

namespace tests {
namespace acdkx {
namespace com {
  
BEGIN_DECLARE_TEST( CreateTypeLib_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( CreateTypeLib_Test  )

BEGIN_DEFINE_TEST( CreateTypeLib_Test )
  ADD_TEST( CreateTypeLib_Test, standard ) 
END_DEFINE_TEST( CreateTypeLib_Test )

using namespace ::acdkx::com;

void
CreateTypeLib_Test::standard()
{
  CoInitialize(NULL);
  try {
    ::acdkx::com::CreateTypeLib ct("./ACDKT1.TLB");
    ct.setName("Test");
    
    static const GUID LIBID_Component  = 
      { 0xfaf6008f, 0x2026, 0x4c14, { 0xbd, 0x8f, 0xb7, 0x7e, 0x24, 0xb0, 0x35, 0x10 } };
    ct->SetGuid(LIBID_Component);
    ct->SetVersion(1, 0);
    ct->SetLcid(LANG_NEUTRAL);
    ct.setDocString("ACDK COM Type Library Test");
    ct.commit();
  } catch (RCoException ex) {
    System::out->println(ex->getMessage());
  }
}


} // com
} // acdkx
} // tests
