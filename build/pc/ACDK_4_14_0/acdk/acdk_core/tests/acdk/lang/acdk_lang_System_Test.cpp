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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_System_Test.cpp,v 1.2 2005/03/19 21:33:06 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Double.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( System_Test )
  DECLARE_TEST( standardProperties )
  
END_DECLARE_TEST( System_Test  )

BEGIN_DEFINE_TEST( System_Test )
  ADD_TEST( System_Test, standardProperties ) 
END_DEFINE_TEST( System_Test )

using namespace acdk::lang;

#define FECH_PROPERTY(name) \
  #name << ": " << System::getProperty(#name) << "\n"

void 
System_Test::standardProperties()
{
  StringBuffer sb;
  sb << FECH_PROPERTY(acdk.vm.version)
     << FECH_PROPERTY(acdk.vm.vendor)
     << FECH_PROPERTY(acdk.vm.name)
     << FECH_PROPERTY(acdk.io.tmpdir)
     << FECH_PROPERTY(user.name)
     << FECH_PROPERTY(user.home)
     << FECH_PROPERTY(line.separator)
     << FECH_PROPERTY(os.name)
     << FECH_PROPERTY(os.arch)
     << FECH_PROPERTY(file.separator)
     << FECH_PROPERTY(path.separator)
     << FECH_PROPERTY(user.language)
     << FECH_PROPERTY(user.region)
     << FECH_PROPERTY(user.variant)
     << FECH_PROPERTY(user.encoding)
     ;
  System::out->println("ACDK is running with following parameter:\n" + sb.toString());
     
}



} // namespace lang 
} //namespace acdk 
} //namespace tests 



