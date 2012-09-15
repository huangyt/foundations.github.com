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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/acdk_io_Package.cpp,v 1.6 2005/02/05 10:44:54 kommer Exp $





#include <acdk.h>

#include <acdk/lang/Package.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_PACKAGE_VERSION(acdk_io, 1.4.1);

PackageDefinition __acdk_io_package_def = 
{
  0,
  "acdk.io",
  { 
    "ACDK Core Package",
    "artefaktur",
    acdk_io_version
  },
  { 
    "ACDK Core Package",
    "artefaktur",
    acdk_io_version
  },
  0
};


RegisterPackage __preg(&__acdk_io_package_def);

} // namespace io
} // namespace acdk 

