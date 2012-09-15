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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/acdk_net_Package.cpp,v 1.7 2005/04/08 10:53:20 kommer Exp $



#include <acdk.h>
#include <acdk/lang/Package.h>

namespace acdk {
namespace net {

using namespace acdk::lang;

ACDK_DECL_PACKAGE_VERSION(acdk_net, 1.4.1);

PackageDefinition __acdk_net_package_def =
{
  0,
  "acdk.net",
  { 
    "ACDK Net Package",
    "artefaktur",
    acdk_net_version
  },
  { 
    "ACDK Net Package",
    "artefaktur",
    acdk_net_version
  },
  0
};


/** @internal */
RegisterPackage __preg(&__acdk_net_package_def);

} // namespace net
} // namespace acdk 

