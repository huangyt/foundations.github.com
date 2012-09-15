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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/acdk_text_Package.cpp,v 1.6 2005/02/05 10:45:33 kommer Exp $





#include <acdk.h>

#include <acdk/lang/Package.h>

namespace acdk {
namespace text {

using namespace acdk::lang;

ACDK_DECL_PACKAGE_VERSION(acdk_text, 1.4.1);

PackageDefinition __acdk_text_package_def =
{
  0, // flags
  "acdk.text",
  { 
    "ACDK Core Package",
    "artefaktur",
    acdk_text_version
  },
  { 
    "ACDK Core Package",
    "artefaktur",
    acdk_text_version
  }
};


RegisterPackage __preg(&__acdk_text_package_def);

} // namespace text
} // namespace acdk 

