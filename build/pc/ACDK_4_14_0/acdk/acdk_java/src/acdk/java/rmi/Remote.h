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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/rmi/Remote.h,v 1.7 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_rmi_Remote_h
#define acdk_java_rmi_Remote_h

#include <acdk.h>
#include "Config.h"

namespace acdk {
namespace java {
namespace rmi {

ACDK_DECL_INTERFACE(Remote);

class ACDK_JAVA_RMI_PUBLIC Remote
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Remote)
};

} // namespace acdk
} // namespace java
} // namespace rmi

#endif //acdk_java_rmi_Remote_h

