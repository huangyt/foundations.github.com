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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/rmi/Naming.h,v 1.7 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_rmi_Naming_h
#define acdk_java_rmi_Naming_h

#include "Remote.h"

namespace acdk {
namespace java {
namespace rmi {

ACDK_DECL_CLASS(Naming);

class ACDK_JAVA_RMI_PUBLIC Naming
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Naming)
public:
  static void bind(IN(RString) name, IN(RRemote) obj);
  static RStringArray list(IN(RString) name);
  static RRemote lookup(IN(RString) name);
  static void rebind(IN(RString) name, IN(RRemote) obj);
  static void unbind(IN(RString) name);
};

} // namespace acdk
} // namespace java
} // namespace rmi

#endif //acdk_java_rmi_Naming_h

