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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/security/security.h,v 1.9 2005/02/07 11:17:24 kommer Exp $

#ifndef acdk_security_security_h
#define acdk_security_security_h

#include <acdk.h>

ACDK_UNITATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_UnitInclude1", "acdk_core.idl"))

ACDK_DECL_UNIT(acdk_security)



#if defined(ACDK_NEED_DLLEXPORT)
# if defined(IN_ACDK_SECURITY_LIB)
#   define ACDK_SECURITY_LIB_PUBLIC __declspec(dllexport)
# elif defined(ACDK_STATIC_LIB)
# 	define ACDK_SECURITY_LIB_PUBLIC
# else
#   define ACDK_SECURITY_LIB_PUBLIC __declspec(dllimport)
# endif
#else
# define ACDK_SECURITY_LIB_PUBLIC
#endif

namespace acdk {
/**
  Equally to the Java package java.security.
*/ 
namespace security {



} // security
} // acdk


#endif //acdk_security_security_h

