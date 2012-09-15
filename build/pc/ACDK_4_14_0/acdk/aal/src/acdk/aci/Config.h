// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/Config.h,v 1.3 2005/02/07 17:11:24 kommer Exp $

#ifndef acdk_aci_Config_h
#define acdk_aci_Config_h

#include <acdk.h>

#if defined(ACDK_NEED_DLLEXPORT)
# if defined(IN_ACDK_ACI_LIB)
#   define ACDK_ACI_PUBLIC __declspec(dllexport)
# elif defined(ACDK_STATIC_LIB)
# 	define ACDK_ACI_PUBLIC
# else
#   define ACDK_ACI_PUBLIC __declspec(dllimport)
# endif
#else
# define ACDK_ACI_PUBLIC
#endif


#endif //acdk_aci_Config_h

