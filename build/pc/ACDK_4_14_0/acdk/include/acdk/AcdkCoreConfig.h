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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/AcdkCoreConfig.h,v 1.2 2005/04/13 12:56:08 kommer Exp $

#ifndef acdk_AcdkCoreConfig_h
#define acdk_AcdkCoreConfig_h

#if defined(ACDK_IN_ACDK_LIB) || defined(IN_ACDK_CORE_LIB)
# define ACDK_CORE_PUBLIC ACDK_DLL_EXPORT
#else
# define ACDK_CORE_PUBLIC ACDK_DLL_IMPORT
#endif

#endif //acdk_Config_h

