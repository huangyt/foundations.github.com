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
// $Header: /cvsroot/acdk/acdk/acdk_vfile/src/acdk/vfile/Config.h,v 1.8 2005/02/07 17:16:28 kommer Exp $

#ifndef acdk_vfile_Config_h
#define acdk_vfile_Config_h

#include <acdk.h>

#if defined(ACDK_NEED_DLLEXPORT)
# if defined(IN_ACDK_VFILE_LIB)
#   define ACDK_VFILE_PUBLIC __declspec(dllexport)
# elif defined(ACDK_STATIC_LIB)
# 	define ACDK_VFILE_PUBLIC
# else
#   define ACDK_VFILE_PUBLIC __declspec(dllimport)
# endif
#else
# define ACDK_VFILE_PUBLIC
#endif


namespace acdk {
/**
   Virtual file system implemenetations
*/
namespace vfile {

}
} // namespace acdk 
#endif //acdk_vfile_Config_h
