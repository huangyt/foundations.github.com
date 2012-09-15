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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_prim.h,v 1.5 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_core_prim_h
#define acdk_lang_sys_core_prim_h

#include <acdk.h>

namespace acdk {
namespace lang {
namespace sys {
  
/**
  get next greater int whis is prim
  implementation in core_system.cpp
*/
int ACDK_CORE_PUBLIC core_get_next_prim(int num);
bool ACDK_CORE_PUBLIC core_is_prim(int num);

}
}
}

#endif //acdk_lang_sys_core_prim_h

