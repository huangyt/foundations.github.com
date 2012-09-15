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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Modifier.h,v 1.25 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_reflect_Modifier_h
#define acdk_lang_reflect_Modifier_h

namespace acdk {
namespace lang {
namespace reflect {

/*
#ifdef CLONEABLE
# undif CLONEABLE
#endif
#ifdef VIRTUAL
# undef VIRTUAL
#endif
#ifdef ONEWAY
# undef ONEWAY
#endif
#ifdef INTERFACE
# undef INTERFACE
#endif
*/

/**
  This class is outdated. Use 
  gw_cref[acdk::lang::dmi::MetaInfoFlags] instead.
*/
class ACDK_CORE_PUBLIC Modifier 
{
public:
              
};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Modifier_h
