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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Member.h,v 1.7 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_reflect_Member_h
#define acdk_lang_reflect_Member_h

namespace acdk {
namespace lang {
namespace reflect {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Member);

class ACDK_CORE_PUBLIC Member
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Member)
private:
public:
 virtual RClass getDeclaringClass() = 0;
 virtual int getModifiers() = 0;
 virtual RString getName() = 0;
};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_Member_h

