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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Void.h,v 1.8 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_Void_h
#define acdk_lang_Void_h

namespace acdk {
namespace lang {


ACDK_DECL_CLASS(Void);

/** 
  Object wrapper for a void type.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:51 $
*/  
class ACDK_CORE_PUBLIC Void
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Void)
public:
  static RClass getTYPE();
};


} // lang
} // acdk



#endif //acdk_lang_Void_h

