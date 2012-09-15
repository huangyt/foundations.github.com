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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/NoSuchElementException.h,v 1.9 2005/02/05 10:44:56 kommer Exp $

#ifndef acdk_lang_NoSuchElementException_h
#define acdk_lang_NoSuchElementException_h


#include "DmiException.h"


namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(NoSuchElementException, DmiException);

/** 
  While processing a dynamic Method Invokation (like New, invoke, invoke_static, peek, poke, peek_static, poke_static)
  and the element cannot be found, this exception will be thrown.
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:44:56 $
*/  

class ACDK_CORE_PUBLIC NoSuchElementException
: extends DmiException
{
  ACDK_WITH_METAINFO(NoSuchElementException)
public:
  NoSuchElementException() : DmiException() { }
  NoSuchElementException(IN(RString) what) : DmiException(what) { }
};


} // lang
} // acdk

#endif //acdk_lang_NoSuchElementException_h

