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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ClassNotFoundException.h,v 1.11 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_ClassNotFoundException_h
#define acdk_lang_ClassNotFoundException_h


#include "NoSuchDmiElementException.h"


namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(ClassNotFoundException, NoSuchDmiElementException);

/** 
  Will be thrown if a class cannot be found.
  API: Java
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:48 $
  @see acdk::lang::Class
  @see acdk::lang::ClassLoader
*/

class ACDK_CORE_PUBLIC ClassNotFoundException
: extends NoSuchDmiElementException
{
  ACDK_WITH_METAINFO(ClassNotFoundException)
public:
  ClassNotFoundException() : NoSuchDmiElementException() { }
  ClassNotFoundException(IN(RString) what) : NoSuchDmiElementException(what) { }
};


} // lang
} // acdk

#endif //acdk_lang_ClassNotFoundException_h

