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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ClassCastException.h,v 1.9 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_ClassCastException_h
#define acdk_lang_ClassCastException_h

#include "Exception.h"

namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(ClassCastException, Exception);

ACDK_DECL_THROWABLE(ClassCastException, Exception);

/** 
  Will thrown if casting from one type to another will fail

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:48 $
*/
class ACDK_CORE_PUBLIC ClassCastException 
: extends Exception
{
  ACDK_WITH_METAINFO(ClassCastException)
protected:
  foreign const dmi::ClazzInfo* _fromClazz;
  foreign const dmi::ClazzInfo* _toClazz;
public:
  ClassCastException() 
  : Exception() 
  , _fromClazz(0)
  , _toClazz(0)
  { 
  }
  ClassCastException(IN(RString) what) 
  : Exception(what) 
  , _fromClazz(0)
  , _toClazz(0)
  { 
  }
  ClassCastException(IN(RClass) fromObject, IN(RClass) toObject);
  foreign ClassCastException(const dmi::ClazzInfo* from, const dmi::ClazzInfo* to);
  foreign const dmi::ClazzInfo* fromClazz() { return _fromClazz; }
  foreign const dmi::ClazzInfo* toClazz() { return _toClazz; }
protected:
  void _setWhat();

};


} // lang
} // acdk

#endif //acdk_lang_ClassCastException_h

