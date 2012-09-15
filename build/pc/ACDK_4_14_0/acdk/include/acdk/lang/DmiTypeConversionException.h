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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/DmiTypeConversionException.h,v 1.10 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_DmiTypeConversionException_h
#define acdk_lang_DmiTypeConversionException_h


#include "DmiException.h"


namespace acdk {
namespace lang {

ACDK_DECL_THROWABLE(DmiTypeConversionException, DmiException);

/** 
  While processing a dynamic Method Invokation (like New, invoke, invoke_static, peek, poke, peek_static, poke_static)
  and one of the param data types cannot be converted.
  @see gw_ref[acdk_hb_dmi]
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:48 $
*/  

class ACDK_CORE_PUBLIC DmiTypeConversionException
: extends DmiException
{
  ACDK_WITH_METAINFO(DmiTypeConversionException)
public:
  DmiTypeConversionException() : DmiException() { }
  DmiTypeConversionException(IN(RString) what) : DmiException(what) { }
};


} // lang
} // acdk

#endif //acdk_lang_DmiTypeConversionException_h

