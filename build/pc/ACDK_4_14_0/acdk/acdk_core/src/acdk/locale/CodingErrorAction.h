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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/CodingErrorAction.h,v 1.6 2005/02/05 10:45:02 kommer Exp $

#ifndef acdk_locale_CodingErrorAction_h
#define acdk_locale_CodingErrorAction_h

#include <acdk.h>

namespace acdk {
namespace locale {

/**
  Action to perform if a character
  cannot be encoded/decoded
*/
enum CodingErrorAction
{
  /** 
    The unmappable character will be
    simply ignored.
  */
  IgnoreCodingError,
  /*
    The unmappable character will be 
    replaces by Encoder::getEncodingReplacement()
  */
  ReplaceCodingError,
  /**
    An UnmappableCharacterException will be thrown
  */
  ReportCodingError
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, CodingErrorAction);



} // locale
} // acdk

#endif //acdk_locale_CodingErrorAction_h

