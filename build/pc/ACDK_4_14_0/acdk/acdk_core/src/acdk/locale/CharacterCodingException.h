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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/locale/CharacterCodingException.h,v 1.8 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_locale_CharacterCodingException_h
#define acdk_locale_CharacterCodingException_h

#include <acdk/io/IOException.h>
#include <acdk/io/PrintWriter.h>

namespace acdk {
namespace locale {

ACDK_DECL_THROWABLE_FQ(CharacterCodingException, ::acdk::io::, IOException);
USING_CLASS(::acdk::io::, IOException);

/**
  Will be thrown if a character has wrong byte encoding.

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:56 $
*/
class ACDK_CORE_PUBLIC CharacterCodingException 
: extends acdk::io::IOException
{
  ACDK_WITH_METAINFO(CharacterCodingException)  
public:
  CharacterCodingException() : IOException(false) {}
  CharacterCodingException(IN(RString) what) : IOException(what) {}
};

} // locale
} // acdk

#endif //acdk_locale_CharacterCodingException_h

