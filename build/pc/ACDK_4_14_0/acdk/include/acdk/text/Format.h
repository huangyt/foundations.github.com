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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/Format.h,v 1.11 2005/03/07 18:32:54 kommer Exp $

#ifndef acdk_text_Format_h
#define   acdk_text_Format_h

#include <acdk.h>
#include <acdk/io/Serializable.h>
#include <acdk/lang/Cloneable.h>

#include "text.h"

namespace acdk {
namespace text {

using namespace acdk::lang;

ACDK_DECL_CLASS(FieldPosition);

ACDK_DECL_CLASS(ParsePosition);

ACDK_DECL_CLASS(Format);

/**
 *
 */
class ACDK_TEXT_PUBLIC  Format
: public acdk::lang::Object,
  implements acdk::io::Serializable,
  implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(Format)
public:
  /**
     Constructor
   */
  Format();

  /**
     Destructor
   */
  virtual ~Format();
public:
  /**
     This method formats an Object into a String.
     
     @param obj The Object to format
     @return The formatted String
     @exeption IllegalArgumentException If the Object cannot be formatted
   */
  virtual RString format(IN(RObject) obj);
  
  /**
     This method formats an Object into a String an appends the String to a
     StringBuffer.
     
     @param obj The Object to format.
     @param sb The StringBuffer to append to.
     @param pos The desired FieldPosition, which is also updated by this call
     @return The updated StringBuffer.
     @exeption IllegalArgumentException If the Object cannot be formatted
   */
  virtual RStringBuffer format(IN(RObject) obj, IN(RStringBuffer) sb, IN(RFieldPosition) pos) = 0;
  
  /**
     This method parses aString and coverts the parsed contends into an Object.
     
     @param str The String to parse.
   *
     @return The resulting Object.
     
     @exception ParseException If the String cannot be parsed.
   */
  virtual RObject parseObject(IN(RString) str);
  
  virtual RObject parseObject(IN(RString) str, IN(RParsePosition) pos) = 0;
  /**
     Creates a copy of this object.
     
     @return The copied Object.
   */
  virtual RObject clone();
  static RString dos2unix(IN(RString) str);
  static RString unix2dos(IN(RString) str);
  static RString dumpbin(IN(RbyteArray) data, int wide);
  static RString hexToString(IN(RcharArray) ch, int offset = 0, int length = 1);

  foreign static RString hexToString(const char* buffer, int length);
protected:
  
private:
  
};


} // text
} // acdk

#endif //acdk_text_Format_h

