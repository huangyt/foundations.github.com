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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/ParsePosition.cpp,v 1.8 2005/03/08 18:49:55 kommer Exp $





#include "ParsePosition.h"

namespace acdk {
namespace text {

ParsePosition::ParsePosition() 
  : Object(),
    _index(0),
    _errorIndex(-1)
{
}

ParsePosition::ParsePosition(int index) 
  : Object(),
    _index(index),
    _errorIndex(-1)
{
}

ParsePosition::~ParsePosition() 
{
}

bool
ParsePosition::equals(IN(RObject) obj)
{
  if (obj == Nil)
    return false;
  
  if ( instanceof(obj,ParsePosition) == false)
    return false;
  
  RParsePosition pp = RParsePosition(obj);
  
  if (pp->getIndex() != getIndex())
    return false;
  
  if (pp->getErrorIndex() != getErrorIndex()) 
    return false;
  
  return true;
}

RString
ParsePosition::toString()
{
  RString reVal = new String("ParsePosition");
  reVal = reVal + "[index=" + getIndex() + ",errorIndex=" + _errorIndex + "]"; 
  return reVal; 
 
}

} // text
} // acdk

