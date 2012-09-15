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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/FieldPosition.cpp,v 1.6 2005/02/05 10:45:33 kommer Exp $





#include "FieldPosition.h"

namespace acdk {
namespace text {

FieldPosition::FieldPosition() 
  : Object(),
    _fieldID(0),
    _begin(0),
    _end(0)
{
}

FieldPosition::FieldPosition(int fieldID) 
  : Object(),
    _fieldID(fieldID),
    _begin(0),
    _end(0)
{
}

//virtual
FieldPosition::~FieldPosition() 
{
}

//virtual
bool
FieldPosition::equals(IN(RObject) obj)
{
  if (obj == Nil)
    return false;
  //### const prop if (obj == (RObject)this)
  //  return true;
  if (instanceof(obj,FieldPosition) == false)
    return false;
  RFieldPosition fp = RFieldPosition(obj);
  if (fp->getField() != _fieldID)
    return false;
  if (fp->getBeginIndex() != _begin)
    return false;
  if (fp->getEndIndex() != _end)
    return false;
  return true;
}

//virtual
RString
FieldPosition::toString()
{
  RString reVal = new String("FiedPosition");
  reVal = reVal + "[field=" +_fieldID + ",beginIndex=" + _begin + 
    ",endIndex=" + _end + "]"; 
  return reVal; 
}

} // Text
} // MM
