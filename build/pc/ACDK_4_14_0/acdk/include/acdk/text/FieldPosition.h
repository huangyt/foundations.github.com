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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/FieldPosition.h,v 1.7 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_text_FieldPosition_h
#define acdk_text_FieldPosition_h

#include "text.h"

namespace acdk {
namespace text {


using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(FieldPosition);

class ACDK_TEXT_PUBLIC FieldPosition
  : public acdk::lang::Object
{
  ACDK_WITH_METAINFO(FieldPosition)
private:
  int _fieldID;
  int _begin;
  int _end;
public:
  FieldPosition();
  FieldPosition(int fieldID);
  virtual ~FieldPosition();
  virtual bool equals(IN(RObject) obj);
  virtual RString toString();

  int getField() { return _fieldID; }
  int getBeginIndex() { return _begin; }
  void setBeginIndex(int begin) 
  { 
    _begin = begin; 
  }
  int getEndIndex() { return _end;}
  void setEndIndex(int end) { _end = end;}
  

protected:


};



} // text
} // acdk

#endif //acdk_text_FieldPosition_h

