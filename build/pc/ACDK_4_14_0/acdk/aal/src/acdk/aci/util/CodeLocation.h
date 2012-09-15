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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/util/CodeLocation.h,v 1.4 2005/02/05 10:44:51 kommer Exp $


#ifndef acdk_aci_util_CodeLocation_h
#define acdk_aci_util_CodeLocation_h

#include <acdk.h>
#include "../Config.h"

namespace acdk {
namespace aci {
namespace util {



ACDK_DECL_CLASS(CodeLocation);
/**
  @todo make it more abstract and also make it
        usable for compiled binary format
*/
class ACDK_ACI_PUBLIC CodeLocation
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(CodeLocation)
private:
  /**
    @todo replace with Source
  */
  RString _fileName;
  /**
    Character offset
  */
  int _charPos;
  /**
    column (vertical) position
  */
  int _columnPos;
  /**
    line number
  */
  int _linePos;
  /**
    end of source location
  */
  int _endCharPos;
public:
  CodeLocation()
  : _fileName("")
  , _charPos(0)
  , _columnPos(0)
  , _linePos(0)
  , _endCharPos(0)
  {
  }
  CodeLocation(IN(RString) fileName, int charPos, int columnPos, int linePos, int endCharPos = -1)
  : _fileName(fileName)
  , _charPos(charPos)
  , _columnPos(columnPos)
  , _linePos(linePos)
  , _endCharPos(endCharPos)
  {
  }
  RString toString()
  {
    return SBSTR("SC:" << _charPos << ";EC:" << _endCharPos << ";LN:" << _linePos << ";CL:" << _columnPos);
  }
  void reset()
  {
    _charPos = _columnPos = _linePos = _endCharPos = 0;
  }
  inline int getCharPos() const { return _charPos; }
  inline void setCharPos(int pos) { _charPos = pos; }
  inline int getEndCharPos() const { return _endCharPos; }
  inline void setEndCharPos(int pos) { _endCharPos = pos; }
  inline int getColumnPos() const { return _columnPos; }
  inline void setColumnPos(int pos) { _columnPos = pos; }
  inline int getLinePos() const { return _linePos; }
  inline void setLinePos(int pos) { _linePos = pos; }
  RCodeLocation getCloned() const { return new CodeLocation(_fileName, _charPos, _columnPos, _linePos, _endCharPos); }
  
};

} // util
} // aci
} // acdk


#endif //acdk_aci_util_CodeLocation_h

