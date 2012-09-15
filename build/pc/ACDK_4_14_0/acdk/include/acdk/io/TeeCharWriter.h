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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/TeeCharWriter.h,v 1.7 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_TeeCharWriter_h
#define acdk_io_TeeCharWriter_h

#include "AbstractCharWriter.h"
#include "TeeWriter.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(TeeCharWriter);

/** 
  Duplicate written characters into two given CharWriter.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC TeeCharWriter 
: extends AbstractCharWriter
{
  ACDK_WITH_METAINFO(TeeCharWriter)
protected:
  RCharWriter _first;
  RCharWriter _second;
public:
  TeeCharWriter(IN(RCharWriter) first, IN(RCharWriter) second)
    : _first(first)
    , _second(second)
  {
  }
  virtual void writeChar(char c)
  {
    _first->writeChar(c);
    _second->writeChar(c);
  }
  virtual void writeChar(ucchar c) 
  {
    _first->writeChar(c);
    _second->writeChar(c);
  }
  virtual void writeString(IN(RString) str) 
  {
    _first->writeString(str);
    _second->writeString(str);
  }
  virtual void flush() 
  {
    _first->flush();
    _second->flush();
  }
  virtual void close()
  {
    _first->close();
    _second->close();
  }
  virtual RWriter getWriter(IN(acdk::locale::RDecoder) decoder = Nil) { return new TeeWriter(_first->getWriter(decoder), _second->getWriter(decoder)); } 
};



} // namespace io 
} // namespace acdk 

#endif //acdk_io_TeeCharWriter_h


