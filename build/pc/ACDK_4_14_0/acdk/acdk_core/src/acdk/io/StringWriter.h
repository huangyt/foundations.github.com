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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/StringWriter.h,v 1.7 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_StringWriter_h
#define acdk_io_StringWriter_h

#include "CharWriter.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(StringWriter);

/** 
  Reads character, not bytes
  Similar to Javas InputStreamReader
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC StringWriter
: extends acdk::lang::Object
, implements CharWriter 

{
  ACDK_WITH_METAINFO(StringWriter)
protected:
  RStringBuffer _stringBuffer;
public:
  StringWriter() 
  : _stringBuffer(new StringBuffer())
  {
  }
  RStringBuffer getStringBuffer() { return _stringBuffer; }
  void setStringBuffer(IN(RStringBuffer) sb) { _stringBuffer = sb; }
  RString getString() { return _stringBuffer->toString(); }
  void setString(IN(RString) str) { _stringBuffer = new StringBuffer(str); }

  foreign virtual void writeChar(char c) { _stringBuffer->append(c); }
  foreign virtual void writeChar(ucchar c) { _stringBuffer->append(c); }
  foreign virtual void writeString(const char* cstr) { _stringBuffer->append(cstr); }
  foreign virtual void writeString(const ucchar* cstr) { _stringBuffer->append(cstr); }
  foreign virtual void writeString(IN(RString) str) { _stringBuffer->append(str); }
  foreign virtual void flush() {}
  foreign virtual void close() {}
};



} // namespace io 
} // namespace acdk 

#endif //acdk_io_StringWriter_h


