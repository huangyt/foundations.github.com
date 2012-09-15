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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/CharArrayWriter.h,v 1.18 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_CharArrayWriter_h
#define acdk_io_CharArrayWriter_h

#include "AbstractStorageWriter.h"
#include <acdk/lang/StringBuffer.h>
#include <acdk/locale/Encoder.h>
#include "AbstractCharWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(CharArrayWriter);

/** 
  Reads from an uccharArray
  API: Java
  
  @author Roger Rene Kommer
  @version $Revision: 1.18 $
  @date $Date: 2005/04/09 19:26:44 $
  @see InterfaceXXX
  @bug Incomplete, Untested, etc.

*/
class ACDK_CORE_PUBLIC CharArrayWriter 
: extends AbstractCharWriter
{
  ACDK_WITH_METAINFO(CharArrayWriter)
public:
  RuccharArray _sb;
  int _writePos;
  CharArrayWriter(int initialSize = 64, IN(RObject) iolock = Nil)
  : AbstractCharWriter(iolock)
  , _sb(new (allocator()) uccharArray(initialSize))
  , _writePos(0)
  {
  }
  virtual void writeChar(char c) { writeChar((ucchar)c); }
  virtual void writeChar(ucchar c) 
  {
    SYNCTHIS();
    ensureCapacity(_writePos + 1);
    _sb[_writePos++] = c;
  }
  foreign virtual void writeString(const char* cstr)
  {
    SYNCTHIS();
    int len = strlen((const char*)cstr);
    ensureCapacity(_writePos + len);
    for (int i = 0; i < len; i++)
      _sb[_writePos++] = cstr[i];
  }
  foreign virtual void writeString(const ucchar* cstr)
  {
    SYNCTHIS();
    int len = StringUtf8Utils::uc2length(cstr);
    ensureCapacity(_writePos + len);
    for (int i = 0; i < len; i++)
      _sb[_writePos++] = cstr[i];
  }
  virtual void writeString(IN(RString) str)
  {
    SYNCTHIS();
    int len = str->length();
    ensureCapacity(_writePos + len);
    for (String::iterator it = str->begin(); it < str->end(); ++it)
      _sb[_writePos++] = *it;
  }

  void reset()
  {
    _sb = new (allocator()) uccharArray(_sb->length());
  }
  RuccharArray tocharArray()
  {
    _sb->resize(_writePos);
    return _sb;
  }
  int size()
  {
    return _writePos;
  }
  int capacity()
  {
    return _sb->length();
  }
  foreign RString toString()
  {
    _sb->resize(_writePos);
    return new String(_sb);
  }
  void ensureCapacity(int newSize)
  {
    if (newSize <= _sb->length())
      return;
    int ns = _sb->length() * 2;
    if (ns < newSize)
      ns = newSize;
    _sb->resize(newSize);
  }
  foreign void flush() {}
  foreign void close() {}
};



} // namespace io 
} // namespace acdk 

#endif //acdk_io_CharArrayWriter_h


