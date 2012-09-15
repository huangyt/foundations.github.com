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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractCharWriter.h,v 1.9 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractCharWriter_h
#define acdk_io_AbstractCharWriter_h

#include "CharWriter.h"
#include "../locale/Encoder.h"

namespace acdk {
namespace io {


ACDK_DECL_INTERFACE(AbstractCharWriter);

/** 
  Reads character, not bytes
  Similar to Javas InputStreamReader.
  Abstract implementation for CharReader.
  This class already implements the IO locking mechanism.
   API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC AbstractCharWriter 
: extends acdk::lang::Object
, implements acdk::io::CharWriter
{
  ACDK_WITH_METAINFO(AbstractCharWriter)
protected:
  RObject _iolock;
public:
  AbstractCharWriter(IN(RObject) lock = Nil)
    : _iolock(lock)
  {
  }
  inline void lock()
  {
    if (_iolock != Nil)
      _iolock->lock();
  }
  inline void unlock()
  {
    if (_iolock != Nil)
      _iolock->unlock();
  }
  /**
    API: enhanced
    Set the Object, which will be used for synchronization
  */
  void setLock(IN(RObject) obj)
  {
    _iolock = obj;
  }

  /**
    API: enhanced
    @return the Object, which will be used for synchronization
  */
  RObject getLock()
  {
    return _iolock;
  }
  
  //foreign virtual acdk::locale::REncoder getEncoder() { return _encoder; }
  // foreign virtual void setEncoder(IN(acdk::locale::REncoder) encoder) { _encoder = encoder; }

  virtual void writeChar(char c)  = 0;
  virtual void writeChar(ucchar c)  = 0;
  foreign virtual void writeString(const char* cstr) 
  {
    for (; *cstr != 0; ++cstr)
      writeChar(*cstr);
  }
  foreign virtual void writeString(const ucchar* cstr) 
  {
    for (; *cstr != 0; ++cstr)
      writeChar(*cstr);
  }
  virtual void writeString(IN(RString) str) 
  {
    String::iterator it = str->begin();
    String::iterator end = str->end();
    for (; it < end; ++it)
      writeChar(*it);
  }
  foreign virtual void flush() = 0;
  foreign virtual void close() = 0;
};



} // namespace io 
} // namespace acdk 

#endif //acdk_io_CharArrayWriter_h


