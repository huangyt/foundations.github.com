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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractWriter.h,v 1.13 2005/03/01 10:12:47 kommer Exp $

#ifndef acdk_io_AbstractWriter_h
#define acdk_io_AbstractWriter_h

#include <acdk.h>
#include "Writer.h"
#include <acdk/lang/ArrayIndexOutOfBoundsException.h>

namespace acdk {
namespace io {


using namespace acdk::lang;


ACDK_DECL_CLASS(AbstractWriter);

/**
  An default implementation of Writer with locking mechanism.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/03/01 10:12:47 $
  
  @see Writer
*/
    

class ACDK_CORE_PUBLIC AbstractWriter
: extends acdk::lang::Object
, implements Writer
{
  ACDK_WITH_METAINFO(AbstractWriter)
protected:
  /** 
    API: JDK modified
    In the JDK this member variable is named lock.
    But this is confusing with the method lock.
  */
  acdk::lang::RObject iolock;
public :
  
  AbstractWriter()
  :  iolock()
  {
    /* implemetation note:
      to avoid cyclic reference here (and a waste of Mutex) I don't follow the JDK way
      // no: lock = this;
    */
  }
  AbstractWriter(IN(RObject) olock)
  :  iolock(olock)
  {
  }
  
  overwrite void flush() = 0;
  overwrite void close() = 0;
  

  virtual void lock()
  {
    if (iolock != Nil)
      iolock->lock();
    /*else
      Object::lock();
    */
  }
  virtual void unlock()
  {
    if (iolock != Nil)
      iolock->unlock();
    /*else
      Object::unlock();
    */
  }
  /**
    API: enhanced
    Set the Object, which will be used for synchronization
  */
  virtual void setLock(IN(RObject) obj)
  {
    iolock = obj;
  }

  /**
    API: enhanced
    @return the Object, which will be used for synchronization
  */
  virtual RObject getLock()
  {
    return iolock;
  }

  foreign virtual void write(const byte* cstr, int offset, int len);
  /** 
    default implementation:
    byte cbuf[2]; cbuf[1] = 0; cbuf[0] = c;
    return write((const byte*)cbuf, 0, 1);
  */
  overwrite void write(byte c) = 0;
  /** 
    default implementation:
    return write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  */
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    if (len == -1)
      len = ch->length() - offset;
    if (ch->length() < offset + len)
      THROW0(ArrayIndexOutOfBoundsException);
    write(ch->data(), offset, len);
  }
};



} // io
} // acdk

#endif //acdk_io_AbstractWriter_h

