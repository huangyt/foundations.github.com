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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractReader.h,v 1.17 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractReader_h
#define acdk_io_AbstractReader_h

#include <acdk.h>

#include "IOException.h"
#include "Reader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(AbstractReader);

/** 
  Basic incomplete implementation for a Reader.
  This class already implements the IO locking mechanism.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC AbstractReader
: extends acdk::lang::Object,
  implements Reader
{
  ACDK_WITH_METAINFO(AbstractReader)
protected:

  /** 
    API: JDK modified
    In the JDK this member variable is named lock.
    But this is confusing with the method lock.
  */
  RObject iolock;
public :
  AbstractReader()  
  {
  }
  AbstractReader(IN(RObject) obj)  
  : iolock(obj)
  { 
  }
  virtual ~AbstractReader() 
  {
  }
  virtual void lock()
  {
    if (iolock != Nil)
      iolock->lock();
  }
  virtual void unlock()
  {
    if (iolock != Nil)
      iolock->unlock();
  }
  /**
    API: enhanced
    Set the Object, which will be used for synchronization
  */
  void setLock(IN(RObject) obj)
  {
    iolock = obj;
  }

  /**
    API: enhanced
    @return the Object, which will be used for synchronization
  */
  RObject getLock()
  {
    return iolock;
  }
  virtual jlong seek(SeekPos seekrel, jlong seekpos)
  {
    THROW1(IOException, ": seek is not supported "); 
    return 0;
  }

  virtual jlong skip(jlong n) { return seek(SeekCur, n); } // #### TODO wrong return value
  virtual int read() { return Reader::read(); }
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);
  overwrite void reset() = 0;
};


} // io
} // acdk

#endif //acdk_io_AbstractReader_h

