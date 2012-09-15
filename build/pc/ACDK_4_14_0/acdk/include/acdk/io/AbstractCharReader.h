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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractCharReader.h,v 1.8 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractCharReader_h
#define acdk_io_AbstractCharReader_h

#include "CharReader.h"

namespace acdk {
namespace io {



ACDK_DECL_CLASS(AbstractCharReader); 

/** 
  Abstract implementation for CharReader.
  This class already implements the IO locking mechanism.

  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC AbstractCharReader 
: extends acdk::lang::Object
, implements CharReader
{
  ACDK_WITH_METAINFO(AbstractCharReader)
protected:
  RObject _iolock;
public :
  AbstractCharReader(IN(RObject) lock = Nil) 
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
  virtual int readChar() = 0;
  virtual RString readString();
};


} // namespace io 
} // namespace acdk 

#endif //acdk_io_AbstractCharReader_h


