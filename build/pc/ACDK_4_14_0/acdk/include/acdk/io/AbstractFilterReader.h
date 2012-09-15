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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractFilterReader.h,v 1.14 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractFilterReader_h
#define acdk_io_AbstractFilterReader_h

#include "AbstractReader.h"
#include "FilterReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

class AbstractFilterReader;
ACDK_DECL_CLASS(AbstractFilterReader);

/** 
  Implements a filter for a given Reader.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.14 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC AbstractFilterReader
: extends AbstractReader,
  implements FilterReader
{
  ACDK_WITH_METAINFO(AbstractFilterReader)
protected:
  RReader _in;
public :
  AbstractFilterReader(IN(RReader) in, IN(RObject) lock = Nil)
  : AbstractReader(lock),
    _in(in)
  {
  }
  virtual void setIn(IN(RReader) reader)
  {
    _in = reader;
  }
  virtual RReader getIn() { return _in; }
  /** Gets the storage reader. If this contains an FilterReader instead of
   *  a Storage, the storage reader of this filter reader will be
   *  returned (recursive). */
  virtual RStorage getStorage();
  virtual int available()  
  { 
    return _in->available(); 
  }
  virtual void close() 
  { 
    _in->close();
  }
  virtual jlong seek(SeekPos seekrel, jlong seekpos)
  {
    return _in->seek(seekrel, seekpos);
  }

  virtual jlong skip(jlong n) 
  { 
    return _in->skip(n);
  }
  virtual int read()
  {
    return _in->read();
  }
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1) 
  {
    return _in->read(buffer, offset, len);
  }
  foreign virtual int read(byte* buffer, int offset, int len)
  {
    return _in->read(buffer, offset, len);
  }
  virtual void reset()
  {
    _in->reset();
  }
  virtual bool ready()
  {
    return _in->ready();
  }
  virtual void mark(int readAheadLimit)
  {
    _in->mark(readAheadLimit);
  }
  virtual bool markSupported()
  { 
    return  _in->markSupported();
  }
  virtual RReader getStorageReader()
  {
    if (instanceof(_in, FilterReader) == true)
      return RFilterReader(_in)->getStorageReader();
    return _in;
  }
};

} // io
} // acdk

#endif //acdk_io_AbstractFilterReader_h

