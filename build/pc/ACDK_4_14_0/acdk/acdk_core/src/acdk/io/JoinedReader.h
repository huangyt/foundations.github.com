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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/JoinedReader.h,v 1.11 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_io_JoinedReader_h
#define acdk_io_JoinedReader_h

#include "AbstractReader.h"
#include "FilterReader.h"
#include "Storage.h"

namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;


ACDK_DECL_CLASS(JoinedReader);

/** 
  Joines multiple Reader to one virtual stream
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:44:54 $
  
*/
class ACDK_CORE_PUBLIC JoinedReader
: extends AbstractReader,
  implements FilterReader
{
  ACDK_WITH_METAINFO(JoinedReader)
protected:
  RReaderArray _ins;
  int _curPos;
public :
  JoinedReader(IN(RReader) firstIn, IN(RObject) lock = Nil);
  virtual ~JoinedReader();
  void addIn(IN(RReader) in)
  {
    _ins->append(in);
  }
  virtual RReader getIn() 
  { 
    if (_curPos == -1)
      return Nil;
    return _ins[_curPos]; 
  }
  foreign virtual void setIn(IN(RReader) in)
  {
    addIn(in);
  }
  foreign virtual int available()  
  { 
    if (_curPos == -1)
      return 0;
    return _ins[_curPos]->available();
  }
  foreign virtual void close();
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos)
  {
    if (_curPos == -1)
      return 0;
    return _ins[_curPos]->seek(seekrel, seekpos);
  }

  foreign virtual jlong skip(jlong n) 
  { 
     if (_curPos == -1)
      return 0;
    return _ins[_curPos]->skip(n);
  }
  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);
  foreign virtual void reset();
  foreign virtual bool ready();
  foreign virtual void mark(int readAheadLimit);
  foreign virtual bool markSupported();
  foreign virtual RStorage getStorage() { return (RStorage)(RObject)getStorageReader(); }
  foreign virtual RReader getStorageReader();
};

} // io
} // acdk

#endif //acdk_io_JoinedReader_h

