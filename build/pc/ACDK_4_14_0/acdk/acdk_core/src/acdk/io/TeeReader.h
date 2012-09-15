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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/TeeReader.h,v 1.11 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_io_TeeReader_h
#define acdk_io_TeeReader_h

#include "AbstractReader.h"
#include "FilterReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(TeeReader);

/** 
  Tee reader spawn a reader stream into 1 reader and 1 writer streams.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:44:54 $
  
*/
class ACDK_CORE_PUBLIC TeeReader 
: extends AbstractFilterReader,
  implements FilterReader
{
  ACDK_WITH_METAINFO(TeeReader)
private:
  RWriter _out;
public:
  TeeReader(IN(RReader) from, IN(RWriter) to)
  : AbstractFilterReader(from),
    _out(to)
  {
  }
  foreign RWriter getOut() { return _out; }
  foreign virtual int read()
  {
    int i = _in->read();
    if (i == -1)
      return i;
    _out->write((byte)i);
    return i;
  }
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1)
  {
    int erg = _in->read(buffer, offset, len);
    if (erg != 0)
      _out->write(buffer, offset, erg);
    return erg;
  }
  foreign virtual int read(byte* buffer, int offset, int len)
  {
    int erg = _in->read(buffer, offset, len);
    if (erg != 0)
      _out->write(buffer, offset, erg);
    return erg;
  }
};


} // io
} // acdk

#endif //acdk_io_TeeReader_h

