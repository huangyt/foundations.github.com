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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/TeeWriter.h,v 1.13 2005/02/05 10:44:54 kommer Exp $

#ifndef acdk_io_TeeWriter_h
#define acdk_io_TeeWriter_h

#include "AbstractWriter.h"
#include "FilterWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(TeeWriter);

/** 
  Tee writer spawn a writer stream to 2 writer streams 
  API: ACDK<br>
  The default operatations of FilterWriter acts on the first output Writer.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/02/05 10:44:54 $
  
*/

class ACDK_CORE_PUBLIC TeeWriter
: extends AbstractWriter,
  implements FilterWriter
{
  ACDK_WITH_METAINFO(TeeWriter)
protected:
  RWriter _out1;
  RWriter _out2;
public :
  TeeWriter(IN(RWriter) out1, IN(RWriter) out2, IN(RObject) lock = Nil)
  : AbstractWriter(lock),
    _out1(out1),
    _out2(out2)
  {
  }
  foreign virtual void setOut(IN(RWriter) writer)
  {
    _out1 = writer;
  }
  
  foreign virtual RWriter getOut() { return _out1; }
  
  foreign virtual RStorage getStorage();
  foreign virtual void write(byte c)
  {
    if (_out1 != Nil)
      _out1->write(c);
    if (_out2 != Nil)
      _out2->write(c);
  }
  foreign virtual void write(const byte* cstr, int offset, int len)
  {
    if (_out1 != Nil)
      _out1->write(cstr, offset, len);
    if (_out2 != Nil)
      _out2->write(cstr, offset, len);
  }
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  }
  foreign virtual void flush()
  {
    if (_out1 != Nil)
      _out1->flush();
    if (_out2 != Nil)
      _out2->flush();
  }
  foreign virtual void close()
  {
    if (_out1 != Nil)
      _out1->close();
    if (_out2 != Nil)
      _out2->close();
  }
  foreign virtual RWriter getStorageWriter()
  {
    if (instanceof(_out1, FilterWriter) == true)
      return RFilterWriter(_out1)->getStorageWriter();
    return _out1;
  }
};

} // io
} // acdk

#endif //acdk_io_TeeWriter_h

