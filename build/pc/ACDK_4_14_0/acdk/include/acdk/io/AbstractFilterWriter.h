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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractFilterWriter.h,v 1.14 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractFilterWriter_h
#define acdk_io_AbstractFilterWriter_h

#include "AbstractWriter.h"
#include "FilterWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(AbstractFilterWriter);

/** 
  Implements a filter for a given Writer.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.14 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC AbstractFilterWriter
: extends AbstractWriter,
  implements FilterWriter
{
  ACDK_WITH_METAINFO(AbstractFilterWriter)
protected:
  RWriter _out;
public :
  AbstractFilterWriter(IN(RWriter) out, IN(RObject) lock = Nil)
  : AbstractWriter(lock),
    _out(out)
  {
  }
  virtual void setOut(IN(RWriter) writer)
  {
    _out = writer;
  }
  
  virtual RWriter getOut() { return _out; }

  /** Gets the storage reader. If this contains an FilterReader instead of
   *  a Storage, the storage reader of this filter reader will be
   *  returned (recursive). */
  virtual RStorage getStorage();
  virtual void write(byte c)
  {
    _out->write(c);
  }
  foreign virtual void write(const byte* cstr, int offset, int len)
  {
    _out->write(cstr, offset, len);
  }
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  }
  virtual void flush()
  {
    _out->flush();
  }
  virtual void close()
  {
    _out->close();
  }
  virtual RWriter getStorageWriter()
  {
    if (instanceof(_out, FilterWriter) == true)
      return RFilterWriter(_out)->getStorageWriter();
    return _out;
  }
};

} // io
} // acdk

#endif //acdk_io_AbstractFilterWriter_h

