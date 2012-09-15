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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PipedWriter.h,v 1.13 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_PipedWriter_h
#define acdk_io_PipedWriter_h

#include "AbstractStorageWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(PipedReader);
ACDK_DECL_CLASS(PipedWriter);

/**
  A create a Reader end of a pipe construct.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC PipedWriter
: extends AbstractStorageWriter
{
  ACDK_WITH_METAINFO(PipedWriter)
private:
  RPipedReader _out;

  bool _connected;
  bool _closed;
public:
  PipedWriter();
  PipedWriter(IN(RPipedReader) out);
  
  /// implements from Writer
  foreign virtual void close();

  virtual void connect(IN(RPipedReader) out);

  /// implements from Writer
  foreign virtual void flush();
  /// implements from Writer
  foreign virtual void write(byte c);
  /// implements from Writer
  foreign virtual void write(const byte* cstr, int offset, int len);
  /// implements from Writer
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1)
  {
    write(ch->data(), offset, len == -1 ? ch->length() - offset : len);
  }
// Storage
  /// implements from Storage
  foreign virtual RString getDeviceName();
  /// implements from Storage
  foreign virtual bool isWriteable();
  /// implements from Storage
  foreign virtual bool isReadable();
 
  friend class PipedReader;
};


} // io
} // acdk

#endif //acdk_io_PipedWriter_h

