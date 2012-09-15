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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PipedReader.h,v 1.15 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_PipedReader_h
#define acdk_io_PipedReader_h

#include "AbstractStorageReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(PipedReader);
ACDK_DECL_CLASS(PipedWriter);

/**
  A create a Reader end of a pipe construct.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC PipedReader
: extends AbstractStorageReader
{
  ACDK_WITH_METAINFO(PipedReader)
private:
  RPipedWriter _in;
  static int PIPE_SIZE;
  bool _connected;
  bool _closed;
  RbyteArray _buffer;
  int _inPos;
  int _outPos;
public:
  PipedReader();
  PipedReader(IN(RPipedWriter) in);

  virtual void connect(IN(RPipedWriter) in);
  /// implements from Reader
  foreign virtual void close();
  /// implements from Reader
  foreign virtual bool ready();
  /// implements from Reader
  foreign virtual int read();
  /// implements from Reader
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  /// implements from Reader
  foreign virtual int read(byte* buffer, int offset, int len) { return AbstractStorageReader::read(buffer, offset, len); }
  /// implements from Reader
  foreign virtual void reset();
protected:
  void pipewrite(byte c);
  void pipewrite(IN(RbyteArray) buffer, int offset, int len = -1);
  void pipewrite(const byte* buffer, int offset, int len);
  
  friend class PipedWriter;
  
// Storage
  /// implements from Reader
  foreign virtual RString getDeviceName();
  /// implements from Storage
  foreign virtual bool isWriteable();
  /// implements from Storage
  foreign virtual bool isReadable();
};


} // io
} // acdk

#endif //acdk_io_PipedReader_h

