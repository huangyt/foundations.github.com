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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/BufferedReader.h,v 1.13 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_BufferedReader_h
#define acdk_io_BufferedReader_h

#include "AbstractFilterReader.h"


namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(BufferedReader); 

/**
  Buffers input from a given Reader
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:44 $
*/
class ACDK_CORE_PUBLIC BufferedReader
: extends AbstractFilterReader
{
  ACDK_WITH_METAINFO(BufferedReader)

protected:
  static int DEFAULT_BUFFER_SIZE;

protected:
  RbyteArray _buffer;
  int _count;
  int _pos;
  int _markpos;
  int _marklimit;
  RbyteArray _markbuf;
  transient int _markbufpos;
  transient int _markbufcount;
  transient bool _doing_reset;
  transient bool _primed;
  transient bool _eof;
public:
  BufferedReader(IN(RReader) in, int buffsize = BufferedReader::DEFAULT_BUFFER_SIZE);
  
  virtual void close() { _in->close(); }
  virtual int available();
  
  virtual jlong seek(SeekPos seekrel, jlong seekpos) { return _in->seek(seekrel, seekpos); }
  
  virtual void mark(int readlimit);
  virtual bool markSupported() { return true; }
  virtual void reset();
  virtual bool ready();
  virtual jlong skip(jlong num_bytes);
  virtual int read();
  virtual int read(IN(RbyteArray) buf, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len) { return AbstractFilterReader::read(buffer, offset, len); }
private:
  void _fillBuffer();
};

} // io
} // acdk

#endif //acdk_io_BufferedReader_h

