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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/LineNumberReader.h,v 1.15 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_LineNumberReader_h
#define acdk_io_LineNumberReader_h

#include "InputReader.h"
#include "ByteToCharReader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

class LineNumberReader;
ACDK_DECL_CLASS(LineNumberReader);

/**
  A filter reader provides information about
  line and column of current read position.
  @see acdk::io::LineNumberCharReader for character based LineNumberReader
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC LineNumberReader
: extends AbstractFilterReader
{
  ACDK_WITH_METAINFO(LineNumberReader)
protected:
  int _charPos;
  int _markedCharPos;
  int _lineNumber;
  int _markedLineNumber;
  int _xPos;
  int _markedXPos;
  /// 0 not eof, 1 at eof 2 behind eof
  byte _eof;
public:
  LineNumberReader(IN(RReader) in, IN(RObject) lock = Nil);
  /**
    return the line number of current read position
  */
  virtual int getLineNumber() { return _lineNumber; }
  virtual void setLineNumber(int ln) { _lineNumber = ln; }
  virtual void mark(int readlimit);
  foreign virtual void reset();
  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len) { return _in->read(buffer, offset, len); }
  /**
    return the column position 
  */
  int getXPos() { return _xPos; }
  /**
    provides information of current readed byte position
  */
  int getBytePos() { return _charPos; }
private:
  int _read();
};


} // io
} // acdk

#endif //acdk_io_LineNumberReader_h
