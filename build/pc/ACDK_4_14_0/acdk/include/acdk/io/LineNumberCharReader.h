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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/LineNumberCharReader.h,v 1.12 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_LineNumberCharReader_h
#define acdk_io_LineNumberCharReader_h

#include "AbstractCharFilterReader.h"


namespace acdk {
namespace io {


ACDK_DECL_INTERFACE(LineNumberCharReader);
/**
  Similar to acdk::io::LineNumberReader, but based on CharReader
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC LineNumberCharReader
: implements CharReader
{
   ACDK_WITH_METAINFO(LineNumberCharReader)
public:
     // CharReader
  int readChar() = 0;
  RString readString() = 0;
  void close() = 0;
  
  // LineNumberCharReader
  virtual RString readLine() = 0;
  virtual int getCharPos() = 0;
  virtual void setCharPos(int ch) = 0;
  virtual int getLineNumber() = 0;
  virtual void setLineNumber(int ln) = 0;
  virtual int getColumnNumber() = 0;
  virtual void setColumnNumber(int cn) = 0;
};

ACDK_DECL_CLASS(LineNumberCharReaderImpl);

/**
  Standard filter implementation LineNumberCharReader.
  
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC LineNumberCharReaderImpl
: extends AbstractCharFilterReader
, implements LineNumberCharReader
{
  ACDK_WITH_METAINFO(LineNumberCharReaderImpl)
protected:
  int _charPos;
  int _markedCharPos;
  int _lineNumber;
  int _markedLineNumber;
  int _xPos;
  int _markedXPos;
  int _extra_char;
public:
  LineNumberCharReaderImpl(IN(RCharReader) in, IN(RObject) iolock = Nil);
  
  // Reader
  foreign void close() { AbstractCharFilterReader::close(); }

  // LineNumberCharReader
  foreign virtual int getCharPos() { return _charPos; }
  foreign virtual void setCharPos(int ch) { _charPos = ch; }
  
  foreign virtual int getLineNumber() { return _lineNumber; }
  foreign virtual void setLineNumber(int ln) { _lineNumber = ln; }
  foreign int getColumnNumber()  { return _xPos; }
  foreign void setColumnNumber(int cn)  { _xPos = cn; }

  foreign virtual int readChar();
  foreign virtual RString readString();

  foreign virtual RString readLine();

  
};


} // io
} // acdk

#endif //acdk_io_LineNumberCharReader_h

