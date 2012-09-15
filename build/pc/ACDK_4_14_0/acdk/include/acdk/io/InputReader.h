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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/InputReader.h,v 1.19 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_InputReader_h
#define acdk_io_InputReader_h


#include "AbstractCharFilterReader.h"


namespace acdk {
namespace io {

using namespace acdk::lang;
enum SeekPos;

ACDK_DECL_CLASS(InputReader);

/**
  The Class InputReader is the counterpart to PrintWriter.
  It is just a Wrapper on a given Reader to read from Reader into standard structures.<br>
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.19 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/

class ACDK_CORE_PUBLIC InputReader
: extends AbstractCharFilterReader
{
  ACDK_WITH_METAINFO(InputReader)
  /** next call should return -1 */
  bool _isEof;
  /** already returned eof -> throw EOF ex */
  bool _eofReturned;
  bool _skipNextNl;
public:

  static RObject create_instance() { return new InputReader(RCharReader(Nil)); }
  

  InputReader(IN(RReader) in, IN(acdk::locale::RDecoder) decoder = Nil, IN(RObject) lock = Nil);
  InputReader(IN(RCharReader) in);
  
  /*
  foreign virtual int available() { return _in->available(); }
  foreign virtual void close() { _in->close(); }
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos)
  {
    return _in->seek(seekrel, seekpos);
  }
  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);
  */  

  
  /**
    @return RString read until isspace.
  */
  virtual RString readAString();
  /**
    @return RString reads until EOL
  */
  virtual RString readLine();
  /**
    @return int reads int
  */
  virtual int readInt();
  /**
    @return bool read bool
  */
  virtual bool readBoolean();
  /**
    @return double read double
  */
  virtual double readDouble();
  /**
    @return jlong reads jlong
  */
  virtual jlong readLong();

  /**
    reads len Bytes or until EOF is reached.
    @return number of skiped bytes
  */
  virtual int skipBytes(int len);
  bool eof() { return _isEof; }
  /**
    read C quoted string
    @see PrintWriter::printQuoted
  */
  RString readQuoted();
};


} // io
} // acdk

#endif //acdk_io_InputReader_h

