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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/Reader.h,v 1.23 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_Reader_h
#define acdk_io_Reader_h

#include <acdk.h>


#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Storage);


/**
  used by Reader::seek(SeekPos seekrel, jlong seekpos) to
  determine the seek direction
*/
enum SeekPos 
{
  /** seek forward counting from current position */
  SeekCur,
  /** seek counting backward from end position */
  SeekEnd,
  /** seek counting forward from start position */
  SeekSet
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, SeekPos);

ACDK_DECL_INTERFACE(CharReader);
ACDK_DECL_INTERFACE(Reader);

/**
  General byte base reader
  @note 
    To implement a reader a function has either to overload
    read() or read(byte* buffer, int offset, int len);

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.23 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC Reader
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Reader)
public :
  /**
    returns the number of bytes available in this stream before blocking
    Some Reader always return 0
  */
  virtual int available() { return 0; }
  /**
    closes this Reader.
    Note: Some Reader must be closed explicitaly
    Note: Reader implementation must be avare from closing 
          a reader more than one
  */
  foreign virtual void close() { }
  /// DMI version of close()
  void closeReader() { close(); }
  /**
    Set stream position in this Reader to given 
    position
    @return the absolute position in this 
            stream
  */
  virtual jlong seek(SeekPos seekrel, jlong seekpos) = 0;
  /**
    Move forwart into the stream.

    @return how many bytes skipped. May less then input, in
            case EOS is reached
  */
  virtual jlong skip(jlong n) = 0;
  /**
    read a single byte from stream
    @return the byte readed or -1 if EOS.
    @throw EOF exception, if trying to read behind EOS
  */
  virtual int read()
  {
    byte b;
    int erg = read(&b, 0, 1);
    if (erg == 0) 
      return -1;
    return b;
  }
  /**
    read the len bytes into buffer at offset of the buffer
    @param buffer where to write the bytes
    @param offset offset to write into buffer
    @param len how many bytes to read. if == -1 read into
           many bytes, that fits into buffer (buffer->length() - offset)
  ``@return number of bytes readed. may less than input if EOS is
            reached
    @throw EOF exception, if trying to read behind EOS
  */
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1)
  {
    if (len == -1)
      len = buffer->length() - offset;
    return read(buffer->data(), offset, len);
  }
  /**
    @see other read(RbyteArray, int, int)
   */
  virtual int read(byte* buffer, int offset, int len);
  /**
  */
  virtual void mark(int readAheadLimit)
  {
    THROW0(UnsupportedOperationException);
  }
  virtual bool markSupported() { return false; }
  /**
    if mark is set, set stream to mark point
    otherwise to begin of this stream.
  */
  virtual void reset() = 0;
  /**
    @return true, if at least 1 byte can be readed from stream
  */
  virtual bool ready() { return available() > 0; }
  /**
    reads until EOF into returned byteArray
  */
  RbyteArray readAll();
  /**
    writes reader until EOF into given Writer.
    API: Extended
  */
  void trans(IN(RWriter) out);

  virtual RStorage getReaderStorage();
   /**
    returns a character reader using standard encoding
    @param decoder use decoder to read from bytes. if Nil uses the system encoding
  */
  virtual RCharReader getCharReader(IN(acdk::locale::RDecoder) decoder = Nil);
  /**
    reads the content as String
    using standard encoding
  */
  RString readAllAsString();
 
};



} // io
} // acdk

#endif //acdk_io_Writer_h

