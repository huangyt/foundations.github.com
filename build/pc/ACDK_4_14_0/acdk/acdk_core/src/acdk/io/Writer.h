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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/Writer.h,v 1.24 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_Writer_h
#define acdk_io_Writer_h


namespace acdk {
namespace io {


ACDK_DECL_INTERFACE(Storage);

ACDK_DECL_INTERFACE(Writer);
ACDK_DECL_INTERFACE(CharWriter);

/**
  Different to JDK a Writer (an Reader too) are not classes, 
  but only Interfaces. The role of java.io.Writer takes AbstractWriter
  It differs to the java IO-Modell, which is IMHO too confusing.<br>
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.24 $
  @date $Date: 2005/04/09 19:26:45 $
  
  @see Reader
*/
class ACDK_CORE_PUBLIC Writer
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Writer)
public :
  virtual void flush() = 0;
  virtual void close() = 0;

  // DMI version
  //void closeWriter() { close(); }

  virtual void write(const byte* cstr, int offset, int len);
  
  /** 
    default implementation:
    byte cbuf[2]; cbuf[1] = 0; cbuf[0] = c;
    return write((const byte*)cbuf, 0, 1);
  */
  virtual void write(byte c) = 0;
  /** 
    Uses write(byte c) to write bytes. For performance reason this method should be implemented
  */
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  
  virtual RStorage getWriterStorage();
  /**
    **
    Get writer from this CharWriter
    @param encoder uses the encoding. If Nil uses the System encoder
  */
  virtual RCharWriter getCharWriter(IN(acdk::locale::REncoder) encoder = Nil);
};



} // io
} // acdk

#endif //acdk_io_Writer_h

