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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/NullWriter.h,v 1.11 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_NullWriter_h
#define acdk_io_NullWriter_h

#include "AbstractStorageWriter.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(NullWriter);

/**
  This class is the implementation of Writer to  /dev/null
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC NullWriter
: extends AbstractStorageWriter
{
  ACDK_WITH_METAINFO(NullWriter)
public :
  /** Does nothing */
  foreign virtual void write(byte c);

  /** Does nothing */
  foreign virtual void write(const byte* cstr, int offset, int len);
  /** Does nothing */
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  /** Does nothing */
  foreign virtual void flush();

  /** Does nothing */
  foreign virtual void close();
// Storage
  foreign virtual RString getDeviceName()
  {
    return "[NullWriter]";
  }
  foreign virtual bool isWriteable() { return true; }
  foreign virtual bool isReadable() { return false; }
};

//virtual 
inline
void 
NullWriter::write(byte c) 
{ 

}

//virtual 
inline
void 
NullWriter::write(const byte* cstr, int offset, int len)
{

}

//virtual 
inline
void 
NullWriter::write(IN(RbyteArray) ch, int offset, int len)
{

}
  
//virtual 
inline
void 
NullWriter::flush() 
{
}


//virtual 
inline
void 
NullWriter::close()
{
  
}


} // io
} // acdk

#endif //acdk_io_NullWriter_h

