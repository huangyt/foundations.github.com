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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FilterWriter.h,v 1.14 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FilterWriter_h
#define acdk_io_FilterWriter_h

#include <acdk.h>
#include "Writer.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

class FilterWriter;
ACDK_DECL_INTERFACE(FilterWriter);


/**
  Interface for a writer filter 
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.14 $
  @date $Date: 2005/04/09 19:26:45 $

*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC FilterWriter
: implements Writer
{
  ACDK_WITH_METAINFO(FilterWriter)
public :

  // Writer:
  virtual void flush() = 0;
  virtual void write(byte c) = 0;
  virtual void write(IN(RbyteArray) ch, int offset, int len) = 0;
  foreign void write(const byte* cstr, int offset, int len) { Writer::write(cstr, offset, len); }
  virtual void setOut(IN(RWriter) writer) = 0;
  virtual RStorage getStorage() = 0;
  /**
    returns the unfiltered Writer
    */
  virtual RWriter getStorageWriter() = 0;
};

} // io
} // acdk

#endif //acdk_io_FilterWriter_h

