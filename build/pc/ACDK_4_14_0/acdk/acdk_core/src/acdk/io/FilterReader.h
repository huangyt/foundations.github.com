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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FilterReader.h,v 1.11 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_FilterReader_h
#define acdk_io_FilterReader_h

#include "Storage.h"
#include "Reader.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

class FilterReader;
ACDK_DECL_INTERFACE(FilterReader);


/** 
  Interface for a reader filter.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC FilterReader
: implements Reader
{
  ACDK_WITH_METAINFO(FilterReader)
public :
  // Reader
  overwrite  jlong seek(SeekPos seekrel, jlong seekpos) = 0;
  overwrite jlong skip(jlong n) = 0;
  overwrite void reset() = 0;

  virtual void setIn(IN(RReader) reader) = 0;
  virtual RStorage getStorage() = 0;
  virtual RReader getStorageReader() = 0;
};

} // io
} // acdk

#endif //acdk_io_FilterReader_h

