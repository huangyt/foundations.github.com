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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractStorageWriter.h,v 1.12 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractStorageWriter_h
#define acdk_io_AbstractStorageWriter_h

#include <acdk.h>
#include "AbstractWriter.h"
#include "Storage.h"

namespace acdk {
namespace io {

using namespace acdk::lang;


ACDK_DECL_CLASS(AbstractStorageWriter);

/** 
  Standard incomplete implementation for a StorageWriter.
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC AbstractStorageWriter
: extends AbstractWriter
, implements Storage
{
  ACDK_WITH_METAINFO(AbstractStorageWriter)
public :
  AbstractStorageWriter(IN(RObject) iolock = Nil)  
  : AbstractWriter(iolock)
  {
  }
  virtual ~AbstractStorageWriter() 
  {
  }
// storage
  foreign virtual RString getDeviceName() = 0;
  foreign virtual bool isWriteable() = 0;
  foreign virtual bool isReadable() = 0;
};


} // io
} // acdk

#endif //acdk_io_AbstractStorageWriter_h

