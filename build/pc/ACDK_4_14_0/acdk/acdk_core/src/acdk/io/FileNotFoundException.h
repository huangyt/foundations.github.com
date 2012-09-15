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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/FileNotFoundException.h,v 1.11 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_FileNotFoundException_h
#define acdk_io_FileNotFoundException_h

#include "IOException.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_THROWABLE(FileNotFoundException, IOException);

/** 
  Will be thrown if a method cannot find a given name.
  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:44 $
  
*/
class ACDK_CORE_PUBLIC FileNotFoundException 
: extends IOException
{
  ACDK_WITH_METAINFO(FileNotFoundException)  
public:
  FileNotFoundException() : IOException() { }
  FileNotFoundException(IN(RString) what) : IOException(what) { }
};

} //IO
} // acdk

#endif //acdk_io_FileNotFoundException_h

