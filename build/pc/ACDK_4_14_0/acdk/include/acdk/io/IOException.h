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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/IOException.h,v 1.11 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_IOException_h
#define acdk_io_IOException_h


#ifndef acdk_lang_Exception_h
#include <acdk/lang/Exception.h>
#endif // acdk_lang_Exception_h

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_THROWABLE(IOException, Exception);


/**
  Signals error in IO operations.
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC IOException 
: extends acdk::lang::Exception
{
  ACDK_WITH_METAINFO(IOException)  
public:
  IOException(bool withErrno = true);
  IOException(IN(RString) what, bool withErrno = false);
};


} // io
} // acdk

#endif //acdk_io_IOException_h
