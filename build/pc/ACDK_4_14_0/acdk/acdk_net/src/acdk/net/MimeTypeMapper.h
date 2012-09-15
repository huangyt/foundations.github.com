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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/MimeTypeMapper.h,v 1.10 2005/04/30 14:06:54 kommer Exp $

#ifndef acdk_net_MimeTypeMapper_h
#define acdk_net_MimeTypeMapper_h

#include "net.h"
#include <acdk/util/HashMap.h>
#include "FileNameMap.h"

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::util;

ACDK_DECL_CLASS(MimeTypeMapper);

class ACDK_NET_PUBLIC MimeTypeMapper
: extends acdk::lang::Object,
  implements FileNameMap
  
{
  ACDK_WITH_METAINFO(MimeTypeMapper)
protected:

/**
  * This array of strings is used to identify a MIME type based on a file 
  * extension.  This is list is based on the Apache mime.types file.
  */
  foreign static char* mime_strings[][2];
private:
/**
  * The MIME types above are put into this Hashtable for faster lookup.
  */
  static RHashMap __mime_types;
  static RHashMap mime_types();
public:

  MimeTypeMapper()
    : Object()
  {
  }

/**
  * The method returns the MIME type of the filename passed as an argument.
  * The value returned is based on the extension of the filename.  The 
  * default content type returned if this method cannot determine the
  * actual content type is "
  *
  * @param filename The name of the file to return the MIME type for
  *
  * @return The MIME type
  */

  virtual RString getContentTypeFor(IN(RString) filename);
};

} // namespace acdk
} // namespace net

#endif //acdk_net_MimeTypeMapper_h



