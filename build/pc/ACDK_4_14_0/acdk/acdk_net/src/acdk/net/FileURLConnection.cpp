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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/FileURLConnection.cpp,v 1.6 2005/02/05 10:45:29 kommer Exp $


#include "FileURLConnection.h"

namespace acdk {
namespace net {


void 
FileURLConnection::connect()
{
  RString host = url->getHost();
  
  if (host != Nil && host->length() >= 3 && (host->charAt(1) == '|' || host->charAt(0) == '/')) 
  {
    if (host->charAt(1) == '|')
      host = host->substr(0, 1) + ":" + host->substr(2);
    file = new File(host);
  } else {
    file = new File(url->getFile());
  }
  if (file != Nil)
    connected = true;
}



} // namespace acdk
} // namespace net




