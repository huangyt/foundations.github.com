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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/net.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_net_h
#define acdk_net_net_h
#include <acdk.h>

#if defined(ACDK_OS_WIN32)
#include <acdk/lang/sys/disable_vc_warnings.h>
#endif

#include "Config.h"

// defines the unit acdk::net
ACDK_DECL_UNIT(acdk_net)

namespace acdk {
/**
  Equally to the Java package java.net.
*/ 
namespace net {

ACDK_DECL_CLASS(Socket);
ACDK_DECL_CLASS(SocketImpl);
ACDK_DECL_CLASS(InetAddress);
ACDK_DECL_CLASS(PasswordAuthentication);

} //namespace net
} // namespace acdk 
/*
#include "InetAddress.h"
#include "Socket.h"
#include "SocketImpl.h"
#include "SocketException.h"
#include "SocketImplFactory.h"
#include "TCPSocket.h"
#include "ServerSocket.h"

#include "URL.h"
#include "URLConnection.h"
#include "URLStreamHandler.h"
#include "URLStreamHandlerFactory.h"
#include "URLDecoder.h"
#include "URLEncoder.h"
#include "UnknownServiceException.h"
#include "ProtocolException.h"
#include "PasswordAuthentication.h"
#include "MimeTypeMapper.h"
#include "MalformedURLException.h"
#include "HttpURLConnection.h"
#include "FileNameMap.h"
#include "ContentHandler.h"
#include "ContentHandlerFactory.h"
#include "Authenticator.h"
*/
#endif //acdk_net_net_h

