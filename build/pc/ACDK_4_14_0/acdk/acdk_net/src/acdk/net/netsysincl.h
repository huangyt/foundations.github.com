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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/netsysincl.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_netsysincl_h
#define acdk_net_net_h

#include <acdk.h>

#if defined(ACDK_OS_WIN32)
#include <acdk/lang/sys/disable_vc_warnings.h>
#endif

#include <acdk_all.h>

#include <acdk/net/Config.h>


#if defined(ACDK_OS_WIN32)
#if defined(_MSC_VER) ||  defined(__BORLANDC__)
# if (_MSC_VER >= 1300) || (__BORLANDC__ < 0x600)
#   include <winsock.h>
# else //_MSC_VER >= 1300
#   include <winsock2.h>
# endif //_MSC_VER >= 1300
#else // defined(_MSC_VER) ||  defined(__BORLANDC__)
# include <winsock.h>
#endif
typedef int socklen_t;
#endif
#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_SOLARIS) || defined(ACDK_OS_BSD)
#include <netdb.h>
#endif
#if defined(ACDK_OS_UNIX)
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
# include <sys/time.h>
#if !defined(_SOCKLEN_T)
#if defined(ACDK_OS_SOLARIS)
typedef int socklen_t;
#else
typedef unsigned socklen_t;
#endif
#endif // _SOCKLEN_T
#endif
#endif //acdk_net_netsysincl_h

