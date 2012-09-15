// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License (LGPL) or the Q public License (QPL).
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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdkx/net/ssl/Config.h,v 1.1 2005/03/29 12:13:52 kommer Exp $

#ifndef acdkx_net_ssl_Config_h
#define acdkx_net_ssl_Config_h

#if defined(_MSC_VER) || defined(__BORLANDC__)
# ifdef IN_ACDKX_NET_SSL_LIB
#   define ACDKX_NET_SSL_PUBLIC __declspec(dllexport)
# elif defined(USE_ACDKX_NET_SSL_LIB)
#   define ACDKX_NET_SSL_PUBLIC __declspec(dllimport)
# else
#   define ACDKX_NET_SSL_PUBLIC
# endif
#else
# define ACDKX_NET_SSL_PUBLIC
#endif

#endif //acdkx_net_ssl_Config_h

