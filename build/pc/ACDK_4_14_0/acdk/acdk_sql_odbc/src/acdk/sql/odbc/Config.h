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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/Config.h,v 1.3 2005/02/05 10:45:31 kommer Exp $

#ifndef acdk_sql_odbc_Config_h
#define acdk_sql_odbc_Config_h

#if defined(_MSC_VER) || defined(__BORLANDC__)
# ifdef IN_ACDK_SQL_ODBC_LIB
#   define ACDK_SQL_ODBC_PUBLIC __declspec(dllexport)
# elif defined(USE_ACDK_SQL_ODBC_LIB)
#   define ACDK_SQL_ODBC_PUBLIC __declspec(dllimport)
# else
#   define ACDK_SQL_ODBC_PUBLIC
# endif
#else
# define ACDK_SQL_ODBC_PUBLIC
#endif

#endif //acdk_sql_odbc_Config_h
