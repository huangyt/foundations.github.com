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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/odbc.h,v 1.17 2005/02/09 13:27:44 kommer Exp $

#ifndef acdk_sqlodbc_sqlodbc_h
#define acdk_sqlodbc_sqlodbc_h

#include "Config.h"

#include <acdk/sql/sql.h>

// defines the unit 
ACDK_DECL_UNIT(acdk_sql_odbc)

#if !defined(OS_WIN32)
# if defined(UNICODE)
#   undef UNICODE
#   define UNICODE_WASDEFINED
# endif
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
/*
  #else
  #include <isql.h>
  #include <isqlext.h>
  //#include <isqltypes.h>
  #endif
*/

#if defined(UNICODE_WASDEFINED)
# define UNICODE 1
#endif

#if !defined(SQL_NO_DATA)
# if defined(SQL_NO_DATA_FOUND)
#  define SQL_NO_DATA              SQL_NO_DATA_FOUND
# else
#  error no definition for SQL_NO_DATA
# endif 
#endif // !defined(SQL_NO_DATA)

#if !defined(SQL_COLUMN_IGNORE)
# if defined(SQL_IGNORE)
#  define SQL_COLUMN_IGNORE              SQL_IGNORE
# else
#  error no definition for SQL_COLUMN_IGNORE
# endif 
#endif // !defined(SQL_COLUMN_IGNORE)

#if !defined(SQL_DESC_SCHEMA_NAME)
# if defined(SQL_COLUMN_OWNER_NAME)
#  define SQL_DESC_SCHEMA_NAME           SQL_COLUMN_OWNER_NAME
# else
#  error no definition for SQL_DESC_SCHEMA_NAME
# endif 
#endif // !defined(SQL_DESC_SCHEMA_NAME)

#if !defined(SQL_DESC_SEARCHABLE)
# if defined(SQL_COLUMN_SEARCHABLE)
#  define SQL_DESC_SEARCHABLE            SQL_COLUMN_SEARCHABLE
# else
#  error no definition for SQL_DESC_SEARCHABLE
# endif 
#endif // !defined(SQL_DESC_SEARCHABLE)

#if !defined(SQL_DESC_TYPE_NAME)
# if defined(SQL_COLUMN_TYPE_NAME)
#  define SQL_DESC_TYPE_NAME             SQL_COLUMN_TYPE_NAME
# else
#  error no definition for SQL_DESC_TYPE_NAME
# endif 
#endif // !defined(SQL_DESC_TYPE_NAME)

#if !defined(SQL_DESC_TABLE_NAME)
# if defined(SQL_COLUMN_TABLE_NAME)
#  define SQL_DESC_TABLE_NAME            SQL_COLUMN_TABLE_NAME
# else
#  error no definition for SQL_DESC_TABLE_NAME
# endif 
#endif // !defined(SQL_DESC_TABLE_NAME)

#if !defined(SQL_DESC_UNSIGNED)
# if defined(SQL_COLUMN_UNSIGNED)
#  define SQL_DESC_UNSIGNED              SQL_COLUMN_UNSIGNED
# else
#  error no definition for SQL_DESC_UNSIGNED
# endif 
#endif // !defined(SQL_DESC_UNSIGNED)

#if !defined(SQL_DESC_UPDATABLE)
# if defined(SQL_COLUMN_UPDATABLE)
#  define SQL_DESC_UPDATABLE             SQL_COLUMN_UPDATABLE
# else
#  error no definition for SQL_DESC_UPDATABLE
# endif 
#endif // !defined(SQL_DESC_UPDATABLE)

#if !defined(SQL_DESC_DISPLAY_SIZE)
# if defined(SQL_COLUMN_DISPLAY_SIZE)
#  define SQL_DESC_DISPLAY_SIZE          SQL_COLUMN_DISPLAY_SIZE
# else
#  error no definition for SQL_DESC_DISPLAY_SIZE
# endif 
#endif // !defined(SQL_DESC_DISPLAY_SIZE)

#if !defined(SQL_DESC_FIXED_PREC_SCALE)
# if defined(SQL_COLUMN_MONEY)
#  define SQL_DESC_FIXED_PREC_SCALE      SQL_COLUMN_MONEY
# else
#  error no definition for SQL_DESC_FIXED_PREC_SCALE
# endif 
#endif // !defined(SQL_DESC_FIXED_PREC_SCALE)

#if !defined(SQL_DESC_LABEL)
# if defined(SQL_COLUMN_LABEL)
#  define SQL_DESC_LABEL                 SQL_COLUMN_LABEL
# else
#  error no definition for SQL_DESC_LABEL
# endif 
#endif // !defined(SQL_DESC_LABEL)

#if !defined(SQL_DESC_CASE_SENSITIVE)
# if defined(SQL_COLUMN_CASE_SENSITIVE)
#  define SQL_DESC_CASE_SENSITIVE        SQL_COLUMN_CASE_SENSITIVE
# else
#  error no definition for SQL_DESC_CASE_SENSITIVE
# endif 
#endif // !defined(SQL_DESC_CASE_SENSITIVE)

#if !defined(SQL_DESC_CATALOG_NAME)
# if defined(SQL_COLUMN_QUALIFIER_NAME)
#  define SQL_DESC_CATALOG_NAME          SQL_COLUMN_QUALIFIER_NAME
# else
#  error no definition for SQL_DESC_CATALOG_NAME
# endif 
#endif // !defined(SQL_DESC_CATALOG_NAME)

#if !defined(SQL_DESC_CONCISE_TYPE)
# if defined(SQL_COLUMN_TYPE)
#  define SQL_DESC_CONCISE_TYPE          SQL_COLUMN_TYPE
# else
#  error no definition for SQL_DESC_CONCISE_TYPE
# endif 
#endif // !defined(SQL_DESC_CONCISE_TYPE)

#if !defined(SQL_DESC_AUTO_UNIQUE_VALUE)
# if defined(SQL_COLUMN_AUTO_INCREMENT)
#  define SQL_DESC_AUTO_UNIQUE_VALUE     SQL_COLUMN_AUTO_INCREMENT
# else
#  error no definition for SQL_DESC_AUTO_UNIQUE_VALUE
# endif 
#endif // !defined(SQL_DESC_AUTO_UNIQUE_VALUE)

#if !defined(SQL_ATTR_CONCURRENCY)
# if defined(SQL_CONCURRENCY)
#  define SQL_ATTR_CONCURRENCY           SQL_CONCURRENCY
# else
#  error no definition for SQL_ATTR_CONCURRENCY
# endif 
#endif // !defined(SQL_ATTR_CONCURRENCY)

#if !defined(SQL_ATTR_CURSOR_TYPE)
# if defined(SQL_CURSOR_TYPE)
#  define SQL_ATTR_CURSOR_TYPE           SQL_CURSOR_TYPE
# else
#  error no definition for SQL_ATTR_CURSOR_TYPE
# endif 
#endif // !defined(SQL_ATTR_CURSOR_TYPE)

#if !defined(SQL_ATTR_KEYSET_SIZE)
# if defined(SQL_KEYSET_SIZE)
#  define SQL_ATTR_KEYSET_SIZE           SQL_KEYSET_SIZE
# else
#  error no definition for SQL_ATTR_KEYSET_SIZE
# endif 
#endif // !defined(SQL_ATTR_KEYSET_SIZE)

#if !defined(SQL_ATTR_MAX_LENGTH)
# if defined(SQL_MAX_LENGTH)
#  define SQL_ATTR_MAX_LENGTH            SQL_MAX_LENGTH
# else
#  error no definition for SQL_ATTR_MAX_LENGTH
# endif 
#endif // !defined(SQL_ATTR_MAX_LENGTH)

#if !defined(SQL_ATTR_MAX_ROWS)
# if defined(SQL_MAX_ROWS)
#  define SQL_ATTR_MAX_ROWS              SQL_MAX_ROWS
# else
#  error no definition for SQL_ATTR_MAX_ROWS
# endif 
#endif // !defined(SQL_ATTR_MAX_ROWS)

#if !defined(SQL_ATTR_NOSCAN)
# if defined(SQL_NOSCAN)
#  define SQL_ATTR_NOSCAN                SQL_NOSCAN
# else
#  error no definition for SQL_ATTR_NOSCAN
# endif 
#endif // !defined(SQL_ATTR_NOSCAN)

#if !defined(SQL_ATTR_QUERY_TIMEOUT)
# if defined(SQL_QUERY_TIMEOUT)
#  define SQL_ATTR_QUERY_TIMEOUT         SQL_QUERY_TIMEOUT
# else
#  error no definition for SQL_ATTR_QUERY_TIMEOUT
# endif 
#endif // !defined(SQL_ATTR_QUERY_TIMEOUT)

#if !defined(SQL_ATTR_RETRIEVE_DATA)
# if defined(SQL_RETRIEVE_DATA)
#  define SQL_ATTR_RETRIEVE_DATA         SQL_RETRIEVE_DATA
# else
#  error no definition for SQL_ATTR_RETRIEVE_DATA
# endif 
#endif // !defined(SQL_ATTR_RETRIEVE_DATA)

#if !defined(SQL_ATTR_ROW_BIND_TYPE)
# if defined(SQL_BIND_TYPE)
#  define SQL_ATTR_ROW_BIND_TYPE         SQL_BIND_TYPE
# else
#  error no definition for SQL_ATTR_ROW_BIND_TYPE
# endif 
#endif // !defined(SQL_ATTR_ROW_BIND_TYPE)

#if !defined(SQL_ATTR_ROW_NUMBER)
# if defined(SQL_ROW_NUMBER)
#  define SQL_ATTR_ROW_NUMBER            SQL_ROW_NUMBER
# else
#  error no definition for SQL_ATTR_ROW_NUMBER
# endif 
#endif // !defined(SQL_ATTR_ROW_NUMBER)

#if !defined(SQL_ATTR_SIMULATE_CURSOR)
# if defined(SQL_SIMULATE_CURSOR)
#  define SQL_ATTR_SIMULATE_CURSOR       SQL_SIMULATE_CURSOR
# else
#  error no definition for SQL_ATTR_SIMULATE_CURSOR
# endif 
#endif // !defined(SQL_ATTR_SIMULATE_CURSOR)

#if !defined(SQL_ATTR_USE_BOOKMARKS)
# if defined(SQL_USE_BOOKMARKS)
#  define SQL_ATTR_USE_BOOKMARKS         SQL_USE_BOOKMARKS
# else
#  error no definition for SQL_ATTR_USE_BOOKMARKS
# endif 
#endif // !defined(SQL_ATTR_USE_BOOKMARKS)

#if !defined(SQL_ATTR_ACCESS_MODE)
# if defined(SQL_ACCESS_MODE)
#  define SQL_ATTR_ACCESS_MODE           SQL_ACCESS_MODE
# else
#  error no definition for SQL_ATTR_ACCESS_MODE
# endif 
#endif // !defined(SQL_ATTR_ACCESS_MODE)

#if !defined(SQL_ATTR_AUTOCOMMIT)
# if defined(SQL_AUTOCOMMIT)
#  define SQL_ATTR_AUTOCOMMIT            SQL_AUTOCOMMIT
# else
#  error no definition for SQL_ATTR_AUTOCOMMIT
# endif 
#endif // !defined(SQL_ATTR_AUTOCOMMIT)

#if !defined(SQL_ATTR_CURRENT_CATALOG)
# if defined(SQL_CURRENT_QUALIFIER)
#  define SQL_ATTR_CURRENT_CATALOG       SQL_CURRENT_QUALIFIER
# else
#  error no definition for SQL_ATTR_CURRENT_CATALOG
# endif 
#endif // !defined(SQL_ATTR_CURRENT_CATALOG)

#if !defined(SQL_ATTR_LOGIN_TIMEOUT)
# if defined(SQL_LOGIN_TIMEOUT)
#  define SQL_ATTR_LOGIN_TIMEOUT         SQL_LOGIN_TIMEOUT
# else
#  error no definition for SQL_ATTR_LOGIN_TIMEOUT
# endif 
#endif // !defined(SQL_ATTR_LOGIN_TIMEOUT)

#if !defined(SQL_ATTR_ODBC_CURSORS)
# if defined(SQL_ODBC_CURSORS)
#  define SQL_ATTR_ODBC_CURSORS          SQL_ODBC_CURSORS
# else
#  error no definition for SQL_ATTR_ODBC_CURSORS
# endif 
#endif // !defined(SQL_ATTR_ODBC_CURSORS)

#if !defined(SQL_ATTR_PACKET_SIZE)
# if defined(SQL_PACKET_SIZE)
#  define SQL_ATTR_PACKET_SIZE           SQL_PACKET_SIZE
# else
#  error no definition for SQL_ATTR_PACKET_SIZE
# endif 
#endif // !defined(SQL_ATTR_PACKET_SIZE)

#if !defined(SQL_ATTR_QUIET_MODE)
# if defined(SQL_QUIET_MODE)
#  define SQL_ATTR_QUIET_MODE            SQL_QUIET_MODE
# else
#  error no definition for SQL_ATTR_QUIET_MODE
# endif 
#endif // !defined(SQL_ATTR_QUIET_MODE)

#if !defined(SQL_ATTR_TRACE)
# if defined(SQL_OPT_TRACE)
#  define SQL_ATTR_TRACE                 SQL_OPT_TRACE
# else
#  error no definition for SQL_ATTR_TRACE
# endif 
#endif // !defined(SQL_ATTR_TRACE)

#if !defined(SQL_ATTR_TRACEFILE)
# if defined(SQL_OPT_TRACEFILE)
#  define SQL_ATTR_TRACEFILE             SQL_OPT_TRACEFILE
# else
#  error no definition for SQL_ATTR_TRACEFILE
# endif 
#endif // !defined(SQL_ATTR_TRACEFILE)

#if !defined(SQL_ATTR_TRANSLATE_LIB)
# if defined(SQL_TRANSLATE_DLL)
#  define SQL_ATTR_TRANSLATE_LIB         SQL_TRANSLATE_DLL
# else
#  error no definition for SQL_ATTR_TRANSLATE_LIB
# endif 
#endif // !defined(SQL_ATTR_TRANSLATE_LIB)

#if !defined(SQL_ATTR_TRANSLATE_OPTION)
# if defined(SQL_TRANSLATE_OPTION)
#  define SQL_ATTR_TRANSLATE_OPTION      SQL_TRANSLATE_OPTION
# else
#  error no definition for SQL_ATTR_TRANSLATE_OPTION
# endif 
#endif // !defined(SQL_ATTR_TRANSLATE_OPTION)

#if !defined(SQL_ATTR_TXN_ISOLATION)
# if defined(SQL_TXN_ISOLATION)
#  define SQL_ATTR_TXN_ISOLATION         SQL_TXN_ISOLATION
# else
#  error no definition for SQL_ATTR_TXN_ISOLATION
# endif 
#endif // !defined(SQL_ATTR_TXN_ISOLATION)

#if !defined(SQL_TYPE_DATE)
# if defined(SQL_DATE)
#  define SQL_TYPE_DATE                  SQL_DATE
# else
#  error no definition for SQL_TYPE_DATE
# endif 
#endif // !defined(SQL_TYPE_DATE)

#if !defined(SQL_TYPE_TIME)
# if defined(SQL_TIME)
#  define SQL_TYPE_TIME                  SQL_TIME
# else
#  error no definition for SQL_TYPE_TIME
# endif 
#endif // !defined(SQL_TYPE_TIME)

#if !defined(SQL_TYPE_TIMESTAMP)
# if defined(SQL_TIMESTAMP)
#  define SQL_TYPE_TIMESTAMP             SQL_TIMESTAMP
# else
#  error no definition for SQL_TYPE_TIMESTAMP
# endif 
#endif // !defined(SQL_TYPE_TIMESTAMP)

#if !defined(SQL_NULL_HANDLE)
# define SQL_NULL_HANDLE 0
#endif // !defined(SQL_NULL_HANDLE)

namespace acdk {
namespace sql {
  /**
    Implements the ODBC driver for the ACDK Database Connector.
  */
namespace odbc {

using namespace acdk::lang;

ACDK_DECL_CLASS(ODBCArray);
ACDK_DECL_CLASS(ODBCColumn);
ACDK_DECL_CLASS(ODBCConnection);
ACDK_DECL_CLASS(ODBCDatabaseMetaData);
ACDK_DECL_CLASS(ODBCDriver);
ACDK_DECL_CLASS(ODBCHandle);
ACDK_DECL_CLASS(ODBCStatusRecord);
ACDK_DECL_CLASS(ODBCResultSet);
ACDK_DECL_CLASS(ODBCResultSetMetaData);
ACDK_DECL_CLASS(ODBCStatement);
ACDK_DECL_CLASS(ODBCPreparedStatement);



template <class CT>
int realstrlen(const CT* ptr, int maxlen = -1)
{
  for (int i = 0; maxlen == -1 || i < maxlen; ++i)
    if (ptr[i] == 0)
      return i;
  return maxlen;
}

// currently unicode doesn't work on unices
#if (ODBCVER >= 0x0300) && defined(ACDK_OS_WIN32)
# define ACDK_HAS_SQL_WCHAR
#endif

#if defined(ACDK_HAS_SQL_WCHAR)

# if defined(ACDK_OS_WIN32)
  typedef wchar_t ODBC_NATIVE_CHAR;
# else // unix
  typedef SQLWCHAR ODBC_NATIVE_CHAR;
# endif
# define ODBC_StringCharacterClass ::acdk::lang::CCUcs2

# define ODBCSTR2STR(buffer, len) (new String(reinterpret_cast<wchar_t*>(buffer), \
                                             realstrlen(reinterpret_cast<wchar_t*>(buffer), len), \
                                             ODBC_StringCharacterClass | ::acdk::lang::NormalSST))
# define ODBC_STR2NSTR(str) str->convert(ODBC_StringCharacterClass)
# define ODBC_STR2NCSRT(str) ((ODBC_NATIVE_CHAR*)str->native_c_str())


#else // defined(ACDK_HAS_SQL_WCHAR)

#define ODBC_StringCharacterClass ::acdk::lang::CCAscii

# define ODBCSTR2STR(buffer, len) (new ::acdk::lang::String(reinterpret_cast<char*>(buffer), \
                                                             realstrlen(reinterpret_cast<char*>(buffer), len), \
                                                             ::acdk::lang::NormalSST | ODBC_StringCharacterClass))
# if defined(ACDK_OS_WIN32)
   typedef char ODBC_NATIVE_CHAR;
# else
   typedef unsigned char ODBC_NATIVE_CHAR;
# endif // defined(ACDK_OS_WIN32)
# define ODBC_STR2NSTR(str) str->convert(ODBC_StringCharacterClass)
# define ODBC_STR2NCSRT(str) ((ODBC_NATIVE_CHAR*)str->c_str())

#endif //defined(ACDK_HAS_SQL_WCHAR)


} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_sqlodbc_h
