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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCHandle.h,v 1.7 2005/02/05 10:45:32 kommer Exp $

#ifndef acdk_sqlodbc_Handle_h
#define acdk_sqlodbc_Handle_h

#include "odbc.h"

#include <acdk/sql/SQLException.h>
#include <acdk/sql/SQLWarning.h>
#include <acdk/util/logging/Log.h>


#define SQLTHROW(chain) \
if (chain != Nil) { \
  THROW_INSTANCE(chain); \
}

namespace acdk {
namespace sql {
namespace odbc {

ACDK_DECL_CLASS(ODBCHandle);
ACDK_DECL_CLASS(ODBCStatusRecord);

class ACDK_SQL_ODBC_PUBLIC ODBCStatusRecord
: extends Object
{
  ACDK_WITH_METAINFO(ODBCStatusRecord)
private:
    // valid for any handle
  RString _sqlState;
  RString _classOrigin;
  RString _subClassOrigin;
  RString _messageText;
  int _nativeErr;
  bool _nativeErrValid;
    // valid only for statement-handles
  int _columnNumber;
  bool _columnNumberValid;
  int _rowNumber;
  bool _rowNumberValid;
    // valid for any handle with a valid connection
  RString _connectionName;
    // valid for any message from server
  //RString _serverName; // can be better accessed via SQLGetInfo().
public:
  RString getSQLState() { return _sqlState; }
  RString getClassOrigin() { return _classOrigin; }
  RString getSubClassOrigin() { return _subClassOrigin; }
  RString getMessageText() { return _messageText; }
  bool getColumnNumber(int& val) { if (_columnNumberValid == true) val = _columnNumberValid; return _columnNumberValid; }
  bool getRowNumber(int& val) { if (_rowNumberValid == true) val = _rowNumber; return _rowNumberValid; }
  RString getConnectionName() { return _connectionName; }
  ODBCStatusRecord() : _nativeErrValid(false), _columnNumberValid(false), _rowNumberValid(false) {}
  ~ODBCStatusRecord() {}
  friend class ODBCHandle;
};

class ACDK_SQL_ODBC_PUBLIC ODBCHandle
: extends Object
{
  ACDK_WITH_METAINFO(ODBCHandle)
private:
  SQLHANDLE _handle;  // this is NOT allocated within the constructor, but at first try of usage or via init().
  SQLSMALLINT _htype;
  bool _initialized;  // if handle is valid, because it is implementation-depended either a void* or a long
  ::acdk::sql::RSQLException _excpt;
  ::acdk::sql::RSQLWarning _warng;
    // valid for any handle
  int _returnCode;
  bool _returnCodeValid;
  int _numberRecords;
  bool _numberRecordsValid;
    // valid only for statement-handles
  int _cursorRowCount; // count of rows at current cursor-position
  bool _cursorRowCountValid;
  int _rowCount; // count of rows affected by last call
  bool _rowCountValid;
    // next two are redundant.
  RString _dynamicFunction;
  int _dynamicFunctionCode;
  bool _dynamicFunctionCodeValid;
  RODBCStatusRecordArray _statusRecords;

public:
  ODBCHandle(SQLSMALLINT htype);
  ~ODBCHandle();

  RODBCHandle init(INP(RODBCHandle) prnt);
  void deinit();

  SQLHANDLE _getSQLHandle() { return _handle; }
  SQLSMALLINT _getSQLHType() { return _htype; }
  void _setSQLFlag(SQLUINTEGER key, SQLUINTEGER val);

  ::acdk::sql::RSQLException _getExceptions() { return _excpt; }
  ::acdk::sql::RSQLWarning _getWarnings() { return _warng; }
  void _clearExceptions() { _excpt = Nil; }
  void _clearWarnings() { _warng = Nil; }
  void _addException(INP(::acdk::sql::RSQLException) excpt) { if (_excpt != Nil) excpt->setNextException(_excpt); _excpt = excpt; }
  void _addException(INP(RString) msg) { _addException(new SQLException(msg)); }
  void _addException(char *msg) { _addException(new SQLException(msg)); }
  void _addWarning(INP(::acdk::sql::RSQLWarning) warng) { if (_warng != Nil) warng->setNextException(_warng); _warng = warng; }
  void _addWarning(INP(RString) msg) { _addWarning(new SQLWarning(msg)); }
  void _addWarning(char *msg) { _addWarning(new SQLWarning(msg)); }

  int _chkSQLrcode(SQLRETURN err, char *fnam, char *file, int line);

  bool getReturnCode(int& val) { if (_returnCodeValid == true) val = _returnCodeValid; return _returnCodeValid; }
  bool getCursorRowCount(int& val) { if (_cursorRowCountValid == true) val = _cursorRowCount; return _cursorRowCountValid; }
  RString getDynamicFunction() { return _dynamicFunction; }
  bool getDynamicFunctionCode(int& val) { if (_dynamicFunctionCodeValid == true) val = _dynamicFunctionCode; return _dynamicFunctionCodeValid; }
  bool getRowCount(int& val) { if (_rowCountValid == true) val = _rowCount; return _rowCountValid; }
  bool getNumberRecords(int& val) { if (_numberRecordsValid == true) val = _numberRecords; return _numberRecordsValid; }

  RODBCStatusRecordArray getStatusRecords() { return _statusRecords; }
  //static int odbcTypeToSqlType(int typ);
  //static int sqlTypeToOdbcType(int typ);

private:
  void _getSQLdiag(SQLRETURN err, char *file, int line);
  void _getallSQLdiags(const char* errstr, char *file, int line);
  bool _getsingleSQLdiag(SQLSMALLINT diagRec, SQLSMALLINT diagID, SQLPOINTER diagPtr, SQLSMALLINT diagSize, SQLSMALLINT& diagLen, const char* errstr);
};

/* some helper macros for calling ODBC-functions */

#define callSQL0(__handle, __func) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle()), #__func, __FILE__, __LINE__)
#define callSQL1(__handle, __func, __arg1) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1), #__func, __FILE__, __LINE__)
#define callSQL2(__handle, __func, __arg1, __arg2) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2), #__func, __FILE__, __LINE__)
#define callSQL3(__handle, __func, __arg1, __arg2, __arg3) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3), #__func, __FILE__, __LINE__)
#define callSQL4(__handle, __func, __arg1, __arg2, __arg3, __arg4) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3, __arg4), #__func, __FILE__, __LINE__)
#define callSQL5(__handle, __func, __arg1, __arg2, __arg3, __arg4, __arg5) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3, __arg4, __arg5), #__func, __FILE__, __LINE__)
#define callSQL6(__handle, __func, __arg1, __arg2, __arg3, __arg4, __arg5, __arg6) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3, __arg4, __arg5, __arg6), #__func, __FILE__, __LINE__)
#define callSQL7(__handle, __func, __arg1, __arg2, __arg3, __arg4, __arg5, __arg6, __arg7) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3, __arg4, __arg5, __arg6, __arg7), #__func, __FILE__, __LINE__)
#define callSQL8(__handle, __func, __arg1, __arg2, __arg3, __arg4, __arg5, __arg6, __arg7, __arg8) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3, __arg4, __arg5, __arg6, __arg7, __arg8), #__func, __FILE__, __LINE__)

#define callSQL9(__handle, __func, __arg1, __arg2, __arg3, __arg4, __arg5, __arg6, __arg7, __arg8, __arg9) \
  __handle->_chkSQLrcode(::  __func(__handle->_getSQLHandle(), __arg1, __arg2, __arg3, __arg4, __arg5, __arg6, __arg7, __arg8, __arg9), #__func, __FILE__, __LINE__)

} // odbc
} // sql
} // acdk

#endif // acdk_sqlodbc_Handle_h
