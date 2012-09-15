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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCHandle.cpp,v 1.13 2005/03/08 18:55:37 kommer Exp $

#include "ODBCHandle.h"
#include <acdk/lang/Integer.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>

int forceODBCLibToLink()
{
  return 0;
}
namespace acdk {
namespace sql {
namespace odbc {

struct TypeToTypeMap
{
  int odbcType;
  int sqlType;
};

/*
TypeToTypeMap typeMap[] =
{
  
};

int 
ODBCHandle::odbcTypeToSqlType(int typ)
{

}
int 
ODBCHandle::sqlTypeToOdbcType(int typ)
{
}
*/

// #define DOUT(msg) do { std::cout <<  msg << ". " << __FILE__ << ":" << __LINE__ << std::endl; } while(false)
#define DOUT(msg) do { } while(false)

#if 0
#define SQLCHAINP(chain, msg) \
if ((chain != Nil) /* && (msg != Nil) */) { \
  ::acdk::sql::RSQLException tSQL = new SQLException(msg); \
  tSQL->setNextException(chain); \
  chain = tSQL; \
  tSQL = Nil; \
} else { \
  chain = new SQLException(msg); \
}

#endif // 0

ODBCHandle::ODBCHandle(SQLSMALLINT htype) 
: _handle(SQL_NULL_HANDLE)
, _htype(htype)
, _initialized(false)
, _returnCodeValid(false)
, _numberRecordsValid(false)
, _cursorRowCountValid(false)
, _rowCountValid(false)
, _dynamicFunctionCodeValid(false)
, _statusRecords(Nil) 
{
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle::ODBCHandle");
}

ODBCHandle::~ODBCHandle() 
{ 
  //crashes on linux because called in __do_global_dtors ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle::~ODBCHandle");
  deinit();
  DOUT("ODBCHandle::~ODBCHandle() AFTER dinit()");
}

RODBCHandle
ODBCHandle::init(INP(RODBCHandle) prnt)
{
  SQLRETURN err;
  SQLHANDLE parent;
  RString hndltype;
  ::acdk::sql::RSQLException excpt = Nil;
  ::acdk::sql::RSQLWarning warng = Nil;
  
  if (prnt != Nil)
    parent = prnt->_getSQLHandle();
  else
    parent = SQL_NULL_HANDLE;
  
  if (_initialized == true)
    return this;
#if ODBCVER >= 0x300
  ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLAllocHandle");
  err = ::SQLAllocHandle(_htype, parent, &_handle);
#else // ODBCVER
  switch (_htype) {
    case SQL_HANDLE_ENV:
      ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLAllocEnv");
      err = ::SQLAllocEnv(&_handle);
      break;
    case SQL_HANDLE_DBC:
      ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLAllocEnv");
      err = ::SQLAllocConnect(parent, &_handle);
      break;
    case SQL_HANDLE_STMT:
      ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLAllocEnv");
      err = ::SQLAllocStmt(parent, &_handle);
      break;
    default:
      _addException("unknown handle-type");
      SQLTHROW(_excpt);
      break;
  }
#endif // ODBCVER
  switch (err) {
    case SQL_SUCCESS: // all went ok.
      _initialized = true;
      break;
    case SQL_SUCCESS_WITH_INFO:
      if ((prnt != Nil) && (parent != SQL_NULL_HANDLE))
        prnt->_getSQLdiag(err, __FILE__, __LINE__);
      SQLTHROW(_excpt);
      break;
    case SQL_INVALID_HANDLE:
      _addException("invalid parent handle");
      SQLTHROW(_excpt);
      break;
    case SQL_ERROR:
      if (_handle != SQL_NULL_HANDLE) {
        _getSQLdiag(err, __FILE__, __LINE__);
      } else if ((prnt != Nil) && (parent != SQL_NULL_HANDLE)) {
        prnt->_getSQLdiag(err, __FILE__, __LINE__);
        SQLTHROW(prnt->_getExceptions());
        break;
      } else {
        _addException("SQLAllocHandle returns error, and there's no possibility to get more info about the reason");
      }
      SQLTHROW(_excpt);
      break;
    default:  // ooops!
      _addException("unknown error from SQLAllocHandle");
      SQLTHROW(_excpt);
      break;
  }
#if ODBCVER >= 0x0300
  if (_htype == SQL_HANDLE_ENV) {
    /*
     * SQLRETURN SQLSetEnvAttr(SQLHENV EnvironmentHandle, SQLINTEGER Attribute, SQLPOINTER ValuePtr, SQLINTEGER StringLength);
     */
    // we always have to set the version.
    callSQL3(this, SQLSetEnvAttr, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC2, SQL_IS_UINTEGER);
  }
#endif // ODBCVER
  return this;
}

void
ODBCHandle::deinit()
{
  if (_initialized == false)
    return;
  if (sys::core_system::getState() == sys::AfterMain)
    return;
  // SQLFreeHandle() is (maybe) the only SQL-call without the handle as first arg...
#if ODBCVER >= 0x300
  if (_htype == SQL_HANDLE_DBC)
    ::SQLDisconnect(_handle);
  _chkSQLrcode(SQLFreeHandle(_htype, _handle), "SQLFreeHandle", __FILE__, __LINE__);
#else // ODBCVER
  switch (_htype) {
    case SQL_HANDLE_ENV:
      ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLFreeEnv");
      ::SQLFreeEnv(&_handle);
      break;
    case SQL_HANDLE_DBC:
      ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLDisconnect & SQLFreeConnect");
      ::SQLDisconnect(&_handle);
      ::SQLFreeConnect(&_handle);
      break;
    case SQL_HANDLE_STMT:
      ACDK_NLOG("acdk.sql.odbc", Debug, "ODBCHandle:: SQLFreeStmt");
      ::SQLFreeStmt(&_handle, SQL_DROP);
      break;
    default:
      _addException("unknown handle-type");
      SQLTHROW(_excpt);
      break;
  }
#endif // ODBCVER
  _initialized = false;
  _handle = 0;
  _htype = -1;
}

/*
 * helper-functions to get combined warning/error-List from underlaying ODBC-Driver and
 * devide it into separate object-lists of type SQLException and SQLWarning.
 * it returns false for no error or warning, and true if there're warnings
 * the references may be already initialized or not.
 */


void
ODBCHandle::_getSQLdiag(SQLRETURN err, char *file, int line)
{
  SYNCTHIS();
  const char *errstr;
  
  switch (err) {
    case SQL_SUCCESS:
      errstr = "SQL_SUCCESS";
      break;
    case SQL_SUCCESS_WITH_INFO:
      errstr = "SQL_SUCCESS_WITH_INFO";
      break;
    case SQL_INVALID_HANDLE:
      errstr = "SQL_INVALID_HANDLE";
      break;
    case SQL_NO_DATA:
      errstr = "SQL_NO_DATA";
      break;
    case SQL_NEED_DATA:
      errstr = "SQL_NEED_DATA";
      break;
    case SQL_STILL_EXECUTING:
      errstr = "SQL_STILL_EXECUTING";
      break;
    case SQL_ERROR:
      errstr = "SQL_ERROR";
      break;
    default:
      errstr = "SQL_UNKNOWN_ERR";
      break;
  }

  // maybe there're multiple errors, which we chain to a SQLException-list
  //### on linux crash this:
  _getallSQLdiags(errstr, file, line);
  DOUT("after _getallSQLdiags(errstr, file, line);");
#if ODBCVER < 0x0300
  _returnCodeValid = true;
  _returnCode = err;
#endif // ODBCVER
}



void
ODBCHandle::_getallSQLdiags(const char* errstr, char *file, int line)
{
  SQLRETURN resRet = 0;
  char resStr[SQL_MAX_MESSAGE_LENGTH + 1];
  memset(resStr, '\0', sizeof(resStr));
  SQLINTEGER resInt = 0;
  SQLSMALLINT diagLen = 0;
  bool ret = false;
  
#if ODBCVER >= 0x0300 && !defined(ACDK_OS_LINUX)
    // first all ODBC HeaderFields
  _numberRecordsValid = _getsingleSQLdiag(0, SQL_DIAG_NUMBER, &resInt, sizeof(resInt), diagLen, errstr);
  _numberRecords = (_numberRecordsValid == true) ? resInt : 0;

  _returnCodeValid = _getsingleSQLdiag(0, SQL_DIAG_RETURNCODE, &resRet, sizeof(resRet), diagLen, errstr);
  _returnCode = (_returnCodeValid == true)? resRet : 0;

  if (_htype == SQL_HANDLE_STMT) {
    _cursorRowCountValid = _getsingleSQLdiag(0, SQL_DIAG_CURSOR_ROW_COUNT, &resInt, sizeof(resInt), diagLen, errstr);
    _cursorRowCount = (_cursorRowCountValid == true)? resInt : 0;

    ret = _getsingleSQLdiag(0, SQL_DIAG_DYNAMIC_FUNCTION, resStr, sizeof(resStr), diagLen, errstr);
    if (diagLen < 0 || diagLen > (SQLSMALLINT)sizeof(resStr))
      ret = false;
    if (ret == true) {
      _dynamicFunction = ODBCSTR2STR(resStr, diagLen);
    } else { 
      _dynamicFunction = Nil;
    }

    _dynamicFunctionCodeValid = _getsingleSQLdiag(0, SQL_DIAG_DYNAMIC_FUNCTION_CODE, &resInt, sizeof(resInt), diagLen, errstr);
    _dynamicFunctionCode = (_dynamicFunctionCodeValid == true)? resInt : 0;

    _rowCountValid = _getsingleSQLdiag(0, SQL_DIAG_ROW_COUNT, &resInt, sizeof(resInt), diagLen, errstr);
    _rowCount = (_rowCountValid == true)? resInt : 0;
  } else {
    _cursorRowCount = 0;
    _cursorRowCountValid = false;
    _dynamicFunction = Nil;
    _dynamicFunctionCode = 0;
    _dynamicFunctionCodeValid = false;
    _rowCount = 0;
    _rowCountValid = false;
  }
  _statusRecords = Nil;
  if (_numberRecords == 0) 
  {
    StringBuffer sb;
    sb << "SQL-Error: " << (char*)errstr; // << " at " << (const char*)file << ":" << line;
    _addException(SBSTR("SQL-Error: " << errstr << " at " << file << ":" << line));
  }
  else
  {
    _statusRecords = new ODBCStatusRecordArray(_numberRecords);
    for (int n = 1; n <= _numberRecords; n++) 
    {
      RODBCStatusRecord sr = new ODBCStatusRecord();

      ret = _getsingleSQLdiag(n, SQL_DIAG_CLASS_ORIGIN, resStr, sizeof(resStr), diagLen, errstr);
      if (diagLen < 0 || diagLen > (SQLSMALLINT)sizeof(resStr))
        ret = false;
      sr->_classOrigin = (ret == true)? RString(ODBCSTR2STR(resStr, diagLen)) : RString(Nil);
      ret = _getsingleSQLdiag(n, SQL_DIAG_SUBCLASS_ORIGIN, resStr, sizeof(resStr), diagLen, errstr);
      if (diagLen < 0 || diagLen > (SQLSMALLINT)sizeof(resStr))
        ret = false;
      sr->_subClassOrigin = (ret == true)? RString(ODBCSTR2STR(resStr, diagLen)) : RString(Nil);
      ret = _getsingleSQLdiag(n, SQL_DIAG_CONNECTION_NAME, resStr, sizeof(resStr), diagLen, errstr);
      if (diagLen < 0 || diagLen > (SQLSMALLINT)sizeof(resStr))
        ret = false;
      sr->_connectionName = (ret == true)? RString(ODBCSTR2STR(resStr, diagLen)) : RString(Nil);
      ret = _getsingleSQLdiag(n, SQL_DIAG_MESSAGE_TEXT, resStr, sizeof(resStr), diagLen, errstr);
      if (diagLen < 0 || diagLen > (SQLSMALLINT)sizeof(resStr))
        ret = false;
      sr->_messageText = (ret == true)? RString(ODBCSTR2STR(resStr, diagLen)) : RString(Nil);
      sr->_nativeErrValid = _getsingleSQLdiag(n, SQL_DIAG_NATIVE, &resInt, sizeof(resInt), diagLen, errstr);
      sr->_nativeErr = (sr->_nativeErrValid == true)? resInt : 0;
      ret = _getsingleSQLdiag(n, SQL_DIAG_SQLSTATE, resStr, sizeof(resStr), diagLen, errstr);
      if (diagLen < 0 || diagLen > (SQLSMALLINT)sizeof(resStr))
        ret = false;
      sr->_sqlState = (ret == true)? RString(ODBCSTR2STR(resStr, diagLen)) : RString(Nil);
      if (_htype == SQL_HANDLE_STMT) {
        sr->_columnNumberValid = _getsingleSQLdiag(n, SQL_DIAG_COLUMN_NUMBER, &resInt, sizeof(resInt), diagLen, errstr);
        sr->_columnNumber = (sr->_columnNumberValid == true)? resInt : -1;
        sr->_rowNumberValid = _getsingleSQLdiag(n, SQL_DIAG_ROW_NUMBER, &resInt, sizeof(resInt), diagLen, errstr);
        sr->_rowNumber = (sr->_rowNumberValid == true)? resInt : -1;
      }
      _statusRecords->append(sr);
      DOUT("_statusRecords->append(sr);");

      if (true) // ((sr->_sqlState != Nil) && (sr->_sqlState->startsWith("00") == false)) {
      { 
        StringBuffer sb;
        RString unset("<unset>");
        sb.append("[SQLSTATE:");
        sb.append(sr->_sqlState);
        sb.append("][CLASS_ORIGIN:");
        sb.append((sr->_classOrigin != Nil)? sr->_classOrigin : unset);
        sb.append("][SUBCLASS_ORIGIN:");
        sb.append((sr->_subClassOrigin != Nil)? sr->_subClassOrigin : unset);
        sb.append("][CONNECTION_NAME:");
        sb.append((sr->_connectionName != Nil)? sr->_connectionName : unset);
        sb.append("][MESSAGE_TEXT:");
        sb.append((sr->_messageText != Nil)? sr->_messageText : unset);
        sb.append("][NATIVE_ERR:");
        sb.append((sr->_nativeErrValid == true)? ::acdk::lang::Integer::toString(sr->_nativeErr) : unset);
        if (_htype == SQL_HANDLE_STMT) {
          sb.append("][COLUMN_NUMBER:");
          if (sr->_columnNumberValid == true) {
            switch (sr->_columnNumber) {
              case SQL_NO_COLUMN_NUMBER:
                sb.append("<no_column>");
                break;
              case SQL_COLUMN_NUMBER_UNKNOWN:
                sb.append("<unknown_column>");
                break;
              default:
                sb.append(::acdk::lang::Integer::toString(sr->_columnNumber));
                break;
            }
          } else {
            sb.append(unset);
          }
          sb.append("][ROW_NUMBER:");
          if (sr->_rowNumberValid == true) {
            switch (sr->_rowNumber) {
              case SQL_NO_ROW_NUMBER:
                sb.append("<no_row>");
                break;
              case SQL_ROW_NUMBER_UNKNOWN:
                sb.append("<row_column>");
                break;
              default:
                sb.append(::acdk::lang::Integer::toString(sr->_rowNumber));
                break;
            }
          } else {
            sb.append("<unset>");
          }
        }
        DOUT("X1");
        sb.append("]");
        if (sr->_sqlState->startsWith("00") == true) 
        {
          /* just do nothing for simple status-messages */
#if defined(ACDK_ODBC_DEBUG)
          ::acdk::lang::System::err->println(RString("[") + file + ":" + Integer::toString(line) + "] INFO: " + sb.toString());
#endif
        } 
        else if (sr->_sqlState->startsWith("01") == true) 
        {
          _addWarning(RString("[") + file + ":" + Integer::toString(line) + "] WARN:");
          _addWarning(sb.toString());
        } 
        else 
        {
          _addException(RString("[") + file + ":" + Integer::toString(line) + "] ERR:");
          _addException(sb.toString());
        }
      }
      DOUT("X2");
    }
  }
#else // ODBCVER >= 0x0300
  DOUT("X4");
  _numberRecordsValid = true;
  _numberRecords = 1;

  _cursorRowCount = 0;
  _cursorRowCountValid = false;
  _dynamicFunction = Nil;
  _dynamicFunctionCode = 0;
  _dynamicFunctionCodeValid = false;
  _rowCount = 0;
  _rowCountValid = false;

  StringBuffer sb;
  RODBCStatusRecord sr = new ODBCStatusRecord();
  SQLINTEGER nativeerr;
  ODBC_NATIVE_CHAR state[6];
  DOUT("X5");
  switch (_htype) {
    case SQL_HANDLE_ENV:
      resRet = SQLError(_handle, SQL_NULL_HDBC, SQL_NULL_HSTMT, state, &nativeerr, (ODBC_NATIVE_CHAR *)resStr, sizeof(resStr), &diagLen);
      break;
    case SQL_HANDLE_DBC:
      resRet = SQLError(SQL_NULL_HDBC, _handle, SQL_NULL_HSTMT, state, &nativeerr, (ODBC_NATIVE_CHAR *)resStr, sizeof(resStr), &diagLen);
      break;
    case SQL_HANDLE_STMT:
      resRet = SQLError(SQL_NULL_HDBC, SQL_NULL_HSTMT, _handle, state, &nativeerr, (ODBC_NATIVE_CHAR *)resStr, sizeof(resStr), &diagLen);
      break;
    default:
      _addException("unknown handle-type");
      SQLTHROW(_excpt);
      break;
  }

  sr->_messageText = ODBCSTR2STR(resStr, diagLen);
  sr->_nativeErrValid = (nativeerr != 0)? true : false;
  sr->_columnNumberValid = false;
  sr->_columnNumber =  -1;
  sr->_rowNumberValid = false;
  sr->_rowNumber = -1;
  if (sr->_sqlState == Nil)
    sr->_sqlState = "";

  sb.append("[SQLSTATE:");
  sb.append(sr->_sqlState);
  sb.append("][MESSAGE_TEXT:");
  sb.append((sr->_messageText != Nil)? sr->_messageText : "<unset>");
  sb.append("][NATIVE_ERR:");
  sb.append((sr->_nativeErrValid == true)? ::acdk::lang::Integer::toString(sr->_nativeErr) : RString("<unset>"));
  sb.append("]");
  
  if (sr->_sqlState->startsWith("00") == true) {
    /* just do nothing for simple status-messages */
#if defined(ACDK_ODBC_EDBUG)
    ::acdk::lang::System::err->println(RString("[") + file + ":" + Integer::toString(line) + "] INFO: ") + sb.toString());
#endif
  } else if (sr->_sqlState->startsWith("01") == true) {
    _addWarning(RString("[") + file + ":" + Integer::toString(line) + "] WARN:");
    _addWarning(sb.toString());
  } else {
    _addException(RString("[") + file + ":" + Integer::toString(line) + "] ERR:");
    _addException(sb.toString());
  }
  _statusRecords = new ODBCStatusRecordArray(_numberRecords);
  _statusRecords->append(sr);
#endif // ODBCVER
  DOUT("Xa");
}

bool
ODBCHandle::_getsingleSQLdiag(SQLSMALLINT diagRec, SQLSMALLINT diagID, SQLPOINTER diagPtr, SQLSMALLINT diagSize, SQLSMALLINT& diagLen, const char* errstr)
{

#if ODBCVER >= 0x0300
  SQLRETURN err;
  StringBuffer errmsg;
  RString diagName;

  /*
   * SQLRETURN SQLGetDiagRec(SQLSMALLINT HandleType,SQLHANDLE Handle,SQLSMALLINT RecNumber,SQLCHAR *Sqlstate,
   *                         SQLINTEGER *NativeErrorPtr,SQLCHAR *MessageText,SQLSMALLINT BufferLength,
   *                         SQLSMALLINT *TextLengthPtr);
   */
  /*
   * SQLRETURN SQLGetDiagField(SQLSMALLINT HandleType,SQLHANDLE Handle,SQLSMALLINT RecNumber,
   *                           SQLSMALLINT DiagIdentifier,SQLPOINTER DiagInfoPtr,SQLSMALLINT BufferLength,
   *                           SQLSMALLINT *StringLengthPtr);
   */
  err = SQLGetDiagField(_htype, _handle, diagRec, diagID, diagPtr, diagSize, &diagLen);
  DOUT("SQLGetDiagField1: rec=" << diagRec << "; diagId=" << diagID << "; err=" << err);

  if (err != SQL_SUCCESS) {

#define MKDIAGSTR(__id) \
  case SQL_DIAG_ ## __id: \
    diagName = new String(#__id); \
    break;

    switch (diagID) {
      MKDIAGSTR(NUMBER)
      MKDIAGSTR(RETURNCODE)
      MKDIAGSTR(CURSOR_ROW_COUNT)
      MKDIAGSTR(DYNAMIC_FUNCTION)
      MKDIAGSTR(DYNAMIC_FUNCTION_CODE)
      MKDIAGSTR(ROW_COUNT)
      MKDIAGSTR(CLASS_ORIGIN)
      MKDIAGSTR(SUBCLASS_ORIGIN)
      MKDIAGSTR(CONNECTION_NAME)
      MKDIAGSTR(MESSAGE_TEXT)
      MKDIAGSTR(NATIVE)
      MKDIAGSTR(SQLSTATE)
      MKDIAGSTR(COLUMN_NUMBER)
      MKDIAGSTR(ROW_NUMBER)
      default:
        diagName = Nil;
        break;
    }
#undef MKDIAGSTR
  }

  switch (err) 
  {
    case SQL_SUCCESS: // nothing went wrong this time.
      return true;
      break;
    case SQL_SUCCESS_WITH_INFO: // message-buffer is too small, need at least msglen bytes
      errmsg.append("double err: message buffer too small (");
      errmsg.append(::acdk::lang::String::valueOf(diagSize));
      errmsg.append(" instead of ");
      errmsg.append(::acdk::lang::String::valueOf(diagLen));
      errmsg.append(") after ");
      errmsg.append(errstr);
      _addWarning(errmsg.toString());
      return true;
      /* not reached */
    case SQL_INVALID_HANDLE:  // maybe the caller have forgotten to call SQLAllocHandle()
      errmsg.append("double err: invalid handle after ");
      errmsg.append(errstr);
      _addException(errmsg.toString());
      return false;
      /* not reached */
    case SQL_ERROR: // either (RecNumber <= 0) or (BufferLength < 0)
      errmsg.append("double err: invalid BufferLength (");
      errmsg.append(Integer::toString(diagSize));
      errmsg.append(") or RecNumber (");
      errmsg.append(Integer::toString(diagRec));
      errmsg.append(") for DiagIdentifier ");
      if (diagName != Nil) {
        errmsg.append("\"");
        errmsg.append(diagName);
        errmsg.append("\" ");
      }
      errmsg.append("(");
      errmsg.append(Integer::toString(diagID));
      errmsg.append(") after ");
      errmsg.append(errstr);
      _addException(errmsg.toString());
      return false;
      /* not reached */
    case SQL_NO_DATA:/*
      errmsg.append("no further information available for RecNumber (");
      errmsg.append(Integer::toString(diagRec));
      errmsg.append(") for DiagIdentifier ");
      if (diagName != Nil) {
        errmsg.append("\"");
        errmsg.append(diagName);
        errmsg.append("\" ");
      }
      errmsg.append("(");
      errmsg.append(Integer::toString(diagID));
      errmsg.append(") after ");
      errmsg.append(errstr);
      _addException(errmsg.toString());*/
      return false;
      /* not reached */
      break;
    default:
    {

      errmsg.append("double err: unhandled SQL error ");
      errmsg.append(::acdk::lang::String::valueOf(err));
      errmsg.append(" for RecNumber (");
      errmsg.append(Integer::toString(diagRec));
      errmsg.append(") for DiagIdentifier ");
      if (diagName != Nil) {
        errmsg.append("\"");
        errmsg.append(diagName);
        errmsg.append("\" ");
      }
      errmsg.append("(");
      errmsg.append(Integer::toString(diagID));
      errmsg.append(") after ");
      errmsg.append(errstr);
      _addException(errmsg.toString());
      return false;
      /* not reached */
    }
  }
#else
  return false;
#endif // ODBCVER
}

int
ODBCHandle::_chkSQLrcode(SQLRETURN err, char *func, char *file, int line)
{
  ACDK_NLOG("acdk.sql.odbc", Debug, SBSTR("Called " << func << "(" << (int)_handle << ") with rc=" << err << " on " << file << ":" << line));
  RString msg = Nil;
  switch (err) {
    case SQL_SUCCESS: // all went ok.
      //_getSQLdiag(err, file, line);
      break;
    case SQL_NO_DATA:
      if (RString(func)->compareTo("SQLFetch") == 0) {
        _getSQLdiag(err, file, line);
        break;
      }
      // else fall through
    case SQL_SUCCESS_WITH_INFO:
    case SQL_NEED_DATA:
    case SQL_STILL_EXECUTING:
    case SQL_ERROR:
      _getSQLdiag(err, file, line);
      SQLTHROW(_excpt); // may return, if getSQLdiag only found some warnings
      break;
    case SQL_INVALID_HANDLE:
      msg = RString("invalid handle for ") + func;
      _addException(RString("[") + file + ":" + Integer::toString(line) + "] ERR:");
      _addException(msg);
      SQLTHROW(_excpt);
      break;
    default:  // ooops!
      msg = RString("unknown error from ") + func + ": " + Integer::toString(err);
      _addException(RString("[") + file + ":" + Integer::toString(line) + "] ERR:");
      _addException(msg);
      SQLTHROW(_excpt);
      break;
  }
#if defined(ACDK_ODBC_DEBUG)
  if (_warng != Nil) {
    ::acdk::sql::RSQLWarning w = _warng;
    ::acdk::lang::System::err->println("following warnings have been collected:");
    do {
      ::acdk::lang::System::err->println(w->getMessage());
      w = w->getNextWarning();
    } while (w != Nil);
    _warng = Nil;
  }
#endif
  return err;
}

/* this function is a quick implementation and should be later a wrapper to _setSQLAttr(key, ptr, len) */

void
ODBCHandle::_setSQLFlag(SQLUINTEGER key, SQLUINTEGER val)
{
  SQLRETURN err;
  char *fnam;
  RString msg;
  
  /*
   * options can be set with:
   *
   * SQLRETURN SQLSetXXXXAttr(SQLHANDLE Handle, SQLINTEGER Attribute, SQLPOINTER ValuePtr, SQLINTEGER StringLength);
   *
   * where namespace odbc {X can be one of Env, Connect, Stmt or Desc
   */

  switch (_htype) {
#if ODBCVER >= 0x0300
    case SQL_HANDLE_ENV:
      fnam = "SQLSetEnvAttr";
      err = ::SQLSetEnvAttr(_handle, key, &val, sizeof(val));
      break;
#endif // ODBCVER
    case SQL_HANDLE_DBC:
      fnam = "SQLSetConnectAttr";
#if ODBCVER >= 0x0300
      err = ::SQLSetConnectAttr(_handle, key, &val, sizeof(val));
#else // ODBCVER
      err = ::SQLSetConnectOption(_handle, key, val);
#endif // ODBCVER
      break;
    case SQL_HANDLE_STMT:
      fnam = "SQLSetStmtAttr";
#if ODBCVER >= 0x0300
      err = ::SQLSetStmtAttr(_handle, key, &val, sizeof(val));
#else // ODBCVER
      err = ::SQLSetStmtOption(_handle, key, val);
#endif // ODBCVER
      break;
#if ODBCVER >= 0x0300
    case SQL_HANDLE_DESC:
      fnam = "SQLSetDescAttr";
      //err = ::SQLSetDescAttr(_handle, key, &val, sizeof(val)); // not existant.
      _addException("no method to set Attributes for a descriptor handle");
      SQLTHROW(_excpt);
      break;
#endif // ODBCVER
    default:
      _addException("unknown handle-type");
      SQLTHROW(_excpt);
      break;
  }
  _chkSQLrcode(err, fnam, __FILE__, __LINE__);
}

} // odbc
} // sql
} // acdk
