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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteTable.cpp,v 1.9 2005/04/26 15:47:43 kommer Exp $


#include "LiteResultSet.h"
#include <acdk/sql/Types.h>
#include <acdk/util/logging/Log.h>


#include "../../../sqlitesrc/sqlite3.h"

namespace acdk {
namespace sql {
namespace sqlite {

using namespace acdk::lang::dmi;

//static 
RLiteDb 
LiteDb::openDb(IN(RString) fileName)
{
  RString ldbFile = STR2LITESTR(fileName);
  const char* fname = ldbFile->c_str(); 
  sqlite3* db = 0;
  int ret = sqlite3_open(fname, &db);
  if (ret != SQLITE_OK)
    THROW1(SQLException, "Cannot connect to DB: " + fileName);  
  return new LiteDb(db);
}

static void acdk_trace(void* p, const char* msg)
{
  ACDK_NLOG("acdk.sql.sqlite", Trace, msg);
}


LiteDb::LiteDb(sqlite3* con)
: _con(con)
{
  void* r = sqlite3_trace(con, acdk_trace, 0);
}


void 
LiteDb::closeDb()
{
  LiteDb::checkResult(_con, sqlite3_close(_con));
  _con = 0;
}

int 
LiteDb::changes()
{
  return sqlite3_changes(_con);
}

void 
LiteDb::checkLastResult(sqlite3* con)
{
  checkResult(con, sqlite3_errcode(con));
}

int 
LiteDb::checkResult(sqlite3* con, int result)
{
  switch(result)
  {
  case SQLITE_ROW:
  case SQLITE_DONE:
  case SQLITE_OK:
    return result;

  case SQLITE_ABORT:
  case SQLITE_BUSY:
  case SQLITE_LOCKED:
  case SQLITE_INTERRUPT:
    THROW1(SQLException, "locking not supported yet");
    break;
  default:
  {
    RString msg = LITESTR2STR(sqlite3_errmsg(con));
    THROW1(SQLException, msg);
    break;
  }
  }
  return result;
}

void 
LiteDb::_checkOpenDb()
{
  if (_con == 0)
    THROW1(SQLException, "connection is closed");
}

//static 
RString 
LiteDb::getVersion()
{
  const char*cptr = sqlite3_libversion();
  return LITESTR2STR(cptr);
}

void
LiteDb::execute(IN(RString) cmd)
{
  _checkOpenDb();
  char* errmsg;
  RString lcmd = STR2LITESTR(cmd);
  int r = sqlite3_exec(_con, lcmd->c_str(), 0, 0, &errmsg);
  if (r != SQLITE_OK)
  {
    RString msg = SCS(errmsg);
    sqlite3_free(errmsg);
    THROW1(SQLException, msg);
  }
}


struct CallBackHolder
{
  RLiteDb db;
  acdk::lang::dmi::RDmiDelegate delegate;
  CallBackHolder(IN(RLiteDb) db_, IN(acdk::lang::dmi::RDmiDelegate) delegate_)
    : db(db_)
    , delegate(delegate_)
  {
  }
};

int sqlite3_dmicallback(void* vcb, int columns, char** colvals, char** colnames)
{
  CallBackHolder* cbh = (CallBackHolder*)vcb;
  acdk::lang::dmi::DmiNamedArgArray na(columns + 1);
  na[0] = new acdk::lang::dmi::DmiNamedArg("__db", new acdk::lang::dmi::DmiObject(inOf(cbh->db)));
  for (int i = 0; i < columns; ++i)
  {
    na[i + 1] =  new acdk::lang::dmi::DmiNamedArg(LITESTR2STR(colnames[i]), new acdk::lang::dmi::DmiObject(inOf(LITESTR2STR(colvals[i]))));
  }
  acdk::lang::dmi::RDmiObject ret = cbh->delegate->call((acdk::lang::dmi::RDmiNamedArgArray)&na);
  if (ret == Nil || ret->getVarType() == UnknownVT)
    return SQLITE_OK;
  return ret->getIntVar();
}

void 
LiteDb::execute(IN(RString) sql, IN(acdk::lang::dmi::RDmiDelegate) delegate)
{
   _checkOpenDb();
  char* errmsg;
  RString lcmd = STR2LITESTR(sql);
  CallBackHolder dbh(this, delegate);
  int r = sqlite3_exec(_con, lcmd->c_str(), sqlite3_dmicallback, &dbh, &errmsg);
  if (r != SQLITE_OK)
  {
    RString msg = SCS(errmsg);
    sqlite3_free(errmsg);
    THROW1(SQLException, msg);
  }
}


int  
LiteDb::executeUpdate(IN(RString) cmd)
{
  _checkOpenDb();
  char* errmsg;
  RString lcmd = STR2LITESTR(cmd);
  int r = sqlite3_exec(_con, lcmd->c_str(), 0, 0, &errmsg);
  if (r != SQLITE_OK)
  {
    RString msg = LITESTR2STR(errmsg);
    sqlite3_free(errmsg);
    THROW1(SQLException, msg);
  }
  return sqlite3_changes(_con);
}

RLiteMemTable 
LiteDb::execDirect(IN(RString) sql)
{
  _checkOpenDb();
  RString lsql = STR2LITESTR(sql);
  char** table = 0;
  int rows = 0;
  int cols = 0;
  char* errmsg = 0;
  int r = sqlite3_get_table(_con, lsql->c_str(), &table, &rows, &cols, &errmsg);
  if (r != SQLITE_OK)
  {
    RString aerrmsg = LITESTR2STR(errmsg);
    sqlite3_free(errmsg);
    THROW1(SQLException, aerrmsg);
  }
  return new LiteMemTable(table, cols, rows);
}

RLiteTable 
LiteDb::prepareStatement(IN(RString) sql)
{
  sqlite3_stmt* stm = 0;
  RString lstr = STR2LITESTR(sql);
  checkResult(sqlite3_prepare(_con, lstr->c_str(), sql->length(), &stm, 0));
  return new LiteTable(this, stm);
}

struct SqlLiteFuncWrapper
{
  acdk::lang::dmi::RDmiDelegate delegate;
  SqlLiteFuncWrapper(IN(acdk::lang::dmi::RDmiDelegate) del)
    : delegate(del)
  {}
};

ScriptVar
sqlite2sv(sqlite3_value* v)
{
  switch(sqlite3_value_type(v))
  {
   case SQLITE_INTEGER:
    return inOf(sqlite3_value_int(v));
  case SQLITE_FLOAT:
    return inOf(sqlite3_value_double(v));
  case SQLITE_TEXT:
    return inOf(LITESTR2STR((const char*)sqlite3_value_text(v)));
  case SQLITE_BLOB:
  {
    const byte *d = (const byte *)sqlite3_value_blob(v);
    int l = sqlite3_value_bytes(v);
    RCoreByteBuffer bb = new CoreByteBuffer(l);
    memcpy(bb->begin(), d, l);
    return new StandardMemBlob(&bb);
  }
  case SQLITE_NULL: 
    return inOf(Nil);
  default:
    THROW1(SQLException, "unmappable SQL type");
  }
}
  /*
  const void *sqlite3_value_blob(sqlite3_value*);
int sqlite3_value_bytes(sqlite3_value*);
int sqlite3_value_bytes16(sqlite3_value*);
double sqlite3_value_double(sqlite3_value*);
int sqlite3_value_int(sqlite3_value*);
long long int sqlite3_value_int64(sqlite3_value*);
const unsigned char *sqlite3_value_text(sqlite3_value*);
const void *sqlite3_value_text16(sqlite3_value*);
const void *sqlite3_value_text16be(sqlite3_value*);
const void *sqlite3_value_text16le(sqlite3_value*);
int sqlite3_value_type(sqlite3_value*);

  void sqlite3_result_blob(sqlite3_context*, const void*, int n, void(*)(void*));
void sqlite3_result_double(sqlite3_context*, double);
void sqlite3_result_error(sqlite3_context*, const char*, int);
void sqlite3_result_error16(sqlite3_context*, const void*, int);
void sqlite3_result_int(sqlite3_context*, int);
void sqlite3_result_int64(sqlite3_context*, long long int);
void sqlite3_result_null(sqlite3_context*);
void sqlite3_result_text(sqlite3_context*, const char*, int n, void(*)(void*));
void sqlite3_result_text16(sqlite3_context*, const void*, int n, void(*)(void*));
void sqlite3_result_text16be(sqlite3_context*, const void*, int n, void(*)(void*));
void sqlite3_result_text16le(sqlite3_context*, const void*, int n, void(*)(void*));
void sqlite3_result_value(sqlite3_context*, sqlite3_value*);
*/




void setResult(sqlite3_context* ctx, IN(RDmiObject) ret)
{
  if (ret == Nil || ret->getVarType() == UnknownVT)
  {
    sqlite3_result_null(ctx);
    return;
  }
  switch(ret->getVarType())
  {
  case BoolVT:
    if (ret->getBoolVar() == true)
      sqlite3_result_int(ctx, 1);
    else
      sqlite3_result_int(ctx, 0);
    break;
  case CharVT:
  case UcCharVT:
    //???
  case ShortVT:
  case IntVT:
    sqlite3_result_int(ctx, ret->getIntVar());
    break;
  case LongVT:
    sqlite3_result_int64(ctx, ret->getLongVar());
    break;
  case FloatVT:
  case DoubleVT:
    sqlite3_result_double(ctx, ret->getDoubleVar());
    break;
  case ObjectVT:
  {
    RObject o = ret->getObjectVar();
    if (o == Nil)
    {
      sqlite3_result_null(ctx);
      return;
    }

    if (instanceof(o, Blob) == true)
    {
      RBlob b = (RBlob)o;
      RReadByteBuffer rb = Buffers::getNativeReadByteBuffer(b->getReadByteBuffer());
      sqlite3_result_blob(ctx, rb->begin(), rb->length(), SQLITE_TRANSIENT);
      break;
    }
    RString sv = o->toString();
    sv = STR2LITESTR(sv);
    sqlite3_result_text(ctx, sv->c_str(), sv->length(), SQLITE_TRANSIENT);
    break;
  }
  }
}

void FuncToDelegate(sqlite3_context* ctx,int args, sqlite3_value** values)
{
  try {
    SqlLiteFuncWrapper* w = (SqlLiteFuncWrapper*)sqlite3_user_data(ctx);
    RDmiObjectArray dargs = new DmiObjectArray(args);
    for (int i = 0; i < args; ++i)
    {
      dargs[i] = new DmiObject(sqlite2sv(values[i]));
    }
    RDmiObject ret = w->delegate->call(dargs);
    setResult(ctx, ret);
  } catch (RThrowable ex) {
    RString msg = STR2LITESTR(ex->getMessage());
    sqlite3_result_error(ctx, msg->c_str(), msg->length()); // ### todo msg->length may not correct!
  }
}

void FreeFuncToDelegate(sqlite3_context* ctx)
{
  SqlLiteFuncWrapper* w = (SqlLiteFuncWrapper*)sqlite3_user_data(ctx);
  delete w;
}

void 
LiteDb::createSqlFunction(IN(RString) function, int args, IN(acdk::lang::dmi::RDmiDelegate) delegate)
{
  RString lfunction = STR2LITESTR(function);
  SqlLiteFuncWrapper* wrapper = new SqlLiteFuncWrapper(delegate);
  checkResult(sqlite3_create_function(_con, lfunction->c_str(), args, SQLITE_UTF8, wrapper, FuncToDelegate, 0, 0));
}



LiteTable::LiteTable(IN(RLiteDb) db, sqlite3_stmt* stm)
: _db(db)
, _stm(stm)
, _currow(-1)
, _rowDelivered(true)
, _hasRow(false)
, _rowCount(-1)
{

}


LiteTable::~LiteTable()
{
  sqlite3_finalize(_stm);
}


void
LiteTable::clearParameters()
{
  checkResult(sqlite3_reset(_stm));
}

bool 
LiteTable::hasNext()
{
  if (_rowDelivered == false)
    return _hasRow;
  next();
  _rowDelivered = false;
  return _hasRow;
}

RString 
LiteTable::getColName(int col) 
{ 
  if (_hasRow == false)
  {
    next();
    _rowDelivered = false;
  }
  const char* cn = sqlite3_column_name(_stm, col);
  if (cn == 0)
    checkLastResult();
  return LITESTR2STR(cn);
}

RObject
LiteTable::next()
{
  if (getNext() == true)
    return this;
  return Nil;
}

void 
LiteTable::remove()
{
  THROW1(UnsupportedOperationException, "LiteTable::remove()");
}

bool 
LiteTable::getNext()
{
  if (_rowDelivered == false)
  {
    _rowDelivered = true;
    return true;
  }
  int r = checkResult(sqlite3_step(_stm));
  if (r == SQLITE_ROW)
  {
    _hasRow = true;
    return true;
  }
  _hasRow = false;
  return false;
}

void 
LiteTable::reset() 
{ 
  checkResult(sqlite3_reset(_stm));
  _currow = -1;
}


int 
LiteTable::getLiteType(int col)
{
  return sqlite3_column_type(_stm, col);
}

int 
LiteTable::getSqlType(int col)
{
  int ltype = getLiteType(col);
  switch(ltype)
  {
  case SQLITE_INTEGER:
    return IntegerSqlType;
  case SQLITE_FLOAT:
    return FloatSqlType;
  case SQLITE_TEXT:
    return VarCharSqlType;
  case SQLITE_BLOB:
    return BlobSqlType;
  case SQLITE_NULL: 
    return NullSqlType;
  default:
    THROW1(SQLException, "unmappable SQL type");
  }
  return 0;
}

RString 
LiteTable::getSQLTypeName(int col)
{
  const char* dcl = sqlite3_column_decltype(_stm, col);
  if (dcl == 0)
    checkLastResult();
  return LITESTR2STR(dcl);
}

RString 
LiteTable::getColumnClassName(int col)
{
  int ltype = getLiteType(col);

  switch(ltype)
  {
  case 0:
    checkLastResult();
    break;
  case SQLITE_INTEGER:
    return "acdk/lang/Integer";
  case SQLITE_FLOAT:
    return "acdk/lang/Float";
  case SQLITE_TEXT:
    return "acdk/lang/String";
  case SQLITE_BLOB:
    return "acdk/sql/Blob";
  case SQLITE_NULL: 
    return "acdk/lang/Object";
  default:
    THROW1(SQLException, "unmappable SQL type");
  }
  return Nil;
}


int 
LiteTable::colCount() 
{ 
  return sqlite3_column_count(_stm); 
}
  
int 
LiteTable::getColumnByName(IN(RString) name)
{
  int colNum = colCount();
  for (int i = 0; i < colNum; ++i)
  {
    RString s = getColName(i);
    if (name->equalsIgnoreCase(s) == true)
      return i;
  }
  THROW1(SQLException, "column " + name + " not found");
  return -1;
}

bool 
LiteTable::getBoolean(int col)
{
  int r = getInt(col);
  return r != 0;
}

byte 
LiteTable::getByte(int col)
{
   return (byte)getInt(col);
}

short 
LiteTable::getShort(int col)
{
  return (short)getInt(col);
}

int 
LiteTable::getInt(int col)
{
  return sqlite3_column_int(_stm, col);
}

jlong 
LiteTable::getLong(int col)
{
  return sqlite3_column_int64(_stm, col);
}

float 
LiteTable::getFloat(int col)
{
  return (float)getDouble(col);
}

double 
LiteTable::getDouble(int col)
{
  return sqlite3_column_double(_stm, col);
}

RString 
LiteTable::getString(int col)
{
  const char* t = (const char*)sqlite3_column_text(_stm, col);
  if (t == 0)
    checkLastResult();
  return LITESTR2STR(t);
}

bool 
LiteTable::seek(int rowCount)
{
  
  if (rowCount == -1)
  {
    reset();
    return true;
  }

  if (rowCount == 0)
  {
    reset();
    return hasNext();
  }

  if (rowCount == -2 || rowCount == -3)
  {
    seekEnd();
    if (rowCount == -3)
      return seek(_rowCount + 1);
    else
      return seek(_rowCount);
  }

  if (rowCount < _currow)
    reset();
  while (_currow < rowCount)
    if (getNext() == false)
      return false;
  return true;
}

int 
LiteTable::seekEnd()
{
  if (_rowCount != -1)
    return _rowCount;
  while (getNext() == true)
    ;
  return _rowCount = _currow;
}

void 
LiteTable::bindInt(int col, int val)
{
  checkResult(sqlite3_bind_int(_stm, col, val));

}

void 
LiteTable::bindDouble(int col, double val)
{
  checkResult(sqlite3_bind_double(_stm, col, val));
}

void 
LiteTable::bindLong(int col, jlong val)
{
  checkResult(sqlite3_bind_int64(_stm, col, val));
}



void 
LiteTable::bindText(int col, IN(RString) value)
{
  RString lvalue = STR2LITESTR16(value);
  checkResult(sqlite3_bind_text16(_stm, col, lvalue->byte_begin(), lvalue->length() * 2, SQLITE_TRANSIENT));
  
}

void 
LiteTable::bindNull(int col)
{
  checkResult(sqlite3_bind_null(_stm, col));
}

int 
LiteTable::getParameterCount()
{
  return sqlite3_bind_parameter_count(_stm);
}

RString 
LiteTable::getParameterName(int col)
{
  const char* n = sqlite3_bind_parameter_name(_stm, col);
  if (n == 0)
  {
    checkLastResult();
    return Nil;
  }
  return LITESTR2STR(n);
}

int 
LiteTable::getParamterIndexByName(IN(RString) name)
{
  RString lname = STR2LITESTR(name);
  return sqlite3_bind_parameter_index(_stm, lname->c_str());
}

RFlexByteBuffer 
LiteTable::getBlob(int col)
{
  const byte* data = (const byte*)sqlite3_column_blob(_stm, col);
  int l = sqlite3_column_bytes(_stm, col);
  
  RCoreByteBuffer bb = new CoreByteBuffer(l);
  memcpy(bb->begin(), data, l);
  return &bb;
}

void 
LiteTable::bindBlob(int col, IN(RReadByteBuffer) buffer)
{
  RReadByteBuffer nb = Buffers::getNativeReadByteBuffer(buffer);
  checkResult(sqlite3_bind_blob(_stm, col, nb->begin(), nb->length(), SQLITE_TRANSIENT));
}

LiteMemTable::~LiteMemTable()
{
  sqlite3_free_table(_table);
}

RStringArray 
LiteMemTable::getColumnNames()
{
  RStringArray ca = new StringArray(_cols);
  for (int i = 0; i < _cols; ++i)
  {
    ca[i] = getColumnName(i + 1);
  }
  return ca;
}


int 
LiteMemTable::getMaxColWidth(int col)
{
  col = _checkColIdx(col);
  int maxw = 0;
  for (int i = 0; i <= _rows; ++i)
  {
    RString cv = LITESTR2STR(_table[i * _cols + col]);
    if (cv->length() > maxw)
      maxw = cv->length();
  }
  return maxw;
}

void 
LiteMemTable::printTable(IN(acdk::io::RPrintWriter) out)
{
  int cols = columnCount();
  int rows = rowCount();
  intArray widths;
  int i;
  int sumwidth = 0;
  for (i = 0; i < cols; ++i)
  {
    int w = getMaxColWidth(i + 1);
    sumwidth = sumwidth + w;
    widths.append(w);
  }
  for (i = 0; i < cols; ++i)
  {
    if (i > 0)
      out->print("|");
    out->print(getColumnName(i + 1)->rightPad(widths.get(i)));  
  }
  out->println("");
  out->println(RString("-")->repeat(sumwidth + cols));
  for (int r = 0; r < rows; ++r)
  {
    for (i = 0; i < cols; ++i)
    {
      if (i > 0)
        out->print("|");
      out->print(getField(r, i + 1)->rightPad(widths.get(i))); 
    }
    out->println("");
  }
}



} // sqlite
} // sql 
} // acdk

