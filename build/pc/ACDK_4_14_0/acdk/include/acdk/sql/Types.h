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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/Types.h,v 1.8 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_sql_Types_h
#define acdk_sql_Types_h

#include "sql.h"
#include <acdk/io/Reader.h>
#include <acdk/util/SysDate.h>

namespace acdk {
namespace sql {

using namespace acdk::lang;
using namespace acdk::io;
using namespace acdk::util;

#if defined(_UNICODE)
#define SQL_NATIVE_CHAR WCharSqlType
#define SQL_NATIVE_VCHAR WVarCharSqlType
#else
#define SQL_NATIVE_CHAR CharSqlType
#define SQL_NATIVE_VCHAR VarCharSqlType
#endif

enum SQLType 
{
    
    WLongVarCharSqlType = -10,
    WVarCharSqlType = -9,
    WCharSqlType = -8,
    BitSqlType = -7,
    TinyIntSqlType = -6,
    BigIntSqlType = -5,
    LongVarBinarySqlType = -4,
    VarBinarySqlType = -3,
    BinarySqlType  = -2,
    LongVarCharSqlType = -1,
    NullSqlType = 0,
    CharSqlType = 1,
    NumericSqlType = 2,
    DecimalSqlType = 3,
    IntegerSqlType = 4,
    SmallIntSqlType = 5,
    FloatSqlType = 6,
    RealSqlType = 7,
    DoubleSqlType = 8,
    VarCharSqlType = 12,
    OldDateSqlType = 9,
    OldTimeSqlType = 10,
    OldTimeStampSqlType = 11,
    DateSqlType = 91,
    TimeSqlType = 92,
    TimeStampSqlType = 93,
    OtherSqlType = 1111,
    ACDK_ObjectSqlType = 2000,
    DistinctSqlType = 2001,
    StructSqlType = 2002,
    ArraySqlType = 2003,
    BlobSqlType = 2004,
    ClobSqlType = 2005,
    RefSqlType = 2006,
    NativeCharSqlType = SQL_NATIVE_CHAR,
    NativeVarCharSqlType = SQL_NATIVE_VCHAR
/*    
    Bit = SQL_BIT,
    Char = SQL_CHAR,
#if ODBCVER >= 0x0300
    Date = SQL_TYPE_DATE,
    Time = SQL_TYPE_TIME,
    TimeStamp= SQL_TYPE_TIMESTAMP,
#else
    Date = SQL_DATE,
    Time = SQL_TIME,
    TimeStamp= SQL_TIMESTAMP,
#endif
    Decimal = SQL_DECIMAL,
    Double = SQL_DOUBLE,
    Float = SQL_FLOAT,
    Integer = SQL_INTEGER,
    LongVarBinary = SQL_LONGVARBINARY,
    LongVarChar = SQL_LONGVARCHAR,
    Numeric = SQL_NUMERIC,
    Real = SQL_REAL,
    SmallInt = SQL_SMALLINT,
    TinyInt = SQL_TINYINT,
    VarBinary = SQL_VARBINARY,
    VarChar  = SQL_VARCHAR
// to specific
    BigInt    = SQL_BIGINT, 
    Binary = SQL_BINARY,
    Bit = SQL_BIT,
    Char = SQL_CHAR,
#if ODBCVER >= 0x0300
    Date = SQL_TYPE_DATE,
    Time = SQL_TYPE_TIME,
    TimeStamp= SQL_TYPE_TIMESTAMP,
#else
    Date = SQL_DATE,
    Time = SQL_TIME,
    TimeStamp= SQL_TIMESTAMP,
#endif
    Decimal = SQL_DECIMAL,
    Double = SQL_DOUBLE,
    Float = SQL_FLOAT,
    Integer = SQL_INTEGER,
    LongVarBinary = SQL_LONGVARBINARY,
    LongVarChar = SQL_LONGVARCHAR,
    Numeric = SQL_NUMERIC,
    Real = SQL_REAL,
    SmallInt = SQL_SMALLINT,
    TinyInt = SQL_TINYINT,
    VarBinary = SQL_VARBINARY,
    VarChar  = SQL_VARCHAR
*/
/* better not to use this:
  static int BIT;// = -7
  static int TINYINT;// = -6
  static int BIGINT;// = -5
  static int LONGVARBINARY;// = -4
  static int VARBINARY;// = -3
  static int BINARY;// = -2
  static int LONGVARCHAR;// = -1 
  static int NULL;// = 0;
  static int CHAR;// = 1
  static int NUMERIC;// = 2
  static int DECIMAL;// = 3
  static int INTEGER;// = 4
  static int SMALLINT;// = 5
  static int FLOAT;// = 6
  static int REAL;// = 7
  static int DOUBLE;// = 8
  static int VARCHAR;// = 12
  static int DATE;// = 91
  static int TIME;// = 92
  static int TIMESTAMP;// = 93
  static int OTHER;// = 1111
  static int JAVA_OBJECT;// = 2000
  static int DISTINCT;// = 2001
  static int STRUCT;// = 2002
  static int ARRAY;// = 2003
  static int BLOB;// = 2004
  static int CLOB;// = 2005
  static int REF;// = 2006
*/
};

ACDK_DECL_INTERFACE(Types);
/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/04/08 10:53:21 $
  
*/
class ACDK_SQL_PUBLIC Types
{
  //ACDK_WITHOUT_METAINFO
public:
  
};

} // sql
} // acdk

#endif //acdk_sql_Types_h

