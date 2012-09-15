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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk/sql/odbc/ODBCDatabaseMetaData.h,v 1.7 2005/02/05 10:45:31 kommer Exp $

#ifndef acdk_sqlodbc_DatabaseMetaData_h
#define acdk_sqlodbc_DatabaseMetaData_h

#include "odbc.h"
#include "ODBCResultSet.h"

#include <acdk/sql/DatabaseMetaData.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace sql {
namespace odbc {

using namespace acdk::lang;

ACDK_DECL_CLASS(ODBCDatabaseMetaData);

class ACDK_SQL_ODBC_PUBLIC ODBCDatabaseMetaData
: extends ::acdk::lang::Object, implements ::acdk::sql::DatabaseMetaData
{
  ACDK_WITH_METAINFO(ODBCDatabaseMetaData)
public:
  virtual bool allProceduresAreCallable() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_ACCESSIBLE_PROCEDURES)->equals("Y") == true; }
  virtual bool allTablesAreSelectable() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_ACCESSIBLE_TABLES)->equals("Y") == true; }
  virtual RString getURL() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual RString getUserName() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_USER_NAME); }
  virtual bool isReadOnly() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_DATA_SOURCE_READ_ONLY)->equals("Y"); }
  virtual bool nullsAreSortedHigh() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_NULL_COLLATION) == SQL_NC_HIGH; }
  virtual bool nullsAreSortedLow() THROWS1(::acdk::sql::RSQLException) {  return getShortInfo(SQL_NULL_COLLATION) == SQL_NC_LOW; }
  virtual bool nullsAreSortedAtStart() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_NULL_COLLATION) == SQL_NC_START;  }
  virtual bool nullsAreSortedAtEnd() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_NULL_COLLATION) == SQL_NC_END; }
  virtual RString getDatabaseProductName() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_DBMS_NAME); }
  virtual RString getDatabaseProductVersion() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_DBMS_VER);  }
  virtual RString getDriverName() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_DRIVER_NAME); }
  virtual RString getDriverVersion() THROWS1(::acdk::sql::RSQLException) {  return getStringInfo(SQL_DRIVER_VER);  }
  virtual int getDriverMajorVersion() { THROW0(UnsupportedOperationException); return Nil; }
  virtual int getDriverMinorVersion() { THROW0(UnsupportedOperationException); return Nil; }

  virtual bool usesLocalFiles() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_FILE_USAGE) != SQL_FILE_NOT_SUPPORTED; }
  virtual bool usesLocalFilePerTable() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_FILE_USAGE) == SQL_FILE_TABLE;  }

  virtual bool supportsMixedCaseIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_IDENTIFIER_CASE) == SQL_IC_SENSITIVE;   }
  virtual bool storesUpperCaseIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_IDENTIFIER_CASE) == SQL_IC_UPPER; }
  virtual bool storesLowerCaseIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_IDENTIFIER_CASE) == SQL_IC_LOWER; }
  virtual bool storesMixedCaseIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_IDENTIFIER_CASE) == SQL_IC_LOWER; }

  virtual bool supportsMixedCaseQuotedIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_QUOTED_IDENTIFIER_CASE) == SQL_IC_SENSITIVE; }
  virtual bool storesUpperCaseQuotedIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_QUOTED_IDENTIFIER_CASE) == SQL_IC_UPPER; }
  virtual bool storesLowerCaseQuotedIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_QUOTED_IDENTIFIER_CASE) == SQL_IC_LOWER; }
  virtual bool storesMixedCaseQuotedIdentifiers() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_QUOTED_IDENTIFIER_CASE) == SQL_IC_SENSITIVE;  }
  virtual RString getIdentiferQuoteString() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual RString getSQLKeywords() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_KEYWORDS); }
  virtual RString getNumericFunctions() THROWS1(::acdk::sql::RSQLException);
  virtual RString getStringFunctions() THROWS1(::acdk::sql::RSQLException);
  virtual RString getSystemFunctions() THROWS1(::acdk::sql::RSQLException);
  virtual RString getDateTimeFunctions() THROWS1(::acdk::sql::RSQLException);
  virtual RString getSearchStringEscape() THROWS1(::acdk::sql::RSQLException) {  return getStringInfo(SQL_SEARCH_PATTERN_ESCAPE);  }
  virtual RString getExtraNameCharacters() THROWS1(::acdk::sql::RSQLException) {  return getStringInfo(SQL_SPECIAL_CHARACTERS);  }

  virtual bool supportsAlterTableWithAddColumn() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_ALTER_TABLE) & SQL_AT_ADD_COLUMN; }
  virtual bool supportsAlterTableWithDropColumn() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_ALTER_TABLE) & SQL_AT_DROP_COLUMN; }
  virtual bool supportsColumnAliasing() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_COLUMN_ALIAS)->equals("Y"); }
  virtual bool nullPlusNonNullIsNull() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_CONCAT_NULL_BEHAVIOR) == SQL_CB_NULL; }
  
  virtual bool supportsConvert() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_CONVERT_FUNCTIONS) & SQL_FN_CVT_CONVERT; }
  /**
    @param fromType is SQLType
    @param toType is SQLType
  */
  virtual bool supportsConvert(int fromType, int toType) THROWS1(::acdk::sql::RSQLException);
  virtual bool supportsTableCorrelationNames() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_CORRELATION_NAME) != SQL_CN_NONE; }
  virtual bool supportsDifferentTableCorrelationNames() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_CORRELATION_NAME) != SQL_CN_DIFFERENT; }
  virtual bool supportsExpressionsInOrderBy() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_EXPRESSIONS_IN_ORDERBY)->equals("Y") == true; }
  virtual bool supportsOrderByUnrelated() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_ORDER_BY_COLUMNS_IN_SELECT)->equals("Y") == true; }
  virtual bool supportsGroupBy() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_GROUP_BY) != SQL_GB_NOT_SUPPORTED; }
  virtual bool supportsGroupByUnrelated() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_GROUP_BY) == SQL_GB_NO_RELATION; }
  virtual bool supportsGroupByBeyondSelect() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_GROUP_BY) == SQL_GB_GROUP_BY_CONTAINS_SELECT; }
  
  virtual bool supportsLikeEscapeClause() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_LIKE_ESCAPE_CLAUSE)->equals("Y") == true; }
  virtual bool supportsMultipleResultSets() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_MULT_RESULT_SETS)->equals("Y") == true; }
  virtual bool supportsMultipleTransactions() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_MULTIPLE_ACTIVE_TXN)->equals("Y") == true; }
  virtual bool supportsNonNullableColumns() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_NON_NULLABLE_COLUMNS) == SQL_NNC_NON_NULL; }
  virtual bool supportsMinimumSQLGrammar() THROWS1(::acdk::sql::RSQLException) { return true; }
  virtual bool supportsCoreSQLGrammar() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_ODBC_SQL_CONFORMANCE) != SQL_OSC_MINIMUM; }
  virtual bool supportsExtendedSQLGrammar() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_ODBC_SQL_CONFORMANCE) == SQL_OSC_EXTENDED; }
  virtual bool supportsANSI92EntryLevelSQL() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SQL_CONFORMANCE) & SQL_SC_SQL92_ENTRY; }
  virtual bool supportsANSI92IntermediateSQL() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SQL_CONFORMANCE) & SQL_SC_SQL92_INTERMEDIATE; }
  virtual bool supportsANSI92FullSQL() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SQL_CONFORMANCE) & SQL_SC_SQL92_FULL; }
  virtual bool supportsIntegrityEnhancementFacility() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_ODBC_SQL_OPT_IEF)->equals("Y") == true; ; }
  virtual bool supportsOuterJoins() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_OJ_CAPABILITIES) & (SQL_OJ_LEFT | SQL_OJ_RIGHT | SQL_OJ_FULL | SQL_OJ_NESTED); }
  virtual bool supportsFullOuterJoins() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_OJ_CAPABILITIES) & (SQL_OJ_FULL | SQL_OJ_NESTED); }
  virtual bool supportsLimitedOuterJoins() THROWS1(::acdk::sql::RSQLException) { return supportsFullOuterJoins() == false &&  supportsFullOuterJoins(); }
  virtual RString getSchemaTerm() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_SCHEMA_TERM); }
  virtual RString getProcedureTerm() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_PROCEDURE_TERM); }
  virtual RString getCatalogTerm() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_CATALOG_TERM); }
  virtual bool isCatalogAtStart() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_CATALOG_LOCATION) == SQL_QL_START; }
  virtual RString getCatalogSeparator() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_CATALOG_NAME_SEPARATOR); }
  virtual bool supportsSchemasInDataManipulation() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SCHEMA_USAGE) & SQL_SU_DML_STATEMENTS; }
  virtual bool supportsSchemasInProcedureCalls() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SCHEMA_USAGE) & SQL_SU_PROCEDURE_INVOCATION;  }
  virtual bool supportsSchemasInTableDefinitions() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SCHEMA_USAGE) & SQL_SU_TABLE_DEFINITION;  }
  virtual bool supportsSchemasInIndexDefinitions() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SCHEMA_USAGE) & SQL_SU_INDEX_DEFINITION; }
  virtual bool supportsSchemasInPrivilegeDefinitions() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SCHEMA_USAGE) & SQL_SU_PRIVILEGE_DEFINITION;  }
  virtual bool supportsCatalogsInDataManipulation() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_CATALOG_USAGE) & SQL_CU_DML_STATEMENTS;  }
  virtual bool supportsCatalogsInProcedureCalls() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_CATALOG_USAGE) & SQL_CU_PROCEDURE_INVOCATION; }
  virtual bool supportsCatalogsInTableDefinitions() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_CATALOG_USAGE) & SQL_CU_TABLE_DEFINITION; }
  virtual bool supportsCatalogsInIndexDefinitions() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_CATALOG_USAGE) & SQL_CU_INDEX_DEFINITION; }
  virtual bool supportsCatalogInPrivilegeDefinitions() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool supportsPositionedDelete() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_POSITIONED_STATEMENTS) & SQL_PS_POSITIONED_DELETE; }
  virtual bool supportsPositionedUpdate() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_POSITIONED_STATEMENTS) & SQL_PS_POSITIONED_UPDATE; }
  virtual bool supportsSelectForUpdate() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_POSITIONED_STATEMENTS) & SQL_PS_SELECT_FOR_UPDATE; }
  
  virtual bool supportsStoredProcedures() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_PROCEDURES)->equals("Y") == true; }
  virtual bool supportsSubqueriesInComparisons() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SUBQUERIES) & SQL_SQ_COMPARISON; }
  virtual bool supportsSubqueriesInExists() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SUBQUERIES) & SQL_SQ_EXISTS; }
  virtual bool supportsSubqueriesInIns() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SUBQUERIES) & SQL_SQ_IN; }
  virtual bool supportsSubqueriesInQuantifieds() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SUBQUERIES) & SQL_SQ_QUANTIFIED; }
  virtual bool supportsCorrelatedSubqueries() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_SUBQUERIES) & SQL_SQ_CORRELATED_SUBQUERIES;  }

  virtual bool supportsUnion() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_UNION) & SQL_U_UNION;  }
  virtual bool supportsUnionAll() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_UNION) & (SQL_U_UNION | SQL_U_UNION_ALL); }
  virtual bool supportsOpenCursorsAcrossCommit() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_CURSOR_COMMIT_BEHAVIOR) == SQL_CB_PRESERVE; }
  virtual bool supportsOpenCursorsAcrossRollback() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_CURSOR_ROLLBACK_BEHAVIOR) == SQL_CB_PRESERVE; }
  virtual bool supportsOpenStatementsAcrossCommit() THROWS1(::acdk::sql::RSQLException) { return supportsOpenCursorsAcrossCommit(); }
  virtual bool supportsOpenStatementsAcrossRollback() THROWS1(::acdk::sql::RSQLException) { return supportsOpenCursorsAcrossRollback(); }
  
  virtual int getMaxBinaryLiteralLength() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_BINARY_LITERAL_LEN); }
  virtual int getMaxCharLiteralLength() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_CHAR_LITERAL_LEN); }
  virtual int getMaxColumnNameLength() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_COLUMN_NAME_LEN);  }
  virtual int getMaxColumnsInGroupBy() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_COLUMNS_IN_GROUP_BY);  }
  virtual int getMaxColumnsInIndex() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_COLUMNS_IN_INDEX);  }
  virtual int getMaxColumnsInOrderBy() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_COLUMNS_IN_ORDER_BY);  }
  virtual int getMaxColumnsInSelect() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_COLUMNS_IN_SELECT);  }
  virtual int getMaxColumnsInTable() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_COLUMNS_IN_TABLE);  }
  virtual int getMaxConnections() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_DRIVER_CONNECTIONS);  }
  virtual int getMaxCursorNameLength() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_CURSOR_NAME_LEN);  }
  virtual int getMaxIndexLength() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_INDEX_SIZE);  }
  
  virtual int getMaxSchemaNameLength() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_SCHEMA_NAME_LEN); }
  virtual int getMaxProcedureNameLength() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_PROCEDURE_NAME_LEN); }
  virtual int getMaxCatalogNameLength() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_CATALOG_NAME_LEN); }
  virtual int getMaxRowSize() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_ROW_SIZE); }
  virtual bool doesMaxRowSizeIncludeBlobs() THROWS1(::acdk::sql::RSQLException) { return getStringInfo(SQL_MAX_ROW_SIZE_INCLUDES_LONG)->equals("Y") == true; }
  
  virtual int getMaxStatementLength() THROWS1(::acdk::sql::RSQLException) { return getIntInfo(SQL_MAX_STATEMENT_LEN);  }
  virtual int getMaxStatements() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_CONCURRENT_ACTIVITIES); }
  virtual int getMaxTableNameLength() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_TABLE_NAME_LEN); }

  virtual int getMaxTablesInSelect() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_MAX_TABLES_IN_SELECT); }
  virtual int getMaxTableUserLength() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException);  return Nil; }

  virtual int getDefaultTransactionIsolation() THROWS1(::acdk::sql::RSQLException);
  virtual bool supportsTransactions() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_TXN_CAPABLE) != SQL_TC_NONE; }
  virtual bool supportsTransactionIsolationLevel(int level) THROWS1(::acdk::sql::RSQLException);

  virtual bool supportsDataDefinitionAndDataManipulationTransactions() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_TXN_CAPABLE) == SQL_TC_ALL; }
  virtual bool supportsDataManipulationTransactionsOnly() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_TXN_CAPABLE) == SQL_TC_DML;  }
  virtual bool dataDefinitionCausesTransactionCommit() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_TXN_CAPABLE) == SQL_TC_DDL_COMMIT; }
  virtual bool dataDefinitionIgnoredInTransactions() THROWS1(::acdk::sql::RSQLException) { return getShortInfo(SQL_TXN_CAPABLE) == SQL_TC_DDL_IGNORE; }
  
  virtual ::acdk::sql::RResultSet getProcedures(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getProcedureColumns(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern, INP(RString) columnPattern) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getTables(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(::acdk::sql::RSQLException);
  virtual ::acdk::sql::RResultSet getSchemas() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getCatalogs() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getTableTypes() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getColumns(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern,  INP(RString) columnPattern) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getColumnPrivileges(INP(RString) catalog, INP(RString) schema, INP(RString) table,  INP(RString) columnPattern) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getTablePrivileges(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getBestRowIdentifier(INP(RString) catalog, INP(RString) schema, INP(RString) table, int scope, bool nullable) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getVersionColumns(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getPrimaryKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getImportedKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getExportedKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getCrossReference(INP(RString) primCatalog, INP(RString) primSchema, INP(RString) primTable, INP(RString) forCatalog, INP(RString) forSchema, INP(RString) forTable)  THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getTypeInfo() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getIndexInfo(INP(RString) catalog, INP(RString) schema, INP(RString) table, bool unique, bool approx) THROWS1(::acdk::sql::RSQLException)  { THROW0(UnsupportedOperationException); return Nil; }

  virtual bool supportsResultType(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool supportsResultSetConcurrency(int type, int concur) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool ownUpdatesAreVisible(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool ownDeletesAreVisible(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool ownInsertsAreVisible(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool othersUpdatesAreVisible(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool othersDeletesAreVisible(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool othersInsertsAreVisible(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool updatesAreDetected(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool deletesAreDetected(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool insertsAreDetected(int type) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual bool supportsBatchUpdates() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RResultSet getUDTs(INP(RString) catalog, INP(RString) schema, INP(RString) typePattern, INP(RintArray) types) THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  virtual ::acdk::sql::RConnection getConnection() THROWS1(::acdk::sql::RSQLException) { THROW0(UnsupportedOperationException); return Nil; }
  ODBCDatabaseMetaData(RODBCConnection conn) : _conn(conn) { }
  ~ODBCDatabaseMetaData() { }
protected:
  RString getStringInfo(int code) THROWS1(::acdk::sql::RSQLException);
  short getShortInfo(int code) THROWS1(::acdk::sql::RSQLException);
  int getIntInfo(int code) THROWS1(::acdk::sql::RSQLException);
private:
  RODBCConnection _conn;
};

} // odbc
} // sql
} // acdk

#endif //acdk_sqlodbc_DatabaseMetaData_h

