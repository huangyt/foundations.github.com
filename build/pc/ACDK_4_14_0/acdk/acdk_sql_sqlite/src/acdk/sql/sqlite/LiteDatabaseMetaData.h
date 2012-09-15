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
// $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk/sql/sqlite/LiteDatabaseMetaData.h,v 1.6 2005/04/13 15:38:04 kommer Exp $

#ifndef acdk_sql_sqlite_LiteDatabaseMetaData_h
#define acdk_sql_sqlite_LiteDatabaseMetaData_h

#include "Config.h"
#include <acdk.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/DatabaseMetaData.h>

#include "LiteConnection.h"
#include "LiteTable.h"


namespace acdk {
namespace sql {
namespace sqlite {


ACDK_DECL_CLASS(LiteDatabaseMetaData);

/**
  Meta data for a open database
  @todo many function may not return valid values
*/
class ACDK_SQL_SQLITE_PUBLIC LiteDatabaseMetaData
: extends acdk::lang::Object
, implements acdk::sql::DatabaseMetaData
{
  ACDK_WITH_METAINFO(LiteDatabaseMetaData)
protected:
  RLiteConnection _con;
public:
  LiteDatabaseMetaData(IN(RLiteConnection) con) 
  : _con(con) 
  {}

  
  virtual bool allProceduresAreCallable() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool allTablesAreSelectable() THROWS1(RSQLException)
  {
    return true;
  }
  virtual RString getURL() THROWS1(RSQLException)
  {
    return _con->getURL(); 
  }

  virtual RString getUserName() THROWS1(RSQLException)
  {
    return "";
  }
  virtual bool isReadOnly() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool nullsAreSortedHigh() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool nullsAreSortedLow() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool nullsAreSortedAtStart() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool nullsAreSortedAtEnd() THROWS1(RSQLException)
  {
    return false;
  }

  virtual RString getDatabaseProductName() THROWS1(RSQLException)
  {
    return "SQLite";
  }

  virtual RString getDatabaseProductVersion() THROWS1(RSQLException)
  {
    return LiteDb::getVersion();
  }
  virtual RString getDriverName() THROWS1(RSQLException)
  {
    return "jdbc:SQLite";
  }
  virtual RString getDriverVersion() THROWS1(RSQLException)
  {
    return "1.2";
  }
  virtual int getDriverMajorVersion() { return 1; }
  virtual int getDriverMinorVersion() { return 2; }
  virtual bool usesLocalFiles() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool usesLocalFilePerTable() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool supportsMixedCaseIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool storesUpperCaseIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool storesLowerCaseIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool storesMixedCaseIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsMixedCaseQuotedIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool storesUpperCaseQuotedIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }

  virtual bool storesLowerCaseQuotedIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool storesMixedCaseQuotedIdentifiers() THROWS1(RSQLException)
  {
    return false; // don't know
  }

  virtual RString getIdentiferQuoteString() THROWS1(RSQLException)
  {
    return "\"";
  }
  virtual RString getSQLKeywords() THROWS1(RSQLException)
  {
    return "CREATE,DROP,ALTER,SELECT,UPDATE,DELETE,INSERT,VIEW,TABLE,INDEX,FROM,WHERE,COMMIT,ROLLBACK,TRIGGER";
  }
  virtual RString getNumericFunctions() THROWS1(RSQLException)
  {
    return ""; // don't know
  }

  virtual RString getStringFunctions() THROWS1(RSQLException)
  {
    return ""; // don't know
  }

  virtual RString getSystemFunctions() THROWS1(RSQLException)
  {
    return ""; // don't know
  }
  virtual RString getDateTimeFunctions() THROWS1(RSQLException)
  {
    return ""; // don't know
  }

  virtual RString getSearchStringEscape() THROWS1(RSQLException)
  {
    return ""; // don't know
  }
  virtual RString getExtraNameCharacters() THROWS1(RSQLException)
  {
    return ""; // don't know
  }
  virtual bool supportsAlterTableWithAddColumn() THROWS1(RSQLException)
  {
    return false; // don't know
  }

  virtual bool supportsAlterTableWithDropColumn() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsColumnAliasing() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool nullPlusNonNullIsNull() THROWS1(RSQLException)
  {
    return false; // don't know
  }

  virtual bool supportsConvert() THROWS1(RSQLException)
  {
    return true; 
  }
  virtual bool supportsConvert(int fromType, int toType) THROWS1(RSQLException)
  {
    return false; // ### TODO
  }
  virtual bool supportsTableCorrelationNames() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsDifferentTableCorrelationNames() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsExpressionsInOrderBy() THROWS1(RSQLException)
  {
    return true; 
  }
  virtual bool supportsOrderByUnrelated() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsGroupBy() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsGroupByUnrelated() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsGroupByBeyondSelect() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsLikeEscapeClause() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsMultipleResultSets() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsMultipleTransactions() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsNonNullableColumns() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsMinimumSQLGrammar() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsCoreSQLGrammar() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsExtendedSQLGrammar() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsANSI92EntryLevelSQL() THROWS1(RSQLException)
  {
    return true; // hope so
  }

  virtual bool supportsANSI92IntermediateSQL() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsANSI92FullSQL() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsIntegrityEnhancementFacility() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsOuterJoins() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsFullOuterJoins() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsLimitedOuterJoins() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual RString getSchemaTerm() THROWS1(RSQLException)
  {
    return ""; // don't know
  }
  virtual RString getProcedureTerm() THROWS1(RSQLException)
  {
    return ""; // don't know
  }

  virtual RString getCatalogTerm() THROWS1(RSQLException)
  {
    return ""; // don't know
  }
  virtual bool isCatalogAtStart() THROWS1(RSQLException)
  {
    return false;
  }
  virtual RString getCatalogSeparator() THROWS1(RSQLException)
  {
    return ""; // don't know
  }
  virtual bool supportsSchemasInDataManipulation() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsSchemasInProcedureCalls() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool supportsSchemasInTableDefinitions() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsSchemasInIndexDefinitions() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsSchemasInPrivilegeDefinitions() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsCatalogsInDataManipulation() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsCatalogsInProcedureCalls() THROWS1(RSQLException)
  {
    return false;
  }
  virtual bool supportsCatalogsInTableDefinitions() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsCatalogsInIndexDefinitions() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsCatalogInPrivilegeDefinitions() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsPositionedDelete() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsPositionedUpdate() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }
  virtual bool supportsSelectForUpdate() THROWS1(RSQLException)
  {
    return true;
  }
  virtual bool supportsStoredProcedures() THROWS1(RSQLException)
  {
    return false; 
  }

  virtual bool supportsSubqueriesInComparisons() THROWS1(RSQLException)
  {
    return true;
  }

  virtual bool supportsSubqueriesInExists() THROWS1(RSQLException)
  {
    return true;
  }

  virtual bool supportsSubqueriesInIns() THROWS1(RSQLException)
  {
    return true;
  }

  virtual bool supportsSubqueriesInQuantifieds() THROWS1(RSQLException)
  {
    return false; // don't know, probably not
  }

  virtual bool supportsCorrelatedSubqueries() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }
  virtual bool supportsUnion() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }

  virtual bool supportsUnionAll() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }

  virtual bool supportsOpenCursorsAcrossCommit() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }

  virtual bool supportsOpenCursorsAcrossRollback() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }
  virtual bool supportsOpenStatementsAcrossCommit() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }
  virtual bool supportsOpenStatementsAcrossRollback() THROWS1(RSQLException)
  {
     return false; // don't know, probably not
  }
  virtual int getMaxBinaryLiteralLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }

  virtual int getMaxCharLiteralLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxColumnNameLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxColumnsInGroupBy() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxColumnsInIndex() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxColumnsInOrderBy() THROWS1(RSQLException)
  {
    return 0; // don't know
  }

  virtual int getMaxColumnsInSelect() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxColumnsInTable() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxConnections() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxCursorNameLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxIndexLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }

  virtual int getMaxSchemaNameLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxProcedureNameLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxCatalogNameLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }

  virtual int getMaxRowSize() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual bool doesMaxRowSizeIncludeBlobs() THROWS1(RSQLException)
  {
    return true; //???
  }
  virtual int getMaxStatementLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxStatements() THROWS1(RSQLException)
  {
    return 0; // don't know
  }

  virtual int getMaxTableNameLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getMaxTablesInSelect() THROWS1(RSQLException)
  {
    return 0; // don't know
  }

  virtual int getMaxTableUserLength() THROWS1(RSQLException)
  {
    return 0; // don't know
  }
  virtual int getDefaultTransactionIsolation() THROWS1(RSQLException)
  {
    return Connection::TRANSACTION_READ_UNCOMMITTED;
  }
  virtual bool supportsTransactions() THROWS1(RSQLException)
  {
    return true;
  }

  virtual bool supportsTransactionIsolationLevel(int level) THROWS1(RSQLException)
  {
    if (level == Connection::TRANSACTION_READ_UNCOMMITTED)
      return true;
    return false;
  }
  virtual bool supportsDataDefinitionAndDataManipulationTransactions() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsDataManipulationTransactionsOnly() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool dataDefinitionCausesTransactionCommit() THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool dataDefinitionIgnoredInTransactions() THROWS1(RSQLException)
  {
    return false; // don't know
  }

  virtual RResultSet getProcedures(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(RSQLException)
  {
    return Nil; 
  }
  virtual RResultSet getProcedureColumns(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern, INP(RString) columnPattern) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getTables(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(RSQLException);

  virtual RResultSet getSchemas() THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getCatalogs() THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getTableTypes() THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getColumns(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern,  INP(RString) columnPattern) THROWS1(RSQLException)
  {
    return Nil;
  }

  virtual RResultSet getColumnPrivileges(INP(RString) catalog, INP(RString) schema, INP(RString) table,  INP(RString) columnPattern) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getTablePrivileges(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException)
  {
    return Nil;
  }

  virtual RResultSet getBestRowIdentifier(INP(RString) catalog, INP(RString) schema, INP(RString) table, int scope, bool nullable) THROWS1(RSQLException)
  {
    return Nil;
  }

  virtual RResultSet getVersionColumns(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getPrimaryKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getImportedKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getExportedKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getCrossReference(INP(RString) primCatalog, INP(RString) primSchema, INP(RString) primTable, INP(RString) forCatalog, INP(RString) forSchema, INP(RString) forTable)  THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getTypeInfo() THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual RResultSet getIndexInfo(INP(RString) catalog, INP(RString) schema, INP(RString) table, bool unique, bool approx) THROWS1(RSQLException)
  {
    return Nil;
  }
  virtual bool supportsResultType(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsResultSetConcurrency(int type, int concur) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool ownUpdatesAreVisible(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool ownDeletesAreVisible(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool ownInsertsAreVisible(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool othersUpdatesAreVisible(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool othersDeletesAreVisible(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool othersInsertsAreVisible(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool updatesAreDetected(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool deletesAreDetected(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool insertsAreDetected(int type) THROWS1(RSQLException)
  {
    return false; // don't know
  }
  virtual bool supportsBatchUpdates() THROWS1(RSQLException)
  {
    return false; 
  }
  virtual RResultSet getUDTs(INP(RString) catalog, INP(RString) schema, INP(RString) typePattern, INP(RintArray) types) THROWS1(RSQLException)
  {
    return Nil;
  }

  virtual RConnection getConnection() THROWS1(RSQLException) { return &_con; }
};

} // sqlite
} // sql 
} // acdk

#endif //acdk_sql_sqlite_LiteDatabaseMetaData_h
