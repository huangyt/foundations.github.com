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
// $Header: /cvsroot/acdk/acdk/acdk_sql/src/acdk/sql/DatabaseMetaData.h,v 1.12 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_sql_DatabaseMetaData_h
#define acdk_sql_DatabaseMetaData_h

#include <acdk.h>
#include <acdk/sql/sql.h>
#include "ResultSet.h"


namespace acdk {
namespace sql {


enum BestRowFlags1
{
  bestRowUnknown = 0,
    bestRowNotPseudo = 1,
    bestRowPseudo = 2
  };
  enum BestRowFlags2
  {
    bestRowTemporary = 0,
    bestRowTransaction = 1,
    bestRowSession = 2
  };
  enum ProcedureResultFlags 
  {
    procedureResultUnknown = 0,
    procedureNoResult = 1,
    procedureReturnsResult = 2
  };
  enum ProcedureColumnFlags
  {
    procedureColumnUnknown = 0,
    procedureColumnIn = 1,
    procedureColumnInOut = 2,
    procedureColumnOut = 4,
    procedureColumnResult = 3,
    procedureColumnReturn = 5
  };
  enum ProcedureNullableFlags
  {
    procedureNoNulls = 0,
    procedureNullable = 1,
    procedureNullableUnknown = 2
  };
  enum ColumnNullableFlags
  {
    columnNoNulls = 0,
    columnNullable = 1,
    columnNullableUnknown = 2
  };
  enum VersionColumnFlags 
  {
    versionColumnUnknown = 0,
    versionColumnNotPseudo = 1,
    versionColumnPseudo = 2
  };
  enum ForeignKeyFlags
  {
    importedKeyCascade = 0,
    importedKeyRestrict = 1,
    importedKeySetNull = 2,
    importedKeyNoAction = 3,
    importedKeySetDefault = 4,
    importedKeyInitiallyDeferred = 5,
    importedKeyInitiallyImmediate = 6,
    importedKeyNotDeferrable = 7
  };
  enum TypeNullableFlags
  {
    typeNoNulls = 0,
    typeNullable = 1,
    typeNullableUnknown = 2
  };
  enum TypePredFlags
  {
    typePredNone = 0,
    typePredChar = 1,
    typePredBasic = 2,
    typeSearchable = 3
  };
  enum TableIndexFlags
  {
    tableIndexStatistic = 0,
    tableIndexClustered = 1,
    tableIndexHashed = 2,
    tableIndexOther = 3
  };



ACDK_DECL_INTERFACE(DatabaseMetaData);

/** 
  API: JDBC 2.0
  @author Roger Rene Kommer
  @version $Revision: 1.12 $
  @date $Date: 2005/04/08 10:53:20 $
  
*/
class ACDK_SQL_PUBLIC DatabaseMetaData
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DatabaseMetaData)
public:

  virtual bool allProceduresAreCallable() THROWS1(RSQLException) = 0;
  virtual bool allTablesAreSelectable() THROWS1(RSQLException) = 0;
  virtual RString getURL() THROWS1(RSQLException) = 0;
  virtual RString getUserName() THROWS1(RSQLException) = 0;
  virtual bool isReadOnly() THROWS1(RSQLException) = 0;
  virtual bool nullsAreSortedHigh() THROWS1(RSQLException) = 0;
  virtual bool nullsAreSortedLow() THROWS1(RSQLException) = 0;
  virtual bool nullsAreSortedAtStart() THROWS1(RSQLException) = 0;
  virtual bool nullsAreSortedAtEnd() THROWS1(RSQLException) = 0;
  virtual RString getDatabaseProductName() THROWS1(RSQLException) = 0;
  virtual RString getDatabaseProductVersion() THROWS1(RSQLException) = 0;
  virtual RString getDriverName() THROWS1(RSQLException) = 0;
  virtual RString getDriverVersion() THROWS1(RSQLException) = 0;
  virtual int getDriverMajorVersion() = 0;
  virtual int getDriverMinorVersion() = 0;
  virtual bool usesLocalFiles() THROWS1(RSQLException) = 0;
  virtual bool usesLocalFilePerTable() THROWS1(RSQLException) = 0;
  virtual bool supportsMixedCaseIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool storesUpperCaseIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool storesLowerCaseIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool storesMixedCaseIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool supportsMixedCaseQuotedIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool storesUpperCaseQuotedIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool storesLowerCaseQuotedIdentifiers() THROWS1(RSQLException) = 0;
  virtual bool storesMixedCaseQuotedIdentifiers() THROWS1(RSQLException) = 0;
  virtual RString getIdentiferQuoteString() THROWS1(RSQLException) = 0;
  virtual RString getSQLKeywords() THROWS1(RSQLException) = 0;
  virtual RString getNumericFunctions() THROWS1(RSQLException) = 0;
  virtual RString getStringFunctions() THROWS1(RSQLException) = 0;
  virtual RString getSystemFunctions() THROWS1(RSQLException) = 0;
  virtual RString getDateTimeFunctions() THROWS1(RSQLException) = 0;
  virtual RString getSearchStringEscape() THROWS1(RSQLException) = 0;
  virtual RString getExtraNameCharacters() THROWS1(RSQLException) = 0;
  virtual bool supportsAlterTableWithAddColumn() THROWS1(RSQLException) = 0;
  virtual bool supportsAlterTableWithDropColumn() THROWS1(RSQLException) = 0;
  virtual bool supportsColumnAliasing() THROWS1(RSQLException) = 0;
  virtual bool nullPlusNonNullIsNull() THROWS1(RSQLException) = 0;
  virtual bool supportsConvert() THROWS1(RSQLException) = 0;
  virtual bool supportsConvert(int fromType, int toType) THROWS1(RSQLException) = 0;
  virtual bool supportsTableCorrelationNames() THROWS1(RSQLException) = 0;
  virtual bool supportsDifferentTableCorrelationNames() THROWS1(RSQLException) = 0;
  virtual bool supportsExpressionsInOrderBy() THROWS1(RSQLException) = 0;
  virtual bool supportsOrderByUnrelated() THROWS1(RSQLException) = 0;
  virtual bool supportsGroupBy() THROWS1(RSQLException) = 0;
  virtual bool supportsGroupByUnrelated() THROWS1(RSQLException) = 0;
  virtual bool supportsGroupByBeyondSelect() THROWS1(RSQLException) = 0;
  virtual bool supportsLikeEscapeClause() THROWS1(RSQLException) = 0;
  virtual bool supportsMultipleResultSets() THROWS1(RSQLException) = 0;
  virtual bool supportsMultipleTransactions() THROWS1(RSQLException) = 0;
  virtual bool supportsNonNullableColumns() THROWS1(RSQLException) = 0;
  virtual bool supportsMinimumSQLGrammar() THROWS1(RSQLException) = 0;
  virtual bool supportsCoreSQLGrammar() THROWS1(RSQLException) = 0;
  virtual bool supportsExtendedSQLGrammar() THROWS1(RSQLException) = 0;
  virtual bool supportsANSI92EntryLevelSQL() THROWS1(RSQLException) = 0;
  virtual bool supportsANSI92IntermediateSQL() THROWS1(RSQLException) = 0;
  virtual bool supportsANSI92FullSQL() THROWS1(RSQLException) = 0;
  virtual bool supportsIntegrityEnhancementFacility() THROWS1(RSQLException) = 0;
  virtual bool supportsOuterJoins() THROWS1(RSQLException) = 0;
  virtual bool supportsFullOuterJoins() THROWS1(RSQLException) = 0;
  virtual bool supportsLimitedOuterJoins() THROWS1(RSQLException) = 0;
  virtual RString getSchemaTerm() THROWS1(RSQLException) = 0;
  virtual RString getProcedureTerm() THROWS1(RSQLException) = 0;
  virtual RString getCatalogTerm() THROWS1(RSQLException) = 0;
  virtual bool isCatalogAtStart() THROWS1(RSQLException) = 0;
  virtual RString getCatalogSeparator() THROWS1(RSQLException) = 0;
  virtual bool supportsSchemasInDataManipulation() THROWS1(RSQLException) = 0;
  virtual bool supportsSchemasInProcedureCalls() THROWS1(RSQLException) = 0;
  virtual bool supportsSchemasInTableDefinitions() THROWS1(RSQLException) = 0;
  virtual bool supportsSchemasInIndexDefinitions() THROWS1(RSQLException) = 0;
  virtual bool supportsSchemasInPrivilegeDefinitions() THROWS1(RSQLException) = 0;
  virtual bool supportsCatalogsInDataManipulation() THROWS1(RSQLException) = 0;
  virtual bool supportsCatalogsInProcedureCalls() THROWS1(RSQLException) = 0;
  virtual bool supportsCatalogsInTableDefinitions() THROWS1(RSQLException) = 0;
  virtual bool supportsCatalogsInIndexDefinitions() THROWS1(RSQLException) = 0;
  virtual bool supportsCatalogInPrivilegeDefinitions() THROWS1(RSQLException) = 0;
  virtual bool supportsPositionedDelete() THROWS1(RSQLException) = 0;
  virtual bool supportsPositionedUpdate() THROWS1(RSQLException) = 0;
  virtual bool supportsSelectForUpdate() THROWS1(RSQLException) = 0;
  virtual bool supportsStoredProcedures() THROWS1(RSQLException) = 0;
  virtual bool supportsSubqueriesInComparisons() THROWS1(RSQLException) = 0;
  virtual bool supportsSubqueriesInExists() THROWS1(RSQLException) = 0;
  virtual bool supportsSubqueriesInIns() THROWS1(RSQLException) = 0;
  virtual bool supportsSubqueriesInQuantifieds() THROWS1(RSQLException) = 0;
  virtual bool supportsCorrelatedSubqueries() THROWS1(RSQLException) = 0;
  virtual bool supportsUnion() THROWS1(RSQLException) = 0;
  virtual bool supportsUnionAll() THROWS1(RSQLException) = 0;
  virtual bool supportsOpenCursorsAcrossCommit() THROWS1(RSQLException) = 0;
  virtual bool supportsOpenCursorsAcrossRollback() THROWS1(RSQLException) = 0;
  virtual bool supportsOpenStatementsAcrossCommit() THROWS1(RSQLException) = 0;
  virtual bool supportsOpenStatementsAcrossRollback() THROWS1(RSQLException) = 0;
  virtual int getMaxBinaryLiteralLength() THROWS1(RSQLException) = 0;
  virtual int getMaxCharLiteralLength() THROWS1(RSQLException) = 0;
  virtual int getMaxColumnNameLength() THROWS1(RSQLException) = 0;
  virtual int getMaxColumnsInGroupBy() THROWS1(RSQLException) = 0;
  virtual int getMaxColumnsInIndex() THROWS1(RSQLException) = 0;
  virtual int getMaxColumnsInOrderBy() THROWS1(RSQLException) = 0;
  virtual int getMaxColumnsInSelect() THROWS1(RSQLException) = 0;
  virtual int getMaxColumnsInTable() THROWS1(RSQLException) = 0;
  virtual int getMaxConnections() THROWS1(RSQLException) = 0;
  virtual int getMaxCursorNameLength() THROWS1(RSQLException) = 0;
  virtual int getMaxIndexLength() THROWS1(RSQLException) = 0;
  virtual int getMaxSchemaNameLength() THROWS1(RSQLException) = 0;
  virtual int getMaxProcedureNameLength() THROWS1(RSQLException) = 0;
  virtual int getMaxCatalogNameLength() THROWS1(RSQLException) = 0;
  virtual int getMaxRowSize() THROWS1(RSQLException) = 0;
  virtual bool doesMaxRowSizeIncludeBlobs() THROWS1(RSQLException) = 0;
  virtual int getMaxStatementLength() THROWS1(RSQLException) = 0;
  virtual int getMaxStatements() THROWS1(RSQLException) = 0;
  virtual int getMaxTableNameLength() THROWS1(RSQLException) = 0;
  virtual int getMaxTablesInSelect() THROWS1(RSQLException) = 0;
  virtual int getMaxTableUserLength() THROWS1(RSQLException) = 0;
  virtual int getDefaultTransactionIsolation() THROWS1(RSQLException) = 0;
  virtual bool supportsTransactions() THROWS1(RSQLException) = 0;
  virtual bool supportsTransactionIsolationLevel(int level) THROWS1(RSQLException) = 0;
  virtual bool supportsDataDefinitionAndDataManipulationTransactions() THROWS1(RSQLException) = 0;
  virtual bool supportsDataManipulationTransactionsOnly() THROWS1(RSQLException) = 0;
  virtual bool dataDefinitionCausesTransactionCommit() THROWS1(RSQLException) = 0;
  virtual bool dataDefinitionIgnoredInTransactions() THROWS1(RSQLException) = 0;
  virtual RResultSet getProcedures(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(RSQLException) = 0;
  virtual RResultSet getProcedureColumns(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern, INP(RString) columnPattern) THROWS1(RSQLException) = 0;
  virtual RResultSet getTables(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern) THROWS1(RSQLException) = 0;
  virtual RResultSet getSchemas() THROWS1(RSQLException) = 0;
  virtual RResultSet getCatalogs() THROWS1(RSQLException) = 0;
  virtual RResultSet getTableTypes() THROWS1(RSQLException) = 0;
  virtual RResultSet getColumns(INP(RString) catalog, INP(RString) schemaPattern, INP(RString) namePattern,  INP(RString) columnPattern) THROWS1(RSQLException) = 0;
  virtual RResultSet getColumnPrivileges(INP(RString) catalog, INP(RString) schema, INP(RString) table,  INP(RString) columnPattern) THROWS1(RSQLException) = 0;
  virtual RResultSet getTablePrivileges(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException) = 0;
  virtual RResultSet getBestRowIdentifier(INP(RString) catalog, INP(RString) schema, INP(RString) table, int scope, bool nullable) THROWS1(RSQLException) = 0;
  virtual RResultSet getVersionColumns(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException) = 0;
  virtual RResultSet getPrimaryKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException) = 0;
  virtual RResultSet getImportedKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException) = 0;
  virtual RResultSet getExportedKeys(INP(RString) catalog, INP(RString) schema, INP(RString) table) THROWS1(RSQLException) = 0;
  virtual RResultSet getCrossReference(INP(RString) primCatalog, INP(RString) primSchema, INP(RString) primTable, INP(RString) forCatalog, INP(RString) forSchema, INP(RString) forTable)  THROWS1(RSQLException) = 0;
  virtual RResultSet getTypeInfo() THROWS1(RSQLException) = 0;
  virtual RResultSet getIndexInfo(INP(RString) catalog, INP(RString) schema, INP(RString) table, bool unique, bool approx) THROWS1(RSQLException) = 0; 
  virtual bool supportsResultType(int type) THROWS1(RSQLException) = 0;
  virtual bool supportsResultSetConcurrency(int type, int concur) THROWS1(RSQLException) = 0;
  virtual bool ownUpdatesAreVisible(int type) THROWS1(RSQLException) = 0;
  virtual bool ownDeletesAreVisible(int type) THROWS1(RSQLException) = 0;
  virtual bool ownInsertsAreVisible(int type) THROWS1(RSQLException) = 0;
  virtual bool othersUpdatesAreVisible(int type) THROWS1(RSQLException) = 0;
  virtual bool othersDeletesAreVisible(int type) THROWS1(RSQLException) = 0;
  virtual bool othersInsertsAreVisible(int type) THROWS1(RSQLException) = 0;
  virtual bool updatesAreDetected(int type) THROWS1(RSQLException) = 0;
  virtual bool deletesAreDetected(int type) THROWS1(RSQLException) = 0;
  virtual bool insertsAreDetected(int type) THROWS1(RSQLException) = 0;
  virtual bool supportsBatchUpdates() THROWS1(RSQLException) = 0;
  virtual RResultSet getUDTs(INP(RString) catalog, INP(RString) schema, INP(RString) typePattern, INP(RintArray) types) THROWS1(RSQLException) = 0;
  virtual RConnection getConnection() THROWS1(RSQLException) = 0;
  
};

} // sql
} // acdk

#endif //acdk_sql_DatabaseMetaData_h

