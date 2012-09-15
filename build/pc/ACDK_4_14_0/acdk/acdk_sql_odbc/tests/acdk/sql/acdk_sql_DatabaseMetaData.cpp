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
// $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/tests/acdk/sql/acdk_sql_DatabaseMetaData.cpp,v 1.6 2005/04/25 19:20:46 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
//#include <acdk/lang/Thread.h>
#include <acdk/sql/sql.h>
#include <acdk/sql/DriverManager.h>
#include <acdk/sql/SQLException.h>
#include <acdk/sql/Connection.h>
#include <acdk/sql/Statement.h>
#include <acdk/sql/ResultSet.h>
#include <acdk/sql/ResultSetMetaData.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/Win32DbgConsumer.h>
#include <acdk/sql/odbc/ODBCHandle.h>
#include <acdk/sql/DatabaseMetaData.h>

//int forceODBCLibToLink();

namespace tests {
namespace acdk {
namespace sql {
  
BEGIN_DECLARE_TEST( DatabaseMetaData_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( DatabaseMetaData_Test  )

BEGIN_DEFINE_TEST( DatabaseMetaData_Test )
  ADD_TEST( DatabaseMetaData_Test, standard ) 
END_DEFINE_TEST( DatabaseMetaData_Test )

using namespace ::acdk::lang; 
using namespace ::acdk::sql;


RString getDbUrl();

void DatabaseMetaData_Test::standard()
{
  //forceODBCLibToLink();
  //::acdk::util::logging::RLogger log = ::acdk::util::logging::LogManager::getRootLogger();
  //log->addConsumer(new ::acdk::util::logging::Win32DbgConsumer());
  //::acdk::util::logging::LogManager::Threshold = ::acdk::util::logging::SysDebug;
  // This Statement is currently needed to load the ODBC-DLL.
  // If no Class from acdk::sql::odbc is used, Class::forName will fail!
  // So let's start with a dummy line ...
    RDriver tdriver = (RDriver)Class::forName("acdk::sql::odbc::ODBCDriver")->newInstance();
    //DriverManager::registerDriver(tdriver);

  //RString url = "jdbc:odbc:acdk/user=acdk/password=acdk";
  RString url = getDbUrl();
  RDriver driver = DriverManager::getDriver(url);
  if (driver == Nil)
    testAssertComment(false, "Cannot load driver: " + url);
  RConnection connection = driver->connect(url, Nil);
  RDatabaseMetaData dmd = connection->getMetaData();
  RString dbname = dmd->getDatabaseProductName();
  RString dbversion = dmd->getDatabaseProductVersion();
  System::out 
    << "Database: " << dbname << "; Version: " << dbversion << "\n"
    << "User: " << dmd->getUserName() << "\n"
    << "IsReadOnly: " << dmd->isReadOnly() << "\n"
    << "nullsAreSortedHigh: " << dmd->nullsAreSortedHigh() << "\n"
    << "nullsAreSortedLow: " << dmd->nullsAreSortedHigh() << "\n"
    << "nullsAreSortedAtStart: " << dmd->nullsAreSortedHigh() << "\n"
    << "nullsAreSortedAtEnd: " << dmd->nullsAreSortedHigh() << "\n"
    << "getDriverName: " << dmd->getDriverName() << "\n"
    << "getDriverVersion: " << dmd->getDriverVersion() << "\n"
    << "usesLocalFiles: " << dmd->usesLocalFiles() << "\n"
    << "usesLocalFilePerTable: " << dmd->usesLocalFilePerTable() << "\n"
    << "supportsMixedCaseIdentifiers: " << dmd->supportsMixedCaseIdentifiers() << "\n"
    << "storesUpperCaseIdentifiers: " << dmd->storesUpperCaseIdentifiers() << "\n"
    << "storesLowerCaseIdentifiers: " << dmd->storesLowerCaseIdentifiers() << "\n"
    << "storesMixedCaseIdentifiers: " << dmd->storesMixedCaseIdentifiers() << "\n"
    << "supportsMixedCaseQuotedIdentifiers: " << dmd->supportsMixedCaseQuotedIdentifiers() << "\n"
    << "storesUpperCaseQuotedIdentifiers: " << dmd->storesUpperCaseQuotedIdentifiers() << "\n"
    << "storesLowerCaseQuotedIdentifiers: " << dmd->storesLowerCaseQuotedIdentifiers() << "\n"
    << "storesMixedCaseQuotedIdentifiers: " << dmd->storesMixedCaseQuotedIdentifiers() << "\n"
    << "getSQLKeywords: " << dmd->getSQLKeywords() << "\n"
    << "getNumericFunctions: " << dmd->getNumericFunctions() << "\n"
    << "getStringFunctions: " << dmd->getStringFunctions() << "\n"
    << "getSystemFunctions: " << dmd->getSystemFunctions() << "\n"
    << "getDateTimeFunctions: " << dmd->getDateTimeFunctions() << "\n"
    << "getSearchStringEscape: " << dmd->getSearchStringEscape() << "\n"
    << "getExtraNameCharacters: " << dmd->getExtraNameCharacters() << "\n"
    
    << "supportsAlterTableWithAddColumn: " << dmd->supportsAlterTableWithAddColumn() << "\n"
    << "supportsAlterTableWithDropColumn: " << dmd->supportsAlterTableWithDropColumn() << "\n"
    << "supportsColumnAliasing: " << dmd->supportsColumnAliasing() << "\n"
    << "nullPlusNonNullIsNull: " << dmd->nullPlusNonNullIsNull() << "\n"
    << "supportsConvert: " << dmd->supportsConvert() << "\n"
    << "supportsTableCorrelationNames: " << dmd->supportsTableCorrelationNames() << "\n"
    << "supportsDifferentTableCorrelationNames: " << dmd->supportsDifferentTableCorrelationNames() << "\n"
    << "supportsExpressionsInOrderBy: " << dmd->supportsExpressionsInOrderBy() << "\n"
    << "supportsOrderByUnrelated: " << dmd->supportsOrderByUnrelated() << "\n"
    << "supportsGroupBy: " << dmd->supportsGroupBy() << "\n"
    << "supportsGroupByUnrelated: " << dmd->supportsGroupByUnrelated() << "\n"
    << "supportsGroupByBeyondSelect: " << dmd->supportsGroupByBeyondSelect() << "\n"
    << "supportsLikeEscapeClause: " << dmd->supportsLikeEscapeClause() << "\n"
    << "supportsMultipleResultSets: " << dmd->supportsMultipleResultSets() << "\n"
    << "supportsMultipleTransactions: " << dmd->supportsMultipleTransactions() << "\n"
    << "supportsNonNullableColumns: " << dmd->supportsNonNullableColumns() << "\n"
    << "supportsMinimumSQLGrammar: " << dmd->supportsMinimumSQLGrammar() << "\n"
    << "supportsCoreSQLGrammar: " << dmd->supportsCoreSQLGrammar() << "\n"
    << "supportsExtendedSQLGrammar: " << dmd->supportsExtendedSQLGrammar() << "\n"
    ;
  if (dbname->equals("PostgreSQL") == false)
    System::out
    << "supportsANSI92EntryLevelSQL: " << dmd->supportsANSI92EntryLevelSQL() << "\n"
    << "supportsANSI92IntermediateSQL: " << dmd->supportsANSI92IntermediateSQL() << "\n"
    << "supportsANSI92FullSQL: " << dmd->supportsANSI92FullSQL() << "\n"
      ;

  System::out
    << "supportsIntegrityEnhancementFacility: " << dmd->supportsIntegrityEnhancementFacility() << "\n"
    << "supportsFullOuterJoins: " << dmd->supportsFullOuterJoins() << "\n"
    << "supportsLimitedOuterJoins: " << dmd->supportsLimitedOuterJoins() << "\n"
    << "getSchemaTerm: " << dmd->getSchemaTerm() << "\n"
    << "getProcedureTerm: " << dmd->getProcedureTerm() << "\n"
    << "getCatalogTerm: " << dmd->getCatalogTerm() << "\n"
    << "isCatalogAtStart: " << dmd->isCatalogAtStart() << "\n"
    << "getCatalogSeparator: " << dmd->getCatalogSeparator() << "\n"
    << "getCatalogSeparator: " << dmd->getCatalogSeparator() << "\n"
    << "supportsSchemasInProcedureCalls: " << dmd->supportsSchemasInProcedureCalls() << "\n"
    << "supportsSchemasInTableDefinitions: " << dmd->supportsSchemasInTableDefinitions() << "\n"
    << "supportsSchemasInIndexDefinitions: " << dmd->supportsSchemasInIndexDefinitions() << "\n"
    << "supportsSchemasInPrivilegeDefinitions: " << dmd->supportsSchemasInPrivilegeDefinitions() << "\n"
    << "supportsCatalogsInDataManipulation: " << dmd->supportsCatalogsInDataManipulation() << "\n"
    << "supportsCatalogsInProcedureCalls: " << dmd->supportsCatalogsInProcedureCalls() << "\n"
    << "supportsCatalogsInTableDefinitions: " << dmd->supportsCatalogsInTableDefinitions() << "\n"
    << "supportsCatalogsInIndexDefinitions: " << dmd->supportsCatalogsInIndexDefinitions() << "\n"
    // not supported << "supportsCatalogInPrivilegeDefinitions: " << dmd->supportsCatalogInPrivilegeDefinitions() << "\n"
    << "supportsPositionedDelete: " << dmd->supportsPositionedDelete() << "\n"
    << "supportsPositionedUpdate: " << dmd->supportsPositionedUpdate() << "\n"
    << "supportsSelectForUpdate: " << dmd->supportsSelectForUpdate() << "\n"
    << "supportsStoredProcedures: " << dmd->supportsStoredProcedures() << "\n"
    << "supportsSubqueriesInComparisons: " << dmd->supportsSubqueriesInComparisons() << "\n"
    << "supportsSubqueriesInExists: " << dmd->supportsSubqueriesInExists() << "\n"
    << "supportsSubqueriesInIns: " << dmd->supportsSubqueriesInIns() << "\n"
    << "supportsSubqueriesInQuantifieds: " << dmd->supportsSubqueriesInQuantifieds() << "\n"
    << "supportsCorrelatedSubqueries: " << dmd->supportsCorrelatedSubqueries() << "\n"
    << "supportsUnion: " << dmd->supportsUnion() << "\n"
    << "supportsUnionAll: " << dmd->supportsUnionAll() << "\n"
    << "supportsOpenCursorsAcrossCommit: " << dmd->supportsOpenCursorsAcrossCommit() << "\n"
    << "supportsOpenCursorsAcrossRollback: " << dmd->supportsOpenCursorsAcrossRollback() << "\n"
    << "supportsOpenStatementsAcrossCommit: " << dmd->supportsOpenStatementsAcrossCommit() << "\n"
    << "supportsOpenStatementsAcrossRollback: " << dmd->supportsOpenStatementsAcrossRollback() << "\n"
    << "getMaxBinaryLiteralLength: " << dmd->getMaxBinaryLiteralLength() << "\n"
    << "getMaxCharLiteralLength: " << dmd->getMaxCharLiteralLength() << "\n"
    << "getMaxColumnNameLength: " << dmd->getMaxColumnNameLength() << "\n"
    << "getMaxColumnsInGroupBy: " << dmd->getMaxColumnsInGroupBy() << "\n"
    << "getMaxColumnsInIndex: " << dmd->getMaxColumnsInIndex() << "\n"
    << "getMaxColumnsInOrderBy: " << dmd->getMaxColumnsInOrderBy() << "\n"
    << "getMaxColumnsInSelect: " << dmd->getMaxColumnsInSelect() << "\n"
    << "getMaxColumnsInTable: " << dmd->getMaxColumnsInTable() << "\n"
    << "getMaxConnections: " << dmd->getMaxConnections() << "\n"
    << "getMaxCursorNameLength: " << dmd->getMaxCursorNameLength() << "\n"
    << "getMaxIndexLength: " << dmd->getMaxIndexLength() << "\n"
    << "getMaxSchemaNameLength: " << dmd->getMaxSchemaNameLength() << "\n"
    << "getMaxProcedureNameLength: " << dmd->getMaxProcedureNameLength() << "\n"
    << "getMaxCatalogNameLength: " << dmd->getMaxCatalogNameLength() << "\n"
    << "getMaxRowSize: " << dmd->getMaxRowSize() << "\n"
    << "doesMaxRowSizeIncludeBlobs: " << dmd->doesMaxRowSizeIncludeBlobs() << "\n"
    << "getMaxStatementLength: " << dmd->getMaxStatementLength() << "\n"
    << "getMaxStatements: " << dmd->getMaxStatements() << "\n"
    << "getMaxTableNameLength: " << dmd->getMaxTableNameLength() << "\n"
    << "getMaxTablesInSelect: " << dmd->getMaxTablesInSelect() << "\n"
    //not supported << "getMaxTableUserLength: " << dmd->getMaxTableUserLength() << "\n"
    << "getDefaultTransactionIsolation: " << dmd->getDefaultTransactionIsolation() << "\n"
    << "supportsTransactions: " << dmd->supportsTransactions() << "\n"
    << "supportsDataDefinitionAndDataManipulationTransactions: " << dmd->supportsDataDefinitionAndDataManipulationTransactions() << "\n"
    << "supportsDataManipulationTransactionsOnly: " << dmd->supportsDataManipulationTransactionsOnly() << "\n"
    << "dataDefinitionCausesTransactionCommit: " << dmd->dataDefinitionCausesTransactionCommit() << "\n"
    << "dataDefinitionIgnoredInTransactions: " << dmd->dataDefinitionIgnoredInTransactions() << "\n"
/*

    << "nullsAreSortedHigh: " << dmd->nullsAreSortedHigh() << "\n"
    << "nullsAreSortedHigh: " << dmd->nullsAreSortedHigh() << "\n"
    */
  ;
  //RResultSet rset = dmd->getTables("", "", ""); // works for oracle, but not for  Microsoft SQL Server
  RResultSet rset = dmd->getTables("", "", ""); // works for oracle, but not for  Microsoft SQL Server
  System::out << "Tables:\n";
  while (rset->next() == true) 
  {
    System::out << "\t";
    RString catalog = rset->getString(1);
    RString schema = rset->getString(2);
    RString tableName = rset->getString(3);
    RString tableType = rset->getString(4);
    RString remarks = rset->getString(5);
    if (catalog != Nil)
      System::out << "; Catalog: " << catalog;
    if (schema != Nil)
      System::out << "; Schema: " << schema;
    if (tableName != Nil)
      System::out << "; TableName: " << tableName;
    if (tableType != Nil)
      System::out << "; tableType: " << tableType;
    if (remarks != Nil)
      System::out << "; remarks: " << tableType;
    System::out->println("");
  }
  System::out << "\n";
  System::out->flush();


  DriverManager::deregisterDriver(tdriver);  
}


} // namespace sql
} // namespace acdk
} // namespace tests



