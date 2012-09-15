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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestRunner.cpp,v 1.35 2005/04/21 13:30:40 kommer Exp $


#include "TestRunner.h"
#include "TestConsoleListener.h"
#include "TestDebuggerListener.h"
#include "TestHtmlReport.h"

#include <acdk/lang/System.h>
#include <acdk/lang/CmdLineParser.h>
#include <acdk/lang/CmdLineParseException.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/NullWriter.h>
#include <acdk/io/File.h>
#include <acdk/io/TeeCharWriter.h>

namespace acdk {
namespace tools {
namespace aunit {

//static 
//core_vector<RTest> TestRunner::_tests(0);

using namespace acdk::io;

//static 
RTestArray 
TestRunner::_getTests()
{
  static RTestArray _stests = new TestArray(0);
  return _stests;
}


bool 
TestRunner::executeTest(IN(RTest) test, IN(RTestResult) result)
{
  test->run(result);
  /*
  @todo replace with TestReport
  _countTest += test->testCount();
  _countRunTest += result.runTests();
  _countFailure += result.testFailures();
  _countError += result.testErrors();
  if (_print_summary == true)
  {
    ::acdk::lang::System::out->print(
      "=========================================\n"
      " Test " + test->getName() + " result:\n");
    ::acdk::lang::System::out->println(
      "Tests=[" + String::valueOf(test->testCount()) + "]  Run=[" + result.runTests() 
      + "]  Failed=[" + result.testFailures() + "]  Error=[" + result.testErrors() + "]\n");
  }
  */
  return result->wasSuccessful();
}

bool TestRunner::executeTests(IN(RTestResult) result)
{
  
  bool erg = true;
  for (int i = 0; i < _getTests().length(); i++) 
  {
    RTest testToRun = _getTests()[i];
    
    if (instanceof(testToRun, TestSuite) == true) 
      testToRun = RTestSuite(testToRun)->suite();

    
    erg &= executeTest(testToRun, result);
  }
  return erg;
}

void TestRunner::listTests(IN(RTest) test)
{
  if (instanceof(test, TestSuite) == true) 
  {
    RTest sl = RTestSuite(test)->suite();
    if (sl.impl() != test.impl()) 
    {
      //System::out->println("TestSuite: " + test->getName());
      listTests(sl);
    } 
    else 
    {
      System::out->println("TestSuite: " + test->getName());
      RTestArray ta = RTestSuite(test)->tests();
      for (int i = 0; i < ta->length(); i++) 
      {
        listTests(ta[i]);
      }
    }
  } else if (instanceof(test, TestCase) == true) {
    System::out->println("TestCase:  " + test->getName());
  } else {
    System::out->println("Test:      " + test->getName());
  }
}

void TestRunner::listTests()
{
  RTestArray tests = _getTests();
  for (int i = 0; i < tests.length(); i++) 
  {
    listTests(tests[i]);
  }
}

bool TestRunner::executeTest(IN(RString) instrtest, IN(RTestResult) result)
{
  RString strtest = instrtest->replace("::", ".");
  RTestArray _tests = _getTests();
  for (int i = 0; i < _tests.length(); i++) 
  {
    if (_tests[i]->getName()->equals(strtest) == true) {
      if (instanceof(_tests[i], TestSuite) == true) 
        return executeTest(RTestSuite(_tests[i])->suite(), result);
      return executeTest(_tests[i], result);
    }
    if (instanceof(_tests[i], TestSuite) == true) {
      RTest ts = RTestSuite(_tests[i])->suite();
      RTestArray ta = RTestSuite(ts)->tests();
      for (int j = 0; j < ta->length(); j++) 
      {
        if (ta[j]->getName()->equals(strtest) == true) 
          return executeTest(ta[j], result);
      }
    }
  }
  System::out->println("Test not found: " + strtest);
  return false;
}

int TestRunner::executeTests(IN(RStringArray) args)
{
  CmdLineParser cmdparser;
  int printFlags = ReportAllSummary | ReportTestDetails | ReportStartEndTest;
  int reportFlags = ReportAllSummary | ReportTestDetails | ReportTestSuccess | ReportStartEndTest;
  bool printHtmlReport = false;
  try {
    cmdparser.addOption("-test-verbose", "-verbose", false, "prints status information for each test (default)");
    cmdparser.addOption("-test-print-summary", "-print-summary", false, "prints status information for each test (default)");
    cmdparser.addOption("-test-quiet", "-quite", false, "prints no status information for each test");
    cmdparser.addOption("-test-out", "-out", true, "Where to redirect out. 'nul' discarges the output. <filenename> append to file. By default output to console");
    cmdparser.addOption("-test-err", "-err", true, "Where to redirect err. 'nul' discarges the output. <filenename> append to file. By default output to console");
    cmdparser.addOption("-test-list", "-list", false, "List available tests.");
    cmdparser.addOption("-test-abortonfail", "-abortonfail", false, "Abort all Test if one test fails");
    cmdparser.addOption("-test-htmlreport", "-htmlreport", false, "Abort all Test if one test fails");
    cmdparser.addOption("-test-interactive", "-interactive", false, "Allow to run user interactive tests");
    
    ::acdk::util::RProperties props = cmdparser.parse(System::getProperties(), args, false, true);
    if (props->getProperty("-list") != Nil) 
    {
      listTests();
      return 0;
    }
    if (props->getProperty("-verbose") != Nil) 
    {
      printFlags |= ReportAllSummary | ReportTestDetails | ReportTestSuccess | ReportStartEndTest;
    } 
    else if (props->getProperty("-quite") != Nil) 
    {
      printFlags = 0;
    } 
    else if (props->getProperty("-print-summary") != Nil)
    {
      printFlags |= ReportAllSummary;
    }
    
    if (props->getProperty("-out") != Nil) 
    {
      RString of = props->getProperty("-out");
      if (of->equals("nul") == true)
        System::setOut(new ::acdk::io::PrintWriter((acdk::io::RWriter)new ::acdk::io::NullWriter()));
      else {
        System::setOut(new ::acdk::io::PrintWriter((acdk::io::RWriter)new ::acdk::io::FileWriter(of, true, true)));
      }
    }
    if (props->getProperty("-err") != Nil) 
    {
      RString of = props->getProperty("-err");
      if (of->equals("nul") == true)
        System::setErr(new ::acdk::io::PrintWriter((acdk::io::RWriter)new ::acdk::io::NullWriter()));
      else {
        System::setErr(new ::acdk::io::PrintWriter((acdk::io::RWriter)new ::acdk::io::FileWriter(of, true, true)));
      }
    }
    if (props->getProperty("-htmlreport") != Nil) 
    {
      printHtmlReport = true;
      
    }
    if (props->getProperty("-interactive") != Nil) 
    {
      TestCase::TestInBatchMode = false;
    }
  
    if (props->getProperty("-abortonfail") != Nil) 
    {
        _abortonfail = true;
    }
  } catch (RCmdLineParseException ex) {
    System::out->println(ex->getMessage());
    return 1;
  }
  RCharWriter errwriter = System::err->getOut();
  RCharWriter outwriter = System::out->getOut();
  RStringWriter buferr = new StringWriter();
  RStringWriter bufout = new StringWriter();
  System::err->setOut(new TeeCharWriter(&outwriter, &bufout));
  System::out->setOut(new TeeCharWriter(&errwriter, &buferr));
  RTestResult result = new TestResult(bufout, buferr);
  result->addTestListener(new TestConsoleListener(System::out, printFlags));
  result->addTestListener(new TestDebuggerListener(printFlags | ReportTestSuccess));
  bool erg = true;
  try {
    if (args->length() <= 1)
    {
      erg = executeTests(result);
    }
    else
    {
      for (int i = 1; i < args->length(); i++) 
      {
        erg &= executeTest(args[i], result);
        if (erg == false && _abortonfail == true)
          break;
      }
    }  
    if (printHtmlReport == true)
    {
      RString fileName = SBSTR(System::getAcdkHome() << acdk::io::File::separatorChar() << "testreports" <<  acdk::io::File::separatorChar() << System::getModuleName() << ".html");
      TestHtmlReport(reportFlags, fileName).print(result->getResults());
    }
    return (erg == true ? 0 : 1);
  } catch (RThrowable ex) {
    ex->printStackTrace();
    System::out->println("Unhandled exception " + ex->getClass()->getName() + " in TestRunner.executeTests: " + ex->getMessage());
  /* must not handle otherwise NullPointer doesn't work
  } catch (...) {
    System::out->println("Unhandled unknown exception in TestRunner.executeTests");
  */
  }
  
  return 1;
}



//static 
int TestRunner::testmain(RStringArray args)
{
   TestRunner trunner;
   return trunner.executeTests(args);
}


} // namespace aunit
} // namespace tools
} // acdk


