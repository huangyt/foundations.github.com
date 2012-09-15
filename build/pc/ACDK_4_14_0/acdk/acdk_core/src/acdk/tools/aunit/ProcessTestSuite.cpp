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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/ProcessTestSuite.cpp,v 1.13 2005/02/05 10:45:03 kommer Exp $



#include "ProcessTestSuite.h"
#include <acdk/lang/Runtime.h>
#include <acdk/lang/Thread.h>
#include <acdk/io/CharReader.h>
#include <acdk/io/StringReader.h>

#include <acdk/lang/System.h>

namespace acdk {
namespace tools {
namespace aunit {

using namespace acdk::io;

enum ParsedTestResult
{
  Nothing,
  Begin,
  End,
  Success,
  Failure,
  Error
};

ACDK_DECL_CLASS(ProcOutpReader);
class ProcOutpReader
: extends Thread
{
public:
  //RString buffer;
  RCharReader _pin;
  RCharWriter _cout;
  RTest _test;
  
  RTestResult _testResult;
  ProcOutpReader(IN(RCharReader) pin, IN(RCharWriter) cout, IN(RTest) test = Nil, IN(RTestResult) testResult = Nil)
  : _pin(pin)
  , _cout(cout)
  , _test(test)
  , _testResult(testResult)
  {
  }
  RTest getTestFromName(IN(RString) tn)
  {
    if (instanceof(_test, ProcessTestCase) == true)
      return _test;
    else if (instanceof(_test, ProcessTestSuite) == true)
      return new ProcessTestCase(RProcessTestSuite(_test)->_executable, tn);
    return _test;
  }
  RTest getTestFromError(IN(RString) testLine, OUT(RString) errorDesc)
  {
    RString tn = testLine;
    int idx = tn->indexOf(": ");
    if (idx == -1)
      return Nil;
    tn = tn->substr(idx + 2);
    idx = tn->indexOf(": ");
    RString testname = tn->substr(0, idx);
    errorDesc = tn->substr(idx + 2);
    if (instanceof(_test, ProcessTestCase) == true)
      return _test;
    else if (instanceof(_test, ProcessTestSuite) == true)
      return new ProcessTestCase(RProcessTestSuite(_test)->_executable, tn);
    return Nil;
  }
  bool parseLine(IN(RString) line)
  {
    if (_testResult == Nil || line->startsWith("[AUNIT:") == false)
      return true;
    int ofl = strlen("[AUNIT:STRT][");
    RString rl = line->substr(ofl);
    int idxesn = rl->indexOf(']');
    RString testName = rl->substr(0, idxesn);
    //RString rest = rl->substr(idxesn + 1);
    RString rest = rl->substr(idxesn);
    if (line->startsWith("[AUNIT:STRT][") == true)
    {
      if (_testResult->startTest(getTestFromName(testName)) == false)
        return false;
    }
    else if (line->startsWith("[AUNIT:ENDT]") == true)
    {
      _testResult->endTest(getTestFromName(testName));
    }
    else if (line->startsWith("[AUNIT:ERRR]") == true)
    {
      _testResult->addError(getTestFromName(testName), new Throwable(rest));
    }
    else if (line->startsWith("[AUNIT:ERRR]") == true)
    {
      _testResult->addError(getTestFromName(testName), new TestException(rest));
    }
    return true;

  }
  void run()
  {
    try {
    int c;
    StringBuffer sb;
    while ((c = _pin->readChar()) != -1)
    {
      if (c == '\n')
      {
        RString line = sb.toString();
        if (parseLine(line) == false)
          break;
        sb << (ucchar)c;
        _cout->writeString(sb.toString());
        sb.set("");
      }
      else
        sb << (ucchar)c;
    }
    _cout->writeString(sb.toString());
    _pin->close();
    } catch (RIOException ex) {
    }
  }
};

RTestArray 
ProcessTestSuite::tests()
{
  if (_testFilled == true)
    return TestSuite::tests();
  _fillTestNames();
  return TestSuite::tests();
}


void 
ProcessTestSuite::run(IN(RTestResult) result)
{
  RString cmdline = _executable;
  if (_testName != Nil)
    cmdline = SBSTR(_executable << " -test-htmlreport " << _testName);
  else
    cmdline = SBSTR(_executable << " -test-htmlreport");
  RProcess proc = Runtime::exec(cmdline, _env);
  RCharReader pin = proc->getOutputReader()->getCharReader();
  
  RProcOutpReader outpthr = new ProcOutpReader(pin, _cout);
  RCharReader perr = proc->getErrorReader()->getCharReader();
  RProcOutpReader errpthr = new ProcOutpReader(perr, _cerr, this, result);
  if (result->startTest(this) == false)
    return;
  outpthr->start();
  errpthr->start();
  Thread::sleep(300);
  int pret = proc->waitFor(); 
  
  outpthr->join();
  errpthr->join();
  if (pret != 0)
  {
    result->addError(this, new Throwable(SBSTR("Process terminated with exit status: " << pret)));
  }
  result->endTest(this);
}

void 
ProcessTestSuite::runTest()
{
  RTestResult result = new TestResult();
  run(result);
}

RString 
ProcessTestSuite::toString()
{
  if (_testName != Nil)
    return SBSTR(_executable << ";" << _testName);
  return _executable;
}




RString
runProcess(IN(RProcess) proc, int waitUntil = 1000)
{
  StringWriter cout;
  RCharReader pin = proc->getOutputReader()->getCharReader();
  RProcOutpReader outpthr = new ProcOutpReader(pin, &cout);
  outpthr->start();
  int pret = proc->waitFor(waitUntil);
  outpthr->join();
  
  return cout.getString();
}

void 
ProcessTestSuite::setOutWriter(IN(RCharWriter) c) 
{ 
  _cout = c; 
  RTestArray ta = tests();
  for (int i = 0; i < ta->length(); ++i)
  {
    RTest t = ta[i];
    if (instanceof(t, ProcessTestSuite) == true)
      RProcessTestSuite(t)->setOutWriter(c);
    else if (instanceof(t, ProcessTestCase) == true)
      RProcessTestCase(t)->setOutWriter(c);
  }
}

void 
ProcessTestSuite::setErrWriter(IN(RCharWriter) c) 
{ 
  _cerr = c; 
  RTestArray ta = tests();
  for (int i = 0; i < ta->length(); ++i)
  {
    RTest t = ta[i];
    if (instanceof(t, ProcessTestSuite) == true)
      RProcessTestSuite(t)->setErrWriter(c);
    else if (instanceof(t, ProcessTestCase) == true)
      RProcessTestCase(t)->setErrWriter(c);
  }
}

bool 
ProcessTestSuite::_fillTestNames()
{
  RString cmdline = _executable + " -test-list";
  RProcess proc = acdk::lang::Runtime::exec(cmdline, _env);
  RString outp = runProcess(proc);
  //acdk::lang::System::out->println("OUTPUT: " + outp);
  RInputReader inp = new InputReader((RCharReader)new StringReader(outp));
  RString l;
  RString lastTestSuite;
  RProcessTestSuite lts;
  while ((l = inp->readLine()) != Nil)
  {
    l = l->trim();
    if (l->startsWith("TestSuite: ") == true)
    {
      
      lastTestSuite = l->substr(strlen("TestSuite: "));
      lts = new ProcessTestSuite(_executable, lastTestSuite);
      lts->_testFilled = true;
      addTest(&lts);
    }
    else if (l->startsWith("TestCase: ") == true)
    {
      RString tc = l->substr(strlen("TestCase: "));
      int idx;
      if ((idx = tc->indexOf(".")) != -1)
      {
        RString ts = tc->substr(0, idx);
        RString ltc = tc->substr(idx + 1);
        if (ts->equals(lastTestSuite) == true)
        {
          lts->addTest(new ProcessTestCase(_executable, tc));
        }
        else
        {
          addTest(new ProcessTestCase(_executable, tc));
        }
      }
      else
      {
        addTest(new ProcessTestCase(_executable, tc));
      }
    }
    else
    {
      //acdk::lang::System::err->println("Illformat line: " + l);
    }
  }
  _testFilled = true;
  return true;
}


RString 
ProcessTestCase::toString()
{
  return SBSTR(_executable << ";" << _testName);
}



void 
ProcessTestCase::run(IN(RTestResult) result)
{
  //acdk::lang::System::out->println("run: " + toString());
  RString cmdline = SBSTR(_executable << " -test-htmlreport " << _testName);
  RProcess proc = Runtime::exec(cmdline, _env);
  RCharReader pin = proc->getOutputReader()->getCharReader();
  
  RProcOutpReader outpthr = new ProcOutpReader(pin, _cout);
  RCharReader perr = proc->getErrorReader()->getCharReader();
  RProcOutpReader errpthr = new ProcOutpReader(perr, _cerr, this, result);
  if (result->startTest(this) == false)
    return;
  
  outpthr->start();
  errpthr->start();
  int pret = proc->waitFor();
  outpthr->join();
  errpthr->join();
  if (pret != 0)
  {
    result->addError(this, new Throwable(SBSTR("Process terminated with exit status: " << pret)));
  }
  result->endTest(this);
 
}


void 
ProcessTestCase::runTest()
{
  RTestResult result = new TestResult();
  run(result);
}

} //namespace aunit
} // namespace tools
} // namespace acdk 

