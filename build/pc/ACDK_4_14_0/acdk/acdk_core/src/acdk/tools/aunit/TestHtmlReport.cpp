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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestHtmlReport.cpp,v 1.13 2005/04/19 10:37:27 kommer Exp $


#include "TestHtmlReport.h"
#include <acdk/lang/System.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/CharToByteWriter.h>
#include <acdk/util/SysDate.h>
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace tools {
namespace aunit {

TestHtmlReport::TestHtmlReport(int options, INP(acdk::io::RPrintWriter) out)
: _options(options)
, _out(out)
{
  if (_out == Nil)
    _out = acdk::lang::System::out;
}


TestHtmlReport::TestHtmlReport(int options, INP(RString) fileName)
: _options(options)
, _out(new acdk::io::PrintWriter((acdk::io::RCharWriter)new acdk::io::CharToByteWriter(new acdk::io::FileWriter(fileName), 
       acdk::locale::Encoding::getEncoding("UTF-8")->getEncoder())))
{
}

namespace {
  
void printHeader(INP(acdk::io::RPrintWriter) _out)
{
  _out->println("<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head>\n<body>");
  _out->println("<h1>Unit Test " + acdk::util::SysDate().toString() + "</h1>");
  _out->println("<a href=\"index.html\">Back to Index</a><br>");
}

void printFooter(INP(acdk::io::RPrintWriter) _out)
{
  _out->println("</body></html>");
}

RString getLocalLinkId(IN(RString) str)
{
  return str->replace("::", "_")->replace("/", "_");
}


void printInBox(StringBuffer& sb, IN(RString) text, IN(RString) color)
{
  sb << "<table border=0 width=\"100%\"><tr><td bgcolor=\"" << color << "\">" << text << "</td></tr></table>\n";
}

void printTestResult(INP(RTestResultEntry) entry, INP(acdk::io::RPrintWriter) _out)
{
  StringBuffer sb;
  sb << "<h2><a id=\"" << getLocalLinkId(entry->test->getName()) << "\">" << entry->test->getName() << "</a></h2>\n";
  int failures = entry->failures->length();
  int errors = entry->errors->length();
  int success = entry->success->length();
  if (failures == 0 && errors == 0)
  {
    printInBox(sb, SBSTR("Test OK with " << success << "  Test Expressions"), "green");
    
  }
  else
  {
    printInBox(sb, "Test FAILED ", "red");
    if (failures > 0)
    {
      sb << "Failures:<br>\n";
      sb << "<ul>\n";
      for (int i = 0; i < entry->failures->length(); ++i)
      {
        sb << "<li> " << entry->failures[i]->getMessage() << "</li>\n";
      }
      sb << "</ul>\n";
    }
    if (errors > 0)
    {
      sb << "Errors:<br>\n";
      sb << "<ul>\n";
      for (int i = 0; i < entry->errors->length(); ++i)
      {
        RThrowable ex = entry->errors[i];
        sb << "<li> " << ex->getMessage() << "<br>Called in:<br><pre>";
        _out->print(sb.toString());
        ex->printStackTrace(_out);
        sb.set("");
        
        sb << "</pre></li>\n";
      }
      sb << "</ul>\n";
    }
  }
  if (entry->output != Nil &&  entry->output->length() > 0)
    sb << "<br>Standard Output:<hr><pre>" << entry->output << "</pre>\n";
  if (entry->errput != Nil &&  entry->errput->length() > 0)
      sb << "<br>Error output:<hr><pre>" << entry->errput << "</pre>\n";
  _out->print(sb.toString());
}



void
_printStats(INP(RTestResultEntryArray) tests, INP(acdk::io::RPrintWriter) _out, int flags)
{
  using namespace acdk::io;
  RTestResultEntryArray errsOrFails = new TestResultEntryArray(0);
  int overallErrors = 0;
  int overallFailures = 0;
  int overallTests = 0;
  int i;
  StringBuffer sb;
  for (i = 0; i < tests->length(); ++i)
  {
    overallErrors += tests[i]->errors->length();
    overallFailures += tests[i]->failures->length();
    if (tests[i]->errors->length() > 0 || tests[i]->failures->length() > 0)
      errsOrFails->append(tests[i]);
  }
  if (overallFailures == 0 && overallErrors == 0)
    printInBox(sb, "TEST OK", "green");
  else
    printInBox(sb, "TEST FAILED", "red");
  sb << "Tests: " << tests->length() << "; Failures: " << overallFailures << "; Errors: " << overallErrors << "<br>\n";
  
  for (i = 0; i < tests->length(); ++i)
  {
    RString testname = tests[i]->test->getName();
    RString locid = getLocalLinkId(testname);
    bool error = false;
    if (tests[i]->errors->length() > 0 || tests[i]->failures->length() > 0)
      error = true;
    printInBox(sb,  SBSTR(" <a href=\"#" << locid << "\">" << testname << "</a> <br>\n"), RString(error == false ? "ltgreen" : "FF8080"));
  }
  _out->print(sb.toString());
}

} // anon namespace



void 
TestHtmlReport::print(INP(RTestResultEntryArray) tests)   
{
  printHeader(_out);
  _printStats(tests, _out, _options);
  for (int i = 0; i < tests->length(); ++i)
  {
    printTestResult(tests[i], _out);
  }
  printFooter(_out);
}


} //namespace aunit
} // namespace tools
} // namespace acdk 

